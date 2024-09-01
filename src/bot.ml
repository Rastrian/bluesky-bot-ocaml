open Lwt.Infix
open Utils
open Str

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

let send_webhook_notifications webhook_url messages =
  let rec send_next = function
    | [] -> Lwt.return_unit
    | message :: rest ->
        let body = `Assoc [("content", `String message)] |> Yojson.Safe.to_string in
        let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
        Cohttp_lwt_unix.Client.post
          ~headers
          ~body:(Cohttp_lwt.Body.of_string body)
          (Uri.of_string webhook_url)
        >>= fun (_, body) ->
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        Logs.debug (fun m -> m "Webhook response: %s" body_str);
        Lwt_unix.sleep 0.5 >>= fun () ->
        send_next rest
  in
  send_next messages

let repost_loop () =
  Logs.info (fun m -> m "Starting repost loop...");
  let rec loop () =
    Logs.debug (fun m -> m "Fetching access token.");
    Api.get_access_token () >>= fun (token, did) ->
    Logs.debug (fun m -> m "Access token retrieved. Fetching mentions.");
    Api.get_mentions token >>= fun mentions ->

    let user_handle = get_env_var "IDENTIFIER" in
    let auto_repost = get_env_var "AUTO_REPOST" = "true" in
    let cc_pattern = Printf.sprintf "cc @%s" user_handle in

    Logs.info (fun m -> m "Number of mentions retrieved: %d" (List.length mentions));

    Lwt_list.iter_s (fun mention ->
        let text = Yojson.Safe.Util.(mention |> member "record" |> member "text" |> to_string) in
        let uri = Yojson.Safe.Util.(mention |> member "uri" |> to_string) in
        let cid = Yojson.Safe.Util.(mention |> member "cid" |> to_string) in
        Logs.debug (fun m -> m "Processing mention with text: \"%s\"." text);

        if auto_repost && (try ignore (search_forward (regexp_string cc_pattern) text 0); true with Not_found -> false) then
          Redis.get ("reposted:" ^ cid) >>= function
          | None ->
              Logs.debug (fun m -> m "Reposting mention with CID: %s." cid);
              Api.repost token did uri cid >>= fun () ->
              Redis.set ("reposted:" ^ cid) "1"
          | Some _ ->
              Logs.debug (fun m -> m "Already reposted mention with CID: %s." cid);
              Lwt.return_unit
        else (
          Logs.debug (fun m -> m "Mention does not contain the repost pattern or reposting is disabled.");
          Lwt.return_unit
        )
      ) mentions >>= fun () ->

    Logs.debug (fun m -> m "Repost cycle complete. Waiting for 1 minute before the next check.");
    Lwt_unix.sleep 60.0 >>= fun () ->
    loop ()
  in
  loop ()

let auto_follow_loop () =
  let auto_follow = get_env_var "AUTO_FOLLOW" = "true" in
  if not auto_follow then (
    Logs.info (fun m -> m "Auto-follow is disabled via AUTO_FOLLOW environment variable.");
    Lwt.return_unit
  ) else (
    Logs.info (fun m -> m "Starting auto-follow loop...");
    let rec loop () =
      Logs.debug (fun m -> m "Fetching access token.");
      Api.get_access_token () >>= fun (token, did) ->
      Logs.debug (fun m -> m "Access token retrieved. Fetching current followers and following lists.");
  
      Api.get_followers token did [] >>= fun followers ->
      Api.get_following token did [] >>= fun following ->
  
      let followers_handles = List.map (fun user -> Yojson.Safe.Util.(user |> member "handle" |> to_string)) followers in
      let following_handles = List.map (fun user -> Yojson.Safe.Util.(user |> member "handle" |> to_string)) following in
  
      Logs.info (fun m -> m "Number of followers retrieved: %d" (List.length followers_handles));
      Logs.info (fun m -> m "Number of following retrieved: %d" (List.length following_handles));
  
      Lwt_list.iter_s (fun handle ->
          Logs.debug (fun m -> m "Checking follower handle: %s." handle);
          if not (List.mem handle following_handles) then (
            Redis.get ("followed:" ^ handle) >>= function
            | None ->
                Logs.debug (fun m -> m "New follower detected: %s. Following back." handle);
                Api.follow token did handle >>= fun () ->
                Redis.set ("followed:" ^ handle) "1"
            | Some _ ->
                Logs.debug (fun m -> m "Already following: %s." handle);
                Lwt.return_unit
          ) else (
            Logs.debug (fun m -> m "Already following user %s according to following list." handle);
            Lwt.return_unit
          )
        ) followers_handles >>= fun () ->
  
      Logs.debug (fun m -> m "Auto-follow cycle complete. Waiting for 10 minutes before the next check.");
      Lwt_unix.sleep 600.0 >>= fun () ->
      loop ()
    in
    loop ()
  )  

let unfollow_management_loop () =
  Logs.info (fun m -> m "Starting unfollow management loop...");
  let rec loop () =
    Logs.debug (fun m -> m "Fetching access token.");
    Api.get_access_token () >>= fun (token, did) ->
    Logs.debug (fun m -> m "Access token retrieved. Fetching current followers and following lists.");
  
    Api.get_followers token did [] >>= fun followers ->
    Api.get_following token did [] >>= fun following ->
  
    let followers_handles = List.map (fun user -> Yojson.Safe.Util.(user |> member "handle" |> to_string)) followers in
    let following_handles = List.map (fun user -> Yojson.Safe.Util.(user |> member "handle" |> to_string)) following in
  
    let auto_unfollow = get_env_var "AUTO_UNFOLLOW" = "true" in
    let unfollow_webhook = get_env_var "UNFOLLOW_WEBHOOK" in
  
    if auto_unfollow then (
      Logs.debug (fun m -> m "Checking for users to auto-unfollow.");
      let unfollowed_count = ref 0 in 
      Lwt_list.iter_s (fun handle ->
          if not (List.mem handle followers_handles) then (
            Logs.debug (fun m -> m "%s does not follow back. Unfollowing." handle);
            Api.unfollow token did handle >>= fun () ->
            Redis.del handle >>= fun () ->
            incr unfollowed_count;
            Lwt.return_unit
          ) else Lwt.return_unit
        ) following_handles >>= fun () ->
      Logs.info (fun m -> m "Total users unfollowed: %d" !unfollowed_count);
      Lwt.return_unit
    ) else if unfollow_webhook <> "" then (
      Logs.debug (fun m -> m "Preparing to send unfollow notifications.");
      let messages = ref [] in
      let notified_count = ref 0 in
      Lwt_list.iter_s (fun handle ->
          if not (List.mem handle followers_handles) then (
            Redis.get ("notified:" ^ handle) >>= function
            | None ->
                let unfollow_url = "https://bsky.app/profile/" ^ handle in
                messages := !messages @ [unfollow_url];
                Redis.set_with_expiry ("notified:" ^ handle) "1" (7 * 24 * 60 * 60) >>= fun () ->
                incr notified_count;
                Lwt.return_unit
            | Some _ ->
                Logs.debug (fun m -> m "Already notified about %s unfollowing." handle);
                Lwt.return_unit
          ) else
            Lwt.return_unit
        ) following_handles >>= fun () ->
      send_webhook_notifications unfollow_webhook !messages >>= fun () ->
      Logs.info (fun m -> m "Total users notified of unfollow: %d" !notified_count);
      Lwt.return_unit
    ) else Lwt.return_unit >>= fun () ->
  
      Logs.debug (fun m -> m "Unfollow management cycle complete. Waiting for 10 minutes before the next check.");
      Lwt_unix.sleep 600.0 >>= fun () ->
      loop ()
  in
  loop ()  
