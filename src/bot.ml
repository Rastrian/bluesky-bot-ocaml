open Lwt.Infix
open Utils
open Str

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

module Effect = struct
  type _ t =
    | FetchFollowers : string * string -> Yojson.Safe.t list t
    | FetchFollowing : string * string -> Yojson.Safe.t list t
    | FollowUser : string * string * string -> unit t
    | UnfollowUser : string * string * string -> unit t
    | RepostMention : string * string * string * string -> unit t
    | FetchMentions : string -> Yojson.Safe.t list t
end

let perform_effect : type a. a Effect.t -> a Lwt.t = function
  | Effect.FetchFollowers (token, did) -> Api.get_followers token did
  | Effect.FetchFollowing (token, did) -> Api.get_following token did
  | Effect.FollowUser (token, did, handle) -> Api.follow token did handle
  | Effect.UnfollowUser (token, did, record_key) -> Api.unfollow token did record_key
  | Effect.RepostMention (token, did, uri, cid) -> Api.repost token did uri cid
  | Effect.FetchMentions token -> Api.get_mentions token

let process_users_in_batches process_func users batch_size =
  let rec process_batch start_index =
    let end_index = min (start_index + batch_size) (List.length users) in
    let batch = List.filteri (fun i _ -> i >= start_index && i < end_index) users in
    Lwt_list.iter_s process_func batch >>= fun () ->
    if end_index < List.length users then
      Lwt_unix.sleep 1.0 >>= fun () -> process_batch end_index
    else Lwt.return_unit
  in
  process_batch 0

let fetch_all_users (effect_func : string * string -> Yojson.Safe.t list Effect.t) token did =
  let rec fetch_page acc =
    perform_effect (effect_func (token, did)) >>= fun users_list ->
    let all_users = acc @ users_list in
    match users_list with
    | [] -> Lwt.return all_users
    | _ :: _ ->
        (match Yojson.Safe.Util.(List.hd users_list |> member "cursor" |> to_string_option) with
         | Some _ -> Lwt_unix.sleep 1.0 >>= fun () -> fetch_page all_users
         | None -> Lwt.return all_users)
  in
  fetch_page []

let send_webhook_notifications webhook_url messages =
  let rec send_next = function
    | [] -> Lwt.return_unit
    | message :: rest ->
        let body = `Assoc [("content", `String message)] |> Yojson.Safe.to_string in
        let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
        Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string webhook_url)
        >>= fun (_, body) ->
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        Logs.debug (fun m -> m "Webhook response: %s" body_str);
        Lwt_unix.sleep 0.5 >>= fun () -> send_next rest
  in
  send_next messages

let auto_follow_loop () =
  Logs.info (fun m -> m "Starting auto-follow loop...");
  let rec loop () =
    let auto_follow = get_env_var "AUTO_FOLLOW" = "true" in
    if not auto_follow then (
      Logs.info (fun m -> m "Auto-follow is disabled. Skipping auto-follow loop.");
      Lwt_unix.sleep 600.0 >>= fun () -> loop ()
    ) else (
      Api.get_access_token () >>= fun (token, did) ->
      fetch_all_users (fun (token, did) -> Effect.FetchFollowers (token, did)) token did >>= fun followers ->
      fetch_all_users (fun (token, did) -> Effect.FetchFollowing (token, did)) token did >>= fun following ->
      let followers_set = StringSet.of_list (List.map (fun user -> Yojson.Safe.Util.(user |> member "handle" |> to_string)) followers) in
      let following_set = StringSet.of_list (List.map (fun user -> Yojson.Safe.Util.(user |> member "handle" |> to_string)) following) in
      let users_to_follow = StringSet.diff followers_set following_set in
      Logs.info (fun m -> m "Number of users to follow: %d" (StringSet.cardinal users_to_follow));
      process_users_in_batches
        (fun handle ->
          Redis.get ("followed:" ^ handle) >>= function
          | None ->
              Logs.info (fun m -> m "Following user: %s" handle);
              perform_effect (Effect.FollowUser (token, did, handle)) >>= fun () ->
              Redis.set ("followed:" ^ handle) "1"
          | Some _ ->
              Logs.debug (fun m -> m "Already following: %s" handle);
              Lwt.return_unit
        )
        (StringSet.elements users_to_follow)
        10 >>= fun () ->
      Lwt_unix.sleep 600.0 >>= fun () -> loop ()
    )
  in
  loop ()

let unfollow_management_loop () =
  Logs.info (fun m -> m "Starting unfollow management loop...");
  let rec loop () =
    Api.get_access_token () >>= fun (token, did) ->
    fetch_all_users (fun (token, did) -> Effect.FetchFollowers (token, did)) token did >>= fun followers ->
    fetch_all_users (fun (token, did) -> Effect.FetchFollowing (token, did)) token did >>= fun following ->
    let followers_set = StringSet.of_list (List.map (fun user -> Yojson.Safe.Util.(user |> member "handle" |> to_string)) followers) in
    let following_data = List.map (fun user ->
      let handle = Yojson.Safe.Util.(user |> member "handle" |> to_string) in
      let record_key = Yojson.Safe.Util.(user |> member "uri" |> to_string |> Filename.basename) in
      (handle, record_key)
    ) following in
    let auto_unfollow = get_env_var "AUTO_UNFOLLOW" = "true" in
    let unfollow_webhook = get_env_var "UNFOLLOW_WEBHOOK" in
    (if auto_unfollow then
      let users_to_unfollow = List.filter (fun (handle, _) -> not (StringSet.mem handle followers_set)) following_data in
      Logs.info (fun m -> m "Users to unfollow: %d" (List.length users_to_unfollow));
      process_users_in_batches
        (fun (handle, record_key) ->
          Logs.info (fun m -> m "Unfollowing user: %s" handle);
          perform_effect (Effect.UnfollowUser (token, did, record_key)) >>= fun () ->
          Redis.del handle
        )
        users_to_unfollow
        10
    else if unfollow_webhook <> "" then
      let users_to_notify = List.filter (fun (handle, _) -> not (StringSet.mem handle followers_set)) following_data in
      Logs.info (fun m -> m "Users to notify about unfollowing: %d" (List.length users_to_notify));
      process_users_in_batches
        (fun (handle, _) ->
          Redis.get ("notified:" ^ handle) >>= function
          | None ->
            let unfollow_url = "https://bsky.app/profile/" ^ handle in
            send_webhook_notifications unfollow_webhook [unfollow_url] >>= fun () ->
            Redis.set_with_expiry ("notified:" ^ handle) "1" (7 * 24 * 60 * 60)
          | Some _ ->
            Logs.debug (fun m -> m "Already notified about %s unfollowing." handle);
            Lwt.return_unit
        )
        users_to_notify
        10
    else Lwt.return_unit) >>= fun () ->
    Lwt_unix.sleep 600.0 >>= fun () -> loop ()
  in
  loop ()

let repost_loop () =
  Logs.info (fun m -> m "Starting repost loop...");
  let rec loop () =
    Api.get_access_token () >>= fun (token, did) ->
    perform_effect (Effect.FetchMentions token) >>= fun mentions ->
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
              perform_effect (Effect.RepostMention (token, did, uri, cid)) >>= fun () ->
              Redis.set ("reposted:" ^ cid) "1"
          | Some _ ->
              Logs.debug (fun m -> m "Already reposted mention with CID: %s." cid);
              Lwt.return_unit
        else
          Lwt.return_unit
      ) mentions >>= fun () ->
    Lwt_unix.sleep 60.0 >>= fun () -> loop ()
  in
  loop ()
