open Lwt.Infix
open Yojson.Safe
open Utils
open Ptime_clock

let () = Ssl.init ()

let api_url = "https://bsky.social/xrpc"

let make_request ~method_ ~headers ?body uri =
  let open Cohttp_lwt_unix.Client in
  (match method_ with
   | `GET -> get ~headers uri
   | `POST -> post ~headers ?body uri)
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  Lwt.return (status_code, body_str)

let handle_response ~action (status_code, body_str) =
  Logs.debug (fun m -> m "Response for %s received: %s" action body_str);
  if status_code = 200 then
    Ok (from_string body_str)
  else (
    Logs.err (fun m -> m "Failed to %s. Status code: %d, Response: %s" action status_code body_str);
    Error "API request failed"
  )

let get_access_token () =
  Logs.info (fun m -> m "Requesting access token...");
  let identifier = get_env_var "IDENTIFIER" in
  let password = get_env_var "PASSWORD" in
  let body = `Assoc [("identifier", `String identifier); ("password", `String password)] |> to_string in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  make_request ~method_:`POST ~headers ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string (api_url ^ "/com.atproto.server.createSession"))
  >>= fun response ->
  match handle_response ~action:"retrieve access token" response with
  | Ok json ->
      let token = Util.(json |> member "accessJwt" |> to_string) in
      let did = Util.(json |> member "did" |> to_string) in
      Logs.info (fun m -> m "Access token retrieved successfully.");
      Lwt.return (token, did)
  | Error e -> Lwt.fail_with e

let rec fetch_paginated_data token did endpoint cursor_key acc =
  Logs.info (fun m -> m "Fetching data from %s..." endpoint);
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)] in
  let uri = match cursor_key with
    | None -> Uri.of_string (api_url ^ endpoint ^ "?actor=" ^ did)
    | Some cursor -> Uri.of_string (api_url ^ endpoint ^ "?actor=" ^ did ^ "&cursor=" ^ cursor)
  in
  make_request ~method_:`GET ~headers uri
  >>= fun response ->
  match handle_response ~action:("retrieve " ^ endpoint) response with
  | Ok json ->
      let new_data = Util.(json |> member (if endpoint = "/app.bsky.graph.getFollowers" then "followers" else "follows") |> to_list) in
      let all_data = acc @ new_data in
      Logs.info (fun m -> m "Number of users retrieved so far: %d" (List.length all_data));
      (match Util.(json |> member "cursor" |> to_string_option) with
       | Some next_cursor ->
           Logs.info (fun m -> m "Fetching next page of users...");
           Lwt_unix.sleep 1.0 >>= fun () -> fetch_paginated_data token did endpoint (Some next_cursor) all_data
       | None ->
           Logs.info (fun m -> m "All users retrieved. Total: %d" (List.length all_data));
           Lwt.return all_data)
  | Error e -> Logs.err (fun m -> m "Error fetching data: %s" e); Lwt.return acc

let get_followers token did = fetch_paginated_data token did "/app.bsky.graph.getFollowers" None []
let get_following token did = fetch_paginated_data token did "/app.bsky.graph.getFollows" None []

let resolve_handle_to_did handle token =
  Logs.info (fun m -> m "Resolving handle to DID for user: %s" handle);
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)] in
  let uri = Uri.of_string (api_url ^ "/app.bsky.actor.getProfile?actor=" ^ handle) in
  make_request ~method_:`GET ~headers uri
  >>= fun response ->
  match handle_response ~action:("resolve handle to DID for " ^ handle) response with
  | Ok json -> Lwt.return (Util.(json |> member "did" |> to_string_option))
  | Error _ -> Lwt.return None

let perform_action token action user_data =
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token); ("Content-Type", "application/json")] in
  let body = to_string (`Assoc user_data) in
  let uri = Uri.of_string (api_url ^ (if action = "follow" then "/com.atproto.repo.createRecord" else "/com.atproto.repo.deleteRecord")) in

  make_request ~method_:`POST ~headers ~body:(Cohttp_lwt.Body.of_string body) uri
  >>= fun response ->
  match handle_response ~action:(action ^ " user") response with
  | Ok _ -> Logs.info (fun m -> m "Successfully performed %s action." action); Lwt.return_unit
  | Error e -> Logs.err (fun m -> m "Failed to perform %s action: %s" action e); Lwt.return_unit

let follow token did handle =
  Logs.info (fun m -> m "Following user: %s" handle);
  resolve_handle_to_did handle token >>= function
  | Some subject_did ->
      let created_at = now () |> Ptime.to_rfc3339 ~tz_offset_s:0 in
      let user_data = [
        ("repo", `String did);
        ("collection", `String "app.bsky.graph.follow");
        ("record", `Assoc [
            ("subject", `String subject_did);
            ("createdAt", `String created_at)
          ])
      ] in
      Logs.debug (fun m -> m "Sending follow request with data: %s" (to_string (`Assoc user_data)));
      perform_action token "follow" user_data >>= fun () ->
      Lwt.return_unit
  | None -> 
      Logs.err (fun m -> m "Could not resolve DID for handle %s. Cannot follow user." handle);
      Lwt.return_unit
    
let unfollow token did record_key =
  Logs.info (fun m -> m "Unfollowing user with record key: %s" record_key);
  let user_data = [
    ("repo", `String did);
    ("collection", `String "app.bsky.graph.follow");
    ("rkey", `String record_key)
  ] in
  Logs.debug (fun m -> m "Sending unfollow request with data: %s" (Yojson.Safe.to_string (`Assoc user_data)));
  perform_action token "unfollow" user_data >>= fun () ->
  Logs.info (fun m -> m "Successfully unfollowed user with record key: %s." record_key);
  Lwt.return_unit

let get_mentions token =
  Logs.info (fun m -> m "Fetching mentions...");
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)] in
  make_request ~method_:`GET ~headers (Uri.of_string (api_url ^ "/app.bsky.notification.listNotifications"))
  >>= fun response ->
  match handle_response ~action:"retrieve mentions" response with
  | Ok json ->
      let notifications = Util.(json |> member "notifications" |> to_list) in
      let mentions = List.filter (fun n -> Util.(n |> member "reason" |> to_string) = "mention") notifications in
      Logs.info (fun m -> m "Mentions retrieved successfully.");
      Lwt.return mentions
  | Error _ -> Logs.err (fun m -> m "Failed to retrieve mentions."); Lwt.return []

let repost token did uri cid =
  Logs.info (fun m -> m "Reposting mention with URI: %s" uri);
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token); ("Content-Type", "application/json")] in
  let body = `Assoc [
      ("$type", `String "app.bsky.feed.repost");
      ("repo", `String did);
      ("collection", `String "app.bsky.feed.repost");
      ("record", `Assoc [
          ("subject", `Assoc [("uri", `String uri); ("cid", `String cid)]);
          ("createdAt", `String (now () |> Ptime.to_rfc3339 ~tz_offset_s:0))
        ])
    ] |> to_string in

  make_request ~method_:`POST ~headers ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string (api_url ^ "/com.atproto.repo.createRecord"))
  >>= fun response ->
  match handle_response ~action:("repost URI " ^ uri) response with
  | Ok _ -> Logs.info (fun m -> m "Successfully reposted URI: %s." uri); Lwt.return_unit
  | Error _ -> Logs.err (fun m -> m "Failed to repost URI: %s." uri); Lwt.return_unit
