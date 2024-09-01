open Lwt.Infix
open Yojson.Safe
open Utils
open Ptime_clock

let () =
  Ssl.init ()

let api_url = "https://bsky.social/xrpc"

let get_access_token () =
  Logs.info (fun m -> m "Requesting access token...");
  let identifier = get_env_var "IDENTIFIER" in
  let password = get_env_var "PASSWORD" in
  let body = `Assoc [("identifier", `String identifier); ("password", `String password)] |> to_string in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in

  Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string (api_url ^ "/com.atproto.server.createSession"))
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  Logs.debug (fun m -> m "Response for access token received: %s" body_str);
  if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
    let json = from_string body_str in
    let token = Util.(json |> member "accessJwt" |> to_string) in
    let did = Util.(json |> member "did" |> to_string) in
    Logs.info (fun m -> m "Access token retrieved successfully.");
    (token, did)
  else (
    Logs.err (fun m -> m "Failed to retrieve access token. Response: %s" body_str);
    failwith "Failed to get access token"
  )

let rec get_following token did ?(cursor=None) acc =
  Logs.info (fun m -> m "Fetching following users...");
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)] in
  let url = match cursor with
    | None -> Uri.of_string (api_url ^ "/app.bsky.graph.getFollows?actor=" ^ did)
    | Some cursor -> Uri.of_string (api_url ^ "/app.bsky.graph.getFollows?actor=" ^ did ^ "&cursor=" ^ cursor)
  in
  Cohttp_lwt_unix.Client.get ~headers url
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  Logs.debug (fun m -> m "Response for following users received: %s" body_str);
  if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
    let json = from_string body_str in
    let following = Util.(json |> member "follows" |> to_list) in
    let all_following = acc @ following in
    Logs.info (fun m -> m "Number of following users retrieved so far: %d" (List.length all_following));
    match Util.(json |> member "cursor" |> to_string_option) with
    | Some next_cursor ->
        Logs.info (fun m -> m "Fetching next page of following users...");
        get_following token did ~cursor:(Some next_cursor) all_following
    | None ->
        Logs.info (fun m -> m "All following users retrieved. Total: %d" (List.length all_following));
        Lwt.return all_following
  else (
    Logs.err (fun m -> m "Failed to retrieve following users. Response: %s" body_str);
    Lwt.return acc
  )
  
let rec get_followers token did ?(cursor=None) acc =
  Logs.info (fun m -> m "Fetching followers...");
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)] in
  let url = match cursor with
    | None -> Uri.of_string (api_url ^ "/app.bsky.graph.getFollowers?actor=" ^ did)
    | Some cursor -> Uri.of_string (api_url ^ "/app.bsky.graph.getFollowers?actor=" ^ did ^ "&cursor=" ^ cursor)
  in
  Cohttp_lwt_unix.Client.get ~headers url
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  Logs.debug (fun m -> m "Response for followers received: %s" body_str);
  if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
    let json = from_string body_str in
    let followers = Util.(json |> member "followers" |> to_list) in
    let all_followers = acc @ followers in
    Logs.info (fun m -> m "Number of followers retrieved so far: %d" (List.length all_followers));
    match Util.(json |> member "cursor" |> to_string_option) with
    | Some next_cursor ->
        Logs.info (fun m -> m "Fetching next page of followers...");
        get_followers token did ~cursor:(Some next_cursor) all_followers
    | None ->
        Logs.info (fun m -> m "All followers retrieved. Total: %d" (List.length all_followers));
        Lwt.return all_followers
  else (
    Logs.err (fun m -> m "Failed to retrieve followers. Response: %s" body_str);
    Lwt.return acc
  )

let resolve_handle_to_did handle token =
  Logs.info (fun m -> m "Resolving handle to DID for user: %s" handle);
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)] in
  let uri = Uri.of_string (api_url ^ "/app.bsky.actor.getProfile?actor=" ^ handle) in
  Cohttp_lwt_unix.Client.get ~headers uri
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  Logs.debug (fun m -> m "Response for resolving handle to DID: %s" body_str);
  if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
    let json = Yojson.Safe.from_string body_str in
    Util.(json |> member "did" |> to_string_option)
  else (
    Logs.err (fun m -> m "Failed to resolve handle to DID for %s. Response: %s" handle body_str);
    None
  )

let follow token did handle =
  Logs.info (fun m -> m "Following user: %s" handle);
  resolve_handle_to_did handle token >>= function
  | Some subject_did -> (
      let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token); ("Content-Type", "application/json")] in
      let created_at = now () |> Ptime.to_rfc3339 ~tz_offset_s:0 in
      let body = `Assoc [
          ("repo", `String did);
          ("collection", `String "app.bsky.graph.follow");
          ("record", `Assoc [
              ("subject", `String subject_did);
              ("createdAt", `String created_at)
            ])
        ] |> to_string in

      Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string (api_url ^ "/com.atproto.repo.createRecord"))
      >>= fun (resp, body) ->
      Cohttp_lwt.Body.to_string body >|= fun body_str ->
      Logs.debug (fun m -> m "Response for follow user %s: %s" handle body_str);
      if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
        Logs.info (fun m -> m "Successfully followed %s." handle)
      else
        Logs.err (fun m -> m "Failed to follow %s. Response: %s" handle body_str)
    )
  | None ->
      Logs.err (fun m -> m "Could not resolve DID for handle %s. Cannot follow user." handle);
      Lwt.return_unit

let unfollow token did record_key =
  Logs.info (fun m -> m "Unfollowing user with record key: %s" record_key);
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token); ("Content-Type", "application/json")] in
  let body = `Assoc [
      ("repo", `String did);
      ("rkey", `String record_key);
      ("collection", `String "app.bsky.graph.follow")
    ] |> to_string in

  let uri = Uri.of_string (api_url ^ "/com.atproto.repo.deleteRecord") in

  Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  Logs.debug (fun m -> m "Response for unfollowing user with record key %s: %s" record_key body_str);
  if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
    Logs.info (fun m -> m "Successfully unfollowed user with record key: %s." record_key)
  else
    Logs.err (fun m -> m "Failed to unfollow user with record key: %s. Response: %s" record_key body_str)

let get_mentions token =
  Logs.info (fun m -> m "Fetching mentions...");
  let headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)] in

  Cohttp_lwt_unix.Client.get ~headers (Uri.of_string (api_url ^ "/app.bsky.notification.listNotifications"))
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  Logs.debug (fun m -> m "Response for mentions received: %s" body_str);
  if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
    let notifications = Util.(from_string body_str |> member "notifications" |> to_list) in
    let mentions = List.filter (fun n -> Util.(n |> member "reason" |> to_string) = "mention") notifications in
    Logs.info (fun m -> m "Mentions retrieved successfully.");
    mentions
  else (
    Logs.err (fun m -> m "Failed to retrieve mentions. Response: %s" body_str);
    []
  )

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

  Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string (api_url ^ "/com.atproto.repo.createRecord"))
  >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_str ->
  Logs.debug (fun m -> m "Response for repost URI %s: %s" uri body_str);
  if Cohttp.Response.status resp |> Cohttp.Code.code_of_status = 200 then
    Logs.info (fun m -> m "Successfully reposted URI: %s." uri)
  else
    Logs.err (fun m -> m "Failed to repost URI: %s. Response: %s" uri body_str)
