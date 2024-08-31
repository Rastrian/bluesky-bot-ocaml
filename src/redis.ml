open Lwt.Infix
open Redis_lwt.Client
open Utils

let redis_url = get_env_var "REDIS_URL"

let get_client () =
  let uri = Uri.of_string redis_url in
  let host = Uri.host_with_default ~default:"localhost" uri in
  let port = Uri.port uri in
  let spec = { host; port = Option.value ~default:6379 port } in
  Redis_lwt.Client.connect spec

let get key =
  get_client () >>= fun client ->
  Redis_lwt.Client.get client key >>= function
  | Some value -> Lwt.return_some value
  | None -> Lwt.return_none

let set key value =
  get_client () >>= fun client ->
  Redis_lwt.Client.set client key value >|= fun _ -> ()

let set_with_expiry key value expiry =
  get_client () >>= fun client ->
  Redis_lwt.Client.setex client key expiry value >|= fun _ -> ()

let del key =
  get_client () >>= fun client ->
  Redis_lwt.Client.del client [key] >|= fun _ -> ()

let get_all_keys () =
  get_client () >>= fun client ->
  Redis_lwt.Client.keys client "*" >|= fun keys ->
  keys
