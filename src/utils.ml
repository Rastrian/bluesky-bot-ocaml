let get_env_var key =
  try Sys.getenv key with Not_found -> ""

let log_info message =
  Printf.printf "[INFO] %s\n" message;
  flush stdout
