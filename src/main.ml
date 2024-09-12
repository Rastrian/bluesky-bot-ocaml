let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);

  Logs.info (fun m -> m "Starting the bot application...");

  Lwt_main.run (
    Lwt.join [
      (if Utils.get_env_var "AUTO_REPOST" = "true" then Bot.repost_loop () else Lwt.return_unit);
      (if Utils.get_env_var "AUTO_FOLLOW" = "true" then Bot.auto_follow_loop () else Lwt.return_unit);
      (if Utils.get_env_var "AUTO_UNFOLLOW" = "true" then Bot.unfollow_management_loop () else Lwt.return_unit);
    ]
  )