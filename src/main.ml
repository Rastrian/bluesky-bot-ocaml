let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);

  Logs.info (fun m -> m "Starting the bot application...");

  Lwt_main.run (
    let open Bot in
    Lwt.join [
      repost_loop ();
      auto_follow_loop ();
      unfollow_management_loop ()
    ]
  )
