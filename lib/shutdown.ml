include Async.Std.Shutdown

let deprecated_shutdown_and_raise ?force status =
  shutdown ?force status;
  raise Async_kernel.Raw_monitor.Shutdown;
;;
