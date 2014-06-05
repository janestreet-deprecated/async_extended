module Any_error              = Any_error
module Async_heap             = Async_heap
module Cml                    = Cml
module Coalesced_throttle     = Coalesced_throttle
module Command_rpc            = Command_rpc
module Deferred_list          = Deferred_list
module Delimited              = Delimited
module Deprecated_async_bench = Deprecated_async_bench
module Embedded_script        = Embedded_script
module Enforcer               = Enforcer
module Fd_leak_check          = Fd_leak_check
module Generator              = Generator
module Gzip                   = Async_gzip

module Log = struct
  include Async.Std.Log
  include Extended_log
end

module Durable                = Durable
module Durable_subscription   = Durable_subscription
module Mailbox                = Mailbox
module Priority_queue         = Priority_queue
module Process                = Process
module Reader                 = Reader_ext
module Safe_pipe              = Safe_pipe
module Semaphore              = Semaphore
module Shutdown               = Shutdown
module Sound                  = Sound
module Udp                    = Udp
module Watcher                = Watcher
module Pipe_zipper            = Pipe_zipper
