## 113.24.00

- Make LTL predicates comparable by tagging and id to each one. Fixes a
  functional comparison bug.

- Switched to PPX.

- Add an mli for `async_extended/src/reader_ext.ml` and remove a
  couple of unused functions (notably `Reader_ext` had its own version of
  `Reader.read_char`).

- Add async-friendly color print

- `Ltl.eval` should close the pipe after it is done with it.

- Deleted `Async_extended.Cml`.

- Remove `Async_extended.Std.Gzip` and redirect references to `Async_gzip.Std.Gzip`.

- Update `Command_rpc.Connection` to check the program before exec'ing it.
  The filename must now be absolute, exist, and be executable.  Previously
  errors with nonexistent or nonexecutable files would only be found out
  after forking.

- Change `Command_rpc.Command` to use `Versioned_rpc.Callee_converts` instead of
  `Versioned_rpc.Both_convert` so that commands can be constructed without client-side
  conversions.  Clients remain free to use conversions or not, as appropriate.

  Removed `val rpcs` from `Callee_converts` interfaces because nothing appears to use it,
  and `Both_convert` does not provide it.  Now `Both_convert.S` can be supplied to satisfy
  `Callee_converts.S`.

- Add simple example of Command_rpc

- Add `Deferred_cache`.

- Fixing a couple of issues noticed in `Command_rpc`:

  - If `propagate_stderr` is false, the child's stderr is now drained instead of
    ignored.

  - When connections are closed, stderr is now closed as well, which prevents
    a file descriptor leak if the child process is unresponsive.

## 113.00.00

- Added a more raw interface to `Delimited.Csv`.

## 112.35.00

- Added `Ltl` module, an implementation of linear temporal logic, which
  can be used to run online queries on sequences of states.
- Added `Interactive.Job`, for printing start/done messages for multiple
  simultaneous jobs.
- Made `Any_error` be `Applicative`.
- Added `Command_rpc` support for `Versioned_rpc.Both_convert.Plain`.

## 112.24.00

- Fixed misspelling in `Command_rpc.Connection`, renaming `propogate_stderr` as
  `propagate_stderr`.

## 112.17.00

- Added `Interactive` module for terminal interaction with users by
  command-line executables.

  `Interactive` was previously in `Iron_common`.
- In `Process`, added an `?env` argument to some functions.
- Allowed `Command_rpc` implementations to access the
  `Rpc.Connection.t` used to invoke them.

  There is an option to invoke `Command_rpc` implementations via sexp
  communication instead of `Rpc`, so implementations are given a value
  of a variant type `Sexp | Bin_io of Rpc.Connection.t`.
- Added `Resource` module, which abstracts the idea of acquiring and
  releasing a handle to a resource.

## 112.06.00

- Unwound a recent change to `Mailbox` where one invocation of `receive`
  would put aside items, preventing other invocations from noticing
  them.
- Added `Delimited.Row.nth_conv_exn`, as a counterpart to
  `get_conv_exn`.
- Fixed `File_updates` handling of identical mtimes.

## 112.01.00

- Clarified an error in `Rpc_proxy`.

