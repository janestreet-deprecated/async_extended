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

