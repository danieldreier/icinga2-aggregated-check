-module(nagios).
-export([ok/0, ok/1, warning/0, warning/1, critical/0, critical/1, unknown/0, unknown/1]).

-define(OK, 0).
-define(WARNING, 1).
-define(CRITICAL, 2).
-define(UNKNOWN, 3).

% just keeping this logic around becuase we'll probably need it later in writing the check
string2code(String) ->
  % the desired check state must be converted from strings to numeric values
  case string:to_lower(String) of
    "ok"       -> Val = ?OK;
    "0"        -> Val = ?OK;
    "warning"  -> Val = ?WARNING;
    "1"        -> Val = ?WARNING;
    "critical" -> Val = ?CRITICAL;
    "2"        -> Val = ?CRITICAL;
    "unknown"  -> Val = ?UNKNOWN;
    "3"        -> Val = ?UNKNOWN
  end,
  Val.

ok(Message) ->
    io:format("OK: exited with message \"~w\" \n\n", [Message]),
    ok().
ok() ->
    halt(?OK).

warning(Message) ->
    io:format("WARNING: unexpected error message \"~w\" \n\n", [Message]),
    warning().
warning() ->
    halt(?WARNING).

critical(Message) ->
    io:format("ERROR: unexpected error message \"~w\" \n\n", [Message]),
    warning().
critical() ->
    halt(?CRITICAL).


unknown(Message) ->
    io:format("ERROR: unexpected error message \"~w\" \n\n", [Message]),
    unknown().
unknown() ->
    halt(?UNKNOWN).

