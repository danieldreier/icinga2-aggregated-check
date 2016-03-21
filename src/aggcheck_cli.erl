-module(aggcheck_cli).
-export([main/1,setting/2]).

main([]) ->
    getopt:usage(option_spec_list(), escript:script_name());
main(Args) ->
    OptSpecList = option_spec_list(),
    %io:format("For command line: ~p~n"
    %          "getopt:parse/2 returns:~n~n", [Args]),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            run_check(Options);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, escript:script_name())
    end.

setting(Param, Settings) ->
  %io:format("Settings: ~p", [Settings]),
  {_Param, Value} = lists:keyfind(Param, 1, Settings),
  Value.

run_check(Options) ->
   Result = aggcheck:query_icinga(Options),
   io:format("result: ~p\n\n", [Result]).

option_spec_list() ->
    [
     %% {Name,        ShortOpt,  LongOpt,          ArgSpec,               HelpMsg}
     {help,           undefined, "help",           undefined,             "Show the program options"},
     {username,       $U,        "username",       {string, "icinga2"},   "Username to connect to the icinga2 API"},
     {password,       $P,        "password",       string,                "Password to connect to the icinga2 API"},
     {host,           $h,        "host",           {string, "localhost"}, "Icinga2 API host name or IP address"},
     {port,           $p,        "port",           {integer, 5665},       "Icinga2 API server port"},
     {service_filter, $f,        "service-filter", string,                "icinga2 query filter for services"},
     {warn_threshold, $w,        "warn-threshold", {integer, 1},          "warning threshold for matching checks"},
     {crit_threshold, $c,        "crit-threshold", {integer, 2},          "critical threshold for matching checks"},
     {state,          $s,        "state",          {string, "CRITICAL"},  "check state to test (ok, warn, critical, unknown)"},
     {threshold_order,$o,        "order",          {string, "min"},       "set whether thresholds are treated as minimum or maximum values (min, max)"},
     {debug,          undefined, "debug",          undefined,             "Enable verbose debug output"},
     {verbose,        $v,        "verbose",        integer,               "Verbosity level"}
    ].
