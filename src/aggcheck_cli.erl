-module(aggcheck_cli).
-export([main/1,setting/2]).

main([]) ->
    getopt:usage(option_spec_list(), escript:script_name());
main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, _NonOptArgs}} ->
            run_check(Options);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, escript:script_name())
    end.

setting(Param, Settings) ->
  {Param, Value} = lists:keyfind(Param, 1, Settings),
  Value.

run_check(Options) ->
   StdOut = nagios:new(setting(check_name, Options)),
   Result = case aggcheck:query_icinga(Options) of
     {ok, Data}      -> Data;
     {error, Reason} ->  nagios:add_output("error while connecting to Icinga API: " ++ io_lib:format("~p",[Reason]), nagios:set_state(unknown, StdOut))
   end,


   %io:format("~p", [aggcheck:getHostNames(Result)]).
   Counts = aggcheck:count_checks(Result),
   WarnThreshold = setting(warn_threshold, Options),
   CritThreshold = setting(crit_threshold, Options),
   OrderMode = setting(threshold_order, Options),
   TargetState = setting(state, Options),
   CheckState = aggcheck:overall_state(Counts, TargetState, WarnThreshold, CritThreshold, OrderMode),
   %io:format("result: ~p\n\n", [CheckState]).

   StdOut1 = nagios:set_state(CheckState, StdOut),
   {TargetState, ResultCount} = lists:keyfind(TargetState, 1, Counts),
   StdOut2 = nagios:add_perfdata("matches", integer_to_list(ResultCount), StdOut1),

   OutputMessage = aggcheck:alert_message(Counts, TargetState, WarnThreshold, CritThreshold, CheckState, OrderMode),
   StdOut3 = nagios:add_output(OutputMessage, StdOut2),

   % add perfdata to show each status
   StdOut4 = lists:foldl(fun({Key,Value},Output) -> nagios:add_perfdata(atom_to_list(Key), integer_to_list(Value), Output) end, StdOut3, Counts),

   % add extended output to show each check result by hostname and status
   StdOut5 = lists:foldl(fun([{host,Hostname},{check,Check},{check_hard_state,State}], Output) ->
                             Message = binary_to_list(Hostname) ++ ": " ++ binary_to_list(Check) ++ "=" ++ atom_to_list(nagios:code_to_status(trunc(State))), nagios:add_output(Message, Output) end,
                             StdOut4, aggcheck:getHostNames(Result)),
   io:format("~s\n", [nagios:render(StdOut5)]),
   nagios:halt_with(CheckState).


option_spec_list() ->
    [
     %% {Name,        ShortOpt,  LongOpt,          ArgSpec,               HelpMsg}
     {help,           undefined, "help",           undefined,             "Show the program options"},
     {check_name,     $n,        "name",           {string, "aggregated check"},   "Check name to display in output"},
     {username,       $U,        "username",       {string, "icinga2"},   "Username to connect to the icinga2 API"},
     {password,       $P,        "password",       string,                "Password to connect to the icinga2 API"},
     {host,           $h,        "host",           {string, "localhost"}, "Icinga2 API host name or IP address"},
     {port,           $p,        "port",           {integer, 5665},       "Icinga2 API server port"},
     {service_filter, $f,        "service-filter", string,                "icinga2 query filter for services"},
     {warn_threshold, $w,        "warn-threshold", {integer, 1},          "warning threshold for matching checks"},
     {crit_threshold, $c,        "crit-threshold", {integer, 2},          "critical threshold for matching checks"},
     {state,          $s,        "state",          {atom, "critical"},  "check state to test (ok, warn, critical, unknown)"},
     {threshold_order,$o,        "order",          {string, "min"},       "set whether thresholds are treated as minimum or maximum values (min, max)"},
     {debug,          undefined, "debug",          undefined,             "Enable verbose debug output"},
     {verbose,        $v,        "verbose",        integer,               "Verbosity level"}
    ].
