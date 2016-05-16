-module(aggcheck).
-export([query_icinga/1, start/2, count_checks/1]).
-compile([export_all]).

-define(OK, 0.0).
-define(WARNING, 1.0).
-define(CRITICAL, 2.0).
-define(UNKNOWN, 3.0).

start(_Type, _Args) ->
    {ok,self()}.

% start SSL processes that inets depends on because httpc client needs inets
setup_ssl() ->
  setup_ssl([asn1, crypto, public_key, ssl]).
setup_ssl([]) ->
  case inets:start() of
    ok                             -> ok;
    {error,{already_started,_App}} -> ok
  end;
setup_ssl([App|Apps]) ->
  case application:start(App) of
    ok                             -> ok;
    {error,{already_started,_App}} -> ok
  end,
  setup_ssl(Apps),
  ok.

auth_header({username,Username,password,Password}) ->
  { "Authorization", "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password) }.

% this appears to work as of March 20th
query_icinga(Options) ->
  ok = setup_ssl(),

  ServiceFilter = aggcheck_cli:setting(service_filter, Options),
  Username = aggcheck_cli:setting(username, Options),
  Password = aggcheck_cli:setting(password, Options),
  Host = aggcheck_cli:setting(host, Options),
  Port = aggcheck_cli:setting(port, Options),
  Url = "https://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/v1/objects/services",
  %io:format("\n\nservice_filter: ~p\n\n", [list_to_binary(ServiceFilter)]),
  PostBody = jsx:encode([{<<"type">>,<<"Host">>},{<<"filter">>,list_to_binary(ServiceFilter)}]),
  %io:format("\n\nPostBody: ~p\n\n", [PostBody]),
  PostContentType = "application/json",
  Response = httpc:request(post, {Url, [auth_header({username,Username,password,Password}),
                                        {"X-HTTP-Method-Override","GET"}], PostContentType, PostBody},
                                      [{ssl, [{verify, verify_none}]}], []),
  Result = case Response of
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}}   -> ok;
    {ok, {{"HTTP/1.1",404,"Not Found"}, _      , _   }}   -> Body = [], Headers = [], ok; %, io:format("404, no results found")
    {ok, {{_Version,401,"Unauthorized"}, Headers, Body}} -> {error, "HTTP 401 Unauthorized"};
    {error, Reason}                                       -> Body = [], {error, Reason}
    end,
  [{<<"results">>,ExtractedResults}] = jsx:decode(binary:list_to_bin(Body)),
  case Result of
    ok         -> {ok, lists:map( fun(X) -> {<<"attrs">>, Record} = lists:nth(1, X), Record end, ExtractedResults)};
    {error, Why} -> {error, Why}
  end.


% filter Icinga API response returns a list in the same format, but only
% containing results for checks in the state indicated by the State parameter.
% State param must be a float (1.0, 2.0) because of how the API response is decoded.
filter_results_by_state(ResultList, State) ->
  lists:filter(fun (X) ->
    case lists:keyfind(<<"last_hard_state">>, 1, X) of
        {<<"last_hard_state">>,State} -> Return = true;
        {<<"last_hard_state">>,_}     -> Return = false
    end,
    Return end,
  ResultList).

% select checks with active set to true
filter_active_results(ResultList) ->
  lists:filter(fun (X) ->
    case lists:keyfind(<<"active">>, 1, X) of
        {<<"active">>,true} -> Return = true;
        false               -> Return = false
    end,
    Return end,
  ResultList).

% extended check output will list which hosts matched, and what the display
% name and last hard state of the matched check was. This should make it
% faster to determine which backend host(s) and check(s) to investigate.
getHostNames(ResultList) ->
  lists:map(fun (X) ->
    {<<"host_name">>,       HostName}         = lists:keyfind(<<"host_name">>,       1, X),
    {<<"display_name">>,    CheckDisplayName} = lists:keyfind(<<"display_name">>,    1, X),
    {<<"last_hard_state">>, CheckHardState}   = lists:keyfind(<<"last_hard_state">>, 1, X),
    [{host, HostName}, {check, CheckDisplayName}, {check_hard_state, CheckHardState}] end,
  ResultList).

count_results_by_state(ResultList, State) ->
  length(filter_results_by_state(ResultList, State)).

count_checks(ResultList) ->
  % we only care about active (not disabled) checks, so filter for those first
  % after this, we only look at ActiveChecks not the full ResultList
  ActiveChecks = filter_active_results(ResultList),
  % find results in "OK" state
  OkCount = count_results_by_state(ResultList, ?OK),
  % find results in "warning" state
  WarnCount = count_results_by_state(ResultList, ?WARNING),
  % find results in "critical" state
  CritCount = count_results_by_state(ResultList, ?CRITICAL),
  % find results in "unknown" state
  UnknownCount = count_results_by_state(ResultList, ?UNKNOWN),
  % count total number of active checks
  TotalCount = length(ActiveChecks),
  [{total, TotalCount}, {ok, OkCount}, {warning, WarnCount}, {critical, CritCount}, {unknown, UnknownCount}].

% given a set of check counts, and a check to look for, determine whether we're above/below that threshold
check_threshold(Counts, TargetState, Threshold, OrderMode) ->
  case {lists:keyfind(TargetState, 1, Counts),OrderMode} of
    {{_State, Count}, OrderMode} when Count >= Threshold andalso OrderMode == "max" -> true;
    {{_State, Count}, OrderMode} when Count <  Threshold andalso OrderMode == "max" -> false;
    {{_State, Count}, OrderMode} when Count >= Threshold andalso OrderMode == "min" -> false;
    {{_State, Count}, OrderMode} when Count <  Threshold andalso OrderMode == "min" -> true
  end.


alert_message(Counts, TargetState, WarnThreshold, CritThreshold, ThresholdType, OrderMode) ->
  ThresholdViolated = case ThresholdType of
    critical -> CritThreshold;
    warning  -> WarnThreshold;
    ok       -> WarnThreshold
  end,
  OrderWord = case {OrderMode,ThresholdType} of
    {"min",ok} -> "is greater than warning(" ++ integer_to_list(WarnThreshold) ++ ") and critical(" ++ integer_to_list(CritThreshold) ++ ") thresholds";
    {"max",ok} -> "is less than warning(" ++ integer_to_list(WarnThreshold) ++ ") and critical(" ++ integer_to_list(CritThreshold) ++ ") thresholds";
    {"min",_} ->  "is less than or equal to" ++ " " ++ OrderMode ++ " " ++ atom_to_list(ThresholdType) ++ " threshold of " ++ integer_to_list(ThresholdViolated);
    {"max",_} ->  "is greater than or equal to" ++ " " ++ OrderMode ++ " " ++ atom_to_list(ThresholdType) ++ " threshold of " ++ integer_to_list(ThresholdViolated)
  end,
  {TargetState, CheckCount} = lists:keyfind(TargetState, 1, Counts),
  {total, TotalCount} = lists:keyfind(total, 1, Counts),
  integer_to_list(CheckCount) ++ "/" ++ integer_to_list(TotalCount) ++ " checks in " ++ atom_to_list(TargetState) ++ " state, which " ++ OrderWord.


% determine what check state this check should return, given the check counts,
% thresholds, and ordering mode
overall_state(Counts, TargetState, WarnThreshold, CritThreshold, OrderMode) ->
  Warn = check_threshold(Counts, TargetState, WarnThreshold, OrderMode),
  Crit = check_threshold(Counts, TargetState, CritThreshold, OrderMode),
  case {Warn, Crit} of
    {_,     true } -> critical;
    {true,  _    } -> warning;
    {false, false} -> ok
  end.
