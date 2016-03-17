#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose
-define(OK, 0).
-define(WARNING, 1).
-define(CRITICAL, 2).
-define(UNKNOWN, 3).

main(Args) ->
  %Settings = param(Args),
  %io:format("settings: ~p \n", [Settings]),
  %{ok, Threshold, _} = setting("--warn-threshold", param(Args)),
  %io:format("warn threshold: ~p \n", [Threshold]).
  query_icinga().

%% Argument Parser
%% take array of arguments
%% TODO: add default settings, like for port
param(Args) ->
  param([], Args).
param(Settings, []) ->
  Settings;
param(Settings, [Flag|Tail]) when Flag == "--debug" ->
  param(Settings ++ [{Flag, true}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--service-filter" ->
  param(Settings ++ [{Parameter, Setting}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--warn-threshold" ->
  param(Settings ++ [{Parameter, list_to_integer(Setting)}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "-w" ->
  param(Settings, ["--warn-threshold", Setting|Tail]);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--crit-threshold" ->
  param(Settings ++ [{Parameter, list_to_integer(Setting)}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "-c" ->
  param(Settings, ["--crit-threshold", Setting|Tail]);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--state" ->
  % the desired check state must be converted from strings to numeric values
  case string:to_lower(Setting) of
    "ok"       -> Val = ?OK;
    "0"        -> Val = ?OK;
    "warning"  -> Val = ?WARNING;
    "1"        -> Val = ?WARNING;
    "critical" -> Val = ?CRITICAL;
    "2"        -> Val = ?CRITICAL;
    "unknown"  -> Val = ?UNKNOWN;
    "3"        -> Val = ?UNKNOWN;
    _          -> Val = undef, usage({arg, Parameter, setting, Setting, suggestion, "'ok', 'warning', 'critical', or 'unknown'"})
  end,
  param(Settings ++ [{Parameter, Val}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--order" ->
  case string:to_lower(Setting) of
    "min" -> Val = min;
    "max" -> Val = max;
    _     -> Val = undef, usage({arg, Parameter, setting, Setting, suggestion, "'min' or 'max'"})
  end,
  param(Settings ++ [{Parameter, Val}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--username" ->
  param(Settings ++ [{Parameter, Setting}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--password" ->
  param(Settings ++ [{Parameter, Setting}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--hostname" ->
  param(Settings ++ [{Parameter, Setting}], Tail);
param(Settings, [Parameter, Setting|Tail]) when Parameter == "--port" ->
  param(Settings ++ [{Parameter, list_to_integer(Setting)}], Tail);
param(_Settings, [Flag|_Tail]) when Flag == "--help" ->
  usage();
param(_Settings, [Flag|_Tail]) ->
  usage(Flag).

setting(Param, Settings) ->
  {_Param, Value} = lists:keyfind(Param, 1, Settings),
  {ok, Value, Settings}.

%% Provide Usage Help
usage({arg, Arg, setting, Setting, suggestion, Suggestion}) ->
    io:format("\nERROR: \"~s\" is not a valid setting for parameter \"~s\". Try ~s.\n\n", [Setting,Arg, Suggestion]),
    usage();
usage(Arg) ->
    io:format("\nERROR: \"~s\" is not a valid parameter.\n\n", [Arg]),
    usage().
usage() ->
    io:format("usage: lots of flags\n\n"),
    halt(?UNKNOWN).
unknown(Message) ->
    io:format("ERROR: unexpected error message \"~w\" \n\n", [Message]),
    unknown().
unknown() ->
    halt(?UNKNOWN).

%debug(Msg,Settings) ->
%  {ok, Debug, _} = setting("--debug", param(Settings)),
%  case Debug of
%    true -> io:format(Msg);
%    _    -> ok
%  end.

query_icinga() ->
  Url = "https://icinga-master01-prod.ops.puppetlabs.net:5665/v1/objects/hosts",
  %Url = "https://google.com/",
  ok = application:start(asn1),
  ok = application:start(crypto),
  ok = application:start(public_key),
  ok = application:start(ssl),
  ok = inets:start(),
  Username = "icinga2",
  Password = "eech0Ogho8iivai1siegh5Ii",
  PostBody = "{}",
  PostContentType = "application/json",
  Response = httpc:request(post, {Url, [auth_header({username,Username,password,Password}),
                                        {"X-HTTP-Method-Override","GET"}], PostContentType, PostBody},
                                      [{ssl, [{verify, verify_none}]}], []),


  %unknown(Response).
  case Response of
    {error, {failed_connect, _}} -> Body = undef, Headers = undef, unknown("SSL error");
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} -> io:format("something worked, got headers ~p\n\n", [Headers])
  end,
  io:format("response headers: ~p\n\n", [Headers]),
  io:format("body: ~p\n\n", [binary:list_to_bin(Body)]).
%
auth_header({username,Username,password,Password}) ->
% ruby version:
% auth = 'Basic ' + Base64.encode64("#{config[:username]}:#{config[:password]}").chomp
% auth_headers = { 'Authorization' => auth }
  { "Authorization", "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password) }.


