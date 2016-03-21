-module(aggcheck).
-export([query_icinga/1, start/2, count_checks/2]).

-define(OK, 0).
-define(WARNING, 1).
-define(CRITICAL, 2).
-define(UNKNOWN, 3).

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

query_icinga(Options) ->
  ok = setup_ssl(),

  ServiceFilter = aggcheck_cli:setting(service_filter, Options),
  Username = aggcheck_cli:setting(username, Options),
  Password = aggcheck_cli:setting(password, Options),
  Host = aggcheck_cli:setting(host, Options),
  Port = aggcheck_cli:setting(port, Options),
  Url = "https://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/v1/objects/services",
  %io:format("URL: ~p", [Url]),
  %PostBody = "{}",
  io:format("\n\nservice_filter: ~p\n\n", [list_to_binary(ServiceFilter)]),
  %PostBody = "{ \"type\": \"Host\", \"filter\": \"\\\"role::elasticsearch::data\\\" in host.groups  && service.name == \\\"disk\\\"\" }",
  %io:format(jsx:prettify(jsx:encode([{type,['Host', filter, [<<"\"role::elasticsearch\" in host.groups && service.name == \"disk\"">>]]}]))),
  %io:format(jsx:prettify(jsx:encode([{type,['Host', filter, [list_to_binary(ServiceFilter)]]}]))),
  %PostBody = "{ \"type\": \"Host\", \"filter\": \"" ++ ServiceFilter ++ "\" }",
  %TestJson = jsx:encode({type, {'Host', filter, {ServiceFilter}}}),
  PostBody = jsx:encode([{<<"type">>,<<"Host">>},{<<"filter">>,list_to_binary(ServiceFilter)}]),
  io:format("\n\nPostBody: ~p\n\n", [PostBody]),
  PostContentType = "application/json",
  Response = httpc:request(post, {Url, [auth_header({username,Username,password,Password}),
                                        {"X-HTTP-Method-Override","GET"}], PostContentType, PostBody},
                                      [{ssl, [{verify, verify_none}]}], []),

  %unknown(Response).
  case Response of
%    {error, {failed_connect, _}} -> Body = undef, Headers = undef, nagios:unknown("SSL error");
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} -> io:format("something worked, got headers ~p\n\n", [Headers]);
    {ok, {{"HTTP/1.1",404,"Not Found"}, _      , _   }} -> Body = [], Headers = [], io:format("404, no results found")
    end,
  io:format("response headers: ~p\n\n", [Headers]),
  io:format("body: ~p\n\n", [binary:list_to_bin(Body)]),
  jsx:decode(binary:list_to_bin(Body)).
%%
%
count_checks(IcingaResponse, Options) ->
  ok.

auth_header({username,Username,password,Password}) ->
% ruby version:
% auth = 'Basic ' + Base64.encode64("#{config[:username]}:#{config[:password]}").chomp
% auth_headers = { 'Authorization' => auth }
  { "Authorization", "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password) }.


