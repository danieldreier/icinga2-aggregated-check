-module(nagios).
-export([new/1, add_perfdata/3, add_output/2,set_state/2, render/1]).
-export([halt_with/1, code_to_status/1]).

% This module uses strings and list appending for readability. The performance
% impact of using strings instead of bit strings, and using list appending, is
% not noticable, so it's better to optimize for readability by people with less
% erlang experience.

% TODO: use function heads here instead of case
halt_with(ok)       -> halt(0);
halt_with(warning)  -> halt(1);
halt_with(critical) -> halt(2);
halt_with(unknown)  -> halt(3).

% TODO: use function heads here instead of case
code_to_status(0) -> ok;
code_to_status(1) -> warning;
code_to_status(2) -> critical;
code_to_status(3) -> unknown.

new(Name) when is_list(Name) ->
  #{name => Name,
    perfdata => [],
    text_output => [],
    check_state => unknown }.

add_perfdata(Metric, Value, #{perfdata := OldPerfData} = CheckData) when is_list(Metric) and is_list(Value) and is_map(CheckData) ->
  CheckData#{perfdata := OldPerfData ++ [{Metric, Value}]}.

add_output(Output, CheckData) when is_list(Output) and is_map(CheckData) ->
  #{text_output := OldOutput} = CheckData,
  CheckData#{text_output:= OldOutput ++ [Output]}.

set_state(State, CheckData) when is_map(CheckData) and is_atom(State) ->
  CheckData#{check_state := State}.

render(#{name := Name, text_output := TextOutput, check_state := Status, perfdata := PerfData} = Map) ->
  render_remaining(Name, Status, TextOutput, PerfData, []).

render_remaining(Name, Status, [FirstTextOutput|RestText], [{PerfKey,PerfValue}|RestPerfData], Output) when Name /= nil andalso Status /= nil ->
  NewOutput = Name ++ " " ++ string:to_upper(atom_to_list(Status)) ++ " - " ++ FirstTextOutput ++ " | " ++ PerfKey ++ "=" ++ PerfValue,
  render_remaining(nil, nil, RestText, RestPerfData, [NewOutput|Output]);

render_remaining(Name, Status, [FirstTextOutput|RestText], [], Output) when Name /= nil andalso Status /= nil ->
  NewOutput = Name ++ " " ++ string:to_upper(atom_to_list(Status)) ++ " - " ++ FirstTextOutput,
  render_remaining(nil, nil, RestText, [], [NewOutput|Output]);

% last text output when there is at least one perf data remaining
render_remaining(nil, nil, [LastTextOutput], [{PerfKey,PerfValue}|RestPerfData], Output) ->
  NewOutput = LastTextOutput ++ " | " ++ PerfKey ++ "=" ++ PerfValue,
  render_remaining(nil, nil, [], RestPerfData, [NewOutput|Output]);

% last text output when there is no perf data
render_remaining(nil, nil, [LastTextOutput], [], Output) ->
  render_remaining(nil, nil, [], [], [LastTextOutput|Output]);

render_remaining(nil, nil, [NextTextOutput|RestText], PerfData, Output) ->
  render_remaining(nil, nil, RestText, PerfData, [NextTextOutput|Output]);

render_remaining(nil, nil, [], [{PerfKey,PerfValue}|RestPerfData], Output) ->
  NewOutput = PerfKey ++ "=" ++ PerfValue,
  render_remaining(nil, nil, [], RestPerfData, [NewOutput|Output]);

render_remaining(nil, nil, [], [], Output) ->
  string:join(lists:reverse(Output), "\n").
