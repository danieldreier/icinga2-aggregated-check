-module(nagios).
%-export([ok/0, ok/1, warning/0, warning/1, critical/0, critical/1, unknown/0, unknown/1]).
-compile([export_all]).

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

%% known problems with this output constructor:
%% - how to append the initial metric

% we will iteratively construct the output in nagios format
% first we start by rendering the first line of output, and tail calling render(CheckData, Output)
render(#{name := Name, text_output := [TextOutput|Output], check_state := State} = CheckData) ->
  render(CheckData#{
          text_output := Output},
          [Name ++ " " ++ string:to_upper(atom_to_list(State)) ++ " - " ++ TextOutput],{firstline,true}).

% if there is performance data to render, we put the first perf data in the first line
% dumb hack of the "{firstline,true}" parameter to ensure this is only called once, for the first line.
render(#{perfdata := [{MetricName,MetricValue}|RemainingMetrics]} = CheckData, [FirstOutput|[]],{firstline,true}) ->
  render(CheckData#{
          perfdata := RemainingMetrics},
          [FirstOutput ++ " | " ++ MetricName ++ "=" ++ MetricValue]).

% if there are remaining performance metrics to render, and one output string, we render them together
render(#{perfdata := [{MetricName,MetricValue}|RemainingMetrics], text_output := [TextOutput|[]] } = CheckData, Output) when is_list(Output) ->
  render(CheckData#{
          perfdata := RemainingMetrics,
          text_output := [] },
          Output ++ [TextOutput ++ " | " ++ MetricName ++ "=" ++ MetricValue]);

% if there are additional text output lines, we must render those before we get to any remaining metrics
render(#{text_output := [Head|Tail]} = CheckData, Output) when is_list(Output) ->
  render(CheckData#{text_output := Tail},
         Output ++ [Head]);

% if there are remaining performance metrics to render, but no more output strings, we render the metrics
render(#{perfdata := [{MetricName,MetricValue}|RemainingMetrics], text_output := [] } = CheckData, Output) when is_list(Output) ->
  render(CheckData#{
          perfdata := RemainingMetrics},
          Output ++ [MetricName ++ "=" ++ MetricValue ]);

%render(#{perfdata := [{MetricName,MetricValue}|RemainingMetrics], text_output := [] } = CheckData, Output) when is_list(Output) ->
render(#{perfdata := [], text_output := []}, Output) ->
  string:join(Output, "\n") ++ "\n".
