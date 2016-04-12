%%----------------------------------------------------------------------
%% @author Ivan Mic <ivan.micr@gmail.com>
%% @doc
%%   Utilities.
%% @end
%%----------------------------------------------------------------------
-module(ebird_utils).

-include("ebird.hrl").

-export([cmd/1,
         cmd/2,
         raw_request/1,
         request/1,
         print_request/1
        ]).

%% =====================================================================
%% API functions
%% =====================================================================
%%----------------------------------------------------------------------
%% @doc Executes Cmd, returns output from its stdout and exit code.
%% @end
%%----------------------------------------------------------------------
-spec
cmd (Cmd :: string()) -> {Out :: string(), Code :: integer()}.
cmd (Cmd) ->
  cmd (Cmd, []).

%%----------------------------------------------------------------------
%% @doc Executes Cmd and sends Data to its stdin.
%%   Returns output from its stdout and exit code.
%% @end
%%----------------------------------------------------------------------
-spec
cmd (Cmd :: string(), Data :: iodata()) -> {Out :: string(), Code :: integer()}.
cmd (Cmd, Data) ->
  Port = open_port ({spawn, Cmd}, [use_stdio, exit_status, stream]),
  port_command (Port, Data),
  Res = cmd_recv (Port, [], 0),
  case process_info (self (), trap_exit) of
    {_, true} -> receive
                   {'EXIT', Port, _} -> ok
                 end;
    _ -> ok
  end,
  case erlang:port_info (Port) of
    undefined -> ok;
    _         -> port_close (Port)
  end,
  Res.

%%----------------------------------------------------------------------
%% @doc Communicate with birdc interface. Return value is the same as
%%      cmd() function.
%% @end
%%----------------------------------------------------------------------
-spec
raw_request(Cmd :: string()) -> {Out :: string(), Code :: integer()}.
raw_request(Cmd) ->
  cmd(?BIRDC, Cmd ++ "\nexit\n").

%%----------------------------------------------------------------------
%% @doc Communicate with birdc interface.
%% @end
%%----------------------------------------------------------------------
-spec
request(Cmd :: string()) -> {ok, Out :: string()} | {error | term()}.
request(Cmd) ->
  case raw_request(Cmd) of
    {Out, 0} ->
      {ok, Out};
    Err ->
      {error, Err}
  end.

%%----------------------------------------------------------------------
%% @doc Communicate with birdc interface. Print output to stdin.
%% @end
%%----------------------------------------------------------------------
-spec
print_request(Cmd :: string()) -> ok | {error | term()}.
print_request(Cmd) ->
  case raw_request(Cmd) of
    {Out, 0} ->
      io:format("~s~n", [prepare(Out)]);
    Err ->
      {error, Err}
  end.

%% =====================================================================
%% Auxiliary functions
%% =====================================================================
cmd_recv (Port, Data, Status) ->
  receive
    {Port, {data, Msg}} ->
      cmd_recv (Port, Data ++ Msg, Status);
    {Port, {exit_status, Code}} ->
      {Data, Code}
  end.

%% @doc Remove extra lines from birdc output.
prepare(RawOut) ->
  [_, _ | Needed] = string:tokens(RawOut, "\n"),
  OutList = lists:droplast(Needed),
  string:join(OutList, "\n").

