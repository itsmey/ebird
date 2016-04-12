%%----------------------------------------------------------------------
%% @author Ivan Mic <ivan.micr@gmail.com>
%% @doc
%%   Module responsible for start/restart of BIRD daemon.
%% @end
%%----------------------------------------------------------------------
-module(ebird_control).

-include("ebird.hrl").

-export([start_link/0,
         restart/0
        ]).

%% =====================================================================
%% API functions
%% =====================================================================
%%----------------------------------------------------------------------
%% @doc Find or start BIRD daemon.
%% @end
%%----------------------------------------------------------------------
-spec
start_link() -> ignore | {error, cant_communicate |
                                 cant_run_bird |
                                 extract_pid_error()}.
start_link() ->
  case inspect() of
    {ok, _Pid} ->
      ?LOG("BIRD daemon found. Application started."),
      ignore;
    {error, no_process} ->
      case run() of
        {ok, _Pid} ->
          ?LOG("BIRD daemon started. Application started."),
          ignore;
        {error, Reason} = Err ->
          ?LOG("Can't start application. Reason is ~w", [Reason]),
          Err
      end;
    {error, Reason} = Err ->
      ?LOG("Can't start application. Reason is ~w", [Reason]),
      Err
  end.

%%----------------------------------------------------------------------
%% @doc Kill and try to start again BIRD daemon.
%% @end
%%----------------------------------------------------------------------
-spec
restart() -> {ok, os_pid()} | {error, extract_pid_error() |
                                      cant_run_bird |
                                      cant_kill_bird}.
restart() ->
  case ebird_utils:cmd("pkill bird") of
    {_, 0} ->
      run();
    _ ->
      {error, cant_kill_bird}
  end.

%% =====================================================================
%% Auxiliary functions
%% =====================================================================
%%----------------------------------------------------------------------
%% @doc Inspect whether bird process exists and we can communicate with
%%      it via birdc.
%% @end
%%----------------------------------------------------------------------
-spec
inspect() -> {ok, Pid :: os_pid()} | {error, cant_communicate |
                                             extract_pid_error()}.
inspect() ->
  case extract_pid() of
    {ok, Pid} ->
      case ebird_utils:raw_request("show protocols") of
        {_, 0} ->
          {ok, Pid};
        _ ->
          {error, cant_communicate}
      end;
    Err ->
      Err
  end.

%%----------------------------------------------------------------------
%% @doc Determine PID of "bird" process in the system.
%% @end
%%----------------------------------------------------------------------
-spec
extract_pid() -> {ok, os_pid()} | {error, extract_pid_error()}.
extract_pid() ->
  case ebird_utils:cmd("pidof bird") of
    {PidStr, 0} ->
      case string:tokens(PidStr, " \n") of
        [Pid] ->
          {ok, Pid};
        _ ->
          {error, multiple_processes}
      end;
    {_, 1} ->
      {error, no_process};
    _ ->
      {error, access_error}
  end.

%%----------------------------------------------------------------------
%% @doc Run "bird" process.
%% @end
%%----------------------------------------------------------------------
-spec
run() -> {ok, os_pid()} | {error, extract_pid_error() | cant_run_bird}.
run() ->
  case ebird_utils:cmd(?BIRD) of
    {_, 0} ->
      extract_pid();
    _ ->
      {error, cant_run_bird}
  end.
