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
      case ebird_utils:run_bird() of
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
restart() -> {ok, Pid :: os_pid()} | {error, term()}.
restart() ->
  {ok, 0}.

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
  case ebird_utils:extract_pid() of
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
