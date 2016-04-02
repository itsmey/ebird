-module(ebird_control).

-include("ebird.hrl").

-export([start_link/0]).

%% =====================================================================
%% API functions
%% =====================================================================
%%----------------------------------------------------------------------
%% @doc Find or start BIRD daemon.
%% @end
%%----------------------------------------------------------------------
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
      case ebird_utils:raw_birdc_request("show protocols") of
        {_, 0} ->
          {ok, Pid};
        _ ->
          {error, cant_communicate}
      end;
    Err ->
      Err
  end.






