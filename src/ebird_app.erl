%%----------------------------------------------------------------------
%% @author Ivan Mic <ivan.micr@gmail.com>
%% @doc
%%   Application callback module.
%% @end
%%----------------------------------------------------------------------
-module(ebird_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ebird_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

simple_test() ->
    ok = application:start(ebird),
    ?assertNot(undefined == whereis(ebird_sup)),
    ok = ebird:show_status(),
    ok = application:stop(ebird).

-endif.
