%%----------------------------------------------------------------------
%% @author Ivan Mic <ivan.micr@gmail.com>
%% @doc
%%   Interface module.
%% @end
%%----------------------------------------------------------------------
-module(ebird).

-export([
          command/1,
          show/1,
          configure/1,
          configure/0,
          check/1,
          check/0,
          action/2
        ]).

%% =====================================================================
%% API functions
%% =====================================================================
%%----------------------------------------------------------------------
%% @doc Perform arbitrary remote control command and print output.
%% @end
%%----------------------------------------------------------------------
-spec
command(string()) -> ok | {error, term()}.
command(Command) ->
  ebird_utils:print_request(Command).

%%----------------------------------------------------------------------
%% @doc Perform "show" command.
%% @end
%%----------------------------------------------------------------------
-spec
show(string()) -> ok | {error, term()}.
show(Arg) ->
  command("show " ++ Arg).

%%----------------------------------------------------------------------
%% @doc Perform "configure" command.
%% @end
%%----------------------------------------------------------------------
-spec
configure(string() | nil) -> ok | {error, term()}.
configure(Arg) ->
  Command = case Arg of
    nil      -> "configure";
    Filename -> "configure " ++ Filename
  end,
  command(Command).

-spec
configure() -> ok | {error, term()}.
configure() ->
  configure(nil).

%%----------------------------------------------------------------------
%% @doc Perform "configure check" command.
%% @end
%%----------------------------------------------------------------------
-spec
check(string() | nil) -> ok | {error, term()}.
check(Arg) ->
  Command = case Arg of
    nil      -> "configure check";
    Filename -> "configure check" ++ Filename
  end,
  command(Command).

-spec
check() -> ok | {error, term()}.
check() ->
  check(nil).

%%----------------------------------------------------------------------
%% @doc Perform protocol action command.
%% @end
%%----------------------------------------------------------------------
-spec
action(Action   :: enable | disable | restart | reload,
       Protocol :: all | {name, string()} | {pattern, string()}) -> ok |
                                                          {error, term()}.
action(Action, Protocol) ->
  ProtocolStr = case Protocol of
    all                -> "all";
    {name, Name}       -> Name;
    {pattern, Pattern} -> "\"" ++ Pattern ++ "\""
  end,
  Command = format("~w ~s", [Action, ProtocolStr]),
  command(Command).

%% =====================================================================
%% Internal functions
%% =====================================================================
% cat(Things) ->
%   lists:concat(Things).

format(Format, Args) ->
  lists:flatten(io_lib:format(Format, Args)).
