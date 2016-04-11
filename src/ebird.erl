%%----------------------------------------------------------------------
%% @author Ivan Mic <ivan.micr@gmail.com>
%% @doc
%%   Interface module.
%% @end
%%----------------------------------------------------------------------
-module(ebird).

-export([
          command/1,
          show_status/0
        ]).

-type show_ospf_param() :: interface | neighbors | state |
                           tpology   | lsadb.

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
%% @doc Equivalent of "show status" remote control command.
%% @end
%%----------------------------------------------------------------------
-spec
show_status() -> ok | {error, term()}.
show_status() ->
  command("show status").

-spec
show_protocols() -> ok | {error, term()}.
show_protocols() ->
  command("show protocols").

-spec
show_protocols_all() -> ok | {error, term()}.
show_protocols_all() ->
  command("show protocols all").

-spec
show_ospf(Param     :: ospf_show_param(),
          Proto     :: string() | all,
          Interface :: string() | all) -> ok | {error, term()}.
show_ospf(Param, Proto, Interface) ->
  ProtoStr = case Proto of all -> ""; _ -> Proto end,
  InterfaceStr = case Interface of all -> ""; _ -> Interface end,
  Command = case Param of
    lsadb -> format("show ospf ~w ~s", [Param, ProtoStr]);
    _     -> format("show ospf ~w ~s ~"~s~"", [Param, ProtoStr, InterfaceStr]);
  end,
  command(Command).


%% =====================================================================
%% Internal functions
%% =====================================================================
cat(Things) ->
  lists:concat(Things).

format(Format, Args) ->
  lists:flatten(io_lib:format(Format, Args)).
