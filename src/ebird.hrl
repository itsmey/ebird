-define(LOG(F, A),
     io:format (standard_error, "* ~s:~w: " ++ F ++ "~n", [?MODULE, ?LINE | A])).
-define(LOG(F), ?LOG(F, [])).

-define(BIRD, "/usr/sbin/bird").
-define(BIRDC, "/usr/sbin/birdc").

-type os_pid() :: integer().
-type extract_pid_error() :: no_process |
                             multiple_processes |
                             access_error.
