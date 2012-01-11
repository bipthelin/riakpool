%% @author David Weldon
%% @hidden

-module(riakpool_connection_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                          ignore | {error, Reason::any()}.
init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{connections, {riakc_pb_socket, start_link, []},
            temporary, brutal_kill, worker, [riakc_pb_socket]}]}}.
