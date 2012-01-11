%% @author David Weldon
%% @hidden

-module(riakpool_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                          ignore | {error, Reason::any()}.
init([]) ->
    ConnectionSup =
        {riakpool_connection_sup, {riakpool_connection_sup, start_link, []},
         permanent, 2000, supervisor, [riakpool_connection_sup]},   
    Pool =
        {riakpool, {riakpool, start_link, []},
         permanent, 2000, worker, [riakpool]},
    {ok, {{one_for_all, 5, 30}, [ConnectionSup, Pool]}}.
