-module(imboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, SqlPool} = application:get_env(imboy, sqlPool),
    Name = proplists:get_value(name, SqlPool),
    PoolArgs = proplists:get_value(poolConf, SqlPool),
    WorkerArgs = proplists:get_value(sqlConf, SqlPool),
    Mysql = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    {ok, {{one_for_one, 5, 60}, [Mysql]}}.
