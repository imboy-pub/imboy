-module(imboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, SqlPoolboy} = application:get_env(imboy, sqlpoolboy),
    SqlName = proplists:get_value(name, SqlPoolboy),
    SqlPoolArgs = proplists:get_value(pool_sql, SqlPoolboy),
    SqlConfArgs = proplists:get_value(sql_conf, SqlPoolboy),
    Mysql = poolboy:child_spec(SqlName, SqlPoolArgs, SqlConfArgs),

    % {ok, RedisPoolboy} = application:get_env(wsyaoxin, redispoolboy),
    % RedisName = proplists:get_value(name, RedisPoolboy),
    % RedisPoolArgs = proplists:get_value(pool_redis, RedisPoolboy),
    % RedisConfArgs = proplists:get_value(redis_conf, RedisPoolboy),
    % Redis = poolboy:child_spec(RedisName, RedisPoolArgs, RedisConfArgs),

    Offline = {account_server, {account_server, start_link, []},
                               permanent,
                               infinity,
                               worker,
                               [account_server]},

    User = {user_server, {user_server, start_link, []},
                         permanent,
                         infinity,
                         worker,
                         [user_server]},
    {ok, {{one_for_one, 5, 60}, [Mysql, Offline, User]}}.
