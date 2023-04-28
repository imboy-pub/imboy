-module(imboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SqlPoolboy= imboy_func:env(sqlpoolboy),
    SqlName = proplists:get_value(name, SqlPoolboy),
    SqlPoolArgs = proplists:get_value(pool_sql, SqlPoolboy),
    SqlConfArgs = proplists:get_value(sql_conf, SqlPoolboy),
    Mysql = poolboy:child_spec(SqlName, SqlPoolArgs, SqlConfArgs),

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

    % KVProps default is [{depcache_memory_max, 100}],
    KVProps = imboy_func:env(depcache),
    IMBoyKV = {imboy_cache, {imboy_cache, start_link, [KVProps]},
            permanent,
            5000,
            worker, dynamic},
    {ok, {{one_for_one, 5, 60}, [Mysql, Offline, User, IMBoyKV]}}.
