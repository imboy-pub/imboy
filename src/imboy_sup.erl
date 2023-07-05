-module(imboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->

    PgConf = config_ds:env(pg_conf),
    pooler:new_pool(PgConf),

    AccountServer = {account_server, {account_server, start_link, []},
                               permanent,
                               infinity,
                               worker,
                               [account_server]},

    UserServer = {user_server, {user_server, start_link, []},
                         permanent,
                         infinity,
                         worker,
                         [user_server]},

    % KVProps default is [{depcache_memory_max, 100}],
    KVProps = config_ds:env(depcache),
    IMBoyCache = {imboy_cache, {imboy_cache, start_link, [KVProps]},
            permanent,
            5000,
            worker, dynamic},
    Restart = {one_for_one, 5, 50},
    {ok, {Restart, [
        IMBoyCache
        , AccountServer
        , UserServer
    ]}}.
