-module(imboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    % application:ensure_all_started(pgo),
    % {PoolName, PoolConfig} = config_ds:env(pgo),
    % PgoChildSpec = #{
    %     id => pgo_pool
    %     , start => {pgo_pool, start_link, [PoolName, PoolConfig]}
    %     , shutdown => 1000
    % },

    PgConf = config_ds:env(pg_conf),
    pooler:new_pool(PgConf),

    % https://blog.csdn.net/Dylan_2018/article/details/110150142
    % child_spec() = #{id => child_id(),             % mandatory
    %     start => mfargs(),            % mandatory
    %     restart => restart(),         % optional
    %     significant => significant(), % boolean() optional
    %     shutdown => shutdown(),       % brutal_kill | timeout() optional 当子进程为supervisor进程时，应该设置成infinity。默认为50000;
    %     type => worker(),             % optional
    %     modules => modules()}         % optional
    AccountServer = #{
        id => account_server
        , start => {account_server, start_link, []}
        , restart => permanent
        , shutdown => infinity
        , type => worker
        , modules => [account_server]
    },

    UserServer = #{
        id => user_server
        , start => {user_server, start_link, []}
        , restart => permanent
        , shutdown => infinity
        , type => worker
        , modules => [user_server]
    },

    % KVProps default is [{depcache_memory_max, 100}],
    KVProps = config_ds:env(depcache),
    IMBoyCache = #{
        id => imboy_cache
        ,  start => {imboy_cache, start_link, [KVProps]}
        , restart => permanent
        , shutdown => 5000
        , type => worker
        , modules => dynamic
    },
    Specs = [
        IMBoyCache
        % , PgoChildSpec
        , AccountServer
        , UserServer
    ],
    Restart = #{strategy => one_for_one, intensity => 5, period => 50},
    {ok, {Restart, Specs}}.
