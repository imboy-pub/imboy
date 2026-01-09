-module(msg_store_sup).
%%%
% msg_store_sup 是消息写入队列的监督树
% 管理 msg_store_ds 和 msg_store_worker 两个子进程
%
% 监督策略：one_for_one
% - 任意子进程崩溃时独立重启
% - 最多 10 次/60 秒
%%%

-behaviour(supervisor).

%% ==================== API ====================

-export([start_link/0]).

%% ==================== Callbacks ====================

-export([init/1]).

-include("log.hrl").

%% ==================== API Functions ====================

%% @doc 启动监督树
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ==================== Callbacks ====================

%% @private
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        % msg_store_ds: 队列管理服务
        #{
            id => msg_store_ds,
            start => {msg_store_ds, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [msg_store_ds]
        },
        % msg_store_worker: 批量写入 worker
        #{
            id => msg_store_worker,
            start => {msg_store_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [msg_store_worker]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
