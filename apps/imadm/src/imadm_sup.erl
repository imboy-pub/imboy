-module(imadm_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


%% @doc 启动并链接管理后台监督者
%% 创建并启动管理后台应用的顶层监督者进程
%% @return {ok, Pid} | {ignore, Reason} | {error, Reason} 启动结果
-spec start_link() -> {ok, pid()} | {ignore, any()} | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc 初始化管理后台监督者
%% 设置子进程规范和监督策略
%% @param Args 启动参数（当前为空列表）
%% @return {ok, {SupFlags, ChildSpecs}} 监督者配置
-spec init(list()) -> {ok, {{atom(), non_neg_integer(), pos_integer()}, list()}}.
init([]) ->
    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
