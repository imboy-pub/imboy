-module(eb_retention_purge_worker).
-moduledoc "企业留存 bounded purge 定时 worker（R4-②）—— 补全 F-1 孤儿资产清理调度入口。".
-behaviour(gen_server).
%%%===================================================================
%%% @doc 企业留存 bounded purge 定时 worker（R4-②：round 3 登记项②的修复，
%%% 补全 F-1 孤儿资产清理的调度入口）。
%%%
%%% 每个周期对 sys.config **显式列出**的 (org, workspace) 目标各跑一批
%%% `eb_retention_app:purge_batch/2`——消息到期清理与 F-1 孤儿 pending 资产
%%% 清理都随该批次生效。守卫语义（retain_until 未到 / active hold / 非 purge
%%% 角色）全部由 purge 用例端口 + DB 触发器裁决，本模块不做任何例外；
%%% 单目标失败只记日志并继续下一目标（互不拖累）。
%%%
%%% ⚠️ 默认禁用 + 目标空列表 = 什么都不清（避免擅自全库物理清理）。上线需运维
%%% 在 sys.config 显式配置：
%%%   {eb_retention_purge_enabled, true}       %% 默认 false
%%%   {eb_retention_purge_interval_ms, 86400000}  %% 默认每日，F-1 注释口径
%%%   {eb_retention_purge_targets, [
%%%       #{org_id => 501, workspace_id => 601}
%%%   ]}                                       %% 默认 []，示例非真实租户
%%%
%%% 目标是**显式清单**而非 DB 扫描：与 license_notice_worker 的收件人清单同款
%%% 哲学（运维知道要清理哪些租户）。多租户规模增长后如需自动发现，应扩
%%% `eb_purge_port` 具名用例（如 purge_sweep_targets/1），走契约门四件套流程，
%%% 不在本模块直查 SQL。
%%% @end
%%%===================================================================

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% 供测试：扫批决策纯函数（PurgeFun 可注入）
-export([run_sweep/0, run_sweep/1, parse_targets/1]).

-include("log.hrl").

%% 每日一批（F-1 注释的「按天调度的 bounded purge 运维节奏」）；启动后延迟首扫
-define(SWEEP_INTERVAL_MS, 86400000).
-define(INITIAL_DELAY_MS, 60000).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(?INITIAL_DELAY_MS, self(), sweep),
    {ok, #{}}.

handle_info(sweep, State) ->
    run_sweep(),
    erlang:send_after(interval_ms(), self(), sweep),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% @doc 默认扫批：目标来自 sys.config，批次经应用层唯一入口 purge_batch/2。
-spec run_sweep() -> {ok, map()}.
run_sweep() ->
    run_sweep(fun(OrgId, WsId) -> eb_retention_app:purge_batch(OrgId, #{workspace_id => WsId}) end).

%% @doc 扫批核心（PurgeFun 注入供测试）。未启用 / 无有效目标 = 零调用 no-op。
%% 返回摘要供日志与测试断言：swept/ok/failed/invalid_targets。
-spec run_sweep(fun((integer(), integer()) -> {ok, map()} | {error, term()})) -> {ok, map()}.
run_sweep(PurgeFun) when is_function(PurgeFun, 2) ->
    {Targets, Invalid} = parse_targets(application:get_env(imboy, eb_retention_purge_targets, [])),
    Enabled = enabled(),
    case Enabled andalso Targets =/= [] of
        false ->
            {ok, #{swept => 0, ok => 0, failed => 0, invalid_targets => Invalid}};
        true ->
            Summary = do_sweep(PurgeFun, Targets, 0, 0, 0),
            {ok, Summary#{invalid_targets => Invalid}}
    end.

do_sweep(_PurgeFun, [], Swept, Ok, Failed) ->
    #{swept => Swept, ok => Ok, failed => Failed};
do_sweep(PurgeFun, [{OrgId, WsId} | Rest], Swept, Ok, Failed) ->
    case PurgeFun(OrgId, WsId) of
        {ok, Summary} ->
            ?INFO_LOG("[eb_retention_purge] org=~p ws=~p purge_batch ok: ~p", [OrgId, WsId, Summary]),
            do_sweep(PurgeFun, Rest, Swept + 1, Ok + 1, Failed);
        {error, Reason} ->
            ?WARN_LOG("[eb_retention_purge] org=~p ws=~p purge_batch failed: ~p", [
                OrgId, WsId, Reason
            ]),
            do_sweep(PurgeFun, Rest, Swept + 1, Ok, Failed + 1)
    end.

%% @doc 目标清单解析（纯函数）：合法条目为 `#{org_id => 整数, workspace_id =>
%% 整数}`；非法条目不计入扫批、仅计数（fail-closed 单条，不拖垮整批）。
-spec parse_targets(term()) -> {[{integer(), integer()}], non_neg_integer()}.
parse_targets(Env) when is_list(Env) ->
    lists:foldl(
        fun(T, {Acc, Bad}) ->
            case target_pair(T) of
                {ok, Pair} -> {[Pair | Acc], Bad};
                error -> {Acc, Bad + 1}
            end
        end,
        {[], 0},
        Env
    );
parse_targets(_Other) ->
    {[], 1}.

target_pair(#{org_id := OrgId, workspace_id := WsId}) when
    is_integer(OrgId), OrgId > 0, is_integer(WsId), WsId > 0
->
    {ok, {OrgId, WsId}};
target_pair(_Other) ->
    error.

-spec enabled() -> boolean().
enabled() ->
    application:get_env(imboy, eb_retention_purge_enabled, false).

-spec interval_ms() -> pos_integer().
interval_ms() ->
    application:get_env(imboy, eb_retention_purge_interval_ms, ?SWEEP_INTERVAL_MS).
