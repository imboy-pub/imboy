%%% @doc Agent Recovery and Concurrency（AG31-09；架构 §9.5/§14 恢复语义）。
%%%
%%% 职责（卡面冻结）：
%%%   * **单 owner 恢复**：`recover_run/5` 经 `agent_run_pg:lease_take_over/5`
%%%     数据库 CAS 抢占（WHERE status='running' AND lease 过期）——并发恢复
%%%     恰一胜者（A11：winner_count=1，duplicate_effect=0）；败者零动作。
%%%   * **每 Effect reauthorize**：胜者对 run 下未决 effect（authorized）
%%%     逐条 `recheck_effect/4` **重走 authorize/3 全链**（实时重读 Grant/
%%%     membership/org）——Grant 已撤销 → {deny, grant_revoked}（A04）；
%%%     membership 已 suspend → {deny, *_denied}（A05）。**重派本身不在
%%%     recovery 内**：recheck 返回 allow 的 effect 才允许执行器重派，且
%%%     同幂等键重派受 effect dedup 保护（A09：external attempt ≤1）。
%%%   * **unknown 不自动重发**（A09/§14）：dispatching→unknown 只经
%%%     `mark_unknown/3` 显式登记；unknown→succeeded/failed 只经
%%%     `reconcile_effect/4` 显式确认（effect FSM 边表冻结）；recovery
%%%     对 unknown/dispatching 态 effect **零自动出边**。
%%%   * **取消**：`cancel_run/3` 复用 04B 冻结迁移（E10 running→cancelled，
%%%     human actor）。
%%%
%%% 副作用纪律：本模块不触外部系统、不写 Tool 调用；一切 ledger 变更走
%%% agent_run_pg 既有 CAS API；无盲重试（attempt 上限由 04B
%%% assert_attempt_below_cap 钉死）。
-module(agent_recovery).

-moduledoc "Agent 恢复与并发语义（AG31-09，架构 §9.5/§14）。".
-export([
    recover_run/5,
    recheck_effect/4,
    mark_unknown/3,
    reconcile_effect/5,
    cancel_run/3
]).

%% @doc 抢占过期 lease 并产出恢复计划：
%% ```
%% recover_run(Conn, RunId, NewOwner, LeaseSeconds, #{now => ...})
%%   -> {ok, #{owner, recheck => [EffectId]}}   %% 胜者：待重查的 authorized effects
%%    | {error, lease_not_acquired}             %% 败者：零动作（A11）
%%    | {error, run_not_running}
%% '''
recover_run(Conn, RunId, NewOwner, LeaseSeconds, Ctx) ->
    Now = maps:get(now, Ctx),
    ExpiresAt = shift(Now, LeaseSeconds),
    case agent_run_pg:lease_take_over(Conn, RunId, NewOwner, ExpiresAt, Now) of
        {ok, _Lease} ->
            Pending = [
                maps:get(id, Effect)
             || Effect <- agent_run_pg:list_run_effects(Conn, RunId),
                maps:get(status, Effect) =:= authorized
            ],
            {ok, #{owner => NewOwner, recheck => Pending}};
        {error, lease_not_acquired} = E ->
            E
    end.

%% @doc 每 Effect reauthorize：**重走 authorize/3 全链**（零缓存、实时事实）。
%% ```
%% recheck_effect(RunCtx, ToolDescriptor, ResourceContext, EffectId)
%%   -> {allow, DecisionContext}     %% 允许执行器重派（dedup 仍兜底）
%%    | {deny, ReasonCode}           %% A04/A05：失效事实即拒
%%    | {approval_required, _} | {error, _}
%% '''
%% RunCtx 需携带 approval_effect_id => EffectId（authorize 第 8 步消费既有
%% 批准绑定；无绑定且需审批 → approval_required，不自动放行）。
recheck_effect(RunCtx, ToolDescriptor, ResourceContext, EffectId) ->
    Ctx = RunCtx#{approval_effect_id => EffectId},
    agent_tool_authorizer:authorize(Ctx, ToolDescriptor, ResourceContext).

%% @doc dispatching → unknown（显式登记；§14 unknown 语义——外部结果不明，
%% 禁止自动重发）。经 agent_run_pg:effect_to_tx CAS（含 FSM 边校验）。
mark_unknown(Conn, EffectId, Ctx) ->
    effect_transition(Conn, EffectId, dispatching, unknown, Ctx).

%% @doc unknown → succeeded | failed（人工/运维 reconcile 确认面；effect FSM
%% 冻结出边；unknown 永不经自动路径出边——A09 unknown not retried）。
%% Outcome 仅接受 succeeded | failed。
reconcile_effect(Conn, EffectId, Outcome, ExpectedVersion, Ctx) when
    Outcome =:= succeeded; Outcome =:= failed
->
    effect_transition(Conn, EffectId, unknown, Outcome, ExpectedVersion, Ctx);
reconcile_effect(_Conn, _EffectId, _BadOutcome, _V, _Ctx) ->
    {error, invalid_outcome}.

%% @doc running → cancelled（E10 冻结边，复用 agent_run_command:transition）。
cancel_run(Conn, RunId, Ctx) ->
    agent_run_command:transition(
        Conn,
        RunId,
        running,
        cancelled,
        #{
            expected_version => maps:get(expected_version, Ctx),
            now => maps:get(now, Ctx),
            actor_id => maps:get(actor_id, Ctx, <<"system:recovery">>)
        }
    ).

%% ===================================================================
%% 内部
%% ===================================================================

effect_transition(Conn, EffectId, From, To, Ctx) ->
    effect_transition(Conn, EffectId, From, To, maps:get(expected_version, Ctx, 1), Ctx).

effect_transition(Conn, EffectId, From, To, ExpectedVersion, Ctx) ->
    Now = maps:get(now, Ctx),
    case agent_run_pg:effect_to_tx(Conn, EffectId, From, To, ExpectedVersion, Now, #{}) of
        {ok, _Version} = Ok -> Ok;
        {error, cas_conflict} -> {error, cas_conflict};
        {error, illegal_transition} = E -> E;
        {rollback, Reason} -> {error, {effect_rollback, Reason}}
    end.

shift({_, {_, _, _}} = Now, Seconds) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + Seconds
    );
shift(_Bad, _S) ->
    erlang:error(bad_now).
