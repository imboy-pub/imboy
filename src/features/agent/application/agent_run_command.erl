%%% @doc AgentRun 应用层用例（AG31-04B；架构合同 §9.2/§9.3/§10.3/§14）。
%%%
%%% 职责边界（镜像 cs_session_app 的分层角色）：
%%%   * FSM 语义唯一真源是 domain `agent_run_fsm`；本模块做参数收敛 →
%%%     domain 判定 → 经 infrastructure `agent_run_pg` 的 CAS 用例写库。
%%%   * **CAS 迁移**：每条边 UPDATE...WHERE id AND status AND version，
%%%     与 agent_run_event 同事务（§9.3 L456-457）。
%%%   * **duplicate trigger 幂等**（§14 A08）：同五元组返回既有 run_id，无第二次执行。
%%%   * **effect 记账**：authorized→dispatching 必须在调用 adapter 前持久化（§10.3 L557）；
%%%     adapter 由调用方注入（本期=mock；不执行 Tool/Hirð）。
%%%   * **reconcile**（AG-A0 裁决 CS-1=内部 API）：unknown→succeeded|failed 仅此二边。
%%%   * **Grant 实时重检**：authorize/approve 均读 00000132 agent_grant 实时状态
%%%     （grant_version_at_start 只是审计快照，不用于放行，§9.2）。
%%%   * timeout 非状态：映射 failed + reason_code=timeout（裁决 CS-3）；
%%%     attempt 上限=agent_run_fsm:max_attempts()（裁决 CS-6）。
%%%   * 本模块为内部应用 API；start_run/cancel_run 等公开 facade（§12.1）不在本期 scope。
-module(agent_run_command).

-export([
    create_run/2,
    transition/5,
    cancel_run/4,
    acquire_lease/5,
    renew_lease/6,
    take_over_lease/5,
    authorize_effect/3,
    approve_effect/4,
    reject_effect/4,
    dispatch_effect/5,
    reconcile_run/4,
    reconcile_effect/5,
    get_run/2
]).

-type conn() :: pid().

-define(TRIGGER_TYPES, [message, schedule, webhook]).
-define(RUNTIME_TYPES, [mock, hird]).

%% ===================================================================
%% Run 创建（含 duplicate trigger 幂等，§14 A08）
%% ===================================================================

%% @doc 创建 Run：immutable context 落库为 created 态 + 创建事件同事务；
%% 同 (agent_id, organization_id, trigger_type, trigger_id, idempotency_key)
%% 重复触发返回既有 run_id（duplicated=true，execution=1）。
%%
%% Ctx 必备键：agent_id/organization_id/grant_id/delegating_principal_id（正整数）、
%% grant_version_at_start（>=1）、workspace_id（可空整数）、
%% trigger_type（message|schedule|webhook）、trigger_id/runtime_type（mock|hird）、
%% context_digest/idempotency_key（非空 binary）、now；可选：id、actor_id。
-spec create_run(conn(), map()) ->
    {ok, #{run_id := pos_integer(), version := pos_integer(), duplicated := boolean()}}
    | {error, validation_failed | {duplicate_effect, binary()} | term()}.
create_run(Conn, Ctx) ->
    case validate_run_ctx(Ctx) of
        ok ->
            RunId =
                case maps:get(id, Ctx, undefined) of
                    undefined -> agent_run_pg:next_id(agent_run);
                    Id -> Id
                end,
            Now = maps:get(now, Ctx),
            ActorId = maps:get(actor_id, Ctx, <<"system:trigger">>),
            IdemKey = maps:get(idempotency_key, Ctx),
            case
                agent_run_pg:find_run_by_trigger(
                    Conn,
                    maps:get(agent_id, Ctx),
                    maps:get(organization_id, Ctx),
                    maps:get(trigger_type, Ctx),
                    maps:get(trigger_id, Ctx),
                    IdemKey
                )
            of
                {ok, Run} ->
                    {ok, #{
                        run_id => maps:get(id, Run),
                        version => maps:get(version, Run),
                        duplicated => true
                    }};
                {error, not_found} ->
                    Run = #{
                        id => RunId,
                        agent_id => maps:get(agent_id, Ctx),
                        organization_id => maps:get(organization_id, Ctx),
                        workspace_id => maps:get(workspace_id, Ctx, undefined),
                        grant_id => maps:get(grant_id, Ctx),
                        grant_version_at_start => maps:get(grant_version_at_start, Ctx),
                        delegating_principal_id => maps:get(delegating_principal_id, Ctx),
                        trigger_type => maps:get(trigger_type, Ctx),
                        trigger_id => maps:get(trigger_id, Ctx),
                        runtime_type => maps:get(runtime_type, Ctx),
                        context_digest => maps:get(context_digest, Ctx),
                        idempotency_key => IdemKey,
                        now => Now
                    },
                    CreationEvent = #{
                        id => agent_run_pg:next_id(agent_run_event),
                        run_id => RunId,
                        from_status => undefined,
                        to_status => created,
                        reason_code => undefined,
                        actor_kind => system,
                        actor_id => ActorId,
                        detail => #{},
                        idempotency_key => <<IdemKey/binary, ":created">>,
                        now => Now
                    },
                    case agent_run_pg:insert_run_tx(Conn, Run, CreationEvent) of
                        {ok, RunId} -> {ok, #{run_id => RunId, version => 1, duplicated => false}};
                        {rollback, Reason} -> {error, {insert_rollback, Reason}}
                    end
            end;
        {error, _} = Err ->
            Err
    end.

%% ===================================================================
%% CAS 迁移 / cancel（E01-E16）
%% ===================================================================

%% @doc 通用 CAS 迁移：边必须为 §9.3 十六合法边之一，否则 illegal_transition；
%% actor_kind 缺省取 per-edge 默认（裁决 CS-5）。
%%
%% Opts 必备：expected_version/now/actor_id；可选：reason_code/actor_kind/detail。
-spec transition(conn(), pos_integer(), atom(), atom(), map()) ->
    {ok, pos_integer()}
    | {error, illegal_transition | cas_conflict | invalid_actor_kind | not_found}.
transition(Conn, RunId, From, To, Opts) ->
    case agent_run_fsm:edge_for(From, To) of
        {ok, EdgeId, DefaultActor} ->
            case agent_run_pg:get_run(Conn, RunId) of
                {ok, Run} ->
                    case maps:get(status, Run) of
                        From ->
                            ActorKind = maps:get(actor_kind, Opts, DefaultActor),
                            ok = agent_run_fsm:assert_actor_kind(ActorKind),
                            Event = transition_event(
                                Run,
                                From,
                                To,
                                EdgeId,
                                maps:get(expected_version, Opts),
                                maps:get(reason_code, Opts, undefined),
                                ActorKind,
                                maps:get(actor_id, Opts),
                                maps:get(now, Opts),
                                maps:get(detail, Opts, #{})
                            ),
                            case
                                agent_run_pg:cas_transition_tx(
                                    Conn,
                                    RunId,
                                    From,
                                    To,
                                    maps:get(expected_version, Opts),
                                    maps:get(now, Opts),
                                    maps:get(reason_code, Opts, undefined),
                                    Event
                                )
                            of
                                {ok, V} -> {ok, V};
                                {error, cas_conflict} -> {error, cas_conflict};
                                {rollback, Reason} -> {error, {transition_rollback, Reason}}
                            end;
                        _OtherStatus ->
                            {error, cas_conflict}
                    end;
                {error, not_found} ->
                    {error, not_found}
            end;
        {error, illegal_transition} ->
            {error, illegal_transition}
    end.

%% @doc cancel：created/queued/running/waiting_approval → cancelled（E03/E06/E10/E14，
%% actor=human）；unknown/终态拒绝（unknown 无 cancelled 出口，§9.3 L450）。
-spec cancel_run(conn(), pos_integer(), binary(), map()) ->
    {ok, pos_integer()} | {error, cancel_rejected | not_found | cas_conflict | term()}.
cancel_run(Conn, RunId, ActorId, Opts) ->
    case agent_run_pg:get_run(Conn, RunId) of
        {ok, Run} ->
            Status = maps:get(status, Run),
            case
                lists:member(Status, [created, queued, running, waiting_approval]) andalso
                    agent_run_fsm:transition_allowed(Status, cancelled)
            of
                true ->
                    transition(Conn, RunId, Status, cancelled, Opts#{
                        actor_kind => human,
                        actor_id => ActorId,
                        reason_code => cancelled
                    });
                false ->
                    {error, cancel_rejected}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% ===================================================================
%% lease（获取/续租/过期接管；DB 条件更新为唯一真源）
%% ===================================================================

%% @doc queued→running（E04）：attempt+1；attempt 达上限（裁决 CS-6=3）则
%% 驱动 queued→failed(reason=max_attempts_exceeded) 并拒绝。
%% Opts 必备：now；可选：actor_id（默认 WorkerId）。
-spec acquire_lease(conn(), pos_integer(), binary(), pos_integer(), map()) ->
    {ok, #{version := pos_integer(), attempt := non_neg_integer()}}
    | {error, lease_not_acquired | run_not_queued | max_attempts_exceeded | not_found | term()}.
acquire_lease(Conn, RunId, WorkerId, LeaseSeconds, Opts) ->
    case agent_run_pg:get_run(Conn, RunId) of
        {ok, Run} ->
            case maps:get(status, Run) of
                queued ->
                    Now = maps:get(now, Opts),
                    case agent_run_fsm:assert_attempt_below_cap(maps:get(attempt, Run)) of
                        ok ->
                            Event = transition_event(
                                Run,
                                queued,
                                running,
                                e04,
                                maps:get(version, Run),
                                undefined,
                                system,
                                maps:get(actor_id, Opts, WorkerId),
                                Now,
                                #{lease_owner => WorkerId}
                            ),
                            case
                                agent_run_pg:lease_acquire_tx(
                                    Conn,
                                    RunId,
                                    maps:get(version, Run),
                                    WorkerId,
                                    shift_seconds(Now, LeaseSeconds),
                                    Now,
                                    Event
                                )
                            of
                                {ok, Res} -> {ok, Res};
                                {error, lease_not_acquired} -> {error, lease_not_acquired};
                                {rollback, Reason} -> {error, {lease_rollback, Reason}}
                            end;
                        {error, max_attempts_exceeded} ->
                            _ = transition(Conn, RunId, queued, failed, Opts#{
                                expected_version => maps:get(version, Run),
                                actor_kind => system,
                                actor_id => maps:get(actor_id, Opts, <<"system:lease">>),
                                reason_code => max_attempts_exceeded
                            }),
                            {error, max_attempts_exceeded}
                    end;
                _Other ->
                    {error, run_not_queued}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 续租：仅 lease_owner 本人可续（DB 条件更新）。
-spec renew_lease(conn(), pos_integer(), binary(), pos_integer(), pos_integer(), map()) ->
    {ok, pos_integer()} | {error, lease_not_acquired}.
renew_lease(Conn, RunId, WorkerId, LeaseSeconds, ExpectedVersion, Opts) ->
    Now = maps:get(now, Opts),
    agent_run_pg:lease_renew(
        Conn, RunId, WorkerId, shift_seconds(Now, LeaseSeconds), Now, ExpectedVersion
    ).

%% @doc 过期 lease 接管：同态字段更新（非 FSM 边）；并发竞争恰一 winner（A11）。
-spec take_over_lease(conn(), pos_integer(), binary(), pos_integer(), map()) ->
    {ok, #{version := pos_integer(), attempt := non_neg_integer()}}
    | {error, lease_not_acquired | max_attempts_exceeded | not_found}.
take_over_lease(Conn, RunId, WorkerId, LeaseSeconds, Opts) ->
    case agent_run_pg:get_run(Conn, RunId) of
        {ok, Run} ->
            case agent_run_fsm:assert_attempt_below_cap(maps:get(attempt, Run)) of
                ok ->
                    Now = maps:get(now, Opts),
                    agent_run_pg:lease_take_over(
                        Conn, RunId, WorkerId, shift_seconds(Now, LeaseSeconds), Now
                    );
                {error, max_attempts_exceeded} ->
                    {error, max_attempts_exceeded}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% ===================================================================
%% Effect 记账（authorize / approve / reject）
%% ===================================================================

%% @doc 授权记账（§10.2 第 10 步 persist decision before dispatch）：
%%   1. Run 门禁：仅 running 可新增 Effect；unknown 禁止（§9.3 L450）；
%%   2. Grant 实时重检（00000132 agent_grant 实时状态，§9.2 快照不作放行依据）；
%%   3. Ctx.decision（allow|approval_required|deny，来自上层 authorizer 的裁决输入）
%%      与 Grant 检查合并：Grant 异常一律压为 deny（§14 deny by default）；
%%   4. effect INSERT(created) + CAS created→decided 同事务；
%%      approval_required 时同事务驱动 Run E07 running→waiting_approval。
-spec authorize_effect(conn(), pos_integer(), map()) ->
    {ok, #{effect_id := pos_integer(), status := atom()}}
    | {error, term()}.
authorize_effect(Conn, RunId, Ctx) ->
    Now = maps:get(now, Ctx),
    case agent_run_pg:get_run(Conn, RunId) of
        {ok, Run} ->
            case agent_run_fsm:new_effect_gate(maps:get(status, Run)) of
                ok ->
                    case grant_check(Conn, maps:get(grant_id, Run), Now) of
                        {allow, Grant} ->
                            decide_effect(Conn, Run, Ctx, maps:get(version, Grant), Now);
                        {deny, Reason} ->
                            decide_deny(Conn, Run, Ctx, Reason, Now)
                    end;
                {error, GateReason} ->
                    {error, GateReason}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 批准（human，CS-5）：digest 不匹配拒绝（§14）；批准不能覆盖已撤销 Grant
%% （§10.3）——Grant 异常时 effect→denied 且 Run E13 waiting_approval→failed；
%% 正常路径 effect→authorized + Run E12 waiting_approval→queued，同事务。
-spec approve_effect(conn(), pos_integer(), pos_integer(), map()) ->
    {ok, #{effect_version := pos_integer(), run_version := pos_integer() | undefined}}
    | {error, term()}.
approve_effect(Conn, RunId, EffectId, Ctx) ->
    Now = maps:get(now, Ctx),
    case agent_run_pg:get_effect(Conn, EffectId) of
        {ok, Effect} ->
            case maps:get(status, Effect) of
                waiting_approval ->
                    approve_waiting(Conn, RunId, EffectId, Effect, Ctx, Now);
                _Other ->
                    {error, effect_not_waiting_approval}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

approve_waiting(Conn, RunId, EffectId, Effect, Ctx, Now) ->
    ArgsDigest = maps:get(args_digest, Ctx, undefined),
    case ArgsDigest =:= maps:get(args_digest, Effect) of
        false ->
            {error, approval_digest_mismatch};
        true ->
            case agent_run_pg:get_run(Conn, RunId) of
                {ok, Run} ->
                    RunV = maps:get(version, Run),
                    ActorId = maps:get(actor_id, Ctx),
                    case grant_check(Conn, maps:get(grant_id, Run), Now) of
                        {allow, Grant} ->
                            Event = transition_event(
                                Run,
                                waiting_approval,
                                queued,
                                e12,
                                RunV,
                                approved,
                                human,
                                ActorId,
                                Now,
                                #{}
                            ),
                            case
                                agent_run_pg:combined_effect_run_tx(
                                    Conn,
                                    Now,
                                    EffectId,
                                    waiting_approval,
                                    authorized,
                                    maps:get(version, Effect),
                                    #{
                                        approval_ref => maps:get(approval_ref, Ctx),
                                        authorization_reason => <<"approved">>,
                                        grant_version_checked => maps:get(version, Grant)
                                    },
                                    {cas_run, RunId, waiting_approval, queued, RunV, approved,
                                        Event}
                                )
                            of
                                {ok, EV, RV} ->
                                    {ok, #{effect_version => EV, run_version => RV}};
                                {error, _} = Err ->
                                    Err;
                                {rollback, R} ->
                                    {error, {tx_rollback, R}}
                            end;
                        {deny, Reason} ->
                            %% 批准不能覆盖已撤销 Grant/暂停/归档（§10.3 L556）
                            Event = transition_event(
                                Run,
                                waiting_approval,
                                failed,
                                e13,
                                RunV,
                                Reason,
                                system,
                                <<"system:grant-recheck">>,
                                Now,
                                #{}
                            ),
                            case
                                agent_run_pg:combined_effect_run_tx(
                                    Conn,
                                    Now,
                                    EffectId,
                                    waiting_approval,
                                    denied,
                                    maps:get(version, Effect),
                                    #{authorization_reason => reason_to_b(Reason)},
                                    {cas_run, RunId, waiting_approval, failed, RunV, Reason, Event}
                                )
                            of
                                {ok, _, _} -> {error, Reason};
                                {error, _} = Err -> Err;
                                {rollback, R} -> {error, {tx_rollback, R}}
                            end
                    end;
                {error, not_found} ->
                    {error, not_found}
            end
    end.

%% @doc 拒绝（human，CS-5）：effect→denied + Run E13 waiting_approval→failed，同事务。
-spec reject_effect(conn(), pos_integer(), pos_integer(), map()) ->
    {ok, #{effect_version := pos_integer(), run_version := pos_integer()}}
    | {error, term()}.
reject_effect(Conn, RunId, EffectId, Ctx) ->
    Now = maps:get(now, Ctx),
    Reason = maps:get(reason_code, Ctx, approval_rejected),
    case {agent_run_pg:get_effect(Conn, EffectId), agent_run_pg:get_run(Conn, RunId)} of
        {{error, not_found}, _} ->
            {error, effect_not_found};
        {_, {error, not_found}} ->
            {error, not_found};
        {{ok, Effect}, {ok, Run}} ->
            case {maps:get(status, Effect), maps:get(status, Run)} of
                {waiting_approval, waiting_approval} ->
                    RunV = maps:get(version, Run),
                    Event = transition_event(
                        Run,
                        waiting_approval,
                        failed,
                        e13,
                        RunV,
                        Reason,
                        human,
                        maps:get(actor_id, Ctx),
                        Now,
                        #{}
                    ),
                    case
                        agent_run_pg:combined_effect_run_tx(
                            Conn,
                            Now,
                            EffectId,
                            waiting_approval,
                            denied,
                            maps:get(version, Effect),
                            #{authorization_reason => reason_to_b(Reason)},
                            {cas_run, RunId, waiting_approval, failed, RunV, Reason, Event}
                        )
                    of
                        {ok, EV, RV} -> {ok, #{effect_version => EV, run_version => RV}};
                        Other -> map_tx_result(Other)
                    end;
                _Other ->
                    {error, effect_not_waiting_approval}
            end
    end.

%% ===================================================================
%% dispatch（authorized→dispatching 先持久化，再调用注入 adapter）
%% ===================================================================

%% @doc dispatch 用例（§10.3 L557）：effect CAS authorized→dispatching **先持久化**，
%% 然后调用 Adapter（本期=mock 注入；不执行 Tool/Hirð）。Adapter 结果：
%%   {ok, ResultDigest} → succeeded；{error, FailureCode} → failed；
%%   unknown / crash → unknown（禁止自动重发，转 reconcile 或 HITL，§10.3 L559-560）。
-spec dispatch_effect(conn(), pos_integer(), pos_integer(), fun((map()) -> term()), map()) ->
    {ok, #{status := atom(), effect_version := pos_integer()}}
    | {error, term()}.
dispatch_effect(Conn, RunId, EffectId, Adapter, Opts) ->
    Now = maps:get(now, Opts),
    case {agent_run_pg:get_effect(Conn, EffectId), agent_run_pg:get_run(Conn, RunId)} of
        {{error, not_found}, _} ->
            {error, effect_not_found};
        {_, {error, not_found}} ->
            {error, not_found};
        {{ok, Effect}, {ok, Run}} ->
            case {maps:get(status, Effect), maps:get(status, Run)} of
                {authorized, running} ->
                    case
                        agent_run_pg:effect_to_tx(
                            Conn,
                            EffectId,
                            authorized,
                            dispatching,
                            maps:get(version, Effect),
                            Now,
                            #{}
                        )
                    of
                        {ok, _V} ->
                            AdapterInput = #{
                                effect_id => EffectId,
                                run_id => RunId,
                                tool_id => maps:get(tool_id, Effect),
                                external_idempotency_key =>
                                    maps:get(external_idempotency_key, Effect),
                                args_digest => maps:get(args_digest, Effect)
                            },
                            settle_dispatch(Conn, EffectId, Adapter, AdapterInput, Now);
                        {error, cas_conflict} ->
                            {error, effect_not_dispatchable};
                        Other ->
                            map_tx_result(Other)
                    end;
                _Other ->
                    {error, effect_not_dispatchable}
            end
    end.

settle_dispatch(Conn, EffectId, Adapter, Input, Now) ->
    {ok, Effect} = agent_run_pg:get_effect(Conn, EffectId),
    V = maps:get(version, Effect),
    try Adapter(Input) of
        {ok, ResultDigest} ->
            finish_dispatch(Conn, EffectId, dispatching, succeeded, V, Now, #{
                result_digest => ResultDigest
            });
        {error, FailureCode} ->
            finish_dispatch(Conn, EffectId, dispatching, failed, V, Now, #{
                failure_code => FailureCode
            });
        unknown ->
            finish_dispatch(Conn, EffectId, dispatching, unknown, V, Now, #{
                failure_code => <<"indeterminate">>
            })
    catch
        _Class:_Reason ->
            %% dispatching 后 crash/timeout 且结果不可知 → 必须写 unknown，禁止自动重发
            finish_dispatch(Conn, EffectId, dispatching, unknown, V, Now, #{
                failure_code => <<"adapter_crash">>
            })
    end.

finish_dispatch(Conn, EffectId, From, To, ExpectedVersion, Now, Extras) ->
    case agent_run_pg:effect_to_tx(Conn, EffectId, From, To, ExpectedVersion, Now, Extras) of
        {ok, V} ->
            {ok, #{status => To, effect_version => V}};
        {error, cas_conflict} ->
            {error, cas_conflict};
        Other ->
            map_tx_result(Other)
    end.

%% ===================================================================
%% reconcile（内部 API，裁决 CS-1；unknown→succeeded|failed 仅此二边）
%% ===================================================================

%% @doc Run reconcile：unknown → succeeded|failed（E15/E16，actor=system）。
%% Outcome 其他值或 Run 非 unknown 一律拒绝；无自动转终态（§10.3 L559-560）。
-spec reconcile_run(conn(), pos_integer(), succeeded | failed, map()) ->
    {ok, pos_integer()} | {error, invalid_outcome | reconcile_rejected | not_found | term()}.
reconcile_run(Conn, RunId, Outcome, Opts) when Outcome =:= succeeded; Outcome =:= failed ->
    case agent_run_pg:get_run(Conn, RunId) of
        {ok, Run} ->
            case maps:get(status, Run) of
                unknown ->
                    transition(Conn, RunId, unknown, Outcome, Opts#{
                        actor_kind => system,
                        actor_id => maps:get(actor_id, Opts, <<"system:reconcile">>),
                        reason_code => reconciled
                    });
                _Other ->
                    {error, reconcile_rejected}
            end;
        {error, not_found} ->
            {error, not_found}
    end;
reconcile_run(_Conn, _RunId, _Outcome, _Opts) ->
    {error, invalid_outcome}.

%% @doc Effect reconcile：unknown → succeeded(result_digest)|failed(failure_code)，
%% 仅显式调用（§10.3 "以查询/返回结果收敛 ledger"）。
-spec reconcile_effect(conn(), pos_integer(), succeeded | failed, binary(), map()) ->
    {ok, pos_integer()} | {error, invalid_outcome | reconcile_rejected | not_found | term()}.
reconcile_effect(Conn, EffectId, Outcome, Detail, Opts) when
    Outcome =:= succeeded; Outcome =:= failed
->
    case agent_run_pg:get_effect(Conn, EffectId) of
        {ok, Effect} ->
            case maps:get(status, Effect) of
                unknown ->
                    Extras =
                        case Outcome of
                            succeeded -> #{result_digest => Detail};
                            failed -> #{failure_code => Detail}
                        end,
                    case
                        agent_run_pg:effect_to_tx(
                            Conn,
                            EffectId,
                            unknown,
                            Outcome,
                            maps:get(version, Effect),
                            maps:get(now, Opts),
                            Extras
                        )
                    of
                        {ok, V} -> {ok, V};
                        Other -> map_tx_result(Other)
                    end;
                _Other ->
                    {error, reconcile_rejected}
            end;
        {error, not_found} ->
            {error, not_found}
    end;
reconcile_effect(_Conn, _EffectId, _Outcome, _Detail, _Opts) ->
    {error, invalid_outcome}.

%% ===================================================================
%% 读
%% ===================================================================

-spec get_run(conn(), pos_integer()) -> {ok, map()} | {error, not_found}.
get_run(Conn, RunId) ->
    agent_run_pg:get_run(Conn, RunId).

%% ===================================================================
%% 内部：context 校验 / Grant 实时重检 / effect 记账
%% ===================================================================

validate_run_ctx(Ctx) ->
    Checks = [
        validate_enum(trigger_type, Ctx, ?TRIGGER_TYPES),
        validate_enum(runtime_type, Ctx, ?RUNTIME_TYPES),
        validate_pos_int(agent_id, Ctx),
        validate_pos_int(organization_id, Ctx),
        validate_pos_int(grant_id, Ctx),
        validate_pos_int(delegating_principal_id, Ctx),
        validate_min(grant_version_at_start, Ctx, 1),
        validate_workspace(maps:get(workspace_id, Ctx, undefined)),
        validate_nonempty(trigger_id, Ctx),
        validate_nonempty(context_digest, Ctx),
        validate_nonempty(idempotency_key, Ctx),
        validate_now(Ctx)
    ],
    Errors = [Err || {error, _} = Err <- Checks],
    case Errors of
        [] -> ok;
        _ -> {error, validation_failed}
    end.

validate_enum(Key, Ctx, Allowed) ->
    case maps:get(Key, Ctx, undefined) of
        V when is_atom(V) ->
            case lists:member(V, Allowed) of
                true -> ok;
                false -> {error, {Key, bad_enum}}
            end;
        _ ->
            {error, {Key, bad_enum}}
    end.

validate_pos_int(Key, Ctx) ->
    case maps:get(Key, Ctx, undefined) of
        V when is_integer(V), V > 0 -> ok;
        _ -> {error, {Key, bad_pos_int}}
    end.

validate_min(Key, Ctx, Min) ->
    case maps:get(Key, Ctx, undefined) of
        V when is_integer(V), V >= Min -> ok;
        _ -> {error, {Key, bad_min}}
    end.

validate_workspace(undefined) -> ok;
validate_workspace(V) when is_integer(V), V > 0 -> ok;
validate_workspace(_) -> {error, {workspace_id, bad_workspace}}.

validate_nonempty(Key, Ctx) ->
    case maps:get(Key, Ctx, undefined) of
        V when is_binary(V), V =/= <<>> -> ok;
        _ -> {error, {Key, bad_nonempty}}
    end.

validate_now(Ctx) ->
    case maps:get(now, Ctx, undefined) of
        {{Y, M, D}, {H, I, S}} when
            is_integer(Y),
            is_integer(M),
            is_integer(D),
            is_integer(H),
            is_integer(I),
            is_integer(S)
        ->
            ok;
        _ ->
            {error, {now, bad_datetime}}
    end.

%% Grant 实时重检（读 00000132 表实时状态）：missing/revoked/expired 一律 deny。
grant_check(Conn, GrantId, Now) ->
    case agent_run_pg:get_grant(Conn, GrantId) of
        {ok, Grant} ->
            Status = maps:get(status, Grant),
            NowSec = to_sec(Now),
            Valid =
                Status =:= active andalso
                    to_sec(maps:get(valid_from, Grant)) =< NowSec andalso
                    NowSec =< to_sec(maps:get(expires_at, Grant)),
            case Valid of
                true -> {allow, Grant};
                false when Status =:= revoked -> {deny, grant_revoked};
                false when Status =/= active -> {deny, grant_missing_state};
                false -> {deny, grant_expired}
            end;
        {error, not_found} ->
            {deny, grant_missing}
    end.

decide_effect(Conn, Run, Ctx, GrantVersion, Now) ->
    Decision = maps:get(decision, Ctx, allow),
    EffectBase = effect_base(Run, Ctx, Now),
    case Decision of
        deny ->
            insert_decided(
                Conn, EffectBase#{decided_status => denied, denial_reason => denied_by_policy}, none
            ),
            {error, denied_by_policy};
        approval_required ->
            RunV = maps:get(version, Run),
            Event = transition_event(
                Run,
                running,
                waiting_approval,
                e07,
                RunV,
                undefined,
                system,
                maps:get(actor_id, Ctx, <<"system:authorizer">>),
                Now,
                #{}
            ),
            case
                insert_decided(
                    Conn,
                    EffectBase#{
                        decided_status => waiting_approval,
                        grant_version_checked => GrantVersion
                    },
                    {cas_run, maps:get(id, Run), running, waiting_approval, RunV, undefined, Event}
                )
            of
                {ok, EffectId, _RV} ->
                    {ok, #{effect_id => EffectId, status => waiting_approval}};
                {error, _} = Err ->
                    Err
            end;
        allow ->
            case
                insert_decided(
                    Conn,
                    EffectBase#{
                        decided_status => authorized,
                        grant_version_checked => GrantVersion
                    },
                    none
                )
            of
                {ok, EffectId, _RV} ->
                    {ok, #{effect_id => EffectId, status => authorized}};
                {error, _} = Err ->
                    Err
            end
    end.

decide_deny(Conn, Run, Ctx, Reason, Now) ->
    EffectBase = effect_base(Run, Ctx, Now),
    case
        insert_decided(
            Conn,
            EffectBase#{decided_status => denied, denial_reason => Reason},
            none
        )
    of
        {ok, _EffectId, _RV} -> {error, Reason};
        {error, _} = Err -> Err
    end.

effect_base(Run, Ctx, Now) ->
    #{
        id => maps:get(id, Ctx, agent_run_pg:next_id(agent_effect)),
        run_id => maps:get(id, Run),
        sequence => maps:get(sequence, Ctx),
        tool_id => maps:get(tool_id, Ctx),
        capability => maps:get(capability, Ctx),
        action => maps:get(action, Ctx),
        resource_digest => maps:get(resource_digest, Ctx),
        args_digest => maps:get(args_digest, Ctx),
        external_idempotency_key => maps:get(external_idempotency_key, Ctx, undefined),
        now => Now
    }.

insert_decided(Conn, Effect, RunOp) ->
    case agent_run_pg:insert_effect_tx(Conn, Effect, RunOp) of
        {ok, EffectId, RV} -> {ok, EffectId, RV};
        {error, _} = Err -> Err;
        {rollback, Reason} -> {error, {effect_rollback, Reason}}
    end.

map_tx_result({rollback, Reason}) ->
    {error, {tx_rollback, Reason}};
map_tx_result(Other) ->
    Other.

transition_event(
    Run, From, To, EdgeId, ExpectedVersion, ReasonCode, ActorKind, ActorId, Now, Detail
) ->
    #{
        id => agent_run_pg:next_id(agent_run_event),
        run_id => maps:get(id, Run),
        from_status => From,
        to_status => To,
        reason_code => ReasonCode,
        actor_kind => ActorKind,
        actor_id => ActorId,
        detail => Detail,
        idempotency_key =>
            <<
                (maps:get(idempotency_key, Run))/binary,
                ":",
                (atom_to_binary(EdgeId, utf8))/binary,
                ":v",
                (integer_to_binary(ExpectedVersion))/binary
            >>,
        now => Now
    }.

reason_to_b(R) when is_atom(R) -> atom_to_binary(R, utf8);
reason_to_b(R) when is_binary(R) -> R.

to_sec({{Y, M, D}, {H, I, S}}) when is_float(S) ->
    %% epgsql 把 timestamptz 解码为带小数秒的 datetime，截断到整秒再比较
    to_sec({{Y, M, D}, {H, I, trunc(S)}});
to_sec({{Y, M, D}, {H, I, S}}) ->
    calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, I, S}}).

shift_seconds({Date, Time}, Seconds) ->
    Total = calendar:datetime_to_gregorian_seconds({Date, Time}) + Seconds,
    calendar:gregorian_seconds_to_datetime(Total).
