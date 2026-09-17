%%% @doc Agent Tool Permission 唯一 fail-closed 授权咽喉（AG31-05；架构合同
%%% §10.1 冻结入口，签名 verbatim）+ 强制决策审计 + R5/R6 接缝。
%%%
%%% 冻结公开 API（架构 §10.1）：
%%%
%%%   authorize(AgentRunContext, ToolDescriptor, ResourceContext)
%%%     -> {allow, DecisionContext}
%%%      | {approval_required, ApprovalContext}
%%%      | {deny, ReasonCode}
%%%
%%% 职责边界：
%%%   * 纯十步裁决在 domain `agent_tool_decision:decide/1`；本模块做入参
%%%     形状门槛 → **全量实时重读装配（零缓存，每个读取 try/catch fail
%%%     closed）** → domain 裁决 → 决策持久化（agent_effect CAS）→ 审计 →
%%%     dispatcher 接缝。任何一步失败 → deny；持久化失败绝不 allow。
%%%   * **决策审计**：每次 authorize/3 恰一条结构化审计行（logger，
%%%     what=agent_tool_decision，零 PII/凭据/原文——只有 id、枚举、digest、
%%%     reason）。审计行发射本身崩溃 → {deny, audit_failed}（audit 失败拒绝）。
%%%   * **顺序不变量**：deny/approval_required 结果不触发 dispatcher；
%%%     dispatcher 仅在 持久化成功 ∧ 审计成功 ∧ allow 后恰一次调用。
%%%   * **R5 dispatcher 接缝**：env `agent_tool_dispatcher_module`（默认本
%%%     模块 `dispatch/1` 无操作——OWNED FILES 冻结所致的命名妥协，真实
%%%     dispatcher 由 AG31-06 经此 seam 接管）。deny/approval 路径调用数恒 0。
%%%   * **R6 审批摄取**：`approve_effect/4` 为应用层附加命令（不冒充公开
%%%     契约），绑定 run_id+effect_id+args_digest+grant_version 进 agent_effect
%%%     审批字段；参数变/Grant 版本变/Grant 撤销 → 旧批准拒；dispatch 前的
%%%     再次授权 = 下次 authorize/3 全链重走（RunCtx 携带 approval_effect_id）。
%%%   * **R7 效果持久化**：复用 agent_run_pg 既有 effect API（insert_effect_tx
%%%     /next_id/get_effect），仅严格附加 get_agent_identity/2、
%%%     next_effect_sequence/2 与 insert_effect_guarded_tx/2（A2 review
%%%     MEDIUM-2 加固：allow 路径 run 行锁终态守卫；result.md 披露），
%%%     不改任何既有行为。
%%%   * Conn 由 AgentRunContext.conn 注入（authorize/3 冻结三参；与
%%%     agent_run_command/agent_grant_command 的调用方注入口径一致）。
-module(agent_tool_authorizer).

-export([authorize/3, approve_effect/4, dispatch/1]).

%% ===================================================================
%% seam 绑定（冻结默认 + env 覆盖，测试/运维用）
%% ===================================================================

membership_module() ->
    application:get_env(imboy, agent_membership_module, agent_org_membership_adapter).

catalog_module() ->
    application:get_env(imboy, agent_capability_catalog_module, agent_capability_catalog).

resource_policy_module() ->
    application:get_env(imboy, agent_resource_policy_module, agent_resource_policy).

hitl_module() ->
    application:get_env(imboy, agent_hitl_policy_module, agent_hitl_policy).

dispatcher_module() ->
    application:get_env(imboy, agent_tool_dispatcher_module, ?MODULE).

%% ===================================================================
%% authorize/3（十步编排；§T 顺序即权威）
%% ===================================================================

authorize(AgentRunContext, ToolDescriptor, ResourceContext) ->
    case
        {
            agent_tool_decision:validate_tool_descriptor(ToolDescriptor),
            validate_run_context(AgentRunContext),
            agent_tool_decision:validate_resource_context(ResourceContext)
        }
    of
        {ok, ok, ok} ->
            authorize_after_validate(AgentRunContext, ToolDescriptor, ResourceContext);
        {{error, Reason}, _, _} ->
            refuse_without_read(Reason, AgentRunContext, ToolDescriptor, ResourceContext);
        {_, {error, Reason}, _} ->
            refuse_without_read(Reason, AgentRunContext, ToolDescriptor, ResourceContext);
        {_, _, {error, Reason}} ->
            refuse_without_read(Reason, AgentRunContext, ToolDescriptor, ResourceContext)
    end.

authorize_after_validate(RunCtx, Tool, Resource) ->
    Conn = maps:get(conn, RunCtx),
    Facts = assemble_facts(Conn, RunCtx, Tool, Resource),
    case agent_tool_decision:decide(Facts) of
        {deny, Reason} ->
            finish_deny(Conn, RunCtx, Tool, Resource, Facts, Reason);
        {allow, Decision} ->
            finish_allow(Conn, RunCtx, Tool, Resource, Facts, Decision);
        {approval_required, Approval} ->
            finish_approval(Conn, RunCtx, Tool, Resource, Facts, Approval)
    end.

%% ===================================================================
%% 实时重读供给器装配（零缓存；每个读取独立 try/catch fail closed，§10.2；
%% 供给器为按需 fun——前序 gate 未过则后序读取不发生，与十步序读短路一致）
%% ===================================================================

assemble_facts(Conn, RunCtx, Tool, Resource) ->
    RunId = maps:get(run_id, RunCtx),
    OrgId = maps:get(organization_id, RunCtx),
    AgentId = maps:get(agent_id, RunCtx),
    #{
        now => maps:get(now, RunCtx),
        run_ctx => RunCtx,
        tool => Tool,
        resource => Resource,
        run => fun() ->
            read(fun() -> agent_run_pg:get_run(Conn, RunId) end, run_not_found, run_read_failed)
        end,
        agent_identity => fun() ->
            read(
                fun() -> agent_run_pg:get_agent_identity(Conn, AgentId) end,
                agent_not_found,
                agent_read_failed
            )
        end,
        org_state => fun() ->
            read(
                fun() -> (membership_module()):resolve_organization_state(OrgId) end,
                not_found,
                unavailable
            )
        end,
        org_membership => fun() ->
            read(
                fun() ->
                    (membership_module()):resolve_organization_membership(OrgId, AgentId)
                end,
                not_found,
                unavailable
            )
        end,
        ws_membership => fun() -> ws_fact(OrgId, AgentId, Resource) end,
        %% 一元供给器：入参为步骤 1 读得并随链传递的 Run 行（grant_id 真源，
        %% 避免同链双读）
        grant => fun(Run) -> grant_fact(Conn, OrgId, Run) end,
        grant_workspace_ids =>
            fun(Grant) -> read_ws_ids(Conn, OrgId, maps:get(id, Grant)) end,
        grant_capabilities =>
            fun(Grant) -> read_caps(Conn, OrgId, maps:get(id, Grant)) end,
        catalog_entry => fun() ->
            catalog_call(
                maps:get(capability, Tool),
                maps:get(action, Tool),
                maps:get(resource_type, Resource)
            )
        end,
        approval_binding => fun() -> approval_fact(Conn, RunCtx) end,
        resource_policy_verdict => fun() -> policy_fact(RunCtx, Tool, Resource) end,
        hitl_verdict_result => fun() -> hitl_fact(Tool) end
    }.

%% port 错误语义原样透传（archived/inactive/cross_organization/unavailable 等
%% ——domain 按语义裁决）；not_found 按资源翻译；crash/未知形状才归并
%% FailedReason（fail closed）。
read(Fun, NotFoundReason, FailedReason) ->
    try
        case Fun() of
            {ok, _} = Ok -> Ok;
            {error, not_found} -> {error, NotFoundReason};
            {error, Reason} when is_atom(Reason) -> {error, Reason};
            _UnknownShape -> {error, FailedReason}
        end
    catch
        _Class:_Reason -> {error, FailedReason}
    end.

ws_fact(OrgId, AgentId, Resource) ->
    case maps:get(workspace_id, Resource, undefined) of
        undefined ->
            not_required;
        WsId ->
            read(
                fun() ->
                    (membership_module()):resolve_workspace_membership(OrgId, WsId, AgentId)
                end,
                not_found,
                unavailable
            )
    end.

%% 一元供给器：入参为步骤 1 读得并随链传递的 Run 行（grant_id 唯一真源），
%% 避免同一 authorize 链内双读 run。
grant_fact(Conn, OrgId, Run) ->
    read_grant(Conn, OrgId, maps:get(grant_id, Run, undefined)).

read_grant(Conn, _OrgId, GrantId) when is_integer(GrantId) ->
    try
        case agent_run_pg:get_grant(Conn, GrantId) of
            {ok, _} = Ok -> Ok;
            {error, not_found} = E -> E;
            _ -> {error, grant_read_failed}
        end
    catch
        _Class:_Reason -> {error, grant_read_failed}
    end;
read_grant(_Conn, _OrgId, _BadGrantId) ->
    {error, not_found}.

read_ws_ids(Conn, OrgId, GrantId) ->
    %% MEDIUM-1 加固（A2 review）：ws 范围读取失败**上抛**（由 domain 十步链
    %% 收敛为 deny）——吞为 [] 会把 explicit-scope 读取故障伪装成 none-scope
    %% 放行语义（fail-open）。对比 read_caps 的 [] 只导致 not_granted（fail
    %% closed 方向正确），此处必须区分。
    try agent_grant_pg:list_workspace_ids(Conn, OrgId, GrantId) of
        L when is_list(L) -> L;
        _ -> error(ws_ids_bad_shape)
    catch
        _Class:_Reason -> error(ws_ids_read_failed)
    end.

read_caps(Conn, OrgId, GrantId) ->
    try agent_grant_pg:list_capabilities(Conn, OrgId, GrantId) of
        L when is_list(L) -> L;
        _ -> []
    catch
        _Class:_Reason -> []
    end.

catalog_call(Capability, Action, ResourceType) ->
    try (catalog_module()):lookup(Capability, Action, ResourceType) of
        {ok, _} = Ok -> Ok;
        {error, not_found} = E -> E;
        _ -> {error, catalog_unavailable}
    catch
        _Class:_Reason -> {error, catalog_unavailable}
    end.

approval_fact(Conn, RunCtx) ->
    case maps:get(approval_effect_id, RunCtx, undefined) of
        undefined ->
            none;
        EffectId ->
            try
                case agent_run_pg:get_effect(Conn, EffectId) of
                    {ok, _} = Ok -> Ok;
                    {error, not_found} -> {error, not_found};
                    _ -> {error, approval_read_failed}
                end
            catch
                _Class:_Reason -> {error, approval_read_failed}
            end
    end.

policy_fact(RunCtx, Tool, Resource) ->
    try (resource_policy_module()):evaluate(RunCtx, Tool, Resource) of
        allow -> allow;
        {deny, _Reason} = Deny -> Deny;
        _UnknownShape -> {deny, resource_policy_unavailable}
    catch
        _Class:_Reason -> {deny, resource_policy_unavailable}
    end.

hitl_fact(Tool) ->
    try (hitl_module()):evaluate(maps:get(risk_level, Tool), maps:get(side_effect_class, Tool)) of
        no_approval -> no_approval;
        approval_required -> approval_required;
        {deny, _Reason} = Deny -> Deny;
        _UnknownShape -> {deny, hitl_policy_unavailable}
    catch
        _Class:_Reason -> {deny, hitl_policy_unavailable}
    end.

%% ===================================================================
%% 决策持久化 + 审计 + dispatcher（步骤 10；持久化失败绝不 allow）
%% ===================================================================

finish_allow(Conn, RunCtx, Tool, Resource, Facts, Decision) ->
    PersistSpec = #{
        decided_status => authorized,
        grant_version_checked => maps:get(grant_version, Decision),
        %% MEDIUM-2 加固（A2 review）：allow 决策走 run 行锁守卫事务——
        %% 步骤 1 无锁读与本事务之间的并发终态迁移（cancel/timeout）在此
        %% 被拒（§14 Run terminal → deny all new effects）。
        persist_mode => guarded
    },
    case persist_decision(Conn, RunCtx, Tool, Resource, Facts, PersistSpec) of
        {ok, EffectId} ->
            case emit_audit(allow, undefined, RunCtx, Tool, Resource, EffectId, persist_ok) of
                ok ->
                    DispatchResult = run_dispatcher(
                        RunCtx, Tool, Resource, Facts, Decision, EffectId
                    ),
                    {allow, Decision#{effect_id => EffectId, dispatch_result => DispatchResult}};
                {error, audit_failed} ->
                    {deny, audit_failed}
            end;
        {error, duplicate_effect} ->
            %% LOW-1 加固：无 effect 行产生——EffectId=undefined，
            %% persist_outcome 如实记 duplicate_effect。
            deny_result(
                duplicate_effect, RunCtx, Tool, Resource, Facts, undefined, duplicate_effect
            );
        {error, run_not_active} ->
            %% 守卫事务观测到并发终态：按步骤 1 同义语 deny（绝不 allow）。
            deny_result(
                run_not_running, RunCtx, Tool, Resource, Facts, undefined, run_not_active
            );
        {error, PersistReason} ->
            deny_result(
                decision_persist_failed, RunCtx, Tool, Resource, Facts, undefined, PersistReason
            )
    end.

finish_approval(Conn, RunCtx, Tool, Resource, Facts, Approval) ->
    Run = maps:get(run_row, Approval),
    PersistSpec = #{
        decided_status => waiting_approval,
        grant_version_checked => maps:get(grant_version, Approval),
        run_op => approval_run_op(RunCtx, Run)
    },
    case persist_decision(Conn, RunCtx, Tool, Resource, Facts, PersistSpec) of
        {ok, EffectId} ->
            ApprovalCtx = maps:without([run_row], Approval#{
                effect_id => EffectId,
                run_id => maps:get(run_id, RunCtx),
                args_digest => maps:get(args_digest, Resource)
            }),
            case
                emit_audit(
                    approval_required, undefined, RunCtx, Tool, Resource, EffectId, persist_ok
                )
            of
                ok -> {approval_required, ApprovalCtx};
                {error, audit_failed} -> {deny, audit_failed}
            end;
        {error, duplicate_effect} ->
            %% LOW-1 加固：同 finish_allow——EffectId=undefined、口径如实。
            deny_result(
                duplicate_effect, RunCtx, Tool, Resource, Facts, undefined, duplicate_effect
            );
        {error, PersistReason} ->
            deny_result(
                decision_persist_failed, RunCtx, Tool, Resource, Facts, undefined, PersistReason
            )
    end.

finish_deny(Conn, RunCtx, Tool, Resource, Facts, Reason) ->
    PersistOutcome = try_persist_deny(Conn, RunCtx, Tool, Resource, Facts, Reason),
    deny_result(
        Reason,
        RunCtx,
        Tool,
        Resource,
        Facts,
        effect_id_of(PersistOutcome),
        PersistOutcome
    ).

%% deny 的持久化是审计性质：失败不改变已定的 deny（仍 fail closed），
%% 但持久化结果记入审计行；仅 running 态的 Run 才可挂 effect 行
%% （与 04B new_effect_gate 口径一致——非 running 结构性拒绝只审计不落行）。
try_persist_deny(Conn, RunCtx, Tool, Resource, Facts, Reason) ->
    case run_persistable(Facts) of
        false ->
            not_persistable;
        true ->
            case
                persist_decision(Conn, RunCtx, Tool, Resource, Facts, #{
                    decided_status => denied, denial_reason => Reason
                })
            of
                {ok, EffectId} -> {persisted, EffectId};
                {error, Reason2} -> Reason2
            end
    end.

%% deny 记账仅对 running 态 Run 落 effect 行（与 04B new_effect_gate 口径
%% 一致——非 running 结构性拒绝只审计不落行）。经 run 供给器实时重读：
%% domain 步骤 1 缓存的 run_row 在 domain 本地映射内，调用方映射不可见。
run_persistable(Facts) ->
    case (maps:get(run, Facts))() of
        {ok, #{status := running}} -> true;
        _ -> false
    end.

effect_id_of({persisted, EffectId}) -> EffectId;
effect_id_of(_) -> undefined.

persist_decision(Conn, RunCtx, Tool, Resource, Facts, PersistSpec) ->
    try
        Effect = effect_base(Conn, RunCtx, Tool, Resource, Facts, PersistSpec),
        RunOp = maps:get(run_op, PersistSpec, none),
        case maps:get(persist_mode, PersistSpec, tx) of
            guarded ->
                %% MEDIUM-2：allow 路径——run 行锁 + status='running' 守卫
                %% 与 effect 落账同事务（insert_effect_guarded_tx 为 R7 严格
                %% 附加新函数；deny/approval 路径不受影响继续走 insert_effect_tx）。
                case agent_run_pg:insert_effect_guarded_tx(Conn, Effect) of
                    {ok, EffectId} ->
                        {ok, EffectId};
                    {error, {run_not_active, _Status}} ->
                        {error, run_not_active};
                    {error, {duplicate_effect, _Constraint}} ->
                        {error, duplicate_effect};
                    {error, Reason} ->
                        {error, {persist_failed, Reason}};
                    {rollback, Reason} ->
                        {error, {persist_failed, Reason}}
                end;
            tx ->
                case agent_run_pg:insert_effect_tx(Conn, Effect, RunOp) of
                    {ok, EffectId, _RunVersion} -> {ok, EffectId};
                    {error, {duplicate_effect, _Constraint}} -> {error, duplicate_effect};
                    {error, Reason} -> {error, {persist_failed, Reason}};
                    {rollback, Reason} -> {error, {persist_failed, Reason}}
                end
        end
    catch
        _Class:_Reason -> {error, {persist_failed, effect_build_crashed}}
    end.

effect_base(Conn, RunCtx, Tool, Resource, Facts, PersistSpec) ->
    RunId = maps:get(run_id, RunCtx),
    #{
        id => agent_run_pg:next_id(agent_effect),
        run_id => RunId,
        sequence => sequence_of(Conn, RunCtx, Facts),
        tool_id => maps:get(tool_id, Tool),
        capability => maps:get(capability, Tool),
        action => maps:get(action, Tool),
        resource_digest => maps:get(resource_digest, Resource),
        args_digest => maps:get(args_digest, Resource),
        external_idempotency_key => maps:get(external_idempotency_key, Resource, undefined),
        now => maps:get(now, Facts),
        decided_status => maps:get(decided_status, PersistSpec),
        denial_reason => maps:get(denial_reason, PersistSpec, undefined),
        grant_version_checked => maps:get(grant_version_checked, PersistSpec, undefined)
    }.

%% 调用方可显式给 sequence；缺省经严格附加的 next_effect_sequence/2 实时算
%% （UNIQUE(run_id,sequence) 兜底并发碰撞为 duplicate_effect deny）。
sequence_of(Conn, RunCtx, _Facts) ->
    case maps:get(sequence, RunCtx, undefined) of
        Seq when is_integer(Seq), Seq > 0 ->
            Seq;
        _ ->
            default_sequence(Conn, maps:get(run_id, RunCtx))
    end.

default_sequence(Conn, RunId) ->
    try agent_run_pg:next_effect_sequence(Conn, RunId) of
        Seq when is_integer(Seq), Seq > 0 -> Seq;
        _ -> 1
    catch
        _Class:_Reason -> 1
    end.

%% approval_required 的同事务 Run E07 迁移（running→waiting_approval），
%% 事件口径与 agent_run_command:transition_event 一致。
approval_run_op(RunCtx, Run) ->
    RunV = maps:get(version, Run),
    Now = maps:get(now, RunCtx),
    Event = #{
        id => agent_run_pg:next_id(agent_run_event),
        run_id => maps:get(id, Run),
        from_status => running,
        to_status => waiting_approval,
        reason_code => undefined,
        actor_kind => system,
        actor_id => <<"system:authorizer">>,
        detail => #{},
        idempotency_key =>
            <<
                (maps:get(idempotency_key, Run))/binary,
                ":e07:v",
                (integer_to_binary(RunV))/binary
            >>,
        now => Now
    },
    {cas_run, maps:get(id, Run), running, waiting_approval, RunV, undefined, Event}.

%% ===================================================================
%% R5 dispatcher 接缝（仅 allow 且持久化+审计成功后恰一次）
%% ===================================================================

run_dispatcher(RunCtx, Tool, Resource, Facts, Decision, EffectId) ->
    Input = #{
        run_id => maps:get(run_id, RunCtx),
        effect_id => EffectId,
        tool_id => maps:get(tool_id, Tool),
        capability => maps:get(capability, Tool),
        action => maps:get(action, Tool),
        args_digest => maps:get(args_digest, Resource),
        resource_digest => maps:get(resource_digest, Resource),
        external_idempotency_key => maps:get(external_idempotency_key, Resource, undefined),
        grant_version_checked => maps:get(grant_version, Decision),
        now => maps:get(now, Facts)
    },
    try (dispatcher_module()):dispatch(Input) of
        Result ->
            Result
    catch
        _Class:_Reason -> {error, dispatcher_crashed}
    end.

%% 默认无操作 dispatcher（R5）：不触碰 effect ledger、零外部副作用。
dispatch(_Input) ->
    {ok, no_op}.

%% ===================================================================
%% 统一 deny 收口（审计先行；审计失败 → audit_failed 拒绝）
%% ===================================================================

deny_result(Reason, RunCtx, Tool, Resource, Facts, EffectId) ->
    deny_result(Reason, RunCtx, Tool, Resource, Facts, EffectId, not_persistable).

deny_result(Reason, RunCtx, Tool, Resource, _Facts, EffectId, PersistOutcome) ->
    case emit_audit(deny, Reason, RunCtx, Tool, Resource, EffectId, PersistOutcome) of
        ok -> {deny, Reason};
        {error, audit_failed} -> {deny, audit_failed}
    end.

%% 形状非法的拒绝发生在任何读取之前：无可挂 effect 行，仅审计。
refuse_without_read(Reason, RunCtx, Tool, Resource) ->
    deny_result(Reason, RunCtx, Tool, Resource, #{}, undefined).

%% ===================================================================
%% R6 审批摄取（应用层附加命令；绑定 run_id+effect_id+args_digest+grant_version）
%% ===================================================================

%% @doc 人工批准落库：effect 必须 waiting_approval 且归属该 Run（四元组绑定
%% 校验）；摘要不匹配拒绝（approval_digest_mismatch）；批准时实时重检 Grant
%% （已撤销/暂停/归档不能被批准覆盖 → effect denied + Run E13 failed）。
%% 正常路径 effect→authorized + Run E12 waiting_approval→queued（复用 04B
%% 冻结行为 agent_run_command:approve_effect/4）。
%%
%% Ctx 必备键：args_digest（非空 binary）、approval_ref、actor_id、now。
approve_effect(Conn, RunId, EffectId, Ctx) ->
    case agent_run_pg:get_effect(Conn, EffectId) of
        {ok, Effect} ->
            approve_bound(Conn, RunId, EffectId, Effect, Ctx);
        {error, not_found} ->
            audit_approval(rejected, effect_not_found, RunId, EffectId, Ctx),
            {error, effect_not_found}
    end.

approve_bound(Conn, RunId, EffectId, Effect, Ctx) ->
    case maps:get(run_id, Effect) =:= RunId of
        false ->
            %% effect 不归属该 Run → 错配批准拒（A10 四元组绑定面）
            audit_approval(rejected, approval_run_mismatch, RunId, EffectId, Ctx),
            {error, approval_run_mismatch};
        true ->
            case agent_run_command:approve_effect(Conn, RunId, EffectId, Ctx) of
                {ok, _Versions} = Ok ->
                    audit_approval(approved, ok, RunId, EffectId, Ctx),
                    Ok;
                {error, Reason} = Err ->
                    audit_approval(rejected, Reason, RunId, EffectId, Ctx),
                    Err
            end
    end.

%% ===================================================================
%% 强制决策审计（每 authorize 恰一条；零 PII/凭据/原文）
%% ===================================================================

emit_audit(Outcome, Reason, RunCtx, Tool, Resource, EffectId, PersistOutcome) ->
    Report = #{
        what => agent_tool_decision,
        outcome => Outcome,
        reason => Reason,
        tool_id => safe_get(tool_id, Tool),
        capability => safe_get(capability, Tool),
        action => safe_get(action, Tool),
        risk_level => safe_get(risk_level, Tool),
        side_effect_class => safe_get(side_effect_class, Tool),
        effect_id => EffectId
    },
    Meta = #{
        domain => [imboy, agent, tool_permission],
        audit => true,
        summary => #{
            run_id => safe_get(run_id, RunCtx),
            agent_id => safe_get(agent_id, RunCtx),
            organization_id => safe_get(organization_id, RunCtx),
            workspace_id => safe_get(workspace_id, Resource),
            resource_organization_id => safe_get(organization_id, Resource),
            resource_type => safe_get(resource_type, Resource),
            args_digest => safe_get(args_digest, Resource),
            approval_effect_id => safe_get(approval_effect_id, RunCtx),
            persist_outcome => persist_brief(PersistOutcome)
        }
    },
    try
        logger:info(Report, Meta)
    catch
        _Class:_Reason -> {error, audit_failed}
    end.

persist_brief(persist_ok) -> <<"ok">>;
persist_brief(not_persistable) -> <<"not_persistable">>;
persist_brief({persisted, _EffectId}) -> <<"ok">>;
persist_brief({persist_failed, _Reason}) -> <<"persist_failed">>;
persist_brief(duplicate_effect) -> <<"duplicate_effect">>;
persist_brief(run_not_active) -> <<"run_not_active">>;
persist_brief(_) -> <<"other">>.

safe_get(Key, Map) when is_map(Map) -> maps:get(Key, Map, undefined);
safe_get(_Key, _NotMap) -> undefined.

%% ===================================================================
%% 入参形状（authorize 侧的最小门槛；conn/now/ids 由本层持有）
%% ===================================================================

validate_run_context(Ctx) when is_map(Ctx) ->
    IdsOk =
        is_pos_int(maps:get(run_id, Ctx, undefined)) andalso
            is_pos_int(maps:get(agent_id, Ctx, undefined)) andalso
            is_pos_int(maps:get(organization_id, Ctx, undefined)),
    NowOk =
        case maps:get(now, Ctx, undefined) of
            {{Y, M, D}, {H, I, S}} when
                is_integer(Y),
                is_integer(M),
                is_integer(D),
                is_integer(H),
                is_integer(I),
                is_integer(S)
            ->
                true;
            _ ->
                false
        end,
    ConnOk = maps:is_key(conn, Ctx),
    verdict(IdsOk andalso NowOk andalso ConnOk, invalid_run_context);
validate_run_context(_NotMap) ->
    {error, invalid_run_context}.

%% ===================================================================
%% 审批审计（R6 摄取面；与决策审计同域不同 what）
%% ===================================================================

audit_approval(Verdict, Reason, RunId, EffectId, Ctx) ->
    Report = #{
        what => agent_tool_approval,
        verdict => Verdict,
        reason => Reason,
        effect_id => EffectId
    },
    Meta = #{
        domain => [imboy, agent, tool_permission],
        audit => true,
        summary => #{
            run_id => RunId,
            actor_id => safe_get(actor_id, Ctx),
            approval_ref => safe_get(approval_ref, Ctx),
            args_digest => safe_get(args_digest, Ctx)
        }
    },
    try
        logger:info(Report, Meta)
    catch
        _Class:_Reason -> {error, audit_failed}
    end.

is_pos_int(V) when is_integer(V), V > 0 -> true;
is_pos_int(_) -> false.

verdict(true, _Reason) -> ok;
verdict(false, Reason) -> {error, Reason}.
