%%% @doc Agent Grant 命令面用例（AG31-03；架构合同 §7.3 issue/revoke contract +
%%% §8 Delegation=Grant+event lineage）。
%%%
%%% 职责边界（镜像 agent_run_command 的 application 角色）：
%%%   * 纯决策在 domain `agent_grant_domain`；持久化在 infrastructure
%%%     `agent_grant_pg`；本模块做参数收敛 → domain 判定 → 身份/成员关系/目录
%%%     事实装配 → 单事务写库。
%%%   * **membership 校验一律经 configurable module**（`imboy` app env
%%%     `agent_membership_module`，默认 `agent_org_membership_adapter`——AG31-01
%%%     seam 延续）；目录查询同理（`agent_capability_catalog_module`，默认
%%%     `agent_capability_catalog`——D7 契约 §4）。unavailable 一律 fail closed。
%%%   * **身份权威事实 = user 表 account_type**（迁移 00000070 注释枚举
%%%     0=human 1=agent 2=system_bot 3=bot）：delegator 必须存在且 =0
%%%     （Human；架构 §7.2 + ORG-06 操作人判据先例）；Agent 必须 =1（§M.1）。
%%%   * **有效态实时算**：存储态只有 active|revoked；pending/active/expired/
%%%     revoked 由 domain 以注入时钟 Now 计算，expired 不落库（§7.2 L291-292）。
%%%   * **幂等**（§M.1）：UNIQUE(organization_id, delegator_user_id,
%%%     idempotency_key)；同 key 同请求 → 返回既有 Grant（replay=true）；
%%%     同 key 异载荷 → idempotency_conflict。发行竞态（23505）经 re-read 收敛。
%%%   * **拒绝也要审计**（§M.4）：无 grant 行可挂的前置拒绝 → 结构化审计行
%%%     （logger，含全部裁决入参摘要，零 PII/凭据）；已有 grant 行上的拒绝
%%%     （revoke 冲突/重复撤销）→ 结构化返回 + 审计行（event 枚举冻结为
%%%     issued|revoked|expiry_observed，不能承载冲突事件——见 evidence 决策）。
%%%   * 时钟由调用方注入（Ctx.now）；domain 与本模块均不取系统时间。
-module(agent_grant_command).

-export([issue/2, get/4, list/2, revoke/2]).

%% ===================================================================
%% seam 绑定（冻结默认 + env 覆盖，测试/运维用）
%% ===================================================================

membership_module() ->
    application:get_env(imboy, agent_membership_module, agent_org_membership_adapter).

catalog_module() ->
    application:get_env(imboy, agent_capability_catalog_module, agent_capability_catalog).

%% ===================================================================
%% issue（§M.1）
%% ===================================================================

%% @doc 发行 Grant：delegator Human + Agent 身份/active member + 时间窗 +
%%% workspace scope（跨 Org 由 DB 复合 FK 拒绝）+ capability 命中目录 +
%%% constraint 只收窄 + 幂等；四写单事务（grant/workspace/capability/event）。
%%
%% Ctx 必备键：organization_id/agent_id/delegator_user_id（正整数）、
%% workspace_scope_kind（none|explicit）、workspace_ids、capabilities
%% （[#{capability,action,resource_type,constraint}]，≥1 条）、
%% valid_from/expires_at（calendar datetime）、idempotency_key（非空 binary）、
%% now；可选：id（Grant TSID，测试注入用）。
issue(Conn, Ctx) ->
    case
        agent_grant_domain:validate_issue(
            maps:get(organization_id, Ctx, undefined),
            maps:get(agent_id, Ctx, undefined),
            maps:get(delegator_user_id, Ctx, undefined),
            maps:get(workspace_scope_kind, Ctx, undefined),
            maps:get(workspace_ids, Ctx, undefined),
            maps:get(capabilities, Ctx, undefined),
            maps:get(valid_from, Ctx, undefined),
            maps:get(expires_at, Ctx, undefined),
            maps:get(idempotency_key, Ctx, undefined)
        )
    of
        {ok, Norm} ->
            issue_after_validate(Conn, Ctx, Norm);
        {error, _} = Err ->
            audit(<<"issue">>, Err, audit_summary(Ctx)),
            Err
    end.

issue_after_validate(Conn, Ctx, Norm) ->
    %% delegator 必须存在且是 Human（account_type=0；架构 §7.2「运行时要求
    %% Human」+ 仓内权威先例 ORG-06 organization_agent_membership_app 操作人
    %% 判据；枚举 0=human/1=ai_agent/2=system_bot/3=bot 见迁移 00000070
    %% 注释——1/2/3 一律非 Human，全部拒绝）
    case agent_grant_pg:get_user_account_type(Conn, maps:get(delegator_user_id, Norm)) of
        {ok, 0} ->
            case agent_grant_pg:get_user_account_type(Conn, maps:get(agent_id, Norm)) of
                {ok, 1} ->
                    issue_membership_gate(Conn, Ctx, Norm);
                {ok, _NotAgent} ->
                    audit(<<"issue">>, {error, agent_not_agent}, audit_summary(Ctx)),
                    {error, agent_not_agent};
                {error, not_found} ->
                    audit(<<"issue">>, {error, agent_not_found}, audit_summary(Ctx)),
                    {error, agent_not_found}
            end;
        {ok, _NotHuman} ->
            %% Agent(1)/system_bot(2)/bot(3) 当 delegator：V3.1 一律拒绝——
            %% Human 权威判据=account_type=0（验收轮勘误，原「≠1」会放行 2/3）
            audit(<<"issue">>, {error, delegator_not_human}, audit_summary(Ctx)),
            {error, delegator_not_human};
        {error, not_found} ->
            audit(<<"issue">>, {error, delegator_not_found}, audit_summary(Ctx)),
            {error, delegator_not_found}
    end.

issue_membership_gate(Conn, Ctx, Norm) ->
    %% Agent membership：经 configurable port 必须 {ok, active/member}（§M.1；
    %% unavailable/未知形状 fail closed）
    OrgId = maps:get(organization_id, Norm),
    AgentId = maps:get(agent_id, Norm),
    case membership_call(OrgId, AgentId) of
        {ok, #{status := active, role := member}} ->
            issue_capability_gate(Conn, Ctx, Norm);
        {error, unavailable} ->
            audit(<<"issue">>, {error, membership_unavailable}, audit_summary(Ctx)),
            {error, membership_unavailable};
        {error, _MembershipReason} ->
            audit(<<"issue">>, {error, agent_membership_denied}, audit_summary(Ctx)),
            {error, agent_membership_denied}
    end.

membership_call(OrgId, AgentId) ->
    Mod = membership_module(),
    try
        Mod:resolve_organization_membership(OrgId, AgentId)
    catch
        _Class:_Reason -> {error, unavailable}
    end.

issue_capability_gate(Conn, Ctx, Norm) ->
    %% capability 逐条命中目录 + constraint 只收窄（D7 契约 §3；目录查询
    %% 异常 fail closed）
    case check_capabilities(Norm) of
        ok ->
            issue_idempotency_gate(Conn, Ctx, Norm);
        {error, _} = Err ->
            audit(<<"issue">>, Err, audit_summary(Ctx)),
            Err
    end.

check_capabilities(Norm) ->
    Mod = catalog_module(),
    check_capabilities(Mod, maps:get(capabilities, Norm)).

check_capabilities(_Mod, []) ->
    ok;
check_capabilities(Mod, [Cap | Rest]) ->
    Triple =
        {maps:get(capability, Cap), maps:get(action, Cap), maps:get(resource_type, Cap)},
    try
        case Mod:lookup(element(1, Triple), element(2, Triple), element(3, Triple)) of
            {ok, Entry} ->
                case
                    agent_grant_domain:validate_constraint(
                        maps:get(constraint, Cap, undefined),
                        maps:get(legal_constraint_keys, Entry)
                    )
                of
                    ok -> check_capabilities(Mod, Rest);
                    {error, _} = Err -> Err
                end;
            {error, not_found} ->
                {error, {unknown_capability, Triple}}
        end
    catch
        _Class:_Reason -> {error, catalog_unavailable}
    end.

issue_idempotency_gate(Conn, Ctx, Norm) ->
    OrgId = maps:get(organization_id, Norm),
    DelegatorId = maps:get(delegator_user_id, Norm),
    IdemKey = maps:get(idempotency_key, Norm),
    case agent_grant_pg:find_grant_by_idempotency(Conn, OrgId, DelegatorId, IdemKey) of
        {ok, Stored} ->
            resolve_replay(Conn, Norm, Stored, Ctx);
        {error, not_found} ->
            insert_grant(Conn, Ctx, Norm)
    end.

%% 幂等命中：同载荷 → 返回既有 Grant（same Grant 语义）；异载荷 → 冲突拒绝。
resolve_replay(Conn, Norm, Stored, Ctx) ->
    StoredView = stored_view(Conn, Stored),
    case agent_grant_domain:idempotency_verdict(Norm, StoredView) of
        same ->
            Now = maps:get(now, Ctx),
            {ok, #{
                grant_id => maps:get(id, Stored),
                version => maps:get(version, Stored),
                effective_status =>
                    agent_grant_domain:effective_status(Stored, Now),
                replay => true
            }};
        conflict ->
            audit(<<"issue">>, {error, idempotency_conflict}, audit_summary(Ctx)),
            {error, idempotency_conflict}
    end.

stored_view(Conn, Stored) ->
    OrgId = maps:get(organization_id, Stored),
    GrantId = maps:get(id, Stored),
    Stored#{
        workspace_ids => agent_grant_pg:list_workspace_ids(Conn, OrgId, GrantId),
        capabilities => agent_grant_pg:list_capabilities(Conn, OrgId, GrantId)
    }.

insert_grant(Conn, Ctx, Norm) ->
    Now = maps:get(now, Ctx),
    GrantId =
        case maps:get(id, Ctx, undefined) of
            undefined -> agent_grant_pg:next_id(agent_grant);
            Id -> Id
        end,
    Grant = #{
        id => GrantId,
        agent_id => maps:get(agent_id, Norm),
        organization_id => maps:get(organization_id, Norm),
        delegator_user_id => maps:get(delegator_user_id, Norm),
        workspace_scope_kind => maps:get(workspace_scope_kind, Norm),
        valid_from => maps:get(valid_from, Norm),
        expires_at => maps:get(expires_at, Norm),
        idempotency_key => maps:get(idempotency_key, Norm),
        now => Now
    },
    Event = #{
        id => agent_grant_pg:next_id(agent_grant_event),
        grant_id => GrantId,
        event_type => issued,
        actor_kind => human,
        actor_user_id => maps:get(delegator_user_id, Norm),
        from_version => undefined,
        to_version => 1,
        detail => #{
            workspace_scope_kind => maps:get(workspace_scope_kind, Norm),
            workspace_count => length(maps:get(workspace_ids, Norm)),
            capability_count => length(maps:get(capabilities, Norm))
        },
        idempotency_key => event_key(<<"issued">>, Norm, maps:get(idempotency_key, Norm)),
        now => Now
    },
    case
        agent_grant_pg:insert_grant_tx(
            Conn,
            Grant,
            maps:get(workspace_ids, Norm),
            maps:get(capabilities, Norm),
            Event
        )
    of
        {ok, GrantId} ->
            {ok, #{
                grant_id => GrantId,
                version => 1,
                effective_status =>
                    agent_grant_domain:effective_status(
                        #{
                            status => active,
                            valid_from => maps:get(valid_from, Norm),
                            expires_at => maps:get(expires_at, Norm)
                        },
                        Now
                    ),
                replay => false
            }};
        {error, idempotency_duplicate} ->
            %% 发行竞态：唯一索引挡下并发同 key 写，re-read 收敛为
            %% same（既有 Grant）或 conflict（异载荷）
            case
                agent_grant_pg:find_grant_by_idempotency(
                    Conn,
                    maps:get(organization_id, Norm),
                    maps:get(delegator_user_id, Norm),
                    maps:get(idempotency_key, Norm)
                )
            of
                {ok, Stored} -> resolve_replay(Conn, Norm, Stored, Ctx);
                {error, not_found} -> {error, {issue_failed, idempotency_duplicate}}
            end;
        {error, Reason} ->
            audit(<<"issue">>, {error, Reason}, audit_summary(Ctx)),
            {error, Reason};
        {rollback, Reason} ->
            audit(<<"issue">>, {error, {db_error, Reason}}, audit_summary(Ctx)),
            {error, {db_error, Reason}}
    end.

%% event 幂等键全局唯一（uq_age_idempotency_key）；Grant 幂等键的唯一定位域
%% 是 (org, delegator)，故 event 键前缀带上这两者消除跨 Org 碰撞。
event_key(Action, Norm, IdemKey) ->
    iolist_to_binary([
        <<"agent_grant:">>,
        integer_to_binary(maps:get(organization_id, Norm)),
        $:,
        integer_to_binary(maps:get(delegator_user_id, Norm)),
        $:,
        IdemKey,
        $:,
        Action
    ]).

%% ===================================================================
%% get / list（§M.2：org 内 bounded；含实时有效态）
%% ===================================================================

%% @doc org 域内读单张 Grant（含 workspace/capability 子行与实时有效态）。
get(Conn, OrgId, GrantId, Now) ->
    case agent_grant_pg:get_grant(Conn, OrgId, GrantId) of
        {ok, Grant} ->
            {ok, grant_view(Conn, Grant, Now)};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc org 域内 bounded 列举。Filter 必备键：organization_id、now；
%% 可选：agent_id/limit/offset（limit 上限 200 由 repo 收敛）。
list(Conn, Filter) ->
    Now = maps:get(now, Filter),
    Rows = agent_grant_pg:list_grants(Conn, maps:without([now], Filter)),
    {ok, [grant_view(Conn, Row, Now) || Row <- Rows]}.

grant_view(Conn, Grant, Now) ->
    OrgId = maps:get(organization_id, Grant),
    GrantId = maps:get(id, Grant),
    Grant#{
        workspace_ids => agent_grant_pg:list_workspace_ids(Conn, OrgId, GrantId),
        capabilities => agent_grant_pg:list_capabilities(Conn, OrgId, GrantId),
        effective_status => agent_grant_domain:effective_status(Grant, Now)
    }.

%% ===================================================================
%% revoke（§M.3：CAS version；仅 active→revoked）
%% ===================================================================

%% @doc 撤销：CAS expected_version，仅存储 active→revoked；置 revoked_at +
%% revoked_by_user_id；与 event(revoked) 同事务；并发 revoke 一胜一拒
%% （败者 version_conflict）。
%%
%% Ctx 必备键：organization_id/grant_id/revoker_user_id（正整数）、
%% expected_version（>=1）、now。
revoke(Conn, Ctx) ->
    case validate_revoke_ctx(Ctx) of
        ok ->
            revoke_after_validate(Conn, Ctx);
        {error, _} = Err ->
            audit(<<"revoke">>, Err, audit_summary(Ctx)),
            Err
    end.

validate_revoke_ctx(Ctx) ->
    Pairs = [
        {organization_id, maps:get(organization_id, Ctx, undefined)},
        {grant_id, maps:get(grant_id, Ctx, undefined)},
        {revoker_user_id, maps:get(revoker_user_id, Ctx, undefined)}
    ],
    ExpectedVersion = maps:get(expected_version, Ctx, undefined),
    IdsOk =
        case [K || {K, V} <- Pairs, not is_pos_int(V)] of
            [] -> ok;
            _ -> {error, validation_failed}
        end,
    VersionOk =
        case is_pos_int(ExpectedVersion) of
            true -> ok;
            false -> {error, validation_failed}
        end,
    NowOk =
        case maps:get(now, Ctx, undefined) of
            {_, _} -> ok;
            _ -> {error, validation_failed}
        end,
    first_error([IdsOk, VersionOk, NowOk]).

first_error([]) -> ok;
first_error([ok | Rest]) -> first_error(Rest);
first_error([{error, _} = Err | _Rest]) -> Err.

revoke_after_validate(Conn, Ctx) ->
    OrgId = maps:get(organization_id, Ctx),
    GrantId = maps:get(grant_id, Ctx),
    case agent_grant_pg:get_grant(Conn, OrgId, GrantId) of
        {ok, Grant} ->
            revoke_cas(Conn, Ctx, Grant);
        {error, not_found} ->
            audit(<<"revoke">>, {error, not_found}, audit_summary(Ctx)),
            {error, not_found}
    end.

revoke_cas(Conn, Ctx, Grant) ->
    case agent_grant_domain:assert_revoke_allowed(Grant) of
        ok ->
            ExpectedVersion = maps:get(expected_version, Ctx),
            Now = maps:get(now, Ctx),
            Event = #{
                id => agent_grant_pg:next_id(agent_grant_event),
                grant_id => maps:get(id, Grant),
                event_type => revoked,
                actor_kind => human,
                actor_user_id => maps:get(revoker_user_id, Ctx),
                %% repo 的 UPDATE 参数读此键（置 agent_grant.revoked_by_user_id）
                revoked_by_user_id => maps:get(revoker_user_id, Ctx),
                from_version => ExpectedVersion,
                to_version => ExpectedVersion + 1,
                detail => #{},
                idempotency_key =>
                    event_key(
                        <<"revoked">>,
                        #{
                            organization_id => maps:get(organization_id, Ctx),
                            delegator_user_id => maps:get(delegator_user_id, Grant)
                        },
                        maps:get(idempotency_key, Grant)
                    ),
                now => Now
            },
            case
                agent_grant_pg:revoke_cas_tx(
                    Conn,
                    maps:get(organization_id, Ctx),
                    maps:get(id, Grant),
                    ExpectedVersion,
                    Now,
                    Event
                )
            of
                {ok, NewVersion} ->
                    {ok, #{
                        grant_id => maps:get(id, Grant),
                        version => NewVersion,
                        effective_status => revoked
                    }};
                {error, Reason} ->
                    audit(<<"revoke">>, {error, Reason}, audit_summary(Ctx)),
                    {error, Reason};
                {rollback, Reason} ->
                    audit(<<"revoke">>, {error, {db_error, Reason}}, audit_summary(Ctx)),
                    {error, {db_error, Reason}}
            end;
        {error, already_revoked} = Err ->
            %% 已有 grant 行上的拒绝：结构化返回 + 审计行（event 枚举冻结，
            %% 无冲突事件类型——见 evidence result 决策记录）
            audit(<<"revoke">>, Err, audit_summary(Ctx)),
            Err;
        {error, _Other} ->
            audit(<<"revoke">>, {error, not_found}, audit_summary(Ctx)),
            {error, not_found}
    end.

%% ===================================================================
%% 审计（§M.4：拒绝也要审计；零 PII/凭据——只含 id、枚举、计数、幂等键）
%% ===================================================================

audit(Action, {error, Reason}, Summary) ->
    logger:info(
        #{
            what => agent_grant_denied,
            action => Action,
            reason => safe_reason(Reason)
        },
        #{
            domain => [imboy, agent, grant],
            audit => true,
            summary => Summary
        }
    ).

%% reason 归一为可序列化形状（嵌套 error 元组拍平为原子/二元组）
safe_reason(Reason) when is_atom(Reason) -> Reason;
safe_reason({unknown_capability, Triple}) -> {unknown_capability, Triple};
safe_reason({invalid_constraint, Key}) -> {invalid_constraint, Key};
safe_reason({db_error, _Raw}) -> db_error;
safe_reason({issue_failed, Sub}) -> {issue_failed, Sub};
safe_reason(Other) -> Other.

audit_summary(Ctx) ->
    #{
        organization_id => zero(maps:get(organization_id, Ctx, undefined)),
        agent_id => zero(maps:get(agent_id, Ctx, undefined)),
        delegator_user_id => zero(maps:get(delegator_user_id, Ctx, undefined)),
        grant_id => zero(maps:get(grant_id, Ctx, undefined)),
        revoker_user_id => zero(maps:get(revoker_user_id, Ctx, undefined)),
        expected_version => zero(maps:get(expected_version, Ctx, undefined)),
        workspace_scope_kind => maps:get(workspace_scope_kind, Ctx, undefined),
        workspace_count =>
            count_or(maps:get(workspace_ids, Ctx, undefined)),
        capability_count =>
            count_or(maps:get(capabilities, Ctx, undefined)),
        idempotency_key => maps:get(idempotency_key, Ctx, undefined)
    }.

zero(V) when is_integer(V) -> V;
zero(_) -> undefined.

count_or(L) when is_list(L) -> length(L);
count_or(_) -> undefined.

is_pos_int(V) when is_integer(V), V > 0 -> true;
is_pos_int(_) -> false.
