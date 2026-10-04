%%% @doc Agent Tool Permission 纯决策链真源（AG31-05；架构合同 §10 Interface 5，
%%% 规范本 SHA256=05808674d4825320de867a2a8d2899fb4babddbfcea6fe4bf27a4e43a55dd6b2）。
%%%
%%% 职责边界（镜像 agent_run_fsm / agent_grant_domain 的 domain 角色）：
%%%   * 十步决策序（§10.2，顺序即权威）：本模块 `decide/1` 对 application 层
%%%     **实时重读装配**的 Facts 快照逐步裁决；任何一步失败 → deny；全部
%%%     deny/approval 判定在此收敛，application 只做 I/O 装配与决策持久化。
%%%   * 纯函数：无 I/O、无 epgsql、无时钟（Now 由调用方注入）、无进程状态。
%%%   * fail-closed 枚举（§10.2 原文）：任何读取异常、未知 tool、未知 risk、
%%%     未知 constraint、scope mismatch、过期或版本冲突均 deny。
%%%   * R4（A0 裁决）：constraint_json 仅实现保守匹配（workspace_ids 归属/
%%%     resource_id 相等类）；未知键或无法保守判定的约束 → deny（宁拒勿扩）。
%%%   * R3（A0 裁决）：HITL 纯分类在本模块 `hitl_verdict/2`——readonly →
%%%     不需审批；其他**已知** class → approval_required；未知 class 或未知
%%%     risk_level → deny。默认政策零产品语义、纯保守。
%%%   * 目录消费（D7 契约 §3 授权侧复检）：capability 必须命中目录条目，
%%%     constraint 顶层键 ⊆ 条目 legal_constraint_keys；目录空集 = 一切
%%%     capability unknown_capability deny（能力面 staged off）。
-module(agent_tool_decision).

-moduledoc "Agent Tool Permission 纯决策链真源（AG31-05，Interface 5）。".
-export([
    decide/1,
    validate_tool_descriptor/1,
    validate_resource_context/1,
    hitl_verdict/2,
    constraint_verdict/3,
    known_risk_levels/0,
    known_side_effect_classes/0
]).

%% ===================================================================
%% 冻结枚举（§10.1 ToolDescriptor ≥ 五键；R3 已知 class/risk 白名单）
%% ===================================================================

%% risk_level 已知值（加法演进）；未知值一律 deny（R3）。
known_risk_levels() ->
    [low, medium, high, critical].

%% side_effect_class 已知值（加法演进）；readonly 之外一律需审批（R3）。
known_side_effect_classes() ->
    [readonly, write].

%% CS 域标记（§11：CS Gate 未 PASS 前凡 CS 域工具一律 deny——冻结约束）。
-define(CS_DOMAINS, [cs, <<"cs">>]).

%% ===================================================================
%% 入参形状校验（decide 之前的最小门槛；形状非法 fail closed）
%% ===================================================================

%% @doc ToolDescriptor ≥ {tool_id, capability, action, risk_level,
%% side_effect_class}（§10.1）；非空 binary + 已知枚举；可选 domain 键。
validate_tool_descriptor(Tool) when is_map(Tool) ->
    NonEmptyB = fun(V) -> is_binary(V) andalso V =/= <<>> end,
    ShapeOk =
        NonEmptyB(maps:get(tool_id, Tool, undefined)) andalso
            NonEmptyB(maps:get(capability, Tool, undefined)) andalso
            NonEmptyB(maps:get(action, Tool, undefined)),
    RiskOk = lists:member(maps:get(risk_level, Tool, undefined), known_risk_levels()),
    ClassOk = lists:member(
        maps:get(side_effect_class, Tool, undefined), known_side_effect_classes()
    ),
    DomainOk = domain_ok(maps:get(domain, Tool, native)),
    verdict(ShapeOk andalso RiskOk andalso ClassOk andalso DomainOk, invalid_tool_descriptor);
validate_tool_descriptor(_NotMap) ->
    {error, invalid_tool_descriptor}.

%% 形状层面 domain 键只要是有值原子/binary 即合法（cs 等已知域是合法描述符，
%% 由步骤 9 CS gate 统一 deny cs_gate_blocked——deny 语义归步骤 9，不归形状）。
domain_ok(native) -> true;
domain_ok(Domain) when is_atom(Domain), Domain =/= undefined -> true;
domain_ok(Domain) when is_binary(Domain), Domain =/= <<>> -> true;
domain_ok(_) -> false.

%% @doc ResourceContext 必备键（服务端 adapter 解析产物）：org 正整数、
%% resource_type 非空 binary、digest 双非空 binary（只收 digest 不收原文）。
validate_resource_context(Resource) when is_map(Resource) ->
    OrgOk = is_pos_int(maps:get(organization_id, Resource, undefined)),
    WsOk = ws_shape_ok(maps:get(workspace_id, Resource, undefined)),
    TypeOk =
        is_binary(maps:get(resource_type, Resource, undefined)) andalso
            maps:get(resource_type, Resource) =/= <<>>,
    DigestOk =
        is_binary(maps:get(resource_digest, Resource, undefined)) andalso
            maps:get(resource_digest, Resource) =/= <<>> andalso
            is_binary(maps:get(args_digest, Resource, undefined)) andalso
            maps:get(args_digest, Resource) =/= <<>>,
    verdict(OrgOk andalso WsOk andalso TypeOk andalso DigestOk, invalid_resource_context);
validate_resource_context(_NotMap) ->
    {error, invalid_resource_context}.

ws_shape_ok(undefined) -> true;
ws_shape_ok(WsId) -> is_pos_int(WsId).

%% ===================================================================
%% R3：HITL 纯分类（默认政策 agent_hitl_policy 直接委托本函数）
%% ===================================================================

%% @doc risk_level + side_effect_class → no_approval | approval_required |
%% {deny, Reason}。readonly 不需审批；其他已知 class 一律 approval_required；
%% 未知 class 或未知 risk_level deny（R3 冻结：零产品语义、纯保守）。
hitl_verdict(RiskLevel, SideEffectClass) ->
    case lists:member(RiskLevel, known_risk_levels()) of
        false ->
            {deny, unknown_risk_level};
        true ->
            case lists:member(SideEffectClass, known_side_effect_classes()) of
                false -> {deny, unknown_side_effect_class};
                true -> hitl_of_class(SideEffectClass)
            end
    end.

hitl_of_class(readonly) -> no_approval;
hitl_of_class(_NonReadonly) -> approval_required.

%% ===================================================================
%% R4：constraint 保守求值（授权侧；发行侧白名单见 agent_grant_domain）
%% ===================================================================

%% @doc 对单条 grant capability 行的 constraint 针对 ResourceContext 求值。
%% 已知保守键：`workspace_ids`（列表归属）/`resource_id`（相等）。未知键或
%% 无法保守判定 → deny（宁拒勿扩，R4）。目录白名单复检（键 ⊆
%% legal_constraint_keys）由 `constraint_keys_legal/2` 承担。
constraint_verdict(Constraint, Resource, LegalKeys) when is_map(Constraint), is_list(LegalKeys) ->
    case constraint_keys_legal(Constraint, LegalKeys) of
        ok -> eval_constraints(maps:to_list(Constraint), Resource);
        {error, _} = Err -> Err
    end;
constraint_verdict(_Bad, _Resource, _LegalKeys) ->
    {error, invalid_constraint}.

constraint_keys_legal(Constraint, LegalKeys) ->
    case maps:keys(Constraint) -- lists:usort(LegalKeys) of
        [] -> ok;
        [Unknown | _] -> {error, {invalid_constraint, Unknown}}
    end.

eval_constraints([], _Resource) ->
    ok;
eval_constraints([{Key, Value} | Rest], Resource) ->
    case eval_constraint(Key, Value, Resource) of
        ok -> eval_constraints(Rest, Resource);
        {error, _} = Err -> Err
    end.

eval_constraint(<<"workspace_ids">>, Value, Resource) ->
    case is_proper_list(Value) andalso lists:all(fun is_constraint_scalar/1, Value) of
        false ->
            {error, invalid_constraint};
        true ->
            case maps:get(workspace_id, Resource, undefined) of
                undefined -> {error, scope_mismatch};
                WsId -> verdict(lists:member(WsId, Value), scope_mismatch)
            end
    end;
eval_constraint(<<"resource_id">>, Value, Resource) ->
    case is_constraint_scalar(Value) of
        false ->
            {error, invalid_constraint};
        true ->
            case maps:get(resource_id, Resource, undefined) of
                undefined -> {error, scope_mismatch};
                Rid -> verdict(Rid =:= Value, scope_mismatch)
            end
    end;
eval_constraint(_UnknownKey, _Value, _Resource) ->
    %% 目录白名单可能有而本执行器不支持的键：无法保守判定 → deny（R4）
    {error, unsupportable_constraint}.

is_proper_list(L) when is_list(L) -> true;
is_proper_list(_) -> false.

is_constraint_scalar(V) when is_binary(V); is_integer(V) -> true;
is_constraint_scalar(_) -> false.

%% ===================================================================
%% 十步决策链（§10.2，顺序即权威；Providers 全部由 application 实时重读装配）
%% ===================================================================

%% @doc 十步纯裁决。Providers 是**零缓存数据供给器**映射：每个键是零元
%% fun（grant/grant_workspace_ids/grant_capabilities 为一元 fun，入参分别为
%% 步骤 1 读得的 Run 行与步骤 5 读得的 Grant 行——同链零双读），由 application 层 try/catch 后返回 fail-closed 形状。链按
%% 步序按需调用——前序 gate 未过则后序供给器不被调用（读短路 + 决策顺序
%% 一致）。静态事实直接放 Providers 顶：now/run_ctx/tool/resource。
%%
%% 供给器形状：
%%
%%   `run`                {ok, RunRow} | {error, run_not_found | run_read_failed}
%%   `agent_identity`     {ok, #{account_type, status}} | {error, Reason}
%%   `org_state`          {ok, #{status, version}} | {error, archived | not_found | unavailable}
%%   `org_membership`     {ok, #{status, role, version}} | {error, Reason}
%%   `ws_membership`      not_required | {ok, _} | {error, Reason}
%%   `grant`              fun((RunRow) -> {ok, GrantRow} | {error, not_found | grant_read_failed})
%%   `grant_workspace_ids` fun((GrantRow) -> [pos_integer()])
%%   `grant_capabilities`  fun((GrantRow) -> [#{capability, action, resource_type, constraint}])
%%   `catalog_entry`      {ok, Entry} | {error, not_found | catalog_unavailable}
%%   `approval_binding`   none | {ok, EffectRow} | {error, not_found | approval_read_failed}
%%   `resource_policy_verdict`  allow | {deny, Reason}
%%   `hitl_verdict_result`      no_approval | approval_required | {deny, Reason}
%%
%% 返回 {allow, Decision} | {approval_required, Approval} | {deny, Reason}；
%% Decision/Approval 只携带版本与绑定事实，effect_id 由持久化后回填。
%%
%% 步序裁决优先级：任一步失败 → deny（deny > approval_required——同一链上
%% 后置步的冻结否定不因前置步的审批要求而放行，如 CS 域工具永不返回
%% approval_required）。
decide(Providers) when is_map(Providers) ->
    case validate_tool_descriptor(maps:get(tool, Providers)) of
        ok ->
            case validate_resource_context(maps:get(resource, Providers)) of
                ok -> step1_run(fetch(Providers, run), Providers);
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

fetch(Providers, Key) ->
    (maps:get(Key, Providers))().

%% ---- 步骤 1：Run 非终态 + 上下文不可变（§9.2/§9.3） ----
step1_run({error, run_not_found} = E, _Providers) ->
    deny_of(E, run_not_found);
step1_run({error, _ReadFailed}, _Providers) ->
    {deny, run_read_failed};
step1_run({ok, Run}, Providers) ->
    RunCtx = maps:get(run_ctx, Providers),
    Resource = maps:get(resource, Providers),
    Status = maps:get(status, Run),
    ContextOk =
        maps:get(organization_id, Run) =:= maps:get(organization_id, RunCtx) andalso
            maps:get(agent_id, Run) =:= maps:get(agent_id, RunCtx),
    ResourceOrgOk = maps:get(organization_id, Resource) =:= maps:get(organization_id, RunCtx),
    case {Status, ContextOk, ResourceOrgOk} of
        {running, true, true} ->
            %% 缓存本链已读的 Run 行供后续步使用（同一链内不重读；
            %% 跨 authorize/3 调用零缓存——每次全链重走重读）。
            step2_agent(fetch(Providers, agent_identity), Providers#{run_row => Run});
        {_, false, _} ->
            {deny, context_mismatch};
        {_, _, false} ->
            {deny, cross_org};
        {S, _, _} when S =:= succeeded; S =:= failed; S =:= cancelled -> {deny, run_terminal};
        {unknown, _, _} ->
            {deny, run_unknown};
        _NotRunning ->
            {deny, run_not_running}
    end.

%% ---- 步骤 2：Agent enabled（R1 权威事实：行存在 ∧ account_type=1 ∧ status=1） ----
step2_agent({error, agent_not_found}, _Providers) ->
    {deny, agent_not_found};
step2_agent({error, _ReadFailed}, _Providers) ->
    {deny, agent_read_failed};
step2_agent({ok, Identity}, Providers) ->
    case maps:get(account_type, Identity) of
        1 ->
            case maps:get(status, Identity) of
                1 ->
                    step3_org(
                        fetch(Providers, org_state), fetch(Providers, org_membership), Providers
                    );
                _Disabled ->
                    {deny, agent_disabled}
            end;
        _NotAgent ->
            {deny, agent_not_agent}
    end.

%% ---- 步骤 3：Organization state + Membership 实时重读（port 必须 active/member） ----
step3_org({ok, #{status := active}}, {ok, #{status := active, role := member}}, Providers) ->
    step4_workspace(fetch(Providers, ws_membership), Providers);
step3_org({error, archived}, _, _) ->
    {deny, org_archived};
step3_org({error, not_found}, _, _) ->
    {deny, org_not_found};
step3_org({error, _}, _, _) ->
    {deny, org_state_unavailable};
step3_org(_, {error, unavailable}, _) ->
    {deny, membership_unavailable};
step3_org(_, {error, _}, _) ->
    {deny, membership_denied};
%% LOW-2 加固（A2 review）+ 子句序修复（review 残留 LOW）：未知 {ok, Shape}
%% 收敛为 deny 且**归因精确**——此前 {ok,_} 通配先命中，org 合法 active 而
%% membership 形状坏时被误标 org_state_unavailable。现按维度精确分派：
step3_org({ok, #{status := active}}, {ok, _BadMemberShape}, _) ->
    {deny, membership_denied};
step3_org({ok, #{status := BadStatus}}, _, _) when BadStatus =/= active ->
    {deny, org_not_active};
step3_org({ok, _BadOrgShape}, _, _) ->
    {deny, org_state_unavailable}.

%% ---- 步骤 4：需要时 Workspace ownership + Membership（port 必须 {ok, active}） ----
step4_workspace(not_required, Providers) ->
    step5_grant(grant_read(Providers), Providers);
step4_workspace({ok, #{status := active}}, Providers) ->
    step5_grant(grant_read(Providers), Providers);
step4_workspace({error, cross_organization}, _Providers) ->
    {deny, cross_org};
step4_workspace({error, unavailable}, _Providers) ->
    {deny, workspace_membership_unavailable};
step4_workspace({error, not_found}, _Providers) ->
    {deny, workspace_membership_denied};
step4_workspace({error, _}, _Providers) ->
    {deny, workspace_membership_denied};
%% LOW-2 加固（A2 review）：同步骤 3，未知 {ok, Shape} 收敛为 deny。
step4_workspace({ok, _BadWsShape}, _Providers) ->
    {deny, workspace_membership_denied}.

%% grant 供给器为一元：入参步骤 1 随链传递的 Run 行（grant_id 真源）。
grant_read(Providers) ->
    (maps:get(grant, Providers))(maps:get(run_row, Providers)).

%% ---- 步骤 5：当前 Grant 实时重读 + version/lifecycle（有效态 active，捕获版本） ----
step5_grant({ok, Grant}, Providers) ->
    case agent_grant_domain:effective_status(Grant, maps:get(now, Providers)) of
        active -> step6_match(Grant, Providers);
        pending -> {deny, grant_pending};
        expired -> {deny, grant_expired};
        revoked -> {deny, grant_revoked}
    end;
step5_grant({error, not_found}, _Providers) ->
    {deny, grant_missing};
step5_grant({error, _}, _Providers) ->
    {deny, grant_read_failed}.

%% ---- 步骤 6：capability/action/resource 约束匹配（目录命中 + Grant 三元组 + 只收窄） ----
step6_match(Grant, Providers) ->
    case fetch(Providers, catalog_entry) of
        {ok, Entry} -> step6_grant_gate(Grant, Providers, Entry);
        {error, not_found} -> {deny, unknown_capability};
        {error, _CatalogDown} -> {deny, catalog_unavailable}
    end.

step6_grant_gate(Grant, Providers, Entry) ->
    GrantVersion = maps:get(version, Grant),
    Resource = maps:get(resource, Providers),
    Caps = (maps:get(grant_capabilities, Providers))(Grant),
    %% MEDIUM-1 加固（A2 review）：ws 范围读取失败不得伪装成空集（空集=
    %% none-scope 放行语义）——供给器崩溃在本链收敛为 deny（宁拒勿扩）。
    WsOutcome =
        try
            {ok, (maps:get(grant_workspace_ids, Providers))(Grant)}
        catch
            _Class:_Reason -> {error, grant_scope_read_failed}
        end,
    case WsOutcome of
        {error, Reason} ->
            {deny, Reason};
        {ok, WsIds} ->
            step6_scope_constraint(GrantVersion, Providers, Resource, Entry, Caps, WsIds)
    end.

step6_scope_constraint(GrantVersion, Providers, Resource, Entry, Caps, WsIds) ->
    Triple =
        {
            maps:get(capability, maps:get(tool, Providers)),
            maps:get(action, maps:get(tool, Providers)),
            maps:get(resource_type, Resource)
        },
    case match_grant_capability(Triple, Caps) of
        {ok, CapRow} ->
            case scope_in_grant(Resource, WsIds) of
                ok ->
                    case
                        constraint_verdict(
                            maps:get(constraint, CapRow, #{}),
                            Resource,
                            maps:get(legal_constraint_keys, Entry, [])
                        )
                    of
                        ok -> step7_policy(GrantVersion, Providers);
                        {error, _} = Err -> deny_of(Err, constraint_denied(Err))
                    end;
                {error, _} = Err ->
                    deny_of(Err, grant_scope_mismatch)
            end;
        %% 目录命中但 Grant 未授予该三元组
        error ->
            {deny, not_granted}
    end.

%% 目录命中与否决定「未授予」还是「未知能力」——目录异常 fail closed 优先。
match_grant_capability(Triple, CapRows) ->
    case
        lists:search(
            fun(#{capability := C, action := A, resource_type := R}) ->
                {C, A, R} =:= Triple
            end,
            CapRows
        )
    of
        {value, CapRow} -> {ok, CapRow};
        false -> error
    end.

%% Grant explicit scope → Resource 的 workspace 必须在其中；none scope → 不加
%% ws 约束；无 workspace 的 org 级调用对 explicit scope 一律拒绝（保守）。
scope_in_grant(Resource, GrantWsIds) ->
    case maps:get(workspace_id, Resource, undefined) of
        undefined when GrantWsIds =:= [] -> ok;
        undefined ->
            {error, not_in_scope};
        WsId ->
            case lists:member(WsId, GrantWsIds) of
                true -> ok;
                false -> {error, not_in_scope}
            end
    end.

%% 入参是 {error, Inner}（deny_of 语义）——先解包再映射稳定 reason。
constraint_denied({error, Reason}) -> constraint_denied(Reason);
constraint_denied({invalid_constraint, _Key}) -> invalid_constraint;
constraint_denied(unsupportable_constraint) -> unsupportable_constraint;
constraint_denied(scope_mismatch) -> scope_mismatch;
constraint_denied(_) -> invalid_constraint.

%% ---- 步骤 7：domain Resource Policy（port verdict；默认恒 deny） ----
step7_policy(GrantVersion, Providers) ->
    case fetch(Providers, resource_policy_verdict) of
        allow -> step8_hitl(GrantVersion, Providers);
        {deny, Reason} -> {deny, Reason}
    end.

%% ---- 步骤 8：Tool risk + HITL policy（含批准绑定的摄取校验） ----
step8_hitl(GrantVersion, Providers) ->
    case fetch(Providers, hitl_verdict_result) of
        no_approval -> step9_cs_gate(GrantVersion, Providers, allow);
        {deny, Reason} -> {deny, Reason};
        approval_required -> approval_gate(GrantVersion, Providers)
    end.

%% HITL 需要审批：无批准绑定 → approval_required；带绑定 → 全量校验（A10：
%% 参数变/Grant 版本变/批准缺位 → 旧批准拒）。
approval_gate(GrantVersion, Providers) ->
    case fetch(Providers, approval_binding) of
        %% 待批不是链终：步骤 9（CS gate）仍须走完——冻结否定优先于审批要求。
        %% run_row 随载荷回传 application（供 E07 同事务 Run 迁移；同链零双读）。
        none ->
            step9_cs_gate(
                GrantVersion,
                Providers,
                {approval_required, #{
                    grant_version => GrantVersion,
                    run_row => maps:get(run_row, Providers)
                }}
            );
        %% （AllowOutcome 形状：allow | {approval_required, ExtraPayload}）
        {error, not_found} ->
            {deny, approval_not_found};
        {error, _} ->
            {deny, approval_read_failed};
        {ok, Effect} ->
            verify_approval(GrantVersion, Providers, Effect)
    end.

verify_approval(GrantVersion, Providers, Effect) ->
    Run = maps:get(run_row, Providers),
    Resource = maps:get(resource, Providers),
    Digest = maps:get(args_digest, Effect, undefined),
    BoundVersion = maps:get(grant_version_checked, Effect, undefined),
    RunOk = maps:get(run_id, Effect, undefined) =:= maps:get(id, Run),
    Authorized = maps:get(status, Effect) =:= authorized,
    DigestOk = Digest =:= maps:get(args_digest, Resource),
    VersionOk = BoundVersion =:= GrantVersion,
    if
        not RunOk -> {deny, approval_run_mismatch};
        not Authorized -> {deny, approval_not_authorized};
        not DigestOk -> {deny, stale_approval_args};
        not VersionOk -> {deny, stale_approval_grant_version};
        true -> step9_cs_gate(GrantVersion, Providers, allow)
    end.

%% ---- 步骤 9：CS tools 过 CS gate（未 PASS → 凡 CS 域工具一律 deny，冻结） ----
%% AllowOutcome：步骤 9/10 走完后的非 deny 结果（allow 或待批形状）。
step9_cs_gate(GrantVersion, Providers, AllowOutcome) ->
    Tool = maps:get(tool, Providers),
    case lists:member(maps:get(domain, Tool, native), ?CS_DOMAINS) of
        true -> {deny, cs_gate_blocked};
        false -> step10_outcome(GrantVersion, Providers, AllowOutcome)
    end.

%% ---- 步骤 10：dispatch 前持久化决策（application 执行；此处收敛裁决形状） ----
step10_outcome(GrantVersion, Providers, allow) ->
    {allow, base_payload(GrantVersion, Providers)};
step10_outcome(GrantVersion, Providers, {approval_required, Extra}) ->
    {approval_required, maps:merge(base_payload(GrantVersion, Providers), Extra)}.

base_payload(GrantVersion, Providers) ->
    Tool = maps:get(tool, Providers),
    Resource = maps:get(resource, Providers),
    #{
        grant_version => GrantVersion,
        tool_id => maps:get(tool_id, Tool),
        args_digest => maps:get(args_digest, Resource)
    }.

deny_of({error, _Inner}, Fallback) -> {deny, Fallback};
deny_of(Reason, _Fallback) when is_atom(Reason) -> {deny, Reason}.

%% ===================================================================
%% 内部
%% ===================================================================

is_pos_int(V) when is_integer(V), V > 0 -> true;
is_pos_int(_) -> false.

verdict(true, _Reason) -> ok;
verdict(false, Reason) -> {error, Reason}.
