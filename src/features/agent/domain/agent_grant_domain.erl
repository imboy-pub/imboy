%%% @doc Agent Grant 纯域决策真源（AG31-03；架构合同 §7 Frozen Grant Schema
%%% Contract + §8 Delegation，规范本 SHA256=05808674d4825320de867a2a8d2899fb4babddbfcea6fe4bf27a4e43a55dd6b2）。
%%%
%%% 职责边界（镜像 agent_run_fsm 的 domain 角色）：
%%%   * 纯函数：无 I/O、无 epgsql、**无时钟**（Now 一律由 application 注入——
%%%     铁律 4「domain 疑似隐式时间依赖」规避）、无进程状态。
%%%   * 本模块裁决：issue 入参形状与时间窗、constraint 只收窄校验、
%%%     有效态实时计算、revoke 状态迁移合法性、幂等载荷比对。
%%%   * 存储态只有 `active | revoked`（§7.2 CHECK）；`pending | active |
%%%     expired | revoked` 是**读时实时计算**的 API 有效态，`expired` 不落库
%%%     （§7.2 L291-292：到期不依赖后台任务才能拒绝）。
%%%   * capability 目录语义不在本模块（目录归 Agent Tool Contract，消费方
%%%     agent_grant_command 经 env seam 查目录）；本模块只做 constraint 键
%%%     白名单与「只收窄」形状校验。
%%%   * Delegation = Grant + event lineage（ADR-AG31-003）：无独立 Delegation
%%%     聚合，V3.1 禁止 Agent 转授权（delegator 必须非 Agent 身份，§8.2）。
-module(agent_grant_domain).

-export([
    scope_kinds/0,
    validate_issue/9,
    effective_status/2,
    assert_revoke_allowed/1,
    validate_constraint/2,
    idempotency_verdict/2
]).

-export_type([scope_kind/0, effective_status/0, capability_req/0]).

-type scope_kind() :: none | explicit.
-type effective_status() :: pending | active | expired | revoked.
-type capability_req() :: #{
    capability := binary(),
    action := binary(),
    resource_type := binary(),
    constraint => #{binary() => binary() | integer() | [binary() | integer()]}
}.

%% ===================================================================
%% 枚举
%% ===================================================================

%% V3.1 冻结 workspace scope 枚举（§7.2 ck_ag_workspace_scope_kind）；
%% 通配 all 在 V3.1 禁止（§7.1 WORKSPACE SCOPE 契约）。
scope_kinds() ->
    [none, explicit].

%% ===================================================================
%% issue 入参裁决（§M.1；DB CHECK 是第二道防线）
%% ===================================================================

%% @doc issue 前置纯校验（DB 访问之前调用）。
%%
%% 返回 {ok, Normalized}（scope 规范为原子、workspace 排序去重、capability
%% 规范化）或 {error, Reason}：
%%
%%   * `validation_failed`：id 非正整数 / idempotency_key 非空 binary /
%%     capability 列表空或形状非法；
%%   * `invalid_workspace_scope`：explicit 但 workspace_ids 空（必须 ≥1 行）/
%%     none 但 workspace_ids 非空（必须零行）/ id 非正整数 / 重复；
%%   * `invalid_validity`：expires_at 必须严格晚于 valid_from（§7.2
%%%     ck_ag_validity；同刻亦拒绝）。
%%
%% capability 逐条：capability/action/resource_type 必须非空 binary（§7.2
%% ck_agc_*）；同一 grant 内三元组不得重复（DB PK (grant_id, capability,
%% action, resource_type) 的前置形态）。
validate_issue(
    OrgId,
    AgentId,
    DelegatorUserId,
    ScopeKind,
    WorkspaceIds,
    Capabilities,
    ValidFrom,
    ExpiresAt,
    IdemKey
) ->
    case
        validate_all([
            fun() ->
                validate_ids([
                    {organization_id, OrgId},
                    {agent_id, AgentId},
                    {delegator_user_id, DelegatorUserId}
                ])
            end,
            fun() -> validate_idem_key(IdemKey) end,
            fun() -> validate_window(ValidFrom, ExpiresAt) end,
            fun() -> validate_scope(ScopeKind, WorkspaceIds) end,
            fun() -> validate_capabilities(Capabilities) end
        ])
    of
        {ok, Results} ->
            %% validate_all 保序累积：三个 ok + scope + capabilities
            [ok, ok, ok, {ok, {Scope, WsIds}}, {ok, NormCaps}] = Results,
            {ok, #{
                organization_id => OrgId,
                agent_id => AgentId,
                delegator_user_id => DelegatorUserId,
                workspace_scope_kind => Scope,
                workspace_ids => WsIds,
                capabilities => NormCaps,
                valid_from => ValidFrom,
                expires_at => ExpiresAt,
                idempotency_key => IdemKey
            }};
        {error, _} = Err ->
            Err
    end.

validate_all(Funs) ->
    validate_all(Funs, []).

validate_all([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_all([F | Rest], Acc) ->
    case F() of
        ok -> validate_all(Rest, [ok | Acc]);
        {ok, Value} -> validate_all(Rest, [{ok, Value} | Acc]);
        {error, _} = Err -> Err
    end.

validate_idem_key(K) when is_binary(K), K =/= <<>> -> ok;
validate_idem_key(_) -> {error, validation_failed}.

validate_ids(Pairs) ->
    case [K || {K, V} <- Pairs, not is_pos_int(V)] of
        [] -> ok;
        _ -> {error, validation_failed}
    end.

is_pos_int(V) when is_integer(V), V > 0 -> true;
is_pos_int(_) -> false.

%% expires_at 严格大于 valid_from（同刻拒绝，与 ck_ag_validity 一致）。
validate_window(ValidFrom, ExpiresAt) when is_tuple(ValidFrom), is_tuple(ExpiresAt) ->
    case ExpiresAt > ValidFrom of
        true -> ok;
        false -> {error, invalid_validity}
    end;
validate_window(_V, _E) ->
    {error, invalid_validity}.

validate_scope(none, WorkspaceIds) when WorkspaceIds =:= []; WorkspaceIds =:= undefined ->
    {ok, {none, []}};
validate_scope(explicit, WorkspaceIds) when is_list(WorkspaceIds), WorkspaceIds =/= [] ->
    case
        lists:usort(WorkspaceIds) =:= WorkspaceIds andalso lists:all(fun is_pos_int/1, WorkspaceIds)
    of
        true -> {ok, {explicit, lists:sort(WorkspaceIds)}};
        false -> {error, invalid_workspace_scope}
    end;
validate_scope(explicit, _EmptyOrBad) ->
    %% explicit 必须 ≥1 行（§7.2 跨表条件）
    {error, invalid_workspace_scope};
validate_scope(<<"none">>, WorkspaceIds) ->
    validate_scope(none, WorkspaceIds);
validate_scope(<<"explicit">>, WorkspaceIds) ->
    validate_scope(explicit, WorkspaceIds);
validate_scope(_Bad, _WsIds) ->
    {error, invalid_workspace_scope}.

validate_capabilities(Capabilities) when is_list(Capabilities), Capabilities =/= [] ->
    case lists:all(fun is_capability_req/1, Capabilities) of
        false ->
            {error, validation_failed};
        true ->
            Norm = [
                #{
                    capability => maps:get(capability, C),
                    action => maps:get(action, C),
                    resource_type => maps:get(resource_type, C),
                    constraint => constraint_of(C)
                }
             || C <- Capabilities
            ],
            Triples = [
                {Ca, A, R}
             || #{capability := Ca, action := A, resource_type := R} <- Norm
            ],
            case length(Triples) =:= length(lists:usort(Triples)) of
                true -> {ok, Norm};
                false -> {error, validation_failed}
            end
    end;
validate_capabilities(_EmptyOrBad) ->
    %% 发行必须至少授予一条 capability（DEFAULT=DENY 下零 capability 的 Grant
    %% 无意义；避免空壳行污染账本）
    {error, validation_failed}.

is_capability_req(C) when is_map(C) ->
    NonEmptyB = fun(V) -> is_binary(V) andalso V =/= <<>> end,
    NonEmptyB(maps:get(capability, C, undefined)) andalso
        NonEmptyB(maps:get(action, C, undefined)) andalso
        NonEmptyB(maps:get(resource_type, C, undefined));
is_capability_req(_) ->
    false.

constraint_of(C) ->
    case maps:get(constraint, C, #{}) of
        M when is_map(M) -> M;
        _ -> #{}
    end.

%% ===================================================================
%% constraint 只收窄校验（§7.2 L320-321 + D7 契约 §3）
%% ===================================================================

%% @doc constraint_json 顶层键必须 ⊆ 目录条目 legal_constraint_keys，且取值
%% 只许标量/标量列表（不允许嵌套 map——防「否定后再由其它字段扩大」的表达面；
%% 未知 key 在发行阶段拒绝，不忽略）。
validate_constraint(undefined, _LegalKeys) ->
    ok;
validate_constraint(Constraint, LegalKeys) when is_map(Constraint), is_list(LegalKeys) ->
    Keys = lists:sort(maps:keys(Constraint)),
    case Keys -- lists:usort(LegalKeys) of
        [] ->
            values_shape_ok(Keys, Constraint);
        [Unknown | _] ->
            {error, {invalid_constraint, Unknown}}
    end;
validate_constraint(_Bad, _LegalKeys) ->
    {error, {invalid_constraint, <<>>}}.

values_shape_ok([], _Constraint) ->
    ok;
values_shape_ok([K | Rest], Constraint) ->
    case is_narrow_value(maps:get(K, Constraint)) of
        true -> values_shape_ok(Rest, Constraint);
        false -> {error, {invalid_constraint, K}}
    end.

is_narrow_value(V) when is_binary(V); is_integer(V) -> true;
is_narrow_value(V) when is_list(V) ->
    lists:all(fun(E) -> is_binary(E) orelse is_integer(E) end, V);
is_narrow_value(_) ->
    false.

%% ===================================================================
%% 有效态实时计算（§7.2 L291-292；Now 由调用方注入）
%% ===================================================================

%% @doc 存储态 + 注入时钟 → API 有效态。
%%
%% revoked 是终态，优先于一切时间判断；存储 active 时按时间窗实时算
%% pending（未生效）/ active（窗内）/ expired（已到期）。
effective_status(#{status := revoked}, _Now) ->
    revoked;
effective_status(#{status := active, valid_from := ValidFrom, expires_at := ExpiresAt}, Now) ->
    if
        Now < ValidFrom -> pending;
        Now >= ExpiresAt -> expired;
        true -> active
    end.

%% ===================================================================
%% revoke 迁移裁决（§M.3：仅 active→revoked）
%% ===================================================================

%% @doc 存储态必须 active 才可撤销；已 revoked → already_revoked（终态重复
%% 撤销拒绝，区别于 version_conflict）。pending（未生效但存储 active）允许
%% 撤销——撤销裁决针对**存储态**，撤一张尚未生效的 Grant 是合法治理动作。
assert_revoke_allowed(#{status := active}) ->
    ok;
assert_revoke_allowed(#{status := revoked}) ->
    {error, already_revoked};
assert_revoke_allowed(_Other) ->
    {error, not_found}.

%% ===================================================================
%% 幂等载荷比对（§M.1：同 key 同请求 → 既有 Grant；异载荷 → 冲突）
%% ===================================================================

%% @doc 比对本次请求指纹与既有 Grant 存储行（含子行）：全等 → same（返回
%% 既有）；任一差异 → conflict（拒绝）。
%%
%% 指纹字段：agent_id / workspace_scope_kind / workspace_ids（排序后）/
%% capability 三元组与 constraint（排序后）/ valid_from / expires_at。
%% delegator 与 org 是幂等键的唯一定位域，不参与比对。
idempotency_verdict(Request, Stored) when is_map(Stored) ->
    RequestFingerprint = fingerprint(Request),
    StoredFingerprint = stored_fingerprint(Stored),
    case RequestFingerprint =:= StoredFingerprint of
        true -> same;
        false -> conflict
    end;
idempotency_verdict(_Request, _NotStored) ->
    conflict.

fingerprint(Req) ->
    {
        maps:get(agent_id, Req, undefined),
        scope_atom(maps:get(workspace_scope_kind, Req, undefined)),
        lists:sort(maps:get(workspace_ids, Req, [])),
        caps_fingerprint(maps:get(capabilities, Req, [])),
        maps:get(valid_from, Req, undefined),
        maps:get(expires_at, Req, undefined)
    }.

stored_fingerprint(Stored) ->
    {
        maps:get(agent_id, Stored, undefined),
        scope_atom(maps:get(workspace_scope_kind, Stored, undefined)),
        lists:sort(maps:get(workspace_ids, Stored, [])),
        caps_fingerprint(maps:get(capabilities, Stored, [])),
        maps:get(valid_from, Stored, undefined),
        maps:get(expires_at, Stored, undefined)
    }.

%% Stored 侧 capabilities 由 repo 读出为 #{capability=>..,action=>..,
%% resource_type=>..,constraint=>Map}，与请求侧同形。
caps_fingerprint(Caps) ->
    lists:sort([
        {
            maps:get(capability, C, undefined),
            maps:get(action, C, undefined),
            maps:get(resource_type, C, undefined),
            sort_term(maps:get(constraint, C, #{}))
        }
     || C <- Caps, is_map(C)
    ]).

scope_atom(none) -> none;
scope_atom(explicit) -> explicit;
scope_atom(<<"none">>) -> none;
scope_atom(<<"explicit">>) -> explicit;
scope_atom(Other) -> Other.

%% constraint 的稳定比较形：键序固定（binary 键排序），值不深排（标量/标量列表）。
sort_term(M) when is_map(M) ->
    lists:sort([{K, V} || {K, V} <- maps:to_list(M), is_binary(K)]);
sort_term(Other) ->
    Other.
