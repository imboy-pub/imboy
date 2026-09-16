%%% @doc 企业业务身份与经办关系的应用层用例（EB-05）。
%%%
%%% 依据：plan v4.1 §8 EB-05、§2.1 #1/#3/#4、EB-D02 / EB-D03 / EB-D07 / EB-D11、
%%% 以及 `docs/architecture/feature-slice-rules.md` 铁律 4/6/7。
%%%
%%% **职责边界**
%%%   * 本模块实现「用例」：参数收敛 → domain 纯函数判定 → 经扩展点读写。
%%%   * 授权（active member / active assignment / permission / governance role）由
%%%     EB-04 的 `eb_auth_app` 在请求进入时判定；本模块**不**重造 RBAC，也不把
%%%     `function_key` 当权限使用（EB-D11）。本模块只判**业务前提**。
%%%   * 三分固定：owner = `organization_id`，assignee = `business_identity_id`，
%%%     actor = `actor_user_id`。任何把自然人 user 当 owner 的输入由
%%%     `eb_identity:owner_assignee_actor/1` 直接拒绝（domain 是语义唯一真源）。
%%%
%%% **数据访问**：全部经扩展点——`eb_store_port`（装配实现 `eb_infra_ports:store/0`
%%% → `eb_pg_store`）与 `eb_audit_port`。本模块零 SQL、零 `elib_pg`、不触
%%% `*_repo` / `*_ds`（`make arch-check` 会拦）。每个资源级调用前两个业务参数都是
%%% OrgId / WorkspaceId，Workspace 归属由 store 的同语句租户自检裁决。
%%%
%%% **可注入的端口**（与 EB-04 的 `authorize_via_port/3` 同思路）：`Params` 里给
%%% `store` / `audit` / `id` 键即可替换对应扩展点实现，未给则用 `eb_infra_ports`
%%% 的装配默认值。生产路径不需要注入。
%%%
%%% **EB-03R 补齐后的正向能力（本卡 E5-1 / E5-2 / E5-10 消费）**
%%%   * `list_identities/2`：走 `eb_store_port:list_identities/2`（P2）真列举，
%%%     只返回本 Org 行（SQL 同语句带 Org；Workspace 归属由 join 裁决）；
%%%     支持**键集**分页（`after_id` 严格 `id > 游标`、`limit`），不用 OFFSET。
%%%   * `bind_assignment/2` 在**需要新建 assignment 行**时走
%%%     `eb_store_port:insert_assignment/3`（P1）真 INSERT：这是「首次绑定」的
%%%     唯一可达路径（CAS `advance_assignment/5` 只能改既有行）。
%%%   * 「active member」事实源走**只读事实 Port** `eb_member_fact_port`
%%%     （P10；装配实现 `eb_member_fact_pg`）。事实 ≠ 授权结论：授权仍由 EB-04
%%%     的 `eb_auth_port` / `eb_auth_app` 逐请求判定，本模块只判业务前提。
%%%     `Params` 里的 `member_facts => fun/2` 仍可注入（测试与特殊装配用），
%%%     未注入时才回落到只读事实 Port。
-module(eb_identity_app).

-export([
    create_identity/2,
    list_identities/2,
    bind_assignment/2,
    end_assignment/2
]).

%% ===================================================================
%% identity：创建
%% ===================================================================

%% @doc 在 Org 下创建 sales / customer_service 业务身份（owner = Organization）。
%%
%% Params：
%%   workspace_id   必填整数（租户操作范围；归属由 store 同语句校验）
%%   function_key   必填，V1 仅 sales | customer_service（domain 判定）
%%   display_name   必填非空
%%   actor_user_id  可选；仅作审计快照（默认回落到 assigned_by / created_by_user_id）
-spec create_identity(integer(), map()) -> {ok, map()} | {error, term()}.
create_identity(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            create_identity_in(OrgId, WorkspaceId, Params)
    end;
create_identity(_OrgId, _Params) ->
    {error, {invalid_argument, create_identity}}.

create_identity_in(OrgId, WorkspaceId, Params) ->
    Input = #{
        organization_id => OrgId,
        function_key => maps:get(function_key, Params, undefined),
        display_name => maps:get(display_name, Params, undefined)
    },
    case eb_identity:new_identity(Input) of
        {error, _} = Err ->
            Err;
        {ok, Draft} ->
            case new_id(business_identity, Params) of
                {error, _} = Err ->
                    Err;
                {ok, IdentityId} ->
                    insert_identity(OrgId, WorkspaceId, Draft, IdentityId, Params)
            end
    end.

insert_identity(OrgId, WorkspaceId, Draft, IdentityId, Params) ->
    %% 三分语义（user 不得当 owner）由 domain 判定，app 只消费结论。
    TripleInput = Params#{
        organization_id => OrgId,
        business_identity_id => IdentityId,
        actor_user_id => actor_user_id(Params)
    },
    case eb_identity:owner_assignee_actor(TripleInput) of
        {error, _} = Err ->
            Err;
        {ok, Triple} ->
            Actor = maps:get(actor, Triple),
            FunctionKey = maps:get(function_key, Draft),
            Identity = #{
                id => IdentityId,
                function_key => FunctionKey,
                display_name => maps:get(display_name, Draft),
                created_by_user_id => Actor
            },
            case
                with_store(Params, fun(Store) ->
                    Store:insert_identity(OrgId, WorkspaceId, Identity)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    audit_result(
                        Params,
                        OrgId,
                        #{
                            resource_type => <<"organization_business_identity">>,
                            resource_id => IdentityId,
                            action => <<"business_identity.create">>,
                            business_identity_id => IdentityId,
                            actor_user_id => Actor,
                            detail => #{<<"function_key">> => FunctionKey}
                        },
                        Stored
                    )
            end
    end.

%% ===================================================================
%% identity：列举（E5-1）
%% ===================================================================

%% @doc 列举 Org 下的业务身份（§5.1 GET /business-identities）。
%%
%% 经 `eb_store_port:list_identities/2` 真落库回读：SQL 同语句带
%% `organization_id`，并用 `workspace` 做归属校验，因此**只可能返回本 Org 行**；
%% 跨 Org / Workspace 不匹配一律空列表（不是内部错误，也不报能力缺失）。
%%
%% 分页为**键集**语义：`after_id` 严格 `id > 游标`、`limit` 截断；
%% 不使用 OFFSET（键集下删除/插入不产生窗口漂移）。
%%
%% Params：
%%   workspace_id  必填整数
%%   after_id      可选整数（键集游标；严格 id > after_id）
%%   limit         可选非负整数
-spec list_identities(integer(), map()) -> {ok, [map()]} | {error, term()}.
list_identities(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            list_identities_in(OrgId, WorkspaceId, Params)
    end;
list_identities(_OrgId, _Params) ->
    {error, {invalid_argument, list_identities}}.

list_identities_in(OrgId, WorkspaceId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:list_identities(OrgId, WorkspaceId)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            {ok, keyset_page(Rows, Params)}
    end.

%% ===================================================================
%% assignment：绑定 / 结束
%% ===================================================================

%% @doc 把 active member 绑定到业务身份（assignee = business_identity_id）。
%%
%% 判定顺序（固定，使失败原因唯一可复现）：
%%   1. 租户参数（OrgId / workspace_id）必须成立 → 否则不触库；
%%   2. **成员事实**必须先判：suspended / removed / 非成员在任何租户数据被读写前被拒；
%%   3. identity 必须在同一 (Org, Workspace) 且 `status = active`；
%%   4. `function_key` **取自库里的 identity**（不采信入参）——调用方无法靠自报职能绕过
%%      基数约束，也无法把 identity 当权限用；
%%   5. 同 (Org, user, function_key) 已有 active ⇒ 拒绝（EB-D02 V1 基数）且零写入；
%%   6. identity 已有 active ⇒ 拒绝（同一 identity 同时最多一个 active）；
%%   7. identity 的历史经办人是同一 user 且已 ended ⇒ CAS `{ended,reopen} → active`；
%%      若是**别人** ⇒ 改经办人属 handover / offboarding（EB-D07），本用例拒绝；
%%   8. 需要新建 assignment 行 ⇒ 冻结 store 无此能力，显式 fail-closed。
%%
%% Params：
%%   workspace_id, identity_id, user_id  必填整数
%%   member_facts                        必填 fun/2：(OrgId, UserId) -> 状态/事实
%%   actor_user_id                       可选审计快照
-spec bind_assignment(integer(), map()) -> {ok, map()} | {error, term()}.
bind_assignment(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            bind_assignment_args(OrgId, WorkspaceId, Params)
    end;
bind_assignment(_OrgId, _Params) ->
    {error, {invalid_argument, bind_assignment}}.

bind_assignment_args(OrgId, WorkspaceId, Params) ->
    IdentityId = maps:get(identity_id, Params, undefined),
    UserId = maps:get(user_id, Params, undefined),
    case is_pos_int(IdentityId) of
        false ->
            {error, {invalid_business_identity_id, IdentityId}};
        true ->
            case is_pos_int(UserId) of
                false -> {error, {invalid_user_id, UserId}};
                true -> bind_member_gate(OrgId, WorkspaceId, IdentityId, UserId, Params)
            end
    end.

bind_member_gate(OrgId, WorkspaceId, IdentityId, UserId, Params) ->
    case member_status(OrgId, UserId, Params) of
        {error, _} = Err ->
            Err;
        {ok, active} ->
            bind_identity_gate(OrgId, WorkspaceId, IdentityId, UserId, Params);
        {ok, OtherStatus} ->
            {error, {member_not_active, OtherStatus}}
    end.

bind_identity_gate(OrgId, WorkspaceId, IdentityId, UserId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_identity(OrgId, WorkspaceId, IdentityId)
        end)
    of
        {error, not_found} ->
            {error, {identity_not_found, IdentityId}};
        {error, _} = Err ->
            Err;
        {ok, Identity} ->
            bind_status_gate(OrgId, WorkspaceId, IdentityId, UserId, Identity, Params)
    end.

bind_status_gate(OrgId, WorkspaceId, IdentityId, UserId, Identity, Params) ->
    case maps:get(status, Identity, undefined) of
        active ->
            FunctionKey = maps:get(function_key, Identity, undefined),
            bind_cardinality_gate(
                OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, Params
            );
        OtherStatus ->
            {error, {identity_not_active, OtherStatus}}
    end.

bind_cardinality_gate(OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, Params) ->
    case with_store(Params, fun(Store) -> Store:list_assignments(OrgId, WorkspaceId) end) of
        {error, _} = Err ->
            Err;
        {ok, Assignments} ->
            case has_active_for_user_function(Assignments, UserId, FunctionKey) of
                true ->
                    {error, {duplicate_active_user_function, {OrgId, UserId, FunctionKey}}};
                false ->
                    bind_existing_rows(
                        OrgId,
                        WorkspaceId,
                        IdentityId,
                        UserId,
                        FunctionKey,
                        rows_for_identity(Assignments, IdentityId),
                        Params
                    )
            end
    end.

bind_existing_rows(
    OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, [], Params
) ->
    %% 该 identity 从未有过经办关系 ⇒ **首次绑定**：必须 INSERT 一行
    %% （CAS `advance_assignment/5` 只能改既有行，做不到这件事）。
    bind_insert(OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, Params);
bind_existing_rows(OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, Rows, Params) ->
    case active_rows(Rows) of
        [Active | _] ->
            case maps:get(user_id, Active, undefined) of
                UserId -> {error, duplicate_occupation};
                OtherUserId -> {error, {identity_bound_to_other_member, OtherUserId}}
            end;
        [] ->
            case [Row || Row <- Rows, maps:get(user_id, Row, undefined) =:= UserId] of
                [] ->
                    %% 历史经办人是别人：改经办人属 offboarding / handover（EB-D07）
                    {error, {assignee_change_requires_offboarding, IdentityId}};
                [EndedRow | _] ->
                    bind_reopen(
                        OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, EndedRow, Params
                    )
            end
    end.

%% 首次绑定：INSERT 一行 active（P1）。`function_key` 取自库里的 identity，
%% 调用方无法自报职能绕过 `organization_business_identity_assignment` 上的
%% 唯一索引（`uq_obia_active_identity` / `uq_obia_active_user_function`）——
%% 并发下同一 identity 或同一 (user, function) 的第二个 active 会被 DB 裁决成
%% `{error, conflict}`，本模块翻译为 `duplicate_occupation`（不静默成功）。
bind_insert(OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, Params) ->
    case new_id(organization_business_identity_assignment, Params) of
        {error, _} = Err ->
            Err;
        {ok, AssignmentId} ->
            Row = #{
                id => AssignmentId,
                business_identity_id => IdentityId,
                function_key => FunctionKey,
                user_id => UserId,
                assigned_by => actor_user_id(Params)
            },
            case
                with_store(Params, fun(Store) ->
                    Store:insert_assignment(OrgId, WorkspaceId, Row)
                end)
            of
                {error, conflict} ->
                    {error, duplicate_occupation};
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    Actor = actor_user_id(Params),
                    audit_result(
                        Params,
                        OrgId,
                        #{
                            resource_type =>
                                <<"organization_business_identity_assignment">>,
                            resource_id => maps:get(id, Stored, AssignmentId),
                            action => <<"business_identity_assignment.bind">>,
                            business_identity_id => IdentityId,
                            actor_user_id => Actor,
                            detail => #{
                                <<"user_id">> => UserId,
                                <<"function_key">> => FunctionKey,
                                <<"mode">> => <<"insert">>
                            }
                        },
                        #{
                            identity_id => IdentityId,
                            user_id => UserId,
                            function_key => FunctionKey,
                            assignment_id => maps:get(id, Stored, AssignmentId),
                            status => active,
                            %% 新建行的真实版本（DB 默认 1）——CAS reopen 会 >1，
                            %% 故该字段也是「首次绑定走 INSERT」的机械判据。
                            version => maps:get(version, Stored, undefined),
                            mode => insert
                        }
                    )
            end
    end.

bind_reopen(OrgId, WorkspaceId, IdentityId, UserId, FunctionKey, EndedRow, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:advance_assignment(OrgId, WorkspaceId, IdentityId, {ended, reopen}, active)
        end)
    of
        {error, _} = Err ->
            Err;
        ok ->
            Actor = actor_user_id(Params),
            audit_result(
                Params,
                OrgId,
                #{
                    resource_type => <<"organization_business_identity_assignment">>,
                    resource_id => maps:get(id, EndedRow, undefined),
                    action => <<"business_identity_assignment.bind">>,
                    business_identity_id => IdentityId,
                    actor_user_id => Actor,
                    detail => #{
                        <<"user_id">> => UserId,
                        <<"function_key">> => FunctionKey,
                        <<"mode">> => <<"reopen">>
                    }
                },
                #{
                    identity_id => IdentityId,
                    user_id => UserId,
                    function_key => FunctionKey,
                    assignment_id => maps:get(id, EndedRow, undefined),
                    status => active,
                    mode => reopen
                }
            )
    end.

%% @doc 结束业务身份的 active 经办关系（CAS `active → ended`）。
%%
%% `user_id` 必须是该关系当前的发办人（assignee）；不匹配即拒绝且零写入——
%% 防止用「结束关系」隐式完成换人（换人属 EB-D07 的 handover / offboarding）。
-spec end_assignment(integer(), map()) -> {ok, map()} | {error, term()}.
end_assignment(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            end_assignment_args(OrgId, WorkspaceId, Params)
    end;
end_assignment(_OrgId, _Params) ->
    {error, {invalid_argument, end_assignment}}.

end_assignment_args(OrgId, WorkspaceId, Params) ->
    IdentityId = maps:get(identity_id, Params, undefined),
    UserId = maps:get(user_id, Params, undefined),
    Reason = maps:get(end_reason, Params, undefined),
    case is_pos_int(IdentityId) of
        false ->
            {error, {invalid_business_identity_id, IdentityId}};
        true ->
            case is_pos_int(UserId) of
                false ->
                    {error, {invalid_user_id, UserId}};
                true ->
                    case is_non_empty_binary(Reason) of
                        false ->
                            {error, {invalid_end_reason, Reason}};
                        true ->
                            end_assignment_identity(
                                OrgId, WorkspaceId, IdentityId, UserId, Reason, Params
                            )
                    end
            end
    end.

end_assignment_identity(OrgId, WorkspaceId, IdentityId, UserId, Reason, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_identity(OrgId, WorkspaceId, IdentityId)
        end)
    of
        {error, not_found} ->
            {error, {identity_not_found, IdentityId}};
        {error, _} = Err ->
            Err;
        {ok, Identity} ->
            case maps:get(status, Identity, undefined) of
                active ->
                    end_assignment_rows(OrgId, WorkspaceId, IdentityId, UserId, Reason, Params);
                OtherStatus ->
                    {error, {identity_not_active, OtherStatus}}
            end
    end.

end_assignment_rows(OrgId, WorkspaceId, IdentityId, UserId, Reason, Params) ->
    case with_store(Params, fun(Store) -> Store:list_assignments(OrgId, WorkspaceId) end) of
        {error, _} = Err ->
            Err;
        {ok, Assignments} ->
            Rows = active_rows(rows_for_identity(Assignments, IdentityId)),
            end_assignment_active(OrgId, WorkspaceId, IdentityId, UserId, Reason, Rows, Params)
    end.

end_assignment_active(_OrgId, _WorkspaceId, IdentityId, _UserId, _Reason, [], _Params) ->
    {error, {assignment_not_found, IdentityId}};
end_assignment_active(OrgId, WorkspaceId, IdentityId, UserId, Reason, [Active | _], Params) ->
    case maps:get(user_id, Active, undefined) of
        UserId ->
            end_assignment_cas(OrgId, WorkspaceId, IdentityId, UserId, Reason, Active, Params);
        OtherUserId ->
            {error, {assignee_mismatch, {IdentityId, UserId, OtherUserId}}}
    end.

end_assignment_cas(OrgId, WorkspaceId, IdentityId, UserId, Reason, Active, Params) ->
    %% 状态迁移合法性由 domain 判定（语义唯一真源），app 不另立一套。
    case eb_identity:valid_transition(assignment, {active, ended}) of
        {error, _} = Err ->
            Err;
        ok ->
            case
                with_store(Params, fun(Store) ->
                    Store:advance_assignment(OrgId, WorkspaceId, IdentityId, active, ended)
                end)
            of
                {error, _} = Err ->
                    Err;
                ok ->
                    Actor = actor_user_id(Params),
                    audit_result(
                        Params,
                        OrgId,
                        #{
                            resource_type =>
                                <<"organization_business_identity_assignment">>,
                            resource_id => maps:get(id, Active, undefined),
                            action => <<"business_identity_assignment.end">>,
                            business_identity_id => IdentityId,
                            actor_user_id => Actor,
                            detail => #{
                                <<"user_id">> => UserId,
                                <<"end_reason">> => Reason
                            }
                        },
                        #{
                            identity_id => IdentityId,
                            user_id => UserId,
                            assignment_id => maps:get(id, Active, undefined),
                            status => ended
                        }
                    )
            end
    end.

%% ===================================================================
%% 内部辅助：端口 / 租户 / 事实 / 审计
%% ===================================================================

%% 端口解析：Params 里的同键可注入实现（测试与装配用），否则用 eb_infra_ports 装配。
port(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(Key)
    end.

with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

new_id(Kind, Params) ->
    case port(id, Params) of
        {ok, IdPort} ->
            try
                {ok, IdPort:new_id(Kind)}
            catch
                Class:Reason ->
                    {error, {id_generation_failed, Kind, {Class, Reason}}}
            end;
        {error, _} = Err ->
            Err
    end.

%% 写入成功后追加 append-only 审计；审计失败如实报错，不假装成功。
audit_result(Params, OrgId, Event, Resource) ->
    case port(audit, Params) of
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {ok, AuditId} -> {ok, Resource#{audit_id => AuditId}};
                {error, Reason} -> {error, {audit_append_failed, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

tenant(OrgId, Params) ->
    case is_pos_int(OrgId) of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                Ws when is_integer(Ws), Ws > 0 -> {ok, Ws};
                Other -> {error, {invalid_workspace_id, Other}}
            end
    end.

%% actor 只作审计快照：显式 actor_user_id → assigned_by → created_by_user_id。
actor_user_id(Params) ->
    first_defined([
        maps:get(actor_user_id, Params, undefined),
        maps:get(assigned_by, Params, undefined),
        maps:get(created_by_user_id, Params, undefined)
    ]).

first_defined([]) ->
    undefined;
first_defined([undefined | Rest]) ->
    first_defined(Rest);
first_defined([Value | _Rest]) ->
    Value.

%% 成员事实（只读、逐请求）：E5-10 —— 默认走**只读事实 Port**
%% `eb_member_fact_port`（装配实现 `eb_member_fact_pg`）；`Params` 里的
%% `member_facts => fun/2`（形态对齐 EB-04 的 `eb_auth_port:load_request_facts/1`）
%% 仍可注入以替换事实源。两者都不可用时 fail-closed（不默认放行）。
%%
%% 事实 ≠ 授权结论：本函数只回答「成员关系当前是什么状态」，不回答「该请求
%% 是否有权做某事」；授权判定仍归 `eb_auth_port` + `eb_auth_app`。
member_status(OrgId, UserId, Params) ->
    case maps:get(member_facts, Params, undefined) of
        Loader when is_function(Loader, 2) ->
            normalize_member(Loader(OrgId, UserId));
        _NoInjection ->
            member_status_via_port(OrgId, UserId, Params)
    end.

member_status_via_port(OrgId, UserId, Params) ->
    case port(member_fact, Params) of
        {error, _} = Err ->
            Err;
        {ok, MemberFact} ->
            try
                normalize_member(MemberFact:member_status(OrgId, UserId))
            catch
                Class:Reason ->
                    {error, {member_fact_query_failed, {Class, Reason}}}
            end
    end.

normalize_member(Status) when Status =:= active; Status =:= suspended; Status =:= removed ->
    {ok, Status};
normalize_member(not_a_member) ->
    {ok, not_a_member};
normalize_member({ok, Status}) when is_atom(Status) ->
    {ok, Status};
normalize_member({ok, Facts}) when is_map(Facts) ->
    {ok, maps:get(status, Facts, unknown)};
%% 只读事实 Port 的「无成员关系」信号（`eb_member_fact_port` 契约）：
%% 归一为 not_a_member 后由成员门拒绝（fail-closed，不当成 active）。
normalize_member({error, no_member}) ->
    {ok, not_a_member};
normalize_member({error, _} = Err) ->
    Err;
normalize_member(Other) ->
    {error, {invalid_member_fact, Other}}.

%% ===================================================================
%% 内部辅助：assignment 事实
%% ===================================================================

active_rows(Rows) ->
    [Row || Row <- Rows, maps:get(status, Row, undefined) =:= active].

rows_for_identity(Rows, IdentityId) ->
    [Row || Row <- Rows, maps:get(business_identity_id, Row, undefined) =:= IdentityId].

has_active_for_user_function(Rows, UserId, FunctionKey) ->
    lists:any(
        fun(Row) ->
            maps:get(status, Row, undefined) =:= active andalso
                maps:get(user_id, Row, undefined) =:= UserId andalso
                maps:get(function_key, Row, undefined) =:= FunctionKey
        end,
        Rows
    ).

%% 键集分页（非 offset）：按 id 升序 → 严格 `id > after_id` → `limit` 截断。
%% 键集语义下分页之间插入/删除行不会让窗口漂移（offset 会）。
keyset_page(Rows, Params) ->
    Sorted = lists:sort(
        fun(A, B) -> maps:get(id, A, 0) =< maps:get(id, B, 0) end,
        Rows
    ),
    After = maps:get(after_id, Params, undefined),
    Filtered =
        case is_pos_int(After) of
            true -> [Row || Row <- Sorted, maps:get(id, Row, 0) > After];
            false -> Sorted
        end,
    case maps:get(limit, Params, undefined) of
        Limit when is_integer(Limit), Limit >= 0 -> lists:sublist(Filtered, Limit);
        _NoLimit -> Filtered
    end.

is_pos_int(Value) ->
    is_integer(Value) andalso Value > 0.

is_non_empty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
