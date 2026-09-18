-module(organization_preflight_facts_pg).

%% Organization 域 DeletionPreflightFacts provider（infrastructure，只读）。
%%
%% Core Contract C17 + 计划 §1.6：providers 只读、不做 handover/offboarding，
%% 返回不含 PII/credential/内部 SQL，resource_id 为 opaque string。
%%
%% 本模块同时供给两个域（ORG-02 任务卡冻结的只读消费口径）：
%%   * facts_organization/1 —— organization 域：
%%       ORG_OWNER_ACTIVE      active Human owner membership（真源
%%                             organization_member.role='owner' AND status='active'，
%%                             与 organization.owner_id 投影一致由 00000126 invariant 保证）
%%       ORG_MEMBERSHIP_ACTIVE active 非 owner 成员 membership
%%     不按 organization.status 过滤：C16 archive 不改成员状态，且
%%     fk_organization_owner 的 ON DELETE RESTRICT 与 org 是否 archived 无关，
%%     facts 必须与最终 DB guard 行为一致（否则 preflight 放行、删除仍被拒）。
%%   * facts_workspace/1 —— workspace 域（对 workspace 表 owner 字段的只读代查，
%%     不是改 workspace 域）：
%%       WORKSPACE_OWNER_ACTIVE 用户名下任一 workspace 的 owner（不限 status：
%%       archived workspace 的 owner 关系仍存在，闭合与否由 workspace 域裁决）
%%
%% 返回形状（逐字段冻结，计划 §1.6 DeletionPreflightFacts）：
%%   {ok, #{subject_user_id => UserId, domain => Domain, observed_at => Ms,
%%          fact_version => 1, blockers => [#{code => B, resource_type => T,
%%          resource_id => OpaqueId, organization_id => OrgIdOrNull}]}}
%%   | {error, unavailable}

-export([facts_organization/1, facts_workspace/1]).
-export([fact_version/0]).

-define(FACT_VERSION, 1).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec facts_organization(integer()) -> {ok, map()} | {error, unavailable}.
facts_organization(UserId) when is_integer(UserId), UserId > 0 ->
    case
        elib_pg:query(
            <<"SELECT organization_id, role FROM organization_member",
                " WHERE user_id = $1 AND status = 'active'",
                " AND role IN ('owner','member','admin') ORDER BY organization_id">>,
            [UserId]
        )
    of
        {ok, Rows} ->
            Blockers = org_blockers(Rows),
            {ok, fact(UserId, organization, Blockers)};
        {error, _Reason} ->
            {error, unavailable}
    end;
facts_organization(_) ->
    {error, unavailable}.

-spec facts_workspace(integer()) -> {ok, map()} | {error, unavailable}.
facts_workspace(UserId) when is_integer(UserId), UserId > 0 ->
    case
        elib_pg:query(
            <<"SELECT id, organization_id FROM workspace WHERE owner_id = $1 ORDER BY id">>,
            [UserId]
        )
    of
        {ok, Rows} ->
            Blockers = [
                #{
                    code => <<"WORKSPACE_OWNER_ACTIVE">>,
                    resource_type => <<"workspace">>,
                    resource_id => opaque(maps:get(<<"id">>, Row)),
                    organization_id => maybe_org_id(maps:get(<<"organization_id">>, Row, null))
                }
             || Row <- Rows
            ],
            {ok, fact(UserId, workspace, Blockers)};
        {error, _Reason} ->
            {error, unavailable}
    end;
facts_workspace(_) ->
    {error, unavailable}.

-spec fact_version() -> pos_integer().
fact_version() ->
    ?FACT_VERSION.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

fact(UserId, Domain, Blockers) ->
    #{
        subject_user_id => UserId,
        domain => Domain,
        observed_at => erlang:system_time(millisecond),
        fact_version => ?FACT_VERSION,
        blockers => Blockers
    }.

%% owner 行 → ORG_OWNER_ACTIVE；非 owner active 成员 → ORG_MEMBERSHIP_ACTIVE
org_blockers(Rows) ->
    lists:map(
        fun(#{<<"organization_id">> := OrgId, <<"role">> := Role}) ->
            Code =
                case Role of
                    <<"owner">> -> <<"ORG_OWNER_ACTIVE">>;
                    _ -> <<"ORG_MEMBERSHIP_ACTIVE">>
                end,
            ResourceType =
                case Role of
                    <<"owner">> -> <<"organization">>;
                    _ -> <<"organization_member">>
                end,
            #{
                code => Code,
                resource_type => ResourceType,
                resource_id => opaque(OrgId),
                organization_id => OrgId
            }
        end,
        Rows
    ).

opaque(Id) when is_integer(Id) ->
    integer_to_binary(Id);
opaque(Id) when is_binary(Id) ->
    Id.

maybe_org_id(null) ->
    null;
maybe_org_id(Id) when is_integer(Id) ->
    Id.
