-module(organization_agent_facts_app).

%% Agent Organization Boundary 公共事实适配器（application，ORG-06）。
%%
%% Agent V3.1 唯一允许消费的 Organization 侧读合同
%% （Agent Organization Contract §8，FROZEN_ORGANIZATION_SIDE_CONTRACT）：
%%   resolve membership / resolve workspace membership /
%%   resolve organization state / validate workspace ownership /
%%   consume default workspace。
%% Agent Domain 不得直读 Organization 表——本适配器即受控边界。
%%
%% 冻结语义：
%%   * facts 返回 versioned（fact_version 锚定行 updated_at 投影）+ observed_at；
%%   * archived / suspended / removed 一律 fail closed（allowed=false，
%%     裁决在 organization_agent_boundary domain，单一决策真源）；
%%   * 重复 fact read 零副作用（纯 SELECT，不写审计、不改版本）；
%%   * 事实如实上报真实 status/role（含非 active 态）——消费方据 allowed
%%     与 status 做确定性 fail closed（AG-ORG-A01/A02/A03/A05/A10 的
%%     Organization 侧事实由本适配器确定性提供）；
%%   * default workspace 是**指路事实而非权限事实**（C05）：仅可作为
%%     Run 创建的候选解析输入，fact 不携带 allowed 授权语义；
%%   * 返回不含 PII/credential/内部 SQL。
%%
%% 运行环境：Agent membership 产品入口在通用 Agent registry/Grant ready 前
%% 默认关闭（C12 TRANSITION）；本适配器不提供任何 HTTP 端点。

-export([
    fact_version/0,
    resolve_organization/1,
    resolve_membership/2,
    resolve_workspace_membership/2,
    validate_workspace_in_org/2,
    consume_default_workspace/1
]).

%% ===================================================================
%% facts 合同版本
%% ===================================================================

-spec fact_version() -> pos_integer().
fact_version() ->
    organization_agent_boundary:fact_version().

%% ===================================================================
%% resolve organization state（§8：active/archived + version）
%% ===================================================================

-spec resolve_organization(integer()) -> {ok, map()} | {error, {integer(), binary()}}.
resolve_organization(OrgId) ->
    case valid_id(OrgId) of
        false ->
            {error, {400, invalid_org_id_msg()}};
        true ->
            case organization_agent_facts_pg:organization_row(OrgId) of
                {ok, Row} ->
                    {ok, org_fact(OrgId, Row)};
                {error, not_found} ->
                    {error, {404, org_not_found_msg()}};
                {error, _Reason} ->
                    {error, internal_error_msg()}
            end
    end.

%% ===================================================================
%% resolve membership（§8：status, role(member), version）
%% ===================================================================

-spec resolve_membership(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
resolve_membership(OrgId, AgentId) ->
    case precheck_subject(OrgId, AgentId) of
        {error, _} = Error ->
            Error;
        {ok, OrgRow} ->
            case organization_agent_facts_pg:membership_row(OrgId, AgentId) of
                {ok, Row} ->
                    {ok, membership_fact(OrgId, AgentId, OrgRow, Row)};
                {error, not_found} ->
                    {error, {404, not_a_member_msg()}};
                {error, _Reason} ->
                    {error, internal_error_msg()}
            end
    end.

%% ===================================================================
%% resolve workspace membership（§8：status, role, version）
%% ===================================================================

-spec resolve_workspace_membership(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
resolve_workspace_membership(WsId, AgentId) ->
    case valid_id(WsId) andalso valid_id(AgentId) of
        false ->
            {error, {400, invalid_ws_or_agent_id_msg()}};
        true ->
            case organization_agent_facts_pg:workspace_row(WsId) of
                {error, not_found} ->
                    {error, {404, ws_not_found_msg()}};
                {error, _Reason} ->
                    {error, internal_error_msg()};
                {ok, WsRow} ->
                    case ensure_agent(AgentId) of
                        {error, _} = Error ->
                            Error;
                        ok ->
                            case
                                organization_agent_facts_pg:workspace_membership_row(
                                    WsId, AgentId
                                )
                            of
                                {ok, Row} ->
                                    {ok, ws_membership_fact(WsId, AgentId, WsRow, Row)};
                                {error, not_found} ->
                                    {error, {404, not_a_ws_member_msg()}};
                                {error, _Reason} ->
                                    {error, internal_error_msg()}
                            end
                    end
            end
    end.

%% ===================================================================
%% validate workspace ownership（§8：same-org boolean/fact version）
%% ===================================================================

-spec validate_workspace_in_org(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
validate_workspace_in_org(OrgId, WsId) ->
    case valid_id(OrgId) andalso valid_id(WsId) of
        false ->
            {error, {400, invalid_org_or_ws_id_msg()}};
        true ->
            case organization_agent_facts_pg:organization_row(OrgId) of
                {error, not_found} ->
                    {error, {404, org_not_found_msg()}};
                {error, _Reason} ->
                    {error, internal_error_msg()};
                {ok, _OrgRow} ->
                    case organization_agent_facts_pg:workspace_row(WsId) of
                        {error, not_found} ->
                            {error, {404, ws_not_found_msg()}};
                        {error, _Reason2} ->
                            {error, internal_error_msg()};
                        {ok, WsRow} ->
                            {ok, ws_ownership_fact(OrgId, WsRow)}
                    end
            end
    end.

%% ===================================================================
%% consume default workspace（§8：nullable workspace_id；only at Run creation）
%% ===================================================================

%% @doc 只读消费 ORG-05 显式默认关系（organization_default_workspace_app:get/1，
%% 不回落 min-ID 推导）。指路事实：不带 allowed 授权语义。
-spec consume_default_workspace(integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
consume_default_workspace(OrgId) ->
    case valid_id(OrgId) of
        false ->
            {error, {400, invalid_org_id_msg()}};
        true ->
            case organization_agent_facts_pg:organization_row(OrgId) of
                {error, not_found} ->
                    {error, {404, org_not_found_msg()}};
                {error, _Reason} ->
                    {error, internal_error_msg()};
                {ok, OrgRow} ->
                    WsId =
                        case organization_default_workspace_app:get(OrgId) of
                            {ok, Id} -> Id;
                            {error, not_set} -> null;
                            {error, _Reason2} -> null
                        end,
                    {ok, #{
                        kind => default_workspace,
                        organization_id => OrgId,
                        workspace_id => WsId,
                        usage => run_creation_candidate_only,
                        fact_version => fact_version_of(OrgRow),
                        observed_at => observed_at()
                    }}
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 公共预检：id 形状 + Org 存在 + 目标必须是 Agent 身份。
-spec precheck_subject(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
precheck_subject(OrgId, AgentId) ->
    case valid_id(OrgId) andalso valid_id(AgentId) of
        false ->
            {error, {400, invalid_org_or_agent_id_msg()}};
        true ->
            case organization_agent_facts_pg:organization_row(OrgId) of
                {error, not_found} ->
                    {error, {404, org_not_found_msg()}};
                {error, _Reason} ->
                    {error, internal_error_msg()};
                {ok, OrgRow} ->
                    case ensure_agent(AgentId) of
                        ok ->
                            {ok, OrgRow};
                        {error, _} = Error ->
                            Error
                    end
            end
    end.

-spec ensure_agent(integer()) -> ok | {error, {integer(), binary()}}.
ensure_agent(AgentId) ->
    case organization_agent_facts_pg:account_type(AgentId) of
        {ok, AccountType} ->
            case organization_agent_boundary:ensure_agent_identity(AccountType) of
                ok ->
                    ok;
                {error, not_agent} ->
                    {error, {403, not_agent_msg()}}
            end;
        {error, not_found} ->
            {error, {404, subject_not_found_msg()}};
        {error, _Reason} ->
            {error, internal_error_msg()}
    end.

-spec org_fact(integer(), map()) -> map().
org_fact(OrgId, Row) ->
    Status = maps:get(<<"status">>, Row),
    #{
        kind => organization_state,
        organization_id => OrgId,
        status => Status,
        allowed => organization_agent_boundary:organization_allowed(Status),
        fact_version => fact_version_of(Row),
        observed_at => observed_at()
    }.

-spec membership_fact(integer(), integer(), map(), map()) -> map().
membership_fact(OrgId, AgentId, OrgRow, Row) ->
    OrgStatus = maps:get(<<"status">>, OrgRow),
    Status = maps:get(<<"status">>, Row),
    Role = maps:get(<<"role">>, Row),
    #{
        kind => organization_membership,
        organization_id => OrgId,
        subject_user_id => AgentId,
        status => Status,
        role => Role,
        org_status => OrgStatus,
        allowed =>
            organization_agent_boundary:membership_allowed(OrgStatus, Status, Role),
        fact_version => fact_version_of(Row),
        observed_at => observed_at()
    }.

-spec ws_membership_fact(integer(), integer(), map(), map()) -> map().
ws_membership_fact(WsId, AgentId, WsRow, Row) ->
    WsStatus = maps:get(<<"status">>, WsRow),
    Status = maps:get(<<"status">>, Row),
    Role = maps:get(<<"role">>, Row),
    #{
        kind => workspace_membership,
        workspace_id => WsId,
        subject_user_id => AgentId,
        status => Status,
        role => Role,
        workspace_status => WsStatus,
        workspace_organization_id => maybe_org_id(maps:get(<<"organization_id">>, WsRow, null)),
        allowed => organization_agent_boundary:workspace_membership_allowed(WsStatus, Status),
        fact_version => fact_version_of(Row),
        observed_at => observed_at()
    }.

-spec ws_ownership_fact(integer(), map()) -> map().
ws_ownership_fact(OrgId, WsRow) ->
    RowOrgId = maps:get(<<"organization_id">>, WsRow, null),
    WsStatus = maps:get(<<"status">>, WsRow),
    SameOrg = is_integer(RowOrgId) andalso RowOrgId =:= OrgId,
    #{
        kind => workspace_ownership,
        organization_id => OrgId,
        workspace_id => maps:get(<<"id">>, WsRow),
        same_org => SameOrg,
        workspace_status => WsStatus,
        allowed => organization_agent_boundary:workspace_ownership_allowed(SameOrg, WsStatus),
        fact_version => fact_version_of(WsRow),
        observed_at => observed_at()
    }.

-spec fact_version_of(map()) -> non_neg_integer().
fact_version_of(Row) ->
    elib_cnv:safe_to_integer(maps:get(<<"fact_version">>, Row, 0)).

-spec observed_at() -> integer().
observed_at() ->
    %% domain 纯净：时间由 application 供给（organization_agent_boundary 不取时间）。
    erlang:system_time(millisecond).

-spec maybe_org_id(null | integer()) -> null | integer().
maybe_org_id(null) ->
    null;
maybe_org_id(Id) when is_integer(Id) ->
    Id;
maybe_org_id(Id) when is_binary(Id) ->
    elib_cnv:safe_to_integer(Id).

-spec valid_id(term()) -> boolean().
valid_id(Id) when is_integer(Id), Id > 0 ->
    true;
valid_id(_) ->
    false.

-spec org_not_found_msg() -> binary().
org_not_found_msg() ->
    <<"Organization 不存在"/utf8>>.

-spec ws_not_found_msg() -> binary().
ws_not_found_msg() ->
    <<"Workspace 不存在"/utf8>>.

-spec subject_not_found_msg() -> binary().
subject_not_found_msg() ->
    <<"主体用户不存在"/utf8>>.

-spec not_agent_msg() -> binary().
not_agent_msg() ->
    <<"事实解析主体必须是 Agent 身份（account_type=1）"/utf8>>.

-spec not_a_member_msg() -> binary().
not_a_member_msg() ->
    <<"Agent 不是该 Organization 成员"/utf8>>.

-spec not_a_ws_member_msg() -> binary().
not_a_ws_member_msg() ->
    <<"Agent 不是该 Workspace 成员"/utf8>>.

-spec invalid_org_id_msg() -> binary().
invalid_org_id_msg() ->
    <<"organization_id 必须是正整数"/utf8>>.

-spec invalid_org_or_agent_id_msg() -> binary().
invalid_org_or_agent_id_msg() ->
    <<"organization_id 和 agent_id 必须是正整数"/utf8>>.

-spec invalid_ws_or_agent_id_msg() -> binary().
invalid_ws_or_agent_id_msg() ->
    <<"workspace_id 和 agent_id 必须是正整数"/utf8>>.

-spec invalid_org_or_ws_id_msg() -> binary().
invalid_org_or_ws_id_msg() ->
    <<"organization_id 和 workspace_id 必须是正整数"/utf8>>.

-spec internal_error_msg() -> {500, binary()}.
internal_error_msg() ->
    {500, <<"事实读取失败，请稍后重试"/utf8>>}.
