-module(enterprise_org_member_repo).

%%%
% enterprise_org_member_repo 是 EPGZ-03（INT-02）绑定前置甄别仓储：
% 在 bind 之前一次性读出目标 user 在本 Org 的成员行 + 账号事实
% （member.status / user.account_type / user.status），供 logic 把
% 「目标非本 Org」与「非 active Human member」区分为两个 stable 错误
% （organization_boundary_violation vs identity_not_mapped）。
% DB 触发器 trg_enterprise_external_identity_member_guard（23514）仍是
% 终局守卫；本读法仅做先行分类（与触发器判定同源同字段）。
%%%

-export([find_membership_with_user_tx/3]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 事务内读 (Org, User) 的成员行 + 账号事实。
%% 无成员行 → {error, not_found}（目标不属于本 Org）。
%% 返回字段：member_status / role / account_type / user_status。
-spec find_membership_with_user_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_membership_with_user_tx(Conn, OrgId, UserId) when
    is_integer(OrgId), is_integer(UserId)
->
    Sql =
        <<"SELECT om.status AS member_status, om.role, ",
            "u.account_type, u.status AS user_status ", "FROM organization_member om ",
            "JOIN \"user\" u ON u.id = om.user_id ",
            "WHERE om.organization_id = $1 AND om.user_id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, UserId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.
