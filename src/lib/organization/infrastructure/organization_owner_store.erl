-module(organization_owner_store).

%% Organization Owner 读写 SQL（infrastructure，事务内使用）。
%%
%% 只封装语句与行集；锁序与业务裁决在 organization_owner_transfer。
%% 所有查询显式贯穿 organization_id（org 作用域），锁定行用 FOR UPDATE
%% 与既有治理写路径保持「组织行先、成员行后」的同一锁顺序。

-export([
    lock_organization_tx/2,
    lock_member_with_account_tx/3,
    demote_previous_owner_tx/3,
    promote_target_tx/3,
    update_owner_projection_tx/3
]).

-spec lock_organization_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
lock_organization_tx(Conn, OrgId) ->
    Sql =
        <<"SELECT id, owner_id, status FROM ", (org_table())/binary, " WHERE id = $1 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId]).

%% 成员行 + 账号类型一次取齐并锁定成员行：
%% account_type 在锁内读取，使「目标是否 Human」的裁决与变更串行化。
-spec lock_member_with_account_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
lock_member_with_account_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"SELECT om.role, om.status, u.account_type FROM ", (member_table())/binary, " om JOIN ",
            (user_table())/binary, " u ON u.id = om.user_id",
            " WHERE om.organization_id = $1 AND om.user_id = $2 FOR UPDATE OF om">>,
    one_tx(Conn, Sql, [OrgId, Uid]).

%% 降级旧 owner：owner -> admin（deferred guard 在提交时以 owner_id 最终值裁决）。
-spec demote_previous_owner_tx(any(), integer(), integer()) -> ok | {error, term()}.
demote_previous_owner_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"UPDATE ", (member_table())/binary, " SET role = 'admin', updated_at = CURRENT_TIMESTAMP",
            " WHERE organization_id = $1 AND user_id = $2",
            " AND role = 'owner' AND status = 'active'">>,
    execute_one(Conn, Sql, [OrgId, Uid]).

%% 升级新 owner：role -> owner（status 保持 active）。
%% 唯一索引 uq_organization_member_single_active_owner 保证此刻旧 owner 已降级。
-spec promote_target_tx(any(), integer(), integer()) -> ok | {error, term()}.
promote_target_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"UPDATE ", (member_table())/binary, " SET role = 'owner', updated_at = CURRENT_TIMESTAMP",
            " WHERE organization_id = $1 AND user_id = $2",
            " AND status = 'active' AND role IN ('admin','member')">>,
    execute_one(Conn, Sql, [OrgId, Uid]).

%% owner_id 兼容投影更新（C04：投影唯一写入口 = transfer command）。
-spec update_owner_projection_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
update_owner_projection_tx(Conn, OrgId, NewOwnerUid) ->
    Sql =
        <<"UPDATE ", (org_table())/binary, " SET owner_id = $1, updated_at = CURRENT_TIMESTAMP",
            " WHERE id = $2 RETURNING id, owner_id, status">>,
    one_tx(Conn, Sql, [NewOwnerUid, OrgId]).

%%--------------------------------------------------------------------
-spec org_table() -> binary().
org_table() ->
    elib_pg_sql:public_tablename(<<"organization">>).

-spec member_table() -> binary().
member_table() ->
    elib_pg_sql:public_tablename(<<"organization_member">>).

-spec user_table() -> binary().
user_table() ->
    elib_pg_sql:public_tablename(<<"user">>).

-spec one_tx(any(), binary(), list()) -> {ok, map()} | {error, not_found | term()}.
one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec execute_one(any(), binary(), list()) -> ok | {error, term()}.
execute_one(Conn, Sql, Params) ->
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1} -> ok;
        {ok, _} -> {error, affected_rows_mismatch};
        {error, Reason} -> {error, Reason}
    end.
