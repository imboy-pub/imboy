-module(organization_lifecycle_pg).

%% Organization archive/restore 读写 SQL（infrastructure，事务内使用）。
%%
%% 只封装语句与行集；锁序与业务裁决在 organization_lifecycle（application）。
%% 与既有治理写保持「组织行先、成员行后」的同一锁顺序（C04/ORG-01 口径）。
%% 不新增 DDL（M08 = NO_SEPARATE_DDL_BY_DEFAULT）：status 枚举沿用
%% 00000095 + 00000155 的 CHECK（active | archived | pending | rejected），
%% 归档时间复用 updated_at。

-export([
    lock_organization_tx/2,
    lock_member_role_tx/3,
    set_status_tx/3,
    set_review_status_tx/3
]).

-define(COLUMNS, <<"id,name,owner_id,status,branding,settings,created_at,updated_at">>).

-spec org_table() -> binary().
org_table() ->
    elib_pg_sql:public_tablename(<<"organization">>).

-spec member_table() -> binary().
member_table() ->
    elib_pg_sql:public_tablename(<<"organization_member">>).

%% 锁组织行并取生命周期裁决所需列。
-spec lock_organization_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
lock_organization_tx(Conn, OrgId) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (org_table())/binary, " WHERE id = $1 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId]).

%% 锁并读 actor 的 active 成员角色（组织行已先锁）。
-spec lock_member_role_tx(any(), integer(), integer()) ->
    {ok, binary()} | {error, not_found | term()}.
lock_member_role_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"SELECT role FROM ", (member_table())/binary,
            " WHERE organization_id = $1 AND user_id = $2 AND status = 'active'"
            " FOR SHARE">>,
    case elib_pg:query(Conn, Sql, [OrgId, Uid]) of
        {ok, [#{<<"role">> := Role} | _]} ->
            {ok, Role};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% 生命周期状态推进（幂等由调用方先锁行裁决）。
-spec set_status_tx(any(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
set_status_tx(Conn, OrgId, Status) when Status =:= <<"active">>; Status =:= <<"archived">> ->
    Sql =
        <<"UPDATE ", (org_table())/binary,
            " SET status = $1, updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $2 RETURNING ", ?COLUMNS/binary>>,
    one_tx(Conn, Sql, [Status, OrgId]);
set_status_tx(_Conn, _OrgId, _Other) ->
    {error, bad_status}.

%% 注册审核 CAS 推进（00000155）：仅 pending 行生效，返回行数 0/1——
%% 并发审核/重复点击天然幂等收敛（0 行 = 已被处理，调用方归一 409）。
%% TargetStatus 仅 active（approve）| rejected（reject）。
-spec set_review_status_tx(any(), integer(), binary()) ->
    {ok, 0 | 1} | {error, term()}.
set_review_status_tx(Conn, OrgId, TargetStatus) when
    TargetStatus =:= <<"active">>; TargetStatus =:= <<"rejected">>
->
    Sql =
        <<"UPDATE ", (org_table())/binary,
            " SET status = $1, updated_at = CURRENT_TIMESTAMP"
            " WHERE id = $2 AND status = 'pending'">>,
    case elib_pg:execute(Conn, Sql, [TargetStatus, OrgId]) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {ok, _, _} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end;
set_review_status_tx(_Conn, _OrgId, _Other) ->
    {error, bad_status}.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.
