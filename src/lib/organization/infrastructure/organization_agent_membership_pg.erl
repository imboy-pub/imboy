-module(organization_agent_membership_pg).

%% Agent 成员生命周期命令读写 SQL（infrastructure，事务内使用，ORG-06）。
%%
%% Agent membership 仍在 `organization_member`（C12：Human/Agent 共表，
%% 不建第二套 membership）。只封装语句与行集；锁序与裁决在
%% organization_agent_membership_app（application）+ organization_agent_boundary
%% （domain）。无新 DDL：写路径是通用状态迁移 UPDATE / INSERT。
%%
%% 锁顺序（与 owner transfer / lifecycle 同口径）：组织行先（FOR UPDATE）、
%% 操作人成员行 FOR SHARE、目标成员行 FOR UPDATE。
%% fact_version 与读路径同一投影（updated_at → epoch 微秒，见
%% organization_agent_facts_pg:?FACT_VERSION_EXPR），保证命令前后版本可比。

-export([
    lock_operator_tx/3,
    subject_account_type_tx/2,
    lock_subject_tx/3,
    insert_member_tx/4,
    activate_member_tx/4,
    set_status_tx/5
]).

%% fact_version 与读路径同一投影（updated_at → epoch 微秒，见
%% organization_agent_facts_pg:?FACT_VERSION_EXPR），保证命令前后版本可比。
-define(FACT_VERSION_EXPR,
    <<"COALESCE((EXTRACT(EPOCH FROM updated_at) * 1000000)::bigint, 0) AS fact_version">>
).

-define(COLUMNS, <<"role, status, ", (?FACT_VERSION_EXPR)/binary>>).

%% ------------------------------------------------------------------
%% 锁
%% ------------------------------------------------------------------

%% @doc 锁并读操作人的 active 成员角色 + Human 判定（组织行已被先锁）。
-spec lock_operator_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
lock_operator_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"SELECT om.role AS role, u.account_type AS account_type FROM ", (member_table())/binary,
            " om JOIN ", (user_table())/binary, " u ON u.id = om.user_id",
            " WHERE om.organization_id = $1 AND om.user_id = $2 AND om.status = 'active'"
            " FOR SHARE">>,
    one_tx(Conn, Sql, [OrgId, Uid]).

%% @doc 目标主体身份类型（Agent 命令仅接受 account_type=1；裁决在 application）。
-spec subject_account_type_tx(any(), integer()) ->
    {ok, integer()} | {error, not_found | term()}.
subject_account_type_tx(Conn, Uid) ->
    Sql = <<"SELECT account_type FROM ", (user_table())/binary, " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [Uid]) of
        {ok, [Row | _]} ->
            {ok, elib_cnv:safe_to_integer(maps:get(<<"account_type">>, Row))};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 锁并读目标成员行（**不带 status 过滤**：幂等/OCC 裁决需要真实状态；
%% 行不存在返回 not_found，调用方以 fact_version=0 语义处理）。
-spec lock_subject_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
lock_subject_tx(Conn, OrgId, Uid) ->
    Sql =
        <<"SELECT role, status, ", (?FACT_VERSION_EXPR)/binary, " FROM ", (member_table())/binary,
            " WHERE organization_id = $1 AND user_id = $2 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId, Uid]).

%% ------------------------------------------------------------------
%% 写（事务内；调用方已持组织行锁与目标行锁）
%% ------------------------------------------------------------------

%% @doc attach：Agent 成员行**只以 role='member' 创建**（member-only 冻结语义，
%% 不存在任何能写 owner/admin 的命令入口）。
-spec insert_member_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
insert_member_tx(Conn, OrgId, Uid, InvitedBy) ->
    Sql =
        <<"INSERT INTO ", (member_table())/binary,
            " (organization_id, user_id, role, invited_by, joined_at, status,"
            " created_at, updated_at)"
            " VALUES ($1, $2, 'member', $3, CURRENT_TIMESTAMP, 'active',"
            " CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
            " RETURNING ", ?COLUMNS/binary>>,
    one_tx(Conn, Sql, [OrgId, Uid, InvitedBy]).

%% @doc attach：从 suspended/removed 恢复为 active member（显式重投，角色强制 member）。
-spec activate_member_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
activate_member_tx(Conn, OrgId, Uid, InvitedBy) ->
    Sql =
        <<"UPDATE ", (member_table())/binary,
            " SET role = 'member', status = 'active', invited_by = $3,"
            " joined_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP"
            " WHERE organization_id = $1 AND user_id = $2 AND status IN ('suspended','removed')"
            " RETURNING ", ?COLUMNS/binary>>,
    one_tx(Conn, Sql, [OrgId, Uid, InvitedBy]).

%% @doc 精确单列状态迁移：WHERE 带 expected 当前状态（行锁已持，双保险，
%% 并发重复迁移影响行数≠1 即失败，不静默成功）。
-spec set_status_tx(any(), integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, not_active | term()}.
set_status_tx(Conn, OrgId, Uid, ExpectedStatus, TargetStatus) ->
    Sql =
        <<"UPDATE ", (member_table())/binary,
            " SET status = $3, updated_at = CURRENT_TIMESTAMP"
            " WHERE organization_id = $1 AND user_id = $2 AND status = $4"
            " RETURNING ", ?COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [OrgId, Uid, TargetStatus, ExpectedStatus]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_active};
        {error, Reason} ->
            {error, Reason}
    end.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

-spec one_tx(any(), binary(), list()) -> {ok, map()} | {error, not_found | term()}.
one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec user_table() -> binary().
user_table() ->
    elib_pg_sql:public_tablename(<<"user">>).

-spec member_table() -> binary().
member_table() ->
    elib_pg_sql:public_tablename(<<"organization_member">>).
