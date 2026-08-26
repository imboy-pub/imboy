-module(workspace_member_repo).
%%%
% workspace_member_repo 是 workspace_member repository 缩写
% 工作区成员数据仓库层（迁移 00000076，双体验 v2.5.2 WP3/T4）
%
% 表结构：workspace_member(workspace_id, user_id, role owner|member|guest,
%   invited_by, joined_at, status active|removed, timestamps)
%   复合主键 (workspace_id, user_id) 即唯一约束；无独立 id 列（不产 TSID）。
% 术语：工作区成员（Workspace Member）≠ 群成员（Group Member）/频道订阅者（Channel Subscriber）。
%%%

-export([tablename/0]).
-export([insert_member_tx/3]).
-export([upsert_active_tx/5]).
-export([find/3]).
-export([find_tx/4]).
-export([list_by_workspace/2]).
-export([page_by_workspace/4]).
-export([count_by_role/2]).
-export([update_role_tx/4]).
-export([remove_tx/3]).
-export([list_active_workspace_groups_of_user/3]).
-export([owned_projects_of_user/3]).
-export([unfinished_tasks_of_user/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"workspace_member">>).

%% @doc 事务内插入工作区成员（Template 初始化：Owner 记录）
%% PK (workspace_id,user_id) 冲突由调用方幂等逻辑前置规避。
-spec insert_member_tx(any(), integer(), map()) -> ok | {error, term()}.
insert_member_tx(Conn, WsId, Data0) ->
    Tb = tablename(),
    Data = Data0#{<<"workspace_id">> => WsId},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _Count} -> ok;
        {error, _} = Err -> Err
    end.

%% @doc 事务内幂等邀请：插入或恢复 active（ON CONFLICT DO UPDATE）
%% 返回 {ok, changed} 表示状态真实变化；{ok, unchanged} 表示已是目标状态
%% （active 且角色相同 → 幂等；active 但角色不同 → 按调用方策略拒绝，不静默改角色）。
-spec upsert_active_tx(any(), integer(), integer(), binary(), integer() | nil) ->
    {ok, changed | unchanged | role_conflict, map()} | {error, term()}.
upsert_active_tx(Conn, WsId, Uid, Role, InvitedBy) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    %% 先取现行行：active+同角色 → unchanged；active+不同角色 → role_conflict
    case find_tx(Conn, WsId, Uid, <<"role,status">>) of
        #{<<"status">> := <<"active">>, <<"role">> := Role} ->
            {ok, unchanged, #{}};
        #{<<"status">> := <<"active">>} ->
            {ok, role_conflict, #{}};
        _ ->
            Sql =
                <<"INSERT INTO ", Tb/binary,
                    " (workspace_id, user_id, role, invited_by, joined_at, status, created_at, updated_at)",
                    " VALUES ($1, $2, $3, $4, $5, 'active', $6, $6)",
                    " ON CONFLICT (workspace_id, user_id) DO UPDATE",
                    " SET role = EXCLUDED.role, invited_by = EXCLUDED.invited_by,",
                    "     joined_at = EXCLUDED.joined_at, status = 'active', updated_at = EXCLUDED.updated_at">>,
            case elib_pg:execute(Conn, Sql, [WsId, Uid, Role, InvitedBy, Now, Now]) of
                {ok, 1} -> {ok, changed, #{}};
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 查询工作区成员行（自动提交连接；空 map = 无记录）
-spec find(integer(), integer(), binary()) -> map().
find(WsId, Uid, Column) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE workspace_id = $1 AND user_id = $2">>,
    case elib_pg:one(Sql, [WsId, Uid]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 事务内查询工作区成员行
-spec find_tx(any(), integer(), integer(), binary()) -> map().
find_tx(Conn, WsId, Uid, Column) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE workspace_id = $1 AND user_id = $2">>,
    case elib_pg:query(Conn, Sql, [WsId, Uid]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 工作区成员预览（Overview 用，JOIN user，稳定排序 role 主序）
-spec list_by_workspace(integer(), non_neg_integer()) -> {ok, [map()]} | {error, term()}.
list_by_workspace(WsId, Limit) ->
    Tb = tablename(),
    UTb = user_repo:tablename(),
    Sql =
        <<"SELECT wm.workspace_id, wm.user_id, wm.role, wm.joined_at,",
            " u.nickname, u.avatar, u.account", " FROM ", Tb/binary, " wm", " LEFT JOIN ",
            UTb/binary, " u ON u.id = wm.user_id",
            " WHERE wm.workspace_id = $1 AND wm.status = 'active'",
            " ORDER BY wm.joined_at ASC, wm.user_id ASC LIMIT $2">>,
    elib_pg:query(Sql, [WsId, Limit]).

%% @doc 工作区成员分页列表（成员管理页）
-spec page_by_workspace(integer(), integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
page_by_workspace(WsId, Page, Size, Column) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    CountSql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE workspace_id = $1 AND status = 'active'">>,
    Total =
        case elib_pg:one(CountSql, [WsId]) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    DataSql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " wm", " LEFT JOIN ",
            (user_repo:tablename())/binary, " u ON u.id = wm.user_id",
            " WHERE wm.workspace_id = $1 AND wm.status = 'active'",
            " ORDER BY wm.joined_at ASC, wm.user_id ASC LIMIT $2 OFFSET $3">>,
    case elib_pg:query(DataSql, [WsId, Size, Offset]) of
        {ok, Items} ->
            TotalPage =
                case Total > 0 of
                    true -> ((Total - 1) div Size) + 1;
                    false -> 0
                end,
            {ok, #{
                list => Items, page => Page, size => Size, total => Total, total_page => TotalPage
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 统计某角色 active 成员数（最后 Owner 保护用）
-spec count_by_role(integer(), binary()) -> non_neg_integer().
count_by_role(WsId, Role) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE workspace_id = $1 AND role = $2 AND status = 'active'">>,
    case elib_pg:one(Sql, [WsId, Role]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 事务内改角色（最后 Owner 保护由 logic 层前置）
-spec update_role_tx(any(), integer(), integer(), binary()) -> ok | {error, term()}.
update_role_tx(Conn, WsId, Uid, Role) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET role = $1, updated_at = $2",
            " WHERE workspace_id = $3 AND user_id = $4 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Role, Now, WsId, Uid]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, member_not_active};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内移除工作区成员（软删 status=removed）
%% 移除保护触发器 trg_workspace_member_remove_guard（00000077，可延迟）
%% 要求同事务先禁用下属 active 群成员，否则 COMMIT 拒绝——本函数只写父关系。
-spec remove_tx(any(), integer(), integer()) -> ok | {error, term()}.
remove_tx(Conn, WsId, Uid) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = 'removed', updated_at = $1",
            " WHERE workspace_id = $2 AND user_id = $3 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, WsId, Uid]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, member_not_active};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内列出用户在该工作区仍 active 的下属 workspace 群成员行
%% （移除流程的"受影响资源清单"来源；W0 下属关系 = scope='workspace' 群）
-spec list_active_workspace_groups_of_user(any(), integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_active_workspace_groups_of_user(Conn, WsId, Uid) ->
    Sql =
        <<"SELECT gm.id AS gm_id, gm.group_id, g.title", " FROM group_member gm",
            " JOIN \"group\" g ON g.id = gm.group_id",
            " WHERE g.workspace_id = $1 AND g.scope = 'workspace'",
            " AND gm.user_id = $2 AND gm.status = 1", " ORDER BY gm.group_id ASC">>,
    case elib_pg:query(Conn, Sql, [WsId, Uid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 移除冲突检查①：用户在本工作区仍为 Owner 的项目（W0：project.owner_id）
-spec owned_projects_of_user(any(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
owned_projects_of_user(Conn, WsId, Uid) ->
    Sql =
        <<"SELECT id, name FROM project", " WHERE workspace_id = $1 AND owner_id = $2",
            " ORDER BY id ASC">>,
    case elib_pg:query(Conn, Sql, [WsId, Uid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 移除冲突检查②：用户在本工作区未完成的任务（status != done）
-spec unfinished_tasks_of_user(any(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
unfinished_tasks_of_user(Conn, WsId, Uid) ->
    Sql =
        <<"SELECT t.id, t.title, t.status, t.project_id", " FROM project_task t",
            " JOIN project p ON p.id = t.project_id",
            " WHERE p.workspace_id = $1 AND t.assignee_id = $2", " AND t.status <> 'done'",
            " ORDER BY t.id ASC">>,
    case elib_pg:query(Conn, Sql, [WsId, Uid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
