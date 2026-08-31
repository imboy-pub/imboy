-module(project_member_repo).
%%%
% project_member_repo 是 project_member repository 缩写
% 项目成员数据仓库层（迁移 00000081，Channel-first-class W2 ZC-02）
%
% 表结构：project_member(workspace_id 冗余, project_id, user_id, invited_by
%   可空, joined_at, status active|removed, timestamps)
%   复合主键 (project_id, user_id) 即唯一约束；无独立 id 列（不产 TSID，
%   同 workspace_member 先例）。
% W2 硬约束（DB 层 fail-closed，迁移 00000081）：
%   * 写入端 trg_project_member_ws_active：active project_member 提交时必须是
%     同 workspace 的 active workspace_member（可延迟触发器，COMMIT 校验）；
%   * 移除端 trg_workspace_member_remove_guard_pm：workspace_member 移除前
%     同 workspace 不得残留 active project_member（正确流程：同一事务先移除
%     项目成员再移除工作区成员）；
%   * 复合 FK fk_project_member_project / fk_project_member_ws_member 强制
%     Project 与成员同 Workspace（Project Member ⊆ Workspace Member）。
%%%

-export([tablename/0]).
-export([upsert_active_tx/5]).
-export([find/3]).
-export([find_row/2]).
-export([find_tx/4]).
-export([remove_tx/3]).
-export([page_by_project/4]).
-export([unfinished_assignee_tasks_tx/3]).

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
    elib_pg_sql:public_tablename(<<"project_member">>).

%% @doc 事务内幂等邀请：插入或恢复 active（ON CONFLICT DO UPDATE）
%% 返回 {ok, changed, Row} 状态真实变化（新增或 removed→active 恢复）；
%% {ok, unchanged, Row} 已是 active（幂等，不写事件）。
%% "重复成员只一行"由 PK (project_id, user_id) 保证。
-spec upsert_active_tx(any(), integer(), integer(), integer(), integer() | null) ->
    {ok, changed | unchanged, map()} | {error, term()}.
upsert_active_tx(Conn, WsId, ProjectId, Uid, InvitedBy) ->
    Columns = <<"workspace_id,project_id,user_id,invited_by,joined_at,status">>,
    case find_tx(Conn, ProjectId, Uid, Columns) of
        #{<<"status">> := <<"active">>} = Row ->
            {ok, unchanged, Row};
        _ ->
            Tb = tablename(),
            Now = elib_dt:now(),
            Sql =
                <<"INSERT INTO ", Tb/binary,
                    " (workspace_id, project_id, user_id, invited_by, joined_at, status,",
                    " created_at, updated_at)", " VALUES ($1, $2, $3, $4, $5, 'active', $5, $5)",
                    " ON CONFLICT (project_id, user_id) DO UPDATE",
                    " SET status = 'active', invited_by = EXCLUDED.invited_by,",
                    "     joined_at = EXCLUDED.joined_at, updated_at = EXCLUDED.updated_at">>,
            case elib_pg:execute(Conn, Sql, [WsId, ProjectId, Uid, InvitedBy, Now]) of
                {ok, _Count} ->
                    {ok, changed, find_tx(Conn, ProjectId, Uid, Columns)};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 查询项目成员行（自动提交连接；空 map = 无记录）
-spec find(integer(), integer(), binary()) -> map().
find(ProjectId, Uid, Column) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE project_id = $1 AND user_id = $2">>,
    case elib_pg:one(Sql, [ProjectId, Uid]) of
        {ok, Row} ->
            Row;
        {error, Reason} ->
            %% M-4：DB 故障不得与"无记录"静默同形——记错误后仍按空处理
            %% （权限判定保持 fail-closed 403 方向，但排障可见）
            _ = ?ERROR_LOG([project_member_find_failed, ProjectId, Uid, Reason]),
            #{}
    end.

%% @doc 查询项目成员整行（自动提交连接；空 map = 无记录；ZC-05 收敛点：
%% 供关联/里程碑模块的权限只读查询转发，消除重复 SQL）
-spec find_row(integer(), integer()) -> map().
find_row(ProjectId, Uid) ->
    find(ProjectId, Uid, <<"project_id, user_id, workspace_id, status, invited_by, joined_at">>).

%% @doc 事务内查询项目成员行
-spec find_tx(any(), integer(), integer(), binary()) -> map().
find_tx(Conn, ProjectId, Uid, Column) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE project_id = $1 AND user_id = $2">>,
    case elib_pg:query(Conn, Sql, [ProjectId, Uid]) of
        {ok, [Row | _]} ->
            Row;
        {error, Reason} ->
            _ = ?ERROR_LOG([project_member_find_tx_failed, ProjectId, Uid, Reason]),
            #{};
        _ ->
            #{}
    end.

%% @doc 事务内移除项目成员（软删 status=removed）
%% 返回 {ok, 1} 真实移除（应写 member_removed 事件）；
%% {ok, 0} 无 active 行（并发/重复移除幂等，不写事件）。
-spec remove_tx(any(), integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
remove_tx(Conn, ProjectId, Uid) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = 'removed', updated_at = $1",
            " WHERE project_id = $2 AND user_id = $3 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, ProjectId, Uid]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 项目成员分页列表（只列 active；JOIN user 供前端展示；
%% 稳定排序 joined_at ASC, user_id ASC，走 PK 前缀 project_id 过滤）
-spec page_by_project(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
page_by_project(ProjectId, Page, Size, Column) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    CountSql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE project_id = $1 AND status = 'active'">>,
    Total =
        case elib_pg:one(CountSql, [ProjectId]) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    DataSql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " pm", " LEFT JOIN ",
            (user_repo:tablename())/binary, " u ON u.id = pm.user_id",
            " WHERE pm.project_id = $1 AND pm.status = 'active'",
            " ORDER BY pm.joined_at ASC, pm.user_id ASC LIMIT $2 OFFSET $3">>,
    case elib_pg:query(DataSql, [ProjectId, Size, Offset]) of
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

%% @doc 事务内查询目标用户在本项目未完成的 assignee 任务
%% （Owner 转移冲突校验数据源；status <> 'done' 视为未完成）
-spec unfinished_assignee_tasks_tx(any(), integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
unfinished_assignee_tasks_tx(Conn, ProjectId, Uid) ->
    Sql =
        <<"SELECT t.id, t.title, t.status", " FROM project_task t",
            " WHERE t.project_id = $1 AND t.assignee_id = $2 AND t.status <> 'done'",
            " ORDER BY t.id ASC">>,
    case elib_pg:query(Conn, Sql, [ProjectId, Uid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
