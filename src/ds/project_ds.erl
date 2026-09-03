-module(project_ds).
-compile([nowarn_deprecated_catch]).
%%%
% project_ds 是 project domain service 缩写
% 项目领域服务（双体验 v2.5.2 WP4/T6a；分层镜像 WP3 workspace_ds）
%
% 核心职责：
%   1. 创建（单事务）：归档写守卫（workspace_guard:ensure_writable_tx，先锁
%      workspace 行）→ INSERT project（owner_id=creator，DB 复合 FK+可延迟
%      触发器兜底 owner active membership）。
%   2. 更新（单事务）：改名/描述/状态（active|done）——事务内先读
%      workspace_id → 写守卫锁行 → UPDATE；无物理删除。
%   3. 查询：find_by_id / 工作区及成员项目分页列表（稳定排序
%      created_at DESC, id DESC，limit 钳制 ≤100）。
%
% W2 使用 project_member 做项目级授权；project_event 在 task 状态流转写入。
%%%

-export([create/4]).
-export([find_by_id/1]).
-export([page_by_workspace/3]).
-export([page_by_workspace_member/4]).
-export([update_fields/2]).
-export([update_status/2]).
%% Admin 运营管理查询（双体验 v2.5.2 WP7/T11b）
-export([admin_page/4]).
-export([admin_batch_task_counts/1]).
-export([admin_task_status_stats/1]).
-export([admin_assignee_overview/2]).

-include("log.hrl").

-define(MAX_PAGE_SIZE, 100).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建项目（单事务：写守卫 + INSERT；creator 即 Project Owner）
-spec create(integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
create(CreatorUid, WsId, Name, Description) ->
    Now = elib_dt:now(),
    Result =
        elib_pg:with_tx(fun(Conn) ->
            %% 归档写守卫：先锁 workspace 行（与归档/恢复事务线性化）
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
            ),
            Data = #{
                <<"workspace_id">> => WsId,
                <<"name">> => Name,
                <<"description">> => Description,
                <<"owner_id">> => CreatorUid,
                <<"status">> => <<"active">>,
                <<"created_at">> => Now,
                <<"updated_at">> => Now
            },
            case project_repo:add_tx(Conn, Data) of
                {ok, ProjectId} ->
                    %% W2：Owner 自动入项目（幂等；可延迟触发器允许同事务
                    %% 先建成员，COMMIT 时统一校验 active workspace membership）
                    ok = project_member_ds:ensure_owner_member_tx(
                        Conn, WsId, ProjectId, CreatorUid
                    ),
                    {ok, ProjectId};
                {error, Reason} ->
                    throw({abort_tx, {project_create_failed, Reason}})
            end
        end),
    case Result of
        {ok, ProjectId} -> {ok, find_by_id(ProjectId)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 项目详情
-spec find_by_id(integer()) -> map() | {error, term()}.
find_by_id(ProjectId) ->
    project_repo:find_by_id(
        ProjectId, <<"id,workspace_id,name,description,owner_id,status,created_at,updated_at">>
    ).

%% @doc 工作区项目列表（稳定排序 created_at DESC, id DESC；limit ≤ 100）
-spec page_by_workspace(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_workspace(WsId, Page0, Size0) ->
    Size = max(1, min(Size0, ?MAX_PAGE_SIZE)),
    Page = max(Page0, 1),
    project_repo:page_by_workspace(
        WsId,
        Page,
        Size,
        <<"id,workspace_id,name,description,owner_id,status,created_at,updated_at">>
    ).

%% @doc 当前用户在工作区内已加入的项目列表。
-spec page_by_workspace_member(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
page_by_workspace_member(WsId, Uid, Page0, Size0) ->
    Size = max(1, min(Size0, ?MAX_PAGE_SIZE)),
    Page = max(Page0, 1),
    project_repo:page_by_workspace_member(
        WsId,
        Uid,
        Page,
        Size,
        <<
            "p.id,p.workspace_id,p.name,p.description,p.owner_id,p.status,"
            "p.created_at,p.updated_at"
        >>
    ).

%% @doc 更新项目字段（单事务：先读 workspace_id → 写守卫 → UPDATE）
%% Fields 仅含白名单键 name/description（logic 层已清洗）。
-spec update_fields(integer(), map()) -> {ok, map()} | {error, term()}.
update_fields(ProjectId, Fields) ->
    update_tx(ProjectId, fun(Conn, _Project) ->
        {ok, _} = project_repo:update_fields_tx(Conn, ProjectId, Fields),
        ok
    end).

%% @doc 更新项目状态（active|done；单事务同上）
-spec update_status(integer(), binary()) -> {ok, map()} | {error, term()}.
update_status(ProjectId, Status) ->
    update_tx(ProjectId, fun(Conn, _Project) ->
        Now = elib_dt:now(),
        {ok, _} = project_repo:update_fields_tx(
            Conn, ProjectId, #{<<"status">> => Status, <<"updated_at">> => Now}
        ),
        ok
    end).

%% ===================================================================
%% Admin 运营管理查询（双体验 v2.5.2 WP7/T11b；只读，供 project_logic admin 函数调用）
%% ===================================================================

%% @doc Admin 项目分页列表（JOIN workspace 名称 + owner 用户摘要）
%% Status = all | <<"active">> | <<"done">>；keyword 模糊匹配项目名。
-spec admin_page(integer(), integer(), binary() | all, binary()) ->
    {ok, map()} | {error, term()}.
admin_page(Page0, Size0, Status, Keyword) ->
    Page = max(Page0, 1),
    Size = max(min(Size0, 100), 1),
    PTb = project_repo:tablename(),
    WTb = workspace_repo:tablename(),
    UTb = user_repo:tablename(),
    {WhereSql, Params} = admin_page_where(Status, Keyword),
    CountSql = <<"SELECT COUNT(*) AS count FROM ", PTb/binary, " p", WhereSql/binary>>,
    Total =
        case elib_pg:one(CountSql, Params) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    Offset = (Page - 1) * Size,
    DataSql = [
        <<"SELECT p.id, p.workspace_id, p.owner_id, p.name, p.description, p.status,",
            " p.created_at, p.updated_at, w.name AS workspace_name, w.status AS workspace_status,",
            " u.nickname AS owner_nickname, u.account AS owner_account", " FROM ", PTb/binary,
            " p LEFT JOIN ", WTb/binary, " w ON w.id = p.workspace_id LEFT JOIN ", UTb/binary,
            " u ON u.id = p.owner_id">>,
        WhereSql,
        <<" ORDER BY p.created_at DESC, p.id DESC LIMIT $">>,
        integer_to_binary(length(Params) + 1),
        <<" OFFSET $">>,
        integer_to_binary(length(Params) + 2)
    ],
    case elib_pg:query(DataSql, Params ++ [Size, Offset]) of
        {ok, Items} ->
            TotalPage =
                case Total > 0 of
                    true -> ((Total - 1) div Size) + 1;
                    false -> 0
                end,
            {ok, #{
                list => Items,
                page => Page,
                size => Size,
                total => Total,
                total_page => TotalPage
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec admin_page_where(binary() | all, binary()) -> {binary(), [term()]}.
admin_page_where(all, Keyword) when byte_size(Keyword) > 0 ->
    {<<" WHERE p.name ILIKE $1">>, [<<"%", Keyword/binary, "%">>]};
admin_page_where(all, _Keyword) ->
    {<<>>, []};
admin_page_where(Status, Keyword) when byte_size(Keyword) > 0 ->
    {<<" WHERE p.status = $1 AND p.name ILIKE $2">>, [Status, <<"%", Keyword/binary, "%">>]};
admin_page_where(Status, _Keyword) ->
    {<<" WHERE p.status = $1">>, [Status]}.

%% @doc Admin 批量任务计数（一条 GROUP BY 拿全页：总数 + done 数）
-spec admin_batch_task_counts([integer()]) -> map().
admin_batch_task_counts([]) ->
    #{};
admin_batch_task_counts(ProjectIds) ->
    IdsSql = join_int_ids(ProjectIds),
    Sql = [
        <<"SELECT project_id AS pid, COUNT(*) AS total,",
            " COUNT(*) FILTER (WHERE status = 'done') AS done",
            " FROM project_task WHERE project_id IN (", IdsSql/binary, ") GROUP BY project_id">>
    ],
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            maps:from_list([
                {
                    maps:get(<<"pid">>, Row, 0),
                    #{
                        total => maps:get(<<"total">>, Row, 0),
                        done => maps:get(<<"done">>, Row, 0)
                    }
                }
             || Row <- Rows
            ]);
        _ ->
            #{}
    end.

%% @doc 项目任务状态分布（详情页概览：todo/doing/review/done 计数）
-spec admin_task_status_stats(integer()) -> map().
admin_task_status_stats(ProjectId) ->
    Sql = [
        <<"SELECT status, COUNT(*) AS count FROM project_task",
            " WHERE project_id = $1 GROUP BY status">>
    ],
    case elib_pg:query(Sql, [ProjectId]) of
        {ok, Rows} ->
            maps:from_list([
                {maps:get(<<"status">>, Row, <<"unknown">>), maps:get(<<"count">>, Row, 0)}
             || Row <- Rows
            ]);
        _ ->
            #{}
    end.

%% @doc 项目 assignee 概览（详情页只读：每人任务总数/已完成数，前 Limit 名）
%% 这里只聚合 assignee；成员治理由 Project Member 模块负责。
-spec admin_assignee_overview(integer(), integer()) -> {ok, [map()]} | {error, term()}.
admin_assignee_overview(ProjectId, Limit) ->
    UTb = user_repo:tablename(),
    Sql = [
        <<"SELECT pt.assignee_id, u.nickname, u.account, u.avatar,",
            " COUNT(*) AS total, COUNT(*) FILTER (WHERE pt.status = 'done') AS done",
            " FROM project_task pt LEFT JOIN ", UTb/binary, " u ON u.id = pt.assignee_id",
            " WHERE pt.project_id = $1 AND pt.assignee_id IS NOT NULL",
            " GROUP BY pt.assignee_id, u.nickname, u.account, u.avatar",
            " ORDER BY total DESC, pt.assignee_id ASC LIMIT $2">>
    ],
    elib_pg:query(Sql, [ProjectId, Limit]).

-spec join_int_ids([integer()]) -> binary().
join_int_ids(Ids) ->
    elib_cnv:implode(<<",">>, [integer_to_binary(Id) || Id <- Ids]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 通用事务更新：事务内先读项目行（含 workspace_id）→ 写守卫锁行 → Fun
-spec update_tx(integer(), fun((any(), map()) -> ok | {error, term()})) ->
    {ok, map()} | {error, term()}.
update_tx(ProjectId, Fun) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id">>),
            case maps:get(<<"workspace_id">>, Project, undefined) of
                WsId when is_integer(WsId) ->
                    ok = workspace_guard:abort_on_error(
                        workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
                    ),
                    case Fun(Conn, Project) of
                        ok -> ok;
                        {error, Reason} -> throw({abort_tx, Reason})
                    end;
                _ ->
                    throw({abort_tx, project_not_found})
            end
        end),
    case Result of
        ok -> {ok, find_by_id(ProjectId)};
        {error, Reason} -> {error, Reason}
    end.
