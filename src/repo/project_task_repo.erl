-module(project_task_repo).
%%%
% project_task_repo 是 project_task repository 缩写
% 项目任务数据仓库层（迁移 00000078，双体验 v2.5.2 WP4/T6b）
%
% 表结构：project_task(id TSID, project_id FK, title, creator_id, assignee_id
%   可空, status todo|doing|review|done, sort, timestamps)
% W0：assignee 必须是同 workspace 的 active workspace_member（应用层同事务
% 校验；schema 仅保留 user 存在性外键）。轻量执行实体：仅
% title/assignee/status/排序，禁甘特图/依赖/估点/子任务（§三 边界）。
%%%

-export([tablename/0]).
-export([add_tx/2]).
-export([find_by_id/2]).
-export([find_tx/3]).
-export([list_by_project/4]).
-export([update_fields_tx/3]).
-export([find_idempotent_tx/4]).

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
    elib_pg_sql:public_tablename(<<"project_task">>).

%% @doc 事务内插入任务（assignee 的 active 校验由 DS 层同事务前置）
-spec add_tx(any(), map()) -> {ok, integer()} | {error, term()}.
add_tx(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(project_task),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 按 ID 查询任务（自动提交；空 map = 无记录）
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, term()}.
find_by_id(TaskId, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => TaskId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内查询任务行
-spec find_tx(any(), integer(), binary()) -> map().
find_tx(Conn, TaskId, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [TaskId]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 项目任务列表（status 可选过滤；稳定排序 sort ASC, id ASC；
%% 走 i_project_task_project_status_sort）
-spec list_by_project(integer(), binary() | all, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_by_project(ProjectId, Status, Page, Size) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    {Where, Params} =
        case Status of
            all ->
                {<<" WHERE project_id = $1">>, [ProjectId]};
            _ ->
                {<<" WHERE project_id = $1 AND status = $2">>, [ProjectId, Status]}
        end,
    Sql =
        <<"SELECT id,project_id,title,creator_id,assignee_id,status,sort,",
            "created_at,updated_at FROM ", Tb/binary, Where/binary,
            " ORDER BY sort ASC, id ASC LIMIT ", (integer_to_binary(Size))/binary, " OFFSET ",
            (integer_to_binary(Offset))/binary>>,
    elib_pg:query(Sql, Params).

%% @doc 事务内更新任务字段（title/sort/assignee_id/status；守卫与 assignee
%% 校验由 DS 层同事务前置）
-spec update_fields_tx(any(), integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update_fields_tx(Conn, TaskId, Data) ->
    Tb = tablename(),
    %% elib_pg_sql:update/4 签名：WHERE 子句与参数显式分开（T14 Demo B 发现
    %% update/3 不存在——单测 mock 掩盖、端到端暴露的调用点）
    {Sql, Params} = elib_pg_sql:update(Tb, Data, <<"id = $1">>, [TaskId]),
    elib_pg:execute(Conn, Sql, Params).

%% @doc 事务内幂等查询：同 project + 同 creator + 同 title 的既有任务
%% （create 语义幂等：重复请求返回既有任务，不产生重复行）
-spec find_idempotent_tx(any(), integer(), integer(), binary()) -> map().
find_idempotent_tx(Conn, ProjectId, CreatorId, Title) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id FROM ", Tb/binary, " WHERE project_id = $1 AND creator_id = $2 AND title = $3",
            " ORDER BY id ASC LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [ProjectId, CreatorId, Title]) of
        {ok, [#{<<"id">> := TaskId} | _]} -> #{<<"id">> => TaskId};
        _ -> #{}
    end.
