-module(group_task_assignment_repo).
%%%
% group_task_assignment 相关操作都放到该模块，存储库模块
% group_task_assignment related operations are put in this module, repository module
% 群作业分配数据仓库层，提供群作业分配信息的基础数据库操作
%%%

-export([tablename/0]).
-export([insert/1]).
-export([update/2]).
-export([find_by_id/1]).
-export([find_by_task_and_user/2]).
-export([list_by_task_id/3]).
-export([list_by_user_id/3]).
-export([list_by_user_id/4]).
-export([count_by_status/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群作业分配表的表名
%% @return 返回群作业分配表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_task_assignment">>).

%% @doc 插入群作业分配
%% @param Data 作业分配数据映射
%% @return {ok, AssignmentId, InsertResult} | {error, Reason}
-spec insert(map()) -> {ok, integer(), map()} | {error, term()}.
insert(Data) ->
    Tb = tablename(),
    % 验证必填字段
    case {maps:get(task_id, Data, undefined), maps:get(user_id, Data, undefined)} of
        {undefined, _} ->
            {error, {missing_field, task_id}};
        {_, undefined} ->
            {error, {missing_field, user_id}};
        {TaskId, UserId} when is_binary(TaskId), is_integer(UserId), UserId > 0 ->
            % 设置默认值
            Now = elib_dt:now(),
            Data2 = Data#{
                created_at => maps:get(created_at, Data, Now),
                updated_at => maps:get(updated_at, Data, Now),
                status => maps:get(status, Data, 0),
                content => maps:get(content, Data, <<>>),
                attachment => maps:get(attachment, Data, <<>>)
            },
            Id = elib_tsid:generate(group_task_assignment),
            Data3 = Data2#{id => Id},
            {Sql, Params} = elib_pg_sql:insert(Tb, Data3),
            case elib_pg:query(Sql, Params) of
                {ok, _Count} -> {ok, Id};
                {error, _} = Err -> Err
            end;
        _ ->
            {error, invalid_param}
    end.

%% @doc 更新群作业分配
%% @param AssignmentId 作业分配ID
%% @param Data 要更新的数据
%% @return {ok, Count} | {error, Reason}
-spec update(integer(), map()) -> {ok, integer()} | {error, term()}.
update(AssignmentId, Data) when is_integer(AssignmentId), AssignmentId > 0 ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Data2 = Data#{updated_at => Now},
    Where = <<"id = $1">>,
    elib_pg:update(Tb, Data2, Where, [AssignmentId]);
update(_AssignmentId, _Data) ->
    {error, invalid_assignment_id}.

%% @doc 根据ID查询群作业分配
%% @param AssignmentId 作业分配ID
%% @return {ok, Assignment} | {error, not_found}
-spec find_by_id(integer()) -> {ok, map()} | {error, not_found}.
find_by_id(AssignmentId) when is_integer(AssignmentId), AssignmentId > 0 ->
    Tb = tablename(),
    Column = <<"*">>,
    Where = <<"id = $1">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [AssignmentId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Assignment]} ->
            {ok, Assignment};
        {error, Reason} ->
            {error, Reason}
    end;
find_by_id(_AssignmentId) ->
    {error, invalid_assignment_id}.

%% @doc 根据task_id和user_id查询作业分配
%% @param TaskId 作业唯一标识
%% @param UserId 用户ID
%% @return {ok, Assignment} | {error, not_found}
-spec find_by_task_and_user(binary(), integer()) -> {ok, map()} | {error, not_found}.
find_by_task_and_user(TaskId, UserId) when
    is_binary(TaskId),
    byte_size(TaskId) > 0,
    is_integer(UserId),
    UserId > 0
->
    Tb = tablename(),
    Column = <<"*">>,
    Where = <<"task_id = $1 AND user_id = $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [TaskId, UserId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Assignment]} ->
            {ok, Assignment};
        {error, Reason} ->
            {error, Reason}
    end;
find_by_task_and_user(_TaskId, _UserId) ->
    {error, invalid_param}.

%% @doc 分页查询作业的所有分配
%% @param TaskId 作业唯一标识
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Assignment]} | {error, Reason}
-spec list_by_task_id(binary(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_task_id(TaskId, Page, Size) when
    is_binary(TaskId),
    byte_size(TaskId) > 0,
    is_integer(Page),
    Page > 0,
    is_integer(Size),
    Size > 0,
    Size =< 100
->
    Tb = tablename(),
    Column =
        <<"id, task_id, user_id, status, submitted_at, content, attachment, score, comment, reviewed_by, reviewed_at, created_at, updated_at">>,
    Where = <<"task_id = $1">>,
    OrderBy = <<"id DESC">>,
    Offset = (Page - 1) * Size,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary, " ORDER BY ",
            OrderBy/binary, " LIMIT ", (integer_to_binary(Size))/binary, " OFFSET ",
            (integer_to_binary(Offset))/binary>>,
    elib_pg:query(Sql, [TaskId]);
list_by_task_id(_TaskId, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 分页查询用户的作业列表
%% @param UserId 用户ID
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Assignment]} | {error, Reason}
-spec list_by_user_id(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_user_id(UserId, Page, Size) when
    is_integer(UserId),
    UserId > 0,
    is_integer(Page),
    Page > 0,
    is_integer(Size),
    Size > 0,
    Size =< 100
->
    list_by_user_id(UserId, undefined, Page, Size);
list_by_user_id(_UserId, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 分页查询用户的作业列表（支持状态筛选）
%% Status:
%% - undefined: 不筛选
%% - 0: 未完成（status < 2）
%% - 1: 已完成（status >= 2）
%% - 2/3: 精确匹配 status
-spec list_by_user_id(integer(), integer() | undefined, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_by_user_id(UserId, Status, Page, Size) when
    is_integer(UserId),
    UserId > 0,
    is_integer(Page),
    Page > 0,
    is_integer(Size),
    Size > 0,
    Size =< 100
->
    Tb = tablename(),
    Column =
        <<"id, task_id, user_id, status, submitted_at, content, attachment, score, comment, reviewed_by, reviewed_at, created_at, updated_at">>,
    case user_status_where(Status) of
        {error, Reason} ->
            {error, Reason};
        StatusWhere ->
            Where = <<"user_id = $1", StatusWhere/binary>>,
            OrderBy = <<"id DESC">>,
            Offset = (Page - 1) * Size,
            Sql =
                <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary,
                    " ORDER BY ", OrderBy/binary, " LIMIT ", (integer_to_binary(Size))/binary,
                    " OFFSET ", (integer_to_binary(Offset))/binary>>,
            elib_pg:query(Sql, [UserId])
    end;
list_by_user_id(_UserId, _Status, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 统计指定状态的作业数量
%% @param TaskId 作业唯一标识
%% @param Status 状态
%% @return {ok, Count} | {error, Reason}
-spec count_by_status(binary(), integer()) -> {ok, integer()} | {error, term()}.
count_by_status(TaskId, Status) when
    is_binary(TaskId),
    byte_size(TaskId) > 0,
    is_integer(Status),
    Status >= 0,
    Status =< 3
->
    Tb = tablename(),
    Where = <<"task_id = $1 AND status = $2">>,
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [TaskId, Status]) of
        {ok, [#{<<"count">> := Count}]} ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end;
count_by_status(_TaskId, _Status) ->
    {error, invalid_param}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec user_status_where(integer() | undefined) -> binary() | {error, term()}.
user_status_where(undefined) ->
    <<>>;
user_status_where(0) ->
    <<" AND (CASE WHEN status >= 2 THEN 1 ELSE 0 END) = 0">>;
user_status_where(1) ->
    <<" AND (CASE WHEN status >= 2 THEN 1 ELSE 0 END) = 1">>;
user_status_where(2) ->
    <<" AND status = 2">>;
user_status_where(3) ->
    <<" AND status = 3">>;
user_status_where(_Status) ->
    {error, invalid_status}.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
