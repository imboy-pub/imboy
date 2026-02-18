-module(group_task_repo).
%%%
% group_task 相关操作都放到该模块，存储库模块
% group_task related operations are put in this module, repository module
% 群作业数据仓库层，提供群作业信息的基础数据库操作
%%%

-export ([tablename/0]).
-export ([insert/1]).
-export ([update/2]).
-export ([find_by_id/1]).
-export ([find_by_task_id/1]).
-export ([list_by_group_id/3]).
-export ([count_by_group_id/1]).
-export ([soft_delete/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取群作业表的表名
%% @return 返回群作业表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_task">>).

%% @doc 插入群作业
%% @param Data 作业数据映射
%% @return {ok, TaskId, InsertResult} | {error, Reason}
-spec insert(map()) -> {ok, integer(), map()} | {error, term()}.
insert(Data) ->
    Tb = tablename(),
    % 验证必填字段
    case {maps:get(group_id, Data, undefined), maps:get(task_id, Data, undefined), maps:get(title, Data, undefined)} of
        {undefined, _, _} ->
            {error, {missing_field, group_id}};
        {_, undefined, _} ->
            {error, {missing_field, task_id}};
        {_, _, <<>>} ->
            {error, {missing_field, title}};
        {_, _, undefined} ->
            {error, {missing_field, title}};
        {Gid, TaskId, Title} when is_integer(Gid), is_binary(TaskId), is_binary(Title), byte_size(Title) > 0 ->
            % 设置默认值
            Now = elib_dt:now(),
            Data2 = Data#{
                created_at => maps:get(created_at, Data, Now),
                updated_at => maps:get(updated_at, Data, Now),
                status => maps:get(status, Data, 1),
                description => maps:get(description, Data, <<>>)
            },
            elib_pg:insert(Tb, Data2, <<"RETURNING id">>);
        _ ->
            {error, invalid_param}
    end.

%% @doc 更新群作业
%% @param TaskId 作业ID
%% @param Data 要更新的数据
%% @return {ok, Count} | {error, Reason}
-spec update(integer(), map()) -> {ok, integer()} | {error, term()}.
update(TaskId, Data) when is_integer(TaskId), TaskId > 0 ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Data2 = Data#{updated_at => Now},
    Where = <<"id = $1">>,
    elib_pg:update(Tb, Data2, Where, [TaskId]);
update(_TaskId, _Data) ->
    {error, invalid_task_id}.

%% @doc 根据ID查询群作业
%% @param TaskId 作业ID
%% @return {ok, Task} | {error, not_found}
-spec find_by_id(integer()) -> {ok, map()} | {error, not_found}.
find_by_id(TaskId) when is_integer(TaskId), TaskId > 0 ->
    Tb = tablename(),
    Column = <<"*">>,
    Where = <<"id = $1">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [TaskId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Task]} ->
            {ok, Task};
        {error, Reason} ->
            {error, Reason}
    end;
find_by_id(_TaskId) ->
    {error, invalid_task_id}.

%% @doc 根据task_id查询群作业
%% @param TaskId 作业唯一标识
%% @return {ok, Task} | {error, not_found}
-spec find_by_task_id(binary()) -> {ok, map()} | {error, not_found}.
find_by_task_id(TaskId) when is_binary(TaskId), byte_size(TaskId) > 0 ->
    Tb = tablename(),
    Column = <<"*">>,
    Where = <<"task_id = $1">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [TaskId]) of
        {ok, []} ->
            {error, not_found};
        {ok, [Task]} ->
            {ok, Task};
        {error, Reason} ->
            {error, Reason}
    end;
find_by_task_id(_TaskId) ->
    {error, invalid_task_id}.

%% @doc 分页查询群组的作业列表
%% @param GroupId 群组ID
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Task]} | {error, Reason}
-spec list_by_group_id(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_group_id(GroupId, Page, Size)
    when is_integer(GroupId), GroupId > 0,
         is_integer(Page), Page > 0,
         is_integer(Size), Size > 0, Size =< 100 ->
    Tb = tablename(),
    Column = <<"id, group_id, task_id, title, description, creator_id, deadline, status, attachment, created_at, updated_at">>,
    Where = <<"group_id = $1">>,
    OrderBy = <<"id DESC">>,
    Offset = (Page - 1) * Size,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE ", Where/binary,
            " ORDER BY ", OrderBy/binary,
            " LIMIT ", (integer_to_binary(Size))/binary,
            " OFFSET ", (integer_to_binary(Offset))/binary>>,
    elib_pg:query(Sql, [GroupId]);
list_by_group_id(_GroupId, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 统计群组的作业数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_by_group_id(integer()) -> {ok, integer()} | {error, term()}.
count_by_group_id(GroupId) when is_integer(GroupId), GroupId > 0 ->
    Tb = tablename(),
    Where = <<"group_id = $1">>,
    Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE ", Where/binary>>,
    case elib_pg:query(Sql, [GroupId]) of
        {ok, [[{<<"count">>, Count}]]} ->
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end;
count_by_group_id(_GroupId) ->
    {error, invalid_param}.

%% @doc 软删除群作业
%% @param TaskId 作业ID
%% @return {ok, Count} | {error, Reason}
-spec soft_delete(integer()) -> {ok, integer()} | {error, term()}.
soft_delete(TaskId) when is_integer(TaskId), TaskId > 0 ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Data = #{deleted_at => Now},
    Where = <<"id = $1">>,
    elib_pg:update(Tb, Data, Where, [TaskId]);
soft_delete(_TaskId) ->
    {error, invalid_task_id}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
