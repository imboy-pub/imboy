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
-export ([find_any_by_id/1]).
-export ([find_any_by_task_id/1]).
-export ([list_by_group_id/3]).
-export ([list_by_group_id/4]).
-export ([list_deleted_by_group_id/3]).
-export ([list_deleted_by_group_id/4]).
-export ([list_by_group_and_user/4]).
-export ([list_by_group_and_user/5]).
-export ([count_by_group_id/1]).
-export ([count_by_group_id/2]).
-export ([count_deleted_by_group_id/1]).
-export ([count_deleted_by_group_id/2]).
-export ([soft_delete/1]).
-export ([restore/1]).

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
    Where = <<"id = $1 AND deleted_at IS NULL">>,
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

%% @doc 根据ID查询群作业（包含已删除）
%% @param TaskId 作业ID
%% @return {ok, Task} | {error, not_found}
-spec find_any_by_id(integer()) -> {ok, map()} | {error, not_found}.
find_any_by_id(TaskId) when is_integer(TaskId), TaskId > 0 ->
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
find_any_by_id(_TaskId) ->
    {error, invalid_task_id}.

%% @doc 根据task_id查询群作业
%% @param TaskId 作业唯一标识
%% @return {ok, Task} | {error, not_found}
-spec find_by_task_id(binary()) -> {ok, map()} | {error, not_found}.
find_by_task_id(TaskId) when is_binary(TaskId), byte_size(TaskId) > 0 ->
    Tb = tablename(),
    Column = <<"*">>,
    Where = <<"task_id = $1 AND deleted_at IS NULL">>,
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

%% @doc 根据task_id查询群作业（包含已删除）
%% @param TaskId 作业唯一标识
%% @return {ok, Task} | {error, not_found}
-spec find_any_by_task_id(binary()) -> {ok, map()} | {error, not_found}.
find_any_by_task_id(TaskId) when is_binary(TaskId), byte_size(TaskId) > 0 ->
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
find_any_by_task_id(_TaskId) ->
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
    list_by_group_id(GroupId, undefined, Page, Size);
list_by_group_id(_GroupId, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 分页查询群组的作业列表（支持按任务状态筛选）
%% @param GroupId 群组ID
%% @param Status 任务状态（1/2/3）或 undefined
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Task]} | {error, Reason}
-spec list_by_group_id(integer(), integer() | undefined, integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_group_id(GroupId, Status, Page, Size)
    when is_integer(GroupId), GroupId > 0,
         is_integer(Page), Page > 0,
         is_integer(Size), Size > 0, Size =< 100 ->
    case group_status_condition(GroupId, Status) of
        {error, Reason} ->
            {error, Reason};
        {Where, Params} ->
            Tb = tablename(),
            Column = <<"id, group_id, task_id, title, description, creator_id, deadline, status, attachment, created_at, updated_at">>,
            OrderBy = <<"id DESC">>,
            Offset = (Page - 1) * Size,
            Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
                    " WHERE ", Where/binary,
                    " ORDER BY ", OrderBy/binary,
                    " LIMIT ", (integer_to_binary(Size))/binary,
                    " OFFSET ", (integer_to_binary(Offset))/binary>>,
            elib_pg:query(Sql, Params)
    end;
list_by_group_id(_GroupId, _Status, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 分页查询群组的已删除作业列表
%% @param GroupId 群组ID
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Task]} | {error, Reason}
-spec list_deleted_by_group_id(integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_deleted_by_group_id(GroupId, Page, Size)
    when is_integer(GroupId), GroupId > 0,
         is_integer(Page), Page > 0,
         is_integer(Size), Size > 0, Size =< 100 ->
    list_deleted_by_group_id(GroupId, undefined, Page, Size);
list_deleted_by_group_id(_GroupId, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 分页查询群组的已删除作业列表（支持按任务状态筛选）
%% @param GroupId 群组ID
%% @param Status 任务状态（1/2/3）或 undefined
%% @param Page 页码（从1开始）
%% @param Size 每页数量
%% @return {ok, [Task]} | {error, Reason}
-spec list_deleted_by_group_id(integer(), integer() | undefined, integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_deleted_by_group_id(GroupId, Status, Page, Size)
    when is_integer(GroupId), GroupId > 0,
         is_integer(Page), Page > 0,
         is_integer(Size), Size > 0, Size =< 100 ->
    case group_status_condition(GroupId, Status, true) of
        {error, Reason} ->
            {error, Reason};
        {Where, Params} ->
            Tb = tablename(),
            Column = <<"id, group_id, task_id, title, description, creator_id, deadline, status, attachment, deleted_at, created_at, updated_at">>,
            OrderBy = <<"id DESC">>,
            Offset = (Page - 1) * Size,
            Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
                    " WHERE ", Where/binary,
                    " ORDER BY ", OrderBy/binary,
                    " LIMIT ", (integer_to_binary(Size))/binary,
                    " OFFSET ", (integer_to_binary(Offset))/binary>>,
            elib_pg:query(Sql, Params)
    end;
list_deleted_by_group_id(_GroupId, _Status, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 查询指定用户在群内可见的作业列表
%% 规则：
%% - 关联到该用户的作业分配
%% - 或该用户创建的作业（即使尚未分配）
%% status 统一输出为 0/1：
%% - 0: 未完成
%% - 1: 已完成（assignment.status >= 2）
-spec list_by_group_and_user(integer(), integer(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_group_and_user(GroupId, UserId, Page, Size) ->
    list_by_group_and_user(GroupId, UserId, undefined, Page, Size).

%% @doc 查询指定用户在群内可见的作业列表（支持状态筛选）
%% Status:
%% - undefined: 不筛选
%% - 0: 未完成（assignment.status < 2 或无 assignment）
%% - 1: 已完成（assignment.status >= 2）
%% - 2/3: 按 assignment.status 精确匹配
-spec list_by_group_and_user(integer(), integer(), integer() | undefined, integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_group_and_user(GroupId, UserId, Status, Page, Size)
    when is_integer(GroupId), GroupId > 0,
         is_integer(UserId), UserId > 0,
         is_integer(Page), Page > 0,
         is_integer(Size), Size > 0, Size =< 100 ->
    Tb = tablename(),
    ATb = group_task_assignment_repo:tablename(),
    Column = <<
        "t.id, t.group_id, t.task_id, t.title, t.description, t.creator_id, t.deadline, t.attachment, "
        "CASE WHEN a.status IS NULL THEN 0 WHEN a.status >= 2 THEN 1 ELSE 0 END AS status, "
        "a.status AS assignment_status, t.created_at, t.updated_at"
    >>,
    Where = <<"t.group_id = $1 AND t.deleted_at IS NULL AND (a.user_id IS NOT NULL OR t.creator_id = $2)">>,
    OrderBy = <<"id DESC">>,
    Offset = (Page - 1) * Size,
    case user_status_filter(Status) of
        {error, Reason} ->
            {error, Reason};
        StatusWhere ->
            Sql = <<
                "SELECT ", Column/binary,
                " FROM ", Tb/binary, " t",
                " LEFT JOIN ", ATb/binary, " a ON t.task_id = a.task_id AND a.user_id = $2",
                " WHERE ", Where/binary, StatusWhere/binary,
                " ORDER BY ", OrderBy/binary,
                " LIMIT ", (integer_to_binary(Size))/binary,
                " OFFSET ", (integer_to_binary(Offset))/binary
            >>,
            elib_pg:query(Sql, [GroupId, UserId])
    end;
list_by_group_and_user(_GroupId, _UserId, _Status, _Page, _Size) ->
    {error, invalid_param}.

%% @doc 统计群组的作业数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_by_group_id(integer()) -> {ok, integer()} | {error, term()}.
count_by_group_id(GroupId) ->
    count_by_group_id(GroupId, undefined).

%% @doc 统计群组的作业数量（支持按状态筛选）
%% @param GroupId 群组ID
%% @param Status 任务状态（1/2/3）或 undefined
%% @return {ok, Count} | {error, Reason}
-spec count_by_group_id(integer(), integer() | undefined) -> {ok, integer()} | {error, term()}.
count_by_group_id(GroupId, Status) when is_integer(GroupId), GroupId > 0 ->
    case group_status_condition(GroupId, Status) of
        {error, Reason} ->
            {error, Reason};
        {Where, Params} ->
            Tb = tablename(),
            Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE ", Where/binary>>,
            parse_count_result(elib_pg:query(Sql, Params))
    end;
count_by_group_id(_GroupId, _Status) ->
    {error, invalid_param}.

%% @doc 统计群组的已删除作业数量
%% @param GroupId 群组ID
%% @return {ok, Count} | {error, Reason}
-spec count_deleted_by_group_id(integer()) -> {ok, integer()} | {error, term()}.
count_deleted_by_group_id(GroupId) ->
    count_deleted_by_group_id(GroupId, undefined).

%% @doc 统计群组的已删除作业数量（支持按状态筛选）
%% @param GroupId 群组ID
%% @param Status 任务状态（1/2/3）或 undefined
%% @return {ok, Count} | {error, Reason}
-spec count_deleted_by_group_id(integer(), integer() | undefined) -> {ok, integer()} | {error, term()}.
count_deleted_by_group_id(GroupId, Status) when is_integer(GroupId), GroupId > 0 ->
    case group_status_condition(GroupId, Status, true) of
        {error, Reason} ->
            {error, Reason};
        {Where, Params} ->
            Tb = tablename(),
            Sql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE ", Where/binary>>,
            parse_count_result(elib_pg:query(Sql, Params))
    end;
count_deleted_by_group_id(_GroupId, _Status) ->
    {error, invalid_param}.

%% @doc 软删除群作业
%% @param TaskId 作业ID
%% @return {ok, Count} | {error, Reason}
-spec soft_delete(integer()) -> {ok, integer()} | {error, term()}.
soft_delete(TaskId) when is_integer(TaskId), TaskId > 0 ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Data = #{deleted_at => Now, updated_at => Now},
    Where = <<"id = $1 AND deleted_at IS NULL">>,
    elib_pg:update(Tb, Data, Where, [TaskId]);
soft_delete(_TaskId) ->
    {error, invalid_task_id}.

%% @doc 恢复已软删除群作业
%% @param TaskId 作业ID
%% @return {ok, Count} | {error, Reason}
-spec restore(integer()) -> {ok, integer()} | {error, term()}.
restore(TaskId) when is_integer(TaskId), TaskId > 0 ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Data = #{
        deleted_at => {raw, <<"NULL">>},
        updated_at => Now
    },
    Where = <<"id = $1 AND deleted_at IS NOT NULL">>,
    elib_pg:update(Tb, Data, Where, [TaskId]);
restore(_TaskId) ->
    {error, invalid_task_id}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec group_status_condition(integer(), integer() | undefined) -> {binary(), list()} | {error, term()}.
group_status_condition(GroupId, Status) ->
    group_status_condition(GroupId, Status, false).

-spec group_status_condition(integer(), integer() | undefined, boolean()) -> {binary(), list()} | {error, term()}.
group_status_condition(GroupId, undefined, false) ->
    {<<"group_id = $1 AND deleted_at IS NULL">>, [GroupId]};
group_status_condition(GroupId, Status, false) when is_integer(Status), Status >= 1, Status =< 3 ->
    {<<"group_id = $1 AND status = $2 AND deleted_at IS NULL">>, [GroupId, Status]};
group_status_condition(GroupId, undefined, true) ->
    {<<"group_id = $1 AND deleted_at IS NOT NULL">>, [GroupId]};
group_status_condition(GroupId, Status, true) when is_integer(Status), Status >= 1, Status =< 3 ->
    {<<"group_id = $1 AND status = $2 AND deleted_at IS NOT NULL">>, [GroupId, Status]};
group_status_condition(_GroupId, _Status, _DeletedOnly) ->
    {error, invalid_status}.

-spec user_status_filter(integer() | undefined) -> binary() | {error, term()}.
user_status_filter(undefined) ->
    <<>>;
user_status_filter(0) ->
    <<" AND (CASE WHEN a.status IS NULL THEN 0 WHEN a.status >= 2 THEN 1 ELSE 0 END) = 0">>;
user_status_filter(1) ->
    <<" AND (CASE WHEN a.status IS NULL THEN 0 WHEN a.status >= 2 THEN 1 ELSE 0 END) = 1">>;
user_status_filter(2) ->
    <<" AND a.status = 2">>;
user_status_filter(3) ->
    <<" AND a.status = 3">>;
user_status_filter(_Status) ->
    {error, invalid_status}.

-spec parse_count_result(term()) -> {ok, integer()} | {error, term()}.
parse_count_result({ok, [[{<<"count">>, Count}]]}) when is_integer(Count) ->
    {ok, Count};
parse_count_result({ok, [{[{<<"count">>, Count}]}]}) when is_integer(Count) ->
    {ok, Count};
parse_count_result({ok, [#{<<"count">> := Count}]}) when is_integer(Count) ->
    {ok, Count};
parse_count_result({ok, #{<<"count">> := Count}}) when is_integer(Count) ->
    {ok, Count};
parse_count_result({error, Reason}) ->
    {error, Reason};
parse_count_result(Other) ->
    {error, {unexpected_count_result, Other}}.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
