-module(group_task_logic).
%% Stable group_collab task domain boundary.
%% HTTP adapters should call this module instead of repo internals.
%%%
% 群作业业务逻辑模块
% Group task business logic module
%%%

%% 创建作业
-export ([create/4]).
%% 更新作业
-export ([update/3]).
%% 分配作业
-export ([assign/2]).
%% 提交作业
-export ([submit/3]).
%% 批改作业
-export ([review/3]).
%% 查询作业详情
-export ([detail/1]).
%% 查询群作业列表
-export ([list/3]).
%% 查询群作业列表（带筛选）
-export ([list/5]).
%% 查询我的作业
-export ([my_tasks/3]).
-export ([my_tasks/4]).
%% 查询待批改作业
-export ([pending_review/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("error_code.hrl").
-include("common.hrl").

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 创建群作业
%% @param GroupId 群组ID
%% @param CreatorId 创建者ID
%% @param Title 作业标题
%% @param Data 作业数据（description, deadline, attachment等）
%% @return {ok, TaskId} | {error, Reason}
-spec create(integer(), integer(), binary(), map()) -> {ok, integer()} | {error, binary()}.
create(GroupId, CreatorId, Title, Data) when is_integer(GroupId), GroupId > 0,
                                             is_integer(CreatorId), CreatorId > 0,
                                             is_binary(Title), byte_size(Title) > 0 ->
    % 生成唯一的作业ID（使用HashID）
    TaskId = GroupId + CreatorId + erlang:unique_integer([positive]),
    TaskData = #{
        group_id => GroupId,
        task_id => TaskId,
        title => Title,
        creator_id => CreatorId
    },
    TaskData2 = maps:merge(TaskData, Data),
    case group_task_repo:insert(TaskData2) of
        {ok, Id, _} ->
            {ok, Id};
        {error, {missing_field, _Field}} ->
            {error, imboy_error:error_msg(?ERR_TASK_TITLE_REQUIRED), ?ERR_TASK_TITLE_REQUIRED};
        {error, Reason} ->
            {error, io_lib:format("~p", [Reason]), ?ERR_INTERNAL_SERVER_ERROR}
    end;
create(_GroupId, _CreatorId, Title, _Data) when is_binary(Title), byte_size(Title) =:= 0 ->
    {error, imboy_error:error_msg(?ERR_TASK_TITLE_REQUIRED), ?ERR_TASK_TITLE_REQUIRED};
create(_GroupId, _CreatorId, _Title, _Data) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 更新群作业
%% @param TaskId 作业ID
%% @param CreatorId 创建者ID
%% @param Data 要更新的数据
%% @return ok | {error, Reason}
-spec update(integer(), integer(), map()) -> ok | {error, binary()}.
update(TaskId, CreatorId, Data) when is_integer(TaskId), TaskId > 0,
                                     is_integer(CreatorId), CreatorId > 0 ->
    case group_task_repo:find_by_id(TaskId) of
        {ok, Task} ->
            CreatorId2 = maps:get(<<"creator_id">>, Task, 0),
            Status = maps:get(<<"status">>, Task, 1),
            case CreatorId2 of
                CreatorId ->
                    case Status of
                        3 ->
                            {error, <<"作业已截止，无法修改"/utf8>>, ?ERR_TASK_DEADLINE_PASSED};
                        _ ->
                            UpdateData = normalize_update_data(Data),
                            case maps:size(UpdateData) of
                                0 ->
                                    {error, <<"没有要更新的字段"/utf8>>, ?ERR_BAD_REQUEST};
                                _ ->
                                    group_task_repo:update(TaskId, UpdateData),
                                    ok
                            end
                    end;
                _ ->
                    {error, imboy_error:error_msg(?ERR_TASK_PERMISSION_DENIED), ?ERR_TASK_PERMISSION_DENIED}
            end;
        {error, not_found} ->
            {error, imboy_error:error_msg(?ERR_TASK_NOT_FOUND), ?ERR_TASK_NOT_FOUND}
    end;
update(_TaskId, _CreatorId, _Data) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 分配作业给成员
%% @param TaskId 作业ID
%% @param UserIds 用户ID列表
%% @return ok | {error, Reason}
-spec assign(integer(), [integer()]) -> ok | {error, binary()}.
assign(TaskId, UserIds) when is_integer(TaskId), TaskId > 0, is_list(UserIds) ->
    case length(UserIds) of
        0 ->
            {error, <<"成员列表不能为空"/utf8>>, ?ERR_BAD_REQUEST};
        _ ->
            case group_task_repo:find_by_id(TaskId) of
                {ok, Task} ->
                    TaskIdStr = maps:get(<<"task_id">>, Task, <<>>),
                    % 批量插入作业分配
                    lists:foreach(fun(UserId) ->
                        case group_task_assignment_repo:find_by_task_and_user(TaskIdStr, UserId) of
                            {error, not_found} ->
                                group_task_assignment_repo:insert(#{
                                    task_id => TaskIdStr,
                                    user_id => UserId,
                                    status => 0
                                });
                            _ ->
                                ok
                        end
                    end, UserIds),
                    ok;
                {error, not_found} ->
                    {error, imboy_error:error_msg(?ERR_TASK_NOT_FOUND), ?ERR_TASK_NOT_FOUND}
            end
    end;
assign(_TaskId, _UserIds) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 提交作业
%% @param TaskId 作业唯一标识
%% @param UserId 用户ID
%% @param Data 提交数据（content, attachment）
%% @return ok | {error, Reason}
-spec submit(binary(), integer(), map()) -> ok | {error, binary()}.
submit(TaskId, UserId, Data) when is_binary(TaskId), byte_size(TaskId) > 0,
                                  is_integer(UserId), UserId > 0 ->
    % 检查作业是否存在且未过期
    case group_task_repo:find_by_task_id(TaskId) of
        {ok, Task} ->
            Deadline = maps:get(<<"deadline">>, Task, undefined),
            case check_deadline(Deadline) of
                true ->
                    {error, imboy_error:error_msg(?ERR_TASK_DEADLINE_PASSED), ?ERR_TASK_DEADLINE_PASSED};
                false ->
                    % 检查作业分配是否存在
                    case group_task_assignment_repo:find_by_task_and_user(TaskId, UserId) of
                        {ok, Assignment} ->
                            Status = maps:get(<<"status">>, Assignment, 0),
                            case Status of
                                2 ->
                                    {error, imboy_error:error_msg(?ERR_TASK_ALREADY_SUBMITTED), ?ERR_TASK_ALREADY_SUBMITTED};
                                3 ->
                                    {error, imboy_error:error_msg(?ERR_TASK_ALREADY_REVIEWED), ?ERR_TASK_ALREADY_REVIEWED};
                                _ ->
                                    AssignmentId = maps:get(<<"id">>, Assignment),
                                    Now = elib_dt:now(),
                                    SubmitData = #{
                                        status => 2,
                                        submitted_at => Now
                                    },
                                    SubmitData2 = maps:merge(SubmitData, Data),
                                    group_task_assignment_repo:update(AssignmentId, SubmitData2),
                                    ok
                            end;
                        {error, not_found} ->
                            {error, imboy_error:error_msg(?ERR_TASK_ASSIGNMENT_NOT_FOUND), ?ERR_TASK_ASSIGNMENT_NOT_FOUND}
                    end
            end;
        {error, not_found} ->
            {error, imboy_error:error_msg(?ERR_TASK_NOT_FOUND), ?ERR_TASK_NOT_FOUND}
    end;
submit(_TaskId, _UserId, _Data) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 批改作业
%% @param AssignmentId 作业分配ID
%% @param ReviewerId 批改人ID
%% @param Data 批改数据（score, comment）
%% @return ok | {error, Reason}
-spec review(integer(), integer(), map()) -> ok | {error, binary()}.
review(AssignmentId, ReviewerId, Data) when is_integer(AssignmentId), AssignmentId > 0,
                                            is_integer(ReviewerId), ReviewerId > 0 ->
    case group_task_assignment_repo:find_by_id(AssignmentId) of
        {ok, Assignment} ->
            Status = maps:get(<<"status">>, Assignment, 0),
            case Status of
                2 ->
                    Now = elib_dt:now(),
                    ReviewData = #{
                        status => 3,
                        reviewed_by => ReviewerId,
                        reviewed_at => Now
                    },
                    ReviewData2 = maps:merge(ReviewData, Data),
                    group_task_assignment_repo:update(AssignmentId, ReviewData2),
                    ok;
                3 ->
                    {error, imboy_error:error_msg(?ERR_TASK_ALREADY_REVIEWED), ?ERR_TASK_ALREADY_REVIEWED};
                _ ->
                    {error, <<"作业未提交，无法批改"/utf8>>, ?ERR_BAD_REQUEST}
            end;
        {error, not_found} ->
            {error, imboy_error:error_msg(?ERR_TASK_ASSIGNMENT_NOT_FOUND), ?ERR_TASK_ASSIGNMENT_NOT_FOUND}
    end;
review(_AssignmentId, _ReviewerId, _Data) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 查询作业详情
%% @param TaskId 作业ID
%% @return {ok, Task} | {error, Reason}
-spec detail(integer()) -> {ok, map()} | {error, binary()}.
detail(TaskId) when is_integer(TaskId), TaskId > 0 ->
    case group_task_repo:find_by_id(TaskId) of
        {ok, Task} ->
            {ok, Task};
        {error, not_found} ->
            {error, imboy_error:error_msg(?ERR_TASK_NOT_FOUND), ?ERR_TASK_NOT_FOUND}
    end;
detail(_TaskId) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 查询群作业列表
%% @param GroupId 群组ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, List} | {error, Reason}
-spec list(integer(), integer(), integer()) -> {ok, [map()]} | {error, binary()}.
list(GroupId, Page, Size) when is_integer(GroupId), GroupId > 0,
                                is_integer(Page), Page > 0,
                                is_integer(Size), Size > 0, Size =< 100 ->
    list(GroupId, undefined, undefined, Page, Size);
list(_GroupId, _Page, _Size) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 查询群作业列表（支持状态与执行人筛选）
%% @param GroupId 群组ID
%% @param Status 状态（undefined/0/1/2/3）
%% @param AssigneeId 执行人ID（undefined 表示群全量视角）
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, List} | {error, Reason}
-spec list(integer(), integer() | undefined, integer() | undefined, integer(), integer()) -> {ok, [map()]} | {error, binary()}.
list(GroupId, Status, AssigneeId, Page, Size)
    when is_integer(GroupId), GroupId > 0,
         is_integer(Page), Page > 0,
         is_integer(Size), Size > 0, Size =< 100 ->
    case AssigneeId of
        undefined ->
            case Status of
                undefined ->
                    group_task_repo:list_by_group_id(GroupId, Page, Size);
                S when is_integer(S), S >= 1, S =< 3 ->
                    group_task_repo:list_by_group_id(GroupId, S, Page, Size);
                _ ->
                    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}
            end;
        A when is_integer(A), A > 0 ->
            case Status of
                undefined ->
                    group_task_repo:list_by_group_and_user(GroupId, A, Page, Size);
                S when is_integer(S), S >= 0, S =< 3 ->
                    group_task_repo:list_by_group_and_user(GroupId, A, S, Page, Size);
                _ ->
                    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}
            end;
        _ ->
            {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}
    end;
list(_GroupId, _Status, _AssigneeId, _Page, _Size) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 查询我的作业
%% @param UserId 用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, List} | {error, Reason}
-spec my_tasks(integer(), integer(), integer()) -> {ok, [map()]} | {error, binary()}.
my_tasks(UserId, Page, Size) when is_integer(UserId), UserId > 0,
                                   is_integer(Page), Page > 0,
                                   is_integer(Size), Size > 0, Size =< 100 ->
    my_tasks(UserId, undefined, Page, Size);
my_tasks(_UserId, _Page, _Size) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 查询我的作业（支持状态筛选）
%% @param Status 状态（undefined/0/1/2/3）
-spec my_tasks(integer(), integer() | undefined, integer(), integer()) -> {ok, [map()]} | {error, binary()}.
my_tasks(UserId, Status, Page, Size) when is_integer(UserId), UserId > 0,
                                          is_integer(Page), Page > 0,
                                          is_integer(Size), Size > 0, Size =< 100 ->
    case Status of
        undefined ->
            group_task_assignment_repo:list_by_user_id(UserId, undefined, Page, Size);
        S when is_integer(S), S >= 0, S =< 3 ->
            group_task_assignment_repo:list_by_user_id(UserId, S, Page, Size);
        _ ->
            {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}
    end;
my_tasks(_UserId, _Status, _Page, _Size) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% @doc 查询待批改作业
%% @param TaskId 作业ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, List} | {error, Reason}
-spec pending_review(binary(), integer(), integer()) -> {ok, [map()]} | {error, binary()}.
pending_review(TaskId, Page, Size) when is_binary(TaskId), byte_size(TaskId) > 0,
                                        is_integer(Page), Page > 0,
                                        is_integer(Size), Size > 0, Size =< 100 ->
    % 查询已提交但未批改的作业
    case group_task_assignment_repo:list_by_task_id(TaskId, Page, Size) of
        {ok, Assignments} ->
            % 过滤出状态为2（已提交）的作业
            Filtered = lists:filter(fun(A) ->
                maps:get(<<"status">>, A, 0) =:= 2
            end, Assignments),
            {ok, Filtered};
        {error, Reason} ->
            {error, Reason}
    end;
pending_review(_TaskId, _Page, _Size) ->
    {error, imboy_error:error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST}.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 检查截止时间是否已过
%% @param Deadline 截止时间
%% @return true if expired, false otherwise
-spec check_deadline(binary() | undefined) -> boolean().
check_deadline(undefined) ->
    false;
check_deadline(Deadline) ->
    try
        DeadlineTs = elib_dt:rfc3339_to(Deadline),
        NowTs = elib_dt:now(),
        NowTs > DeadlineTs
    catch
        _:_ ->
            false
    end.

-spec normalize_update_data(map()) -> map().
normalize_update_data(Data) ->
    Fields = [
        {title, <<"title">>},
        {description, <<"description">>},
        {deadline, <<"deadline">>},
        {attachment, <<"attachment">>},
        {status, <<"status">>}
    ],
    lists:foldl(fun({AtomKey, BinaryKey}, Acc) ->
        case update_field(Data, AtomKey, BinaryKey) of
            undefined ->
                Acc;
            Value ->
                Acc#{AtomKey => Value}
        end
    end, #{}, Fields).

-spec update_field(map(), atom(), binary()) -> term().
update_field(Data, AtomKey, BinaryKey) ->
    case maps:find(AtomKey, Data) of
        {ok, Value} ->
            Value;
        error ->
            maps:get(BinaryKey, Data, undefined)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
