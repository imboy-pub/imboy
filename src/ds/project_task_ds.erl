-module(project_task_ds).
-compile([nowarn_deprecated_catch]).
%%%
% project_task_ds 是 project_task domain service 缩写
% 项目任务领域服务（双体验 v2.5.2 WP4/T6b）
%
% 核心职责（全部单事务，with_tx）：
%   1. create：project 归属解析 → 归档写守卫（先锁 workspace 行）→
%      create 语义幂等（同 project+creator+title → 返回既有）→
%      assignee active workspace_member 校验（W0）→ INSERT。
%   2. update：task→project→workspace 解析 → 写守卫 → assignee 变更同校验
%      → UPDATE。
%   3. change_status：同上守卫 → 四态流转校验（非法流转 400，状态机见
%      project_task_logic:legal_transition/2）→ UPDATE status →
%      project_event 写入（同一 Conn = 同一事务，event_type='task_status'，
%      payload 含 from/to + actor；无孤儿事件——回滚时事件与状态一起消失）。
%
% W0：assignee 校验直接查 active workspace_member（无 project_member）。
%%%

-export([create/5]).
-export([find_by_id/1]).
-export([list_by_project/4]).
-export([update/5]).
-export([change_status/3]).
-export([valid_status/1]).
-export([legal_transition/2]).

-include("log.hrl").

%% 任务状态顺序（rank 用于状态机判定；前向仅相邻一步，回退任意）
-define(TASK_STATUS_RANK, #{
    <<"todo">> => 1,
    <<"doing">> => 2,
    <<"review">> => 3,
    <<"done">> => 4
}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建任务（单事务：守卫 + 幂等 + assignee 校验 + INSERT）
%% AssigneeId = 0 | undefined | null 表示未指派（可空列）。
-spec create(integer(), integer(), binary(), integer() | undefined, integer()) ->
    {ok, map(), created | existing} | {error, term()}.
create(CreatorUid, ProjectId, Title, AssigneeId, Sort) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            WsId = ensure_project_writable_tx(Conn, ProjectId),
            %% create 语义幂等：同 project+creator+title → 返回既有
            case project_task_repo:find_idempotent_tx(Conn, ProjectId, CreatorUid, Title) of
                #{<<"id">> := ExistingId} ->
                    {ok, ExistingId, existing};
                _ ->
                    ok = ensure_assignee_tx(Conn, WsId, AssigneeId),
                    Now = elib_dt:now(),
                    Data = #{
                        <<"project_id">> => ProjectId,
                        <<"title">> => Title,
                        <<"creator_id">> => CreatorUid,
                        <<"assignee_id">> => normalize_assignee(AssigneeId),
                        <<"status">> => <<"todo">>,
                        <<"sort">> => max(Sort, 0),
                        <<"created_at">> => Now,
                        <<"updated_at">> => Now
                    },
                    case project_task_repo:add_tx(Conn, Data) of
                        {ok, TaskId} -> {ok, TaskId, created};
                        {error, Reason} -> throw({abort_tx, {task_create_failed, Reason}})
                    end
            end
        end),
    case Result of
        {ok, TaskId, Status} -> {ok, find_by_id(TaskId), Status};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 任务详情
-spec find_by_id(integer()) -> map() | {error, term()}.
find_by_id(TaskId) ->
    project_task_repo:find_by_id(
        TaskId,
        <<"id,project_id,title,creator_id,assignee_id,status,sort,created_at,updated_at">>
    ).

%% @doc 项目任务列表（status all|todo|doing|review|done；排序 sort ASC, id ASC）
-spec list_by_project(integer(), binary() | all, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_by_project(ProjectId, Status, Page, Size) ->
    project_task_repo:list_by_project(ProjectId, Status, Page, Size).

%% @doc 更新任务（title/sort/assignee；单事务：守卫 + assignee 变更校验）
%% 仅更新提交的字段（undefined 字段跳过）。
-spec update(
    integer(),
    integer(),
    binary() | undefined,
    integer() | undefined | null,
    integer() | undefined
) ->
    {ok, map()} | {error, term()}.
update(TaskId, _ActorUid, Title, AssigneeId, Sort) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Task = project_task_repo:find_tx(
                Conn, TaskId, <<"id,project_id,assignee_id">>
            ),
            WsId = ensure_task_writable_tx(Conn, Task),
            Data0 =
                case Title of
                    T when is_binary(T), T =/= <<>> -> #{<<"title">> => T};
                    _ -> #{}
                end,
            Data1 =
                case Sort of
                    S when is_integer(S), S >= 0 -> Data0#{<<"sort">> => S};
                    _ -> Data0
                end,
            Data2 =
                case AssigneeId of
                    A when A =:= undefined -> Data1;
                    A when A =:= null -> Data1#{<<"assignee_id">> => null};
                    A when is_integer(A), A > 0 ->
                        ok = ensure_assignee_tx(Conn, WsId, A),
                        Data1#{<<"assignee_id">> => A};
                    _ ->
                        Data1
                end,
            case map_size(Data2) of
                0 ->
                    ok;
                _ ->
                    Now = elib_dt:now(),
                    {ok, _} = project_task_repo:update_fields_tx(
                        Conn, TaskId, Data2#{<<"updated_at">> => Now}
                    ),
                    ok
            end
        end),
    case Result of
        ok -> {ok, find_by_id(TaskId)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 状态流转（单事务：守卫 → 状态机校验 → UPDATE → 事件同事务写入）
%% 非法流转（含同态）返回 {error, {400, Msg}}，不写任何行。
-spec change_status(integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
change_status(ActorUid, TaskId, ToStatus) ->
    Result =
        elib_pg:with_tx(fun(Conn) ->
            Task = project_task_repo:find_tx(
                Conn, TaskId, <<"id,project_id,status">>
            ),
            _WsId = ensure_task_writable_tx(Conn, Task),
            FromStatus = maps:get(<<"status">>, Task, <<>>),
            case project_task_logic:legal_transition(FromStatus, ToStatus) of
                true ->
                    Now = elib_dt:now(),
                    {ok, _} = project_task_repo:update_fields_tx(
                        Conn, TaskId, #{<<"status">> => ToStatus, <<"updated_at">> => Now}
                    ),
                    %% 事件原子性：与状态 UPDATE 同一 Conn（同事务）
                    ProjectId = maps:get(<<"project_id">>, Task),
                    {ok, _} = project_event_ds:record_task_status_tx(
                        Conn, ProjectId, TaskId, ActorUid, FromStatus, ToStatus
                    ),
                    ok;
                false ->
                    throw(
                        {abort_tx, {400, <<"非法任务状态流转"/utf8>>}}
                    )
            end
        end),
    case Result of
        ok -> {ok, find_by_id(TaskId)};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 状态机（纯函数；logic 层委托，DS 事务内复检）
%% ===================================================================

%% @doc 任务状态值域
-spec valid_status(binary()) -> boolean().
valid_status(Status) ->
    maps:is_key(Status, ?TASK_STATUS_RANK).

%% @doc 四态状态机：前向仅相邻一步（rank +1），回退任意（rank 递减）；
%% 同态与跳级前向均非法。
-spec legal_transition(binary(), binary()) -> boolean().
legal_transition(From, To) ->
    case {?TASK_STATUS_RANK, From, To} of
        {_, F, T} when F =:= T ->
            false;
        {R, F, T} ->
            case {maps:get(F, R, undefined), maps:get(T, R, undefined)} of
                {FR, TR} when is_integer(FR), is_integer(TR) ->
                    TR =:= FR + 1 orelse TR < FR;
                _ ->
                    false
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 事务内：解析 project 归属 + 归档写守卫（返回 workspace_id）
-spec ensure_project_writable_tx(any(), integer()) -> integer() | no_return().
ensure_project_writable_tx(Conn, ProjectId) ->
    Project = project_repo:find_tx(Conn, ProjectId, <<"id,workspace_id">>),
    case maps:get(<<"workspace_id">>, Project, undefined) of
        WsId when is_integer(WsId) ->
            ok = workspace_guard:abort_on_error(
                workspace_guard:ensure_writable_tx(Conn, {workspace, WsId})
            ),
            WsId;
        _ ->
            throw({abort_tx, {404, <<"项目不存在"/utf8>>}})
    end.

%% 事务内：task → project 归属 + 归档写守卫（返回 workspace_id）
-spec ensure_task_writable_tx(any(), map()) -> integer() | no_return().
ensure_task_writable_tx(Conn, Task) ->
    ProjectId = maps:get(<<"project_id">>, Task, undefined),
    case ProjectId of
        P when is_integer(P), P > 0 ->
            ensure_project_writable_tx(Conn, ProjectId);
        _ ->
            throw({abort_tx, {404, <<"任务不存在"/utf8>>}})
    end.

%% 事务内 assignee 校验（W0）：必须是同 workspace 的 active workspace_member
%% 未指派（0/undefined/null）跳过校验。
-spec ensure_assignee_tx(any(), integer(), integer() | undefined | null) ->
    ok | no_return().
ensure_assignee_tx(_Conn, _WsId, A) when A =:= 0; A =:= undefined; A =:= null ->
    ok;
ensure_assignee_tx(Conn, WsId, AssigneeId) when is_integer(AssigneeId), AssigneeId > 0 ->
    case workspace_member_repo:find_tx(Conn, WsId, AssigneeId, <<"status">>) of
        #{<<"status">> := <<"active">>} ->
            ok;
        _ ->
            throw(
                {abort_tx, {400, <<"任务负责人必须是该工作区的 active 成员"/utf8>>}}
            )
    end.

-spec normalize_assignee(integer() | undefined | null) -> integer() | nil.
normalize_assignee(A) when is_integer(A), A > 0 -> A;
normalize_assignee(_) -> nil.
