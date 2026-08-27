-module(project_task_logic).
-compile([nowarn_deprecated_catch]).
%%%
% project_task_logic 项目任务业务逻辑（双体验 v2.5.2 WP4/T6b）
%
% W0 权限模型：
%   创建/更新/流转：Workspace Owner/Member ✅；Guest 只读 403；非成员 403
%   详情/列表：active Workspace Member 可读
%   archived workspace 拒写（workspace_guard，稳定错误码 980）
%
% 四态状态机（§三 边界：Task 只有 title/assignee/status/排序）：
%   前向仅相邻一步：todo→doing→review→done
%   回退任意：done→review/doing/todo、review→doing/todo、doing→todo
%   非法（400）：跳级前向（todo→review、todo→done、doing→done）与同态
%
% assignee 校验（W0）：必须是同 workspace 的 active workspace_member，
%% 否则 400；assignee 变更走同校验（DS 层同事务）。
%% 移除 Workspace Member 的冲突数据来源：
%%   workspace_member_repo:unfinished_tasks_of_user/3
%%   （project_task JOIN project WHERE assignee_id=? AND status<>'done'）。
%
% 错误约定：{error, {Code, Msg}}。
%%%

-export([create/5]).
-export([detail/2]).
-export([list/5]).
-export([update/5]).
-export([change_status/3]).
-export([valid_status/1]).
-export([legal_transition/2]).

-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建任务（Owner/Member；Guest 403；assignee 非 active 成员 400；
%% 重复请求幂等返回既有任务）
-spec create(integer(), integer(), binary(), integer() | undefined, integer()) ->
    {ok, map(), created | existing} | {error, {integer(), binary()}}.
create(Uid, ProjectId, Title, AssigneeId, Sort) ->
    case ensure_can_write(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case valid_title(Title) of
                false ->
                    {error, {400, <<"任务标题不能为空且不超过 500 字符"/utf8>>}};
                true ->
                    case project_task_ds:create(Uid, ProjectId, Title, AssigneeId, Sort) of
                        {ok, Task, Status} ->
                            _ = ?INFO_LOG([
                                project_task_created,
                                Uid,
                                ProjectId,
                                maps:get(<<"id">>, Task, 0)
                            ]),
                            {ok, Task, Status};
                        {error, {Code, Msg}} when is_integer(Code) ->
                            {error, {Code, Msg}};
                        {error, Reason2} ->
                            _ = ?ERROR_LOG([project_task_create_failed, Uid, ProjectId, Reason2]),
                            {error, {500, <<"任务创建失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% @doc 任务详情（active 工作区成员可读）
-spec detail(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
detail(Uid, TaskId) ->
    case load_task_with_project(TaskId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, Task, WsId} ->
            case workspace_logic:ensure_member(WsId, Uid) of
                {ok, _Role} -> {ok, Task};
                {error, Forbidden} -> {error, Forbidden}
            end
    end.

%% @doc 项目任务列表（active 工作区成员可读；status 过滤 all|todo|doing|review|done）
-spec list(integer(), integer(), binary() | all, integer(), integer()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
list(Uid, ProjectId, Status, Page, Size) ->
    case ensure_can_read(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            Status2 =
                case Status of
                    S when
                        S =:= all;
                        S =:= <<"todo">>;
                        S =:= <<"doing">>;
                        S =:= <<"review">>;
                        S =:= <<"done">>
                    ->
                        S;
                    _ ->
                        all
                end,
            case project_task_ds:list_by_project(ProjectId, Status2, Page, Size) of
                {ok, Tasks} ->
                    {ok, Tasks};
                {error, Reason2} ->
                    _ = ?ERROR_LOG([project_task_page_failed, ProjectId, Reason2]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 更新任务（title/assignee/sort；Owner/Member；Guest 403；
%% assignee 变更同 active 成员校验 400；archived 980）
-spec update(
    integer(),
    integer(),
    binary() | undefined,
    integer() | undefined | null,
    integer() | undefined
) ->
    {ok, map()} | {error, {integer(), binary()}}.
update(Uid, TaskId, Title, AssigneeId, Sort) ->
    case valid_optional_title(Title) of
        false ->
            {error, {400, <<"任务标题不能为空且不超过 500 字符"/utf8>>}};
        true ->
            case ensure_task_can_write(Uid, TaskId) of
                {error, Reason} ->
                    {error, Reason};
                {ok, _Task} ->
                    case project_task_ds:update(TaskId, Uid, Title, AssigneeId, Sort) of
                        {ok, Task} ->
                            {ok, Task};
                        {error, {Code, Msg}} when is_integer(Code) ->
                            {error, {Code, Msg}};
                        {error, Reason2} ->
                            _ = ?ERROR_LOG([project_task_update_failed, TaskId, Uid, Reason2]),
                            {error, {500, <<"更新失败，请稍后重试"/utf8>>}}
                    end
            end
    end.

%% @doc 状态流转（四态+回退；非法 400；Guest 403；archived 980；
%% 事件与状态变更同事务）
-spec change_status(integer(), integer(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
change_status(Uid, TaskId, ToStatus) ->
    case valid_status(ToStatus) of
        false ->
            {error, {400, <<"任务状态仅支持 todo/doing/review/done"/utf8>>}};
        true ->
            case ensure_task_can_write(Uid, TaskId) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Task} ->
                    FromStatus = maps:get(<<"status">>, Task, <<>>),
                    case legal_transition(FromStatus, ToStatus) of
                        true ->
                            do_change_status(Uid, TaskId, FromStatus, ToStatus);
                        false ->
                            {error,
                                {400,
                                    <<"非法任务状态流转："/utf8, FromStatus/binary, " → ", ToStatus/binary>>}}
                    end
            end
    end.

do_change_status(Uid, TaskId, FromStatus, ToStatus) ->
    case project_task_ds:change_status(Uid, TaskId, ToStatus) of
        {ok, Task} ->
            _ = ?INFO_LOG([
                project_task_status_changed, Uid, TaskId, FromStatus, ToStatus
            ]),
            {ok, Task};
        {error, {Code, Msg}} when is_integer(Code) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([project_task_status_failed, TaskId, Uid, Reason]),
            {error, {500, <<"更新失败，请稍后重试"/utf8>>}}
    end.

%% ===================================================================
%% 状态机与校验（纯函数委托 DS 层，保持 Handler→Logic→DS→Repo 单向依赖）
%% ===================================================================

%% @doc 任务状态值域
-spec valid_status(binary()) -> boolean().
valid_status(Status) ->
    project_task_ds:valid_status(Status).

%% @doc 四态状态机：前向仅相邻一步（rank +1），回退任意（rank 递减）；
%% 同态与跳级前向均非法。
-spec legal_transition(binary(), binary()) -> boolean().
legal_transition(From, To) ->
    project_task_ds:legal_transition(From, To).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 任务写权限：Guest 只读/非成员 403（镜像 project_logic:ensure_can_write）
-spec ensure_task_can_write(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_task_can_write(Uid, TaskId) ->
    case load_task_with_project(TaskId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, Task, WsId} ->
            case workspace_logic:ensure_can_create_resource(WsId, Uid) of
                ok -> {ok, Task};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec ensure_can_write(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_write(Uid, ProjectId) ->
    case project_logic:detail(Uid, ProjectId) of
        {error, _} = Err ->
            %% 非成员 403 / 项目不存在 404 原样透传
            Err;
        {ok, Project} ->
            WsId = maps:get(<<"workspace_id">>, Project),
            case workspace_logic:ensure_can_create_resource(WsId, Uid) of
                ok -> {ok, Project};
                {error, Reason} -> {error, Reason}
            end
    end.

-spec ensure_can_read(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_read(Uid, ProjectId) ->
    project_logic:detail(Uid, ProjectId).

-spec load_task_with_project(integer()) ->
    {ok, map(), integer()} | {error, {404, binary()}}.
load_task_with_project(TaskId) ->
    case project_task_ds:find_by_id(TaskId) of
        Task when is_map(Task), map_size(Task) > 0 ->
            WsId = task_workspace_id(Task),
            {ok, Task, WsId};
        _ ->
            {error, {404, <<"任务不存在"/utf8>>}}
    end.

%% task 行 → workspace_id（project 表非直挂；经 project_id 二跳，
%% 数量级恒小走 pkey，无 N+1——单任务加载固定 2 次索引点查）
-spec task_workspace_id(map()) -> integer().
task_workspace_id(Task) ->
    ProjectId = maps:get(<<"project_id">>, Task, 0),
    case project_repo:find_by_id(ProjectId, <<"workspace_id">>) of
        #{<<"workspace_id">> := WsId} -> WsId;
        _ -> 0
    end.

-spec valid_title(term()) -> boolean().
valid_title(Title) when is_binary(Title), byte_size(Title) > 0 ->
    string:length(Title) =< 500;
valid_title(_) ->
    false.

-spec valid_optional_title(term()) -> boolean().
valid_optional_title(undefined) ->
    true;
valid_optional_title(Title) ->
    valid_title(Title).
