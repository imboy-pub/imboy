-module(project_task_handler).
%%%
% 项目任务 HTTP handler（双体验 v2.5.2 WP4/T6b）
%
% 轻量执行实体（§三 边界）：仅 title/assignee/status/sort；
% 无 milestone 端点（defer）；无拖拽看板/估点/依赖/子任务。
%
%%% =====================================================================
%%% 路由片段清单（T7 统一注册 imboy_router.erl 时按此合并；本模块不改 router）
%%% =====================================================================
%%% METHOD path                                            → handler 动作
%%% POST   /api/v1/projects/:project_id/tasks              → tasks（method 分派：POST=create）
%%% GET    /api/v1/projects/:project_id/tasks              → tasks（method 分派：GET=list）
%%% GET    /api/v1/tasks/:task_id                          → show
%%% POST   /api/v1/tasks/:task_id/update                   → update（title/assignee/sort）
%%% POST   /api/v1/tasks/:task_id/status                   → update_status（四态流转）
%%%
%%% 全部路由 JWT 保护（auth_middleware /api/v1/* 默认门），State 携带 current_uid。
%%% =====================================================================

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = resolve_action(maps:get(action, State0), Req0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% 集合路径（/tasks）同路径双语义：POST=创建 / GET=列表（method 分派模式）
-spec resolve_action(atom(), cowboy_req:req()) -> atom().
resolve_action(tasks, Req) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> create;
        _ -> list
    end;
resolve_action(Action, _Req) ->
    Action.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(show, Req, State) -> show(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(update_status, Req, State) -> update_status(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建任务（Owner/Member；Guest 403；assignee 非 active 成员 400；幂等）
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_ids(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, ProjectId, _TaskId} ->
            Title = maps:get(<<"title">>, PostVals, <<>>),
            AssigneeId = to_assignee(maps:get(<<"assignee_id">>, PostVals, undefined)),
            Sort = elib_cnv:safe_to_integer(maps:get(<<"sort">>, PostVals, 0)),
            case project_task_logic:create(Uid, ProjectId, Title, AssigneeId, Sort) of
                {ok, Task, created} ->
                    elib_response:success(Req0, Task#{status_flag => created});
                {ok, Task, existing} ->
                    %% 重复请求幂等：返回既有任务，不报错、不重复入库
                    elib_response:success(Req0, Task#{status_flag => existing});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 项目任务列表（active 工作区成员可读；status 过滤）
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_ids(Req0, #{}) of
        {error, Req} ->
            Req;
        {ok, ProjectId, _TaskId} ->
            Qs = cowboy_req:parse_qs(Req0),
            Status = proplists:get_value(<<"status">>, Qs, all),
            {Page, Size} = elib_param:page(Req0),
            case project_task_logic:list(Uid, ProjectId, Status, Page, Size) of
                {ok, Tasks} ->
                    elib_response:success(Req0, #{list => Tasks});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 任务详情（active 工作区成员可读）
-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_ids(Req0, #{}) of
        {error, Req} ->
            Req;
        {ok, _ProjectId, TaskId} ->
            case project_task_logic:detail(Uid, TaskId) of
                {ok, Task} ->
                    elib_response:success(Req0, Task);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 更新任务（title/assignee_id/sort；assignee 变更同 active 校验 400）
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_ids(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, _ProjectId, TaskId} ->
            Title = maps:get(<<"title">>, PostVals, undefined),
            AssigneeId = to_assignee(maps:get(<<"assignee_id">>, PostVals, undefined)),
            Sort =
                case maps:get(<<"sort">>, PostVals, undefined) of
                    undefined -> undefined;
                    V -> elib_cnv:safe_to_integer(V)
                end,
            case project_task_logic:update(Uid, TaskId, Title, AssigneeId, Sort) of
                {ok, Task} ->
                    elib_response:success(Req0, Task);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 状态流转（四态+回退；非法流转 400）
-spec update_status(cowboy_req:req(), map()) -> cowboy_req:req().
update_status(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_ids(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, _ProjectId, TaskId} ->
            Status = maps:get(<<"status">>, PostVals, <<>>),
            case project_task_logic:change_status(Uid, TaskId, Status) of
                {ok, Task} ->
                    elib_response:success(Req0, Task);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 解析 project_id（路径 binding 优先）与 task_id（路径 binding 优先）
-spec resolve_ids(cowboy_req:req(), map()) ->
    {ok, integer(), integer()} | {error, cowboy_req:req()}.
resolve_ids(Req0, PostVals) ->
    RawProject =
        case cowboy_req:binding(project_id, Req0) of
            undefined -> maps:get(<<"project_id">>, PostVals, undefined);
            Binding -> Binding
        end,
    ProjectId = elib_cnv:safe_to_integer(RawProject),
    RawTask =
        case cowboy_req:binding(task_id, Req0) of
            undefined -> maps:get(<<"task_id">>, PostVals, undefined);
            Binding2 -> Binding2
        end,
    TaskId = elib_cnv:safe_to_integer(RawTask),
    PidOk = is_integer(ProjectId) andalso ProjectId > 0,
    TidOk = is_integer(TaskId) andalso TaskId > 0,
    if
        PidOk andalso TidOk ->
            {ok, ProjectId, TaskId};
        PidOk ->
            {ok, ProjectId, 0};
        TidOk ->
            {ok, 0, TaskId};
        true ->
            {error, elib_response:error(Req0, <<"project/task id 必须"/utf8>>, 400)}
    end.

%% assignee 参数清洗：null/空串 → 未指派；正整数 → 指派；其余 → 0（未指派）
-spec to_assignee(term()) -> integer() | undefined | null.
to_assignee(undefined) ->
    undefined;
to_assignee(null) ->
    null;
to_assignee(V) ->
    Id = elib_cnv:safe_to_integer(V),
    case is_integer(Id) andalso Id > 0 of
        true -> Id;
        false -> 0
    end.
