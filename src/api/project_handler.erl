-module(project_handler).
%%%
% 项目 HTTP handler（双体验 v2.5.2 WP4/T6a）
%
% W2：Project Member、Milestone、Channel 关联与聚合端点由各自 handler 提供；
% 本模块只负责项目本身的创建、列表、详情与状态流转。
% 跨域文案：本模块对外一律使用全称"项目"（Project），审计含 workspace 上下文。
%
%%% =====================================================================
%%% 路由片段清单（T7 统一注册 imboy_router.erl 时按此合并；本模块不改 router）
%%% =====================================================================
%%% METHOD path                                        → handler 动作
%%% POST   /api/v1/workspaces/:workspace_id/projects   → projects（method 分派：POST=create）
%%% GET    /api/v1/workspaces/:workspace_id/projects   → projects（method 分派：GET=list）
%%% GET    /api/v1/projects/:project_id                → show
%%% POST   /api/v1/projects/:project_id/update         → update（改名/描述）
%%% POST   /api/v1/projects/:project_id/status         → update_status
%%%
%%% 全部路由 JWT 保护（auth_middleware /api/v1/* 默认门），State 携带 current_uid。
%%% =====================================================================

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = resolve_action(maps:get(action, State0), Req0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% 集合路径（/projects）同路径双语义：POST=创建 / GET=列表（镜像
%% adm_sso_handler / agent_mandate_handler 的 method 分派模式）
-spec resolve_action(atom(), cowboy_req:req()) -> atom().
resolve_action(projects, Req) ->
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

%% @doc 创建项目（Workspace Owner/Member；Guest/非成员 403；archived 980）
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_ids(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId, _ProjectId} ->
            Name = maps:get(<<"name">>, PostVals, <<>>),
            Description = maps:get(<<"description">>, PostVals, <<>>),
            case project_logic:create(Uid, WsId, Name, Description) of
                {ok, Project} ->
                    elib_response:success(Req0, Project);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 项目列表（active 工作区成员可读；稳定排序+分页 ≤100）
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_ids(Req0, #{}) of
        {error, Req} ->
            Req;
        {ok, WsId, _ProjectId} ->
            {Page, Size} = elib_param:page(Req0),
            case project_logic:list(Uid, WsId, Page, Size) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 项目详情（Workspace Owner 或 active Project Member 可读，W2）
-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_ids(Req0, #{}) of
        {error, Req} ->
            Req;
        {ok, _WsId, ProjectId} ->
            case project_logic:detail(Uid, ProjectId) of
                {ok, Project} ->
                    elib_response:success(Req0, Project);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 改名/描述（Owner/Member；Guest 403）
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_ids(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, _WsId, ProjectId} ->
            Name = maps:get(<<"name">>, PostVals, undefined),
            Description = maps:get(<<"description">>, PostVals, undefined),
            case project_logic:update(Uid, ProjectId, Name, Description) of
                {ok, Project} ->
                    elib_response:success(Req0, Project);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 状态流转（active|done；Owner/Member；Guest 403；archived 980）
-spec update_status(cowboy_req:req(), map()) -> cowboy_req:req().
update_status(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_ids(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, _WsId, ProjectId} ->
            Status = maps:get(<<"status">>, PostVals, <<>>),
            case project_logic:update_status(Uid, ProjectId, Status) of
                {ok, Project} ->
                    elib_response:success(Req0, Project);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 解析 workspace_id（body 兜底）与 project_id（路径 binding 优先）
-spec resolve_ids(cowboy_req:req(), map()) ->
    {ok, integer(), integer()} | {error, cowboy_req:req()}.
resolve_ids(Req0, PostVals) ->
    RawWs =
        case cowboy_req:binding(workspace_id, Req0) of
            undefined -> maps:get(<<"workspace_id">>, PostVals, undefined);
            Binding -> Binding
        end,
    WsId = elib_cnv:safe_to_integer(RawWs),
    RawProject =
        case cowboy_req:binding(project_id, Req0) of
            undefined -> maps:get(<<"project_id">>, PostVals, undefined);
            Binding2 -> Binding2
        end,
    ProjectId = elib_cnv:safe_to_integer(RawProject),
    WsOk = is_integer(WsId) andalso WsId > 0,
    PidOk = is_integer(ProjectId) andalso ProjectId > 0,
    if
        WsOk andalso PidOk ->
            {ok, WsId, ProjectId};
        WsOk ->
            {ok, WsId, 0};
        PidOk ->
            %% 无 workspace binding：show/update/status 仅带 project_id
            {ok, 0, ProjectId};
        true ->
            {error, elib_response:error(Req0, <<"workspace/project id 必须"/utf8>>, 400)}
    end.
