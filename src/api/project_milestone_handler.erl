-module(project_milestone_handler).
%%%
% 项目里程碑 HTTP handler（Channel-first-class W2 / ZC-03，迁移 00000081）
%
% 轻量计划实体（计划边界）：仅 name/due_date/status（+reached_at 审计）；
% 禁依赖/甘特图/复杂状态机。
%
% 字段语义（与 project_milestone_logic 契约一致）：
%   * create：name 必填；due_date 可选（YYYY-MM-DD | null）；未知字段忽略；
%   * update：name/due_date 可选（undefined=保留；due_date null=清空）；
%   * status 字段在 create/update 中显式拒绝（400）——里程碑状态只能经
%     /reach 端点单向流转（planned→reached，重复 reach 幂等），
%     避免绕过状态机与 milestone_reached 事件；
%   * 未知字段（白名单外）一律忽略。
%
%%% =====================================================================
%%% 路由片段清单（T7/ZC-05 统一注册 imboy_router.erl 时按此合并；本模块不改 router）
%%% =====================================================================
%%% METHOD path                                            → handler 动作
%%% POST   /api/v1/projects/:project_id/milestones         → milestones（method 分派：POST=create）
%%% GET    /api/v1/projects/:project_id/milestones         → milestones（method 分派：GET=list）
%%% POST   /api/v1/milestones/:milestone_id/update         → update（name/due_date）
%%% POST   /api/v1/milestones/:milestone_id/reach          → reach（planned→reached 单向，幂等）
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

%% 集合路径（/milestones）同路径双语义：POST=创建 / GET=列表（method 分派模式）
-spec resolve_action(atom(), cowboy_req:req()) -> atom().
resolve_action(milestones, Req) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> create;
        _ -> list
    end;
resolve_action(Action, _Req) ->
    Action.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(reach, Req, State) -> reach(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建里程碑（Owner/Project Member；Guest 403；归档 980）
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case reject_status_param(PostVals) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, 400);
        ok ->
            case resolve_project_id(Req0, PostVals) of
                {error, Req} ->
                    Req;
                {ok, ProjectId} ->
                    Name = maps:get(<<"name">>, PostVals, <<>>),
                    DueDate = maps:get(<<"due_date">>, PostVals, undefined),
                    case project_milestone_logic:create(Uid, ProjectId, Name, DueDate) of
                        {ok, Ms} ->
                            elib_response:success(Req0, Ms);
                        {error, {Code, Msg2}} ->
                            elib_response:error(Req0, Msg2, Code)
                    end
            end
    end.

%% @doc 项目里程碑列表（active 工作区成员且项目成员可读；status 过滤；分页）
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_project_id(Req0, #{}) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            Qs = cowboy_req:parse_qs(Req0),
            Status = proplists:get_value(<<"status">>, Qs, all),
            {Page, Size} = elib_param:page(Req0),
            case project_milestone_logic:list(Uid, ProjectId, Status, Page, Size) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 更新里程碑（name/due_date；undefined=保留；due_date null=清空）
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case reject_status_param(PostVals) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, 400);
        ok ->
            case resolve_milestone_id(Req0, PostVals) of
                {error, Req} ->
                    Req;
                {ok, MsId} ->
                    Name = maps:get(<<"name">>, PostVals, undefined),
                    DueDate = maps:get(<<"due_date">>, PostVals, undefined),
                    case project_milestone_logic:update(Uid, MsId, Name, DueDate) of
                        {ok, Ms} ->
                            elib_response:success(Req0, Ms);
                        {error, {Code, Msg2}} ->
                            elib_response:error(Req0, Msg2, Code)
                    end
            end
    end.

%% @doc 达成里程碑（planned→reached 单向；重复 reach 幂等返回 already_reached）
-spec reach(cowboy_req:req(), map()) -> cowboy_req:req().
reach(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_milestone_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, MsId} ->
            case project_milestone_logic:reach(Uid, MsId) of
                {ok, Ms, Flag} ->
                    elib_response:success(Req0, Ms#{status_flag => Flag});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% status 字段显式拒绝（状态机唯一入口为 /reach；白名单外字段忽略）
-spec reject_status_param(map()) -> ok | {error, binary()}.
reject_status_param(PostVals) ->
    case maps:is_key(<<"status">>, PostVals) of
        true ->
            {error, <<"里程碑状态请通过 /reach 接口流转，不接受 status 字段"/utf8>>};
        false ->
            ok
    end.

%% @doc 解析 project_id（路径 binding 优先）
-spec resolve_project_id(cowboy_req:req(), map()) ->
    {ok, integer()} | {error, cowboy_req:req()}.
resolve_project_id(Req0, PostVals) ->
    Raw =
        case cowboy_req:binding(project_id, Req0) of
            undefined -> maps:get(<<"project_id">>, PostVals, undefined);
            Binding -> Binding
        end,
    Id = elib_cnv:safe_to_integer(Raw),
    case is_integer(Id) andalso Id > 0 of
        true ->
            {ok, Id};
        false ->
            {error, elib_response:error(Req0, <<"project id 必须"/utf8>>, 400)}
    end.

%% @doc 解析 milestone_id（路径 binding 优先）
-spec resolve_milestone_id(cowboy_req:req(), map()) ->
    {ok, integer()} | {error, cowboy_req:req()}.
resolve_milestone_id(Req0, PostVals) ->
    Raw =
        case cowboy_req:binding(milestone_id, Req0) of
            undefined -> maps:get(<<"milestone_id">>, PostVals, undefined);
            Binding -> Binding
        end,
    Id = elib_cnv:safe_to_integer(Raw),
    case is_integer(Id) andalso Id > 0 of
        true ->
            {ok, Id};
        false ->
            {error, elib_response:error(Req0, <<"milestone id 必须"/utf8>>, 400)}
    end.
