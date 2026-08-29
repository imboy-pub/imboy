-module(project_channel_handler).
%%%
% 项目↔频道关联 + 四类聚合 HTTP handler（channel-firstclass W2 ZC-04）
%
% 关联管理：link（幂等 created|existing）/ unlink / list；project.links
% 全量替换 update-links；Pinned / Resources / Activity / Related Posts
% 四个有界只读聚合。业务规则与权限见 project_channel_logic。
%
%%% =====================================================================
%%% 路由片段清单（ZC-05 统一注册 imboy_router.erl 时按此合并；本模块不改 router）
%%% =====================================================================
%%% METHOD path                                                            → handler 动作
%%% POST   /api/v1/projects/:project_id/channels                           → channels（method 分派：POST=link，body: channel_id）
%%% GET    /api/v1/projects/:project_id/channels                           → channels（method 分派：GET=list）
%%% POST   /api/v1/projects/:project_id/channels/:channel_id/unlink        → unlink
%%% POST   /api/v1/projects/:project_id/links/update                       → update_links（body: links=[{name,url}]）
%%% GET    /api/v1/projects/:project_id/aggregations/pinned                → pinned
%%% GET    /api/v1/projects/:project_id/aggregations/resources             → resources
%%% GET    /api/v1/projects/:project_id/aggregations/activity              → activity
%%% GET    /api/v1/projects/:project_id/aggregations/related_posts         → related_posts
%%%
%%% 全部路由 JWT 保护（auth_middleware /api/v1/* 默认门），State 携带 current_uid。
%%% =====================================================================

-behavior(cowboy_rest).

%% link/2 是本模块自定义动作函数，与 auto-imported BIF erlang:link/2 同名冲突
-compile({no_auto_import, [link/2]}).

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

%% 集合路径（/channels）同路径双语义：POST=关联 / GET=列表（method 分派模式）
-spec resolve_action(atom(), cowboy_req:req()) -> atom().
resolve_action(channels, Req) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> link;
        _ -> list
    end;
resolve_action(Action, _Req) ->
    Action.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(link, Req, State) -> link(Req, State);
handle_action(list, Req, State) -> list(Req, State);
handle_action(unlink, Req, State) -> unlink(Req, State);
handle_action(update_links, Req, State) -> update_links(Req, State);
handle_action(pinned, Req, State) -> pinned(Req, State);
handle_action(resources, Req, State) -> resources(Req, State);
handle_action(activity, Req, State) -> activity(Req, State);
handle_action(related_posts, Req, State) -> related_posts(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 关联频道（Owner/active Member 非 guest；幂等）
-spec link(cowboy_req:req(), map()) -> cowboy_req:req().
link(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_project_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            ChannelId = elib_cnv:safe_to_integer(maps:get(<<"channel_id">>, PostVals, undefined)),
            case is_integer(ChannelId) andalso ChannelId > 0 of
                false ->
                    elib_response:error(Req0, <<"channel_id 必须为正整数"/utf8>>, 400);
                true ->
                    case project_channel_logic:link(Uid, ProjectId, ChannelId) of
                        {ok, created} ->
                            elib_response:success(Req0, #{status_flag => created});
                        {ok, existing} ->
                            %% 重复请求幂等：返回 existing，不报错、不重复入库
                            elib_response:success(Req0, #{status_flag => existing});
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% @doc 项目关联频道列表（active Member/guest 可读）
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_project_id(Req0, #{}) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            {Page, Size} = elib_param:page(Req0),
            case project_channel_logic:list_channels(Uid, ProjectId, Page, Size) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 解除关联（缺失关联 404）
-spec unlink(cowboy_req:req(), map()) -> cowboy_req:req().
unlink(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_project_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            ChannelId =
                case cowboy_req:binding(channel_id, Req0) of
                    undefined ->
                        elib_cnv:safe_to_integer(maps:get(<<"channel_id">>, PostVals, undefined));
                    Binding ->
                        elib_cnv:safe_to_integer(Binding)
                end,
            case is_integer(ChannelId) andalso ChannelId > 0 of
                false ->
                    elib_response:error(Req0, <<"channel_id 必须为正整数"/utf8>>, 400);
                true ->
                    case project_channel_logic:unlink(Uid, ProjectId, ChannelId) of
                        {ok, unlinked} ->
                            elib_response:success(Req0, #{status_flag => unlinked});
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% @doc 全量替换 project.links（links=[{name,url}]，应用层校验）
-spec update_links(cowboy_req:req(), map()) -> cowboy_req:req().
update_links(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_project_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            Links = maps:get(<<"links">>, PostVals, []),
            case project_channel_logic:update_links(Uid, ProjectId, Links) of
                {ok, Saved} ->
                    elib_response:success(Req0, #{links => Saved});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc Pinned 聚合（置顶消息元数据；排除公告形态）
-spec pinned(cowboy_req:req(), map()) -> cowboy_req:req().
pinned(Req0, State) ->
    read_agg(pinned, Req0, State).

%% @doc Resources 聚合（project.links 原样返回）
-spec resources(cowboy_req:req(), map()) -> cowboy_req:req().
resources(Req0, State) ->
    read_agg(resources, Req0, State).

%% @doc Activity 聚合（事件元数据流；payload 无正文字段）
-spec activity(cowboy_req:req(), map()) -> cowboy_req:req().
activity(Req0, State) ->
    read_agg(activity, Req0, State).

%% @doc Related Posts 聚合（关联频道最近帖子有界摘要，不含正文）
-spec related_posts(cowboy_req:req(), map()) -> cowboy_req:req().
related_posts(Req0, State) ->
    read_agg(related_posts, Req0, State).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% 四聚合共用的只读分派（pinned/activity 带 page/size；resources/related_posts 无参）
-spec read_agg(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
read_agg(Action, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_project_id(Req0, #{}) of
        {error, Req} ->
            Req;
        {ok, ProjectId} ->
            Result =
                case Action of
                    pinned ->
                        {Page, Size} = elib_param:page(Req0),
                        project_channel_logic:pinned(Uid, ProjectId, Page, Size);
                    activity ->
                        {Page, Size} = elib_param:page(Req0),
                        project_channel_logic:activity(Uid, ProjectId, Page, Size);
                    resources ->
                        project_channel_logic:resources(Uid, ProjectId);
                    related_posts ->
                        project_channel_logic:related_posts(Uid, ProjectId)
                end,
            case Result of
                {ok, Data} ->
                    elib_response:success(Req0, Data);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 解析 project_id（路径 binding 优先，body 兜底）
-spec resolve_project_id(cowboy_req:req(), map()) ->
    {ok, integer()} | {error, cowboy_req:req()}.
resolve_project_id(Req0, PostVals) ->
    RawProject =
        case cowboy_req:binding(project_id, Req0) of
            undefined -> maps:get(<<"project_id">>, PostVals, undefined);
            Binding -> Binding
        end,
    ProjectId = elib_cnv:safe_to_integer(RawProject),
    case is_integer(ProjectId) andalso ProjectId > 0 of
        true ->
            {ok, ProjectId};
        false ->
            {error, elib_response:error(Req0, <<"project_id 必须为正整数"/utf8>>, 400)}
    end.
