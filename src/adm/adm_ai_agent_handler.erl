-module(adm_ai_agent_handler).

%%%
% AI Agent 账号管理控制器 / AI Agent account management controller
%
% 路由（前端 baseURL=/api/adm）：
%   GET  /adm/ai_agent/list          -> 分页列出 agent
%   GET  /adm/ai_agent/detail        -> ?user_id 单个 agent 详情
%   POST /adm/ai_agent/create        -> 新建 agent（建号+绑 provider/role/owner）
%   POST /adm/ai_agent/update        -> 更新既有 agent 绑定
%   POST /adm/ai_agent/set_status    -> 启用/停用（status 0|1）
%
% 权限：users:read 读，users:create 建，users:update 改。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").

-define(PERM_READ, <<"users:read">>).
-define(PERM_CREATE, <<"users:create">>).
-define(PERM_UPDATE, <<"users:update">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list -> list(Method, Req0, State);
            detail -> detail(Method, Req0, State);
            create -> create(Method, Req0, State);
            update -> update(Method, Req0, State);
            set_status -> set_status(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        case ai_agent_ds:list(Page, Size) of
            {ok, P} -> elib_response:success(Req0, P);
            {error, _} -> elib_response:error(Req0, <<"读取 Agent 列表失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
list(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        {ok, UserId} = elib_param:int(user_id, Req0, 0),
        case ai_agent_ds:get(UserId) of
            {ok, Agent} -> elib_response:success(Req0, Agent);
            {error, notfound} -> elib_response:error(Req0, <<"Agent 不存在"/utf8>>, ?ERR_BAD_REQUEST);
            {error, _} -> elib_response:error(Req0, <<"读取 Agent 详情失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
detail(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
create(<<"POST">>, Req0, State) ->
    with_perm(?PERM_CREATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case ai_agent_ds:create(PostVals) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"创建成功"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
create(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec update(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
update(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        UserId = ec_cnv:to_integer(maps:get(<<"user_id">>, PostVals, 0)),
        case ai_agent_ds:update(UserId, PostVals) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"更新成功"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
update(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec set_status(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
set_status(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        UserId = ec_cnv:to_integer(maps:get(<<"user_id">>, PostVals, 0)),
        Status = ec_cnv:to_integer(maps:get(<<"status">>, PostVals, 1)),
        case ai_agent_ds:set_status(UserId, Status) of
            {ok, _} -> elib_response:success(Req0, #{}, <<"操作成功"/utf8>>);
            {error, _} -> elib_response:error(Req0, <<"操作失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
set_status(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 权限门控：ok 则执行 Fun，否则回 acl 的拒绝 Req
-spec with_perm(binary(), map(), cowboy_req:req(), fun(() -> cowboy_req:req())) ->
    cowboy_req:req().
with_perm(Perm, State, Req0, Fun) ->
    case adm_acl:ensure_permission(State, Perm, Req0) of
        ok -> Fun();
        {error, Req1} -> Req1
    end.

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
