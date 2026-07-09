-module(adm_mcp_handler).

%%%
% MCP 治理管理控制器 / MCP governance admin controller
%
% 路由（前端 baseURL=/api/adm）：
%   GET  /adm/mcp/clients          -> 分页列出 MCP 客户端（status/keyword 过滤）
%   POST /adm/mcp/clients/approve  -> 审批通过（授予当前全部 tool）
%   POST /adm/mcp/clients/reject   -> 拒绝（revoked + reason）
%   POST /adm/mcp/clients/revoke   -> 撤销（revoked + reason）
%   GET  /adm/mcp/clients/grants   -> ?client_id 查授权 tool/scope
%   GET  /adm/mcp/audit            -> 分页审计
%
% 权限：mcp_clients:read 读，mcp_clients:approve 审批/拒绝/撤销。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").

-define(PERM_READ, <<"mcp_clients:read">>).
-define(PERM_APPROVE, <<"mcp_clients:approve">>).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list -> list(Method, Req0, State);
            approve -> approve(Method, Req0, State);
            reject -> reject(Method, Req0, State);
            revoke -> revoke(Method, Req0, State);
            grants -> grants(Method, Req0, State);
            audit -> audit(Method, Req0, State);
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
        {ok, Status} = elib_param:binary(status, Req0, <<>>),
        {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
        case mcp_governance_logic:list_clients(Page, Size, Status, Keyword) of
            {ok, P} -> elib_response:success(Req0, P);
            {error, _} -> elib_response:error(Req0, <<"读取客户端列表失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
list(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec approve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
approve(<<"POST">>, Req0, State) ->
    with_perm(?PERM_APPROVE, State, Req0, fun() ->
        Post = elib_param:post(Req0),
        transition_reply(
            mcp_governance_logic:approve(client_id(Post), adm_uid(State), ip(Req0)),
            Req0
        )
    end);
approve(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec reject(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
reject(<<"POST">>, Req0, State) ->
    with_perm(?PERM_APPROVE, State, Req0, fun() ->
        Post = elib_param:post(Req0),
        transition_reply(
            mcp_governance_logic:reject(client_id(Post), reason(Post), adm_uid(State), ip(Req0)),
            Req0
        )
    end);
reject(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec revoke(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
revoke(<<"POST">>, Req0, State) ->
    with_perm(?PERM_APPROVE, State, Req0, fun() ->
        Post = elib_param:post(Req0),
        transition_reply(
            mcp_governance_logic:revoke(client_id(Post), reason(Post), adm_uid(State), ip(Req0)),
            Req0
        )
    end);
revoke(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec grants(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
grants(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        {ok, ClientId} = elib_param:int(client_id, Req0, 0),
        case mcp_governance_logic:grants(ClientId) of
            {ok, G} -> elib_response:success(Req0, G);
            {error, _} -> elib_response:error(Req0, <<"读取授权失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
grants(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec audit(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
audit(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        {ok, ClientId} = elib_param:int(client_id, Req0, 0),
        {ok, Action} = elib_param:binary(action, Req0, <<>>),
        case mcp_governance_logic:audit_page(Page, Size, ClientId, Action) of
            {ok, P} -> elib_response:success(Req0, P);
            {error, _} -> elib_response:error(Req0, <<"读取审计失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
audit(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ---- helpers ----

transition_reply({ok, R}, Req0) ->
    elib_response:success(Req0, R, <<"操作成功"/utf8>>);
transition_reply({error, Reason}, Req0) ->
    elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST).

client_id(Post) ->
    ec_cnv:to_integer(maps:get(<<"client_id">>, Post, 0)).

reason(Post) ->
    maps:get(<<"reason">>, Post, <<>>).

adm_uid(State) ->
    maps:get(adm_user_id, State, 0).

ip(Req0) ->
    elib_req:peer_ip(Req0).

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
