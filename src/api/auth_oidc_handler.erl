%%%-------------------------------------------------------------------
%%% @doc OIDC 登录流处理器（P0-C 企业 SSO）
%%%
%%% API 端点（均在 imboy_router:open/0 白名单——浏览器重定向无 sign/did 头）:
%%% - GET  /api/v1/auth/oidc/authorize?client=app|test - 302 跳转 IdP
%%% - GET  /api/v1/auth/oidc/callback?code&state       - IdP 回调，换 token 签发
%%% - POST /api/v1/auth/oidc/exchange {"otc": "..."}   - App 深链一次性码换登录 payload
%%% @end
%%%-------------------------------------------------------------------
-module(auth_oidc_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([handle_request/2]).

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% Cowboy Callbacks
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {cowboy_rest, cowboy_req:req(), map()}.
init(Req, State) ->
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), map()) ->
    {[binary()], cowboy_req:req(), map()}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), map()) ->
    {[{binary(), atom()}], cowboy_req:req(), map()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request}], Req, State}.

-spec content_types_accepted(cowboy_req:req(), map()) ->
    {[{binary(), atom()}], cowboy_req:req(), map()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_request}], Req, State}.

%% ===================================================================
%% Request Handler
%% ===================================================================

-spec handle_request(cowboy_req:req(), map()) ->
    {stop, cowboy_req:req(), map()}.
handle_request(Req0, State) ->
    Action = maps:get(action, State, undefined),
    Method = cowboy_req:method(Req0),
    Req1 =
        case {Method, Action} of
            {<<"GET">>, authorize} -> handle_authorize(Req0);
            {<<"GET">>, callback} -> handle_callback(Req0);
            {<<"POST">>, exchange} -> handle_exchange(Req0);
            _ -> elib_response:error(Req0, <<"Bad Request"/utf8>>, ?ERR_BAD_REQUEST)
        end,
    {stop, Req1, State}.

%% ===================================================================
%% API Handlers
%% ===================================================================

%% @doc GET /api/v1/auth/oidc/authorize?client=app|test
handle_authorize(Req) ->
    Ip = client_ip(Req),
    %% P2：authorize 也限流，防高频调用膨胀 state 缓存（内存 DoS）
    case login_attempt_ds:check_ip_rate_limit(Ip) of
        {error, rate_limited} ->
            elib_response:error(Req, <<"请求过于频繁，请稍后重试"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            Qs = cowboy_req:parse_qs(Req),
            Client = resolve_client(proplists:get_value(<<"client">>, Qs, <<"app">>)),
            case auth_oidc_logic:authorize(Client) of
                {redirect, Url} ->
                    redirect(Req, Url);
                {error, Msg} ->
                    elib_response:error(Req, Msg, ?ERR_BAD_REQUEST)
            end
    end.

%% @doc client=test 让登录 payload 直出响应体（E2E 用），仅非生产环境允许；
%% 生产一律降级为 app（深链一次性码），避免 JWT 出现在 HTTP 响应体。
-spec resolve_client(binary()) -> binary().
resolve_client(<<"test">>) ->
    case application:get_env(imboy, env) of
        {ok, test} -> <<"test">>;
        _ -> <<"app">>
    end;
resolve_client(_) ->
    <<"app">>.

%% @doc GET /api/v1/auth/oidc/callback?code=xxx&state=xxx
handle_callback(Req) ->
    Params = maps:from_list(cowboy_req:parse_qs(Req)),
    Ip = client_ip(Req),
    case auth_oidc_logic:callback(Params, Ip) of
        {ok, Payload} ->
            elib_response:success(Req, Payload);
        {redirect, Url} ->
            redirect(Req, Url);
        {error, Msg, Code} ->
            elib_response:error(Req, Msg, Code);
        {error, Msg} ->
            elib_response:error(Req, Msg, ?ERR_BAD_REQUEST)
    end.

%% @doc POST /api/v1/auth/oidc/exchange  Body: {"otc": "xxx"}
handle_exchange(Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Otc = decode_otc(Body),
    case auth_oidc_logic:exchange(Otc) of
        {ok, Payload} ->
            elib_response:success(Req1, Payload);
        {error, Msg} ->
            elib_response:error(Req1, Msg, ?ERR_BAD_REQUEST)
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 从请求体解析 otc 字段；非法/非对象 JSON 一律回退空（不泄错误细节）
-spec decode_otc(binary()) -> binary().
decode_otc(Body) ->
    try
        Data = jsx:decode(Body, [return_maps]),
        maps:get(<<"otc">>, Data, <<>>)
    catch
        _:_ -> <<>>
    end.

-spec redirect(cowboy_req:req(), binary()) -> cowboy_req:req().
redirect(Req, Url) ->
    cowboy_req:reply(302, #{<<"location">> => Url}, <<>>, Req).

%% @doc 提取客户端真实 IP（代理头优先，回退 peer）
-spec client_ip(cowboy_req:req()) -> binary().
client_ip(Req) ->
    case login_attempt_ds:extract_real_ip(cowboy_req:headers(Req)) of
        {ok, <<"0.0.0.0">>} ->
            peer_ip(Req);
        {ok, Ip} ->
            Ip
    end.

peer_ip(Req) ->
    {PeerIp, _Port} = cowboy_req:peer(Req),
    list_to_binary(inet:ntoa(PeerIp)).
