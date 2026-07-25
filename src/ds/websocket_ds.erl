-module(websocket_ds).
-dialyzer({nowarn_function, [auth/4]}).

%%%
% websocket_ds 是 websocket domain service 缩写
%%%
-export([check_subprotocols/2]).
-export([select_subprotocol/1]).
-export([auth/4]).
-export([idle_timeout/1]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 检查WebSocket子协议
%%
%% 验证和选择WebSocket连接支持的子协议
%%
%% @param Subprotocols 子协议列表
%% @param Req0 Cowboy请求对象
%% @returns {ok, any()} | {cowboy_websocket, any()} 处理结果
-spec check_subprotocols(undefined | [binary()], cowboy_req:req()) ->
    {ok, cowboy_req:req()}
    | {cowboy_websocket, cowboy_req:req(), map(), map()}.
check_subprotocols(undefined, Req0) ->
    % HTTP 400 - 请求无效
    Req = cowboy_req:reply(400, Req0),
    {ok, Req};
check_subprotocols([], Req0) ->
    % HTTP 406 - 无法接受
    Req = cowboy_req:reply(406, Req0),
    {ok, Req};
check_subprotocols([_ | _] = SubPt, Req0) ->
    case select_subprotocol(SubPt) of
        undefined ->
            Req = cowboy_req:reply(406, Req0),
            {ok, Req};
        Selected ->
            Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, Selected, Req0),
            {cowboy_websocket, Req, #{}, #{}}
    end.

%% @doc 从客户端支持的子协议列表中选择最佳协议
%% 优先级：imboy.v2 > imboy-protobuf > imboy-json > text
-spec select_subprotocol([binary()] | undefined) -> binary() | undefined.
select_subprotocol(Subprotocols) when is_list(Subprotocols), length(Subprotocols) > 0 ->
    Preferred = [<<"imboy.v2">>, <<"imboy-protobuf">>, <<"imboy-json">>, <<"text">>],
    select_first_match(Preferred, Subprotocols);
select_subprotocol(_) ->
    undefined.

%% @private
-spec select_first_match([binary()], [binary()]) -> binary() | undefined.
select_first_match([], _) ->
    undefined;
select_first_match([P | Rest], Subprotocols) ->
    case lists:member(P, Subprotocols) of
        true -> P;
        false -> select_first_match(Rest, Subprotocols)
    end.

%% @doc WebSocket认证处理
%%
%% 验证WebSocket连接的token，处理认证结果和错误情况
%%
%% @param Token 认证token
%% @param Req Cowboy请求对象
%% @param State 请求状态
%% @param Opt 额外选项
%% @returns any() 认证结果
-spec auth(binary(), cowboy_req:req(), map(), map()) ->
    {ok, cowboy_req:req(), map()} | {cowboy_websocket, cowboy_req:req(), map(), map()}.
auth(Token, Req, State, Opt) when is_binary(Token) ->
    % ?DEBUG_LOG(["token", Token, token_ds:decrypt_token(Token)]),
    case token_ds:decrypt_token(Token) of
        % Token 有效且未过期（token_ds 已检查过期）
        {ok, Uid, ExpireDAt, <<"tk">>, Did} ->
            % 将过期时间传递给后续处理，便于提前刷新 Token
            State1 = State#{token_expire_at => ExpireDAt, token_type => <<"tk">>},
            auth_device(Uid, Did, Req, State1, Opt);
        %% refresh token（356 天有效期）不是 WS 门票：只接受 <<"tk">>，
        %% 与 HTTP 侧 auth_ds:verify_token/1 的既有行为对齐。
        {ok, _Uid, _ExpireDAt, _OtherType, _Did} ->
            ok = ?WARN_LOG([ws_refresh_token_rejected]),
            Req2 = cowboy_req:reply(
                401,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"x-token-error">> => <<"refresh_not_allowed">>
                },
                <<"{\"code\":901,\"msg\":\"token_refresh_not_allowed\"}">>,
                Req
            ),
            {ok, Req2, State#{error => 901, msg => <<"token_refresh_not_allowed">>}};
        {error, 705, _, _Map} ->
            %% 【安全修复】过期 Token 应该拒绝连接，要求客户端重新登录
            %% 原本用 4401 是非法 HTTP 状态码，cowboy 会静默关闭不发 response；
            %% 改为 401 Unauthorized + 业务码头 X-Token-Error 让客户端区分语义。
            ok = ?WARN_LOG([token_expired_rejected]),
            Req2 = cowboy_req:reply(
                401,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"x-token-error">> => <<"expired">>
                },
                <<"{\"code\":705,\"msg\":\"token_expired\"}">>,
                Req
            ),
            {ok, Req2, State#{error => 705, msg => <<"token_expired">>}};
        %% 【修复】所有 decrypt 失败都必须显式 reply，否则 cowboy_handler
        %% 默认合成 204 No Content，诊断极不友好。
        %% 706（签名无效/解码崩溃）→ 401 Unauthorized
        {error, 706, _Msg, _Map} ->
            ok = ?WARN_LOG([token_invalid_rejected]),
            Req2 = cowboy_req:reply(
                401,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"x-token-error">> => <<"invalid">>
                },
                <<"{\"code\":706,\"msg\":\"token_invalid\"}">>,
                Req
            ),
            {ok, Req2, State#{error => 706, msg => <<"token_invalid">>}};
        %% 其他未识别错误码也必须 reply，避免 204 回归
        {error, Code, Msg, _Map} ->
            ok = ?WARN_LOG([token_rejected, Code, Msg]),
            Req2 = cowboy_req:reply(
                401,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"x-token-error">> => <<"rejected">>
                },
                <<"{\"code\":0,\"msg\":\"token_rejected\"}">>,
                Req
            ),
            {ok, Req2, State#{error => Code, msg => elib_cnv:safe_to_binary(Msg)}}
    end;
auth(Auth, Req0, State0, _Opt) ->
    ok = ?DEBUG_LOG(["Auth", Auth]),
    % HTTP 412 - 先决条件失败 缺少token参数
    Req1 = cowboy_req:reply(412, Req0),
    {ok, Req1, State0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 设备维度校验：token 的 did 权威 + 设备吊销检查
%%
%% did 以 token 为准：header/query 的 did 是"客户端自称"，可任意伪造，
%% 只有 token 未绑定 did（legacy 签发）时才回退到 websocket_handler 解析出的值。
%% did 不可伪造是设备吊销能生效的前提。
%%
%% did 为空的 legacy token 直接放行（无设备身份可比对，强制失效会造成全端登出）。
-spec auth_device(non_neg_integer(), binary(), cowboy_req:req(), map(), map()) ->
    {ok, cowboy_req:req(), map()}
    | {cowboy_websocket, cowboy_req:req(), map(), map()}.
auth_device(Uid, <<>>, Req, State, Opt) ->
    auth_after(Uid, Req, State, Opt);
auth_device(Uid, Did, Req, State, Opt) when is_binary(Did) ->
    case user_device_ds:is_active(Uid, Did) of
        true ->
            auth_after(Uid, Req, State#{did => Did}, Opt);
        false ->
            ok = ?WARN_LOG([ws_device_revoked, Uid, Did]),
            Req2 = cowboy_req:reply(
                401,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"x-token-error">> => <<"device_revoked">>
                },
                <<"{\"code\":401,\"msg\":\"device_revoked\"}">>,
                Req
            ),
            {ok, Req2, State#{error => 401, msg => <<"device_revoked">>}}
    end.

%% @doc WebSocket认证后的处理
%%
%% 认证成功后设置WebSocket连接的超时时间和用户信息
%%
%% @param Uid 用户ID
%% @param Req Cowboy请求对象
%% @param State 请求状态
%% @param Opt 额外选项
%% @returns {ok, any(), map()} | {cowboy_websocket, any(), map(), map()} WebSocket连接设置
-spec auth_after(non_neg_integer(), cowboy_req:req(), map(), map()) ->
    {ok, cowboy_req:req(), map()}
    | {cowboy_websocket, cowboy_req:req(), map(), map()}.
% auth_after(true, _Uid, Req0, State0, _Opt) ->
%     % elib_log:warning("DeviceID ~p is online", [State0]),
%     % 429 Too Many Requests
%     Req = cowboy_req:reply(429, Req0),
%     {ok, Req, State0};
auth_after(Uid, Req, State, Opt) ->
    Timeout = idle_timeout(Uid),
    {cowboy_websocket, Req, State#{current_uid => Uid}, Opt#{idle_timeout := Timeout}}.

%% @doc 设置用户WebSocket超时时间
%%
%% 根据用户设置WebSocket连接的空闲超时时间
%%
%% @param Uid 用户ID
%% @returns integer() 超时时间（毫秒）
% 设置用户websocket超时时间，默认60秒
% Cowboy关闭连接空闲180秒（客户端心跳60秒，3倍余量） 默认值为 60000
% GAP-07: 从 config_ds 读取可配置超时，支持管理员通过 /adm/config 热更新
-spec idle_timeout(integer()) -> integer().
idle_timeout(_Uid) ->
    config_ds:env(ws_idle_timeout_ms, 180000).
