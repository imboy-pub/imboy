-module(websocket_ds).

%%%
% websocket_ds 是 websocket domain service 缩写
%%%
-export([check_subprotocols/2]).
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
                            {ok, cowboy_req:req()} |
                            {cowboy_websocket, cowboy_req:req(), map(), map()}.
check_subprotocols(undefined, Req0) ->
    % HTTP 400 - 请求无效
    Req = cowboy_req:reply(400, Req0),
    {ok, Req};
check_subprotocols([], Req0) ->
    % HTTP 406 - 无法接受
    Req = cowboy_req:reply(406, Req0),
    {ok, Req};
check_subprotocols([H | _Tail], Req0) ->
    % [<<"sip">>,<<"text">>] = Subprotocols
    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, H, Req0),
    {cowboy_websocket, Req, #{}, #{}}.

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
        % TODO check token expire
        {ok, Uid, _ExpireDAt, _Type} ->
            auth_after(Uid, Req, State, Opt);
        {error, 705, _, Map} ->
            Uid = maps:get(uid, Map),
            DID = maps:get(did, State),
            MsgId = <<"please_refresh_token">>,
            ToUid = imboy_hashids:encode(Uid),
            Msg = message_ds:assemble_msg(<<"S2C">>,
                                          <<>>,
                                          ToUid,
                                          #{<<"msg_type">> => MsgId},
                                          MsgId),
            Msg2 = jsone:encode(Msg, [native_utf8]),
            Fun = fun() ->
                     Li = imboy_syn:list_by_uid(Uid),
                     Reason = <<"token invalid, please login again.">>,
                     [Pid ! {close, 4006, Reason} || {Pid, {_DType1, DID1}} <- Li, DID1 == DID]
                  end,
            % 只给当前设备发生消息
            message_ds:send_next(Uid, MsgId, Msg2, [7000, 11000] ++ [Fun], [DID], true),
            auth_after(Uid, Req, State, Opt);
        {error, Code, Msg, _Map} ->
            {ok, Req, State#{error => Code, msg => Msg}}
    end;
auth(Auth, Req0, State0, _Opt) ->
    ok = ?DEBUG_LOG(["Auth", Auth]),
    % HTTP 412 - 先决条件失败 缺少token参数
    Req1 = cowboy_req:reply(412, Req0),
    {ok, Req1, State0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
                    {ok, cowboy_req:req(), map()} |
                    {cowboy_websocket, cowboy_req:req(), map(), map()}.
% auth_after(true, _Uid, Req0, State0, _Opt) ->
%     % imboy_log:warning("DeviceID ~p is online", [State0]),
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
% Cowboy关闭连接空闲128秒 默认值为 60000
-spec idle_timeout(integer()) -> integer().
idle_timeout(_Uid) ->
    128000.
