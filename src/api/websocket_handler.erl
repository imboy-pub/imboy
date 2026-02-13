-module(websocket_handler).
-behavior(cowboy_websocket).
%%%
%% websocket API 获取 header里面的token
%%%
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("log.hrl").
-include("chat.hrl").


%% @doc WebSocket握手
%% 初始化WebSocket连接，验证设备限流和认证
%%
%% @param Req0 Cowboy请求对象，包含设备信息和token
%% @param State0 状态映射
%% @return {ok, Req, State} 或 {cowboy_websocket, Req, State}
%% @end
-spec init(cowboy_req:req(), map()) ->
          {ok, cowboy_req:req(), map()} | {cowboy_websocket, cowboy_req:req(), map(), map()}.
init(Req0, State0) ->
    % Env = os:getenv("IMBOYENV"),
    % DID device id
    % AppVsn: 应用版本
    % DID: 设备 ID
    % DType: 设备类型 (ios/android/web)
    % Auth: JWT token
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Auth = cowboy_req:header(<<"authorization">>, Req0, undefined),
    SubPt = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0),
    % [<<"sip">>,<<"text">>] = Subprotocols

    % ?DEBUG_LOG([Env, DID, DType, Auth, SubPt]),
    Opt0 = #{num_acceptors => infinity,
             max_connections => infinity,
             enable_connect_protocol => true,  % since Cowboy 2.11 set to true in order to use Websocket over HTTP/2 for the time being.
             max_frame_size => 2097152,  % 2MB
             % Cowboy关闭连接空闲128秒 默认值为 60000
             % ./apps/imds/src/websocket_ds.erl 里面的 idle_timeout 方法会覆盖该值
             idle_timeout => 128000},
    State1 = State0#{dtype => DType, did => DID, vsn => AppVsn},
    case throttle:check(throttle_ws, DID) of
        {limit_exceeded, _, _} ->
            ok = elib_log:warning("DeviceID ~p exceeded api limit", [DID]),
            % 429 Too Many Requests
            Req = cowboy_req:reply(429, Req0),
            {ok, Req, State0};
        _ ->
            % ?DEBUG_LOG([SubPt]),
            case websocket_ds:check_subprotocols(SubPt, Req0) of
                {ok, Req1} ->
                    {ok, Req1, State1};
                {cowboy_websocket, Req1, _, _} ->
                    % ?DEBUG_LOG([State1]),
                    Auth2 = auth_ds:parse_authorization_header(Auth),
                    websocket_ds:auth(Auth2, Req1, State1, Opt0)
            end
    end.


%% @doc WebSocket连接初始化
%% 连接建立后的初始化处理，用户上线
%%
%% @param State 状态映射，包含 current_uid 等信息
%% @return {ok, State, hibernate} 或 {reply, {text, binary()}, State, hibernate}
%% @end
-spec websocket_init(map()) -> {ok, map(), hibernate} | {reply, {text, binary()}, map(), hibernate}.
websocket_init(State) ->
    case is_error_state(State) of
        true ->
            Code = maps:get(error, State),
            Msg = #{<<"code">> => Code, <<"msg">> => <<>>, <<"server_ts">> => elib_dt:now()},
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        false ->
            CurrentUid = auth_ds:current_uid(State),
            % 用户上线
            DID = maps:get(did, State, <<>>),
            DType = maps:get(dtype, State, <<>>),
            user_logic:online(CurrentUid, DType, self(), DID),
            {ok, State, hibernate}
    end.


%% @doc 处理WebSocket消息
%% 处理客户端发送的WebSocket消息
%%
%% @param Frame WebSocket帧（ping/pong/text/binary）
%% @param State 状态映射
%% @return {ok, State, hibernate} 或 {reply, ..., State, hibernate}
%% @end
-spec websocket_handle(ping | pong | {ping, binary()} | {pong, binary()} | {text, binary()} | {binary, binary()},
                       map()) ->
          {ok, map(), hibernate} | {reply, {text, binary()} | {binary, binary()}, map(), hibernate}.

%% WebSocket 协议层 Ping 帧（RFC 6455 标准）
%% Cowboy 会自动回复 Pong，这里仅用于监控/日志
%% 注意：不需要显式返回 {reply, pong, ...}，因为 Cowboy 已经自动回复了
websocket_handle(ping, State) ->
    ok = ?DEBUG_LOG({websocket_ping, no_payload}),
    {ok, State, hibernate};

%% WebSocket 协议层 Ping 帧（带 payload）
%% Cowboy 会自动回复 Pong，这里仅用于监控/日志
websocket_handle({ping, Payload}, State) ->
    ok = ?DEBUG_LOG({websocket_ping, byte_size(Payload)}),
    {ok, State, hibernate};

%% 应用层心跳消息（文本格式，用于业务层面的心跳检测）
%% 支持大小写，但不建议混用，建议统一使用小写
websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State, hibernate};
websocket_handle({text, <<"PING">>}, State) ->
    {reply, {text, <<"PONG">>}, State, hibernate};

% 客户端确认消息
% CLIENT_ACK,type,msgid,did
websocket_handle({text, <<"CLIENT_ACK,", Tail/binary>>}, State) ->
    handle_client_ack(Tail, State);
websocket_handle({text, Msg}, State) ->
    handle_json_message(Msg, State);
websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.


%% ===================================================================
%% 消息处理辅助函数
%% ===================================================================

%% @doc 处理 CLIENT_ACK 消息（客户端确认消息已接收）
%% 格式: CLIENT_ACK,type,msgid,did
-spec handle_client_ack(binary(), map()) -> {reply, {text, binary()}, map(), hibernate}.
handle_client_ack(Tail, State) ->
    ok = ?DEBUG_LOG({client_ack, Tail}),
    CurrentUid = auth_ds:current_uid(State),
    try
        binary:split(Tail, <<",">>, [global])
    of
        [Type, MsgId, DID] ->
            case validate_ack_params(Type, MsgId, DID, State) of
                ok ->
                    ok = ?DEBUG_LOG({client_ack_received, Type, MsgId, DID}),
                    websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                    %% 【改进】返回ACK确认消息，防止客户端重试
                    %%  客户端根据“CLIENT_ACK_CONFIRM”处理 CLIENT_ACK 的确认消息，通知AckManager停止重试
                    AckConfirmMsg = #{<<"id">> => MsgId,
                                      <<"action">> => <<"CLIENT_ACK_CONFIRM">>,
                                      <<"server_ts">> => elib_dt:millisecond()},
                    process_ack_type(Type, MsgId, CurrentUid, DID),
                    {reply, {text, jsone:encode(AckConfirmMsg, [native_utf8])}, State, hibernate};
                {error, Reason} ->
                    ok = ?WARN_LOG({client_ack_invalid_params, Reason, Type, MsgId, DID}),
                    ErrorMsg = #{<<"action">> => <<"CLIENT_ACK_ERROR">>,
                                 <<"reason">> => Reason,
                                 <<"server_ts">> => elib_dt:millisecond()},
                    {reply, {text, jsone:encode(ErrorMsg, [native_utf8])}, State, hibernate}
            end
    catch
        Class:Reason:_Stacktrace ->
            %% 【修复】返回错误响应并记录详细日志
            ok = ?ERROR_LOG({client_ack_parse_error, Class, Reason}),
            ErrorMsg = #{<<"action">> => <<"CLIENT_ACK_ERROR">>,
                         <<"reason">> => <<"parse_error">>,
                         <<"server_ts">> => elib_dt:millisecond()},
            {reply, {text, jsone:encode(ErrorMsg, [native_utf8])}, State, hibernate}
    end.


%% @doc 处理 JSON 消息（包括普通消息和 action 消息）
-spec handle_json_message(binary(), map()) -> {ok, map(), hibernate} | {reply, {text, binary()}, map(), hibernate}.
handle_json_message(Msg, State) ->
    ok = ?DEBUG_LOG({json_message, byte_size(Msg)}),
    try
        CurrentUid = auth_ds:current_uid(State),

        %% 使用 v2.0 解码函数解析消息
        Data = message_ds:decode_websocket_message(Msg),

        MsgId = maps:get(<<"id">>, Data),
        Type = maps:get(<<"type">>, Data),

        %% 注入发送者设备信息到 payload
        Payload0 = maps:get(<<"payload">>, Data),

        % 将发送者的设备信息（sender_did 和 sender_dtype）注入到消息的 payload 中。
        Payload = message_ds:inject_sender_device(Payload0, State),
        Data2 = maps:put(<<"payload">>, Payload, Data),

        % ?DEBUG_LOG([MsgId, Type, Data]),

        %% 统一消息路由：根据 action 和 type 分发到对应的 logic 模块
        Result = message_router_logic:route(MsgId, CurrentUid, Data2, Type, Msg),

        case Result of
            ok ->
                {ok, State, hibernate};
            {reply, Msg2} ->
                {reply, {text, jsone:encode(Msg2, [native_utf8])}, State, hibernate}
        end
    catch
        Class:Reason:Stacktrace ->
            ok = ?ERROR_LOG({json_message_error, Class, Reason, Stacktrace}),
            {ok, State, hibernate}
    end.


%% 处理从其他进程发送到 WebSocket 进程的消息
websocket_info({reply, Msg}, State) when is_map(Msg); is_list(Msg) ->
    {reply, {text, jsone:encode(Msg, [native_utf8])}, State, hibernate};
websocket_info({reply, Msg}, State) ->
    {reply, {text, Msg}, State, hibernate};

%% 处理消息重试超时
websocket_info({timeout, _Ref, {[], _, Msg}}, State) ->
    ok = ?INFO_LOG({retry_timeout, stop_online_retry, Msg}),
    {reply, {text, Msg}, State, hibernate};
websocket_info({timeout, _Ref, {MsLi, {Uid, DID, MsgId}, Msg}}, State) ->
    ok = ?DEBUG_LOG({timeout_retry, MsgId, Uid, DID}),
    AckReceivedKey = {ack_received, Uid, DID, MsgId},
    case imboy_cache:get(AckReceivedKey) of
        {ok, true} ->
            ok = ?DEBUG_LOG({timeout_ack_received, MsgId, Uid}),
            {ok, State, hibernate};
        {ok, _} ->
            ok = ?WARN_LOG({timeout_ack_flag_invalid, MsgId}),
            message_ds:send_next(Uid, MsgId, Msg, MsLi, [DID], false),
            {reply, {text, Msg}, State, hibernate};
        undefined ->
            ok = ?DEBUG_LOG({timeout_no_ack, MsgId, Uid}),
            message_ds:send_next(Uid, MsgId, Msg, MsLi, [DID], false),
            {reply, {text, Msg}, State, hibernate}
    end;

%% 处理其他超时消息
websocket_info({timeout, _Ref, Msg}, State) ->
    ok = ?DEBUG_LOG({timeout, Msg}),
    {reply, {text, Msg}, State, hibernate};
websocket_info({close, CloseCode, Reason}, State) ->
    ok = ?DEBUG_LOG({close, CloseCode, Reason}),
    {reply, {close, CloseCode, Reason}, State};
%% 停止 WebSocket 连接
websocket_info(stop, State) ->
    ok = ?DEBUG_LOG({stop}),
    {stop, State};

%% 处理跨节点的 ACK 取消消息
websocket_info({ack_cancel, Uid, DID, MsgId, Timestamp}, State) ->
    ok = ?DEBUG_LOG({ack_cancel_from_remote, MsgId, Uid, DID, Timestamp}),
    websocket_logic:handle_ack_cancel(Uid, DID, MsgId),
    {ok, State, hibernate};

%% 处理设备踢出消息
websocket_info({kick_device, ReasonMap}, State) ->
    ok = ?INFO_LOG({device_kicked, ReasonMap}),
    Reason = maps:get(<<"reason">>, ReasonMap, <<"设备被踢出"/utf8>>),
    Msg = #{
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"device_kicked">>,
        <<"payload">> => ReasonMap,
        <<"server_ts">> => elib_dt:millisecond()
    },
    {reply, {text, jsone:encode(Msg, [native_utf8])}, {close, 4000, Reason}, State};

websocket_info(_Info, State) ->
    {ok, State}.


%% @doc 处理Erlang消息
%% 处理从其他进程发送到WebSocket进程的消息
%%
%% @param Info 消息内容
%% @param State 状态映射
%% @return {ok, State, hibernate} 或 {reply, ..., State, hibernate} 或 {stop, State}
%% @end
-spec websocket_info(term(), map()) ->
          {ok, map(), hibernate} |
          {reply, {text, binary()} | {close, integer(), binary()}, map(), hibernate} |
          {stop, map()}.


%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
%% @doc WebSocket连接终止
%% 连接关闭时的清理工作，用户下线
%%
%% @param Reason 终止原因
%% @param _Req Cowboy请求对象
%% @param State 状态映射
%% @return ok
%% @end
-spec terminate(term(), cowboy_req:req(), map()) -> ok.
terminate(Reason, _Req, State) ->
    ok = ?DEBUG_LOG({terminate, Reason}),
    case maps:find(current_uid, State) of
        {ok, Uid} when is_integer(Uid) ->
            DID = maps:get(did, State, <<>>),
            user_logic:offline(Uid, self(), DID),
            ok;
        error ->
            ok
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 检查 State 是否包含错误状态
%% @return true | false
-spec is_error_state(map()) -> boolean().
is_error_state(State) ->
    case maps:find(error, State) of
        {ok, _} ->
            true;
        error ->
            false
    end.


%% @doc 验证 CLIENT_ACK 参数
%% 验证规则:
%% 1. MsgId 长度: 1-128 字节
%% 2. DID 长度: 1-64 字节
%% 3. DID 必须与当前连接的 DID 匹配
%% @return ok | {error, binary()}
-spec validate_ack_params(binary(), binary(), binary(), map()) -> ok | {error, binary()}.
validate_ack_params(_Type, MsgId, DID, State) ->
    CurrentDID = maps:get(did, State, <<>>),
    MsgIdSize = byte_size(MsgId),
    DIDSize = byte_size(DID),
    case {MsgIdSize > 0 andalso MsgIdSize =< 128, DIDSize > 0 andalso DIDSize =< 64, DID =:= CurrentDID} of
        {true, true, true} ->
            ok;
        {false, _, _} ->
            {error, <<"invalid_msgid">>};
        {_, false, _} ->
            {error, <<"invalid_did">>};
        {_, _, false} ->
            {error, <<"did_mismatch">>}
    end.


%% @doc 处理 ACK 消息类型
-spec process_ack_type(binary(), binary(), integer(), binary()) -> ok.
process_ack_type(<<"C2C">>, MsgId, CurrentUid, DID) ->
    ok = ?DEBUG_LOG({client_ack_processing_c2c, MsgId}),
    msg_c2c_logic:c2c_client_ack(MsgId, CurrentUid, DID);
process_ack_type(<<"C2G">>, MsgId, CurrentUid, DID) ->
    ok = ?DEBUG_LOG({client_ack_processing_c2g, MsgId}),
    msg_c2g_logic:c2g_client_ack(MsgId, CurrentUid, DID);
process_ack_type(<<"S2C">>, MsgId, CurrentUid, DID) ->
    ok = ?DEBUG_LOG({client_ack_processing_s2c, MsgId}),
    msg_s2c_logic:s2c_client_ack(MsgId, CurrentUid, DID);
process_ack_type(<<"C2S">>, MsgId, _CurrentUid, _DID) ->
    ok = ?DEBUG_LOG({client_ack_processing_c2s, MsgId});
process_ack_type(Type, MsgId, _CurrentUid, _DID) ->
    ok = ?WARN_LOG({client_ack_unknown_type, Type, MsgId}).
