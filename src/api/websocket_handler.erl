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
-include("imboy_frame.hrl").

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
    Opt0 = #{
        num_acceptors => infinity,
        max_connections => infinity,
        % since Cowboy 2.11 set to true in order to use Websocket over HTTP/2 for the time being.
        enable_connect_protocol => true,
        % 2MB
        max_frame_size => 2097152,
        % Cowboy关闭连接空闲180秒（客户端心跳60秒，3倍余量） 默认值为 60000
        % ./apps/imds/src/websocket_ds.erl 里面的 idle_timeout 方法会覆盖该值
        idle_timeout => 180000
    },
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
                    {Protocol, Framing} = negotiate_protocol(SubPt),
                    State2 = State1#{protocol => Protocol, framing => Framing},
                    Auth2 = auth_ds:parse_authorization_header(Auth),
                    websocket_ds:auth(Auth2, Req1, State2, Opt0)
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
            %% 连接级错误始终使用 JSON text frame（非 IM 消息格式）
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        false ->
            CurrentUid = auth_ds:current_uid(State),
            % 用户上线
            DID = maps:get(did, State, <<>>),
            DType = maps:get(dtype, State, <<>>),
            user_logic:online(CurrentUid, DType, self(), DID),
            %% 版本保护：检查客户端版本是否过低，发送升级提醒
            AppVsn = maps:get(vsn, State, undefined),
            maybe_send_upgrade_notice(AppVsn, DType, DID),
            {ok, State, hibernate}
    end.

%% @doc 处理WebSocket消息
%% 处理客户端发送的WebSocket消息
%%
%% @param Frame WebSocket帧（ping/pong/text/binary）
%% @param State 状态映射
%% @return {ok, State, hibernate} 或 {reply, ..., State, hibernate}
%% @end
-spec websocket_handle(
    ping | pong | {ping, binary()} | {pong, binary()} | {text, binary()} | {binary, binary()},
    map()
) ->
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
    %% 对用户发送的业务消息进行速率限制（CLIENT_ACK 不受限）
    CurrentUid = auth_ds:current_uid(State),
    case throttle:check(msg_per_user, CurrentUid) of
        {limit_exceeded, _, _} ->
            ok = ?WARN_LOG({msg_rate_limited, CurrentUid}),
            RateLimitMsg = ws_validation_error(
                <<>>,
                <<"rate_limited">>,
                <<"消息发送过于频繁，请稍后再试"/utf8>>
            ),
            {reply, {text, jsone:encode(RateLimitMsg, [native_utf8])}, State, hibernate};
        _ ->
            handle_json_message(Msg, State)
    end;
websocket_handle({binary, Msg}, State) ->
    Framing = maps:get(framing, State, none),
    Protocol = maps:get(protocol, State, json),
    case Framing of
        v2 ->
            handle_v2_binary(Msg, State);
        _ ->
            handle_legacy_binary(Msg, Protocol, State)
    end;
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.

%% @doc 非 v2 framing 下的 binary 帧处理（原有逻辑）
handle_legacy_binary(Msg, Protocol, State) ->
    case Protocol of
        protobuf ->
            %% 先解码检查消息类型，CLIENT_ACK 不受速率限制
            %% 解码一次，根据消息类型路由（CLIENT_ACK 不受速率限制）
            try imboy_codec:decode(protobuf, Msg) of
                #{<<"type">> := <<"CLIENT_ACK">>} = Data ->
                    handle_protobuf_client_ack(Data, Msg, State);
                Data0 ->
                    CurrentUid = auth_ds:current_uid(State),
                    case throttle:check(msg_per_user, CurrentUid) of
                        {limit_exceeded, _, _} ->
                            ok = ?WARN_LOG({msg_rate_limited, CurrentUid}),
                            RateLimitMsg = ws_validation_error(
                                <<>>,
                                <<"rate_limited">>,
                                <<"消息发送过于频繁，请稍后再试"/utf8>>
                            ),
                            {reply, ws_reply(protobuf, RateLimitMsg), State, hibernate};
                        _ ->
                            handle_protobuf_message_decoded(Data0, State)
                    end
            catch
                _:_ ->
                    ok = ?WARN_LOG({protobuf_decode_failed, byte_size(Msg)}),
                    ErrorMsg = ws_validation_error(
                        <<>>, <<"invalid_protobuf">>, <<"decode_error">>
                    ),
                    {reply, ws_reply(protobuf, ErrorMsg), State, hibernate}
            end;
        _ ->
            %% JSON 协议不应收到 binary 帧，忽略
            ok = ?WARN_LOG({unexpected_binary_frame, Protocol}),
            {ok, State, hibernate}
    end.

%% @doc v2 framing binary 帧处理：按 Type 分派
-spec handle_v2_binary(binary(), map()) ->
    {ok, map(), hibernate}
    | {reply, {binary, binary()} | {text, binary()}, map(), hibernate}.
handle_v2_binary(Msg, State) ->
    case imboy_codec:unwrap_v2_frame(Msg) of
        {ok, Frame} ->
            Type = imboy_frame:type(Frame),
            Payload = imboy_frame:payload(Frame),
            dispatch_v2_frame(Type, Payload, State);
        {error, Reason} ->
            ok = ?WARN_LOG({v2_frame_decode_failed, Reason, byte_size(Msg)}),
            {ok, State, hibernate}
    end.

%% @doc 根据 v2 帧 Type 分派处理
dispatch_v2_frame(?FRAME_TYPE_HEARTBEAT_PING, <<Seq:16/big-unsigned>>, State) ->
    ok = ?DEBUG_LOG({v2_heartbeat_ping, Seq}),
    Pong = imboy_frame:heartbeat_pong(Seq),
    {reply, {binary, Pong}, State, hibernate};
dispatch_v2_frame(?FRAME_TYPE_HEARTBEAT_PING, _Bad, State) ->
    ok = ?WARN_LOG({v2_heartbeat_ping_bad_payload}),
    {ok, State, hibernate};
dispatch_v2_frame(?FRAME_TYPE_ACK, <<MsgIdInt:64/big-unsigned>>, State) ->
    ok = ?DEBUG_LOG({v2_ack, MsgIdInt}),
    %% 将 msg_id 适配为现有 protobuf ACK 处理管道
    MsgIdBin = integer_to_binary(MsgIdInt),
    AckPayload = #{
        <<"msg_id">> => MsgIdBin,
        <<"did">> => maps:get(did, State, <<>>),
        <<"msg_direction">> => <<"C2C">>
    },
    %% 构造 payload bytes：重用 protobuf 子消息，交给 handle_protobuf_client_ack
    EncodedPayload = imboy_codec:encode_payload(protobuf, <<"client_ack">>, AckPayload),
    Data = #{
        <<"type">> => <<"CLIENT_ACK">>,
        <<"payload">> => EncodedPayload
    },
    handle_protobuf_client_ack(Data, <<>>, State);
dispatch_v2_frame(?FRAME_TYPE_NACK, <<MsgIdInt:64/big-unsigned>>, State) ->
    ok = ?WARN_LOG({v2_nack_received, MsgIdInt}),
    {ok, State, hibernate};
dispatch_v2_frame(?FRAME_TYPE_MSG_C2S, <<"CLIENT_ACK,", Tail/binary>>, State) ->
    %% msg_c2s 帧中的纯文本 CLIENT_ACK（Dart 客户端兼容路径）
    %% 等价于老的 websocket_handle({text, <<"CLIENT_ACK,...">>}, State)
    handle_client_ack(Tail, State);
dispatch_v2_frame(Type, Payload, State) when
    Type =:= ?FRAME_TYPE_MSG_C2C;
    Type =:= ?FRAME_TYPE_MSG_C2G;
    Type =:= ?FRAME_TYPE_MSG_C2S
->
    CurrentUid = auth_ds:current_uid(State),
    case throttle:check(msg_per_user, CurrentUid) of
        {limit_exceeded, _, _} ->
            ok = ?WARN_LOG({msg_rate_limited, CurrentUid}),
            RateLimitMsg = ws_validation_error(
                <<>>,
                <<"rate_limited">>,
                <<"消息发送过于频繁，请稍后再试"/utf8>>
            ),
            {reply, ws_reply(protobuf, v2, RateLimitMsg), State, hibernate};
        _ ->
            dispatch_v2_business_payload(Type, Payload, State)
    end;
dispatch_v2_frame(Type, _Payload, State) ->
    ok = ?WARN_LOG({v2_frame_unsupported_type, Type}),
    {ok, State, hibernate}.

%% @doc 业务消息 payload 宽容解码：先尝试 JSON 文本，再回退 protobuf
%% Dart 客户端当前使用 UTF-8(JSON string) 作为 v2 frame payload；
%% 未来 protobuf 客户端直接发 protobuf 字节。两者共存由此函数桥接。
-spec dispatch_v2_business_payload(0..255, binary(), map()) ->
    {ok, map(), hibernate}
    | {reply, {binary, binary()} | {text, binary()}, map(), hibernate}.
dispatch_v2_business_payload(Type, Payload, State) ->
    case try_decode_json_payload(Payload) of
        {ok, JsonBin} ->
            handle_json_message(JsonBin, State);
        not_json ->
            try imboy_codec:decode(protobuf, Payload) of
                Data0 when is_map(Data0), map_size(Data0) > 0 ->
                    handle_protobuf_message_decoded(Data0, State);
                _ ->
                    ok = ?WARN_LOG({v2_msg_decode_empty, Type, byte_size(Payload)}),
                    {ok, State, hibernate}
            catch
                Class:Reason ->
                    ok = ?WARN_LOG({v2_msg_decode_failed, Class, Reason, Type, byte_size(Payload)}),
                    {ok, State, hibernate}
            end
    end.

%% @doc 尝试把 payload 当作 UTF-8(JSON) 文本解码
%% 返回原始 JSON binary（交由 handle_json_message/2 再次解码并走完整流水线），
%% 避免重复实现 validate/convert/route 流程。
-spec try_decode_json_payload(binary()) -> {ok, binary()} | not_json.
try_decode_json_payload(<<>>) ->
    not_json;
try_decode_json_payload(Payload) ->
    try jsone:decode(Payload, [{object_format, map}]) of
        Map when is_map(Map) ->
            {ok, Payload};
        _ ->
            not_json
    catch
        _:_ ->
            not_json
    end.

%% ===================================================================
%% 消息处理辅助函数
%% ===================================================================

%% @doc 处理文本格式的 CLIENT_ACK 消息（向后兼容）
%% 格式: CLIENT_ACK,type,msgid,did
-spec handle_client_ack(binary(), map()) ->
    {reply, {text, binary()} | {binary, binary()}, map(), hibernate}.
handle_client_ack(Tail, State) ->
    ok = ?DEBUG_LOG({client_ack, Tail}),
    CurrentUid = auth_ds:current_uid(State),
    try binary:split(Tail, <<",">>, [global]) of
        [Type, MsgId, DID] ->
            case validate_ack_params(Type, MsgId, DID, State) of
                ok ->
                    ok = ?DEBUG_LOG({client_ack_received, Type, MsgId, DID}),
                    websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                    AckConfirmMsg = #{
                        <<"id">> => MsgId,
                        <<"type">> => <<"CLIENT_ACK_CONFIRM">>,
                        <<"action">> => <<"CLIENT_ACK_CONFIRM">>,
                        <<"server_ts">> => elib_dt:millisecond()
                    },
                    process_ack_type(Type, MsgId, CurrentUid, DID),
                    %% 协议感知的响应帧
                    Protocol = maps:get(protocol, State, json),
                    {reply, ws_reply(Protocol, AckConfirmMsg), State, hibernate};
                {error, Reason} ->
                    ok = ?WARN_LOG({client_ack_invalid_params, Reason, Type, MsgId, DID}),
                    ErrorMsg = #{
                        <<"action">> => <<"CLIENT_ACK_ERROR">>,
                        <<"reason">> => Reason,
                        <<"server_ts">> => elib_dt:millisecond()
                    },
                    Protocol = maps:get(protocol, State, json),
                    {reply, ws_reply(Protocol, ErrorMsg), State, hibernate}
            end
    catch
        Class:CatchReason2:_Stacktrace ->
            ok = ?ERROR_LOG({client_ack_parse_error, Class, CatchReason2}),
            ErrorMsg2 = #{
                <<"action">> => <<"CLIENT_ACK_ERROR">>,
                <<"reason">> => <<"parse_error">>,
                <<"server_ts">> => elib_dt:millisecond()
            },
            Protocol2 = maps:get(protocol, State, json),
            {reply, ws_reply(Protocol2, ErrorMsg2), State, hibernate}
    end.

%% @doc 处理 protobuf 格式的 CLIENT_ACK 消息
%% IMBoyMessage { type: CLIENT_ACK, payload: PayloadClientAck }
-spec handle_protobuf_client_ack(map(), binary(), map()) ->
    {reply, {text, binary()} | {binary, binary()}, map(), hibernate}.
handle_protobuf_client_ack(Data, _RawMsg, State) ->
    ok = ?DEBUG_LOG({protobuf_client_ack, Data}),
    CurrentUid = auth_ds:current_uid(State),
    try
        %% 从 payload 解码 PayloadClientAck
        PayloadBin = maps:get(<<"payload">>, Data, <<>>),
        AckPayload = imboy_codec:decode_payload(protobuf, <<"client_ack">>, PayloadBin),

        %% 提取 ACK 参数
        MsgDirection = maps:get(
            msg_direction,
            AckPayload,
            maps:get(<<"msg_direction">>, AckPayload, <<>>)
        ),
        MsgId = maps:get(
            msg_id,
            AckPayload,
            maps:get(<<"msg_id">>, AckPayload, <<>>)
        ),
        DID = maps:get(
            did,
            AckPayload,
            maps:get(<<"did">>, AckPayload, <<>>)
        ),

        %% 将 enum atom 转为 binary type string
        Type = ack_direction_to_type(MsgDirection),

        case validate_ack_params(Type, MsgId, DID, State) of
            ok ->
                ok = ?DEBUG_LOG({protobuf_client_ack_received, Type, MsgId, DID}),
                websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
                AckConfirmMsg = #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"CLIENT_ACK_CONFIRM">>,
                    <<"action">> => <<"CLIENT_ACK_CONFIRM">>,
                    <<"server_ts">> => elib_dt:millisecond()
                },
                process_ack_type(Type, MsgId, CurrentUid, DID),
                {reply, ws_reply(protobuf, AckConfirmMsg), State, hibernate};
            {error, Reason} ->
                ok = ?WARN_LOG({protobuf_client_ack_invalid, Reason, MsgId}),
                ErrorMsg = #{
                    <<"action">> => <<"CLIENT_ACK_ERROR">>,
                    <<"reason">> => Reason,
                    <<"server_ts">> => elib_dt:millisecond()
                },
                {reply, ws_reply(protobuf, ErrorMsg), State, hibernate}
        end
    catch
        Class:CatchReason:Stacktrace ->
            ok = ?ERROR_LOG({protobuf_client_ack_error, Class, CatchReason, Stacktrace}),
            ErrorMsg2 = #{
                <<"action">> => <<"CLIENT_ACK_ERROR">>,
                <<"reason">> => <<"decode_error">>,
                <<"server_ts">> => elib_dt:millisecond()
            },
            {reply, ws_reply(protobuf, ErrorMsg2), State, hibernate}
    end.

%% @doc 将 protobuf MsgDirection 枚举转为 ACK 处理使用的 type binary
-spec ack_direction_to_type(atom() | binary()) -> binary().
ack_direction_to_type('C2C') -> <<"C2C">>;
ack_direction_to_type('C2G') -> <<"C2G">>;
ack_direction_to_type('S2C') -> <<"S2C">>;
ack_direction_to_type('C2S') -> <<"C2S">>;
ack_direction_to_type(Bin) when is_binary(Bin) -> Bin;
ack_direction_to_type(_) -> <<>>.

%% @doc 处理 JSON 消息（包括普通消息和 action 消息）
-spec handle_json_message(binary(), map()) ->
    {ok, map(), hibernate} | {reply, {text, binary()}, map(), hibernate}.
handle_json_message(Msg, State) ->
    ok = ?DEBUG_LOG({json_message, byte_size(Msg)}),
    try
        CurrentUid = auth_ds:current_uid(State),

        %% 统一消息处理流程：decode -> convert -> validate -> route
        Data0 = message_ds:decode_websocket_message(Msg),
        Data = message_ds:convert_v1_to_v2(Data0),

        case message_ds:validate_message(Data) of
            {error, Reason} ->
                MsgId0 = maps:get(<<"id">>, Data, <<>>),
                ok = ?WARN_LOG({json_message_invalid, Reason, MsgId0}),
                ErrorMsg = ws_validation_error(MsgId0, <<"invalid_message">>, Reason),
                {reply, {text, jsone:encode(ErrorMsg, [native_utf8])}, State, hibernate};
            {ok, ValidatedData} ->
                MsgId = maps:get(<<"id">>, ValidatedData, <<>>),
                Type = maps:get(<<"type">>, ValidatedData, <<>>),

                %% 注入发送者设备信息到 payload
                Payload0 = maps:get(<<"payload">>, ValidatedData, #{}),

                % 将发送者的设备信息（sender_did 和 sender_dtype）注入到消息的 payload 中。
                Payload = message_ds:inject_sender_device(Payload0, State),
                Data2 = maps:put(<<"payload">>, Payload, ValidatedData),

                %% 统一消息路由：根据 action 和 type 分发到对应的 logic 模块
                Result = message_router_logic:route(MsgId, CurrentUid, Data2, Type, Msg),

                case Result of
                    ok ->
                        {ok, State, hibernate};
                    {reply, Msg2} ->
                        {reply, {text, jsone:encode(Msg2, [native_utf8])}, State, hibernate}
                end
        end
    catch
        Class:CatchReason:Stacktrace ->
            ok = ?ERROR_LOG({json_message_error, Class, CatchReason, Stacktrace}),
            ReasonBin = elib_cnv:safe_to_binary(CatchReason),
            ErrorMsg2 = ws_validation_error(<<>>, <<"invalid_json">>, ReasonBin),
            {reply, {text, jsone:encode(ErrorMsg2, [native_utf8])}, State, hibernate}
    end.

%% @doc 处理从其他进程发送到 WebSocket 进程的消息
-spec websocket_info(term(), map()) ->
    {ok, map()}
    | {ok, map(), hibernate}
    | {reply, {text, binary()} | {binary, binary()} | {close, integer(), binary()}, map(),
        hibernate}
    | {stop, map()}.
websocket_info({reply, Msg}, State) when is_map(Msg) ->
    Protocol = maps:get(protocol, State, json),
    Framing = maps:get(framing, State, none),
    {reply, ws_reply(Protocol, Framing, Msg), State, hibernate};
websocket_info({reply, Msg}, State) when is_list(Msg) ->
    %% List (e.g., proplist/JSON array) — always JSON text frame
    {reply, {text, jsone:encode(Msg, [native_utf8])}, State, hibernate};
websocket_info({reply, Msg}, State) ->
    %% Pre-encoded binary from delivery pipeline — adapt to client protocol
    {reply, encode_delivery_frame(Msg, State), State, hibernate};
%% 处理消息重试超时
websocket_info({timeout, _Ref, {[], _, Msg}}, State) ->
    ok = ?INFO_LOG({retry_timeout, stop_online_retry, Msg}),
    {reply, encode_delivery_frame(Msg, State), State, hibernate};
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
            {reply, encode_delivery_frame(Msg, State), State, hibernate};
        undefined ->
            ok = ?DEBUG_LOG({timeout_no_ack, MsgId, Uid}),
            message_ds:send_next(Uid, MsgId, Msg, MsLi, [DID], false),
            {reply, encode_delivery_frame(Msg, State), State, hibernate}
    end;
%% 处理其他超时消息
websocket_info({timeout, _Ref, Msg}, State) ->
    ok = ?DEBUG_LOG({timeout, Msg}),
    {reply, encode_delivery_frame(Msg, State), State, hibernate};
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
    try
        websocket_logic:handle_ack_cancel(Uid, DID, MsgId)
    catch
        Class:Reason ->
            _ = ?WARN_LOG({ack_cancel_handle_failed, MsgId, Uid, DID, Class, Reason}),
            ok
    end,
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

%% @doc 组装统一的 WebSocket 校验错误消息
%% @param MsgId 客户端消息ID（可为空）
%% @param Action 错误动作类型
%% @param Reason 具体错误原因
%% @return map()
-spec ws_validation_error(binary(), binary(), binary()) -> map().
ws_validation_error(MsgId, Action, Reason) ->
    #{
        <<"id">> => MsgId,
        <<"type">> => <<"S2C">>,
        <<"action">> => Action,
        <<"payload">> => #{<<"reason">> => Reason},
        <<"server_ts">> => elib_dt:millisecond()
    }.

%% @doc 验证 CLIENT_ACK 参数
%% 验证规则:
%% 1. MsgId 长度: 1-128 字节
%% 2. DID 长度: 1-64 字节
%% 3. DID 必须与当前连接的 DID 匹配
%% @return ok | {error, binary()}
-spec validate_ack_params(binary(), binary(), binary(), map()) -> ok | {error, binary()}.
validate_ack_params(Type, MsgId, DID, State) ->
    ValidTypes = [<<"C2C">>, <<"C2G">>, <<"S2C">>, <<"C2S">>],
    case lists:member(Type, ValidTypes) of
        false ->
            {error, <<"invalid_type">>};
        true ->
            CurrentDID = maps:get(did, State, <<>>),
            MsgIdSize = byte_size(MsgId),
            DIDSize = byte_size(DID),
            case
                {
                    MsgIdSize > 0 andalso MsgIdSize =< 128,
                    DIDSize > 0 andalso DIDSize =< 64,
                    DID =:= CurrentDID
                }
            of
                {true, true, true} ->
                    ok;
                {false, _, _} ->
                    {error, <<"invalid_msgid">>};
                {_, false, _} ->
                    {error, <<"invalid_did">>};
                {_, _, false} ->
                    {error, <<"did_mismatch">>}
            end
    end.

%% @doc WebSocket 版本保护：版本过低时发送 S2C 升级提醒
%% 不断开连接，只发消息提醒，用户体验更好
-spec maybe_send_upgrade_notice(binary() | undefined, binary(), binary()) -> ok.
maybe_send_upgrade_notice(undefined, _DType, _DID) ->
    ok;
maybe_send_upgrade_notice(<<>>, _DType, _DID) ->
    ok;
maybe_send_upgrade_notice(AppVsn, DType, DID) ->
    try
        Result = app_version_logic:check(AppVsn, DType, DID),
        UpgradeType = maps:get(<<"upgrade_type">>, Result, <<"none">>),
        case UpgradeType of
            <<"force">> ->
                %% 版本过低需要强制升级，发送 S2C 升级消息
                Msg = #{
                    <<"type">> => <<"S2C">>,
                    <<"action">> => <<"app_upgrade">>,
                    <<"payload">> => #{
                        <<"upgrade_type">> => UpgradeType,
                        <<"vsn">> => maps:get(<<"vsn">>, Result, <<>>),
                        <<"download_url">> => maps:get(<<"download_url">>, Result, <<>>),
                        <<"description">> => maps:get(<<"description">>, Result, <<>>),
                        <<"changelog">> => maps:get(<<"changelog">>, Result, []),
                        <<"file_size">> => maps:get(<<"file_size">>, Result, 0),
                        <<"file_hash">> => maps:get(<<"file_hash">>, Result, <<>>)
                    },
                    <<"server_ts">> => elib_dt:millisecond()
                },
                self() ! {reply, Msg},
                ok;
            _ ->
                ok
        end
    catch
        _:_ -> ok
    end.

%% @doc 处理 protobuf 二进制帧消息（接受预解码的 map）
-spec handle_protobuf_message_decoded(map(), map()) ->
    {ok, map(), hibernate} | {reply, {text, binary()} | {binary, binary()}, map(), hibernate}.
handle_protobuf_message_decoded(Data0, State) ->
    ok = ?DEBUG_LOG({protobuf_message, Data0}),
    try
        CurrentUid = auth_ds:current_uid(State),
        Protocol = maps:get(protocol, State, protobuf),

        %% 解码 payload 子消息（bytes → map）
        MsgType = maps:get(<<"msg_type">>, Data0, <<>>),
        PayloadBin = maps:get(<<"payload">>, Data0, <<>>),
        PayloadMap = imboy_codec:decode_payload(protobuf, MsgType, PayloadBin),

        %% 将 integer ID 转为 binary 以兼容现有消息管道
        Data1 = normalize_protobuf_ids(Data0#{<<"payload">> => PayloadMap}),
        Data = message_ds:convert_v1_to_v2(Data1),

        case message_ds:validate_message(Data) of
            {error, Reason} ->
                MsgId0 = maps:get(<<"id">>, Data, <<>>),
                ok = ?WARN_LOG({protobuf_message_invalid, Reason, MsgId0}),
                ErrorMsg = ws_validation_error(MsgId0, <<"invalid_message">>, Reason),
                {reply, ws_reply(Protocol, ErrorMsg), State, hibernate};
            {ok, ValidatedData} ->
                MsgId = maps:get(<<"id">>, ValidatedData, <<>>),
                Type = maps:get(<<"type">>, ValidatedData, <<>>),

                Payload0 = maps:get(<<"payload">>, ValidatedData, #{}),
                Payload = message_ds:inject_sender_device(Payload0, State),
                Data2 = maps:put(<<"payload">>, Payload, ValidatedData),

                %% 路由（当前投递管道使用 JSON，迭代4 将适配 protobuf）
                RawJson = jsone:encode(Data2, [native_utf8]),
                Result = message_router_logic:route(MsgId, CurrentUid, Data2, Type, RawJson),

                case Result of
                    ok ->
                        {ok, State, hibernate};
                    {reply, Msg2} ->
                        {reply, ws_reply(Protocol, Msg2), State, hibernate}
                end
        end
    catch
        Class:CatchReason:Stacktrace ->
            ok = ?ERROR_LOG({protobuf_message_error, Class, CatchReason, Stacktrace}),
            Protocol2 = maps:get(protocol, State, protobuf),
            ErrorMsg2 = ws_validation_error(<<>>, <<"invalid_protobuf">>, <<"decode_error">>),
            {reply, ws_reply(Protocol2, ErrorMsg2), State, hibernate}
    end.

%% @doc 根据客户端子协议列表确定编码协议与 framing
%% 复用 websocket_ds:select_subprotocol 的优先级逻辑，确保一致性
-spec negotiate_protocol([binary()] | undefined) ->
    {imboy_codec:protocol(), imboy_codec:framing()}.
negotiate_protocol(SubPt) when is_list(SubPt), length(SubPt) > 0 ->
    Selected = websocket_ds:select_subprotocol(SubPt),
    {imboy_codec:protocol_atom(Selected), imboy_codec:framing_atom(Selected)};
negotiate_protocol(_) ->
    {json, none}.

%% @doc 根据协议编码并包装为 WebSocket 帧（非 v2 framing）
-spec ws_reply(imboy_codec:protocol(), map()) -> {text, binary()} | {binary, binary()}.
ws_reply(Protocol, Msg) when is_map(Msg) ->
    ws_reply(Protocol, none, Msg).

%% @doc 根据协议与 framing 编码并包装为 WebSocket 帧
%% v2 framing：protobuf 序列化后用 imboy_frame 包裹，类型默认 MSG_S2C
-spec ws_reply(imboy_codec:protocol(), imboy_codec:framing(), map()) ->
    {text, binary()} | {binary, binary()}.
ws_reply(protobuf, v2, Msg) when is_map(Msg) ->
    Encoded = imboy_codec:encode(protobuf, Msg),
    FrameType = msg_to_v2_frame_type(Msg),
    Frame = imboy_codec:wrap_v2_frame(FrameType, 0, Encoded),
    {binary, Frame};
ws_reply(Protocol, _Framing, Msg) when is_map(Msg) ->
    Encoded = imboy_codec:encode(Protocol, Msg),
    imboy_codec:encode_ws_frame(Protocol, Encoded).

%% @doc 根据消息 type 字段映射到 v2 帧类型
-spec msg_to_v2_frame_type(map()) -> 0..255.
msg_to_v2_frame_type(Msg) ->
    case maps:get(<<"type">>, Msg, <<>>) of
        <<"C2C">> -> ?FRAME_TYPE_MSG_C2C;
        <<"C2G">> -> ?FRAME_TYPE_MSG_C2G;
        <<"C2S">> -> ?FRAME_TYPE_MSG_C2S;
        <<"S2C">> -> ?FRAME_TYPE_MSG_S2C;
        _ -> ?FRAME_TYPE_MSG_S2C
    end.

%% @doc 将投递管道的预编码消息转换为客户端协议帧
%% 投递管道（send_next/timer）当前使用 JSON 预编码；
%% protobuf 客户端需要 decode JSON -> reencode protobuf；
%% v2 framing 进一步把 protobuf 字节包裹为 imboy_frame。
-spec encode_delivery_frame(binary() | term(), map()) -> {text, binary()} | {binary, binary()}.
encode_delivery_frame(Msg, _State) when not is_binary(Msg) ->
    ok = ?WARN_LOG({encode_delivery_frame_not_binary, Msg}),
    {text, iolist_to_binary(io_lib:format("~p", [Msg]))};
encode_delivery_frame(Msg, State) when is_binary(Msg) ->
    Protocol = maps:get(protocol, State, json),
    Framing = maps:get(framing, State, none),
    case {Protocol, Framing} of
        {_, v2} ->
            encode_delivery_frame_v2(Msg);
        {json, _} ->
            {text, Msg};
        {protobuf, _} ->
            encode_delivery_frame_protobuf(Msg)
    end.

-spec encode_delivery_frame_protobuf(binary()) -> {text, binary()} | {binary, binary()}.
encode_delivery_frame_protobuf(Msg) ->
    try
        Map = jsone:decode(Msg, [{object_format, map}]),
        case imboy_codec:encode(protobuf, Map) of
            <<>> ->
                ok = ?WARN_LOG({encode_delivery_frame_pb_empty, maps:get(<<"id">>, Map, unknown)}),
                {text, Msg};
            Encoded ->
                {binary, Encoded}
        end
    catch
        Class:Reason ->
            ok = ?WARN_LOG({encode_delivery_frame_fallback, Class, Reason, byte_size(Msg)}),
            {text, Msg}
    end.

-spec encode_delivery_frame_v2(binary()) -> {binary, binary()}.
encode_delivery_frame_v2(Msg) ->
    %% v2 frame 投递 payload 始终用 JSON binary，不再走 protobuf。
    %% 原因：protobuf 的 `payload` 是 bytes 类型，protobuf-dart 解码后把 bytes
    %% 当作 base64 字符串暴露给应用层（observed: 客户端 [E2EE_DEBUG] 看到
    %% `bE8vQ0NXUmt...` 即 base64('lO/CCWRk...')），导致客户端 e2ee 解密时
    %% `ciphertext.split('.')` 只得到 1 段（base64 无 dot），
    %% 抛出 "Invalid ciphertext format"。
    %% Dart 客户端 imboy_frame 解析同时支持 JSON / protobuf payload，
    %% 这里保留 v2 frame wrap，把 payload 永远填为 JSON 原文。
    try
        Map = jsone:decode(Msg, [{object_format, map}]),
        FrameType = msg_to_v2_frame_type(Map),
        {binary, imboy_codec:wrap_v2_frame(FrameType, 0, Msg)}
    catch
        Class:Reason ->
            ok = ?WARN_LOG({encode_delivery_frame_v2_fallback, Class, Reason, byte_size(Msg)}),
            {binary, imboy_codec:wrap_v2_frame(?FRAME_TYPE_MSG_S2C, 0, Msg)}
    end.

%% @doc 将 protobuf 解码后的 from/to integer ID 转为 binary
%% 仅处理 from 和 to 字段，其他 ID 字段由各 Logic 模块按需处理
-spec normalize_protobuf_ids(map()) -> map().
normalize_protobuf_ids(Data) ->
    From = maps:get(<<"from">>, Data, 0),
    To = maps:get(<<"to">>, Data, 0),
    Data#{
        <<"from">> => to_id_binary(From),
        <<"to">> => to_id_binary(To)
    }.

-spec to_id_binary(integer() | binary()) -> binary().
to_id_binary(V) when is_integer(V), V > 0 -> integer_to_binary(V);
to_id_binary(V) when is_binary(V), byte_size(V) > 0 -> V;
to_id_binary(_) -> <<>>.

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
