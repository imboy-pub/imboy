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


%%websocket 握手
init(Req0, State0) ->
    % Env = os:getenv("IMBOYENV"),
    % DID device id
    AppVsn = cowboy_req:header(<<"vsn">>, Req0, undefined),
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Auth = cowboy_req:header(<<"authorization">>, Req0, undefined),
    % [<<"sip">>,<<"text">>] = Subprotocols
    SubPt = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0),

    % ?DEBUG_LOG([Env, DID, DType, Auth, SubPt]),
    Opt0 = #{
             num_acceptors => infinity,
             max_connections => infinity,
             enable_connect_protocol => true, % since Cowboy 2.11 set to true in order to use Websocket over HTTP/2 for the time being.
             max_frame_size => 1048576,  % 1MB
             % Cowboy关闭连接空闲128秒 默认值为 60000
             % ./apps/imds/src/websocket_ds.erl 里面的 idle_timeout 方法会覆盖该值
             idle_timeout => 128000
            },
    State1 = State0#{dtype => DType, did => DID, vsn => AppVsn},
    case throttle:check(throttle_ws, DID) of
        {limit_exceeded, _, _} ->
            ok = imboy_log:warning("DeviceID ~p exceeded api limit", [DID]),
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
                    websocket_ds:auth(Auth, Req1, State1, Opt0)
            end
    end.


%%连接初始 onopen
websocket_init(State) ->
    case maps:find(error, State) of
        {ok, Code} ->
            Msg = #{
                <<"type">> => <<"error">>,
                <<"code">> => Code,
                <<"server_ts">> => imboy_dt:now()
            },
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        error ->
            CurrentUid = maps:get(current_uid, State),
            % 用户上线
            DID = maps:get(did, State, <<"">>),
            DType = maps:get(dtype, State, <<"">>),
            user_logic:online(CurrentUid, DType, self(), DID),
            {ok, State, hibernate}
    end.


%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    ok = ?DEBUG_LOG([ping, cowboy_clock:rfc1123(), State]),
    case maps:find(error, State) of
        {ok, _Code} ->
            {stop, State};
        error ->
            {reply, pong, State, hibernate}
    end;
websocket_handle({text, <<"ping">>}, State) ->
    % ?DEBUG_LOG([<<"ping">>, cowboy_clock:rfc1123(), State]),
    case maps:find(error, State) of
        {ok, _Code} ->
            {stop, State};
        error ->
            {reply, {text, <<"pong2">>}, State, hibernate}
    end;

% 客户端确认消息
% CLIENT_ACK,type,msgid,did
websocket_handle({text, <<"CLIENT_ACK,", Tail/binary>>}, State) ->
    ok = ?DEBUG_LOG(["CLIENT_ACK", Tail, State]),
    CurrentUid = maps:get(current_uid, State),
    try binary:split(Tail, <<",">>, [global]) of
        [Type, MsgId, DID] ->
            %% 【改进】打印ACK接收日志
            io:format("📥 [CLIENT_ACK] Received: Type=~p, MsgId=~s, DID=~s~n",
                      [Type, MsgId, DID]),

            websocket_logic:cancel_timer(CurrentUid, DID, MsgId),
            %% 【改进】返回ACK确认消息，防止客户端重试
            AckConfirmMsg = #{
                <<"id">> => MsgId,
                <<"type">> => <<"CLIENT_ACK_CONFIRM">>,
                <<"server_ts">> => imboy_dt:millisecond()
            },
            case Type of
                <<"C2C">> ->
                    io:format("📥 [CLIENT_ACK] Processing C2C ACK: MsgId=~s~n", [MsgId]),
                    msg_c2c_logic:c2c_client_ack(MsgId, CurrentUid, DID);
                <<"C2G">> ->
                    io:format("📥 [CLIENT_ACK] Processing C2G ACK: MsgId=~s~n", [MsgId]),
                    msg_c2g_logic:c2g_client_ack(MsgId, CurrentUid, DID);
                <<"S2C">> ->
                    io:format("📥 [CLIENT_ACK] Processing S2C ACK: MsgId=~s~n", [MsgId]),
                    msg_s2c_logic:s2c_client_ack(MsgId, CurrentUid, DID);
                <<"C2S">> ->
                    io:format("📥 [CLIENT_ACK] Processing C2S ACK: MsgId=~s~n", [MsgId]);
                _ ->
                    io:format("⚠️ [CLIENT_ACK] Unknown Type: ~p, MsgId=~s~n", [Type, MsgId])
            end,
            {reply, {text, jsone:encode(AckConfirmMsg, [native_utf8])}, State, hibernate}
    catch
        Class:Reason:_Stacktrace ->
            %% 【修复】返回错误响应并记录详细日志
            ErrorMsg = #{
                <<"type">> => <<"CLIENT_ACK_ERROR">>,
                <<"reason">> => <<"parse_error">>,
                <<"server_ts">> => imboy_dt:millisecond()
            },
            ok = ?ERROR_LOG([client_ack_parse_error, Class, Reason]),
            io:format("❌ [CLIENT_ACK] Parse error: Class=~p, Reason=~p~n", [Class, Reason]),
            {reply, {text, jsone:encode(ErrorMsg, [native_utf8])}, State, hibernate}
    end;

websocket_handle({text, Msg}, State) ->
    ok = ?DEBUG_LOG([State, Msg]),
    try
        CurrentUid = maps:get(current_uid, State),
        Data = jsone:decode(Msg, [{object_format, map}]),
        MsgId = maps:get(<<"id">>, Data),
        Type = maps:get(<<"type">>, Data),
        Payload = maps:get(<<"payload">>, Data),
        Action = case Payload of
                    undefined -> null;
                    _ -> maps:get(<<"action">>, Payload, null)
                 end,
        % ?DEBUG_LOG([MsgId, Type, Action, Data]),
        % 优先检查action字段，如果存在则按action处理
        case Action of
            <<"message_revoke">> ->  % 消息撤销请求
                handle_message_action(revoke, MsgId, CurrentUid, Data, Type);
            <<"message_revoke_ack">> ->  % 消息撤销确认
                handle_message_action(revoke_ack, MsgId, CurrentUid, Data, Type);
            <<"message_edit">> ->  % 消息编辑请求
                handle_message_action(edit, MsgId, CurrentUid, Data, Type);
            <<"message_edit_ack">> ->  % 消息编辑确认
                handle_message_action(edit_ack, MsgId, CurrentUid, Data, Type);
            _ ->
                % 无action字段，按原有逻辑处理
                handle_normal_message(MsgId, CurrentUid, Data, Type, Msg)
        end
    of
        ok ->
            {ok, State, hibernate};
        {reply, Msg2} ->
            ok = ?DEBUG_LOG([reply, 2, Msg2, State]),
            {reply, {text, jsone:encode(Msg2, [native_utf8])}, State, hibernate}
    catch
        Class:Reason:Stacktrace ->
            ok = ?DEBUG_LOG(["websocket_handle try catch: Class:",
                  Class,
                  "Reason:",
                  Reason,
                  "Stacktrace:",
                  Stacktrace,
                  erlang:trace(all, true, [call])]),
            {ok, State, hibernate}
    end;

websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.


%% 处理基于action的消息
-spec handle_message_action(atom(), binary(), integer(), map(), binary()) -> ok | {reply, map()}.
handle_message_action(Action, MsgId, CurrentUid, Data, Type) ->
    case cowboy_bstr:to_lower(Type) of
        <<"c2c">> ->  % 单聊消息
            case Action of
                revoke -> msg_c2c_logic:c2c_revoke(MsgId, CurrentUid, Data);
                revoke_ack -> msg_c2c_logic:c2c_revoke_ack(MsgId, CurrentUid, Data);
                edit -> msg_c2c_logic:c2c_edit(MsgId, CurrentUid, Data);
                edit_ack -> msg_c2c_logic:c2c_edit_ack(MsgId, CurrentUid, Data)
            end;
        <<"c2g">> ->  % 群聊消息
            case Action of
                revoke -> msg_c2g_logic:c2g_revoke(MsgId, CurrentUid, Data);
                revoke_ack -> msg_c2g_logic:c2g_revoke_ack(MsgId, CurrentUid, Data);
                edit -> msg_c2g_logic:c2g_edit(MsgId, CurrentUid, Data);
                edit_ack -> msg_c2g_logic:c2g_edit_ack(MsgId, CurrentUid, Data)
            end;
        _ ->
            % 不支持的消息类型，返回错误消息
            {reply, message_ds:assemble_s2c(MsgId, <<"invalid_message_type">>, <<>>)}
    end.

%% 处理普通消息（无action字段）
-spec handle_normal_message(binary(), integer(), map(), binary(), binary()) -> ok | {reply, map() | list() | binary()}.
handle_normal_message(MsgId, CurrentUid, Data, Type, Msg) ->
    % 逻辑层负责IM系统各项功能的核心逻辑实现
    % Type 包括单聊（c2c）、推送(s2c)、群聊(c2g)
    case cowboy_bstr:to_lower(Type) of
        <<"c2s">> ->  % 机器人聊天消息
            msg_c2s_logic:c2s(MsgId, CurrentUid, Data);
        <<"s2c">> ->  %
            Payload = maps:get(<<"payload">>, Data),
            MsgType = maps:get(<<"msg_type">>, Payload),
            msg_s2c_logic:s2c(MsgType, MsgId, CurrentUid, Data);
        <<"c2c">> ->  % 单聊消息
            msg_c2c_logic:c2c(MsgId, CurrentUid, Data);
        <<"c2g">> ->  % 群聊消息
            msg_c2g_logic:c2g(MsgId, CurrentUid, Data);
        <<"webrtc_", _Event/binary>> -> % webrt信令处理
            To = maps:get(<<"to">>, Data),
            ToUid = imboy_hashids:decode(To),
            webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg);
        _ ->
            ok
    end.

%% 处理从其他进程发送到 WebSocket 进程的消息。
websocket_info({reply, Msg}, State) when is_map(Msg); is_list(Msg) ->
    % ?DEBUG_LOG([reply, State, Msg]),
    {reply, {text, jsone:encode(Msg, [native_utf8])}, State, hibernate};
websocket_info({reply, Msg}, State) ->
    % ?DEBUG_LOG([reply, State, Msg]),
    {reply, {text, Msg}, State, hibernate};

websocket_info({timeout, _Ref, {[], _, Msg}}, State) ->
    % 【新增】记录停止在线投递的日志
    ok = ?INFO_LOG([retry_timeout, stop_online_retry, Msg, cowboy_clock:rfc1123()]),
    io:format("⏹️ [STOP_ONLINE_RETRY] MsLi empty, stop online retry: Msg=~p~n", [Msg]),
    {reply, {text, Msg}, State, hibernate};
websocket_info({timeout, _Ref, {MsLi, {Uid, DID, MsgId}, Msg}}, State) ->
    ok = ?DEBUG_LOG([timeout, _Ref, {Uid, DID, MsgId}, MsLi, State, Msg, cowboy_clock:rfc1123()]),
    %% 【改进】检查是否已收到 ACK，防止重复投递
    AckReceivedKey = {ack_received, Uid, DID, MsgId},
    io:format("⏰ [TIMEOUT_CHECK] MsgId=~s, Uid=~p, DID=~s, checking ACK flag...~n",
              [MsgId, Uid, DID]),
    case imboy_cache:get(AckReceivedKey) of
        {ok, true} ->
            io:format("⏹️ [TIMEOUT] ACK已接收，停止重试: MsgId=~s, Uid=~p~n", [MsgId, Uid]),
            {ok, State, hibernate};
        {ok, _} ->
            io:format("⚠️ [TIMEOUT] ACK标志值异常: MsgId=~s~n", [MsgId]),
            %% false，其他设备也能收到
            message_ds:send_next(Uid, MsgId, Msg, MsLi, [DID], false),
            {reply, {text, Msg}, State, hibernate};
        undefined ->
            io:format("⚠️ [TIMEOUT] 未收到ACK，继续重试: MsgId=~s, Uid=~p~n", [MsgId, Uid]),
            %% false，其他设备也能收到
            message_ds:send_next(Uid, MsgId, Msg, MsLi, [DID], false),
            {reply, {text, Msg}, State, hibernate}
    end;

websocket_info({timeout, _Ref, Msg}, State) ->
    ok = ?DEBUG_LOG([timeout, cowboy_clock:rfc1123(), _Ref, Msg, State]),
    {reply, {text, Msg}, State, hibernate};
websocket_info({close, CloseCode, Reason}, State) ->
    ok = ?DEBUG_LOG([close, CloseCode, Reason, State]),
    {reply, {close, CloseCode, Reason}, State};
websocket_info(stop, State) ->
    ok = ?DEBUG_LOG([stop, State]),
    {stop, State};

%% 【新增】处理跨节点的 ACK 取消消息
websocket_info({ack_cancel, Uid, DID, MsgId, Timestamp}, State) ->
    io:format("📥 [ACK_CANCEL_FROM_REMOTE] MsgId=~s, Uid=~p, DID=~s, Timestamp=~p~n",
              [MsgId, Uid, DID, Timestamp]),
    websocket_logic:handle_ack_cancel(Uid, DID, MsgId),
    {ok, State, hibernate};

websocket_info(_Info, State) ->
    {ok, State}.


%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
terminate(Reason, _Req, State) ->
    ok = ?DEBUG_LOG([terminate, cowboy_clock:rfc1123(), State, Reason]),
    case maps:find(current_uid, State) of
        {ok, Uid} when is_integer(Uid) ->
            DID = maps:get(did, State, <<>>),
            user_logic:offline(Uid, self(), DID),
            ok;
        error ->
            ok
    end.
