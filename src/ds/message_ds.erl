-module(message_ds).

%%%
% message_ds 是 message domain service 缩写
%%%

-include("log.hrl").
-include("chat.hrl").

%% 注意：优先使用 Erlang 原生类型，不新增自定义类型

-export([assemble_s2c/3]).
-export([assemble_msg/5]).
-export([send_next/4, send_next/6]).
-export([check_and_notify_offline_msgs/1]).
-export([get_offline_msg_threshold/0]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 给指定用户所有设备发送消息并支持多次重发
%% 根据时间间隔列表对消息进行多次重发，确保消息可靠投递。
%% 如果消息一直没有被客户端确认，将按照MsLi定义的频率投递。
%% @param ToUid 目标用户ID
%% @param MsgId 消息ID
%% @param Msg 消息内容（JSON字符串）
%% @param MsLi 延迟时间列表（毫秒）
-spec send_next(pos_integer() | binary(), binary(), binary(), [non_neg_integer()]) -> ok.
send_next(ToUid, MsgId, Msg, MsLi) ->
    % ?DEBUG_LOG(["message_ds:send_next/4", ToUid, MsgId, length(MsLi)]),
    send_next(ToUid, MsgId, Msg, MsLi, [], false).

% 如果消息一直没有被客户端确认，
% 那么它将按照 MillisecondList 定义的频率投递 length(MillisecondList) 次，
% 除非投递期间收到客户端确认消息（ CLIENT_ACK,type,msgid,did ）才终止投递；
% 也就是说，消息会按特地平率至少投递一次，至多投递 length(MillisecondList) 次。
%% 支持按设备DID列表过滤，并指定是否为白名单（IncludeDIDLi）
%% send_next/6: 支持设备过滤（DIDLi, IncludeDIDLi 控制是白名单还是黑名单）
%% @param ToUid 目标用户ID
%% @param MsgId 消息ID
%% @param Msg 消息内容
%% @param MsLi 延迟时间列表
%% @param DIDLi 设备ID过滤列表
%% @param IncludeDIDLi true表示白名单模式，false表示黑名单模式
-spec send_next(pos_integer() | binary(),
                binary(),
                binary(),
                [non_neg_integer()],
                [binary()],
                boolean()) ->
                   ok.
send_next(_ToUid, _MsgId, _Msg, [], _, _) ->
    ok;
send_next(ToUid, MsgId, Msg, MsLi, DIDLi, IncludeDIDLi) ->
    % ?DEBUG_LOG(["message_ds:send_next/6", ToUid, MsgId, length(MsLi), length(DIDLi), IncludeDIDLi]),
    % 只允许整数或定时重发间隔组成的列表
    case lists:all(fun(T) -> is_integer(T) andalso T >= 0 end, MsLi) of
        false ->
            % ?DEBUG_LOG(["message_ds:send_next/6 invalid MsLi", MsLi]),
            ok; % 非法间隔直接忽略
        true ->
            send_next_loop(ToUid, MsgId, Msg, MsLi, DIDLi, IncludeDIDLi)
    end.

%% 实际消息分发和定时重发控制
send_next_loop(_ToUid, _MsgId, _Msg, [], _DIDLi, _IncludeDIDLi) ->
    % ?DEBUG_LOG(["message_ds:send_next_loop/6 no more retries"]),
    ok;
send_next_loop(ToUid, MsgId, Msg, [Delay | Tail], DIDLi, IncludeDIDLi) ->
    Members = imboy_syn:list_by_uid(ToUid),

    %% 【改进】打印投递前日志
    io:format("⏱️ [SEND_NEXT_LOOP] MsgId: ~s, Delay: ~pms, Members: ~p~n",
              [MsgId, Delay, length(Members)]),

    Filtered =
        case DIDLi of
            [] ->
                Members;
            _ when IncludeDIDLi == true ->
                [{Pid, {_Dtype, DID}} || {Pid, {_Dtype, DID}} <- Members, lists:member(DID, DIDLi)];
            _ -> % IncludeDIDLi == false
                [{Pid, {_Dtype, DID}}
                 || {Pid, {_Dtype, DID}} <- Members, not lists:member(DID, DIDLi)]
        end,

    io:format("⏱️ [SEND_NEXT_LOOP] Filtered: ~p/~p members~n",
              [length(Filtered), length(Members)]),

    case Filtered of
        [] ->
            io:format("⚠️ [SEND_NEXT_LOOP] No filtered members for MsgId: ~s~n", [MsgId]),
            ok;
        _ when Delay =:= 0 ->
            %% 【关键修复】过滤已 ACK 的设备，防止重复投递
            Filtered2 = [{Pid, Info} || {Pid, Info} <- Filtered,
                                   begin
                                       {_Dtype, DID} = Info,
                                       case imboy_cache:get({ack_received, ToUid, DID, MsgId}) of
                                           {ok, true} -> false;
                                           _ -> true
                                       end
                                   end],
            case Filtered2 of
                [] ->
                    io:format("⏹️ [IMMEDIATE_PUBLISH] All devices ACKed, skipping: MsgId: ~s~n", [MsgId]),
                    ok;
                _ ->
                    io:format("⚡ [IMMEDIATE_PUBLISH] MsgId: ~s, Devices: ~p~n", [MsgId, length(Filtered2)]),
                    _ = [imboy_syn:publish(ToUid, Msg, 0) || _ <- [1]],
                    send_next_loop(ToUid, MsgId, Msg, Tail, DIDLi, IncludeDIDLi)
            end;
        _ when is_integer(Delay), Delay > 0 ->
            io:format("⏰ [DELAYED_PUBLISH] MsgId: ~s, Delay: ~pms, Members: ~p~n",
                      [MsgId, Delay, length(Filtered)]),
            [begin
                 TimerKey = {ToUid, DID, MsgId},
                 %% 【修复】取消旧定时器，防止 Ref 覆盖
                 case imboy_cache:get(TimerKey) of
                     {ok, OldRef} when is_reference(OldRef) ->
                         _ = erlang:cancel_timer(OldRef),
                         ok;
                     _ -> ok
                 end,
                 Ref = erlang:start_timer(Delay, Pid, {Tail, TimerKey, Msg}),
                 imboy_cache:set(TimerKey, Ref, Delay + 1000), % 超时时间略大于 timer
                 io:format("⏰ [TIMER_SET] MsgId: ~s, Delay: ~pms, DID: ~s, Ref: ~p~n",
                           [MsgId, Delay, DID, Ref])
             end
             || {Pid, {_Dtype, DID}} <- Filtered],
            ok
    end.

%% @doc 组装系统消息
%% 为系统到客户端的消息创建标准格式。
%% 消息类型范围为[500-1000)，用于系统级通知。
%% @param MsgId 消息ID
%% @param MsgType 消息类型
%% @param To 目标用户ID或ID列表
%% @returns 组装后的消息格式（map格式）
-spec assemble_s2c(binary(), binary(), binary()) -> map().
assemble_s2c(MsgId, MsgType, To) ->
    Payload = #{<<"msg_type">> => MsgType},
    assemble_msg(<<"S2C">>, <<"">>, To, Payload, MsgId).

%%% 系统消息 end

%% @doc 组装标准IM消息
%% 创建标准格式的即时通讯消息，支持多种参数格式并自动编码用户ID。
%% 消息包含ID、类型、发送方、接收方、载荷和时间戳等标准字段。
%% @param Type 消息类型（如C2C、C2G、S2C等）
%% @param From 发送方用户ID，支持整数或编码后的字符串
%% @param To 接收方用户ID，支持整数、字符串或列表
%% @param Payload 消息载荷数据
%% @param MsgId 消息ID
%% @returns 标准格式的消息数据（map格式）
-spec assemble_msg(binary(),
                   pos_integer() | binary(),
                   pos_integer() | binary() | [pos_integer() | binary()],
                   map(),
                   binary()) ->
                      map().
assemble_msg(Type, From, To, Payload, MsgId) when is_integer(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:encode(From), To, Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_list(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:encode(From), To, Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_list(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:encode(To), Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_integer(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:encode(To), Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) ->
    #{<<"id">> => MsgId,
      <<"type">> => Type,
      <<"from">> => From,
      <<"to">> => To,
      <<"payload">> => Payload,
      <<"server_ts">> => imboy_dt:millisecond()}.

%% @doc 检查并通知离线消息
%% 检查用户的所有类型离线消息（C2C、C2G、S2C），根据消息数量决定
%% 是直接推送还是发送pull通知。超过阈值时发送pull通知。
%% @param Uid 用户ID
%% @returns ok
% message_ds:check_and_notify_offline_msgs(1).
-spec check_and_notify_offline_msgs(integer() | binary()) -> ok.
check_and_notify_offline_msgs(Uid) ->
    % ?DEBUG_LOG([check_and_notify_offline_msgs, 1, Uid]),
    % 检查各类型离线消息数量
    C2CMsgs = msg_c2c_ds:read_msg(Uid, ?SAVE_MSG_LIMIT),
    C2GMsgs = msg_c2g_ds:read_msg(Uid, ?SAVE_MSG_LIMIT, undefined),
    S2CMsgs = msg_s2c_ds:read_msg(Uid, ?SAVE_MSG_LIMIT),

    % 计算各类型消息数量
    C2CCount = length(C2CMsgs),
    C2GCount = length(C2GMsgs),
    S2CCount = length(S2CMsgs),
    % ?DEBUG_LOG([<<"Offline msgs for Uid ">>, Uid,
    %             <<": C2C=">>, C2CCount, <<", C2G=">>, C2GCount, <<", S2C=">>, S2CCount]),
    % 处理各类型离线消息，收集是否需要发送pull通知
    {NeedPull1, NeedPull2, NeedPull3} =
        {handle_offline_msgs(Uid, <<"C2C">>, C2CMsgs, C2CCount),
         handle_offline_msgs(Uid, <<"C2G">>, C2GMsgs, C2GCount),
         handle_offline_msgs(Uid, <<"S2C">>, S2CMsgs, S2CCount)},

    % 如果任意类型需要发送pull通知，则只发送一次
    case NeedPull1 orelse NeedPull2 orelse NeedPull3 of
        true ->
            ok = ?DEBUG_LOG(["check_and_notify_offline_msgs", Uid]),
            send_pull_offline_msg(Uid);
        false ->
            ok
    end,

    ok.

%% @doc 处理离线消息的内部函数
%% 根据消息数量决定是直接发送消息还是返回是否需要发送pull通知。
%% 当消息数量超过阈值时返回true触发pull通知，否则直接推送消息。
%% @param Uid 用户ID
%% @param Type 消息类型（C2C、C2G、S2C）
%% @param Msgs 消息列表
%% @param Count 消息数量
%% @returns 是否需要发送pull通知
-spec handle_offline_msgs(pos_integer() | binary(),
                          binary(),
                          [map()],
                          non_neg_integer()) ->
                             boolean().
handle_offline_msgs(_Uid, _Type, [], _Count) ->
    % 没有离线消息
    false;
handle_offline_msgs(Uid, Type, Msgs, Count) when Count > 0 ->
    Threshold = get_offline_msg_threshold(),
    case Count > Threshold of
        true ->
            % 消息数量超过阈值，需要pull通知
            ok = ?DEBUG_LOG([Type, <<"OFFLINE_MSG_THRESHOLD">>, Count, Threshold, Uid]),
            true;
        false ->
            % 消息数量在阈值内，直接发送离线消息
            % ?DEBUG_LOG([Type, <<"OFFLINE_MSG_THRESHOLD">>, Count, Threshold, Uid]),
            sent_offline_msg(Uid, Type, Msgs),
            false
    end.

%% 检查并通知离线消息

%% 发送pull_offline_msg通知
-spec send_pull_offline_msg(integer()) -> ok.
send_pull_offline_msg(Uid) ->
    % 在这里 sleep 8000 毫秒再执行后面逻辑
    timer:sleep(8000),
    MsgId = imboy_func:uid("pull_offline"),
    Payload = #{<<"msg_type">> => <<"pull_offline_msg">>},
    Msg = assemble_msg(<<"S2C">>, <<>>, imboy_hashids:encode(Uid), Payload, MsgId),
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 30000, 30000],
    send_next(Uid, MsgId, MsgJson, MsLi),
    % ?DEBUG_LOG(["send_pull_offline_msg", Uid, MsgId]),
    ok.

%% 发送离线消息
-spec sent_offline_msg(integer(), binary(), list()) -> ok.
sent_offline_msg(_Uid, _Type, []) ->
    ok;
sent_offline_msg(Uid, Type, [Row | Tail]) ->
    ok = ?DEBUG_LOG([<<"Sending offline msg ">>, Type, <<" to Uid ">>, Uid]),
    % 统一从 from_id 和 to_id 获取（确保是 integer）
    MsgId = maps:get(<<"msg_id">>, Row),
    FromId = maps:get(<<"from_id">>, Row),
    ToId = maps:get(<<"to_id">>, Row),
    Payload = maps:get(<<"payload">>, Row),
    CreatedAtRaw = maps:get(<<"created_at">>, Row, undefined),
    ServerTsRaw = maps:get(<<"server_ts">>, Row, undefined),

    Row2 =
        imboy_cnv:convert_at_timestamps(#{<<"created_at">> => CreatedAtRaw,
                                          <<"server_ts">> => ServerTsRaw}),

    % 发送给前端时 encode
    Msg = #{<<"id">> => MsgId,
            <<"type">> => Type,
            <<"from">> => imboy_hashids:encode(FromId),
            <<"to">> => imboy_hashids:encode(ToId),
            <<"payload">> => Payload,
            <<"created_at">> => maps:get(<<"created_at">>, Row2, CreatedAtRaw),
            <<"server_ts">> => maps:get(<<"server_ts">>, Row2, ServerTsRaw)},
    % 添加日志，方便调试
    io:format("📤 [SENT_OFFLINE_MSG] MsgId: ~s, FromId: ~p -> From: ~s, ToId: "
              "~p -> To: ~s~n",
              [MsgId, FromId, maps:get(<<"from">>, Msg), ToId, maps:get(<<"to">>, Msg)]),
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 30000, 30000],
    send_next(Uid, MsgId, MsgJson, MsLi),
    sent_offline_msg(Uid, Type, Tail).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 获取离线消息阈值配置
%% 从应用配置中获取离线消息阈值，默认值为10。
%% 当离线消息数量超过此值时发送pull通知而非直接推送。
%% @returns 离线消息阈值数量
-spec get_offline_msg_threshold() -> non_neg_integer().
get_offline_msg_threshold() ->
    application:get_env(imboy, offline_msg_threshold, 10).
