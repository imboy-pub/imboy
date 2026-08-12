-module(message_ds).

%%%
% message_ds 是 message domain service 缩写
%%%

-include("log.hrl").
-include("chat.hrl").

%% 注意：优先使用 Erlang 原生类型，不新增自定义类型

-export([assemble_s2c/3]).
-export([assemble_msg/5, assemble_msg/8]).
-export([decode_websocket_message/1]).
-export([validate_message/1]).
-export([convert_v1_to_v2/1]).
-export([send_next/4, send_next/6]).
-export([check_and_notify_offline_msgs/1, check_and_notify_offline_msgs/2]).
-export([inject_sender_device/2]).
-export([stamp_sender_device/2, with_sender_device/2]).
-export([offline_envelope/2]).
-export([build_adm_union_sql/1]).

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
-spec send_next(
    pos_integer() | binary(), binary(), binary(), [non_neg_integer()], [binary()], boolean()
) -> ok.
send_next(_ToUid, _MsgId, _Msg, [], _, _) ->
    ok;
send_next(ToUid, MsgId, Msg, MsLi, DIDLi, IncludeDIDLi) ->
    % ?DEBUG_LOG(["message_ds:send_next/6", ToUid, MsgId, length(MsLi), length(DIDLi), IncludeDIDLi]),
    %% 消息发送计数
    elib_metric:increment(msg_sent_total),
    % 只允许整数或定时重发间隔组成的列表
    case lists:all(fun(T) -> is_integer(T) andalso T >= 0 end, MsLi) of
        false ->
            % ?DEBUG_LOG(["message_ds:send_next/6 invalid MsLi", MsLi]),

            % 非法间隔直接忽略
            ok;
        true ->
            send_next_loop(ToUid, MsgId, Msg, MsLi, DIDLi, IncludeDIDLi)
    end.

%% 实际消息分发和定时重发控制
send_next_loop(_ToUid, _MsgId, _Msg, [], _DIDLi, _IncludeDIDLi) ->
    % ?DEBUG_LOG(["message_ds:send_next_loop/6 no more retries"]),
    ok;
send_next_loop(ToUid, MsgId, Msg, [Delay | Tail], DIDLi, IncludeDIDLi) ->
    Members = imboy_syn:list_by_uid(ToUid),

    ok = ?DEBUG_LOG({send_next_loop_start, MsgId, Delay, length(Members)}),

    Filtered =
        case DIDLi of
            [] ->
                Members;
            _ when IncludeDIDLi == true ->
                [{Pid, {_Dtype, DID}} || {Pid, {_Dtype, DID}} <- Members, lists:member(DID, DIDLi)];
            % IncludeDIDLi == false
            _ ->
                [
                    {Pid, {_Dtype, DID}}
                 || {Pid, {_Dtype, DID}} <- Members, not lists:member(DID, DIDLi)
                ]
        end,

    ok = ?DEBUG_LOG({send_next_loop_filtered, MsgId, length(Filtered), length(Members)}),

    case Filtered of
        [] ->
            ok = ?DEBUG_LOG({send_next_loop_no_members, MsgId}),
            ok;
        _ when Delay =:= 0 ->
            %% 【关键修复】过滤已 ACK 的设备，防止重复投递
            Filtered2 = [
                {Pid, Info}
             || {Pid, Info} <- Filtered,
                begin
                    {_Dtype, DID} = Info,
                    case imboy_cache:get({ack_received, ToUid, DID, MsgId}) of
                        {ok, true} ->
                            false;
                        _ ->
                            true
                    end
                end
            ],
            case Filtered2 of
                [] ->
                    ok = ?DEBUG_LOG({immediate_publish_all_acked, MsgId}),
                    ok;
                _ ->
                    ok = ?DEBUG_LOG({immediate_publish, MsgId, length(Filtered2)}),
                    %% 仅向未 ACK 的设备发送，避免同 UID 下已 ACK 设备被重复投递
                    [erlang:start_timer(0, Pid, Msg) || {Pid, {_Dtype, _DID}} <- Filtered2],
                    send_next_loop(ToUid, MsgId, Msg, Tail, DIDLi, IncludeDIDLi)
            end;
        _ when is_integer(Delay), Delay > 0 ->
            ok = ?DEBUG_LOG({delayed_publish, MsgId, Delay, length(Filtered)}),
            %% 【关键修复】延迟投递前也要检查 ACK，防止重复投递
            Filtered3 = [
                {Pid, Info}
             || {Pid, Info} <- Filtered,
                begin
                    {_Dtype, DID} = Info,
                    case imboy_cache:get({ack_received, ToUid, DID, MsgId}) of
                        {ok, true} ->
                            false;
                        _ ->
                            true
                    end
                end
            ],
            [
                begin
                    TimerKey = {ToUid, DID, MsgId},
                    case imboy_cache:get(TimerKey) of
                        {ok, OldRef} when is_reference(OldRef) ->
                            _ = erlang:cancel_timer(OldRef),
                            ok;
                        _ ->
                            ok
                    end,
                    Ref = erlang:start_timer(Delay, Pid, {Tail, TimerKey, Msg}),
                    %% 缓存需覆盖定时器存活期（Delay 毫秒）+ 5s 余量供 ACK 竞态取消。
                    %% imboy_cache:set/3 的 TTL 单位是「秒」，故 (Delay + 5000) 毫秒需 div 1000；
                    %% 此前直接把毫秒数值当秒，TTL 被放大约 1000 倍，过期 Ref 长期滞留缓存。
                    imboy_cache:set(TimerKey, Ref, (Delay + 5000) div 1000),
                    ok = ?DEBUG_LOG({timer_set, MsgId, Delay, DID, Ref})
                end
             || {Pid, {_Dtype, DID}} <- Filtered3
            ],
            ok
    end.

%% @doc 组装系统消息（v2.0 格式）
%% 为系统到客户端的消息创建标准格式。
%% v2.0: S2C 消息使用 action 字段（而非 msg_type）
%% 消息类型范围为[500-1000)，用于系统级通知。
%% @param MsgId 消息ID
%% @param Action 系统操作类型（如 pull_offline_msg、please_refresh_token 等）
%% @param To 目标用户ID或ID列表
%% @returns v2.0 格式的消息（action 在顶层）
-spec assemble_s2c(binary(), binary(), integer() | binary()) -> map().
assemble_s2c(MsgId, Action, To) ->
    %% v2.0: S2C 消息使用 action 字段，直接使用 assemble_msg/8
    Payload = #{},
    assemble_msg(<<"S2C">>, <<>>, To, Payload, MsgId, <<>>, Action, null).

%%% 系统消息 end

%% @doc 组装标准IM消息（v2.0 格式，带 MsgType/Action/E2EE 参数）
%% 创建标准格式的即时通讯消息，支持多种参数格式并自动编码用户ID。
%% v2.0 格式：msg_type、action、e2ee 字段作为独立参数传递到顶层。
%%
%% @param Type 消息类型（如C2C、C2G、S2C等）
%% @param From 发送方用户ID，支持整数或编码后的字符串
%% @param To 接收方用户ID，支持整数、字符串或列表
%% @param Payload 消息载荷数据（不包含 msg_type/action/e2ee）
%% @param MsgId 消息ID
%% @param MsgType 消息内容类型（text/image/video/file/location/custom/e2ee）
%% @param Action S2C消息的指令（仅S2C使用）
%% @param E2EE 端到端加密信息（仅C2C/C2G使用）
%% @returns v2.0 格式的消息数据（map格式）
-spec assemble_msg(
    binary(),
    pos_integer() | binary(),
    pos_integer() | binary() | [pos_integer() | binary()],
    map(),
    binary(),
    binary(),
    binary(),
    map() | null
) -> map().
assemble_msg(Type, From, To, Payload, MsgId, MsgType, Action, E2EE) ->
    %% v2.0: MsgType/Action/E2EE 作为独立参数，直接使用
    %% TSID 大整数超过 JS 安全范围，所有 ID 必须转为 binary 字符串
    %% S0-1: 信封带 ver 字段（出站=当前版本，架构保险）
    #{
        <<"ver">> => ?CUR_MSG_VER,
        <<"id">> => MsgId,
        <<"type">> => Type,
        <<"from">> => From,
        <<"to">> => To,
        <<"msg_type">> => MsgType,
        <<"action">> => Action,
        <<"e2ee">> => E2EE,
        <<"payload">> => Payload,
        <<"server_ts">> => elib_dt:millisecond()
    }.

%% @doc 组装标准IM消息（v2.0 格式，向后兼容版本）
%% 创建标准格式的即时通讯消息，从 Payload 中提取 msg_type/action/e2ee。
%% 注意：新代码应使用 assemble_msg/8，直接传递 MsgType/Action/E2EE 参数。
%%
%% @param Type 消息类型（如C2C、C2G、S2C等）
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param Payload 消息载荷数据（可包含 msg_type/action/e2ee，会被提取到顶层）
%% @param MsgId 消息ID
%% @returns v2.0 格式的消息数据（map格式）
-spec assemble_msg(
    binary(),
    pos_integer() | binary(),
    pos_integer() | binary() | [pos_integer() | binary()],
    map(),
    binary()
) -> map().
assemble_msg(Type, From, To, Payload, MsgId) ->
    %% v2.0: 从 Payload 中提取 msg_type/action/e2ee 到顶层（向后兼容）
    MsgType = maps:get(<<"msg_type">>, Payload, <<>>),
    Action = maps:get(<<"action">>, Payload, <<>>),
    E2EE = maps:get(<<"e2ee">>, Payload, null),

    %% 清理 Payload 中的顶层字段（避免重复）
    Payload2 = maps:without([<<"msg_type">>, <<"action">>, <<"e2ee">>], Payload),

    %% 调用 assemble_msg/8
    assemble_msg(Type, From, To, Payload2, MsgId, MsgType, Action, E2EE).

%% @doc 编码 WebSocket 消息（v2.0 格式）
%% 将数据库消息或内部消息转换为 v2.0 WebSocket 格式。
%% 字段提升到顶层：msg_type、action、e2ee 都在消息顶层。
%%
%% 输入格式支持：
%% 1. 数据库格式：包含 from_id/to_id/msg_type/action/e2ee/payload
%% 2. 内部格式：已包含 from/to/msg_type/action/e2ee/payload
%%
%% @param Msg 消息 map，包含：
%%   - id: 消息ID
%%   - type: 消息类型 (C2C/C2G/C2S/S2C)
%%   - from/from_id: 发送者ID（优先 from，回退 from_id）
%%   - to/to_id: 接收者ID（优先 to，回退 to_id）
%%   - msg_type: 消息内容类型（必需，C2C/C2G/S2C）
%%   - payload: 消息载荷（必需）
%%   - action: S2C消息的指令（仅S2C）
%%   - e2ee: 端到端加密信息（仅C2C/C2G，可选）
%%   - server_ts: 服务器时间戳（可选）
%% @returns 符合 v2.0 规范的 WebSocket 消息格式
-spec encode_websocket_message(map()) -> map().
encode_websocket_message(Msg) ->
    Type = maps:get(<<"type">>, Msg),

    %% 兼容两种格式：from_id（数据库）和 from（内部）
    From =
        case maps:get(<<"from">>, Msg, undefined) of
            undefined ->
                maps:get(<<"from_id">>, Msg);
            FromVal ->
                FromVal
        end,
    To =
        case maps:get(<<"to">>, Msg, undefined) of
            undefined ->
                maps:get(<<"to_id">>, Msg);
            ToVal ->
                ToVal
        end,

    %% v2.0: msg_type、action、e2ee 都在顶层（不需要根据 type 过滤）
    MsgType = maps:get(<<"msg_type">>, Msg, <<>>),
    Action = maps:get(<<"action">>, Msg, <<>>),
    E2EE = maps:get(<<"e2ee">>, Msg, null),

    %% v2.0 格式：所有字段提升到顶层
    %% TSID 大整数超过 JS 安全范围，所有 ID 必须转为 binary 字符串
    %% S0-1: 信封带 ver 字段（出站=当前版本，架构保险）
    #{
        <<"ver">> => ?CUR_MSG_VER,
        <<"id">> => maps:get(<<"id">>, Msg),
        <<"type">> => Type,
        <<"from">> => From,
        <<"to">> => To,
        <<"msg_type">> => MsgType,
        <<"action">> => Action,
        <<"e2ee">> => E2EE,
        <<"payload">> => maps:get(<<"payload">>, Msg, #{}),
        <<"server_ts">> => maps:get(<<"server_ts">>, Msg, elib_dt:millisecond())
    }.

%% @doc 解码 WebSocket 消息（v2.0 格式）
%% 从顶层读取 msg_type/action/e2ee 字段，转换为内部格式供后续处理使用。
%% v2.0 格式：msg_type、action、e2ee 都在顶层。
%%
%% @param Data JSON 二进制数据
%% @returns 解码后的消息 map
-spec decode_websocket_message(binary()) -> map().
decode_websocket_message(Data) ->
    Msg = jsone:decode(Data, [{object_format, map}]),
    Type = maps:get(<<"type">>, Msg, <<>>),

    %% v2.0: msg_type、action、e2ee 都在顶层（不需要根据 type 过滤）
    MsgType = maps:get(<<"msg_type">>, Msg, <<>>),
    Action = maps:get(<<"action">>, Msg, <<>>),
    % map() | null
    E2EE = maps:get(<<"e2ee">>, Msg, null),

    %% S0-1: 信封版本号透传；旧客户端不带 ver 时缺省视为当前版本（向后兼容）
    Ver = maps:get(<<"ver">>, Msg, ?CUR_MSG_VER),

    %% 保持客户端字段名：from/to（binary，TSID 字符串）
    %% Logic 层会使用 ec_cnv:to_integer/1 将其转换为 integer
    %% 消息自毁秒数（可选，0 或 undefined 表示不自毁）
    ExpireSecs =
        case maps:get(<<"expire_secs">>, Msg, undefined) of
            N when is_integer(N), N > 0 -> N;
            _ -> undefined
        end,

    #{
        <<"ver">> => Ver,
        <<"id">> => maps:get(<<"id">>, Msg, <<>>),
        <<"type">> => Type,
        <<"from">> => maps:get(<<"from">>, Msg, <<>>),
        <<"to">> => maps:get(<<"to">>, Msg, <<>>),
        <<"msg_type">> => MsgType,
        <<"action">> => Action,
        <<"e2ee">> => E2EE,
        <<"expire_secs">> => ExpireSecs,
        <<"payload">> => maps:get(<<"payload">>, Msg, #{}),
        <<"created_at">> => maps:get(<<"created_at">>, Msg, elib_dt:millisecond())
    }.

%% @doc 检查并通知离线消息
%% 检查用户的所有类型离线消息（C2C、C2G、S2C），根据消息数量决定
%% 是直接推送还是发送pull通知。超过阈值时发送pull通知。
%% @param Uid 用户ID
%% @returns ok
% message_ds:check_and_notify_offline_msgs(1).
-spec check_and_notify_offline_msgs(integer() | binary()) -> ok.
check_and_notify_offline_msgs(Uid) ->
    check_and_notify_offline_msgs(Uid, <<>>).

%% @doc 检查并通知离线消息（按设备维度，T03/P0-1）
%% DID 有效时：C2C/S2C 只取该设备尚未确认的消息，且只推送给该设备
%% （其他在线设备已走实时投递，各自的送达状态由 msg_delivery 跟踪）。
%% DID 为空：退回按 uid 读取 + 全设备广播的旧语义。
%% C2G 仍按 uid timeline 读取（V7 另行立项）。
-spec check_and_notify_offline_msgs(integer() | binary(), binary()) -> ok.
check_and_notify_offline_msgs(Uid, DID) ->
    % 检查各类型离线消息数量
    C2CMsgs = msg_c2c_ds:read_msg_for_device(Uid, DID, ?SAVE_MSG_LIMIT, undefined),
    C2GMsgs = msg_c2g_ds:read_msg(Uid, ?SAVE_MSG_LIMIT, undefined),
    S2CMsgs = msg_s2c_ds:read_msg_for_device(Uid, DID, ?SAVE_MSG_LIMIT, undefined),

    % 计算各类型消息数量
    C2CCount = length(C2CMsgs),
    C2GCount = length(C2GMsgs),
    S2CCount = length(S2CMsgs),
    % ?DEBUG_LOG([<<"Offline msgs for Uid ">>, Uid,
    %             <<": C2C=">>, C2CCount, <<", C2G=">>, C2GCount, <<", S2C=">>, S2CCount]),
    % 处理各类型离线消息，收集是否需要发送pull通知
    {NeedPull1, NeedPull2, NeedPull3} = {
        handle_offline_msgs(Uid, DID, <<"C2C">>, C2CMsgs, C2CCount),
        handle_offline_msgs(Uid, DID, <<"C2G">>, C2GMsgs, C2GCount),
        handle_offline_msgs(Uid, DID, <<"S2C">>, S2CMsgs, S2CCount)
    },

    % 如果任意类型需要发送pull通知，则只发送一次
    case NeedPull1 orelse NeedPull2 orelse NeedPull3 of
        true ->
            ok = ?DEBUG_LOG(["check_and_notify_offline_msgs", Uid, DID]),
            send_pull_offline_msg(Uid, DID);
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
-spec handle_offline_msgs(pos_integer() | binary(), binary(), binary(), [map()], non_neg_integer()) ->
    boolean().
handle_offline_msgs(_Uid, _DID, _Type, [], _Count) ->
    % 没有离线消息
    false;
handle_offline_msgs(Uid, DID, Type, Msgs, Count) when Count > 0 ->
    Threshold = get_offline_msg_threshold(),
    case Count > Threshold of
        true ->
            % 消息数量超过阈值，需要pull通知
            ok = ?DEBUG_LOG([Type, <<"OFFLINE_MSG_THRESHOLD">>, Count, Threshold, Uid]),
            true;
        false ->
            % 消息数量在阈值内，直接发送离线消息
            % ?DEBUG_LOG([Type, <<"OFFLINE_MSG_THRESHOLD">>, Count, Threshold, Uid]),
            sent_offline_msg(Uid, DID, Type, Msgs),
            false
    end.

%% 检查并通知离线消息

%% @doc DID 有效时定向白名单，只推给当前连接设备；为空则不过滤（全设备广播）
-spec did_whitelist(binary()) -> [binary()].
did_whitelist(<<>>) -> [];
did_whitelist(DID) when is_binary(DID) -> [DID];
did_whitelist(_) -> [].

%% 发送pull_offline_msg通知（v2.0 格式）
-spec send_pull_offline_msg(integer(), binary()) -> ok.
send_pull_offline_msg(Uid, DID) ->
    MsgId = elib_id:gen("pull_offline"),
    %% v2.0: S2C 消息使用 action 字段
    Msg = assemble_s2c(MsgId, <<"pull_offline_msg">>, Uid),
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = elib_retry_config:intervals(<<"pull">>),
    send_next(Uid, MsgId, MsgJson, MsLi, did_whitelist(DID), true),
    % ?DEBUG_LOG(["send_pull_offline_msg", Uid, MsgId]),
    ok.

%% 发送离线消息（v2.0 格式）
-spec sent_offline_msg(integer(), binary(), binary(), list()) -> ok.
sent_offline_msg(_Uid, _DID, _Type, []) ->
    ok;
sent_offline_msg(Uid, DID, Type, [Row | Tail]) ->
    ok = ?DEBUG_LOG([<<"Sending offline msg ">>, Type, <<" to Uid ">>, Uid]),
    %% msg_c2c/msg_c2g 的 `id` 是数据库内部 TSID，`msg_id` 才是客户端
    %% 受保护头绑定的业务消息 ID。离线帧必须沿用后者，否则 PFv3
    %% 接收侧会稳定命中 context_mismatch_id。
    MsgId = maps:get(<<"msg_id">>, Row, maps:get(<<"id">>, Row)),
    Msg = offline_envelope(Type, Row),

    ok = ?DEBUG_LOG({sent_offline_msg, MsgId, maps:get(<<"from">>, Msg, <<>>)}),

    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = elib_retry_config:intervals(<<"pull">>),
    send_next(Uid, MsgId, MsgJson, MsLi, did_whitelist(DID), true),
    sent_offline_msg(Uid, DID, Type, Tail).

%% @doc 把一行离线消息（msg_c2c / msg_c2g / msg_s2c 的读取结果）组装成出站信封。
%%
%% 从 sent_offline_msg/4 抽出，唯一生产调用方即 sent_offline_msg/4
%% （链路：websocket_handler → message_ds:check_and_notify_offline_msgs/2
%% → handle_offline_msgs/5 → sent_offline_msg/4 → 本函数）。
%% 抽成纯函数只为可测：离线信封的字段集是 PFv3 接收侧的硬依赖，
%% 必须能在不起 syn / 不发真实 WS 帧的前提下被断言。
%%
%% @param Type 消息类别（<<"C2C">> / <<"C2G">> / <<"S2C">>）
%% @param Row  数据库行（binary key map）
%% @return 出站信封 map
-spec offline_envelope(binary(), map()) -> map().
offline_envelope(Type, Row) ->
    % 统一从 from_id 和 to_id 获取（确保是 integer）
    %% 优先使用业务 msg_id；兼容没有独立 msg_id 的旧系统消息行时回退 id。
    MsgId = maps:get(<<"msg_id">>, Row, maps:get(<<"id">>, Row)),
    FromId = maps:get(<<"from_id">>, Row),
    ToId = maps:get(<<"to_id">>, Row),
    Payload = maps:get(<<"payload">>, Row),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, null),
    %% action 旧表结构没有独立列；E2EE 编辑旁路将 relay_action 放在
    %% 可见元数据中，供离线帧恢复顶层 action，而不读取密文正文。
    Action = maps:get(<<"action">>, Row, e2ee_relay_action(E2EE)),
    CreatedAtRaw = maps:get(<<"created_at">>, Row, undefined),
    ServerTsRaw = maps:get(<<"server_ts">>, Row, undefined),

    Row2 = elib_cnv:convert_at_timestamps(#{
        <<"created_at">> => CreatedAtRaw, <<"server_ts">> => ServerTsRaw
    }),

    % 发送给前端时使用 v2.0 格式
    %% 使用 encode_websocket_message/1 统一编码
    Msg = encode_websocket_message(#{
        <<"id">> => MsgId,
        <<"type">> => Type,
        <<"from_id">> => FromId,
        <<"to_id">> => ToId,
        <<"msg_type">> => MsgType,
        <<"action">> => Action,
        <<"e2ee">> => E2EE,
        <<"payload">> => Payload,
        <<"created_at">> => maps:get(<<"created_at">>, Row2, CreatedAtRaw),
        <<"server_ts">> => maps:get(<<"server_ts">>, Row2, ServerTsRaw)
    }),
    %% encode_websocket_message/1 是白名单，sender_did 必须在其后单独并入。
    %% 与 with_sender_device/2 同一原则：**没有就不补**——补 <<>> 会让接收侧
    %% 把「服务端没提供」误判成「设备 ID 是空串」，两者失败语义不同。
    %% 旧行（迁移 48 之前落库）与非 C2C 类型的列值为 null，走此分支。
    case maps:get(<<"sender_did">>, Row, null) of
        Did when is_binary(Did), Did =/= <<>> -> Msg#{<<"sender_did">> => Did};
        _ -> Msg
    end.

-spec e2ee_relay_action(term()) -> binary().
e2ee_relay_action(E2EE) when is_map(E2EE) ->
    maps:get(<<"relay_action">>, E2EE, <<>>);
e2ee_relay_action(_) ->
    <<>>.

%% @doc 将发送者设备信息注入到消息 payload 中
%%
%% 此函数用于在服务端验证用户身份后，将可信的设备信息注入到消息 payload 中。
%% 这样可以防止客户端伪造设备信息，确保消息中携带的 sender_did 是真实可靠的。
%%
%% 使用场景：
%%   1. E2EE 签名验证 - payload 中的 sender_did 用于验证发送者身份
%%   2. 多设备管理 - 区分同一用户的不同设备（手机/平板/桌面）
%%   3. 安全审计 - 追踪消息来自哪个设备
%%   4. 消息撤回 - 只撤回特定设备发送的消息
%%   5. 在线状态 - 管理每个设备的在线/离线状态
%%
%% @param Payload 消息 payload，可以是 Map、Binary(JSON)、List 或其他类型
%% @param State WebSocket 状态映射，包含 did 和 dtype 字段
%% @return 修改后的 payload（Map 或其他类型），包含 sender_did 和 sender_dtype 字段
%%
%% @end
-spec inject_sender_device(map() | binary() | [binary()] | term(), map()) ->
    map() | binary() | [binary()] | term().
inject_sender_device(Payload, State) when is_map(Payload) ->
    %% 从 State 中提取设备 ID 和设备类型
    DID = maps:get(did, State, <<>>),
    DType = maps:get(dtype, State, <<>>),

    %% 将设备信息注入到 payload 中
    %% sender_did: 发送者的设备 ID（由服务端验证，不可伪造）
    %% sender_dtype: 发送者的设备类型（ios/android/web/desktop）
    Payload2 = maps:put(<<"sender_did">>, DID, Payload),
    maps:put(<<"sender_dtype">>, DType, Payload2);
%% @doc 处理 JSON 格式的 payload（二进制 JSON 字符串）
%%
%% 当 payload 为 JSON 字符串时，先解码为 Map，然后递归调用 inject_sender_device/2
%% 这样可以支持客户端发送的 JSON 格式 payload
%%
%% @param Payload JSON 格式的 payload（二进制字符串）
%% @param State WebSocket 状态映射
%% @return 解码并注入设备信息后的 payload（Map 或其他类型）
inject_sender_device(Payload, State) when is_binary(Payload) ->
    try jsone:decode(Payload, [{object_format, map}]) of
        Map when is_map(Map) ->
            %% 解码成功，递归调用处理 Map
            inject_sender_device(Map, State);
        _ ->
            %% 解码失败或不是 Map 类型，原样返回
            Payload
    catch
        _:_ ->
            %% JSON 解码异常，原样返回（保持向后兼容）
            Payload
    end;
%% @doc 处理 iolist 格式的 payload
%%
%% 当 payload 为 iolist（Erlang 列表二进制格式）时，先转换为 binary，然后递归处理
%% 这种格式通常在 Erlang 内部数据处理中使用
%%
%% @param Payload iolist 格式的 payload
%% @param State WebSocket 状态映射
%% @return 转换并注入设备信息后的 payload
inject_sender_device(Payload, State) when is_list(Payload) ->
    %% 将 iolist 转换为 binary，然后递归调用
    inject_sender_device(iolist_to_binary(Payload), State);
%% @doc 其他类型的 payload（不处理）
%%
%% 对于其他类型（text、atom 等），直接原样返回，不做任何修改
%% 这样可以保持向后兼容性，不会破坏现有的消息处理逻辑
%%
%% @param Payload 其他类型的 payload
%% @param _State WebSocket 状态映射（未使用）
%% @return 原样返回的 payload
inject_sender_device(Payload, _State) ->
    Payload.

%% @doc 把服务端验证过的发送者设备标识盖到**消息信封顶层**。
%%
%% 与 inject_sender_device/2 的区别（也是它存在的理由）：
%% inject_sender_device/2 注入的是 **payload 内部**，因此只在 payload 是 map、
%% 或可 JSON 解码为 map 的 binary 时才生效。而 E2EE 消息的 payload 对服务端
%% 是不透明的——
%%   - v1/v2：payload 是密文字符串 `base64(nonce).base64(ct)`，JSON 解码失败；
%%   - v3（PFv3）：密文在 e2ee.devices 内，**外层 payload 恒为空串**。
%% 两种情况下 inject_sender_device/2 都原样返回，什么都没注入。
%%
%% 后果（已实证，见 imboyapp/test/service/e2ee/production_inbound_frame_gate_test.dart）：
%% 客户端 `_validateContextBinding` 第 6 项拿 `sender_did` 与受认证的
%% protected_header.sender_did 硬比对（ADR 15 §3.3），服务端从未在帧顶层提供该
%% 字段 → 每条生产 C2C v3 消息都被判 `context_mismatch_sender_did` 而不可读。
%%
%% 因此设备标识必须盖在**信封层**而不是 payload 层：信封是服务端组装的、
%% 与 payload 是否加密无关，这样才对所有 payload 形状都成立。
%%
%% 安全语义不变：值取自 WebSocket State 的 did/dtype（连接建立时认证得到），
%% 客户端无法伪造——这正是 ADR 15 §3.3 第 6 项要绑定的那个「服务端验证过的」值。
%%
%% @param Data 消息信封（顶层 map）
%% @param State WebSocket 状态映射，包含 did 和 dtype
%% @return 顶层带 sender_did/sender_dtype 的信封
%% @end
-spec stamp_sender_device(map(), map()) -> map().
stamp_sender_device(Data, State) when is_map(Data) ->
    DID = maps:get(did, State, <<>>),
    DType = maps:get(dtype, State, <<>>),
    Data2 = maps:put(<<"sender_did">>, DID, Data),
    maps:put(<<"sender_dtype">>, DType, Data2);
stamp_sender_device(Data, _State) ->
    Data.

%% @doc 把信封顶层的发送者设备标识带进待投递消息。
%%
%% assemble_msg/8 只认识固定字段集，不接受调用方透传的额外字段；投递侧据此
%% 把 stamp_sender_device/2 盖好的两个字段并进去。缺字段时不补空值——
%% 补 `<<>>` 会让接收侧把「服务端没提供」误判成「设备 ID 是空串」。
%% @end
-spec with_sender_device(map(), map()) -> map().
with_sender_device(Msg, Data) when is_map(Msg), is_map(Data) ->
    maps:merge(Msg, maps:with([<<"sender_did">>, <<"sender_dtype">>], Data));
with_sender_device(Msg, _Data) ->
    Msg.

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

%% ===================================================================
%% 消息验证功能 (v2.0)
%% ===================================================================

%% @doc 验证 v2.0 消息格式
%% 验证消息是否包含必填字段，并根据消息类型验证特定字段。
%%
%% @param Msg 消息 map
%% @returns {ok, ValidatedMsg} | {error, Reason}
%%
%% @example
%% case message_ds:validate_message(Msg) of
%%     {ok, Validated} -> process_message(Validated);
%%     {error, Reason} -> handle_error(Reason)
%% end
-spec validate_message(map()) -> {ok, map()} | {error, binary()}.
validate_message(Msg) when is_map(Msg) ->
    case validate_base_required_fields(Msg) of
        {error, _} = Err ->
            Err;
        ok ->
            Type = maps:get(<<"type">>, Msg, <<>>),
            validate_message_by_type(Type, Msg)
    end;
validate_message(_Msg) ->
    {error, <<"invalid_message_format">>}.

%% @private
%% @doc 验证基础必填字段（id/type）
-spec validate_base_required_fields(map()) -> ok | {error, binary()}.
validate_base_required_fields(Msg) ->
    Id = maps:get(<<"id">>, Msg, <<>>),
    Type = maps:get(<<"type">>, Msg, <<>>),
    case is_non_empty_binary(Id) andalso is_non_empty_binary(Type) of
        true -> ok;
        false -> {error, <<"missing_required_fields">>}
    end.

%% @private
%% @doc 根据消息类型验证特定字段
-spec validate_message_by_type(binary(), map()) -> {ok, map()} | {error, binary()}.
validate_message_by_type(<<"S2C">>, Msg) ->
    % S2C 消息：action 必须有
    Action = maps:get(<<"action">>, Msg, <<>>),
    case Action of
        <<>> ->
            {error, <<"s2c_message_missing_action">>};
        _ ->
            % S2C 消息不应该有 e2ee
            case maps:get(<<"e2ee">>, Msg, null) of
                null -> {ok, Msg};
                _ -> {error, <<"s2c_message_not_support_e2ee">>}
            end
    end;
validate_message_by_type(Type, Msg) when
    Type =:= <<"C2C">>; Type =:= <<"C2G">>; Type =:= <<"C2S">>
->
    case validate_peer_fields(Msg) of
        {error, _} = Err ->
            Err;
        ok ->
            % 非 S2C 消息：msg_type 必须有
            MsgType = maps:get(<<"msg_type">>, Msg, <<>>),
            case MsgType of
                <<>> ->
                    {error, <<"non_s2c_message_missing_msg_type">>};
                _ ->
                    case validate_e2ee_envelope(maps:get(<<"e2ee">>, Msg, null)) of
                        ok -> {ok, Msg};
                        {error, _} = Err2 -> Err2
                    end
            end
    end;
validate_message_by_type(Type, Msg) when is_binary(Type) ->
    case is_webrtc_type(Type) of
        true ->
            case validate_peer_fields(Msg) of
                ok -> {ok, Msg};
                {error, _} = Err -> Err
            end;
        false ->
            % 未知消息类型
            {error, <<"unknown_message_type">>}
    end;
validate_message_by_type(_Type, _Msg) ->
    {error, <<"invalid_message_format">>}.

%% @private
%% @doc 外层 E2EE 信封校验（E2EE-060 / ADR 15 §10）
%%
%% 服务端**不解密、不解析** protected_header 内容，只做三件事：
%%   1. 外层尺寸上界，避免未授权大分配；
%%   2. meta_version >= 3 时必填字段存在且类型正确；
%%   3. 通过后原样透传，不重建、不裁剪任何字段（含未知扩展字段）。
%%
%% 校验发生在 staging 落库与广播之前，因此超限/畸形信封得到的是稳定的
%% 业务错误码，而不是 cowboy 层 max_frame_size 触发的连接断开。
-spec validate_e2ee_envelope(term()) -> ok | {error, binary()}.
validate_e2ee_envelope(null) ->
    ok;
%% 未加密占位：契约（docs/reference/websocket-api-2.md）允许未加密消息
%% e2ee 字段为不存在或空字符串 ""，客户端 action 帧（message_revoke 等）
%% 按契约发 ""；空串等价于 null，不得按畸形信封拒绝（BUG#141）。
validate_e2ee_envelope(<<>>) ->
    ok;
validate_e2ee_envelope(E2EE) when is_map(E2EE) ->
    case e2ee_envelope_within_limit(E2EE) of
        false ->
            {error, <<"e2ee_envelope_too_large">>};
        true ->
            case maps:get(<<"meta_version">>, E2EE, 0) of
                V when is_integer(V), V >= 3 -> validate_v3_envelope_shape(E2EE);
                _ -> ok
            end
    end;
validate_e2ee_envelope(_) ->
    {error, <<"e2ee_envelope_invalid">>}.

%% @private
%% @doc v3 信封形态：ADR 15 §3.3 单信封，或 E2EE-029 per-device fan-out
-spec validate_v3_envelope_shape(map()) -> ok | {error, binary()}.
validate_v3_envelope_shape(E2EE) ->
    case maps:get(<<"devices">>, E2EE, undefined) of
        undefined ->
            require_envelope_fields(E2EE);
        Devices when is_map(Devices), map_size(Devices) > 0 ->
            case lists:all(fun(D) -> require_envelope_fields(D) =:= ok end, maps:values(Devices)) of
                true -> ok;
                false -> {error, <<"e2ee_envelope_invalid">>}
            end;
        _ ->
            {error, <<"e2ee_envelope_invalid">>}
    end.

%% @private
%% @doc 必填外层字段：存在且为非空 binary（不校验内容语义）
-spec require_envelope_fields(term()) -> ok | {error, binary()}.
require_envelope_fields(M) when is_map(M) ->
    Required = [<<"protected_header">>, <<"header_hash">>, <<"ciphertext">>],
    case lists:all(fun(K) -> is_non_empty_binary(maps:get(K, M, undefined)) end, Required) of
        true -> ok;
        false -> {error, <<"e2ee_envelope_invalid">>}
    end;
require_envelope_fields(_) ->
    {error, <<"e2ee_envelope_invalid">>}.

%% @private
%% @doc 外层信封尺寸上界
%%
%% 默认 1 MiB：远大于任何合法信封（PFv3 header 客户端上限 8 KiB，
%% C2C fan-out 只针对对端少数几台设备），又低于 cowboy 的
%% max_frame_size（2 MiB），保证超限时先返回稳定业务错误而不是断连。
%% ponytail: 这里为取尺寸多做一次 jsone:encode；正常信封只有几 KB，
%% 相对"投递不可解密消息"的代价可以忽略。若成为热点再换增量计数。
-spec e2ee_envelope_within_limit(map()) -> boolean().
e2ee_envelope_within_limit(E2EE) ->
    Max = application:get_env(imboy, e2ee_envelope_max_bytes, 1048576),
    try
        byte_size(jsone:encode(E2EE, [native_utf8])) =< Max
    catch
        _:_ -> false
    end.

%% @private
%% @doc 验证消息发送方和接收方字段
%% v2.0: TSID 通过 JSON 传输时是 integer（见根目录 CLAUDE.md TSID 规范），
%% 同时本地代码可能用 binary 字符串形式，两者都视为合法。
-spec validate_peer_fields(map()) -> ok | {error, binary()}.
validate_peer_fields(Msg) ->
    From = maps:get(<<"from">>, Msg, <<>>),
    To = maps:get(<<"to">>, Msg, <<>>),
    case is_valid_peer_id(From) andalso is_valid_peer_id(To) of
        true -> ok;
        false -> {error, <<"missing_required_fields">>}
    end.

%% @private
%% @doc TSID 在 JSON 线上格式为正整数，本地代码可能传 binary 字符串
-spec is_valid_peer_id(term()) -> boolean().
is_valid_peer_id(Id) when is_integer(Id), Id > 0 -> true;
is_valid_peer_id(Id) when is_binary(Id), byte_size(Id) > 0 -> true;
is_valid_peer_id(_) -> false.

%% @private
%% @doc 判断是否为 WebRTC 信令类型
-spec is_webrtc_type(binary()) -> boolean().
is_webrtc_type(Type) when is_binary(Type) ->
    case cowboy_bstr:to_lower(Type) of
        <<"webrtc_", _Rest/binary>> -> true;
        _ -> false
    end.

%% @private
%% @doc 判断字段是否为非空 binary
-spec is_non_empty_binary(term()) -> boolean().
is_non_empty_binary(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    true;
is_non_empty_binary(_) ->
    false.

%% @doc 将 v1.0 格式消息转换为 v2.0 格式
%% 自动检测消息版本并转换。如果已经是 v2.0 格式则直接返回。
%%
%% v1.0 格式：
%%   payload 包含 msg_type/action/e2ee
%%
%% v2.0 格式：
%%   msg_type/action/e2ee 在顶层
%%
%% @param Msg 消息 map（v1.0 或 v2.0）
%% @returns v2.0 格式的消息 map
%%
%% @example
%% V1Msg = #{<<"type">> => <<"C2C">>, <<"payload">> => #{<<"msg_type">> => <<"text">>}},
%% V2Msg = message_ds:convert_v1_to_v2(V1Msg).
-spec convert_v1_to_v2(map()) -> map().
convert_v1_to_v2(Msg) when is_map(Msg) ->
    % 检测消息版本
    Payload = maps:get(<<"payload">>, Msg, #{}),
    HasTopLevelMsgType = maps:is_key(<<"msg_type">>, Msg),
    HasTopLevelAction = maps:is_key(<<"action">>, Msg),

    % 如果顶层已有 msg_type 或 action，说明是 v2.0 格式
    case HasTopLevelMsgType orelse HasTopLevelAction of
        true ->
            Msg;
        false when is_map(Payload) ->
            % v1.0 格式：从 payload 提取字段到顶层
            MsgType = maps:get(<<"msg_type">>, Payload, <<>>),
            Action = maps:get(<<"action">>, Payload, <<>>),
            E2EE = maps:get(<<"e2ee">>, Payload, null),

            % 清理 payload 中的顶层字段
            Payload2 = maps:without([<<"msg_type">>, <<"action">>, <<"e2ee">>], Payload),

            % 构建 v2.0 格式消息
            Msg#{
                <<"msg_type">> => MsgType,
                <<"action">> => Action,
                <<"e2ee">> => E2EE,
                <<"payload">> => Payload2
            };
        false ->
            % payload 不是 map，无法转换，直接返回
            Msg
    end;
convert_v1_to_v2(Msg) ->
    Msg.

%% @doc 构建管理后台消息多表 UNION SQL（用于跨 c2c/c2g/c2s/s2c 搜索）
%% Scopes: [c2c | c2g | c2s | s2c]
-spec build_adm_union_sql(list()) -> binary().
build_adm_union_sql(Scopes) ->
    Tbc2c = msg_c2c_repo:tablename(),
    Tbc2g = msg_c2g_repo:tablename(),
    Tbc2s = msg_c2s_repo:tablename(),
    Tbs2c = msg_s2c_repo:tablename(),
    TbTimeline = msg_c2g_timeline_repo:tablename(),
    ScopeSqls = [adm_scope_sql(Scope, Tbc2c, Tbc2g, Tbc2s, Tbs2c, TbTimeline) || Scope <- Scopes],
    iolist_to_binary(lists:join(<<" UNION ALL ">>, ScopeSqls)).

adm_scope_sql(c2c, Tbc2c, _Tbc2g, _Tbc2s, _Tbs2c, _TbTimeline) ->
    iolist_to_binary([
        <<"SELECT 'c2c' AS scope, msg_id, from_id, to_id, msg_type, ''::text AS action, payload, created_at, server_ts ">>,
        <<"FROM ">>,
        Tbc2c,
        <<" m ">>,
        <<"WHERE ($1 = 0 OR from_id = $1 OR to_id = $1) ">>,
        <<"AND ($2 = 0 OR ((from_id = $2 AND to_id = $3) OR (from_id = $3 AND to_id = $2))) ">>,
        <<"AND ($5::timestamptz IS NULL OR created_at >= $5::timestamptz) ">>,
        <<"AND ($6::timestamptz IS NULL OR created_at <= $6::timestamptz) ">>,
        <<"AND ($7::text IS NULL OR payload::text ILIKE $7::text) ">>,
        <<"AND ($8::text IS NULL OR msg_id::text = $8::text)">>
    ]);
adm_scope_sql(c2g, _Tbc2c, Tbc2g, _Tbc2s, _Tbs2c, TbTimeline) ->
    iolist_to_binary([
        <<"SELECT 'c2g' AS scope, msg_id, from_id, to_id, msg_type, ''::text AS action, payload::text AS payload, created_at, server_ts ">>,
        <<"FROM ">>,
        Tbc2g,
        <<" m ">>,
        <<"WHERE ($1 = 0 OR from_id = $1 OR EXISTS (">>,
        <<"SELECT 1 FROM ">>,
        TbTimeline,
        <<" t WHERE t.msg_id = m.msg_id AND t.to_uid = $1)) ">>,
        <<"AND ($4 = 0 OR to_id = $4) ">>,
        <<"AND ($5::timestamptz IS NULL OR created_at >= $5::timestamptz) ">>,
        <<"AND ($6::timestamptz IS NULL OR created_at <= $6::timestamptz) ">>,
        <<"AND ($7::text IS NULL OR payload::text ILIKE $7::text) ">>,
        <<"AND ($8::text IS NULL OR msg_id::text = $8::text)">>
    ]);
adm_scope_sql(c2s, _Tbc2c, _Tbc2g, Tbc2s, _Tbc2s, TbTimeline) ->
    iolist_to_binary([
        <<"SELECT 'c2s' AS scope, msg_id, from_id, to_id, msg_type, ''::text AS action, payload, created_at, COALESCE(server_ts, created_at) AS server_ts ">>,
        <<"FROM ">>,
        Tbc2s,
        <<" m ">>,
        <<"WHERE ($1 = 0 OR from_id = $1 OR EXISTS (">>,
        <<"SELECT 1 FROM ">>,
        TbTimeline,
        <<" t WHERE t.msg_id = m.msg_id AND t.to_uid = $1)) ">>,
        <<"AND ($4 = 0 OR to_id = $4) ">>,
        <<"AND ($5::timestamptz IS NULL OR created_at >= $5::timestamptz) ">>,
        <<"AND ($6::timestamptz IS NULL OR created_at <= $6::timestamptz) ">>,
        <<"AND ($7::text IS NULL OR payload::text ILIKE $7::text) ">>,
        <<"AND ($8::text IS NULL OR msg_id::text = $8::text)">>
    ]);
adm_scope_sql(s2c, _Tbc2c, _Tbc2g, _Tbc2s, Tbs2c, _TbTimeline) ->
    iolist_to_binary([
        <<"SELECT 's2c' AS scope, msg_id, from_id, to_id, msg_type, action, payload::text AS payload, created_at, server_ts ">>,
        <<"FROM ">>,
        Tbs2c,
        <<" m ">>,
        <<"WHERE ($1 = 0 OR from_id = $1 OR to_id = $1) ">>,
        <<"AND ($4 = 0 OR from_id = $4 OR to_id = $4) ">>,
        <<"AND ($5::timestamptz IS NULL OR created_at >= $5::timestamptz) ">>,
        <<"AND ($6::timestamptz IS NULL OR created_at <= $6::timestamptz) ">>,
        <<"AND ($7::text IS NULL OR payload::text ILIKE $7::text) ">>,
        <<"AND ($8::text IS NULL OR msg_id::text = $8::text)">>
    ]).
