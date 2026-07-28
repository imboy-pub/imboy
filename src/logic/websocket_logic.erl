-module(websocket_logic).

%%%
% WebSocket 连接管理模块（ACK 定时器管理）
%%%

-include("log.hrl").

%% @doc 取消 ACK 重试定时器（跨节点广播）
%% 使用 syn 库实现高效的非阻塞跨节点广播
%% @param CurrentUid 当前用户ID
%% @param DID 设备ID
%% @param MsgId 消息ID
%% @returns ok
-export([cancel_timer/3]).
-export([handle_ack_cancel/3]).
-export([decode_message/1]).
-export([convert_v1_to_v2/1]).
-export([validate_message/1]).
-export([inject_sender_device/2]).
-export([stamp_sender_device/2]).
-export([send_next/6]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 取消 ACK 重试定时器（跨节点广播）
% 使用 syn 库实现高效的非阻塞跨节点广播
-spec cancel_timer(pos_integer(), binary(), binary()) -> ok.
cancel_timer(CurrentUid, DID, MsgId) ->
    Key = {CurrentUid, DID, MsgId},
    _ = ?DEBUG_LOG({cancel_timer, Key}),

    %% 【优化】使用 syn 广播替代 rpc:multicall
    %% 优势：
    %% 1. 非阻塞式广播，无需等待所有节点响应
    %% 2. 自动处理节点故障，无需手动重试
    %% 3. 性能更高，适合高频 ACK 场景
    _ = broadcast_ack_cancel_safe(CurrentUid, DID, MsgId),

    %% 【重要】立即执行本地处理
    %% 确保当前节点立即处理 ACK，不依赖 syn 的广播延迟
    handle_ack_cancel(CurrentUid, DID, MsgId),

    ok.

-spec handle_ack_cancel(pos_integer(), binary(), binary()) -> ok.
%% @doc 实际执行 timer 撤销（本地节点）
handle_ack_cancel(ToUid, DID, MsgId) ->
    TimerKey = {ToUid, DID, MsgId},

    %% 【关键修复】先设置 ACK 标志，再取消定时器
    %% 这样即使定时器消息已在队列中，投递前也会检查到 ACK
    AckReceivedKey = {ack_received, ToUid, DID, MsgId},
    % 40 秒 TTL（覆盖最大重试窗口）——imboy_cache:set/3 的 TTL 单位是「秒」，
    % 此前误传 40000 被当作 40000 秒（约 11 小时），导致每条已 ACK 消息的标志
    % 在缓存中滞留 11 小时，高并发下缓存持续膨胀。
    imboy_cache:set(AckReceivedKey, true, 40),

    _ = ?DEBUG_LOG({ack_cancel_processing, MsgId, ToUid, DID}),

    case imboy_cache:get(TimerKey) of
        {ok, Ref} when is_reference(Ref) ->
            _ = ?DEBUG_LOG({ack_cancel_canceling_timer, MsgId, Ref}),
            case erlang:cancel_timer(Ref) of
                false ->
                    _ = ?DEBUG_LOG({ack_cancel_timer_already_fired, MsgId});
                Time ->
                    _ = ?DEBUG_LOG({ack_cancel_timer_canceled, MsgId, Time})
            end,
            imboy_cache:flush(TimerKey),
            ok;
        undefined ->
            _ = ?DEBUG_LOG({ack_cancel_timer_not_found, MsgId}),
            ok;
        {ok, Other} ->
            _ = ?WARN_LOG({ack_cancel_invalid_cache_value, MsgId, Other}),
            imboy_cache:flush(TimerKey),
            ok
    end.

%% 防御性封装：跨节点广播失败时不影响本地 ACK 取消
broadcast_ack_cancel_safe(Uid, DID, MsgId) ->
    try
        imboy_syn:broadcast_ack_cancel(Uid, DID, MsgId)
    catch
        Class:Reason ->
            _ = ?WARN_LOG({ack_cancel_broadcast_failed, Uid, DID, MsgId, Class, Reason}),
            ok
    end.

%% @doc 解码 WebSocket 原始消息为 map
-spec decode_message(binary()) -> map().
decode_message(Msg) ->
    message_ds:decode_websocket_message(Msg).

%% @doc 将 v1 消息格式转换为 v2
-spec convert_v1_to_v2(map()) -> map().
convert_v1_to_v2(Data) ->
    message_ds:convert_v1_to_v2(Data).

%% @doc 校验消息合法性
-spec validate_message(map()) -> {ok, map()} | {error, binary()}.
validate_message(Data) ->
    message_ds:validate_message(Data).

%% @doc 将发送者设备信息注入 payload
-spec inject_sender_device(map(), map()) -> map().
inject_sender_device(Payload, State) ->
    message_ds:inject_sender_device(Payload, State).

%% @doc 将发送者设备信息盖到消息信封顶层（payload 不透明时唯一有效的位置）
-spec stamp_sender_device(map(), map()) -> map().
stamp_sender_device(Data, State) ->
    message_ds:stamp_sender_device(Data, State).

%% @doc 发送消息（含设备过滤重试）
-spec send_next(pos_integer() | binary(), binary(), binary(), list(), [binary()], boolean()) -> ok.
send_next(ToUid, MsgId, Msg, MsLi, DIDLi, IncludeDIDLi) ->
    message_ds:send_next(ToUid, MsgId, Msg, MsLi, DIDLi, IncludeDIDLi).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
