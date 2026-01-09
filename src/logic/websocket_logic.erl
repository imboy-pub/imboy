-module(websocket_logic).

%%%
% WebSocket 连接管理模块（ACK 定时器管理）
%%%

-include("log.hrl").

-export([cancel_timer/3, handle_ack_cancel/3]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 取消 ACK 重试定时器（跨节点广播）
% 使用 syn 库实现高效的非阻塞跨节点广播
-spec cancel_timer(pos_integer(), binary(), binary()) -> ok.
cancel_timer(CurrentUid, DID, MsgId) ->
    Key = {CurrentUid, DID, MsgId},
    ok = ?DEBUG_LOG(["CANCEL_TIMER", Key]),

    %% 【优化】使用 syn 广播替代 rpc:multicall
    %% 优势：
    %% 1. 非阻塞式广播，无需等待所有节点响应
    %% 2. 自动处理节点故障，无需手动重试
    %% 3. 性能更高，适合高频 ACK 场景
    imboy_syn:broadcast_ack_cancel(CurrentUid, DID, MsgId),

    %% 【重要】立即执行本地处理
    %% 确保当前节点立即处理 ACK，不依赖 syn 的广播延迟
    handle_ack_cancel(CurrentUid, DID, MsgId),

    ok.

%% @doc 实际执行 timer 撤销（本地节点）
-spec handle_ack_cancel(pos_integer(), binary(), binary()) -> ok.
handle_ack_cancel(ToUid, DID, MsgId) ->
    TimerKey = {ToUid, DID, MsgId},

    %% 【关键修复】先设置 ACK 标志，再取消定时器
    %% 这样即使定时器消息已在队列中，投递前也会检查到 ACK
    AckReceivedKey = {ack_received, ToUid, DID, MsgId},
    imboy_cache:set(AckReceivedKey, true, 40000),  % 40秒 TTL（最大重试时间）

    %% 【改进】打印ACK处理日志
    io:format("📥 [ACK_CANCEL] Processing: MsgId=~s, Uid=~p, DID=~s~n",
              [MsgId, ToUid, DID]),
    io:format("✅ [ACK_CANCEL] ACK received flag set first: MsgId=~s~n", [MsgId]),

    case imboy_cache:get(TimerKey) of
        {ok, Ref} when is_reference(Ref) ->
            io:format("✅ [ACK_CANCEL] Canceling timer: MsgId=~s, Ref=~p~n", [MsgId, Ref]),
            case erlang:cancel_timer(Ref) of
                false ->
                    io:format("⚠️ [ACK_CANCEL] Timer already fired: MsgId=~s~n", [MsgId]);
                Time ->
                    io:format("✅ [ACK_CANCEL] Timer canceled, remaining time: ~pms~n", [Time])
            end,
            imboy_cache:flush(TimerKey),
            ok;
        undefined ->
            io:format("⚠️ [ACK_CANCEL] Timer not found: MsgId=~s~n", [MsgId]),
            ok;
        {ok, Other} ->
            io:format("⚠️ [ACK_CANCEL] Invalid cache value: MsgId=~s, Value=~p~n", [MsgId, Other]),
            imboy_cache:flush(TimerKey),
            ok
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
