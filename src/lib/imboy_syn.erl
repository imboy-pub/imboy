-module(imboy_syn).
%%%
% imboy 用户websocket会话 模块
% imboy user websocket session module
% 对 https://github.com/ostinelli/syn 封装
%%%

-include_lib("stdlib/include/qlc.hrl").
-include("log.hrl").
-include("chat.hrl").

% for user
-export([
    init/0,
    join/4,
    leave/2
]).
-export([publish/2, publish/3, start_delivery_timer/3, schedule_ack_retry/5]).
%% BE-14 慢端预算与热点公平调度
-export([
    delivery_budget_check/3,
    supports_remote_delivery_budget/1,
    supports_remote_delivery_batch/1,
    defer_delivery/3,
    defer_delivery/2,
    clear_backlog_notice/1,
    drop_delivery_budget/1,
    delivery_budget_counters/0,
    reset_delivery_budget_state/0
]).
-export([
    count_user/0, count_user/1,
    count/0
]).
-export([
    list_by_uid/1,
    list_by_limit/1
]).
-export([
    is_online/2,
    online_dids/1
]).

%% ACK 同步相关导出
-export([broadcast_ack_cancel/3]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化 syn 模块，添加节点到 scopes
-spec init() -> ok.
init() ->
    %% 与 ACK 缓存一样由应用启动进程持表，避免首个投递进程退出时删表。
    ok = ensure_budget_table(),
    _ = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([
        ?CHAT_SCOPE,
        % , ?GROUP_SCOPE
        ?ROOM_SCOPE,
        ?CACHE_SCOPE,
        ?QR_LOGIN_SCOPE
    ]),
    ok = announce_delivery_capability(delivery_budget, undefined),
    _ = code:ensure_loaded(message_ds),
    case erlang:function_exported(message_ds, deliver_group_batch, 3) of
        true ->
            announce_delivery_capability(delivery_batch_v1, {message_ds, deliver_group_batch, 3});
        false ->
            ok
    end.

announce_delivery_capability(Name, Metadata) ->
    Key = {Name, node()},
    case syn:lookup(?CACHE_SCOPE, Key) of
        undefined -> syn:register(?CACHE_SCOPE, Key, self(), Metadata);
        _ -> ok
    end.

-spec join(integer(), binary(), pid(), binary()) -> ok | {error, term()}.
join(Uid, DType, Pid, DID) ->
    % Li = list_by_uid(Uid),
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    % ?DEBUG_LOG(["imboy_syn:join/4", Uid, Li]),
    % [P ! stop || {P, {_DT, DID1}} <- Li, DID1 == DID],
    try
        ok = syn:join(?CHAT_SCOPE, Uid, Pid, {DType, DID}),
        ok
    catch
        _:Reason -> {error, Reason}
    end.

-spec leave(integer(), pid()) -> ok | {error, term()}.
leave(Uid, Pid) ->
    try
        syn:leave(?CHAT_SCOPE, Uid, Pid)
    catch
        _:Reason -> {error, Reason}
    end.

% 在线用户数量统计，一个用户在多个不同的设备类型登录，算一个
-spec count_user() -> non_neg_integer().
count_user() ->
    try
        syn:group_count(?CHAT_SCOPE)
    catch
        _:_ -> 0
    end.

% 用户在线设备统计
-spec count_user(integer()) -> non_neg_integer().
count_user(Uid) ->
    try
        syn:member_count(?CHAT_SCOPE, Uid)
    catch
        _:_ -> 0
    end.

% 所有用户在线设备统计
-spec count() -> non_neg_integer().
count() ->
    try
        TableByName = syn_backbone:get_table_name(syn_pg_by_name, ?CHAT_SCOPE),
        case TableByName of
            undefined -> 0;
            _ -> ets:info(TableByName, size)
        end
    catch
        _:_ -> 0
    end.

% [{{Uid, Pid}, {DType, DID}, Nano, Ref, Node}, ...]
-spec list_by_limit(integer() | error) ->
    [{{integer(), pid()}, {binary(), binary()}, integer(), reference(), node()}].
list_by_limit(error) ->
    [];
list_by_limit(Limit) when is_integer(Limit), Limit > 0 ->
    try
        case syn_backbone:get_table_name(syn_pg_by_name, ?CHAT_SCOPE) of
            undefined ->
                [];
            TableByName ->
                case ets:select(TableByName, [{'$1', [], ['$1']}], Limit) of
                    '$end_of_table' -> [];
                    {Li, _} -> Li
                end
        end
    catch
        _:_ -> []
    end;
list_by_limit(_) ->
    [].

-spec list_by_uid(integer()) -> [{pid(), {binary(), binary()}}].
list_by_uid(Uid) ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    try
        syn:members(?CHAT_SCOPE, Uid)
    catch
        _:_ -> []
    end.

%% @doc 检查用户是否在线（按条件过滤）
%% Condition 可以是 {dtype, DType} 或 {did, DID}
-spec is_online(integer(), {dtype | did, binary()}) -> boolean().
is_online(Uid, {dtype, DType}) ->
    Members = list_by_uid(Uid),
    lists:any(fun({_Pid, {DT, _DID}}) -> DT =:= DType end, Members);
is_online(Uid, {did, DID}) ->
    Members = list_by_uid(Uid),
    lists:any(fun({_Pid, {_DType, D}}) -> D =:= DID end, Members).

%% @doc 获取用户在线设备ID列表
-spec online_dids(integer()) -> [binary()].
online_dids(Uid) ->
    Members = list_by_uid(Uid),
    [DID || {_Pid, {_DType, DID}} <- Members].

%% @doc 发布消息到指定用户的所有设备
-spec publish(integer(), term()) -> {ok, non_neg_integer()}.
publish(Uid, Msg) ->
    publish(Uid, Msg, 0).

% Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
-spec publish(integer(), term(), non_neg_integer()) -> {ok, non_neg_integer()}.
publish(Uid, Msg, Delay) when is_integer(Delay), Delay >= 0 ->
    % [{<0.2497.0>,{<<"macos">>,<<"did13">>}}]
    Members = list_by_uid(Uid),
    % ?DEBUG_LOG(["imboy_syn:publish/3", Uid, length(Members), Delay, Msg]),
    do_publish(Members, Msg, Delay);
publish(_Uid, _Msg, _Delay) ->
    % ?DEBUG_LOG(["imboy_syn:publish/3 invalid parameters", _Uid, _Msg, _Delay]),
    {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec do_publish(list(), term(), non_neg_integer()) -> {ok, non_neg_integer()}.
do_publish(Members, Message, 0) ->
    % ?DEBUG_LOG(["imboy_syn:do_publish/3 immediate", length(Members), Message]),
    % 使用 start_timer(0, ...) 确保立即投递和延迟投递的消息格式统一
    % 消息会被包装为 {timeout, Ref, Message} 格式，与延迟投递一致
    [start_delivery_timer(0, Pid, Message) || {Pid, _Meta} <- Members],
    {ok, length(Members)};
do_publish(Members, Message, Delay) when Delay > 0 ->
    % ?DEBUG_LOG(["imboy_syn:do_publish/3 delayed", length(Members), Delay, Message]),
    % Pid ! Message
    % Delay: 最大的值为2^32 -1 milliseconds, 大约为49.7天。
    [start_delivery_timer(Delay, Pid, Message) || {Pid, _Meta} <- Members],
    {ok, length(Members)}.

-spec start_delivery_timer(non_neg_integer(), pid(), term()) -> reference() | ok.
start_delivery_timer(0, Pid, Message) ->
    %% BE-14：0ms 帧改为同步直达邮箱（形状与 start_timer 完全一致：
    %% {timeout, Ref, Msg}，消费侧 websocket_info/delivery_guard 通用子句
    %% 匹配任意 Ref）。start_timer(0) 经定时器轮异步落邮箱，突发连发时
    %% 预算门读到的 message_queue_len 滞后，会让整个 burst 越过预算
    %% （实证：cap=5 下 6 连发全通过）。同步直达让预算信号即时准确；
    %% hop0 语义本就是"立即投递"，无延迟回退，且 hop0 从不在
    %% ack_retry_cache 登记，无可取消语义损失。hop>=1 仍用真实 timer
    %% （cancel_timer/Ref 取消语义依赖）。
    Pid ! {timeout, erlang:make_ref(), Message},
    ok;
start_delivery_timer(Delay, Pid, Message) when node(Pid) =:= node() ->
    erlang:start_timer(Delay, Pid, Message);
start_delivery_timer(Delay, Pid, Message) ->
    erpc:cast(node(Pid), ?MODULE, start_delivery_timer, [Delay, Pid, Message]).

-spec schedule_ack_retry(
    non_neg_integer(), pid(), {term(), term(), term()}, term(), non_neg_integer()
) -> ok.
schedule_ack_retry(Delay, Pid, TimerKey, Message, TTL) when node(Pid) =:= node() ->
    {Uid, DID, MsgId} = TimerKey,
    case ack_retry_cache:get({ack_received, Uid, DID, MsgId}) of
        {ok, true} ->
            ok;
        _ ->
            case ack_retry_cache:get(TimerKey) of
                {ok, OldRef} when is_reference(OldRef) ->
                    _ = erlang:cancel_timer(OldRef);
                _ ->
                    ok
            end,
            Ref = erlang:start_timer(Delay, Pid, Message),
            ack_retry_cache:set(TimerKey, Ref, TTL)
    end;
schedule_ack_retry(Delay, Pid, TimerKey, Message, TTL) ->
    erpc:cast(node(Pid), ?MODULE, schedule_ack_retry, [Delay, Pid, TimerKey, Message, TTL]).

%% ===================================================================
%% ACK 同步功能（复用 ?CHAT_SCOPE）
%% ===================================================================

%% @doc 广播 ACK 取消消息到指定 UID 的所有设备
%% 使用现有的 ?CHAT_SCOPE，无需额外注册
%% 优势：
%% 1. 零额外资源消耗（复用现有注册）
%% 2. 极高性能（直接查询 UID，不扫描全表）
%% 3. 代码简洁（单 Scope 设计）
%% @param Uid 用户ID
%% @param DID 设备ID（用于标识当前设备）
%% @param MsgId 消息ID
-spec broadcast_ack_cancel(pos_integer(), binary(), binary()) -> ok.
broadcast_ack_cancel(Uid, DID, MsgId) ->
    %% 构建 ACK 取消消息
    Message = {ack_cancel, Uid, DID, MsgId, erlang:monotonic_time(millisecond)},

    try
        %% 【优化】直接查询该 UID 的所有设备进程（跨节点）
        %% syn:members/?CHAT_SCOPE 会返回所有节点上该 UID 的进程
        % [
        %     {
        %         <0.9568.0>,
        %         {<<"macos">>,<<"03F17F1F-770F-5CBC-AADA-F3722CF4E90B">>}
        %     }
        % ]
        Members = syn:members(?CHAT_SCOPE, Uid),

        _ = ?DEBUG_LOG([ack_cancel_broadcast, MsgId, Uid, DID, length(Members)]),

        %% 异步发送到所有相关进程（跨节点自动处理）
        lists:foreach(
            fun({Pid, _Meta}) ->
                Pid ! Message
            end,
            Members
        ),
        _ = ?DEBUG_LOG([ack_cancel_broadcast_complete, MsgId, Uid, DID, length(Members)]),
        ok
    catch
        _:Error ->
            _ = ?ERROR_LOG([ack_cancel_broadcast_failed, MsgId, Uid, DID, Error]),
            %% 降级：确保本地至少执行一次
            local_ack_cancel(Uid, DID, MsgId),
            ok
    end.

%% @doc 本地执行 ACK 取消（降级方案）
-spec local_ack_cancel(pos_integer(), binary(), binary()) -> ok.
local_ack_cancel(Uid, DID, MsgId) ->
    %% 调用 websocket_logic:handle_ack_cancel
    websocket_logic:handle_ack_cancel(Uid, DID, MsgId),
    ok.

%% ===================================================================
%% BE-14 慢端投递预算与热点公平调度
%% ===================================================================
%% 节点本地 ETS（应用启动时创建、public；行由
%% websocket_handler:terminate 显式 drop，孤儿行无害且可被 reset 清理）。
%%
%% 语义（BE-14:LOCAL:01/02/03）：
%% - 预算按目标 WS 连接进程的**实时邮箱**判定（message_queue_len 为真源）：
%%   无需记账/释放回路——慢端积压即受限，排空后自动恢复（慢端之后可补齐）。
%% - 字节预算 = QLen x 该连接帧尺寸 EWMA 估算（无逐帧簿记，天然无泄漏；
%%   取消的重试跳从未进邮箱，也就永不计入）。
%% - 双通道公平：bulk（C2G 热点群实时扇出）必须为 direct（C2C/S2C/C2S/
%%   离线补拉）预留 headroom——bulk 饱和不饿死普通会话。
%% - 超预算帧**不入队**（不投递、不排重试 timer）。事实仍持久在库，由
%%   合并后的轻通知（S2C pull_offline_msg）引导客户端拉取补齐；轻通知
%%   不写 ack_received、不落任何库表（不虚增已读/送达事实）。
%% - 控制帧（close/kick_device/ack_cancel/stop）不经本通道：撤权/删除
%%   语义不受预算影响；投递边界授权（websocket_delivery_guard）仍在
%%   下游对每次投递（含重试跳）把关，预算门不构成绕过。

-define(DELIVERY_BUDGET_TAB, imboy_delivery_budget_ets).
-define(BUDGET_EWMA_ALPHA, 0.25).
%% 默认值：256 帧滞留上限（远大于 MsLi 重试窗口合计）与 8 MiB 字节估算
%% 上限；direct 预留 32 帧 / 1 MiB。均可经 app env 覆盖，非法值回退默认。
-define(DEFAULT_BUDGET_MAX_COUNT, 256).
-define(DEFAULT_BUDGET_MAX_BYTES, 8388608).
-define(DEFAULT_BUDGET_RESERVE_COUNT, 32).
-define(DEFAULT_BUDGET_RESERVE_BYTES, 1048576).

-type delivery_lane() :: bulk | direct.

%% A mixed-version peer advertises this before accepting WebSocket sessions.
-spec supports_remote_delivery_budget(node()) -> boolean().
supports_remote_delivery_budget(Node) ->
    syn:lookup(?CACHE_SCOPE, {delivery_budget, Node}) =/= undefined.

%% A bound registry PID advertises the loaded batch entry point for this node
%% incarnation. Do not reuse a separate node-name capability cache on reconnect.
-spec supports_remote_delivery_batch(node()) -> boolean().
supports_remote_delivery_batch(Node) ->
    case syn:lookup(?CACHE_SCOPE, {delivery_batch_v1, Node}) of
        {Pid, {message_ds, deliver_group_batch, 3}} when node(Pid) =:= Node ->
            lists:member(Node, nodes(connected));
        _ ->
            false
    end.

%% @doc 投递预算门：allow=按原路径投递；defer=超预算降级（不入队）。
%% 判定与计数在此完成——计数即"调用数 oracle"（delivered+deferred==尝试数）。
-spec delivery_budget_check(pid(), non_neg_integer(), delivery_lane()) -> allow | defer.
delivery_budget_check(Pid, FrameBytes, Lane) when is_pid(Pid) ->
    ok = ensure_budget_table(),
    QLen =
        case erlang:process_info(Pid, message_queue_len) of
            {message_queue_len, N} -> N;
            undefined -> 0
        end,
    Decision =
        case QLen of
            0 ->
                allow;
            _ ->
                EstBytes = estimate_retained_bytes(Pid, QLen, FrameBytes),
                case Lane of
                    bulk ->
                        budget_decide(QLen < bulk_cap_count(), EstBytes < bulk_cap_bytes());
                    direct ->
                        budget_decide(QLen < budget_max_count(), EstBytes < budget_max_bytes())
                end
        end,
    bump_budget_counter(counter_key(Lane, Decision)),
    Decision.

budget_decide(true, true) -> allow;
budget_decide(_, _) -> defer.

counter_key(bulk, allow) -> {c, bulk_delivered};
counter_key(bulk, defer) -> {c, bulk_deferred};
counter_key(direct, allow) -> {c, direct_delivered};
counter_key(direct, defer) -> {c, direct_deferred}.

%% @doc 超预算降级收尾：累计该连接滞留计数，并按"单连接至多一条在途轻
%% 通知"合并投递一条 S2C pull_offline_msg（绕过预算门，否则永饿死）。
%% 通知只携带合并计数与原因，不携带业务载荷/密文，不写任何库表。
-spec defer_delivery(pid(), integer() | binary(), delivery_lane()) -> ok.
defer_delivery(Pid, ToUid, Lane) ->
    ok = ensure_budget_table(),
    ok = bump_budget_counter(counter_key(Lane, defer)),
    defer_delivery(Pid, ToUid).

-spec defer_delivery(pid(), integer() | binary()) -> ok.
defer_delivery(Pid, Uid) when is_pid(Pid) ->
    ok = ensure_budget_table(),
    Deferred = ets:update_counter(
        ?DELIVERY_BUDGET_TAB, {deferred_total, Pid}, {2, 1}, {{deferred_total, Pid}, 0}
    ),
    case claim_backlog_notice_slot(Pid) of
        allowed ->
            Notice =
                #{
                    <<"ver">> => ?CUR_MSG_VER,
                    <<"id">> => elib_id:gen("pull_offline"),
                    <<"type">> => <<"S2C">>,
                    <<"from">> => <<>>,
                    <<"to">> => Uid,
                    <<"msg_type">> => <<>>,
                    <<"action">> => <<"pull_offline_msg">>,
                    <<"e2ee">> => null,
                    <<"payload">> => #{
                        <<"reason">> => <<"delivery_budget">>,
                        <<"deferred">> => Deferred
                    },
                    <<"server_ts">> => elib_dt:millisecond()
                },
            Json = jsone:encode(Notice, [native_utf8]),
            _ = start_delivery_timer(0, Pid, Json),
            bump_budget_counter({c, notice_sent});
        coalesced ->
            bump_budget_counter({c, notice_coalesced})
    end,
    ok.

%% 原子占用通知槽：0 -> 1 成功即 allowed；已占用/并发占用失败即 coalesced。
-spec claim_backlog_notice_slot(pid()) -> allowed | coalesced.
claim_backlog_notice_slot(Pid) ->
    ok = ensure_budget_table(),
    case select_set_notice_pending(Pid, 0, 1) of
        1 ->
            allowed;
        0 ->
            %% 行缺失（首次）与已置位（合并）都返回 0：插入哨兵后重试一次区分。
            _ = ets:insert_new(?DELIVERY_BUDGET_TAB, {Pid, 0}),
            case select_set_notice_pending(Pid, 0, 1) of
                1 -> allowed;
                0 -> coalesced
            end
    end.

select_set_notice_pending(Pid, From, To) ->
    ets:select_replace(?DELIVERY_BUDGET_TAB, [
        {{Pid, From}, [], [{{Pid, To}}]}
    ]).

%% @doc 连接消费到 pull_offline_msg 触发帧（登录阈值或本预算轻通知）即
%% 释放通知槽：客户端将拉取补齐，后续新滞留可再通知。由
%% websocket_delivery_guard 在解码后的投递边界调用。
-spec clear_backlog_notice(pid()) -> ok.
clear_backlog_notice(Pid) when is_pid(Pid) ->
    case ets:whereis(?DELIVERY_BUDGET_TAB) of
        undefined ->
            ok;
        _ ->
            _ = ets:insert(?DELIVERY_BUDGET_TAB, {Pid, 0}),
            ok
    end.

%% @doc 连接终止清理：pid 键行（通知槽/EWMA/滞留计数）全部删除；
%% 聚合计数行保留（接受事实仍可解释，不随连接消失）。
-spec drop_delivery_budget(pid()) -> ok.
drop_delivery_budget(Pid) when is_pid(Pid) ->
    case ets:whereis(?DELIVERY_BUDGET_TAB) of
        undefined ->
            ok;
        _ ->
            _ = ets:delete(?DELIVERY_BUDGET_TAB, Pid),
            _ = ets:delete(?DELIVERY_BUDGET_TAB, {ewma, Pid}),
            _ = ets:delete(?DELIVERY_BUDGET_TAB, {deferred_total, Pid}),
            ok
    end.

%% @doc 预算聚合计数（调用数 oracle 观测面；缺省 0）。
-spec delivery_budget_counters() -> #{atom() => non_neg_integer()}.
delivery_budget_counters() ->
    Keys = [
        bulk_delivered,
        bulk_deferred,
        direct_delivered,
        direct_deferred,
        notice_sent,
        notice_coalesced
    ],
    maps:from_list([{K, budget_counter_value(K)} || K <- Keys]).

budget_counter_value(Key) ->
    case ets:whereis(?DELIVERY_BUDGET_TAB) of
        undefined ->
            0;
        _ ->
            case ets:lookup(?DELIVERY_BUDGET_TAB, {c, Key}) of
                [{{c, Key}, V}] when is_integer(V) -> V;
                _ -> 0
            end
    end.

%% @doc 清空全部预算状态（测试隔离用；生产幂等无害）。
-spec reset_delivery_budget_state() -> ok.
reset_delivery_budget_state() ->
    case ets:whereis(?DELIVERY_BUDGET_TAB) of
        undefined ->
            ok;
        _ ->
            _ = ets:delete_all_objects(?DELIVERY_BUDGET_TAB),
            ok
    end.

%% 字节滞留估算：QLen x 帧尺寸 EWMA。EWMA 只增信息不参与投递正确性，
%% 并发写竞态仅造成估算轻微偏移（启发式门），可接受。
estimate_retained_bytes(Pid, QLen, FrameBytes) ->
    Key = {ewma, Pid},
    Prev =
        case ets:lookup(?DELIVERY_BUDGET_TAB, Key) of
            [{_, V}] when is_number(V) -> V;
            _ -> FrameBytes
        end,
    Ewma = Prev + ?BUDGET_EWMA_ALPHA * (FrameBytes - Prev),
    _ = ets:insert(?DELIVERY_BUDGET_TAB, {Key, Ewma}),
    trunc(QLen * Ewma).

bump_budget_counter(Key) ->
    _ = ets:update_counter(?DELIVERY_BUDGET_TAB, Key, 1, {Key, 0}),
    ok.

budget_max_count() -> budget_env(delivery_budget_max_count, ?DEFAULT_BUDGET_MAX_COUNT).
budget_max_bytes() -> budget_env(delivery_budget_max_bytes, ?DEFAULT_BUDGET_MAX_BYTES).
budget_reserve_count() ->
    budget_env(delivery_budget_reserve_count, ?DEFAULT_BUDGET_RESERVE_COUNT).
budget_reserve_bytes() ->
    budget_env(delivery_budget_reserve_bytes, ?DEFAULT_BUDGET_RESERVE_BYTES).

budget_env(Key, Default) ->
    case application:get_env(imboy, Key, Default) of
        V when is_integer(V), V >= 0 ->
            V;
        _ ->
            Default
    end.

bulk_cap_count() -> erlang:max(0, budget_max_count() - budget_reserve_count()).
bulk_cap_bytes() -> erlang:max(0, budget_max_bytes() - budget_reserve_bytes()).

ensure_budget_table() ->
    case ets:whereis(?DELIVERY_BUDGET_TAB) of
        undefined ->
            try
                _ = ets:new(?DELIVERY_BUDGET_TAB, [
                    set,
                    public,
                    named_table,
                    {keypos, 1},
                    {write_concurrency, true},
                    {read_concurrency, true}
                ]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.
