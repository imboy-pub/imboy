-module(msg_ack_logic).
%%%
% msg_ack_logic 是消息确认处理逻辑模块
% 统一处理 C2C、C2G、S2C、C2S 的客户端确认 (CLIENT_ACK)
%%%

-include("log.hrl").

-export([client_ack/4]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 统一的客户端 ACK 处理
%% @param Type 消息类型 (<<"c2c">>, <<"c2g">>, <<"s2c">>, <<"c2s">>)
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @param DID 设备ID
-spec client_ack(binary(), binary(), integer(), binary()) -> ok.
client_ack(Type, MsgId, CurrentUid, DID) ->
    ok = ?DEBUG_LOG({unified_ack, Type, MsgId, CurrentUid, DID}),

    % 根据类型执行相应的 ACK 处理 - 使用 DS 层接口
    % 【P0-1】C2C/S2C 按设备送达：DID 有效时只标记该设备已确认，
    % 主行等全部活跃设备确认后才删，避免"一端 ACK、另一端离线永丢"。
    % C2G timeline 仍为 per-uid 标记（V7 多端未读串扰另行立项）。
    AckResult =
        case Type of
            <<"c2c">> -> msg_operation_ds:ack_c2c_msg(MsgId, CurrentUid, DID);
            <<"c2g">> -> msg_operation_ds:ack_c2g_timeline(MsgId, CurrentUid);
            <<"s2c">> -> msg_operation_ds:ack_s2c_msg(MsgId, CurrentUid, DID);
            <<"c2s">> -> msg_operation_ds:ack_c2s_msg(MsgId, CurrentUid);
            _ -> ok = ?ERROR_LOG({unknown_msg_type_for_ack, Type})
        end,

    %% 消息投递确认计数
    %% 【MSG-P2-1】{ok, 0} = 重复 ACK（送达标记已存在），不自增防指标虚高；
    %% legacy/c2g/c2s 路径返回 ok，维持原计数行为
    case AckResult of
        {ok, 0} ->
            ok;
        _ ->
            elib_metric:increment(msg_delivered_total),
            record_deliver_duration(MsgId)
    end,

    %% 【P0-2】staging 生命周期完全交给 msg_store_worker：仅当 worker do_write
    %% 成功后才 unstage（见 msg_store_worker.erl:167）。此处不得提前 unstage——
    %% 否则接收方 ACK 快于 worker 落库时会把 staging 行提前清除，claim_pending 的
    %% processed_at IS NULL 跳过该行 → 消息永不落正式表（C2G 尤重：首个在线成员
    %% ACK 快过 worker 则整条群消息不落 msg_c2g，其余离线成员全丢）。
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc B-27：记录投递延迟（秒），供 imboy_msg_deliver_duration_seconds_bucket
%% 的 p50/p95/p99 面板消费。
%%
%% 时间戳直接从 MsgId 的 TSID 里取 —— 服务端生成消息 id 时就编码了毫秒时间戳，
%% 不需要另建一套"发送时刻"的埋点管道或查库。
%%
%% ponytail: 只覆盖 id 是服务端 TSID 的消息。客户端自带 id 的消息 parse 不出
%%   合法时间戳，直接跳过不记 —— 记一个假的比不记更糟（分位数会被污染）。
%%   上限：跨机时钟漂移会体现在这个值里；要精确到端到端就得客户端回传收到时刻。
-spec record_deliver_duration(binary()) -> ok.
record_deliver_duration(MsgId) ->
    case msg_created_ms(MsgId) of
        0 ->
            ok;
        CreatedMs ->
            DeltaMs = elib_dt:millisecond() - CreatedMs,
            %% 负数(时钟回拨)与超过 1 小时的一律丢弃：那不是投递延迟，
            %% 是脏数据，混进直方图会把 p99 拉到毫无意义的量级。
            case DeltaMs >= 0 andalso DeltaMs =< 3600000 of
                true -> elib_metric:record(imboy_msg_deliver_duration_seconds, DeltaMs / 1000);
                false -> ok
            end
    end.

-spec msg_created_ms(term()) -> integer().
msg_created_ms(MsgId) ->
    try
        Id =
            case MsgId of
                I when is_integer(I) -> I;
                B when is_binary(B) -> binary_to_integer(B)
            end,
        %% 用 timestamp/1 而非 parse/1：只要那一个字段，不必构造整个 map
        case elib_tsid:timestamp(Id) of
            Ts when is_integer(Ts), Ts > 0 -> Ts;
            _ -> 0
        end
    catch
        _:_ -> 0
    end.
