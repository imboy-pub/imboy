-module(red_packet_logic).
%%%===================================================================
%%% @doc 红包业务逻辑层 / Red packet business logic
%%%===================================================================

-include("log.hrl").

-export([send/5, open/2, detail/2]).
%% ecron 入口（B-10）
-export([run_expire_refund/0, run_expire_refund/1]).

%% 单轮最多处理多少个过期红包。有上限是为了让一轮事务时间可预期；
%% 处理不完的下一轮继续（每小时一轮，正常量级远吃不满）。
-define(DEFAULT_EXPIRE_BATCH, 500).

%% @doc 发送红包（原子扣减钱包余额 + 创建红包）
-spec send(integer(), binary(), integer(), integer(), binary()) ->
    {ok, integer()} | {error, term()}.
send(SenderUid, Type, Amount, Count, Greeting) ->
    %% 校验参数 / Validate input
    case is_integer(Amount) andalso Amount >= 100 andalso is_integer(Count) andalso Count >= 1 of
        false ->
            {error, <<"红包参数不合法"/utf8>>};
        true ->
            %% 开启事务扣除发送者钱包
            Wallet = wallet_ds:ensure_wallet(SenderUid),
            case map_size(Wallet) =:= 0 of
                true ->
                    {error, <<"钱包不可用"/utf8>>};
                false ->
                    WalletId = maps:get(<<"id">>, Wallet),
                    TxData = #{
                        <<"wallet_id">> => WalletId,
                        <<"user_id">> => SenderUid,
                        <<"amount">> => -Amount,
                        % 发红包
                        <<"tx_type">> => 7,
                        <<"remark">> => <<"发红包"/utf8>>,
                        <<"status">> => 1
                    },
                    RefNo = gen_ref_no(),
                    case wallet_ds:atomic_balance_change(-Amount, SenderUid, TxData, RefNo) of
                        {ok, _} ->
                            %% 创建红包（有效期 24 小时：86400 秒）
                            ExpiresAt = elib_dt:second() + 86400,
                            case
                                red_packet_repo:create(
                                    SenderUid, Type, Amount, Count, Greeting, ExpiresAt
                                )
                            of
                                {ok, PacketId} ->
                                    {ok, PacketId};
                                {error, Reason} ->
                                    %% 极端情况下，钱包扣款成功但红包建单失败，需在此发起原路退款
                                    RefundRef = <<RefNo/binary, "_RF">>,
                                    TxDataRefund = TxData#{
                                        <<"amount">> => Amount,
                                        % 红包/转账退回
                                        <<"tx_type">> => 9,
                                        <<"remark">> => <<"发红包失败退款"/utf8>>
                                    },
                                    _ = wallet_ds:atomic_balance_change(
                                        Amount, SenderUid, TxDataRefund, RefundRef
                                    ),
                                    {error, Reason}
                            end;
                        %% atomic_balance_change 余额不足时抛 {rollback, insufficient_balance}
                        %% （wallet_repo.erl 内 throw），不是 {error, insufficient_balance}；
                        %% 旧写法这里恒不匹配 → 发红包余额不足时必现 case_clause 崩溃。
                        {rollback, insufficient_balance} ->
                            {error, <<"钱包余额不足"/utf8>>};
                        %% with_tx 对任意事务内 throw({rollback, _}) 均原样返回
                        %% {rollback, Reason}；缺此分支会 case_clause 崩溃（500）。
                        %% 事务已回滚、无资金变动，归一为可重试错误。
                        {rollback, _Reason} ->
                            {error, <<"发红包失败，请稍后再试"/utf8>>};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 抢红包
-spec open(integer(), integer()) -> {ok, integer()} | {error, term()}.
open(PacketId, ReceiverUid) ->
    %% 先确保领取者钱包存在：red_packet_repo:grab 在事务内用
    %% "UPDATE wallet ... WHERE user_id RETURNING balance, id" 给领取者入账，
    %% 若领取者从未开通钱包则命中 0 行 → 硬匹配 {ok, 1, _} badmatch → 事务崩溃回滚，
    %% 红包名额被占又领不到钱。ensure_wallet 幂等，与发红包(send)入账前置一致。
    _ = wallet_ds:ensure_wallet(ReceiverUid),
    case red_packet_repo:grab(PacketId, ReceiverUid) of
        {ok, GrabAmount} ->
            {ok, GrabAmount};
        {rollback, already_received} ->
            {error, <<"您已领过该红包"/utf8>>};
        {rollback, red_packet_unavailable} ->
            {error, <<"红包已被领完或已过期"/utf8>>};
        {rollback, not_found} ->
            {error, <<"红包不存在"/utf8>>};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 红包详情页（红包元数据 + 已领取明细列表）
%% SEC-03：仅发送者或已领取者可查看，防止任意登录用户凭红包 id
%% 越权读发送者/祝福语/金额/领取名单。
-spec detail(integer(), integer()) -> {ok, map()} | {error, term()}.
detail(PacketId, ViewerUid) ->
    Packet = red_packet_repo:find_by_id(PacketId),
    case map_size(Packet) =:= 0 of
        true ->
            {error, <<"红包不存在"/utf8>>};
        false ->
            SenderUid = maps:get(<<"sender_uid">>, Packet, 0),
            IsReceiver = red_packet_repo:find_receive_by_user(PacketId, ViewerUid),
            case ViewerUid =:= SenderUid orelse map_size(IsReceiver) > 0 of
                false ->
                    {error, <<"无权查看该红包详情"/utf8>>};
                true ->
                    Receivers = red_packet_repo:get_receivers(PacketId),
                    Payload = #{
                        <<"packet">> => Packet,
                        <<"receivers">> => Receivers
                    },
                    {ok, Payload}
            end
    end.

%% ===================================================================
%% B-10 ecron 入口：过期红包未领完的余额退回发送者
%% ===================================================================

%% @doc ecron 入口。恒 ok，任何异常记日志后跳过，绝不抛给调度器。
-spec run_expire_refund() -> ok.
run_expire_refund() ->
    Batch = config_ds:env(red_packet_expire_batch, ?DEFAULT_EXPIRE_BATCH),
    run_expire_refund(Batch).

-spec run_expire_refund(integer()) -> ok.
run_expire_refund(Batch0) ->
    Batch = max(1, to_int(Batch0)),
    try
        Rows = red_packet_repo:list_expired_active(Batch),
        %% 心跳：scanned 在"本轮 0 条"时不会产出序列，没有它分不清"没过期红包"和"job 死了"
        _ = elib_metric:increment(red_packet_expire_run_total, 1),
        _ = elib_metric:increment(red_packet_expire_scanned_total, length(Rows)),
        lists:foreach(fun refund_one/1, Rows),
        ok
    catch
        Class:Reason:St ->
            ?ERROR_LOG([red_packet_expire, run_failed, Class, Reason, St]),
            _ = elib_metric:increment(red_packet_expire_error_total, 1),
            ok
    end.

%% @doc 单个红包退款。失败只计数不中断后续 —— 一个坏行不能让整批停摆。
-spec refund_one(map()) -> ok.
refund_one(Row) ->
    PacketId = to_int(maps:get(<<"id">>, Row, 0)),
    Result =
        try red_packet_repo:expire_and_refund(PacketId) of
            {ok, _Amount} -> refunded;
            {rollback, already_settled} -> skipped;
            {rollback, Why} -> log_expire_failed(PacketId, Why);
            {error, Why} -> log_expire_failed(PacketId, Why)
        catch
            Class:Why:St -> log_expire_failed(PacketId, {Class, Why, St})
        end,
    _ = elib_metric:increment(red_packet_expire_total, 1, #{outcome => Result}),
    ok.

-spec log_expire_failed(integer(), term()) -> failed.
log_expire_failed(PacketId, Why) ->
    ?ERROR_LOG([red_packet_expire, refund_failed, PacketId, Why]),
    failed.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec to_int(term()) -> integer().
to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.

gen_ref_no() ->
    Id = elib_tsid:generate(wallet_transaction),
    <<"RPO_", (integer_to_binary(Id))/binary>>.
