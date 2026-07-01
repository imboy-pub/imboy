-module(red_packet_logic).
%%%===================================================================
%%% @doc 红包业务逻辑层 / Red packet business logic
%%%===================================================================

-include("log.hrl").

-export([send/5, open/2, detail/1]).

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
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 抢红包
-spec open(integer(), integer()) -> {ok, integer()} | {error, term()}.
open(PacketId, ReceiverUid) ->
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
-spec detail(integer()) -> {ok, map()} | {error, term()}.
detail(PacketId) ->
    Packet = red_packet_repo:find_by_id(PacketId),
    case map_size(Packet) =:= 0 of
        true ->
            {error, <<"红包不存在"/utf8>>};
        false ->
            Receivers = red_packet_repo:get_receivers(PacketId),
            Payload = #{
                <<"packet">> => Packet,
                <<"receivers">> => Receivers
            },
            {ok, Payload}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

gen_ref_no() ->
    Id = elib_tsid:generate(wallet_transaction),
    <<"RPO_", (integer_to_binary(Id))/binary>>.
