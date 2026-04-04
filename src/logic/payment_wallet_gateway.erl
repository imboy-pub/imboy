-module(payment_wallet_gateway).
-behaviour(payment_gateway).
-export([pay/3, refund/2]).
-include("log.hrl").

%% 从钱包扣款支付
%% Opts 中必须包含 uid（用户 ID）和 amount（金额，分）
pay(OrderNo, Amount, Opts) ->
    Uid = maps:get(uid, Opts, 0),
    case Uid =:= 0 orelse Amount =< 0 of
        true ->
            {error, <<"支付参数错误"/utf8>>};
        false ->
            case wallet_repo:find_by_uid(Uid) of
                Wallet when is_map(Wallet), map_size(Wallet) > 0 ->
                    WalletId = maps:get(<<"id">>, Wallet, 0),
                    PayNo = iolist_to_binary([<<"WPY_">>, OrderNo]),
                    TxData = #{
                        <<"wallet_id">> => WalletId,
                        <<"user_id">> => Uid,
                        <<"amount">> => -Amount,
                        <<"tx_type">> => 2,
                        <<"remark">> => <<"频道订阅"/utf8>>,
                        <<"status">> => 1
                    },
                    case wallet_repo:atomic_balance_change(-Amount, Uid, TxData, PayNo) of
                        {ok, _NewBalance} ->
                            {ok, PayNo};
                        {rollback, insufficient_balance} ->
                            {error, <<"钱包余额不足"/utf8>>};
                        {error, Reason} ->
                            ?ERROR_LOG(["wallet pay error: ", Reason]),
                            {error, <<"支付失败"/utf8>>}
                    end;
                _ ->
                    {error, <<"钱包不存在，请先充值"/utf8>>}
            end
    end.

refund(PaymentNo, _Amount) ->
    case wallet_repo:find_transaction_by_ref(PaymentNo) of
        Tx when is_map(Tx), map_size(Tx) > 0 ->
            Uid = maps:get(<<"user_id">>, Tx, 0),
            PaidAmount = abs(maps:get(<<"amount">>, Tx, 0)),
            case Uid > 0 andalso PaidAmount > 0 of
                false ->
                    {error, <<"退款参数错误"/utf8>>};
                true ->
                    case wallet_repo:find_by_uid(Uid) of
                        Wallet when is_map(Wallet), map_size(Wallet) > 0 ->
                            WalletId = maps:get(<<"id">>, Wallet, 0),
                            RefNo = iolist_to_binary([<<"WRF_">>, PaymentNo]),
                            TxData = #{
                                <<"wallet_id">> => WalletId,
                                <<"user_id">> => Uid,
                                <<"amount">> => PaidAmount,
                                <<"tx_type">> => 3,
                                <<"remark">> => <<"退款"/utf8>>,
                                <<"status">> => 1
                            },
                            case wallet_repo:atomic_balance_change(PaidAmount, Uid, TxData, RefNo) of
                                {ok, _NewBalance} ->
                                    ok;
                                {error, Reason} ->
                                    ?ERROR_LOG(["wallet refund error: ", Reason]),
                                    {error, <<"退款失败"/utf8>>}
                            end;
                        _ ->
                            {error, <<"钱包不存在"/utf8>>}
                    end
            end;
        _ ->
            {error, <<"原支付记录不存在"/utf8>>}
    end.
