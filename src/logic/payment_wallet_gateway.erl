-module(payment_wallet_gateway).
-compile([nowarn_deprecated_catch]).
-behaviour(payment_gateway).

%%%===================================================================
%%% @doc 钱包支付网关 —— 从用户钱包余额扣款完成订单支付
%%%
%%% 单位换算（关键）：订单 amount 单位为「元」(numeric)，钱包 balance
%%% 单位为「分」(bigint)。本模块负责元→分换算后扣减钱包余额。
%%%
%%% 幂等：
%%%   - 支付以 <<"WPY_", OrderNo>> 作为流水 reference_no，
%%%     reference_no 有 UNIQUE 约束（见迁移 000010），重复支付直接返回成功。
%%%   - 退款以 <<"R_WPY_", OrderNo>> 作为流水 reference_no，重复退款幂等。
%%% @end
%%%===================================================================

-export([pay/3, refund/2]).

-ifdef(TEST).
-export([yuan_to_fen/1]).
-endif.

%% @doc 钱包扣款支付
%% Amount: 订单金额（元；integer/float/binary/list 均可）
%% Opts:   #{uid => Uid, ...}
-spec pay(binary(), term(), map()) -> {ok, binary()} | {error, binary()}.
pay(OrderNo, Amount, Opts) ->
    case maps:get(uid, Opts, 0) of
        Uid when is_integer(Uid), Uid > 0 ->
            do_pay(OrderNo, Uid, yuan_to_fen(Amount));
        _ ->
            {error, <<"缺少用户标识"/utf8>>}
    end.

-spec do_pay(binary(), integer(), integer()) -> {ok, binary()} | {error, binary()}.
do_pay(_OrderNo, _Uid, Fen) when Fen =< 0 ->
    {error, <<"支付金额无效"/utf8>>};
do_pay(OrderNo, Uid, Fen) ->
    RefNo = <<"WPY_", OrderNo/binary>>,
    %% 幂等：同一订单号已扣款则直接返回成功
    case wallet_ds:find_transaction_by_ref(RefNo) of
        Tx when is_map(Tx), map_size(Tx) > 0 ->
            {ok, RefNo};
        _ ->
            charge(OrderNo, Uid, Fen, RefNo)
    end.

-spec charge(binary(), integer(), integer(), binary()) -> {ok, binary()} | {error, binary()}.
charge(_OrderNo, Uid, Fen, RefNo) ->
    Wallet = wallet_ds:find_by_uid(Uid),
    case map_size(Wallet) =:= 0 of
        true ->
            {error, <<"钱包不存在"/utf8>>};
        false ->
            WalletId = maps:get(<<"id">>, Wallet),
            TxData = #{
                <<"wallet_id">> => WalletId,
                <<"user_id">> => Uid,
                <<"amount">> => -Fen,
                <<"tx_type">> => 2,
                <<"remark">> => <<"频道订单支付"/utf8>>,
                <<"status">> => 1
            },
            case wallet_ds:atomic_balance_change(-Fen, Uid, TxData, RefNo) of
                {ok, _NewBalance} ->
                    {ok, RefNo};
                {rollback, insufficient_balance} ->
                    {error, <<"钱包余额不足"/utf8>>};
                {rollback, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @doc 钱包退款 —— 反向加回余额
%% PaymentNo: 原支付流水号 <<"WPY_", OrderNo>>
%% Amount:    退款金额（元）
-spec refund(binary(), term()) -> ok | {error, binary()}.
refund(PaymentNo, Amount) ->
    Fen = yuan_to_fen(Amount),
    refund_fen(PaymentNo, Fen).

-spec refund_fen(binary(), integer()) -> ok | {error, binary()}.
refund_fen(_PaymentNo, Fen) when Fen =< 0 ->
    {error, <<"退款金额无效"/utf8>>};
refund_fen(PaymentNo, Fen) ->
    RefundRef = <<"R_", PaymentNo/binary>>,
    %% 幂等：已退款直接返回成功
    case wallet_ds:find_transaction_by_ref(RefundRef) of
        Done when is_map(Done), map_size(Done) > 0 ->
            ok;
        _ ->
            do_refund(PaymentNo, Fen, RefundRef)
    end.

-spec do_refund(binary(), integer(), binary()) -> ok | {error, binary()}.
do_refund(PaymentNo, Fen, RefundRef) ->
    case wallet_ds:find_transaction_by_ref(PaymentNo) of
        Orig when is_map(Orig), map_size(Orig) > 0 ->
            Uid = maps:get(<<"user_id">>, Orig),
            WalletId = maps:get(<<"wallet_id">>, Orig),
            TxData = #{
                <<"wallet_id">> => WalletId,
                <<"user_id">> => Uid,
                <<"amount">> => Fen,
                <<"tx_type">> => 3,
                <<"remark">> => <<"订单退款"/utf8>>,
                <<"status">> => 1
            },
            case wallet_ds:atomic_balance_change(Fen, Uid, TxData, RefundRef) of
                {ok, _} -> ok;
                {rollback, Reason} -> {error, elib_cnv:safe_to_binary(Reason)};
                {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
            end;
        _ ->
            {error, <<"原支付流水不存在"/utf8>>}
    end.

%%%===================================================================
%%% 元 → 分 安全换算（避免浮点精度误差）
%%%===================================================================

-spec yuan_to_fen(term()) -> integer().
yuan_to_fen(V) when is_integer(V) -> V * 100;
yuan_to_fen(V) when is_float(V) -> round(V * 100);
yuan_to_fen(V) when is_binary(V) -> yuan_to_fen_bin(V);
yuan_to_fen(V) when is_list(V) -> yuan_to_fen_bin(list_to_binary(V));
yuan_to_fen(_) -> 0.

-spec yuan_to_fen_bin(binary()) -> integer().
yuan_to_fen_bin(Bin) ->
    case binary:split(Bin, <<".">>) of
        [IntPart] ->
            safe_int(IntPart) * 100;
        [IntPart, FracPart] ->
            safe_int(IntPart) * 100 + safe_int(normalize_frac(FracPart));
        _ ->
            0
    end.

%% 规整小数部分到恰好 2 位（截断，不四舍五入；金额建表已约束 2 位精度）
-spec normalize_frac(binary()) -> binary().
normalize_frac(F) ->
    case byte_size(F) of
        0 -> <<"00">>;
        1 -> <<F/binary, "0">>;
        _ -> binary:part(F, 0, 2)
    end.

-spec safe_int(binary()) -> integer().
safe_int(B) ->
    case catch binary_to_integer(B) of
        I when is_integer(I) -> I;
        _ -> 0
    end.
