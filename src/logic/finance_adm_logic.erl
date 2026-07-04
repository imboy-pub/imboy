-module(finance_adm_logic).
%%%===================================================================
%%% @doc 运营财务查询业务逻辑 / Admin finance query logic
%%%
%%% 供管理后台 /adm/finance/* 端点使用，提供跨用户的运营查询：
%%%   - 钱包分页列表 / 某用户钱包流水
%%%   - 充值订单分页列表
%%%   - 支付流水分页列表（对账）
%%%
%%% 与 /v1/* 用户态查询的区别：用户态是单用户视角，本模块是跨用户运营视角。
%%% billing 套餐/订阅/账单的运营查询复用 billing_logic（不在本模块）。
%%%
%%% 金额单位全程「分」(bigint) 透传不改，前端负责分→元显示。
%%% TSID 字段（id/user_id 等）经 elib_id:tsid_keys_to_bin/2 转字符串，避免 JS 精度丢失。
%%% @end
%%%===================================================================

-export([list_wallets/3]).
-export([list_wallet_transactions/3]).
-export([list_recharge_orders/3]).
-export([list_payment_transactions/3]).
-export([list_withdrawals/3]).
-export([complete_withdrawal/1]).
-export([reject_withdrawal/1]).
-export([refund_recharge_order/1]).
-export([refund_payment_transaction/1]).
-export([freeze_wallet/2, unfreeze_wallet/2]).

-include("log.hrl").

%% 支付流水查询列（与 payment_transaction_repo 对齐）
-define(PAYMENT_TX_COLUMNS,
    <<"id, trade_no, biz_type, biz_order_no, user_id, gateway, ",
        "gateway_payment_no, amount, currency, status, paid_at, created_at, updated_at">>
).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 钱包分页列表（运营）
%% @param Filter map，可选 user_id / status（等值筛选）
%% @return {ok, Payload} list 中 id/user_id 已转字符串 | {error, term()}
-spec list_wallets(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list_wallets(Filter, Page, Size) ->
    WhereMap = build_where(Filter, [user_id, status]),
    case wallet_ds:page(WhereMap, Page, Size) of
        {ok, Payload} ->
            {ok, normalize_payload(Payload, [<<"id">>, <<"user_id">>])};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 某用户钱包流水分页（运营）
%% @param Uid 用户 id（整数，>0）
%% @return {ok, Payload} list 中 id/wallet_id/user_id 已转字符串 | {error, term()}
-spec list_wallet_transactions(integer(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
list_wallet_transactions(Uid, Page, Size) ->
    %% wallet_ds:page_transactions/3 成功类型恒为 {ok, Payload}（dialyzer 推断 ok-only），
    %% 故此处直接解包返回。
    {ok, Payload} = wallet_ds:page_transactions(Page, Size, Uid),
    {ok, normalize_payload(Payload, [<<"id">>, <<"wallet_id">>, <<"user_id">>])}.

%% @doc 充值订单分页列表（运营）
%% @param Filter map，可选 status / payment_method / user_id / order_no（等值筛选）
%% @return {ok, Payload} list 中 id/user_id 已转字符串 | {error, term()}
-spec list_recharge_orders(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list_recharge_orders(Filter, Page, Size) ->
    WhereMap = build_where(Filter, [status, payment_method, user_id, order_no]),
    case recharge_order_ds:page(WhereMap, Page, Size) of
        {ok, Payload} ->
            {ok, normalize_payload(Payload, [<<"id">>, <<"user_id">>])};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 支付流水分页列表（对账，运营）
%% @param Filter map，可选 gateway / biz_type / status / user_id（等值筛选）
%% @return {ok, Payload} list 中 id/user_id 已转字符串 | {error, term()}
-spec list_payment_transactions(map(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
list_payment_transactions(Filter, Page, Size) ->
    WhereMap = build_where(Filter, [gateway, biz_type, status, user_id, trade_no]),
    case payment_transaction_ds:page(?PAYMENT_TX_COLUMNS, WhereMap, <<"id desc">>, Page, Size) of
        {ok, Payload} ->
            {ok, normalize_payload(Payload, [<<"id">>, <<"user_id">>])};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 提现流水分页（运营，跨用户，tx_type=10）
%% @param Filter map，可选 user_id / status
-spec list_withdrawals(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
list_withdrawals(Filter, Page, Size) ->
    WhereMap = build_where(Filter, [user_id, status]),
    case wallet_ds:page_withdrawals(WhereMap, Page, Size) of
        {ok, Payload} ->
            {ok, normalize_payload(Payload, [<<"id">>, <<"wallet_id">>, <<"user_id">>])};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 标记提现完成（admin 已手动转账后调用）
%% 仅操作 status=0 的待处理记录，防止重复操作。
-spec complete_withdrawal(integer()) -> {ok, non_neg_integer()} | {error, term()}.
complete_withdrawal(TxId) ->
    wallet_ds:update_tx_status(TxId, 1).

%% @doc 拒绝提现并原子退款：status=2 + 余额还原 + 写退款流水
-spec reject_withdrawal(integer()) -> {ok, 0 | 1} | {error, term()}.
reject_withdrawal(TxId) ->
    wallet_ds:reject_and_refund(TxId).

%% @doc 充值订单退款（管理端）：单事务原路扣回钱包可用余额 + 订单置退款态 + 退款流水。
%% 幂等：底层订单状态 CAS(仅已支付可退) + 退款流水 refno UNIQUE 双保险；已退款直接拒绝。
%% @param OrderNo 充值订单号
%% @returns {ok, NewBalance :: integer()} | {error, binary()}
-spec refund_recharge_order(binary()) -> {ok, integer()} | {error, binary()}.
refund_recharge_order(OrderNo) ->
    case recharge_order_ds:refund_in_tx(OrderNo) of
        {ok, NewBalance} when is_integer(NewBalance) ->
            {ok, NewBalance};
        {rollback, already_refunded} ->
            {error, <<"该充值订单已退款，请勿重复操作"/utf8>>};
        {rollback, order_not_refundable} ->
            {error, <<"订单状态不允许退款（仅「已支付」订单可退）"/utf8>>};
        {rollback, insufficient_available} ->
            {error, <<"用户可用余额不足（冻结部分不可退），无法退款"/utf8>>};
        {rollback, wallet_not_found} ->
            {error, <<"用户钱包不存在，无法退款"/utf8>>};
        {rollback, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 支付流水退款（管理端，对账层「原路退回」）：调网关退款 + 幂等标记流水为已退款(3)。
%%
%% 资金安全边界（保守设计，避免重复退款）：
%%   - biz_type=1 充值：钱包入账的权威反向操作在「充值订单退款」，此处拒绝，防钱包重复退。
%%   - biz_type=2 频道订单：内部反向(退订阅+网关退款)在 /adm/channel/order/refund，此处拒绝。
%%   - 其它(biz_type=3 SaaS 账单等)：无内部钱包联动，本端点做网关原路退回 + 标记，是唯一资金动作。
%% 幂等：先查状态(仅成功可退，已退款拒绝) + mark_refunded 的 status=1->3 CAS。
%% @param TradeNo 内部交易号
%% @returns {ok, refunded} | {error, binary()}
-spec refund_payment_transaction(binary()) -> {ok, refunded} | {error, binary()}.
refund_payment_transaction(TradeNo) ->
    Row = payment_transaction_ds:find_by_trade_no(TradeNo),
    case map_size(Row) =:= 0 of
        true ->
            {error, <<"支付流水不存在"/utf8>>};
        false ->
            Status = maps:get(<<"status">>, Row, -1),
            case Status of
                3 -> {error, <<"该支付流水已退款，请勿重复操作"/utf8>>};
                1 -> refund_payment_by_biz(Row, TradeNo, maps:get(<<"biz_type">>, Row, 0));
                _ -> {error, <<"仅「支付成功」的流水可退款"/utf8>>}
            end
    end.

%% @doc 按业务类型分流支付流水退款：充值/频道订单走各自专用退款入口，防重复退款。
-spec refund_payment_by_biz(map(), binary(), integer()) -> {ok, refunded} | {error, binary()}.
refund_payment_by_biz(_Row, _TradeNo, 1) ->
    {error, <<"充值类支付请使用「充值订单退款」（原路退回钱包），避免重复退款"/utf8>>};
refund_payment_by_biz(_Row, _TradeNo, 2) ->
    {error, <<"频道订单请使用「频道订单退款」入口（含退订阅），避免重复退款"/utf8>>};
refund_payment_by_biz(Row, TradeNo, _BizType) ->
    Gateway = maps:get(<<"gateway">>, Row, <<>>),
    GwPayNo = maps:get(<<"gateway_payment_no">>, Row, <<>>),
    Amount = maps:get(<<"amount">>, Row, 0),
    case GwPayNo of
        <<>> ->
            {error, <<"该流水缺少网关支付单号，无法原路退回"/utf8>>};
        _ ->
            case payment_gateway:refund(Gateway, GwPayNo, Amount) of
                ok ->
                    case payment_transaction_ds:mark_refunded(TradeNo) of
                        {ok, 1} -> {ok, refunded};
                        {ok, 0} -> {error, <<"支付流水状态已变更（可能已退款）"/utf8>>};
                        {error, R} -> {error, elib_cnv:safe_to_binary(R)}
                    end;
                {error, Msg} when is_binary(Msg) ->
                    {error, Msg};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @doc 冻结钱包资金（管理端）：把 Amount（分）从可用余额移入冻结额。
%% @returns ok | {error, binary()}
-spec freeze_wallet(integer(), integer()) -> ok | {error, binary()}.
freeze_wallet(Uid, Amount) ->
    case wallet_ds:freeze(Uid, Amount) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, <<"冻结失败：钱包不存在、已停用或可用余额不足"/utf8>>};
        {error, R} -> {error, elib_cnv:safe_to_binary(R)}
    end.

%% @doc 解冻钱包资金（管理端）：把 Amount（分）从冻结额移回可用余额。
%% @returns ok | {error, binary()}
-spec unfreeze_wallet(integer(), integer()) -> ok | {error, binary()}.
unfreeze_wallet(Uid, Amount) ->
    case wallet_ds:unfreeze(Uid, Amount) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, <<"解冻失败：钱包不存在、已停用或冻结额不足"/utf8>>};
        {error, R} -> {error, elib_cnv:safe_to_binary(R)}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 从 Filter 中按白名单提取非 undefined 字段，组装等值 WhereMap。
%% 仅保留白名单内且有值的键，避免任意筛选注入与无意义条件。
-spec build_where(map(), [atom()]) -> map().
build_where(Filter, AllowedKeys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case maps:get(Key, Filter, undefined) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        #{},
        AllowedKeys
    ).

%% @doc 规范化分页 payload：list 中各条目的指定 TSID 键转字符串。
-spec normalize_payload(map(), [binary()]) -> map().
normalize_payload(Payload, TsidKeys) ->
    List = maps:get(list, Payload, []),
    List2 = [elib_id:tsid_keys_to_bin(Item, TsidKeys) || Item <- List],
    Payload#{list => List2}.
