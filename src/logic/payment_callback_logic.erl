-module(payment_callback_logic).
%%%===================================================================
%%% @doc 统一支付回调处理逻辑 / Unified payment webhook handler logic
%%%
%%% 回调来自第三方支付服务器（无 JWT），由 payment_callback_handler 解析
%%% 路径中的 :gateway 与原始报文后调用本模块。
%%%
%%% 处理流程：
%%%   1) 验签 payment_sign:verify(Gateway, RawBody, Headers)
%%%        - sandbox 直通；live 按网关真实验签（真实点为 TODO[live]）。
%%%   2) 幂等 —— 按 (gateway, gateway_payment_no) 查 payment_transaction：
%%%        - 已成功 -> 直接返回 {ok, already}（不重复入账）。
%%%        - 已存在但未成功 -> 复用其 trade_no 继续入账。
%%%   3) 入账（按 biz_type）：
%%%        - 1 充值：更新 recharge_order 为已支付 + wallet_ds:atomic_balance_change 加余额。
%%%        - 2 频道订单：channel_order_ds:pay + channel_ds:subscribe。
%%%   4) 写/更新 payment_transaction status=1（成功），落 gateway_payment_no/notify_data/paid_at。
%%%
%%% 双保险幂等：
%%%   - payment_transaction UNIQUE(gateway, gateway_payment_no)（回调级）。
%%%   - wallet_transaction.reference_no UNIQUE（入账级，reference_no=trade_no）。
%%%   两者任一命中重复都不会重复加钱。
%%%
%%% 回调 map（由 handler 透传，已解析）约定字段（不同网关由 handler 归一化）：
%%%   #{<<"gateway_payment_no">> => binary(),  %% 第三方支付单号（必填）
%%%     <<"biz_type">>           => 1|2,        %% 业务类型（必填）
%%%     <<"biz_order_no">>       => binary(),   %% 业务订单号 recharge_order/channel_order（必填）
%%%     <<"user_id">>            => integer(),  %% 用户 ID（充值入账必填）
%%%     <<"amount">>             => integer(),  %% 金额（分，充值入账必填）
%%%     <<"currency">>           => binary(),   %% 可选，默认 CNY
%%%     <<"trade_no">>           => binary(),   %% 可选，缺省自动生成
%%%     <<"raw">>                => binary()}   %% 原始报文（落 notify_data）
%%% @end
%%%===================================================================

-export([handle/3]).

-include("log.hrl").

%% biz_type
-define(BIZ_RECHARGE, 1).
-define(BIZ_CHANNEL_ORDER, 2).
%% payment_transaction.status
-define(TX_SUCCESS, 1).

%% @doc 回调主入口。
%% @param Gateway 网关标识（来自路径 :gateway）
%% @param Notify  归一化后的回调 map
%% @param Ctx     #{raw => RawBody, headers => Headers}
%% @return {ok, paid} | {ok, already} | {error, binary()}
-spec handle(binary(), map(), map()) -> {ok, paid | already} | {error, binary()}.
handle(Gateway, Notify, Ctx) ->
    RawBody = maps:get(raw, Ctx, <<>>),
    Headers = maps:get(headers, Ctx, #{}),
    %% 1) 验签
    case payment_sign:verify(Gateway, RawBody, Headers) of
        ok ->
            handle_verified(Gateway, Notify, RawBody);
        {error, Reason} ->
            ?WARN_LOG([payment_callback, sign_failed, Gateway, Reason]),
            {error, <<"验签失败"/utf8>>}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec handle_verified(binary(), map(), binary()) ->
    {ok, paid | already} | {error, binary()}.
handle_verified(Gateway, Notify, RawBody) ->
    case extract(Gateway, Notify) of
        {ok, Fields} ->
            dispatch_idempotent(Gateway, Fields, RawBody);
        {error, Msg} ->
            ?WARN_LOG([payment_callback, bad_params, Gateway, Msg]),
            {error, Msg}
    end.

%% @doc 从回调 map 提取并校验必填字段
-spec extract(binary(), map()) -> {ok, map()} | {error, binary()}.
extract(Gateway, Notify) ->
    GwPayNo = to_bin(maps:get(<<"gateway_payment_no">>, Notify, <<>>)),
    BizType = to_int(maps:get(<<"biz_type">>, Notify, 0)),
    BizOrderNo = to_bin(maps:get(<<"biz_order_no">>, Notify, <<>>)),
    case {GwPayNo, BizType, BizOrderNo} of
        {<<>>, _, _} ->
            {error, <<"缺少支付单号"/utf8>>};
        {_, BT, _} when BT =/= ?BIZ_RECHARGE, BT =/= ?BIZ_CHANNEL_ORDER ->
            {error, <<"不支持的业务类型"/utf8>>};
        {_, _, <<>>} ->
            {error, <<"缺少业务订单号"/utf8>>};
        _ ->
            TradeNo = resolve_trade_no(Gateway, GwPayNo, Notify),
            {ok, #{
                gateway => Gateway,
                gateway_payment_no => GwPayNo,
                biz_type => BizType,
                biz_order_no => BizOrderNo,
                user_id => to_int(maps:get(<<"user_id">>, Notify, 0)),
                amount => to_int(maps:get(<<"amount">>, Notify, 0)),
                currency => to_bin(maps:get(<<"currency">>, Notify, <<"CNY">>)),
                trade_no => TradeNo
            }}
    end.

%% @doc 幂等分派：先按 (gateway, gateway_payment_no) 探测既有流水
-spec dispatch_idempotent(binary(), map(), binary()) ->
    {ok, paid | already} | {error, binary()}.
dispatch_idempotent(Gateway, Fields, RawBody) ->
    GwPayNo = maps:get(gateway_payment_no, Fields),
    Existing = payment_transaction_ds:find_by_gateway_no(Gateway, GwPayNo),
    case map_size(Existing) =:= 0 of
        true ->
            %% 首次回调：先写 pending 流水（命中唯一约束=并发重复回调），再入账
            create_then_credit(Fields, RawBody);
        false ->
            case maps:get(<<"status">>, Existing, 0) of
                ?TX_SUCCESS ->
                    %% 已成功处理，重复回调直接成功（不重复入账）
                    {ok, already};
                _ ->
                    %% 既有未成功流水：复用其 trade_no 继续入账
                    TradeNo = maps:get(<<"trade_no">>, Existing, maps:get(trade_no, Fields)),
                    credit_and_finalize(Fields#{trade_no => TradeNo}, RawBody)
            end
    end.

%% @doc 写入 pending 流水后入账；唯一约束冲突视为并发重复回调（幂等成功）
-spec create_then_credit(map(), binary()) -> {ok, paid | already} | {error, binary()}.
create_then_credit(Fields, RawBody) ->
    TxData = #{
        <<"trade_no">> => maps:get(trade_no, Fields),
        <<"biz_type">> => maps:get(biz_type, Fields),
        <<"biz_order_no">> => maps:get(biz_order_no, Fields),
        <<"user_id">> => maps:get(user_id, Fields),
        <<"gateway">> => maps:get(gateway, Fields),
        <<"gateway_payment_no">> => maps:get(gateway_payment_no, Fields),
        <<"amount">> => maps:get(amount, Fields),
        <<"currency">> => maps:get(currency, Fields),
        <<"status">> => 0
    },
    case payment_transaction_ds:create(TxData) of
        {ok, _Id} ->
            credit_and_finalize(Fields, RawBody);
        {error, duplicate} ->
            %% 并发重复回调：另一请求已抢先建流水，按幂等成功返回
            {ok, already};
        {error, Reason} ->
            ?ERROR_LOG([payment_callback, tx_create_failed, Reason]),
            {error, <<"支付流水写入失败"/utf8>>}
    end.

%% @doc 按业务类型入账，成功后把 payment_transaction 标记为成功
-spec credit_and_finalize(map(), binary()) -> {ok, paid} | {error, binary()}.
credit_and_finalize(Fields, RawBody) ->
    case credit(Fields) of
        ok ->
            finalize_success(Fields, RawBody);
        {error, Msg} ->
            {error, Msg}
    end.

%% @doc 入账分派
-spec credit(map()) -> ok | {error, binary()}.
credit(#{biz_type := ?BIZ_RECHARGE} = Fields) ->
    credit_recharge(Fields);
credit(#{biz_type := ?BIZ_CHANNEL_ORDER} = Fields) ->
    credit_channel_order(Fields);
credit(_) ->
    {error, <<"不支持的业务类型"/utf8>>}.

%% @doc 充值入账：单事务(订单状态+钱包余额+流水)原子完成，幂等。
%% 取代此前 mark_recharge_order_paid + do_wallet_credit 两步非原子写法
%% （曾有"订单已支付但钱没进"风险）。reference_no 统一为 RCH_<OrderNo>，
%% 与线D沙箱即时入账一致，两条入账路径互相幂等。
-spec credit_recharge(map()) -> ok | {error, binary()}.
credit_recharge(Fields) ->
    Uid = maps:get(user_id, Fields),
    Amount = maps:get(amount, Fields),
    case Uid > 0 andalso Amount > 0 of
        false ->
            {error, <<"充值入账参数无效"/utf8>>};
        true ->
            OrderNo = maps:get(biz_order_no, Fields),
            GwPayNo = maps:get(gateway_payment_no, Fields),
            _ = wallet_ds:ensure_wallet(Uid),
            case recharge_order_ds:credit_in_tx(OrderNo, GwPayNo, Uid, Amount) of
                {ok, _NewBalance} -> ok;
                {rollback, already_credited} -> ok;
                {rollback, order_not_payable} -> {error, <<"充值订单状态异常"/utf8>>};
                {rollback, wallet_not_found} -> {error, <<"钱包不可用"/utf8>>};
                {error, Reason} -> classify_credit_error(Reason)
            end
    end.

%% @doc 钱包入账错误归类：唯一冲突=已入账（幂等成功），其余=失败
-spec classify_credit_error(term()) -> ok | {error, binary()}.
classify_credit_error(Reason) ->
    case is_unique_violation(Reason) of
        true ->
            %% reference_no 已存在 => 此 trade_no 已入账，幂等成功
            ok;
        false ->
            ?ERROR_LOG([payment_callback, wallet_credit_failed, Reason]),
            {error, <<"入账失败"/utf8>>}
    end.

%% @doc 频道订单入账：标记订单已支付 + 订阅频道
-spec credit_channel_order(map()) -> ok | {error, binary()}.
credit_channel_order(Fields) ->
    OrderNo = maps:get(biz_order_no, Fields),
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
            ChannelId = to_int(maps:get(<<"channel_id">>, Order, 0)),
            Uid = to_int(maps:get(<<"user_id">>, Order, 0)),
            Status = to_int(maps:get(<<"status">>, Order, 0)),
            case Status of
                1 ->
                    %% 订单已支付：仅确保订阅生效（幂等补偿）
                    ensure_subscribed(ChannelId, Uid);
                _ ->
                    PaymentData = #{
                        payment_no => maps:get(gateway_payment_no, Fields),
                        payment_method => maps:get(gateway, Fields)
                    },
                    case channel_order_ds:pay(OrderNo, PaymentData) of
                        ok ->
                            ensure_subscribed(ChannelId, Uid);
                        {error, not_found_or_expired} ->
                            %% 并发/过期竞态：复查最终状态
                            recheck_channel_order(OrderNo);
                        {error, Reason} ->
                            ?ERROR_LOG([payment_callback, channel_pay_failed, Reason]),
                            {error, <<"频道订单入账失败"/utf8>>}
                    end
            end;
        _ ->
            {error, <<"频道订单不存在"/utf8>>}
    end.

-spec ensure_subscribed(integer(), integer()) -> ok | {error, binary()}.
ensure_subscribed(ChannelId, Uid) when ChannelId > 0, Uid > 0 ->
    case channel_ds:subscribe(ChannelId, Uid) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG([payment_callback, subscribe_failed, ChannelId, Uid, Reason]),
            {error, <<"频道订阅失败"/utf8>>}
    end;
ensure_subscribed(_, _) ->
    {error, <<"频道订单数据异常"/utf8>>}.

-spec recheck_channel_order(binary()) -> ok | {error, binary()}.
recheck_channel_order(OrderNo) ->
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} ->
            case to_int(maps:get(<<"status">>, Order, 0)) of
                1 ->
                    ensure_subscribed(
                        to_int(maps:get(<<"channel_id">>, Order, 0)),
                        to_int(maps:get(<<"user_id">>, Order, 0))
                    );
                _ ->
                    {error, <<"频道订单不存在或已过期"/utf8>>}
            end;
        _ ->
            {error, <<"频道订单不存在或已过期"/utf8>>}
    end.

%% @doc 入账成功后把支付流水标记为成功，落 notify_data/paid_at
-spec finalize_success(map(), binary()) -> {ok, paid} | {error, binary()}.
finalize_success(Fields, RawBody) ->
    TradeNo = maps:get(trade_no, Fields),
    Extra = #{
        <<"gateway_payment_no">> => maps:get(gateway_payment_no, Fields),
        <<"notify_data">> => notify_data_json(RawBody)
    },
    case payment_transaction_ds:mark_success(TradeNo, Extra) of
        {ok, _} ->
            {ok, paid};
        {error, Reason} ->
            %% 钱已入账但流水状态更新失败：记 error，返回成功避免网关重推导致重复入账
            %% （入账本身已幂等，重推也安全；此处倾向返回 paid 让网关停推）
            ?ERROR_LOG([payment_callback, finalize_failed, TradeNo, Reason]),
            {ok, paid}
    end.

%% @doc 把原始报文包装为可落 jsonb 的 JSON 文本
-spec notify_data_json(binary()) -> binary().
notify_data_json(RawBody) when is_binary(RawBody), byte_size(RawBody) > 0 ->
    %% RawBody 可能本身是 JSON，也可能是 form 串；统一包一层保证是合法 jsonb
    jsone:encode(#{<<"raw">> => RawBody}, [native_utf8]);
notify_data_json(_) ->
    jsone:encode(#{<<"raw">> => <<>>}, [native_utf8]).

%% ===================================================================
%% Helpers
%% ===================================================================

%% @doc 生成/解析内部交易号：优先用回调带的 trade_no，否则按 gateway+支付单号派生
-spec resolve_trade_no(binary(), binary(), map()) -> binary().
resolve_trade_no(Gateway, GwPayNo, Notify) ->
    case to_bin(maps:get(<<"trade_no">>, Notify, <<>>)) of
        <<>> -> <<"PT_", Gateway/binary, "_", GwPayNo/binary>>;
        Tn -> Tn
    end.

-spec is_unique_violation(term()) -> boolean().
is_unique_violation({pgsql_error, #{code := <<"23505">>}}) -> true;
is_unique_violation({error, {pgsql_error, #{code := <<"23505">>}}}) -> true;
is_unique_violation({error, error, _, unique_violation, _, _}) -> true;
is_unique_violation(_) -> false.

-spec to_bin(term()) -> binary().
to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) when is_list(V) -> list_to_binary(V);
to_bin(_) -> <<>>.

-spec to_int(term()) -> integer().
to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    case catch binary_to_integer(V) of
        I when is_integer(I) -> I;
        _ -> 0
    end;
to_int(V) when is_list(V) ->
    case catch list_to_integer(V) of
        I when is_integer(I) -> I;
        _ -> 0
    end;
to_int(_) ->
    0.
