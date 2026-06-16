-module(channel_logic_order).

-export([create_order/2]).
-export([pay_order/2]).
-export([get_my_orders/1]).
-export([get_order/2]).

-ifdef(TEST).
-export([to_gateway_amount/2, yuan_to_fen/1]).
-endif.
-export([refund_order/2]).

%% 生产环境允许的支付方式白名单（不含 mock）
-define(ALLOWED_PAYMENT_METHODS, [<<"wallet">>, <<"alipay">>, <<"wechat">>, <<"stripe">>]).
%% 非生产环境额外允许 mock 支付（用于测试/开发）
-define(DEV_ALLOWED_PAYMENT_METHODS, [<<"mock">> | ?ALLOWED_PAYMENT_METHODS]).

-spec create_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
create_order(Uid, ChannelIdBin) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_ds:find_by_id(ChannelId, <<"id,type,status">>) of
                {error, not_found} ->
                    {error, <<"频道不存在"/utf8>>};
                {error, Reason} when is_binary(Reason) ->
                    {error, Reason};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                Channel when is_map(Channel) ->
                    Type = maps:get(<<"type">>, Channel, 0),
                    Status = maps:get(<<"status">>, Channel, 0),
                    if
                        Status =/= 1 ->
                            {error, <<"频道已禁用或删除"/utf8>>};
                        Type =/= 2 ->
                            {error, <<"只有付费频道支持购买"/utf8>>};
                        true ->
                            do_create_order(ChannelId, Uid)
                    end;
                _ ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec do_create_order(integer(), integer()) -> {ok, map()} | {error, binary()}.
do_create_order(ChannelId, Uid) ->
    case channel_order_ds:has_purchased(ChannelId, Uid) of
        true ->
            {error, <<"您已购买此频道"/utf8>>};
        false ->
            case channel_order_ds:get_price(ChannelId) of
                {ok, Price} ->
                    Amount = maps:get(<<"price">>, Price, 0),
                    Currency = maps:get(<<"currency">>, Price, <<"CNY">>),
                    Data = #{
                        channel_id => ChannelId,
                        user_id => Uid,
                        amount => Amount,
                        currency => Currency
                    },
                    case channel_order_ds:create_order(Data) of
                        {ok, OrderNo} ->
                            case channel_order_ds:find_by_order_no(OrderNo) of
                                {ok, Order} when is_map(Order) ->
                                    {ok, order_transfer(Order)};
                                {ok, _} ->
                                    {error, <<"订单不存在"/utf8>>};
                                {error, Reason} ->
                                    {error, elib_cnv:safe_to_binary(Reason)};
                                Other ->
                                    {error, elib_cnv:safe_to_binary(Other)}
                            end;
                        {error, Reason} when is_binary(Reason) ->
                            {error, Reason};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)};
                        _Other ->
                            {error, elib_cnv:safe_to_binary(_Other)}
                    end;
                {error, not_found} ->
                    {error, <<"频道价格未配置"/utf8>>};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

%% @returns {ok, Envelope} | {error, binary()}
%% Envelope 为统一支付信封（binary key）：
%%   #{<<"payment_method">>, <<"payment_no">>, <<"pay_params">>}
%%   pay_params 透传网关支付参数：
%%     alipay #{<<"order_str">>}，wechat #{<<"prepay_id">>|<<"code_url">>}，
%%     wallet/mock #{}（即时入账，无第三方拉起）。
-spec pay_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
pay_order(Uid, OrderNo) ->
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
            OrderUserId = maps:get(<<"user_id">>, Order, 0),
            ChannelId = maps:get(<<"channel_id">>, Order, 0),
            case
                is_integer(OrderUserId) andalso
                    OrderUserId > 0 andalso
                    is_integer(ChannelId) andalso
                    ChannelId > 0
            of
                false ->
                    {error, <<"订单不存在"/utf8>>};
                true ->
                    if
                        OrderUserId =/= Uid ->
                            {error, <<"无权操作此订单"/utf8>>};
                        true ->
                            Method = maps:get(<<"payment_method">>, Order, <<"wallet">>),
                            case is_payment_method_allowed(Method) of
                                false ->
                                    {error, <<"不支持的支付方式"/utf8>>};
                                true ->
                                    Amount = maps:get(<<"amount">>, Order, 0),
                                    %% channel_order.amount 单位为「元」；按目标网关期望单位适配：
                                    %% wallet 网关期望元(内部换分)，第三方网关期望分(分=元×100)。
                                    PayOpts = #{
                                        uid => Uid,
                                        amount => to_gateway_amount(Method, Amount)
                                    },
                                    case
                                        normalize_pay_result(
                                            payment_gateway:pay(Method, OrderNo, PayOpts)
                                        )
                                    of
                                        {ok, PayNo, Extra} ->
                                            PaymentData = #{
                                                payment_no => PayNo,
                                                payment_method => Method
                                            },
                                            Envelope = #{
                                                <<"payment_method">> => Method,
                                                <<"payment_no">> => PayNo,
                                                <<"pay_params">> => Extra
                                            },
                                            case
                                                do_pay_order(
                                                    ChannelId, Uid, OrderNo, PaymentData, Order
                                                )
                                            of
                                                ok -> {ok, Envelope};
                                                {error, _} = Err -> Err
                                            end;
                                        {error, PayReason} ->
                                            {error, PayReason}
                                    end
                            end
                    end
            end;
        {ok, _} ->
            {error, <<"订单不存在"/utf8>>};
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        _Other ->
            {error, elib_cnv:safe_to_binary(_Other)}
    end.

%% @doc 归一网关返回：兼容 {ok, PayNo} 与 {ok, PayNo, Extra}，
%% 统一成 {ok, PayNo, Extra::map()}，错误原样透传（wallet/mock 无第三元组 → 空 map）。
-spec normalize_pay_result(term()) -> {ok, binary(), map()} | {error, term()}.
normalize_pay_result({ok, PayNo, Extra}) when is_map(Extra) ->
    {ok, PayNo, Extra};
normalize_pay_result({ok, PayNo}) ->
    {ok, PayNo, #{}};
normalize_pay_result({error, _} = Err) ->
    Err;
normalize_pay_result(Other) ->
    {error, elib_cnv:safe_to_binary(Other)}.

-spec do_pay_order(integer(), integer(), binary(), map(), map()) -> ok | {error, binary()}.
do_pay_order(ChannelId, Uid, OrderNo, PaymentData, Order) ->
    case maps:get(<<"status">>, Order, 0) of
        1 ->
            %% already paid — ensure subscription active
            case channel_ds:subscribe(ChannelId, Uid) of
                ok ->
                    channel_logic_notify:notify_order_paid(ChannelId, Uid);
                {error, _} ->
                    {error, <<"订单已支付"/utf8>>}
            end;
        _ ->
            case channel_order_ds:pay(OrderNo, PaymentData) of
                ok ->
                    case channel_ds:subscribe(ChannelId, Uid) of
                        ok ->
                            channel_logic_notify:notify_order_paid(ChannelId, Uid);
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, not_found_or_expired} ->
                    %% payment race: check final state, compensate if already paid
                    maybe_compensate_subscription(OrderNo);
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)}
            end
    end.

-spec maybe_compensate_subscription(binary()) -> ok | {error, binary()}.
maybe_compensate_subscription(OrderNo) ->
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} ->
            case maps:get(<<"status">>, Order, 0) of
                1 ->
                    ChannelId = maps:get(<<"channel_id">>, Order),
                    Uid = maps:get(<<"user_id">>, Order),
                    case channel_ds:subscribe(ChannelId, Uid) of
                        ok ->
                            channel_logic_notify:notify_order_paid(ChannelId, Uid);
                        {error, _} ->
                            {error, <<"订单已支付"/utf8>>}
                    end;
                _ ->
                    {error, <<"订单不存在或已过期"/utf8>>}
            end;
        {error, _} ->
            {error, <<"订单不存在或已过期"/utf8>>}
    end.

-spec get_my_orders(integer()) -> {ok, [map()]} | {error, binary()}.
get_my_orders(Uid) ->
    case channel_order_ds:list_by_user(Uid, 50) of
        {ok, Orders} when is_list(Orders) ->
            Orders2 = lists:map(fun order_transfer/1, [O || O <- Orders, is_map(O)]),
            {ok, Orders2};
        {ok, Other} ->
            {error, elib_cnv:safe_to_binary(Other)};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        _Other ->
            {error, elib_cnv:safe_to_binary(_Other)}
    end.

-spec get_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
get_order(Uid, OrderNo) ->
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
            OrderUserId = maps:get(<<"user_id">>, Order, 0),
            case is_integer(OrderUserId) andalso OrderUserId > 0 of
                false ->
                    {error, <<"订单不存在"/utf8>>};
                true ->
                    if
                        OrderUserId =/= Uid ->
                            {error, <<"无权查看此订单"/utf8>>};
                        true ->
                            {ok, order_transfer(Order)}
                    end
            end;
        {ok, _} ->
            {error, <<"订单不存在"/utf8>>};
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)};
        _Other ->
            {error, elib_cnv:safe_to_binary(_Other)}
    end.

-spec refund_order(integer(), binary()) -> ok | {error, binary()}.
refund_order(Uid, OrderNo) ->
    refund_order(Uid, OrderNo, <<"用户申请退款"/utf8>>).

-spec refund_order(integer(), binary(), binary()) -> ok | {error, binary()}.
refund_order(Uid, OrderNo, Reason0) ->
    Reason = normalize_refund_reason(Reason0),
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
            OrderUserId = maps:get(<<"user_id">>, Order, 0),
            ChannelId = maps:get(<<"channel_id">>, Order, 0),
            Status = maps:get(<<"status">>, Order, 0),
            case
                is_integer(OrderUserId) andalso
                    OrderUserId > 0 andalso
                    is_integer(ChannelId) andalso
                    ChannelId > 0
            of
                false ->
                    {error, <<"订单不存在"/utf8>>};
                true when OrderUserId =/= Uid ->
                    {error, <<"无权操作此订单"/utf8>>};
                true ->
                    %% 幂等：已退款订单直接返回提示，不重复退
                    case Status of
                        2 ->
                            {error, <<"订单已退款"/utf8>>};
                        1 ->
                            do_refund_order(ChannelId, Uid, OrderNo, Order, Reason);
                        _ ->
                            {error, <<"订单状态不允许退款"/utf8>>}
                    end
            end;
        {ok, _} ->
            {error, <<"订单不存在"/utf8>>};
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, Reason1} when is_binary(Reason1) ->
            {error, Reason1};
        {error, Reason1} ->
            {error, elib_cnv:safe_to_binary(Reason1)};
        _Other ->
            {error, elib_cnv:safe_to_binary(_Other)}
    end.

%% @doc 退款执行：网关退款 → 改订单状态为已退款(2) → 取消频道订阅
%% 金额单位：channel_order.amount 为元；按目标网关期望单位适配(wallet 元 / 第三方 分)。
-spec do_refund_order(integer(), integer(), binary(), map(), binary()) ->
    ok | {error, binary()}.
do_refund_order(ChannelId, Uid, OrderNo, Order, Reason) ->
    Method = maps:get(<<"payment_method">>, Order, <<"wallet">>),
    PaymentNo = maps:get(<<"payment_no">>, Order, <<>>),
    Amount = maps:get(<<"amount">>, Order, 0),
    case payment_gateway:refund(Method, PaymentNo, to_gateway_amount(Method, Amount)) of
        ok ->
            %% 网关退款成功后再更新订单状态（带 status=1 守卫，并发安全）
            case channel_order_ds:refund(OrderNo, Uid, Reason) of
                ok ->
                    %% 退款成功后取消该用户对频道的订阅（失败不回退退款，仅记录）
                    _ = channel_ds:unsubscribe(ChannelId, Uid),
                    ok;
                {error, not_found_or_not_paid} ->
                    %% 并发场景：状态已被其他流程改变，按已退款处理
                    {error, <<"订单已退款"/utf8>>};
                {error, RefReason} ->
                    {error, elib_cnv:safe_to_binary(RefReason)}
            end;
        {error, PayReason} when is_binary(PayReason) ->
            {error, PayReason};
        {error, PayReason} ->
            {error, elib_cnv:safe_to_binary(PayReason)}
    end.

%% @doc 按目标网关期望单位适配金额（修复 channel 第三方支付收款/退款 100 倍偏差）：
%%   - wallet 网关期望「元」(payment_wallet_gateway 内部 yuan_to_fen 换分)
%%   - 第三方网关(alipay/wechat/stripe)期望「分」
-spec to_gateway_amount(binary(), term()) -> term().
to_gateway_amount(<<"wallet">>, Amount) -> Amount;
to_gateway_amount(_Method, Amount) -> yuan_to_fen(Amount).

%% 元 → 分 安全换算（整数运算避浮点；Amount 为 numeric: binary/float/integer）
-spec yuan_to_fen(term()) -> integer().
yuan_to_fen(V) when is_integer(V) -> V * 100;
yuan_to_fen(V) when is_float(V) -> round(V * 100);
yuan_to_fen(V) when is_binary(V) -> yuan_to_fen_bin(V);
yuan_to_fen(V) when is_list(V) -> yuan_to_fen_bin(list_to_binary(V));
yuan_to_fen(_) -> 0.

-spec yuan_to_fen_bin(binary()) -> integer().
yuan_to_fen_bin(Bin) ->
    case binary:split(Bin, <<".">>) of
        [I] -> safe_int(I) * 100;
        [I, F] -> safe_int(I) * 100 + safe_int(norm_frac(F));
        _ -> 0
    end.

-spec norm_frac(binary()) -> binary().
norm_frac(F) ->
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

-spec normalize_refund_reason(term()) -> binary().
normalize_refund_reason(Reason) when is_binary(Reason), Reason =/= <<>> ->
    Reason;
normalize_refund_reason(_) ->
    <<"用户申请退款"/utf8>>.

-spec is_payment_method_allowed(binary()) -> boolean().
is_payment_method_allowed(Method) ->
    Env = config_ds:env(env, <<"local">>),
    EnvBin = ec_cnv:to_binary(Env),
    Allowed =
        case EnvBin of
            <<"pro">> -> ?ALLOWED_PAYMENT_METHODS;
            <<"prod">> -> ?ALLOWED_PAYMENT_METHODS;
            <<"production">> -> ?ALLOWED_PAYMENT_METHODS;
            _ -> ?DEV_ALLOWED_PAYMENT_METHODS
        end,
    lists:member(Method, Allowed).

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case catch ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

-spec order_transfer(map()) -> map().
order_transfer(Order) ->
    Order.
