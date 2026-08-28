-module(channel_logic_order).
-compile([nowarn_deprecated_catch]).

-export([create_order/2]).
-export([create_order/3]).
-export([pay_order/2]).
-export([cancel_order/2]).
-export([get_my_orders/1]).
-export([get_order/2]).
-export([payment_data_with_subscription/2]).

-ifdef(TEST).
-export([to_gateway_amount/2, yuan_to_fen/1]).
-endif.
-export([refund_order/2, refund_order/3]).
-export([admin_refund_order/2]).

%% 生产环境允许的支付方式白名单（不含 mock）
-define(ALLOWED_PAYMENT_METHODS, [<<"wallet">>, <<"alipay">>, <<"wechat">>, <<"stripe">>]).
%% 非生产环境额外允许 mock 支付（用于测试/开发）
-define(DEV_ALLOWED_PAYMENT_METHODS, [<<"mock">> | ?ALLOWED_PAYMENT_METHODS]).

%% 即时入账网关：钱包(余额同步扣减)与 mock(沙箱联调)，钱在 pay/3 返回时已真实划走。
%% 其余网关(alipay/wechat/stripe)的 pay/3 只创建**支付意图**，用户尚未付款，
%% 必须等 payment_callback_logic 收到回调才发货 —— 否则即"零元购"。
-define(INSTANT_SETTLE_METHODS, [<<"wallet">>, <<"mock">>]).

%% channel_order.status（迁移 00000003 列注释：0待支付 1已支付 2已退款 3已取消 4已过期）
-define(STATUS_PENDING, 0).
-define(STATUS_PAID, 1).
-define(STATUS_EXPIRED, 4).
-define(STATUS_CANCELLED, 3).

%% 兼容旧调用方：未指定支付方式时默认钱包支付；mock 只能由调用方显式选择。
-spec create_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
create_order(Uid, ChannelIdBin) ->
    create_order(Uid, ChannelIdBin, <<"wallet">>).

-spec create_order(integer(), binary(), binary()) -> {ok, map()} | {error, binary()}.
create_order(Uid, ChannelIdBin, PaymentMethod) ->
    case is_payment_method_allowed(PaymentMethod) of
        false ->
            {error, <<"不支持的支付方式"/utf8>>};
        true ->
            create_order_for_method(Uid, ChannelIdBin, PaymentMethod)
    end.

create_order_for_method(Uid, ChannelIdBin, PaymentMethod) ->
    ChannelId = decode_positive_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case
                channel_ds:find_by_id(ChannelId, <<"id,access_type,join_policy,visibility,status">>)
            of
                {error, not_found} ->
                    {error, <<"频道不存在"/utf8>>};
                {error, Reason} when is_binary(Reason) ->
                    {error, Reason};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason)};
                Channel when is_map(Channel) ->
                    AccessType = maps:get(<<"access_type">>, Channel, 0),
                    JoinPolicy = maps:get(<<"join_policy">>, Channel, 0),
                    Visibility = maps:get(<<"visibility">>, Channel, 0),
                    Status = maps:get(<<"status">>, Channel, 0),
                    if
                        Status =/= 1 ->
                            {error, <<"频道已禁用或删除"/utf8>>};
                        AccessType =/= 1 ->
                            {error, <<"只有付费频道支持购买"/utf8>>};
                        JoinPolicy =/= 3 ->
                            {error, <<"该频道暂不支持购买"/utf8>>};
                        Visibility =:= 1 ->
                            %% C4: private paid — 必须有有效邀请或购买链接上下文
                            case channel_invitation_ds:is_invited(ChannelId, Uid) of
                                true -> do_create_order(ChannelId, Uid, PaymentMethod);
                                false -> {error, <<"私有付费频道需要邀请才能购买"/utf8>>}
                            end;
                        true ->
                            do_create_order(ChannelId, Uid, PaymentMethod)
                    end;
                _Unexpected ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec do_create_order(integer(), integer(), binary()) -> {ok, map()} | {error, binary()}.
do_create_order(ChannelId, Uid, PaymentMethod) ->
    case channel_order_ds:has_purchased(ChannelId, Uid) of
        true ->
            {error, <<"您已购买此频道"/utf8>>};
        false ->
            case channel_order_ds:get_price(ChannelId) of
                {ok, Price} when is_map(Price) ->
                    Amount = maps:get(<<"price">>, Price, 0),
                    Currency = maps:get(<<"currency">>, Price, <<"CNY">>),
                    case normalize_subscription_type(maps:get(<<"subscription_type">>, Price, 1)) of
                        {error, invalid} ->
                            {error, <<"频道订阅类型无效"/utf8>>};
                        {ok, SubscriptionType} ->
                            case is_positive_amount(Amount) of
                                false ->
                                    {error, <<"频道价格无效"/utf8>>};
                                true ->
                                    Data = #{
                                        channel_id => ChannelId,
                                        user_id => Uid,
                                        amount => Amount,
                                        currency => Currency,
                                        payment_method => PaymentMethod,
                                        extra_data => #{
                                            <<"subscription_type">> => SubscriptionType
                                        }
                                    },
                                    case channel_order_ds:create_order(Data) of
                                        {ok, OrderNo} ->
                                            case channel_order_ds:find_by_order_no(OrderNo) of
                                                {ok, Order} when is_map(Order) ->
                                                    {ok, order_transfer(Order)};
                                                {ok, _InvalidOrder} ->
                                                    {error, <<"订单不存在"/utf8>>};
                                                {error, Reason} ->
                                                    {error, error_binary(Reason)};
                                                _UnexpectedOrder ->
                                                    {error, <<"订单数据异常"/utf8>>}
                                            end;
                                        {error, Reason} ->
                                            {error, error_binary(Reason)};
                                        _UnexpectedCreate ->
                                            {error, <<"订单创建结果异常"/utf8>>}
                                    end
                            end
                    end;
                {error, not_found} ->
                    {error, <<"频道价格未配置"/utf8>>};
                {error, Reason} ->
                    {error, error_binary(Reason)};
                _UnexpectedPrice ->
                    {error, <<"频道价格数据异常"/utf8>>}
            end;
        {error, Reason} ->
            {error, error_binary(Reason)};
        _UnexpectedPurchased ->
            {error, <<"购买状态数据异常"/utf8>>}
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
                                    case is_positive_amount(Amount) of
                                        false ->
                                            {error, <<"订单金额无效"/utf8>>};
                                        true ->
                                            Status = maps:get(<<"status">>, Order, 0),
                                            case Status of
                                                ?STATUS_PAID ->
                                                    {ok, paid_envelope(Order, Method)};
                                                ?STATUS_PENDING ->
                                                    pay_with_gateway(
                                                        OrderNo,
                                                        ChannelId,
                                                        Uid,
                                                        Method,
                                                        Amount,
                                                        Order
                                                    );
                                                _ ->
                                                    {error, <<"订单状态不允许支付"/utf8>>}
                                            end
                                    end
                            end
                    end
            end;
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, LookupReason} ->
            {error, error_binary(LookupReason)};
        {ok, _InvalidOrder} ->
            {error, <<"订单不存在"/utf8>>};
        _UnexpectedOrder ->
            {error, <<"订单数据异常"/utf8>>}
    end.

%% @doc 用户取消待支付订单；已支付订单必须走退款，避免绕过支付网关。
-spec cancel_order(integer(), binary()) -> ok | {error, binary()}.
cancel_order(Uid, OrderNo) ->
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
            OrderUserId = elib_cnv:safe_to_integer(maps:get(<<"user_id">>, Order, 0)),
            Status = elib_cnv:safe_to_integer(
                maps:get(<<"status">>, Order, ?STATUS_PENDING)
            ),
            case is_integer(OrderUserId) andalso OrderUserId > 0 of
                false ->
                    {error, <<"订单不存在"/utf8>>};
                true when OrderUserId =/= Uid ->
                    {error, <<"无权操作此订单"/utf8>>};
                true when Status =/= ?STATUS_PENDING ->
                    {error, <<"订单状态不允许取消"/utf8>>};
                true ->
                    case channel_order_ds:cancel(OrderNo) of
                        ok ->
                            ok;
                        {error, not_found_or_not_pending} ->
                            {error, <<"订单状态不允许取消"/utf8>>};
                        {error, Reason} ->
                            {error, error_binary(Reason)};
                        _UnexpectedCancel ->
                            {error, <<"订单取消结果异常"/utf8>>}
                    end
            end;
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, LookupReason} ->
            {error, error_binary(LookupReason)};
        {ok, _InvalidOrder} ->
            {error, <<"订单不存在"/utf8>>};
        _UnexpectedOrder ->
            {error, <<"订单数据异常"/utf8>>}
    end.

%% @doc 按网关结算模式决定是否就地发货。
%%   即时入账(wallet/mock)：钱已划走 → 标记已支付 + 订阅频道 + 通知。
%%   第三方(alipay/wechat/stripe)：仅创建支付意图 → 订单保持待支付、**频道不可见**，
%%     发货交给 payment_callback_logic 的 biz_type=2 分支（那侧已完整：
%%     双保险幂等 + 金额从订单反查 + 验签失败单独计数）。
-spec settle(binary(), integer(), integer(), binary(), binary(), map(), map()) ->
    {ok, map()} | {error, binary()}.
settle(Method, ChannelId, Uid, OrderNo, PayNo, Extra, Order) ->
    Envelope = #{
        <<"payment_method">> => Method,
        <<"payment_no">> => PayNo,
        <<"pay_params">> => Extra
    },
    case lists:member(Method, ?INSTANT_SETTLE_METHODS) of
        false ->
            {ok, Envelope#{<<"status">> => ?STATUS_PENDING}};
        true ->
            PaymentData = #{payment_no => PayNo, payment_method => Method},
            case do_pay_order(ChannelId, Uid, OrderNo, PaymentData, Order) of
                ok -> {ok, Envelope#{<<"status">> => ?STATUS_PAID}};
                {error, _} = Err -> Err
            end
    end.

%% @doc 归一网关返回：兼容 {ok, PayNo} 与 {ok, PayNo, Extra}，
%% 统一成 {ok, PayNo, Extra::map()}，错误原样透传（wallet/mock 无第三元组 → 空 map）。
-spec normalize_pay_result(term()) -> {ok, binary(), map()} | {error, term()}.
normalize_pay_result({ok, PayNo, Extra}) when is_binary(PayNo), is_map(Extra) ->
    {ok, PayNo, Extra};
normalize_pay_result({ok, PayNo}) when is_binary(PayNo) ->
    {ok, PayNo, #{}};
normalize_pay_result({error, Reason}) ->
    {error, error_binary(Reason)};
normalize_pay_result(_Unexpected) ->
    {error, <<"支付网关返回格式异常"/utf8>>}.

%% @doc 已支付订单的统一信封。
-spec paid_envelope(map(), binary()) -> map().
paid_envelope(Order, Method) ->
    #{
        <<"payment_method">> => Method,
        <<"payment_no">> => maps:get(<<"payment_no">>, Order, <<>>),
        <<"pay_params">> => #{},
        <<"status">> => ?STATUS_PAID
    }.

%% @doc jsonb 列经 repo 读回是 JSON 字符串（repo 层不做解码），
%% 直接当 map 用会 badmap（order/pay 恒 500 的根因）；统一归一成 map。
-spec normalize_extra_data(term()) -> map().
normalize_extra_data(Bin) when is_binary(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    catch
        _:_ ->
            #{}
    end;
normalize_extra_data(Map) when is_map(Map) ->
    Map;
normalize_extra_data(_) ->
    #{}.

%% @doc 支付网关执行：优先复用已有支付参数，否则创建新支付意图。
%% 复用路径：extra_data 中的 gateway_pay_no / gateway_extra 来自前一次 pay_order 调用。
%% B-00：复用时不检查订单是否过期（settle → do_pay_order 的 channel_order_ds:pay/2
%% 自带 expires_at 守卫，过期订单会自然失败）。
-spec pay_with_gateway(binary(), integer(), integer(), binary(), term(), map()) ->
    {ok, map()} | {error, binary()}.
pay_with_gateway(OrderNo, ChannelId, Uid, Method, Amount, Order) ->
    ExtraData = normalize_extra_data(maps:get(<<"extra_data">>, Order, null)),
    case maps:find(<<"gateway_pay_no">>, ExtraData) of
        {ok, ExistingPayNo} ->
            ExistingExtra = maps:get(<<"gateway_extra">>, ExtraData, #{}),
            settle(Method, ChannelId, Uid, OrderNo, ExistingPayNo, ExistingExtra, Order);
        error ->
            PayOpts = #{
                uid => Uid,
                amount => to_gateway_amount(Method, Amount),
                subject => <<"频道购买"/utf8>>
            },
            case normalize_pay_result(payment_gateway:pay(Method, OrderNo, PayOpts)) of
                {ok, PayNo, Extra} ->
                    _ = channel_order_ds:set_gateway_params(OrderNo, PayNo, Extra),
                    settle(Method, ChannelId, Uid, OrderNo, PayNo, Extra, Order);
                {error, PayReason} ->
                    {error, PayReason}
            end
    end.

-spec do_pay_order(integer(), integer(), binary(), map(), map()) -> ok | {error, binary()}.
do_pay_order(ChannelId, Uid, OrderNo, PaymentData, Order) ->
    case maps:get(<<"status">>, Order, 0) of
        1 ->
            %% already paid — ensure subscription active
            case channel_ds:subscribe(ChannelId, Uid) of
                ok ->
                    channel_logic_notify:notify_order_paid(ChannelId, Uid);
                {error, _} ->
                    {error, <<"订单已支付"/utf8>>};
                _UnexpectedSubscribe ->
                    {error, <<"订阅结果异常"/utf8>>}
            end;
        _ ->
            case
                channel_order_ds:pay(
                    OrderNo,
                    payment_data_with_subscription(Order, PaymentData)
                )
            of
                ok ->
                    case channel_ds:subscribe(ChannelId, Uid) of
                        ok ->
                            channel_logic_notify:notify_order_paid(ChannelId, Uid);
                        {error, Reason} ->
                            {error, error_binary(Reason)};
                        _UnexpectedSubscribe ->
                            {error, <<"订阅结果异常"/utf8>>}
                    end;
                {error, not_found_or_expired} ->
                    %% payment race: check final state, compensate if already paid
                    maybe_compensate_subscription(OrderNo);
                {error, Reason} ->
                    {error, error_binary(Reason)};
                _UnexpectedPay ->
                    {error, <<"订单支付结果异常"/utf8>>}
            end
    end.

%% @doc 将下单时锁定的订阅类型转换为支付时的有效期。
%% 旧订单缺失 extra_data 时按一次性购买兼容，不会意外延长权益。
-spec payment_data_with_subscription(map(), map()) -> map().
payment_data_with_subscription(Order, PaymentData) ->
    Start = normalize_subscription_start(maps:get(subscription_start_at, PaymentData, undefined)),
    Type = order_subscription_type(Order),
    End = subscription_end(Start, Type),
    PaymentData#{subscription_start_at => Start, subscription_end_at => End}.

-spec normalize_subscription_start(term()) -> integer().
normalize_subscription_start(Start) when is_integer(Start), Start > 0 -> Start;
normalize_subscription_start(_) -> elib_dt:millisecond().

-spec subscription_end(integer(), 1 | 2 | 3) -> null | integer().
subscription_end(_Start, 1) -> null;
subscription_end(Start, 2) -> Start + 30 * 24 * 60 * 60 * 1000;
subscription_end(Start, 3) -> Start + 365 * 24 * 60 * 60 * 1000.

-spec order_subscription_type(map()) -> 1 | 2 | 3.
order_subscription_type(Order) ->
    %% jsonb 经 repo 读回是 JSON 字符串，必须归一（否则 subscription_type 恒落 1，
    %% 包月/包年订单写成永久权益——2026-08-28 发布审查 C-2）
    Extra = normalize_extra_data(
        maps:get(<<"extra_data">>, Order, maps:get(extra_data, Order, null))
    ),
    Raw = maps:get(<<"subscription_type">>, Extra, maps:get(subscription_type, Extra, 1)),
    case normalize_subscription_type(Raw) of
        {ok, Type} -> Type;
        {error, invalid} -> 1
    end.

-spec normalize_subscription_type(term()) -> {ok, 1 | 2 | 3} | {error, invalid}.
normalize_subscription_type(1) -> {ok, 1};
normalize_subscription_type(2) -> {ok, 2};
normalize_subscription_type(3) -> {ok, 3};
normalize_subscription_type(<<"1">>) -> {ok, 1};
normalize_subscription_type(<<"2">>) -> {ok, 2};
normalize_subscription_type(<<"3">>) -> {ok, 3};
normalize_subscription_type(_) -> {error, invalid}.

-spec maybe_compensate_subscription(binary()) -> ok | {error, binary()}.
maybe_compensate_subscription(OrderNo) ->
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
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
            {error, <<"订单不存在或已过期"/utf8>>};
        _UnexpectedOrder ->
            {error, <<"订单数据异常"/utf8>>}
    end.

-spec get_my_orders(integer()) -> {ok, [map()]} | {error, binary()}.
get_my_orders(Uid) ->
    case channel_order_ds:list_by_user(Uid, 50) of
        {ok, Orders} when is_list(Orders) ->
            Orders2 = lists:map(fun order_transfer/1, [O || O <- Orders, is_map(O)]),
            {ok, Orders2};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, error_binary(Reason)};
        _UnexpectedOrders ->
            {error, <<"订单列表数据异常"/utf8>>}
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
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, LookupReason} ->
            {error, error_binary(LookupReason)};
        {ok, _InvalidOrder} ->
            {error, <<"订单不存在"/utf8>>};
        _UnexpectedOrder ->
            {error, <<"订单数据异常"/utf8>>}
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
                    %% 幂等：已退款/退款中订单直接返回，不重复处理
                    case Status of
                        2 ->
                            {error, <<"订单已退款"/utf8>>};
                        5 ->
                            {error, <<"订单退款中，请稍后重试"/utf8>>};
                        1 ->
                            do_refund_order(ChannelId, Uid, OrderNo, Order, Reason);
                        _ ->
                            {error, <<"订单状态不允许退款"/utf8>>}
                    end
            end;
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, LookupReason} ->
            {error, error_binary(LookupReason)};
        _UnexpectedOrder ->
            {error, <<"订单数据异常"/utf8>>}
    end.

%% @doc 管理端代发退款：不做订单归属校验，管理员可退任意订单。
%% 复用 do_refund_order 原语；注意取消订阅与退款流水用的是**订单买家的 user_id**，
%% 而非管理员 id（管理员身份仅用于 handler 层权限门控）。
-spec admin_refund_order(binary(), binary()) -> ok | {error, binary()}.
admin_refund_order(OrderNo, Reason0) ->
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
                true ->
                    %% 幂等：已退款/退款中订单直接返回，不重复处理
                    case Status of
                        2 ->
                            {error, <<"订单已退款"/utf8>>};
                        5 ->
                            {error, <<"订单退款中，请稍后重试"/utf8>>};
                        1 ->
                            do_refund_order(ChannelId, OrderUserId, OrderNo, Order, Reason);
                        _ ->
                            {error, <<"订单状态不允许退款"/utf8>>}
                    end
            end;
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, LookupReason} ->
            {error, error_binary(LookupReason)};
        _UnexpectedOrder ->
            {error, <<"订单数据异常"/utf8>>}
    end.

%% @doc 退款执行：CAS 占位(1→5) → 调网关 → CAS 收尾(5→2) → 取消订阅。
%% 金额单位：channel_order.amount 为元；按目标网关期望单位适配(wallet 元 / 第三方 分)。
%%
%% B-09 CAS 模式的好处：
%%   - 并发退款抢占：第二个请求拿不到 1→5 的 CAS，走不到网关调用那一步
%%   - 网关失败可重试：释放占位(5→1)后重试仍能走完整流程
%%   - 同步 payment_transaction 状态，避免对账系统误判
-spec do_refund_order(integer(), integer(), binary(), map(), binary()) ->
    ok | {error, binary()}.
do_refund_order(ChannelId, Uid, OrderNo, Order, Reason) ->
    Method = maps:get(<<"payment_method">>, Order, <<"wallet">>),
    PaymentNo = maps:get(<<"payment_no">>, Order, <<>>),
    Amount = maps:get(<<"amount">>, Order, 0),
    case is_payment_method_allowed(Method) of
        false ->
            {error, <<"不支持的支付方式"/utf8>>};
        true ->
            %% Step 1: CAS 抢占退款占位
            do_refund_cas(ChannelId, Uid, OrderNo, Method, PaymentNo, Amount, Reason)
    end.

-spec do_refund_cas(integer(), integer(), binary(), binary(), binary(), term(), binary()) ->
    ok | {error, binary()}.
do_refund_cas(ChannelId, Uid, OrderNo, Method, PaymentNo, Amount, Reason) ->
    case channel_order_ds:mark_refunding(OrderNo) of
        {ok, 0} ->
            {error, <<"订单已退款或退款中"/utf8>>};
        {ok, 1} ->
            do_refund_gateway(ChannelId, Uid, OrderNo, Method, PaymentNo, Amount, Reason);
        {error, Err} ->
            {error, elib_cnv:safe_to_binary(Err)}
    end.

-spec do_refund_gateway(integer(), integer(), binary(), binary(), binary(), term(), binary()) ->
    ok | {error, binary()}.
do_refund_gateway(ChannelId, Uid, OrderNo, Method, PaymentNo, Amount, Reason) ->
    %% Step 2: 同步标记 payment_transaction 为退款中（best-effort）
    _ = mark_payment_tx_refunding(OrderNo),
    %% Step 3: 调网关退款
    case payment_gateway:refund(Method, PaymentNo, to_gateway_amount(Method, Amount)) of
        ok ->
            do_refund_finalize(ChannelId, Uid, OrderNo, Reason);
        {error, PayReason} ->
            %% 网关失败：释放占位回到 paid(1)，允许重试
            _ = channel_order_ds:release_refunding(OrderNo),
            _ = release_payment_tx_refunding(OrderNo),
            {error, error_binary(PayReason)};
        _UnexpectedRefund ->
            _ = channel_order_ds:release_refunding(OrderNo),
            _ = release_payment_tx_refunding(OrderNo),
            {error, <<"退款网关返回格式异常"/utf8>>}
    end.

-spec do_refund_finalize(integer(), integer(), binary(), binary()) ->
    ok | {error, binary()}.
do_refund_finalize(ChannelId, Uid, OrderNo, Reason) ->
    %% Step 4: CAS 收尾 — 订单状态 refunding(5) → refunded(2)
    case channel_order_ds:finalize_refund(OrderNo, Uid, Reason) of
        ok ->
            %% Step 5: 同步 payment_transaction 为已退款（best-effort）
            _ = finalize_payment_tx_refund(OrderNo),
            %% Step 6: 取消订阅（失败不回退退款，仅告警日志）
            _ = channel_ds:unsubscribe(ChannelId, Uid),
            ok;
        {error, not_found_or_not_refunding} ->
            {error, <<"订单状态异常，退款无法完成"/utf8>>};
        {error, RefReason} ->
            {error, elib_cnv:safe_to_binary(RefReason)}
    end.

%% ===================================================================
%% 退款支付流水同步（best-effort 辅助，不阻断退款主流程）
%% ===================================================================

-spec mark_payment_tx_refunding(binary()) -> ok.
mark_payment_tx_refunding(OrderNo) ->
    case payment_transaction_ds:find_by_biz_order_no(2, OrderNo) of
        #{<<"trade_no">> := TradeNo, <<"status">> := 1} ->
            _ = payment_transaction_ds:mark_refunding(TradeNo),
            ok;
        _ ->
            ok
    end.

-spec release_payment_tx_refunding(binary()) -> ok.
release_payment_tx_refunding(OrderNo) ->
    case payment_transaction_ds:find_by_biz_order_no(2, OrderNo) of
        #{<<"trade_no">> := TradeNo, <<"status">> := 5} ->
            _ = payment_transaction_ds:release_refunding(TradeNo),
            ok;
        _ ->
            ok
    end.

-spec finalize_payment_tx_refund(binary()) -> ok.
finalize_payment_tx_refund(OrderNo) ->
    case payment_transaction_ds:find_by_biz_order_no(2, OrderNo) of
        #{<<"trade_no">> := TradeNo} ->
            _ = payment_transaction_ds:mark_refunded(TradeNo),
            ok;
        _ ->
            ok
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

-spec is_positive_amount(term()) -> boolean().
is_positive_amount(Amount) ->
    yuan_to_fen(Amount) > 0.

-spec error_binary(term()) -> binary().
error_binary(Reason) when is_binary(Reason) ->
    Reason;
error_binary(Reason) ->
    elib_cnv:safe_to_binary(Reason).

-spec normalize_refund_reason(term()) -> binary().
normalize_refund_reason(Reason) when is_binary(Reason), Reason =/= <<>> ->
    Reason;
normalize_refund_reason(_) ->
    <<"用户申请退款"/utf8>>.

-spec is_payment_method_allowed(binary()) -> boolean().
is_payment_method_allowed(Method) ->
    %% 统一使用启动环境解析器：未配置时 current/0 fail-safe 为 production，
    %% 避免生产部署遗漏 imboy.env 时意外放行 mock 支付。
    EnvBin = imboy_env:current(),
    Allowed =
        case EnvBin of
            <<"pro">> -> ?ALLOWED_PAYMENT_METHODS;
            <<"prod">> -> ?ALLOWED_PAYMENT_METHODS;
            <<"production">> -> ?ALLOWED_PAYMENT_METHODS;
            _ -> ?DEV_ALLOWED_PAYMENT_METHODS
        end,
    lists:member(Method, Allowed) andalso
        (lists:member(Method, ?INSTANT_SETTLE_METHODS) orelse
            payment_gateway:enabled()).

-spec decode_positive_id(term()) -> integer().
decode_positive_id(Value) ->
    case catch ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 ->
            Id;
        _ ->
            0
    end.

%% @doc 出参整形：把「已超时但仍是待支付」的订单显示为已过期(4)。
%%
%% B-03：`channel_order_repo:pay/2` 的 SQL 带 `expires_at > NOW()` 守卫，超时订单
%% 事实上已经付不了了，但库里 status 永远停在 0 —— 用户和管理后台看到的是一个
%% 永久「待支付」的僵尸订单。这里在读取侧派生，**不写库**：
%%   - 迟到的回调仍能按 B-08 补单（一旦落库改状态，补单就再无可能）
%%   - 不需要新的定时任务
%%
%% 取值 4(已过期) 而非计划判据写的 3(已取消)：迁移 00000003 的列注释是
%% 「0待支付 1已支付 2已退款 3已取消 4已过期」，3 是用户主动取消(cancel/1 用的就是它)。
%% 混用会让管理后台分不清"用户放弃"和"超时作废"。
%%
%% ponytail: 用节点时钟比 PG 的 expires_at，与 pay 的 NOW() 守卫存在时钟漂移窗口
%%   （只影响显示，不影响能否支付）。要精确一致就把 `expires_at <= NOW()` 放进
%%   SELECT 让 PG 自己判。
-spec order_transfer(map()) -> map().
order_transfer(Order) ->
    case maps:get(<<"status">>, Order, -1) =:= ?STATUS_PENDING of
        false -> Order;
        true -> maybe_mark_expired(Order)
    end.

-spec maybe_mark_expired(map()) -> map().
maybe_mark_expired(Order) ->
    case expires_at_ms(maps:get(<<"expires_at">>, Order, undefined)) of
        Ms when is_integer(Ms), Ms =< 0 -> Order;
        Ms when is_integer(Ms) ->
            case elib_dt:millisecond() >= Ms of
                true -> Order#{<<"status">> => ?STATUS_EXPIRED};
                false -> Order
            end;
        _ ->
            Order
    end.

%% @doc PG `timestamp with time zone` 经 epgsql 回来是 {{Y,M,D},{H,Mi,S}}（UTC，S 可能是浮点秒）。
%% 取不到或形状不认识（含非法日期）时返回 0 → 按"未过期"处理，不误杀正常订单。
%% 注：`elib_dt:datetime_to/2` 明示为内部函数不导出，这里直接用 stdlib calendar。
-define(EPOCH_GREGORIAN_SECS, 62167219200).

-spec expires_at_ms(term()) -> integer().
expires_at_ms({{_, _, _}, {H, Mi, S}} = Dt) when is_number(H), is_number(Mi), is_number(S) ->
    {Date, {_, _, _}} = Dt,
    try calendar:datetime_to_gregorian_seconds({Date, {trunc(H), trunc(Mi), trunc(S)}}) of
        Secs -> (Secs - ?EPOCH_GREGORIAN_SECS) * 1000
    catch
        _:_ -> 0
    end;
expires_at_ms(Ms) when is_integer(Ms), Ms > 0 ->
    Ms;
expires_at_ms(_) ->
    0.
