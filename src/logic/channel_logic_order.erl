-module(channel_logic_order).

-export([create_order/2]).
-export([pay_order/2]).
-export([get_my_orders/1]).
-export([get_order/2]).

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

-spec pay_order(integer(), binary()) -> ok | {error, binary()}.
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
                                    PayOpts = #{uid => Uid, amount => Amount},
                                    case payment_gateway:pay(Method, OrderNo, PayOpts) of
                                        {ok, PayNo} ->
                                            PaymentData = #{
                                                payment_no => PayNo,
                                                payment_method => Method
                                            },
                                            do_pay_order(
                                                ChannelId, Uid, OrderNo, PaymentData, Order
                                            );
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
