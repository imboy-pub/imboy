-module(channel_logic_order).

-export([create_order/2]).
-export([pay_order/2]).
-export([get_my_orders/1]).
-export([get_order/2]).

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
                            case channel_subscribe_ds:create_order(ChannelId, Uid, #{}) of
                                {ok, OrderNo} ->
                                    case channel_order_ds:find_by_order_no(OrderNo) of
                                        {ok, Order} when is_map(Order) ->
                                            Order2 = order_transfer(Order),
                                            {ok, Order2};
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
                                Other ->
                                    {error, elib_cnv:safe_to_binary(Other)}
                            end
                    end;
                _ ->
                    {error, <<"频道不存在"/utf8>>}
            end
    end.

-spec pay_order(integer(), binary()) -> ok | {error, binary()}.
pay_order(Uid, OrderNo) ->
    case channel_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
            OrderUserId = maps:get(<<"user_id">>, Order, 0),
            ChannelId = maps:get(<<"channel_id">>, Order, 0),
            case is_integer(OrderUserId)
                andalso OrderUserId > 0
                andalso is_integer(ChannelId)
                andalso ChannelId > 0 of
                false ->
                    {error, <<"订单不存在"/utf8>>};
                true ->
                    if
                        OrderUserId =/= Uid ->
                            {error, <<"无权操作此订单"/utf8>>};
                        true ->
                            % 从订单中获取支付方式（默认 mock，可传 wallet）
                            Method = maps:get(<<"payment_method">>, Order, <<"mock">>),
                            Amount = maps:get(<<"amount">>, Order, 0),
                            PayOpts = #{uid => Uid, amount => Amount},
                            case payment_gateway:pay(Method, OrderNo, PayOpts) of
                                {ok, PayNo} ->
                                    PaymentData = #{
                                        payment_no => PayNo,
                                        payment_method => Method
                                    },
                                    case channel_subscribe_ds:pay_order(OrderNo, PaymentData) of
                                        ok ->
                                            channel_logic_notify:notify_order_paid(ChannelId, Uid);
                                        {error, already_paid} ->
                                            {error, <<"订单已支付"/utf8>>};
                                        {error, not_found_or_expired} ->
                                            {error, <<"订单不存在或已过期"/utf8>>};
                                        {error, Reason} when is_binary(Reason) ->
                                            {error, Reason};
                                        {error, Reason} ->
                                            {error, elib_cnv:safe_to_binary(Reason)};
                                        Other ->
                                            {error, elib_cnv:safe_to_binary(Other)}
                                    end;
                                {error, PayReason} ->
                                    {error, PayReason}
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
        Other ->
            {error, elib_cnv:safe_to_binary(Other)}
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
        Other ->
            {error, elib_cnv:safe_to_binary(Other)}
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
        Other ->
            {error, elib_cnv:safe_to_binary(Other)}
    end.

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

