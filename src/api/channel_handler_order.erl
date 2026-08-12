-module(channel_handler_order).
-behavior(cowboy_rest).
-export([init/2, handle_action/3]).
-export([
    create_order/2,
    pay_order/2,
    cancel_order/2,
    order_status/2,
    refund_order/2,
    my_orders/2,
    get_order/2
]).
-include("error_code.hrl").

init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

handle_action(create_order, Req, State) -> create_order(Req, State);
handle_action(pay_order, Req, State) -> pay_order(Req, State);
handle_action(cancel_order, Req, State) -> cancel_order(Req, State);
handle_action(order_status, Req, State) -> order_status(Req, State);
handle_action(refund_order, Req, State) -> refund_order(Req, State);
handle_action(my_orders, Req, State) -> my_orders(Req, State);
handle_action(get_order, Req, State) -> get_order(Req, State);
handle_action(false, Req, _State) -> Req.

%% 订单相关 API（付费频道）
%% ===================================================================

%% @doc 创建订单
-spec create_order(cowboy_req:req(), map()) -> cowboy_req:req().
create_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    ChannelId = resolve_channel_id(Req0, PostVals),
    PaymentMethod = normalize_payment_method(
        maps:get(<<"payment_method">>, PostVals, <<"wallet">>)
    ),

    case ChannelId of
        <<>> ->
            elib_response:error(Req0, <<"频道ID不能为空"/utf8>>);
        _ ->
            case channel_logic:create_order(Uid, ChannelId, PaymentMethod) of
                {ok, Order} ->
                    elib_response:success(Req0, Order);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 支付订单（钱包即时扣款；第三方返回支付意图，等待回调确认）
-spec pay_order(cowboy_req:req(), map()) -> cowboy_req:req().
pay_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    OrderNo = normalize_non_empty_binary(maps:get(<<"order_no">>, PostVals, <<>>)),

    case OrderNo of
        <<>> ->
            elib_response:error(Req0, <<"订单号不能为空"/utf8>>);
        _ ->
            case channel_logic:pay_order(Uid, OrderNo) of
                {ok, Envelope} ->
                    %% 统一支付信封：payment_method / payment_no / pay_params（binary key）
                    elib_response:success(Req0, Envelope);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 取消待支付订单；已支付订单必须使用退款接口。
-spec cancel_order(cowboy_req:req(), map()) -> cowboy_req:req().
cancel_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    OrderNo = normalize_non_empty_binary(maps:get(<<"order_no">>, PostVals, <<>>)),
    case OrderNo of
        <<>> ->
            elib_response:error(Req0, <<"订单号不能为空"/utf8>>);
        _ ->
            case channel_logic:cancel_order(Uid, OrderNo) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 获取我的订单列表
-spec my_orders(cowboy_req:req(), map()) -> cowboy_req:req().
my_orders(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case channel_logic:get_my_orders(Uid) of
        {ok, Orders} ->
            elib_response:success(Req0, #{list => Orders});
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 获取订单详情
-spec get_order(cowboy_req:req(), map()) -> cowboy_req:req().
get_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    OrderNo = normalize_non_empty_binary(cowboy_req:binding(order_no, Req0)),
    case OrderNo of
        <<>> ->
            elib_response:error(Req0, <<"订单号不能为空"/utf8>>);
        _ ->
            case channel_logic:get_order(Uid, OrderNo) of
                {ok, Order} ->
                    elib_response:success(Req0, Order);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

-spec order_status(cowboy_req:req(), map()) -> cowboy_req:req().
order_status(Req0, State) ->
    get_order(Req0, State).

-spec refund_order(cowboy_req:req(), map()) -> cowboy_req:req().
refund_order(Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_param:post(Req0),
    OrderNo = resolve_order_no(Req0, PostVals),
    RefundReason = normalize_non_empty_binary(
        maps:get(<<"refund_reason">>, PostVals, <<>>)
    ),
    case OrderNo of
        <<>> ->
            elib_response:error(Req0, <<"订单号不能为空"/utf8>>);
        _ ->
            case channel_logic:refund_order(Uid, OrderNo, RefundReason) of
                ok ->
                    elib_response:success(Req0, #{});
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec resolve_channel_id(cowboy_req:req(), map()) -> binary().
resolve_channel_id(Req0, PostVals) ->
    case binding_or_empty(channel_id, Req0) of
        <<>> -> maps:get(<<"channel_id">>, PostVals, <<>>);
        ChannelId -> ChannelId
    end.

-spec resolve_order_no(cowboy_req:req(), map()) -> binary().
resolve_order_no(Req0, PostVals) ->
    case normalize_non_empty_binary(binding_or_empty(order_no, Req0)) of
        <<>> -> normalize_non_empty_binary(maps:get(<<"order_no">>, PostVals, <<>>));
        OrderNo -> OrderNo
    end.

-spec binding_or_empty(atom(), cowboy_req:req()) -> binary().
binding_or_empty(Key, Req0) ->
    case cowboy_req:binding(Key, Req0) of
        undefined -> <<>>;
        Val -> Val
    end.

-spec normalize_non_empty_binary(term()) -> binary().
normalize_non_empty_binary(undefined) ->
    <<>>;
normalize_non_empty_binary(Value) when is_binary(Value) ->
    list_to_binary(string:trim(binary_to_list(Value)));
normalize_non_empty_binary(Value) when is_list(Value) ->
    list_to_binary(string:trim(Value));
normalize_non_empty_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_non_empty_binary(_) ->
    <<>>.

-spec normalize_payment_method(term()) -> binary().
normalize_payment_method(Value) ->
    case normalize_non_empty_binary(Value) of
        <<>> -> <<"wallet">>;
        Method -> Method
    end.
