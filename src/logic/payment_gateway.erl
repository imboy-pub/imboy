-module(payment_gateway).

%%%===================================================================
%%% @doc 支付网关 Behaviour 定义
%%%
%%% 所有实现者须导出 pay/3 和 refund/2
%%% @end
%%%===================================================================

-callback pay(OrderNo :: binary(), Amount :: integer(), Opts :: map()) ->
    {ok, binary()} | {error, binary()}.
%% 返回 {ok, PaymentNo} 或 {error, Reason}

-callback refund(PaymentNo :: binary(), Amount :: integer()) ->
    ok | {error, binary()}.

%% 根据 payment_method 选择实现模块

-export([pay/3]).

pay(Method, OrderNo, Opts) ->
    case method_module(Method) of
        {ok, Module} ->
            Amount = maps:get(amount, Opts, 0),
            Module:pay(OrderNo, Amount, Opts);
        {error, _} = Err ->
            Err
    end.

method_module(<<"mock">>) -> {ok, payment_mock_gateway};
method_module(<<"wallet">>) -> {ok, payment_wallet_gateway};
method_module(_Unknown) -> {error, <<"不支持的支付方式"/utf8>>}.
