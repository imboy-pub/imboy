-module(payment_mock_gateway).
-behaviour(payment_gateway).
-export([pay/3, refund/2]).

pay(OrderNo, _Amount, _Opts) ->
    PayNo = iolist_to_binary([<<"MOCK_">>, OrderNo]),
    {ok, PayNo}.

refund(_PaymentNo, _Amount) ->
    ok.
