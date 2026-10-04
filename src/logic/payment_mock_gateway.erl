-module(payment_mock_gateway).
-moduledoc "payment_gateway behaviour 的 mock 实现 —— 本地/测试用假支付网关（MOCK_ 前缀支付号，退款恒 ok）。".
-behaviour(payment_gateway).
-export([pay/3, refund/2]).

pay(OrderNo, _Amount, _Opts) ->
    PayNo = iolist_to_binary([<<"MOCK_">>, OrderNo]),
    {ok, PayNo}.

refund(_PaymentNo, _Amount) ->
    ok.
