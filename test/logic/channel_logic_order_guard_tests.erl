-module(channel_logic_order_guard_tests).

%%% 回归测试：付费订单路径必须对脏价格、异常 DS payload 和异常支付网关响应 fail-closed。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

zero_price_fixture_never_creates_order_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                    #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
                end}
            ]},
            {channel_order_ds, [
                {'has_purchased', 2, fun(11, 1001) -> false end},
                {'get_price', 1, fun(11) ->
                    {ok, #{<<"price">> => 0, <<"currency">> => <<"CNY">>}}
                end},
                {'create_order', 1, fun(_) ->
                    erlang:error(zero_price_order_must_not_be_created)
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"频道价格无效"/utf8>>},
                channel_logic_order:create_order(1001, <<"11">>, <<"mock">>)
            )
        end
    ).

subscription_payment_window_test_() ->
    Start = 1700000000000,
    Monthly = channel_logic_order:payment_data_with_subscription(
        #{<<"extra_data">> => #{<<"subscription_type">> => 2}},
        #{payment_no => <<"P1">>, subscription_start_at => Start}
    ),
    Yearly = channel_logic_order:payment_data_with_subscription(
        #{<<"extra_data">> => #{<<"subscription_type">> => 3}},
        #{subscription_start_at => Start}
    ),
    OneTime = channel_logic_order:payment_data_with_subscription(
        #{}, #{subscription_start_at => Start}
    ),
    [
        ?_assertEqual(
            Start + 30 * 24 * 60 * 60 * 1000,
            maps:get(subscription_end_at, Monthly)
        ),
        ?_assertEqual(
            Start + 365 * 24 * 60 * 60 * 1000,
            maps:get(subscription_end_at, Yearly)
        ),
        ?_assertEqual(null, maps:get(subscription_end_at, OneTime)),
        ?_assertEqual(<<"P1">>, maps:get(payment_no, Monthly))
    ].

invalid_subscription_price_never_creates_order_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [{'current', 0, fun() -> <<"local">> end}]},
            {channel_ds, [
                {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                    #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
                end}
            ]},
            {channel_order_ds, [
                {'has_purchased', 2, fun(11, 1001) -> false end},
                {'get_price', 1, fun(11) ->
                    {ok, #{<<"price">> => 9.90, <<"subscription_type">> => 9}}
                end},
                {'create_order', 1, fun(_) -> erlang:error(invalid_price_must_not_create) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"频道订阅类型无效"/utf8>>},
                channel_logic_order:create_order(1001, <<"11">>, <<"mock">>)
            )
        end
    ).

create_order_locks_subscription_type_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [{'current', 0, fun() -> <<"local">> end}]},
            {channel_ds, [
                {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                    #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
                end}
            ]},
            {channel_order_ds, [
                {'has_purchased', 2, fun(11, 1001) -> false end},
                {'get_price', 1, fun(11) ->
                    {ok, #{
                        <<"price">> => 9.90,
                        <<"currency">> => <<"CNY">>,
                        <<"subscription_type">> => 2
                    }}
                end},
                {'create_order', 1, fun(_Data) ->
                    {ok, <<"ORD-SUBSCRIPTION">>}
                end},
                {'find_by_order_no', 1, fun(<<"ORD-SUBSCRIPTION">>) ->
                    {ok, #{<<"status">> => 0}}
                end}
            ]}
        ],
        fun() ->
            {ok, _Order} = channel_logic_order:create_order(1001, <<"11">>, <<"mock">>),
            [{_Pid, {channel_order_ds, create_order, [Data]}, _Ret}] = [
                H
             || H = {_Pid, {channel_order_ds, create_order, [_Data]}, _Ret} <-
                    meck:history(channel_order_ds)
            ],
            ?assertEqual(#{<<"subscription_type">> => 2}, maps:get(extra_data, Data))
        end
    ).

mock_payment_is_rejected_in_production_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [
                {'current', 0, fun() -> <<"production">> end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun(_, _) ->
                    erlang:error(production_mock_must_not_query_channel)
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"不支持的支付方式"/utf8>>},
                channel_logic_order:create_order(1001, <<"11">>, <<"mock">>)
            )
        end
    ).

external_payment_is_rejected_when_gateway_disabled_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [
                {'current', 0, fun() -> <<"production">> end}
            ]},
            {payment_gateway, [
                {'enabled', 0, fun() -> false end}
            ]},
            {channel_ds, [
                {'find_by_id', 2, fun(_, _) ->
                    erlang:error(disabled_gateway_must_not_query_channel)
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"不支持的支付方式"/utf8>>},
                channel_logic_order:create_order(1001, <<"11">>, <<"alipay">>)
            )
        end
    ).

cancel_pending_order_test_() ->
    ?WITH_MECKS(
        [
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-CANCEL">>) ->
                    {ok, #{<<"user_id">> => 1001, <<"status">> => 0}}
                end},
                {'cancel', 1, fun(<<"ORD-CANCEL">>) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_logic_order:cancel_order(1001, <<"ORD-CANCEL">>))
        end
    ).

cancel_paid_order_is_rejected_test_() ->
    ?WITH_MECKS(
        [
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-PAID">>) ->
                    {ok, #{<<"user_id">> => 1001, <<"status">> => 1}}
                end},
                {'cancel', 1, fun(_) -> erlang:error(paid_order_must_use_refund) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"订单状态不允许取消"/utf8>>},
                channel_logic_order:cancel_order(1001, <<"ORD-PAID">>)
            )
        end
    ).

cancel_order_ds_error_is_normalized_test_() ->
    ?WITH_MECKS(
        [
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-CANCEL-DB">>) ->
                    {ok, #{<<"user_id">> => 1001, <<"status">> => 0}}
                end},
                {'cancel', 1, fun(<<"ORD-CANCEL-DB">>) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"db_down">>},
                channel_logic_order:cancel_order(1001, <<"ORD-CANCEL-DB">>)
            )
        end
    ).

cancel_order_accepts_numeric_binary_owner_and_status_test_() ->
    ?WITH_MECKS(
        [
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-CANCEL-BINARY">>) ->
                    {ok, #{
                        <<"user_id">> => <<"1001">>,
                        <<"status">> => <<"0">>
                    }}
                end},
                {'cancel', 1, fun(<<"ORD-CANCEL-BINARY">>) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, channel_logic_order:cancel_order(1001, <<"ORD-CANCEL-BINARY">>))
        end
    ).

pay_order_returns_ds_error_instead_of_case_clause_test_() ->
    ?WITH_MECKS(
        [
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-DB">>) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"db_down">>},
                channel_logic_order:pay_order(1001, <<"ORD-DB">>)
            )
        end
    ).

malformed_gateway_response_fails_closed_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-BAD-GW">>) ->
                    {ok, #{
                        <<"user_id">> => 1001,
                        <<"channel_id">> => 11,
                        <<"status">> => 0,
                        <<"payment_method">> => <<"mock">>,
                        <<"amount">> => 9.90
                    }}
                end}
            ]},
            {payment_gateway, [
                {'pay', 3, fun(<<"mock">>, <<"ORD-BAD-GW">>, _Opts) ->
                    malformed_gateway_response
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"支付网关返回格式异常"/utf8>>},
                channel_logic_order:pay_order(1001, <<"ORD-BAD-GW">>)
            )
        end
    ).

external_refund_is_rejected_when_gateway_disabled_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [
                {'current', 0, fun() -> <<"production">> end}
            ]},
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-REFUND-GATEWAY-OFF">>) ->
                    {ok, #{
                        <<"user_id">> => 1001,
                        <<"channel_id">> => 11,
                        <<"status">> => 1,
                        <<"payment_method">> => <<"alipay">>,
                        <<"payment_no">> => <<"PAY-1">>,
                        <<"amount">> => 9.90
                    }}
                end},
                {'refund', 3, fun(_, _, _) ->
                    erlang:error(disabled_gateway_must_not_refund)
                end}
            ]},
            {payment_gateway, [
                {'enabled', 0, fun() -> false end},
                {'refund', 3, fun(_, _, _) ->
                    erlang:error(disabled_gateway_must_not_call_gateway)
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"不支持的支付方式"/utf8>>},
                channel_logic_order:refund_order(1001, <<"ORD-REFUND-GATEWAY-OFF">>)
            )
        end
    ).

legacy_zero_amount_order_cannot_be_paid_test_() ->
    ?WITH_MECKS(
        [
            {imboy_env, [
                {'current', 0, fun() -> <<"local">> end}
            ]},
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-ZERO">>) ->
                    {ok, #{
                        <<"user_id">> => 1001,
                        <<"channel_id">> => 11,
                        <<"status">> => 0,
                        <<"payment_method">> => <<"mock">>,
                        <<"amount">> => 0
                    }}
                end}
            ]},
            {payment_gateway, [
                {'pay', 3, fun(_, _, _) -> erlang:error(zero_amount_must_not_reach_gateway) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"订单金额无效"/utf8>>},
                channel_logic_order:pay_order(1001, <<"ORD-ZERO">>)
            )
        end
    ).

refund_order_returns_ds_error_instead_of_case_clause_test_() ->
    ?WITH_MECKS(
        [
            {channel_order_ds, [
                {'find_by_order_no', 1, fun(<<"ORD-REFUND-DB">>) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"db_down">>},
                channel_logic_order:refund_order(1001, <<"ORD-REFUND-DB">>)
            )
        end
    ).
