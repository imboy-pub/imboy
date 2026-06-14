-module(billing_logic_page_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc billing_logic 运营分页查询测试
%%% 覆盖：套餐分页（quota_config 解码）、订阅分页、账单分页的薄封装。

%% 套餐分页：list 中每条 quota_config 由 jsonb binary 解码为 map
list_plans_page_decodes_quota_test_() ->
    ?WITH_MECK(
        billing_plan_ds,
        [
            {'page', 4, fun(WhereMap, Order, 1, 20) ->
                ?assertEqual(#{status => 1}, WhereMap),
                ?assertEqual(<<"id desc">>, Order),
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            <<"id">> => 5,
                            <<"code">> => <<"pro">>,
                            <<"price">> => 9900,
                            <<"quota_config">> => <<"{\"max_users\":500}">>
                        }
                    ]
                }}
            end}
        ],
        fun() ->
            {ok, Payload} = billing_logic:list_plans_page(#{status => 1}, 1, 20),
            [Plan] = maps:get(list, Payload),
            %% quota_config 已解码为 map，price（分）不变
            ?assertEqual(#{<<"max_users">> => 500}, maps:get(<<"quota_config">>, Plan)),
            ?assertEqual(9900, maps:get(<<"price">>, Plan))
        end
    ).

list_plans_page_error_passthrough_test_() ->
    ?WITH_MECK(
        billing_plan_ds,
        [
            {'page', 4, fun(_W, _O, _P, _S) -> {error, db_down} end}
        ],
        fun() ->
            ?assertEqual({error, db_down}, billing_logic:list_plans_page(#{}, 1, 20))
        end
    ).

%% 订阅分页：薄封装透传 WhereMap/Order
list_subscriptions_page_passthrough_test_() ->
    ?WITH_MECK(
        billing_subscription_ds,
        [
            {'page', 4, fun(WhereMap, Order, 1, 20) ->
                ?assertEqual(#{status => 1, plan_id => 5}, WhereMap),
                ?assertEqual(<<"id desc">>, Order),
                {ok, #{total => 0, page => 1, size => 20, list => []}}
            end}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{list := []}},
                billing_logic:list_subscriptions_page(#{status => 1, plan_id => 5}, 1, 20)
            )
        end
    ).

%% 账单分页：薄封装透传 WhereMap/Order
list_invoices_page_passthrough_test_() ->
    ?WITH_MECK(
        billing_invoice_ds,
        [
            {'page', 4, fun(WhereMap, Order, 1, 20) ->
                ?assertEqual(#{status => 0}, WhereMap),
                ?assertEqual(<<"id desc">>, Order),
                {ok, #{total => 0, page => 1, size => 20, list => []}}
            end}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{list := []}},
                billing_logic:list_invoices_page(#{status => 0}, 1, 20)
            )
        end
    ).
