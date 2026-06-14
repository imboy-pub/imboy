-module(finance_adm_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc finance_adm_logic 运营财务查询逻辑测试
%%% 覆盖：白名单筛选（build_where）、TSID 字段归一化、错误透传。

%% ===================================================================
%% list_wallets/3
%% ===================================================================

%% 仅白名单字段进入 WhereMap，非法键被丢弃；id/user_id 转字符串
list_wallets_filters_whitelist_and_normalizes_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [
            {'page', 3, fun(WhereMap, 1, 20) ->
                %% 断言：白名单 user_id/status 透传，非白名单 evil 被剔除
                ?assertEqual(#{user_id => 100, status => 1}, WhereMap),
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [#{<<"id">> => 555, <<"user_id">> => 100, <<"balance">> => 999}]
                }}
            end}
        ],
        fun() ->
            Filter = #{user_id => 100, status => 1, evil => <<"drop">>},
            {ok, Payload} = finance_adm_logic:list_wallets(Filter, 1, 20),
            [Row] = maps:get(list, Payload),
            %% TSID 字段转为字符串，金额（分）不变
            ?assertEqual(<<"555">>, maps:get(<<"id">>, Row)),
            ?assertEqual(<<"100">>, maps:get(<<"user_id">>, Row)),
            ?assertEqual(999, maps:get(<<"balance">>, Row))
        end
    ).

%% 空 Filter -> 空 WhereMap（查全部）
list_wallets_empty_filter_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [
            {'page', 3, fun(WhereMap, 2, 10) ->
                ?assertEqual(#{}, WhereMap),
                {ok, #{total => 0, page => 2, size => 10, list => []}}
            end}
        ],
        fun() ->
            ?assertMatch({ok, #{list := []}}, finance_adm_logic:list_wallets(#{}, 2, 10))
        end
    ).

list_wallets_error_passthrough_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [
            {'page', 3, fun(_W, _P, _S) -> {error, db_down} end}
        ],
        fun() ->
            ?assertEqual({error, db_down}, finance_adm_logic:list_wallets(#{}, 1, 20))
        end
    ).

%% ===================================================================
%% list_wallet_transactions/3
%% ===================================================================

list_wallet_transactions_normalizes_ids_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [
            {'page_transactions', 3, fun(1, 20, 100) ->
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            <<"id">> => 7,
                            <<"wallet_id">> => 8,
                            <<"user_id">> => 100,
                            <<"amount">> => -50
                        }
                    ]
                }}
            end}
        ],
        fun() ->
            {ok, Payload} = finance_adm_logic:list_wallet_transactions(100, 1, 20),
            [Row] = maps:get(list, Payload),
            ?assertEqual(<<"7">>, maps:get(<<"id">>, Row)),
            ?assertEqual(<<"8">>, maps:get(<<"wallet_id">>, Row)),
            ?assertEqual(<<"100">>, maps:get(<<"user_id">>, Row)),
            ?assertEqual(-50, maps:get(<<"amount">>, Row))
        end
    ).

%% ===================================================================
%% list_recharge_orders/3
%% ===================================================================

list_recharge_orders_filters_whitelist_test_() ->
    ?WITH_MECK(
        recharge_order_ds,
        [
            {'page', 3, fun(WhereMap, 1, 20) ->
                ?assertEqual(
                    #{
                        status => 1,
                        payment_method => <<"alipay">>,
                        user_id => 100,
                        order_no => <<"RCH123">>
                    },
                    WhereMap
                ),
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [#{<<"id">> => 9, <<"user_id">> => 100, <<"amount">> => 10000}]
                }}
            end}
        ],
        fun() ->
            Filter = #{
                status => 1,
                payment_method => <<"alipay">>,
                user_id => 100,
                order_no => <<"RCH123">>,
                not_allowed => 1
            },
            {ok, Payload} = finance_adm_logic:list_recharge_orders(Filter, 1, 20),
            [Row] = maps:get(list, Payload),
            ?assertEqual(<<"9">>, maps:get(<<"id">>, Row)),
            ?assertEqual(<<"100">>, maps:get(<<"user_id">>, Row))
        end
    ).

%% ===================================================================
%% list_payment_transactions/3
%% ===================================================================

list_payment_transactions_passes_columns_and_filters_test_() ->
    ?WITH_MECK(
        payment_transaction_ds,
        [
            {'page', 5, fun(Column, WhereMap, Order, 1, 20) ->
                %% 列与排序由 logic 固定，筛选白名单透传
                ?assert(is_binary(Column)),
                ?assertEqual(<<"id desc">>, Order),
                ?assertEqual(#{gateway => <<"wechat">>, biz_type => 1, status => 1}, WhereMap),
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [#{<<"id">> => 12, <<"user_id">> => 100, <<"amount">> => 500}]
                }}
            end}
        ],
        fun() ->
            Filter = #{gateway => <<"wechat">>, biz_type => 1, status => 1},
            {ok, Payload} = finance_adm_logic:list_payment_transactions(Filter, 1, 20),
            [Row] = maps:get(list, Payload),
            ?assertEqual(<<"12">>, maps:get(<<"id">>, Row)),
            ?assertEqual(<<"100">>, maps:get(<<"user_id">>, Row))
        end
    ).
