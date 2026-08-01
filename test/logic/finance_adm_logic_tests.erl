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

%% ===================================================================
%% refund_recharge_order/1 —— 充值订单退款（资金反向 + 幂等 + 状态校验）
%% ===================================================================

refund_recharge_order_success_test_() ->
    ?WITH_MECK(
        recharge_order_ds,
        [{'refund_in_tx', 1, fun(<<"RCH1">>) -> {ok, 8800} end}],
        fun() ->
            ?assertEqual({ok, 8800}, finance_adm_logic:refund_recharge_order(<<"RCH1">>))
        end
    ).

%% 幂等：已退款订单再退 -> 明确拒绝，不重复退
refund_recharge_order_already_refunded_test_() ->
    ?WITH_MECK(
        recharge_order_ds,
        [{'refund_in_tx', 1, fun(_) -> {rollback, already_refunded} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_recharge_order(<<"RCH1">>))
        end
    ).

%% 状态校验：非「已支付」态订单不可退
refund_recharge_order_not_refundable_test_() ->
    ?WITH_MECK(
        recharge_order_ds,
        [{'refund_in_tx', 1, fun(_) -> {rollback, order_not_refundable} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_recharge_order(<<"RCH1">>))
        end
    ).

%% 资金边界：可用余额不足 -> 拒绝退款（不把余额扣成负 / 不动冻结额）
refund_recharge_order_insufficient_test_() ->
    ?WITH_MECK(
        recharge_order_ds,
        [{'refund_in_tx', 1, fun(_) -> {rollback, insufficient_available} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_recharge_order(<<"RCH1">>))
        end
    ).

%% ===================================================================
%% refund_payment_transaction/1 —— 支付流水退款（原路退回 + 保守分流）
%% ===================================================================

%% 流水不存在
refund_payment_tx_not_found_test_() ->
    ?WITH_MECK(
        payment_transaction_ds,
        [{'find_by_trade_no', 1, fun(_) -> #{} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>))
        end
    ).

%% 幂等：流水已退款(status=3) -> 拒绝
refund_payment_tx_already_refunded_test_() ->
    ?WITH_MECK(
        payment_transaction_ds,
        [{'find_by_trade_no', 1, fun(_) -> #{<<"status">> => 3, <<"biz_type">> => 3} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>))
        end
    ).

%% 状态校验：非成功态(如待支付0)不可退
refund_payment_tx_not_success_test_() ->
    ?WITH_MECK(
        payment_transaction_ds,
        [{'find_by_trade_no', 1, fun(_) -> #{<<"status">> => 0, <<"biz_type">> => 3} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>))
        end
    ).

%% 保守分流：充值类(biz_type=1)拒绝，引导走「充值订单退款」防钱包重复退
refund_payment_tx_recharge_rejected_test_() ->
    ?WITH_MECK(
        payment_transaction_ds,
        [{'find_by_trade_no', 1, fun(_) -> #{<<"status">> => 1, <<"biz_type">> => 1} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>))
        end
    ).

%% 保守分流：频道订单(biz_type=2)拒绝，引导走频道订单退款
refund_payment_tx_channel_rejected_test_() ->
    ?WITH_MECK(
        payment_transaction_ds,
        [{'find_by_trade_no', 1, fun(_) -> #{<<"status">> => 1, <<"biz_type">> => 2} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>))
        end
    ).

%% biz_type=3(账单)成功：网关原路退回 ok + CAS 标记 {ok,1}
refund_payment_tx_billing_success_test_() ->
    ?WITH_MECKS(
        [
            {payment_transaction_ds, [
                {'find_by_trade_no', 1, fun(_) ->
                    #{
                        <<"status">> => 1,
                        <<"biz_type">> => 3,
                        <<"gateway">> => <<"alipay">>,
                        <<"gateway_payment_no">> => <<"GW123">>,
                        <<"amount">> => 500
                    }
                end},
                %% B-09：占位(1→5) 必须先于网关调用
                {'mark_refunding', 1, fun(<<"T1">>) -> {ok, 1} end},
                {'mark_refunded', 1, fun(<<"T1">>) -> {ok, 1} end}
            ]},
            {payment_gateway, [
                {'refund', 3, fun(<<"alipay">>, <<"GW123">>, 500) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual({ok, refunded}, finance_adm_logic:refund_payment_transaction(<<"T1">>)),
            ?assertEqual(1, meck:num_calls(payment_transaction_ds, mark_refunding, '_')),
            ?assertEqual(1, meck:num_calls(payment_gateway, refund, '_'))
        end
    ).

%% 竞态：网关退款成功但 CAS 标记 0 行（已被并发退）-> 报错
refund_payment_tx_mark_race_test_() ->
    ?WITH_MECKS(
        [
            {payment_transaction_ds, [
                {'find_by_trade_no', 1, fun(_) ->
                    #{
                        <<"status">> => 1,
                        <<"biz_type">> => 3,
                        <<"gateway">> => <<"alipay">>,
                        <<"gateway_payment_no">> => <<"GW123">>,
                        <<"amount">> => 500
                    }
                end},
                {'mark_refunding', 1, fun(_) -> {ok, 1} end},
                {'mark_refunded', 1, fun(_) -> {ok, 0} end}
            ]},
            {payment_gateway, [
                {'refund', 3, fun(_, _, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>))
        end
    ).

%% 缺网关支付单号 -> 无法原路退回
refund_payment_tx_missing_gwno_test_() ->
    ?WITH_MECK(
        payment_transaction_ds,
        [
            {'find_by_trade_no', 1, fun(_) ->
                #{<<"status">> => 1, <<"biz_type">> => 3, <<"gateway_payment_no">> => <<>>}
            end}
        ],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>))
        end
    ).

%% ===================================================================
%% freeze_wallet/2 与 unfreeze_wallet/2 —— 冻结/解冻（对称可逆）
%% ===================================================================

freeze_wallet_success_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [{'freeze', 2, fun(100, 500) -> {ok, 1} end}],
        fun() ->
            ?assertEqual(ok, finance_adm_logic:freeze_wallet(100, 500))
        end
    ).

%% 冻结失败：可用余额不足/钱包不存在（{ok,0}）-> 明确错误
freeze_wallet_blocked_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [{'freeze', 2, fun(_, _) -> {ok, 0} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:freeze_wallet(100, 500))
        end
    ).

unfreeze_wallet_success_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [{'unfreeze', 2, fun(100, 500) -> {ok, 1} end}],
        fun() ->
            ?assertEqual(ok, finance_adm_logic:unfreeze_wallet(100, 500))
        end
    ).

%% 解冻失败：冻结额不足（{ok,0}）-> 明确错误
unfreeze_wallet_blocked_test_() ->
    ?WITH_MECK(
        wallet_ds,
        [{'unfreeze', 2, fun(_, _) -> {ok, 0} end}],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:unfreeze_wallet(100, 500))
        end
    ).

%% ===================================================================
%% B-09 三态退款：mark_refunded 失败后重试**不产生第二次网关调用**
%% ===================================================================

%% 判据本体：占位成功 → 网关退款 ok → mark_refunded 报错 → 流水留在 5(退款中)。
%% 管理员重试时 find_by_trade_no 返回 5，直接被拒，网关一次都不再调。
refund_payment_tx_retry_after_mark_failure_test_() ->
    ?WITH_MECKS(
        [
            {payment_transaction_ds, [
                {'find_by_trade_no', 1, fun(_) ->
                    #{
                        <<"status">> => 5,
                        <<"biz_type">> => 3,
                        <<"gateway">> => <<"alipay">>,
                        <<"gateway_payment_no">> => <<"GW123">>,
                        <<"amount">> => 500
                    }
                end}
            ]},
            {payment_gateway, [
                {'refund', 3, fun(_, _, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>)),
            %% 这一条就是 B-09 的判据
            ?assertEqual(0, meck:num_calls(payment_gateway, refund, '_'))
        end
    ).

%% 占位抢不到（并发第二个请求 / 已在退款中）→ 网关不得被调用
refund_payment_tx_cas_lost_skips_gateway_test_() ->
    ?WITH_MECKS(
        [
            {payment_transaction_ds, [
                {'find_by_trade_no', 1, fun(_) ->
                    #{
                        <<"status">> => 1,
                        <<"biz_type">> => 3,
                        <<"gateway">> => <<"alipay">>,
                        <<"gateway_payment_no">> => <<"GW123">>,
                        <<"amount">> => 500
                    }
                end},
                {'mark_refunding', 1, fun(_) -> {ok, 0} end}
            ]},
            {payment_gateway, [
                {'refund', 3, fun(_, _, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>)),
            ?assertEqual(0, meck:num_calls(payment_gateway, refund, '_'))
        end
    ).

%% 网关明确失败 → 释放占位(5→1)，让管理员还能重试；不得留下永久卡死的流水
refund_payment_tx_gateway_error_releases_placeholder_test_() ->
    ?WITH_MECKS(
        [
            {payment_transaction_ds, [
                {'find_by_trade_no', 1, fun(_) ->
                    #{
                        <<"status">> => 1,
                        <<"biz_type">> => 3,
                        <<"gateway">> => <<"alipay">>,
                        <<"gateway_payment_no">> => <<"GW123">>,
                        <<"amount">> => 500
                    }
                end},
                {'mark_refunding', 1, fun(_) -> {ok, 1} end},
                {'release_refunding', 1, fun(_) -> {ok, 1} end}
            ]},
            {payment_gateway, [
                {'refund', 3, fun(_, _, _) -> {error, <<"网关拒绝"/utf8>>} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"网关拒绝"/utf8>>},
                finance_adm_logic:refund_payment_transaction(<<"T1">>)
            ),
            ?assertEqual(1, meck:num_calls(payment_transaction_ds, release_refunding, '_'))
        end
    ).

%% 网关成功但收尾报错 → **故意不释放占位**，留在 5 让重试被挡住，人工收尾。
%% 释放回 1 才是真正危险的那条路（下一次重试就会二次退款）。
refund_payment_tx_mark_error_keeps_placeholder_test_() ->
    ?WITH_MECKS(
        [
            {payment_transaction_ds, [
                {'find_by_trade_no', 1, fun(_) ->
                    #{
                        <<"status">> => 1,
                        <<"biz_type">> => 3,
                        <<"gateway">> => <<"alipay">>,
                        <<"gateway_payment_no">> => <<"GW123">>,
                        <<"amount">> => 500
                    }
                end},
                {'mark_refunding', 1, fun(_) -> {ok, 1} end},
                {'mark_refunded', 1, fun(_) -> {error, db_down} end},
                {'release_refunding', 1, fun(_) -> {ok, 1} end}
            ]},
            {payment_gateway, [
                {'refund', 3, fun(_, _, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, finance_adm_logic:refund_payment_transaction(<<"T1">>)),
            ?assertEqual(0, meck:num_calls(payment_transaction_ds, release_refunding, '_'))
        end
    ).
