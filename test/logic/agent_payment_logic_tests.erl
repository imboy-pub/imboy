-module(agent_payment_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_payment_logic 单测：Agent 受控支付三道闸门（mandate 授权 + 门控扣款）。
% 全部用 meck 隔离 DS/wallet，纯验证闸门编排与「付款人恒为 owner_uid」不变量。
%
% 断言不扣款用 process dictionary 打点（mock fun 与测试同进程执行）；每个用例
% 开头 erase 打点键避免跨用例串味。
%%%

%% 标准 mandate：owner=999（付款人）、agent=100、单笔上限 10000、周期累计上限 50000
mandate() ->
    #{
        <<"id">> => 555,
        <<"owner_uid">> => 999,
        <<"agent_uid">> => 100,
        <<"max_amount_fen">> => 10000,
        <<"max_total_fen">> => 50000,
        <<"period_secs">> => 86400,
        <<"spent_fen">> => 0,
        <<"status">> => 1
    }.

clear() ->
    lists:foreach(fun erase/1, [debit, reserved, debited, released]),
    ok.

%% Happy path：闸门全过 → 从 owner_uid(999) 扣负数，返回 ok
happy_path_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, mandate()} end},
                {try_reserve, 2, fun(555, 5000) -> {ok, 5000} end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(_) -> #{} end},
                {ensure_wallet, 1, fun(999) -> #{<<"id">> => 7} end},
                {atomic_balance_change, 4, fun(Amount, Uid, _Tx, _Ref) ->
                    put(debit, {Amount, Uid}),
                    {ok, 95000}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF1">>),
            ?assertMatch({ok, _}, R),
            %% 关键不变量：付款人是 owner 999（非 agent 100），且扣负数
            ?assertEqual({-5000, 999}, get(debit))
        end
    ).

%% 超单笔上限（20000 > 10000）→ 拒，绝不预留/扣款
over_single_limit_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, mandate()} end},
                {try_reserve, 2, fun(_, _) ->
                    put(reserved, true),
                    {ok, 1}
                end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(_) -> #{} end},
                {ensure_wallet, 1, fun(_) -> #{<<"id">> => 7} end},
                {atomic_balance_change, 4, fun(_, _, _, _) ->
                    put(debited, true),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 20000, <<"REF2">>),
            ?assertEqual({error, exceeds_single_limit}, R),
            ?assertEqual(undefined, get(reserved)),
            ?assertEqual(undefined, get(debited))
        end
    ).

%% 超周期累计上限（try_reserve 原子闸门拒）→ 拒，绝不扣款
over_total_limit_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, mandate()} end},
                {try_reserve, 2, fun(555, 5000) -> {error, exceeds_total_limit} end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(_) -> #{} end},
                {ensure_wallet, 1, fun(_) -> #{<<"id">> => 7} end},
                {atomic_balance_change, 4, fun(_, _, _, _) ->
                    put(debited, true),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF3">>),
            ?assertEqual({error, exceeds_total_limit}, R),
            ?assertEqual(undefined, get(debited))
        end
    ).

%% mandate 过期/撤销（find_active 返回 notfound）→ 拒，绝不扣款
mandate_invalid_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {error, notfound} end},
                {try_reserve, 2, fun(_, _) ->
                    put(reserved, true),
                    {ok, 1}
                end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(_) -> #{} end},
                {ensure_wallet, 1, fun(_) -> #{<<"id">> => 7} end},
                {atomic_balance_change, 4, fun(_, _, _, _) ->
                    put(debited, true),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF4">>),
            ?assertEqual({error, mandate_invalid}, R),
            ?assertEqual(undefined, get(reserved)),
            ?assertEqual(undefined, get(debited))
        end
    ).

%% 幂等：同 RefNo 已入账（find_transaction_by_ref 命中）→ 直接返回 ok，
%% 不重复预留、不重复扣款
idempotent_replay_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, mandate()} end},
                {try_reserve, 2, fun(_, _) ->
                    put(reserved, true),
                    {ok, 1}
                end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(<<"DUP">>) -> #{<<"id">> => 111} end},
                {ensure_wallet, 1, fun(_) -> #{<<"id">> => 7} end},
                {atomic_balance_change, 4, fun(_, _, _, _) ->
                    put(debited, true),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"DUP">>),
            ?assertMatch({ok, _}, R),
            ?assertEqual(undefined, get(reserved)),
            ?assertEqual(undefined, get(debited))
        end
    ).

%% 扣款失败（余额不足）→ 释放已预留额度（周期额度不被空耗），返回 error
debit_failure_releases_reservation_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, mandate()} end},
                {try_reserve, 2, fun(555, 5000) -> {ok, 5000} end},
                {release, 2, fun(555, 5000) ->
                    put(released, true),
                    ok
                end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(_) -> #{} end},
                {ensure_wallet, 1, fun(_) -> #{<<"id">> => 7} end},
                {atomic_balance_change, 4, fun(_, _, _, _) -> {rollback, insufficient_balance} end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF6">>),
            ?assertEqual({error, insufficient_balance}, R),
            ?assertEqual(true, get(released))
        end
    ).

%% 非法入参：金额<=0 / RefNo 空 / ToUid=AgentUid → invalid_params（不触碰 DS）
invalid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_params},
            agent_payment_logic:pay_with_mandate(100, 200, 0, <<"R">>)
        ),
        ?assertEqual(
            {error, invalid_params},
            agent_payment_logic:pay_with_mandate(100, 200, 5000, <<>>)
        ),
        ?assertEqual(
            {error, invalid_params},
            agent_payment_logic:pay_with_mandate(100, 100, 5000, <<"R">>)
        )
    end).
