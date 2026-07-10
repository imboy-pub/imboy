-module(agent_payment_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% agent_payment_logic 单测：Agent 受控支付三道闸门（mandate 授权 + 两腿原子结算）。
% 全部用 meck 隔离 DS/wallet，纯验证闸门编排与「付款人恒为 owner_uid、收款方=ToUid」不变量。
%
% 断言未结算用 process dictionary 打点（mock fun 与测试同进程执行）；每个用例
% 开头 clear 打点键避免跨用例串味。
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
    lists:foreach(fun erase/1, [reserved, released, transfer, transferred]),
    ok.

%% 两钱包 mock：owner 999 → wallet 7；ToUid 200 → wallet 8
ensure_wallet_fun() ->
    fun
        (999) -> #{<<"id">> => 7};
        (200) -> #{<<"id">> => 8};
        (_) -> #{<<"id">> => 9}
    end.

%% Happy path：闸门全过 → 单事务借记 owner(999) + 贷记 ToUid(200)，两腿不同 RefNo
happy_path_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, mandate()} end},
                {try_reserve, 2, fun(555, 5000) -> {ok, 5000} end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(_) -> #{} end},
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(Debit, Credit) ->
                    put(transfer, {Debit, Credit}),
                    {ok, #{debit_balance => 95000, credit_balance => 5000}}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF1">>),
            ?assertMatch({ok, _}, R),
            {Debit, Credit} = get(transfer),
            %% 关键不变量：付款人=owner 999（非 agent 100），收款方=ToUid 200
            ?assertEqual(999, maps:get(user_id, Debit)),
            ?assertEqual(200, maps:get(user_id, Credit)),
            %% 借贷等额（本切片无手续费）
            ?assertEqual(5000, maps:get(amount, Debit)),
            ?assertEqual(5000, maps:get(amount, Credit)),
            %% 两腿不同 RefNo：贷记派生 _CR，避免撞 wallet_transaction 唯一索引
            ?assertEqual(<<"REF1">>, maps:get(reference_no, Debit)),
            ?assertEqual(<<"REF1_CR">>, maps:get(reference_no, Credit)),
            %% wallet_id 正确映射（owner→7、ToUid→8）
            ?assertEqual(7, maps:get(wallet_id, Debit)),
            ?assertEqual(8, maps:get(wallet_id, Credit)),
            %% tx_type：借记 20 / 贷记 21
            ?assertEqual(20, maps:get(tx_type, Debit)),
            ?assertEqual(21, maps:get(tx_type, Credit)),
            %% 返回体含收款方与双方余额
            {ok, Res} = R,
            ?assertEqual(200, maps:get(to_uid, Res)),
            ?assertEqual(95000, maps:get(owner_balance, Res)),
            ?assertEqual(5000, maps:get(payee_balance, Res))
        end
    ).

%% 超单笔上限（20000 > 10000）→ 拒，绝不预留/结算
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
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) ->
                    put(transferred, true),
                    {ok, #{debit_balance => 1, credit_balance => 1}}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 20000, <<"REF2">>),
            ?assertEqual({error, exceeds_single_limit}, R),
            ?assertEqual(undefined, get(reserved)),
            ?assertEqual(undefined, get(transferred))
        end
    ).

%% 超周期累计上限（try_reserve 原子闸门拒）→ 拒，绝不结算
over_total_limit_test_() ->
    ?WITH_MECKS(
        [
            {agent_payment_mandate_ds, [
                {find_active, 1, fun(100) -> {ok, mandate()} end},
                {try_reserve, 2, fun(555, 5000) -> {error, exceeds_total_limit} end}
            ]},
            {wallet_ds, [
                {find_transaction_by_ref, 1, fun(_) -> #{} end},
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) ->
                    put(transferred, true),
                    {ok, #{debit_balance => 1, credit_balance => 1}}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF3">>),
            ?assertEqual({error, exceeds_total_limit}, R),
            ?assertEqual(undefined, get(transferred))
        end
    ).

%% mandate 过期/撤销（find_active 返回 notfound）→ 拒，绝不结算
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
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) ->
                    put(transferred, true),
                    {ok, #{debit_balance => 1, credit_balance => 1}}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF4">>),
            ?assertEqual({error, mandate_invalid}, R),
            ?assertEqual(undefined, get(reserved)),
            ?assertEqual(undefined, get(transferred))
        end
    ).

%% 收款方=付款人（ToUid=owner_uid 999）→ invalid_payee，绝不预留/结算
payee_is_owner_test_() ->
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
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) ->
                    put(transferred, true),
                    {ok, #{debit_balance => 1, credit_balance => 1}}
                end}
            ]}
        ],
        fun() ->
            clear(),
            %% ToUid=999=owner_uid：自己付给自己（绕过额度的空转）必须挡
            R = agent_payment_logic:pay_with_mandate(100, 999, 5000, <<"REF5">>),
            ?assertEqual({error, invalid_payee}, R),
            ?assertEqual(undefined, get(reserved)),
            ?assertEqual(undefined, get(transferred))
        end
    ).

%% 幂等：同 RefNo 已入账（find_transaction_by_ref 命中）→ 直接返回 ok，
%% 不重复预留、不重复结算
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
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) ->
                    put(transferred, true),
                    {ok, #{debit_balance => 1, credit_balance => 1}}
                end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"DUP">>),
            ?assertMatch({ok, _}, R),
            ?assertEqual(undefined, get(reserved)),
            ?assertEqual(undefined, get(transferred))
        end
    ).

%% 结算失败（余额不足，两腿原子 rollback）→ 释放已预留额度（周期额度不被空耗），返回 error
settle_failure_releases_reservation_test_() ->
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
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) -> {rollback, insufficient_balance} end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF6">>),
            ?assertEqual({error, insufficient_balance}, R),
            ?assertEqual(true, get(released))
        end
    ).

%% 结算 rollback（收款钱包缺失等）归一为 payment_failed，并释放预留
settle_rollback_maps_payment_failed_test_() ->
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
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) -> {rollback, credit_wallet_missing} end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF7">>),
            ?assertEqual({error, payment_failed}, R),
            ?assertEqual(true, get(released))
        end
    ).

%% with_tx 非 rollback 异常重试耗尽返回 {error,Class,Reason}(3元组)等未知形态 →
%% 归一为 payment_failed 且释放预留（H1：不 case_clause 崩溃、不永久泄漏预留额度）
settle_unknown_error_releases_reservation_test_() ->
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
                {ensure_wallet, 1, ensure_wallet_fun()},
                {atomic_transfer, 2, fun(_, _) -> {error, badarg, []} end}
            ]}
        ],
        fun() ->
            clear(),
            R = agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"REF8">>),
            ?assertEqual({error, payment_failed}, R),
            ?assertEqual(true, get(released))
        end
    ).

%% 借记 RefNo 以贷记保留后缀 "_CR" 结尾 → invalid_params（M3：防拒付型唯一索引碰撞 DoS）
reserved_ref_suffix_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_params},
            agent_payment_logic:pay_with_mandate(100, 200, 5000, <<"VICTIM_CR">>)
        )
    end).

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
