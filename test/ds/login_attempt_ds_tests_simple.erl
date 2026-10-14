-module(login_attempt_ds_tests_simple).
-moduledoc "Limiter V1 纯函数与兼容层轻量测试（A1）：cache_key 兼容、身份归一。".
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% cache_key/2 兼容层测试（旧导出保留；新实现不写 Cache，仅
%% login_security_logic:build_lock_info 的历史读取路径仍在引用）
%% ===================================================================

cache_key_combines_identifier_and_ip_test_() ->
    ?TEST_SIMPLE(fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,
        Key = login_attempt_ds:cache_key(Identifier, Ip),
        ?assertEqual({login_attempt, Identifier, Ip}, Key)
    end).

cache_key_is_deterministic_test_() ->
    ?TEST_SIMPLE(fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,
        Key1 = login_attempt_ds:cache_key(Identifier, Ip),
        Key2 = login_attempt_ds:cache_key(Identifier, Ip),
        ?assertEqual(Key1, Key2)
    end).

cache_key_is_different_for_different_inputs_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key1 = login_attempt_ds:cache_key(<<"user1@test.com">>, <<"127.0.0.1">>),
        Key2 = login_attempt_ds:cache_key(<<"user2@test.com">>, <<"127.0.0.1">>),
        ?assertNotEqual(Key1, Key2)
    end).

cache_key_with_empty_identifier_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = login_attempt_ds:cache_key(<<>>, <<"127.0.0.1">>),
        ?assertEqual({login_attempt, <<>>, <<"127.0.0.1">>}, Key)
    end).

cache_key_with_empty_ip_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = login_attempt_ds:cache_key(<<"test@example.com">>, <<>>),
        ?assertEqual({login_attempt, <<"test@example.com">>, <<>>}, Key)
    end).

%% ===================================================================
%% ETS 行键合同（C.1）：{limiter_account, Identity} / {limiter_admit, S, K}
%% 通过真实 owner 写入后的只读观测验证键形状（owner 是唯一写者）。
%% ===================================================================

-define(LOG_MOCK,
    {elib_log, [
        {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
    ]}
).
-define(CLOCK_MOCK,
    {login_attempt_clock, [{'now_ms', 0, fun limiter_test_support:clock_now/0}]}
).

row_keys_follow_frozen_contract_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            limiter_test_support:clock_init(),
            limiter_test_support:ensure_owner(),
            I = {unknown, account, <<"simple-row@t.test">>},
            {ok, _} = login_attempt_ds:failure(I),
            {ok, _} = login_attempt_ds:admit(passport_per_ip, <<"10.9.9.1">>),
            ?assertMatch(
                [{_, #{count := 1}}],
                ets:lookup(login_attempt_ets, {limiter_account, I})
            ),
            ?assertMatch(
                [{_, #{count := 1}}],
                ets:lookup(
                    login_attempt_ets,
                    {limiter_admit, passport_per_ip, <<"10.9.9.1">>}
                )
            )
        end)}.

%% 已知用户身份（{user, Uid}）与未知身份互不冲突、可同域并存。
user_identity_key_isolation_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            limiter_test_support:clock_init(),
            limiter_test_support:ensure_owner(),
            U = {user, 424242},
            N = {unknown, account, <<"424242">>},
            {ok, _} = login_attempt_ds:failure(U),
            {ok, #{count := 1}} = login_attempt_ds:failure(N),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:snapshot(U)),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:snapshot(N)),
            ok = login_attempt_ds:reset(U),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:snapshot(N)),
            ?assertEqual(1, limiter_test_support:account_rows())
        end)}.
