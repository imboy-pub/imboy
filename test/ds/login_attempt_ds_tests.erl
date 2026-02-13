-module(login_attempt_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("log.hrl").
-include("common.hrl").

%%%===================================================================
%%% @doc
%%% login_attempt_ds 模块的 EUnit 测试
%%%
%%% 目标：验证登录失败次数限制功能
%%% 覆盖：记录失败、检查限制、重置计数、锁定时间计算
%%%===================================================================

%% ===================================================================
%% record_failure/2 测试
%% ===================================================================

record_failure_increments_count_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 5, fun(_Key, _Data, _TTL, _Depend, _Server) -> ok end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        % 第一次失败
        {ok, 1} = login_attempt_ds:record_failure(Identifier, Ip),

        % 验证缓存被调用
        ?assert(meck:called(imboy_cache, set, ['_', '_', '_', '_', '_']))
     end).

record_failure_returns_current_count_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 2, <<"first_fail_at">> => 1234567890}} end},
        {'set', 5, fun(_Key, _Data, _TTL, _Depend, _Server) -> ok end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        % 缓存中已有2次失败，应该返回3
        {ok, 3} = login_attempt_ds:record_failure(Identifier, Ip)
     end).

%% ===================================================================
%% is_locked/2 测试
%% ===================================================================

is_locked_returns_false_when_no_failures_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        false = login_attempt_ds:is_locked(Identifier, Ip)
     end).

is_locked_returns_false_when_below_limit_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 3, <<"first_fail_at">> => elib_dt:now()}} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1640995200000 end},
            {'add', 2, fun(_Time, _Duration) -> 1640995200000 + 1800000 end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        % 3次失败，未达到限制（默认5次）
        false = login_attempt_ds:is_locked(Identifier, Ip)
     end).

is_locked_returns_true_when_at_limit_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 5, <<"first_fail_at">> => 1640995200000}} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1640995200000 end},
            {'add', 2, fun(_Time, _Duration) -> 1640995200000 + 1800000 end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        % 5次失败，达到限制
        true = login_attempt_ds:is_locked(Identifier, Ip)
     end).

is_locked_returns_false_after_lock_duration_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 5, <<"first_fail_at">> => 1640995200000}} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1640997000000 end},  % 31分钟后
            {'add', 2, fun(_Time, _Duration) -> 1640995200000 + 1800000 end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        % 锁定时间已过期
        false = login_attempt_ds:is_locked(Identifier, Ip)
     end).

%% ===================================================================
%% get_attempts/2 测试
%% ===================================================================

get_attempts_returns_zero_when_no_failures_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, 0} = login_attempt_ds:get_attempts(Identifier, Ip)
     end).

get_attempts_returns_count_when_exists_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 3, <<"first_fail_at">> => 1234567890}} end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, 3} = login_attempt_ds:get_attempts(Identifier, Ip)
     end).

%% ===================================================================
%% reset/2 测试
%% ===================================================================

reset_clears_failure_count_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'flush', 1, fun(_Key) -> ok end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        ok = login_attempt_ds:reset(Identifier, Ip),

        % 验证缓存被清除
        ?assert(meck:called(imboy_cache, flush, ['_']))
     end).

%% ===================================================================
%% get_remaining_attempts/2 测试
%% ===================================================================

get_remaining_attempts_returns_max_when_no_failures_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, 5} = login_attempt_ds:get_remaining_attempts(Identifier, Ip)
     end).

get_remaining_attempts_returns_decremented_count_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 2, <<"first_fail_at">> => 1234567890}} end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, 3} = login_attempt_ds:get_remaining_attempts(Identifier, Ip)
     end).

get_remaining_attempts_returns_zero_when_locked_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, #{<<"count">> => 5, <<"first_fail_at">> => 1640995200000}} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1640995200000 end},
            {'add', 2, fun(_Time, _Duration) -> 1640995200000 + 1800000 end}
        ]}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        {ok, 0} = login_attempt_ds:get_remaining_attempts(Identifier, Ip)
     end).

%% ===================================================================
%% cache_key/2 测试
%% ===================================================================

cache_key_combines_identifier_and_ip_test_() ->
    ?TEST_SIMPLE(fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        Key = login_attempt_ds:cache_key(Identifier, Ip),

        % 键应该包含标识符和IP
        ?assert(is_binary(Key)),
        ?assert(byte_size(Key) > 0)
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

%% ===================================================================
%% 边界条件测试
%% ===================================================================

record_failure_with_empty_identifier_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 5, fun(_Key, _Data, _TTL, _Depend, _Server) -> ok end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        Identifier = <<>>,
        Ip = <<"127.0.0.1">>,

        % 应该正常处理空标识符
        {ok, 1} = login_attempt_ds:record_failure(Identifier, Ip)
     end).

record_failure_with_empty_ip_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'set', 5, fun(_Key, _Data, _TTL, _Depend, _Server) -> ok end},
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<>>,

        % 应该正常处理空IP
        {ok, 1} = login_attempt_ds:record_failure(Identifier, Ip)
     end).

is_locked_with_both_empty_inputs_test_() ->
    ?WITH_MECK(imboy_cache, [
        {'get', 1, fun(_Key) -> undefined end}
    ], fun() ->
        Identifier = <<>>,
        Ip = <<>>,

        % 空输入不应该被锁定
        false = login_attempt_ds:is_locked(Identifier, Ip)
     end).
