-module(login_attempt_ds_tests_simple).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% cache_key/2 测试 - 不需要 mock
%% ===================================================================

cache_key_combines_identifier_and_ip_test_() ->
    ?TEST_SIMPLE(fun() ->
        Identifier = <<"test@example.com">>,
        Ip = <<"127.0.0.1">>,

        Key = login_attempt_ds:cache_key(Identifier, Ip),

        % 键应该包含标识符和IP
        ?assert(is_binary(Key)),
        ?assertEqual(<<"test@example.com:127.0.0.1">>, Key)
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
        ?assertEqual(<<":127.0.0.1">>, Key)
    end).

cache_key_with_empty_ip_test_() ->
    ?TEST_SIMPLE(fun() ->
        Key = login_attempt_ds:cache_key(<<"test@example.com">>, <<>>),
        ?assertEqual(<<"test@example.com:">>, Key)
    end).
