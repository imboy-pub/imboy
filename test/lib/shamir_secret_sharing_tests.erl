-module(shamir_secret_sharing_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc Shamir Secret Sharing 测试
%%%
%%% 测试 (k, n) 门限秘密共享方案的实现
%%%===================================================================

%%===================================================================
%%% 测试函数
%%===================================================================

%% @doc 测试分割秘密
split_secret_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = <<"test_secret">>,
        N = 5,
        K = 3,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),

        ?assertEqual(length(Shares), N),
        ?assert(lists:all(fun(S) -> is_map(S) end, Shares)),
        ?assert(lists:all(fun(S) -> maps:is_key(index, S) end, Shares)),
        ?assert(lists:all(fun(S) -> maps:is_key(x, S) end, Shares)),
        ?assert(lists:all(fun(S) -> maps:is_key(y, S) end, Shares))
    end).

%% @doc 测试重组秘密
combine_shares_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 使用 32 字节秘密（与前端一致）
        Secret = crypto:strong_rand_bytes(32),
        N = 5,
        K = 3,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),

        % 只使用 K 个分片来重建
        SharesToCombine = lists:sublist(Shares, 1, K),
        RecoveredSecret = shamir_secret_sharing:combine_shares(SharesToCombine),

        ?assertEqual(RecoveredSecret, Secret)
    end).

%% @doc 测试使用不同的分片组合重建秘密
combine_different_combinations_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 使用 32 字节秘密（与前端一致）
        Secret = crypto:strong_rand_bytes(32),
        N = 5,
        K = 2,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),

        % 测试不同的分片组合
        Combinations = [
            {1, 2},
            {2, 3},
            {3, 4},
            {4, 5},
            {1, 5}
        ],

        lists:foreach(fun({I, J}) ->
            S1 = lists:nth(I, Shares),
            S2 = lists:nth(J, Shares),
            RecoveredSecret = shamir_secret_sharing:combine_shares([S1, S2]),
            ?assertEqual(RecoveredSecret, Secret)
        end, Combinations)
    end).

%% @doc 测试短秘密
short_secret_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试一个短于 32 字节的秘密
        % 算法会填充到 32 字节，然后恢复时仍然是填充后的版本
        Secret = <<1, 2, 3, 4, 5>>,
        N = 3,
        K = 2,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),
        RecoveredSecret = shamir_secret_sharing:combine_shares(
            lists:sublist(Shares, 1, K)
        ),

        % 算法总是返回 32 字节，验证最后 5 个字节匹配原始秘密
        <<_:27/binary, Last5:5/binary>> = RecoveredSecret,
        ?assertEqual(Secret, Last5)
    end).

%% @doc 测试大秘密
large_secret_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 使用 32 字节秘密（算法的标准大小）
        Secret = crypto:strong_rand_bytes(32),
        N = 5,
        K = 3,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),
        RecoveredSecret = shamir_secret_sharing:combine_shares(
            lists:sublist(Shares, 1, K)
        ),

        ?assertEqual(RecoveredSecret, Secret)
    end).

%% @doc 测试参数验证 - N <= K
invalid_parameters_n_lte_k_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = <<"test">>,

        ?assertError(
            {invalid_parameters, "N must be greater than K"},
            shamir_secret_sharing:split_secret(Secret, 3, 3)
        )
    end).

%% @doc 测试参数验证 - K < 2
invalid_parameters_k_lt_2_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = <<"test">>,

        ?assertError(
            {invalid_parameters, "K must be at least 2"},
            shamir_secret_sharing:split_secret(Secret, 3, 1)
        )
    end).

%% @doc 测试重组秘密 - 分片数量不足
combine_shares_insufficient_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertError(
            {invalid_parameters, "At least 2 shares are required"},
            shamir_secret_sharing:combine_shares([])
        )
    end).

%% @doc 测试分片一致性
share_consistency_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = <<"consistency_test">>,
        N = 5,
        K = 3,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),

        % 验证所有分片的 x 值都是唯一的
        XValues = [maps:get(x, S) || S <- Shares],
        UniqueXValues = lists:usort(XValues),

        ?assertEqual(length(UniqueXValues), N)
    end).
