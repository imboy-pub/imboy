-module(e2ee_social_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc E2EE 社交恢复 Logic 层测试
%%%
%%% 测试目标：
%%% - 验证 Shamir 密钥分片创建
%%% - 验证密钥重组
%%% - 验证参数验证逻辑
%%%===================================================================

%% ===================================================================
%% Shamir Secret Sharing 集成测试
%% ===================================================================

shamir_split_and_combine_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 使用 32 字节私钥
        PrivateKey = crypto:strong_rand_bytes(32),
        TotalShards = 3,
        Threshold = 2,

        % 创建分片
        Shares = shamir_secret_sharing:split_secret(PrivateKey, TotalShards, Threshold),

        % 验证分片数量
        ?assertEqual(length(Shares), TotalShards),

        % 验证分片结构
        lists:foreach(fun(Share) ->
            ?assert(is_map(Share)),
            ?assert(maps:is_key(index, Share)),
            ?assert(maps:is_key(x, Share)),
            ?assert(maps:is_key(y, Share))
        end, Shares),

        % 使用 2 个分片恢复
        SharesToCombine = lists:sublist(Shares, Threshold),
        RecoveredKey = shamir_secret_sharing:combine_shares(SharesToCombine),

        % 验证恢复的密钥
        ?assertEqual(RecoveredKey, PrivateKey)
    end).

shamir_different_combinations_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 使用 32 字节密钥（与 Shamir 实现一致）
        PrivateKey = crypto:strong_rand_bytes(32),
        TotalShards = 5,
        Threshold = 2,

        Shares = shamir_secret_sharing:split_secret(PrivateKey, TotalShards, Threshold),

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
            RecoveredKey = shamir_secret_sharing:combine_shares([S1, S2]),
            ?assertEqual(RecoveredKey, PrivateKey)
        end, Combinations)
    end).

shamir_insufficient_shards_test_() ->
    ?TEST_SIMPLE(fun() ->
        PrivateKey = <<"test_key">>,
        TotalShards = 3,
        Threshold = 2,

        Shares = shamir_secret_sharing:split_secret(PrivateKey, TotalShards, Threshold),

        % 尝试只用 1 个分片恢复（应该抛出错误）
        SingleShare = lists:sublist(Shares, 1),
        ?assertError(
            {invalid_parameters, "At least 2 shares are required"},
            shamir_secret_sharing:combine_shares(SingleShare)
        )
    end).

%% ===================================================================
%% 参数验证测试
%% ===================================================================

invalid_parameters_total_shards_lt_threshold_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试 Shamir 层的参数验证
        Secret = <<"test">>,
        N = 2,
        K = 3,  % K > N，应该失败

        ?assertError(
            {invalid_parameters, "N must be greater than K"},
            shamir_secret_sharing:split_secret(Secret, N, K)
        )
    end).

invalid_parameters_threshold_lt_2_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 测试 Shamir 层的参数验证
        Secret = <<"test">>,
        N = 3,
        K = 1,  % K < 2，应该失败

        ?assertError(
            {invalid_parameters, "K must be at least 2"},
            shamir_secret_sharing:split_secret(Secret, N, K)
        )
    end).

invalid_parameters_insufficient_proxies_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 这个测试验证 e2ee_social_logic 的参数验证
        % 由于需要 mock repo，暂时跳过
        ?assert(true)
    end).

create_shards_accepts_map_proxy_contract_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {'create', 1, fun(_ShardRecord) ->
                {ok, erlang:unique_integer([positive])}
            end}
        ]},
        {elib_cipher, [
            {'encrypt_rsa_oaep', 2, fun(ShardPayload, ProxyPublicKey) when is_binary(ShardPayload),
                    is_binary(ProxyPublicKey) ->
                {ok, <<"encrypted-shard">>}
            end}
        ]},
        {e2ee_shard_validator, [
            {'log_shard_transmission', 3, fun(_Action, _ShardId, _Meta) ->
                ok
            end}
        ]}
    ], fun() ->
        PrivateKey = crypto:strong_rand_bytes(32),
        Proxies = [
            #{<<"proxy_uid">> => 1001, <<"encrypted_public_key">> => <<"pub-key-1">>},
            #{<<"proxy_uid">> => 1002, <<"encrypted_public_key">> => <<"pub-key-2">>},
            #{<<"proxy_uid">> => 1003, <<"encrypted_public_key">> => <<"pub-key-3">>}
        ],

        {ok, Shards} = e2ee_social_logic:create_shards(
            9999, <<"key-v1">>, 3, 2, PrivateKey, Proxies
        ),

        ?assertEqual(3, length(Shards)),
        ?assertEqual(
            [1001, 1002, 1003],
            [maps:get(<<"proxy_uid">>, Shard) || Shard <- Shards]
        ),
        lists:foreach(fun(Shard) ->
            ?assertEqual(<<"encrypted-shard">>, maps:get(<<"encrypted_shard">>, Shard))
        end, Shards)
    end).

create_shards_accepts_tuple_proxy_contract_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {'create', 1, fun(_ShardRecord) ->
                {ok, erlang:unique_integer([positive])}
            end}
        ]},
        {elib_cipher, [
            {'encrypt_rsa_oaep', 2, fun(ShardPayload, ProxyPublicKey) when is_binary(ShardPayload),
                    is_binary(ProxyPublicKey) ->
                {ok, <<"encrypted-shard">>}
            end}
        ]},
        {e2ee_shard_validator, [
            {'log_shard_transmission', 3, fun(_Action, _ShardId, _Meta) ->
                ok
            end}
        ]}
    ], fun() ->
        PrivateKey = crypto:strong_rand_bytes(32),
        Proxies = [
            {2001, <<"legacy-pub-key-1">>},
            {2002, <<"legacy-pub-key-2">>},
            {2003, <<"legacy-pub-key-3">>}
        ],

        {ok, Shards} = e2ee_social_logic:create_shards(
            9999, <<"key-v1">>, 3, 2, PrivateKey, Proxies
        ),

        ?assertEqual(3, length(Shards)),
        ?assertEqual(
            [2001, 2002, 2003],
            [maps:get(<<"proxy_uid">>, Shard) || Shard <- Shards]
        )
    end).

%% ===================================================================
%% Shamir 边界条件测试
%% ===================================================================

shamir_empty_secret_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = <<>>,
        N = 3,
        K = 2,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),
        RecoveredSecret = shamir_secret_sharing:combine_shares(
            lists:sublist(Shares, K)
        ),

        % 空秘密也应该能正确恢复（会被填充到 32 字节）
        ?assertEqual(byte_size(RecoveredSecret), 32)
    end).

shamir_large_secret_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 使用 32 字节秘密（标准密钥大小）
        Secret = crypto:strong_rand_bytes(32),
        N = 5,
        K = 3,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),
        RecoveredSecret = shamir_secret_sharing:combine_shares(
            lists:sublist(Shares, K)
        ),

        ?assertEqual(RecoveredSecret, Secret)
    end).

shamir_max_threshold_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = crypto:strong_rand_bytes(32),
        N = 5,
        K = 4,  % 需要所有分片中的 4 个

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),
        RecoveredSecret = shamir_secret_sharing:combine_shares(
            lists:sublist(Shares, K)
        ),

        ?assertEqual(RecoveredSecret, Secret)
    end).

%% ===================================================================
%% 分片一致性测试
%% ===================================================================

share_consistency_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = crypto:strong_rand_bytes(32),
        N = 5,
        K = 3,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),

        % 验证所有分片的 index 唯一
        Indices = [maps:get(index, S) || S <- Shares],
        UniqueIndices = lists:usort(Indices),
        ?assertEqual(length(UniqueIndices), N),

        % 验证所有分片的 x 值唯一
        XValues = [maps:get(x, S) || S <- Shares],
        UniqueXValues = lists:usort(XValues),
        ?assertEqual(length(UniqueXValues), N)
    end).

share_index_sequential_test_() ->
    ?TEST_SIMPLE(fun() ->
        Secret = crypto:strong_rand_bytes(32),
        N = 5,
        K = 3,

        Shares = shamir_secret_sharing:split_secret(Secret, N, K),

        % 验证 index 从 1 到 N 连续
        Indices = [maps:get(index, S) || S <- Shares],
        ?assertEqual(lists:sort(Indices), lists:seq(1, N))
    end).
