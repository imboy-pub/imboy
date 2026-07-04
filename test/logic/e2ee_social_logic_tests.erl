-module(e2ee_social_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc E2EE 社交恢复 Logic 层测试
%%%
%%% 零信任契约：客户端本地完成 Shamir 分割与代理公钥加密，服务端只存储密文分片。
%%% Shamir 分割原语（shamir_secret_sharing）及其单测已作为死代码移除
%%% （服务端不再有任何接触明文私钥的路径），本文件只覆盖现行 create_shards/4
%%% 与 consume_proxy_shard/2 的存储/审计/校验逻辑。
%%%===================================================================

%% 零信任契约：客户端已完成 Shamir 分割与加密，服务端只存储密文分片。
%% 旧的 create_shards/6（服务端接收明文私钥做分割）已删除，
%% 本测试对应现行 create_shards/3。
create_shards_stores_client_split_shards_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_social_ds, [
                {'is_trusted_contact', 2, fun(9999, ProxyUid) ->
                    lists:member(ProxyUid, [1001, 1002, 1003])
                end},
                {'create_shard', 1, fun(_ShardRecord) ->
                    {ok, erlang:unique_integer([positive])}
                end}
            ]},
            {e2ee_shard_validator, [
                {'log_shard_transmission', 3, fun(shard_created, _ShardId, Meta) when
                    is_map(Meta)
                ->
                    ok
                end}
            ]},
            {elib_tsid, [
                {'generate', 0, fun() -> erlang:unique_integer([positive]) end}
            ]}
        ],
        fun() ->
            Shards = [
                #{<<"proxy_uid">> => 1001, <<"encrypted_shard">> => <<"cipher-1">>},
                #{<<"proxy_uid">> => 1002, <<"encrypted_shard">> => <<"cipher-2">>},
                #{<<"proxy_uid">> => 1003, <<"encrypted_shard">> => <<"cipher-3">>}
            ],

            {ok, Persisted} = e2ee_social_logic:create_shards(9999, <<"key-v1">>, 2, Shards),

            ?assertEqual(3, length(Persisted)),
            ?assertEqual(
                [1001, 1002, 1003],
                [maps:get(<<"proxy_uid">>, S) || S <- Persisted]
            ),
            %% 恢复门限必须落库真值（曾硬编码 0 使 can_recover 门限形同虚设）
            ?assertEqual([2, 2, 2], [maps:get(<<"threshold">>, S) || S <- Persisted]),
            %% 每个分片都必须写审计日志
            ?assertEqual(3, meck:num_calls(e2ee_shard_validator, log_shard_transmission, 3))
        end
    ).

create_shards_rejects_untrusted_proxy_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_social_ds, [
                {'is_trusted_contact', 2, fun(_, _) -> false end}
            ]}
        ],
        fun() ->
            Shards = [#{<<"proxy_uid">> => 8888, <<"encrypted_shard">> => <<"cipher">>}],
            ?assertMatch(
                {error, {_, _}}, e2ee_social_logic:create_shards(9999, <<"key-v1">>, 2, Shards)
            )
        end
    ).

create_shards_rejects_invalid_threshold_test_() ->
    ?TEST_SIMPLE(fun() ->
        Shards = [
            #{<<"proxy_uid">> => 1001, <<"encrypted_shard">> => <<"c1">>},
            #{<<"proxy_uid">> => 1002, <<"encrypted_shard">> => <<"c2">>}
        ],
        %% 门限 0（旧硬编码值）、1、超过分片总数 均拒绝
        lists:foreach(
            fun(BadThreshold) ->
                ?assertMatch(
                    {error, {_, _}},
                    e2ee_social_logic:create_shards(9999, <<"key-v1">>, BadThreshold, Shards)
                )
            end,
            [0, 1, 3, <<"2">>]
        )
    end).

consume_proxy_shard_marks_used_and_audits_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_social_ds, [
                {'consume_proxy_shard', 2, fun(<<"shard-777">>, 1001) ->
                    {ok, #{
                        <<"uid">> => 9999,
                        <<"key_version">> => <<"key-v1">>,
                        <<"encrypted_shard">> => <<"cipher">>
                    }}
                end}
            ]},
            {e2ee_shard_validator, [
                {'log_shard_transmission', 3, fun(shard_decrypted, <<"shard-777">>, Meta) ->
                    ?assertEqual(9999, maps:get(<<"uid">>, Meta)),
                    ?assertEqual(1001, maps:get(<<"proxy_uid">>, Meta)),
                    ok
                end}
            ]}
        ],
        fun() ->
            {ok, Shard} = e2ee_social_logic:consume_proxy_shard(<<"shard-777">>, 1001),
            ?assertEqual(<<"cipher">>, maps:get(<<"encrypted_shard">>, Shard)),
            ?assertEqual(1, meck:num_calls(e2ee_shard_validator, log_shard_transmission, 3))
        end
    ).
