-module(e2ee_social_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc E2EE 社交恢复 DS 层测试
%%%
%%% 测试目标：
%%% - 验证可信联系人管理功能
%%% - 验证密钥分片创建和管理
%%% - 验证缓存逻辑
%%%===================================================================

%% ===================================================================
%% 可信联系人管理测试
%% ===================================================================

add_trusted_contact_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {is_friend, 2, fun(_Uid, _ContactUid) -> true end}
        ]},
        {e2ee_social_repo, [
            {add_contact, 1, fun(_ContactMap) -> {ok, 999} end}
        ]},
        {imboy_cache, [
            {delete, 1, fun(_CacheKey) -> ok end}
        ]}
    ], fun() ->
        Uid = 10001,
        ContactUid = 10002,
        Nickname = <<"Alice"/utf8>>,

        Result = e2ee_social_ds:add_trusted_contact(Uid, ContactUid, Nickname),

        ?assertEqual(ok, Result)
    end).

add_trusted_contact_self_test_() ->
    ?WITH_MECK(friend_ds, [
        {is_friend, 2, fun(_Uid, _ContactUid) -> true end}
    ], fun() ->
        Uid = 10001,
        ContactUid = 10001,  % 自己

        Result = e2ee_social_ds:add_trusted_contact(Uid, ContactUid, <<"Test"/utf8>>),

        ?assertMatch({error, cannot_add_self}, Result)
    end).

add_trusted_contact_not_friend_test_() ->
    ?WITH_MECK(friend_ds, [
        {is_friend, 2, fun(_Uid, _ContactUid) -> false end}
    ], fun() ->
        Uid = 10001,
        ContactUid = 10002,

        Result = e2ee_social_ds:add_trusted_contact(Uid, ContactUid, <<"Test"/utf8>>),

        ?assertMatch({error, not_friend}, Result)
    end).

remove_trusted_contact_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {remove_contact, 2, fun(_Uid, _ContactUid) -> ok end}
        ]},
        {imboy_cache, [
            {delete, 1, fun(_CacheKey) -> ok end}
        ]}
    ], fun() ->
        Uid = 10001,
        ContactUid = 10002,

        Result = e2ee_social_ds:remove_trusted_contact(Uid, ContactUid),

        ?assertEqual(ok, Result)
    end).

list_trusted_contacts_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {list_contacts, 1, fun(_Uid) ->
                {ok, [
                    #{<<"uid">> => 10001, <<"contact_uid">> => 10002},
                    #{<<"uid">> => 10001, <<"contact_uid">> => 10003}
                ]}
            end}
        ]},
        {imboy_cache, [
            {get, 1, fun(_CacheKey) -> undefined end},
            {set, 3, fun(_CacheKey, _Contacts, _TTL) -> ok end}
        ]}
    ], fun() ->
        Uid = 10001,

        Result = e2ee_social_ds:list_trusted_contacts(Uid),

        ?assertMatch({ok, [_, _]}, Result)
    end).

list_trusted_contacts_cached_test_() ->
    ?WITH_MECK(imboy_cache, [
        {get, 1, fun(_CacheKey) ->
            {ok, [#{<<"uid">> => 10001, <<"contact_uid">> => 10002}]}
        end}
    ], fun() ->
        Uid = 10001,

        Result = e2ee_social_ds:list_trusted_contacts(Uid),

        ?assertMatch({ok, [_]}, Result)
    end).

is_trusted_contact_test_() ->
    ?WITH_MECK(imboy_cache, [
        {get, 1, fun(_CacheKey) ->
            {ok, [#{<<"contact_uid">> => 10002}]}
        end}
    ], fun() ->
        Uid = 10001,
        ContactUid = 10002,

        Result = e2ee_social_ds:is_trusted_contact(Uid, ContactUid),

        ?assertEqual(true, Result)
    end).

is_trusted_contact_false_test_() ->
    ?WITH_MECK(imboy_cache, [
        {get, 1, fun(_CacheKey) ->
            {ok, [#{<<"contact_uid">> => 10003}]}
        end}
    ], fun() ->
        Uid = 10001,
        ContactUid = 10002,

        Result = e2ee_social_ds:is_trusted_contact(Uid, ContactUid),

        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% 密钥分片管理测试
%% ===================================================================

create_key_shares_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {is_friend, 2, fun(_Uid, _ContactUid) -> true end}
        ]},
        {shamir_secret_sharing, [
            {create_shares, 3, fun(_PrivateKeyPem, _Threshold, _TotalShards) ->
                [
                    #{<<"x">> => 1, <<"y">> => <<"share1">>},
                    #{<<"x">> => 2, <<"y">> => <<"share2">>},
                    #{<<"x">> => 3, <<"y">> => <<"share3">>}
                ]
            end}
        ]},
        {user_device_ds, [
            {get_default_device, 1, fun(_ProxyUid) ->
                {ok, #{<<"public_key">> => <<"mock-public-key">>}}
            end}
        ]},
        {elib_cipher, [
            {encrypt_rsa_oaep, 2, fun(_Data, _PublicKey) ->
                {ok, <<"encrypted-data">>}
            end}
        ]},
        {e2ee_social_repo, [
            {generate_shard_id, 0, fun() -> <<"shard-123">> end},
            {create, 1, fun(_ShardRecord) -> {ok, 999} end}
        ]},
        {imboy_cache, [
            {delete, 1, fun(_CacheKey) -> ok end}
        ]}
    ], fun() ->
        Uid = 10001,
        Proxies = [{10002, <<"Bob"/utf8>>}, {10003, <<"Charlie"/utf8>>}, {10004, <<"Dave"/utf8>>}],
        PrivateKeyPem = <<"mock-private-key">>,
        TotalShards = 3,
        Threshold = 2,

        Result = e2ee_social_ds:create_key_shares(
            Uid, Proxies, PrivateKeyPem, TotalShards, Threshold
        ),

        ?assertMatch({ok, [_, _, _]}, Result)
    end).

create_key_shares_insufficient_proxies_test_() ->
    ?WITH_MECK(friend_ds, [
        {is_friend, 2, fun(_Uid, _ContactUid) -> true end}
    ], fun() ->
        Uid = 10001,
        Proxies = [{10002, <<"Bob"/utf8>>}],  % 只有一个代理，需要3个分片
        PrivateKeyPem = <<"mock-private-key">>,
        TotalShards = 3,
        Threshold = 2,

        Result = e2ee_social_ds:create_key_shares(
            Uid, Proxies, PrivateKeyPem, TotalShards, Threshold
        ),

        ?assertMatch({error, insufficient_proxies}, Result)
    end).

create_key_shares_invalid_threshold_test_() ->
    ?WITH_MECK(friend_ds, [
        {is_friend, 2, fun(_Uid, _ContactUid) -> true end}
    ], fun() ->
        Uid = 10001,
        Proxies = [{10002, <<"Bob"/utf8>>}, {10003, <<"Charlie"/utf8>>}, {10004, <<"Dave"/utf8>>}],
        PrivateKeyPem = <<"mock-private-key">>,
        TotalShards = 3,
        Threshold = 5,  % 阈值大于总分片数

        Result = e2ee_social_ds:create_key_shares(
            Uid, Proxies, PrivateKeyPem, TotalShards, Threshold
        ),

        ?assertMatch({error, invalid_threshold}, Result)
    end).

get_user_shards_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {get_user_shards, 2, fun(_Uid, _KeyVersion) ->
                {ok, [
                    #{<<"shard_id">> => <<"shard-1">>, <<"uid">> => 10001},
                    #{<<"shard_id">> => <<"shard-2">>, <<"uid">> => 10001}
                ]}
            end}
        ]},
        {imboy_cache, [
            {get, 1, fun(_CacheKey) -> undefined end},
            {set, 3, fun(_CacheKey, _Shards, _TTL) -> ok end}
        ]}
    ], fun() ->
        Uid = 10001,
        KeyVersion = <<"latest">>,

        Result = e2ee_social_ds:get_user_shards(Uid, KeyVersion),

        ?assertMatch({ok, [_, _]}, Result)
    end).

get_proxy_shards_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {get_proxy_shards, 1, fun(_ProxyUid) ->
                {ok, [
                    #{<<"shard_id">> => <<"shard-1">>, <<"proxy_uid">> => 10002}
                ]}
            end}
        ]},
        {imboy_cache, [
            {get, 1, fun(_CacheKey) -> undefined end},
            {set, 3, fun(_CacheKey, _Shards, _TTL) -> ok end}
        ]}
    ], fun() ->
        ProxyUid = 10002,

        Result = e2ee_social_ds:get_proxy_shards(ProxyUid),

        ?assertMatch({ok, [_]}, Result)
    end).

get_shard_by_id_test_() ->
    ?WITH_MECK(e2ee_social_repo, [
        {find_shard_by_id, 1, fun(_ShardId) ->
            {ok, #{<<"shard_id">> => <<"shard-123">>, <<"uid">> => 10001}}
        end}
    ], fun() ->
        Uid = 10001,
        ShardId = <<"shard-123">>,

        Result = e2ee_social_ds:get_shard_by_id(Uid, ShardId),

        ?assertMatch({ok, _}, Result)
    end).

get_shard_by_id_unauthorized_test_() ->
    ?WITH_MECK(e2ee_social_repo, [
        {find_shard_by_id, 1, fun(_ShardId) ->
            {ok, #{<<"shard_id">> => <<"shard-123">>, <<"uid">> => 10002}}
        end}
    ], fun() ->
        Uid = 10001,  % 不同的用户
        ShardId = <<"shard-123">>,

        Result = e2ee_social_ds:get_shard_by_id(Uid, ShardId),

        ?assertMatch({error, unauthorized}, Result)
    end).

decrypt_shard_test_() ->
    ?WITH_MECKS([
        {elib_cipher, [
            {decrypt_rsa_oaep, 2, fun(_EncryptedShard, _PrivateKeyPem) ->
                {ok, <<"{\"x\":1,\"y\":\"share-data\"}">>}
            end}
        ]},
        {jsx, [
            {decode, 2, fun(Json, _Options) ->
                #{<<"x">> => 1, <<"y">> => <<"share-data">>}
            end}
        ]}
    ], fun() ->
        EncryptedShard = <<"encrypted-shard-data">>,
        PrivateKeyPem = <<"mock-private-key">>,

        Result = e2ee_social_ds:decrypt_shard(EncryptedShard, PrivateKeyPem),

        ?assertMatch({ok, _}, Result)
    end).

decrypt_shard_failure_test_() ->
    ?WITH_MECK(elib_cipher, [
        {decrypt_rsa_oaep, 2, fun(_EncryptedShard, _PrivateKeyPem) ->
            error(decryption_failed)
        end}
    ], fun() ->
        EncryptedShard = <<"invalid-shard">>,
        PrivateKeyPem = <<"mock-private-key">>,

        Result = e2ee_social_ds:decrypt_shard(EncryptedShard, PrivateKeyPem),

        ?assertMatch({error, decryption_failed}, Result)
    end).

recover_key_insufficient_shares_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {get_user_shards, 2, fun(_Uid, _KeyVersion) ->
                {ok, [
                    #{<<"shard_id">> => <<"shard-1">>, <<"threshold">> => 2},
                    #{<<"shard_id">> => <<"shard-2">>, <<"threshold">> => 2}
                ]}
            end}
        ]},
        {imboy_cache, [
            {get, 1, fun(_CacheKey) -> undefined end},
            {set, 3, fun(_CacheKey, _Shards, _TTL) -> ok end}
        ]}
    ], fun() ->
        Uid = 10001,
        KeyVersion = <<"latest">>,
        ShardIds = [<<"shard-1">>],  % 只有一个分片，阈值是2

        Result = e2ee_social_ds:recover_key(Uid, KeyVersion, ShardIds),

        ?assertMatch({error, insufficient_shares}, Result)
    end).

can_recover_test_() ->
    ?WITH_MECK(e2ee_social_repo, [
        {can_recover, 2, fun(_Uid, _KeyVersion) -> {ok, true} end}
    ], fun() ->
        Uid = 10001,
        KeyVersion = <<"latest">>,

        Result = e2ee_social_ds:can_recover(Uid, KeyVersion),

        ?assertMatch({ok, true}, Result)
    end).

delete_restored_shards_test_() ->
    ?WITH_MECKS([
        {e2ee_social_repo, [
            {delete_restored_shards, 2, fun(_Uid, _KeyVersion) -> ok end}
        ]},
        {imboy_cache, [
            {delete, 1, fun(_CacheKey) -> ok end}
        ]}
    ], fun() ->
        Uid = 10001,
        KeyVersion = <<"latest">>,

        Result = e2ee_social_ds:delete_restored_shards(Uid, KeyVersion),

        ?assertEqual(ok, Result)
    end).
