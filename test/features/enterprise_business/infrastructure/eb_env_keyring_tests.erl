%%% @doc F6（RULING-2026-09-15 §七）：`eb_env_keyring` 判据单测 + 与
%%% `eb_managed_crypto` 的轮换/未知版本/拒绝链路。
%%%
%%% 纯函数（无 DB、无 config 依赖——decode/1 直接吃形状），`make eunit` 可跑；
%%% synthetic 经 eb_env_keyring:synthetic_test_key_ref/0（仅 TEST 构建存在）。
-module(eb_env_keyring_tests).

-include_lib("eunit/include/eunit.hrl").

hex_of(Bytes) ->
    binary:encode_hex(Bytes, lowercase).

valid_env() ->
    #{
        active_version => 2,
        keys => #{
            1 => hex_of(crypto:strong_rand_bytes(32)),
            2 => hex_of(crypto:strong_rand_bytes(32))
        }
    }.

%% 合法形状：解码成功且输出即 eb_managed_crypto 的 KeyRef 形状
decode_valid_test() ->
    Env = valid_env(),
    {ok, #{keys := Ring, key_version := 2}} = eb_env_keyring:decode(Env),
    ?assertEqual(2, map_size(Ring)),
    [?assertEqual(32, byte_size(K)) || K <- maps:values(Ring)].

%% 缺配置 / 非 map -> fail-closed
decode_unavailable_test() ->
    ?assertEqual({error, keyring_unavailable}, eb_env_keyring:decode(undefined)),
    ?assertEqual({error, keyring_unavailable}, eb_env_keyring:decode(<<"v3">>)).

%% 缺 active_version / 缺 keys -> fail-closed
decode_missing_parts_test() ->
    ?assertEqual(
        {error, active_version_missing}, eb_env_keyring:decode(#{keys => #{1 => hex_of(<<0:256>>)}})
    ),
    ?assertEqual({error, keys_missing}, eb_env_keyring:decode(#{active_version => 1})).

%% active 版本不在 keys -> fail-closed
decode_active_absent_test() ->
    Env = (valid_env())#{active_version => 9},
    ?assertMatch({error, {active_version_missing, 9}}, eb_env_keyring:decode(Env)).

%% 非法编码（非 hex）/ 非 32 字节 -> fail-closed 并点名版本
decode_bad_encoding_test() ->
    Env = (valid_env())#{keys => (maps:get(keys, valid_env()))#{1 => <<"zz-not-hex">>}},
    ?assertMatch({error, {invalid_key_encoding, 1}}, eb_env_keyring:decode(Env)),
    Env2 = (valid_env())#{keys => (maps:get(keys, valid_env()))#{2 => hex_of(<<0:128>>)}},
    ?assertMatch({error, {invalid_key_length, 2}}, eb_env_keyring:decode(Env2)).

%% 版本条目非法（非正整数版本 / 值非 binary）-> fail-closed
decode_bad_entries_test() ->
    Env = (valid_env())#{
        active_version => 1,
        keys => maps:remove(2, (maps:get(keys, valid_env()))#{0 => hex_of(<<0:256>>)})
    },
    ?assertMatch({error, {invalid_key_version_entry, 0}}, eb_env_keyring:decode(Env)).

%% synthetic keyring（TEST 构建）与 managed_crypto 的完整链路：
%% active 版本可 seal/open；旧版本密文在 key_version=1 的 KeyRef 下可 open（轮换兼容）；
%% 未知版本在 KeyRef 下拒绝。
synthetic_keyring_managed_crypto_roundtrip_test() ->
    Ref = eb_env_keyring:synthetic_test_key_ref(),
    Aad = #{organization_id => 7, workspace_id => 8, conversation_id => 9, message_id => 10},
    {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"f6-roundtrip">>, Ref),
    ?assertEqual(2, maps:get(key_version, Sealed)),
    {ok, <<"f6-roundtrip">>} = eb_managed_crypto:open(Aad, Sealed, Ref),
    %% 轮换兼容：用 v1 的 KeyRef seal 的密文，在 v1 KeyRef 下可开、在 v2 下拒
    RefV1 = Ref#{key_version => 1},
    {ok, SealedV1} = eb_managed_crypto:seal(Aad, <<"f6-v1">>, RefV1),
    {ok, <<"f6-v1">>} = eb_managed_crypto:open(Aad, SealedV1, RefV1),
    ?assertMatch(
        {error, {key_version_mismatch, 2, 1}},
        eb_managed_crypto:open(Aad, SealedV1, Ref)
    ),
    %% 未知版本：密文自称 v3 而 KeyRef active=2 —— 版本不符 fail-closed
    %%（open 先比对 sealed 与 ref 版本，再取 key；两道门都不会放行未知版本）
    ?assertMatch(
        {error, {key_version_mismatch, 2, 3}},
        eb_managed_crypto:open(
            Aad,
            #{
                alg => <<"aes-256-gcm">>,
                key_version => 3,
                cipher => <<0:256>>,
                aad_hash => <<"00">>
            },
            Ref
        )
    ).
