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

%% 生产装配路径（env 形状 → decode → KeyRef）与 managed_crypto 的完整链路：
%% active 版本可 seal/open；旧版本密文在 key_version=1 的 KeyRef 下可 open（轮换兼容）；
%% 未知版本在 KeyRef 下拒绝。
%%
%% 注意：不走 `-ifdef(TEST)` 的 synthetic 构造器——src 侧模块在 eunit 构建下
%% 不带 TEST 宏编译（synthetic 函数不存在，undef），且 decode/1 才是生产真实
%% 装配入口，用它构造同时覆盖「hex 环境 → 32 字节密钥」的解码段。
synthetic_keyring_managed_crypto_roundtrip_test() ->
    {ok, Ref} = eb_env_keyring:decode(valid_env()),
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

%% ===================================================================
%% F6 装配入口 resolve_key_ref（RULING-2026-09-15 §七）
%% ===================================================================

%% 显式注入（map）优先，且不读 env：env 即使是坏的也不影响显式值。
assembly_explicit_injection_wins_test() ->
    Explicit = #{key => crypto:strong_rand_bytes(32), key_version => 3},
    ?assertEqual(Explicit, eb_env_keyring:resolve_key_ref(Explicit)),
    ok = application:set_env(imboy, eb_enterprise_keyring, <<"not-a-map">>),
    ?assertEqual(Explicit, eb_env_keyring:resolve_key_ref(Explicit)),
    _ = application:unset_env(imboy, eb_enterprise_keyring),
    ok.

%% 无显式注入 → 从 env 解析 active key_ref（输出即 managed_crypto 的 KeyRef）。
assembly_env_fallback_resolves_active_test() ->
    Key1 = crypto:strong_rand_bytes(32),
    Key2 = crypto:strong_rand_bytes(32),
    Env = #{
        active_version => 2,
        keys => #{1 => hex_of(Key1), 2 => hex_of(Key2)}
    },
    ok = application:set_env(imboy, eb_enterprise_keyring, Env),
    try
        ?assertEqual(
            #{keys => #{1 => Key1, 2 => Key2}, key_version => 2},
            eb_env_keyring:resolve_key_ref(undefined)
        )
    after
        _ = application:unset_env(imboy, eb_enterprise_keyring)
    end,
    ok.

%% 轮换：active 换版后新写用 active；旧版本密钥仍在环里，旧版本 KeyRef 仍可解
%% 旧密文（新 KeyRef 解旧密文拒绝——版本绑定语义，managed_crypto 不改）。
assembly_rotation_new_writes_use_active_old_version_still_opens_test() ->
    Key1 = crypto:strong_rand_bytes(32),
    Key2 = crypto:strong_rand_bytes(32),
    Aad = #{organization_id => 11, workspace_id => 12, conversation_id => 13, message_id => 14},
    %% v1 active：装配解析出 v1，密文 key_version = 1
    ok = application:set_env(imboy, eb_enterprise_keyring, #{
        active_version => 1, keys => #{1 => hex_of(Key1)}
    }),
    RefV1 = eb_env_keyring:resolve_key_ref(undefined),
    {ok, SealedV1} = eb_managed_crypto:seal(Aad, <<"f6-rotation">>, RefV1),
    ?assertEqual(1, maps:get(key_version, SealedV1)),
    %% 轮换：环里追加 v2 并把 active 指向 2 —— 新写用 v2
    ok = application:set_env(imboy, eb_enterprise_keyring, #{
        active_version => 2, keys => #{1 => hex_of(Key1), 2 => hex_of(Key2)}
    }),
    RefV2 = eb_env_keyring:resolve_key_ref(undefined),
    {ok, SealedV2} = eb_managed_crypto:seal(Aad, <<"f6-rotation-v2">>, RefV2),
    ?assertEqual(2, maps:get(key_version, SealedV2)),
    %% 旧密文：旧版本 KeyRef 仍可解（历史可读）；新 active ref 拒绝（版本不符 fail-closed）
    RefV1After = RefV2#{key_version => 1},
    {ok, <<"f6-rotation">>} = eb_managed_crypto:open(Aad, SealedV1, RefV1After),
    ?assertMatch(
        {error, {key_version_mismatch, 2, 1}}, eb_managed_crypto:open(Aad, SealedV1, RefV2)
    ),
    {ok, <<"f6-rotation-v2">>} = eb_managed_crypto:open(Aad, SealedV2, RefV2),
    _ = application:unset_env(imboy, eb_enterprise_keyring),
    ok.

%% 无 keyring（未配置）→ fail-closed 为 undefined（下游 seal missing_key → 500）；
%% 客户端形态的 binary（乃至任何非 map）不被采信，同样只走 env。
assembly_no_keyring_fails_closed_test() ->
    _ = application:unset_env(imboy, eb_enterprise_keyring),
    ?assertEqual(undefined, eb_env_keyring:resolve_key_ref(undefined)),
    ?assertEqual(undefined, eb_env_keyring:resolve_key_ref(<<"attacker-key-ref">>)),
    %% 下游语义不变：undefined → seal 拒绝（missing_key），绝不降级明文
    Aad = #{organization_id => 1, workspace_id => 2, conversation_id => 3, message_id => 4},
    ?assertMatch({error, missing_key}, eb_managed_crypto:seal(Aad, <<"x">>, undefined)),
    ok.

%% 坏形状 env（缺 active / 非 map）→ 同样 fail-closed 为 undefined，不炸不降级。
assembly_invalid_env_fails_closed_test() ->
    ok = application:set_env(imboy, eb_enterprise_keyring, #{active_version => 1}),
    ?assertEqual(undefined, eb_env_keyring:resolve_key_ref(undefined)),
    ok = application:set_env(imboy, eb_enterprise_keyring, <<"garbage">>),
    ?assertEqual(undefined, eb_env_keyring:resolve_key_ref(undefined)),
    _ = application:unset_env(imboy, eb_enterprise_keyring),
    ok.

%% 失败面不泄密：错误项只含原因原子/版本号，任何可打印表示都不含密钥字节；
%% 模块自身零日志（静态红线：无 logger/error_logger/io/file 调用）。
assembly_failure_leaks_no_key_material_test() ->
    KeyHex = binary:encode_hex(crypto:strong_rand_bytes(32), lowercase),
    ok = application:set_env(imboy, eb_enterprise_keyring, #{
        active_version => 9, keys => #{1 => KeyHex}
    }),
    try
        %% active 缺失（9 ∉ keys）→ resolve undefined → seal missing_key
        Ref = eb_env_keyring:resolve_key_ref(undefined),
        Err = eb_managed_crypto:seal(
            #{organization_id => 1, workspace_id => 2, conversation_id => 3, message_id => 4},
            <<"secret-body">>,
            Ref
        ),
        ?assertMatch({error, missing_key}, Err),
        Printed = iolist_to_binary(io_lib:format("~0p", [Err])),
        ?assertEqual(nomatch, binary:match(Printed, KeyHex)),
        ?assertEqual(nomatch, binary:match(Printed, <<"secret-body">>))
    after
        _ = application:unset_env(imboy, eb_enterprise_keyring)
    end,
    {ok, Src} = file:read_file(
        "src/features/enterprise_business/infrastructure/eb_env_keyring.erl"
    ),
    lists:foreach(
        fun(Needle) ->
            ?assertEqual({Needle, nomatch}, {Needle, string:find(Src, Needle)})
        end,
        [<<"logger">>, <<"error_logger">>, <<"io:format">>, <<"file:">>]
    ),
    ok.
