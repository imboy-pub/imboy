-module(imboy_plugin_signature_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_signature 的 EUnit 测试（P6 切片 1）
%%%
%%% 覆盖 / Coverage:
%%%   1. generate_keypair: keypair 长度正确（pub 32, priv 32）
%%%   2. sign + verify roundtrip 成功
%%%   3. 错误公钥验证失败
%%%   4. 篡改数据验证失败
%%%   5. 篡改签名验证失败
%%%   6. sign_file + verify_file roundtrip
%%%   7. verify_file 文件不存在返回错误
%%%   8. 签名长度固定 64
%%% @end
%%%-------------------------------------------------------------------

%% Helper: 临时文件
unique_path() ->
    filename:join(
        "/tmp",
        "imboy_plugin_signature_test_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ).

%% ===================================================================
%% 1. generate_keypair
%% ===================================================================

generate_keypair_lengths_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        ?assert(is_binary(Pub)),
        ?assert(is_binary(Priv)),
        ?assertEqual(32, byte_size(Pub)),
        ?assertEqual(32, byte_size(Priv))
    end).

%% ===================================================================
%% 2. sign + verify roundtrip
%% ===================================================================

sign_verify_roundtrip_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        Data = <<"hello plugin world">>,
        {ok, Sig} = imboy_plugin_signature:sign_data(Data, Priv),
        ?assertEqual(ok, imboy_plugin_signature:verify_data(Data, Pub, Sig))
    end).

%% ===================================================================
%% 3. 错误公钥验证失败
%% ===================================================================

verify_with_wrong_pubkey_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, _Pub1, Priv1} = imboy_plugin_signature:generate_keypair(),
        {ok, Pub2, _Priv2} = imboy_plugin_signature:generate_keypair(),
        Data = <<"some data">>,
        {ok, Sig} = imboy_plugin_signature:sign_data(Data, Priv1),
        ?assertEqual(
            {error, signature_invalid},
            imboy_plugin_signature:verify_data(Data, Pub2, Sig)
        )
    end).

%% ===================================================================
%% 4. 篡改数据验证失败
%% ===================================================================

verify_with_tampered_data_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        Original = <<"original content">>,
        Tampered = <<"tampered content">>,
        {ok, Sig} = imboy_plugin_signature:sign_data(Original, Priv),
        ?assertEqual(
            {error, signature_invalid},
            imboy_plugin_signature:verify_data(Tampered, Pub, Sig)
        )
    end).

%% ===================================================================
%% 5. 篡改签名验证失败
%% ===================================================================

verify_with_tampered_sig_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        Data = <<"signed data">>,
        {ok, Sig} = imboy_plugin_signature:sign_data(Data, Priv),
        %% 翻转最后一字节（避免影响 length）
        Last = binary:last(Sig),
        SigBody = binary:part(Sig, 0, byte_size(Sig) - 1),
        TamperedSig = <<SigBody/binary, (Last bxor 16#FF)>>,
        ?assertEqual(
            {error, signature_invalid},
            imboy_plugin_signature:verify_data(Data, Pub, TamperedSig)
        )
    end).

%% ===================================================================
%% 6. sign_file + verify_file roundtrip
%% ===================================================================

sign_verify_file_roundtrip_test_() ->
    {setup,
        fun() ->
            Path = unique_path(),
            ok = file:write_file(Path, <<"plugin manifest content">>),
            Path
        end,
        fun(Path) -> file:delete(Path) end, fun(Path) ->
            {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
            {ok, Sig} = imboy_plugin_signature:sign_file(Path, Priv),
            [
                ?_assertEqual(64, byte_size(Sig)),
                ?_assertEqual(ok, imboy_plugin_signature:verify_file(Path, Pub, Sig))
            ]
        end}.

%% ===================================================================
%% 7. verify_file 文件不存在
%% ===================================================================

verify_file_missing_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        {ok, Sig} = imboy_plugin_signature:sign_data(<<"x">>, Priv),
        %% 不创建
        Path = unique_path(),
        ?assertMatch(
            {error, enoent},
            imboy_plugin_signature:verify_file(Path, Pub, Sig)
        )
    end).

%% ===================================================================
%% 8. sign_data 签名长度固定 64 字节（Ed25519 固定长度）
%% ===================================================================

sign_data_signature_length_64_test_() ->
    ?TEST_SIMPLE(fun() ->
        {ok, _Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        %% 不同长度的输入数据，签名长度都应是 64
        {ok, Sig1} = imboy_plugin_signature:sign_data(<<"a">>, Priv),
        {ok, Sig2} = imboy_plugin_signature:sign_data(<<"longer payload here">>, Priv),
        {ok, Sig3} = imboy_plugin_signature:sign_data(crypto:strong_rand_bytes(10000), Priv),
        ?assertEqual(64, byte_size(Sig1)),
        ?assertEqual(64, byte_size(Sig2)),
        ?assertEqual(64, byte_size(Sig3))
    end).

%% ===================================================================
%% 9. strict 模式（P1 插件市场签名校验）
%%    {imboy, plugin_signature_required} = true 时：
%%    - 无可信公钥 → {error, no_trusted_keys}
%%    - 签名文件缺失 → {error, signature_missing}
%%    默认 false 保持宽松（向后兼容）
%% ===================================================================

strict_mode_test_() ->
    {foreach, fun strict_setup/0, fun strict_cleanup/1, [
        fun strict_missing_sig_rejected/1,
        fun non_strict_missing_sig_ok/1,
        fun strict_no_trusted_keys_rejected/1,
        fun strict_valid_signature_ok/1
    ]}.

strict_setup() ->
    Saved = {
        application:get_env(imboy, plugin_trusted_public_keys),
        application:get_env(imboy, plugin_signature_required)
    },
    application:unset_env(imboy, plugin_trusted_public_keys),
    application:unset_env(imboy, plugin_signature_required),
    Path = unique_path(),
    ok = file:write_file(Path, <<"plugin content">>),
    {Saved, Path}.

strict_cleanup({{Keys, Required}, Path}) ->
    restore_env(plugin_trusted_public_keys, Keys),
    restore_env(plugin_signature_required, Required),
    file:delete(Path),
    file:delete(Path ++ ".sig"),
    ok.

restore_env(Key, undefined) -> application:unset_env(imboy, Key);
restore_env(Key, {ok, V}) -> application:set_env(imboy, Key, V).

%% strict 模式下无签名文件拒绝
strict_missing_sig_rejected({_, Path}) ->
    ?_test(begin
        {ok, Pub, _Priv} = imboy_plugin_signature:generate_keypair(),
        application:set_env(imboy, plugin_trusted_public_keys, [Pub]),
        application:set_env(imboy, plugin_signature_required, true),
        ?assertEqual(
            {error, signature_missing},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% 非 strict 模式下无签名放行（向后兼容，默认行为）
non_strict_missing_sig_ok({_, Path}) ->
    ?_test(begin
        {ok, Pub, _Priv} = imboy_plugin_signature:generate_keypair(),
        application:set_env(imboy, plugin_trusted_public_keys, [Pub]),
        %% 显式 false
        application:set_env(imboy, plugin_signature_required, false),
        ?assertEqual(ok, imboy_plugin_signature:verify_file(Path, Path ++ ".sig")),
        %% 未设置（默认）
        application:unset_env(imboy, plugin_signature_required),
        ?assertEqual(ok, imboy_plugin_signature:verify_file(Path, Path ++ ".sig")),
        %% 无可信公钥也放行
        application:unset_env(imboy, plugin_trusted_public_keys),
        ?assertEqual(ok, imboy_plugin_signature:verify_file(Path, Path ++ ".sig"))
    end).

%% strict 模式无可信公钥拒绝（未配置 / 空列表）
strict_no_trusted_keys_rejected({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, plugin_signature_required, true),
        ?assertEqual(
            {error, no_trusted_keys},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        ),
        application:set_env(imboy, plugin_trusted_public_keys, []),
        ?assertEqual(
            {error, no_trusted_keys},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% strict 模式下签名齐全且有效 → ok；篡改文件 → {error, no_matching_key}
strict_valid_signature_ok({_, Path}) ->
    ?_test(begin
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        application:set_env(imboy, plugin_trusted_public_keys, [Pub]),
        application:set_env(imboy, plugin_signature_required, true),
        {ok, Sig} = imboy_plugin_signature:sign_file(Path, Priv),
        ok = file:write_file(Path ++ ".sig", Sig),
        ?assertEqual(ok, imboy_plugin_signature:verify_file(Path, Path ++ ".sig")),
        ok = file:write_file(Path, <<"tampered content">>),
        ?assertEqual(
            {error, no_matching_key},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% ===================================================================
%% 10. 商务版强制签名（SEC-02，审计 #44）
%%    product_profile = enterprise（IMBOY_PRODUCT_PROFILE 口径，与
%%    deploy/preflight.sh 的商务版一致）时：
%%    - signature_required() 恒为 true，plugin_signature_required=false 无法降级
%%    - 缺可信公钥 → {error, no_trusted_keys}（install 拒绝）
%%    - 缺 SIGNATURE → {error, signature_missing}（install 拒绝）
%%    - 坏签名 / 被篡改 payload → {error, no_matching_key}
%%    - 公钥文件路径配置（plugin_trusted_public_key_files）可提供可信公钥
%%    - 非法 profile 值 fail-closed 按强制处理
%%    community 维持现状：默认宽松，lifecycle 关闭不受影响。
%% ===================================================================

business_enforced_test_() ->
    {foreach, fun business_setup/0, fun business_cleanup/1, [
        fun business_missing_keys_rejected/1,
        fun business_config_false_cannot_downgrade/1,
        fun business_missing_signature_rejected/1,
        fun business_valid_signature_via_key_file_ok/1,
        fun business_bad_signature_rejected/1,
        fun business_tampered_payload_rejected/1,
        fun business_invalid_profile_fail_closed/1,
        fun business_validate_config_matrix/1,
        fun community_stays_lenient/1
    ]}.

business_setup() ->
    Saved = {
        application:get_env(imboy, product_profile),
        application:get_env(imboy, plugin_trusted_public_keys),
        application:get_env(imboy, plugin_trusted_public_key_files),
        application:get_env(imboy, plugin_signature_required)
    },
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [
            product_profile,
            plugin_trusted_public_keys,
            plugin_trusted_public_key_files,
            plugin_signature_required
        ]
    ),
    Path = unique_path(),
    ok = file:write_file(Path, <<"plugin content">>),
    {Saved, Path}.

business_cleanup({{Profile, Keys, KeyFiles, Required}, Path}) ->
    restore_env(product_profile, Profile),
    restore_env(plugin_trusted_public_keys, Keys),
    restore_env(plugin_trusted_public_key_files, KeyFiles),
    restore_env(plugin_signature_required, Required),
    file:delete(Path),
    file:delete(Path ++ ".sig"),
    ok.

%% 商务版缺可信公钥 → 拒绝（不降级放行）
business_missing_keys_rejected({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, enterprise),
        ?assertEqual(true, imboy_plugin_signature:signature_required()),
        ?assertEqual(
            {error, no_trusted_keys},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% 商务版下 plugin_signature_required=false 无法降级
business_config_false_cannot_downgrade({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, enterprise),
        application:set_env(imboy, plugin_signature_required, false),
        ?assertEqual(true, imboy_plugin_signature:signature_required()),
        ?assertEqual(
            {error, no_trusted_keys},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% 商务版有公钥但缺 SIGNATURE 文件 → 拒绝
business_missing_signature_rejected({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, enterprise),
        {ok, Pub, _Priv} = imboy_plugin_signature:generate_keypair(),
        application:set_env(imboy, plugin_trusted_public_keys, [Pub]),
        ?assertEqual(
            {error, signature_missing},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% 商务版 + 公钥文件路径配置 + 有效签名 → 唯一放行面
business_valid_signature_via_key_file_ok({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, enterprise),
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        KeyFile = Path ++ ".pub",
        ok = file:write_file(KeyFile, Pub),
        application:set_env(imboy, plugin_trusted_public_key_files, [KeyFile]),
        {ok, Sig} = imboy_plugin_signature:sign_file(Path, Priv),
        ok = file:write_file(Path ++ ".sig", Sig),
        ?assertEqual(ok, imboy_plugin_signature:verify_file(Path, Path ++ ".sig")),
        file:delete(KeyFile)
    end).

%% 商务版坏签名（非可信 key 签发）→ 拒绝
business_bad_signature_rejected({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, enterprise),
        {ok, Pub, _Priv1} = imboy_plugin_signature:generate_keypair(),
        {_ok, _Pub2, Priv2} = imboy_plugin_signature:generate_keypair(),
        application:set_env(imboy, plugin_trusted_public_keys, [Pub]),
        {ok, Sig} = imboy_plugin_signature:sign_file(Path, Priv2),
        ok = file:write_file(Path ++ ".sig", Sig),
        ?assertEqual(
            {error, no_matching_key},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% 商务版被篡改 payload → 拒绝（签名验证失败不降级放行）
business_tampered_payload_rejected({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, enterprise),
        {ok, Pub, Priv} = imboy_plugin_signature:generate_keypair(),
        application:set_env(imboy, plugin_trusted_public_keys, [Pub]),
        {ok, Sig} = imboy_plugin_signature:sign_file(Path, Priv),
        ok = file:write_file(Path ++ ".sig", Sig),
        ok = file:write_file(Path, <<"tampered plugin content">>),
        ?assertEqual(
            {error, no_matching_key},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% 非法 profile 值（拼错的档位）→ fail-closed 按强制签名处理
business_invalid_profile_fail_closed({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, <<"enterprize-typo">>),
        ?assertEqual(true, imboy_plugin_signature:signature_required()),
        ?assertEqual(
            {error, no_trusted_keys},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        )
    end).

%% validate_config（preflight/启动校验等价断言点）：
%%   enterprise + 缺 key → {error, no_trusted_keys}
%%   enterprise + key 文件（坏内容）加载不出有效公钥 → {error, no_trusted_keys}
%%   enterprise + 有效 key → ok
%%   community → ok（即使无 key）
business_validate_config_matrix({_, Path}) ->
    ?_test(begin
        %% enterprise + 无 key
        application:set_env(imboy, product_profile, enterprise),
        ?assertEqual({error, no_trusted_keys}, imboy_plugin_signature:validate_config()),
        %% enterprise + 坏公钥文件（非 32 字节内容）→ 等同无有效 key
        BadKeyFile = Path ++ ".badpub",
        ok = file:write_file(BadKeyFile, <<"not a key">>),
        application:set_env(imboy, plugin_trusted_public_key_files, [BadKeyFile]),
        ?assertEqual({error, no_trusted_keys}, imboy_plugin_signature:validate_config()),
        %% enterprise + 有效 key → ok
        {ok, Pub, _} = imboy_plugin_signature:generate_keypair(),
        application:set_env(imboy, plugin_trusted_public_keys, [Pub]),
        ?assertEqual(ok, imboy_plugin_signature:validate_config()),
        %% enterprise + 内联非法长度 key（防 crypto badarg crash 面）→ 过滤后无 key
        application:set_env(imboy, plugin_trusted_public_keys, [<<"short">>]),
        application:unset_env(imboy, plugin_trusted_public_key_files),
        ?assertEqual({error, no_trusted_keys}, imboy_plugin_signature:validate_config()),
        ?assertEqual(
            {error, no_trusted_keys},
            imboy_plugin_signature:verify_file(Path, Path ++ ".sig")
        ),
        %% community → ok
        application:set_env(imboy, product_profile, community),
        application:unset_env(imboy, plugin_trusted_public_keys),
        ?assertEqual(ok, imboy_plugin_signature:validate_config()),
        file:delete(BadKeyFile)
    end).

%% community 档维持现状：默认宽松（无 key 无签名均放行）
community_stays_lenient({_, Path}) ->
    ?_test(begin
        application:set_env(imboy, product_profile, community),
        ?assertEqual(false, imboy_plugin_signature:signature_required()),
        ?assertEqual(ok, imboy_plugin_signature:verify_file(Path, Path ++ ".sig"))
    end).
