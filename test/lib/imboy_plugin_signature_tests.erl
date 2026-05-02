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
        ?assertEqual({error, signature_invalid},
                     imboy_plugin_signature:verify_data(Data, Pub2, Sig))
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
        ?assertEqual({error, signature_invalid},
                     imboy_plugin_signature:verify_data(Tampered, Pub, Sig))
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
        ?assertEqual({error, signature_invalid},
                     imboy_plugin_signature:verify_data(Data, Pub, TamperedSig))
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
     fun(Path) -> file:delete(Path) end,
     fun(Path) ->
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
        Path = unique_path(),  %% 不创建
        ?assertMatch({error, enoent},
                     imboy_plugin_signature:verify_file(Path, Pub, Sig))
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
