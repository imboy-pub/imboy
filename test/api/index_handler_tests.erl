-module(index_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% index_handler 模块的 EUnit 测试
%%%
%%% 目标：验证首页处理器功能
%%%===================================================================

handle_index_init_success_test_() ->
    ?TEST_WITH_APP(fun() ->
        Mods = [
            cowboy_req,
            config_ds,
            app_version_ds,
            elib_hasher,
            jsone,
            elib_cipher,
            elib_pg,
            elib_response
        ],
        lists:foreach(fun(Mod) -> ok = meck:new(Mod, [unstick, non_strict]) end, Mods),
        SolKey = <<"sol_key">>,
        IV = <<"sol_key_iv">>,
        Encoded = <<"encoded_json">>,
        Cipher = <<"cipher_bin">>,
        QueryRes = {ok, mocked_query},
        meck:expect(cowboy_req, header, fun
            (<<"vsn">>, _Req, _Default) -> <<"1.0.0">>;
            (<<"cos">>, _Req, _Default) -> <<"ios">>;
            (<<"pkg">>, _Req, _Default) -> <<"com.imboy.test">>;
            (<<"sk">>, _Req, Default) -> Default;
            (_Name, _Req, Default) -> Default
        end),
        meck:expect(config_ds, get, fun
            (solidified_key) -> SolKey;
            (solidified_key_iv) -> IV;
            ("ws_url") -> <<"ws://example.test/ws">>;
            ("upload_url") -> <<"https://example.test/upload">>;
            ("upload_key") -> <<"upload_key">>;
            ("upload_scene") -> <<"upload_scene">>;
            ("login_pwd_rsa_encrypt") -> false;
            ("login_rsa_pub_key") -> <<"rsa_pub">>;
            (_) -> undefined
        end),
        meck:expect(app_version_ds, sign_key, fun(_DType, _SignKeyVsn, _Pkg) -> undefined end),
        meck:expect(elib_hasher, md5, fun(_SolKey) -> <<"md5_key">> end),
        meck:expect(jsone, encode, fun(_Data) -> Encoded end),
        meck:expect(elib_cipher, aes_encrypt, fun(aes_256_cbc, _Encoded, _IV) -> Cipher end),
        meck:expect(elib_pg, query, fun(_Sql, _Args) -> QueryRes end),
        meck:expect(elib_response, success, fun(Req, Payload, Msg) -> {success, Req, Payload, Msg} end),
        try
            Req0 = #{},
            {ok, {success, Req0, Payload, "success."}, State} =
                index_handler:init(Req0, #{action => init}),
            ?assertEqual(#{}, State),
            ?assertMatch(#{res := Cipher, test := QueryRes}, Payload),
            ?assert(lists:all(fun(Mod) -> meck:validate(Mod) end, Mods))
        after
            lists:foreach(fun(Mod) -> meck:unload(Mod) end, Mods)
        end
    end).
