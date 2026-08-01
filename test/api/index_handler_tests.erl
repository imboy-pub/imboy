-module(index_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"GET">>, path => <<"/init">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = index_handler:init(Req, #{action => false}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{}, State)
    end).

init_success_test_() ->
    ?WITH_MECKS(
        init_mocks(default),
        fun() ->
            Req = mock_request(),
            {ok, RespReq, State} = index_handler:init(Req, #{action => init}),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(<<"cipher_bin">>, maps:get(res, Payload)),
            %% #94：res_v2 = AES-256-GCM（AEAD 有认证 + 随机 IV）。
            %% 过渡期两者并存，存量客户端读 res、新客户端读 res_v2。
            ?assertEqual(<<"gcm_bin">>, maps:get(res_v2, Payload)),
            ?assertEqual(#{<<"lexeme">> => <<"ok">>}, maps:get(test, Payload)),
            ?assertEqual(#{}, State),
            InitData = erase(captured_init_data),
            ?assertEqual(
                <<"/api/v1/attachment/presign">>,
                maps:get(<<"attach_presign_endpoint">>, InitData)
            )
        end
    ).

%% #94：开关置 off 后必须只下发 res_v2。只要 res 还在，攻击者就能走旧的
%% CBC 无认证路径，加固形同虚设 —— 这条断言就是防止有人日后把 off 分支
%% 改回"两个都发"。
init_legacy_cbc_off_omits_res_test_() ->
    ?WITH_MECKS(
        init_mocks(<<"off">>),
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = index_handler:init(Req, #{action => init}),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(<<"gcm_bin">>, maps:get(res_v2, Payload)),
            ?assertNot(maps:is_key(res, Payload)),
            _ = erase(captured_init_data),
            ok
        end
    ).

%% LegacyCbc = default（取 env 默认值 <<"on">>）| <<"off">>
init_mocks(LegacyCbc) ->
    [
        {cowboy_req, [
            {'header', 3, fun
                (<<"vsn">>, _Req, _Default) -> <<"1.0.0">>;
                (<<"cos">>, _Req, _Default) -> <<"ios">>;
                (<<"pkg">>, _Req, _Default) -> <<"com.imboy.test">>;
                (<<"sk">>, _Req, Default) -> Default;
                (_Name, _Req, Default) -> Default
            end}
        ]},
        {config_ds, [
            %% env/1 — 无 default，用于 solidified_key、login_rsa_pub_key 等
            {'env', 1, fun
                (solidified_key) -> <<"sol_key">>;
                (solidified_key_iv) -> <<"0123456789abcdef">>;
                (login_rsa_pub_key) -> <<"rsa_pub">>
            end},
            %% env/2 — 带 default，用于客户端 init 配置项
            {'env', 2, fun
                (ws_url, _D) ->
                    <<"wss://example.test/ws">>;
                (upload_url, _D) ->
                    <<"https://example.test/upload">>;
                (upload_key, _D) ->
                    <<"upload_key">>;
                (upload_scene, _D) ->
                    <<"upload_scene">>;
                (login_pwd_rsa_encrypt, _D) ->
                    false;
                %% 过渡期开关：default 即 env 默认值 <<"on">>（res + res_v2 并存）
                (init_config_legacy_cbc, D) ->
                    case LegacyCbc of
                        default -> D;
                        V -> V
                    end
            end}
        ]},
        {app_version_ds, [
            {'sign_key', 3, fun(<<"ios">>, <<"1.0.0">>, <<"com.imboy.test">>) ->
                <<>>
            end}
        ]},
        {elib_hasher, [
            {'md5', 1, fun(<<"sol_key">>) ->
                % AES-256-CBC 需要 32 字节 key；返回恰好 32 字节二进制
                <<"0123456789abcdef0123456789abcdef">>
            end}
        ]},
        {jsone, [
            {'encode', 1, fun(Data) ->
                %% 捕获实际编码的 Data，供下方断言 attach_presign_endpoint
                %% 真实路由值（回归 2026-07-08 硬切换 /api 前缀漏改的 bug：
                %% 曾误留 /v1/attachment/presign，客户端拿到会 404）。
                put(captured_init_data, Data),
                <<"encoded_json">>
            end}
        ]},
        {elib_cipher, [
            {'aes_encrypt', 4, fun(
                aes_256_cbc,
                <<"encoded_json">>,
                <<"0123456789abcdef0123456789abcdef">>,
                <<"0123456789abcdef">>
            ) ->
                <<"cipher_bin">>
            end},
            {'aes_gcm_encrypt', 2, fun(
                <<"encoded_json">>, <<"0123456789abcdef0123456789abcdef">>
            ) ->
                {ok, <<"gcm_bin">>}
            end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, []) ->
                {ok, [#{<<"lexeme">> => <<"ok">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ].
