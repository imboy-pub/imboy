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

%% #94 收尾防线：把 init_config_legacy_cbc 置 off 之后，solidified_key_iv 已无
%% 用途。早先版本无条件强校验 IV 必须 16 字节，会在关开关的当天直接
%% erlang:error 崩掉 /api/v1/init，打断全部客户端初始化。
init_legacy_off_tolerates_missing_iv_test_() ->
    Mocks = lists:keyreplace(
        config_ds,
        1,
        init_mocks(<<"off">>),
        {config_ds, [
            {'env', 1, fun
                (solidified_key) -> <<"sol_key">>;
                %% IV 未配置（sys.config 默认就是空 binary）
                (solidified_key_iv) -> <<>>;
                (login_rsa_pub_key) -> <<"rsa_pub">>
            end},
            {'env', 2, fun
                (ws_url, _D) -> <<"wss://example.test/ws">>;
                (upload_url, _D) -> <<"https://example.test/upload">>;
                (upload_key, _D) -> <<"upload_key">>;
                (upload_scene, _D) -> <<"upload_scene">>;
                (login_pwd_rsa_encrypt, _D) -> false;
                (init_config_legacy_cbc, _D) -> <<"off">>
            end}
        ]}
    ),
    ?WITH_MECKS(Mocks, fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = index_handler:init(Req, #{action => init}),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(<<"gcm_bin">>, maps:get(res_v2, Payload)),
        ?assertNot(maps:is_key(res, Payload)),
        _ = erase(captured_init_data),
        ok
    end).

%% LegacyCbc = default（取 env 默认值 <<"on">>）| <<"off">>
%% RsaFlag = login_pwd_rsa_encrypt 配置返回值（false = 模拟配置缺失）
init_mocks(LegacyCbc) ->
    init_mocks(LegacyCbc, false).

init_mocks(LegacyCbc, RsaFlag) ->
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
                    RsaFlag;
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

%% H2 真机走查发现①（2026-08-30）：ws_url 未配置时按请求 Host 同源派生
%% （ws/wss 随 X-Forwarded-Proto/scheme），杜绝本地/内网部署残留旧机器
%% IP 的 stale 配置让客户端 WS 假成功后静默失败。显式配置永远优先
%% （生产可指向独立网关/CDN）。
%%
%% 结构说明：直接生成 {Desc, {setup, S, C, [?_test(...)]}}——
%% "{Desc, fun() -> 返回 setup 对象 end}" 包装式会让 EUnit 把返回值
%% 丢弃、内层断言从不执行（M-6 空转判绿同类陷阱；本次以故意错断言
%% 证伪后改造，同文件 init_login_pwd_rsa_flag_normalized 一并修复）。
init_ws_url_fallback_test_() ->
    Cases = [
        {<<"未配置→按 Host 派生 ws 同源">>, <<"192.168.2.79:9800">>, <<>>, <<>>,
            <<"ws://192.168.2.79:9800/api/v1/ws">>},
        {<<"X-Forwarded-Proto https→派生 wss">>, <<"imboy.example">>, <<"https">>, <<>>,
            <<"wss://imboy.example/api/v1/ws">>},
        {<<"显式配置优先于派生">>, <<"192.168.2.79:9800">>, <<>>, <<"wss://gateway.example/ws">>,
            <<"wss://gateway.example/ws">>}
    ],
    lists:map(
        fun({Desc, Host, Proto, WsConfig, Expect}) ->
            Mocks = ws_url_mocks(Host, Proto, WsConfig),
            {Desc,
                {setup, fun() -> install_mocks(Mocks) end, fun(_) -> cleanup_mocks(Mocks) end, [
                    ?_test(begin
                        Req = mock_request(),
                        {ok, RespReq, _State} = index_handler:init(Req, #{action => init}),
                        ?assertEqual(200, maps:get(response_status, RespReq)),
                        InitData = erase(captured_init_data),
                        ?assertEqual(Expect, maps:get(<<"ws_url">>, InitData))
                    end)
                ]}}
        end,
        Cases
    ).

ws_url_mocks(Host, Proto, WsConfig) ->
    Base = init_mocks(<<"off">>),
    M0 = lists:keyreplace(
        cowboy_req,
        1,
        Base,
        {cowboy_req, [
            {'header', 3, fun
                (<<"vsn">>, _R, _D) -> <<"1.0.0">>;
                (<<"cos">>, _R, _D) -> <<"ios">>;
                (<<"pkg">>, _R, _D) -> <<"com.imboy.test">>;
                (<<"sk">>, _R, D) -> D;
                (<<"host">>, _R, _D) -> Host;
                (<<"x-forwarded-proto">>, _R, _D) -> Proto;
                (_N, _R, D) -> D
            end},
            {'scheme', 1, fun(_R) -> http end}
        ]}
    ),
    lists:keyreplace(
        config_ds,
        1,
        M0,
        {config_ds, [
            {'env', 1, fun
                (solidified_key) -> <<"sol_key">>;
                (solidified_key_iv) -> <<"0123456789abcdef">>;
                (login_rsa_pub_key) -> <<"rsa_pub">>
            end},
            {'env', 2, fun
                (ws_url, _D) -> WsConfig;
                (upload_url, _D) -> <<"https://example.test/upload">>;
                (upload_key, _D) -> <<"upload_key">>;
                (upload_scene, _D) -> <<"upload_scene">>;
                (login_pwd_rsa_encrypt, _D) -> false;
                (init_config_legacy_cbc, _D) -> <<"off">>
            end}
        ]}
    ).

install_mocks(Mocks) ->
    lists:foreach(fun({M, E}) -> meck_helper:setup_mock(M, E) end, Mocks).

cleanup_mocks(Mocks) ->
    lists:foreach(fun({M, _}) -> meck_helper:cleanup_mock(M) end, Mocks).

%% #100：login_pwd_rsa_encrypt 必须归一为线协议 1/0 下发。
%% 客户端加密判定与服务端 safe_rsa_decrypt 都只认 <<"1">>；
%% 此前透传配置原值 on/off，配置为 on 时加密分支静默失效（客户端
%% 判非 "1" 不加密、回传 "on" 服务端也不解密——两端恰好都不报错）。
%% 结构说明：原 {Desc, fun() -> 返回 setup 对象 end} 为 M-6 空转形态
%%（断言从未执行，故意错断言亦全绿，2026-08-30 证伪后改造）。
init_login_pwd_rsa_flag_normalized_test_() ->
    Cases = [
        {<<"config=on 下发 1（激活两端 RSA 链路）">>, <<"on">>, <<"1">>},
        {<<"config=1 下发 1">>, <<"1">>, <<"1">>},
        {<<"config=off 下发 0">>, <<"off">>, <<"0">>},
        {<<"配置缺失（env 返 false）下发 0">>, false, <<"0">>}
    ],
    lists:map(
        fun({Desc, RsaFlag, Expect}) ->
            Mocks = init_mocks(<<"off">>, RsaFlag),
            {Desc,
                {setup, fun() -> install_mocks(Mocks) end, fun(_) -> cleanup_mocks(Mocks) end, [
                    ?_test(begin
                        Req = mock_request(),
                        {ok, RespReq, _State} = index_handler:init(Req, #{action => init}),
                        ?assertEqual(200, maps:get(response_status, RespReq)),
                        %% login_pwd_rsa_encrypt 在加密前的 Data map 内，
                        %% 须断言捕获的 InitData（外层 payload 只有 res/res_v2）
                        InitData = erase(captured_init_data),
                        ?assertEqual(
                            Expect, maps:get(<<"login_pwd_rsa_encrypt">>, InitData)
                        )
                    end)
                ]}}
        end,
        Cases
    ).
