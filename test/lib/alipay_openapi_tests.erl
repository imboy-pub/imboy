-module(alipay_openapi_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc alipay_openapi 模块的 EUnit 测试（APP 支付宝登录服务端对接）
%%%
%%% 覆盖：证书 SN 算法（app_cert_sn / alipay_root_cert_sn，对照 Python
%%%       参考实现 fzlee/alipay 预算值）/ oauth_token 公共参数与 RSA2
%%%       签名可验 / 证书模式参数注入 / user_info_share 字段透传 /
%%%       业务错误码、HTTP 错误、坏 JSON、未配置凭据 四类失败分支。
%%% 说明：httpc 用 meck 模拟（不触网）；签名验证用测试密钥对的公钥
%%%       独立重建签名串后交叉验签。
%%%===================================================================

%% ---- 测试密钥对/证书（openssl 现造，无敏感材料）----
%% 期望 SN 由 fzlee/alipay 同算法预算（cryptography 库）：
%%   app  : c6792ca66fa9a71c300599b9c2b2c6c2
%%   root : efbf4d7effc3cc2e7472ccc05a1b4cf4_6ff4ced82ceac86f86305aac4d8c971c
-define(TEST_APP_SN, <<"c6792ca66fa9a71c300599b9c2b2c6c2">>).
-define(TEST_ROOT_SN,
    <<"efbf4d7effc3cc2e7472ccc05a1b4cf4_6ff4ced82ceac86f86305aac4d8c971c">>
).

test_priv_key() ->
    <<
        "-----BEGIN PRIVATE KEY-----\n"
        "MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCzgDD3tA1FJwjH\n"
        "sRpuE7EyCvA2zN5qrmsJy1AnDkR0Y7zpAf1dDSzMSnRrx8U/XPknzSzUYXkUOOfy\n"
        "RsEN1yq+JitPyJwIspuSP+wUhkZwvpX2tOkUgdE7X4irpwCNZ5hDDez+fcYUoUop\n"
        "cdDCCEScZ2wr7oAg1lIDh6qhujKdfS04AP1HyKNQGNOKiLeVI2tZ/xNYaFrZaf/r\n"
        "aeRq4csClEpJBSd8SxirlKGujvWnTHZSgIYlyMWvKFsubptFahPLRc4LCSbt9N/s\n"
        "DJ61Pltk6+8rc2Tq0Jvs7IfaIH8MAPge9i8RptrcgAJqt8Qi4RtdS+i1BABfw9ol\n"
        "1T0RViv3AgMBAAECggEACSmgq5mT7n+7lwCXVMnJ4Gq72vrs2jflBtmgpx23U37z\n"
        "2auxqdpOUG9MhLlB0aYxTF17eFaGvl2D7tNlxLXDB3RzVDNCo78E6fDmtXV/4FQa\n"
        "MOmzLB95oAo/CQ678dE5QyhRfKbMQalUsSDk2G1g8zWO8/8h/eByzJ3+5m/WvZXc\n"
        "SR9U8Ydkmy4e2wNa0wkP1N2J+Z15XqgnyG/WMHET1fKMLzDASHFmoL8NAR2UpD/l\n"
        "lNkoOFth6+yIYcpSUFy3Qcl1H2LZQowV+pkcxVt9GhyQZdVeABOr9+5YdDLAFHLi\n"
        "HIV5G2Ce8+qV82To3y79YKTDEVkVUp8xUs+R95c9oQKBgQDetDadgbah7u1ILH0M\n"
        "hzF2Ks1344yKr6xfNner6Q49KyyPmfe3DbAGGwSwlY6JHOnfaE5JYSbeyLK1xHcy\n"
        "hvNJPiTBqhkXj/QaTMrbszdUTwWGGV2ohJ2p10EQuTSF4BinSHCKybHwYClhNGo4\n"
        "oK1YtClTDuACcSe1HWcXG3UZ8wKBgQDOVmpOKIGRWtbn0wKMd0Bn0tnyqDkmEIYj\n"
        "q+T1p4hVsi9YmXO3Wv0YUXrKAaR1yZqrXcs0Yzi8Z9puV7MkLT5lcw5bl++AG8ho\n"
        "34FGPK0nGndPEqoAjG9ykv4vNHiNlpxpXxLVFvNpRW3aMuvG/IzKv9OiI7s0PGvj\n"
        "K6pJvHHC7QKBgBdoCXV0dKWAXx/+zQXMD77DsOSw3cyVuiAnsVw4Gm8DYqOVIxvy\n"
        "LFX38P4+OcTfbRU1+URtGZDAt0ezZnPBC0Dfby2LmyeG0bkR6SA+LcqGo6X8dIOJ\n"
        "rKh1HzmKvaJ0cyLh0jJEEJebA75bq+5XtOBlzxITtieLjQjUFkmvcRdRAoGAfvjs\n"
        "d+YzPUhCIOxXkCq+JmW5GrrsWAEpGoBn58dNfTamLgZYEAfc4X8aaX6zPoxuMnAI\n"
        "YNeCDWZ3IxmysltpHzK7LiAEELiIDgkj2x3xi7OcobshhC7nUgEki9XlLXP2El2g\n"
        "ECMMbMZ9wB0/u/ajiQycZhVI2O8pkOTI6JEfDE0CgYEAxTLHs0WEbr6L3Ts/O7sQ\n"
        "KQHnKw9dUfO8d2PUT0ny9kihVkezMOEkJa1zNbQ6wDznqP5NluP/zs719Fa7N9QF\n"
        "VsZ+O0Mw7QwNV7dcIqZnxsXPCZ7P64odq/RGl44nGC3thI8jsuf9gTyOYM8/v1pz\n"
        "Cs2T5lTGOarAiCV5xKyF+3M=\n"
        "-----END PRIVATE KEY-----"
    >>.

test_pub_key() ->
    <<
        "-----BEGIN PUBLIC KEY-----\n"
        "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAs4Aw97QNRScIx7EabhOx\n"
        "MgrwNszeaq5rCctQJw5EdGO86QH9XQ0szEp0a8fFP1z5J80s1GF5FDjn8kbBDdcq\n"
        "viYrT8icCLKbkj/sFIZGcL6V9rTpFIHRO1+Iq6cAjWeYQw3s/n3GFKFKKXHQwghE\n"
        "nGdsK+6AINZSA4eqoboynX0tOAD9R8ijUBjTioi3lSNrWf8TWGha2Wn/62nkauHL\n"
        "ApRKSQUnfEsYq5Shro71p0x2UoCGJcjFryhbLm6bRWoTy0XOCwkm7fTf7AyetT5b\n"
        "ZOvvK3Nk6tCb7OyH2iB/DAD4HvYvEaba3IACarfEIuEbXUvotQQAX8PaJdU9EVYr\n"
        "9wIDAQAB\n"
        "-----END PUBLIC KEY-----"
    >>.

test_app_cert() ->
    <<
        "-----BEGIN CERTIFICATE-----\n"
        "MIIDhzCCAm+gAwIBAgIUJp9xXO7gxqDcrUDQwPoQ7bsFeOswDQYJKoZIhvcNAQEL\n"
        "BQAwUzELMAkGA1UEBhMCQ04xETAPBgNVBAoMCFRlc3QgT3JnMRIwEAYDVQQLDAlU\n"
        "ZXN0IFVuaXQxHTAbBgNVBAMMFFRlc3QgQWxpcGF5IEFwcCBDZXJ0MB4XDTI2MDgx\n"
        "OTA2MzAwOVoXDTM2MDgxNjA2MzAwOVowUzELMAkGA1UEBhMCQ04xETAPBgNVBAoM\n"
        "CFRlc3QgT3JnMRIwEAYDVQQLDAlUZXN0IFVuaXQxHTAbBgNVBAMMFFRlc3QgQWxp\n"
        "cGF5IEFwcCBDZXJ0MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAs4Aw\n"
        "97QNRScIx7EabhOxMgrwNszeaq5rCctQJw5EdGO86QH9XQ0szEp0a8fFP1z5J80s\n"
        "1GF5FDjn8kbBDdcqviYrT8icCLKbkj/sFIZGcL6V9rTpFIHRO1+Iq6cAjWeYQw3s\n"
        "/n3GFKFKKXHQwghEnGdsK+6AINZSA4eqoboynX0tOAD9R8ijUBjTioi3lSNrWf8T\n"
        "WGha2Wn/62nkauHLApRKSQUnfEsYq5Shro71p0x2UoCGJcjFryhbLm6bRWoTy0XO\n"
        "Cwkm7fTf7AyetT5bZOvvK3Nk6tCb7OyH2iB/DAD4HvYvEaba3IACarfEIuEbXUvo\n"
        "tQQAX8PaJdU9EVYr9wIDAQABo1MwUTAdBgNVHQ4EFgQUpicXDIn7BKns5QN4n8WY\n"
        "EPrZd2EwHwYDVR0jBBgwFoAUpicXDIn7BKns5QN4n8WYEPrZd2EwDwYDVR0TAQH/\n"
        "BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAjCn1jv0alJN69uoWyJ4JGdVo4D7i\n"
        "9TQLBdWWkDj8N8u04M9RHSzkBxz1E9NK/NDTx8Ig42yn2fkXUfVPIN4hSJoNrq/g\n"
        "OGh86m9o8TCU/d7T+K7yE/rzO32uzt5hys+zj6tz3DmxP1vErDAzfd2Y+frlQGzS\n"
        "LK6LkxsDe6wH3V6Zh4fYWtxO35ga9Woka6ZWtOCySqpTmB+GEOqF47n8R9cei4tf\n"
        "hkZuP+nyS26yd0qJ7vr9yfNZxbmtPUwPckhmRlHW/a3ZZ1cu81k/GMOwsL68o7pl\n"
        "IyJXhOVAR8czCO2wrOBIpTz2op3NOOamMEENsrxKhCw0ug72sjPPTqDisg==\n"
        "-----END CERTIFICATE-----"
    >>.

%% 根证书链：root(sha256WithRSA) + branch(sha1WithRSA) 双证
test_root_chain() ->
    <<
        "-----BEGIN CERTIFICATE-----\n"
        "MIIDfzCCAmegAwIBAgIUfswPpMWjggfUzF48BRAhYma6yzAwDQYJKoZIhvcNAQEL\n"
        "BQAwTzELMAkGA1UEBhMCQ04xFTATBgNVBAoMDFRlc3QgUm9vdCBDbzESMBAGA1UE\n"
        "CwwJUm9vdCBVbml0MRUwEwYDVQQDDAxUZXN0IFJvb3QgQ0EwHhcNMjYwODE5MDYz\n"
        "MDA5WhcNMzYwODE2MDYzMDA5WjBPMQswCQYDVQQGEwJDTjEVMBMGA1UECgwMVGVz\n"
        "dCBSb290IENvMRIwEAYDVQQLDAlSb290IFVuaXQxFTATBgNVBAMMDFRlc3QgUm9v\n"
        "dCBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALs5sCgXa66nBxlc\n"
        "z/fqmZBxQpZHJPGpUwnzK5EkHoFLO9WgJ7ylOsdEW9hUYluJqQ6YZm5+a/aV7RoQ\n"
        "SJzUKD5YfbvTeU+ClNPUkpyT9RCUL8Ax0FYoAmV+fooMB1exmb3uVPy2vq58XivL\n"
        "/q2yverX+HA5t8YiFdHZva6PcijZiJbe4P9hW+v7N/m418iu8Y7mi6g+jYmc9hgx\n"
        "RjYpXnnZnI+juuYR/D1xA8jjPI4scC7bIHfdpN7YZpJy4pfXvUc7/PSlbCc4E1L1\n"
        "MjZjmVPRTIMnA/uheCqzCR/x4KJ+WZROgZUOoocGmISo5ZdcFzfz70DNGIaK4/dP\n"
        "1SllAZ0CAwEAAaNTMFEwHQYDVR0OBBYEFEsy7/GsNNZAySOa547grhe6Y9vsMB8G\n"
        "A1UdIwQYMBaAFEsy7/GsNNZAySOa547grhe6Y9vsMA8GA1UdEwEB/wQFMAMBAf8w\n"
        "DQYJKoZIhvcNAQELBQADggEBAKjlc+Bi22qBsLqIq+RFZWTmueQcR0iVxYewu8W8\n"
        "aVAoYIRdbPuhowXgIlwYOEx3wrgseO6Rr7a5OQV6qfuEbh/xBJzEijc6qY/sdQG9\n"
        "XFrg6odCqIYbMndCXYdThgcIUJlQQLxb/npdCUNd3s1G+R2NT0nqrlylDbgNhMQC\n"
        "Ehhwg9CuGoDgT5Iu+CztjPnMPPEbhG/H/Nu8yl3ily+uXUPEMhgwaY+/A42VPL7P\n"
        "NG5zsHg1DDVHKUxMARtjlRecy/9t2pVFaCvH3giXe9yvJRwWqAlsrefs+ywUaDUb\n"
        "kCzBM8tmqEy7zt+4535U3cgOdSNFWkfSbV3Q7TyRqydKD38=\n"
        "-----END CERTIFICATE-----\n"
        "\n"
        "-----BEGIN CERTIFICATE-----\n"
        "MIIDhzCCAm+gAwIBAgIULSgnUq1KMFYKgAAbCeGJyarPJZ4wDQYJKoZIhvcNAQEF\n"
        "BQAwUzELMAkGA1UEBhMCQ04xFTATBgNVBAoMDFRlc3QgUm9vdCBDbzEUMBIGA1UE\n"
        "CwwLQnJhbmNoIFVuaXQxFzAVBgNVBAMMDlRlc3QgQnJhbmNoIENBMB4XDTI2MDgx\n"
        "OTA2MzAwOVoXDTM2MDgxNjA2MzAwOVowUzELMAkGA1UEBhMCQ04xFTATBgNVBAoM\n"
        "DFRlc3QgUm9vdCBDbzEUMBIGA1UECwwLQnJhbmNoIFVuaXQxFzAVBgNVBAMMDlRl\n"
        "c3QgQnJhbmNoIENBMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAp+bx\n"
        "70CVW9RgRYT7WNRDBsav+HkyZoFi/Cb3fxq4zIGERJ9O+8RQ1DwpmzIdpdFg1lGp\n"
        "IOKfho/HSC7yKqCzcXxneI3dG8Osw2qqhUNoVWaGEeFMCm1sntP8WiSqLmop69zU\n"
        "kErcGbRgrpzoNLsFI22SlQ0Cxtk94gpZSWIciuXePFM8YpI5DOJ96QNfBAUPfbXT\n"
        "2OUy5m9//AMrJkdxOGZCRxZte2+wb72ywh8pnWK9WnYQCTeEBrmwrT6UpZlo2CgJ\n"
        "Rlw19ifgH+ZyweA5bh/BRDltrDt2MMQHl7gZAPCbj/8hIigvRXkbx8gN64L76rtg\n"
        "Z/n9XfXm33XUNLZ/4QIDAQABo1MwUTAdBgNVHQ4EFgQU6Pimq7SWWn+rpZL+Irrn\n"
        "zL4HdiEwHwYDVR0jBBgwFoAU6Pimq7SWWn+rpZL+IrrnzL4HdiEwDwYDVR0TAQH/\n"
        "BAUwAwEB/zANBgkqhkiG9w0BAQUFAAOCAQEAdV1FnqTQm6nlgxsFIgbL8UlwrNT7\n"
        "029aDNVNL23BNy09A8F64FBzK2k1h629Yya1KC7YZ5dtAAhVjszj+nKn25Zp69rG\n"
        "t2JYNCWDhhqJIlY1hBXOBUGfOKWTfNs6sDo7Mj0FMTHo3Iq1IEvpkMs/f2iFgjsG\n"
        "W1UzX+KKiA6WJ8QKP2KRhOHAB7t2nRWHHoafJl/JC58iAYL8IjVPZgjm41czv8Mf\n"
        "yoq5ivXRLrAtUli6Iysb7qtKs6dWHpNR11eCkYAqppnRNTLHx0bjQ9YqdSlsN7VQ\n"
        "0o0Le0lVRfF7M2oGTeBJVszeiLvZRhKyOvZ9X04gHgVvpAssT0GM0mIxPA==\n"
        "-----END CERTIFICATE-----"
    >>.

cfg() ->
    #{
        app_id => <<"2021004142626807">>,
        private_key => test_priv_key()
    }.

cfg_cert_mode() ->
    (cfg())#{
        app_cert_sn => ?TEST_APP_SN,
        alipay_root_cert_sn => ?TEST_ROOT_SN
    }.

%% httpc mock：进程字典 alipay_tc_body 驱动响应体；alipay_tc_status 驱动状态码
httpc_mock() ->
    {httpc, [
        {'request', 4, fun(post, {_Url, _Hdrs, _CT, Body}, _HttpOpts, _Opts) ->
            erlang:put(alipay_tc_last_body, Body),
            Status =
                case erlang:get(alipay_tc_status) of
                    undefined -> 200;
                    S -> S
                end,
            {ok, {{v, Status, ok}, [], erlang:get(alipay_tc_body)}}
        end}
    ]}.

%% 从最后一次请求体解析参数 map（form 解码）
last_params() ->
    Body = erlang:get(alipay_tc_last_body),
    maps:from_list(cow_qs:parse_qs(Body)).

%% 独立重建签名串并验签：排除 sign 与空值，key 字典序，k=v& 连接
verify_request_sign(Params) ->
    Sign = maps:get(<<"sign">>, Params),
    Pairs = [
        {K, V}
     || K <- lists:sort(maps:keys(Params)),
        K =/= <<"sign">>,
        V <- [maps:get(K, Params)],
        V =/= <<>>
    ],
    Content = iolist_to_binary(
        lists:join(
            "&",
            [<<K/binary, "=", V/binary>> || {K, V} <- Pairs]
        )
    ),
    epay_crypto:rsa_verify_sha256(Content, base64:decode(Sign), test_pub_key()).

token_resp_ok() ->
    jsone:encode(#{
        <<"alipay_system_oauth_token_response">> => #{
            <<"code">> => <<"10000">>,
            <<"msg">> => <<"Success">>,
            <<"access_token">> => <<"at-test-token">>,
            <<"alipay_user_id">> => <<"2088302622035892">>,
            <<"auth_start">> => <<"2026-08-19 12:00:00">>,
            <<"expires_in">> => 1296000,
            <<"re_expires_in">> => 2592000,
            <<"refresh_token">> => <<"rt-test-token">>,
            <<"user_id">> => <<"2088302622035892">>
        },
        <<"sign">> => <<"ignored">>
    }).

userinfo_resp_ok() ->
    jsone:encode(#{
        <<"alipay_user_info_share_response">> => #{
            <<"code">> => <<"10000">>,
            <<"msg">> => <<"Success">>,
            <<"user_id">> => <<"2088302622035892">>,
            <<"avatar">> => <<"https://tfs.alipayobjects.com/avatar.jpg">>,
            <<"nick_name">> => <<"测试用户"/utf8>>,
            <<"gender">> => <<"m">>,
            <<"province">> => <<"广东省"/utf8>>,
            <<"city">> => <<"深圳市"/utf8>>
        },
        <<"sign">> => <<"ignored">>
    }).

%%%===================================================================
%%% 证书 SN 算法
%%%===================================================================

cert_sn_app_test() ->
    ?assertEqual(?TEST_APP_SN, alipay_openapi:cert_sn(test_app_cert())).

root_cert_sn_chain_test() ->
    %% 双证链：sha256 + sha1 两张 RSA 家族证书各算一个 SN，用 _ 拼接
    ?assertEqual(?TEST_ROOT_SN, alipay_openapi:root_cert_sn(test_root_chain())).

root_cert_sn_skip_bad_block_test() ->
    %% 链中混入坏 PEM 块与非证书块：跳过，不崩溃
    Bad = <<"-----BEGIN CERTIFICATE-----\nbm90IGEgY2VydA==\n-----END CERTIFICATE-----\n\n">>,
    Chain = <<Bad/binary, (test_root_chain())/binary>>,
    ?assertEqual(?TEST_ROOT_SN, alipay_openapi:root_cert_sn(Chain)).

%%%===================================================================
%%% oauth_token
%%%===================================================================

oauth_token_ok_test_() ->
    ?WITH_MECKS([httpc_mock()], fun() ->
        erlang:put(alipay_tc_body, token_resp_ok()),
        {ok, Tok} = alipay_openapi:oauth_token(cfg(), <<"authcode123">>),
        ?assertEqual(<<"at-test-token">>, maps:get(access_token, Tok)),
        ?assertEqual(<<"2088302622035892">>, maps:get(user_id, Tok)),
        ?assertEqual(<<"rt-test-token">>, maps:get(refresh_token, Tok)),
        %% 公共参数齐全
        P = last_params(),
        ?assertEqual(<<"2021004142626807">>, maps:get(<<"app_id">>, P)),
        ?assertEqual(<<"alipay.system.oauth.token">>, maps:get(<<"method">>, P)),
        ?assertEqual(<<"utf-8">>, maps:get(<<"charset">>, P)),
        ?assertEqual(<<"RSA2">>, maps:get(<<"sign_type">>, P)),
        ?assertEqual(<<"1.0">>, maps:get(<<"version">>, P)),
        ?assertMatch(<<_Ts:19/binary>>, maps:get(<<"timestamp">>, P)),
        %% biz_content 带授权码与授权类型
        Biz = jsone:decode(maps:get(<<"biz_content">>, P)),
        ?assertEqual(<<"authorization_code">>, maps:get(<<"grant_type">>, Biz)),
        ?assertEqual(<<"authcode123">>, maps:get(<<"code">>, Biz)),
        %% 非证书模式不带 cert SN
        ?assertEqual(false, maps:is_key(<<"app_cert_sn">>, P)),
        %% 签名可用测试公钥验过
        ?assert(verify_request_sign(P))
    end).

oauth_token_cert_mode_params_test_() ->
    ?WITH_MECKS([httpc_mock()], fun() ->
        erlang:put(alipay_tc_body, token_resp_ok()),
        {ok, _Tok} = alipay_openapi:oauth_token(cfg_cert_mode(), <<"authcode123">>),
        P = last_params(),
        ?assertEqual(?TEST_APP_SN, maps:get(<<"app_cert_sn">>, P)),
        ?assertEqual(?TEST_ROOT_SN, maps:get(<<"alipay_root_cert_sn">>, P)),
        ?assert(verify_request_sign(P))
    end).

oauth_token_biz_error_test_() ->
    ?WITH_MECKS([httpc_mock()], fun() ->
        erlang:put(
            alipay_tc_body,
            jsone:encode(#{
                <<"alipay_system_oauth_token_response">> => #{
                    <<"code">> => <<"40002">>,
                    <<"msg">> => <<"Invalid Arguments">>,
                    <<"sub_code">> => <<"isv.code-invalid">>,
                    <<"sub_msg">> => <<"授权码code无效"/utf8>>
                }
            })
        ),
        {error, Msg} = alipay_openapi:oauth_token(cfg(), <<"badcode">>),
        ?assertEqual(<<"授权码code无效"/utf8>>, Msg)
    end).

oauth_token_http_500_test_() ->
    ?WITH_MECKS([httpc_mock()], fun() ->
        erlang:put(alipay_tc_status, 500),
        erlang:put(alipay_tc_body, <<"<html>error</html>">>),
        {error, Msg} = alipay_openapi:oauth_token(cfg(), <<"authcode123">>),
        Prefix = <<"支付宝接口 HTTP "/utf8>>,
        ?assertEqual(Prefix, binary:part(Msg, 0, byte_size(Prefix)))
    end).

oauth_token_bad_json_test_() ->
    ?WITH_MECKS([httpc_mock()], fun() ->
        erlang:put(alipay_tc_body, <<"not-a-json">>),
        {error, Msg} = alipay_openapi:oauth_token(cfg(), <<"authcode123">>),
        ?assertEqual(<<"支付宝响应解析失败"/utf8>>, Msg)
    end).

oauth_token_no_credential_test() ->
    {error, Msg} = alipay_openapi:oauth_token(#{app_id => <<>>, private_key => <<>>}, <<"c">>),
    ?assertEqual(<<"支付宝登录未配置凭据"/utf8>>, Msg).

%%%===================================================================
%%% user_info_share
%%%===================================================================

user_info_share_ok_test_() ->
    ?WITH_MECKS([httpc_mock()], fun() ->
        erlang:put(alipay_tc_body, userinfo_resp_ok()),
        {ok, Info} = alipay_openapi:user_info_share(cfg(), <<"at-test-token">>),
        ?assertEqual(<<"测试用户"/utf8>>, maps:get(<<"nick_name">>, Info)),
        ?assertEqual(<<"m">>, maps:get(<<"gender">>, Info)),
        ?assertEqual(<<"深圳市"/utf8>>, maps:get(<<"city">>, Info)),
        %% auth_token 作为 biz_content 字段下发
        P = last_params(),
        ?assertEqual(<<"alipay.user.info.share">>, maps:get(<<"method">>, P)),
        Biz = jsone:decode(maps:get(<<"biz_content">>, P)),
        ?assertEqual(<<"at-test-token">>, maps:get(<<"auth_token">>, Biz)),
        ?assert(verify_request_sign(P))
    end).

user_info_share_biz_error_test_() ->
    ?WITH_MECKS([httpc_mock()], fun() ->
        erlang:put(
            alipay_tc_body,
            jsone:encode(#{
                <<"alipay_user_info_share_response">> => #{
                    <<"code">> => <<"20001">>,
                    <<"msg">> => <<"Insufficient auth token">>,
                    <<"sub_msg">> => <<"访问令牌已过期"/utf8>>
                }
            })
        ),
        {error, Msg} = alipay_openapi:user_info_share(cfg(), <<"expired">>),
        ?assertEqual(<<"访问令牌已过期"/utf8>>, Msg)
    end).
