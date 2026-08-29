-module(auth_oidc_jwks_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% auth_oidc_jwks 模块的 EUnit 测试（SEC-01 id_token JWKS 验签）
%%%
%%% 覆盖矩阵：
%%%   正向：RS256 / ES256 有效签名、jwks_uri 直配（跳过 discovery）、
%%%         TTL 内缓存命中、无 kid 单 key 兼容
%%%   负向（全部拒绝，fail-closed）：伪造签名、未知 kid（刷新后仍未知）、
%%%         alg=none / HS256 / 白名单外算法、kty/alg 混用、非 P-256 曲线、
%%%         use=enc key、坏 token 结构 / 坏 header JSON、
%%%         JWKS 拉取网络错、discovery 网络错、discovery 缺 jwks_uri、
%%%         非 https 且非本机 jwks_uri、ES256 签名长度错
%%%   轮换：未知 kid 触发强制刷新后通过；最小刷新间隔防未知 kid 洪水
%%%
%%% 说明：密钥（RSA-2048 / EC P-256）全部由测试进程运行时生成，不入任何
%%%       文件；discovery/JWKS 由 meck httpc + 进程字典驱动的 fake IdP 提供，
%%%       绝不真实外网请求。
%%%===================================================================

-define(ISSUER, <<"http://127.0.0.1:5556/dex">>).
-define(JWKS_URI, <<"http://127.0.0.1:5556/dex/keys">>).
-define(RSA_KID, <<"kid-rsa-1">>).
-define(EC_KID, <<"kid-ec-1">>).

%% ===================================================================
%% 测试夹具：fake IdP（进程字典驱动 + meck httpc）
%% ===================================================================

%% RSA fake IdP：返回 {Jwk, Priv}
rsa_idp(Kid) ->
    {[E, N], Priv} = crypto:generate_key(rsa, {2048, 65537}),
    Jwk = #{
        <<"kty">> => <<"RSA">>,
        <<"kid">> => Kid,
        <<"n">> => b64url(N),
        <<"e">> => b64url(E),
        <<"use">> => <<"sig">>,
        <<"alg">> => <<"RS256">>
    },
    {Jwk, Priv}.

%% EC P-256 fake IdP：返回 {Jwk, Priv}
ec_idp(Kid) ->
    {Pub, Priv} = crypto:generate_key(ecdh, secp256r1),
    <<4, X:32/binary, Y:32/binary>> = Pub,
    Jwk = #{
        <<"kty">> => <<"EC">>,
        <<"kid">> => Kid,
        <<"crv">> => <<"P-256">>,
        <<"x">> => b64url(X),
        <<"y">> => b64url(Y),
        <<"use">> => <<"sig">>,
        <<"alg">> => <<"ES256">>
    },
    {Jwk, Priv}.

%% 每用例冷启动：清 JWKS 缓存、重置 fake IdP 状态与 HTTP 计数
reset_idp() ->
    ok = auth_oidc_jwks:clear_cache(),
    erlang:put(tc_discovery, #{<<"issuer">> => ?ISSUER, <<"jwks_uri">> => ?JWKS_URI}),
    erlang:put(tc_jwks_error, none),
    erlang:put(tc_discovery_error, none),
    erlang:put(tc_jwks_count, 0),
    erlang:put(tc_discovery_count, 0),
    ok.

put_jwks(Jwks) ->
    _ = erlang:put(tc_jwks, #{<<"keys">> => Jwks}),
    ok.

httpc_mock() ->
    {httpc, [
        {'request', 4, fun(get, {Url, _Hdrs}, _Opts, _Profile) ->
            route_get(list_to_binary(Url))
        end}
    ]}.

route_get(U) ->
    IsDiscovery = binary:match(U, <<".well-known/openid-configuration">>) =/= nomatch,
    IsJwks = binary:match(U, <<"/dex/keys">>) =/= nomatch,
    if
        IsDiscovery ->
            bump(tc_discovery_count),
            case erlang:get(tc_discovery_error) of
                none -> {ok, {{v, 200, ok}, [], jsone:encode(erlang:get(tc_discovery))}};
                Err -> Err
            end;
        IsJwks ->
            bump(tc_jwks_count),
            case erlang:get(tc_jwks_error) of
                none -> {ok, {{v, 200, ok}, [], jsone:encode(erlang:get(tc_jwks))}};
                Err -> Err
            end;
        true ->
            {ok, {{v, 200, ok}, [], <<"{}">>}}
    end.

bump(Key) ->
    erlang:put(Key, erlang:get(Key) + 1).

jwks_fetches() -> erlang:get(tc_jwks_count).
discovery_fetches() -> erlang:get(tc_discovery_count).

cfg() -> #{<<"issuer">> => ?ISSUER}.

cfg_direct_jwks() ->
    #{<<"issuer">> => ?ISSUER, <<"jwks_uri">> => ?JWKS_URI}.

claims() ->
    #{
        <<"iss">> => ?ISSUER,
        <<"aud">> => <<"imboy">>,
        <<"exp">> => erlang:system_time(second) + 600,
        <<"sub">> => <<"sub-001">>
    }.

%% JWT 构造（header/payload/signature 均正确 b64url）
jwt(Hdr, Claims, SigB64) ->
    H = b64url(jsone:encode(Hdr)),
    P = b64url(jsone:encode(Claims)),
    <<H/binary, ".", P/binary, ".", SigB64/binary>>.

rs256_jwt(Priv, Kid, Claims) ->
    jwt(
        #{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>, <<"kid">> => Kid},
        Claims,
        rs_sign(Priv, Kid, Claims)
    ).

rs_sign(Priv, Kid, Claims) ->
    b64url(crypto:sign(rsa, sha256, signing_input(Kid, Claims), Priv)).

signing_input(Kid, Claims) ->
    H = b64url(
        jsone:encode(#{
            <<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>, <<"kid">> => Kid
        })
    ),
    P = b64url(jsone:encode(Claims)),
    <<H/binary, ".", P/binary>>.

es256_jwt(Priv, Kid, Claims) ->
    Hdr = #{<<"alg">> => <<"ES256">>, <<"typ">> => <<"JWT">>, <<"kid">> => Kid},
    H = b64url(jsone:encode(Hdr)),
    P = b64url(jsone:encode(Claims)),
    SigningInput = <<H/binary, ".", P/binary>>,
    Der = crypto:sign(ecdsa, sha256, SigningInput, [Priv, secp256r1]),
    %% DER -> JWS raw r||s
    {'ECDSA-Sig-Value', R, S} = public_key:der_decode('ECDSA-Sig-Value', Der),
    <<H/binary, ".", P/binary, ".", (b64url(<<R:256/big, S:256/big>>))/binary>>.

b64url(B) ->
    base64:encode(B, #{mode => urlsafe, padding => false}).

with_env(Key, Value, Fun) ->
    Old = application:get_env(imboy, Key),
    ok = application:set_env(imboy, Key, Value),
    try
        Fun()
    after
        case Old of
            undefined -> application:unset_env(imboy, Key);
            _ -> application:set_env(imboy, Key, Old)
        end
    end.

%% ===================================================================
%% 正向：有效签名通过
%% ===================================================================

%% @doc RS256：discovery 解析 jwks_uri -> JWKS 选 key -> 验签通过
rs256_valid_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertMatch(
                {ok, #{<<"iss">> := ?ISSUER}},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            ),
            %% discovery + jwks 各拉取一次
            ?assertEqual(1, discovery_fetches()),
            ?assertEqual(1, jwks_fetches())
        end
    ).

%% @doc ES256：P-256 raw r||s 签名（DER 转换路径）验签通过
es256_valid_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {RsaJwk, _} = rsa_idp(?RSA_KID),
            {EcJwk, EcPriv} = ec_idp(?EC_KID),
            ok = put_jwks([RsaJwk, EcJwk]),
            Token = es256_jwt(EcPriv, ?EC_KID, claims()),
            ?assertMatch(
                {ok, #{<<"sub">> := <<"sub-001">>}},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc 配置直给 jwks_uri：跳过 discovery（零 discovery 请求）
jwks_uri_direct_config_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertMatch(
                {ok, _},
                auth_oidc_jwks:verify_id_token(Token, cfg_direct_jwks())
            ),
            ?assertEqual(0, discovery_fetches()),
            ?assertEqual(1, jwks_fetches())
        end
    ).

%% @doc TTL 内缓存命中：两次验签只拉一次 JWKS
cache_hit_within_ttl_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertMatch({ok, _}, auth_oidc_jwks:verify_id_token(Token, cfg())),
            ?assertMatch({ok, _}, auth_oidc_jwks:verify_id_token(Token, cfg())),
            ?assertEqual(1, jwks_fetches())
        end
    ).

%% @doc TTL 过期后重新拉取（oidc_jwks_cache_ttl=0 -> 每次都 fetch）
cache_ttl_expiry_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            with_env(oidc_jwks_cache_ttl, 0, fun() ->
                ?assertMatch({ok, _}, auth_oidc_jwks:verify_id_token(Token, cfg())),
                ?assertMatch({ok, _}, auth_oidc_jwks:verify_id_token(Token, cfg())),
                ?assertEqual(2, jwks_fetches())
            end)
        end
    ).

%% @doc token 无 kid：JWKS 恰好一把 key 时兼容放行
no_kid_single_key_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Hdr = #{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>},
            H = b64url(jsone:encode(Hdr)),
            P = b64url(jsone:encode(claims())),
            Sig = crypto:sign(rsa, sha256, <<H/binary, ".", P/binary>>, Priv),
            Token = <<H/binary, ".", P/binary, ".", (b64url(Sig))/binary>>,
            ?assertMatch({ok, _}, auth_oidc_jwks:verify_id_token(Token, cfg()))
        end
    ).

%% @doc token 无 kid 且 JWKS 多把 key -> 歧义拒绝（fail-closed）
no_kid_multi_keys_rejected_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk1, Priv1} = rsa_idp(<<"k1">>),
            {Jwk2, _} = rsa_idp(<<"k2">>),
            ok = put_jwks([Jwk1, Jwk2]),
            H = b64url(jsone:encode(#{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>})),
            P = b64url(jsone:encode(claims())),
            Sig = crypto:sign(rsa, sha256, <<H/binary, ".", P/binary>>, Priv1),
            Token = <<H/binary, ".", P/binary, ".", (b64url(Sig))/binary>>,
            ?assertEqual(
                {error, kid_ambiguous},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% ===================================================================
%% 负向：算法白名单（全部拒绝）
%% ===================================================================

%% @doc alg=none（无签名段形态）-> 拒绝
alg_none_empty_sig_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            H = b64url(jsone:encode(#{<<"alg">> => <<"none">>, <<"kid">> => ?RSA_KID})),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".">>,
            ?assertEqual(
                {error, bad_id_token},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc alg=none（带假签名段形态）-> 白名单拒绝
alg_none_fake_sig_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            H = b64url(jsone:encode(#{<<"alg">> => <<"none">>, <<"kid">> => ?RSA_KID})),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
            ?assertEqual(
                {error, {alg_not_allowed, <<"none">>}},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc HS256 -> 白名单拒绝（HMAC 对称签名永久硬拒，防 RS->HS 混淆）
hs256_rejected_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            H = b64url(jsone:encode(#{<<"alg">> => <<"HS256">>, <<"kid">> => ?RSA_KID})),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
            ?assertEqual(
                {error, {alg_not_allowed, <<"HS256">>}},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc 白名单外非对称算法（RS384/RS512/PS256/ES512）-> 拒绝
out_of_whitelist_algs_rejected_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            [
                begin
                    H = b64url(jsone:encode(#{<<"alg">> => Alg, <<"kid">> => ?RSA_KID})),
                    P = b64url(jsone:encode(claims())),
                    Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
                    ?assertEqual(
                        {error, {alg_not_allowed, Alg}},
                        auth_oidc_jwks:verify_id_token(Token, cfg())
                    )
                end
             || Alg <- [<<"RS384">>, <<"RS512">>, <<"PS256">>, <<"ES512">>]
            ]
        end
    ).

%% @doc alg 缺失 -> 拒绝
missing_alg_rejected_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, _} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            H = b64url(jsone:encode(#{<<"kid">> => ?RSA_KID})),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
            ?assertEqual(
                {error, {alg_not_allowed, <<>>}},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% ===================================================================
%% 负向：签名 / key 材料（全部拒绝）
%% ===================================================================

%% @doc 伪造签名（攻击者自签 key，kid 冒充）-> 拒绝
forged_signature_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, _} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            {_, AttackerPriv} = crypto:generate_key(rsa, {2048, 65537}),
            Token = rs256_jwt(AttackerPriv, ?RSA_KID, claims()),
            ?assertEqual(
                {error, signature_invalid},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc 篡改 payload（签名与内容不对应）-> 拒绝
tampered_payload_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Good = rs256_jwt(Priv, ?RSA_KID, claims()),
            %% 换 payload 段（签名还是对原 payload 的）
            [H, _P, S] = binary:split(Good, <<".">>, [global]),
            BaseClaims = claims(),
            EvilClaims = BaseClaims#{<<"sub">> => <<"admin-takeover">>},
            EvilP = b64url(jsone:encode(EvilClaims)),
            Token = <<H/binary, ".", EvilP/binary, ".", S/binary>>,
            ?assertEqual(
                {error, signature_invalid},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc 未知 kid（JWKS 刷新后仍未知）-> 拒绝；断言确实做了"初次+强制刷新"两次拉取
unknown_kid_after_refresh_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            %% 先正常登录一次：建立 JWKS 缓存（拉取 #1）
            ?assertMatch(
                {ok, _},
                auth_oidc_jwks:verify_id_token(rs256_jwt(Priv, ?RSA_KID, claims()), cfg())
            ),
            %% 未知 kid token：触发强制刷新（拉取 #2），刷新后仍未知 -> 拒绝
            {_, OtherPriv} = crypto:generate_key(rsa, {2048, 65537}),
            Token = rs256_jwt(OtherPriv, <<"kid-not-in-jwks">>, claims()),
            with_env(oidc_jwks_min_refresh_ms, 0, fun() ->
                ?assertEqual(
                    {error, unknown_kid},
                    auth_oidc_jwks:verify_id_token(Token, cfg())
                )
            end),
            %% 初次拉取 + 未知 kid 强制刷新 = 2 次
            ?assertEqual(2, jwks_fetches())
        end
    ).

%% @doc ES256 签名段长度错（非 raw r||s 64 字节）-> 拒绝
es256_bad_sig_length_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {EcJwk, _} = ec_idp(?EC_KID),
            ok = put_jwks([EcJwk]),
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"ES256">>, <<"kid">> => ?EC_KID
                })
            ),
            P = b64url(jsone:encode(claims())),
            %% 32 字节签名段（长度不合法）
            BadSig = b64url(crypto:strong_rand_bytes(32)),
            Token = <<H/binary, ".", P/binary, ".", BadSig/binary>>,
            ?assertEqual(
                {error, signature_invalid},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc kty/alg 混用（EC key 却声明 RS256）-> key_type_mismatch 拒绝（防 key confusion）
key_type_mismatch_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {EcJwk, EcPriv} = ec_idp(?EC_KID),
            ok = put_jwks([EcJwk]),
            %% token 声明 RS256 但 key 是 EC —— 即使签的是 EC 签名也不许过
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"RS256">>, <<"kid">> => ?EC_KID
                })
            ),
            P = b64url(jsone:encode(claims())),
            SigningInput = <<H/binary, ".", P/binary>>,
            Der = crypto:sign(ecdsa, sha256, SigningInput, [EcPriv, secp256r1]),
            {'ECDSA-Sig-Value', R, S} = public_key:der_decode('ECDSA-Sig-Value', Der),
            SigB64 = b64url(<<R:256/big, S:256/big>>),
            Token = <<SigningInput/binary, ".", SigB64/binary>>,
            with_env(oidc_jwks_min_refresh_ms, 0, fun() ->
                ?assertEqual(
                    {error, key_type_mismatch},
                    auth_oidc_jwks:verify_id_token(Token, cfg())
                )
            end)
        end
    ).

%% @doc JWK alg 声明与 token alg 不一致（RS256 key 被用于 ES256 token）-> 拒绝
jwk_alg_conflict_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            %% RSA JWK 声明 alg=RS256；token 声明 ES256 + 同 kid
            {Jwk, _} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"ES256">>, <<"kid">> => ?RSA_KID
                })
            ),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
            with_env(oidc_jwks_min_refresh_ms, 0, fun() ->
                ?assertEqual(
                    {error, key_type_mismatch},
                    auth_oidc_jwks:verify_id_token(Token, cfg())
                )
            end)
        end
    ).

%% @doc 非 P-256 曲线（P-384）-> key 被跳过 -> 未知 kid 拒绝
ec_p384_curve_skipped_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {P384Pub, _} = crypto:generate_key(ecdh, secp384r1),
            <<4, X384:48/binary, Y384:48/binary>> = P384Pub,
            Jwk = #{
                <<"kty">> => <<"EC">>,
                <<"kid">> => <<"kid-p384">>,
                <<"crv">> => <<"P-384">>,
                <<"x">> => b64url(X384),
                <<"y">> => b64url(Y384),
                <<"use">> => <<"sig">>
            },
            ok = put_jwks([Jwk]),
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"ES256">>, <<"kid">> => <<"kid-p384">>
                })
            ),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
            with_env(oidc_jwks_min_refresh_ms, 0, fun() ->
                ?assertEqual(
                    {error, unknown_kid},
                    auth_oidc_jwks:verify_id_token(Token, cfg())
                )
            end)
        end
    ).

%% @doc use=enc 的 key（加密用途）-> 不用于验签（跳过）-> 未知 kid 拒绝
use_enc_key_skipped_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {[E, N], _} = crypto:generate_key(rsa, {2048, 65537}),
            Jwk = #{
                <<"kty">> => <<"RSA">>,
                <<"kid">> => ?RSA_KID,
                <<"n">> => b64url(N),
                <<"e">> => b64url(E),
                <<"use">> => <<"enc">>
            },
            ok = put_jwks([Jwk]),
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"RS256">>, <<"kid">> => ?RSA_KID
                })
            ),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
            with_env(oidc_jwks_min_refresh_ms, 0, fun() ->
                ?assertEqual(
                    {error, unknown_kid},
                    auth_oidc_jwks:verify_id_token(Token, cfg())
                )
            end)
        end
    ).

%% @doc JWK 材料（n/e）base64url 非法 -> 该 key 跳过 -> 未知 kid 拒绝
bad_jwk_material_skipped_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            Jwk = #{
                <<"kty">> => <<"RSA">>,
                <<"kid">> => ?RSA_KID,
                <<"n">> => <<"!!!not-base64url!!!">>,
                <<"e">> => <<"AQAB">>,
                <<"use">> => <<"sig">>
            },
            ok = put_jwks([Jwk]),
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"RS256">>, <<"kid">> => ?RSA_KID
                })
            ),
            P = b64url(jsone:encode(claims())),
            Token = <<H/binary, ".", P/binary, ".ZmFrZQ">>,
            with_env(oidc_jwks_min_refresh_ms, 0, fun() ->
                ?assertEqual(
                    {error, unknown_kid},
                    auth_oidc_jwks:verify_id_token(Token, cfg())
                )
            end)
        end
    ).

%% ===================================================================
%% 负向：token 结构 / header（全部拒绝）
%% ===================================================================

malformed_token_test_() ->
    ?WITH_MECKS(
        [],
        fun() ->
            ?assertEqual(
                {error, bad_id_token},
                auth_oidc_jwks:verify_id_token(<<"only.two">>, cfg())
            ),
            ?assertEqual(
                {error, bad_id_token},
                auth_oidc_jwks:verify_id_token(<<"a.b.c.d">>, cfg())
            ),
            ?assertEqual(
                {error, bad_id_token},
                auth_oidc_jwks:verify_id_token(<<>>, cfg())
            ),
            ?assertEqual(
                {error, bad_id_token},
                auth_oidc_jwks:verify_id_token(not_a_binary, cfg())
            ),
            ?assertEqual(
                {error, bad_id_token},
                auth_oidc_jwks:verify_id_token(<<"a.b.c">>, not_a_map)
            )
        end
    ).

%% @doc header 段非合法 JSON -> 拒绝
bad_header_json_test_() ->
    ?WITH_MECKS(
        [],
        fun() ->
            H = base64:encode(<<"not json at all">>, #{mode => urlsafe, padding => false}),
            Token = <<H/binary, ".eyJwbGF5bG9hZCI6MT4.aGVsbG8">>,
            ?assertEqual(
                {error, bad_id_token},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% ===================================================================
%% 负向：discovery / JWKS 拉取与 URL 安全（全部拒绝）
%% ===================================================================

%% @doc JWKS 端点网络错 -> 拒绝（fail-closed，不用任何兜底 key）
jwks_network_error_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            erlang:put(tc_jwks_error, {error, econnrefused}),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertMatch(
                {error, {jwks_fetch_failed, _}},
                auth_oidc_jwks:verify_id_token(Token, cfg_direct_jwks())
            )
        end
    ).

%% @doc JWKS 端点非 200 -> 拒绝
jwks_http_error_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            erlang:put(tc_jwks_error, {ok, {{v, 503, unavailable}, [], <<>>}}),
            {Jwk, Priv} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertMatch(
                {error, {jwks_fetch_failed, {http_status, 503}}},
                auth_oidc_jwks:verify_id_token(Token, cfg_direct_jwks())
            )
        end
    ).

%% @doc discovery 端点网络错 -> 拒绝
discovery_network_error_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            erlang:put(tc_discovery_error, {error, timeout}),
            {_, Priv} = rsa_idp(?RSA_KID),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertMatch(
                {error, {discovery_failed, timeout}},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc discovery 文档缺 jwks_uri -> 拒绝
discovery_missing_jwks_uri_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            erlang:put(tc_discovery, #{<<"issuer">> => ?ISSUER}),
            {_, Priv} = rsa_idp(?RSA_KID),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertEqual(
                {error, bad_discovery_doc},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc discovery 返回的 jwks_uri 非 https 且非本机 -> 拒绝（明文信道只许打本机）
insecure_jwks_uri_rejected_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            erlang:put(tc_discovery, #{
                <<"issuer">> => ?ISSUER, <<"jwks_uri">> => <<"http://evil.example.com/keys">>
            }),
            {_, Priv} = rsa_idp(?RSA_KID),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertEqual(
                {error, insecure_jwks_url},
                auth_oidc_jwks:verify_id_token(Token, cfg())
            )
        end
    ).

%% @doc 配置直给非 https 且非本机 jwks_uri -> 拒绝
%%（token header 合法才会走到 URL 检查，故用合法 RS256 header 构造）
insecure_configured_jwks_uri_rejected_test_() ->
    ?WITH_MECKS(
        [],
        fun() ->
            BadCfg = #{<<"issuer">> => ?ISSUER, <<"jwks_uri">> => <<"http://10.0.0.9/keys">>},
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"RS256">>, <<"kid">> => ?RSA_KID
                })
            ),
            Token = <<H/binary, ".eyJwbGF5bG9hZCI6MX0.aGVsbG8">>,
            ?assertEqual(
                {error, insecure_jwks_url},
                auth_oidc_jwks:verify_id_token(Token, BadCfg)
            )
        end
    ).

%% @doc issuer 未配置且未直配 jwks_uri -> 拒绝
issuer_missing_test_() ->
    ?WITH_MECKS(
        [],
        fun() ->
            H = b64url(
                jsone:encode(#{
                    <<"alg">> => <<"RS256">>, <<"kid">> => ?RSA_KID
                })
            ),
            Token = <<H/binary, ".eyJwbGF5bG9hZCI6MX0.aGVsbG8">>,
            ?assertEqual(
                {error, issuer_missing},
                auth_oidc_jwks:verify_id_token(Token, #{})
            )
        end
    ).

%% @doc JWKS 文档缺 keys 字段 -> 拒绝
jwks_missing_keys_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            erlang:put(tc_jwks, #{<<"not_keys">> => []}),
            {_, Priv} = rsa_idp(?RSA_KID),
            Token = rs256_jwt(Priv, ?RSA_KID, claims()),
            ?assertMatch(
                {error, {jwks_fetch_failed, bad_jwks}},
                auth_oidc_jwks:verify_id_token(Token, cfg_direct_jwks())
            )
        end
    ).

%% ===================================================================
%% 轮换与最小刷新间隔
%% ===================================================================

%% @doc 密钥轮换：新 kid 未知 -> 强制刷新 JWKS -> 拿到新 key -> 验签通过
rotation_refresh_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk1, Priv1} = rsa_idp(<<"kid-old">>),
            ok = put_jwks([Jwk1]),
            OldToken = rs256_jwt(Priv1, <<"kid-old">>, claims()),
            ?assertMatch({ok, _}, auth_oidc_jwks:verify_id_token(OldToken, cfg())),
            ?assertEqual(1, jwks_fetches()),
            %% IdP 轮换到新 key
            {Jwk2, Priv2} = rsa_idp(<<"kid-new">>),
            ok = put_jwks([Jwk1, Jwk2]),
            NewToken = rs256_jwt(Priv2, <<"kid-new">>, claims()),
            with_env(oidc_jwks_min_refresh_ms, 0, fun() ->
                ?assertMatch(
                    {ok, _},
                    auth_oidc_jwks:verify_id_token(NewToken, cfg())
                )
            end),
            %% 未知 kid 触发强制刷新：第二次 JWKS 拉取
            ?assertEqual(2, jwks_fetches())
        end
    ).

%% @doc 未知 kid 洪水防护：默认最小刷新间隔内强制刷新被限流，
%%      仍以未知 kid 拒绝且不多打 IdP（fail-closed + 防放大）
unknown_kid_refresh_rate_limited_test_() ->
    ?WITH_MECKS(
        [httpc_mock()],
        fun() ->
            ok = reset_idp(),
            {Jwk, _} = rsa_idp(?RSA_KID),
            ok = put_jwks([Jwk]),
            {_, OtherPriv} = crypto:generate_key(rsa, {2048, 65537}),
            %% 第一次验证：拉取 JWKS 并缓存（默认 min_refresh_ms=1000）
            Token1 = rs256_jwt(OtherPriv, <<"flood-kid">>, claims()),
            ?assertEqual(
                {error, unknown_kid},
                auth_oidc_jwks:verify_id_token(Token1, cfg())
            ),
            %% 第二次：距首次拉取 < 1s，强制刷新被限流 -> 直接拒绝，不多打 IdP
            ?assertEqual(
                {error, unknown_kid},
                auth_oidc_jwks:verify_id_token(Token1, cfg())
            ),
            %% 初次拉取 1 次（第一次的强制刷新因"本次刚拉取"被跳过）
            ?assertEqual(1, jwks_fetches())
        end
    ).
