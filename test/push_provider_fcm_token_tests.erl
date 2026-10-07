-module(push_provider_fcm_token_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% push_provider_fcm_token（G4/Q09：FCM service account → OAuth2 access
%%% token 刷新链路）的 EUnit 测试。
%%%
%%% 凭据隔离红线（AC22 三重保障）：
%%%   - service account 为测试内现生成的 RSA 密钥对 + 全占位字段
%%%     （*-placeholder / .invalid，RFC 2606）；
%%%   - gun 整体桩掉（L3 注入点）：任何未预期的真实出网在桩上直接
%%%     function_clause / 不匹配暴露，且每用例收尾断言 gun:open 调用数；
%%%   - 不触碰任何真实 Firebase 凭据（真实凭据缺失 = BLOCKED_EXTERNAL，
%%%     只验证刷新机制本身）。
%%%===================================================================

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

-define(CACHE_KEY, {fcm_access_token_dyn}).
-define(TOKEN_URI, <<"https://oauth2.example.invalid/token">>).
-define(CLIENT_EMAIL, <<"fcm-sa-placeholder@example.iam.gserviceaccount.com">>).
-define(FAKE_ACCESS_TOKEN, <<"fake-access-token-placeholder">>).

%% 测试 RSA 私钥（1024 位足够测试签名；PKCS#8 形态与 Google service
%% account JSON 的 "-----BEGIN PRIVATE KEY-----" 完全同构；进程间持久
%% 缓存避免逐用例重复生成）
test_rsa_pem() ->
    case persistent_term:get({?MODULE, rsa_pem}, undefined) of
        undefined ->
            Key = public_key:generate_key({rsa, 1024, 65537}),
            Inner = public_key:der_encode('RSAPrivateKey', Key),
            %% SEQUENCE { OID 1.2.840.113549.1.1.1 (rsaEncryption), NULL }
            AlgDer = <<48, 13, 6, 9, 42, 134, 72, 134, 247, 13, 1, 1, 1, 5, 0>>,
            %% PKCS#8: SEQUENCE { INTEGER 0, AlgorithmIdentifier, OCTET STRING key }
            Pkcs8 = der_tlv(
                16#30,
                <<2, 1, 0, AlgDer/binary, (der_tlv(16#04, Inner))/binary>>
            ),
            Pem = public_key:pem_encode([{'PrivateKeyInfo', Pkcs8, not_encrypted}]),
            persistent_term:put({?MODULE, rsa_pem}, Pem),
            Pem;
        Pem ->
            Pem
    end.

der_tlv(Tag, Content) ->
    Len = byte_size(Content),
    LenBin =
        if
            Len < 128 -> <<Len>>;
            Len < 256 -> <<16#81, Len>>;
            Len < 65536 -> <<16#82, (Len bsr 8), (Len band 255)>>
        end,
    <<Tag, LenBin/binary, Content/binary>>.

test_service_account_json() ->
    jsone:encode(#{
        <<"type">> => <<"service_account">>,
        <<"client_email">> => ?CLIENT_EMAIL,
        <<"private_key">> => test_rsa_pem(),
        <<"token_uri">> => ?TOKEN_URI
    }).

put_sa_config() ->
    application:set_env(imboy, push, [
        {fcm_service_account_json, test_service_account_json()}
    ]).

%% gun 桩：标准 HTTP/2 POST → {response, nofin, Status} → body
stub_gun_response(Status, RespBody) ->
    meck:expect(gun, open, fun(_Host, _Port, _Opts) -> {ok, self()} end),
    meck:expect(gun, await_up, fun(_Pid, _Timeout) -> {ok, http2} end),
    meck:expect(gun, post, fun(_Pid, _Path, _Headers, Body) ->
        self() ! {gun_post_body, Body},
        stream_ref
    end),
    meck:expect(gun, await, fun(_Pid, _Ref, _Timeout) ->
        {response, nofin, Status, []}
    end),
    meck:expect(gun, await_body, fun(_Pid, _Ref, _Timeout) ->
        {ok, RespBody}
    end),
    meck:expect(gun, close, fun(_Pid) -> ok end).

%% ===================================================================
%% 未配置：fail-closed，零出网
%% ===================================================================

not_configured_without_push_env_test() ->
    ?WITH_MECKS([gun, imboy_cache], fun() ->
        application:unset_env(imboy, push),
        meck:expect(gun, open, fun(_, _, _) -> erlang:error(unexpected_real_gun_open) end),
        meck:expect(imboy_cache, get, fun(_) -> undefined end),
        meck:expect(imboy_cache, set, fun(_, _, _) -> ok end),
        ?assertEqual({error, not_configured}, push_provider_fcm_token:get_access_token()),
        ?assertEqual(0, meck:num_calls(gun, open, '_'))
    end).

not_configured_with_static_only_env_test() ->
    %% push env 存在但没有 service account 配置 → 仍 not_configured
    %% （静态 token 由 push_notification_ds:get_fcm_config 自行处理）
    ?WITH_MECKS([gun, imboy_cache], fun() ->
        application:set_env(imboy, push, [{fcm_access_token, <<"static">>}]),
        meck:expect(gun, open, fun(_, _, _) -> erlang:error(unexpected_real_gun_open) end),
        meck:expect(imboy_cache, get, fun(_) -> undefined end),
        meck:expect(imboy_cache, set, fun(_, _, _) -> ok end),
        ?assertEqual({error, not_configured}, push_provider_fcm_token:get_access_token()),
        ?assertEqual(0, meck:num_calls(gun, open, '_'))
    end).

%% ===================================================================
%% 刷新链路：service account → RS256 JWT → token endpoint → 缓存
%% ===================================================================

refresh_flow_returns_and_caches_token_test() ->
    ?WITH_MECKS([gun, imboy_cache], fun() ->
        put_sa_config(),
        meck:expect(imboy_cache, get, fun(_) -> undefined end),
        meck:expect(imboy_cache, set, fun(?CACHE_KEY, Token, Ttl) ->
            self() ! {cache_set, Token, Ttl},
            ok
        end),
        stub_gun_response(
            200,
            jsone:encode(#{
                <<"access_token">> => ?FAKE_ACCESS_TOKEN,
                <<"expires_in">> => 3600
            })
        ),
        ?assertEqual({ok, ?FAKE_ACCESS_TOKEN}, push_provider_fcm_token:get_access_token()),
        %% 恰一次出网（token endpoint）
        ?assertEqual(1, meck:num_calls(gun, open, '_')),
        %% token 已入缓存
        receive
            {cache_set, ?FAKE_ACCESS_TOKEN, Ttl} -> ?assert(Ttl > 0)
        after 1000 ->
            erlang:error(token_not_cached)
        end,
        %% 请求体：jwt-bearer grant + 断言 JWT 存在
        receive
            {gun_post_body, Body} ->
                ?assert(
                    binary:match(Body, [
                        <<"grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer">>
                    ]) =/=
                        nomatch
                ),
                {AIdx, _MatchLen} = binary:match(Body, [<<"assertion=">>]),
                Jwt = binary:part(
                    Body,
                    AIdx + byte_size(<<"assertion=">>),
                    byte_size(Body) - AIdx - byte_size(<<"assertion=">>)
                ),
                assert_jwt_shape(Jwt)
        after 1000 ->
            erlang:error(post_body_not_captured)
        end
    end).

%% JWT 结构断言：header(alg=RS256) + claims(iss/aud/scope) 均为期望占位值
assert_jwt_shape(Jwt) ->
    [H, C, S] = binary:split(Jwt, <<".">>, [global]),
    ?assert(byte_size(S) > 0),
    Header = jsone:decode(b64url_decode(H)),
    ?assertEqual(<<"RS256">>, maps:get(<<"alg">>, Header)),
    Claims = jsone:decode(b64url_decode(C)),
    ?assertEqual(?CLIENT_EMAIL, maps:get(<<"iss">>, Claims)),
    ?assertEqual(?TOKEN_URI, maps:get(<<"aud">>, Claims)),
    ?assertEqual(
        <<"https://www.googleapis.com/auth/firebase.messaging">>,
        maps:get(<<"scope">>, Claims)
    ),
    ?assert(is_integer(maps:get(<<"iat">>, Claims))),
    ?assert(is_integer(maps:get(<<"exp">>, Claims))).

b64url_decode(Bin) ->
    B64 = <<
        <<
            (case C of
                $- -> $+;
                $_ -> $/;
                _ -> C
            end)
        >>
     || <<C>> <= Bin
    >>,
    Pad =
        case byte_size(B64) rem 4 of
            2 -> <<"==">>;
            3 -> <<"=">>;
            _ -> <<>>
        end,
    base64:decode(<<B64/binary, Pad/binary>>).

cache_hit_skips_endpoint_test() ->
    ?WITH_MECKS([gun, imboy_cache], fun() ->
        put_sa_config(),
        meck:expect(imboy_cache, get, fun(?CACHE_KEY) -> {ok, <<"cached-placeholder">>} end),
        meck:expect(gun, open, fun(_, _, _) -> erlang:error(unexpected_real_gun_open) end),
        ?assertEqual(
            {ok, <<"cached-placeholder">>}, push_provider_fcm_token:get_access_token()
        ),
        ?assertEqual(0, meck:num_calls(gun, open, '_'))
    end).

token_endpoint_bad_request_test() ->
    ?WITH_MECKS([gun, imboy_cache], fun() ->
        put_sa_config(),
        meck:expect(imboy_cache, get, fun(_) -> undefined end),
        meck:expect(imboy_cache, set, fun(_, _, _) -> ok end),
        stub_gun_response(400, <<"{\"error\":\"invalid_grant\"}">>),
        ?assertEqual(
            {error, {fcm_token_error, 400}},
            push_provider_fcm_token:get_access_token()
        ),
        ?assertEqual(0, meck:num_calls(imboy_cache, set, '_'))
    end).

%% service account JSON 缺 private_key → 配置类错误，零出网
malformed_service_account_zero_egress_test() ->
    ?WITH_MECKS([gun, imboy_cache], fun() ->
        application:set_env(imboy, push, [
            {fcm_service_account_json,
                jsone:encode(#{
                    <<"client_email">> => ?CLIENT_EMAIL,
                    <<"token_uri">> => ?TOKEN_URI
                })}
        ]),
        meck:expect(imboy_cache, get, fun(_) -> undefined end),
        meck:expect(gun, open, fun(_, _, _) -> erlang:error(unexpected_real_gun_open) end),
        ?assertMatch(
            {error, {config_error, _}},
            push_provider_fcm_token:get_access_token()
        ),
        ?assertEqual(0, meck:num_calls(gun, open, '_'))
    end).

%% 文件路径形态：fcm_service_account_path 指向的 JSON 同样可用
%% （占位内容写入临时文件，测后清理）
service_account_path_form_test() ->
    ?WITH_MECKS([gun, imboy_cache], fun() ->
        Tmp = "/tmp/imboy-fcm-sa-placeholder.json",
        ok = file:write_file(Tmp, test_service_account_json()),
        application:set_env(imboy, push, [{fcm_service_account_path, Tmp}]),
        meck:expect(imboy_cache, get, fun(_) -> undefined end),
        meck:expect(imboy_cache, set, fun(_, _, _) -> ok end),
        stub_gun_response(
            200,
            jsone:encode(#{
                <<"access_token">> => ?FAKE_ACCESS_TOKEN, <<"expires_in">> => 3600
            })
        ),
        try
            ?assertEqual({ok, ?FAKE_ACCESS_TOKEN}, push_provider_fcm_token:get_access_token()),
            ?assertEqual(1, meck:num_calls(gun, open, '_'))
        after
            file:delete(Tmp),
            application:unset_env(imboy, push)
        end
    end).
