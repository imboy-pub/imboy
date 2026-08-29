-module(auth_oidc_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% auth_oidc_logic 模块的 EUnit 测试（P0-C OIDC 登录流 + SEC-01 验签接线）
%%%
%%% 覆盖：state 一次性消费（重放拒绝）/ nonce 不匹配拒绝 /
%%%       PKCE verifier 与 challenge 对应 / 身份映射命中 / email 关联 /
%%%       自动建号（含 quota 拒绝）/ OTC 一次性换取（含并发单赢家）/
%%%       统一错误消息不泄漏 / id_token JWKS 验签接线（正负向）。
%%% 说明：httpc 用进程字典驱动的 meck 模拟（mock fun 在测试进程内执行），
%%%       不触网不触库；state/otc 走 auth_oidc_logic 自有 ETS 表（不 mock，
%%%       否则并发原子性测不出来）。
%%% SEC-01：id_token 由测试内运行时生成的 RSA-2048 密钥真实签名
%%%       （fake IdP：discovery + JWKS 端点均由 httpc mock 提供），
%%%       不入任何密钥文件。
%%%===================================================================

-define(FAIL_MSG, <<"SSO 登录失败"/utf8>>).
-define(ISSUER, <<"http://127.0.0.1:5556/dex">>).
-define(JWKS_URI, <<"http://127.0.0.1:5556/dex/keys">>).
-define(TEST_KID, <<"kid-rsa-1">>).

%% ===================================================================
%% 测试夹具
%% ===================================================================

cfg() ->
    #{
        <<"enabled">> => true,
        <<"client_id">> => <<"imboy">>,
        <<"client_secret">> => <<"e2e-secret">>,
        <<"auth_url">> => <<"http://127.0.0.1:5556/dex/auth">>,
        <<"token_url">> => <<"http://127.0.0.1:5556/dex/token">>,
        <<"userinfo_url">> => <<"http://127.0.0.1:5556/dex/userinfo">>,
        <<"issuer">> => ?ISSUER,
        <<"redirect_uri">> => <<"http://127.0.0.1:9800/api/v1/auth/oidc/callback">>,
        <<"scopes">> => <<"openid profile email">>
    }.

sso_config_mock() ->
    {sso_config_ds, [{'get_provider', 1, fun(<<"oauth2">>) -> {ok, cfg()} end}]}.

rate_limit_mock() ->
    {login_attempt_ds, [
        {'check_ip_rate_limit', 1, fun(_Ip) -> {ok, 1} end},
        {'record_failure', 2, fun(_Id, _Ip) -> {ok, 1} end}
    ]}.

%% httpc 模拟：token/userinfo/discovery/JWKS 响应由测试进程字典驱动
%% （setup 阶段拿不到 authorize 生成的 nonce，须运行期注入 id_token）
dyn_httpc_mock() ->
    {httpc, [
        {'request', 4, fun
            (post, _Req, _Opts, _Profile) ->
                Body = jsone:encode(#{
                    <<"access_token">> => <<"at-1">>,
                    <<"id_token">> => erlang:get(oidc_tc_id_token),
                    <<"token_type">> => <<"bearer">>
                }),
                {ok, {{v, 200, ok}, [], Body}};
            (get, {Url, _Hdrs}, _Opts, _Profile) ->
                U = list_to_binary(Url),
                {ok, {{v, 200, ok}, [], jsone:encode(get_response(U))}}
        end}
    ]}.

get_response(U) ->
    IsDiscovery = binary:match(U, <<".well-known/openid-configuration">>) =/= nomatch,
    IsJwks = binary:match(U, <<"/dex/keys">>) =/= nomatch,
    if
        IsDiscovery ->
            %% OIDC discovery 文档（SEC-01：jwks_uri 经 discovery 解析）
            #{<<"issuer">> => ?ISSUER, <<"jwks_uri">> => ?JWKS_URI};
        IsJwks ->
            erlang:get(oidc_tc_jwks);
        true ->
            erlang:get(oidc_tc_userinfo)
    end.

%% @doc fake IdP 密钥初始化：测试内运行时生成 RSA-2048（不留存任何密钥文件），
%% JWKS 由进程字典驱动（支持负向用例运行期替换）
setup_idp() ->
    {[E, N], Priv} = crypto:generate_key(rsa, {2048, 65537}),
    Jwk = #{
        <<"kty">> => <<"RSA">>,
        <<"kid">> => ?TEST_KID,
        <<"n">> => b64url(N),
        <<"e">> => b64url(E),
        <<"use">> => <<"sig">>,
        <<"alg">> => <<"RS256">>
    },
    erlang:put(oidc_tc_jwks, #{<<"keys">> => [Jwk]}),
    erlang:put(oidc_tc_signer, Priv),
    %% 每个用例从冷 JWKS 缓存开始（跨用例隔离）
    ok = auth_oidc_jwks:clear_cache(),
    ok.

%% @doc 真实签名的 JWT（RS256，fake IdP 密钥）
signed_jwt(Claims) ->
    jwt_with_priv(erlang:get(oidc_tc_signer), #{}, Claims).

%% @doc 指定私钥签名（负向用例：伪造签名/换 key 轮换）
jwt_with_priv(Priv, HdrOverride, Claims) ->
    Hdr = maps:merge(
        #{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>, <<"kid">> => ?TEST_KID},
        HdrOverride
    ),
    H = b64url(jsone:encode(Hdr)),
    P = b64url(jsone:encode(Claims)),
    Sig = crypto:sign(rsa, sha256, <<H/binary, ".", P/binary>>, Priv),
    <<H/binary, ".", P/binary, ".", (b64url(Sig))/binary>>.

%% @doc 无签名 JWT（alg=none 攻击形态；SigOverride 填非空假签名段）
unsigned_jwt(Alg, SigSeg, Claims) ->
    H = b64url(
        jsone:encode(#{
            <<"alg">> => Alg, <<"typ">> => <<"JWT">>, <<"kid">> => ?TEST_KID
        })
    ),
    P = b64url(jsone:encode(Claims)),
    <<H/binary, ".", P/binary, ".", SigSeg/binary>>.

b64url(B) ->
    base64:encode(B, #{mode => urlsafe, padding => false}).

base_claims(Nonce) ->
    #{
        <<"iss">> => ?ISSUER,
        <<"aud">> => <<"imboy">>,
        <<"exp">> => erlang:system_time(second) + 600,
        <<"nonce">> => Nonce,
        <<"sub">> => <<"sub-001">>
    }.

%% @doc 走 authorize 拿真实 state/nonce，并把回放素材写入进程字典
prime_flow(Client, UserInfo) ->
    ok = setup_idp(),
    {redirect, Url} = auth_oidc_logic:authorize(Client),
    #{query := Q} = uri_string:parse(Url),
    QP = maps:from_list(uri_string:dissect_query(Q)),
    State = maps:get(<<"state">>, QP),
    Nonce = maps:get(<<"nonce">>, QP),
    erlang:put(oidc_tc_id_token, signed_jwt(base_claims(Nonce))),
    erlang:put(oidc_tc_userinfo, UserInfo),
    {State, Nonce, QP}.

%% ===================================================================
%% authorize/1
%% ===================================================================

%% @doc provider 未配置 -> 统一错误，不泄配置状态细节
authorize_provider_missing_test_() ->
    ?WITH_MECKS(
        [{sso_config_ds, [{'get_provider', 1, fun(_) -> {error, not_found} end}]}],
        fun() ->
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:authorize(<<"app">>))
        end
    ).

%% @doc provider 未启用 -> 统一错误
authorize_provider_disabled_test_() ->
    Cfg = maps:put(<<"enabled">>, false, cfg()),
    ?WITH_MECKS(
        [{sso_config_ds, [{'get_provider', 1, fun(_) -> {ok, Cfg} end}]}],
        fun() ->
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:authorize(<<"app">>))
        end
    ).

%% @doc 非 https 且非本机 token_url -> 拒绝（JWKS 验签延后的信道边界）
authorize_insecure_token_url_test_() ->
    Cfg = maps:put(<<"token_url">>, <<"http://idp.example.com/token">>, cfg()),
    ?WITH_MECKS(
        [{sso_config_ds, [{'get_provider', 1, fun(_) -> {ok, Cfg} end}]}],
        fun() ->
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:authorize(<<"app">>))
        end
    ).

%% @doc PKCE：URL 中 code_challenge == base64url(sha256(缓存中的 verifier))；
%%      state/nonce 与缓存一致；method 固定 S256
authorize_pkce_challenge_test_() ->
    ?WITH_MECKS(
        [sso_config_mock()],
        fun() ->
            {State, Nonce, QP} = prime_flow(<<"app">>, #{}),
            [{_, #{nonce := CachedNonce, verifier := Verifier, client := Client}, _Exp}] =
                ets:lookup(imboy_oidc_onetime, {oidc_state, State}),
            ?assertEqual(<<"app">>, Client),
            ?assertEqual(CachedNonce, Nonce),
            ?assertEqual(<<"S256">>, maps:get(<<"code_challenge_method">>, QP)),
            Expected = base64:encode(crypto:hash(sha256, Verifier), #{
                mode => urlsafe, padding => false
            }),
            ?assertEqual(Expected, maps:get(<<"code_challenge">>, QP)),
            ?assertEqual(<<"code">>, maps:get(<<"response_type">>, QP)),
            ?assertEqual(<<"imboy">>, maps:get(<<"client_id">>, QP))
        end
    ).

%% ===================================================================
%% callback/2 —— state / nonce / 错误路径
%% ===================================================================

%% @doc state 一次性消费：首次成功，重放同一 callback 必失败
callback_state_replay_test_() ->
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            dyn_httpc_mock(),
            {sso_identity_ds, [{'find_uid', 2, fun(_, _) -> {ok, 55} end}]},
            {user_ds, [
                {'find_by_id', 2, fun(55, _) -> #{<<"id">> => 55, <<"status">> => 1} end}
            ]},
            {passport_logic, [
                {'login_resp', 2, fun(U, R) ->
                    maps:merge(R, #{<<"uid">> => maps:get(<<"id">>, U), <<"token">> => <<"jwt-1">>})
                end}
            ]}
        ],
        fun() ->
            {State, _Nonce, _} = prime_flow(
                <<"test">>, #{<<"sub">> => <<"sub-001">>, <<"email">> => <<"t@x.com">>}
            ),
            Params = #{<<"code">> => <<"code-1">>, <<"state">> => State},
            %% 首次：成功签发
            {ok, Payload} = auth_oidc_logic:callback(Params, <<"1.2.3.4">>),
            ?assertEqual(<<"jwt-1">>, maps:get(<<"token">>, Payload)),
            ?assertEqual(55, maps:get(<<"uid">>, Payload)),
            %% 重放：state 已被一次性消费 -> 统一错误
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:callback(Params, <<"1.2.3.4">>))
        end
    ).

%% @doc id_token nonce 与缓存不符 -> 拒绝
callback_nonce_mismatch_test_() ->
    ?WITH_MECKS(
        [sso_config_mock(), rate_limit_mock(), dyn_httpc_mock()],
        fun() ->
            {State, _Nonce, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            %% 覆盖为错误 nonce 的 id_token（签名有效，claims 错）
            erlang:put(oidc_tc_id_token, signed_jwt(base_claims(<<"wrong-nonce">>))),
            Params = #{<<"code">> => <<"code-1">>, <<"state">> => State},
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:callback(Params, <<"1.2.3.4">>))
        end
    ).

%% @doc 伪造/未知 state -> 拒绝
callback_forged_state_test_() ->
    ?WITH_MECKS(
        [rate_limit_mock()],
        fun() ->
            Params = #{<<"code">> => <<"c">>, <<"state">> => <<"forged-state">>},
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:callback(Params, <<"1.2.3.4">>))
        end
    ).

%% @doc IdP error 参数（用户取消授权）-> 友好失败，不崩
callback_idp_error_param_test_() ->
    ?WITH_MECKS(
        [rate_limit_mock()],
        fun() ->
            Params = #{<<"error">> => <<"access_denied">>},
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:callback(Params, <<"1.2.3.4">>))
        end
    ).

%% @doc token endpoint 失败：响应体不含 IdP 原始错误细节（统一错误消息）
callback_token_error_unified_msg_test_() ->
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            {httpc, [
                {'request', 4, fun(post, _Req, _Opts, _Profile) ->
                    Leak =
                        <<"{\"error\":\"invalid_grant\",\"error_description\":\"secret-detail-xyz\"}">>,
                    {ok, {{v, 400, bad}, [], Leak}}
                end}
            ]}
        ],
        fun() ->
            {State, _, _} = prime_flow(<<"test">>, #{}),
            Params = #{<<"code">> => <<"bad-code">>, <<"state">> => State},
            {error, Msg} = auth_oidc_logic:callback(Params, <<"1.2.3.4">>),
            ?assertEqual(?FAIL_MSG, Msg),
            ?assertEqual(nomatch, binary:match(Msg, <<"invalid_grant">>)),
            ?assertEqual(nomatch, binary:match(Msg, <<"secret-detail-xyz">>))
        end
    ).

%% ===================================================================
%% callback/2 —— 账号映射 / 自动建号
%% ===================================================================

%% @doc 映射未命中但 email 匹配现有用户 -> 关联而非重复建号
callback_email_link_test_() ->
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            dyn_httpc_mock(),
            {sso_identity_ds, [
                {'find_uid', 2, fun(_, _) -> not_found end},
                {'bind', 4, fun(<<"oauth2">>, <<"sub-001">>, 77, <<"t@x.com">>) -> ok end}
            ]},
            {user_ds, [
                {'find_id_by_email', 1, fun(<<"t@x.com">>) -> 77 end},
                {'find_by_id', 2, fun(77, _) -> #{<<"id">> => 77, <<"status">> => 1} end}
            ]},
            {passport_logic, [
                {'login_resp', 2, fun(U, _) ->
                    #{<<"uid">> => maps:get(<<"id">>, U), <<"token">> => <<"jwt-77">>}
                end}
            ]}
        ],
        fun() ->
            %% 安全契约：email_verified=true 才允许按 email 关联现有账号
            {State, _Nonce, _} = prime_flow(
                <<"test">>, #{
                    <<"sub">> => <<"sub-001">>,
                    <<"email">> => <<"t@x.com">>,
                    <<"email_verified">> => true
                }
            ),
            {ok, Payload} = auth_oidc_logic:callback(
                #{<<"code">> => <<"c">>, <<"state">> => State}, <<"1.2.3.4">>
            ),
            ?assertEqual(77, maps:get(<<"uid">>, Payload))
        end
    ).

%% @doc 安全：email 未验证（无 email_verified）-> 禁止按 email 关联现有账号，走建号路径
%% 断言 bind 目标不是既有 uid 77（即没走 find_id_by_email 关联分支）
callback_unverified_email_no_link_test_() ->
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            dyn_httpc_mock(),
            {sso_identity_ds, [
                {'find_uid', 2, fun(_, _) -> not_found end},
                {'bind', 4, fun(<<"oauth2">>, <<"sub-001">>, Uid, _) ->
                    %% 关键断言：绝不绑定到 email 命中的既有账号 77
                    ?assertNotEqual(77, Uid),
                    ok
                end}
            ]},
            {user_ds, [
                %% 若被错误调用即视为越权关联，断言失败
                {'find_id_by_email', 1, fun(_) ->
                    erlang:error(should_not_lookup_email_when_unverified)
                end},
                {'find_by_id', 2, fun(Id, _) -> #{<<"id">> => Id, <<"status">> => 1} end},
                {'insert_and_get_id', 1, fun(_) -> {ok, 999} end}
            ]},
            {passport_logic, [
                {'quota_guard', 0, fun() -> ok end},
                {'pick_data_for_insert', 2, fun(Base, Extra) -> maps:merge(Base, Extra) end},
                {'login_resp', 2, fun(U, _) ->
                    #{<<"uid">> => maps:get(<<"id">>, U), <<"token">> => <<"jwt">>}
                end}
            ]}
        ],
        fun() ->
            {State, _Nonce, _} = prime_flow(
                <<"test">>, #{
                    <<"sub">> => <<"sub-001">>, <<"email">> => <<"t@x.com">>
                }
            ),
            {ok, Payload} = auth_oidc_logic:callback(
                #{<<"code">> => <<"c">>, <<"state">> => State}, <<"1.2.3.4">>
            ),
            ?assertEqual(999, maps:get(<<"uid">>, Payload))
        end
    ).

%% @doc 全新用户（userinfo 无 email）-> 过 quota gate 自动建号 + 建映射，不 crash
callback_provision_test_() ->
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            dyn_httpc_mock(),
            {sso_identity_ds, [
                {'find_uid', 2, fun(_, _) -> not_found end},
                {'bind', 4, fun(<<"oauth2">>, <<"sub-001">>, 88, _) -> ok end}
            ]},
            {user_ds, [
                {'find_id_by_email', 1, fun(_) -> 0 end},
                {'insert_and_get_id', 1, fun(Data) ->
                    %% 建号数据必须含随机密码与 oidc 来源
                    ?assertMatch(<<_/binary>>, maps:get(<<"password">>, Data)),
                    ?assertEqual(<<"oidc">>, maps:get(<<"source">>, Data)),
                    {ok, 88}
                end},
                {'find_by_id', 2, fun(88, _) -> #{<<"id">> => 88, <<"status">> => 1} end}
            ]},
            {passport_logic, [
                {'quota_guard', 0, fun() -> ok end},
                {'pick_data_for_insert', 2, fun(Base, PostVals) ->
                    maps:merge(PostVals, Base)
                end},
                {'login_resp', 2, fun(U, _) ->
                    #{<<"uid">> => maps:get(<<"id">>, U), <<"token">> => <<"jwt-88">>}
                end}
            ]}
        ],
        fun() ->
            %% userinfo 无 email：仅按 sub 建号
            {State, _Nonce, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            {ok, Payload} = auth_oidc_logic:callback(
                #{<<"code">> => <<"c">>, <<"state">> => State}, <<"1.2.3.4">>
            ),
            ?assertEqual(88, maps:get(<<"uid">>, Payload))
        end
    ).

%% @doc License 满额 -> 402 语义拒绝（quota_guard 复用，不绕过）
callback_quota_rejected_test_() ->
    QuotaMsg = <<"用户数已达授权上限"/utf8>>,
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            dyn_httpc_mock(),
            {sso_identity_ds, [{'find_uid', 2, fun(_, _) -> not_found end}]},
            {user_ds, [{'find_id_by_email', 1, fun(_) -> 0 end}]},
            {passport_logic, [
                {'quota_guard', 0, fun() -> {error, QuotaMsg, 402} end}
            ]}
        ],
        fun() ->
            {State, _Nonce, _} = prime_flow(
                <<"test">>, #{<<"sub">> => <<"sub-001">>, <<"email">> => <<"new@x.com">>}
            ),
            Res = auth_oidc_logic:callback(
                #{<<"code">> => <<"c">>, <<"state">> => State}, <<"1.2.3.4">>
            ),
            ?assertEqual({error, QuotaMsg, 402}, Res)
        end
    ).

%% ===================================================================
%% client=app OTC 深链 + exchange/1 一次性
%% ===================================================================

%% @doc app 客户端：callback 302 到 imboy:// 深链；otc 只能兑换一次
otc_exchange_one_time_test_() ->
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            dyn_httpc_mock(),
            {sso_identity_ds, [{'find_uid', 2, fun(_, _) -> {ok, 55} end}]},
            {user_ds, [
                {'find_by_id', 2, fun(55, _) -> #{<<"id">> => 55, <<"status">> => 1} end}
            ]},
            {passport_logic, [
                {'login_resp', 2, fun(_, _) ->
                    #{<<"uid">> => 55, <<"token">> => <<"jwt-app">>}
                end}
            ]}
        ],
        fun() ->
            {State, _Nonce, _} = prime_flow(<<"app">>, #{<<"sub">> => <<"sub-001">>}),
            {redirect, DeepLink} = auth_oidc_logic:callback(
                #{<<"code">> => <<"c">>, <<"state">> => State}, <<"1.2.3.4">>
            ),
            ?assertMatch({0, _}, binary:match(DeepLink, <<"imboy://oidc/callback?otc=">>)),
            [_, Otc] = binary:split(DeepLink, <<"otc=">>),
            %% 首次兑换成功
            {ok, Payload} = auth_oidc_logic:exchange(Otc),
            ?assertEqual(<<"jwt-app">>, maps:get(<<"token">>, Payload)),
            %% 重放兑换失败（一次性）
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:exchange(Otc))
        end
    ).

%% @doc 并发消费同一 OTC：N 个进程同时兑换，有且只有一个成功。
%% 关键：本用例**不** mock imboy_cache/存储层 —— 缺陷本身就是"查+删非原子"，
%%       mock 成进程字典（各进程独立）会把并发窗口一起 mock 掉，测不出问题。
otc_concurrent_exchange_single_winner_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            [
                sso_config_mock(),
                rate_limit_mock(),
                dyn_httpc_mock(),
                {sso_identity_ds, [{'find_uid', 2, fun(_, _) -> {ok, 55} end}]},
                {user_ds, [
                    {'find_by_id', 2, fun(55, _) -> #{<<"id">> => 55, <<"status">> => 1} end}
                ]},
                {passport_logic, [
                    {'login_resp', 2, fun(_, _) ->
                        #{<<"uid">> => 55, <<"token">> => <<"jwt-app">>}
                    end}
                ]}
            ],
            fun() ->
                {State, _Nonce, _} = prime_flow(<<"app">>, #{<<"sub">> => <<"sub-001">>}),
                {redirect, DeepLink} = auth_oidc_logic:callback(
                    #{<<"code">> => <<"c">>, <<"state">> => State}, <<"1.2.3.4">>
                ),
                [_, Otc] = binary:split(DeepLink, <<"otc=">>),
                N = 32,
                Results = race_exchange(Otc, N),
                ?assertEqual(N, length(Results)),
                Winners = [R || {ok, _} = R <- Results],
                %% 一次性码被兑换两次即可重放 -> 同一登录凭据签出多份 token
                ?assertEqual(1, length(Winners)),
                %% 其余全部走统一失败出口
                ?assertEqual(
                    N - 1, length([R || R <- Results, R =:= {error, ?FAIL_MSG}])
                )
            end
        )}.

%% @doc N 个进程在同一栅栏上同时调用 exchange/1，收集全部返回值
race_exchange(Otc, N) ->
    Parent = self(),
    Pids = [
        spawn(fun() ->
            receive
                go -> ok
            end,
            Parent ! {self(), auth_oidc_logic:exchange(Otc)}
        end)
     || _ <- lists:seq(1, N)
    ],
    _ = [P ! go || P <- Pids],
    [
        receive
            {P, R} -> R
        after 5000 -> timeout
        end
     || P <- Pids
    ].

%% @doc 过期语义：已过期的 otc 不可兑换；清扫只回收过期项，在途凭据必须留下
%%（清扫用的是 match spec，写错要么回收不掉，要么误删在途 state 打断正常登录）
otc_expiry_and_sweep_test_() ->
    ?WITH_MECKS(
        [sso_config_mock()],
        fun() ->
            Tab = imboy_oidc_onetime,
            %% 在途 state（TTL 600s）
            {State, _Nonce, _} = prime_flow(<<"app">>, #{}),
            Past = erlang:system_time(second) - 1,
            true = ets:insert(Tab, {{oidc_otc, <<"stale-otc">>}, #{}, Past}),
            %% 过期即不可兑换
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:exchange(<<"stale-otc">>)),
            %% 未被兑换的过期残留由清扫回收
            true = ets:insert(Tab, {{oidc_state, <<"stale-state">>}, #{}, Past}),
            ok = auth_oidc_logic:sweep_expired(),
            ?assertEqual([], ets:lookup(Tab, {oidc_state, <<"stale-state">>})),
            %% 在途 state 不能被误删
            ?assertMatch([{_, _, _}], ets:lookup(Tab, {oidc_state, State}))
        end
    ).

%% @doc 空/未知 otc -> 拒绝
exchange_invalid_otc_test_() ->
    ?WITH_MECKS(
        [],
        fun() ->
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:exchange(<<>>)),
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:exchange(<<"nope">>))
        end
    ).

%% ===================================================================
%% SEC-01：id_token JWKS 验签接线（默认启用，fail-closed）
%% ===================================================================

%% @doc 伪造签名：攻击者自签 key 签发 claims 全对的 id_token -> 拒绝（不落会话）
callback_forged_signature_test_() ->
    ?WITH_MECKS(
        [sso_config_mock(), rate_limit_mock(), dyn_httpc_mock()],
        fun() ->
            {State, Nonce, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            %% 攻击者密钥（不在 JWKS 中）签名，kid 冒充 IdP 的 kid
            {_, AttackerPriv} = crypto:generate_key(rsa, {2048, 65537}),
            erlang:put(
                oidc_tc_id_token,
                jwt_with_priv(AttackerPriv, #{}, base_claims(Nonce))
            ),
            Params = #{<<"code">> => <<"c">>, <<"state">> => State},
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:callback(Params, <<"1.2.3.4">>))
        end
    ).

%% @doc alg=none（无签名段）-> 拒绝（两种攻击形态：空签名段 / 假签名段）
callback_alg_none_test_() ->
    ?WITH_MECKS(
        [sso_config_mock(), rate_limit_mock(), dyn_httpc_mock()],
        fun() ->
            %% 形态 1：规范 alg=none，第三段为空
            {S1, N1, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            erlang:put(
                oidc_tc_id_token,
                unsigned_jwt(<<"none">>, <<>>, base_claims(N1))
            ),
            ?assertEqual(
                {error, ?FAIL_MSG},
                auth_oidc_logic:callback(
                    #{<<"code">> => <<"c">>, <<"state">> => S1}, <<"1.2.3.4">>
                )
            ),
            %% 形态 2：alg=none 但带非空假签名段，同样拒绝
            {S2, N2, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            erlang:put(
                oidc_tc_id_token,
                unsigned_jwt(<<"none">>, <<"c2ln">>, base_claims(N2))
            ),
            ?assertEqual(
                {error, ?FAIL_MSG},
                auth_oidc_logic:callback(
                    #{<<"code">> => <<"c">>, <<"state">> => S2}, <<"1.2.3.4">>
                )
            )
        end
    ).

%% @doc HS256（白名单外 + HMAC 算法混淆面）-> 拒绝
callback_hs256_test_() ->
    ?WITH_MECKS(
        [sso_config_mock(), rate_limit_mock(), dyn_httpc_mock()],
        fun() ->
            {State, Nonce, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            erlang:put(
                oidc_tc_id_token,
                unsigned_jwt(<<"HS256">>, <<"fake-hmac">>, base_claims(Nonce))
            ),
            Params = #{<<"code">> => <<"c">>, <<"state">> => State},
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:callback(Params, <<"1.2.3.4">>))
        end
    ).

%% @doc 签名有效但 claims 错（iss/aud/exp 任一不符）-> 拒绝；
%%      过期与 issuer/audience 误配都必须挡在会话建立之前
callback_claims_rejected_test_() ->
    ?WITH_MECKS(
        [sso_config_mock(), rate_limit_mock(), dyn_httpc_mock()],
        fun() ->
            ParamsOf = fun(State) -> #{<<"code">> => <<"c">>, <<"state">> => State} end,
            %% 错误 issuer
            {S1, N1, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            C1 = base_claims(N1),
            erlang:put(
                oidc_tc_id_token,
                signed_jwt(C1#{<<"iss">> => <<"http://evil.example.com">>})
            ),
            ?assertEqual(
                {error, ?FAIL_MSG},
                auth_oidc_logic:callback(ParamsOf(S1), <<"1.2.3.4">>)
            ),
            %% 错误 audience
            {S2, N2, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            C2 = base_claims(N2),
            erlang:put(
                oidc_tc_id_token,
                signed_jwt(C2#{<<"aud">> => <<"other-client">>})
            ),
            ?assertEqual(
                {error, ?FAIL_MSG},
                auth_oidc_logic:callback(ParamsOf(S2), <<"1.2.3.4">>)
            ),
            %% 过期 token
            {S3, N3, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            C3 = base_claims(N3),
            erlang:put(
                oidc_tc_id_token,
                signed_jwt(C3#{<<"exp">> => erlang:system_time(second) - 1})
            ),
            ?assertEqual(
                {error, ?FAIL_MSG},
                auth_oidc_logic:callback(ParamsOf(S3), <<"1.2.3.4">>)
            )
        end
    ).

%% @doc JWKS 拉取失败（discovery 成功、keys 端点网络错）-> 拒绝（不用陈旧 key 兜底）
callback_jwks_fetch_failed_test_() ->
    HttpMock =
        {httpc, [
            {'request', 4, fun
                (post, _Req, _Opts, _Profile) ->
                    Body = jsone:encode(#{
                        <<"access_token">> => <<"at-1">>,
                        <<"id_token">> => erlang:get(oidc_tc_id_token),
                        <<"token_type">> => <<"bearer">>
                    }),
                    {ok, {{v, 200, ok}, [], Body}};
                (get, {Url, _Hdrs}, _Opts, _Profile) ->
                    U = list_to_binary(Url),
                    case binary:match(U, <<"/dex/keys">>) of
                        nomatch -> {ok, {{v, 200, ok}, [], jsone:encode(get_response(U))}};
                        _ -> {error, econnrefused}
                    end
            end}
        ]},
    ?WITH_MECKS(
        [sso_config_mock(), rate_limit_mock(), HttpMock],
        fun() ->
            {State, Nonce, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
            erlang:put(oidc_tc_id_token, signed_jwt(base_claims(Nonce))),
            Params = #{<<"code">> => <<"c">>, <<"state">> => State},
            ?assertEqual({error, ?FAIL_MSG}, auth_oidc_logic:callback(Params, <<"1.2.3.4">>))
        end
    ).

%% @doc 存量部署显式降级：oidc_verify_signature=false 时回到仅解码路径
%%（alg=none 的 token 在降级模式下按旧行为放行；默认无此 env 时验签必启，
%% 见 callback_alg_none_test_ / callback_forged_signature_test_）
callback_legacy_verification_off_test_() ->
    ?WITH_MECKS(
        [
            sso_config_mock(),
            rate_limit_mock(),
            dyn_httpc_mock(),
            {sso_identity_ds, [{'find_uid', 2, fun(_, _) -> {ok, 55} end}]},
            {user_ds, [
                {'find_by_id', 2, fun(55, _) -> #{<<"id">> => 55, <<"status">> => 1} end}
            ]},
            {passport_logic, [
                {'login_resp', 2, fun(_, _) -> #{<<"uid">> => 55, <<"token">> => <<"jwt-1">>} end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, oidc_verify_signature, false),
            try
                {State, Nonce, _} = prime_flow(<<"test">>, #{<<"sub">> => <<"sub-001">>}),
                %% alg=none 无签名 token 在降级模式下按旧行为放行（仅 claims 校验）
                erlang:put(
                    oidc_tc_id_token,
                    unsigned_jwt(<<"none">>, <<>>, base_claims(Nonce))
                ),
                Params = #{<<"code">> => <<"c">>, <<"state">> => State},
                ?assertMatch(
                    {ok, #{<<"uid">> := 55}},
                    auth_oidc_logic:callback(Params, <<"1.2.3.4">>)
                )
            after
                application:unset_env(imboy, oidc_verify_signature)
            end
        end
    ).

%% ===================================================================
%% 多节点一次性状态自检（C0-OPS/C0-IAM）
%% state/otc 存节点本地 ETS，多节点无粘性会话时回调会随机打到取不到
%% state 的节点。判定口径必须与 deploy/preflight.sh 的 4b 段一致。
%% ===================================================================

state_sharing_single_node_ok_test() ->
    %% 单节点（0 个对端）无论是否粘性都安全
    ?assertEqual(ok, auth_oidc_logic:state_sharing_status(0, false)),
    ?assertEqual(ok, auth_oidc_logic:state_sharing_status(0, true)).

state_sharing_multinode_without_sticky_rejected_test() ->
    ?assertEqual(
        {error, oidc_state_not_shared},
        auth_oidc_logic:state_sharing_status(1, false)
    ),
    ?assertEqual(
        {error, oidc_state_not_shared},
        auth_oidc_logic:state_sharing_status(5, false)
    ).

state_sharing_multinode_with_sticky_ok_test() ->
    ?assertEqual(ok, auth_oidc_logic:state_sharing_status(1, true)),
    ?assertEqual(ok, auth_oidc_logic:state_sharing_status(9, true)).

%% 自检只告警不阻断：单节点承载回调时功能本身是好的，硬失败会误伤。
%% 真正的硬闸门在 preflight，这里只保证不抛异常、不改变 authorize 行为。
warn_if_state_not_shared_never_raises_test() ->
    ?assertEqual(ok, auth_oidc_logic:warn_if_state_not_shared()).
