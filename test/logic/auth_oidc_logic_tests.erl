-module(auth_oidc_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% auth_oidc_logic 模块的 EUnit 测试（P0-C OIDC 登录流）
%%%
%%% 覆盖：state 一次性消费（重放拒绝）/ nonce 不匹配拒绝 /
%%%       PKCE verifier 与 challenge 对应 / 身份映射命中 / email 关联 /
%%%       自动建号（含 quota 拒绝）/ OTC 一次性换取（含并发单赢家）/
%%%       统一错误消息不泄漏。
%%% 说明：httpc 用进程字典驱动的 meck 模拟（mock fun 在测试进程内执行），
%%%       不触网不触库；state/otc 走 auth_oidc_logic 自有 ETS 表（不 mock，
%%%       否则并发原子性测不出来）。
%%%===================================================================

-define(FAIL_MSG, <<"SSO 登录失败"/utf8>>).

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
        <<"issuer">> => <<"http://127.0.0.1:5556/dex">>,
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

%% httpc 模拟：token/userinfo 响应由测试进程字典驱动
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
            (get, _Req, _Opts, _Profile) ->
                {ok, {{v, 200, ok}, [], jsone:encode(erlang:get(oidc_tc_userinfo))}}
        end}
    ]}.

%% @doc 无签名 fake JWT（本流程只解 payload 段，签名由 TLS 信道边界覆盖）
fake_jwt(Claims) ->
    H = base64:encode(<<"{\"alg\":\"none\",\"typ\":\"JWT\"}">>, #{
        mode => urlsafe, padding => false
    }),
    P = base64:encode(jsone:encode(Claims), #{mode => urlsafe, padding => false}),
    <<H/binary, ".", P/binary, ".sig">>.

base_claims(Nonce) ->
    #{
        <<"iss">> => <<"http://127.0.0.1:5556/dex">>,
        <<"aud">> => <<"imboy">>,
        <<"exp">> => erlang:system_time(second) + 600,
        <<"nonce">> => Nonce,
        <<"sub">> => <<"sub-001">>
    }.

%% @doc 走 authorize 拿真实 state/nonce，并把回放素材写入进程字典
prime_flow(Client, UserInfo) ->
    {redirect, Url} = auth_oidc_logic:authorize(Client),
    #{query := Q} = uri_string:parse(Url),
    QP = maps:from_list(uri_string:dissect_query(Q)),
    State = maps:get(<<"state">>, QP),
    Nonce = maps:get(<<"nonce">>, QP),
    erlang:put(oidc_tc_id_token, fake_jwt(base_claims(Nonce))),
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
            %% 覆盖为错误 nonce 的 id_token
            erlang:put(oidc_tc_id_token, fake_jwt(base_claims(<<"wrong-nonce">>))),
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
