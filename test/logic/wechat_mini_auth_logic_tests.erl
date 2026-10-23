%% wechat_mini_auth_logic_tests
%% AUTH-01：微信小程序登录 — code 重放/无效、无效 provider（未配置）、
%% 首登自动开户（方案 B）/幂等/失败折叠/额度上限、网络失败、
%% 成功签发（响应不含 openid/session_key）。
%% 全部外呼/配置/映射均 meck，无真实网络、无真实 AppSecret。

-module(wechat_mini_auth_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 98001).
-define(OPENID, <<"oMOYA_test_openid_0001">>).
%% 真实量级 uid（生产实测 min/max：9000000000000000001）——19 位，远超
%% JS 安全整数 2^53。用于锁死「uid 必须成字符串下发」这条硬规则。
-define(BIG_UID, 9000000000000000001).

%%%===================================================================
%%% Provider 未配置（AUTH-01：无效 provider 路径真实可测）
%%%===================================================================

provider_unconfigured_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (wechat_mini_appid, _) -> <<>>;
                    (wechat_mini_secret, _) -> <<>>;
                    (_, Default) -> Default
                end}
            ]},
            {wechat_mini_client, [
                {'jscode2session', 3, fun(_, _, _) -> {error, invalid_code} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_unconfigured},
                wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            )
        end
    ).

%%%===================================================================
%%% code 无效 / 微信侧 errcode（含 40163 重放）→ 5402 路径
%%%===================================================================

code_invalid_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {error, invalid_code} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, code_invalid},
                wechat_mini_auth_logic:wechat_mini_login(#{code => <<"replayed_or_bad">>})
            )
        end
    ).

%% code 重放：同 code 第二次调用（微信侧已消费 → 40163）同样 code_invalid
code_replay_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, Code) ->
                        case Code of
                            <<"first_time_ok">> ->
                                case get({replay_counter, Code}) of
                                    undefined ->
                                        put({replay_counter, Code}, 1),
                                        {ok, ?OPENID};
                                    _ ->
                                        %% 40163: code been used → 折叠为 invalid_code（T11）
                                        {error, invalid_code}
                                end;
                            _ ->
                                {error, invalid_code}
                        end
                    end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(_, _) -> {ok, ?UID} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(_) -> <<"at_x">> end},
                    {'encrypt_refreshtoken', 2, fun(_, _) -> <<"rt_x">> end}
                ]}
            ]
        ],
        fun() ->
            R1 = wechat_mini_auth_logic:wechat_mini_login(#{code => <<"first_time_ok">>}),
            ?assertMatch({ok, _}, R1),
            R2 = wechat_mini_auth_logic:wechat_mini_login(#{code => <<"first_time_ok">>}),
            ?assertEqual({error, code_invalid}, R2),
            erase({replay_counter, <<"first_time_ok">>})
        end
    ).

%%%===================================================================
%%% 网络失败 → 5401 路径（login_failed）
%%%===================================================================

network_error_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {error, network} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, login_failed},
                wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            )
        end
    ).

%%%===================================================================
%%% 首登自动开户（2026-09-20 试点方案 B）
%%%
%%% 契约变更：sso_identity 无映射时**不再**返回 identity_none（5404），
%%% 而是在本次请求内自动开户（user 行 + sso_identity 映射）并签发 token。
%%% 原「机构侧建立绑定」在实现上不可执行：openid 只在服务端 jscode2session
%%% 那一次可见，不落库、不落日志（log_redact 把 openid 列为脱敏键），
%%% 机构侧拿不到它就无法预先写 sso_identity ⇒ 新用户永远进不了门。
%%%===================================================================

first_login_provisions_and_issues_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {wechat_mini_identity_ds, [
                    {'provision_and_bind', 3, fun(<<"wechat_mini">>, ?OPENID, _Opts) ->
                        {ok, ?UID}
                    end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_provisioned">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_provisioned">> end}
                ]}
            ]
        ],
        fun() ->
            {ok, Payload} = wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>}),
            ?assertEqual(<<"at_provisioned">>, maps:get(token, Payload)),
            %% 全新账号必然没有教学身份 → 客户端 routeByContexts 落到 no-identity 页
            %% 「等待老师开通」，再由老师用 learners/:id/bind 建立用户关系
            %% 开户恰好发生一次
            ?assertEqual(1, meck:num_calls(wechat_mini_identity_ds, provision_and_bind, 3)),
            %% uid 随登录下发，且必须是**字符串**（闭环最后一段靠它：
            %% 用户把 uid 报给老师，老师据此调 learners/:id/bind）
            ?assertEqual(integer_to_binary(?UID), maps:get(uid, Payload)),
            %% 响应键集合仍不含 openid（身份映射层外泄=零容忍）
            ?assertEqual(false, lists:member(openid, maps:keys(Payload)))
        end
    ).

%% 真实量级 uid（19 位）必须以字符串下发。
%% 反例的形状：以 number 下发 ⇒ JS 的 JSON.parse 把它折成 ...000
%% （9223372036854775807 → 9223372036854776000）。它仍然「能解析、能显示、
%% 能提交」，只是用户报给老师的号不是老师要绑的号 —— 全链路无一门禁会报。
uid_is_string_for_big_id_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?BIG_UID} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?BIG_UID) -> <<"at_big">> end},
                    {'encrypt_refreshtoken', 2, fun(?BIG_UID, <<>>) -> <<"rt_big">> end}
                ]}
            ]
        ],
        fun() ->
            {ok, Payload} = wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>}),
            Uid = maps:get(uid, Payload),
            ?assertEqual(false, is_integer(Uid)),
            ?assertEqual(<<"9000000000000000001">>, Uid),
            %% 19 位原样，无截断/进位
            ?assertEqual(19, byte_size(Uid))
        end
    ).

%% 已开户的老用户绝不重复建号（否则每次登录都烧掉一个 account 号 + 一行 user）
known_user_skips_provision_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?UID} end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {wechat_mini_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {error, unreachable} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_known">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_known">> end}
                ]}
            ]
        ],
        fun() ->
            {ok, _} = wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>}),
            ?assertEqual(0, meck:num_calls(wechat_mini_identity_ds, provision_and_bind, 3)),
            %% 命中既有映射时连配额检查都不该走（不做建号动作）
            ?assertEqual(0, meck:num_calls(passport_logic, quota_guard, 0))
        end
    ).

%% 请求上下文（device_id / ip）必须透传到开户层 —— reg_ip 是 NOT NULL 列，
%% 漏传就会被 user 层兜成 127.0.0.1（运维看到全部用户来自本机）
provision_receives_request_context_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {wechat_mini_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, Opts) ->
                        put(provision_opts, Opts),
                        {ok, ?UID}
                    end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_ctx">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_ctx">> end}
                ]}
            ]
        ],
        fun() ->
            {ok, _} = wechat_mini_auth_logic:wechat_mini_login(#{
                code => <<"good_code_123">>, device_id => <<"dev-1">>, ip => <<"203.0.113.9">>
            }),
            Opts = erase(provision_opts),
            ?assertEqual(<<"203.0.113.9">>, maps:get(ip, Opts)),
            ?assertEqual(<<"dev-1">>, maps:get(device_id, Opts))
        end
    ).

%% 开户失败（DB/分配异常）折叠为 5401；绝不因开户失败而假装登录成功
provision_failure_folds_to_login_failed_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {wechat_mini_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {error, db_error} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, login_failed},
                wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            )
        end
    ).

%% License 用户数上限：**永久**条件，必须单独成码（402）而不是并进 5401 ——
%% 否则用户看到的是可重试的文案 + 一个永远不会成功的重试按钮（5404 同款坑）
provision_quota_exceeded_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [
                    {'quota_guard', 0, fun() -> {error, <<"license detail">>, 402} end}
                ]},
                {wechat_mini_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {ok, ?UID} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, account_quota_exceeded},
                wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            ),
            %% 配额耗尽时不得再尝试建号（免绕过 License gate）
            ?assertEqual(0, meck:num_calls(wechat_mini_identity_ds, provision_and_bind, 3))
        end
    ).

%% 查映射本身失败（DB 抖动）绝不退化为「当作新用户开户」——
%% 那会在 DB 抖动时批量造重复账号
lookup_error_does_not_provision_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {error, timeout} end}
                ]},
                {wechat_mini_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {ok, ?UID} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, login_failed},
                wechat_mini_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            ),
            ?assertEqual(0, meck:num_calls(wechat_mini_identity_ds, provision_and_bind, 3))
        end
    ).

%%%===================================================================
%%% 成功签发：payload 无 openid/session_key（身份映射层外泄=零容忍）
%%%===================================================================

login_success_no_openid_leak_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {wechat_mini_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?UID} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_98001">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_98001">> end}
                ]}
            ]
        ],
        fun() ->
            {ok, Payload} = wechat_mini_auth_logic:wechat_mini_login(#{
                code => <<"good_code_123">>, device_id => <<"dev1">>
            }),
            ?assertEqual(<<"at_98001">>, maps:get(token, Payload)),
            ?assertEqual(<<"rt_98001">>, maps:get(refresh_token, Payload)),
            %% AUTH-01 核心断言：响应键集合绝不包含 openid/session_key/unionid
            Keys = maps:keys(Payload),
            ?assertEqual(false, lists:member(openid, Keys)),
            ?assertEqual(false, lists:member(session_key, Keys)),
            ?assertEqual(false, lists:member(unionid, Keys))
        end
    ).

%%%===================================================================
%%% 参数边界
%%%===================================================================

missing_code_test_() ->
    ?WITH_MECKS([base_mocks()], fun() ->
        ?assertEqual({error, missing_code}, wechat_mini_auth_logic:wechat_mini_login(#{}))
    end).

short_code_test_() ->
    ?WITH_MECKS([base_mocks()], fun() ->
        ?assertEqual(
            {error, invalid_code}, wechat_mini_auth_logic:wechat_mini_login(#{code => <<"ab">>})
        )
    end).

base_mocks() ->
    {config_ds, [
        {'env', 2, fun
            (wechat_mini_appid, _) -> <<"wx_test_appid">>;
            (wechat_mini_secret, _) -> <<"test_secret_placeholder">>;
            (_, Default) -> Default
        end}
    ]}.
