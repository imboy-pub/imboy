%% moya_auth_logic_tests
%% AUTH-01：微信小程序登录 — code 重放/无效、无效 provider（未配置）、
%% 首登自动开户（方案 B）/幂等/失败折叠/额度上限、网络失败、
%% 成功签发（响应不含 openid/session_key）。
%% 全部外呼/配置/映射均 meck，无真实网络、无真实 AppSecret。

-module(moya_auth_logic_tests).

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
            {moya_wechat_client, [
                {'jscode2session', 3, fun(_, _, _) -> {error, invalid_code} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_unconfigured},
                moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
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
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {error, invalid_code} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, code_invalid},
                moya_auth_logic:wechat_mini_login(#{code => <<"replayed_or_bad">>})
            )
        end
    ).

%% code 重放：同 code 第二次调用（微信侧已消费 → 40163）同样 code_invalid
code_replay_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {moya_wechat_client, [
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
                ]},
                {moya_context_logic, [
                    {'contexts', 2, fun(_, organization) -> {ok, #{contexts => []}} end}
                ]}
            ]
        ],
        fun() ->
            R1 = moya_auth_logic:wechat_mini_login(#{code => <<"first_time_ok">>}),
            ?assertMatch({ok, _}, R1),
            R2 = moya_auth_logic:wechat_mini_login(#{code => <<"first_time_ok">>}),
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
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {error, network} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, login_failed},
                moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
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
%%% 机构侧拿不到它就无法预先写 sso_identity ⇒ 新家长永远进不了门。
%%%===================================================================

first_login_provisions_and_issues_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {moya_identity_ds, [
                    {'provision_and_bind', 3, fun(<<"wechat_mini">>, ?OPENID, _Opts) ->
                        {ok, ?UID}
                    end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_provisioned">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_provisioned">> end}
                ]},
                {moya_context_logic, [
                    {'contexts', 2, fun(?UID, organization) -> {ok, #{contexts => []}} end}
                ]}
            ]
        ],
        fun() ->
            {ok, Payload} = moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>}),
            ?assertEqual(<<"at_provisioned">>, maps:get(token, Payload)),
            %% 全新账号必然没有教学身份 → 客户端 routeByContexts 落到 no-identity 页
            %% 「等待老师开通」，再由老师用 learners/:id/bind 建立家长关系
            ?assertEqual(false, maps:get(has_teaching_identity, Payload)),
            %% 开户恰好发生一次
            ?assertEqual(1, meck:num_calls(moya_identity_ds, provision_and_bind, 3)),
            %% uid 随登录下发，且必须是**字符串**（闭环最后一段靠它：
            %% 家长把 uid 报给老师，老师据此调 learners/:id/bind）
            ?assertEqual(integer_to_binary(?UID), maps:get(uid, Payload)),
            %% 响应键集合仍不含 openid（身份映射层外泄=零容忍）
            ?assertEqual(false, lists:member(openid, maps:keys(Payload)))
        end
    ).

%% 真实量级 uid（19 位）必须以字符串下发。
%% 反例的形状：以 number 下发 ⇒ JS 的 JSON.parse 把它折成 ...000
%% （9223372036854775807 → 9223372036854776000）。它仍然「能解析、能显示、
%% 能提交」，只是家长报给老师的号不是老师要绑的号 —— 全链路无一门禁会报。
uid_is_string_for_big_id_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?BIG_UID} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?BIG_UID) -> <<"at_big">> end},
                    {'encrypt_refreshtoken', 2, fun(?BIG_UID, <<>>) -> <<"rt_big">> end}
                ]},
                {moya_context_logic, [
                    {'contexts', 2, fun(?BIG_UID, organization) -> {ok, #{contexts => []}} end}
                ]}
            ]
        ],
        fun() ->
            {ok, Payload} = moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>}),
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
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?UID} end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {moya_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {error, unreachable} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_known">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_known">> end}
                ]},
                {moya_context_logic, [
                    {'contexts', 2, fun(?UID, organization) -> {ok, #{contexts => []}} end}
                ]}
            ]
        ],
        fun() ->
            {ok, _} = moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>}),
            ?assertEqual(0, meck:num_calls(moya_identity_ds, provision_and_bind, 3)),
            %% 命中既有映射时连配额检查都不该走（不做建号动作）
            ?assertEqual(0, meck:num_calls(passport_logic, quota_guard, 0))
        end
    ).

%% 请求上下文（device_id / ip）必须透传到开户层 —— reg_ip 是 NOT NULL 列，
%% 漏传就会被 user 层兜成 127.0.0.1（运维看到全部家长来自本机）
provision_receives_request_context_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {moya_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, Opts) ->
                        put(provision_opts, Opts),
                        {ok, ?UID}
                    end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_ctx">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_ctx">> end}
                ]},
                {moya_context_logic, [
                    {'contexts', 2, fun(?UID, organization) -> {ok, #{contexts => []}} end}
                ]}
            ]
        ],
        fun() ->
            {ok, _} = moya_auth_logic:wechat_mini_login(#{
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
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [{'quota_guard', 0, fun() -> ok end}]},
                {moya_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {error, db_error} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, login_failed},
                moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            )
        end
    ).

%% License 用户数上限：**永久**条件，必须单独成码（402）而不是并进 5401 ——
%% 否则家长看到的是可重试的文案 + 一个永远不会成功的重试按钮（5404 同款坑）
provision_quota_exceeded_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> not_found end}
                ]},
                {passport_logic, [
                    {'quota_guard', 0, fun() -> {error, <<"license detail">>, 402} end}
                ]},
                {moya_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {ok, ?UID} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, account_quota_exceeded},
                moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            ),
            %% 配额耗尽时不得再尝试建号（免绕过 License gate）
            ?assertEqual(0, meck:num_calls(moya_identity_ds, provision_and_bind, 3))
        end
    ).

%% 查映射本身失败（DB 抖动）绝不退化为「当作新用户开户」——
%% 那会在 DB 抖动时批量造重复账号
lookup_error_does_not_provision_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {error, timeout} end}
                ]},
                {moya_identity_ds, [
                    {'provision_and_bind', 3, fun(_, _, _) -> {ok, ?UID} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, login_failed},
                moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            ),
            ?assertEqual(0, meck:num_calls(moya_identity_ds, provision_and_bind, 3))
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
                {moya_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?UID} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_98001">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_98001">> end}
                ]},
                {moya_context_logic, [
                    {'contexts', 2, fun(?UID, organization) ->
                        {ok, #{contexts => [ctx_stub()]}}
                    end}
                ]}
            ]
        ],
        fun() ->
            {ok, Payload} = moya_auth_logic:wechat_mini_login(#{
                code => <<"good_code_123">>, device_id => <<"dev1">>
            }),
            ?assertEqual(<<"at_98001">>, maps:get(token, Payload)),
            ?assertEqual(<<"rt_98001">>, maps:get(refresh_token, Payload)),
            ?assertEqual(true, maps:get(has_teaching_identity, Payload)),
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
        ?assertEqual({error, missing_code}, moya_auth_logic:wechat_mini_login(#{}))
    end).

short_code_test_() ->
    ?WITH_MECKS([base_mocks()], fun() ->
        ?assertEqual(
            {error, invalid_code}, moya_auth_logic:wechat_mini_login(#{code => <<"ab">>})
        )
    end).

%%%===================================================================
%%% MFS2-F06（A1-D05）五身份矩阵：has_teaching_identity = 是否存在任何教学身份
%%%
%%% 契约（STEP-04 openapi/moya-teaching.yaml LoginPayload）：
%%%   has_teaching_identity — "是否存在任何教学身份（引导小程序进入身份选择）"。
%%% contexts/2 是三路并集：guardian ∪ staff ∪ organization(owner/admin)——
%%% A1 审计 D-05 只看了 organization_contexts（owner/admin），误判"家长/老师
%%% 恒 false"。本矩阵在 repo 层 meck、走真实 moya_context_logic:contexts/2
%%% 并集路径逐身份证伪：五身份均应 true，无任何身份才 false。
%%% 前端消费核对（wt-a2-moya）：该字段零消费（仅 types.ts:41 类型声明），
%%% 实际路由门是 /moya/contexts 的 contexts.length === 0 → no-identity 页。
%%%===================================================================

teaching_identity_matrix_test_() ->
    Scenarios = [
        {owner_only, #{org_rows => [org_row(<<"owner">>)], staff_rows => [], guardian_rows => []},
            true},
        {admin_only, #{org_rows => [org_row(<<"admin">>)], staff_rows => [], guardian_rows => []},
            true},
        {teacher_only,
            #{org_rows => [], staff_rows => [staff_row(<<"teacher">>)], guardian_rows => []}, true},
        {assistant_only,
            #{org_rows => [], staff_rows => [staff_row(<<"assistant">>)], guardian_rows => []},
            true},
        {pure_guardian, #{org_rows => [], staff_rows => [], guardian_rows => [guardian_row()]},
            true},
        {no_identity, #{org_rows => [], staff_rows => [], guardian_rows => []}, false}
    ],
    [
        {
            lists:flatten(io_lib:format("has_teaching_identity_~p", [Name])),
            ?WITH_MECKS(login_mocks(Rows), fun() ->
                {ok, Payload} = moya_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>}),
                ?assertEqual(Expected, maps:get(has_teaching_identity, Payload))
            end)
        }
     || {Name, Rows, Expected} <- Scenarios
    ].

%%%===================================================================
%%% Helpers
%%%===================================================================

%% 完整登录链 mock + 按场景注入 moya_context_repo 三路上下文行。
%% has_teaching_identity → contexts(Uid, organization)：
%% guardian_contexts + staff_contexts + organization_contexts 三路并集。
login_mocks(#{org_rows := OrgRows, staff_rows := StaffRows, guardian_rows := GuardianRows}) ->
    [
        base_mocks(),
        {moya_wechat_client, [
            {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
        ]},
        {sso_identity_ds, [
            {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?UID} end}
        ]},
        {token_ds, [
            {'encrypt_token', 1, fun(?UID) -> <<"at_98001">> end},
            {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_98001">> end}
        ]},
        {moya_context_repo, [
            {'guardian_contexts', 1, fun(?UID) -> {ok, GuardianRows} end},
            {'staff_contexts', 1, fun(?UID) -> {ok, StaffRows} end},
            {'organization_contexts', 1, fun(?UID) -> {ok, OrgRows} end},
            {'owner_contexts', 1, fun(?UID) -> {ok, []} end}
        ]}
    ].

%% organization_context/1 必填键：org_id（org_name/role 缺省 <<>>）
org_row(Role) ->
    #{<<"org_id">> => 982001, <<"org_name">> => <<"测试机构"/utf8>>, <<"role">> => Role}.

%% staff_context/1 必填键：org_id / workspace_id / group_id
staff_row(Role) ->
    #{
        <<"org_id">> => 982001,
        <<"workspace_id">> => 981001,
        <<"group_id">> => 983001,
        <<"group_title">> => <<"书法一班"/utf8>>,
        <<"role">> => Role
    }.

%% guardian_context/1 必填键：learner_id
guardian_row() ->
    #{<<"learner_id">> => 984001, <<"display_name">> => <<"小墨"/utf8>>}.

%% 基础 mock：provider 已配置（appid/secret 占位值），微信端点/映射默认失败，
%% 各用例按需覆盖。
base_mocks() ->
    {config_ds, [
        {'env', 2, fun
            (wechat_mini_appid, _) -> <<"wx_test_appid">>;
            (wechat_mini_secret, _) -> <<"test_secret_placeholder">>;
            (_, Default) -> Default
        end}
    ]}.

ctx_stub() ->
    #{
        <<"context_type">> => <<"organization">>,
        <<"organization_id">> => <<"3001">>,
        <<"role">> => <<"admin">>
    }.
