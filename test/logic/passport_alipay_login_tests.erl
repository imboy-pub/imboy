-module(passport_alipay_login_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("common.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc passport_logic:alipay_login/2 的 EUnit 测试（APP 支付宝登录）
%%%
%%% 覆盖：老用户按 (alipay, user_id) 映射直登 / 新用户自动建号（昵称头像
%%%       回填、性别 m/f→1/2 映射、source=alipay、sso_identity 绑定）/
%%%       昵称兜底 / 建号 23505 并发冲突回读 / License 配额 402 /
%%%       账号禁用拒绝 / 授权码无效 / userinfo 失败 / 未配置凭据。
%%% 说明：alipay_openapi / sso_identity_ds / user_ds / token_ds /
%%%       imboy_license 全部 meck（不触网不触库）。
%%%===================================================================

-define(ALIPAY_UID, <<"2088302622035892">>).

user_map(Uid, Status) ->
    #{
        <<"id">> => Uid,
        <<"account">> => <<"1000", (integer_to_binary(Uid))/binary>>,
        <<"mobile">> => <<>>,
        <<"password">> => <<>>,
        <<"email">> => <<>>,
        <<"nickname">> => <<"测试用户"/utf8>>,
        <<"avatar">> => <<"https://tfs.alipayobjects.com/avatar.jpg">>,
        <<"background">> => <<>>,
        <<"gender">> => 1,
        <<"region">> => <<>>,
        <<"sign">> => <<>>,
        <<"birthday">> => <<>>,
        <<"profession">> => <<>>,
        <<"school">> => <<>>,
        <<"interests">> => <<>>,
        <<"status">> => Status
    }.

oauth_ok_mock() ->
    {alipay_openapi, [
        {'oauth_token', 2, fun(_Cfg, <<"authcode123">>) ->
            {ok, #{
                access_token => <<"at-test-token">>,
                user_id => ?ALIPAY_UID,
                refresh_token => <<"rt-test-token">>,
                expires_in => 1296000
            }}
        end},
        {'user_info_share', 2, fun(_Cfg, <<"at-test-token">>) ->
            {ok, #{
                <<"user_id">> => ?ALIPAY_UID,
                <<"avatar">> => <<"https://tfs.alipayobjects.com/avatar.jpg">>,
                <<"nick_name">> => <<"测试用户"/utf8>>,
                <<"gender">> => <<"m">>,
                <<"province">> => <<"广东省"/utf8>>,
                <<"city">> => <<"深圳市"/utf8>>
            }}
        end}
    ]}.

token_mock() ->
    {token_ds, [
        {'encrypt_token', 2, fun(Uid, _Did) ->
            <<"tok_", (integer_to_binary(Uid))/binary>>
        end},
        {'encrypt_refreshtoken', 2, fun(Uid, _Did) ->
            <<"rtok_", (integer_to_binary(Uid))/binary>>
        end}
    ]}.

env_setup() ->
    application:set_env(imboy, alipay_app_id, <<"2021004142626807">>),
    application:set_env(imboy, alipay_private_key, <<"dummy-priv">>),
    ok.

env_cleanup(_) ->
    application:unset_env(imboy, alipay_app_id),
    application:unset_env(imboy, alipay_private_key),
    ok.

%%%===================================================================
%%% 老用户直登
%%%===================================================================

existing_user_login_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                oauth_ok_mock(),
                token_mock(),
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"alipay">>, ?ALIPAY_UID) -> {ok, 456} end},
                    {'bind', 4, fun(_, _, _, _) -> ok end}
                ]},
                {user_ds, [
                    {'find_by_id', 2, fun(456, _Cols) -> user_map(456, 1) end}
                ]}
            ],
            fun() ->
                {ok, Data} = passport_logic:alipay_login(<<"authcode123">>, #{<<"did">> => <<"d1">>}),
                ?assertEqual(456, maps:get(<<"uid">>, Data)),
                ?assertEqual(<<"tok_456">>, maps:get(<<"token">>, Data)),
                ?assertEqual(<<"rtok_456">>, maps:get(<<"refreshtoken">>, Data)),
                %% 老用户不触发建号绑定
                ?assertEqual(0, meck:num_calls(sso_identity_ds, bind, 4))
            end
        )
    end}.

%%%===================================================================
%%% 新用户自动建号
%%%===================================================================

new_user_provision_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                oauth_ok_mock(),
                token_mock(),
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"alipay">>, ?ALIPAY_UID) -> not_found end},
                    {'bind', 4, fun(<<"alipay">>, ?ALIPAY_UID, 789, _E) -> ok end}
                ]},
                {account_ds, [{'allocate', 0, fun() -> <<"1000789">> end}]},
                {user_ds, [
                    {'count', 0, fun() -> 100 end},
                    {'insert_and_get_id', 1, fun(Data) ->
                        erlang:put(alipay_tc_insert, Data),
                        {ok, 789}
                    end},
                    {'find_by_id', 2, fun(789, _Cols) -> user_map(789, 1) end}
                ]},
                {imboy_license, [{'check_user_quota', 1, fun(100) -> ok end}]}
            ],
            fun() ->
                {ok, Data} = passport_logic:alipay_login(<<"authcode123">>, #{<<"did">> => <<"d1">>}),
                ?assertEqual(789, maps:get(<<"uid">>, Data)),
                ?assertEqual(<<"tok_789">>, maps:get(<<"token">>, Data)),
                %% 建号数据：昵称/头像回填、性别 m→1、source=alipay
                Ins = erlang:get(alipay_tc_insert),
                ?assertEqual(<<"测试用户"/utf8>>, maps:get(<<"nickname">>, Ins)),
                ?assertEqual(
                    <<"https://tfs.alipayobjects.com/avatar.jpg">>, maps:get(<<"avatar">>, Ins)
                ),
                ?assertEqual(1, maps:get(<<"gender">>, Ins)),
                ?assertEqual(<<"alipay">>, maps:get(<<"source">>, Ins)),
                %% SSO 用户不走密码登录：必须有随机密码占位
                ?assertMatch(<<_/binary>>, maps:get(<<"password">>, Ins)),
                %% 身份映射已绑定
                ?assertEqual(1, meck:num_calls(sso_identity_ds, bind, 4))
            end
        )
    end}.

nickname_fallback_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                {alipay_openapi, [
                    {'oauth_token', 2, fun(_C, _Code) ->
                        {ok, #{access_token => <<"at">>, user_id => ?ALIPAY_UID}}
                    end},
                    {'user_info_share', 2, fun(_C, _At) ->
                        %% 用户未公开昵称/性别
                        {ok, #{<<"user_id">> => ?ALIPAY_UID}}
                    end}
                ]},
                token_mock(),
                {sso_identity_ds, [
                    {'find_uid', 2, fun(_, _) -> not_found end},
                    {'bind', 4, fun(_, _, _, _) -> ok end}
                ]},
                {account_ds, [{'allocate', 0, fun() -> <<"1000790">> end}]},
                {user_ds, [
                    {'count', 0, fun() -> 1 end},
                    {'insert_and_get_id', 1, fun(Data) ->
                        erlang:put(alipay_tc_insert, Data),
                        {ok, 790}
                    end},
                    {'find_by_id', 2, fun(790, _Cols) -> user_map(790, 1) end}
                ]},
                {imboy_license, [{'check_user_quota', 1, fun(_) -> ok end}]}
            ],
            fun() ->
                {ok, _} = passport_logic:alipay_login(<<"authcode123">>, #{}),
                Ins = erlang:get(alipay_tc_insert),
                %% 昵称兜底 alipay_ + user_id 尾 6 位
                ?assertEqual(<<"alipay_035892">>, maps:get(<<"nickname">>, Ins)),
                %% 性别未知 → 0
                ?assertEqual(0, maps:get(<<"gender">>, Ins))
            end
        )
    end}.

gender_map_female_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                {alipay_openapi, [
                    {'oauth_token', 2, fun(_C, _Code) ->
                        {ok, #{access_token => <<"at">>, user_id => ?ALIPAY_UID}}
                    end},
                    {'user_info_share', 2, fun(_C, _At) ->
                        {ok, #{<<"user_id">> => ?ALIPAY_UID, <<"gender">> => <<"f">>}}
                    end}
                ]},
                token_mock(),
                {sso_identity_ds, [
                    {'find_uid', 2, fun(_, _) -> not_found end},
                    {'bind', 4, fun(_, _, _, _) -> ok end}
                ]},
                {account_ds, [{'allocate', 0, fun() -> <<"1000790">> end}]},
                {user_ds, [
                    {'count', 0, fun() -> 1 end},
                    {'insert_and_get_id', 1, fun(Data) ->
                        erlang:put(alipay_tc_insert, Data),
                        {ok, 791}
                    end},
                    {'find_by_id', 2, fun(791, _Cols) -> user_map(791, 1) end}
                ]},
                {imboy_license, [{'check_user_quota', 1, fun(_) -> ok end}]}
            ],
            fun() ->
                {ok, _} = passport_logic:alipay_login(<<"authcode123">>, #{}),
                Ins = erlang:get(alipay_tc_insert),
                ?assertEqual(2, maps:get(<<"gender">>, Ins))
            end
        )
    end}.

%%%===================================================================
%%% 失败分支
%%%===================================================================

invalid_auth_code_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                {alipay_openapi, [
                    {'oauth_token', 2, fun(_C, <<"badcode">>) ->
                        {error, <<"授权码code无效"/utf8>>}
                    end}
                ]}
            ],
            fun() ->
                {error, Msg} = passport_logic:alipay_login(<<"badcode">>, #{}),
                ?assertEqual(<<"授权码code无效"/utf8>>, Msg)
            end
        )
    end}.

userinfo_failed_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                {alipay_openapi, [
                    {'oauth_token', 2, fun(_C, _Code) ->
                        {ok, #{access_token => <<"at">>, user_id => ?ALIPAY_UID}}
                    end},
                    {'user_info_share', 2, fun(_C, _At) ->
                        {error, <<"访问令牌已过期"/utf8>>}
                    end}
                ]}
            ],
            fun() ->
                {error, Msg} = passport_logic:alipay_login(<<"authcode123">>, #{}),
                ?assertEqual(<<"访问令牌已过期"/utf8>>, Msg)
            end
        )
    end}.

%% oauth_token 成功但网关未回 user_id（老版响应缺字段）：空 AlipayUid 不得
%% bind——(alipay, <<>>) 映射会让所有同类登录撞进同一账号（账号混淆）
empty_alipay_uid_never_binds_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                {alipay_openapi, [
                    {'oauth_token', 2, fun(_C, _Code) ->
                        {ok, #{
                            access_token => <<"at">>,
                            user_id => <<>>,
                            refresh_token => <<"rt">>,
                            expires_in => 1296000
                        }}
                    end},
                    {'user_info_share', 2, fun(_C, _At) -> {ok, #{}} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(_, _) -> not_found end},
                    {'bind', 4, fun(_, _, _, _) -> ok end}
                ]}
            ],
            fun() ->
                {error, Msg} = passport_logic:alipay_login(<<"authcode123">>, #{}),
                ?assertEqual(<<"支付宝授权失败：未获取到用户标识"/utf8>>, Msg),
                %% 空守卫在 userinfo 外呼与身份映射之前短路
                ?assertEqual(0, meck:num_calls(alipay_openapi, user_info_share, 2)),
                ?assertEqual(0, meck:num_calls(sso_identity_ds, bind, 4))
            end
        )
    end}.

disabled_user_rejected_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                oauth_ok_mock(),
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"alipay">>, ?ALIPAY_UID) -> {ok, 456} end}
                ]},
                {user_ds, [
                    {'find_by_id', 2, fun(456, _Cols) -> user_map(456, 0) end}
                ]}
            ],
            fun() ->
                {error, Msg} = passport_logic:alipay_login(<<"authcode123">>, #{}),
                ?assertEqual(<<"账号被禁用"/utf8>>, Msg)
            end
        )
    end}.

quota_exceeded_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                oauth_ok_mock(),
                {sso_identity_ds, [
                    {'find_uid', 2, fun(_, _) -> not_found end}
                ]},
                {user_ds, [{'count', 0, fun() -> 99999 end}]},
                {imboy_license, [
                    {'check_user_quota', 1, fun(99999) -> {error, quota_exceeded} end}
                ]}
            ],
            fun() ->
                {error, Msg, Code} = passport_logic:alipay_login(<<"authcode123">>, #{}),
                ?assertEqual(?ERR_PAYMENT_REQUIRED, Code),
                Prefix = <<"用户数已达授权上限"/utf8>>,
                ?assertEqual(Prefix, binary:part(Msg, 0, byte_size(Prefix)))
            end
        )
    end}.

%%%===================================================================
%%% 建号并发冲突（23505）回读映射
%%%===================================================================

provision_conflict_rebind_test_() ->
    {setup, fun env_setup/0, fun env_cleanup/1, fun(_) ->
        ?WITH_MECKS(
            [
                oauth_ok_mock(),
                token_mock(),
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"alipay">>, ?ALIPAY_UID) ->
                        %% 第 1 次查：未绑定；冲突后第 2 次查：竞争者已绑定
                        %% （计数器惰性初始化：?_test 在独立进程跑，外层 put 不可见）
                        N =
                            case erlang:get(alipay_tc_find_uid_n) of
                                undefined -> 0;
                                V -> V
                            end,
                        erlang:put(alipay_tc_find_uid_n, N + 1),
                        case N of
                            0 -> not_found;
                            _ -> {ok, 999}
                        end
                    end},
                    {'bind', 4, fun(_, _, _, _) -> ok end}
                ]},
                {account_ds, [{'allocate', 0, fun() -> <<"1000790">> end}]},
                {user_ds, [
                    {'count', 0, fun() -> 1 end},
                    {'insert_and_get_id', 1, fun(_Data) ->
                        {error, {error, error, <<"23505">>, unique_violation, <<"dup">>, []}}
                    end},
                    {'find_by_id', 2, fun(999, _Cols) -> user_map(999, 1) end}
                ]},
                {imboy_license, [{'check_user_quota', 1, fun(_) -> ok end}]}
            ],
            fun() ->
                {ok, Data} = passport_logic:alipay_login(<<"authcode123">>, #{}),
                ?assertEqual(999, maps:get(<<"uid">>, Data))
            end
        )
    end}.

%%%===================================================================
%%% 未配置凭据
%%%===================================================================

no_credential_test() ->
    application:unset_env(imboy, alipay_app_id),
    application:unset_env(imboy, alipay_private_key),
    {error, Msg} = passport_logic:alipay_login(<<"authcode123">>, #{}),
    ?assertEqual(<<"支付宝登录未配置凭据"/utf8>>, Msg).
