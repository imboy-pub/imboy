%% teaching_auth_logic_tests
%% AUTH-01：微信小程序登录 — code 重放/无效、无效 provider（未配置）、
%% 未绑定用户、网络失败、成功签发（响应不含 openid/session_key）。
%% 全部外呼/配置/映射均 meck，无真实网络、无真实 AppSecret。

-module(teaching_auth_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 98001).
-define(OPENID, <<"oMOYA_test_openid_0001">>).

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
            {teaching_wechat_client, [
                {'jscode2session', 3, fun(_, _, _) -> {error, invalid_code} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_unconfigured},
                teaching_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
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
                {teaching_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {error, invalid_code} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, code_invalid},
                teaching_auth_logic:wechat_mini_login(#{code => <<"replayed_or_bad">>})
            )
        end
    ).

%% code 重放：同 code 第二次调用（微信侧已消费 → 40163）同样 code_invalid
code_replay_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {teaching_wechat_client, [
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
                {teaching_context_logic, [
                    {'contexts', 2, fun(_, organization) -> {ok, #{contexts => []}} end}
                ]}
            ]
        ],
        fun() ->
            R1 = teaching_auth_logic:wechat_mini_login(#{code => <<"first_time_ok">>}),
            ?assertMatch({ok, _}, R1),
            R2 = teaching_auth_logic:wechat_mini_login(#{code => <<"first_time_ok">>}),
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
                {teaching_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {error, network} end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, login_failed},
                teaching_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            )
        end
    ).

%%%===================================================================
%%% 未绑定用户（sso_identity 无映射）→ 5404 路径，不泄漏 openid 细节
%%%===================================================================

identity_none_test_() ->
    ?WITH_MECKS(
        [
            base_mocks()
            | [
                {teaching_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(_, _) -> not_found end}
                ]}
            ]
        ],
        fun() ->
            ?assertEqual(
                {error, identity_none},
                teaching_auth_logic:wechat_mini_login(#{code => <<"good_code_123">>})
            )
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
                {teaching_wechat_client, [
                    {'jscode2session', 3, fun(_, _, _) -> {ok, ?OPENID} end}
                ]},
                {sso_identity_ds, [
                    {'find_uid', 2, fun(<<"wechat_mini">>, ?OPENID) -> {ok, ?UID} end}
                ]},
                {token_ds, [
                    {'encrypt_token', 1, fun(?UID) -> <<"at_98001">> end},
                    {'encrypt_refreshtoken', 2, fun(?UID, <<>>) -> <<"rt_98001">> end}
                ]},
                {teaching_context_logic, [
                    {'contexts', 2, fun(?UID, organization) ->
                        {ok, #{contexts => [ctx_stub()]}}
                    end}
                ]}
            ]
        ],
        fun() ->
            {ok, Payload} = teaching_auth_logic:wechat_mini_login(#{
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
        ?assertEqual({error, missing_code}, teaching_auth_logic:wechat_mini_login(#{}))
    end).

short_code_test_() ->
    ?WITH_MECKS([base_mocks()], fun() ->
        ?assertEqual(
            {error, invalid_code}, teaching_auth_logic:wechat_mini_login(#{code => <<"ab">>})
        )
    end).

%%%===================================================================
%%% Helpers
%%%===================================================================

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
