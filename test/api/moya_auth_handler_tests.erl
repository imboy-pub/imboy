%% moya_auth_handler_tests
%% 微信小程序登录 handler 契约测试（AUTH-01 / 试点方案 B）。
%%
%% 直接模块调用（不起 cowboy，模式照 moya_learner_bind_handler_tests）：
%%   - 参数解析：body 缺 code/device_id → 注入空串；ip 取 elib_req:get_client_ip/1
%%   - 成功：logic {ok, Payload} → elib_response:success_rfc3339/3（payload 原样透传）
%%   - 失败：每个 logic reason → envelope code（5401/5402/5403/5404/402/422）
%%   - 未知 reason → 兜底 5401（不得把内部细节透成新码）
%%
%% 为什么必须有这一层：logic 层测试覆盖不到「reason → 错误码」的映射表，
%% 而**客户端只认 envelope 里的 code**（HTTP 恒 200，见 elib_response:error/3）。
%% 映射表写漏一条，客户端就会掉进兜底文案 —— 2026-09-20 的 5404 死循环正是
%% 「500 段业务码全折叠成同一句兜底」造成的，故此处逐码锁死。
%%
%% 全部 meck，零真实网络、零真实库。

-module(moya_auth_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(CLIENT_IP, <<"203.0.113.77">>).

%%%===================================================================
%%% 基建
%%%===================================================================

%% Body：elib_param:post/1 的返回值；LogicReturn：moya_auth_logic 的返回值
handler_mocks(Body, LogicReturn) ->
    [
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end}
        ]},
        {elib_req, [
            {'get_client_ip', 1, fun(_Req) -> ?CLIENT_IP end}
        ]},
        {elib_response, [
            {'success_rfc3339', 3, fun(_Req, Payload, _Msg) ->
                #{resp => success, payload => Payload}
            end},
            {'error', 3, fun(_Req, _Msg, Code) ->
                #{resp => error, code => Code}
            end}
        ]},
        {moya_auth_logic, [
            {'wechat_mini_login', 1, fun(Params) ->
                self() ! {logic_params, Params},
                LogicReturn
            end}
        ]}
    ].

token_payload() ->
    #{
        token => <<"tok_abc">>,
        expires_in => 3600,
        refresh_token => <<"rt_abc">>,
        has_teaching_identity => false
    }.

call(Req) ->
    moya_auth_handler:handle_action(wechat_mini_login, Req, #{}).

%%%===================================================================
%%% 参数解析
%%%===================================================================

%% body 完整：code / device_id 原样传入；ip 来自 elib_req（不是占位值）
params_parsed_test_() ->
    ?WITH_MECKS(
        handler_mocks(
            #{<<"code">> => <<"good_code_123">>, <<"device_id">> => <<"dev_1">>},
            {ok, token_payload()}
        ),
        fun() ->
            Resp = call(#{req => ok}),
            ?assertMatch(#{resp := success}, Resp),
            ?assertEqual(
                #{code => <<"good_code_123">>, device_id => <<"dev_1">>, ip => ?CLIENT_IP},
                receive
                    {logic_params, P} -> P
                after 0 -> timeout
                end
            )
        end
    ).

%% body 缺字段：注入空串（而不是 crash / 传 undefined 进去）
params_defaults_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{}, {error, missing_code}),
        fun() ->
            _ = call(#{req => ok}),
            ?assertEqual(
                #{code => <<>>, device_id => <<>>, ip => ?CLIENT_IP},
                receive
                    {logic_params, P} -> P
                after 0 -> timeout
                end
            )
        end
    ).

%% 首登自动开户会写 user.reg_ip：ip 必须是真实客户端 IP。
%% 这条是**防回归**——若有人把 ip 换成 <<"127.0.0.1">> 常量，此处必红。
ip_is_real_not_placeholder_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"code">> => <<"good_code_123">>}, {ok, token_payload()}),
        fun() ->
            _ = call(#{req => ok}),
            P =
                receive
                    {logic_params, X} -> X
                after 0 -> timeout
                end,
            ?assertNotEqual(<<"127.0.0.1">>, maps:get(ip, P)),
            ?assertNotEqual(<<>>, maps:get(ip, P))
        end
    ).

%%%===================================================================
%%% 成功路径
%%%===================================================================

success_passes_payload_verbatim_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"code">> => <<"good_code_123">>}, {ok, token_payload()}),
        fun() ->
            ?assertEqual(
                #{resp => success, payload => token_payload()},
                call(#{req => ok})
            )
        end
    ).

%%%===================================================================
%%% 错误码映射表（逐码锁死）
%%%===================================================================

error_mapping_test_() ->
    Cases = [
        {missing_code, ?ERR_MISSING_PARAM},
        {invalid_code, ?ERR_PARAM_INVALID},
        {code_invalid, ?ERR_WECHAT_CODE_INVALID},
        {provider_unconfigured, ?ERR_TEACHING_PROVIDER_UNCONFIGURED},
        {login_failed, ?ERR_WECHAT_LOGIN_FAILED},
        {identity_none, ?ERR_TEACHING_IDENTITY_NONE},
        {account_quota_exceeded, ?ERR_PAYMENT_REQUIRED}
    ],
    [
        {
            atom_to_list(Reason),
            ?WITH_MECKS(
                handler_mocks(#{<<"code">> => <<"good_code_123">>}, {error, Reason}),
                fun() ->
                    ?assertEqual(
                        #{resp => error, code => Expected},
                        call(#{req => ok})
                    )
                end
            )
        }
     || {Reason, Expected} <- Cases
    ].

%% 未知 reason：兜底 5401，不得新造码 / 不得把内部细节透给客户端
unknown_reason_falls_back_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"code">> => <<"good_code_123">>}, {error, some_brand_new_atom}),
        fun() ->
            ?assertEqual(
                #{resp => error, code => ?ERR_WECHAT_LOGIN_FAILED},
                call(#{req => ok})
            )
        end
    ).

%% 授权上限是**永久**条件：必须走 402（全局授权码），客户端据此关掉重试按钮。
%% 若被折叠成 5401，家长会看到可重试文案并无限重试 —— 与 5404 同款坑。
quota_is_not_login_failed_test_() ->
    ?WITH_MECKS(
        handler_mocks(#{<<"code">> => <<"good_code_123">>}, {error, account_quota_exceeded}),
        fun() ->
            Code = maps:get(code, call(#{req => ok})),
            ?assertEqual(?ERR_PAYMENT_REQUIRED, Code),
            ?assertNotEqual(?ERR_WECHAT_LOGIN_FAILED, Code)
        end
    ).
