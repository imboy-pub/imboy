-module(imboy_sms_jverification_tests).

-include_lib("eunit/include/eunit.hrl").

jverification_network_failure_test() ->
    meck:new(config_ds, [no_link]),
    meck:new(elib_req, [no_link]),
    meck:expect(config_ds, env, fun
        (jpush_app_key, _) -> <<"test_app_key">>;
        (jpush_master_secret, _) -> <<"test_master_secret">>
    end),
    meck:expect(elib_req, post, 3, {error, timeout}),
    try
        ?assertEqual(
            {error, <<"一键登录认证服务暂时不可用"/utf8>>},
            imboy_sms:jverification(<<"test_login_token">>)
        )
    after
        meck:unload(config_ds),
        meck:unload(elib_req)
    end.

jverification_provider_failure_includes_code_test() ->
    meck:new(config_ds, [no_link]),
    meck:new(elib_req, [no_link]),
    meck:expect(config_ds, env, fun
        (jpush_app_key, _) -> <<"test_app_key">>;
        (jpush_master_secret, _) -> <<"test_master_secret">>
    end),
    meck:expect(elib_req, post, 3, {ok, #{<<"code">> => 8001, <<"content">> => <<"错误"/utf8>>}}),
    try
        ?assertEqual(
            {error, <<"一键登录认证失败（服务端错误码 8001）"/utf8>>},
            imboy_sms:jverification(<<"test_login_token">>)
        )
    after
        meck:unload(config_ds),
        meck:unload(elib_req)
    end.
