-module(adm_auth_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_auth_middleware 模块的 EUnit 测试
%%%
%%% 目标：验证管理后台认证中间件功能
%%% 覆盖：路径检查、Cookie 认证、重定向处理
%%%===================================================================

%% config_ds:env/2 mock — signing_key/cookie_secret/start_mode all depend on it
config_ds_mock() ->
    {config_ds, [
        {'env', 2, fun
            (jwt_key, <<>>) -> <<"test-jwt-secret-key-32-bytes!">>;
            (start_mode, http) -> http;
            (adm_cookie_secret, <<"imboy-adm-cookie">>) -> <<"test-cookie-secret">>;
            (adm_auth_legacy_cookie_enabled, false) -> false;
            (_, _) -> undefined
        end}
    ]}.

%% 创建 Cowboy 2.x 模拟请求对象
mock_request() ->
    #{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"http">>,
        host => <<"localhost">>,
        port => 8080,
        path => <<"/adm/dashboard">>,
        qs => <<>>,
        headers => #{},
        peer => {{127,0,0,1}, 12345},
        body_length => 0
    }.

%% ===================================================================
%% execute/2 测试
%% ===================================================================

execute_with_static_path_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'path', 1, fun(_Req) -> <<"/static/css/style.css">> end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, _}, Result)
    end).

execute_with_passport_login_path_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'path', 1, fun(_Req) -> <<"/adm/passport/login">> end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, _}, Result)
    end).

execute_with_passport_captcha_path_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'path', 1, fun(_Req) -> <<"/adm/passport/captcha">> end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, _}, Result)
    end).

execute_with_valid_uid_cookie_get_test_() ->
    ?WITH_MECKS([
        config_ds_mock(),
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/dashboard">> end},
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {elib_req, [
            {'cookie', 2, fun
                (<<"adm_user_id">>, _Req) -> <<"100">>;
                (<<"adm_user_sig">>, _Req) ->
                    adm_auth_middleware:sign_admin_cookie(<<"100">>);
                (_, _) ->
                    false
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}, has_sent_resp => false},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 100}}}, Result)
    end).

execute_with_valid_uid_cookie_post_test_() ->
    ?WITH_MECKS([
        config_ds_mock(),
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/api/action">> end},
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_req, [
            {'cookie', 2, fun
                (<<"adm_user_id">>, _Req) -> <<"200">>;
                (<<"adm_user_sig">>, _Req) ->
                    adm_auth_middleware:sign_admin_cookie(<<"200">>);
                (_, _) ->
                    false
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}, has_sent_resp => false},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 200}}}, Result)
    end).

execute_without_uid_cookie_get_test_() ->
    ?WITH_MECKS([
        config_ds_mock(),
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/">> end},
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'uri', 1, fun(_Req) -> <<"https://example.com/adm/">> end},
            {'set_resp_cookie', 4, fun(_name, _value, _Req, _opts) ->
                #{cookie_set => true}
            end},
            {'reply', 3, fun(_Code, _Headers, _Req) ->
                #{response_status => 302}
            end}
        ]},
        {elib_req, [
            {'cookie', 2, fun(_cookie_name, _Req) -> undefined end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({stop, #{response_status := 302}}, Result)
    end).

execute_without_uid_cookie_post_test_() ->
    ?WITH_MECKS([
        config_ds_mock(),
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/api/data">> end},
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'set_resp_cookie', 4, fun(_Name, _Value, Req, _Opts) -> Req end},
            {'reply', 4, fun(Code, Headers, Body, Req) ->
                Req#{response_status => Code, response_headers => Headers, response_body => Body}
            end}
        ]},
        {elib_req, [
            {'cookie', 2, fun(_cookie_name, _Req) -> undefined end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({stop, #{response_status := 401}}, Result)
    end).

%% ===================================================================
%% condition/4 测试
%% ===================================================================

condition_with_binary_uid_test_() ->
    ?WITH_MECKS([
        config_ds_mock()
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{test_key => test_value}},
        Result = adm_auth_middleware:condition(<<"GET">>, <<"100">>, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 100, test_key := test_value}}}, Result)
    end).

condition_without_has_sent_resp_in_env_test_() ->
    ?WITH_MECKS([
        config_ds_mock()
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{existing => data}},
        Result = adm_auth_middleware:condition(<<"POST">>, <<"300">>, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 300, existing := data}}}, Result)
    end).

condition_get_without_uid_redirects_test_() ->
    ?WITH_MECKS([
        config_ds_mock(),
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/index">> end},
            {'uri', 1, fun(_Req) -> <<"https://example.com/adm/index">> end},
            {'set_resp_cookie', 4, fun(_name, _value, _Req, _opts) ->
                #{cookie_set => true}
            end},
            {'reply', 3, fun(_Code, _Headers, _Req) ->
                #{response_status => 302}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:condition(<<"GET">>, undefined, Req, Env),
        ?assertMatch({stop, #{response_status := 302}}, Result)
    end).

condition_post_without_uid_returns_error_test_() ->
    ?WITH_MECKS([
        config_ds_mock(),
        {cowboy_req, [
            {'set_resp_cookie', 4, fun(_Name, _Value, Req, _Opts) -> Req end},
            {'reply', 4, fun(Code, Headers, Body, Req) ->
                Req#{response_status => Code, response_headers => Headers, response_body => Body}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:condition(<<"POST">>, undefined, Req, Env),
        ?assertMatch({stop, #{response_status := 401}}, Result)
    end).

%% ===================================================================
%% remove_last_forward_slash/1 测试
%% ===================================================================

remove_last_forward_slash_with_empty_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_auth_middleware:remove_last_forward_slash(<<>>),
        ?assertEqual(<<"/">>, Result)
    end).

remove_last_forward_slash_with_single_slash_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_auth_middleware:remove_last_forward_slash(<<"/">>),
        ?assertEqual(<<"/">>, Result)
    end).

remove_last_forward_slash_with_trailing_slash_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_auth_middleware:remove_last_forward_slash(<<"/abc/">>),
        ?assertEqual(<<"/abc">>, Result)
    end).

remove_last_forward_slash_without_trailing_slash_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_auth_middleware:remove_last_forward_slash(<<"/abc">>),
        ?assertEqual(<<"/abc">>, Result)
    end).

remove_last_forward_slash_with_nested_path_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_auth_middleware:remove_last_forward_slash(<<"/adm/dashboard/">>),
        ?assertEqual(<<"/adm/dashboard">>, Result)
    end).

remove_last_forward_slash_with_multiple_slashes_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_auth_middleware:remove_last_forward_slash(<<"/path/to/resource/">>),
        ?assertEqual(<<"/path/to/resource">>, Result)
    end).

remove_last_forward_slash_with_complex_path_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = adm_auth_middleware:remove_last_forward_slash(<<"/api/v1/users/123/">>),
        ?assertEqual(<<"/api/v1/users/123">>, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

execute_with_deep_static_path_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'path', 1, fun(_Req) -> <<"/static/js/vendor/lib.js">> end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, _}, Result)
    end).

execute_with_static_root_path_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'path', 1, fun(_Req) -> <<"/static/">> end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, _}, Result)
    end).

execute_with_passport_do_login_path_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'path', 1, fun(_Req) -> <<"/adm/passport/do_login">> end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, _}, Result)
    end).

condition_preserves_existing_handler_opts_test_() ->
    ?WITH_MECKS([
        config_ds_mock()
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{key1 => val1, key2 => val2}},
        Result = adm_auth_middleware:condition(<<"GET">>, <<"999">>, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 999, key1 := val1, key2 := val2}}}, Result)
    end).
