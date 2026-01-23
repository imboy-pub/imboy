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

%% 创建 Cowboy 2.x 模拟请求对象
%% Cowboy 2.x 使用 Map 作为请求对象，而不是 mock_request()
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
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/dashboard">> end},
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {elib_req, [
            {'cookie', 2, fun(_cookie_name, _Req) -> <<"encoded_uid_123">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_encoded) -> 100 end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}, has_sent_resp => false},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 100}}}, Result)
    end).

execute_with_valid_uid_cookie_post_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/api/action">> end},
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_req, [
            {'cookie', 2, fun(_cookie_name, _Req) -> <<"uid_456">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_encoded) -> 200 end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}, has_sent_resp => false},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 200}}}, Result)
    end).

execute_without_uid_cookie_get_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/protected">> end},
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'uri', 1, fun(_Req) -> <<"https://example.com/adm/protected">> end},
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
        {cowboy_req, [
            {'path', 1, fun(_Req) -> <<"/adm/api/data">> end},
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_req, [
            {'cookie', 2, fun(_cookie_name, _Req) -> undefined end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, _Code) ->
                #{response_status => 706}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:execute(Req, Env),
        ?assertMatch({stop, #{response_status := 706}}, Result)
    end).

%% ===================================================================
%% condition/4 测试
%% ===================================================================

condition_with_binary_uid_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(_uid) -> 100 end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{test_key => test_value}},
        Result = adm_auth_middleware:condition(<<"GET">>, <<"uid_123">>, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 100, test_key := test_value}}}, Result)
    end).

condition_without_has_sent_resp_in_env_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(_uid) -> 300 end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{existing => data}},
        Result = adm_auth_middleware:condition(<<"POST">>, <<"uid_abc">>, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 300, existing := data}}}, Result)
    end).

condition_get_without_uid_redirects_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'uri', 1, fun(_Req) -> <<"https://example.com/adm/page">> end},
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
    ?WITH_MECK(elib_response, [
        {'error', 3, fun(_Req, _Msg, _Code) ->
            #{response_status => 706}
        end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{}},
        Result = adm_auth_middleware:condition(<<"POST">>, undefined, Req, Env),
        ?assertMatch({stop, #{response_status := 706}}, Result)
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
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(_uid) -> 999 end}
    ], fun() ->
        Req = mock_request(),
        Env = #{handler_opts => #{key1 => val1, key2 => val2}},
        Result = adm_auth_middleware:condition(<<"GET">>, <<"uid_xyz">>, Req, Env),
        ?assertMatch({ok, _, #{handler_opts := #{adm_user_id := 999, key1 := val1, key2 := val2}}}, Result)
    end).
