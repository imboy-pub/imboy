-module(passport_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% passport_handler 模块的 EUnit 测试
%%%
%%% 目标：验证通行证处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试用户注册功能
signup_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"email">>},
                    {<<"account">>, <<"test@example.com">>},
                    {<<"pwd">>, <<"password123">>},
                    {<<"rsa_encrypt">>, <<"0">>}
                ]
            end}
        ]},
        {passport_logic, [
            {'signup', 3, fun(_Type, _Account, _Password) ->
                {ok, #{<<"uid">> => 12345, <<"account">> => <<"test@example.com">>}}
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => signup}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试用户注册功能 - 账号已存在
signup_account_exists_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"email">>},
                    {<<"account">>, <<"existing@example.com">>},
                    {<<"pwd">>, <<"password123">>},
                    {<<"rsa_encrypt">>, <<"0">>}
                ]
            end}
        ]},
        {passport_logic, [
            {'signup', 3, fun(_Type, _Account, _Password) ->
                {error, <<"账号已存在">>}
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 400,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（账号已存在）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => signup}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试找回密码功能
find_password_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"email">>},
                    {<<"account">>, <<"test@example.com">>}
                ]
            end}
        ]},
        {passport_logic, [
            {'find_password', 2, fun(_Type, _Account) ->
                {ok, #{<<"message">> => <<"重置密码邮件已发送">>}}
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => find_password}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试找回密码功能 - 账号不存在
find_password_account_not_found_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"email">>},
                    {<<"account">>, <<"nonexistent@example.com">>}
                ]
            end}
        ]},
        {passport_logic, [
            {'find_password', 2, fun(_Type, _Account) ->
                {error, <<"账号不存在">>}
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 404,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（账号不存在）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => find_password}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(404, StatusCode)
    end).

%% @doc 测试登录功能
login_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"email">>},
                    {<<"account">>, <<"test@example.com">>},
                    {<<"pwd">>, <<"password123">>},
                    {<<"rsa_encrypt">>, <<"0">>}
                ]
            end}
        ]},
        {passport_logic, [
            {'do_login', 3, fun(_Type, _Account, _Password) ->
                {ok, #{<<"uid">> => 12345, <<"account">> => <<"test@example.com">>}}
            end}
        ]},
        {user_setting_ds, [
            {'find_by_uid', 1, fun(_Uid) ->
                #{<<"theme">> => <<"light">>, <<"language">> => <<"zh-CN">>}
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => login}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试登录功能 - 密码错误
login_password_error_test_() ->
    ?WITH_MOCKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"email">>},
                    {<<"account">>, <<"test@example.com">>},
                    {<<"pwd">>, <<"wrongpassword">>},
                    {<<"rsa_encrypt">>, <<"0">>}
                ]
            end}
        ]},
        {passport_logic, [
            {'do_login', 3, fun(_Type, _Account, _Password) ->
                {error, <<"密码错误">>}
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 401,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（密码错误）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => login}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(401, StatusCode)
    end).

%% @doc 测试刷新令牌功能
refresh_token_test_() ->
    ?WITH_MOCKS([
        {throttle, [
            {'check', 2, fun(_Type, _Token) ->
                ok  % 未超过限制
            end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(_RefreshToken) ->
                {ok, 12345, <<"2025-12-31">>, <<"rtk">>}
            end},
            {'encrypt_token', 1, fun(_UserId) ->
                <<"new_encrypted_token">>
            end}
        ]},
        {imboy_pg, [
            {'pluck', 4, fun(_Table, _Column, _Conditions, _Options) ->
                {ok, 1}  % 用户状态正常
            end}
        ]},
        {imboy_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>,
            headers => #{<<"imboy-refreshtoken">> => <<"valid_refresh_token">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => refreshtoken}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试刷新令牌功能 - 令牌无效
refresh_token_invalid_test_() ->
    ?WITH_MOCKS([
        {throttle, [
            {'check', 2, fun(_Type, _Token) ->
                ok  % 未超过限制
            end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(_RefreshToken) ->
                {error, invalid_token}
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 401,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（无效令牌）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>,
            headers => #{<<"imboy-refreshtoken">> => <<"invalid_token">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => refreshtoken}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(401, StatusCode)
    end).
