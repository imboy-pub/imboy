-module(passport_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% passport_handler 当前接口回归测试
%%%
%%% 这些用例贴合当前 handler/logic 签名，验证：
%%% - signup / find_password 通过 handle_logic_result 收口
%%% - login 走 do_login/5 与 setting 注入
%%% - refreshtoken 走当前 decrypt_token / pluck_value 分支
%%%===================================================================

signup_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"type">> => <<"email">>,
                    <<"account">> => <<"test@example.com">>,
                    <<"pwd">> => <<"password123">>,
                    <<"code">> => <<"123456">>,
                    <<"rsa_encrypt">> => <<"0">>
                }
            end}
        ]},
        {cowboy_req, [
            {'header', 3, fun(_Name, _Req, Default) -> Default end}
        ]},
        {elib_cipher, [
            {'safe_rsa_decrypt', 2, fun(Pwd, _RsaEncrypt) -> Pwd end}
        ]},
        {passport_logic, [
            {'do_signup', 5, fun(<<"email">>, <<"test@example.com">>, <<"password123">>, <<"123456">>, PostVals) ->
                ?assertEqual(<<"{}">>, maps:get(<<"ip">>, PostVals)),
                {ok, #{<<"uid">> => <<"encoded_12345">>}}
            end}
        ]},
        {elib_response, [
            {'handle_logic_result', 2, fun(_Req, {ok, Data}) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Data
                })
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => signup}),
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode),
        ?assertEqual(<<"encoded_12345">>, maps:get(<<"uid">>, Body))
    end).

find_password_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"type">> => <<"email">>,
                    <<"account">> => <<"test@example.com">>,
                    <<"pwd">> => <<"new-password">>,
                    <<"code">> => <<"654321">>,
                    <<"rsa_encrypt">> => <<"0">>
                }
            end}
        ]},
        {cowboy_req, [
            {'header', 3, fun(_Name, _Req, Default) -> Default end}
        ]},
        {elib_cipher, [
            {'safe_rsa_decrypt', 2, fun(Pwd, _RsaEncrypt) -> Pwd end}
        ]},
        {passport_logic, [
            {'find_password', 5, fun(<<"email">>, <<"test@example.com">>, <<"new-password">>, <<"654321">>, PostVals) ->
                ?assertEqual(<<"{}">>, maps:get(<<"ip">>, PostVals)),
                {ok, #{}}
            end}
        ]},
        {elib_response, [
            {'handle_logic_result', 2, fun(_Req, {ok, Data}) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Data
                })
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => find_password}),
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

login_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"type">> => <<"email">>,
                    <<"account">> => <<"test@example.com">>,
                    <<"pwd">> => <<"password123">>,
                    <<"rsa_encrypt">> => <<"0">>
                }
            end}
        ]},
        {cowboy_req, [
            {'header', 3, fun
                (<<"x-forwarded-for">>, _Req, _Default) -> <<"127.0.0.1">>;
                (<<"cos">>, _Req, _Default) -> <<"ios">>;
                (<<"did">>, _Req, _Default) -> <<"device-1">>;
                (<<"dname">>, _Req, _Default) -> <<"iPhone">>;
                (_Name, _Req, Default) -> Default
            end}
        ]},
        {elib_cipher, [
            {'safe_rsa_decrypt', 2, fun(Pwd, _RsaEncrypt) -> Pwd end}
        ]},
        {passport_logic, [
            {'do_login', 5, fun(<<"email">>, <<"test@example.com">>, <<"password123">>, <<"ios">>, <<"device-1">>) ->
                {ok, #{
                    <<"uid">> => <<"encoded_12345">>,
                    <<"account">> => <<"test@example.com">>
                }}
            end}
        ]},
        {user_setting_ds, [
            {'find_by_uid', 1, fun(<<"encoded_12345">>) ->
                #{<<"theme">> => <<"light">>, <<"language">> => <<"zh-CN">>}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Data
                })
            end}
        ]}
    ], fun() ->
        with_user_server(fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = passport_handler:init(MockReq, #{action => login}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertMatch(#{<<"setting">> := _}, Body)
        end)
    end).

login_password_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"type">> => <<"email">>,
                    <<"account">> => <<"test@example.com">>,
                    <<"pwd">> => <<"wrongpassword">>,
                    <<"rsa_encrypt">> => <<"0">>
                }
            end}
        ]},
        {cowboy_req, [
            {'header', 3, fun
                (<<"x-forwarded-for">>, _Req, _Default) -> <<"127.0.0.1">>;
                (<<"cos">>, _Req, _Default) -> <<"ios">>;
                (<<"did">>, _Req, _Default) -> <<"device-1">>;
                (<<"dname">>, _Req, _Default) -> <<"iPhone">>;
                (_Name, _Req, Default) -> Default
            end}
        ]},
        {elib_cipher, [
            {'safe_rsa_decrypt', 2, fun(Pwd, _RsaEncrypt) -> Pwd end}
        ]},
        {passport_logic, [
            {'do_login', 5, fun(<<"email">>, <<"test@example.com">>, <<"wrongpassword">>, <<"ios">>, <<"device-1">>) ->
                {error, <<"密码错误">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 401,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => login}),
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(401, StatusCode)
    end).

refresh_token_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 2, fun(<<"imboy-refreshtoken">>, _Req) ->
                <<"valid_refresh_token">>
            end}
        ]},
        {throttle, [
            {'check', 2, fun(_Type, _Token) -> ok end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(<<"valid_refresh_token">>) ->
                {ok, 12345, <<"2025-12-31">>, <<"rtk">>}
            end},
            {'encrypt_token', 1, fun(12345) ->
                <<"new_encrypted_token">>
            end}
        ]},
        {elib_pg, [
            {'pluck_value', 5, fun(_Table, _Column, _Conditions, _Options, _Default) ->
                1
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Data
                })
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => refreshtoken}),
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode),
        ?assertEqual(<<"new_encrypted_token">>, maps:get(<<"token">>, Body))
    end).

refresh_token_invalid_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 2, fun(<<"imboy-refreshtoken">>, _Req) ->
                <<"invalid_token">>
            end}
        ]},
        {throttle, [
            {'check', 2, fun(_Type, _Token) -> ok end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(<<"invalid_token">>) ->
                {error, 401, <<"invalid token">>, #{}}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Message, Code) ->
                cowboy_req_h:new(#{
                    response_status => Code,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
        {ok, Req, _State} = passport_handler:init(MockReq, #{action => refreshtoken}),
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(401, StatusCode)
    end).

with_user_server(TestFun) ->
    case whereis(user_server) of
        undefined ->
            Pid = spawn(fun user_server_loop/0),
            true = register(user_server, Pid),
            try
                TestFun()
            after
                unregister(user_server),
                exit(Pid, kill)
            end;
        _Pid ->
            TestFun()
    end.

user_server_loop() ->
    receive
        _Msg -> user_server_loop()
    end.
