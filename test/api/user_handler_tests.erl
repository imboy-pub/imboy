-module(user_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_handler 模块的 EUnit 测试
%%%
%%% 目标：验证用户处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试用户搜索功能 - 通过邮箱搜索
search_by_email_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}  % Page=1, Size=20
            end}
        ]},
        {imboy_func, [
            {'is_email', 1, fun(Email) ->
                Email == <<"test@example.com">>
            end},
            {'is_mobile', 1, fun(_Mobile) ->
                false
            end}
        ]},
        {user_repo, [
            {'find_by_email', 2, fun(_Email, _Columns) ->
                #{
                    <<"id">> => 12345,
                    <<"nickname">> => <<"Test User">>,
                    <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                    <<"gender">> => 1,
                    <<"sign">> => <<"Hello World">>,
                    <<"region">> => <<"Beijing">>
                }
            end}
        ]},
        {fts_user_repo, [
            {'allow_search', 1, fun(_UserId) ->
                true
            end}
        ]},
        {friend_ds, [
            {'is_friend', 3, fun(_CurrentUid, _TargetUid, _Fields) ->
                {false, <<>>}
            end}
        ]},
        {elib_hashids, [
            {'replace_id', 1, fun(User) ->
                _UserId = maps:get(<<"id">>, User),
                User#{<<"id">> => <<"encoded_12345">>}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Payload
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"keyword=test@example.com">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => search,
            current_uid => 67890
        }),
        
        % 验证响应数据结构
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode),
        ?assertMatch(#{total := 1, page := 1, size := 20, list := [_]}, Body),
        
        % 验证用户数据
        List = maps:get(list, Body),
        ?assertEqual(1, length(List)),
        
        User = lists:nth(1, List),
        ?assertEqual(<<"encoded_12345">>, maps:get(<<"id">>, User)),
        ?assertEqual(<<"Test User">>, maps:get(<<"nickname">>, User)),
        ?assertEqual(false, maps:get(<<"is_friend">>, User))
    end).

%% @doc 测试用户搜索功能 - 用户不存在
search_user_not_found_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {imboy_func, [
            {'is_email', 1, fun(_Email) ->
                true
            end},
            {'is_mobile', 1, fun(_Mobile) ->
                false
            end}
        ]},
        {user_repo, [
            {'find_by_email', 2, fun(_Email, _Columns) ->
                #{}  % 空结果表示用户不存在
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Payload
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求（不存在的用户）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"keyword=nonexistent@example.com">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => search,
            current_uid => 67890
        }),
        
        % 验证响应数据结构
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode),
        ?assertMatch(#{total := 0, page := 1, size := 20, list := []}, Body)
    end).

%% @doc 测试修改密码功能
change_password_test_() ->
    ?WITH_MOCKS([
        {user_logic, [
            {'change_password', 2, fun(_UserId, _Req) ->
                {ok, #{<<"message">> => <<"密码修改成功">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Data) ->
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
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => change_password,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试修改密码功能 - 旧密码错误
change_password_wrong_old_password_test_() ->
    ?WITH_MOCKS([
        {user_logic, [
            {'change_password', 2, fun(_UserId, _Req) ->
                {error, <<"旧密码错误">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 400,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（旧密码错误）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => change_password,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试获取用户凭证功能
credential_test_() ->
    ?WITH_MOCKS([
        {user_ds, [
            {'webrtc_credential', 1, fun(_UserId) ->
                #{
                    <<"username">> => <<"user123">>,
                    <<"credential">> => <<"base64_credential">>
                }
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Payload
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => credential,
            current_uid => 12345
        }),
        
        % 验证响应状态和数据
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode),
        ?assertMatch(#{<<"username">> := _, <<"credential">> := _}, Body)
    end).

%% @doc 测试二维码扫描功能 - 用户存在且正常
qrcode_user_exists_test_() ->
    ?WITH_MOCKS([
        {elib_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                12345
            end}
        ]},
        {user_logic, [
            {'find_by_id', 2, fun(_UserId, _Columns) ->
                #{
                    <<"id">> => 12345,
                    <<"nickname">> => <<"Test User">>,
                    <<"gender">> => 1,
                    <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                    <<"sign">> => <<"Hello World">>,
                    <<"region">> => <<"Beijing">>,
                    <<"status">> => 1
                }
            end}
        ]},
        {friend_ds, [
            {'is_friend', 3, fun(_CurrentUid, _TargetUid, _Fields) ->
                {true, <<"My Friend">>}
            end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(_UserId) ->
                <<"encoded_12345">>
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Payload
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"id=encoded_12345">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => qrcode,
            current_uid => 67890
        }),
        
        % 验证响应数据
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode),
        ?assertMatch(#{<<"type">> := <<"user">>, <<"id">> := <<"encoded_12345">>}, Body),
        ?assertEqual(true, maps:get(<<"isfriend">>, Body)),
        ?assertEqual(<<"My Friend">>, maps:get(<<"remark">>, Body))
    end).

%% @doc 测试二维码扫描功能 - 用户不存在
qrcode_user_not_exists_test_() ->
    ?WITH_MOCKS([
        {elib_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                99999
            end}
        ]},
        {user_logic, [
            {'find_by_id', 2, fun(_UserId, _Columns) ->
                #{}  % 空结果表示用户不存在
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Payload) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => Payload
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求（不存在的用户）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"id=nonexistent_user">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => qrcode,
            current_uid => 67890
        }),
        
        % 验证响应数据
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode),
        ?assertMatch(#{<<"result">> := <<"user_not_exist">>}, Body)
    end).

%% @doc 测试切换在线状态功能
change_state_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"state">>, <<"online">>}
                ]
            end}
        ]},
        {user_setting_ds, [
            {'save', 3, fun(_UserId, _Key, _Value) ->
                ok
            end}
        ]},
        {user_server, [
            {'cast_notice_friend', 2, fun(_UserId, _State) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 4, fun(_Req, _Data, _Message, _Headers) ->
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
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => change_state,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试用户设置功能
setting_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"theme">>, <<"dark">>},
                    {<<"language">>, <<"zh-CN">>},
                    {<<"notification">>, <<"enabled">>}
                ]
            end}
        ]},
        {user_setting_ds, [
            {'save', 3, fun(_UserId, _Key, _Value) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Data) ->
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
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => setting,
            current_uid => 12345
        }),

        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试更新用户信息功能
update_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"nickname">>, <<"Updated Nickname">>},
                    {<<"avatar">>, <<"https://example.com/new_avatar.jpg">>},
                    {<<"gender">>, 1},
                    {<<"sign">>, <<"Updated signature">>},
                    {<<"region">>, <<"Beijing">>}
                ]
            end}
        ]},
        {user_logic, [
            {'update', 2, fun(_UserId, _Data) ->
                {ok, #{<<"id">> => 12345, <<"nickname">> => <<"Updated Nickname">>}}
            end}
        ]},
        {elib_response, [
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
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => update,
            current_uid => 12345
        }),

        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试获取用户信息功能
show_test_() ->
    ?WITH_MOCKS([
        {user_logic, [
            {'info', 2, fun(_UserId, _Fields) ->
                {ok, #{
                    <<"id">> => 12345,
                    <<"nickname">> => <<"Test User">>,
                    <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                    <<"gender">> => 1,
                    <<"sign">> => <<"Hello World">>,
                    <<"region">> => <<"Beijing">>,
                    <<"mobile">> => <<"+8613800138000">>,
                    <<"email">> => <<"test@example.com">>
                }}
            end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(_UserId) ->
                <<"encoded_12345">>
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 GET 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => show,
            current_uid => 12345
        }),

        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试设置密码功能
set_password_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"password">>, <<"new_password123">>},
                    {<<"rsa_encrypt">>, <<"0">>}
                ]
            end}
        ]},
        {user_logic, [
            {'set_password', 2, fun(_UserId, _Password) ->
                {ok, #{<<"message">> => <<"密码设置成功">>}}
            end}
        ]},
        {elib_response, [
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
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => set_password,
            current_uid => 12345
        }),

        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试申请登出功能
apply_logout_test_() ->
    ?WITH_MOCKS([
        {user_device_ds, [
            {'get_device', 1, fun(_UserId) ->
                {ok, [
                    #{device_id => <<"device_1">>, is_online => true},
                    #{device_id => <<"device_2">>, is_online => true}
                ]}
            end}
        ]},
        {user_logic, [
            {'apply_logout', 2, fun(_UserId, _DeviceList) ->
                {ok, #{<<"message">> => <<"登出申请已发送">>}}
            end}
        ]},
        {elib_response, [
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
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => apply_logout,
            current_uid => 12345
        }),

        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试取消登出功能
cancel_logout_test_() ->
    ?WITH_MOCKS([
        {user_logic, [
            {'cancel_logout', 1, fun(_UserId) ->
                {ok, #{<<"message">> => <<"已取消登出">>}}
            end}
        ]},
        {elib_response, [
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
        {ok, Req, _State} = user_handler:init(MockReq, #{
            action => cancel_logout,
            current_uid => 12345
        }),

        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).
