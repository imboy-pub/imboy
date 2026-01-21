-module(user_denylist_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_denylist_handler 模块的 EUnit 测试
%%%
%%% 目标：验证黑名单处理器功能
%%% 覆盖：添加黑名单、删除黑名单、分页列表
%%%===================================================================

%% ===================================================================
%% 添加到黑名单测试
%% ===================================================================

%% @doc 测试添加用户到黑名单 - 成功场景
handle_add_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"denied_user_id">>, <<"encoded_67890">>}
                ]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(EncodedId) ->
                case EncodedId of
                    <<"encoded_67890">> -> 67890;
                    _ -> 12345
                end
            end},
            {'encode', 1, fun(Id) ->
                integer_to_binary(Id)
            end}
        ]},
        {user_denylist_logic, [
            {'add', 2, fun(_Uid, _DeniedUid) ->
                <<"2026-01-20 10:00:00">>
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(user_denylist_logic, add, 2)
    end).

%% @doc 测试添加用户到黑名单 - 空用户ID
handle_add_empty_user_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"denied_user_id">>, <<>>}
                ]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(EncodedId) ->
                case EncodedId of
                    <<>> -> 0;
                    _ -> 12345
                end
            end},
            {'encode', 1, fun(Id) ->
                integer_to_binary(Id)
            end}
        ]},
        {user_denylist_logic, [
            {'add', 2, fun(_Uid, _DeniedUid) ->
                ?ASSERT_EQUAL(0, _DeniedUid),
                <<"2026-01-20 10:00:00">>
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（空用户ID）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% ===================================================================
%% 从黑名单移除测试
%% ===================================================================

%% @doc 测试从黑名单移除用户 - 成功场景
handle_remove_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"denied_user_id">>, <<"encoded_67890">>}
                ]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(EncodedId) ->
                case EncodedId of
                    <<"encoded_67890">> -> 67890;
                    _ -> 12345
                end
            end}
        ]},
        {user_denylist_logic, [
            {'remove', 2, fun(_Uid, _DeniedUid) ->
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
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(user_denylist_logic, remove, 2)
    end).

%% @doc 测试从黑名单移除用户 - 空用户ID
handle_remove_empty_user_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"denied_user_id">>, <<>>}
                ]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(EncodedId) ->
                case EncodedId of
                    <<>> -> 0;
                    _ -> 12345
                end
            end}
        ]},
        {user_denylist_logic, [
            {'remove', 2, fun(_Uid, _DeniedUid) ->
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
        % 模拟请求（空用户ID）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% ===================================================================
%% 黑名单分页列表测试
%% ===================================================================

%% @doc 测试黑名单分页列表 - 成功场景
handle_page_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {user_denylist_logic, [
            {'page', 3, fun(_Uid, _Page, _Size) ->
                #{
                    total => 2,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            denied_user_id => 67890,
                            nickname => <<"Blocked User 1">>,
                            avatar => <<"https://example.com/avatar1.jpg">>,
                            created_at => <<"2026-01-20 10:00:00">>
                        },
                        #{
                            denied_user_id => 67891,
                            nickname => <<"Blocked User 2">>,
                            avatar => <<"https://example.com/avatar2.jpg">>,
                            created_at => <<"2026-01-19 15:30:00">>
                        }
                    ]
                }
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(user_denylist_logic, page, 3)
    end).

%% @doc 测试黑名单分页列表 - 空列表
handle_page_empty_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {user_denylist_logic, [
            {'page', 3, fun(_Uid, _Page, _Size) ->
                #{
                    total => 0,
                    page => 1,
                    size => 20,
                    list => []
                }
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（空列表）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试黑名单分页列表 - 分页处理
handle_page_pagination_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {2, 10}  % 第2页，每页10条
            end}
        ]},
        {user_denylist_logic, [
            {'page', 3, fun(_Uid, Page, Size) ->
                ?ASSERT_EQUAL(2, Page),
                ?ASSERT_EQUAL(10, Size),
                #{
                    total => 25,
                    page => Page,
                    size => Size,
                    list => [
                        #{
                            denied_user_id => 67892,
                            nickname => <<"Blocked User 3">>,
                            avatar => <<"https://example.com/avatar3.jpg">>,
                            created_at => <<"2026-01-18 10:00:00">>
                        }
                    ]
                }
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（分页）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=2&size=10">>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试黑名单添加 - 验证返回数据结构
handle_add_response_structure_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"denied_user_id">>, <<"encoded_67890">>}
                ]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                67890
            end},
            {'encode', 1, fun(Id) ->
                <<"encoded_", (integer_to_binary(Id))/binary>>
            end}
        ]},
        {user_denylist_logic, [
            {'add', 2, fun(_Uid, _DeniedUid) ->
                <<"2026-01-20 10:00:00">>
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{<<"user_id">> := _, <<"denied_user_id">> := _, <<"created_at">> := _}}, Body),

        % 验证返回数据结构包含必需字段
        #{data := Data} = Body,
        ?ASSERT_EQUAL(<<"encoded_12345">>, maps:get(<<"user_id">>, Data)),
        ?ASSERT_EQUAL(<<"encoded_67890">>, maps:get(<<"denied_user_id">>, Data)),
        ?ASSERT_EQUAL(<<"2026-01-20 10:00:00">>, maps:get(<<"created_at">>, Data))
    end).
