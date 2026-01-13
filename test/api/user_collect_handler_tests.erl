-module(user_collect_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_collect_handler 模块的 EUnit 测试
%%%
%%% 目标：验证用户收藏处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试添加收藏功能
add_collect_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"message">>},
                    {<<"target_id">>, <<"encoded_msg_12345">>},
                    {<<"title">>, <<"Important Message">>},
                    {<<"content">>, <<"This is an important message to collect">>}
                ]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(EncodedId) ->
                case EncodedId of
                    <<"encoded_msg_12345">> -> 12345
                end
            end}
        ]},
        {user_collect_logic, [
            {'add', 5, fun(_UserId, _Type, _TargetId, _Title, _Content) ->
                {ok, #{collect_id => 3001, created => true}}
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
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试移除收藏功能
remove_collect_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"collect_id">>, <<"3001">>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'remove', 2, fun(_UserId, _CollectId) ->
                {ok, #{removed => true}}
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
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            action => remove,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试列出收藏功能
list_collects_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {user_collect_logic, [
            {'list', 2, fun(_UserId, _Page) ->
                [
                    #{
                        collect_id => 3001,
                        type => <<"message">>,
                        target_id => 12345,
                        title => <<"Important Message">>,
                        content => <<"This is an important message to collect">>,
                        created_at => <<"2025-12-24 10:00:00">>
                    },
                    #{
                        collect_id => 3002,
                        type => <<"file">>,
                        target_id => 67890,
                        title => <<"Important Document">>,
                        content => <<"Important document for reference">>,
                        created_at => <<"2025-12-23 15:30:00">>
                    }
                ]
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
            qs => <<"page=1">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            action => list,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试添加收藏功能 - 重复收藏
add_collect_duplicate_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"message">>},
                    {<<"target_id">>, <<"encoded_msg_12345">>},
                    {<<"title">>, <<"Already Collected">>},
                    {<<"content">>, <<"This message is already collected">>}
                ]
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(_EncodedId) ->
                12345
            end}
        ]},
        {user_collect_logic, [
            {'add', 5, fun(_UserId, _Type, _TargetId, _Title, _Content) ->
                {error, already_collected}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 409,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（重复收藏）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(409, StatusCode)
    end).

%% @doc 测试移除收藏功能 - 收藏不存在
remove_collect_not_found_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"collect_id">>, <<"9999">>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'remove', 2, fun(_UserId, _CollectId) ->
                {error, not_found}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 404,
                    response_body => #{status => error}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 POST 请求（收藏不存在）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            action => remove,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(404, StatusCode)
    end).

%% @doc 测试列出收藏功能 - 按类型过滤
list_collects_by_type_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {user_collect_logic, [
            {'list', 3, fun(_UserId, _Page, _Options) ->
                % 验证过滤选项
                ?assertEqual(#{type => <<"message">>}, _Options),
                [
                    #{
                        collect_id => 3001,
                        type => <<"message">>,
                        target_id => 12345,
                        title => <<"Important Message">>,
                        content => <<"This is an important message to collect">>,
                        created_at => <<"2025-12-24 10:00:00">>
                    }
                ]
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
        % 模拟一个 GET 请求（按类型过滤）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=1&type=message">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            action => list,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试添加收藏功能 - 无效类型
add_collect_invalid_type_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"invalid_type">>},
                    {<<"target_id">>, <<"12345">>},
                    {<<"title">>, <<"Invalid Collect">>},
                    {<<"content">>, <<"This should fail">>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'add', 5, fun(_UserId, _Type, _TargetId, _Title, _Content) ->
                {error, invalid_type}
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
        % 模拟一个 POST 请求（无效类型）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).
