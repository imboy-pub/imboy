-module(conversation_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% conversation_handler 模块的 EUnit 测试
%%%
%%% 目标：验证会话处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试在线状态查询功能 - 默认格式
online_default_test_() ->
    ?WITH_MOCKS([
        {application, [
            {'get_key', 2, fun(_App, _Key) ->
                {ok, <<"0.7.2">>}
            end}
        ]},
        {imboy_syn, [
            {'count_user', 0, fun() ->
                150  % 在线用户数
            end},
            {'count', 0, fun() ->
                320  % 在线设备数
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
        % 模拟一个 GET 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<>>
        }),
        
        % 调用 handler
        {ok, Req, _State} = conversation_handler:init(MockReq, #{action => online}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试在线状态查询功能 - 列表格式
online_list_test_() ->
    ?WITH_MOCKS([
        {application, [
            {'get_key', 2, fun(_App, _Key) ->
                {ok, <<"0.7.2">>}
            end}
        ]},
        {imboy_syn, [
            {'count_user', 0, fun() ->
                150
            end},
            {'count', 0, fun() ->
                320
            end},
            {'list_by_limit', 1, fun(_Limit) ->
                [
                    {{12345, self()}, {device_type, device_id_123}, 1640329200000000000, ref1, node@host},
                    {{67890, self()}, {device_type, device_id_456}, 1640329300000000000, ref2, node@host}
                ]
            end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 2, fun(_Nano, _Unit) ->
                "2021-12-24T10:00:00Z"
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
        % 模拟一个 GET 请求（列表格式）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"type=list&limit=5">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = conversation_handler:init(MockReq, #{action => online}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试我的消息功能
mine_test_() ->
    ?WITH_MOCKS([
        {msg_c2c_ds, [
            {'read_msg', 3, fun(_UserId, _Limit, _LastServerTS) ->
                [
                    [
                        {<<"id">>, 1001},
                        {<<"payload">>, <<"{\"from_uid\":12345,\"to_uid\":67890,\"content\":\"Hello\",\"msg_type\":1,\"created_at\":\"2025-12-24T10:00:00Z\"}">>}
                    ],
                    [
                        {<<"id">>, 1002},
                        {<<"payload">>, <<"{\"from_uid\":67890,\"to_uid\":12345,\"content\":\"Hi there\",\"msg_type\":1,\"created_at\":\"2025-12-24T10:01:00Z\"}">>}
                    ]
                ]
            end}
        ]},
        {jsone, [
            {'decode', 2, fun(Json, _Options) ->
                case Json of
                    <<"{\"from_uid\":12345,\"to_uid\":67890,\"content\":\"Hello\",\"msg_type\":1,\"created_at\":\"2025-12-24T10:00:00Z\"}">> ->
                        [{<<"from_uid">>, 12345}, {<<"to_uid">>, 67890}, {<<"content">>, <<"Hello">>}, {<<"msg_type">>, 1}, {<<"created_at">>, <<"2025-12-24T10:00:00Z">>}];
                    <<"{\"from_uid\":67890,\"to_uid\":12345,\"content\":\"Hi there\",\"msg_type\":1,\"created_at\":\"2025-12-24T10:01:00Z\"}">> ->
                        [{<<"from_uid">>, 67890}, {<<"to_uid">>, 12345}, {<<"content">>, <<"Hi there">>}, {<<"msg_type">>, 1}, {<<"created_at">>, <<"2025-12-24T10:01:00Z">>}]
                end
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
            qs => <<"last_server_ts=1640329200">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = conversation_handler:init(MockReq, #{
            action => mine,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试我的消息功能 - 无新消息
mine_no_messages_test_() ->
    ?WITH_MOCKS([
        {msg_c2c_ds, [
            {'read_msg', 3, fun(_UserId, _Limit, _LastServerTS) ->
                []  % 无新消息
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
        % 模拟一个 GET 请求（无新消息）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"last_server_ts=1640329200">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = conversation_handler:init(MockReq, #{
            action => mine,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试我的消息功能 - 指定时间戳
mine_with_timestamp_test_() ->
    ?WITH_MOCKS([
        {msg_c2c_ds, [
            {'read_msg', 3, fun(_UserId, _Limit, LastServerTS) ->
                % 验证传递的时间戳参数
                ?assertEqual(<<"1640329200">>, LastServerTS),
                [
                    [
                        {<<"id">>, 1003},
                        {<<"payload">>, <<"{\"from_uid\":11111,\"to_uid\":12345,\"content\":\"New message\",\"msg_type\":1,\"created_at\":\"2025-12-24T10:02:00Z\"}">>}
                    ]
                ]
            end}
        ]},
        {jsone, [
            {'decode', 2, fun(_Json, _Options) ->
                [{<<"from_uid">>, 11111}, {<<"to_uid">>, 12345}, {<<"content">>, <<"New message">>}, {<<"msg_type">>, 1}, {<<"created_at">>, <<"2025-12-24T10:02:00Z">>}]
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
        % 模拟一个 GET 请求（指定时间戳）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"last_server_ts=1640329200">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = conversation_handler:init(MockReq, #{
            action => mine,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试在线状态查询功能 - 限制数量
online_with_limit_test_() ->
    ?WITH_MOCKS([
        {application, [
            {'get_key', 2, fun(_App, _Key) ->
                {ok, <<"0.7.2">>}
            end}
        ]},
        {imboy_syn, [
            {'count_user', 0, fun() ->
                150
            end},
            {'count', 0, fun() ->
                320
            end},
            {'list_by_limit', 1, fun(Limit) ->
                % 验证限制数量参数
                ?assertEqual(3, Limit),
                [
                    {{12345, self()}, {device_type, device_id_123}, 1640329200000000000, ref1, node@host}
                ]
            end}
        ]},
        {elib_dt, [
            {'to_rfc3339', 2, fun(_Nano, _Unit) ->
                "2021-12-24T10:00:00Z"
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
        % 模拟一个 GET 请求（限制数量）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"type=list&limit=3">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = conversation_handler:init(MockReq, #{action => online}),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).
