-module(feedback_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% feedback_handler 模块的 EUnit 测试
%%%
%%% 目标：验证反馈处理器功能
%%%===================================================================

%% ===================================================================
%% Handler 功能测试
%% ===================================================================

%% @doc 测试反馈分页列表功能
page_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}  % Page=1, Size=20
            end}
        ]},
        {elib_cnv, [
            {'implode', 2, fun(_Separator, Parts) ->
                iolist_to_binary(Parts)
            end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() ->
                <<"feedback">>
            end}
        ]},
        {elib_pg, [
            {'page', 6, fun(_Table, _Column, _Where, _Order, _Size, _Offset) ->
                % 模拟分页查询结果
                #{
                    total => 2,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            feedback_id => 1,
                            device_id => <<"device_123">>,
                            type => 1,
                            rating => 5,
                            contact_detail => <<"user@example.com">>,
                            body => <<"Great app!">>,
                            attach => <<"[]">>,
                            reply_count => 0,
                            status => 1,
                            created_at => <<"2025-12-24 10:00:00">>
                        },
                        #{
                            feedback_id => 2,
                            device_id => <<"device_456">>,
                            type => 2,
                            rating => 4,
                            contact_detail => <<"+8613800138000">>,
                            body => <<"Some suggestions">>,
                            attach => <<"[\"screenshot1.jpg\"]">>,
                            reply_count => 1,
                            status => 1,
                            created_at => <<"2025-12-24 11:00:00">>
                        }
                    ]
                }
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Message) ->
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
        {ok, Req, _State} = feedback_handler:init(MockReq, #{
            action => page,
            current_uid => 12345
        }),
        
        % 验证响应状态和数据结构
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试添加反馈功能
add_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"1">>},
                    {<<"rating">>, <<"5">>},
                    {<<"contact_detail">>, <<"user@example.com">>},
                    {<<"description">>, <<"Great app!">>},
                    {<<"screenshot">>, []},
                    {<<"sys_version">>, <<"iOS 15.0">>}
                ]
            end}
        ]},
        {feedback_ds, [
            {'add', 11, fun(_UserId, _DeviceId, _Cos, _CosV, _AppVsn, _Type, _Rating, _ContactDetail, _Description, _Attach) ->
                ok
            end}
        ]},
        {ec_cnv, [
            {'to_binary', 1, fun(Value) when is_integer(Value) ->
                integer_to_binary(Value);
                (Value) when is_binary(Value) ->
                    Value
            end}
        ]},
        {jsone, [
            {'encode', 2, fun(_Data, _Options) ->
                <<"[]">>
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
            qs => <<>>,
            headers => #{
                <<"cos">> => <<"ios">>,
                <<"vsn">> => <<"1.0.0">>,
                <<"did">> => <<"device_123">>
            }
        }),
        
        % 调用 handler
        {ok, Req, _State} = feedback_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试删除反馈功能
remove_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 1001}  % 反馈ID
            end}
        ]},
        {feedback_ds, [
            {'remove', 2, fun(_UserId, _FeedbackId) ->
                #{<<"deleted">> => 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end}
        ]}
    ], fun() ->
        % 模拟一个 DELETE 请求
        MockReq = cowboy_req_h:new(#{
            method => <<"DELETE">>,
            qs => <<"feedback_id=1001">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = feedback_handler:init(MockReq, #{
            action => remove,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试删除反馈功能 - 无效ID
remove_invalid_id_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, <<"反馈ID必须是整数"/utf8>>}  % 错误消息
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
        % 模拟一个 DELETE 请求（无效ID）
        MockReq = cowboy_req_h:new(#{
            method => <<"DELETE">>,
            qs => <<"feedback_id=invalid">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = feedback_handler:init(MockReq, #{
            action => remove,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试反馈回复分页列表功能
page_reply_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 1001}  % 反馈ID
            end},
            {'page', 1, fun(_Req) ->
                {1, 20}  % Page=1, Size=20
            end}
        ]},
        {elib_cnv, [
            {'implode', 2, fun(_Separator, Parts) ->
                iolist_to_binary(Parts)
            end}
        ]},
        {feedback_reply_repo, [
            {'tablename', 0, fun() ->
                <<"feedback_reply">>
            end}
        ]},
        {elib_pg, [
            {'page', 6, fun(_Table, _Column, _Where, _Order, _Size, _Offset) ->
                % 模拟回复分页查询结果
                #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            feedback_reply_id => 1,
                            feedback_id => 1001,
                            feedback_reply_pid => 0,
                            replier_user_id => 999,
                            replier_name => <<"Admin">>,
                            body => <<"Thank you for your feedback!">>,
                            status => 1,
                            created_at => <<"2025-12-24 12:00:00">>
                        }
                    ]
                }
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Message) ->
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
            qs => <<"feedback_id=1001">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = feedback_handler:init(MockReq, #{
            action => page_reply,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 测试反馈回复分页列表功能 - 无效ID
page_reply_invalid_id_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, <<"反馈ID必须是整数"/utf8>>}  % 错误消息
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
        % 模拟一个 GET 请求（无效ID）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"feedback_id=invalid">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = feedback_handler:init(MockReq, #{
            action => page_reply,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(400, StatusCode)
    end).

%% @doc 测试添加反馈功能 - 包含截图
add_with_screenshots_test_() ->
    ?WITH_MOCKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"type">>, <<"2">>},
                    {<<"rating">>, <<"4">>},
                    {<<"contact_detail">>, <<"+8613800138000">>},
                    {<<"description">>, <<"Bug report">>},
                    {<<"screenshot">>, [<<"screenshot1.jpg">>, <<"screenshot2.jpg">>]},
                    {<<"sys_version">>, <<"Android 12">>}
                ]
            end}
        ]},
        {feedback_ds, [
            {'add', 11, fun(_UserId, _DeviceId, _Cos, _CosV, _AppVsn, _Type, _Rating, _ContactDetail, _Description, _Attach) ->
                % 验证截图附件
                ?assert(binary:contains(_Attach, <<"screenshot1.jpg">>)),
                ?assert(binary:contains(_Attach, <<"screenshot2.jpg">>)),
                ok
            end}
        ]},
        {ec_cnv, [
            {'to_binary', 1, fun(Value) when is_integer(Value) ->
                integer_to_binary(Value);
                (Value) when is_binary(Value) ->
                    Value
            end}
        ]},
        {jsone, [
            {'encode', 2, fun(Data, _Options) ->
                DataJson = jsone:encode(Data, [native_utf8]),
                DataJson
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
        % 模拟一个 POST 请求（包含截图）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>,
            headers => #{
                <<"cos">> => <<"android">>,
                <<"vsn">> => <<"1.0.1">>,
                <<"did">> => <<"device_456">>
            }
        }),
        
        % 调用 handler
        {ok, Req, _State} = feedback_handler:init(MockReq, #{
            action => add,
            current_uid => 12345
        }),
        
        % 验证响应状态
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).
