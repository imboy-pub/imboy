-module(live_room_stream_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% live_room_stream_handler 模块的 EUnit 测试
%%%
%%% 目标：验证直播间流处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 开始直播流测试
%% ===================================================================

%% @doc 测试开始直播流 - 成功场景
handle_stream_start_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"room_id">>, 12345},
                    {<<"stream_key">>, <<"live_stream_key_123">>},
                    {<<"title">>, <<"精彩直播">>},
                    {<<"description">>, <<"今天分享一些有趣的内容">>}
                ]
            end}
        ]},
        {live_room_logic, [
            {'start_stream', 4, fun(_Uid, _RoomId, _StreamKey, _Metadata) ->
                {ok, #{
                    stream_id => 98765,
                    room_id => 12345,
                    stream_url => <<"rtmp://live.example.com/live/98765">>,
                    stream_key => <<"live_stream_key_123">>,
                    status => streaming,
                    started_at => elib_dt:timestamp(),
                    viewer_count => 0
                }}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = live_room_stream_handler:init(MockReq, #{action => start}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{stream_id := 98765}}, Body),
        
        % 验证具体返回值
        #{data := Stream} = Body,
        ?ASSERT_EQUAL(12345, maps:get(<<"room_id">>, Stream)),
        ?ASSERT_EQUAL(streaming, maps:get(<<"status">>, Stream)),
        ?ASSERT_EQUAL(0, maps:get(<<"viewer_count">>, Stream)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(live_room_logic, start_stream, 4),
        meck_helper:verify_called(elib_response, success, 3)
    end).

%% @doc 测试开始直播流 - 房间不存在
handle_stream_start_room_not_found_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"room_id">>, 99999},
                    {<<"stream_key">>, <<"invalid_key">>}
                ]
            end}
        ]},
        {live_room_logic, [
            {'start_stream', 4, fun(_Uid, RoomId, _StreamKey, _Metadata) when RoomId =:= 99999 ->
                {error, room_not_found}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 404,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（房间不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = live_room_stream_handler:init(MockReq, #{action => start}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := room_not_found}, Body)
    end).

%% @doc 测试开始直播流 - 流密钥无效
handle_stream_start_invalid_key_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"room_id">>, 12345},
                    {<<"stream_key">>, <<"invalid_key">>}
                ]
            end}
        ]},
        {live_room_logic, [
            {'start_stream', 4, fun(_Uid, _RoomId, StreamKey, _Metadata) when StreamKey =:= <<"invalid_key">> ->
                {error, invalid_stream_key}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 401,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（流密钥无效）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = live_room_stream_handler:init(MockReq, #{action => start}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(401, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := invalid_stream_key}, Body)
    end).

%% ===================================================================
%% 停止直播流测试
%% ===================================================================

%% @doc 测试停止直播流 - 成功场景
handle_stream_stop_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"room_id">>, 12345},
                    {<<"stream_id">>, 98765}
                ]
            end}
        ]},
        {live_room_logic, [
            {'stop_stream', 3, fun(_Uid, _RoomId, _StreamId) ->
                {ok, #{
                    stream_id => 98765,
                    room_id => 12345,
                    status => stopped,
                    stopped_at => elib_dt:timestamp(),
                    duration => 3600,  % 直播时长（秒）
                    peak_viewer_count => 150,
                    total_viewer_count => 500
                }}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = live_room_stream_handler:init(MockReq, #{action => stop}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{stream_id := 98765}}, Body),
        
        % 验证具体返回值
        #{data := Stream} = Body,
        ?ASSERT_EQUAL(stopped, maps:get(<<"status">>, Stream)),
        ?ASSERT_EQUAL(3600, maps:get(<<"duration">>, Stream)),
        ?ASSERT_EQUAL(150, maps:get(<<"peak_viewer_count">>, Stream)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(live_room_logic, stop_stream, 3)
    end).

%% @doc 测试停止直播流 - 流不存在
handle_stream_stop_not_found_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"room_id">>, 12345},
                    {<<"stream_id">>, 99999}
                ]
            end}
        ]},
        {live_room_logic, [
            {'stop_stream', 3, fun(_Uid, _RoomId, StreamId) when StreamId =:= 99999 ->
                {error, stream_not_found}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 404,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（流不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = live_room_stream_handler:init(MockReq, #{action => stop}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := stream_not_found}, Body)
    end).

%% @doc 测试停止直播流 - 权限不足
handle_stream_stop_permission_denied_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"room_id">>, 12345},
                    {<<"stream_id">>, 98765}
                ]
            end}
        ]},
        {live_room_logic, [
            {'stop_stream', 3, fun(_Uid, _RoomId, _StreamId) ->
                {error, permission_denied}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 403,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（权限不足）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = live_room_stream_handler:init(MockReq, #{action => stop}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(403, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := permission_denied}, Body)
    end).

%% ===================================================================
%% 获取直播流状态测试
%% ===================================================================

%% @doc 测试获取直播流状态 - 成功场景
handle_get_stream_status_test_() ->
    ?WITH_MECKS([
        {live_room_logic, [
            {'get_stream_status', 2, fun(_Uid, RoomId) ->
                {ok, #{
                    room_id => RoomId,
                    stream_id => 98765,
                    status => streaming,
                    title => <<"精彩直播">>,
                    viewer_count => 150,
                    started_at => elib_dt:timestamp(),
                    stream_url => <<"rtmp://live.example.com/live/98765">>
                }}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = meck_helper:test_request(#{
            method => <<"GET">>,
            headers => #{<<"imboy-uid">> => <<"12345">>},
            qs => <<"room_id=12345">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = live_room_stream_handler:init(MockReq, #{action => status}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{status := streaming}}, Body),
        
        % 验证具体返回值
        #{data := Stream} = Body,
        ?ASSERT_EQUAL(12345, maps:get(<<"room_id">>, Stream)),
        ?ASSERT_EQUAL(150, maps:get(<<"viewer_count">>, Stream)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(live_room_logic, get_stream_status, 2)
    end).
