-module(stress_testing_ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% stress_testing_ws_handler 模块的 EUnit 测试
%%%
%%% 目标：验证压力测试 WebSocket 处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 压力测试 WebSocket 处理器测试
%% ===================================================================

%% @doc 测试压力测试 - 成功场景
handle_stress_test_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"test_type">>, <<"connection_stress">>},
                    {<<"client_count">>, 100},
                    {<<"duration">>, 60},
                    {<<"message_rate">>, 10},
                    {<<"payload_size">>, 1024}
                ]
            end}
        ]},
        {stress_testing_logic, [
            {'start_test', 5, fun(_TestType, _ClientCount, _Duration, _MessageRate, _PayloadSize) ->
                {ok, #{
                    test_id => <<"test_123456">>,
                    test_type => <<"connection_stress">>,
                    status => running,
                    started_at => imboy_dt:timestamp(),
                    client_count => 100,
                    duration => 60,
                    expected_messages => 60000
                }}
            end}
        ]},
        {imboy_response, [
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
        {ok, Req, _State} = stress_testing_ws_handler:init(MockReq, #{action => start}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{test_id := <<"test_123456">>}}, Body),
        
        % 验证具体返回值
        #{data := TestData} = Body,
        ?ASSERT_EQUAL(<<"connection_stress">>, maps:get(<<"test_type">>, TestData)),
        ?ASSERT_EQUAL(running, maps:get(<<"status">>, TestData)),
        ?ASSERT_EQUAL(100, maps:get(<<"client_count">>, TestData)),
        ?ASSERT_EQUAL(60000, maps:get(<<"expected_messages">>, TestData)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(stress_testing_logic, start_test, 5)
    end).

%% @doc 测试压力测试 - 参数验证失败
handle_stress_test_invalid_params_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"test_type">>, <<"connection_stress">>},
                    {<<"client_count">>, 1000},  % 超过限制
                    {<<"duration">>, 60},
                    {<<"message_rate">>, 10},
                    {<<"payload_size">>, 1024}
                ]
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 400,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（参数验证失败）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = stress_testing_ws_handler:init(MockReq, #{action => start}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).
