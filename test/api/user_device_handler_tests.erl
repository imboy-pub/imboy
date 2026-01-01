-module(user_device_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_device_handler 模块的 EUnit 测试
%%%
%%% 目标：验证用户设备处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 设备注册测试
%% ===================================================================

%% @doc 测试设备注册 - 成功场景
handle_register_device_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"device_id">>, <<"device_12345">>},
                    {<<"device_type">>, <<"mobile">>},
                    {<<"device_name">>, <<"iPhone 13">>},
                    {<<"push_token">>, <<"push_token_abc123">>},
                    {<<"os_version">>, <<"iOS 15.0">>}
                ]
            end}
        ]},
        {user_device_logic, [
            {'register_device', 6, fun(_Uid, _DeviceId, _DeviceType, _DeviceName, _PushToken, _OsVersion) ->
                {ok, #{
                    device_id => <<"device_12345">>,
                    uid => 12345,
                    device_type => <<"mobile">>,
                    device_name => <<"iPhone 13">>,
                    push_token => <<"push_token_abc123">>,
                    os_version => <<"iOS 15.0">>,
                    status => active,
                    registered_at => imboy_dt:timestamp()
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
        {ok, Req, _State} = user_device_handler:init(MockReq, #{action => register}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{device_id := <<"device_12345">>}}, Body),
        
        % 验证具体返回值
        #{data := Device} = Body,
        ?ASSERT_EQUAL(<<"mobile">>, maps:get(<<"device_type">>, Device)),
        ?ASSERT_EQUAL(active, maps:get(<<"status">>, Device)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_device_logic, register_device, 6)
    end).

%% @doc 测试设备注册 - 设备已存在
handle_register_device_exists_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"device_id">>, <<"device_12345">>},
                    {<<"device_type">>, <<"mobile">>}
                ]
            end}
        ]},
        {user_device_logic, [
            {'register_device', 6, fun(_Uid, DeviceId, _DeviceType, _DeviceName, _PushToken, _OsVersion) when DeviceId =:= <<"device_12345">> ->
                {error, device_already_exists}
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 409,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（设备已存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_device_handler:init(MockReq, #{action => register}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(409, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := device_already_exists}, Body)
    end).

%% ===================================================================
%% 设备更新测试
%% ===================================================================

%% @doc 测试设备更新 - 成功场景
handle_update_device_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"device_id">>, <<"device_12345">>},
                    {<<"push_token">>, <<"new_push_token_456">>},
                    {<<"os_version">>, <<"iOS 15.1">>}
                ]
            end}
        ]},
        {user_device_logic, [
            {'update_device', 5, fun(_Uid, _DeviceId, _PushToken, _OsVersion, _DeviceName) ->
                {ok, #{
                    device_id => <<"device_12345">>,
                    uid => 12345,
                    push_token => <<"new_push_token_456">>,
                    os_version => <<"iOS 15.1">>,
                    updated_at => imboy_dt:timestamp()
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
        {ok, Req, _State} = user_device_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{device_id := <<"device_12345">>}}, Body),
        
        % 验证具体返回值
        #{data := Device} = Body,
        ?ASSERT_EQUAL(<<"new_push_token_456">>, maps:get(<<"push_token">>, Device)),
        ?ASSERT_EQUAL(<<"iOS 15.1">>, maps:get(<<"os_version">>, Device)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_device_logic, update_device, 5)
    end).

%% @doc 测试设备更新 - 设备不存在
handle_update_device_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"device_id">>, <<"device_99999">>},
                    {<<"push_token">>, <<"new_token">>}
                ]
            end}
        ]},
        {user_device_logic, [
            {'update_device', 5, fun(_Uid, DeviceId, _PushToken, _OsVersion, _DeviceName) when DeviceId =:= <<"device_99999">> ->
                {error, device_not_found}
            end}
        ]},
        {imboy_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 404,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（设备不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_device_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := device_not_found}, Body)
    end).
