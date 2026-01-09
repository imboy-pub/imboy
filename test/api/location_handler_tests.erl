-module(location_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% location_handler 模块的 EUnit 测试
%%%
%%% 目标：验证位置处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 位置更新测试
%% ===================================================================

%% @doc 测试更新位置 - 成功场景
handle_update_location_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"latitude">>, 39.9042},
                    {<<"longitude">>, 116.4074},
                    {<<"accuracy">>, 10.0},
                    {<<"address">>, <<"北京市朝阳区">>}
                ]
            end}
        ]},
        {location_logic, [
            {'update_location', 5, fun(_Uid, _Lat, _Lng, _Accuracy, _Address) ->
                {ok, #{
                    uid => 12345,
                    latitude => 39.9042,
                    longitude => 116.4074,
                    accuracy => 10.0,
                    address => <<"北京市朝阳区">>,
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
        {ok, Req, _State} = location_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{latitude := 39.9042}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(location_logic, update_location, 5),
        meck_helper:verify_called(imboy_response, success, 3)
    end).

%% @doc 测试更新位置 - 坐标无效
handle_update_location_invalid_coords_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"latitude">>, 91.0},  % 无效纬度
                    {<<"longitude">>, 116.4074},
                    {<<"accuracy">>, 10.0},
                    {<<"address">>, <<"北京市朝阳区">>}
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
        % 模拟请求（无效坐标）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).

%% @doc 测试更新位置 - 参数缺失
handle_update_location_missing_params_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                % 缺少必要参数
                [
                    {<<"latitude">>, 39.9042}
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
        % 模拟请求（参数缺失）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).

%% ===================================================================
%% 获取附近用户测试
%% ===================================================================

%% @doc 测试获取附近用户 - 成功场景
handle_get_nearby_users_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'get', 1, fun(_Req) ->
                [
                    {<<"latitude">>, 39.9042},
                    {<<"longitude">>, 116.4074},
                    {<<"radius">>, 1000},
                    {<<"limit">>, 20}
                ]
            end}
        ]},
        {location_logic, [
            {'get_nearby_users', 4, fun(_Lat, _Lng, _Radius, _Limit) ->
                {ok, [
                    #{
                        uid => 67890,
                        nickname => <<"Nearby User 1">>,
                        distance => 500.5,
                        last_seen => imboy_dt:timestamp()
                    },
                    #{
                        uid => 67891,
                        nickname => <<"Nearby User 2">>,
                        distance => 800.2,
                        last_seen => imboy_dt:timestamp()
                    }
                ]}
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
            method => <<"GET">>,
            qs => <<"latitude=39.9042&longitude=116.4074&radius=1000&limit=20">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{action => nearby}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := [_|_]}, Body), % 非空列表
        
        % 验证返回数据结构
        #{data := List} = Body,
        ?assert(length(List) >= 1),
        lists:foreach(fun(User) ->
            ?ASSERT_MATCH(#{uid := _, nickname := _, distance := _}, User)
        end, List),
        
        % 验证 Mock 调用
        meck_helper:verify_called(location_logic, get_nearby_users, 4)
    end).

%% @doc 测试获取附近用户 - 无附近用户
handle_get_nearby_users_empty_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'get', 1, fun(_Req) ->
                [
                    {<<"latitude">>, 39.9042},
                    {<<"longitude">>, 116.4074},
                    {<<"radius">>, 100},
                    {<<"limit">>, 20}
                ]
            end}
        ]},
        {location_logic, [
            {'get_nearby_users', 4, fun(_Lat, _Lng, _Radius, _Limit) ->
                {ok, []}
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
        % 模拟请求（小半径，无附近用户）
        MockReq = meck_helper:test_request(#{
            method => <<"GET">>,
            qs => <<"latitude=39.9042&longitude=116.4074&radius=100&limit=20">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{action => nearby}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := []}, Body)
    end).

%% @doc 测试获取附近用户 - 参数无效
handle_get_nearby_users_invalid_params_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'get', 1, fun(_Req) ->
                [
                    {<<"latitude">>, 91.0},  % 无效纬度
                    {<<"longitude">>, 116.4074},
                    {<<"radius">>, 1000},
                    {<<"limit">>, 20}
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
        % 模拟请求（无效参数）
        MockReq = meck_helper:test_request(#{
            method => <<"GET">>,
            qs => <<"latitude=91.0&longitude=116.4074&radius=1000&limit=20">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{action => nearby}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).
