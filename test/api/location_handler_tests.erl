-module(location_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% location_handler 模块的 EUnit 测试
%%%
%%% 目标：验证位置处理器功能
%%% 覆盖：设置可见性、查找附近用户
%%%===================================================================

%% ===================================================================
%% 设置可见性测试
%% ===================================================================

%% @doc 测试设置自己可见 - 成功场景
handle_make_myself_visible_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"latitude">>, 39.9042},
                    {<<"longitude">>, 116.4074}
                ]
            end}
        ]},
        {location_logic, [
            {'make_myself_visible', 3, fun(_Uid, _Lat, _Lng) ->
                ok
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
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => make_myself_visible,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(location_logic, make_myself_visible, 3),
        meck_helper:verify_called(elib_response, success, 3)
    end).

%% @doc 测试设置自己可见 - 参数缺失
handle_make_myself_visible_missing_params_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                []  % 缺少经纬度参数
            end}
        ]},
        {location_logic, [
            {'make_myself_visible', 3, fun(_Uid, _Lat, _Lng) ->
                ok
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
        % 模拟请求（参数缺失）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => make_myself_visible,
            current_uid => 12345
        }),

        % 验证响应（应该返回空坐标）
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试设置自己不可见 - 成功场景
handle_make_myself_unvisible_test_() ->
    ?WITH_MECKS([
        {location_logic, [
            {'make_myself_unvisible', 1, fun(_Uid) ->
                ok
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
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => make_myself_unvisible,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(location_logic, make_myself_unvisible, 1)
    end).

%% @doc 测试设置自己可见 - 位置逻辑错误
handle_make_myself_visible_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"latitude">>, 39.9042},
                    {<<"longitude">>, 116.4074}
                ]
            end}
        ]},
        {location_logic, [
            {'make_myself_visible', 3, fun(_Uid, _Lat, _Lng) ->
                {error, <<"无效的坐标"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Data, _Message) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success}
                })
            end},
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 400,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（位置逻辑返回错误）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => make_myself_visible,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% ===================================================================
%% 查找附近用户测试
%% ===================================================================

%% @doc 测试查找附近用户 - 成功场景
handle_people_nearby_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 1000}  % radius=1000, limit=20
            end}
        ]},
        {location_logic, [
            {'people_nearby', 6, fun(_Uid, _Lng, _Lat, _Radius, _Unit, _Limit) ->
                [
                    #{
                        uid => 67890,
                        nickname => <<"Nearby User 1">>,
                        distance => 500.5,
                        avatar => <<"https://example.com/avatar1.jpg">>
                    },
                    #{
                        uid => 67891,
                        nickname => <<"Nearby User 2">>,
                        distance => 800.2,
                        avatar => <<"https://example.com/avatar2.jpg">>
                    }
                ]
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
            qs => <<"longitude=116.4074&latitude=39.9042&radius=1000&limit=20&unit=m">>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => people_nearby,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(location_logic, people_nearby, 6)
    end).

%% @doc 测试查找附近用户 - 无结果
handle_people_nearby_empty_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 100}  % 小半径
            end}
        ]},
        {location_logic, [
            {'people_nearby', 6, fun(_Uid, _Lng, _Lat, _Radius, _Unit, _Limit) ->
                []  % 无附近用户
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
        % 模拟请求（小半径，无附近用户）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"longitude=116.4074&latitude=39.9042&radius=100&limit=20&unit=m">>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => people_nearby,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试查找附近用户 - 不同单位
handle_people_nearby_different_unit_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 1}  % 1公里
            end}
        ]},
        {location_logic, [
            {'people_nearby', 6, fun(_Uid, _Lng, _Lat, _Radius, Unit, _Limit) ->
                ?ASSERT_EQUAL(<<"km">>, Unit),
                [
                    #{
                        uid => 67892,
                        nickname => <<"Nearby User 3">>,
                        distance => 0.8,
                        avatar => <<"https://example.com/avatar3.jpg">>
                    }
                ]
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
        % 模拟请求（公里单位）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"longitude=116.4074&latitude=39.9042&radius=1&limit=20&unit=km">>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => people_nearby,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试查找附近用户 - 默认参数
handle_people_nearby_default_params_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                % radius 默认 500，limit 默认 100
                case _Key of
                    radius -> {ok, 500};
                    limit -> {ok, 100}
                end
            end}
        ]},
        {location_logic, [
            {'people_nearby', 6, fun(_Uid, _Lng, _Lat, Radius, _Unit, Limit) ->
                ?ASSERT_EQUAL(500, Radius),
                ?ASSERT_EQUAL(100, Limit),
                [
                    #{
                        uid => 67893,
                        nickname => <<"Nearby User 4">>,
                        distance => 300.0,
                        avatar => <<"https://example.com/avatar4.jpg">>
                    }
                ]
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
        % 模拟请求（使用默认参数）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"longitude=116.4074&latitude=39.9042">>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => people_nearby,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试查找附近用户 - 验证返回数据结构
handle_people_nearby_response_structure_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 1000}
            end}
        ]},
        {location_logic, [
            {'people_nearby', 6, fun(_Uid, _Lng, _Lat, _Radius, _Unit, _Limit) ->
                [
                    #{
                        uid => 67894,
                        nickname => <<"Test User">>,
                        distance => 500.0
                    }
                ]
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
            qs => <<"longitude=116.4074&latitude=39.9042&radius=1000&limit=20&unit=m">>
        }),

        % 调用 handler
        {ok, Req, _State} = location_handler:init(MockReq, #{
            action => people_nearby,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{<<"radius">> := _, <<"size">> := _, <<"unit">> := _, <<"list">> := _}}, Body),

        % 验证返回数据结构包含必需字段
        #{data := Data} = Body,
        ?ASSERT_EQUAL(1000, maps:get(<<"radius">>, Data)),
        ?ASSERT_EQUAL(1, maps:get(<<"size">>, Data)),
        ?ASSERT_EQUAL(<<"m">>, maps:get(<<"unit">>, Data)),
        ?assert(length(maps:get(<<"list">>, Data)) >= 1)
    end).
