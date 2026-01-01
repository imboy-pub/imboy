-module(user_denylist_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_denylist_handler 模块的 EUnit 测试
%%%
%%% 目标：验证黑名单处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 添加到黑名单测试
%% ===================================================================

%% @doc 测试添加用户到黑名单 - 成功场景
handle_add_to_denylist_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 67890},
                    {<<"reason">>, <<"spam">>}
                ]
            end}
        ]},
        {user_denylist_logic, [
            {'add_to_denylist', 2, fun(_FromUid, _TargetUid) ->
                {ok, #{target_uid => 67890, created_at => imboy_dt:timestamp()}}
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
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{target_uid := 67890}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_denylist_logic, add_to_denylist, 2),
        meck_helper:verify_called(imboy_response, success, 3)
    end).

%% @doc 测试添加用户到黑名单 - 目标用户不存在
handle_add_to_denylist_user_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 99999},
                    {<<"reason">>, <<"spam">>}
                ]
            end}
        ]},
        {user_denylist_logic, [
            {'add_to_denylist', 2, fun(_FromUid, TargetUid) when TargetUid =:= 99999 ->
                {error, user_not_found}
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
        % 模拟请求（目标用户不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := user_not_found}, Body)
    end).

%% @doc 测试添加用户到黑名单 - 参数缺失
handle_add_to_denylist_missing_params_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                % 缺少 target_uid 参数
                [
                    {<<"reason">>, <<"spam">>}
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
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).

%% ===================================================================
%% 从黑名单移除测试
%% ===================================================================

%% @doc 测试从黑名单移除用户 - 成功场景
handle_remove_from_denylist_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 67890}
                ]
            end}
        ]},
        {user_denylist_logic, [
            {'remove_from_denylist', 2, fun(_FromUid, _TargetUid) ->
                {ok, #{target_uid => 67890, removed_at => imboy_dt:timestamp()}}
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
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{action => remove}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{target_uid := 67890}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_denylist_logic, remove_from_denylist, 2)
    end).

%% @doc 测试从黑名单移除用户 - 用户不在黑名单中
handle_remove_from_denylist_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 67890}
                ]
            end}
        ]},
        {user_denylist_logic, [
            {'remove_from_denylist', 2, fun(_FromUid, _TargetUid) ->
                {error, not_in_denylist}
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
        % 模拟请求（用户不在黑名单中）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{action => remove}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := not_in_denylist}, Body)
    end).

%% ===================================================================
%% 查看黑名单列表测试
%% ===================================================================

%% @doc 测试查看黑名单列表 - 成功场景
handle_list_denylist_test_() ->
    ?WITH_MECKS([
        {user_denylist_logic, [
            {'list_denylist', 1, fun(_FromUid) ->
                {ok, [
                    #{target_uid => 67890, reason => <<"spam">>, created_at => imboy_dt:timestamp()},
                    #{target_uid => 67891, reason => <<"harassment">>, created_at => imboy_dt:timestamp()}
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
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := [_|_]}, Body), % 非空列表
        
        % 验证返回数据结构
        #{data := List} = Body,
        ?assert(length(List) >= 1),
        lists:foreach(fun(Item) ->
            ?ASSERT_MATCH(#{target_uid := _, reason := _, created_at := _}, Item)
        end, List),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_denylist_logic, list_denylist, 1)
    end).

%% @doc 测试查看黑名单列表 - 空列表
handle_list_denylist_empty_test_() ->
    ?WITH_MECKS([
        {user_denylist_logic, [
            {'list_denylist', 1, fun(_FromUid) ->
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
        % 模拟请求
        MockReq = meck_helper:test_request(#{
            method => <<"GET">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_denylist_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := []}, Body)
    end).
