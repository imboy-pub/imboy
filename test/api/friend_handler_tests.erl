-module(friend_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_handler 模块的 EUnit 测试
%%%
%%% 目标：验证好友处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 添加好友测试
%% ===================================================================

%% @doc 测试添加好友 - 成功场景
handle_add_friend_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 67890},
                    {<<"message">>, <<"hello, let's be friends">>}
                ]
            end}
        ]},
        {friend_logic, [
            {'add_friend', 2, fun(_FromUid, _TargetUid) ->
                {ok, #{
                    friend_id => 12345,
                    from_uid => 12345,
                    to_uid => 67890,
                    status => pending,
                    created_at => imboy_dt:timestamp()
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
        {ok, Req, _State} = friend_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{friend_id := 12345}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(friend_logic, add_friend, 2),
        meck_helper:verify_called(imboy_response, success, 3)
    end).

%% @doc 测试添加好友 - 目标用户不存在
handle_add_friend_user_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 99999},
                    {<<"message">>, <<"hello">>}
                ]
            end}
        ]},
        {friend_logic, [
            {'add_friend', 2, fun(_FromUid, TargetUid) when TargetUid =:= 99999 ->
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
        {ok, Req, _State} = friend_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := user_not_found}, Body)
    end).

%% @doc 测试添加好友 - 已经是好友
handle_add_friend_already_friends_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 67890},
                    {<<"message">>, <<"hello">>}
                ]
            end}
        ]},
        {friend_logic, [
            {'add_friend', 2, fun(_FromUid, _TargetUid) ->
                {error, already_friends}
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
        % 模拟请求（已经是好友）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(409, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := already_friends}, Body)
    end).

%% ===================================================================
%% 删除好友测试
%% ===================================================================

%% @doc 测试删除好友 - 成功场景
handle_delete_friend_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 67890}
                ]
            end}
        ]},
        {friend_logic, [
            {'delete_friend', 2, fun(_FromUid, _TargetUid) ->
                {ok, #{
                    deleted_friend_id => 12345,
                    from_uid => 12345,
                    to_uid => 67890,
                    deleted_at => imboy_dt:timestamp()
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
        {ok, Req, _State} = friend_handler:init(MockReq, #{action => delete}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{deleted_friend_id := 12345}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(friend_logic, delete_friend, 2)
    end).

%% @doc 测试删除好友 - 好友关系不存在
handle_delete_friend_not_friends_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"target_uid">>, 67890}
                ]
            end}
        ]},
        {friend_logic, [
            {'delete_friend', 2, fun(_FromUid, _TargetUid) ->
                {error, not_friends}
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
        % 模拟请求（好友关系不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_handler:init(MockReq, #{action => delete}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := not_friends}, Body)
    end).

%% ===================================================================
%% 查看好友列表测试
%% ===================================================================

%% @doc 测试查看好友列表 - 成功场景
handle_list_friends_test_() ->
    ?WITH_MECKS([
        {friend_logic, [
            {'list_friends', 1, fun(_FromUid) ->
                {ok, [
                    #{
                        friend_id => 12345,
                        friend_uid => 67890,
                        nickname => <<"Friend 1">>,
                        status => active,
                        created_at => imboy_dt:timestamp()
                    },
                    #{
                        friend_id => 12346,
                        friend_uid => 67891,
                        nickname => <<"Friend 2">>,
                        status => active,
                        created_at => imboy_dt:timestamp()
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
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := [_|_]}, Body), % 非空列表
        
        % 验证返回数据结构
        #{data := List} = Body,
        ?assert(length(List) >= 1),
        lists:foreach(fun(Friend) ->
            ?ASSERT_MATCH(#{friend_id := _, friend_uid := _, nickname := _}, Friend)
        end, List),
        
        % 验证 Mock 调用
        meck_helper:verify_called(friend_logic, list_friends, 1)
    end).

%% @doc 测试查看好友列表 - 空列表
handle_list_friends_empty_test_() ->
    ?WITH_MECKS([
        {friend_logic, [
            {'list_friends', 1, fun(_FromUid) ->
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
        {ok, Req, _State} = friend_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := []}, Body)
    end).
