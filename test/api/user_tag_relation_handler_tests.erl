-module(user_tag_relation_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_tag_relation_handler 模块的 EUnit 测试
%%%
%%% 目标：验证用户标签关系处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 添加标签到用户测试
%% ===================================================================

%% @doc 测试添加标签到用户 - 成功场景
handle_add_tag_to_user_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"user_id">>, 67890},
                    {<<"tag_id">>, 12345}
                ]
            end}
        ]},
        {user_tag_relation_logic, [
            {'add_tag_to_user', 3, fun(_OperatorUid, _UserId, _TagId) ->
                {ok, #{
                    relation_id => 54321,
                    user_id => 67890,
                    tag_id => 12345,
                    operator_uid => 12345,
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
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{relation_id := 54321}}, Body),
        
        % 验证具体返回值
        #{data := Relation} = Body,
        ?ASSERT_EQUAL(67890, maps:get(<<"user_id">>, Relation)),
        ?ASSERT_EQUAL(12345, maps:get(<<"tag_id">>, Relation)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_relation_logic, add_tag_to_user, 3),
        meck_helper:verify_called(imboy_response, success, 3)
    end).

%% @doc 测试添加标签到用户 - 关系已存在
handle_add_tag_to_user_relation_exists_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"user_id">>, 67890},
                    {<<"tag_id">>, 12345}
                ]
            end}
        ]},
        {user_tag_relation_logic, [
            {'add_tag_to_user', 3, fun(_OperatorUid, _UserId, _TagId) ->
                {error, relation_already_exists}
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
        % 模拟请求（关系已存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(409, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := relation_already_exists}, Body)
    end).

%% @doc 测试添加标签到用户 - 用户不存在
handle_add_tag_to_user_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"user_id">>, 99999},
                    {<<"tag_id">>, 12345}
                ]
            end}
        ]},
        {user_tag_relation_logic, [
            {'add_tag_to_user', 3, fun(_OperatorUid, UserId, _TagId) when UserId =:= 99999 ->
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
        % 模拟请求（用户不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => add}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := user_not_found}, Body)
    end).

%% ===================================================================
%% 从用户移除标签测试
%% ===================================================================

%% @doc 测试从用户移除标签 - 成功场景
handle_remove_tag_from_user_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"user_id">>, 67890},
                    {<<"tag_id">>, 12345}
                ]
            end}
        ]},
        {user_tag_relation_logic, [
            {'remove_tag_from_user', 3, fun(_OperatorUid, _UserId, _TagId) ->
                {ok, #{
                    deleted_relation_id => 54321,
                    user_id => 67890,
                    tag_id => 12345,
                    operator_uid => 12345,
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
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => remove}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{deleted_relation_id := 54321}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_relation_logic, remove_tag_from_user, 3)
    end).

%% @doc 测试从用户移除标签 - 关系不存在
handle_remove_tag_from_user_relation_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"user_id">>, 67890},
                    {<<"tag_id">>, 12345}
                ]
            end}
        ]},
        {user_tag_relation_logic, [
            {'remove_tag_from_user', 3, fun(_OperatorUid, _UserId, _TagId) ->
                {error, relation_not_found}
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
        % 模拟请求（关系不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => remove}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := relation_not_found}, Body)
    end).

%% @doc 测试从用户移除标签 - 参数缺失
handle_remove_tag_from_user_missing_params_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                % 缺少必要参数
                [
                    {<<"user_id">>, 67890}
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
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => remove}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).

%% ===================================================================
%% 查看用户标签关系测试
%% ===================================================================

%% @doc 测试查看用户标签关系 - 成功场景
handle_get_user_tags_test_() ->
    ?WITH_MECKS([
        {user_tag_relation_logic, [
            {'get_user_tags', 2, fun(_OperatorUid, TargetUserId) ->
                {ok, [
                    #{
                        relation_id => 54321,
                        user_id => TargetUserId,
                        tag_id => 12345,
                        tag_name => <<"重要联系人">>,
                        tag_color => <<"#FF5722">>,
                        created_at => imboy_dt:timestamp()
                    },
                    #{
                        relation_id => 54322,
                        user_id => TargetUserId,
                        tag_id => 12346,
                        tag_name => <<"同事">>,
                        tag_color => <<"#4CAF50">>,
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
            headers => #{<<"imboy-uid">> => <<"12345">>},
            qs => <<"user_id=67890">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => get_user_tags}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := [_|_]}, Body), % 非空列表
        
        % 验证返回数据结构
        #{data := List} = Body,
        ?assert(length(List) >= 1),
        lists:foreach(fun(Relation) ->
            ?ASSERT_MATCH(#{relation_id := _, user_id := _, tag_name := _}, Relation)
        end, List),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_relation_logic, get_user_tags, 2)
    end).

%% @doc 测试查看用户标签关系 - 空列表
handle_get_user_tags_empty_test_() ->
    ?WITH_MECKS([
        {user_tag_relation_logic, [
            {'get_user_tags', 2, fun(_OperatorUid, _TargetUserId) ->
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
            headers => #{<<"imboy-uid">> => <<"12345">>},
            qs => <<"user_id=67890">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => get_user_tags}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := []}, Body)
    end).
