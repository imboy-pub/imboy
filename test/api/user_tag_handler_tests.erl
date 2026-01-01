-module(user_tag_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_tag_handler 模块的 EUnit 测试
%%%
%%% 目标：验证用户标签处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 创建用户标签测试
%% ===================================================================

%% @doc 测试创建用户标签 - 成功场景
handle_create_tag_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"name">>, <<"重要联系人">>},
                    {<<"color">>, <<"#FF5722">>},
                    {<<"description">>, <<"重要的联系人标签">>}
                ]
            end}
        ]},
        {user_tag_logic, [
            {'create_tag', 4, fun(_Uid, _Name, _Color, _Description) ->
                {ok, #{
                    tag_id => 12345,
                    uid => 12345,
                    name => <<"重要联系人">>,
                    color => <<"#FF5722">>,
                    description => <<"重要的联系人标签">>,
                    user_count => 0,
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
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => create}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{tag_id := 12345}}, Body),
        
        % 验证具体返回值
        #{data := Tag} = Body,
        ?ASSERT_EQUAL(<<"重要联系人">>, maps:get(<<"name">>, Tag)),
        ?ASSERT_EQUAL(0, maps:get(<<"user_count">>, Tag)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_logic, create_tag, 4),
        meck_helper:verify_called(imboy_response, success, 3)
    end).

%% @doc 测试创建用户标签 - 标签名称已存在
handle_create_tag_name_exists_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"name">>, <<"重要联系人">>},
                    {<<"color">>, <<"#FF5722">>}
                ]
            end}
        ]},
        {user_tag_logic, [
            {'create_tag', 4, fun(_Uid, Name, _Color, _Description) when Name =:= <<"重要联系人">> ->
                {error, tag_name_exists}
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
        % 模拟请求（标签名称已存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => create}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(409, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := tag_name_exists}, Body)
    end).

%% ===================================================================
%% 更新用户标签测试
%% ===================================================================

%% @doc 测试更新用户标签 - 成功场景
handle_update_tag_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"tag_id">>, 12345},
                    {<<"name">>, <<"重要联系人更新">>},
                    {<<"color">>, <<"#2196F3">>},
                    {<<"description">>, <<"更新后的描述">>}
                ]
            end}
        ]},
        {user_tag_logic, [
            {'update_tag', 5, fun(_Uid, _TagId, _Name, _Color, _Description) ->
                {ok, #{
                    tag_id => 12345,
                    uid => 12345,
                    name => <<"重要联系人更新">>,
                    color => <<"#2196F3">>,
                    description => <<"更新后的描述">>,
                    user_count => 5,
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
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{tag_id := 12345}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_logic, update_tag, 5)
    end).

%% @doc 测试更新用户标签 - 标签不存在
handle_update_tag_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"tag_id">>, 99999},
                    {<<"name">>, <<"不存在的标签">>}
                ]
            end}
        ]},
        {user_tag_logic, [
            {'update_tag', 5, fun(_Uid, TagId, _Name, _Color, _Description) when TagId =:= 99999 ->
                {error, tag_not_found}
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
        % 模拟请求（标签不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := tag_not_found}, Body)
    end).

%% ===================================================================
%% 删除用户标签测试
%% ===================================================================

%% @doc 测试删除用户标签 - 成功场景
handle_delete_tag_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"tag_id">>, 12345}
                ]
            end}
        ]},
        {user_tag_logic, [
            {'delete_tag', 2, fun(_Uid, _TagId) ->
                {ok, #{
                    deleted_tag_id => 12345,
                    affected_users => 5,  % 受影响的用户数量
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
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => delete}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{deleted_tag_id := 12345}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_logic, delete_tag, 2)
    end).

%% @doc 测试删除用户标签 - 标签不存在
handle_delete_tag_not_found_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"tag_id">>, 99999}
                ]
            end}
        ]},
        {user_tag_logic, [
            {'delete_tag', 2, fun(_Uid, TagId) when TagId =:= 99999 ->
                {error, tag_not_found}
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
        % 模拟请求（标签不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => delete}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := tag_not_found}, Body)
    end).

%% ===================================================================
%% 查看用户标签列表测试
%% ===================================================================

%% @doc 测试查看用户标签列表 - 成功场景
handle_list_tags_test_() ->
    ?WITH_MECKS([
        {user_tag_logic, [
            {'list_tags', 1, fun(_Uid) ->
                {ok, [
                    #{
                        tag_id => 12345,
                        name => <<"重要联系人">>,
                        color => <<"#FF5722">>,
                        user_count => 5,
                        created_at => imboy_dt:timestamp()
                    },
                    #{
                        tag_id => 12346,
                        name => <<"同事">>,
                        color => <<"#4CAF50">>,
                        user_count => 10,
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
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := [_|_]}, Body), % 非空列表
        
        % 验证返回数据结构
        #{data := List} = Body,
        ?assert(length(List) >= 1),
        lists:foreach(fun(Tag) ->
            ?ASSERT_MATCH(#{tag_id := _, name := _, user_count := _}, Tag)
        end, List),
        
        % 验证 Mock 调用
        meck_helper:verify_called(user_tag_logic, list_tags, 1)
    end).

%% @doc 测试查看用户标签列表 - 空列表
handle_list_tags_empty_test_() ->
    ?WITH_MECKS([
        {user_tag_logic, [
            {'list_tags', 1, fun(_Uid) ->
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
        {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := []}, Body)
    end).
