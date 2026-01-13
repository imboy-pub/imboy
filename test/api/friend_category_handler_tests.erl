-module(friend_category_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_category_handler 模块的 EUnit 测试
%%%
%%% 目标：验证好友分类处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 创建好友分类测试
%% ===================================================================

%% @doc 测试创建好友分类 - 成功场景
handle_create_category_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"name">>, <<"工作">>},
                    {<<"description">>, <<"工作相关的好友">>},
                    {<<"color">>, <<"#FF5722">>}
                ]
            end}
        ]},
        {friend_category_logic, [
            {'create_category', 4, fun(_Uid, _Name, _Description, _Color) ->
                {ok, #{
                    category_id => 12345,
                    uid => 12345,
                    name => <<"工作">>,
                    description => <<"工作相关的好友">>,
                    color => <<"#FF5722">>,
                    friend_count => 0,
                    created_at => elib_dt:timestamp()
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
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => create}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{category_id := 12345}}, Body),
        
        % 验证具体返回值
        #{data := Category} = Body,
        ?ASSERT_EQUAL(<<"工作">>, maps:get(<<"name">>, Category)),
        ?ASSERT_EQUAL(0, maps:get(<<"friend_count">>, Category)),
        
        % 验证 Mock 调用
        meck_helper:verify_called(friend_category_logic, create_category, 4),
        meck_helper:verify_called(elib_response, success, 3)
    end).

%% @doc 测试创建好友分类 - 分类名称已存在
handle_create_category_name_exists_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"name">>, <<"工作">>},
                    {<<"description">>, <<"工作相关的好友">>},
                    {<<"color">>, <<"#FF5722">>}
                ]
            end}
        ]},
        {friend_category_logic, [
            {'create_category', 4, fun(_Uid, Name, _Description, _Color) when Name =:= <<"工作">> ->
                {error, category_name_exists}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Message) ->
                cowboy_req_h:new(#{
                    response_status => 409,
                    response_body => #{status => error, message => Message}
                })
            end}
        ]}
    ], fun() ->
        % 模拟请求（分类名称已存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => create}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(409, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := category_name_exists}, Body)
    end).

%% @doc 测试创建好友分类 - 参数缺失
handle_create_category_missing_params_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                % 缺少必要参数
                [
                    {<<"description">>, <<"工作相关的好友">>}
                ]
            end}
        ]},
        {elib_response, [
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
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => create}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).

%% ===================================================================
%% 更新好友分类测试
%% ===================================================================

%% @doc 测试更新好友分类 - 成功场景
handle_update_category_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"category_id">>, 12345},
                    {<<"name">>, <<"工作更新">>},
                    {<<"description">>, <<"更新后的工作相关好友">>},
                    {<<"color">>, <<"#2196F3">>}
                ]
            end}
        ]},
        {friend_category_logic, [
            {'update_category', 5, fun(_Uid, _CategoryId, _Name, _Description, _Color) ->
                {ok, #{
                    category_id => 12345,
                    uid => 12345,
                    name => <<"工作更新">>,
                    description => <<"更新后的工作相关好友">>,
                    color => <<"#2196F3">>,
                    friend_count => 5,
                    updated_at => elib_dt:timestamp()
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
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{category_id := 12345}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(friend_category_logic, update_category, 5)
    end).

%% @doc 测试更新好友分类 - 分类不存在
handle_update_category_not_found_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"category_id">>, 99999},
                    {<<"name">>, <<"不存在的分类">>}
                ]
            end}
        ]},
        {friend_category_logic, [
            {'update_category', 5, fun(_Uid, CategoryId, _Name, _Description, _Color) when CategoryId =:= 99999 ->
                {error, category_not_found}
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
        % 模拟请求（分类不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => update}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := category_not_found}, Body)
    end).

%% ===================================================================
%% 删除好友分类测试
%% ===================================================================

%% @doc 测试删除好友分类 - 成功场景
handle_delete_category_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"category_id">>, 12345}
                ]
            end}
        ]},
        {friend_category_logic, [
            {'delete_category', 2, fun(_Uid, _CategoryId) ->
                {ok, #{
                    deleted_category_id => 12345,
                    moved_friends_count => 5,  % 移动到默认分类的好友数量
                    deleted_at => elib_dt:timestamp()
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
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => delete}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{deleted_category_id := 12345}}, Body),
        
        % 验证 Mock 调用
        meck_helper:verify_called(friend_category_logic, delete_category, 2)
    end).

%% @doc 测试删除好友分类 - 分类不存在
handle_delete_category_not_found_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"category_id">>, 99999}
                ]
            end}
        ]},
        {friend_category_logic, [
            {'delete_category', 2, fun(_Uid, CategoryId) when CategoryId =:= 99999 ->
                {error, category_not_found}
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
        % 模拟请求（分类不存在）
        MockReq = meck_helper:test_request(#{
            method => <<"POST">>,
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => delete}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(404, StatusCode),
        ?ASSERT_MATCH(#{status := error, message := category_not_found}, Body)
    end).

%% ===================================================================
%% 查看好友分类列表测试
%% ===================================================================

%% @doc 测试查看好友分类列表 - 成功场景
handle_list_categories_test_() ->
    ?WITH_MECKS([
        {friend_category_logic, [
            {'list_categories', 1, fun(_Uid) ->
                {ok, [
                    #{
                        category_id => 12345,
                        name => <<"工作">>,
                        description => <<"工作相关的好友">>,
                        color => <<"#FF5722">>,
                        friend_count => 5,
                        created_at => elib_dt:timestamp()
                    },
                    #{
                        category_id => 12346,
                        name => <<"生活">>,
                        description => <<"生活中的好友">>,
                        color => <<"#4CAF50">>,
                        friend_count => 10,
                        created_at => elib_dt:timestamp()
                    }
                ]}
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
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := [_|_]}, Body), % 非空列表
        
        % 验证返回数据结构
        #{data := List} = Body,
        ?assert(length(List) >= 1),
        lists:foreach(fun(Category) ->
            ?ASSERT_MATCH(#{category_id := _, name := _, friend_count := _}, Category)
        end, List),
        
        % 验证 Mock 调用
        meck_helper:verify_called(friend_category_logic, list_categories, 1)
    end).

%% @doc 测试查看好友分类列表 - 空列表
handle_list_categories_empty_test_() ->
    ?WITH_MECKS([
        {friend_category_logic, [
            {'list_categories', 1, fun(_Uid) ->
                {ok, []}
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
            headers => #{<<"imboy-uid">> => <<"12345">>}
        }),
        
        % 调用 handler
        {ok, Req, _State} = friend_category_handler:init(MockReq, #{action => list}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := []}, Body)
    end).
