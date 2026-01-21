-module(user_collect_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_collect_handler 模块的 EUnit 测试
%%%
%%% 目标：验证用户收藏处理器功能
%%% 覆盖：收藏分页列表、添加收藏、删除收藏、修改收藏
%%%===================================================================

%% ===================================================================
%% 收藏分页列表测试
%% ===================================================================

%% @doc 测试收藏分页列表 - 成功场景
handle_page_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(State) ->
                maps:get(current_uid, State, 12345)
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end},
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 0}  % kind=0 (全部)
            end}
        ]},
        {user_collect_repo, [
            {'tablename', 0, fun() ->
                <<"user_collect">>
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Table, _Column, _Where, _Order, _Page, _Size) ->
                {ok, #{
                    total => 2,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            kind => 1,
                            kind_id => 12345,
                            source => <<"Alice">>,
                            created_at => <<"2026-01-20 10:00:00">>,
                            updated_at => <<"2026-01-20 10:00:00">>,
                            tag => <<"important">>,
                            info => <<"{\"text\": \"Hello\"}"/utf8>>
                        },
                        #{
                            kind => 2,
                            kind_id => 67890,
                            source => <<"Bob">>,
                            created_at => <<"2026-01-19 15:30:00">>,
                            updated_at => <<"2026-01-19 15:30:00">>,
                            tag => <<"work">>,
                            info => <<"{\"url\": \"https://example.com\"}">>
                        }
                    ]
                }}
            end}
        ]},
        {elib_hasher, [
            {'decoded_field', 1, fun(Field) ->
                Field  % 返回原始字段名
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end},
            {'json_decode_list_field', 2, fun(List, _Field) ->
                % 解码 info 字段
                lists:map(fun(Item) ->
                    case maps:get(info, Item) of
                        <<"{", _/binary>> -> Item#{info => jsone:decode(maps:get(info, Item))};
                        _ -> Item
                    end
                end, List)
            end}
        ]}
    ], fun() ->
        % 模拟请求
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(elib_pg, page_with_total, 6)
    end).

%% @doc 测试收藏分页列表 - 按类型过滤
handle_page_with_kind_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end},
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 1}  % kind=1 (文本)
            end}
        ]},
        {user_collect_repo, [
            {'tablename', 0, fun() ->
                <<"user_collect">>
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Table, _Column, Where, _Order, _Page, _Size) ->
                % 验证 WHERE 包含 kind 过滤
                ?assert(maps:is_key(kind, Where)),
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            kind => 1,
                            kind_id => 12345,
                            source => <<"Alice">>,
                            created_at => <<"2026-01-20 10:00:00">>,
                            updated_at => <<"2026-01-20 10:00:00">>,
                            tag => <<>>,
                            info => <<"{\"text\": \"Hello\"}"/utf8>>
                        }
                    ]
                }}
            end}
        ]},
        {elib_hasher, [
            {'decoded_field', 1, fun(Field) ->
                Field
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end},
            {'json_decode_list_field', 2, fun(List, _Field) ->
                List
            end}
        ]}
    ], fun() ->
        % 模拟请求（按类型过滤）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=1&size=20&kind=1">>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试收藏分页列表 - 按标签过滤
handle_page_with_tag_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end},
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 0}
            end}
        ]},
        {user_collect_repo, [
            {'tablename', 0, fun() ->
                <<"user_collect">>
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Table, _Column, Where, _Order, _Page, _Size) ->
                % 验证 WHERE 包含标签过滤
                ?assert(maps:is_key(tag, Where)),
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            kind => 1,
                            kind_id => 12345,
                            source => <<"Alice">>,
                            created_at => <<"2026-01-20 10:00:00">>,
                            updated_at => <<"2026-01-20 10:00:00">>,
                            tag => <<"important">>,
                            info => <<"{}">>
                        }
                    ]
                }}
            end}
        ]},
        {elib_hasher, [
            {'decoded_field', 1, fun(Field) ->
                Field
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end},
            {'json_decode_list_field', 2, fun(List, _Field) ->
                List
            end}
        ]}
    ], fun() ->
        % 模拟请求（按标签过滤）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=1&size=20&tag=important">>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试收藏分页列表 - 按最近使用排序
handle_page_recent_order_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end},
            {'int', 3, fun(_Key, _Req, _Default) ->
                {ok, 0}
            end}
        ]},
        {user_collect_repo, [
            {'tablename', 0, fun() ->
                <<"user_collect">>
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Table, _Column, _Where, Order, _Page, _Size) ->
                % 验证排序方式
                ?ASSERT_EQUAL(<<"updated_at desc, id desc">>, Order),
                {ok, #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            kind => 1,
                            kind_id => 12345,
                            source => <<"Alice">>,
                            created_at => <<"2026-01-20 10:00:00">>,
                            updated_at => <<"2026-01-20 12:00:00">>,
                            tag => <<>>,
                            info => <<"{}">>
                        }
                    ]
                }}
            end}
        ]},
        {elib_hasher, [
            {'decoded_field', 1, fun(Field) ->
                Field
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Data) ->
                cowboy_req_h:new(#{
                    response_status => 200,
                    response_body => #{status => success, data => Data}
                })
            end},
            {'json_decode_list_field', 2, fun(List, _Field) ->
                List
            end}
        ]}
    ], fun() ->
        % 模拟请求（按最近使用排序）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"page=1&size=20&order=recent_use">>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% ===================================================================
%% 添加收藏测试
%% ===================================================================

%% @doc 测试添加收藏 - 成功场景
handle_add_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"kind">>, <<"1">>},
                    {<<"kind_id">>, <<"12345">>},
                    {<<"source">>, <<"Alice">>},
                    {<<"remark">>, <<"Important message">>},
                    {<<"info">>, #{text => <<"Hello">>}}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'add', 6, fun(_Uid, _Kind, _KindId, _Info, _Source, _Remark) ->
                {ok, #{id => 3001, created => true}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Data) ->
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
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(user_collect_logic, add, 6)
    end).

%% @doc 测试添加收藏 - 错误场景
handle_add_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"kind">>, <<"1">>},
                    {<<"kind_id">>, <<"12345">>},
                    {<<"source">>, <<>>},
                    {<<"remark">>, <<>>},
                    {<<"info">>, #{text => <<"Hello">>}}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'add', 6, fun(_Uid, _Kind, _KindId, _Info, _Source, _Remark) ->
                {error, <<"收藏已存在"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Data) ->
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
        % 模拟请求（收藏已存在）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode)
    end).

%% ===================================================================
%% 删除收藏测试
%% ===================================================================

%% @doc 测试删除收藏 - 成功场景
handle_remove_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"kind_id">>, <<"12345">>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'remove', 2, fun(_Uid, _KindId) ->
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
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(user_collect_logic, remove, 2)
    end).

%% @doc 测试删除收藏 - 空kind_id
handle_remove_empty_kind_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"kind_id">>, <<>>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'remove', 2, fun(_Uid, _KindId) ->
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
        % 模拟请求（空kind_id）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% ===================================================================
%% 修改收藏测试
%% ===================================================================

%% @doc 测试修改收藏 - 成功场景
handle_change_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"action">>, <<"update_tag">>},
                    {<<"kind_id">>, <<"12345">>},
                    {<<"tag">>, <<"important">>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'change', 4, fun(_Uid, _Action, _KindId, _PostVals) ->
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
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(user_collect_logic, change, 4)
    end).

%% @doc 测试修改收藏 - 不同action
handle_change_different_action_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"action">>, <<"update_remark">>},
                    {<<"kind_id">>, <<"12345">>},
                    {<<"remark">>, <<"Updated remark">>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'change', 4, fun(_Uid, Action, _KindId, _PostVals) ->
                ?ASSERT_EQUAL(<<"update_remark">>, Action),
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
        % 模拟请求（不同action）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试修改收藏 - 空action
handle_change_empty_action_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                [
                    {<<"action">>, <<>>},
                    {<<"kind_id">>, <<"12345">>}
                ]
            end}
        ]},
        {user_collect_logic, [
            {'change', 4, fun(_Uid, Action, _KindId, _PostVals) ->
                ?ASSERT_EQUAL(<<>>, Action),
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
        % 模拟请求（空action）
        MockReq = cowboy_req_h:new(#{
            method => <<"POST">>,
            qs => <<>>
        }),

        % 调用 handler
        {ok, Req, _State} = user_collect_handler:init(MockReq, #{
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).
