-module(fts_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_handler 模块的 EUnit 测试
%%%
%%% 目标：验证全文搜索处理器功能
%%% 覆盖：用户搜索、最近用户搜索、分页处理
%%%===================================================================

%% ===================================================================
%% 用户搜索测试
%% ===================================================================

%% @doc 测试用户搜索 - 成功场景
handle_user_search_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {fts_logic, [
            {'user_search_page', 4, fun(_Uid, _Page, _Size, _Keyword) ->
                #{
                    total => 2,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            uid => 67890,
                            nickname => <<"Alice">>,
                            avatar => <<"https://example.com/avatar1.jpg">>
                        },
                        #{
                            uid => 67891,
                            nickname => <<"Bob">>,
                            avatar => <<"https://example.com/avatar2.jpg">>
                        }
                    ]
                }
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
            qs => <<"keyword=alice&page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{
            action => user_search,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(fts_logic, user_search_page, 4),
        meck_helper:verify_called(elib_response, success, 2)
    end).

%% @doc 测试用户搜索 - 空结果
handle_user_search_empty_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {fts_logic, [
            {'user_search_page', 4, fun(_Uid, _Page, _Size, _Keyword) ->
                #{
                    total => 0,
                    page => 1,
                    size => 20,
                    list => []
                }
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
        % 模拟请求（空结果）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"keyword=nonexistent&page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{
            action => user_search,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试用户搜索 - 分页处理
handle_user_search_pagination_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {2, 10}  % 第2页，每页10条
            end}
        ]},
        {fts_logic, [
            {'user_search_page', 4, fun(_Uid, Page, Size, _Keyword) ->
                ?ASSERT_EQUAL(2, Page),
                ?ASSERT_EQUAL(10, Size),
                #{
                    total => 25,
                    page => Page,
                    size => Size,
                    list => [
                        #{
                            uid => 67892,
                            nickname => <<"Charlie">>,
                            avatar => <<"https://example.com/avatar3.jpg">>
                        }
                    ]
                }
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
        % 模拟请求（分页）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"keyword=test&page=2&size=10">>
        }),

        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{
            action => user_search,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% ===================================================================
%% 最近用户搜索测试
%% ===================================================================

%% @doc 测试最近用户搜索 - 成功场景
handle_recently_user_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {fts_logic, [
            {'recently_user_page', 4, fun(_Uid, _Page, _Size, _Keyword) ->
                #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            uid => 67893,
                            nickname => <<"New User">>,
                            avatar => <<"https://example.com/avatar4.jpg">>,
                            created_at => <<"2026-01-20 10:00:00">>
                        }
                    ]
                }
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
            qs => <<"keyword=new&page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{
            action => recently_user,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),

        % 验证 Mock 调用
        meck_helper:verify_called(fts_logic, recently_user_page, 4)
    end).

%% @doc 测试最近用户搜索 - 空关键词
handle_recently_user_empty_keyword_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {fts_logic, [
            {'recently_user_page', 4, fun(_Uid, _Page, _Size, _Keyword) ->
                ?ASSERT_EQUAL(<<>>, _Keyword),
                #{
                    total => 3,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            uid => 67894,
                            nickname => <<"Recent User 1">>,
                            avatar => <<"https://example.com/avatar5.jpg">>
                        },
                        #{
                            uid => 67895,
                            nickname => <<"Recent User 2">>,
                            avatar => <<"https://example.com/avatar6.jpg">>
                        },
                        #{
                            uid => 67896,
                            nickname => <<"Recent User 3">>,
                            avatar => <<"https://example.com/avatar7.jpg">>
                        }
                    ]
                }
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
        % 模拟请求（空关键词）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"keyword=&page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{
            action => recently_user,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% @doc 测试最近用户搜索 - 无结果
handle_recently_user_no_results_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {fts_logic, [
            {'recently_user_page', 4, fun(_Uid, _Page, _Size, _Keyword) ->
                #{
                    total => 0,
                    page => 1,
                    size => 20,
                    list => []
                }
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
        % 模拟请求（无结果）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"keyword=old&page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{
            action => recently_user,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).

%% ===================================================================
%% 边界情况测试
%% ===================================================================

%% @doc 测试特殊字符关键词
handle_special_keyword_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {fts_logic, [
            {'user_search_page', 4, fun(_Uid, _Page, _Size, Keyword) ->
                % 验证特殊字符被正确传递
                ?ASSERT_EQUAL(<<"中文测试">>, Keyword),
                #{
                    total => 1,
                    page => 1,
                    size => 20,
                    list => [
                        #{
                            uid => 67897,
                            nickname => <<"中文名">>,
                            avatar => <<"https://example.com/avatar8.jpg">>
                        }
                    ]
                }
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
        % 模拟请求（中文关键词）
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"keyword=%E4%B8%AD%E6%96%87%E6%B5%8B%E8%AF%95&page=1&size=20">>
        }),

        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{
            action => user_search,
            current_uid => 12345
        }),

        % 验证响应
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode)
    end).
