-module(fts_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% fts_handler 模块的 EUnit 测试
%%%
%%% 目标：验证全文搜索处理器功能
%%% 改进：使用 meck_helper 进行 Mock 管理，添加实际业务逻辑测试
%%%===================================================================

%% ===================================================================
%% 全文搜索测试
%% ===================================================================

%% @doc 测试全文搜索 - 成功场景
handle_search_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'get', 1, fun(_Req) ->
                [
                    {<<"query">>, <<"hello world">>},
                    {<<"type">>, <<"user">>},
                    {<<"limit">>, 20},
                    {<<"offset">>, 0}
                ]
            end}
        ]},
        {fts_logic, [
            {'search', 5, fun(_Uid, _Query, _Type, _Limit, _Offset) ->
                {ok, #{
                    results => [
                        #{
                            id => 12345,
                            type => user,
                            title => <<"Hello User">>,
                            content => <<"This is a hello world message">>,
                            score => 0.95,
                            highlighted => <<"<em>Hello</em> <em>world</em> message">>
                        },
                        #{
                            id => 12346,
                            type => user,
                            title => <<"World Hello">>,
                            content => <<"Another hello world content">>,
                            score => 0.87,
                            highlighted => <<"Another <em>hello</em> <em>world</em> content">>
                        }
                    ],
                    total => 2,
                    query => <<"hello world">>,
                    took => 5
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
            method => <<"GET">>,
            headers => #{<<"imboy-uid">> => <<"12345">>},
            qs => <<"query=hello+world&type=user&limit=20&offset=0">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{action => search}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{results := [_|_]}}, Body),
        
        % 验证具体返回值
        #{data := SearchData} = Body,
        ?ASSERT_EQUAL(2, maps:get(<<"total">>, SearchData)),
        ?ASSERT_EQUAL(<<"hello world">>, maps:get(<<"query">>, SearchData)),
        
        % 验证搜索结果
        #{results := Results} = SearchData,
        ?assert(length(Results) >= 1),
        lists:foreach(fun(Result) ->
            ?ASSERT_MATCH(#{id := _, type := _, score := _}, Result),
            Score = maps:get(<<"score">>, Result),
            ?assert(Score > 0 andalso Score =< 1.0, "Expected score between 0 and 1")
        end, Results),
        
        % 验证 Mock 调用
        meck_helper:verify_called(fts_logic, search, 5),
        meck_helper:verify_called(imboy_response, success, 3)
    end).

%% @doc 测试全文搜索 - 空结果
handle_search_empty_results_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'get', 1, fun(_Req) ->
                [
                    {<<"query">>, <<"nonexistent_query">>},
                    {<<"type">>, <<"user">>},
                    {<<"limit">>, 20},
                    {<<"offset">>, 0}
                ]
            end}
        ]},
        {fts_logic, [
            {'search', 5, fun(_Uid, _Query, _Type, _Limit, _Offset) ->
                {ok, #{
                    results => [],
                    total => 0,
                    query => <<"nonexistent_query">>,
                    took => 2
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
        % 模拟请求（无结果）
        MockReq = meck_helper:test_request(#{
            method => <<"GET">>,
            headers => #{<<"imboy-uid">> => <<"12345">>},
            qs => <<"query=nonexistent_query&type=user&limit=20&offset=0">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{action => search}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(200, StatusCode),
        ?ASSERT_MATCH(#{status := success, data := #{results := []}}, Body),
        
        % 验证空结果
        #{data := SearchData} = Body,
        ?ASSERT_EQUAL(0, maps:get(<<"total">>, SearchData)),
        ?ASSERT_EQUAL([], maps:get(<<"results">>, SearchData))
    end).

%% @doc 测试全文搜索 - 查询参数缺失
handle_search_missing_params_test_() ->
    ?WITH_MECKS([
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
            method => <<"GET">>,
            headers => #{<<"imboy-uid">> => <<"12345">>},
            qs => <<"type=user">>  % 缺少 query 参数
        }),
        
        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{action => search}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).

%% @doc 测试全文搜索 - 查询字符串过短
handle_search_query_too_short_test_() ->
    ?WITH_MECKS([
        {imboy_param, [
            {'get', 1, fun(_Req) ->
                [
                    {<<"query">>, <<"hi">>},  % 查询字符串太短
                    {<<"type">>, <<"user">>},
                    {<<"limit">>, 20},
                    {<<"offset">>, 0}
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
        % 模拟请求（查询字符串过短）
        MockReq = meck_helper:test_request(#{
            method => <<"GET">>,
            headers => #{<<"imboy-uid">> => <<"12345">>},
            qs => <<"query=hi&type=user&limit=20&offset=0">>
        }),
        
        % 调用 handler
        {ok, Req, _State} = fts_handler:init(MockReq, #{action => search}),
        
        % 验证响应
        {StatusCode, _, Body} = cowboy_req_h:response(Req),
        ?ASSERT_EQUAL(400, StatusCode),
        ?ASSERT_MATCH(#{status := error}, Body)
    end).
