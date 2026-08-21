-module(channel_discovery_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc channel_discovery_handler 的 API 层单元测试
%%% 覆盖：search、discover、featured、trending、categories
%%%===================================================================

%% ===================================================================
%% search 端点
%% ===================================================================

handle_search_returns_paginated_results_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"q">>, <<"test">>}] end}
            ]},
            {elib_param, [
                {'int', 3, fun
                    (page, _Req, 1) -> {ok, 1};
                    (size, _Req, 20) -> {ok, 20}
                end}
            ]},
            {channel_discovery_logic, [
                {'search', 4, fun(<<"test">>, 1, 20, undefined) ->
                    {ok, #{<<"list">> => [#{<<"id">> => 1}], <<"total">> => 1}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = channel_discovery_handler:init(MockReq, #{action => search}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode),
            ?ASSERT_EQUAL(1, maps:get(<<"total">>, Body))
        end
    ).

%% ===================================================================
%% discover 端点
%% ===================================================================

handle_discover_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_param, [
                {'int', 3, fun
                    (page, _Req, 1) -> {ok, 1};
                    (size, _Req, 20) -> {ok, 20}
                end}
            ]},
            {channel_discovery_logic, [
                {'discover', 4, fun(1, 20, undefined, <<"popular">>) ->
                    {ok, #{<<"list">> => [#{<<"id">> => 1}]}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _} = channel_discovery_handler:init(MockReq, #{action => discover}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% ===================================================================
%% featured 端点
%% ===================================================================

handle_featured_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(limit, _Req, 10) -> {ok, 10} end}
            ]},
            {channel_discovery_logic, [
                {'featured', 1, fun(10) ->
                    {ok, #{<<"list">> => [#{<<"id">> => 1}]}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _} = channel_discovery_handler:init(MockReq, #{action => featured}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% ===================================================================
%% trending 端点
%% ===================================================================

handle_trending_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_param, [
                {'int', 3, fun(limit, _Req, 20) -> {ok, 20} end}
            ]},
            {channel_discovery_logic, [
                {'trending', 2, fun(7, 20) ->
                    {ok, #{<<"list">> => [#{<<"id">> => 1}]}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _} = channel_discovery_handler:init(MockReq, #{action => trending}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% trending 支持 period 参数
handle_trending_with_30d_period_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"period">>, <<"30d">>}] end}
            ]},
            {elib_param, [
                {'int', 3, fun(limit, _Req, 20) -> {ok, 20} end}
            ]},
            {channel_discovery_logic, [
                {'trending', 2, fun(30, 20) ->
                    {ok, #{<<"list">> => []}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _} = channel_discovery_handler:init(MockReq, #{action => trending}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% ===================================================================
%% categories 端点
%% ===================================================================

handle_categories_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {channel_discovery_logic, [
                {'categories', 0, fun() ->
                    {ok, #{<<"list">> => [#{<<"id">> => 1, <<"name">> => <<"科技"/utf8>>}]}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _} = channel_discovery_handler:init(MockReq, #{action => categories}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).
