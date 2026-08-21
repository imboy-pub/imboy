-module(group_discovery_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc group_discovery_handler 的 API 层单元测试
%%% 覆盖：search、discover、featured、hot、categories、preview
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
            {group_discovery_logic, [
                {'search', 4, fun(<<"test">>, 1, 20, undefined) ->
                    {ok, #{
                        <<"list">> => [#{<<"id">> => 1, <<"title">> => <<"Test">>}],
                        <<"total">> => 1
                    }}
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
            {ok, Req, _State} = group_discovery_handler:init(MockReq, #{action => search}),
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
            {group_discovery_logic, [
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
            {ok, Req, _} = group_discovery_handler:init(MockReq, #{action => discover}),
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
            {group_discovery_logic, [
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
            {ok, Req, _} = group_discovery_handler:init(MockReq, #{action => featured}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% ===================================================================
%% hot 端点
%% ===================================================================

handle_hot_returns_list_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(limit, _Req, 20) -> {ok, 20} end}
            ]},
            {group_discovery_logic, [
                {'hot', 1, fun(20) ->
                    {ok, #{<<"list">> => [#{<<"id">> => 1, <<"title">> => <<"Hot">>}]}}
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
            {ok, Req, _} = group_discovery_handler:init(MockReq, #{action => hot}),
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
            {group_discovery_logic, [
                {'categories', 0, fun() ->
                    {ok, #{<<"list">> => [#{<<"id">> => 1, <<"name">> => <<"技术"/utf8>>}]}}
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
            {ok, Req, _} = group_discovery_handler:init(MockReq, #{action => categories}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% ===================================================================
%% preview 端点
%% ===================================================================

handle_preview_returns_group_info_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"group_id">>, <<"123">>}] end}
            ]},
            {group_discovery_logic, [
                {'preview', 1, fun(123) ->
                    {ok, #{<<"id">> => 123, <<"title">> => <<"Test Group">>}}
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
            {ok, Req, _} = group_discovery_handler:init(MockReq, #{action => preview}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% 预览带无效 group_id 返回错误
handle_preview_with_invalid_id_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    cowboy_req_h:new(#{
                        response_status => 400,
                        response_body => #{<<"error">> => <<"群组ID不能为空"/utf8>>}
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _} = group_discovery_handler:init(MockReq, #{action => preview}),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(400, StatusCode)
        end
    ).
