-module(group_page_contract_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% @doc 验证 join 视图查询条件契约
page_join_query_contract_test_() ->
    ?WITH_MOCKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) ->
                12345
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {2, 20}
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, <<"g.*">>, Where, <<"g.id desc">>, 2, 20) ->
                ?assertEqual(1, maps:get(<<"g.status">>, Where)),
                ?assertEqual(1, maps:get(<<"m.status">>, Where)),
                ?assertEqual(12345, maps:get(<<"m.user_id">>, Where)),
                ?assertEqual({op, <<"!=">>, 12345}, maps:get(<<"g.owner_uid">>, Where)),
                {ok, #{
                    total => 0,
                    page => 2,
                    size => 20,
                    list => []
                }}
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Row) ->
                Row
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
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"attr=join&page=2&size=20">>
        }),
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => page,
            current_uid => 12345
        }),
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 验证 manager 视图使用 owner OR role>=3 条件
page_manager_query_contract_test_() ->
    ?WITH_MOCKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) ->
                12345
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, <<"g.*">>, Where, <<"g.id desc">>, 1, 20) ->
                ?assertEqual(1, maps:get(<<"g.status">>, Where)),
                [OwnerCond, ManagerCond] = maps:get(<<"__or">>, Where),
                ?assertEqual(12345, maps:get(<<"g.owner_uid">>, OwnerCond)),
                ?assertEqual(12345, maps:get(<<"m.user_id">>, ManagerCond)),
                ?assertEqual(1, maps:get(<<"m.status">>, ManagerCond)),
                ?assertEqual({op, <<">=">>, 3}, maps:get(<<"m.role">>, ManagerCond)),
                {ok, #{
                    total => 0,
                    page => 1,
                    size => 20,
                    list => []
                }}
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Row) ->
                Row
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
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"attr=manager&page=1&size=20">>
        }),
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => page,
            current_uid => 12345
        }),
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).

%% @doc 验证未知 attr 回退 owner 视图，避免 function_clause
page_unknown_attr_fallback_owner_test_() ->
    ?WITH_MOCKS([
        {auth_ds, [
            {'current_uid', 1, fun(_State) ->
                12345
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 10}
            end}
        ]},
        {elib_pg, [
            {'page_with_total', 4, fun(_Tb, Where, 1, 10) ->
                ?assertEqual(1, maps:get(status, Where)),
                ?assertEqual(12345, maps:get(owner_uid, Where)),
                {ok, #{
                    total => 0,
                    page => 1,
                    size => 10,
                    list => []
                }}
            end}
        ]},
        {group_logic, [
            {'group_transfer', 1, fun(Row) ->
                Row
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
        MockReq = cowboy_req_h:new(#{
            method => <<"GET">>,
            qs => <<"attr=unknown&page=1&size=10">>
        }),
        {ok, Req, _State} = group_handler:init(MockReq, #{
            action => page,
            current_uid => 12345
        }),
        {StatusCode, _, _Body} = cowboy_req_h:response(Req),
        ?assertEqual(200, StatusCode)
    end).
