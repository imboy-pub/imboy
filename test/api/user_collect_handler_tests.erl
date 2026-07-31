-module(user_collect_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

page_success_with_filters_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {2, 5} end},
                {'int', 3, fun(kind, _Req, _Default) -> {ok, 3} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [
                        {<<"order">>, <<"recent_use">>},
                        {<<"kwd">>, <<"alpha">>},
                        {<<"tag">>, <<"工作"/utf8>>}
                    ]
                end}
            ]},
            {elib_hasher, [
                {'decode_list_field', 2, fun(List, _Field) -> List end}
            ]},
            {user_collect_repo, [
                {'tablename', 0, fun() -> <<"public.user_collect">> end}
            ]},
            {elib_pg, [
                {'page_with_total', 6, fun(_Tb, _Column, WhereMap, Order, Page, Size) ->
                    ?assertEqual(101, maps:get(user_id, WhereMap)),
                    ?assertEqual(1, maps:get(status, WhereMap)),
                    ?assertEqual(3, maps:get(kind, WhereMap)),
                    ?assertEqual(
                        {op, <<"LIKE">>, <<"%工作,%"/utf8>>},
                        maps:get(tag, WhereMap)
                    ),
                    ?assertEqual(<<"updated_at desc, id desc">>, Order),
                    ?assertEqual(2, Page),
                    ?assertEqual(5, Size),
                    {ok, #{
                        total => 1,
                        page => Page,
                        size => Size,
                        list => [
                            #{
                                kind => 3,
                                kind_id => <<"collect_1">>,
                                source => <<"alice">>,
                                created_at => 1,
                                updated_at => 1,
                                tag => <<"工作,"/utf8>>,
                                info => <<"{\"text\":\"hello\"}">>
                            }
                        ]
                    }}
                end}
            ]},
            {elib_response, [
                {'json_decode_list_field', 2, fun(List, _Field) -> List end},
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Data
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_collect_handler:init(MockReq, #{action => page}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertEqual(1, maps:get(total, Body)),
            meck_helper:verify_called(elib_pg, page_with_total, 6)
        end
    ).

add_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"kind">> => <<"1">>,
                        <<"kind_id">> => <<"msg_1">>,
                        <<"source">> => <<"chat">>,
                        <<"remark">> => <<"fav">>,
                        <<"info">> => #{<<"text">> => <<"hello">>}
                    }
                end}
            ]},
            {user_collect_logic, [
                {'add', 6, fun(Uid, Kind, KindId, Info, Source, Remark) ->
                    ?assertEqual(101, Uid),
                    ?assertEqual(<<"1">>, Kind),
                    ?assertEqual(<<"msg_1">>, KindId),
                    ?assertEqual(#{<<"text">> => <<"hello">>}, Info),
                    ?assertEqual(<<"chat">>, Source),
                    ?assertEqual(<<"fav">>, Remark),
                    {ok, <<"success">>}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) ->
                    cowboy_req_h:new(#{response_status => 200})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_collect_handler:init(MockReq, #{action => add}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_collect_logic, add, 6)
        end
    ).

remove_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"kind_id">> => <<"msg_1">>
                    }
                end}
            ]},
            {user_collect_logic, [
                {'remove', 2, fun(Uid, KindId) ->
                    ?assertEqual(101, Uid),
                    ?assertEqual(<<"msg_1">>, KindId),
                    {ok, 1}
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    cowboy_req_h:new(#{response_status => 200})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_collect_handler:init(MockReq, #{action => remove}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_collect_logic, remove, 2)
        end
    ).

change_remark_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"action">> => <<"remark">>,
                        <<"kind_id">> => <<"msg_1">>,
                        <<"remark">> => <<"updated">>
                    }
                end}
            ]},
            {user_collect_logic, [
                {'change', 4, fun(Uid, Action, KindId, PostVals) ->
                    ?assertEqual(101, Uid),
                    ?assertEqual(<<"remark">>, Action),
                    ?assertEqual(<<"msg_1">>, KindId),
                    ?assertEqual(<<"updated">>, maps:get(<<"remark">>, PostVals)),
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, _Data, _Msg) ->
                    cowboy_req_h:new(#{response_status => 200})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_collect_handler:init(MockReq, #{action => change}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_collect_logic, change, 4)
        end
    ).
