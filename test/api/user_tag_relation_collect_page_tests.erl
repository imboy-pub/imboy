-module(user_tag_relation_collect_page_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

collect_page_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {2, 5} end},
                {'int', 3, fun(tag_id, _Req, _Default) -> {ok, 9} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"kwd">>, <<"abc">>}] end}
            ]},
            {elib_pg, [
                {'pluck_value', 5, fun(_Tb, _Col, _Where, _Opt, _Default) ->
                    <<"工作"/utf8>>
                end},
                {'page_with_total', 6, fun(_Tb, _Column, WhereMap, _Order, Page, Size) ->
                    ?assertEqual(101, maps:get(user_id, WhereMap)),
                    ?assertEqual(1, maps:get(status, WhereMap)),
                    ?assertEqual({op, <<"LIKE">>, <<"%工作,%"/utf8>>}, maps:get(tag, WhereMap)),
                    ?assert(maps:is_key(<<"__or">>, WhereMap)),
                    ?assertEqual(2, Page),
                    ?assertEqual(5, Size),
                    {ok, #{
                        total => 1,
                        page => Page,
                        size => Size,
                        list => [
                            #{
                                kind => 1,
                                kind_id => <<"msg_1">>,
                                source => <<"alice">>,
                                created_at => 1,
                                updated_at => 1,
                                tag => <<"工作,">>,
                                info => <<"{\"text\":\"hello\"}">>
                            }
                        ]
                    }}
                end}
            ]},
            {elib_hasher, [
                {'decoded_field', 1, fun(_Field) -> <<"info">> end}
            ]},
            {user_collect_repo, [
                {'tablename', 0, fun() -> <<"public.user_collect">> end}
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
            {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => collect_page}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertEqual(1, maps:get(total, Body)),
            meck_helper:verify_called(elib_pg, page_with_total, 6)
        end
    ).


collect_page_tag_not_found_returns_empty_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun(tag_id, _Req, _Default) -> {ok, 9} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_pg, [
                {'pluck_value', 5, fun(_Tb, _Col, _Where, _Opt, _Default) ->
                    <<>>
                end},
                {'page_with_total', 6, fun(_Tb, _Column, _WhereMap, _Order, _Page, _Size) ->
                    ?assert(false, should_not_query_collect_table_when_tag_missing),
                    {ok, #{}}
                end}
            ]},
            {elib_response, [
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
            {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => collect_page}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertEqual([], maps:get(list, Body)),
            ?assertEqual(0, maps:get(total, Body))
        end
    ).


collect_page_invalid_tag_id_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end},
                {'int', 3, fun(tag_id, _Req, _Default) -> {ok, 0} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    cowboy_req_h:new(#{
                        response_status => 400,
                        response_body => #{error => <<"tag_id 格式有误"/utf8>>}
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => collect_page}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(400, StatusCode)
        end
    ).
