-module(user_tag_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

page_invalid_scene_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"scene">>, <<"unknown">>}] end}
            ]},
            {user_tag_logic, [
                {'page', 5, fun(_Scene, _Page, _Size, _WhereMap, _OrderBy) ->
                    ?assert(false, should_not_query_tags_when_scene_invalid),
                    #{}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    cowboy_req_h:new(#{response_status => 400})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => page}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(400, StatusCode),
            ?assertEqual(0, meck:num_calls(user_tag_logic, page, 5))
        end
    ).


page_collect_scene_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {2, 5} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [
                        {<<"kwd">>, <<"alice">>},
                        {<<"scene">>, <<"collect">>}
                    ]
                end}
            ]},
            {user_tag_logic, [
                {'page', 5, fun(Scene, Page, Size, WhereMap, OrderBy) ->
                    ?assertEqual(1, Scene),
                    ?assertEqual(2, Page),
                    ?assertEqual(5, Size),
                    ?assertEqual(<<"id desc">>, OrderBy),
                    ?assertEqual(101, maps:get(creator_user_id, WhereMap)),
                    ?assertEqual(1, maps:get(scene, WhereMap)),
                    ?assertEqual(
                        {op, <<"LIKE">>, <<"%alice%">>},
                        maps:get(name, WhereMap)
                    ),
                    #{total => 1, list => [#{id => 7, name => <<"alice">>}]}
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
            {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => page}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertEqual(1, maps:get(total, Body)),
            meck_helper:verify_called(user_tag_logic, page, 5)
        end
    ).


change_name_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"scene">> => <<"friend">>,
                        <<"tagName">> => <<"合作伙伴"/utf8>>,
                        <<"tagId">> => 7
                    }
                end}
            ]},
            {elib_pg, [
                {'pluck_value', 5, fun(_Tb, _Col, WhereMap, _Opt, _Default) ->
                    ?assertEqual(2, maps:get(<<"scene">>, WhereMap)),
                    ?assertEqual(101, maps:get(<<"creator_user_id">>, WhereMap)),
                    ?assertEqual(<<"合作伙伴"/utf8>>, maps:get(<<"name">>, WhereMap)),
                    ?assertEqual({op, <<"<>">>, 7}, maps:get(<<"id">>, WhereMap)),
                    0
                end}
            ]},
            {user_tag_logic, [
                {'change_name', 5, fun(Count, Uid, Scene, TagId, TagName) ->
                    ?assertEqual(0, Count),
                    ?assertEqual(101, Uid),
                    ?assertEqual(2, Scene),
                    ?assertEqual(7, TagId),
                    ?assertEqual(<<"合作伙伴"/utf8>>, TagName),
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
            {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => change_name}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_tag_logic, change_name, 5)
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
                        <<"scene">> => <<"friend">>,
                        <<"tag">> => <<"同事"/utf8>>
                    }
                end}
            ]},
            {user_tag_logic, [
                {'add', 3, fun(Uid, Scene, Tag) ->
                    ?assertEqual(101, Uid),
                    ?assertEqual(2, Scene),
                    ?assertEqual(<<"同事"/utf8>>, Tag),
                    {ok, 42}
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, Data, _Msg) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Data
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => add}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertEqual(42, maps:get(<<"tagId">>, Body)),
            meck_helper:verify_called(user_tag_logic, add, 3)
        end
    ).


delete_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"scene">> => <<"collect">>,
                        <<"tag">> => <<"归档"/utf8>>
                    }
                end}
            ]},
            {user_tag_logic, [
                {'delete', 3, fun(Uid, Scene, Tag) ->
                    ?assertEqual(101, Uid),
                    ?assertEqual(1, Scene),
                    ?assertEqual(<<"归档"/utf8>>, Tag),
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
            {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => delete}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_tag_logic, delete, 3)
        end
    ).
