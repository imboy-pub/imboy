-module(user_tag_handler_page_scene_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

collect_scene_page_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"scene">>, <<"collect">>}] end}
            ]},
            {user_tag_logic, [
                {'page', 5, fun(Scene, Page, Size, _WhereMap, _OrderBy) ->
                    ?assertEqual(1, Scene),
                    ?assertEqual(1, Page),
                    ?assertEqual(20, Size),
                    #{total => 1, list => [#{id => 1, name => <<"重要"/utf8>>}]}
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
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_tag_logic, page, 5)
        end
    ).


friend_scene_page_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 10} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"scene">>, <<"friend">>}] end}
            ]},
            {user_tag_logic, [
                {'page', 5, fun(Scene, Page, Size, _WhereMap, _OrderBy) ->
                    ?assertEqual(2, Scene),
                    ?assertEqual(1, Page),
                    ?assertEqual(10, Size),
                    #{total => 1, list => [#{id => 2, name => <<"同事"/utf8>>}]}
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
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_tag_logic, page, 5)
        end
    ).


invalid_scene_page_error_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 10} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"scene">>, <<"unknown">>}] end}
            ]},
            {user_tag_logic, [
                {'page', 5, fun(_Scene, _Page, _Size, _WhereMap, _OrderBy) ->
                    #{}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    cowboy_req_h:new(#{
                        response_status => 400,
                        response_body => #{error => <<"不支持的 Scene"/utf8>>}
                    })
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


invalid_scene_delete_error_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"scene">> => <<"unknown">>,
                        <<"tag">> => <<"x">>
                    }
                end}
            ]},
            {user_tag_logic, [
                {'delete', 3, fun(_Uid, _Scene, _Tag) ->
                    ?assert(false, should_not_delete_when_scene_invalid),
                    ok
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    cowboy_req_h:new(#{
                        response_status => 400,
                        response_body => #{error => <<"不支持的 Scene"/utf8>>}
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_tag_handler:init(MockReq, #{action => delete}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(400, StatusCode),
            ?assertEqual(0, meck:num_calls(user_tag_logic, delete, 3))
        end
    ).
