-module(user_tag_relation_add_scene_guard_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

invalid_scene_add_returns_error_instead_of_crash_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"scene">> => <<"unknown">>,
                        <<"tag">> => [<<"abc">>],
                        <<"objectId">> => <<"obj_1">>
                    }
                end}
            ]},
            {user_tag_relation_logic, [
                {'add', 4, fun(_Uid, _Scene, _ObjectId, _Tag) ->
                    ?assert(false, should_not_call_logic_when_scene_invalid),
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
            {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => add}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(400, StatusCode),
            ?assertEqual(0, meck:num_calls(user_tag_relation_logic, add, 4))
        end
    ).
