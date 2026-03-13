-module(user_tag_relation_remove_object_id_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

collect_remove_uses_raw_object_id_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"scene">> => <<"collect">>,
                        <<"tagId">> => 7,
                        <<"objectId">> => <<"kind_xid_1">>
                    }
                end}
            ]},
            {elib_hashids, [
                {'decode', 1, fun(_Obj) ->
                    ?assert(false, should_not_decode_collect_object_id),
                    0
                end}
            ]},
            {user_tag_relation_logic, [
                {'remove', 4, fun(Uid, Scene, ObjectId, TagId) ->
                    ?assertEqual(101, Uid),
                    ?assertEqual(<<"1">>, Scene),
                    ?assertEqual(<<"kind_xid_1">>, ObjectId),
                    ?assertEqual(7, TagId),
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
            {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => remove}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_tag_relation_logic, remove, 4),
            ?assertEqual(0, meck:num_calls(elib_hashids, decode, 1))
        end
    ).


friend_remove_invalid_object_id_error_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"scene">> => <<"friend">>,
                        <<"tagId">> => 7,
                        <<"objectId">> => <<"bad_hashid">>
                    }
                end}
            ]},
            {elib_hashids, [
                {'decode', 1, fun(_Obj) -> 0 end}
            ]},
            {user_tag_relation_logic, [
                {'remove', 4, fun(_Uid, _Scene, _ObjectId, _TagId) ->
                    ?assert(false, should_not_remove_when_friend_object_invalid),
                    ok
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Msg) ->
                    cowboy_req_h:new(#{response_status => 400})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => remove}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(400, StatusCode),
            ?assertEqual(0, meck:num_calls(user_tag_relation_logic, remove, 4))
        end
    ).


friend_remove_valid_object_id_success_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 101 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"scene">> => <<"friend">>,
                        <<"tagId">> => 7,
                        <<"objectId">> => <<"hash_abc">>
                    }
                end}
            ]},
            {elib_hashids, [
                {'decode', 1, fun(_Obj) -> 2233 end}
            ]},
            {user_tag_relation_logic, [
                {'remove', 4, fun(Uid, Scene, ObjectId, TagId) ->
                    ?assertEqual(101, Uid),
                    ?assertEqual(<<"2">>, Scene),
                    ?assertEqual(2233, ObjectId),
                    ?assertEqual(7, TagId),
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
            {ok, Req, _State} = user_tag_relation_handler:init(MockReq, #{action => remove}),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            meck_helper:verify_called(user_tag_relation_logic, remove, 4),
            meck_helper:verify_called(elib_hashids, decode, 1)
        end
    ).
