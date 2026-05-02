-module(user_tag_relation_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/v1/user/tag/relation">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = user_tag_relation_handler:init(Req, #{action => false, current_uid => 12345}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_add_collect_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"scene">> => <<"collect">>,
                    <<"tag">> => [<<"tag-a">>],
                    <<"objectId">> => <<"kind-1">>
                }
            end}
        ]},
        {user_tag_relation_logic, [
            {'add', 4, fun(12345, 1, <<"kind-1">>, [<<"tag-a">>]) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = user_tag_relation_handler:init(Req, #{action => add, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

init_remove_friend_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"scene">> => <<"friend">>,
                    <<"tagId">> => 7,
                    <<"objectId">> => <<"67890">>
                }
            end}
        ]},
        {user_tag_relation_logic, [
            {'remove', 4, fun(12345, <<"2">>, 67890, 7) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = user_tag_relation_handler:init(Req, #{action => remove, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

init_friend_page_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end},
            {'int', 3, fun(tag_id, _Req, 0) ->
                {ok, 9}
            end}
        ]},
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [{<<"kwd">>, <<"alice">>}]
            end}
        ]},
        {friend_ds, [
            {'page_by_tag', 5, fun(12345, 1, 20, 9, <<"alice">>) ->
                #{total => 1, page => 1, size => 20, list => [#{<<"uid">> => 67890}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = user_tag_relation_handler:init(Req, #{action => friend_page, current_uid => 12345}),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(1, maps:get(total, Payload))
    end).

init_add_invalid_scene_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"scene">> => <<"unknown">>, <<"tag">> => [<<"tag-a">>], <<"objectId">> => <<"kind-1">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, <<"不支持的 Scene"/utf8>>) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = user_tag_relation_handler:init(Req, #{action => add, current_uid => 12345}),
        ?assertEqual(400, maps:get(response_status, RespReq))
    end).
