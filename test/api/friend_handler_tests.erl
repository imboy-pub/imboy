-module(friend_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/v1/friend">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = friend_handler:init(Req, #{action => false, current_uid => 12345}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_add_friend_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"to">> => <<"u_hash_67890">>,
                    <<"payload">> => #{<<"hello">> => <<"world">>},
                    <<"created_at">> => <<"2026-03-16T00:00:00Z">>
                }
            end}
        ]},
        {friend_logic, [
            {'add_friend', 4, fun(12345, <<"u_hash_67890">>, #{<<"hello">> := <<"world">>}, <<"2026-03-16T00:00:00Z">>) ->
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
        {ok, RespReq, _State} = friend_handler:init(Req, #{action => add_friend, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

init_delete_friend_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uid">> => <<"u_hash_67890">>}
            end}
        ]},
        {friend_logic, [
            {'delete_friend', 2, fun(12345, <<"u_hash_67890">>) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = friend_handler:init(Req, #{action => delete_friend, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

init_list_success_test_() ->
    ?WITH_MECKS([
        {user_logic, [
            {'find_by_id', 1, fun(12345) ->
                #{<<"id">> => 12345, <<"nickname">> => <<"me">>}
            end},
            {'mine_state', 1, fun(12345) ->
                {<<"status">>, <<"online">>}
            end}
        ]},
        {friend_ds, [
            {'page_by_uid', 1, fun(12345) ->
                [#{<<"uid">> => 67890, <<"remark">> => <<"好友">>}]
            end}
        ]},
        {elib_hashids, [
            {'replace_id', 1, fun(Map) ->
                Map#{<<"id">> => <<"me_hash">>}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = friend_handler:init(Req, #{action => list, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(<<"me_hash">>, maps:get(<<"id">>, maps:get(<<"mine">>, Payload))),
        ?assertEqual(1, length(maps:get(<<"friend">>, Payload)))
    end).

init_change_remark_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"uid">> => <<"u_hash_67890">>, <<"remark">> => <<"新备注"/utf8>>}
            end}
        ]},
        {imboy_error, [
            {'validate_id', 2, fun(_Req, <<"u_hash_67890">>) ->
                {ok, 67890}
            end}
        ]},
        {friend_ds, [
            {'change_remark', 3, fun(12345, 67890, <<"新备注"/utf8>>) ->
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = friend_handler:init(Req, #{action => change_remark, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"新备注"/utf8>>, maps:get(<<"remark">>, maps:get(payload, RespReq)))
    end).
