-module(user_denylist_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/v1/user/denylist">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = user_denylist_handler:init(Req, #{action => false, current_uid => 12345}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_add_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"denied_user_id">> => <<"u_hash_67890">>}
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"u_hash_67890">>) -> 67890 end},
            {'encode', 1, fun(12345) -> <<"u_hash_12345">> end}
        ]},
        {user_denylist_logic, [
            {'add', 2, fun(12345, 67890) ->
                <<"2026-03-16T00:00:00Z">>
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = user_denylist_handler:init(Req, #{action => add, current_uid => 12345}),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"u_hash_12345">>, maps:get(<<"user_id">>, Payload)),
        ?assertEqual(<<"u_hash_67890">>, maps:get(<<"denied_user_id">>, Payload))
    end).

init_remove_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"denied_user_id">> => <<"u_hash_67890">>}
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"u_hash_67890">>) -> 67890 end}
        ]},
        {user_denylist_logic, [
            {'remove', 2, fun(12345, 67890) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 1, fun(Req) ->
                Req#{response_status => 200}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = user_denylist_handler:init(Req, #{action => remove, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq))
    end).

init_page_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'page', 1, fun(_Req) ->
                {1, 20}
            end}
        ]},
        {user_denylist_logic, [
            {'page', 3, fun(12345, 1, 20) ->
                #{total => 1, page => 1, size => 20, list => [#{<<"denied_user_id">> => 67890}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = user_denylist_handler:init(Req, #{action => page, current_uid => 12345}),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(1, maps:get(total, Payload))
    end).
