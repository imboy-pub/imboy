-module(friend_category_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/v1/friend/category">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = friend_category_handler:init(Req, #{action => false, current_uid => 12345}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_add_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"name">> => <<"工作"/utf8>>}
            end}
        ]},
        {friend_category_ds, [
            {'add', 2, fun(12345, <<"工作"/utf8>>) ->
                {ok, 88}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, "success.") ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = friend_category_handler:init(Req, #{action => add, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(88, maps:get(<<"id">>, maps:get(payload, RespReq))),
        ?assertEqual(<<"工作"/utf8>>, maps:get(<<"name">>, maps:get(payload, RespReq))),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_add_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"name">> => <<"工作"/utf8>>}
            end}
        ]},
        {friend_category_ds, [
            {'add', 2, fun(12345, <<"工作"/utf8>>) ->
                {error, <<"category_exists">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, <<"category_exists">>) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = friend_category_handler:init(Req, #{action => add, current_uid => 12345}),
        ?assertEqual(400, maps:get(response_status, RespReq))
    end).

init_delete_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"id">> => 7}
            end}
        ]},
        {friend_category_logic, [
            {'delete', 2, fun(12345, 7) ->
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
        {ok, RespReq, _State} = friend_category_handler:init(Req, #{action => delete, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

init_rename_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"id">> => 7, <<"name">> => <<"新分组"/utf8>>}
            end}
        ]},
        {friend_category_ds, [
            {'rename', 3, fun(12345, 7, <<"新分组"/utf8>>) ->
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
        {ok, RespReq, _State} = friend_category_handler:init(Req, #{action => rename, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).
