-module(user_device_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/api/v1/user/device">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = user_device_handler:init(Req, #{
            action => false, current_uid => 12345
        }),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_page_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'page', 1, fun(_Req) ->
                    {1, 20}
                end}
            ]},
            {user_device_logic, [
                {'page', 3, fun(12345, 1, 20) ->
                    #{total => 1, page => 1, size => 20, list => [#{<<"did">> => <<"device-1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => page, current_uid => 12345
            }),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(1, maps:get(total, Payload))
        end
    ).

init_change_name_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"did">> => <<"device-1">>, <<"name">> => <<"iPhone 15">>}
                end}
            ]},
            {user_device_logic, [
                {'change_name', 3, fun(12345, <<"device-1">>, <<"iPhone 15">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(Req) ->
                    Req#{response_status => 200}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => change_name, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

init_delete_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"did">> => <<"device-1">>}
                end}
            ]},
            {user_device_logic, [
                {'delete', 2, fun(12345, <<"device-1">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(Req) ->
                    Req#{response_status => 200}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => delete, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

init_check_login_no_conflict_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"device_type\":\"ios\"}">>, #{}}
                end}
            ]},
            {jsx, [
                {'decode', 2, fun(<<"{\"device_type\":\"ios\"}">>, [return_maps]) ->
                    #{<<"device_type">> => <<"ios">>}
                end}
            ]},
            {user_device_logic, [
                {'validate_device_type', 1, fun(<<"ios">>) ->
                    true
                end},
                {'check_login_conflict', 2, fun(12345, <<"ios">>) ->
                    {ok, no_conflict}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => check_login, current_uid => 12345
            }),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(false, maps:get(<<"conflict">>, Payload))
        end
    ).
