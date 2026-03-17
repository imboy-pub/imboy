-module(location_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/v1/location">>}.

init_false_action_passthrough_test_() ->
    ?WITH_MECKS([
        {imboy_plugin_registry, [
            {'required_feature', 3, fun(api, location_handler, false) ->
                undefined
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = location_handler:init(Req, #{action => false, current_uid => 12345}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_make_myself_visible_success_test_() ->
    ?WITH_MECKS([
        {imboy_plugin_registry, [
            {'required_feature', 3, fun(api, location_handler, make_myself_visible) ->
                undefined
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"latitude">> => 39.9042, <<"longitude">> => 116.4074}
            end}
        ]},
        {location_logic, [
            {'make_myself_visible', 3, fun(12345, 39.9042, 116.4074) ->
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
        {ok, RespReq, _State} = location_handler:init(Req, #{action => make_myself_visible, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

init_make_myself_visible_error_test_() ->
    ?WITH_MECKS([
        {imboy_plugin_registry, [
            {'required_feature', 3, fun(api, location_handler, make_myself_visible) ->
                undefined
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"latitude">> => 39.9042, <<"longitude">> => 116.4074}
            end}
        ]},
        {location_logic, [
            {'make_myself_visible', 3, fun(12345, 39.9042, 116.4074) ->
                {error, <<"bad_location">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, <<"bad_location">>) ->
                Req#{response_status => 400}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = location_handler:init(Req, #{action => make_myself_visible, current_uid => 12345}),
        ?assertEqual(400, maps:get(response_status, RespReq))
    end).

init_make_myself_unvisible_success_test_() ->
    ?WITH_MECKS([
        {imboy_plugin_registry, [
            {'required_feature', 3, fun(api, location_handler, make_myself_unvisible) ->
                undefined
            end}
        ]},
        {location_logic, [
            {'make_myself_unvisible', 1, fun(12345) ->
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
        {ok, RespReq, _State} = location_handler:init(Req, #{action => make_myself_unvisible, current_uid => 12345}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{}, maps:get(payload, RespReq))
    end).

init_people_nearby_success_test_() ->
    ?WITH_MECKS([
        {imboy_plugin_registry, [
            {'required_feature', 3, fun(api, location_handler, people_nearby) ->
                undefined
            end}
        ]},
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [
                    {<<"longitude">>, <<"116.4074">>},
                    {<<"latitude">>, <<"39.9042">>},
                    {<<"unit">>, <<"m">>}
                ]
            end}
        ]},
        {elib_param, [
            {'int', 3, fun
                (radius, _Req, 500) ->
                    {ok, 500};
                (limit, _Req, 100) ->
                    {ok, 20}
            end}
        ]},
        {location_logic, [
            {'people_nearby', 6, fun(12345, <<"116.4074">>, <<"39.9042">>, 500, <<"m">>, 20) ->
                [#{<<"uid">> => 67890, <<"distance">> => 123}]
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = location_handler:init(Req, #{action => people_nearby, current_uid => 12345}),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(500, maps:get(<<"radius">>, Payload)),
        ?assertEqual(1, maps:get(<<"size">>, Payload))
    end).
