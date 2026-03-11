-module(app_feature_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

init_features_returns_feature_payload_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {imboy_feature, [
            {'all', 0, fun() ->
                #{<<"core">> => true, <<"moment">> => false}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = app_feature_handler:init(#{}, #{action => features}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{<<"core">> => true, <<"moment">> => false}, maps:get(payload, RespReq)),
        ?assertEqual(#{}, State)
    end).
