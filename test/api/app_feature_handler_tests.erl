-module(app_feature_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

init_features_returns_feature_payload_test_() ->
    Payload = #{<<"core">> => true, <<"moment">> => false, <<"chat_multi_select_v1">> => true},
    ?WITH_MOCKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {imboy_feature, [
                {'client_snapshot', 0, fun() -> Payload end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, ResponsePayload) ->
                    Req#{response_status => 200, payload => ResponsePayload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, State} = app_feature_handler:init(#{}, #{action => features}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(Payload, maps:get(payload, RespReq)),
            ?assertEqual(#{}, State)
        end
    ).

init_policy_returns_effective_policy_payload_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end}
            ]},
            {imboy_policy, [
                {'effective_view', 0, fun() ->
                    #{
                        <<"profile">> => <<"enterprise">>,
                        <<"capabilities">> => #{<<"message_search">> => true},
                        <<"features">> => #{<<"channel">> => true},
                        <<"plugins">> => #{<<"channel">> => #{<<"enabled">> => true}}
                    }
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, State} = app_feature_handler:init(#{}, #{action => policy}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(<<"enterprise">>, maps:get(<<"profile">>, maps:get(payload, RespReq))),
            ?assertEqual(#{}, State)
        end
    ).
