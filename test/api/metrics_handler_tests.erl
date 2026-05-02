-module(metrics_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

init_returns_metrics_payload_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 3, fun(_Name, _Req, _Default) -> <<>> end}
        ]},
        {elib_metric, [
            {'get_all_metrics', 0, fun() ->
                #{connections => 2, queue_depth => 5}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = metrics_handler:init(#{}, #{}),
        ?assertEqual(#{}, State),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(2, maps:get(connections, maps:get(payload, RespReq))),
        ?assertEqual("success.", maps:get(msg, RespReq))
    end).

init_returns_error_when_metric_fetch_fails_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'header', 3, fun(_Name, _Req, _Default) -> <<>> end}
        ]},
        {elib_metric, [
            {'get_all_metrics', 0, fun() ->
                erlang:error(metric_down)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(Req, Msg) ->
                Req#{response_status => 400, error_msg => Msg}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = metrics_handler:init(#{}, #{}),
        ?assertEqual(400, maps:get(response_status, RespReq)),
        Msg = maps:get(error_msg, RespReq),
        ?assert(binary:match(Msg, <<"metrics unavailable">>) =/= nomatch)
    end).
