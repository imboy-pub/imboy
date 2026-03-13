-module(feature_gate_public_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

moment_init_short_circuits_when_feature_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, moment) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = moment_handler:init(req_mock(), #{action => feed, current_uid => 1001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{current_uid => 1001}, State)
    end).

channel_discover_init_uses_channel_discover_feature_test_() ->
    ?WITH_MECKS([
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, channel_discover) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = channel_handler:init(req_mock(), #{action => discover, current_uid => 1001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{current_uid => 1001}, State)
    end).

channel_order_init_uses_channel_order_feature_test_() ->
    ?WITH_MECKS([
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, channel_order) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = channel_handler:init(req_mock(), #{action => create_order, current_uid => 1001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{current_uid => 1001}, State)
    end).

group_vote_init_short_circuits_when_feature_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_vote) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = group_vote_handler:init(req_mock(), #{action => list, current_uid => 1001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{current_uid => 1001}, State)
    end).

group_schedule_init_short_circuits_when_feature_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_schedule) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = group_schedule_handler:init(req_mock(), #{action => list, current_uid => 1001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{current_uid => 1001}, State)
    end).

group_task_init_short_circuits_when_feature_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_task) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = group_task_handler:init(req_mock(), #{action => list, current_uid => 1001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{current_uid => 1001}, State)
    end).

location_init_short_circuits_when_feature_disabled_test_() ->
    ?WITH_MECKS([
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, location) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = location_handler:init(req_mock(), #{action => people_nearby, current_uid => 1001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{current_uid => 1001}, State)
    end).

req_mock() ->
    #{}.
