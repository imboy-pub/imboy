-module(feature_gate_admin_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

adm_moment_init_short_circuits_when_feature_disabled_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, moment) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_moment_handler:init(req_mock(), #{action => list, adm_user_id => 9001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{adm_user_id => 9001}, State)
    end).

adm_channel_orders_init_uses_channel_order_feature_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, channel_order) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_channel_handler:init(req_mock(), #{action => orders, adm_user_id => 9001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{adm_user_id => 9001}, State)
    end).

adm_group_vote_init_uses_group_vote_feature_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_vote) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_group_handler:init(req_mock(), #{action => vote_list, adm_user_id => 9001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{adm_user_id => 9001}, State)
    end).

adm_group_schedule_init_uses_group_schedule_feature_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_schedule) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_group_handler:init(req_mock(), #{action => schedule_list, adm_user_id => 9001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{adm_user_id => 9001}, State)
    end).

adm_group_task_init_uses_group_task_feature_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, group_task) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_group_handler:init(req_mock(), #{action => task_list, adm_user_id => 9001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{adm_user_id => 9001}, State)
    end).

req_mock() ->
    #{}.
