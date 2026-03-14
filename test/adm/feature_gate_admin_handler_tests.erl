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

adm_channel_list_init_uses_channel_feature_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, channel) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_channel_handler:init(req_mock(), #{action => list, adm_user_id => 9001}),
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

adm_report_channel_list_init_uses_channel_feature_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => [1]}
            end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(1) ->
                {<<"super_admin">>, [<<"reports:read">>], []}
            end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, channel) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_report_handler:init(req_mock(), #{action => channel_list, adm_user_id => 9001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{adm_user_id => 9001}, State)
    end).

adm_report_list_with_moment_target_uses_moment_feature_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'parse_qs', 1, fun(_Req) -> [{<<"target_type">>, <<"moment">>}] end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 9001, <<"role_id">> => [1]}
            end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(1) ->
                {<<"super_admin">>, [<<"reports:read">>], []}
            end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, moment) ->
                {error, blocked_req}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_report_handler:init(req_mock(), #{action => list, adm_user_id => 9001}),
        ?assertEqual(blocked_req, RespReq),
        ?assertEqual(#{adm_user_id => 9001}, State)
    end).

adm_report_permission_denied_short_circuits_before_feature_gate_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'parse_qs', 1, fun(_Req) -> [{<<"target_type">>, <<"channel">>}] end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, _Feature) ->
                erlang:error(should_not_be_called)
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_report_handler:init(req_mock(), #{action => list, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(#{adm_user_id => 3001}, State),
        ?assertEqual(0, meck:num_calls(imboy_feature, ensure_enabled, 2))
    end).

req_mock() ->
    #{}.
