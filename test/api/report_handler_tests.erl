-module(report_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

create_unified_group_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"target_type">> => <<"group">>,
                    <<"target_id">> => <<"g_hash_1">>,
                    <<"reason">> => <<"spam">>,
                    <<"description">> => <<"group abuse">>
                }
            end}
        ]},
        {report_logic, [
            {'create', 5, fun(1001, <<"group">>, <<"g_hash_1">>, <<"spam">>, <<"group abuse">>) ->
                {ok, #{<<"report_id">> => <<"r_hash_1">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = report_handler:handle_action(create, req_mock(), #{current_uid => 1001}),
        ?assertEqual({ok_resp, #{<<"report_id">> => <<"r_hash_1">>}}, Result)
    end).

group_create_prefers_path_group_id_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    group_id -> <<"g_from_path">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"target_id">> => <<"g_from_body">>,
                    <<"reason">> => <<"ad">>,
                    <<"description">> => <<"illegal ad">>
                }
            end}
        ]},
        {report_logic, [
            {'create', 5, fun(1002, <<"group">>, <<"g_from_path">>, <<"ad">>, <<"illegal ad">>) ->
                {ok, #{<<"report_id">> => <<"r_hash_2">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = report_handler:handle_action(group_create, req_mock(), #{current_uid => 1002}),
        ?assertEqual({ok_resp, #{<<"report_id">> => <<"r_hash_2">>}}, Result)
    end).

create_normalizes_channels_alias_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"target_type">> => <<"channels">>,
                    <<"target_id">> => <<"ch_hash_9">>,
                    <<"reason">> => <<"abuse">>,
                    <<"description">> => <<"channel abuse">>
                }
            end}
        ]},
        {report_logic, [
            {'create', 5, fun(1003, <<"channel">>, <<"ch_hash_9">>, <<"abuse">>, <<"channel abuse">>) ->
                {ok, #{<<"report_id">> => <<"r_hash_3">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Result = report_handler:handle_action(create, req_mock(), #{current_uid => 1003}),
        ?assertEqual({ok_resp, #{<<"report_id">> => <<"r_hash_3">>}}, Result)
    end).

create_error_passthrough_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"target_type">> => <<"user">>,
                    <<"target_id">> => <<>>,
                    <<"reason">> => <<"abuse">>
                }
            end}
        ]},
        {report_logic, [
            {'create', 5, fun(1004, <<"user">>, <<>>, <<"abuse">>, <<>>) ->
                {error, <<"举报对象无效"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Result = report_handler:handle_action(create, req_mock(), #{current_uid => 1004}),
        ?assertEqual({error_resp, <<"举报对象无效"/utf8>>}, Result)
    end).

req_mock() ->
    #{}.

create_moment_target_short_circuits_when_feature_disabled_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"target_type">> => <<"moment">>,
                    <<"target_id">> => <<"m_hash_1">>,
                    <<"reason">> => <<"spam">>,
                    <<"description">> => <<"bad moment">>
                }
            end}
        ]},
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, moment) ->
                {error, blocked_req}
            end}
        ]},
        {report_logic, [
            {'create', 5, fun(_, _, _, _, _) -> erlang:error(should_not_be_called) end}
        ]}
    ], fun() ->
        Result = report_handler:handle_action(create, req_mock(), #{current_uid => 1005}),
        ?assertEqual(blocked_req, Result),
        ?assertEqual(0, meck:num_calls(report_logic, create, 5))
    end).
