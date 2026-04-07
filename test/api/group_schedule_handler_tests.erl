-module(group_schedule_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc
%%% group_schedule_handler 参数校验与错误码回归测试
%%% 这些用例只覆盖 handler 参数校验与兼容行为。
%%% feature gate 行为由 feature_gate_public_handler_tests 独立覆盖。
%%%===================================================================

init_create_missing_group_id_returns_bad_request_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"title">> => <<"周会"/utf8>>,
                    <<"start_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"end_at">> => <<"2026-02-22T11:00:00Z">>
                }
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => create, current_uid => 300}),
        ?assertEqual(req_error, Req1),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

init_create_invalid_time_range_returns_bad_request_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"group_id">> => <<"gid_9">>,
                    <<"title">> => <<"周会"/utf8>>,
                    <<"start_at">> => <<"2026-02-22T11:00:00Z">>,
                    <<"end_at">> => <<"2026-02-22T10:00:00Z">>
                }
            end}
        ]},
        {group_schedule_logic, [
            {'create_schedule', 9, fun(_GroupId, _Uid, _Title, _Desc, _Loc, _StartAt, _EndAt, _Remind, _Participants) ->
                {error, {invalid_time_range, start_at, end_at}}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => create, current_uid => 300}),
        ?assertEqual(req_error, Req1),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

init_create_epoch_time_compatible_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"group_id">> => <<"gid_9">>,
                    <<"title">> => <<"周会"/utf8>>,
                    <<"start_at">> => 1700000000,
                    <<"end_at">> => 1700003600
                }
            end}
        ]},
        {group_schedule_logic, [
            {'create_schedule', 9, fun(9, 300, _Title, _Desc, _Loc, StartAt, EndAt, _Remind, _Participants) ->
                ?assert(is_binary(StartAt)),
                ?assert(is_binary(EndAt)),
                {ok, #{schedule_id => <<"sched_9">>}}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => create, current_uid => 300}),
        ?assertEqual(req_ok, Req1)
    end).

init_update_unauthorized_returns_forbidden_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"schedule_id">> => <<"sch_1">>,
                    <<"title">> => <<"更新会议"/utf8>>,
                    <<"start_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"end_at">> => <<"2026-02-22T11:00:00Z">>
                }
            end}
        ]},
        {group_schedule_logic, [
            {'update_schedule', 7, fun(_ScheduleId, _Uid, _Title, _Desc, _Loc, _StartAt, _EndAt) ->
                {error, unauthorized}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => update, current_uid => 300}),
        ?assertEqual(req_error, Req1),
        ?assertEqual(?ERR_FORBIDDEN, receive_resp_code())
    end).

init_update_numeric_schedule_id_compatible_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"schedule_id">> => 77,
                    <<"title">> => <<"更新会议"/utf8>>,
                    <<"start_at">> => 1700000000,
                    <<"end_at">> => 1700003600
                }
            end}
        ]},
        {group_schedule_repo, [
            {'find_by_id', 2, fun(77, <<"schedule_id">>) ->
                #{<<"schedule_id">> => <<"sched_77">>}
            end}
        ]},
        {group_schedule_logic, [
            {'update_schedule', 7, fun(<<"sched_77">>, 300, _Title, _Desc, _Loc, StartAt, EndAt) ->
                ?assert(is_binary(StartAt)),
                ?assert(is_binary(EndAt)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => update, current_uid => 300}),
        ?assertEqual(req_ok, Req1)
    end).

init_cancel_numeric_schedule_id_compatible_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"schedule_id">> => 88
                }
            end}
        ]},
        {group_schedule_repo, [
            {'find_by_id', 2, fun(88, <<"schedule_id">>) ->
                #{<<"schedule_id">> => <<"sched_88">>}
            end}
        ]},
        {group_schedule_logic, [
            {'cancel_schedule', 2, fun(<<"sched_88">>, 300) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => cancel, current_uid => 300}),
        ?assertEqual(req_ok, Req1)
    end).

init_confirm_participant_not_found_returns_forbidden_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"schedule_id">> => <<"sch_2">>,
                    <<"accept">> => true
                }
            end}
        ]},
        {group_schedule_logic, [
            {'confirm_participation', 3, fun(_ScheduleId, _Uid, _Accept) ->
                {error, participant_not_found}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => confirm, current_uid => 300}),
        ?assertEqual(req_error, Req1),
        ?assertEqual(?ERR_FORBIDDEN, receive_resp_code())
    end).

init_confirm_numeric_schedule_id_compatible_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"schedule_id">> => 99,
                    <<"accept">> => <<"0">>
                }
            end}
        ]},
        {group_schedule_repo, [
            {'find_by_id', 2, fun(99, <<"schedule_id">>) ->
                #{<<"schedule_id">> => <<"sched_99">>}
            end}
        ]},
        {group_schedule_logic, [
            {'confirm_participation', 3, fun(<<"sched_99">>, 300, false) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(_Req, _Payload, _Msg) ->
                req_ok
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => confirm, current_uid => 300}),
        ?assertEqual(req_ok, Req1)
    end).

init_detail_missing_schedule_id_returns_bad_request_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'error', 3, fun(_Req, _Msg, Code) ->
                self() ! {resp_code, Code},
                req_error
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => detail, current_uid => 300}),
        ?assertEqual(req_error, Req1),
        ?assertEqual(?ERR_BAD_REQUEST, receive_resp_code())
    end).

init_detail_hashid_schedule_id_compatible_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"schedule_id">>, <<"hash_66">>}] end}
        ]},
        {group_schedule_repo, [
            {'find_by_id', 2, fun(66, <<"schedule_id">>) ->
                #{<<"schedule_id">> => <<"sched_66">>}
            end}
        ]},
        {group_schedule_logic, [
            {'get_schedule_detail', 1, fun(<<"sched_66">>) ->
                {ok, #{schedule => #{<<"schedule_id">> => <<"sched_66">>}}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => detail, current_uid => 300}),
        ?assertEqual(req_ok, Req1)
    end).

init_list_with_time_range_query_compatible_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [
                    {<<"group_id">>, <<"gid_9">>},
                    {<<"start_at">>, <<"1700000000">>},
                    {<<"end_at">>, <<"1700003600">>},
                    {<<"page">>, <<"1">>},
                    {<<"size">>, <<"20">>}
                ]
            end}
        ]},
        {elib_param, [
            {'int', 3, fun
                (page, _Qs, 1) -> 1;
                (size, _Qs, 20) -> 20
            end}
        ]},
        {group_schedule_logic, [
            {'list_group_schedules', 5, fun(9, StartAt, EndAt, 1, 20) ->
                ?assert(is_binary(StartAt)),
                ?assert(is_binary(EndAt)),
                {ok, #{list => [], total => 0, page => 1, size => 20}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => list, current_uid => 300}),
        ?assertEqual(req_ok, Req1)
    end).

init_my_list_with_time_range_query_compatible_test_() ->
    ?WITH_MECKS(feature_gate_bypass_mocks() ++ [
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) ->
                [
                    {<<"start_at">>, <<"1700000000">>},
                    {<<"end_at">>, <<"1700003600">>},
                    {<<"page">>, <<"1">>},
                    {<<"size">>, <<"20">>}
                ]
            end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {group_schedule_logic, [
            {'list_my_schedules', 5, fun(300, StartAt, EndAt, 1, 20) ->
                ?assert(is_binary(StartAt)),
                ?assert(is_binary(EndAt)),
                {ok, []}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) ->
                req_ok
            end}
        ]}
    ], fun() ->
        {ok, Req1, _State} = group_schedule_handler:init(req_mock(), #{action => my_list, current_uid => 300}),
        ?assertEqual(req_ok, Req1)
    end).

req_mock() ->
    #{mock_req => true}.

feature_gate_bypass_mocks() ->
    [
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, _Feature) ->
                ok
            end}
        ]}
    ].

receive_resp_code() ->
    receive
        {resp_code, Code} -> Code
    after 1000 ->
        timeout
    end.
