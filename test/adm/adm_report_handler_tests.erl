-module(adm_report_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

channel_report_feature_enabled_mocks() ->
    [
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, channel) -> ok end}
        ]}
    ].

init_group_list_reads_filters_and_target_override_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) ->
                    [
                        {<<"status">>, <<"1">>},
                        {<<"target_id">>, <<"g_hash_9">>},
                        {<<"reporter_uid">>, <<"u_hash_7">>},
                        {<<"keyword">>, <<"spam">>}
                    ]
                end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end}
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
            {report_logic, [
                {'admin_list', 5, fun(<<"group">>, 1, 1, 20, Filter) ->
                    ?assertEqual(<<"g_hash_9">>, maps:get(target_id, Filter)),
                    ?assertEqual(<<"u_hash_7">>, maps:get(reporter_uid, Filter)),
                    ?assertEqual(<<"spam">>, maps:get(keyword, Filter)),
                    {ok, #{list => [], total => 0, page => 1, size => 20}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_report_handler:init(#{}, #{
                action => group_list, adm_user_id => 9001
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(0, maps:get(total, maps:get(payload, RespReq)))
        end
    ).

init_channel_resolve_passes_target_override_and_params_test_() ->
    ?WITH_MECKS(
        channel_report_feature_enabled_mocks() ++
            [
                {cowboy_req, [
                    {'method', 1, fun(_Req) -> <<"POST">> end}
                ]},
                {elib_param, [
                    {'post', 1, fun(_Req) ->
                        #{
                            <<"report_id">> => <<"rp_hash_1">>,
                            <<"result">> => <<"2">>,
                            <<"note">> => <<"确认违规"/utf8>>
                        }
                    end}
                ]},
                {adm_user_logic, [
                    {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                        #{<<"id">> => 9001, <<"role_id">> => [1]}
                    end}
                ]},
                {adm_index_handler, [
                    {'role_acl', 1, fun(1) ->
                        {<<"super_admin">>, [<<"reports:handle">>], []}
                    end}
                ]},
                {report_logic, [
                    {'admin_resolve', 5, fun(
                        9001, <<"channel">>, <<"rp_hash_1">>, 2, <<"确认违规"/utf8>>
                    ) ->
                        ok
                    end}
                ]},
                {elib_response, [
                    {'success', 2, fun(Req, Payload) ->
                        Req#{response_status => 200, payload => Payload}
                    end}
                ]}
            ],
        fun() ->
            {ok, RespReq, _State} = adm_report_handler:init(#{}, #{
                action => channel_resolve, adm_user_id => 9001
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(#{}, maps:get(payload, RespReq))
        end
    ).

init_batch_resolve_dedups_report_ids_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"target_type">> => <<"user">>,
                        <<"report_ids">> => [<<"1">>, <<"1">>, <<"2">>],
                        <<"result">> => <<"1">>,
                        <<"note">> => <<"batch">>
                    }
                end}
            ]},
            {adm_user_logic, [
                {'find', 3, fun(9001, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 9001, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"super_admin">>, [<<"reports:handle">>], []}
                end}
            ]},
            {report_logic, [
                {'admin_batch_resolve', 5, fun(
                    9001, <<"user">>, [<<"1">>, <<"2">>], 1, <<"batch">>
                ) ->
                    {ok, #{success => 2, failed => 0}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_report_handler:init(#{}, #{
                action => batch_resolve, adm_user_id => 9001
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(2, maps:get(success, maps:get(payload, RespReq)))
        end
    ).

init_detail_returns_bound_evidence_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [{<<"report_id">>, <<"424242">>}] end}
            ]},
            {adm_user_logic, [
                {'find', 3, fun(9002, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 9002, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"super_admin">>, [<<"reports:read">>], []}
                end}
            ]},
            {report_logic, [
                {'admin_detail', 2, fun(9002, <<"424242">>) ->
                    {ok, #{
                        <<"id">> => 424242,
                        <<"target_type">> => <<"message">>,
                        <<"target_sub_type">> => <<"c2c">>,
                        <<"evidence">> => #{<<"e2ee">> => false}
                    }}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_report_handler:init(#{}, #{
                action => detail, adm_user_id => 9002
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(<<"message">>, maps:get(<<"target_type">>, maps:get(payload, RespReq)))
        end
    ).

init_detail_denies_without_permission_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {adm_user_logic, [
                {'find', 3, fun(9003, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 9003, <<"role_id">> => [2]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(2) ->
                    {<<"viewer">>, [], []}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(Req, _Msg, _Code) -> {replied, Req} end}
            ]},
            {report_logic, [
                {'admin_detail', 2, fun(_, _) -> erlang:error(should_not_be_called) end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_report_handler:init(#{}, #{
                action => detail, adm_user_id => 9003
            }),
            ?assertEqual({replied, #{}}, RespReq),
            ?assertEqual(0, meck:num_calls(report_logic, admin_detail, 2))
        end
    ).

init_list_accepts_message_target_type_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [{<<"target_type">>, <<"message">>}] end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 10} end}
            ]},
            {adm_user_logic, [
                {'find', 3, fun(9004, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 9004, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"super_admin">>, [<<"reports:read">>], []}
                end}
            ]},
            {report_logic, [
                {'admin_list', 5, fun(<<"message">>, -1, 1, 10, _Filter) ->
                    {ok, #{list => [], total => 0, page => 1, size => 10}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            {ok, RespReq, _State} = adm_report_handler:init(#{}, #{
                action => list, adm_user_id => 9004
            }),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).
