-module(adm_attach_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"GET">>, path => <<"/adm/attach">>}.

init_false_action_passthrough_test_() ->
    ?WITH_MECK(
        cowboy_req,
        [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, State} = adm_attach_handler:init(Req, #{
                action => false, adm_user_id => 10
            }),
            ?assertEqual(Req, RespReq),
            ?assertEqual(#{adm_user_id => 10}, State)
        end
    ).

init_auth_single_uri_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"uri">> => <<"/attachment/file1.jpg">>}
                end}
            ]},
            {elib_uri, [
                {'check_auth', 1, fun(<<"/attachment/file1.jpg">>) -> true end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, "success.") ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual([true], maps:get(<<"uri">>, maps:get(payload, RespReq)))
        end
    ).

init_auth_mixed_permissions_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"uri">> => <<"/public/file.jpg,/private/file.pdf">>}
                end}
            ]},
            {elib_uri, [
                {'check_auth', 1, fun(Uri) ->
                    case Uri of
                        <<"/public/", _/binary>> -> true;
                        <<"/private/", _/binary>> -> false
                    end
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, "success.") ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual([true, false], maps:get(<<"uri">>, maps:get(payload, RespReq)))
        end
    ).

init_auth_empty_uri_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"POST">> end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"uri">> => <<>>}
                end}
            ]},
            {elib_uri, [
                {'check_auth', 1, fun(<<>>) -> false end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, "success.") ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual([false], maps:get(<<"uri">>, maps:get(payload, RespReq)))
        end
    ).

init_auth_non_post_passthrough_test_() ->
    ?WITH_MECK(
        cowboy_req,
        [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = adm_attach_handler:init(Req, #{action => auth}),
            ?assertEqual(Req, RespReq)
        end
    ).

%% ===================================================================
%% disable / enable / delete
%% ===================================================================

disable_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
            {adm_user_logic, [
                {'find', 3, fun(1, _, _) ->
                    #{<<"id">> => 1, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"admin">>, [<<"storage:disable">>], []}
                end}
            ]},
            {elib_param, [{'post', 1, fun(_) -> #{<<"id">> => 999} end}]},
            {attachment_ds, [{'disable', 1, fun(999) -> ok end}]},
            {ec_cnv, [{'to_integer', 1, fun(999) -> 999 end}]},
            {elib_response, [{'success', 2, fun(Req, _) -> Req#{response_status => 200} end}]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _} = adm_attach_handler:init(Req, #{action => disable, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

disable_no_permission_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
            {adm_user_logic, [
                {'find', 3, fun(1, _, _) ->
                    #{<<"id">> => 1, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"viewer">>, [], []}
                end}
            ]},
            {elib_response, [{'error', 3, fun(Req, _, _) -> Req#{response_status => 403} end}]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _} = adm_attach_handler:init(Req, #{action => disable, adm_user_id => 1}),
            ?assertEqual(403, maps:get(response_status, RespReq))
        end
    ).

disable_invalid_id_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
            {adm_user_logic, [
                {'find', 3, fun(1, _, _) ->
                    #{<<"id">> => 1, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"admin">>, [<<"storage:disable">>], []}
                end}
            ]},
            {elib_param, [{'post', 1, fun(_) -> #{<<"id">> => <<"bad">>} end}]},
            {ec_cnv, [{'to_integer', 1, fun(<<"bad">>) -> 0 end}]},
            {elib_response, [{'error', 3, fun(Req, _, _) -> Req#{response_status => 400} end}]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _} = adm_attach_handler:init(Req, #{action => disable, adm_user_id => 1}),
            ?assertEqual(400, maps:get(response_status, RespReq))
        end
    ).

enable_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
            {adm_user_logic, [
                {'find', 3, fun(1, _, _) ->
                    #{<<"id">> => 1, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"admin">>, [<<"storage:enable">>], []}
                end}
            ]},
            {elib_param, [{'post', 1, fun(_) -> #{<<"id">> => 111} end}]},
            {attachment_ds, [{'enable', 1, fun(111) -> ok end}]},
            {ec_cnv, [{'to_integer', 1, fun(111) -> 111 end}]},
            {elib_response, [{'success', 2, fun(Req, _) -> Req#{response_status => 200} end}]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _} = adm_attach_handler:init(Req, #{action => enable, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

delete_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
            {adm_user_logic, [
                {'find', 3, fun(1, _, _) ->
                    #{<<"id">> => 1, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"admin">>, [<<"storage:delete">>], []}
                end}
            ]},
            {elib_param, [{'post', 1, fun(_) -> #{<<"id">> => 222} end}]},
            {attachment_ds, [{'soft_delete', 1, fun(222) -> ok end}]},
            {ec_cnv, [{'to_integer', 1, fun(222) -> 222 end}]},
            {elib_response, [{'success', 2, fun(Req, _) -> Req#{response_status => 200} end}]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _} = adm_attach_handler:init(Req, #{action => delete, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

%% ===================================================================
%% orphan / orphan_cleanup
%% ===================================================================

orphan_stats_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_) -> <<"GET">> end},
                {'parse_qs', 1, fun(_) -> [{<<"age_days">>, <<"30">>}] end}
            ]},
            {adm_user_logic, [
                {'find', 3, fun(1, _, _) ->
                    #{<<"id">> => 1, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"admin">>, [<<"storage:cleanup">>], []}
                end}
            ]},
            {attachment_ds, [
                {'orphan_stats', 1, fun(_) ->
                    {ok, #{<<"count">> => 10, <<"total_size">> => 2048000}}
                end}
            ]},
            {ec_cnv, [{'to_integer', 1, fun(<<"30">>) -> 30 end}]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, _) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _} = adm_attach_handler:init(Req, #{action => orphan, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

orphan_cleanup_success_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
            {adm_user_logic, [
                {'find', 3, fun(1, _, _) ->
                    #{<<"id">> => 1, <<"role_id">> => [1]}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) ->
                    {<<"admin">>, [<<"storage:cleanup">>], []}
                end}
            ]},
            {elib_param, [{'post', 1, fun(_) -> #{<<"age_days">> => 30} end}]},
            {attachment_ds, [
                {'orphan_cleanup', 1, fun(_) ->
                    {ok, #{cleaned => 5, errors => 0}}
                end}
            ]},
            {ec_cnv, [{'to_integer', 1, fun(30) -> 30 end}]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, _) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _} = adm_attach_handler:init(Req, #{
                action => orphan_cleanup, adm_user_id => 1
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(5, maps:get(cleaned, Payload)),
            ?assertEqual(0, maps:get(errors, Payload))
        end
    ).
