-module(adm_ai_agent_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc adm_ai_agent_handler 新端点 EUnit 测试
%%% 覆盖：roles（GET 全量 / POST save|delete 幂等保存删除）、
%%%       upload_avatar（multipart → Garage → URL 回显）。
%%% 权限链 mock：adm_acl:ensure_permission 直接放行（权限常量测试
%%% 见 adm_acl_tests）。
%%%===================================================================

mock_req() ->
    #{method => <<"GET">>, path => <<"/api/adm/ai_agent">>}.

with_perm_ok() ->
    [{adm_acl, [{'ensure_permission', 3, fun(_State, _Perm, _Req) -> ok end}]}].

response_ok() ->
    [
        {elib_response, [
            {'success', 2, fun(Req, Data) -> Req#{response_status => 200, data => Data} end},
            {'success', 3, fun(Req, Data, _Msg) -> Req#{response_status => 200, data => Data} end},
            {'error', 3, fun(Req, _Msg, Code) -> Req#{response_status => Code} end}
        ]}
    ].

%% ===================================================================
%% roles — GET 全量 / POST save|delete
%% ===================================================================

roles_get_returns_full_map_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {ai_agent_ds, [
                    {'roles', 0, fun() -> #{<<"doctor">> => <<"你是医生"/utf8>>} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(
                #{<<"roles">> => #{<<"doctor">> => <<"你是医生"/utf8>>}},
                maps:get(data, RespReq)
            )
        end
    ).

roles_save_calls_ds_and_returns_roles_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{
                            <<"action">> => <<"save">>,
                            <<"role_id">> => <<"lawyer">>,
                            <<"prompt">> => <<"你是律师"/utf8>>
                        }
                    end}
                ]},
                {ai_agent_ds, [
                    {'save_role', 2, fun(<<"lawyer">>, <<"你是律师"/utf8>>) -> ok end},
                    {'roles', 0, fun() ->
                        #{<<"lawyer">> => <<"你是律师"/utf8>>}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            %% 保存后回显全量角色
            ?assertEqual(
                #{<<"roles">> => #{<<"lawyer">> => <<"你是律师"/utf8>>}},
                maps:get(data, RespReq)
            )
        end
    ).

roles_delete_calls_ds_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{<<"action">> => <<"delete">>, <<"role_id">> => <<"doctor">>}
                    end}
                ]},
                {ai_agent_ds, [
                    {'delete_role', 1, fun(<<"doctor">>) -> ok end},
                    {'roles', 0, fun() -> #{} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

roles_save_without_prompt_rejected_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{<<"action">> => <<"save">>, <<"role_id">> => <<"doctor">>}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(?ERR_BAD_REQUEST, maps:get(response_status, RespReq))
        end
    ).

roles_invalid_action_rejected_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) -> #{<<"action">> => <<"rename">>} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => roles, adm_user_id => 1}),
            ?assertEqual(?ERR_BAD_REQUEST, maps:get(response_status, RespReq))
        end
    ).

%% C08（V3-13）：upload_avatar（multipart 直传）入口及用例已退役——
%% admin 前端已切 avatar_direct（register→declare→grant→PUT→finalize→publish），
%% 其 application 主体全链由 attach_direct_avatar_tests 覆盖。

%% ===================================================================
%% list — category 筛选透传
%% ===================================================================

list_category_filter_passed_to_ds_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {elib_param, [
                    {'page', 1, fun(_) -> {1, 10} end},
                    {'get', 3, fun(category, _Req, _Def) -> <<"medical">> end}
                ]},
                {ai_agent_ds, [
                    {'list', 3, fun(Page, Size, Category) ->
                        {ok, #{
                            total => 0, page => Page, size => Size, list => [], category => Category
                        }}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => list, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(
                #{total => 0, page => 1, size => 10, list => [], category => <<"medical">>},
                maps:get(data, RespReq)
            )
        end
    ).

list_without_category_passes_empty_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {elib_param, [
                    {'page', 1, fun(_) -> {1, 10} end},
                    {'get', 3, fun(category, _Req, _Def) -> <<>> end}
                ]},
                {ai_agent_ds, [
                    {'list', 3, fun(Page, Size, <<>>) ->
                        {ok, #{total => 0, page => Page, size => Size, list => []}}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} = adm_ai_agent_handler:init(Req, #{action => list, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

role_list_passes_keyword_and_status_filters_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"GET">> end}]},
                {elib_param, [
                    {'page', 1, fun(_) -> {2, 20} end},
                    {'get', 3, fun
                        (keyword, _Req, _Def) -> <<"doctor">>;
                        (status, _Req, _Def) -> <<"1">>
                    end}
                ]},
                {ai_agent_role_ds, [
                    {'page', 3, fun(2, 20, #{keyword := <<"doctor">>, status := 1}) ->
                        {ok, #{total => 1, page => 2, size => 20, list => []}}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} =
                adm_ai_agent_handler:init(Req, #{action => role_list, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(1, maps:get(total, maps:get(data, RespReq)))
        end
    ).

role_publish_passes_operator_and_version_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_param, [
                    {'post', 1, fun(_) ->
                        #{
                            <<"role_code">> => <<"doctor">>,
                            <<"version">> => <<"3">>,
                            <<"admin_uid">> => <<"99">>
                        }
                    end}
                ]},
                {ai_agent_role_ds, [
                    {'publish', 3, fun(<<"doctor">>, 3, 1) -> {ok, #{published => true}} end}
                ]}
            ] ++ response_ok(),
        fun() ->
            Req = mock_req(),
            {ok, RespReq, _} =
                adm_ai_agent_handler:init(Req, #{action => role_publish, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(
                #{published => true},
                maps:get(data, RespReq)
            )
        end
    ).

avatar_direct_declare_returns_empty_success_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_req, [
                    {'body', 2, fun(Req, []) ->
                        {ok,
                            #{
                                <<"op">> => <<"declare">>,
                                <<"object_key">> => <<"avatar-key">>,
                                <<"sha256">> => <<"checksum">>,
                                <<"size_bytes">> => 100,
                                <<"mime">> => <<"image/png">>
                            },
                            Req}
                    end}
                ]},
                {admin_avatar_session_repo, [
                    {'owned', 2, fun(<<"avatar-key">>, 1) -> ok end}
                ]},
                {attach_direct_logic, [
                    {'require_application_session', 1, fun(<<"avatar-key">>) -> ok end},
                    {'declare', 4, fun(<<"avatar-key">>, <<"checksum">>, 100, <<"image/png">>) ->
                        ok
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            {ok, Resp, _} = adm_ai_agent_handler:init(
                mock_req(), #{action => avatar_direct, adm_user_id => 1}
            ),
            ?assertEqual(200, maps:get(response_status, Resp)),
            ?assertEqual(#{}, maps:get(data, Resp))
        end
    ).

avatar_direct_status_requires_application_session_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_req, [
                    {'body', 2, fun(Req, []) ->
                        {ok, #{<<"op">> => <<"status">>, <<"object_key">> => <<"avatar-key">>}, Req}
                    end}
                ]},
                {admin_avatar_session_repo, [
                    {'owned', 2, fun(<<"avatar-key">>, 1) -> ok end}
                ]},
                {attach_direct_logic, [
                    {'require_application_session', 1, fun(<<"avatar-key">>) -> ok end},
                    {'status', 2, fun(<<"avatar-key">>, 0) ->
                        {ok, #{state => <<"CONFIRMED">>}}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            {ok, Resp, _} = adm_ai_agent_handler:init(
                mock_req(), #{action => avatar_direct, adm_user_id => 1}
            ),
            ?assertEqual(#{state => <<"CONFIRMED">>}, maps:get(data, Resp))
        end
    ).

avatar_direct_foreign_owner_is_denied_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_req, [
                    {'body', 2, fun(Req, []) ->
                        {ok, #{<<"op">> => <<"status">>, <<"object_key">> => <<"avatar-key">>}, Req}
                    end}
                ]},
                {admin_avatar_session_repo, [
                    {'owned', 2, fun(<<"avatar-key">>, 7) -> {error, not_session_owner} end}
                ]},
                {attach_direct_logic, [
                    {'require_application_session', 1, fun(_) -> error(must_not_reach_session) end}
                ]}
            ] ++ response_ok(),
        fun() ->
            {ok, Resp, _} = adm_ai_agent_handler:init(
                mock_req(), #{action => avatar_direct, adm_user_id => 7}
            ),
            ?assertEqual(?ERR_FORBIDDEN, maps:get(response_status, Resp))
        end
    ).

avatar_register_reserves_trusted_admin_before_session_test_() ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_req, [
                    {'body', 2, fun(Req, []) ->
                        {ok,
                            #{
                                <<"op">> => <<"register">>,
                                <<"application_id">> => <<"123">>,
                                <<"client_upload_id">> =>
                                    <<"5f0e9c1e-1111-4222-8333-444455556666">>,
                                <<"adm_user_id">> => 99
                            },
                            Req}
                    end}
                ]},
                {ai_agent_ds, [{'get', 1, fun(123) -> {ok, #{}} end}]},
                {elib_oss, [
                    {'get_bucket', 1, fun
                        (<<"private">>) -> <<"private">>;
                        (<<"public">>) -> <<"public">>
                    end}
                ]},
                {attach_direct_logic, [
                    {'register_application_owned', 6, fun(
                        _, _, _, <<"5f0e9c1e-1111-4222-8333-444455556666">>, _, 7
                    ) ->
                        {ok, #{object_key => <<"key">>}}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            {ok, Resp, _} = adm_ai_agent_handler:init(
                mock_req(), #{action => avatar_direct, adm_user_id => 7}
            ),
            ?assertEqual(#{object_key => <<"key">>}, maps:get(data, Resp))
        end
    ).

avatar_register_rejects_cross_domain_or_unknown_agent_test_() ->
    [
        register_denied(Extra)
     || Extra <- [
            #{<<"application_id">> => <<"123">>, <<"org_id">> => <<"9">>},
            #{<<"application_id">> => <<"999">>, <<"org_id">> => <<"0">>},
            #{<<"application_id">> => <<"5f0e9c1e-1111-4222-8333-444455556667">>}
        ]
    ].

register_denied(Extra) ->
    Mocks =
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_req, [
                    {'body', 2, fun(Req, []) ->
                        Body = maps:merge(
                            #{
                                <<"op">> => <<"register">>,
                                <<"client_upload_id">> => <<"5f0e9c1e-1111-4222-8333-444455556666">>
                            },
                            Extra
                        ),
                        {ok, Body, Req}
                    end}
                ]},
                {ai_agent_ds, [{'get', 1, fun(_) -> {error, notfound} end}]},
                {attach_direct_logic, [
                    {'register_application_owned', 6, fun(_, _, _, _, _, _) ->
                        error(unexpected_register)
                    end}
                ]}
            ] ++ response_ok(),
    ?WITH_MECKS(Mocks, fun() ->
        {ok, Resp, _} = adm_ai_agent_handler:init(mock_req(), #{
            action => avatar_direct, adm_user_id => 7
        }),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(response_status, Resp))
    end).

avatar_register_uuid_casing_test_() ->
    [
        register_case(App, Cid)
     || {App, Cid} <- [
            {<<"AA000000-0000-4000-8000-000000000001">>,
                <<"aa000000-0000-4000-8000-000000000001">>},
            {<<"aa000000-0000-4000-8000-000000000001">>, <<"AA000000-0000-4000-8000-000000000001">>}
        ]
    ].

register_case(App, Cid) ->
    ?WITH_MECKS(
        with_perm_ok() ++
            [
                {cowboy_req, [{'method', 1, fun(_) -> <<"POST">> end}]},
                {elib_req, [
                    {'body', 2, fun(Req, []) ->
                        {ok,
                            #{
                                <<"op">> => <<"register">>,
                                <<"application_id">> => App,
                                <<"client_upload_id">> => Cid
                            },
                            Req}
                    end}
                ]},
                {elib_oss, [
                    {'get_bucket', 1, fun
                        (<<"private">>) -> <<"private">>;
                        (<<"public">>) -> <<"public">>
                    end}
                ]},
                {attach_direct_logic, [
                    {'register_application_owned', 6, fun(App0, <<"0">>, <<"public">>, Cid0, _, 7) ->
                        ?assertEqual(App, App0),
                        ?assertEqual(Cid, Cid0),
                        {ok, #{object_key => <<"key">>}}
                    end}
                ]}
            ] ++ response_ok(),
        fun() ->
            {ok, Resp, _} = adm_ai_agent_handler:init(mock_req(), #{
                action => avatar_direct, adm_user_id => 7
            }),
            ?assertEqual(#{object_key => <<"key">>}, maps:get(data, Resp))
        end
    ).
