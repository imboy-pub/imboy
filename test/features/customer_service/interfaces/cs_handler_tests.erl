%%% @doc CS-02 的 handler 套件（真 cowboy 监听器 + 真 HTTP + **meck 的 facade**；
%%% 零 DB——纪律：不跑任何数据库测试）。
%%%
%%% 覆盖（CS-02-A01/A02/A03 的 HTTP 面 + CS-01 审查观察项的回归）：
%%%   * 五类 principal 的**真请求**正例与负例（401/403 的逐类拒绝面）；
%%%   * **必填键前置结构化校验**：缺 `client_msg_id`/`key_ref`/`expected_version`
%%%     → 422 结构化错误，绝不把 application 的无默认 maps:get badarg 泄漏成 500；
%%%   * 服务端派生键（actor_user_id/at/business_identity_id/contact_id）客户端
%%%     提供即 400；
%%%   * offboarding 降级双通道：HTTP **409** + envelope `offboarding_required`
%%%     （A0 客户端契约基准，经 enterprise_business_facade:list_messages 真源）；
%%%   * TSID 出站一律 JSON string；
%%%   * 双管理面：平台动作与租户动作打到**同一 facade 用例**（不复制逻辑）；
%%%   * 错误映射：400/401/403/404/405/409/422 明确可区分。
-module(cs_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(ORG, 7001001).
-define(WS, 90001).
-define(UID, 424242).
-define(ADM, 77).
-define(SESSION, 555000111).
-define(CONVERSATION, 666000222).
-define(CONTACT, 3131).
-define(IDENTITY, 515151).

handler_test_() ->
    {foreach,
        fun() ->
            %% 纯套件也要真 socket：起 cowboy/ranch（不连任何库）。
            {ok, _} = application:ensure_all_started(cowboy),
            meck:new(customer_service_facade, [passthrough]),
            meck:new(enterprise_business_facade, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            meck:unload(enterprise_business_facade),
            cs_fake_facts:clear(),
            persistent_term:erase({cs02_test, captured_contact}),
            ok
        end,
        [
            fun visitor_flow_tests/1,
            fun seat_flow_tests/1,
            fun governance_flow_tests/1,
            fun platform_flow_tests/1,
            fun contract_tests/1
        ]}.

%% ===================================================================
%% 访客/门店面：queue（shop key）、消息（visit）、缺必填 422
%% ===================================================================

visitor_flow_tests(_) ->
    [
        {"shop key opens a session via POST /api/v1/cs/sessions/queue (200)", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_Org, _P) ->
                {ok, #{organization_id => ?ORG, status => active}}
            end),
            meck:expect(customer_service_facade, open_session, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?CONTACT, maps:get(contact_id, Params)),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                ?assert(is_integer(maps:get(at, Params))),
                {ok, #{id => ?SESSION, status => queued, contact_id => ?CONTACT}}
            end),
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/queue">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"contact_id">> => ?CONTACT,
                        <<"conversation_id">> => ?CONVERSATION
                    },
                    #{<<"x-cs-shop-key">> => <<"sk">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(0, ?S:code(Resp)),
                ?assertEqual(
                    integer_to_binary(?SESSION),
                    maps:get(<<"id">>, ?S:payload(Resp))
                )
            end)
        end},

        {"queue without shop key header is 401 and never reaches the use case", fun() ->
            meck:expect(customer_service_facade, open_session, fun(_O, _P) ->
                {ok, #{}}
            end),
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                %% 组内各用例共享同一 meck 实例：请求前清历史，只看本次。
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/queue">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"contact_id">> => 1,
                        <<"conversation_id">> => 2
                    }
                ),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, open_session, '_'))
            end)
        end},

        {"client-supplied actor_user_id is a 400 (server-derived key)", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG}}
            end),
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/queue">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"contact_id">> => 1,
                        <<"conversation_id">> => 2,
                        <<"actor_user_id">> => 9
                    },
                    #{<<"x-cs-shop-key">> => <<"sk">>}
                ),
                ?assertEqual(400, ?S:status(Resp))
            end)
        end},

        {"missing workspace_id is 422", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG}}
            end),
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/queue">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"contact_id">> => 1,
                        <<"conversation_id">> => 2
                    },
                    #{<<"x-cs-shop-key">> => <<"sk">>}
                ),
                ?assertEqual(422, ?S:status(Resp))
            end)
        end},

        {"missing organization_id on frozen path is 400", fun() ->
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/queue">>,
                    #{
                        <<"workspace_id">> => ?WS,
                        <<"contact_id">> => 1,
                        <<"conversation_id">> => 2
                    },
                    #{<<"x-cs-shop-key">> => <<"sk">>}
                ),
                ?assertEqual(400, ?S:status(Resp))
            end)
        end},

        {"GET on a POST-only path is 405", fun() ->
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, <<"/api/v1/cs/sessions/queue">>, <<>>),
                ?assertEqual(405, ?S:status(Resp))
            end)
        end},

        {"malformed JSON body is 400", fun() ->
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/queue">>,
                    <<"{not-json">>,
                    #{
                        <<"x-cs-shop-key">> => <<"sk">>,
                        <<"content-type">> => <<"application/json">>
                    }
                ),
                ?assertEqual(400, ?S:status(Resp))
            end)
        end},

        %% CS-01 审查观察项回归：application 对 client_msg_id/key_ref 用无默认
        %% maps:get——handler 必须前置结构化校验，缺失是 422 不是 500。
        {"missing key_ref is a structured 422, never a 500 badarg", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG, contact_id => ?CONTACT, scope => visit}}
            end),
            meck:expect(customer_service_facade, append_session_message, fun(_O, _P) ->
                erlang:error(badarg_leaked_to_application)
            end),
            ?S:with_listener(tenant, session_messages, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"client_msg_id">> => <<"cmid-1">>,
                        <<"body">> => <<"hi">>
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(
                    <<"missing_param.key_ref">>, ?S:msg(Resp)
                ),
                ?assertNot(
                    meck:called(customer_service_facade, append_session_message, '_')
                )
            end)
        end},

        {"missing client_msg_id is a structured 422", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG, contact_id => ?CONTACT, scope => visit}}
            end),
            ?S:with_listener(tenant, session_messages, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"key_ref">> => <<"kr-1">>,
                        <<"body">> => <<"hi">>
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(<<"missing_param.client_msg_id">>, ?S:msg(Resp))
            end)
        end},

        {"visitor message injects contact from token scope (client contact_id is 400)", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG, contact_id => ?CONTACT, scope => visit}}
            end),
            ?S:with_listener(tenant, session_messages, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"client_msg_id">> => <<"cmid-2">>,
                        <<"key_ref">> => <<"kr-2">>,
                        <<"body">> => <<"hi">>,
                        <<"contact_id">> => 999999
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(400, ?S:status(Resp))
            end),
            %% 正例对照：不带 contact_id 时由 token 作用域派生。
            %% （组内共享同一 meck 实例：替换前面负例安装的错误桩。）
            persistent_term:erase({cs02_test, captured_contact}),
            meck:expect(customer_service_facade, append_session_message, fun(Org, Params) ->
                persistent_term:put(
                    {cs02_test, captured_contact},
                    maps:get(contact_id, Params, undefined)
                ),
                {ok, #{id => 77, organization_id => Org}}
            end),
            ?S:with_listener(tenant, session_messages, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"client_msg_id">> => <<"cmid-3">>,
                        <<"key_ref">> => <<"kr-3">>,
                        <<"body">> => <<"hi">>
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(
                    ?CONTACT, persistent_term:get({cs02_test, captured_contact}, undefined)
                )
            end)
        end}
    ].

%% ===================================================================
%% 坐席面：claim（suspended seat 403 且零用例调用）、transfer/close、企业消息列表
%% ===================================================================

seat_inject() ->
    #{
        auth_facts => cs_fake_facts,
        current_uid => ?UID
    }.

seat_flow_tests(_) ->
    [
        {"A04 suspended seat actor gets 403 and claim is never invoked", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => false}}
            end),
            meck:expect(customer_service_facade, claim, fun(_O, _P) ->
                {ok, #{should_not_happen => true}}
            end),
            ?S:with_listener(tenant, session_claim, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/claim">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"expected_version">> => 1
                    },
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertEqual(<<"seat_disabled">>, ?S:msg(Resp)),
                ?assertNot(meck:called(customer_service_facade, claim, '_'))
            end)
        end},

        {"seat claims own session (business identity is server-derived)", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            meck:expect(customer_service_facade, claim, fun(Org, Params) ->
                ?assertEqual(?IDENTITY, maps:get(business_identity_id, Params)),
                {ok, #{
                    id => ?SESSION,
                    status => active,
                    business_identity_id => ?IDENTITY
                }}
            end),
            ?S:with_listener(tenant, session_claim, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/claim">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"expected_version">> => 1
                    },
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(<<"active">>, maps:get(<<"status">>, ?S:payload(Resp)))
            end)
        end},

        {"missing expected_version on claim is a structured 422", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            ?S:with_listener(tenant, session_claim, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/claim">>,
                    #{<<"organization_id">> => ?ORG, <<"workspace_id">> => ?WS},
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(<<"missing_param.expected_version">>, ?S:msg(Resp))
            end)
        end},

        {"A0 client contract: seat lists enterprise conversation messages (200)", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            meck:expect(enterprise_business_facade, list_messages, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?CONVERSATION, maps:get(conversation_id, Params)),
                {ok, [#{id => 42, body => <<"m1">>}]}
            end),
            ?S:with_listener(tenant, conversation_messages, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/enterprise/conversations/", (int_bin(?CONVERSATION))/binary,
                        "/messages?after_id=&organization_id=", (int_bin(?ORG))/binary,
                        "&workspace_id=", (int_bin(?WS))/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                [Msg] = ?S:payload(Resp),
                ?assertEqual(<<"42">>, maps:get(<<"id">>, Msg))
            end)
        end},

        {"A0 client contract: assignee change downgrades with 409 + offboarding_required", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            meck:expect(enterprise_business_facade, list_messages, fun(_O, _P) ->
                {error, {assignee_change_requires_offboarding, ?IDENTITY}}
            end),
            ?S:with_listener(tenant, conversation_messages, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/enterprise/conversations/", (int_bin(?CONVERSATION))/binary,
                        "/messages?organization_id=", (int_bin(?ORG))/binary, "&workspace_id=",
                        (int_bin(?WS))/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(409, ?S:status(Resp)),
                ?assertEqual(<<"offboarding_required">>, ?S:msg(Resp))
            end)
        end},

        {"cross-org session id is 404 (no existence oracle)", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            meck:expect(customer_service_facade, close, fun(_O, _P) ->
                {error, not_found}
            end),
            ?S:with_listener(tenant, session_close, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/close">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"expected_version">> => 3
                    },
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(404, ?S:status(Resp))
            end)
        end},

        {"seat JWT missing is 401 at handler (no current_uid injected)", fun() ->
            ?S:with_listener(tenant, session_claim, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/claim">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"expected_version">> => 1
                    }
                ),
                ?assertEqual(401, ?S:status(Resp))
            end)
        end}
    ].

%% ===================================================================
%% 治理面：seat suspend / shop key create
%% ===================================================================

governance_flow_tests(_) ->
    [
        {"governance suspends a seat (200, TSID string in response)", fun() ->
            cs_fake_facts:set(owner_facts()),
            AdminInject = #{auth_facts => cs_fake_facts, current_uid => ?UID},
            meck:expect(customer_service_facade, suspend_seat, fun(Org, Params) ->
                ?assertEqual(?IDENTITY, maps:get(business_identity_id, Params)),
                {ok, #{business_identity_id => ?IDENTITY, enabled => false}}
            end),
            ?S:with_listener(tenant, seat_suspend, AdminInject, fun(Port) ->
                Path = ?S:path(tenant, seat_suspend, #{
                    org_id => ?ORG, id => ?IDENTITY
                }),
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    Path,
                    #{<<"workspace_id">> => ?WS},
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(
                    integer_to_binary(?IDENTITY),
                    maps:get(<<"business_identity_id">>, ?S:payload(Resp))
                )
            end)
        end},

        {"governance requires owner/admin role (member is 403)", fun() ->
            cs_fake_facts:set(member_facts()),
            ?S:with_listener(
                tenant,
                seat_suspend,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path = ?S:path(tenant, seat_suspend, #{
                        org_id => ?ORG, id => ?IDENTITY
                    }),
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        Path,
                        #{<<"workspace_id">> => ?WS},
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(403, ?S:status(Resp)),
                    ?assertEqual(<<"governance_insufficient">>, ?S:msg(Resp))
                end
            )
        end},

        {"governance creates a shop key (200; plaintext only in this response)", fun() ->
            cs_fake_facts:set(owner_facts()),
            meck:expect(customer_service_facade, create_shop_key, fun(Org, Params) ->
                ?assertEqual(<<"sk-plain">>, maps:get(secret, Params)),
                ?assertEqual(?UID, maps:get(created_by_user_id, Params)),
                {ok, #{id => 88, organization_id => Org, status => active}}
            end),
            ?S:with_listener(
                tenant,
                shop_key_create,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path = ?S:path(tenant, shop_key_create, #{org_id => ?ORG}),
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        Path,
                        #{
                            <<"workspace_id">> => ?WS,
                            <<"secret">> => <<"sk-plain">>,
                            <<"display_hint">> => <<"shop-12">>
                        },
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    ?assertEqual(<<"88">>, maps:get(<<"id">>, ?S:payload(Resp)))
                end
            )
        end}
    ].

%% ===================================================================
%% 平台面：与租户面共用 facade 用例（A02），显式 Org/Workspace
%% ===================================================================

platform_flow_tests(_) ->
    [
        {"platform admin reads a session via the same facade use case (200)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            meck:expect(customer_service_facade, fetch_session, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                {ok, #{id => ?SESSION, status => active}}
            end),
            ?S:with_listener(platform, p_session, platform_inject(), fun(Port) ->
                Path = ?S:path(platform, p_session, #{
                    org_id => ?ORG, id => ?SESSION
                }),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<Path/binary, "?workspace_id=", (int_bin(?WS))/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(<<"active">>, maps:get(<<"status">>, ?S:payload(Resp)))
            end)
        end},

        {"platform close and tenant close hit the same facade function (A02)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            meck:expect(customer_service_facade, close, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assert(is_integer(maps:get(at, Params))),
                {ok, #{id => ?SESSION, status => closed}}
            end),
            ?S:with_listener(platform, p_session_close, platform_inject(), fun(Port) ->
                Path = ?S:path(platform, p_session_close, #{
                    org_id => ?ORG, id => ?SESSION
                }),
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    Path,
                    #{<<"workspace_id">> => ?WS, <<"expected_version">> => 2},
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(<<"closed">>, maps:get(<<"status">>, ?S:payload(Resp)))
            end)
        end},

        {"platform read permission cannot satisfy write route (403)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            ?S:with_listener(platform, p_session_close, platform_inject(), fun(Port) ->
                Path = ?S:path(platform, p_session_close, #{
                    org_id => ?ORG, id => ?SESSION
                }),
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    Path,
                    #{<<"workspace_id">> => ?WS, <<"expected_version">> => 2},
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp))
            end)
        end},

        {"platform route without workspace_id is 422 (tenant condition explicit)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            ?S:with_listener(platform, p_session, platform_inject(), fun(Port) ->
                Path = ?S:path(platform, p_session, #{
                    org_id => ?ORG, id => ?SESSION
                }),
                Resp = ?S:request(Port, <<"GET">>, Path, <<>>),
                ?assertEqual(422, ?S:status(Resp))
            end)
        end}
    ].

%% ===================================================================
%% 契约细节：TSID 出站 string、表外键忽略、错误映射
%% ===================================================================

contract_tests(_) ->
    [
        {"unknown params are ignored (whitelist projection)", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG}}
            end),
            meck:expect(customer_service_facade, open_session, fun(_O, Params) ->
                ?assertNot(is_map_key(admin_bypass, Params)),
                {ok, #{id => 1, status => queued}}
            end),
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/queue">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"contact_id">> => 1,
                        <<"conversation_id">> => 2,
                        <<"admin_bypass">> => true
                    },
                    #{<<"x-cs-shop-key">> => <<"sk">>}
                ),
                ?assertEqual(200, ?S:status(Resp))
            end)
        end},

        {"error mapping: 409 for stale CAS version", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            meck:expect(customer_service_facade, transfer, fun(_O, _P) ->
                {error, {stale_version, 7}}
            end),
            ?S:with_listener(tenant, session_transfer, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/transfer">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"to_identity_id">> => 999,
                        <<"expected_version">> => 1
                    },
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(409, ?S:status(Resp)),
                ?assertEqual(<<"stale_version">>, ?S:msg(Resp))
            end)
        end},

        {"error mapping: 422 for invalid rating value", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG, contact_id => ?CONTACT, scope => visit}}
            end),
            meck:expect(customer_service_facade, rate, fun(_O, _P) ->
                {error, {invalid_rating, 9}}
            end),
            ?S:with_listener(tenant, session_rating, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/rating">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"rating">> => 9,
                        <<"expected_version">> => 4
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(<<"invalid_rating">>, ?S:msg(Resp))
            end)
        end},

        {"encode_entity keeps TSID keys as JSON strings and non-TSID ids intact", fun() ->
            Out = cs_http:encode_entity(#{
                id => ?SESSION,
                session_id => 12,
                contact_id => 13,
                client_msg_id => <<"cmid">>,
                key_ref => <<"kr">>,
                rating => 5
            }),
            ?assertEqual(<<"12">>, maps:get(session_id, Out)),
            ?assertEqual(<<"13">>, maps:get(contact_id, Out)),
            ?assertEqual(<<"cmid">>, maps:get(client_msg_id, Out)),
            ?assertEqual(<<"kr">>, maps:get(key_ref, Out)),
            ?assertEqual(5, maps:get(rating, Out))
        end},

        {"credential surface paths are exactly the visitor/shop-key actions", fun() ->
            %% 正例：访客/门店动作的路径在 credential 面上。
            ?assert(cs_http:is_credential_surface_path(<<"/api/v1/cs/sessions">>)),
            ?assert(cs_http:is_credential_surface_path(<<"/api/v1/cs/sessions/queue">>)),
            ?assert(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/cs/sessions/123/messages">>
                )
            ),
            ?assert(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/cs/sessions/123/rating">>
                )
            ),
            %% 负例：坐席/治理路径不在 credential 面（照常走中间件 JWT 门）。
            ?assertNot(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/cs/sessions/123/claim">>
                )
            ),
            ?assertNot(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/cs/sessions/123/close">>
                )
            ),
            ?assertNot(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/cs/organizations/1/seats">>
                )
            ),
            ?assertNot(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/enterprise/conversations/1/messages">>
                )
            ),
            ?assertNot(cs_http:is_credential_surface_path(<<"/api/v1/cs/sessionz">>))
        end}
    ].

%% ===================================================================
%% 夹具
%% ===================================================================

seat_facts() ->
    #{
        organization_id => ?ORG,
        member => #{user_id => ?UID, status => active, governance_roles => []},
        assignments => [
            #{
                business_identity_id => ?IDENTITY,
                user_id => ?UID,
                organization_id => ?ORG,
                function_key => <<"customer_service">>,
                status => active,
                version => 1
            }
        ],
        permissions => [<<"conversation.write">>, <<"conversation.read">>]
    }.

owner_facts() ->
    #{
        organization_id => ?ORG,
        member => #{
            user_id => ?UID,
            status => active,
            role => owner,
            governance_roles => [<<"owner">>]
        },
        assignments => [],
        permissions => [<<"org.manage">>]
    }.

member_facts() ->
    #{
        organization_id => ?ORG,
        member => #{
            user_id => ?UID,
            status => active,
            role => member,
            governance_roles => []
        },
        assignments => [],
        permissions => []
    }.

platform_inject() ->
    #{auth_facts => cs_fake_facts, adm_user_id => ?ADM}.

int_bin(N) when is_integer(N) ->
    integer_to_binary(N).
