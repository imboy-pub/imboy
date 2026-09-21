%%% @doc CS-02 的 handler 套件（真 cowboy 监听器 + 真 HTTP + **meck 的 facade**；
%%% 零 DB——纪律：不跑任何数据库测试）。
%%%
%%% 覆盖（CS-02-A01/A02/A03 的 HTTP 面 + CS-01 审查观察项的回归）：
%%%   * 五类 principal 的**真请求**正例与负例（401/403 的逐类拒绝面）；
%%%   * **必填键前置结构化校验**：缺 `client_msg_id`/`expected_version`
%%%     → 422 结构化错误，绝不把 application 的无默认 maps:get badarg 泄漏成 500；
%%%   * F6（RULING-2026-09-15 §七）：主密钥材料不经 HTTP 面——客户端提交
%%%     `key_ref` 即 422（unexpected_argument.key_ref）；缺 key_ref 正常放行，
%%%     密钥由服务端装配；
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
            fun provisioning_flow_tests/1,
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
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
                        "/sessions/queue">>,
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
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
                        "/sessions/queue">>,
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
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
                        "/sessions/queue">>,
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
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
                        "/sessions/queue">>,
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

        %% T-2 后 org 显式在路径：非法 TSID 的 org 段在解析层即 400
        %%（申报参数 organization_id 已不再是本面的 org 来源）。
        {"malformed org_id in the path is 400", fun() ->
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/organizations/notatsid/sessions/queue">>,
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

        %% CSB-02R：/sessions/queue 已按 method 分流（GET=坐席队列），
        %% 「POST-only 405」样本改用 claim 路径（仍是 POST-only）。
        {"GET on a POST-only path is 405", fun() ->
            ?S:with_listener(tenant, session_claim, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary, "/sessions/",
                        (integer_to_binary(555000111))/binary, "/claim">>,
                    <<>>
                ),
                ?assertEqual(405, ?S:status(Resp))
            end)
        end},

        {"malformed JSON body is 400", fun() ->
            ?S:with_listener(tenant, session_queue, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
                        "/sessions/queue">>,
                    <<"{not-json">>,
                    #{
                        <<"x-cs-shop-key">> => <<"sk">>,
                        <<"content-type">> => <<"application/json">>
                    }
                ),
                ?assertEqual(400, ?S:status(Resp))
            end)
        end},

        %% F6（RULING-2026-09-15 §七）：主密钥材料不得经 HTTP/JSON 面出现。
        %% 动作表已删除 `key_ref` 参数——客户端显式提交是结构化 422
        %% （unexpected_argument.key_ref，FND-5 body_cipher 同款先例），
        %% 且请求不抵达 application；不带 key_ref 的请求正常抵达 application，
        %% 密钥由服务端经 `imboy.eb_enterprise_keyring` 装配。
        {"client-submitted key_ref is a structured 422 and never reaches the use case", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG, contact_id => ?CONTACT, scope => visit}}
            end),
            meck:expect(customer_service_facade, append_session_message, fun(_O, _P) ->
                erlang:error(key_ref_leaked_to_application)
            end),
            ?S:with_listener(tenant, session_messages, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"client_msg_id">> => <<"cmid-keyref">>,
                        <<"body">> => <<"hi">>,
                        <<"key_ref">> => <<"attacker-chosen-key-ref">>
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(<<"unexpected_argument.key_ref">>, ?S:msg(Resp)),
                ?assertNot(
                    meck:called(customer_service_facade, append_session_message, '_')
                )
            end)
        end},

        {"session_messages without key_ref reaches the use case (server-side key assembly)",
            fun() ->
                meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                    {ok, #{organization_id => ?ORG, contact_id => ?CONTACT, scope => visit}}
                end),
                meck:expect(customer_service_facade, append_session_message, fun(Org, Params) ->
                    %% HTTP 面不再承载 key_ref：facade 收到的 Params 里没有它。
                    ?assertNot(is_map_key(key_ref, Params)),
                    {ok, #{id => 88, organization_id => Org}}
                end),
                ?S:with_listener(tenant, session_messages, #{auth_facts => cs_fake_facts}, fun(
                    Port
                ) ->
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                        #{
                            <<"organization_id">> => ?ORG,
                            <<"workspace_id">> => ?WS,
                            <<"client_msg_id">> => <<"cmid-no-keyref">>,
                            <<"body">> => <<"hi">>
                        },
                        #{<<"x-cs-visit-token">> => <<"tok">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    ?assert(
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
                        <<"body">> => <<"hi">>
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(
                    ?CONTACT, persistent_term:get({cs02_test, captured_contact}, undefined)
                )
            end)
        end},

        %% DF-6 回归：访客评分写路径的 `at` 必须是 epoch 秒——rating_at 走
        %% `to_timestamp`（epoch 秒），毫秒输入写成约 5.8 万年后（同 claim/close）。
        {"DF-6: visitor rating derives the server clock in epoch seconds", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG, contact_id => ?CONTACT, scope => visit}}
            end),
            meck:expect(customer_service_facade, rate, fun(_O, Params) ->
                persistent_term:put({df6_clock, rating_at}, maps:get(at, Params, undefined)),
                {ok, #{id => ?SESSION, status => closed, rating => 5}}
            end),
            ?S:with_listener(tenant, session_rating, #{auth_facts => cs_fake_facts}, fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/sessions/", (int_bin(?SESSION))/binary, "/rating">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"rating">> => 5,
                        <<"expected_version">> => 4
                    },
                    #{<<"x-cs-visit-token">> => <<"tok">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                At = persistent_term:get({df6_clock, rating_at}, undefined),
                ?assert(is_integer(At)),
                ?assert(abs(At - os:system_time(second)) < 60)
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
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/claim">>,
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
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/claim">>,
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
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/claim">>,
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
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/close">>,
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
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/claim">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"expected_version">> => 1
                    }
                ),
                ?assertEqual(401, ?S:status(Resp))
            end)
        end},

        %% DF-6 回归：claim/close 写路径的 `at` 必须是 epoch 秒——
        %% cs_pg_session 的 claimed_at/closed_at/updated_at 走 `to_timestamp`
        %% （epoch 秒），毫秒输入会把时间戳写成约 5.8 万年后（实证残留行
        %% closed_at=58691-02-01）。动作表 clock_unit => second 在 facade 边界
        %% 锁死量纲（DF-4 吊销族同款机制）。`at` 经 persistent_term 捕获后在
        %% 测试进程断言（meck fun 跑在 cowboy 请求进程，组内断言先例：
        %% captured_contact），避免请求进程崩溃把用例变成 timeout/cancelled。
        {"DF-6: seat claim derives the server clock in epoch seconds", fun() ->
            cs_fake_facts:set(seat_facts()),
            persistent_term:erase({df6_clock, claim_at}),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            meck:expect(customer_service_facade, claim, fun(_O, Params) ->
                persistent_term:put({df6_clock, claim_at}, maps:get(at, Params, undefined)),
                {ok, #{id => ?SESSION, status => active}}
            end),
            ?S:with_listener(tenant, session_claim, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/claim">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"expected_version">> => 1
                    },
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                At = persistent_term:get({df6_clock, claim_at}, undefined),
                ?assert(is_integer(At)),
                ?assert(abs(At - os:system_time(second)) < 60)
            end)
        end},

        {"DF-6: seat close derives the server clock in epoch seconds", fun() ->
            cs_fake_facts:set(seat_facts()),
            persistent_term:erase({df6_clock, close_at}),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            meck:expect(customer_service_facade, close, fun(_O, Params) ->
                persistent_term:put({df6_clock, close_at}, maps:get(at, Params, undefined)),
                {ok, #{id => ?SESSION, status => closed}}
            end),
            ?S:with_listener(tenant, session_close, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/close">>,
                    #{
                        <<"organization_id">> => ?ORG,
                        <<"workspace_id">> => ?WS,
                        <<"expected_version">> => 3
                    },
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                At = persistent_term:get({df6_clock, close_at}, undefined),
                ?assert(is_integer(At)),
                ?assert(abs(At - os:system_time(second)) < 60)
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
                shop_key_list,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path = ?S:path(tenant, shop_key_list, #{org_id => ?ORG}),
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
        end},

        {"C2: governance lists shop keys via GET (200; digest never in payload)", fun() ->
            cs_fake_facts:set(owner_facts()),
            meck:expect(customer_service_facade, list_shop_keys, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                {ok, #{
                    shop_keys => [
                        #{
                            id => 88,
                            display_hint => <<"shop-12">>,
                            status => active,
                            created_at => 1700000000,
                            updated_at => 1700000000
                        }
                    ],
                    next_after_id => undefined
                }}
            end),
            ?S:with_listener(
                tenant,
                shop_key_list,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path =
                        <<
                            (?S:path(tenant, shop_key_list, #{org_id => ?ORG}))/binary,
                            "?workspace_id=",
                            (int_bin(?WS))/binary
                        >>,
                    Resp = ?S:request(
                        Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    Payload = ?S:payload(Resp),
                    Row = hd(maps:get(<<"shop_keys">>, Payload)),
                    ?assertEqual(<<"88">>, maps:get(<<"id">>, Row)),
                    %% 红线：digest/secret 永不进响应。
                    ?assertNot(is_map_key(<<"key_digest">>, Row)),
                    ?assertNot(is_map_key(<<"secret">>, Row)),
                    ?assertEqual(null, maps:get(<<"next_after_id">>, Payload))
                end
            )
        end},

        {"C3: governance lists visit tokens (200; token_digest never in payload)", fun() ->
            cs_fake_facts:set(owner_facts()),
            meck:expect(customer_service_facade, list_visit_tokens, fun(Org, _Params) ->
                ?assertEqual(?ORG, Org),
                {ok, #{
                    visit_tokens => [
                        #{
                            id => 91,
                            contact_id => ?CONTACT,
                            expires_at => 1800000000,
                            revoked_at => undefined,
                            created_at => 1700000000
                        }
                    ],
                    next_after_id => undefined
                }}
            end),
            ?S:with_listener(
                tenant,
                visit_token_list,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path =
                        <<
                            (?S:path(tenant, visit_token_list, #{org_id => ?ORG}))/binary,
                            "?workspace_id=",
                            (int_bin(?WS))/binary
                        >>,
                    Resp = ?S:request(
                        Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    Row = hd(maps:get(<<"visit_tokens">>, ?S:payload(Resp))),
                    ?assertEqual(<<"91">>, maps:get(<<"id">>, Row)),
                    ?assertEqual(integer_to_binary(?CONTACT), maps:get(<<"contact_id">>, Row)),
                    ?assertNot(is_map_key(<<"token_digest">>, Row))
                end
            )
        end},

        %% DF-4 回归：吊销写路径的 `at` 必须是 epoch 秒——store 的
        %% `to_timestamp` 以秒为量纲，毫秒输入会把 revoked_at 写成约 5.8 万年
        %% 后，`cs_session:assert_visitor_scope` 的吊销判定永不命中（写路径
        %% fail-open：治理面 revoke 200 后访客发消息仍 200）。这里在 facade
        %% 边界（真 cowboy 监听 + 动作表 clock_unit => second）锁死量纲。
        {"DF-4: visit-token revoke derives the server clock in epoch seconds", fun() ->
            cs_fake_facts:set(owner_facts()),
            meck:expect(customer_service_facade, revoke_visit_token, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(91, maps:get(id, Params)),
                ?assert(is_integer(maps:get(at, Params))),
                At = maps:get(at, Params),
                NowSec = os:system_time(second),
                ?assert(abs(At - NowSec) < 60),
                ok
            end),
            ?S:with_listener(
                tenant,
                visit_token_revoke,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path = ?S:path(tenant, visit_token_revoke, #{org_id => ?ORG, id => 91}),
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        Path,
                        #{<<"workspace_id">> => ?WS},
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp))
                end
            )
        end},

        {"DF-4: shop-key revoke derives the server clock in epoch seconds", fun() ->
            cs_fake_facts:set(owner_facts()),
            meck:expect(customer_service_facade, revoke_shop_key, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assert(is_integer(maps:get(at, Params))),
                At = maps:get(at, Params),
                NowSec = os:system_time(second),
                ?assert(abs(At - NowSec) < 60),
                ok
            end),
            ?S:with_listener(
                tenant,
                shop_key_revoke,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path = ?S:path(tenant, shop_key_revoke, #{org_id => ?ORG, id => 88}),
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        Path,
                        #{<<"workspace_id">> => ?WS},
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp))
                end
            )
        end},

        {"C4: tenant seats list pushes after_id/limit through (200, paged shape)", fun() ->
            cs_fake_facts:set(owner_facts()),
            meck:expect(customer_service_facade, list_dispatchable_seats, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(<<"123456789012345">>, maps:get(after_id, Params)),
                ?assertEqual(<<"2">>, maps:get(limit, Params)),
                {ok, #{
                    seats => [
                        #{
                            business_identity_id => 515151,
                            function_key => customer_service,
                            enabled => true,
                            max_concurrent => 1,
                            active_count => 0
                        }
                    ],
                    next_after_id => 515151
                }}
            end),
            ?S:with_listener(
                tenant,
                seats,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path =
                        <<
                            (?S:path(tenant, seats, #{org_id => ?ORG}))/binary,
                            "?workspace_id=",
                            (int_bin(?WS))/binary,
                            "&after_id=123456789012345&limit=2"
                        >>,
                    Resp = ?S:request(
                        Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    Payload = ?S:payload(Resp),
                    Row = hd(maps:get(<<"seats">>, Payload)),
                    ?assertEqual(<<"515151">>, maps:get(<<"business_identity_id">>, Row)),
                    ?assertEqual(<<"515151">>, maps:get(<<"next_after_id">>, Payload))
                end
            )
        end},

        {"C2 list requires owner/admin (member is 403)", fun() ->
            cs_fake_facts:set(member_facts()),
            ?S:with_listener(
                tenant,
                shop_key_list,
                #{auth_facts => cs_fake_facts, current_uid => ?UID},
                fun(Port) ->
                    Path =
                        <<
                            (?S:path(tenant, shop_key_list, #{org_id => ?ORG}))/binary,
                            "?workspace_id=",
                            (int_bin(?WS))/binary
                        >>,
                    Resp = ?S:request(
                        Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(403, ?S:status(Resp))
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
        end},

        {"C1: platform lists sessions (200; whitelist payload; params reach the facade)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            meck:expect(customer_service_facade, list_sessions, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                %% 查询参数按冻结口径抵达 application：status/after_id/limit
                %% 均为 binary 原文（白名单/TSID/范围校验在 application，非法
                %% 取值 422 原子）。
                ?assertEqual(<<"active">>, maps:get(status, Params)),
                ?assertEqual(<<"123456789012345">>, maps:get(after_id, Params)),
                ?assertEqual(<<"2">>, maps:get(limit, Params)),
                %% meck 模拟的是 application 的**输出合同**：投影白名单
                %% 由 cs_session_app 裁剪（防泄漏键集断言在
                %% cs_list_contract_tests），handler 只负责出站编码。
                {ok, #{
                    sessions => [
                        #{
                            id => ?SESSION,
                            organization_id => ?ORG,
                            workspace_id => ?WS,
                            contact_id => ?CONTACT,
                            business_identity_id => undefined,
                            status => active,
                            rating => undefined,
                            queued_at => 1700000000,
                            claimed_at => 1700000001,
                            closed_at => undefined,
                            version => 2
                        }
                    ],
                    next_after_id => undefined
                }}
            end),
            ?S:with_listener(platform, p_session_list, platform_inject(), fun(Port) ->
                Path =
                    <<
                        (?S:path(platform, p_session_list, #{org_id => ?ORG}))/binary,
                        "?workspace_id=",
                        (int_bin(?WS))/binary,
                        "&status=active"
                        "&after_id=123456789012345&limit=2"
                    >>,
                Resp = ?S:request(
                    Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Payload = ?S:payload(Resp),
                Row = hd(maps:get(<<"sessions">>, Payload)),
                ?assertEqual(integer_to_binary(?SESSION), maps:get(<<"id">>, Row)),
                ?assertEqual(<<"active">>, maps:get(<<"status">>, Row)),
                %% 红线：visit_token_id / close_reason 永不进投影。
                ?assertNot(is_map_key(<<"visit_token_id">>, Row)),
                ?assertNot(is_map_key(<<"close_reason">>, Row)),
                ?assertEqual(null, maps:get(<<"next_after_id">>, Payload))
            end)
        end},

        {"C1: platform session list requires customer_service:read (403)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            ?S:with_listener(platform, p_session_list, platform_inject(), fun(Port) ->
                Path =
                    <<
                        (?S:path(platform, p_session_list, #{org_id => ?ORG}))/binary,
                        "?workspace_id=",
                        (int_bin(?WS))/binary
                    >>,
                Resp = ?S:request(
                    Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp))
            end)
        end},

        {"C1: platform session list without workspace_id is 422", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            ?S:with_listener(platform, p_session_list, platform_inject(), fun(Port) ->
                Path = ?S:path(platform, p_session_list, #{org_id => ?ORG}),
                Resp = ?S:request(
                    Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(422, ?S:status(Resp))
            end)
        end},

        {"widget installations list uses explicit org/workspace and read permission", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            meck:expect(customer_service_facade, list_widget_installations, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                {ok, #{
                    installations => [
                        #{
                            id => 91,
                            public_widget_id => <<"wgt_pub_test">>,
                            status => active
                        }
                    ],
                    next_after_id => undefined
                }}
            end),
            ?S:with_listener(platform, p_widget_installations, platform_inject(), fun(Port) ->
                Path =
                    <<
                        "/api/adm/customer-service/widget-installations?organization_id=",
                        (int_bin(?ORG))/binary,
                        "&workspace_id=",
                        (int_bin(?WS))/binary
                    >>,
                Resp = ?S:request(
                    Port, <<"GET">>, Path, <<>>, #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                [Installation] = maps:get(<<"installations">>, ?S:payload(Resp)),
                ?assertEqual(<<"wgt_pub_test">>, maps:get(<<"public_widget_id">>, Installation)),
                ?assertNot(is_map_key(<<"shop_key">>, Installation)),
                ?assertNot(is_map_key(<<"secret">>, Installation))
            end)
        end},

        {"widget installation create requires write permission", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            meck:expect(customer_service_facade, create_widget_installation, fun(_Org, _Params) ->
                erlang:error(write_use_case_reached_with_read_permission)
            end),
            ?S:with_listener(platform, p_widget_installations, platform_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/adm/customer-service/widget-installations">>,
                    widget_installation_body(),
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertNot(
                    meck:called(customer_service_facade, create_widget_installation, '_')
                )
            end)
        end},

        {"widget installation create returns public metadata without a shop secret", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            meck:expect(customer_service_facade, create_widget_installation, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                ?assert(is_integer(maps:get(at, Params))),
                {ok, #{
                    installation => #{
                        id => 91,
                        public_widget_id => <<"wgt_pub_test">>,
                        status => active
                    }
                }}
            end),
            ?S:with_listener(platform, p_widget_installations, platform_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/adm/customer-service/widget-installations">>,
                    widget_installation_body(),
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Installation = maps:get(<<"installation">>, ?S:payload(Resp)),
                ?assertEqual(<<"wgt_pub_test">>, maps:get(<<"public_widget_id">>, Installation)),
                ?assertNot(is_map_key(<<"shop_key">>, Installation)),
                ?assertNot(is_map_key(<<"secret">>, Installation))
            end)
        end},

        {"widget installation revoke uses write permission and explicit scope", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            meck:expect(customer_service_facade, revoke_widget_installation, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                ?assertEqual(91, maps:get(id, Params)),
                {ok, #{installation => #{id => 91, status => revoked}}}
            end),
            ?S:with_listener(platform, p_widget_installation_revoke, platform_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/adm/customer-service/widget-installations/91/revoke">>,
                    #{<<"organization_id">> => ?ORG, <<"workspace_id">> => ?WS},
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(
                    <<"revoked">>,
                    maps:get(<<"status">>, maps:get(<<"installation">>, ?S:payload(Resp)))
                )
            end)
        end},

        {"widget installation list without organization_id is 400", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            ?S:with_listener(platform, p_widget_installations, platform_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/adm/customer-service/widget-installations?workspace_id=",
                        (int_bin(?WS))/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(400, ?S:status(Resp))
            end)
        end},

        {"widget installation list without workspace_id is 422", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            ?S:with_listener(platform, p_widget_installations, platform_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/adm/customer-service/widget-installations?organization_id=",
                        (int_bin(?ORG))/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
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
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
                        "/sessions/queue">>,
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
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                        (int_bin(?SESSION))/binary, "/transfer">>,
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
                device_id => <<"dev-1">>,
                rating => 5
            }),
            ?assertEqual(<<"12">>, maps:get(session_id, Out)),
            ?assertEqual(<<"13">>, maps:get(contact_id, Out)),
            ?assertEqual(<<"cmid">>, maps:get(client_msg_id, Out)),
            ?assertEqual(<<"dev-1">>, maps:get(device_id, Out)),
            ?assertEqual(5, maps:get(rating, Out))
        end},

        {"credential surface paths are exactly the visitor/shop-key actions", fun() ->
            %% 正例：访客/门店动作的路径在 credential 面上。
            ?assert(cs_http:is_credential_surface_path(<<"/api/v1/cs/sessions">>)),
            ?assert(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
                        "/sessions/queue">>
                )
            ),
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
                    <<"/api/v1/cs/organizations/1/sessions/123/claim">>
                )
            ),
            ?assertNot(
                cs_http:is_credential_surface_path(
                    <<"/api/v1/cs/organizations/1/sessions/123/close">>
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

%% ===================================================================
%% BE-S01b（A07）：admin provisioning 平台接线——customer_service:write 门、
%% 认证派生 adm_user_id 进参数（审计 actor 记录）、workspace face 必填门。
%% ===================================================================

provisioning_flow_tests(_) ->
    [
        {"platform provisioning reaches the use case with derived adm_user_id (200)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            meck:expect(customer_service_facade, provision_seat, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                ?assertEqual(909091, maps:get(user_id, Params)),
                %% 审计 actor 是认证派生键（Admin session），非客户端申报。
                ?assertEqual(?ADM, maps:get(adm_user_id, Params)),
                {ok, #{
                    organization_id => ?ORG,
                    workspace_id => ?WS,
                    business_identity_id => 515999,
                    identity_created => true,
                    seat => #{enabled => true, max_concurrent => 1}
                }}
            end),
            ?S:with_listener(platform, p_seat_provision, platform_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/adm/customer-service/organizations/", (int_bin(?ORG))/binary,
                        "/provisioning?workspace_id=", (int_bin(?WS))/binary>>,
                    #{
                        <<"user_id">> => 909091,
                        <<"display_name">> => <<"客服一号">>,
                        <<"max_concurrent">> => 1
                    },
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Payload = ?S:payload(Resp),
                ?assertEqual(<<"515999">>, maps:get(<<"business_identity_id">>, Payload)),
                ?assertEqual(true, maps:get(<<"identity_created">>, Payload))
            end)
        end},

        {"platform provisioning with read permission is 403 (no use case touch)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            meck:expect(customer_service_facade, provision_seat, fun(_Org, _Params) ->
                erlang:error(provision_reached_with_read_permission)
            end),
            ?S:with_listener(platform, p_seat_provision, platform_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/adm/customer-service/organizations/", (int_bin(?ORG))/binary,
                        "/provisioning?workspace_id=", (int_bin(?WS))/binary>>,
                    #{<<"user_id">> => 909091, <<"display_name">> => <<"X">>},
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, provision_seat, '_'))
            end)
        end},

        {"platform provisioning without session key is 401", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            ?S:with_listener(
                platform,
                p_seat_provision,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<"/api/adm/customer-service/organizations/", (int_bin(?ORG))/binary,
                            "/provisioning?workspace_id=", (int_bin(?WS))/binary>>,
                        #{<<"user_id">> => 909091, <<"display_name">> => <<"X">>},
                        #{}
                    ),
                    ?assertEqual(401, ?S:status(Resp))
                end
            )
        end},

        {"platform provisioning without workspace_id is 422 (face-level required)", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            ?S:with_listener(platform, p_seat_provision, platform_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/adm/customer-service/organizations/", (int_bin(?ORG))/binary,
                        "/provisioning">>,
                    #{<<"user_id">> => 909091, <<"display_name">> => <<"X">>},
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(422, ?S:status(Resp))
            end)
        end}
    ].

widget_installation_body() ->
    #{
        <<"organization_id">> => ?ORG,
        <<"workspace_id">> => ?WS,
        <<"display_name">> => <<"Store support">>,
        <<"allowed_origins">> => [<<"https://shop.example.com">>],
        <<"branding">> => #{<<"display_name">> => <<"Store">>},
        <<"consent_version">> => <<"v1">>
    }.

int_bin(N) when is_integer(N) ->
    integer_to_binary(N).
