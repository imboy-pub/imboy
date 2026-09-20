%%% @doc CSB-02R 坐席工作台套件（真 cowboy 监听器 + 真 HTTP + **meck 的
%%% facade**；零 DB——纪律：不跑任何数据库测试）。
%%%
%%% 覆盖：
%%%   * **同路径 method+auth_context 分流**：GET /api/v1/cs/sessions/queue
%%%     以坐席 JWT（cs_seat，case_auth 覆盖）进队列视图；POST 仍以 shop key
%%%     门店开会话；双方主体互不采信（带 JWT 的 POST、带 shop key 的 GET
%%%     一律 401，零混淆）。
%%%   * **队列视图契约**：queued 列表（version/status/source/queued_at/
%%%     contact 掩码名/末条安全摘要）、total/total_by_status 稳定计数、
%%%     after_id/limit 键集分页、TSID 全 string；workspace 缺省 org-wide
%%%     （缺失不 422）、显式给出则收窄。
%%%   * **active/closed 两视图**：GET /api/v1/cs/seats/sessions（独立路径的
%%%     理由：GET /sessions 已冻结为访客面，同方法双主体必须换路径）；
%%%     status 必填且仅接受 active|closed（queued → 422）。
%%%   * **中间件/认证门**：无 Authorization 的坐席 GET 401；组织内无
%%%     assignment 的成员 403；坐席动作的业务零触达（facade 未被调用）。
%%%
%%% 所有 facade 均 meck 打桩（`cs_facade_call` 只进 facade——打桩点即边界）。
-module(cs_seat_workbench_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(ORG, 7001001).
-define(WS, 90001).
-define(WS2, 90002).
-define(UID, 424242).
-define(IDENTITY, 515151).
-define(SESSION, 555000111).
-define(SESSION2, 555000112).
-define(CONTACT, 3131).
-define(SHOP_KEY, <<"sk-live-1">>).

%% ===================================================================
%% 套件夹具
%% ===================================================================

workbench_test_() ->
    {foreach,
        fun() ->
            {ok, _} = application:ensure_all_started(cowboy),
            meck:new(customer_service_facade, [passthrough]),
            meck:new(enterprise_business_facade, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            meck:unload(enterprise_business_facade),
            cs_fake_facts:clear(),
            ok
        end,
        [
            fun queue_split_tests/1,
            fun seat_list_tests/1,
            fun seat_context_endpoint_tests/1,
            fun transfer_targets_tests/1,
            fun seat_events_placeholder_tests/1
        ]}.

seat_inject() ->
    #{auth_facts => cs_fake_facts, current_uid => ?UID}.

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

%% meck 的 facade 返回即**最终视图形状**（application 投影已被打桩越过）：
%% 行含 source / contact 掩码名 / 末条安全摘要。
queue_row() ->
    #{
        id => ?SESSION,
        organization_id => ?ORG,
        workspace_id => ?WS,
        contact_id => ?CONTACT,
        conversation_id => 666000222,
        business_identity_id => undefined,
        status => queued,
        version => 4,
        queued_at => 1760000000,
        claimed_at => undefined,
        closed_at => undefined,
        source => <<"widget">>,
        contact => #{masked_name => <<"wx***1">>},
        last_message => #{
            id => 880001,
            sender_type => <<"contact">>,
            created_at => 1760000100
        }
    }.

page_view() ->
    {ok, #{
        sessions => [queue_row()],
        total => 1,
        total_by_status => #{<<"queued">> => 1, <<"active">> => 2, <<"closed">> => 5},
        next_after_id => undefined
    }}.

int_bin(N) ->
    integer_to_binary(N).

%% ===================================================================
%% 同路径 method+auth_context 分流（GET 队列 / POST 门店开会话）
%% ===================================================================

queue_split_tests(_) ->
    [
        {"A seat GET on /sessions/queue returns the queue view (200; case_auth split)", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            meck:expect(customer_service_facade, seat_session_queue, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                %% workspace 缺省 org-wide（缺失不 422，作用域由 Org 决定）。
                ?assertNot(is_map_key(workspace_id, Params)),
                ?assert(is_integer(maps:get(at, Params))),
                page_view()
            end),
            ?S:with_listener(tenant, session_queue, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                        "/sessions/queue?after_id=0&limit=50">>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Payload = ?S:payload(Resp),
                ?assertEqual(0, ?S:code(Resp)),
                [Row] = maps:get(<<"sessions">>, Payload),
                ?assertEqual(int_bin(?SESSION), maps:get(<<"id">>, Row)),
                %% JSON 往返后 status 是 binary（jsx）。
                ?assertEqual(<<"queued">>, maps:get(<<"status">>, Row)),
                ?assertEqual(4, maps:get(<<"version">>, Row)),
                ?assertEqual(1, maps:get(<<"total">>, Payload)),
                ?assertEqual(
                    2, maps:get(<<"active">>, maps:get(<<"total_by_status">>, Payload))
                ),
                ?assertNot(
                    meck:called(customer_service_facade, open_session, '_')
                )
            end)
        end},

        {"POST on /sessions/queue still opens a session with the shop key", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG}}
            end),
            meck:expect(customer_service_facade, open_session, fun(_O, _P) ->
                {ok, #{id => ?SESSION2, status => queued, contact_id => ?CONTACT}}
            end),
            ?S:with_listener(
                tenant,
                session_queue,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    meck:reset(customer_service_facade),
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/queue">>,
                        #{
                            <<"workspace_id">> => ?WS,
                            <<"contact_id">> => 1,
                            <<"conversation_id">> => 2
                        },
                        #{<<"x-cs-shop-key">> => ?SHOP_KEY}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    ?assertNot(
                        meck:called(customer_service_facade, seat_session_queue, '_')
                    )
                end
            )
        end},

        {"A seat GET without JWT is 401 and never reaches the use case", fun() ->
            ?S:with_listener(
                tenant,
                session_queue,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    meck:reset(customer_service_facade),
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/queue">>,
                        <<>>,
                        #{}
                    ),
                    ?assertEqual(401, ?S:status(Resp)),
                    ?assertNot(meck:called(customer_service_facade, seat_session_queue, '_'))
                end
            )
        end},

        {"A shop key on the queue GET is not honored (principal confusion rejected)", fun() ->
            ?S:with_listener(
                tenant,
                session_queue,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    meck:reset(customer_service_facade),
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/queue">>,
                        <<>>,
                        #{<<"x-cs-shop-key">> => ?SHOP_KEY}
                    ),
                    ?assertEqual(401, ?S:status(Resp)),
                    ?assertNot(
                        meck:called(customer_service_facade, seat_session_queue, '_')
                    )
                end
            )
        end},

        {"A shop key POST with an unrelated JWT still authenticates via the shop key", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_O, _P) ->
                {ok, #{organization_id => ?ORG}}
            end),
            meck:expect(customer_service_facade, open_session, fun(_O, _P) ->
                {ok, #{id => ?SESSION2, status => queued, contact_id => ?CONTACT}}
            end),
            ?S:with_listener(
                tenant,
                session_queue,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    meck:reset(customer_service_facade),
                    %% 带一个（可能无效的）JWT 头的门店 POST：中间件照常过 JWT
                    %% 门并注入会话键，但 route metadata 主体仍是 cs_shop_key
                    %%（JWT 不采信为门店，shop key 头才是凭证）。
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/queue">>,
                        #{
                            <<"workspace_id">> => ?WS,
                            <<"contact_id">> => 1,
                            <<"conversation_id">> => 2
                        },
                        #{
                            <<"x-cs-shop-key">> => ?SHOP_KEY,
                            <<"authorization">> => <<"Bearer stale">>
                        }
                    ),
                    ?assertEqual(200, ?S:status(Resp))
                end
            )
        end}
    ].

%% ===================================================================
%% 坐席 active/closed 两视图（GET /api/v1/cs/seats/sessions）
%% ===================================================================

seat_list_tests(_) ->
    [
        {"Seat list returns the active view with scoped counters", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            meck:expect(customer_service_facade, seat_session_list, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(<<"active">>, maps:get(status, Params)),
                %% 显式 workspace 收窄进 Params（action 表 optional 门放行）。
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                {ok, #{
                    sessions => [],
                    total => 2,
                    total_by_status => #{<<"active">> => 2},
                    next_after_id => undefined
                }}
            end),
            ?S:with_listener(tenant, seat_session_list, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                        "/seats/sessions?status=active&workspace_id=", (int_bin(?WS))/binary>>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Payload = ?S:payload(Resp),
                ?assertEqual([], maps:get(<<"sessions">>, Payload)),
                ?assertEqual(2, maps:get(<<"total">>, Payload)),
                %% JSON null（jsx 解出 null 原子）。
                ?assertEqual(null, maps:get(<<"next_after_id">>, Payload))
            end)
        end},

        {"Seat list rejects the closed-less status vocabulary with 422", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            %% 组内共享同一 meck 实例：清掉上一例的 status=active 期望，改走
            %% **真实** facade 的 status 校验（queued → {invalid_status,*} 422）。
            meck:expect(customer_service_facade, seat_session_list, fun(Org, Params) ->
                meck:passthrough([Org, Params])
            end),
            ?S:with_listener(tenant, seat_session_list, seat_inject(), fun(Port) ->
                %% queued 视图冻结在 /sessions/queue；此处显式 422 不漂移。
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                        "/seats/sessions?status=queued">>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(<<"invalid_status">>, ?S:msg(Resp))
            end)
        end},

        {"Seat list without the customer_service assignment is 403", fun() ->
            cs_fake_facts:set(#{
                organization_id => ?ORG,
                member => #{user_id => ?UID, status => active, governance_roles => []},
                assignments => [],
                permissions => [<<"conversation.read">>]
            }),
            ?S:with_listener(tenant, seat_session_list, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                        "/seats/sessions?status=closed">>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp))
            end)
        end},

        {"Seat list without JWT is 401 (middleware gate preserved)", fun() ->
            ?S:with_listener(
                tenant,
                seat_session_list,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                            "/seats/sessions?status=active">>,
                        <<>>,
                        #{}
                    ),
                    ?assertEqual(401, ?S:status(Resp))
                end
            )
        end}
    ].

%% ===================================================================
%% BE-S01a：GET /api/v1/cs/me/seat-contexts（主体自身作用域——无 Org 键，
%% handler 的 self 分支只验 JWT 会话键，聚合事实在 facade/application）
%% ===================================================================

seat_context_endpoint_tests(_) ->
    [
        {"Seat contexts with JWT returns the aggregate view (200)", fun() ->
            meck:expect(customer_service_facade, seat_contexts, fun(OrgIgnored, Params) ->
                %% self 作用域：OrgId 是 0 占位，真作用域键是 actor_user_id。
                ?assertEqual(0, OrgIgnored),
                ?assertEqual(?UID, maps:get(actor_user_id, Params)),
                {ok, #{
                    contexts => [
                        #{
                            organization_id => ?ORG,
                            organization_name => <<"Acme"/utf8>>,
                            workspaces => [#{id => ?WS, name => <<"main">>}],
                            business_identity_id => ?IDENTITY,
                            seat_enabled => true,
                            capabilities => [<<"conversation.read">>]
                        }
                    ],
                    user_id => ?UID
                }}
            end),
            ?S:with_listener(tenant, seat_contexts, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/me/seat-contexts">>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                [Ctx] = maps:get(<<"contexts">>, ?S:payload(Resp)),
                ?assertEqual(int_bin(?ORG), maps:get(<<"organization_id">>, Ctx)),
                ?assertEqual(true, maps:get(<<"seat_enabled">>, Ctx))
            end)
        end},
        {"Seat contexts without JWT is 401", fun() ->
            ?S:with_listener(tenant, seat_contexts, #{auth_facts => cs_fake_facts}, fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(Port, <<"GET">>, <<"/api/v1/cs/me/seat-contexts">>, <<>>, #{}),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, seat_contexts, '_'))
            end)
        end}
    ].

%% ===================================================================
%% BE-S01a：GET /api/v1/cs/organizations/:org_id/transfer-targets
%% ===================================================================

transfer_targets_tests(_) ->
    [
        {"Transfer targets return the minimal projection with org from path", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            meck:expect(customer_service_facade, transfer_targets, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                %% 排除键是认证派生的坐席本人 identity（客户端不可申报）。
                ?assertEqual(?IDENTITY, maps:get(business_identity_id, Params)),
                {ok, #{
                    targets => [
                        #{
                            business_identity_id => 515552,
                            display_name => <<"B 坐席"/utf8>>,
                            available => true
                        }
                    ],
                    next_after_id => undefined
                }}
            end),
            ?S:with_listener(tenant, transfer_targets, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/transfer-targets">>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                [Target] = maps:get(<<"targets">>, ?S:payload(Resp)),
                ?assertEqual(<<"515552">>, maps:get(<<"business_identity_id">>, Target)),
                ?assertEqual(true, maps:get(<<"available">>, Target))
            end)
        end},
        {"Transfer targets of a foreign org is rejected by the seat gate", fun() ->
            %% 坐席事实只在 ?ORG：路径申报其他 org 时 member/assignment 同语句
            %% 查找必失败（fail-closed，不是信任路径申报）。
            cs_fake_facts:set(seat_facts()),
            ?S:with_listener(tenant, transfer_targets, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG + 1))/binary,
                        "/transfer-targets">>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, transfer_targets, '_'))
            end)
        end}
    ].

%% ===================================================================
%% BE-S01b：GET /api/v1/cs/organizations/:org_id/seats/me/events（SSE 流式）
%% ===================================================================

seat_events_placeholder_tests(_) ->
    [
        {"Seat events streams retry+resync+event frames after full auth and workspace gate",
            fun() ->
                cs_fake_facts:set(seat_facts()),
                meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                    {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
                end),
                %% 合同信封（application 投影形状；TSID integer 进 handler 后
                %% 出站编 TSID-string）。
                Envelope = seat_event_envelope(),
                meck:expect(customer_service_facade, seat_events, fun(Org, Params) ->
                    ?assertEqual(?ORG, Org),
                    ?assertEqual(?WS, maps:get(workspace_id, Params)),
                    ?assertEqual(?IDENTITY, maps:get(business_identity_id, Params)),
                    {ok, #{
                        events => [Envelope],
                        cursor => 880001,
                        resync_required => true,
                        resync_reason => <<"unknown">>
                    }}
                end),
                ?S:with_listener(
                    tenant,
                    seat_events,
                    stream_inject(),
                    fun(Port) ->
                        Raw = ?S:stream_request(
                            Port,
                            <<"GET">>,
                            <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                                "/seats/me/events?workspace_id=", (int_bin(?WS))/binary>>,
                            <<>>,
                            #{<<"authorization">> => <<"Bearer x">>},
                            600
                        ),
                        {Head, Body} = sse_parts(Raw),
                        %% 响应头（sse-event-contract response_headers）。
                        ?assertMatch(
                            {match, _},
                            re:run(Head, <<"content-type: text/event-stream">>, [caseless])
                        ),
                        ?assertMatch(
                            {match, _},
                            re:run(Head, <<"x-cs-event-retention-seconds: 86400">>, [caseless])
                        ),
                        %% 首写顺序：retry: 2000（合同字面值）→ 合成 resync 帧
                        %% （首连无游标）。首个 stream_body 合并写出，逐字连续。
                        ?assertMatch(
                            {match, _},
                            re:run(
                                Body,
                                <<"retry: 2000\\nid: 880001\\nevent: resync.required\\n">>
                            )
                        ),
                        %% 事件帧：id/event/data 全合同形状；TSID 出站为 string。
                        ?assertMatch({match, _}, re:run(Body, <<"\"event_id\":\"880001\"">>)),
                        ?assertMatch({match, _}, re:run(Body, <<"\"workspace_id\":\"90001\"">>)),
                        ?assertMatch({match, _}, re:run(Body, <<"\"resource_id\":\"990001\"">>)),
                        %% resource_version 是版本号不是 TSID：保持 number。
                        ?assertMatch({match, _}, re:run(Body, <<"\"resource_version\":1">>)),
                        ?assertMatch({match, _}, re:run(Body, <<"\"reason\":\"created\"">>))
                    end
                )
            end},
        {"Seat events with valid cursor continues without resync frame", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            meck:expect(customer_service_facade, seat_events, fun(_Org, Params) ->
                %% Last-Event-ID 头优先于 after_id 查询参数（cursor_rule）。
                ?assertEqual(880001, maps:get(after_id, Params)),
                {ok, #{
                    events => [],
                    cursor => 880001,
                    resync_required => false,
                    resync_reason => <<"unknown">>
                }}
            end),
            ?S:with_listener(
                tenant,
                seat_events,
                stream_inject(),
                fun(Port) ->
                    Raw = ?S:stream_request(
                        Port,
                        <<"GET">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                            "/seats/me/events?workspace_id=", (int_bin(?WS))/binary,
                            "&after_id=1">>,
                        <<>>,
                        #{
                            <<"authorization">> => <<"Bearer x">>,
                            <<"last-event-id">> => <<"880001">>
                        },
                        600
                    ),
                    {_, Body} = sse_parts(Raw),
                    %% retry 帧仍是首写；合法游标 ⇒ 无 resync 帧。
                    ?assertMatch({match, _}, re:run(Body, <<"retry: 2000\\n">>)),
                    ?assertNotMatch({match, _}, re:run(Body, <<"resync.required">>))
                end
            )
        end},
        {"Seat events cross-org cursor is 403 before streaming (no silent fallback)", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            meck:expect(customer_service_facade, seat_events, fun(_Org, _Params) ->
                {error, cross_org}
            end),
            ?S:with_listener(tenant, seat_events, seat_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                        "/seats/me/events?workspace_id=", (int_bin(?WS))/binary>>,
                    <<>>,
                    #{
                        <<"authorization">> => <<"Bearer x">>,
                        <<"last-event-id">> => <<"880002">>
                    }
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertEqual(<<"cross_org">>, ?S:msg(Resp))
            end)
        end},
        {"Seat events invalid Last-Event-ID header is 400", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            ?S:with_listener(tenant, seat_events, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                        "/seats/me/events?workspace_id=", (int_bin(?WS))/binary>>,
                    <<>>,
                    #{
                        <<"authorization">> => <<"Bearer x">>,
                        <<"last-event-id">> => <<"not-a-tsid">>
                    }
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, seat_events, '_'))
            end)
        end},
        {"Seat events without workspace_id is 422 before streaming", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            ?S:with_listener(tenant, seat_events, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/seats/me/events">>,
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(422, ?S:status(Resp))
            end)
        end},
        {"Seat events without JWT is 401 before streaming", fun() ->
            cs_fake_facts:set(seat_facts()),
            %% 中间件门：无 Authorization 头 ⇒ 生产中间件不注入 current_uid，
            %% 测试扮演中间件（注入集不含 current_uid）。
            ?S:with_listener(tenant, seat_events, #{auth_facts => cs_fake_facts}, fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary,
                        "/seats/me/events?workspace_id=", (int_bin(?WS))/binary>>,
                    <<>>,
                    #{}
                ),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, seat_events, '_'))
            end)
        end}
    ].

seat_event_envelope() ->
    #{
        event_id => 880001,
        type => <<"message.appended">>,
        organization_id => ?ORG,
        workspace_id => ?WS,
        resource_type => <<"message">>,
        resource_id => 990001,
        resource_version => 1,
        occurred_at => <<"2026-09-20T08:00:00Z">>,
        reason => <<"created">>
    }.

sse_parts(Raw) ->
    case binary:split(Raw, <<"\r\n\r\n">>) of
        [Head, Body] -> {Head, Body};
        [_] -> {Raw, <<>>}
    end.

%% SSE 流式用例的注入集：短轮询/短 deadline（流循环在 deadline 正常 fin，
%% 测试客户端限时读 600ms 后主动断开，不拖慢套件）。
stream_inject() ->
    maps:merge(seat_inject(), #{sse_max_ms => 250, sse_poll_ms => 50}).

%% ===================================================================
%% 视图投影（application 直驱 + fake store：掩码名 / 来源 / 末条安全摘要）
%% ——投影逻辑真身在 cs_session_app:seat_session_page，handler 套件的
%% meck facade 越过它，故在此直驱 application（零 DB、零 meck）。
%% ===================================================================

projection_test_() ->
    {foreach, fun() -> ok = cs_fake_store:init() end, fun(_) -> ok = cs_fake_store:destroy() end, [
        fun projection_cases/1
    ]}.

projection_cases(_) ->
    Org = 820000000000001,
    Ws = 42,
    Base = #{
        id => 555000111,
        organization_id => Org,
        workspace_id => Ws,
        contact_id => 3131,
        conversation_id => 666,
        business_identity_id => undefined,
        status => queued,
        version => 4,
        queued_at => 1760000000,
        claimed_at => undefined,
        closed_at => undefined
    },
    [
        {"Projection: subject_mask preferred; display_name masked; guest handle fallback", fun() ->
            %% foreach 对生成器 fun 返回的整组只跑一次 setup——每例自初始化。
            ok = cs_fake_store:init(),
            ok = cs_fake_store:put_session_for_list(
                Base#{
                    visit_token_id => 99000,
                    created_by_user_id => undefined,
                    contact_subject_mask => <<"wx***1">>,
                    contact_display_name => <<"王小明"/utf8>>,
                    last_message_id => 880001,
                    last_message_sender_type => <<"contact">>,
                    last_message_created_at => 1760000100
                }
            ),
            {ok, View} = cs_session_app:seat_session_page(Org, #{
                store => cs_fake_store, status => <<"queued">>, workspace_id => Ws
            }),
            [Row] = maps:get(sessions, View),
            ?assertEqual(<<"wx***1">>, maps:get(masked_name, maps:get(contact, Row))),
            ?assertEqual(<<"widget">>, maps:get(source, Row)),
            LM = maps:get(last_message, Row),
            ?assertEqual(880001, maps:get(id, LM)),
            ?assertNot(is_map_key(body_cipher, LM)),
            ?assertNot(is_map_key(key_version, LM)),
            ?assertNot(is_map_key(client_msg_id, LM)),
            %% 计数：同作用域稳定分布。
            ?assertEqual(1, maps:get(total, View)),
            ?assertEqual(#{<<"queued">> => 1}, maps:get(total_by_status, View))
        end},

        {"Projection: masked display name keeps first/last chars; source facts decide", fun() ->
            ok = cs_fake_store:init(),
            ok = cs_fake_store:put_session_for_list(
                Base#{
                    id => 555000112,
                    visit_token_id => undefined,
                    created_by_user_id => 9,
                    contact_subject_mask => undefined,
                    contact_display_name => <<"王小明"/utf8>>,
                    last_message_id => undefined,
                    last_message_sender_type => undefined,
                    last_message_created_at => undefined
                }
            ),
            {ok, View} = cs_session_app:seat_session_page(Org, #{
                store => cs_fake_store, status => <<"queued">>, workspace_id => Ws
            }),
            [Row] = maps:get(sessions, View),
            Masked = maps:get(masked_name, maps:get(contact, Row)),
            ?assertNotEqual(<<"王小明"/utf8>>, Masked),
            ?assert(byte_size(Masked) >= 3),
            ?assertEqual(<<"seat">>, maps:get(source, Row)),
            ?assertEqual(undefined, maps:get(last_message, Row)),
            ?assertEqual(undefined, maps:get(next_after_id, View))
        end},

        {"Projection: no mask/name falls back to the stable guest handle; workspace narrows",
            fun() ->
                ok = cs_fake_store:init(),
                ok = cs_fake_store:put_session_for_list(
                    Base#{
                        id => 555000113,
                        workspace_id => Ws,
                        visit_token_id => undefined,
                        created_by_user_id => undefined,
                        contact_subject_mask => undefined,
                        contact_display_name => undefined
                    }
                ),
                %% 另一 workspace 的会话：org-wide 可见，收窄后不可见。
                ok = cs_fake_store:put_session_for_list(
                    Base#{
                        id => 555000114,
                        workspace_id => 43,
                        visit_token_id => undefined,
                        created_by_user_id => undefined
                    }
                ),
                {ok, OrgWide} = cs_session_app:seat_session_page(Org, #{
                    store => cs_fake_store, status => <<"queued">>
                }),
                ?assertEqual(2, maps:get(total, OrgWide)),
                Rows0 = maps:get(sessions, OrgWide),
                ?assertEqual(2, length(Rows0)),
                Wide = hd([R || R <- Rows0, maps:get(id, R) =:= 555000113]),
                ?assertEqual(
                    <<"guest#">>,
                    binary:part(
                        maps:get(masked_name, maps:get(contact, Wide)), 0, 6
                    )
                ),
                {ok, Narrowed} = cs_session_app:seat_session_page(Org, #{
                    store => cs_fake_store, status => <<"queued">>, workspace_id => Ws
                }),
                ?assertEqual(1, maps:get(total, Narrowed)),
                %% 键集游标（DESC，`id <` 口径）：after_id=最新行 id →
                %% 只翻出更早一行（不重不漏）。
                {ok, Paged} = cs_session_app:seat_session_page(Org, #{
                    store => cs_fake_store, status => <<"queued">>, after_id => 555000114
                }),
                ?assertEqual(1, length(maps:get(sessions, Paged))),
                %% status 必填：坐席面无全状态页。
                ?assertEqual(
                    {error, {invalid_status, undefined}},
                    cs_session_app:seat_session_page(Org, #{store => cs_fake_store})
                )
            end}
    ].
