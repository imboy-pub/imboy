%%% @doc CSB-03 的 widget 接入面套件（真 cowboy 监听器 + 真 HTTP + **meck 的
%%% facade**；零 DB——纪律：不跑任何数据库测试）。
%%%
%%% 覆盖（CSB-03-A01..A05；A06 = feature 裁剪矩阵脚本，另跑）：
%%%
%%%   * **A01 令牌与 Origin**：widget 凭证 = bootstrap 令牌专用头
%%%     （`x-cs-visit-token`）——缺头 401、查询串携带即 400、正文申报服务端
%%%     派生键（secret/origin/subject_key 等）即 400（CSD-BE-01R：合同 S3
%%%     冻结码 `server_derived_key_rejected`，键名不出线）；bootstrap 的
%%%     Origin 头归一化（大小写/缺省端口折叠）后进 application，形状非法
%%%     400、不在 installation allowlist 403；方法门 405；缺必填 422（绝不
%%%     500）。CSD-BE-01S（合同 S3 v1.1）：**全部 widget 动作面零 org 申报**
%%%     ——持 token 面 OrgId 占位 0 由 facade 按 token digest 命中行派生，
%%%     organization_id 申报即 400；bootstrap 同时注入 request_host（Host 头
%%%     + 客户端侧 scheme 归一 origin，同源放行判定输入）。
%%%   * **A02 SSE**：GET events 返回 `text/event-stream`；先发 `retry:` +
%%%     当前状态 resource-id 事件（TSID string）；`Last-Event-ID` 头驱动
%%%     after 游标补偿（消息事件 id = 消息 id，单调不重）；空轮询周期注释行
%%%     保活；会话状态变更产生后续 `state` 事件；越权/缺头在开流前结构化拒绝。
%%%   * **A03 动态 CORS**：仅成功请求 echo 归一化 Origin（具体值，绝不 `*`，
%%%     绝不由此开 credentials）+ `vary: Origin`。
%%%   * **A04 限流**：widget 路径走 `cs_widget_per_ip` 专用桶（真实
%%%     throttle_middleware 链），超限 429。
%%%   * **A05 契约**：坐席会话详情（GET /api/v1/cs/sessions/:id，坐席 JWT +
%%%     conversation.read）；TSID 出站全 string；identity exchange 的断言
%%%     map 透传。
%%%
%%% 所有 facade 均 meck 打桩（`cs_facade_call` 只进 facade——打桩点即边界）。
-module(cs_widget_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(ORG, 7001001).
-define(WS, 90001).
-define(UID, 424242).
-define(IDENTITY, 515151).
-define(INSTALL, 810001).
-define(SESSION, 555000111).
-define(CONTACT, 3131).
-define(CONVERSATION, 666000222).
-define(TOKEN, <<"wtok-abcdef">>).
-define(ORIGIN, <<"https://shop.example.com">>).

%% ===================================================================
%% 套件夹具
%% ===================================================================

widget_test_() ->
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
            ok
        end,
        [
            fun a01_token_and_origin_tests/1,
            fun a06_capability_and_matrix_tests/1,
            fun asset_content_tests/1,
            fun asset_upload_tests/1,
            fun a02_sse_tests/1,
            fun a03_cors_tests/1,
            fun a04_throttle_tests/1,
            fun a05_contract_and_seat_tests/1
        ]}.

widget_inject() ->
    #{auth_facts => cs_fake_facts}.

sse_inject() ->
    #{auth_facts => cs_fake_facts, sse_poll_ms => 30, sse_max_ms => 300, sse_retry_ms => 3000}.

%% DF-10 回归专用：sse_max_ms 拉长到 60s——修复前的静默保持不会在读窗口内
%% 自然 fin，与修复后的「轮询即关流」在时间轴上可区分。
sse_inject_fatal() ->
    #{auth_facts => cs_fake_facts, sse_poll_ms => 30, sse_max_ms => 60000, sse_retry_ms => 3000}.

bootstrap_body() ->
    %% CSD-BE-01R（hosted-widget-contract S3）：浏览器零 org 申报面——载荷
    %% 只有 public_widget_id + subject_id（FE contract.ts buildBootstrapBody
    %% 同构）；申报 organization_id 即 400 server_derived_key_rejected。
    #{
        <<"public_widget_id">> => <<"wgt_pub_a01">>,
        <<"subject_id">> => <<"browser-random-1">>
    }.

bootstrap_view() ->
    #{
        installation_id => ?INSTALL,
        public_widget_id => <<"wgt_pub_a01">>,
        display_name => <<"Imboy Shop">>,
        consent_version => 1,
        branding => #{<<"primary_color">> => <<"#0066ff">>},
        contact_id => ?CONTACT,
        secret => <<"s3cr3t-once">>,
        expires_at => 4102444800000,
        reused => false
    }.

int_bin(N) ->
    integer_to_binary(N).

%% ===================================================================
%% A01：令牌与 Origin（专用头；查询串禁令；服务端派生键守卫）
%% ===================================================================

a01_token_and_origin_tests(_) ->
    [
        {"A01 bootstrap issues token once (200; TSID string; origin normalized)", fun() ->
            meck:expect(customer_service_facade, widget_bootstrap, fun(Org, Params) ->
                %% CSD-BE-01R：零 org 申报面——OrgId 是 derived 占位 0，租户
                %% 归属由 application 的 public_id 全局反查派生（facade 侧）。
                ?assertEqual(0, Org),
                ?assertNot(is_map_key(organization_id, Params)),
                ?assertEqual(?ORIGIN, maps:get(origin, Params)),
                %% CSD-BE-01S（合同 S3 v1.1）：同源放行判定输入——Host 头 +
                %% 客户端侧 scheme 归一 origin（夹具 Host=localhost + 明文 http
                %% 直连 → 缺省端口折叠）。
                ?assertEqual(<<"http://localhost">>, maps:get(request_host, Params)),
                ?assert(is_integer(maps:get(at, Params))),
                ?assertEqual(<<"wgt_pub_a01">>, maps:get(public_widget_id, Params)),
                ?assertNot(is_map_key(workspace_id, Params)),
                {ok, bootstrap_view()}
            end),
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/bootstrap">>,
                    bootstrap_body(),
                    #{<<"origin">> => ?ORIGIN}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Payload = ?S:payload(Resp),
                ?assertEqual(0, ?S:code(Resp)),
                ?assertEqual(int_bin(?INSTALL), maps:get(<<"installation_id">>, Payload)),
                ?assertEqual(int_bin(?CONTACT), maps:get(<<"contact_id">>, Payload)),
                %% secret 只在签发响应出现一次（明文不出后续任何面）。
                ?assertEqual(<<"s3cr3t-once">>, maps:get(<<"secret">>, Payload))
            end)
        end},

        {"CSD-BE-01R bootstrap body organization_id is server-derived (400, contract code)",
            fun() ->
                ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<"/api/v1/cs/widget/bootstrap">>,
                        (bootstrap_body())#{<<"organization_id">> => ?ORG},
                        #{<<"origin">> => ?ORIGIN}
                    ),
                    ?assertEqual(400, ?S:status(Resp)),
                    ?assertEqual(<<"server_derived_key_rejected">>, ?S:msg(Resp))
                end)
            end},

        {"A01 bootstrap origin header is scheme/host/port normalized", fun() ->
            meck:expect(customer_service_facade, widget_bootstrap, fun(_Org, Params) ->
                ?assertEqual(?ORIGIN, maps:get(origin, Params)),
                {ok, bootstrap_view()}
            end),
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/bootstrap">>,
                    bootstrap_body(),
                    #{<<"origin">> => <<"HTTPS://Shop.Example.COM:443">>}
                ),
                ?assertEqual(200, ?S:status(Resp))
            end)
        end},

        {"A01 bootstrap without Origin header is a structured 400", fun() ->
            meck:expect(customer_service_facade, widget_bootstrap, fun(_O, _P) ->
                {ok, bootstrap_view()}
            end),
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port, <<"POST">>, <<"/api/v1/cs/widget/bootstrap">>, bootstrap_body(), #{}
                ),
                %% 缺必填（含 Origin 头缺失）照既有动作表口径 = 422。
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(<<"missing_param.origin">>, ?S:msg(Resp)),
                ?assertNot(meck:called(customer_service_facade, widget_bootstrap, '_'))
            end)
        end},

        {"A01 bootstrap with malformed Origin is 400 invalid_origin", fun() ->
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/bootstrap">>,
                    bootstrap_body(),
                    #{<<"origin">> => <<"not-an-origin">>}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"invalid_origin">>, ?S:msg(Resp))
            end)
        end},

        {"A01 bootstrap origin outside installation allowlist is 403 (no ACAO)", fun() ->
            meck:expect(customer_service_facade, widget_bootstrap, fun(_O, _P) ->
                {error, origin_not_allowed}
            end),
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/bootstrap">>,
                    bootstrap_body(),
                    #{<<"origin">> => ?ORIGIN}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertEqual(<<"origin_not_allowed">>, ?S:msg(Resp)),
                ?assertNot(
                    is_map_key(<<"access-control-allow-origin">>, maps:get(headers, Resp))
                )
            end)
        end},

        {"A01 client-declared server-derived keys are rejected with 400", fun() ->
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/bootstrap">>,
                    (bootstrap_body())#{<<"origin">> => <<"https://evil.example.com">>},
                    #{<<"origin">> => ?ORIGIN}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                %% CSD-BE-01R（合同 S3 冻结码）：键名不出线（枚举面收口）。
                ?assertEqual(<<"server_derived_key_rejected">>, ?S:msg(Resp))
            end)
        end},

        {"A01 widget session create without token header is 401 and never reaches use case",
            fun() ->
                meck:expect(customer_service_facade, widget_create_session, fun(_O, _P) ->
                    {ok, #{}}
                end),
                ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<"/api/v1/cs/widget/sessions">>,
                        #{<<"installation_id">> => ?INSTALL},
                        #{}
                    ),
                    ?assertEqual(401, ?S:status(Resp)),
                    ?assertNot(meck:called(customer_service_facade, widget_create_session, '_'))
                end)
            end},

        {"A01 credential in query string is a 400 (token travels in header only)", fun() ->
            ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions?token=", ?TOKEN/binary>>,
                    #{<<"installation_id">> => ?INSTALL},
                    #{}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"credential_in_query_string">>, ?S:msg(Resp))
            end)
        end},

        {"A01 client body secret is a 400 (server-derived credential)", fun() ->
            ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions">>,
                    #{<<"installation_id">> => ?INSTALL, <<"secret">> => <<"forged">>},
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"server_derived_key_rejected">>, ?S:msg(Resp))
            end)
        end},

        %% CP-SEC-05（DEC-VISIT-TOKEN=FIX_401_VISIT_TOKEN_INVALID）：伪造
        %% `visit_token_invalid`（与缺头 401、吊销/过期 401 同凭证面）。
        {"A01 forged visit token is 401 visit_token_invalid (contract)", fun() ->
            meck:expect(customer_service_facade, widget_create_session, fun(_Org, _Params) ->
                {error, visit_token_invalid}
            end),
            ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions">>,
                    #{<<"installation_id">> => ?INSTALL},
                    #{<<"x-cs-visit-token">> => <<"wtok-forged-never-issued">>}
                ),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertEqual(<<"visit_token_invalid">>, ?S:msg(Resp))
            end)
        end},

        {"CSD-BE-01S session create zero-org body (OrgId placeholder 0 to facade)", fun() ->
            meck:expect(customer_service_facade, widget_create_session, fun(Org, Params) ->
                %% CSD-BE-01S：零 org 申报面——OrgId 占位 0，租户由 facade
                %% 按 token digest 命中行派生；organization_id 不进 Params。
                ?assertEqual(0, Org),
                ?assertNot(is_map_key(organization_id, Params)),
                ?assertEqual(?TOKEN, maps:get(secret, Params)),
                ?assertEqual(?INSTALL, maps:get(installation_id, Params)),
                ?assert(is_integer(maps:get(at, Params))),
                {ok, #{
                    session_id => ?SESSION,
                    conversation_id => ?CONVERSATION,
                    contact_id => ?CONTACT,
                    workspace_id => ?WS,
                    installation_id => ?INSTALL,
                    status => queued
                }}
            end),
            ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions">>,
                    #{<<"installation_id">> => ?INSTALL},
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(int_bin(?SESSION), maps:get(<<"session_id">>, ?S:payload(Resp)))
            end)
        end},

        {"CSD-BE-01S declared organization_id on sessions is 400 (contract code)", fun() ->
            ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions">>,
                    #{<<"organization_id">> => ?ORG, <<"installation_id">> => ?INSTALL},
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"server_derived_key_rejected">>, ?S:msg(Resp))
            end)
        end},

        {"A01 missing installation_id is a structured 422", fun() ->
            ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions">>,
                    #{},
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(422, ?S:status(Resp)),
                ?assertEqual(<<"missing_param.installation_id">>, ?S:msg(Resp))
            end)
        end},

        {"A01 wrong method is 405", fun() ->
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port, <<"GET">>, <<"/api/v1/cs/widget/bootstrap">>, <<>>, #{}
                ),
                ?assertEqual(405, ?S:status(Resp))
            end)
        end},

        {"A01 visitor message: client_msg_id required (422) and TSID string outbound", fun() ->
            meck:expect(customer_service_facade, widget_visitor_message, fun(Org, Params) ->
                ?assertEqual(0, Org),
                ?assertNot(is_map_key(organization_id, Params)),
                ?assertEqual(?TOKEN, maps:get(secret, Params)),
                ?assertEqual(?SESSION, maps:get(session_id, Params)),
                {ok, #{id => 99001, client_msg_id => <<"cmid-1">>, body => <<"hi">>}}
            end),
            ?S:with_listener(widget, widget_session_messages, widget_inject(), fun(Port) ->
                Missing = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                    #{<<"installation_id">> => ?INSTALL, <<"body">> => <<"hi">>},
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(422, ?S:status(Missing)),
                ?assertEqual(<<"missing_param.client_msg_id">>, ?S:msg(Missing)),
                Ok = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary, "/messages">>,
                    #{
                        <<"installation_id">> => ?INSTALL,
                        <<"body">> => <<"hi">>,
                        <<"client_msg_id">> => <<"cmid-1">>
                    },
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(200, ?S:status(Ok)),
                ?assertEqual(int_bin(99001), maps:get(<<"id">>, ?S:payload(Ok)))
            end)
        end},

        {"A01 visitor history honors after_id cursor (compensation semantics)", fun() ->
            meck:expect(customer_service_facade, widget_history_after, fun(Org, Params) ->
                ?assertEqual(0, Org),
                ?assertNot(is_map_key(organization_id, Params)),
                ?assertEqual(41, maps:get(after_id, Params)),
                {ok, [#{id => 42, body => <<"m1">>}]}
            end),
            ?S:with_listener(widget, widget_session_messages, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary,
                        "/messages?installation_id=", (int_bin(?INSTALL))/binary, "&after_id=41">>,
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                [Msg] = ?S:payload(Resp),
                ?assertEqual(<<"42">>, maps:get(<<"id">>, Msg))
            end)
        end},

        {"A01 identity exchange passes the assertion map through", fun() ->
            meck:expect(customer_service_facade, widget_identity_exchange, fun(Org, Params) ->
                ?assertEqual(0, Org),
                ?assertNot(is_map_key(organization_id, Params)),
                Assertion = maps:get(assertion, Params),
                %% 嵌套对象键保持 JSON 原样（binary）；形状判定在 application。
                ?assertEqual(1, maps:get(<<"key_version">>, Assertion)),
                {ok, #{
                    installation_id => ?INSTALL,
                    contact_id => ?CONTACT,
                    anonymous_contact_id => ?CONTACT,
                    contact_reused => true
                }}
            end),
            ?S:with_listener(widget, widget_identity_exchange, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/identity/exchange">>,
                    #{
                        <<"installation_id">> => ?INSTALL,
                        <<"assertion">> =>
                            #{<<"key_version">> => 1, <<"claims">> => #{<<"jti">> => <<"j">>}}
                    },
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(int_bin(?CONTACT), maps:get(<<"contact_id">>, ?S:payload(Resp)))
            end)
        end}
    ].

%% ===================================================================
%% BE-S01b：访客附件内容代理（GET .../sessions/:id/assets/:asset_id/content）
%% 响应是对象字节本体（mime 定 content-type，private no-store）——不走
%% cs_http:respond 的 JSON 面。
%% ===================================================================

asset_content_tests(_) ->
    [
        {"BE-S01b content proxy streams asset bytes with mime content-type", fun() ->
            meck:expect(customer_service_facade, widget_asset_content, fun(Org, Params) ->
                ?assertEqual(0, Org),
                ?assertNot(is_map_key(organization_id, Params)),
                ?assertEqual(?SESSION, maps:get(session_id, Params)),
                ?assertEqual(990001, maps:get(asset_id, Params)),
                ?assertEqual(?INSTALL, maps:get(installation_id, Params)),
                ?assertEqual(?TOKEN, maps:get(secret, Params)),
                {ok, #{
                    asset_id => 990001,
                    mime => <<"image/png">>,
                    size_bytes => 5,
                    object_hash =>
                        <<"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef">>,
                    body => <<"BYTES">>
                }}
            end),
            ?S:with_listener(widget, widget_asset_content, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary,
                        "/assets/990001/content?installation_id=", (int_bin(?INSTALL))/binary>>,
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Headers = maps:get(headers, Resp),
                ?assertEqual(<<"image/png">>, maps:get(<<"content-type">>, Headers)),
                ?assertEqual(<<"private, no-store">>, maps:get(<<"cache-control">>, Headers)),
                ?assertEqual(<<"BYTES">>, maps:get(body, Resp))
            end)
        end},
        {"BE-S01b content proxy without token is 401 before the use case", fun() ->
            ?S:with_listener(widget, widget_asset_content, widget_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary,
                        "/assets/990001/content?installation_id=", (int_bin(?INSTALL))/binary>>,
                    <<>>,
                    #{}
                ),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, widget_asset_content, '_'))
            end)
        end},
        {"BE-S01b content proxy cross-session asset is structured 404 JSON (not bytes)", fun() ->
            meck:expect(customer_service_facade, widget_asset_content, fun(_Org, _Params) ->
                {error, not_found}
            end),
            ?S:with_listener(widget, widget_asset_content, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary,
                        "/assets/990001/content?installation_id=", (int_bin(?INSTALL))/binary>>,
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(404, ?S:status(Resp)),
                ?assertEqual(<<"not_found">>, ?S:msg(Resp))
            end)
        end},
        {"BE-S01b content proxy with credential in query string is 400", fun() ->
            ?S:with_listener(widget, widget_asset_content, widget_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary,
                        "/assets/990001/content?installation_id=", (int_bin(?INSTALL))/binary,
                        "&token=", ?TOKEN/binary>>,
                    <<>>,
                    #{}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, widget_asset_content, '_'))
            end)
        end}
    ].

%% ===================================================================
%% BE-W01 A06：capability_disabled 收敛 + 三面凭据矩阵（独立 foreach 组，
%% meck passthrough 无 expect——门在真 facade 入口）
%% ===================================================================

a06_capability_and_matrix_tests(_) ->
    [
        %% BE-W01 A06：第一阶段 capability_disabled（默认关）。走真 facade
        %% （meck passthrough 未 expect 该函数）——门在 facade 入口，请求
        %% 不触签名断言链、不触 DB。
        {"A06 identity exchange defaults to capability_disabled (403, explicit tag)", fun() ->
            meck:reset(customer_service_facade),
            ?S:with_listener(widget, widget_identity_exchange, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/identity/exchange">>,
                    #{
                        <<"installation_id">> => ?INSTALL,
                        <<"assertion">> =>
                            #{<<"key_version">> => 1, <<"claims">> => #{<<"jti">> => <<"j">>}}
                    },
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertEqual(<<"capability_disabled.identity_exchange">>, ?S:msg(Resp))
            end)
        end},

        %% BE-W01 A06：开关显式开启后原签名断言链保留（真 facade 门放行 →
        %% 打桩 application 层（零 DB），证明 true 路径未删除）。
        {"A06 identity exchange enabled path still routes the assertion chain", fun() ->
            SavedSwitch = application:get_env(imboy, cs_widget_identity_exchange_enabled),
            try
                ok = application:set_env(imboy, cs_widget_identity_exchange_enabled, true),
                meck:new(cs_widget_support, [passthrough]),
                %% CSD-BE-01S：零 org 申报面——HTTP 链里租户由 token digest 命中
                %% 行派生；本用例打桩派生点（零 DB），证明 enabled 路径照常路由。
                meck:expect(cs_widget_support, derive_org_by_token, fun(_Params) ->
                    {ok, ?ORG}
                end),
                meck:new(cs_widget_app, [passthrough]),
                meck:expect(cs_widget_app, identity_exchange, fun(Org, Params) ->
                    ?assertEqual(?ORG, Org),
                    ?assert(is_map(maps:get(assertion, Params))),
                    {ok, #{
                        installation_id => ?INSTALL,
                        contact_id => ?CONTACT,
                        anonymous_contact_id => ?CONTACT,
                        contact_reused => false
                    }}
                end),
                ?S:with_listener(widget, widget_identity_exchange, widget_inject(), fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<"/api/v1/cs/widget/identity/exchange">>,
                        #{
                            <<"installation_id">> => ?INSTALL,
                            <<"assertion">> =>
                                #{
                                    <<"key_version">> => 1,
                                    <<"claims">> => #{<<"jti">> => <<"j">>},
                                    <<"sig">> => <<"c2ln">>
                                }
                        },
                        #{<<"x-cs-visit-token">> => ?TOKEN}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    ?assert(meck:called(cs_widget_app, identity_exchange, '_'))
                end)
            after
                meck:unload(cs_widget_app),
                meck:unload(cs_widget_support),
                case SavedSwitch of
                    undefined ->
                        _ = application:unset_env(
                            imboy, cs_widget_identity_exchange_enabled
                        );
                    {ok, V} ->
                        ok = application:set_env(
                            imboy, cs_widget_identity_exchange_enabled, V
                        )
                end
            end
        end},

        %% BE-W01 A06 凭据矩阵（eunit 层模拟）：三类凭据不互换。
        {"A06 admin cookie is not a widget credential (401)", fun() ->
            meck:reset(customer_service_facade),
            ?S:with_listener(widget, widget_sessions, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions">>,
                    #{<<"installation_id">> => ?INSTALL},
                    #{<<"cookie">> => <<"imboy_adm_sid=adm-session-1">>}
                ),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertEqual(<<"credential_missing">>, ?S:msg(Resp)),
                ?assertNot(meck:called(customer_service_facade, widget_create_session, '_'))
            end)
        end},

        {"A06 widget visit token is not a seat credential (401)", fun() ->
            %% 不注入 current_uid（中间件只在 Authorization Bearer JWT 通过后
            %% 注入）：浏览器只带 visit token 头时坐席面没有任何可采信凭据。
            cs_fake_facts:set(seat_facts()),
            ?S:with_listener(
                tenant,
                session_detail,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                            (int_bin(?SESSION))/binary, "?workspace_id=", (int_bin(?WS))/binary>>,
                        <<>>,
                        %% visit token 头冒充坐席 JWT：坐席面只认 Authorization
                        %% Bearer（current_uid）——头类型不对即 401，绝不降级采信。
                        #{<<"x-cs-visit-token">> => ?TOKEN}
                    ),
                    ?assertEqual(401, ?S:status(Resp))
                end
            )
        end},

        {"A06 widget visit token is not a platform admin credential (401)", fun() ->
            ?S:with_listener(
                platform,
                p_widget_installations,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<"/api/adm/customer-service/widget-installations?organization_id=",
                            (int_bin(?ORG))/binary, "&workspace_id=", (int_bin(?WS))/binary>>,
                        <<>>,
                        #{<<"x-cs-visit-token">> => ?TOKEN}
                    ),
                    ?assertEqual(401, ?S:status(Resp))
                end
            )
        end}
    ].

%% ===================================================================
%% 访客附件上传面（REVIEW-2 凭证负例缺口闭环：`widget_asset_upload` /
%% `widget_asset_confirm` 在本套件此前零覆盖——E2E A05 只证明合法链，
%% BE 接口层的拒绝面必须在这里锁死）。凭证门与 session create 同源
%% （a01 口径）：缺头 401、查询串携带 400、无效 token 由 facade 裁决
%% 映射 401；零 DB（meck facade + cs_fake_facts）。
%% ===================================================================

asset_upload_tests(_) ->
    PresignPath = <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary, "/assets/presign">>,
    [
        {"BE-S01 presign without visit-token header is 401 and never reaches facade", fun() ->
            meck:expect(customer_service_facade, widget_asset_upload, fun(_O, _P) ->
                {ok, #{}}
            end),
            ?S:with_listener(widget, widget_asset_upload, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    PresignPath,
                    #{},
                    #{}
                ),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, widget_asset_upload, '_'))
            end)
        end},

        {"BE-S01 presign credential in query string is 400 (token travels in header only)", fun() ->
                ?S:with_listener(widget, widget_asset_upload, widget_inject(), fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        <<PresignPath/binary, "?token=", ?TOKEN/binary>>,
                        #{},
                        #{}
                    ),
                    ?assertEqual(400, ?S:status(Resp)),
                    ?assertEqual(<<"credential_in_query_string">>, ?S:msg(Resp))
                end)
            end},

        {"BE-S01 presign invalid visit token is 401 visit_token_invalid (facade ruling maps to credential face)",
            fun() ->
                meck:expect(customer_service_facade, widget_asset_upload, fun(_O, _P) ->
                    {error, visit_token_invalid}
                end),
                %% 参数面取合法形状（四必填齐全），让请求穿过参数校验到达
                %% facade——本用例锁的是凭证裁决，不是参数校验（422 是另一条门）。
                Body = #{
                    <<"installation_id">> => int_bin(?INSTALL),
                    <<"mime">> => <<"text/plain">>,
                    <<"size_bytes">> => 3,
                    <<"object_hash">> =>
                        <<"a1b2c3d4e5f6a7b8a1b2c3d4e5f6a7b8a1b2c3d4e5f6a7b8a1b2c3d4e5f6a7b8">>
                },
                ?S:with_listener(widget, widget_asset_upload, widget_inject(), fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        PresignPath,
                        Body,
                        #{<<"x-cs-visit-token">> => <<"wtok-forged-never-issued">>}
                    ),
                    ?assertEqual(401, ?S:status(Resp)),
                    ?assertEqual(<<"visit_token_invalid">>, ?S:msg(Resp))
                end)
            end},

        {"BE-S01 confirm without visit-token header is 401 and never reaches facade", fun() ->
            meck:expect(customer_service_facade, widget_asset_confirm, fun(_O, _P) ->
                {ok, #{}}
            end),
            ?S:with_listener(widget, widget_asset_confirm, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary,
                        "/assets/confirm">>,
                    #{<<"upload_ref">> => <<"ref-1">>},
                    #{}
                ),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, widget_asset_confirm, '_'))
            end)
        end}
    ].

%% ===================================================================
%% A02：SSE（retry + 首状态事件；Last-Event-ID 补偿；保活；状态变更）
%% ===================================================================

events_path() ->
    <<"/api/v1/cs/widget/sessions/", (int_bin(?SESSION))/binary, "/events?installation_id=",
        (int_bin(?INSTALL))/binary>>.

a02_sse_tests(_) ->
    [
        {"A02 SSE opens with retry line and current state resource-id event", fun() ->
            meck:expect(customer_service_facade, widget_list_sessions, fun(Org, Params) ->
                %% CSD-BE-01S：零 org 申报面——OrgId 占位 0（facade 侧派生）。
                ?assertEqual(0, Org),
                ?assertNot(is_map_key(organization_id, Params)),
                {ok, [#{id => ?SESSION, status => queued}]}
            end),
            meck:expect(customer_service_facade, widget_history_after, fun(Org, Params) ->
                %% CSD-BE-01S（GAP-5）：补偿读把 token-scoped session_id 传入
                %% widget_history_after——缺它 = 轮询恒 {invalid_argument,*}、
                %% 坐席消息帧结构性永不出。
                ?assertEqual(0, Org),
                ?assertEqual(?SESSION, maps:get(session_id, Params)),
                {ok, []}
            end),
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Raw = ?S:stream_request(
                    Port,
                    <<"GET">>,
                    events_path(),
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN},
                    400
                ),
                ?assert(string:find(Raw, <<"text/event-stream">>) =/= nomatch),
                ?assert(string:find(Raw, <<"retry: 3000">>) =/= nomatch),
                ?assert(string:find(Raw, <<"event: state">>) =/= nomatch),
                ?assert(string:find(Raw, <<"id: 0">>) =/= nomatch),
                ?assert(
                    string:find(Raw, <<"\"session_id\":\"", (int_bin(?SESSION))/binary, "\"">>) =/=
                        nomatch
                ),
                ?assert(string:find(Raw, <<"\"status\":\"queued\"">>) =/= nomatch)
            end)
        end},

        {"A02 Last-Event-ID resumes from the message cursor without replay", fun() ->
            meck:expect(customer_service_facade, widget_list_sessions, fun(_O, _P) ->
                {ok, [#{id => ?SESSION, status => claimed}]}
            end),
            meck:expect(customer_service_facade, widget_history_after, fun(_O, Params) ->
                %% 断线重连不重不漏：补偿从 Last-Event-ID 游标起（消息表 after_id）；
                %% 后续轮询从新游标继续（此处无新增消息）。session 级键随行
                %% （CSD-BE-01S GAP-5）。
                ?assertEqual(?SESSION, maps:get(session_id, Params)),
                case maps:get(after_id, Params, undefined) of
                    41 -> {ok, [#{id => 42, body => <<"m1">>}, #{id => 43, body => <<"m2">>}]};
                    43 -> {ok, []};
                    Other -> erlang:error({unexpected_cursor, Other})
                end
            end),
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Raw = ?S:stream_request(
                    Port,
                    <<"GET">>,
                    events_path(),
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN, <<"last-event-id">> => <<"41">>},
                    400
                ),
                ?assert(string:find(Raw, <<"event: message">>) =/= nomatch),
                ?assert(string:find(Raw, <<"id: 42">>) =/= nomatch),
                ?assert(string:find(Raw, <<"id: 43">>) =/= nomatch),
                %% TSID string 出站（消息 data 里 id 是字符串）。
                ?assert(string:find(Raw, <<"\"id\":\"42\"">>) =/= nomatch),
                %% 首状态事件落在游标位（id=41，单调不回退）；游标**消息**不重放：
                %% 不存在 id=41 的 message 帧（41 号在断线前已送达）。
                ?assert(
                    string:find(Raw, <<"id: 41\nevent: state">>) =/= nomatch
                ),
                ?assert(string:find(Raw, <<"id: 41\nevent: message">>) =:= nomatch)
            end)
        end},

        %% DF-5 回归：流内 `widget_history_after` 补偿读与 REST 历史同源——真
        %% facade 的 visitor_session_scope 需要 session_id 裁决会话归属。此前
        %% scoped/1 丢键使补偿读恒 invalid_argument 且被 stream_step 静默吞掉，
        %% message/state 帧全死（长流只剩初始 state 帧）。这里在 facade mock
        %% 边界锁死 session_id 投影：缺键/错键即测试失败。
        {"DF-5 SSE polling read carries session_id so message frames survive", fun() ->
            meck:expect(customer_service_facade, widget_list_sessions, fun(_O, _P) ->
                {ok, [#{id => ?SESSION, status => queued}]}
            end),
            meck:expect(customer_service_facade, widget_history_after, fun(_O, Params) ->
                case maps:get(session_id, Params, undefined) of
                    ?SESSION ->
                        case maps:get(after_id, Params, undefined) of
                            undefined -> {ok, [#{id => 9, body => <<"df5-m1">>}]};
                            9 -> {ok, []};
                            Other -> erlang:error({unexpected_cursor, Other})
                        end;
                    MissingOrWrong ->
                        erlang:error({df5_session_id_missing, MissingOrWrong})
                end
            end),
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Raw = ?S:stream_request(
                    Port,
                    <<"GET">>,
                    events_path(),
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN},
                    400
                ),
                ?assert(string:find(Raw, <<"event: message">>) =/= nomatch),
                ?assert(string:find(Raw, <<"id: 9">>) =/= nomatch),
                ?assert(string:find(Raw, <<"df5-m1">>) =/= nomatch)
            end)
        end},

        %% DF-10 回归：流内轮询命中凭证终态失效（token_revoked 族）必须立即
        %% fin 关流，而非静默保持至 sse_max_ms。修复前 stream_step 吞掉一切
        %% 轮询错误，已吊销访客的既有流保持 open——撤权对访客不可感知（任务
        %% 书 A04「既有 SSE 流立即降级」不满足）。判定口径：sse_max_ms 拉长
        %% 到 60s、读窗口 2s——修复后首轮 poll（30ms）关流 → EOF 提前返回；
        %% 修复前静默保持 → 读满 2s 超时返回（elapsed 越窗即未修复）。
        {"DF-10 poll hitting terminal revocation closes the stream instead of silent keep-open",
            fun() ->
                meck:expect(customer_service_facade, widget_list_sessions, fun(_O, _P) ->
                    {ok, [#{id => ?SESSION, status => queued}]}
                end),
                meck:expect(customer_service_facade, widget_history_after, fun(_O, _P) ->
                    {error, token_revoked}
                end),
                ?S:with_listener(widget, widget_session_events, sse_inject_fatal(), fun(Port) ->
                    T0 = erlang:monotonic_time(millisecond),
                    Raw = ?S:stream_request(
                        Port,
                        <<"GET">>,
                        events_path(),
                        <<>>,
                        #{<<"x-cs-visit-token">> => ?TOKEN},
                        2000
                    ),
                    Elapsed = erlang:monotonic_time(millisecond) - T0,
                    %% 流开过（关流前初始 state 帧已发出）。
                    ?assert(string:find(Raw, <<"event: state">>) =/= nomatch),
                    %% EOF 提前返回：远小于读窗口（静默保持时会读满 2000ms）。
                    ?assert(Elapsed < 1500)
                end)
            end},

        {"A02 idle stream emits keep-alive comment lines", fun() ->
            meck:expect(customer_service_facade, widget_list_sessions, fun(_O, _P) ->
                {ok, [#{id => ?SESSION, status => queued}]}
            end),
            meck:expect(customer_service_facade, widget_history_after, fun(_O, _P) ->
                {ok, []}
            end),
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Raw = ?S:stream_request(
                    Port,
                    <<"GET">>,
                    events_path(),
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN},
                    500
                ),
                ?assert(string:find(Raw, <<": keep-alive">>) =/= nomatch)
            end)
        end},

        {"A02 session status change produces a follow-up state event", fun() ->
            Counter = counters:new(1, []),
            meck:expect(customer_service_facade, widget_list_sessions, fun(_O, _P) ->
                case counters:get(Counter, 1) of
                    0 ->
                        counters:add(Counter, 1, 1),
                        {ok, [#{id => ?SESSION, status => queued}]};
                    _ ->
                        {ok, [#{id => ?SESSION, status => closed}]}
                end
            end),
            meck:expect(customer_service_facade, widget_history_after, fun(_O, _P) ->
                {ok, []}
            end),
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Raw = ?S:stream_request(
                    Port,
                    <<"GET">>,
                    events_path(),
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN},
                    500
                ),
                ?assert(string:find(Raw, <<"\"status\":\"queued\"">>) =/= nomatch),
                ?assert(string:find(Raw, <<"\"status\":\"closed\"">>) =/= nomatch),
                %% 首状态事件 + 变更事件：至少两块 state 帧。
                ?assert(length(string:split(Raw, <<"event: state">>, all)) >= 2)
            end)
        end},

        %% CSD-BE-01S（GAP-5 用例级 oracle）：坐席消息经 SSE **message 帧**到达
        %% ——轮询参数带 session_id（history_after 契约闭合）后，轮询期落库的
        %% 坐席消息以 `event: message` 帧送达（真实流验证归 E2E-03）。
        {"CSD-BE-01S agent message delivered as an SSE message frame mid-stream", fun() ->
            Counter = counters:new(1, []),
            meck:expect(customer_service_facade, widget_list_sessions, fun(_O, _P) ->
                {ok, [#{id => ?SESSION, status => claimed}]}
            end),
            meck:expect(customer_service_facade, widget_history_after, fun(_O, Params) ->
                %% GAP-5 契约：轮询必须携带 session_id（scoped 曾剥掉它，
                %% facade 强制要求 → 每次轮询 invalid_argument 静默重试）。
                ?assertEqual(?SESSION, maps:get(session_id, Params)),
                case counters:get(Counter, 1) of
                    0 ->
                        counters:add(Counter, 1, 1),
                        {ok, []};
                    _ ->
                        {ok, [
                            #{
                                id => 77,
                                conversation_id => ?CONVERSATION,
                                body => <<"agent says hi">>,
                                sender_kind => business_identity,
                                kind => text
                            }
                        ]}
                end
            end),
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Raw = ?S:stream_request(
                    Port,
                    <<"GET">>,
                    events_path(),
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN},
                    400
                ),
                ?assert(string:find(Raw, <<"event: message">>) =/= nomatch),
                ?assert(string:find(Raw, <<"id: 77">>) =/= nomatch),
                ?assert(string:find(Raw, <<"agent says hi">>) =/= nomatch),
                %% 零 secret：消息帧是投影白名单出站。
                ?assert(string:find(Raw, <<"secret">>) =:= nomatch)
            end)
        end},

        {"A02 SSE without token is a structured 401 before the stream opens", fun() ->
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, events_path(), <<>>, #{}),
                ?assertEqual(401, ?S:status(Resp)),
                ?assertEqual(<<"credential_missing">>, ?S:msg(Resp))
            end)
        end},

        {"A02 SSE for a session outside the token scope is 404 (never streams)", fun() ->
            meck:expect(customer_service_facade, widget_list_sessions, fun(_O, _P) ->
                {ok, [#{id => 999999, status => queued}]}
            end),
            ?S:with_listener(widget, widget_session_events, sse_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    events_path(),
                    <<>>,
                    #{<<"x-cs-visit-token">> => ?TOKEN}
                ),
                ?assertEqual(404, ?S:status(Resp))
            end)
        end},

        {"A02 SSE frames are exact text/event-stream syntax (unit)", fun() ->
            Frame = cs_widget_handler:event_frame(42, <<"message">>, <<"{\"id\":\"42\"}">>),
            ?assertEqual(<<"id: 42\nevent: message\ndata: {\"id\":\"42\"}\n\n">>, Frame),
            ?assertEqual(<<"retry: 3000\n">>, cs_widget_handler:retry_frame(3000)),
            ?assertEqual(<<": keep-alive\n\n">>, cs_widget_handler:comment_frame()),
            State = cs_widget_handler:state_data(?SESSION, queued),
            ?assertEqual(
                #{
                    <<"resource">> => <<"cs.session">>,
                    <<"session_id">> => int_bin(?SESSION),
                    <<"status">> => <<"queued">>
                },
                jsone:decode(State)
            ),
            MsgData = cs_widget_handler:message_data(#{
                id => 42, body => <<"hi">>, secret => <<"never">>
            }),
            Decoded = jsone:decode(MsgData),
            ?assertNot(is_map_key(<<"secret">>, Decoded)),
            ?assertEqual(<<"hi">>, maps:get(<<"body">>, Decoded))
        end}
    ].

%% ===================================================================
%% A03：动态 CORS（echo 具体 Origin；绝不通配；vary）
%% ===================================================================

a03_cors_tests(_) ->
    [
        {"A03 success echoes the normalized specific origin (never wildcard)", fun() ->
            meck:expect(customer_service_facade, widget_bootstrap, fun(_O, _P) ->
                {ok, bootstrap_view()}
            end),
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/bootstrap">>,
                    bootstrap_body(),
                    #{<<"origin">> => <<"HTTPS://Shop.Example.COM">>}
                ),
                ?assertEqual(200, ?S:status(Resp)),
                Headers = maps:get(headers, Resp),
                ?assertEqual(?ORIGIN, maps:get(<<"access-control-allow-origin">>, Headers)),
                ?assertNotEqual(<<"*">>, maps:get(<<"access-control-allow-origin">>, Headers)),
                ?assert(
                    string:find(maps:get(<<"vary">>, Headers, <<>>), <<"Origin">>) =/= nomatch
                )
            end)
        end},

        {"A03 rejected request gains no ACAO from the handler (credentials untouched)", fun() ->
            meck:expect(customer_service_facade, widget_bootstrap, fun(_O, _P) ->
                {error, origin_not_allowed}
            end),
            ?S:with_listener(widget, widget_bootstrap, widget_inject(), fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"POST">>,
                    <<"/api/v1/cs/widget/bootstrap">>,
                    bootstrap_body(),
                    #{<<"origin">> => ?ORIGIN}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                Headers = maps:get(headers, Resp),
                ?assertNot(
                    is_map_key(<<"access-control-allow-origin">>, Headers)
                ),
                ?assertNot(
                    is_map_key(<<"access-control-allow-credentials">>, Headers)
                )
            end)
        end}
    ].

%% ===================================================================
%% A04：限流（真实 throttle_middleware 链 + cs_widget_per_ip 专用桶 → 429）
%% ===================================================================

a04_throttle_tests(_) ->
    [
        {"A04 third widget request inside the window is throttled with 429", fun() ->
            {ok, _} = application:ensure_all_started(throttle),
            ok = throttle:setup(cs_widget_per_ip, 2, per_minute),
            {Pattern, Opts} = ?S:route_opt(widget, widget_bootstrap),
            Name = list_to_atom(
                "csb03_throttle_" ++ integer_to_list(erlang:unique_integer([positive]))
            ),
            Dispatch = cowboy_router:compile([
                {'_', [{binary_to_list(to_bin(Pattern)), cs_widget_handler, Opts}]}
            ]),
            {ok, _} = cowboy:start_clear(Name, [{port, 0}], #{
                env => #{dispatch => Dispatch},
                middlewares => [cowboy_router, throttle_middleware, cowboy_handler]
            }),
            Port = ranch:get_port(Name),
            try
                R1 = ?S:request(Port, <<"GET">>, <<"/api/v1/cs/widget/bootstrap">>, <<>>, #{}),
                R2 = ?S:request(Port, <<"GET">>, <<"/api/v1/cs/widget/bootstrap">>, <<>>, #{}),
                %% 前两发放行（抵达 handler 的方法门 → 405），第三发 429。
                ?assertEqual(405, ?S:status(R1)),
                ?assertEqual(405, ?S:status(R2)),
                R3 = ?S:request(Port, <<"GET">>, <<"/api/v1/cs/widget/bootstrap">>, <<>>, #{}),
                ?assertEqual(429, ?S:status(R3))
            after
                ok = cowboy:stop_listener(Name),
                %% 复位为代码级缺省，避免污染同节点后续套件。
                _ = throttle:setup(cs_widget_per_ip, 30, per_minute)
            end
        end}
    ].

%% ===================================================================
%% A05：坐席会话详情 + 契约（TSID string / map 透传）
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

a05_contract_and_seat_tests(_) ->
    [
        {"A05 seat session detail via GET /api/v1/cs/sessions/:id (200; TSID string)", fun() ->
            cs_fake_facts:set(seat_facts()),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            meck:expect(customer_service_facade, seat_session_detail, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?SESSION, maps:get(session_id, Params)),
                ?assertEqual(?WS, maps:get(workspace_id, Params)),
                {ok, #{
                    id => ?SESSION,
                    status => claimed,
                    conversation_id => ?CONVERSATION,
                    business_identity_id => ?IDENTITY,
                    version => 3
                }}
            end),
            ?S:with_listener(
                tenant,
                session_detail,
                #{
                    auth_facts => cs_fake_facts,
                    current_uid => ?UID
                },
                fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                            (int_bin(?SESSION))/binary, "?workspace_id=", (int_bin(?WS))/binary>>,
                        <<>>,
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(200, ?S:status(Resp)),
                    Payload = ?S:payload(Resp),
                    ?assertEqual(int_bin(?SESSION), maps:get(<<"id">>, Payload)),
                    ?assertEqual(int_bin(?CONVERSATION), maps:get(<<"conversation_id">>, Payload)),
                    ?assertEqual(<<"claimed">>, maps:get(<<"status">>, Payload))
                end
            )
        end},

        {"A05 seat session detail without seat JWT is 401", fun() ->
            cs_fake_facts:set(seat_facts()),
            ?S:with_listener(
                tenant,
                session_detail,
                #{auth_facts => cs_fake_facts},
                fun(Port) ->
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/sessions/",
                            (int_bin(?SESSION))/binary, "?workspace_id=", (int_bin(?WS))/binary>>,
                        <<>>,
                        #{}
                    ),
                    ?assertEqual(401, ?S:status(Resp))
                end
            )
        end}
    ].

to_bin(Bin) when is_binary(Bin) -> Bin;
to_bin(List) when is_list(List) -> unicode:characters_to_binary(List).
