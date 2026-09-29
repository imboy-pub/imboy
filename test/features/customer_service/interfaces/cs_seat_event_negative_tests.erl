%%% @doc 坐席 SSE / heartbeat 域负例缺口补遗（车道 R2-3：已有套件矩阵盘点后
%%% 的真实缺口，勿与既有覆盖重复）。
%%%
%%% 纪律与 cs_seat_credential_negative_tests 同源：真 cowboy 监听器（真路由
%%% Opts，含 surface/auth_facts 注入）+ 真 handler/cs_auth 链 + meck 的
%%% facade；零 DB——不跑任何数据库测试。测试监听器不挂 auth 中间件（生产由
%%% auth_middleware_api_v1 写入 current_uid），测试扮演中间件注入角色；每类
%%% 凭证的注入集都与「生产中间件对该凭证的真实裁决」对齐。
%%%
%%% 已有覆盖（本套件不重复，见各套件）：
%%%   * seat_events 无 JWT → 401（cs_seat_workbench_tests「Seat events
%%%     without JWT is 401 before streaming」已锁，含 facade 零调用）；
%%%   * seat_events 跨 org 游标 → 403 / 非法 Last-Event-ID → 400 / 缺
%%%     workspace_id → 422（同上 workbench 套件）；事件流 (Org, Workspace)
%%%     同语句 scope 隔离在 application 层 cs_seat_event_app_tests
%%%     「workspace_scope_is_strict」已锁（零 DB，无需真 SSE 长连）；
%%%   * heartbeat 无 JWT → 401（cs_seat_credential_negative_tests 无 JWT
%%%     矩阵第三端点）。
%%%
%%% 本套件补的真实缺口：
%%%   * **seat_events 畸形 Authorization 矩阵**（非 Bearer scheme /
%%%     有 scheme 无 token / 空 token）：workbench 只锁了「无头」形态；
%%%     生产语义（auth_ds:condition/do_authorization）对畸形形态是
%%%     verify_token → 706（malformed）→ 中间件自身 401 截停、绝不注入
%%%     current_uid——测试以「未注入」注入集请求，真 handler 链给出 401
%%%     兜底；中间件级 706 截停已由 cs_seat_credential_negative_tests 的
%%%     auth_ds:verify_token 单元级实证，不重复。SSE 端点只断言「未授权时
%%%     连接被拒（HTTP 401）」，不做流内容断言（那属 E2E）。
%%%   * **heartbeat 路径 org 与本人 assignment 归属不一致 → 403**：cs_auth
%%%     seat_identity 的同语句过滤（user_id =:= 凭证 user 且 organization_id
%%%     =:= 路径 Org 且 active 且 customer_service）零命中 →
%%%     identity_assignment_missing → 403；seat enabled 门（fetch_seat）未达，
%%%     业务 facade 零触达。与「他人 assignment」维度（credential negative
%%%     已锁 user_id 不匹配）互补，此处锁 organization_id 归属隔离。
%%%     附：heartbeat 契约**无 expected_version CAS**（store heartbeat_seat
%%%     第 4 参恒 undefined）——「旧 expected_version → 409」对 heartbeat
%%%     不适用；stale_version 409 已由 cs_handler_tests（session transfer
%%%     HTTP 链）与 cs_route_contract_tests（错误映射）锁定。
-module(cs_seat_event_negative_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(ORG, 7002001).
-define(OTHER_ORG, 7002999).
-define(WS, 90011).
-define(UID, 425252).
-define(IDENTITY, 516161).

%% ===================================================================
%% 套件夹具
%% ===================================================================

seat_event_negative_test_() ->
    {foreach,
        fun() ->
            {ok, _} = application:ensure_all_started(cowboy),
            meck:new(customer_service_facade, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            cs_fake_facts:clear(),
            ok
        end,
        [fun events_malformed_authorization_tests/1, fun heartbeat_foreign_org_tests/1]}.

%% 与 credential negative 套件同构：合法 Bearer 形态 ⇒ 生产中间件过 JWT 门
%% 并注入会话键，测试扮演注入角色（凭证真伪由中间件裁决，不在本套件复验范围）。
seat_inject() ->
    #{auth_facts => cs_fake_facts, current_uid => ?UID}.

events_path() ->
    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/seats/me/events?workspace_id=",
        (int_bin(?WS))/binary>>.

heartbeat_path() ->
    <<"/api/v1/cs/organizations/", (int_bin(?ORG))/binary, "/seats/me/heartbeat">>.

int_bin(N) ->
    integer_to_binary(N).

%% ===================================================================
%% seat_events 畸形 Authorization 矩阵（无头形态 workbench 已锁，不重复）
%% ===================================================================
%%
%% 生产行为对照（auth_middleware_api_v1 → auth_ds:condition/do_authorization，
%% 与 credential negative 套件 queue 矩阵同源）：
%%   ① 非 Bearer scheme（"Basic dXNlcjpwYXNz"）：parse_authorization_header
%%     不剥前缀，整串进 verify_token → 706 → 中间件自身 401 截停；
%%   ② "Bearer"（有 scheme 无 token）：无 "Bearer " 前缀（缺空格）→ 同 ① → 706；
%%   ③ "Bearer "（空 token）：前缀剥出空 token → 验签必败 → 706。
%% 三形态生产中间件均不注入 current_uid——测试以「未注入」注入集请求，
%% 真 handler 链（cs_auth credential_missing）给 401 兜底；对外合同与中间件
%% 截停一致：401 + 业务零触达（seat_events / fetch_seat 均不被调用）。

events_malformed_authorization_tests(_) ->
    [
        {"Seat events GET with a non-Bearer scheme (Basic) is 401", fun() ->
            assert_events_unauthorized(#{<<"authorization">> => <<"Basic dXNlcjpwYXNz">>})
        end},
        {"Seat events GET with a scheme-only Bearer (no token) is 401", fun() ->
            assert_events_unauthorized(#{<<"authorization">> => <<"Bearer">>})
        end},
        {"Seat events GET with an empty Bearer token is 401", fun() ->
            assert_events_unauthorized(#{<<"authorization">> => <<"Bearer ">>})
        end}
    ].

assert_events_unauthorized(Headers) ->
    ?S:with_listener(
        tenant,
        seat_events,
        #{auth_facts => cs_fake_facts},
        fun(Port) ->
            meck:reset(customer_service_facade),
            Resp = ?S:request(Port, <<"GET">>, events_path(), <<>>, Headers),
            %% SSE 端点的负例合同只到「连接被拒」：401 + 零业务触达，
            %% 不做流内容断言（流帧属 E2E 覆盖面）。
            ?assertEqual(401, ?S:status(Resp)),
            ?assertNot(meck:called(customer_service_facade, seat_events, '_')),
            ?assertNot(meck:called(customer_service_facade, fetch_seat, '_'))
        end
    ).

%% ===================================================================
%% heartbeat 路径 org 与本人 assignment 归属不一致 → 403
%% ===================================================================
%%
%% 凭证为本人（合法 Bearer 形态注入），但事实里本人唯一的 customer_service
%% assignment 挂在**另一个 org**：cs_auth seat_identity 以路径 OrgId 作同语句
%% 过滤条件 → 零命中 → identity_assignment_missing → 403。认证与事实装配先于
%% 一切业务门：seat enabled 门（fetch_seat）与 facade 心跳用例均零触达；
%% 认证先于参数校验——缺 workspace_id / 缺 body 形态不影响裁决（不先 422）。

heartbeat_foreign_org_tests(_) ->
    [
        {
            "Heartbeat with the seat assignment homed in another org is 403 and never "
            "reaches seat_heartbeat",
            fun() ->
                cs_fake_facts:set(#{
                    organization_id => ?OTHER_ORG,
                    member => #{user_id => ?UID, status => active, governance_roles => []},
                    assignments => [
                        #{
                            business_identity_id => ?IDENTITY,
                            user_id => ?UID,
                            organization_id => ?OTHER_ORG,
                            function_key => <<"customer_service">>,
                            status => active,
                            version => 1
                        }
                    ],
                    permissions => [<<"conversation.read">>]
                }),
                ?S:with_listener(tenant, seat_presence_heartbeat, seat_inject(), fun(Port) ->
                    meck:reset(customer_service_facade),
                    Resp = ?S:request(
                        Port,
                        <<"POST">>,
                        heartbeat_path(),
                        #{},
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(403, ?S:status(Resp)),
                    ?assertNot(meck:called(customer_service_facade, seat_heartbeat, '_')),
                    ?assertNot(meck:called(customer_service_facade, fetch_seat, '_'))
                end)
            end
        }
    ].
