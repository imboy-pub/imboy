%%% @doc 坐席面凭证负例矩阵（REVIEW-2 P2「凭证类负例缺口」的 BE 层闭环）。
%%%
%%% 纪律与 cs_seat_workbench_tests 同源：真 cowboy 监听器（真路由 Opts，含
%%% surface/auth_facts 注入）+ 真 handler/cs_auth 链 + **meck 的 facade**；
%%% 零 DB——不跑任何数据库测试。cs_test_support 的监听器不挂 auth 中间件
%%% （生产由 auth_middleware_api_v1 写入 current_uid 会话键），测试扮演
%%% 中间件注入角色；本套件每类负例凭证的注入集都与「生产中间件对该凭证的
%%% 真实裁决」对齐（拒收 ⇒ 不注入 current_uid）。
%%%
%%% 覆盖（全部为业务零触达的负例）：
%%%   * **畸形 Authorization 矩阵**（queue GET）：无头 / 非 Bearer scheme /
%%%     有 scheme 无 token / 空 token。生产语义（auth_ds:condition/
%%%     do_authorization）：无头在 credential 面 option 语义直通（不注入），
%%%     拒绝落在 handler 侧 cs_auth（credential_missing → 401）；其余形态
%%%     verify_token → 706 → 中间件自身 401 截停。HTTP 链路逐形态断言
%%%     401 + facade 零调用。
%%%   * **坐席端点无 JWT 矩阵**：queue / seats/sessions / seats/me/heartbeat
%%%     三端点无头请求逐项 401 + 对应 facade 函数零调用。
%%%   * **合法 Bearer 形态但事实装配不含本人**：事实源整体缺失（cs_fake_facts:clear()
%%%     —— facts_not_configured 未登记进 cs_http 的 server_side 失败面，实测
%%%     兜底 fail-closed 500，用例注释记录）；事实在但只含他人 assignment
%%%     （坐席 assignment 同语句过滤零命中 → 403）。
%%%   * **中间件解析负例**（单元级）：直接驱动 auth_ds:verify_token/1 走
%%%     **真 imboy_jwt 链**（零 meck、零 DB——畸形形态在验签处必停，到不了
%%%     设备/会话查询），为上方注入集选择提供实证。
-module(cs_seat_credential_negative_tests).

-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

-define(S, cs_test_support).
-define(ORG, 7001001).
-define(UID, 424242).
-define(OTHER_UID, 424343).
-define(IDENTITY, 515151).

%% ===================================================================
%% 套件夹具
%% ===================================================================

credential_negative_test_() ->
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
        [
            fun malformed_authorization_tests/1,
            fun no_jwt_endpoint_matrix_tests/1,
            fun bearer_without_seat_facts_tests/1,
            fun middleware_parse_tests/1
        ]}.

%% 与 seat_inject 同构：合法 Bearer 形态 ⇒ 生产中间件过 JWT 门并注入会话键，
%% 测试扮演注入角色（凭证真伪由中间件裁决，不在本套件复验范围）。
seat_inject() ->
    #{auth_facts => cs_fake_facts, current_uid => ?UID}.

queue_path() ->
    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary, "/sessions/queue">>.

seat_list_path() ->
    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary,
        "/seats/sessions?status=active">>.

heartbeat_path() ->
    <<"/api/v1/cs/organizations/", (integer_to_binary(?ORG))/binary, "/seats/me/heartbeat">>.

%% ===================================================================
%% 畸形 Authorization 矩阵（queue GET；真中间件解析行为的 HTTP 链断言）
%% ===================================================================
%%
%% 生产行为对照（auth_middleware_api_v1 → auth_ds:condition/do_authorization）：
%% queue 是 credential 面（cs_http:is_credential_surface_path/1 命中）——
%%   ① 无 Authorization：option 语义直通（不注入 current_uid），拒绝落在
%%     handler 侧 cs_auth（credential_missing → 401）；
%%   ② 非 Bearer scheme（"Basic dXNlcjpwYXNz"）：parse_authorization_header
%%     不剥前缀，整串进 verify_token → 706（malformed）→ 中间件自身 401 截停；
%%   ③ "Bearer"（有 scheme 无 token）：无 "Bearer " 前缀（缺空格）→ 同 ② → 706；
%%   ④ "Bearer "（空 token）：前缀剥出空 token → 验签必败 → 706。
%% 测试监听器不挂 auth 中间件（cs_test_support 纪律），四形态统一以「中间件
%% 未注入 current_uid」的注入集请求，由真实 handler 链给出 401 兜底；②③④
%% 的中间件级截停由 middleware_parse_tests 直接实证。

malformed_authorization_tests(_) ->
    [
        {"Queue GET without any authorization header is 401 (handler gate; middleware passes through)",
            fun() ->
                assert_queue_unauthorized(#{})
            end},
        {"Queue GET with a non-Bearer scheme (Basic) is 401", fun() ->
            assert_queue_unauthorized(#{<<"authorization">> => <<"Basic dXNlcjpwYXNz">>})
        end},
        {"Queue GET with a scheme-only Bearer (no token) is 401", fun() ->
            assert_queue_unauthorized(#{<<"authorization">> => <<"Bearer">>})
        end},
        {"Queue GET with an empty Bearer token is 401", fun() ->
            assert_queue_unauthorized(#{<<"authorization">> => <<"Bearer ">>})
        end}
    ].

assert_queue_unauthorized(Headers) ->
    ?S:with_listener(
        tenant,
        session_queue,
        #{auth_facts => cs_fake_facts},
        fun(Port) ->
            meck:reset(customer_service_facade),
            Resp = ?S:request(Port, <<"GET">>, queue_path(), <<>>, Headers),
            ?assertEqual(401, ?S:status(Resp)),
            %% 业务零触达：被拒凭证形态到不了 seat enabled 门（fetch_seat），
            %% 更到不了任何 facade 用例。
            ?assertNot(meck:called(customer_service_facade, seat_session_queue, '_')),
            ?assertNot(meck:called(customer_service_facade, fetch_seat, '_'))
        end
    ).

%% ===================================================================
%% 坐席端点无 JWT 矩阵（逐端点 401 + 对应 facade 函数零调用）
%% ===================================================================
%%
%% 三个真实坐席 action（route metadata 可查）：queue（case_auth GET）、
%% seat_session_list（独立路径的 active/closed 视图）、seat_presence_heartbeat
%% （POST 心跳）。heartbeat 的生产面是 web-seat surface（免设备签名、JWT 门
%% 不放宽）：无 Authorization 在中间件 do_authorization(undefined) 处即 401
%% 截停；queue 无头为 credential 面直通、落在 handler 门。两种落点的对外
%% 合同一致：401 + 业务零触达（认证先于参数校验，缺 body/缺 status 参数
%% 不会先 422）。

no_jwt_endpoint_matrix_tests(_) ->
    [
        {"Seat queue GET without JWT is 401 and never reaches seat_session_queue", fun() ->
            assert_no_jwt_unauthorized(session_queue, <<"GET">>, queue_path(), seat_session_queue)
        end},
        {"Seat sessions list without JWT is 401 and never reaches seat_session_list", fun() ->
            assert_no_jwt_unauthorized(
                seat_session_list, <<"GET">>, seat_list_path(), seat_session_list
            )
        end},
        {"Seat presence heartbeat without JWT is 401 and never reaches seat_heartbeat", fun() ->
            assert_no_jwt_unauthorized(
                seat_presence_heartbeat, <<"POST">>, heartbeat_path(), seat_heartbeat
            )
        end}
    ].

assert_no_jwt_unauthorized(Action, Method, Path, FacadeFn) ->
    ?S:with_listener(
        tenant,
        Action,
        #{auth_facts => cs_fake_facts},
        fun(Port) ->
            meck:reset(customer_service_facade),
            Resp = ?S:request(Port, Method, Path, #{}, #{}),
            ?assertEqual(401, ?S:status(Resp)),
            ?assertNot(meck:called(customer_service_facade, FacadeFn, '_'))
        end
    ).

%% ===================================================================
%% 合法 Bearer 形态但事实装配不含本人（拒绝语义按实测归类）
%% ===================================================================

bearer_without_seat_facts_tests(_) ->
    [
        {"A well-formed Bearer with no facts at all is rejected fail-closed (500; measured)",
            fun() ->
                %% 实测记录：cs_fake_facts:clear() 后 load_request_facts 返回
                %% {error, facts_not_configured}；该原因未登记进 cs_http 的
                %% server_side 失败面 → classify 兜底 unknown → fail-closed 500
                %% （服务端装配不可用语义，绝不伪装成 4xx），不是 401/403 的
                %% 凭证裁决——凭证裁决需要事实在场，见下一例。中间件侧：合法
                %% Bearer 形态过 JWT 门注入会话键（测试以 seat_inject() 扮演），
                %% 拒绝发生在 handler 的 cs_auth 事实装配步。
                cs_fake_facts:clear(),
                ?S:with_listener(tenant, session_queue, seat_inject(), fun(Port) ->
                    meck:reset(customer_service_facade),
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        queue_path(),
                        <<>>,
                        #{<<"authorization">> => <<"Bearer x">>}
                    ),
                    ?assertEqual(500, ?S:status(Resp)),
                    ?assertNot(meck:called(customer_service_facade, seat_session_queue, '_')),
                    ?assertNot(meck:called(customer_service_facade, fetch_seat, '_'))
                end)
            end},
        {"A well-formed Bearer whose facts carry only another user's seat is 403", fun() ->
            %% 事实在但不含本人：load 成功、member active，但坐席 assignment
            %% 同语句过滤（user_id =:= 凭证 user 且本 Org 且 active 且
            %% customer_service）零命中 → identity_assignment_missing → 403；
            %% seat enabled 门（fetch_seat）未达，业务 facade 零触达。
            cs_fake_facts:set(#{
                organization_id => ?ORG,
                member => #{user_id => ?OTHER_UID, status => active, governance_roles => []},
                assignments => [
                    #{
                        business_identity_id => ?IDENTITY,
                        user_id => ?OTHER_UID,
                        organization_id => ?ORG,
                        function_key => <<"customer_service">>,
                        status => active,
                        version => 1
                    }
                ],
                permissions => [<<"conversation.read">>]
            }),
            ?S:with_listener(tenant, session_queue, seat_inject(), fun(Port) ->
                meck:reset(customer_service_facade),
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    queue_path(),
                    <<>>,
                    #{<<"authorization">> => <<"Bearer x">>}
                ),
                ?assertEqual(403, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, seat_session_queue, '_')),
                ?assertNot(meck:called(customer_service_facade, fetch_seat, '_'))
            end)
        end}
    ].

%% ===================================================================
%% 中间件解析负例（单元级实证：真 imboy_jwt 链、零 meck、零 DB）
%% ===================================================================
%%
%% 直接驱动 auth_ds:verify_token/1：畸形形态在验签处必停（706 =
%% ?ERR_TOKEN_MALFORMED），证明生产中间件对 ②③④ 三形态必在 do_authorization
%% 截停并返回 401、绝不注入 current_uid——上方 HTTP 矩阵注入集选择的依据。

middleware_parse_tests(_) ->
    [
        {"verify_token rejects a non-Bearer scheme with 706 (malformed)", fun() ->
            ?assertMatch(
                {error, ?ERR_TOKEN_MALFORMED, _}, auth_ds:verify_token(<<"Basic dXNlcjpwYXNz">>)
            )
        end},
        {"verify_token rejects a scheme-only Bearer with 706 (malformed)", fun() ->
            ?assertMatch({error, ?ERR_TOKEN_MALFORMED, _}, auth_ds:verify_token(<<"Bearer">>))
        end},
        {"verify_token rejects an empty Bearer token with 706 (malformed)", fun() ->
            ?assertMatch({error, ?ERR_TOKEN_MALFORMED, _}, auth_ds:verify_token(<<"Bearer ">>))
        end}
    ].
