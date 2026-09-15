%%% @doc CS-02-A01/A04 的认证套件（纯套件：cs_auth:authorize/3 直接驱动 + meck facade，
%%% 零 DB、零 HTTP socket）。
%%%
%%% 覆盖：
%%%   * **A01 五类身份无混淆**：五类 principal 各自的凭证类别互斥——route metadata
%%%     声明某类 principal 时只取**该类**凭证（JWT / Admin session / visit token 头 /
%%%     shop key 头），其余类别即使同时在场也不采信；类别错配 → `{principal_mismatch,
%%%     _, _}`；metadata 缺失/未知 → fail-closed。
%%%   * **A04 suspended seat actor 即时拒绝**：坐席门在**任何业务用例之前**查 seat 行，
%%%     `enabled=false` → `seat_disabled`、seat 缺失 → `{seat_not_found, _}`；
%%%     同时验证该拒绝发生在 facade 业务函数（claim 等）**零调用**之后。
%%%   * 成员/治理/平台/访客/门店五类的正例与逐类拒绝面。
%%%
%%% 客户端申报的 organization_id 由事实/凭证逐字比对（申报 ≠ 信任）。
-module(cs_auth_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORG, 7001001).
-define(OTHER_ORG, 7002002).
-define(UID, 424242).
-define(ADM, 77).
-define(IDENTITY, 515151).
-define(WS, 90001).

%% ===================================================================
%% 套件夹具
%% ===================================================================

auth_test_() ->
    {foreach,
        fun() ->
            meck:new(customer_service_facade, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            cs_fake_facts:clear(),
            ok
        end,
        [
            fun seat_tests/1,
            fun governance_tests/1,
            fun platform_tests/1,
            fun visitor_tests/1,
            fun confusion_tests/1,
            fun failclosed_tests/1
        ]}.

%% ===================================================================
%% 坐席（cs_seat）：assignment + seat enabled 门（A04）
%% ===================================================================

seat_tests(_) ->
    Metadata = #{
        auth_context => cs_seat,
        required_function => <<"customer_service">>,
        required_permission => <<"conversation.write">>
    },
    [
        %% 正例：active member + customer_service assignment + enabled seat。
        {"seat happy path returns business identity context", fun() ->
            given_member_with_assignment(),
            meck:expect(customer_service_facade, fetch_seat, fun(_Org, Params) ->
                ?assertEqual(?IDENTITY, maps:get(business_identity_id, Params)),
                {ok, #{business_identity_id => ?IDENTITY, enabled => true}}
            end),
            {ok, Ctx} = cs_auth:authorize(
                Metadata, req([]), state(?ORG, jwt_state())
            ),
            ?assertEqual(cs_seat, maps:get(auth_context, Ctx)),
            ?assertEqual(?IDENTITY, maps:get(business_identity_id, Ctx)),
            ?assertEqual(?ORG, maps:get(organization_id, Ctx)),
            ?assert(meck:called(customer_service_facade, fetch_seat, '_'))
        end},

        %% A04：suspended seat actor 即时拒绝（enabled=false）。
        {"A04 suspended seat actor is rejected with seat_disabled", fun() ->
            given_member_with_assignment(),
            meck:expect(customer_service_facade, fetch_seat, fun(_Org, _Params) ->
                {ok, #{business_identity_id => ?IDENTITY, enabled => false}}
            end),
            ?assertEqual(
                {error, seat_disabled},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},

        %% A04：seat 行不存在同样拒绝。
        {"A04 missing seat row is rejected with seat_not_found", fun() ->
            given_member_with_assignment(),
            meck:expect(customer_service_facade, fetch_seat, fun(_Org, _Params) ->
                {error, not_found}
            end),
            ?assertMatch(
                {error, {seat_not_found, _}},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},

        %% A04 的「即时」语义：拒绝发生在业务用例之前（claim 零调用）。
        {"A04 rejection happens before any business use case runs", fun() ->
            given_member_with_assignment(),
            meck:expect(customer_service_facade, fetch_seat, fun(_Org, _Params) ->
                {ok, #{enabled => false}}
            end),
            meck:expect(customer_service_facade, claim, fun(_Org, _Params) ->
                {ok, #{should_never_happen => true}}
            end),
            {error, seat_disabled} =
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state())),
            ?assertNot(meck:called(customer_service_facade, claim, '_'))
        end},

        %% 职能不符：sales assignment 不满足 customer_service 路由。
        {"sales-only assignment cannot act as seat", fun() ->
            cs_fake_facts:set(facts_with_assignment(<<"sales">>)),
            ?assertEqual(
                {error, identity_assignment_missing},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},

        %% 跨 Org 的 assignment 不算数（申报 Org ≠ assignment Org）。
        {"cross-org assignment is not visible in claimed org", fun() ->
            cs_fake_facts:set(
                #{
                    organization_id => ?ORG,
                    member => #{
                        user_id => ?UID,
                        status => active,
                        governance_roles => []
                    },
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
                    permissions => [<<"conversation.write">>]
                }
            ),
            ?assertEqual(
                {error, identity_assignment_missing},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},

        %% suspended member：事实照回、判定必拒（即时失效，不等 token 过期）。
        {"suspended member is rejected immediately", fun() ->
            cs_fake_facts:set(facts_with_status(suspended)),
            ?assertEqual(
                {error, {member_not_active, suspended}},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},

        %% 多条同职能 active assignment（脏数据）→ fail-closed。
        {"multiple active customer_service assignments are rejected", fun() ->
            cs_fake_facts:set(#{
                organization_id => ?ORG,
                member => #{user_id => ?UID, status => active, governance_roles => []},
                assignments => [
                    assignment(1), assignment(2)
                ],
                permissions => [<<"conversation.write">>]
            }),
            meck:expect(customer_service_facade, fetch_seat, fun(_O, _P) ->
                {ok, #{enabled => true}}
            end),
            ?assertEqual(
                {error, {multiple_active_assignment, <<"customer_service">>}},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},

        %% 坐席权限门：assignment 对但缺 required_permission。
        {"seat without required permission is rejected", fun() ->
            cs_fake_facts:set(facts_with_assignment(<<"customer_service">>, [])),
            ?assertEqual(
                {error, {permission_missing, <<"conversation.write">>}},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},

        %% 无 customer_service assignment（不带权限门，专测 assignment 解析）。
        {"member without any assignment is rejected", fun() ->
            cs_fake_facts:set(facts_with_assignment_none()),
            MetadataNoPerm = #{
                auth_context => cs_seat,
                required_function => <<"customer_service">>
            },
            ?assertEqual(
                {error, identity_assignment_missing},
                cs_auth:authorize(MetadataNoPerm, req([]), state(?ORG, jwt_state()))
            )
        end}
    ].

%% ===================================================================
%% 治理（enterprise_owner_admin）
%% ===================================================================

governance_tests(_) ->
    Metadata = #{
        auth_context => enterprise_owner_admin,
        required_governance => [<<"owner">>, <<"admin">>]
    },
    [
        {"owner passes governance gate", fun() ->
            cs_fake_facts:set(facts_with_role(owner)),
            {ok, Ctx} = cs_auth:authorize(
                Metadata, req([]), state(?ORG, jwt_state())
            ),
            ?assertEqual(enterprise_owner_admin, maps:get(auth_context, Ctx)),
            ?assertEqual([<<"owner">>], maps:get(governance_roles, Ctx))
        end},
        {"plain member fails governance gate", fun() ->
            cs_fake_facts:set(facts_with_role(member)),
            ?assertEqual(
                {error, {governance_insufficient, [<<"owner">>, <<"admin">>]}},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end},
        {"suspended owner fails governance gate (no role laundering)", fun() ->
            cs_fake_facts:set(facts_with_status(suspended)),
            ?assertEqual(
                {error, {member_not_active, suspended}},
                cs_auth:authorize(Metadata, req([]), state(?ORG, jwt_state()))
            )
        end}
    ].

%% ===================================================================
%% 平台（platform_admin）
%% ===================================================================

platform_tests(_) ->
    Metadata = #{
        auth_context => platform_admin,
        required_permission => <<"customer_service:write">>
    },
    [
        {"platform admin with permission passes", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:write">>]
            }),
            {ok, Ctx} = cs_auth:authorize(
                Metadata, req([]), state(?ORG, adm_state())
            ),
            ?assertEqual(platform_admin, maps:get(auth_context, Ctx))
        end},
        {"admin identity mismatch between credential and facts is rejected", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => 999, permissions => [<<"customer_service:write">>]
            }),
            ?assertEqual(
                {error, platform_identity_mismatch},
                cs_auth:authorize(Metadata, req([]), state(?ORG, adm_state()))
            )
        end},
        {"read permission cannot satisfy write route", fun() ->
            cs_fake_facts:set(#{
                adm_user_id => ?ADM, permissions => [<<"customer_service:read">>]
            }),
            ?assertEqual(
                {error, {permission_missing, <<"customer_service:write">>}},
                cs_auth:authorize(Metadata, req([]), state(?ORG, adm_state()))
            )
        end}
    ].

%% ===================================================================
%% 访客 / 门店（cs_visit / cs_shop_key）
%% ===================================================================

visitor_tests(_) ->
    [
        {"visit token maps to contact-scoped context", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(<<"tok">>, maps:get(secret, Params)),
                ?assert(is_integer(maps:get(at, Params))),
                {ok, #{organization_id => ?ORG, contact_id => 3131, scope => visit}}
            end),
            {ok, Ctx} = cs_auth:authorize(
                #{auth_context => cs_visit},
                req([{<<"x-cs-visit-token">>, <<"tok">>}]),
                state(?ORG, #{organization_id => ?ORG})
            ),
            ?assertEqual(cs_visit, maps:get(auth_context, Ctx)),
            ?assertEqual(3131, maps:get(contact_id, Ctx))
        end},
        {"visit token of another org cannot be used against claimed org", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_Org, _P) ->
                {error, not_found}
            end),
            %% digest 未命中在认证层翻译为 credential_invalid（401 语义），
            %% not_found 留给业务资源（404，无枚举）。
            ?assertEqual(
                {error, credential_invalid},
                cs_auth:authorize(
                    #{auth_context => cs_visit},
                    req([{<<"x-cs-visit-token">>, <<"tok">>}]),
                    state(?ORG, #{organization_id => ?ORG})
                )
            )
        end},
        {"revoked visit token is rejected", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_Org, _P) ->
                {error, token_revoked}
            end),
            ?assertEqual(
                {error, token_revoked},
                cs_auth:authorize(
                    #{auth_context => cs_visit},
                    req([{<<"x-cs-visit-token">>, <<"tok">>}]),
                    state(?ORG, #{organization_id => ?ORG})
                )
            )
        end},
        {"shop key maps to org context", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(Org, _Params) ->
                ?assertEqual(?ORG, Org),
                {ok, #{organization_id => Org, status => active}}
            end),
            {ok, Ctx} = cs_auth:authorize(
                #{auth_context => cs_shop_key},
                req([{<<"x-cs-shop-key">>, <<"sk">>}]),
                state(?ORG, #{organization_id => ?ORG})
            ),
            ?assertEqual(cs_shop_key, maps:get(auth_context, Ctx))
        end},
        {"revoked shop key is rejected", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_Org, _P) ->
                {error, revoked}
            end),
            ?assertEqual(
                {error, revoked},
                cs_auth:authorize(
                    #{auth_context => cs_shop_key},
                    req([{<<"x-cs-shop-key">>, <<"sk">>}]),
                    state(?ORG, #{organization_id => ?ORG})
                )
            )
        end}
    ].

%% ===================================================================
%% A01：五类身份无混淆（凭证类别互斥）
%% ===================================================================

confusion_tests(_) ->
    [
        {"seat route does not accept visit/shop-key tokens as identity", fun() ->
            %% 有 visit/shop key 头但无 JWT → credential_missing（不是降级放行）。
            ?assertEqual(
                {error, credential_missing},
                cs_auth:authorize(
                    #{
                        auth_context => cs_seat,
                        required_function => <<"customer_service">>
                    },
                    req([
                        {<<"x-cs-visit-token">>, <<"t">>},
                        {<<"x-cs-shop-key">>, <<"s">>}
                    ]),
                    state(?ORG, #{current_uid => 0})
                )
            )
        end},
        {"visit route ignores a JWT and requires the token header", fun() ->
            meck:expect(customer_service_facade, verify_visit_token, fun(_O, _P) ->
                erlang:error(should_not_be_called)
            end),
            %% 有 JWT（state current_uid）但无 token 头 → credential_missing，
            %% 且 verify_visit_token 不被调用（JWT 不被混用为访客凭证）。
            ?assertEqual(
                {error, credential_missing},
                cs_auth:authorize(
                    #{auth_context => cs_visit},
                    req([]),
                    state(?ORG, jwt_state())
                )
            ),
            ?assertNot(
                meck:called(customer_service_facade, verify_visit_token, '_')
            )
        end},
        {"shop-key route ignores JWT and visit headers", fun() ->
            meck:expect(customer_service_facade, verify_shop_key, fun(_O, _P) ->
                erlang:error(should_not_be_called)
            end),
            ?assertEqual(
                {error, credential_missing},
                cs_auth:authorize(
                    #{auth_context => cs_shop_key},
                    req([{<<"x-cs-visit-token">>, <<"t">>}]),
                    state(?ORG, jwt_state())
                )
            ),
            ?assertNot(meck:called(customer_service_facade, verify_shop_key, '_'))
        end},
        {"platform route does not accept member JWT", fun() ->
            cs_fake_facts:set(#{adm_user_id => ?ADM, permissions => []}),
            ?assertEqual(
                {error, credential_missing},
                cs_auth:authorize(
                    #{
                        auth_context => platform_admin,
                        required_permission => <<"customer_service:read">>
                    },
                    req([]),
                    state(?ORG, jwt_state())
                )
            )
        end},
        {"five principals resolve to four credential classes (JWT shared by member-class)", fun() ->
            Classes = [{P, cs_auth:credential_class(P)} || P <- cs_auth:principals()],
            ?assertEqual(5, length(Classes)),
            %% enterprise_owner_admin 与 cs_seat 同为 imboy_jwt（成员类凭证，
            %% 由事实装配区分职能），其余三类各持专属凭证类别。
            ?assertEqual(4, length(lists:usort([C || {_P, C} <- Classes]))),
            ?assertEqual(imboy_jwt, cs_auth:credential_class(enterprise_owner_admin)),
            ?assertEqual(imboy_jwt, cs_auth:credential_class(cs_seat))
        end}
    ].

%% ===================================================================
%% fail-closed：metadata 缺失 / 未知 / 装配缺失 / 事实源失败
%% ===================================================================

failclosed_tests(_) ->
    [
        {"missing auth_context fails closed", fun() ->
            ?assertEqual(
                {error, route_metadata_missing_auth_context},
                cs_auth:authorize(#{}, req([]), state(?ORG, jwt_state()))
            )
        end},
        {"unknown auth_context fails closed", fun() ->
            ?assertEqual(
                {error, {unknown_auth_context, super_admin}},
                cs_auth:authorize(
                    #{auth_context => super_admin},
                    req([]),
                    state(?ORG, jwt_state())
                )
            )
        end},
        {"missing facts assembly fails closed", fun() ->
            Metadata = #{auth_context => enterprise_owner_admin},
            State = maps:remove(
                auth_facts, state(?ORG, jwt_state())
            ),
            ?assertEqual(
                {error, auth_assembly_missing},
                cs_auth:authorize(Metadata, req([]), State)
            )
        end},
        {"facts source failure fails closed", fun() ->
            cs_fake_facts:fail_with({member_fact_query_failed, down}),
            Metadata = #{auth_context => enterprise_owner_admin},
            ?assertEqual(
                {error, {member_fact_query_failed, down}},
                cs_auth:authorize(
                    Metadata, req([]), state(?ORG, jwt_state())
                )
            )
        end},
        {"missing org fails closed for member classes", fun() ->
            cs_fake_facts:set(facts_with_role(owner)),
            Metadata = #{auth_context => enterprise_owner_admin},
            State = maps:remove(organization_id, state(?ORG, jwt_state())),
            ?assertEqual(
                {error, missing_org_id},
                cs_auth:authorize(Metadata, req([]), State)
            )
        end}
    ].

%% ===================================================================
%% 夹具
%% ===================================================================

given_member_with_assignment() ->
    cs_fake_facts:set(facts_with_assignment(<<"customer_service">>)).

facts_with_role(Role) ->
    #{
        organization_id => ?ORG,
        member => #{
            user_id => ?UID,
            status => active,
            role => Role,
            governance_roles => governance_of(Role)
        },
        assignments => [],
        permissions => []
    }.

governance_of(owner) -> [<<"owner">>];
governance_of(admin) -> [<<"admin">>];
governance_of(_) -> [].

facts_with_status(Status) ->
    #{
        organization_id => ?ORG,
        member => #{user_id => ?UID, status => Status, governance_roles => [<<"owner">>]},
        assignments => [assignment(?IDENTITY)],
        permissions => []
    }.

facts_with_assignment(FunctionKey) ->
    facts_with_assignment(FunctionKey, [<<"conversation.write">>]).

facts_with_assignment(FunctionKey, Permissions) ->
    #{
        organization_id => ?ORG,
        member => #{user_id => ?UID, status => active, governance_roles => []},
        assignments => [assignment(?IDENTITY, FunctionKey)],
        permissions => Permissions
    }.

facts_with_assignment_none() ->
    #{
        organization_id => ?ORG,
        member => #{user_id => ?UID, status => active, governance_roles => []},
        assignments => [],
        permissions => []
    }.

assignment(BusinessIdentityId) ->
    assignment(BusinessIdentityId, <<"customer_service">>).

assignment(BusinessIdentityId, FunctionKey) ->
    #{
        business_identity_id => BusinessIdentityId,
        user_id => ?UID,
        organization_id => ?ORG,
        function_key => FunctionKey,
        status => active,
        version => 1
    }.

%% 伪 cowboy 请求：cs_auth 只从 map 读 headers。
req(Headers) ->
    #{headers => maps:from_list(Headers)}.

%% JWT 类主体的会话键（current_uid 由中间件注入 handler State）。
jwt_state() ->
    #{current_uid => ?UID}.

%% 平台主体的会话键（adm_user_id 由 adm_auth_middleware 注入）。
adm_state() ->
    #{adm_user_id => ?ADM}.

state(OrgId, Base) ->
    Base#{
        organization_id => OrgId,
        auth_facts => cs_fake_facts,
        now => 1700000000000
    }.
