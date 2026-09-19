%%% @doc EB-04 企业授权与职能身份（企业 tenant 面 + 平台 Admin 面的授权判定）测试。
%%%
%%% 依据：plan v4.1 EB-04、EB-D02/EB-D03/EB-D09/EB-D10/EB-D11、§2.1 #4、§5.4 必测负例。
%%%
%%% 本套件是**纯逻辑**套件：零 DB、零 app 启动、零外部依赖。
%%% 只对「注入事实 + 注入时钟」的决策函数做判定，因此可用
%%% `make eunit t=eb_auth_tests` 直接跑（不需要 config/sys.local）。
%%%
%%% Acceptance 覆盖：
%%%   EB-04-A01 五类 principal 与 function/permission/governance 三概念无混淆
%%%             （负例必须**可区分**：identity 对但 permission 不足 / function 对但
%%%              governance 不够 / credential 类别不符，三者错误原子互不相同）；
%%%   EB-04-A02 suspended / removed 对**旧 JWT 立即失效**（逐请求重新加载事实，
%%%             JWT 自报的成员状态与治理角色一律不采信，不依赖 token 过期）；
%%%   EB-04-A03 跨 Org / 跨 identity fail-closed 且**零副作用**
%%%             （授权路径的扩展点契约里没有任何写 callback；拒绝路径无写操作、
%%%              重复调用结果逐字相同、注入事实对象未被改动）；
%%%   EB-04-A04 现有个人 JWT / open / admin 路由分流回归不变（含静态标记与
%%%              真实分发行为两类证据）。
-module(eb_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(PRINCIPAL_REL, "src/features/enterprise_business/application/auth/eb_auth_principal.erl").
-define(PERMISSION_REL, "src/features/enterprise_business/application/auth/eb_auth_permission.erl").
-define(PORT_REL, "src/features/enterprise_business/application/auth/eb_auth_port.erl").
-define(APP_REL, "src/features/enterprise_business/application/auth/eb_auth_app.erl").
-define(AUTH_MIDDLEWARE_REL, "src/api/auth_middleware.erl").
-define(AUTH_MIDDLEWARE_V1_REL, "src/api/auth_middleware_api_v1.erl").

-define(ORG_A, 4200001).
-define(ORG_B, 4200002).
-define(USER_A, 910001).
-define(USER_B, 910002).
-define(WS_A, 730001).
-define(IDENTITY_SALES, 510001).
-define(IDENTITY_CS, 510002).
-define(CONTACT_A, 620001).
-define(NOW, 1770000000).
-define(VISIT_DIGEST, <<"3f0d1c9a5e77b2a4c1d8f6e5a4b3c2d1e0f9a8b7c6d5e4f3a2b1c0d9e8f7a6b5">>).
-define(SHOP_DIGEST, <<"aa11bb22cc33dd44ee55ff660077889900aabbccddeeff0011223344556677">>).

%% ===================================================================
%% 夹具：合成事实（随机 TSID，无真实账号 / 联系方式 / 凭据）
%% ===================================================================

perms_all() ->
    eb_auth_permission:known_permissions().

member_facts(Overrides) ->
    Base = #{
        organization_id => ?ORG_A,
        member => #{status => active, governance_roles => []},
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => active
            }
        ],
        permissions => perms_all()
    },
    maps:merge(Base, Overrides).

jwt_credential(Overrides) ->
    maps:merge(
        #{
            class => imboy_jwt,
            user_id => ?USER_A,
            claims => #{}
        },
        Overrides
    ).

%% 逐请求加载器：计数 + 固定/序列结果。counters 而非 mock（零 meck）。
counting_loader(Ref, Result) ->
    fun() ->
        _ = counters:add(Ref, 1, 1),
        Result
    end.

sequence_loader(Ref, Results) ->
    fun() ->
        N = counters:get(Ref, 1) + 1,
        _ = counters:add(Ref, 1, 1),
        case length(Results) >= N of
            true -> lists:nth(N, Results);
            false -> lists:last(Results)
        end
    end.

loader_calls(Ref) ->
    counters:get(Ref, 1).

%% ===================================================================
%% EB-04-A01：五类 principal + 三概念无混淆
%% ===================================================================

principals_are_exactly_five_test() ->
    ?assertEqual(
        [
            enterprise_owner_admin,
            enterprise_member,
            platform_admin,
            cs_visit,
            cs_shop_key
        ],
        eb_auth_principal:principals()
    ).

principal_credential_classes_test() ->
    ?assertEqual(imboy_jwt, eb_auth_principal:credential_class(enterprise_owner_admin)),
    ?assertEqual(imboy_jwt, eb_auth_principal:credential_class(enterprise_member)),
    ?assertEqual(adm_session, eb_auth_principal:credential_class(platform_admin)),
    ?assertEqual(visit_token, eb_auth_principal:credential_class(cs_visit)),
    ?assertEqual(shop_key, eb_auth_principal:credential_class(cs_shop_key)).

%% 三概念分离的**值域**证据：function_key 值域复用 domain（eb_identity），
%% governance role 值域与既有 moya_acl:resolve_org_manager/2 相同的 owner/admin，
%% permission 是独立字符串集合，三集合两两不相等。
three_concepts_are_distinct_value_domains_test() ->
    ?assertEqual(eb_identity:function_keys(), eb_auth_permission:function_keys()),
    ?assertEqual([<<"owner">>, <<"admin">>], eb_auth_permission:governance_roles()),
    Permissions = eb_auth_permission:known_permissions(),
    ?assert(length(Permissions) > 0),
    ?assertEqual([], [P || P <- Permissions, lists:member(P, eb_auth_permission:function_keys())]),
    ?assertEqual([], [P || P <- Permissions, lists:member(P, eb_auth_permission:governance_roles())]).

%% 负例 1：identity 正确（active sales assignment）但**独立 permission 不足**。
member_route_denies_when_permission_missing_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/conversations">>,
        required_function => <<"sales">>,
        required_permission => <<"conversation.write">>
    },
    Facts = member_facts(#{permissions => [<<"contact.read">>]}),
    Result = eb_auth_app:authorize(Route, #{
        credential => jwt_credential(#{}),
        organization_id => ?ORG_A,
        facts => counting_loader(Ref, {ok, Facts})
    }),
    ?assertEqual({error, {permission_missing, <<"conversation.write">>}}, Result),
    %% identity 是正确的（不是 function 不匹配、也不是 assignment 缺失）
    ?assertNotEqual({error, identity_assignment_missing}, Result).

%% 负例 2：function_key 正确（customer_service identity）但 **governance role 不够**。
platform_owner_route_denies_function_holder_without_governance_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => enterprise_owner_admin,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/members/2/suspend">>
    },
    Facts = member_facts(#{
        member => #{status => active, governance_roles => []},
        assignments => [
            #{
                business_identity_id => ?IDENTITY_CS,
                organization_id => ?ORG_A,
                function_key => <<"customer_service">>,
                user_id => ?USER_A,
                status => active
            }
        ]
    }),
    Result = eb_auth_app:authorize(Route, #{
        credential => jwt_credential(#{}),
        organization_id => ?ORG_A,
        facts => counting_loader(Ref, {ok, Facts})
    }),
    ?assertEqual({error, {governance_insufficient, [<<"owner">>, <<"admin">>]}}, Result),
    %% 与「permission 不足」必须可区分
    ?assertNotMatch({error, {permission_missing, _}}, Result).

%% 负例 2b：持有 owner 治理角色但**没有**任何业务 identity 也能读治理面；
%% 治理角色不因 function 自动获得，反之 function 也不因治理角色自动获得。
owner_governance_does_not_grant_member_route_test() ->
    Ref = counters:new(1, []),
    MemberRoute = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    Facts = member_facts(#{
        member => #{status => active, governance_roles => [<<"owner">>]},
        assignments => []
    }),
    ?assertEqual(
        {error, identity_assignment_missing},
        eb_auth_app:authorize(MemberRoute, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => counting_loader(Ref, {ok, Facts})
        })
    ).

%% 负例 3：identity 存在但 function_key 不是路由要求的那一类。
member_route_denies_function_mismatch_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"customer_service">>,
        required_permission => <<"contact.read">>
    },
    Facts = member_facts(#{}),
    Result = eb_auth_app:authorize(Route, #{
        credential => jwt_credential(#{}),
        organization_id => ?ORG_A,
        facts => counting_loader(Ref, {ok, Facts})
    }),
    ?assertEqual({error, {function_mismatch, <<"customer_service">>, [<<"sales">>]}}, Result).

%% §EB-D02 基数冻结：同一 user 在同一 Org 可同时持有一条 `sales` 与一条
%% `customer_service` active assignment；授权按路由要求的职能挑那一条 identity，
%% 而不是「取第一条 active」。同职能出现两条 active（脏数据）→ fail-closed。
member_with_two_active_identities_selects_required_function_test() ->
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/conversations">>,
        required_function => <<"customer_service">>,
        required_permission => <<"conversation.read">>
    },
    Facts = member_facts(#{
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => active
            },
            #{
                business_identity_id => ?IDENTITY_CS,
                organization_id => ?ORG_A,
                function_key => <<"customer_service">>,
                user_id => ?USER_A,
                status => active
            }
        ]
    }),
    ?assertMatch(
        {ok, #{
            function_key := <<"customer_service">>,
            business_identity_id := ?IDENTITY_CS
        }},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Facts} end}
        })
    ),
    Duplicated = member_facts(#{
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => active
            },
            #{
                business_identity_id => ?IDENTITY_SALES + 1,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => active
            }
        ]
    }),
    ?assertEqual(
        {error, {multiple_active_assignment, <<"sales">>}},
        eb_auth_app:authorize(Route#{required_function => <<"sales">>}, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Duplicated} end}
        })
    ).

%% function_key 字符串不得被当作 permission 使用（新增 function 名不能替代权限）。
function_key_cannot_substitute_permission_test() ->
    ?assertEqual(
        {error, {function_cannot_substitute_permission, <<"sales">>}},
        eb_auth_permission:permission_satisfied(<<"sales">>, [<<"sales">>])
    ),
    ?assertEqual(
        {error, {function_cannot_substitute_permission, <<"customer_service">>}},
        eb_auth_permission:permission_satisfied(
            <<"customer_service">>, [<<"customer_service">>]
        )
    ),
    %% 正常 permission 不受影响
    ?assertEqual(
        ok, eb_auth_permission:permission_satisfied(<<"contact.read">>, [<<"contact.read">>])
    ),
    ?assertEqual(ok, eb_auth_permission:permission_satisfied(undefined, [])).

%% 负例 4：platform_admin 需要 Admin session；普通 IMBoy JWT 在平台面被拒，
%% 且拒绝原因与「permission 不足」可区分（不加载任何事实 = 零副作用）。
platform_admin_denies_tenant_jwt_before_loading_facts_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => platform_admin,
        surface => platform,
        path => <<"/api/adm/enterprise-business/organizations">>,
        required_permission => <<"enterprise_business:read">>
    },
    Result = eb_auth_app:authorize(Route, #{
        credential => jwt_credential(#{}),
        organization_id => ?ORG_A,
        facts => counting_loader(Ref, {ok, #{adm_user_id => ?USER_A, permissions => perms_all()}})
    }),
    ?assertEqual({error, {principal_mismatch, platform_admin, imboy_jwt}}, Result),
    ?assertEqual(0, loader_calls(Ref)).

%% 负例 4b：Admin session 不得冒充 Organization member（反向 principal_mismatch）。
platform_admin_credential_cannot_enter_tenant_route_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    Result = eb_auth_app:authorize(Route, #{
        credential => #{class => adm_session, adm_user_id => ?USER_A},
        organization_id => ?ORG_A,
        facts => counting_loader(Ref, {ok, member_facts(#{})})
    }),
    ?assertEqual({error, {principal_mismatch, enterprise_member, adm_session}}, Result),
    ?assertEqual(0, loader_calls(Ref)).

%% 负例 4c：平台 Admin 有 read 无 write 时执行写操作必须失败（§5.4）。
platform_admin_read_without_write_test() ->
    Ref = counters:new(1, []),
    WriteRoute = #{
        auth_context => platform_admin,
        surface => platform,
        path => <<"/api/adm/enterprise-business/organizations/1/correct">>,
        required_permission => <<"enterprise_business:write">>
    },
    ReadRoute = WriteRoute#{required_permission => <<"enterprise_business:read">>},
    Request = #{
        credential => #{class => adm_session, adm_user_id => ?USER_A},
        organization_id => ?ORG_A,
        facts => counting_loader(
            Ref,
            {ok, #{
                adm_user_id => ?USER_A,
                permissions => [<<"enterprise_business:read">>]
            }}
        )
    },
    ?assertEqual(
        {error, {permission_missing, <<"enterprise_business:write">>}},
        eb_auth_app:authorize(WriteRoute, Request)
    ),
    ?assertMatch(
        {ok, #{auth_context := platform_admin}}, eb_auth_app:authorize(ReadRoute, Request)
    ).

%% 负例 5：cs_visit token 的四类可区分拒绝（过期 / 吊销 / digest 不符 / 跨 Org / 跨 contact）。
cs_visit_token_negative_matrix_test() ->
    Route = #{
        auth_context => cs_visit,
        surface => tenant,
        path => <<"/api/v1/cs/visit/session">>,
        require_contact => true
    },
    Base = #{
        organization_id => ?ORG_A,
        contact_id => ?CONTACT_A,
        digest => ?VISIT_DIGEST,
        expires_at => ?NOW + 60,
        revoked => false
    },
    Request = fun(Facts) ->
        #{
            credential => #{class => visit_token, digest => ?VISIT_DIGEST},
            organization_id => ?ORG_A,
            contact_id => ?CONTACT_A,
            now => ?NOW,
            facts => {load, fun() -> {ok, Facts} end}
        }
    end,
    ?assertMatch(
        {ok, #{auth_context := cs_visit, contact_id := ?CONTACT_A}},
        eb_auth_app:authorize(Route, Request(Base))
    ),
    ?assertEqual(
        {error, visit_token_expired},
        eb_auth_app:authorize(Route, Request(Base#{expires_at => ?NOW}))
    ),
    ?assertEqual(
        {error, visit_token_revoked},
        eb_auth_app:authorize(Route, Request(Base#{revoked => true}))
    ),
    ?assertEqual(
        {error, visit_token_digest_mismatch},
        eb_auth_app:authorize(Route, Request(Base#{digest => <<"00ff">>}))
    ),
    ?assertEqual(
        {error, visit_token_digest_mismatch},
        eb_auth_app:authorize(Route, Request(Base#{digest => undefined}))
    ),
    ?assertEqual(
        {error, cross_org},
        eb_auth_app:authorize(Route, Request(Base#{organization_id => ?ORG_B}))
    ),
    ?assertEqual(
        {error, cross_contact},
        eb_auth_app:authorize(Route, Request(Base#{contact_id => ?CONTACT_A + 1}))
    ).

%% 负例 6：shop key 的吊销 / digest / 跨 Org（不泄露 key 与 digest）。
cs_shop_key_negative_matrix_test() ->
    Route = #{
        auth_context => cs_shop_key,
        surface => tenant,
        path => <<"/api/v1/cs/shop/session">>
    },
    Base = #{organization_id => ?ORG_A, digest => ?SHOP_DIGEST, revoked => false},
    Request = fun(Facts) ->
        #{
            credential => #{class => shop_key, digest => ?SHOP_DIGEST},
            organization_id => ?ORG_A,
            now => ?NOW,
            facts => {load, fun() -> {ok, Facts} end}
        }
    end,
    ?assertMatch(
        {ok, #{auth_context := cs_shop_key, organization_id := ?ORG_A}},
        eb_auth_app:authorize(Route, Request(Base))
    ),
    ?assertEqual(
        {error, shop_key_revoked},
        eb_auth_app:authorize(Route, Request(Base#{revoked => true}))
    ),
    ?assertEqual(
        {error, shop_key_digest_mismatch},
        eb_auth_app:authorize(Route, Request(Base#{digest => <<"deadbeef">>}))
    ),
    ?assertEqual(
        {error, cross_org},
        eb_auth_app:authorize(Route, Request(Base#{organization_id => ?ORG_B}))
    ),
    %% digest 绝不进入错误项 / 授权上下文
    Denied = eb_auth_app:authorize(Route, Request(Base#{digest => <<"deadbeef">>})),
    ?assertEqual(false, term_contains(Denied, ?SHOP_DIGEST)).

route_metadata_is_the_only_principal_source_test() ->
    %% 缺 auth_context → fail-closed，不按 path 字符串猜
    ?assertEqual(
        {error, missing_auth_context},
        eb_auth_principal:principal_for_route(#{path => <<"/api/v1/enterprise/organizations">>})
    ),
    ?assertEqual(
        {error, {unknown_auth_context, <<"manager">>}},
        eb_auth_principal:principal_for_route(#{auth_context => <<"manager">>})
    ),
    %% 声明 tenant 面却要 platform_admin（或反之）→ fail-closed，杜绝企业身份越权到 Admin 面
    ?assertEqual(
        {error, {surface_principal_mismatch, tenant, platform_admin}},
        eb_auth_principal:principal_for_route(#{
            auth_context => platform_admin,
            surface => tenant,
            required_permission => <<"enterprise_business:read">>
        })
    ),
    ?assertEqual(
        {error, {surface_principal_mismatch, platform, enterprise_member}},
        eb_auth_principal:principal_for_route(#{
            auth_context => enterprise_member,
            surface => platform,
            required_function => <<"sales">>,
            required_permission => <<"contact.read">>
        })
    ),
    %% 元数据内部矛盾（声明 platform 但 path 落在 tenant 前缀）→ fail-closed
    ?assertEqual(
        {error, {surface_mismatch, platform, tenant}},
        eb_auth_principal:principal_for_route(#{
            auth_context => platform_admin,
            surface => platform,
            path => <<"/api/v1/enterprise/organizations/1/contacts">>,
            required_permission => <<"enterprise_business:read">>
        })
    ).

route_metadata_is_authoritative_over_untrusted_path_test() ->
    %% path 不是分流来源：principal 与 required_* 全部来自 metadata；
    %% 未声明 surface 时只能由 path 推断，且必须与 principal 相容。
    ?assertEqual(
        {ok, #{
            surface => tenant,
            principal => enterprise_member,
            required_function => <<"sales">>,
            required_permission => <<"contact.read">>,
            required_governance => [],
            require_contact => false
        }},
        eb_auth_principal:principal_for_route(#{
            auth_context => enterprise_member,
            path => <<"/api/v1/enterprise/organizations/1/contacts">>,
            required_function => <<"sales">>,
            required_permission => <<"contact.read">>
        })
    ),
    %% 未声明 surface 时，租户 URL 也不能把 platform_admin 放行（fail-closed）
    ?assertEqual(
        {error, {surface_principal_mismatch, tenant, platform_admin}},
        eb_auth_principal:principal_for_route(#{
            auth_context => platform_admin,
            path => <<"/api/v1/enterprise/organizations/1/contacts">>,
            required_permission => <<"enterprise_business:read">>
        })
    ).

%% cs_visit 的 contact 绑定是冻结语义，元数据不得关掉它。
visit_contact_binding_cannot_be_disabled_by_metadata_test() ->
    ?assertMatch(
        {ok, #{principal := cs_visit, require_contact := true}},
        eb_auth_principal:principal_for_route(#{
            auth_context => cs_visit,
            surface => tenant,
            path => <<"/api/v1/cs/visit/session">>,
            require_contact => false
        })
    ).

%% §5.4：secret / cipher / Authorization 不得出现在错误项或授权上下文里。
errors_and_context_do_not_leak_secrets_test() ->
    Route = #{
        auth_context => cs_visit,
        surface => tenant,
        path => <<"/api/v1/cs/visit/session">>,
        require_contact => true
    },
    Facts = #{
        organization_id => ?ORG_A,
        contact_id => ?CONTACT_A,
        digest => ?VISIT_DIGEST,
        expires_at => ?NOW + 60,
        revoked => true
    },
    Denied = eb_auth_app:authorize(Route, #{
        credential => #{
            class => visit_token, digest => ?VISIT_DIGEST, authorization => <<"Bearer secret">>
        },
        organization_id => ?ORG_A,
        contact_id => ?CONTACT_A,
        now => ?NOW,
        facts => {load, fun() -> {ok, Facts} end}
    }),
    ?assertEqual({error, visit_token_revoked}, Denied),
    ?assertEqual(false, term_contains(Denied, ?VISIT_DIGEST)),
    ?assertEqual(false, term_contains(Denied, <<"Bearer secret">>)).

%% ===================================================================
%% EB-04-A02：suspended / removed 对旧 JWT 立即失效
%% ===================================================================

%% 旧 JWT 携带「我是 owner、成员状态 active」的自报 claim：一律不采信，
%% 事实源说 suspended 就立即拒绝 —— 不依赖 token 过期时间。
suspended_old_jwt_is_rejected_immediately_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    Facts = member_facts(#{
        member => #{status => suspended, governance_roles => [<<"owner">>]}
    }),
    Credential = jwt_credential(#{
        claims => #{
            <<"member_status">> => <<"active">>,
            <<"role">> => <<"owner">>,
            <<"organization_id">> => ?ORG_A,
            <<"function_key">> => <<"sales">>,
            <<"permissions">> => perms_all()
        }
    }),
    Result = eb_auth_app:authorize(Route, #{
        credential => Credential,
        organization_id => ?ORG_A,
        facts => counting_loader(Ref, {ok, Facts})
    }),
    ?assertEqual({error, {member_not_active, suspended}}, Result),
    %% 事实源被逐请求读取（不是读 JWT / 不是缓存）
    ?assertEqual(1, loader_calls(Ref)).

removed_member_is_rejected_immediately_test() ->
    Route = #{
        auth_context => enterprise_owner_admin,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/members/2/suspend">>
    },
    Facts = member_facts(#{member => #{status => removed, governance_roles => [<<"owner">>]}}),
    ?assertEqual(
        {error, {member_not_active, removed}},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Facts} end}
        })
    ).

%% 同一枚未过期 JWT 在两次请求之间被 suspend：第二次立即失败。
no_caching_between_requests_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    Loader = sequence_loader(Ref, [
        {ok, member_facts(#{})},
        {ok, member_facts(#{member => #{status => suspended, governance_roles => []}})}
    ]),
    Request = fun() ->
        #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => Loader
        }
    end,
    ?assertMatch(
        {ok, #{auth_context := enterprise_member}}, eb_auth_app:authorize(Route, Request())
    ),
    ?assertEqual(
        {error, {member_not_active, suspended}},
        eb_auth_app:authorize(Route, Request())
    ),
    ?assertEqual(2, loader_calls(Ref)).

member_record_missing_test() ->
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    Facts = maps:remove(member, member_facts(#{})),
    ?assertEqual(
        {error, member_not_found},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Facts} end}
        })
    ).

%% ===================================================================
%% EB-04-A03：跨 Org / 跨 identity fail-closed 且零副作用
%% ===================================================================

cross_org_facts_are_rejected_test() ->
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    %% 事实属于 Org B，路由目标为 Org A
    Facts = member_facts(#{organization_id => ?ORG_B}),
    ?assertEqual(
        {error, cross_org},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Facts} end}
        })
    ),
    %% 事实缺 organization_id 同样 fail-closed（不得退化为「无租户条件」）
    ?assertEqual(
        {error, cross_org},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, maps:remove(organization_id, Facts)} end}
        })
    ).

cross_identity_assignment_are_rejected_test() ->
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    %% assignment 指向另一个 Org 的 identity → 跨 identity fail-closed
    Facts = member_facts(#{
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_B,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => active
            }
        ]
    }),
    ?assertEqual(
        {error, cross_identity},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Facts} end}
        })
    ),
    %% assignment 的 assignee 不是请求者 → 不得借用他人 identity
    Facts2 = member_facts(#{
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_B,
                status => active
            }
        ]
    }),
    ?assertEqual(
        {error, identity_assignment_missing},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Facts2} end}
        })
    ),
    %% ended assignment 不构成授权
    Facts3 = member_facts(#{
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => ended
            }
        ]
    }),
    ?assertEqual(
        {error, identity_assignment_missing},
        eb_auth_app:authorize(Route, #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Facts3} end}
        })
    ).

%% 零副作用（一）：拒绝路径不写任何东西、事实对象不被改动、重复调用逐字相同。
denial_is_side_effect_free_and_deterministic_test() ->
    Ref = counters:new(1, []),
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/contacts">>,
        required_function => <<"sales">>,
        required_permission => <<"contact.read">>
    },
    Facts = member_facts(#{organization_id => ?ORG_B}),
    Request = fun() ->
        #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => counting_loader(Ref, {ok, Facts})
        }
    end,
    First = eb_auth_app:authorize(Route, Request()),
    Second = eb_auth_app:authorize(Route, Request()),
    ?assertEqual({error, cross_org}, First),
    ?assertEqual(First, Second),
    %% 两次请求各读一次（无缓存、无重试放大）
    ?assertEqual(2, loader_calls(Ref)),
    %% 注入的事实对象逐字未被改动（不可变、无原地改写）
    ?assertEqual(member_facts(#{organization_id => ?ORG_B}), Facts).

%% 零副作用（二）：授权路径的扩展点契约里**没有任何写 callback**，
%% 且授权实现源码不含 insert/update/delete/append/advance 等写操作。
authz_path_has_no_write_capability_test() ->
    Callbacks = [Name || {Name, _} <- eb_auth_port:behaviour_info(callbacks)],
    ?assert(length(Callbacks) > 0),
    ?assertEqual([], [
        N
     || N <- Callbacks,
        re:run(
            atom_to_list(N),
            "(insert|update|delete|append|advance|write|purge|save|put)",
            [{capture, none}]
        ) =/= nomatch
    ]),
    lists:foreach(
        fun(Rel) ->
            Code = code_only(read_source(Rel)),
            ?assertNotEqual(undefined, Code),
            lists:foreach(
                fun(Needle) -> ?assertEqual(false, contains(Code, Needle)) end,
                [
                    "elib_pg",
                    "SELECT ",
                    "INSERT ",
                    "UPDATE ",
                    "DELETE ",
                    "WITH ",
                    "meck"
                ]
            ),
            ?assertEqual([], [
                Ref
             || Ref <- mod_refs(Code),
                lists:suffix("_repo", atom_to_list(Ref)) orelse
                    lists:suffix("_ds", atom_to_list(Ref))
            ])
        end,
        [?APP_REL, ?PRINCIPAL_REL, ?PERMISSION_REL, ?PORT_REL]
    ).

%% ===================================================================
%% EB-04-A04：现有个人 JWT / open / admin 路由回归不变
%% ===================================================================

%% 静态：既有的免认证直通标记必须原样保留（与 auth_middleware_api_v1_tests 同口径）。
existing_api_v1_markers_preserved_test() ->
    Source = read_source(?AUTH_MIDDLEWARE_V1_REL),
    ?assert(binary:match(Source, <<"IsMcpPath">>) =/= nomatch),
    ?assert(binary:match(Source, <<"IsChannelWebhook orelse IsMcpPath">>) =/= nomatch),
    ?assert(binary:match(Source, <<"IsPaymentCallback">>) =/= nomatch).

%% 静态：新分支必须经企业授权 application 判定 tenant 面，不得在中间件里硬编码前缀猜测。
api_v1_middleware_uses_enterprise_surface_helper_test() ->
    Source = read_source(?AUTH_MIDDLEWARE_V1_REL),
    ?assert(binary:match(Source, <<"eb_auth_principal:is_tenant_surface_path">>) =/= nomatch).

%% 行为：个人 / open 路由的分发与免签名语义不变。
personal_open_and_admin_dispatch_regression_test_() ->
    DispatchMocks = [
        {cowboy_req, [
            {'path', 1, fun(Req) -> maps:get(path, Req, <<"/">>) end},
            {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
        ]},
        {auth_ds, [
            {'remove_last_forward_slash', 1, fun(P) -> P end},
            {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
            {'condition', 5, fun(_InOpt, _InOpen, _Auth, Req, Env) ->
                {ok, Req#{went => fallback}, Env}
            end}
        ]},
        {adm_auth_middleware, [
            {'execute', 2, fun(Req, Env) -> {ok, Req#{went => adm}, Env} end}
        ]},
        {auth_middleware_api_v1, [
            {'execute', 2, fun(Req, Env) -> {ok, Req#{went => api_v1}, Env} end}
        ]},
        {imboy_router, [
            {'open', 0, fun() -> [] end},
            {'option', 0, fun() -> [] end}
        ]},
        {config_ds, [
            {'env', 2, fun(api_auth_switch, D) -> D end}
        ]}
    ],
    ?WITH_MECKS(DispatchMocks, fun() ->
        ?assertEqual(api_v1, route(<<"/api/v1/passport/login">>)),
        ?assertEqual(api_v1, route(<<"/api/v1/user/info">>)),
        ?assertEqual(adm, route(<<"/api/adm/user/list">>)),
        ?assertEqual(adm, route(<<"/api/adm/enterprise-business/organizations">>)),
        ?assertEqual(passthrough, route(<<"/static/img/a.png">>)),
        ?assertEqual(fallback, route(<<"/v1/enterprise/x">>)),
        ?assertEqual(fallback, route(<<"/help">>))
    end).

%% 行为：企业租户路径**不得**因被误登记进 open() 而直通（fail-closed），
%% 而普通 open 路由仍保持免签名直通。
enterprise_tenant_path_never_becomes_open_passthrough_test_() ->
    [
        {
            "tenant_enterprise_path_requires_signature",
            tenant_path_must_not_passthrough(
                <<"/api/v1/enterprise/organizations/1/business-identities">>
            )
        },
        {
            "tenant_cs_path_requires_signature",
            tenant_path_must_not_passthrough(<<"/api/v1/cs/admin/seats">>)
        },
        {"ordinary_open_path_still_passthrough", ordinary_open_path_stays_passthrough()}
    ].

tenant_path_must_not_passthrough(Path) ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> Path end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Default) -> <<"on">> end}
            ]},
            {imboy_router, [
                {'open', 0, fun() -> [Path] end},
                {'option', 0, fun() -> [] end}
            ]},
            {auth_ds, [
                {'remove_last_forward_slash', 1, fun(V) -> V end},
                {'verify_sign', 2, fun(Req, Env) ->
                    {stop, Req#{auth_error => 902, env => Env}}
                end},
                {'condition', 5, fun(_Opt, _Open, _Auth, Req, Env) -> {ok, Req, Env} end}
            ]}
        ],
        fun() ->
            Result = auth_middleware_api_v1:execute(#{}, #{}),
            ?assertMatch({stop, #{auth_error := 902}}, Result),
            ?assertEqual(1, meck:num_calls(auth_ds, verify_sign, 2))
        end
    ).

ordinary_open_path_stays_passthrough() ->
    Path = <<"/api/v1/brand">>,
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> Path end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, _Default) -> <<"on">> end}
            ]},
            {imboy_router, [
                {'open', 0, fun() -> [Path] end},
                {'option', 0, fun() -> [] end}
            ]},
            {auth_ds, [
                {'remove_last_forward_slash', 1, fun(V) -> V end},
                {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
                {'condition', 5, fun(_Opt, InOpen, _Auth, Req, Env) ->
                    {ok, Req#{in_open => InOpen}, Env}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, #{in_open := true}, _}, auth_middleware_api_v1:execute(#{}, #{})),
            ?assertEqual(0, meck:num_calls(auth_ds, verify_sign, 2))
        end
    ).

%% 行为：auth_middleware 的平台面 / 租户面分流顺序不变。
middleware_surface_split_regression_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(Req) -> maps:get(path, Req, <<"/">>) end},
                {'header', 2, fun(<<"authorization">>, _Req) -> undefined end}
            ]},
            {auth_ds, [
                {'remove_last_forward_slash', 1, fun(P) -> P end},
                {'verify_sign', 2, fun(Req, Env) -> {ok, Req, Env} end},
                {'condition', 5, fun(_InOpt, _InOpen, _Auth, Req, Env) ->
                    {ok, Req#{went => fallback}, Env}
                end}
            ]},
            {adm_auth_middleware, [
                {'execute', 2, fun(Req, Env) -> {ok, Req#{went => adm}, Env} end}
            ]},
            {auth_middleware_api_v1, [
                {'execute', 2, fun(Req, Env) -> {ok, Req#{went => api_v1}, Env} end}
            ]},
            {imboy_router, [
                {'open', 0, fun() -> [] end},
                {'option', 0, fun() -> [] end}
            ]},
            {config_ds, [
                {'env', 2, fun(api_auth_switch, D) -> D end}
            ]}
        ],
        fun() ->
            ?assertEqual(adm, route(<<"/api/adm/enterprise-businessx">>)),
            ?assertEqual(api_v1, route(<<"/api/v1/enterprise/organizations/1/contacts">>)),
            ?assertEqual(api_v1, route(<<"/api/v1/cs/admin/seats">>))
        end
    ).

%% 负例（承重）：不得新建第二套 RBAC。
no_second_rbac_test() ->
    Sources = [read_source(?APP_REL), read_source(?PERMISSION_REL), read_source(?PORT_REL)],
    lists:foreach(
        fun(Source) ->
            Code = code_only(Source),
            lists:foreach(
                fun(Needle) -> ?assertEqual(false, contains(Code, Needle)) end,
                ["rbac", "role_permission", "permission_role", "CREATE TABLE", "acl_table"]
            )
        end,
        Sources
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

route(Path) ->
    {ok, Req, _Env} = auth_middleware:execute(#{path => Path}, #{}),
    maps:get(went, Req, passthrough).

read_source(Rel) ->
    case
        [P || P <- [Rel, "../" ++ Rel, "../../" ++ Rel, "../../../" ++ Rel], filelib:is_regular(P)]
    of
        [Path | _] ->
            case file:read_file(Path) of
                {ok, Bin} -> Bin;
                {error, _} -> undefined
            end;
        [] ->
            undefined
    end.

code_only(undefined) ->
    undefined;
code_only(Source) ->
    strip_comments(Source).

strip_comments(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    iolist_to_binary([[strip_line(Line), <<"\n">>] || Line <- Lines]).

strip_line(Line) ->
    case binary:split(Line, <<"%">>) of
        [Before, _After] -> Before;
        [Only] -> Only
    end.

mod_refs(undefined) ->
    [];
mod_refs(Code) ->
    CallRefs = capture_all(Code, "\\b([a-z][a-z0-9_]*):[a-z_][a-z0-9_]*\\("),
    BehaviourRefs = capture_all(Code, "^-behaviou?r\\(([a-z][a-z0-9_]*)\\)"),
    lists:usort(CallRefs ++ BehaviourRefs).

capture_all(Code, Pattern) ->
    case re:run(Code, Pattern, [global, multiline, {capture, [1], binary}]) of
        {match, Matches} -> lists:usort([binary_to_atom(M, utf8) || [M] <- Matches]);
        nomatch -> []
    end.

contains(undefined, _Needle) ->
    false;
contains(Source, Needle) ->
    binary:match(Source, list_to_binary(Needle)) =/= nomatch.

term_contains(Term, Needle) ->
    %% 递归遍历 term：任何位置的 binary 命中即算泄露（不依赖 term 编码细节）。
    term_contains_walk(Term, Needle).

term_contains_walk(Bin, Needle) when is_binary(Bin) ->
    binary:match(Bin, Needle) =/= nomatch;
term_contains_walk(List, Needle) when is_list(List) ->
    lists:any(fun(Item) -> term_contains_walk(Item, Needle) end, List);
term_contains_walk(Tuple, Needle) when is_tuple(Tuple) ->
    term_contains_walk(tuple_to_list(Tuple), Needle);
term_contains_walk(Map, Needle) when is_map(Map) ->
    term_contains_walk(lists:append([[K, V] || {K, V} <- maps:to_list(Map)]), Needle);
term_contains_walk(_Other, _Needle) ->
    false.

%%% ==================================================================
%%% A0FIX-02（2026-09-14）：授权事实 payload 的**键集合**是 load-bearing 的
%%% ==================================================================
%%%
%%% BUG-02：`eb_auth_port:load_request_facts/1` 原先只冻结 arity、不管键集合；
%%% 装配实现 `eb_pg_auth_facts` 的 `assignments` 行只投影了
%%% business_identity_id/function_key/status/version，缺 `user_id` 与
%%% `organization_id`。而本模块的 `active_assignments/3` 用这两键做归属过滤
%%% （`user_id`）与跨 Org 判定（`organization_id`）⇒ **真实装配路径下企业成员恒被
%%% `identity_assignment_missing` 拒绝**（fail-closed，安全但不可用）。
%%%
%%% 下面三条把「形状」钉成可回归的事实：缺键必红、补齐必通、提供端 SQL 必须投影。

%% 负例：缺 `user_id`/`organization_id` 的旧形状 ⇒ 恒拒（证明补键是必要的）。
auth_facts_missing_member_keys_is_rejected_test() ->
    ?assertEqual(
        {error, identity_assignment_missing},
        eb_auth_app:authorize(enterprise_sales_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts =>
                {load, fun() ->
                    {ok,
                        member_facts(#{
                            assignments => [
                                #{
                                    business_identity_id => ?IDENTITY_SALES,
                                    function_key => <<"sales">>,
                                    status => active
                                }
                            ]
                        })}
                end}
        })
    ).

%% 正向：同一请求 + 完整形状 ⇒ 通过。与上条合起来证明形状是 load-bearing 的。
auth_facts_complete_member_keys_is_accepted_test() ->
    ?assertMatch(
        {ok, #{function_key := <<"sales">>}},
        eb_auth_app:authorize(enterprise_sales_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, member_facts(#{})} end}
        })
    ).

%% 机械对齐：装配实现的 SQL 必须投影消费者要求的两键（防有人删列）。
auth_facts_sql_projects_consumer_required_keys_test() ->
    [MemberSql, AssignmentsSql] = eb_pg_auth_facts:sql_statements(),
    ?assert(binary:match(AssignmentsSql, <<"a.user_id">>) =/= nomatch),
    ?assert(binary:match(AssignmentsSql, <<"a.organization_id">>) =/= nomatch),
    ?assert(binary:match(MemberSql, <<"m.user_id">>) =/= nomatch),
    ?assert(binary:match(MemberSql, <<"m.organization_id">>) =/= nomatch).

enterprise_sales_route() ->
    #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/conversations">>,
        required_function => <<"sales">>,
        required_permission => <<"conversation.read">>
    }.

%% ===================================================================
%% EB-01 / A01.36 方案 a：身份归属确定性 hint（2026-09-20）
%% ===================================================================

%% A01.36 场景夹具：offboarding 承接后同一 user 同持 sales + customer_service
%% 各一条 active；消息真源面（CSX-01 白名单 [sales, customer_service]）双命中。
dual_function_facts() ->
    dual_function_facts(undefined).

%% Hint 为身份归属确定性 hint：生产路径由 facts 实现投影（见
%% eb_pg_auth_facts:maybe_add_identity_hint/3），纯逻辑注入按同键位补齐。
dual_function_facts(Hint) ->
    Base = member_facts(#{
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => active
            },
            #{
                business_identity_id => ?IDENTITY_CS,
                organization_id => ?ORG_A,
                function_key => <<"customer_service">>,
                user_id => ?USER_A,
                status => active
            }
        ]
    }),
    case Hint of
        undefined -> Base;
        _ -> Base#{resource_identity_hint => Hint}
    end.

conversation_hint_route() ->
    #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/conversations/42/messages">>,
        required_function => [<<"sales">>, <<"customer_service">>],
        required_permission => <<"conversation.read">>
    }.

%% 无 hint（兼容回归）：白名单双命中歧义维持 fail-closed 拒绝。
identity_hint_absent_keeps_ambiguity_rejected_test() ->
    ?assertEqual(
        {error, {multiple_active_assignment, [<<"sales">>, <<"customer_service">>]}},
        eb_auth_app:authorize(conversation_hint_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, dual_function_facts()} end}
        })
    ).

%% hint 恰命中一条 → 以会话经办身份执行（A01.36 承接人读不到继承历史的翻转）。
identity_hint_exactly_one_flips_to_conversation_identity_test() ->
    ?assertMatch(
        {ok, #{business_identity_id := ?IDENTITY_SALES, function_key := <<"sales">>}},
        eb_auth_app:authorize(conversation_hint_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, dual_function_facts(?IDENTITY_SALES)} end}
        })
    ),
    ?assertMatch(
        {ok, #{business_identity_id := ?IDENTITY_CS, function_key := <<"customer_service">>}},
        eb_auth_app:authorize(conversation_hint_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, dual_function_facts(?IDENTITY_CS)} end}
        })
    ).

%% hint 落空（零命中）→ 维持歧义拒绝：fail-closed 不放宽。
identity_hint_zero_match_keeps_rejected_test() ->
    ?assertEqual(
        {error, {multiple_active_assignment, [<<"sales">>, <<"customer_service">>]}},
        eb_auth_app:authorize(conversation_hint_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, dual_function_facts(999999)} end}
        })
    ).

%% hint 畸形（非整数）→ 与任何 identity 不等 → 零命中 → 维持拒绝。
identity_hint_malformed_keeps_rejected_test() ->
    ?assertEqual(
        {error, {multiple_active_assignment, [<<"sales">>, <<"customer_service">>]}},
        eb_auth_app:authorize(conversation_hint_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, dual_function_facts(<<"not-an-identity">>)} end}
        })
    ).

%% 同 identity 双职能脏数据：hint 过滤后仍多条 → 维持拒绝（恰一条 ≠ 至少一条）。
identity_hint_multiple_match_keeps_rejected_test() ->
    Dirty = member_facts(#{
        assignments => [
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"sales">>,
                user_id => ?USER_A,
                status => active
            },
            #{
                business_identity_id => ?IDENTITY_SALES,
                organization_id => ?ORG_A,
                function_key => <<"customer_service">>,
                user_id => ?USER_A,
                status => active
            }
        ]
    }),
    ?assertEqual(
        {error, {multiple_active_assignment, [<<"sales">>, <<"customer_service">>]}},
        eb_auth_app:authorize(conversation_hint_route(), #{
            credential => jwt_credential(#{}),
            organization_id => ?ORG_A,
            facts => {load, fun() -> {ok, Dirty#{resource_identity_hint => ?IDENTITY_SALES}} end}
        })
    ).
