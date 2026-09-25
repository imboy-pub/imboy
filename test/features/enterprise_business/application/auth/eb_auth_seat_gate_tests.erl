%%% @doc CS-BE-01C（GAP-1）：eb 面 enterprise_member 授权的 **seat enabled 门**测试。
%%%
%%% 依据：CS-INT-01 集成门 GAP-1——「eb 面授权缺 seat enabled 门：suspend 后
%%% enterprise/assets/messages 仍可读写（CS 面 fail-closed 已证）」。
%%%
%%% 语义与错误码对齐 CS 面 `cs_auth:seat_enabled_gate/3`：
%%%
%%%   * 选中 assignment 的 `function_key = customer_service` 时，逐请求查
%%%     `customer_service_facade:fetch_seat(OrgId, IdentityId)`；
%%%   * seat 行存在且 `enabled = false` → `{error, seat_disabled}`（suspend 即时
%%%     拒绝，不等 token 过期、不跨请求缓存）；
%%%   * seat 行不存在（从未开通坐席）→ 维持 eb 面既有授权行为（放行至后续
%%%     asset ACL / 用例门；`eb_tenant_handler_tests` 的 cs_seat_not_assignee
%%%     场景钉住该行为，本门不得提前拦截改标签）；
%%%   * 取数失败（非 not_found）→ fail-closed 原样拒绝，不得静默放行；
%%%   * `sales` 职能不属坐席域 → 门不触发、零 seat 查询。
%%%
%%% 本套件是**纯逻辑**套件：零 DB、零 app 启动；seat 事实经 meck
%%% `customer_service_facade:fetch_seat/2` 注入（与 eb_auth_tests 的
%%% ?WITH_MECKS harness 同模式）。修复前：suspended seat 场景红（旧授权链
%%% 无门即放行）；修复后：全绿。
-module(eb_auth_seat_gate_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_A, 4230001).
-define(ORG_B, 4230002).
-define(USER_A, 913001).
-define(IDENTITY_CS, 513001).
-define(IDENTITY_SALES, 513002).

%% ===================================================================
%% 夹具：合成事实 + seat 查询 meck（随机 TSID，无真实账号 / 凭据）
%% ===================================================================

%% GAP-1 点名的 enterprise assets/messages 读写权限面（读 + 写各取其双）。
gap_permissions() ->
    [
        <<"asset.read">>,
        <<"asset.write">>,
        <<"conversation.read">>,
        <<"message.write">>
    ].

cs_assignment() ->
    cs_assignment(?IDENTITY_CS).

cs_assignment(IdentityId) ->
    #{
        business_identity_id => IdentityId,
        organization_id => ?ORG_A,
        function_key => <<"customer_service">>,
        user_id => ?USER_A,
        status => active
    }.

sales_assignment() ->
    #{
        business_identity_id => ?IDENTITY_SALES,
        organization_id => ?ORG_A,
        function_key => <<"sales">>,
        user_id => ?USER_A,
        status => active
    }.

cs_member_facts(Overrides) ->
    Base = #{
        organization_id => ?ORG_A,
        member => #{status => active, governance_roles => []},
        assignments => [cs_assignment()],
        permissions => gap_permissions() ++ [<<"conversation.write">>, <<"contact.read">>]
    },
    maps:merge(Base, Overrides).

sales_member_facts() ->
    #{
        organization_id => ?ORG_A,
        member => #{status => active, governance_roles => []},
        assignments => [sales_assignment()],
        permissions => gap_permissions() ++ [<<"conversation.write">>, <<"contact.read">>]
    }.

%% eb 面真源路由形态（assets/messages 均挂 enterprise_member + 职能白名单，
%% 见 eb_enterprise_actions 的 member_auth 声明）。
member_route(Function, Permission) ->
    #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/conversations/42/messages">>,
        required_function => Function,
        required_permission => Permission
    }.

request(Facts) ->
    #{
        credential => #{class => imboy_jwt, user_id => ?USER_A, claims => #{}},
        organization_id => ?ORG_A,
        facts => {load, fun() -> {ok, Facts} end}
    }.

%% seat 查询 meck：固定结果 + 参数记录（进程字典，测试进程内隔离）。
seat_mock(SeatResult) ->
    [
        {customer_service_facade, [
            {fetch_seat, 2, fun(OrgId, #{business_identity_id := IdentityId}) ->
                put(seat_gate_last_args, {OrgId, IdentityId}),
                SeatResult
            end}
        ]}
    ].

%% 「绝不该被查到」的哨兵 meck：任何调用即 crash（测试失败）。
seat_never_mock() ->
    [
        {customer_service_facade, [
            {fetch_seat, 2, fun(_OrgId, _Params) ->
                erlang:error(unexpected_seat_lookup)
            end}
        ]}
    ].

last_seat_args() ->
    erase(seat_gate_last_args).

%% ===================================================================
%% GAP-1 主场景（before-failing）：suspended seat 后，eb 面 assets/messages
%% 读写全部 fail-closed（seat_disabled），错误码与 CS 面逐字一致。
%% ===================================================================

cs_assignment_with_suspended_seat_is_denied_on_gap_surfaces_test_() ->
    ?WITH_MECKS(seat_mock({ok, #{enabled => false}}), fun() ->
        Facts = cs_member_facts(#{}),
        lists:foreach(
            fun(Permission) ->
                Result = eb_auth_app:authorize(
                    member_route(<<"customer_service">>, Permission), request(Facts)
                ),
                ?assertEqual({error, seat_disabled}, Result),
                %% 拒绝原因必须是 seat 门（不是 permission/function/identity 的旧门）
                ?assertNotMatch({error, {permission_missing, _}}, Result),
                ?assertNotEqual({error, identity_assignment_missing}, Result),
                %% 门查的是目标 Org + 选中 identity（租户作用域显式，铁律 6）
                ?assertEqual({?ORG_A, ?IDENTITY_CS}, last_seat_args())
            end,
            gap_permissions()
        )
    end).

%% 白名单路由（sales|customer_service）以 CS 身份命中时同样过门——
%% 这是坐席经 eb 面回消息（INT-01 C 链）的路径形态。
cs_identity_in_whitelist_route_with_suspended_seat_is_denied_test_() ->
    ?WITH_MECKS(seat_mock({ok, #{enabled => false}}), fun() ->
        Route = member_route([<<"sales">>, <<"customer_service">>], <<"message.write">>),
        Facts = cs_member_facts(#{resource_identity_hint => ?IDENTITY_CS}),
        ?assertEqual(
            {error, seat_disabled},
            eb_auth_app:authorize(Route, request(Facts))
        )
    end).

%% suspend 即时生效：同一 JWT 同一路由，seat 由 enabled 翻转为 disabled 的
%% **下一个请求**立即被拒（逐请求查 seat，不缓存、不等 token 过期）。
seat_suspension_blocks_the_next_request_immediately_test_() ->
    Ref = counters:new(1, []),
    Results = [{ok, #{enabled => true}}, {ok, #{enabled => false}}],
    Mocks = [
        {customer_service_facade, [
            {fetch_seat, 2, fun(_OrgId, _Params) ->
                N = counters:get(Ref, 1) + 1,
                _ = counters:add(Ref, 1, 1),
                lists:nth(N, Results)
            end}
        ]}
    ],
    ?WITH_MECKS(Mocks, fun() ->
        Route = member_route(<<"customer_service">>, <<"asset.write">>),
        Request = request(cs_member_facts(#{})),
        ?assertMatch(
            {ok, #{auth_context := enterprise_member}}, eb_auth_app:authorize(Route, Request)
        ),
        ?assertEqual(
            {error, seat_disabled},
            eb_auth_app:authorize(Route, Request)
        )
    end).

%% 正例：enabled seat 放行，且授权上下文仍是 enterprise_member + 选中 identity。
cs_assignment_with_enabled_seat_is_allowed_test_() ->
    ?WITH_MECKS(seat_mock({ok, #{enabled => true}}), fun() ->
        Route = member_route(<<"customer_service">>, <<"asset.write">>),
        ?assertMatch(
            {ok, #{
                auth_context := enterprise_member,
                business_identity_id := ?IDENTITY_CS,
                function_key := <<"customer_service">>
            }},
            eb_auth_app:authorize(Route, request(cs_member_facts(#{})))
        )
    end).

%% seat 行不存在（成员从未开通坐席）→ 维持 eb 面既有授权行为（不提前拦截，
%% 落后续 asset ACL / 用例门；与 eb_tenant_handler_tests 的 cs_seat_not_assignee
%% 钉住的行为一致），门参数仍带目标 Org。
cs_assignment_without_seat_row_keeps_prior_authorization_test_() ->
    ?WITH_MECKS(seat_mock({error, not_found}), fun() ->
        Route = member_route(<<"customer_service">>, <<"asset.read">>),
        ?assertMatch(
            {ok, #{auth_context := enterprise_member, business_identity_id := ?IDENTITY_CS}},
            eb_auth_app:authorize(Route, request(cs_member_facts(#{})))
        ),
        ?assertEqual({?ORG_A, ?IDENTITY_CS}, last_seat_args())
    end).

%% 取数失败（非 not_found）→ fail-closed 原样拒绝，绝不静默放行。
seat_lookup_failure_fails_closed_test_() ->
    ?WITH_MECKS(seat_mock({error, pool_timeout}), fun() ->
        Route = member_route(<<"customer_service">>, <<"asset.read">>),
        ?assertEqual(
            {error, pool_timeout},
            eb_auth_app:authorize(Route, request(cs_member_facts(#{})))
        )
    end).

%% sales 职能不属坐席域：门不触发（零 seat 查询），授权行为不变。
sales_assignment_does_not_query_seat_test_() ->
    ?WITH_MECKS(seat_mock({ok, #{enabled => false}}), fun() ->
        Route = member_route(<<"sales">>, <<"asset.write">>),
        ?assertMatch(
            {ok, #{auth_context := enterprise_member, function_key := <<"sales">>}},
            eb_auth_app:authorize(Route, request(sales_member_facts()))
        ),
        %% sales 路由从未查 seat（mock 未被调用 → 无参数记录）
        ?assertEqual(undefined, last_seat_args())
    end).

%% CS 职能但选中 assignment 缺 business_identity_id（脏数据）→ 无法证明 seat
%% 状态 → fail-closed（identity_assignment_missing，与 CS 面兜底同标签）；
%% 且不得触达 seat 查询。
malformed_cs_identity_fails_closed_without_seat_query_test_() ->
    ?WITH_MECKS(seat_never_mock(), fun() ->
        Dirty = cs_member_facts(#{
            assignments => [maps:remove(business_identity_id, cs_assignment())]
        }),
        ?assertEqual(
            {error, identity_assignment_missing},
            eb_auth_app:authorize(
                member_route(<<"customer_service">>, <<"asset.read">>), request(Dirty)
            )
        )
    end).

%% 治理面不受门影响：enterprise_owner_admin 判定与坐席域无关（负例回归）。
owner_admin_route_is_not_affected_by_seat_gate_test_() ->
    ?WITH_MECKS(seat_never_mock(), fun() ->
        Route = #{
            auth_context => enterprise_owner_admin,
            surface => tenant,
            path => <<"/api/v1/enterprise/organizations/1/members/2/suspend">>
        },
        Facts = #{
            organization_id => ?ORG_A,
            member => #{status => active, governance_roles => [<<"owner">>], user_id => ?USER_A},
            assignments => [cs_assignment()],
            permissions => []
        },
        ?assertMatch(
            {ok, #{auth_context := enterprise_owner_admin}},
            eb_auth_app:authorize(Route, request(Facts))
        )
    end).
