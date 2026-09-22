%% enterprise_internal_grant_pg_tests
%% FULL-01 — Application Grant 授权求值 / IDOR 矩阵 / 撤权与降级即时生效
%% （迁移 00000139 + enterprise_application_grant_logic + 既有认证链组合）。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL01_INTTEST，直连
%% imboy_pg18:4323）。业务用例每条 BEGIN ... ROLLBACK，不留数据。
%%
%% 覆盖（plan-full §3.1 授权语义、§7 安全硬门）：
%%   ① 未受管应用沿用广州期口径（grant_governed=false ⇒ allowed_scopes 生效、
%%      Grant 边界不加限制）——既有 210 条企业域用例语义不漂移的前提
%%   ② 首个 Grant 即刻收窄：生效 scope = allowed_scopes ∩ 生效 Grant scopes
%%      （双向交集：Grant 收窄 + Application 上限仍生效）
%%   ③ 资源边界：workspace 必须由**同一个** Grant 同时覆盖 scope 与 workspace
%%      （禁止「scope 来自 A、workspace 来自 B」拼接）
%%   ④ 双 Org × 双 Workspace × 多 Grant 的 cross-tenant/IDOR 矩阵全部拒绝
%%   ⑤ grant revoked ⇒ 下一请求即失败（真认证链 decide，非仅函数级断言）
%%   ⑥ scope downgrade ⇒ 下一请求即失败，且未降级的 scope 仍然放行
%%   ⑦ 有效期窗口（未生效 / 已过期）不产生授权
%%   ⑧ 与广州期既有认证链组合：credential 错/撤销、application disabled、
%%      organization archived 仍先于 Grant 判定失败（不建第二套 middleware）
%%   ⑨ 治理接口输入校验与 CAS：幂等键、固定枚举、跨 Org、版本冲突、状态机
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% ID 段：997xxx（本 run 独立 marker 库，跨套件不共享数据）。

-module(enterprise_internal_grant_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(OWNER_A, 997001).
-define(OWNER_B, 997002).
-define(ORG_A, 997101).
-define(ORG_B, 997102).

-define(WS_A1, 997301).
-define(WS_A2, 997302).
-define(WS_B1, 997303).
-define(WS_B2, 997304).

-define(SECRET_A, <<"full01_app_a_high_entropy_secret_0123456789">>).
-define(SECRET_B, <<"full01_app_b_high_entropy_secret_0123456789">>).
-define(SECRET_C, <<"full01_app_c_high_entropy_secret_0123456789">>).

%% App A 的 allowed_scopes（Grant 只能在其中取交集）
-define(SCOPES_A, [
    <<"application:read">>,
    <<"identities:read">>,
    <<"identities:write">>,
    <<"groups:write">>,
    <<"files:write">>
]).

-define(FAR_FUTURE, <<"2099-12-31T00:00:00+00:00">>).
-define(RATE_CFG, #{internal_read => 1000, internal_write => 1000, internal_sso => 1000}).

-define(IDEM, {<<"idempotency-key">>, <<"full01-idem-key">>}).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    {ok, _} = application:ensure_all_started(throttle),
    application:set_env(imboy, enterprise_internal_rate_limits, ?RATE_CFG),
    inttest_marker_db:provision(#{
        env_prefix => <<"FULL01_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_conn(State) ->
    application:unset_env(imboy, enterprise_internal_rate_limits),
    inttest_marker_db:release(State),
    ok.

with_tx(C, TestFun) ->
    ?_test(begin
        ok = exec(C, <<"BEGIN">>),
        try
            TestFun(C),
            ok
        after
            exec(C, <<"ROLLBACK">>)
        end
    end).

%% 负例包装：失败的 SQL 会把整个事务置为 aborted（25P02），后续语句一律
%% 25P02 直到 ROLLBACK；预期报错的调用必须包在 SAVEPOINT 内执行并回退。
in_savepoint(C, Fun) ->
    ok = exec(C, <<"SAVEPOINT full01_b_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT full01_b_sp">>),
        exec(C, <<"RELEASE SAVEPOINT full01_b_sp">>)
    end.

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

grant_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [
                {"ungoverned_app_keeps_gz_scope_semantics", with_tx(C, fun ungoverned_oracle/1)},
                {"first_grant_narrows_effective_scope_immediately",
                    with_tx(C, fun first_grant_oracle/1)},
                {"effective_scope_is_intersection_both_directions",
                    with_tx(C, fun intersection_oracle/1)},
                {"workspace_boundary_requires_same_grant", with_tx(C, fun same_grant_oracle/1)},
                {"cross_org_idor_matrix_all_rejected", with_tx(C, fun idor_matrix_oracle/1)},
                {"grant_revoked_next_request_fails", with_tx(C, fun revoke_immediate_oracle/1)},
                {"scope_downgrade_next_request_fails", with_tx(C, fun scope_downgrade_oracle/1)},
                {"grant_validity_window_gates_authorization",
                    with_tx(C, fun validity_window_oracle/1)},
                {"chain_composition_preserves_gz_gates",
                    with_tx(C, fun chain_composition_oracle/1)},
                {"governance_ops_validation_and_cas", with_tx(C, fun governance_ops_oracle/1)}
            ]
        end}}.

%%%===================================================================
%%% ① 未受管应用：广州期口径不变
%%%===================================================================

ungoverned_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    %% 无任何 Grant：grant_governed=false，生效 scope = allowed_scopes
    ?assertEqual(
        {ok, #{grant_governed => false, effective_scopes => lists:usort(?SCOPES_A)}},
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppA, ?SCOPES_A)
    ),
    {ok, Ctx} = auth_ctx(C, Fx, a),
    ?assertEqual(false, maps:get(grant_governed, Ctx)),
    ?assertEqual(lists:usort(?SCOPES_A), maps:get(granted_scopes, Ctx)),
    %% 认证链放行 Grant 相关的静态 scope 路由
    ?assertMatch({ok, #{route_id := <<"INT-01">>}}, decide(C, Fx, a, <<"GET">>, app_path(), [])),
    %% 未受管 ⇒ Grant 边界层不加限制（org/workspace 边界仍由既有 handler 判定）
    ?assertEqual(ok, require_workspace(C, Ctx, ?WS_A1, <<"groups:write">>)),
    ?assertEqual(ok, require_workspace(C, Ctx, ?WS_B1, <<"groups:write">>)).

%%%===================================================================
%%% ② 首个 Grant：生效 scope 立即收窄
%%%===================================================================

first_grant_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    {ok, Grant} =
        issue(C, ?ORG_A, AppA, <<"k-narrow">>, [<<"groups:write">>, <<"identities:read">>]),
    ?assertEqual(1, maps:get(<<"version">>, Grant)),

    ?assertEqual(
        {ok, #{
            grant_governed => true,
            effective_scopes => [<<"groups:write">>, <<"identities:read">>]
        }},
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppA, ?SCOPES_A)
    ),
    {ok, Ctx} = auth_ctx(C, Fx, a),
    ?assertEqual(true, maps:get(grant_governed, Ctx)),
    %% application:read 仍在 allowed_scopes 内，但没有任何 Grant 授予它 ⇒ 拒绝
    ?assertEqual(
        {error, insufficient_scope}, decide(C, Fx, a, <<"GET">>, app_path(), [])
    ),
    %% 被 Grant 覆盖的 scope 正常放行（mutation 带 Idempotency-Key）
    ?assertMatch(
        {ok, #{route_id := <<"INT-04">>}},
        decide(C, Fx, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ),
    %% 表外路径仍然 fail-closed
    ?assertEqual(
        {error, resource_not_found}, decide(C, Fx, a, <<"GET">>, <<"/api/internal/v1/nope">>, [])
    ).

%%%===================================================================
%%% ③ 交集双向生效（Grant 收窄 & Application 上限）
%%%===================================================================

intersection_oracle(C) ->
    Fx = seed_fixture(C),
    %% App C 的 allowed_scopes 只有 groups:write；Grant 额外给 files:write
    AppC = maps:get(app_c, Fx),
    {ok, _} = issue(C, ?ORG_A, AppC, <<"k-ceiling">>, [<<"groups:write">>, <<"files:write">>]),
    ?assertEqual(
        {ok, #{grant_governed => true, effective_scopes => [<<"groups:write">>]}},
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppC, [<<"groups:write">>])
    ),
    {ok, CtxC} = auth_ctx(C, Fx, c),
    ?assertEqual([<<"groups:write">>], maps:get(granted_scopes, CtxC)),
    ?assertMatch(
        {ok, #{route_id := <<"INT-04">>}},
        decide(C, Fx, c, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ),
    ?assertEqual(
        {error, insufficient_scope},
        decide(C, Fx, c, <<"POST">>, <<"/api/internal/v1/files/presign">>, [?IDEM])
    ),
    %% 反向：App A（allowed 含 files:write）拿到只授 files:write 的 Grant
    AppA = maps:get(app_a, Fx),
    {ok, _} = issue(C, ?ORG_A, AppA, <<"k-narrow2">>, [<<"files:write">>]),
    ?assertEqual(
        {ok, #{grant_governed => true, effective_scopes => [<<"files:write">>]}},
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppA, ?SCOPES_A)
    ).

%%%===================================================================
%%% ④ 资源边界必须由同一个 Grant 同时覆盖 scope 与 workspace
%%%===================================================================

same_grant_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    {ok, _} =
        issue_ws(C, ?ORG_A, AppA, <<"k-ws-a1">>, [<<"files:write">>], [?WS_A1]),
    {ok, _} =
        issue_ws(C, ?ORG_A, AppA, <<"k-ws-a2">>, [<<"groups:write">>], [?WS_A2]),
    {ok, Ctx} = auth_ctx(C, Fx, a),
    ?assertEqual([<<"files:write">>, <<"groups:write">>], maps:get(granted_scopes, Ctx)),

    %% 各自命中的组合放行
    ?assertEqual(ok, require_workspace(C, Ctx, ?WS_A1, <<"files:write">>)),
    ?assertEqual(ok, require_workspace(C, Ctx, ?WS_A2, <<"groups:write">>)),
    %% 交叉组合必须拒绝：scope 生效，但覆盖该 workspace 的 Grant 没有这个 scope
    ?assertEqual(
        {error, organization_boundary_violation},
        require_workspace(C, Ctx, ?WS_A1, <<"groups:write">>)
    ),
    ?assertEqual(
        {error, organization_boundary_violation},
        require_workspace(C, Ctx, ?WS_A2, <<"files:write">>)
    ),
    %% 未授予的 scope 直接 insufficient_scope
    ?assertEqual(
        {error, insufficient_scope}, require_workspace(C, Ctx, ?WS_A1, <<"identities:read">>)
    ),
    %% 跨 Org workspace 不可能被本 Application 的 Grant 覆盖
    ?assertEqual(
        {error, organization_boundary_violation},
        require_workspace(C, Ctx, ?WS_B1, <<"files:write">>)
    ),

    %% org 全域 Grant（kind=none）覆盖本 Org 全部 workspace，但不跨 Org
    {ok, _} = issue(C, ?ORG_A, AppA, <<"k-ws-none">>, [<<"identities:read">>]),
    {ok, Ctx2} = auth_ctx(C, Fx, a),
    ?assertEqual(ok, require_workspace(C, Ctx2, ?WS_A1, <<"identities:read">>)),
    ?assertEqual(ok, require_workspace(C, Ctx2, ?WS_A2, <<"identities:read">>)),
    %% 跨 Org workspace：即便存在 org 全域 Grant 也必须拒绝（workspace 以
    %% (organization_id, id) 进 SQL，org 全域不等于跨租户旁路）
    ?assertEqual(
        {error, organization_boundary_violation},
        require_workspace(C, Ctx2, ?WS_B1, <<"identities:read">>)
    ),
    ?assertEqual(
        {ok, false},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_A, AppA, ?WS_B2, <<"identities:read">>
        )
    ),
    %% 显式 Workspace Grant 只覆盖列出的 workspace
    ?assertEqual(
        {ok, false},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_A, AppA, ?WS_A2, <<"files:write">>
        )
    ).

%%%===================================================================
%%% ⑤ 双 Org × 双 Workspace IDOR 矩阵
%%%===================================================================

idor_matrix_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    AppB = maps:get(app_b, Fx),
    %% App A：org 全域 groups:write + 仅 A1 的 files:write
    {ok, _} = issue(C, ?ORG_A, AppA, <<"k-idor-none">>, [<<"groups:write">>]),
    {ok, _} = issue_ws(C, ?ORG_A, AppA, <<"k-idor-a1">>, [<<"files:write">>], [?WS_A1]),
    %% App B（Org B）：仅 B1 的 groups:write
    {ok, _} = issue_ws(C, ?ORG_B, AppB, <<"k-idor-b1">>, [<<"groups:write">>], [?WS_B1]),

    {ok, CtxA} = auth_ctx(C, Fx, a),
    {ok, CtxB} = auth_ctx(C, Fx, b),
    ?assertEqual([<<"files:write">>, <<"groups:write">>], maps:get(granted_scopes, CtxA)),
    ?assertEqual([<<"groups:write">>], maps:get(granted_scopes, CtxB)),

    %% {上下文, workspace, scope, 期望}
    Matrix = [
        {a, ?WS_A1, <<"groups:write">>, ok},
        {a, ?WS_A2, <<"groups:write">>, ok},
        {a, ?WS_B1, <<"groups:write">>, {error, organization_boundary_violation}},
        {a, ?WS_B2, <<"groups:write">>, {error, organization_boundary_violation}},
        {a, ?WS_A1, <<"files:write">>, ok},
        {a, ?WS_A2, <<"files:write">>, {error, organization_boundary_violation}},
        {b, ?WS_B1, <<"groups:write">>, ok},
        {b, ?WS_A1, <<"groups:write">>, {error, organization_boundary_violation}},
        {b, ?WS_A2, <<"groups:write">>, {error, organization_boundary_violation}},
        {b, ?WS_B2, <<"groups:write">>, {error, organization_boundary_violation}},
        {b, ?WS_B1, <<"files:write">>, {error, insufficient_scope}},
        {b, ?WS_A1, <<"files:write">>, {error, insufficient_scope}}
    ],
    CtxOf = fun
        (a) -> CtxA;
        (b) -> CtxB
    end,
    lists:foreach(
        fun({Who, WsId, Scope, Expected}) ->
            ?assertEqual(
                Expected,
                require_workspace(C, CtxOf(Who), WsId, Scope),
                {idor_case, Who, WsId, Scope}
            )
        end,
        Matrix
    ),
    %% 直接读面复核：受管应用的 workspace 覆盖只在同 Org、同 Grant 内成立
    ?assertEqual(
        {ok, false},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_B, AppB, ?WS_A1, <<"groups:write">>
        )
    ),
    ?assertEqual(
        {ok, false},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_A, AppA, ?WS_B1, <<"files:write">>
        )
    ),
    %% 跨 Org 读同一 AppId 不串号（Org 边界进 SQL）
    ?assertEqual(
        {ok, false},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_B, AppA, ?WS_A1, <<"groups:write">>
        )
    ).

%%%===================================================================
%%% ⑥ 撤权 ⇒ 下一请求即失败
%%%===================================================================

revoke_immediate_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    {ok, Grant} = issue(C, ?ORG_A, AppA, <<"k-revoke">>, [<<"groups:write">>]),
    GrantId = maps:get(<<"id">>, Grant),
    Headers = headers(Fx, a, [?IDEM]),

    %% 撤权前：真认证链 + 真路由判定放行
    ?assertMatch(
        {ok, #{route_id := <<"INT-04">>}},
        enterprise_internal_auth:decide(
            <<"POST">>, <<"/api/internal/v1/groups">>, Headers, auth_fun(C, Fx, a)
        )
    ),
    {ok, Before} = auth_ctx(C, Fx, a),
    ?assertEqual([<<"groups:write">>], maps:get(granted_scopes, Before)),

    %% 撤权（CAS v1 → v2）
    ?assertEqual(ok, revoke(C, ?ORG_A, AppA, GrantId, 1, ?OWNER_A)),

    %% 下一请求即失败：同一 Authorization 头，重跑两次都必须是 insufficient_scope
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_auth:decide(
            <<"POST">>, <<"/api/internal/v1/groups">>, Headers, auth_fun(C, Fx, a)
        )
    ),
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_auth:decide(
            <<"POST">>, <<"/api/internal/v1/groups">>, Headers, auth_fun(C, Fx, a)
        )
    ),
    {ok, After} = auth_ctx(C, Fx, a),
    ?assertEqual([], maps:get(granted_scopes, After)),
    %% 受管状态与撤销 lineage 保持：不回退到更宽的未受管边界
    ?assertEqual(true, maps:get(grant_governed, After)),
    ?assertEqual({ok, []}, enterprise_application_grant_repo:effective_scopes_tx(C, ?ORG_A, AppA)),
    %% 撤销后 workspace 边界同样全拒（scope 已不在生效集）
    ?assertEqual(
        {error, insufficient_scope}, require_workspace(C, After, ?WS_A1, <<"groups:write">>)
    ),
    %% 已撤销是终态：任何版本号再撤都返回 already_revoked（不再有版本语义）
    ?assertEqual(
        {error, already_revoked}, revoke(C, ?ORG_A, AppA, GrantId, 1, ?OWNER_A)
    ),
    ?assertEqual(
        {error, already_revoked}, revoke(C, ?ORG_A, AppA, GrantId, 2, ?OWNER_A)
    ),
    %% version_conflict：对仍 active 的 Grant 用错版本号（并发撤销不丢更新）
    {ok, Grant2} = issue(C, ?ORG_A, AppA, <<"k-version">>, [<<"groups:write">>]),
    GrantId2 = maps:get(<<"id">>, Grant2),
    ?assertEqual(
        {error, version_conflict}, revoke(C, ?ORG_A, AppA, GrantId2, 7, ?OWNER_A)
    ),
    ?assertEqual(ok, revoke(C, ?ORG_A, AppA, GrantId2, 1, ?OWNER_A)),
    %% 跨 Org / 不存在：一律 not_found（不给 oracle）
    ?assertEqual(
        {error, not_found}, revoke(C, ?ORG_B, AppA, GrantId, 2, ?OWNER_B)
    ),
    ?assertEqual(
        {error, not_found}, revoke(C, ?ORG_A, AppA, 999999999, 1, ?OWNER_A)
    ).

%%%===================================================================
%%% ⑦ scope downgrade ⇒ 下一请求即失败
%%%===================================================================

scope_downgrade_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    {ok, Grant} =
        issue(C, ?ORG_A, AppA, <<"k-downgrade">>, [<<"groups:write">>, <<"identities:write">>]),
    GrantId = maps:get(<<"id">>, Grant),

    ?assertMatch(
        {ok, #{route_id := <<"INT-04">>}},
        decide(C, Fx, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ),
    ?assertMatch(
        {ok, #{route_id := <<"INT-02">>}},
        decide(C, Fx, a, <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, [?IDEM])
    ),

    %% 降级：移除 identities:write（CAS v1 → v2）
    ?assertEqual(
        ok,
        enterprise_internal_ops:set_grant_scopes_tx(
            C, ?ORG_A, AppA, GrantId, 1, [<<"groups:write">>]
        )
    ),
    {ok, Ctx} = auth_ctx(C, Fx, a),
    ?assertEqual([<<"groups:write">>], maps:get(granted_scopes, Ctx)),
    %% 被移除的 scope 下一请求即失败；未被移除的仍然放行（定向收窄，不是一刀切）
    ?assertEqual(
        {error, insufficient_scope},
        decide(C, Fx, a, <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, [?IDEM])
    ),
    ?assertMatch(
        {ok, #{route_id := <<"INT-04">>}},
        decide(C, Fx, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ),
    ?assertEqual(
        {ok, [<<"groups:write">>]},
        enterprise_application_grant_repo:effective_scopes_tx(C, ?ORG_A, AppA)
    ),

    %% CAS 与输入校验
    ?assertEqual(
        {error, version_conflict},
        enterprise_internal_ops:set_grant_scopes_tx(
            C, ?ORG_A, AppA, GrantId, 1, [<<"groups:write">>]
        )
    ),
    ?assertEqual(
        {error, empty_scopes},
        enterprise_internal_ops:set_grant_scopes_tx(C, ?ORG_A, AppA, GrantId, 2, [])
    ),
    ?assertEqual(
        {error, invalid_scope},
        enterprise_internal_ops:set_grant_scopes_tx(
            C, ?ORG_A, AppA, GrantId, 2, [<<"groups:*">>]
        )
    ).

%%%===================================================================
%%% ⑧ 有效期窗口
%%%===================================================================

validity_window_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    %% 未生效
    {ok, _} =
        repo_create(C, ?ORG_A, AppA, #{
            scopes => [<<"groups:write">>],
            idempotency_key => <<"k-future">>,
            valid_from => <<"2099-01-01T00:00:00+00:00">>,
            expires_at => <<"2099-06-30T00:00:00+00:00">>
        }),
    ?assertEqual(
        {ok, #{grant_governed => true, effective_scopes => []}},
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppA, ?SCOPES_A)
    ),
    ?assertEqual(
        {error, insufficient_scope},
        decide(C, Fx, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ),
    %% 已过期
    {ok, _} =
        repo_create(C, ?ORG_A, AppA, #{
            scopes => [<<"groups:write">>],
            idempotency_key => <<"k-expired">>,
            valid_from => <<"2020-01-01T00:00:00+00:00">>,
            expires_at => <<"2020-12-31T00:00:00+00:00">>
        }),
    ?assertEqual(
        {ok, #{grant_governed => true, effective_scopes => []}},
        enterprise_application_grant_logic:context_tx(C, ?ORG_A, AppA, ?SCOPES_A)
    ),
    %% 生效窗口内的 Grant 恢复放行（同一应用、同一请求）
    {ok, _} = issue(C, ?ORG_A, AppA, <<"k-live">>, [<<"groups:write">>]),
    ?assertMatch(
        {ok, #{route_id := <<"INT-04">>}},
        decide(C, Fx, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ).

%%%===================================================================
%%% ⑨ 与广州期既有认证链组合（不建第二套 middleware）
%%%===================================================================

chain_composition_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    CredA = maps:get(cred_a, Fx),
    {ok, _} = issue(C, ?ORG_A, AppA, <<"k-chain">>, [<<"groups:write">>]),
    Body = fun() ->
        enterprise_internal_auth:decide(
            <<"POST">>,
            <<"/api/internal/v1/groups">>,
            headers(Fx, a, [?IDEM]),
            auth_fun(C, Fx, a)
        )
    end,
    ?assertMatch({ok, _}, Body()),

    %% credential 错 secret / 未知 prefix：先于 Grant 判定失败
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:decide(
            <<"POST">>,
            <<"/api/internal/v1/groups">>,
            #{<<"authorization">> => <<"Bearer ", (maps:get(prefix_a, Fx))/binary, ".wrong">>},
            fun() ->
                enterprise_internal_auth:authenticate_tx(
                    C, maps:get(prefix_a, Fx), <<"wrong-secret">>
                )
            end
        )
    ),
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:decide(
            <<"POST">>,
            <<"/api/internal/v1/groups">>,
            #{<<"authorization">> => <<"Bearer ib_int_999999999.x">>},
            fun() ->
                enterprise_internal_auth:authenticate_tx(C, <<"ib_int_999999999">>, <<"x">>)
            end
        )
    ),

    %% credential 撤销：仍是 invalid_credential（Grant 完好也不放行）
    ok = enterprise_internal_ops:revoke_credential_tx(C, ?ORG_A, CredA),
    ?assertEqual({error, invalid_credential}, Body()),
    {ok, #{credential := NewFull}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppA, ?SECRET_A, undefined),
    Fx2 = Fx#{prefix_a => prefix_of(NewFull)},
    ?assertMatch({ok, _}, decide(C, Fx2, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])),

    %% application disabled：先于 Grant 判定失败
    ok = enterprise_internal_ops:set_application_status_tx(C, ?ORG_A, AppA, <<"disabled">>),
    ?assertEqual(
        {error, application_disabled},
        decide(C, Fx2, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ),
    ok = enterprise_internal_ops:set_application_status_tx(C, ?ORG_A, AppA, <<"active">>),
    ?assertMatch({ok, _}, decide(C, Fx2, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])),

    %% organization archived：先于 Grant 判定失败
    ok = exec(C, [
        <<"UPDATE organization SET status = 'archived' WHERE id = ">>,
        integer_to_binary(?ORG_A)
    ]),
    ?assertEqual(
        {error, organization_disabled},
        decide(C, Fx2, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])
    ),
    ok = exec(C, [
        <<"UPDATE organization SET status = 'active' WHERE id = ">>,
        integer_to_binary(?ORG_A)
    ]),
    %% 恢复后同一请求再次放行（证明前面几次失败来自各前置门，而非 Grant 被破坏）
    ?assertMatch({ok, _}, decide(C, Fx2, a, <<"POST">>, <<"/api/internal/v1/groups">>, [?IDEM])).

%%%===================================================================
%%% ⑩ 治理接口校验与 CAS
%%%===================================================================

governance_ops_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    AppB = maps:get(app_b, Fx),
    %% 缺幂等键 / 非法 scope / 空 scope
    ?assertEqual(
        {error, missing_idempotency_key},
        enterprise_internal_ops:issue_grant_tx(C, ?ORG_A, AppA, #{
            scopes => [<<"groups:write">>], expires_at => ?FAR_FUTURE
        })
    ),
    ?assertEqual(
        {error, empty_scopes},
        enterprise_internal_ops:issue_grant_tx(C, ?ORG_A, AppA, #{
            scopes => [], idempotency_key => <<"k-empty">>, expires_at => ?FAR_FUTURE
        })
    ),
    ?assertEqual(
        {error, invalid_scope},
        in_savepoint(C, fun() -> issue(C, ?ORG_A, AppA, <<"k-invalid">>, [<<"*">>]) end)
    ),
    %% 跨 Org Application / workspace：not_found（不给 oracle）
    ?assertEqual(
        {error, application_not_found},
        in_savepoint(C, fun() ->
            issue(C, ?ORG_B, AppA, <<"k-cross-app">>, [<<"groups:write">>])
        end)
    ),
    ?assertEqual(
        {error, application_not_found},
        in_savepoint(C, fun() ->
            issue(C, ?ORG_A, 999999999, <<"k-missing-app">>, [<<"groups:write">>])
        end)
    ),
    ?assertEqual(
        {error, workspace_not_found},
        in_savepoint(C, fun() ->
            issue_ws(C, ?ORG_A, AppA, <<"k-cross-ws">>, [<<"files:write">>], [?WS_B1])
        end)
    ),

    %% 合法签发 → 重复幂等键 key_conflict
    {ok, Grant} = issue(C, ?ORG_A, AppA, <<"k-gov">>, [<<"groups:write">>]),
    GrantId = maps:get(<<"id">>, Grant),
    ?assertEqual(
        {error, key_conflict},
        in_savepoint(C, fun() ->
            issue(C, ?ORG_A, AppA, <<"k-gov">>, [<<"groups:write">>])
        end)
    ),

    %% workspace 边界编辑：none → explicit → none（CAS 每次 +1）
    ?assertEqual(
        ok,
        enterprise_internal_ops:set_grant_workspaces_tx(
            C, ?ORG_A, AppA, GrantId, 1, explicit, [?WS_A1, ?WS_A2]
        )
    ),
    ?assertEqual(
        {ok, true},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_A, AppA, ?WS_A2, <<"groups:write">>
        )
    ),
    ?assertEqual(
        ok,
        enterprise_internal_ops:set_grant_workspaces_tx(C, ?ORG_A, AppA, GrantId, 2, none, [])
    ),
    %% 切回 org 全域：本 Org 全部 workspace 覆盖（跨 Org 仍为 false）
    ?assertEqual(
        {ok, true},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_A, AppA, ?WS_A2, <<"groups:write">>
        )
    ),
    ?assertEqual(
        {ok, false},
        enterprise_application_grant_repo:workspace_covered_tx(
            C, ?ORG_A, AppA, ?WS_B2, <<"groups:write">>
        )
    ),
    ?assertEqual(
        {error, invalid_workspaces},
        enterprise_internal_ops:set_grant_workspaces_tx(
            C, ?ORG_A, AppA, GrantId, 3, explicit, []
        )
    ),
    ?assertEqual(
        {error, version_conflict},
        enterprise_internal_ops:set_grant_workspaces_tx(
            C, ?ORG_A, AppA, GrantId, 1, none, []
        )
    ),

    %% 治理状态读面：受管标记 + 生效 scope + effective 标记
    {ok, Status} = enterprise_internal_ops:grant_status_tx(C, ?ORG_A, AppA),
    ?assertEqual(true, maps:get(grant_governed, Status)),
    ?assertEqual([<<"groups:write">>], maps:get(effective_scopes, Status)),
    ?assertEqual(
        [GrantId], [maps:get(grant_id, G) || G <- maps:get(grants, Status)]
    ),
    ?assertEqual(
        [GrantId],
        [maps:get(grant_id, G) || G <- maps:get(grants, Status), maps:get(effective, G)]
    ),
    %% App B（同 Org 之外）：治理状态互不可见
    {ok, StatusB} = enterprise_internal_ops:grant_status_tx(C, ?ORG_B, AppB),
    ?assertEqual(false, maps:get(grant_governed, StatusB)),
    ?assertEqual([], maps:get(grants, StatusB)).

%%%===================================================================
%%% 夹具与辅助
%%%===================================================================

%% 双 Org + 三 Application（A 全权 / B 独立 Org / C 仅 groups:write）+ 四 workspace
seed_fixture(C) ->
    ok = seed_user(C, ?OWNER_A, <<"t997_owner_a">>),
    ok = seed_user(C, ?OWNER_B, <<"t997_owner_b">>),
    ok = seed_org(C, ?ORG_A, <<"t997_org_a">>, ?OWNER_A),
    ok = seed_org(C, ?ORG_B, <<"t997_org_b">>, ?OWNER_B),
    {ok, AppA} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"t997-app-a">>, <<"a"/utf8>>, ?SCOPES_A
    ),
    {ok, AppB} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_B, <<"t997-app-b">>, <<"b"/utf8>>, ?SCOPES_A
    ),
    {ok, AppC} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"t997-app-c">>, <<"c"/utf8>>, [<<"groups:write">>]
    ),
    AppAId = maps:get(<<"id">>, AppA),
    AppBId = maps:get(<<"id">>, AppB),
    AppCId = maps:get(<<"id">>, AppC),
    {ok, #{credential_id := CredA, credential := FullA}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppAId, ?SECRET_A, undefined),
    {ok, #{credential := FullB}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_B, AppBId, ?SECRET_B, undefined),
    {ok, #{credential := FullC}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppCId, ?SECRET_C, undefined),
    ok = seed_ws(C, ?WS_A1, <<"t997-ws-a1">>, ?ORG_A),
    ok = seed_ws(C, ?WS_A2, <<"t997-ws-a2">>, ?ORG_A),
    ok = seed_ws(C, ?WS_B1, <<"t997-ws-b1">>, ?ORG_B),
    ok = seed_ws(C, ?WS_B2, <<"t997-ws-b2">>, ?ORG_B),
    #{
        app_a => AppAId,
        app_b => AppBId,
        app_c => AppCId,
        cred_a => CredA,
        prefix_a => prefix_of(FullA),
        secret_a => ?SECRET_A,
        prefix_b => prefix_of(FullB),
        secret_b => ?SECRET_B,
        prefix_c => prefix_of(FullC),
        secret_c => ?SECRET_C
    }.

seed_user(C, Uid, Account) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        <<", 'x', '">>,
        Account,
        <<"', '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, Name, OwnerUid) ->
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings,">>,
        <<" created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        <<", '">>,
        Name,
        <<"', ">>,
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_ws(C, WsId, Name, OrgId) ->
    exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id) VALUES (">>,
        integer_to_binary(WsId),
        <<", '">>,
        Name,
        <<"', ">>,
        integer_to_binary(?OWNER_A),
        <<", 'active', ">>,
        integer_to_binary(OrgId),
        <<")">>
    ]).

%% ---- 授权治理（经 ops 稳定 API） ----

issue(C, OrgId, AppId, IdemKey, Scopes) ->
    enterprise_internal_ops:issue_grant_tx(C, OrgId, AppId, #{
        scopes => Scopes,
        idempotency_key => IdemKey,
        expires_at => ?FAR_FUTURE
    }).

issue_ws(C, OrgId, AppId, IdemKey, Scopes, Workspaces) ->
    enterprise_internal_ops:issue_grant_tx(C, OrgId, AppId, #{
        scopes => Scopes,
        workspace_scope_kind => explicit,
        workspace_ids => Workspaces,
        idempotency_key => IdemKey,
        expires_at => ?FAR_FUTURE
    }).

repo_create(C, OrgId, AppId, Spec) ->
    enterprise_application_grant_repo:create_tx(C, OrgId, AppId, Spec).

revoke(C, OrgId, AppId, GrantId, ExpectedVersion, RevokedBy) ->
    enterprise_internal_ops:revoke_grant_tx(C, OrgId, AppId, GrantId, ExpectedVersion, RevokedBy).

%% ---- 认证链与授权求值（真链路，不 mock DB） ----

auth_ctx(C, Fx, Who) ->
    enterprise_internal_auth:authenticate_tx(
        C, maps:get(prefix(Who), Fx), maps:get(secret(Who), Fx)
    ).

prefix(a) -> prefix_a;
prefix(b) -> prefix_b;
prefix(c) -> prefix_c.

secret(a) -> secret_a;
secret(b) -> secret_b;
secret(c) -> secret_c.

auth_fun(C, Fx, Who) ->
    fun() -> auth_ctx(C, Fx, Who) end.

headers(Fx, Who, Extra) ->
    Full = <<(maps:get(prefix(Who), Fx))/binary, ".", (maps:get(secret(Who), Fx))/binary>>,
    maps:from_list([{<<"authorization">>, <<"Bearer ", Full/binary>>} | Extra]).

decide(C, Fx, Who, Method, Path, Extra) ->
    enterprise_internal_auth:decide(Method, Path, headers(Fx, Who, Extra), auth_fun(C, Fx, Who)).

require_workspace(C, Ctx, WorkspaceId, Scope) ->
    enterprise_application_grant_logic:require_workspace_tx(C, Ctx, WorkspaceId, Scope).

app_path() -> <<"/api/internal/v1/application">>.

prefix_of(Full) ->
    case binary:split(Full, <<".">>) of
        [Prefix, _Secret] -> Prefix
    end.
