%% @doc AG31-05：中央工具授权器隔离 PG 验收测试（agent_effect 决策 CAS 写入 /
%% 决策持久化 / 版本冲突 / R1 身份事实 / R6 审批摄取真链）。
%%
%% == 运行前置 ==
%%
%% 隔离一次性 PG（容器 imboy-ag31-05-pg18、端口 4402，非 4323 共享库、非生产），
%% 库内已应用全链迁移到 00000133。连接参数只经进程环境变量注入（不写入任何
%% 文件/日志）：
%%
%%   AG31_PG_HOST / AG31_PG_PORT / AG31_PG_DB / AG31_PG_USER / AG31_PG_PASSWORD(可空)
%%
%% == 铁律 ==
%%
%% 环境变量缺失或连不上库 → **显式 FAIL**（erlang:error），禁止静默 skip/pass。
%% 测试夹具自建自清（固定高位 id 区段 992000+，与 990000/991000 区段不相交，
%% 不 TRUNCATE 任何共享表）；清理走 session_replication_role=replica 一次性
%% 旁路，按子表→父表顺序显式删除。env seam（membership/catalog/policy/
%% dispatcher）经 meck 假模块注入（non_strict 可 mock 不存在的模块名），
%% 与生产默认绑定零接触。
-module(agent_tool_authorizer_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ID_ORG, 992001).
-define(ID_DELEG, 992010).
-define(ID_AGENT, 992011).
-define(ID_HUMAN, 992012).
-define(ID_WS, 992021).
-define(ID_GRANT, 992100).
-define(ID_RUN1, 992301).
-define(ID_RUN2, 992302).
-define(ID_RUN3, 992303).
-define(ID_RUN4, 992304).

-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

-define(AUTHORIZER, agent_tool_authorizer).
-define(FAKE_MEM, ag31_05_pg_membership).
-define(FAKE_CAT, ag31_05_pg_catalog).
-define(FAKE_POLICY, ag31_05_pg_policy).
-define(FAKE_DISP, ag31_05_pg_dispatcher).

%% ===================================================================
%% 套件组织
%% ===================================================================

agent_tool_authorizer_pg_test_() ->
    {setup, fun connect_required/0, fun disconnect/1, fun(Conn) ->
        [
            {"a: allow 决策落 agent_effect（authorized + grant_version_checked + dispatcher 1）",
                fun() -> t_allow_persist(Conn) end},
            {"b: Grant 撤销 → deny(grant_revoked) + denied 决策行 + dispatcher 0", fun() ->
                t_revoke_deny(Conn)
            end},
            {"c: write 工具审批全链（E07→批准→重租→全链重走 allow）", fun() ->
                t_approval_chain(Conn)
            end},
            {"d: 陈旧批准拒（args 变更 + 绑定错挂 run）+ denied 决策行", fun() ->
                t_stale_approval(Conn)
            end},
            {"e: duplicate effect（同幂等键）→ deny(duplicate_effect)", fun() ->
                t_duplicate_effect(Conn)
            end},
            {"f: R1 身份事实（status=0 禁用）→ agent_disabled；恢复后放行", fun() ->
                t_agent_disabled(Conn)
            end},
            {"h: MEDIUM-2 守卫事务——run 非 running 时 guarded insert 拒绝落账", fun() ->
                t_guarded_run_terminal(Conn)
            end},
            {"g: 链头 00000133 非 dirty + 夹具残留为零", fun() -> t_chain_head(Conn) end}
        ]
    end}.

connect_required() ->
    Missing = [
        K
     || K <- ["AG31_PG_HOST", "AG31_PG_PORT", "AG31_PG_DB", "AG31_PG_USER"],
        os:getenv(K) =:= false
    ],
    case Missing of
        [] -> ok;
        _ -> erlang:error({ag31_pg_missing_env, Missing})
    end,
    ensure_tsid(),
    inject_env_seams(),
    ConnOpts = #{
        host => os:getenv("AG31_PG_HOST"),
        port => list_to_integer(os:getenv("AG31_PG_PORT")),
        username => os:getenv("AG31_PG_USER"),
        password => os:getenv("AG31_PG_PASSWORD", ""),
        database => os:getenv("AG31_PG_DB")
    },
    case epgsql:connect(ConnOpts) of
        {ok, Conn} ->
            fixture_reset(Conn),
            Conn;
        {error, Reason} ->
            erlang:error({ag31_pg_connect_failed, Reason})
    end.

%% eunit-local 环境无 imboy app 时 elib_tsid 需要节点标识（04B 同口径）。
ensure_tsid() ->
    try elib_tsid:generate(default) of
        _ -> ok
    catch
        _:_ ->
            ok = elib_tsid:init(#{dc_id => 0, node_id => 7})
    end.

%% env seam 注入（与生产默认零接触；teardown 统一卸载）。
inject_env_seams() ->
    meck:new(?FAKE_MEM, [no_link, non_strict]),
    meck:expect(?FAKE_MEM, resolve_organization_state, fun(_O) ->
        {ok, #{status => active, version => 5}}
    end),
    meck:expect(?FAKE_MEM, resolve_organization_membership, fun(_O, _A) ->
        {ok, #{status => active, role => member, version => 7}}
    end),
    meck:expect(?FAKE_MEM, resolve_workspace_membership, fun(_O, _W, _A) ->
        {ok, #{status => active, role => member, version => 9}}
    end),
    meck:new(?FAKE_CAT, [no_link, non_strict]),
    meck:expect(?FAKE_CAT, lookup, fun(C, A, R) ->
        {ok, #{
            capability => C,
            action => A,
            resource_type => R,
            legal_constraint_keys => [<<"workspace_ids">>, <<"resource_id">>]
        }}
    end),
    meck:new(?FAKE_POLICY, [no_link, non_strict]),
    meck:expect(?FAKE_POLICY, evaluate, fun(_R, _T, _Res) -> allow end),
    meck:new(?FAKE_DISP, [no_link, non_strict]),
    meck:expect(?FAKE_DISP, dispatch, fun(_Input) ->
        put(ag31_05_pg_disp, disp_count() + 1),
        {ok, dispatched}
    end),
    ok = application:set_env(imboy, agent_membership_module, ?FAKE_MEM),
    ok = application:set_env(imboy, agent_capability_catalog_module, ?FAKE_CAT),
    ok = application:set_env(imboy, agent_resource_policy_module, ?FAKE_POLICY),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ?FAKE_DISP),
    ok.

disconnect(Conn) ->
    try epgsql:close(Conn) of
        _ -> ok
    catch
        _:_ -> ok
    end,
    try
        meck:unload(?FAKE_MEM),
        meck:unload(?FAKE_CAT),
        meck:unload(?FAKE_POLICY),
        meck:unload(?FAKE_DISP)
    catch
        _:_ -> ok
    end,
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [
            agent_membership_module,
            agent_capability_catalog_module,
            agent_resource_policy_module,
            agent_hitl_policy_module,
            agent_tool_dispatcher_module
        ]
    ),
    ok.

disp_count() ->
    case get(ag31_05_pg_disp) of
        undefined -> 0;
        N -> N
    end.

reset_disp() ->
    erase(ag31_05_pg_disp).

%% ===================================================================
%% a: allow 决策持久化
%% ===================================================================

t_allow_persist(Conn) ->
    start_running_run(Conn, ?ID_RUN1, <<"ag31-05-pg-a">>),
    reset_disp(),
    {allow, DC} = authorize(Conn, ?ID_RUN1, readonly_tool(), resource()),
    ?assert(is_integer(maps:get(effect_id, DC))),
    ?assertEqual(1, maps:get(grant_version, DC)),
    %% dispatcher 恰一次（R5：allow 路径）
    ?assertEqual(1, disp_count()),
    %% agent_effect 行：authorized + grant_version_checked + digests 只存摘要
    {ok, _, [{Status, Reason, Gv, ArgsDigest, ResourceDigest, Seq}]} = epgsql:equery(
        Conn,
        "SELECT status, authorization_reason, grant_version_checked, args_digest, "
        "resource_digest, sequence FROM agent_effect WHERE id = $1",
        [maps:get(effect_id, DC)]
    ),
    ?assertEqual({<<"authorized">>, <<"allow">>, 1, <<"sha256:args">>, <<"sha256:res">>}, {
        Status, Reason, Gv, ArgsDigest, ResourceDigest
    }),
    ?assertEqual(1, Seq),
    fixture_reset(Conn).

%% ===================================================================
%% b: Grant 撤销 → 实时重检 deny + 决策行
%% ===================================================================

t_revoke_deny(Conn) ->
    start_running_run(Conn, ?ID_RUN1, <<"ag31-05-pg-b">>),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET status = 'revoked', revoked_at = now(), "
        "revoked_by_user_id = $2, version = version + 1 "
        "WHERE id = $1 AND status = 'active' AND version = 1",
        [?ID_GRANT, ?ID_DELEG]
    ),
    reset_disp(),
    ?assertEqual({deny, grant_revoked}, authorize(Conn, ?ID_RUN1, readonly_tool(), resource())),
    ?assertEqual(0, disp_count()),
    {ok, _, [{Denied, Reason}]} = epgsql:equery(
        Conn,
        "SELECT count(*), min(authorization_reason) FROM agent_effect "
        "WHERE run_id = $1 AND status = 'denied'",
        [?ID_RUN1]
    ),
    ?assertEqual(1, Denied),
    ?assertEqual(<<"grant_revoked">>, Reason),
    fixture_reset(Conn).

%% ===================================================================
%% c: 审批全链——write 工具 → E07 → 批准 → E12 → 重租 → 全链重走 allow
%% ===================================================================

t_approval_chain(Conn) ->
    start_running_run(Conn, ?ID_RUN1, <<"ag31-05-pg-c">>),
    reset_disp(),
    {approval_required, AC} = authorize(Conn, ?ID_RUN1, write_tool(), resource()),
    EffectId = maps:get(effect_id, AC),
    %% effect waiting_approval + Run E07 waiting_approval 同事务落账
    {ok, Run} = agent_run_pg:get_run(Conn, ?ID_RUN1),
    ?assertEqual(waiting_approval, maps:get(status, Run)),
    {ok, Eff} = agent_run_pg:get_effect(Conn, EffectId),
    ?assertEqual(waiting_approval, maps:get(status, Eff)),
    ?assertEqual(<<"approval_required">>, maps:get(authorization_reason, Eff)),
    ?assertEqual(0, disp_count()),
    %% 人工批准（真 agent_run_command 链：绑定 digest + Grant 实时重检）
    {ok, #{effect_version := _EV, run_version := _RV}} =
        ?AUTHORIZER:approve_effect(Conn, ?ID_RUN1, EffectId, #{
            args_digest => <<"sha256:args">>,
            approval_ref => <<"appr-pg-1">>,
            actor_id => <<"human-1">>,
            now => now0()
        }),
    {ok, EffAuth} = agent_run_pg:get_effect(Conn, EffectId),
    ?assertEqual(authorized, maps:get(status, EffAuth)),
    ?assertEqual(<<"appr-pg-1">>, maps:get(approval_ref, EffAuth)),
    ?assertEqual(1, maps:get(grant_version_checked, EffAuth)),
    {ok, RunQ} = agent_run_pg:get_run(Conn, ?ID_RUN1),
    ?assertEqual(queued, maps:get(status, RunQ)),
    %% 重新获取租约（E04）→ running
    {ok, _} = agent_run_command:acquire_lease(Conn, ?ID_RUN1, <<"w1">>, 60, #{now => now0()}),
    %% dispatch 前再次授权 = authorize/3 全链重走（携带批准绑定）
    {allow, _} =
        authorize(Conn, ?ID_RUN1, #{approval_effect_id => EffectId}, write_tool(), resource()),
    %% 全链重走产出第二条 effect（authorized）+ dispatcher 恰一次
    {ok, _, [{Authorized2}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM agent_effect WHERE run_id = $1 AND status = 'authorized' "
        "AND id <> $2",
        [?ID_RUN1, EffectId]
    ),
    ?assertEqual(1, Authorized2),
    ?assertEqual(1, disp_count()),
    fixture_reset(Conn).

%% ===================================================================
%% d: 陈旧批准拒（args 变更 / 绑定错挂 run）+ denied 决策行
%% ===================================================================

t_stale_approval(Conn) ->
    start_running_run(Conn, ?ID_RUN1, <<"ag31-05-pg-d">>),
    %% eunit 组内同进程 inorder：pd 计数器须先清零（a/b/c/e 同惯例），
    %% 否则继承上一测试残留计数，dispatcher=0 断言失真。
    reset_disp(),
    {approval_required, AC} = authorize(Conn, ?ID_RUN1, write_tool(), resource()),
    EffectId = maps:get(effect_id, AC),
    %% 批准：digest 不匹配拒绝，effect 保持 waiting_approval
    {error, approval_digest_mismatch} =
        ?AUTHORIZER:approve_effect(Conn, ?ID_RUN1, EffectId, #{
            args_digest => <<"sha256:args-WRONG">>,
            approval_ref => <<"appr-pg-2">>,
            actor_id => <<"human-1">>,
            now => now0()
        }),
    {ok, EffStill} = agent_run_pg:get_effect(Conn, EffectId),
    ?assertEqual(waiting_approval, maps:get(status, EffStill)),
    ?assertEqual(0, disp_count()),
    %% effect 错挂其他 Run → 四元组绑定拒（run_id+effect_id）
    {error, approval_run_mismatch} =
        ?AUTHORIZER:approve_effect(Conn, 999999, EffectId, #{
            args_digest => <<"sha256:args">>,
            approval_ref => <<"appr-pg-3">>,
            actor_id => <<"human-1">>,
            now => now0()
        }),
    %% Run 仍 waiting_approval（批准被拒未回租）→ 重走在步骤 1 即拒：
    %% 冻结十步序 step1（run 非 running）先于 step8（批准绑定校验）。
    ?assertEqual(
        {deny, run_not_running},
        authorize(Conn, ?ID_RUN1, #{approval_effect_id => EffectId}, write_tool(), resource())
    ),
    ?assertEqual(0, disp_count()),
    %% 规范流补全：正确 digest 批准（E12→queued）→ 重租（E04→running），
    %% 重走方可在步骤 8 触达 stale 批准绑定校验（与 c 全链/E12+E04 同路径）。
    {ok, _} =
        ?AUTHORIZER:approve_effect(Conn, ?ID_RUN1, EffectId, #{
            args_digest => <<"sha256:args">>,
            approval_ref => <<"appr-pg-4">>,
            actor_id => <<"human-1">>,
            now => now0()
        }),
    {ok, _} = agent_run_command:acquire_lease(Conn, ?ID_RUN1, <<"w1">>, 60, #{now => now0()}),
    %% 参数变更后全链重走 → stale_approval_args + denied 决策行
    ResMutated = resource(#{args_digest => <<"sha256:args-NEW">>}),
    {deny, stale_approval_args} =
        authorize(Conn, ?ID_RUN1, #{approval_effect_id => EffectId}, write_tool(), ResMutated),
    {ok, _, [{StaleDenied}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM agent_effect WHERE run_id = $1 AND status = 'denied' "
        "AND authorization_reason = 'stale_approval_args'",
        [?ID_RUN1]
    ),
    ?assertEqual(1, StaleDenied),
    ?assertEqual(0, disp_count()),
    fixture_reset(Conn).

%% ===================================================================
%% e: duplicate effect（UNIQUE(tool_id, external_idempotency_key)）
%% ===================================================================

t_duplicate_effect(Conn) ->
    start_running_run(Conn, ?ID_RUN1, <<"ag31-05-pg-e">>),
    reset_disp(),
    {allow, _} =
        authorize(
            Conn,
            ?ID_RUN1,
            readonly_tool(),
            resource(#{external_idempotency_key => <<"ext-key-e">>})
        ),
    ?assertEqual(
        {deny, duplicate_effect},
        authorize(
            Conn,
            ?ID_RUN1,
            readonly_tool(),
            resource(#{external_idempotency_key => <<"ext-key-e">>})
        )
    ),
    ?assertEqual(1, disp_count()),
    fixture_reset(Conn).

%% ===================================================================
%% f: R1 身份事实（user.status 显式列加核：0=禁用 → agent_disabled）
%% ===================================================================

t_agent_disabled(Conn) ->
    start_running_run(Conn, ?ID_RUN1, <<"ag31-05-pg-f">>),
    {ok, 1} =
        epgsql:equery(Conn, "UPDATE \"user\" SET status = 0 WHERE id = $1", [?ID_AGENT]),
    ?assertEqual({deny, agent_disabled}, authorize(Conn, ?ID_RUN1, readonly_tool(), resource())),
    {ok, 1} =
        epgsql:equery(Conn, "UPDATE \"user\" SET status = 1 WHERE id = $1", [?ID_AGENT]),
    {allow, _} = authorize(Conn, ?ID_RUN1, readonly_tool(), resource()),
    fixture_reset(Conn).

%% ===================================================================
%% ===================================================================
%% h: MEDIUM-2 守卫事务（A2 review）——run 终态下 guarded insert 拒绝落账
%% ===================================================================

t_guarded_run_terminal(Conn) ->
    start_running_run(Conn, ?ID_RUN1, <<"ag31-05-pg-h">>),
    %% running → cancelled（E10 边，默认 human actor；acquire_lease 后 version=3）
    {ok, _} = agent_run_command:transition(
        Conn,
        ?ID_RUN1,
        running,
        cancelled,
        #{expected_version => 3, now => now0(), actor_id => <<"human-1">>}
    ),
    EffectId = agent_run_pg:next_id(agent_effect),
    Effect = #{
        id => EffectId,
        run_id => ?ID_RUN1,
        sequence => agent_run_pg:next_effect_sequence(Conn, ?ID_RUN1),
        tool_id => <<"tool.demo.v1">>,
        capability => <<"demo.write">>,
        action => <<"create">>,
        resource_digest => <<"sha256:res">>,
        args_digest => <<"sha256:args">>,
        external_idempotency_key => <<"ext-key-h">>,
        now => now0(),
        decided_status => authorized
    },
    %% 守卫事务在 run 行锁上观测到终态 → 拒绝（§14 deny all new effects）
    ?assertEqual(
        {error, {run_not_active, <<"cancelled">>}},
        agent_run_pg:insert_effect_guarded_tx(Conn, Effect)
    ),
    {ok, _, [{None}]} = epgsql:equery(
        Conn, "SELECT count(*) FROM agent_effect WHERE id = $1", [EffectId]
    ),
    ?assertEqual(0, None),
    fixture_reset(Conn).

%% g: 链头 + 残留
%% ===================================================================

t_chain_head(Conn) ->
    {ok, _, [{Version, Dirty}]} = epgsql:equery(
        Conn, "SELECT version, dirty FROM schema_migrations", []
    ),
    ?assertEqual(133, Version),
    ?assertEqual(false, Dirty),
    fixture_reset(Conn),
    {ok, _, [{Residue}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM agent_effect e JOIN agent_run r ON r.id = e.run_id "
        "WHERE r.agent_id = $1",
        [?ID_AGENT]
    ),
    ?assertEqual(0, Residue).

%% ===================================================================
%% 授权调用 fixtures
%% ===================================================================

authorize(Conn, RunId, Tool, Resource) ->
    authorize(Conn, RunId, #{}, Tool, Resource).

authorize(Conn, RunId, CtxOver, Tool, Resource) ->
    RunCtx = maps:merge(
        #{
            run_id => RunId,
            agent_id => ?ID_AGENT,
            organization_id => ?ID_ORG,
            now => ?NOW,
            conn => Conn
        },
        CtxOver
    ),
    ?AUTHORIZER:authorize(RunCtx, Tool, Resource).
readonly_tool() ->
    #{
        tool_id => <<"tool.demo.v1">>,
        capability => <<"demo.write">>,
        action => <<"create">>,
        risk_level => low,
        side_effect_class => readonly
    }.

write_tool() ->
    #{
        tool_id => <<"tool.demo.v1">>,
        capability => <<"demo.write">>,
        action => <<"create">>,
        risk_level => medium,
        side_effect_class => write
    }.

resource() -> resource(#{}).

resource(Over) ->
    maps:merge(
        #{
            organization_id => ?ID_ORG,
            workspace_id => undefined,
            resource_type => <<"demo">>,
            resource_digest => <<"sha256:res">>,
            args_digest => <<"sha256:args">>
        },
        Over
    ).

%% ===================================================================
%% DB 夹具（自建自清；replica 旁路仅用于测试清理）
%% ===================================================================

fixture_reset(Conn) ->
    fixture_cleanup(Conn),
    fixture_insert(Conn).

fixture_insert(Conn) ->
    {ok, 3} = epgsql:equery(
        Conn,
        "INSERT INTO \"user\" (id, account, password, reg_ip, reg_cosv, status) "
        "VALUES ($1,'ag31_05_pg_delegator','x','127.0.0.1','e2e',1), "
        "($2,'ag31_05_pg_agent','x','127.0.0.1','e2e',1), "
        "($3,'ag31_05_pg_human','x','127.0.0.1','e2e',1)",
        [?ID_DELEG, ?ID_AGENT, ?ID_HUMAN]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE \"user\" SET account_type = 1 WHERE id = $1",
        [?ID_AGENT]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO organization (id, name, owner_id) VALUES ($1,'ag31-05-pg-org',$2)",
        [?ID_ORG, ?ID_DELEG]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO workspace (id, name, owner_id, organization_id) "
        "VALUES ($1,'ag31-05-pg-ws',$2,$3)",
        [?ID_WS, ?ID_DELEG, ?ID_ORG]
    ),
    %% 有效窗口相对断言时钟 ?NOW 计算（不得用 DB now()——跨天运行会翻车：
    %% 09-18 重跑时 now()-1h 晚于写死的 ?NOW → grant_pending）
    Vf = shift(?NOW, -3600),
    Exp = shift(?NOW, 86400),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant (id, agent_id, organization_id, delegator_user_id, "
        "workspace_scope_kind, status, valid_from, expires_at, version, idempotency_key) "
        "VALUES ($1,$2,$3,$4,'none','active', $5, $6, 1, 'ag31-05-pg-grant-k1')",
        [?ID_GRANT, ?ID_AGENT, ?ID_ORG, ?ID_DELEG, Vf, Exp]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant_capability (grant_id, capability, action, resource_type, "
        "constraint_json) VALUES ($1,'demo.write','create','demo','{}'::jsonb)",
        [?ID_GRANT]
    ),
    ok.

fixture_cleanup(Conn) ->
    ok = squery_ok(Conn, "SET session_replication_role = replica"),
    lists:foreach(
        fun(S) -> ok = squery_ok(Conn, S) end,
        cleanup_stmts()
    ),
    ok = squery_ok(Conn, "RESET session_replication_role"),
    ok.

cleanup_stmts() ->
    AgentId = integer_to_list(?ID_AGENT),
    [
        "DELETE FROM agent_effect WHERE run_id IN "
        "(SELECT id FROM agent_run WHERE agent_id = " ++ AgentId ++ ")",
        "DELETE FROM agent_run_event WHERE run_id IN "
        "(SELECT id FROM agent_run WHERE agent_id = " ++ AgentId ++ ")",
        "DELETE FROM agent_run WHERE agent_id = " ++ AgentId,
        "DELETE FROM agent_grant_capability WHERE grant_id IN (" ++
            id_list([?ID_GRANT]) ++ ")",
        "DELETE FROM agent_grant_workspace WHERE grant_id IN (" ++
            id_list([?ID_GRANT]) ++ ")",
        "DELETE FROM agent_grant_event WHERE grant_id IN (" ++ id_list([?ID_GRANT]) ++ ")",
        "DELETE FROM agent_grant WHERE id IN (" ++ id_list([?ID_GRANT]) ++ ")",
        "DELETE FROM workspace WHERE id IN (" ++ id_list([?ID_WS]) ++ ")",
        "DELETE FROM organization WHERE id IN (" ++ id_list([?ID_ORG]) ++ ")",
        "DELETE FROM \"user\" WHERE id IN (" ++
            id_list([?ID_DELEG, ?ID_AGENT, ?ID_HUMAN]) ++ ")"
    ].

%% 域命令路径：create → queued → running（04B 同口径）
start_running_run(Conn, RunId, Key) ->
    {ok, _} = agent_run_command:create_run(Conn, run_fixture_ctx(RunId, Key)),
    {ok, _} = agent_run_command:transition(
        Conn,
        RunId,
        created,
        queued,
        #{expected_version => 1, now => now0(), actor_id => <<"system:trigger">>}
    ),
    {ok, _} = agent_run_command:acquire_lease(Conn, RunId, <<"w0">>, 60, #{now => now0()}),
    ok.

run_fixture_ctx(RunId, Key) ->
    #{
        id => RunId,
        agent_id => ?ID_AGENT,
        organization_id => ?ID_ORG,
        workspace_id => undefined,
        grant_id => ?ID_GRANT,
        grant_version_at_start => 1,
        delegating_principal_id => ?ID_DELEG,
        trigger_type => message,
        trigger_id => <<"trigger-", (integer_to_binary(RunId))/binary>>,
        runtime_type => mock,
        context_digest => <<"sha256:ctx-", (integer_to_binary(RunId))/binary>>,
        idempotency_key => Key,
        now => now0()
    }.

now0() ->
    calendar:universal_time().

%% ===================================================================
%% 断言辅助（02B/04B 同口径）
%% ===================================================================

shift(Dt, Seconds) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Dt) + Seconds
    ).

squery_ok(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        Other -> erlang:error({squery_failed, Sql, Other})
    end.

id_list(Ids) ->
    string:join([integer_to_list(I) || I <- Ids], ",").
