%% @doc AG31-04B：agent_run 三表（迁移 00000133，架构合同 §9.3-9.4 Frozen
%% Run/Effect Schema Contract）的隔离 PG 验收测试。
%%
%% == 运行前置 ==
%%
%% 隔离一次性 PG（非 4323 共享库、非生产），库内已应用全链迁移到 00000133。
%% 连接参数只经进程环境变量注入（不写入任何文件/日志）：
%%
%%   AG31_PG_HOST / AG31_PG_PORT / AG31_PG_DB / AG31_PG_USER / AG31_PG_PASSWORD(可空)
%%
%% == 铁律 ==
%%
%% 环境变量缺失或连不上库 → **显式 FAIL**（erlang:error），禁止静默 skip/pass。
%% 测试夹具自建自清（固定高位 id 区段 991000+，与 02B 的 990000+ 区段不相交，
%% 不 TRUNCATE 任何共享表）；清理走 session_replication_role=replica 一次性旁路
%% ——append-only 触发器按合同拒绝一切 DELETE（含夹具行），且该模式下
%% FK CASCADE 同样被绕过，故按子表→父表顺序显式删除。
-module(agent_run_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHAIN_HEAD, 133).
%% 夹具固定 id 区段（991000+，与 agent_grant_pg_tests 的 990000+ 不相交）
-define(ID_DELEGATOR, 991010).
-define(ID_AGENT, 991011).
-define(ID_HUMAN, 991012).
-define(ID_ORG, 991001).
-define(ID_WS, 991020).
-define(ID_GRANT, 991100).
-define(ID_RUN1, 991201).
-define(ID_RUN2, 991202).
-define(ID_RUN3, 991203).
-define(ID_RUN4, 991204).
-define(ID_RUN5, 991205).
-define(ID_RUN6, 991206).
-define(ID_RUN7, 991207).
-define(ID_RUN8, 991208).
-define(ID_RUN9, 991209).

%% ===================================================================
%% 套件组织
%% ===================================================================

agent_run_pg_test_() ->
    {setup, fun connect_required/0, fun disconnect/1, fun(Conn) ->
        [
            {"a: three tables + key constraints/indexes/append-only guard (pg_catalog)", fun() ->
                t_structure(Conn)
            end},
            {"b: A08 duplicate trigger -> same run_id, execution=1", fun() ->
                t_a08_duplicate_trigger(Conn)
            end},
            {"c: A09 duplicate effect idempotency key -> external attempt<=1", fun() ->
                t_a09_duplicate_effect(Conn)
            end},
            {"d: A11 two connections race an expired lease -> exactly one winner", fun() ->
                t_a11_lease_race(Conn)
            end},
            {"e: grant revoked -> new effect authorize denied (real-time recheck)", fun() ->
                t_revoke_race(Conn)
            end},
            {"f: unknown -> no new effect / no cancel / reconcile succeeded|failed", fun() ->
                t_unknown_reconcile(Conn)
            end},
            {"g: CAS conflict & illegal edge rejected with no event row + attempt cap", fun() ->
                t_cas_and_cap(Conn)
            end},
            {"h: HITL approval chain (E07/E12/E13) + digest mismatch + revoke-during-approval",
                fun() -> t_hitl(Conn) end},
            {"i: chain head at 00000133 not dirty + structure re-assert", fun() ->
                t_chain_head(Conn)
            end}
        ]
    end}.

%% 环境变量缺失 → 显式 FAIL（任务铁律：禁止静默 skip/pass）
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
    ConnOpts = #{
        host => os:getenv("AG31_PG_HOST"),
        port => list_to_integer(os:getenv("AG31_PG_PORT")),
        username => os:getenv("AG31_PG_USER"),
        password => os:getenv("AG31_PG_PASSWORD", ""),
        database => os:getenv("AG31_PG_DB")
    },
    case epgsql:connect(ConnOpts) of
        {ok, Conn} -> Conn;
        {error, Reason} -> erlang:error({ag31_pg_connect_failed, Reason})
    end.

%% eunit-local 环境无 imboy app：elib_tsid 命名生成器需要 node 标识（init/1）；
%% 已初始化（全量 app 先行启动）则保持原状不动。
ensure_tsid() ->
    try elib_tsid:generate(default) of
        _ -> ok
    catch
        _:_ ->
            ok = elib_tsid:init(#{dc_id => 0, node_id => 7})
    end.

disconnect(Conn) ->
    try epgsql:close(Conn) of
        _ -> ok
    catch
        _:_ -> ok
    end,
    ok.

%% ===================================================================
%% a: 结构断言 + append-only 守卫 + CHECK 抽样
%% ===================================================================

t_structure(Conn) ->
    fixture_reset(Conn),
    %% 三表存在
    lists:foreach(
        fun(T) ->
            {ok, _, [{Exists}]} = epgsql:equery(
                Conn, "SELECT to_regclass('public.' || $1) IS NOT NULL", [T]
            ),
            ?assertEqual(true, Exists)
        end,
        [<<"agent_run">>, <<"agent_run_event">>, <<"agent_effect">>]
    ),
    %% agent_run 列契约（23 列，类型/可空性逐列）
    {ok, _, Cols} = epgsql:equery(
        Conn,
        "SELECT column_name, data_type, is_nullable FROM information_schema.columns "
        "WHERE table_name = 'agent_run' ORDER BY ordinal_position",
        []
    ),
    ?assertMatch(
        [
            {<<"id">>, <<"bigint">>, <<"NO">>},
            {<<"agent_id">>, <<"bigint">>, <<"NO">>},
            {<<"organization_id">>, <<"bigint">>, <<"NO">>},
            {<<"workspace_id">>, <<"bigint">>, <<"YES">>},
            {<<"grant_id">>, <<"bigint">>, <<"NO">>},
            {<<"grant_version_at_start">>, <<"integer">>, <<"NO">>},
            {<<"delegating_principal_id">>, <<"bigint">>, <<"NO">>},
            {<<"trigger_type">>, <<"text">>, <<"NO">>},
            {<<"trigger_id">>, <<"text">>, <<"NO">>},
            {<<"runtime_type">>, <<"text">>, <<"NO">>},
            {<<"status">>, <<"text">>, <<"NO">>},
            {<<"reason_code">>, <<"text">>, <<"YES">>},
            {<<"version">>, <<"integer">>, <<"NO">>},
            {<<"context_digest">>, <<"text">>, <<"NO">>},
            {<<"idempotency_key">>, <<"text">>, <<"NO">>},
            {<<"lease_owner">>, <<"text">>, <<"YES">>},
            {<<"lease_expires_at">>, <<"timestamp with time zone">>, <<"YES">>},
            {<<"attempt">>, <<"integer">>, <<"NO">>},
            {<<"created_at">>, <<"timestamp with time zone">>, <<"NO">>},
            {<<"queued_at">>, <<"timestamp with time zone">>, <<"YES">>},
            {<<"started_at">>, <<"timestamp with time zone">>, <<"YES">>},
            {<<"finished_at">>, <<"timestamp with time zone">>, <<"YES">>},
            {<<"updated_at">>, <<"timestamp with time zone">>, <<"NO">>}
        ],
        Cols
    ),
    %% agent_run 约束 16 项（PK + UNIQUE×2 + CHECK×8 + FK×5）
    ?assertEqual(
        16,
        count_existing_constraints(Conn, <<"agent_run">>, [
            <<"pk_agent_run">>,
            <<"uq_ar_org_id">>,
            <<"uq_ar_agent_org_trigger_idem">>,
            <<"ck_ar_trigger_type">>,
            <<"ck_ar_status">>,
            <<"ck_ar_terminal_finished">>,
            <<"ck_ar_version">>,
            <<"ck_ar_attempt">>,
            <<"ck_ar_trigger_id">>,
            <<"ck_ar_context_digest">>,
            <<"ck_ar_idempotency_key">>,
            <<"fk_ar_agent">>,
            <<"fk_ar_organization">>,
            <<"fk_ar_workspace">>,
            <<"fk_ar_grant">>,
            <<"fk_ar_delegating_principal">>
        ])
    ),
    %% agent_run_event 约束 6 项（PK + UNIQUE + CHECK×3 + FK）
    ?assertEqual(
        6,
        count_existing_constraints(Conn, <<"agent_run_event">>, [
            <<"pk_agent_run_event">>,
            <<"uq_are_run_idem">>,
            <<"ck_are_from_status">>,
            <<"ck_are_to_status">>,
            <<"ck_are_actor_kind">>,
            <<"fk_are_run">>
        ])
    ),
    %% agent_effect 约束 12 项（PK + UNIQUE×2 + CHECK×8 + FK）
    ?assertEqual(
        12,
        count_existing_constraints(Conn, <<"agent_effect">>, [
            <<"pk_agent_effect">>,
            <<"uq_ae_run_sequence">>,
            <<"uq_ae_tool_external_idem">>,
            <<"ck_ae_status">>,
            <<"ck_ae_sequence">>,
            <<"ck_ae_version">>,
            <<"ck_ae_tool_id">>,
            <<"ck_ae_capability">>,
            <<"ck_ae_action">>,
            <<"ck_ae_resource_digest">>,
            <<"ck_ae_args_digest">>,
            <<"fk_ae_run">>
        ])
    ),
    %% 合同 3 条 INDEX（含 partial index 谓词）
    ?assertEqual(
        3,
        count_existing_indexes(Conn, [
            <<"i_ar_status_lease">>, <<"i_ar_agent_org_created">>, <<"i_ar_grant_status">>
        ])
    ),
    {ok, _, [{PartialDef}]} = epgsql:equery(
        Conn, "SELECT indexdef FROM pg_indexes WHERE indexname = 'i_ar_status_lease'", []
    ),
    ?assertEqual(true, str_contains(PartialDef, <<"WHERE">>)),
    ?assertEqual(true, str_contains(PartialDef, <<"queued">>)),
    ?assertEqual(true, str_contains(PartialDef, <<"running">>)),
    %% append-only 守卫：触发器 + 函数
    {ok, _, [{Trg}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM pg_trigger WHERE tgrelid = 'agent_run_event'::regclass "
        "AND tgname = 'trg_agent_run_event_append_only' AND NOT tgisinternal",
        []
    ),
    ?assertEqual(1, Trg),
    {ok, _, [{Fn}]} = epgsql:equery(
        Conn, "SELECT to_regprocedure('fn_agent_run_event_append_only()') IS NOT NULL", []
    ),
    ?assertEqual(true, Fn),
    %% append-only 行为：UPDATE/DELETE 一律 23514
    {ok, 1} = insert_fixture_run(Conn, ?ID_RUN1, <<"ag31d-a-key">>, <<"created">>),
    {ok, 1, _, [{EventId}]} = epgsql:equery(
        Conn,
        "INSERT INTO agent_run_event (id, run_id, from_status, to_status, actor_kind, "
        "actor_id, detail_json, idempotency_key) "
        "VALUES (991400,$1,NULL,'created','system','fixture','{}'::jsonb,'ag31d-a-ev1') "
        "RETURNING id",
        [?ID_RUN1]
    ),
    {error, ErrUpd} = epgsql:equery(
        Conn,
        "UPDATE agent_run_event SET detail_json = '{\"tampered\":true}'::jsonb WHERE id = $1",
        [EventId]
    ),
    ?assertEqual(<<"23514">>, err_code(ErrUpd)),
    ?assertEqual(<<"trg_agent_run_event_append_only">>, err_constraint(ErrUpd)),
    {error, ErrDel} = epgsql:equery(
        Conn, "DELETE FROM agent_run_event WHERE id = $1", [EventId]
    ),
    ?assertEqual(<<"23514">>, err_code(ErrDel)),
    ?assertEqual(<<"trg_agent_run_event_append_only">>, err_constraint(ErrDel)),
    %% CHECK 抽样（DB 层冻结值域）
    {error, E1} = epgsql:equery(
        Conn,
        "UPDATE agent_run SET status = 'timeout' WHERE id = $1",
        [?ID_RUN1]
    ),
    assert_check(E1, <<"ck_ar_status">>),
    {error, E2} = epgsql:equery(
        Conn,
        "INSERT INTO agent_run (id, agent_id, organization_id, grant_id, "
        "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
        "runtime_type, status, context_digest, idempotency_key) "
        "VALUES (991299,$1,$2,$3,1,$4,'message','t-x','mock','succeeded','d','k-x')",
        [?ID_AGENT, ?ID_ORG, ?ID_GRANT, ?ID_DELEGATOR]
    ),
    assert_check(E2, <<"ck_ar_terminal_finished">>),
    {error, E3} = epgsql:equery(
        Conn, "UPDATE agent_run SET attempt = -1 WHERE id = $1", [?ID_RUN1]
    ),
    assert_check(E3, <<"ck_ar_attempt">>),
    {error, E4} = epgsql:equery(
        Conn,
        "INSERT INTO agent_run (id, agent_id, organization_id, grant_id, "
        "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
        "runtime_type, status, context_digest, idempotency_key) "
        "VALUES (991298,$1,$2,$3,1,$4,'schedule','t-y','hird','created','','k-y')",
        [?ID_AGENT, ?ID_ORG, ?ID_GRANT, ?ID_DELEGATOR]
    ),
    assert_check(E4, <<"ck_ar_context_digest">>),
    {error, E5} = epgsql:equery(
        Conn,
        "INSERT INTO agent_run (id, agent_id, organization_id, grant_id, "
        "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
        "runtime_type, status, context_digest, idempotency_key) "
        "VALUES (991297,$1,$2,$3,1,$4,'cron','t-z','mock','created','d','k-z')",
        [?ID_AGENT, ?ID_ORG, ?ID_GRANT, ?ID_DELEGATOR]
    ),
    assert_check(E5, <<"ck_ar_trigger_type">>),
    {error, E6} = epgsql:equery(
        Conn,
        "INSERT INTO agent_run_event (id, run_id, to_status, actor_kind, actor_id, "
        "idempotency_key) VALUES (991401,$1,'created','robot','x','ag31d-a-ev2')",
        [?ID_RUN1]
    ),
    assert_check(E6, <<"ck_are_actor_kind">>),
    {error, E7} = epgsql:equery(
        Conn,
        "INSERT INTO agent_run_event (id, run_id, to_status, actor_kind, actor_id, "
        "idempotency_key) VALUES (991402,$1,'dispatching','system','x','ag31d-a-ev3')",
        [?ID_RUN1]
    ),
    assert_check(E7, <<"ck_are_to_status">>),
    {error, E8} = epgsql:equery(
        Conn,
        "INSERT INTO agent_effect (id, run_id, sequence, tool_id, capability, action, "
        "resource_digest, args_digest, status) "
        "VALUES (991601,$1,1,'t','c','a','r','g','running')",
        [?ID_RUN1]
    ),
    assert_check(E8, <<"ck_ae_status">>),
    {error, E9} = epgsql:equery(
        Conn,
        "INSERT INTO agent_effect (id, run_id, sequence, tool_id, capability, action, "
        "resource_digest, args_digest, status) "
        "VALUES (991602,$1,0,'t','c','a','r','g','created')",
        [?ID_RUN1]
    ),
    assert_check(E9, <<"ck_ae_sequence">>),
    %% effect 双 UNIQUE
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_effect (id, run_id, sequence, tool_id, capability, action, "
        "resource_digest, args_digest, status, external_idempotency_key) "
        "VALUES (991603,$1,1,'tool.demo.v1','c','a','r','g','created','ext-a')",
        [?ID_RUN1]
    ),
    {error, E10} = epgsql:equery(
        Conn,
        "INSERT INTO agent_effect (id, run_id, sequence, tool_id, capability, action, "
        "resource_digest, args_digest, status, external_idempotency_key) "
        "VALUES (991604,$1,2,'tool.demo.v1','c','a','r','g','created','ext-a')",
        [?ID_RUN1]
    ),
    ?assertEqual(<<"23505">>, err_code(E10)),
    ?assertEqual(<<"uq_ae_tool_external_idem">>, err_constraint(E10)),
    {error, E11} = epgsql:equery(
        Conn,
        "INSERT INTO agent_effect (id, run_id, sequence, tool_id, capability, action, "
        "resource_digest, args_digest, status, external_idempotency_key) "
        "VALUES (991605,$1,1,'tool.other','c','a','r','g','created','ext-b')",
        [?ID_RUN1]
    ),
    ?assertEqual(<<"23505">>, err_code(E11)),
    ?assertEqual(<<"uq_ae_run_sequence">>, err_constraint(E11)),
    %% Run 五元组 UNIQUE（同 agent/org/trigger_type/trigger_id/idempotency_key，仅换 id）
    {error, E12} = epgsql:equery(
        Conn,
        "INSERT INTO agent_run (id, agent_id, organization_id, grant_id, "
        "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
        "runtime_type, status, context_digest, idempotency_key) "
        "VALUES (991296,$1,$2,$3,1,$4,'message',$5,'mock','created','d',$6)",
        [?ID_AGENT, ?ID_ORG, ?ID_GRANT, ?ID_DELEGATOR, trigger_id_of(?ID_RUN1), <<"ag31d-a-key">>]
    ),
    ?assertEqual(<<"23505">>, err_code(E12)),
    ?assertEqual(<<"uq_ar_agent_org_trigger_idem">>, err_constraint(E12)),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% b: A08 duplicate trigger → 同一 run_id、execution=1
%% ===================================================================

t_a08_duplicate_trigger(Conn) ->
    fixture_reset(Conn),
    Key = <<"ag31d-a08-key">>,
    {ok, #{run_id := RunId1, version := 1, duplicated := false}} =
        agent_run_command:create_run(Conn, run_ctx(?ID_RUN1, Key)),
    {ok, #{run_id := RunId2, version := 1, duplicated := true}} =
        agent_run_command:create_run(Conn, run_ctx(?ID_RUN1, Key)),
    ?assertEqual(RunId1, RunId2),
    ?assertEqual(?ID_RUN1, RunId1),
    %% execution=1：五元组命中恰一行 run、恰一条创建事件，无第二次执行痕迹
    {ok, _, [{RunCount}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM agent_run WHERE agent_id = $1 AND organization_id = $2 "
        "AND trigger_type = 'message' AND trigger_id = $3 AND idempotency_key = $4",
        [?ID_AGENT, ?ID_ORG, trigger_id_of(?ID_RUN1), Key]
    ),
    ?assertEqual(1, RunCount),
    ?assertEqual(
        1, agent_run_pg:count_run_events(Conn, ?ID_RUN1)
    ),
    {ok, Run} = agent_run_command:get_run(Conn, ?ID_RUN1),
    ?assertEqual(created, maps:get(status, Run)),
    %% attempt 只在 lease 获取时递增（created 态为 0）
    ?assertEqual(0, maps:get(attempt, Run)),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% c: A09 同 effect 幂等键 dispatch 两次 → 外部 attempt<=1（mock 计数）
%% ===================================================================

t_a09_duplicate_effect(Conn) ->
    fixture_reset(Conn),
    {_RunId, _V, _A} = start_running_run(Conn, ?ID_RUN1, <<"ag31d-a09-key">>),
    put(ag31d_calls, 0),
    Adapter = fun(_Input) ->
        put(ag31d_calls, get(ag31d_calls) + 1),
        {ok, <<"result-digest-1">>}
    end,
    {ok, #{effect_id := E1, status := authorized}} =
        agent_run_command:authorize_effect(Conn, ?ID_RUN1, eff_ctx(1, <<"ext-a09-1">>)),
    {ok, #{status := succeeded}} =
        agent_run_command:dispatch_effect(Conn, ?ID_RUN1, E1, Adapter, #{now => now0()}),
    ?assertEqual(1, get(ag31d_calls)),
    %% 同幂等键再登记 → duplicate_effect（UNIQUE(tool_id,external_idempotency_key)）
    {error, {duplicate_effect, <<"uq_ae_tool_external_idem">>}} =
        agent_run_command:authorize_effect(Conn, ?ID_RUN1, eff_ctx(2, <<"ext-a09-1">>)),
    ?assertEqual(1, get(ag31d_calls)),
    %% 已终态 effect 再 dispatch → 拒绝（无 redispatch）
    {error, effect_not_dispatchable} =
        agent_run_command:dispatch_effect(Conn, ?ID_RUN1, E1, Adapter, #{now => now0()}),
    ?assertEqual(1, get(ag31d_calls)),
    {ok, Effect} = agent_run_pg:get_effect(Conn, E1),
    ?assertEqual(succeeded, maps:get(status, Effect)),
    ?assertEqual(<<"result-digest-1">>, maps:get(result_digest, Effect)),
    erase(ag31d_calls),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% d: A11 双连接抢过期 lease → 恰一 winner
%% ===================================================================

t_a11_lease_race(Conn) ->
    fixture_reset(Conn),
    {_RunId, _V, 1} = start_running_run(Conn, ?ID_RUN1, <<"ag31d-a11-key">>, <<"w1">>),
    %% 模拟 worker crash：lease 过期（直接条件 SQL，仅夹具）
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE agent_run SET lease_expires_at = now() - interval '1 hour' WHERE id = $1",
        [?ID_RUN1]
    ),
    {ok, Conn2} = epgsql:connect(
        #{
            host => os:getenv("AG31_PG_HOST"),
            port => list_to_integer(os:getenv("AG31_PG_PORT")),
            username => os:getenv("AG31_PG_USER"),
            password => os:getenv("AG31_PG_PASSWORD", ""),
            database => os:getenv("AG31_PG_DB")
        }
    ),
    try
        %% 双连接竞争过期 lease：行锁串行化 + 谓词重估 → 恰一 winner
        R1 = agent_run_command:take_over_lease(
            Conn2, ?ID_RUN1, <<"w2">>, 60, #{now => now0()}
        ),
        R2 = agent_run_command:take_over_lease(
            Conn, ?ID_RUN1, <<"w3">>, 60, #{now => now0()}
        ),
        Winners = [ok || {ok, _} <- [R1, R2]],
        ?assertEqual(1, length(Winners)),
        {ok, #{version := V2, attempt := 2}} = R1,
        ?assertMatch({error, lease_not_acquired}, R2),
        {ok, Run} = agent_run_command:get_run(Conn, ?ID_RUN1),
        ?assertEqual(<<"w2">>, maps:get(lease_owner, Run)),
        ?assertEqual(running, maps:get(status, Run)),
        ?assertEqual(2, maps:get(attempt, Run)),
        %% winner 可续租；loser 续租被拒
        {ok, _} = agent_run_command:renew_lease(
            Conn2, ?ID_RUN1, <<"w2">>, 60, V2, #{now => now0()}
        ),
        {error, lease_not_acquired} = agent_run_command:renew_lease(
            Conn, ?ID_RUN1, <<"w3">>, 60, V2 + 1, #{now => now0()}
        )
    after
        try epgsql:close(Conn2) of
            _ -> ok
        catch
            _:_ -> ok
        end
    end,
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% e: revoke race——Grant 置 revoked 后新 effect authorize 拒绝（读 00000132 实时状态）
%% ===================================================================

t_revoke_race(Conn) ->
    fixture_reset(Conn),
    {_RunId, _V, _A} = start_running_run(Conn, ?ID_RUN1, <<"ag31d-e-key">>),
    %% Grant CAS 撤销（00000132 agent_grant 实时状态）
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET status = 'revoked', revoked_at = now(), "
        "revoked_by_user_id = $2, version = version + 1 "
        "WHERE id = $1 AND status = 'active' AND version = 1",
        [?ID_GRANT, ?ID_DELEGATOR]
    ),
    {error, grant_revoked} =
        agent_run_command:authorize_effect(Conn, ?ID_RUN1, eff_ctx(1, <<"ext-e-1">>)),
    {ok, _, [{Denied}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM agent_effect WHERE run_id = $1 AND status = 'denied' "
        "AND authorization_reason = 'grant_revoked'",
        [?ID_RUN1]
    ),
    ?assertEqual(1, Denied),
    %% Run 仍 running（Grant revoke 不重写历史 Run，§9.3）：后续收敛由 cancel/失败边走
    {ok, Run} = agent_run_command:get_run(Conn, ?ID_RUN1),
    ?assertEqual(running, maps:get(status, Run)),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% f: unknown——dispatching 后 crash → 新 effect 拒绝 → reconcile→succeeded/failed 各一
%% ===================================================================

t_unknown_reconcile(Conn) ->
    fixture_reset(Conn),
    %% --- 场景 1：reconcile → succeeded ---
    {_R1, _V1, _A1} = start_running_run(Conn, ?ID_RUN1, <<"ag31d-f1-key">>),
    {ok, #{effect_id := E1}} =
        agent_run_command:authorize_effect(Conn, ?ID_RUN1, eff_ctx(1, <<"ext-f-1">>)),
    {ok, #{status := unknown}} =
        agent_run_command:dispatch_effect(
            Conn,
            ?ID_RUN1,
            E1,
            fun(_Input) -> throw(adapter_crash) end,
            #{now => now0()}
        ),
    {ok, EffectU} = agent_run_pg:get_effect(Conn, E1),
    ?assertEqual(unknown, maps:get(status, EffectU)),
    ?assertEqual(<<"adapter_crash">>, maps:get(failure_code, EffectU)),
    %% Run E11 running→unknown
    {ok, _VUnknown} = agent_run_command:transition(
        Conn,
        ?ID_RUN1,
        running,
        unknown,
        #{expected_version => 3, now => now0(), actor_id => <<"system:worker">>}
    ),
    %% unknown 禁止新 Effect / cancel / 非法 reconcile outcome
    {error, run_unknown_no_new_effect} =
        agent_run_command:authorize_effect(Conn, ?ID_RUN1, eff_ctx(2, <<"ext-f-2">>)),
    {ok, _, [{EffectCount}]} = epgsql:equery(
        Conn, "SELECT count(*) FROM agent_effect WHERE run_id = $1", [?ID_RUN1]
    ),
    ?assertEqual(1, EffectCount),
    {error, cancel_rejected} = agent_run_command:cancel_run(
        Conn, ?ID_RUN1, <<"human-1">>, #{now => now0(), expected_version => 4}
    ),
    {error, invalid_outcome} = agent_run_command:reconcile_run(
        Conn, ?ID_RUN1, cancelled, #{now => now0(), expected_version => 4}
    ),
    %% 显式 reconcile → succeeded（E15）
    {ok, _VSucc} = agent_run_command:reconcile_run(
        Conn, ?ID_RUN1, succeeded, #{now => now0(), expected_version => 4}
    ),
    {ok, RunS} = agent_run_command:get_run(Conn, ?ID_RUN1),
    ?assertEqual(succeeded, maps:get(status, RunS)),
    ?assertEqual(<<"reconciled">>, maps:get(reason_code, RunS)),
    ?assert(maps:get(finished_at, RunS) =/= undefined),
    %% effect ledger 同步收敛（显式 reconcile，§10.3）
    {ok, _} = agent_run_command:reconcile_effect(Conn, E1, succeeded, <<"digest-f-1">>, #{
        now => now0()
    }),
    {ok, EffectS} = agent_run_pg:get_effect(Conn, E1),
    ?assertEqual(succeeded, maps:get(status, EffectS)),
    ?assertEqual(<<"digest-f-1">>, maps:get(result_digest, EffectS)),
    {error, reconcile_rejected} = agent_run_command:reconcile_effect(
        Conn, E1, failed, <<"x">>, #{now => now0()}
    ),
    %% --- 场景 2：reconcile → failed（E16）---
    {_R2, _V2, _A2} = start_running_run(Conn, ?ID_RUN2, <<"ag31d-f2-key">>),
    {ok, _} = agent_run_command:authorize_effect(Conn, ?ID_RUN2, eff_ctx(1, <<"ext-f-3">>)),
    {ok, _VU2} = agent_run_command:transition(
        Conn,
        ?ID_RUN2,
        running,
        unknown,
        #{expected_version => 3, now => now0(), actor_id => <<"system:worker">>}
    ),
    {ok, _} = agent_run_command:reconcile_run(
        Conn, ?ID_RUN2, failed, #{now => now0(), expected_version => 4}
    ),
    {ok, RunF} = agent_run_command:get_run(Conn, ?ID_RUN2),
    ?assertEqual(failed, maps:get(status, RunF)),
    ?assertEqual(<<"reconciled">>, maps:get(reason_code, RunF)),
    %% unknown→cancelled 仍非法：取消被拒
    {error, cancel_rejected} = agent_run_command:cancel_run(
        Conn, ?ID_RUN2, <<"human-1">>, #{now => now0(), expected_version => 5}
    ),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% g: CAS 冲突 / 非法边拒绝且无 event 行 / attempt 上限（CS-6）
%% ===================================================================

t_cas_and_cap(Conn) ->
    fixture_reset(Conn),
    {ok, _} = agent_run_command:create_run(Conn, run_ctx(?ID_RUN1, <<"ag31d-g-key">>)),
    %% 旧 version CAS 被拒
    {error, cas_conflict} = agent_run_command:transition(
        Conn,
        ?ID_RUN1,
        created,
        queued,
        #{expected_version => 99, now => now0(), actor_id => <<"system">>}
    ),
    %% 非法边被拒（域层先拒，SQL 不触达）
    {error, illegal_transition} = agent_run_command:transition(
        Conn,
        ?ID_RUN1,
        created,
        running,
        #{expected_version => 1, now => now0(), actor_id => <<"system">>}
    ),
    {error, illegal_transition} = agent_run_command:transition(
        Conn,
        ?ID_RUN1,
        created,
        succeeded,
        #{expected_version => 1, now => now0(), actor_id => <<"system">>}
    ),
    %% 均无 event 行（只有创建事件 1 条）
    ?assertEqual(1, agent_run_pg:count_run_events(Conn, ?ID_RUN1)),
    %% 合法边推进正常
    {ok, _} = agent_run_command:transition(
        Conn,
        ?ID_RUN1,
        created,
        queued,
        #{expected_version => 1, now => now0(), actor_id => <<"system:trigger">>}
    ),
    ?assertEqual(2, agent_run_pg:count_run_events(Conn, ?ID_RUN1)),
    %% attempt 达上限（3）：lease 获取拒绝，Run 以 failed(max_attempts_exceeded) 收束
    {ok, 1} = epgsql:equery(
        Conn, "UPDATE agent_run SET attempt = 3 WHERE id = $1", [?ID_RUN1]
    ),
    {error, max_attempts_exceeded} = agent_run_command:acquire_lease(
        Conn, ?ID_RUN1, <<"w1">>, 60, #{now => now0()}
    ),
    {ok, RunCap} = agent_run_command:get_run(Conn, ?ID_RUN1),
    ?assertEqual(failed, maps:get(status, RunCap)),
    ?assertEqual(<<"max_attempts_exceeded">>, maps:get(reason_code, RunCap)),
    ?assert(maps:get(finished_at, RunCap) =/= undefined),
    %% attempt 版本事件已追加（queued→failed 边）
    ?assertEqual(3, agent_run_pg:count_run_events(Conn, ?ID_RUN1)),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% h: HITL 链——E07/E12/E13 + approval digest 绑定 + 批准不覆盖已撤销 Grant
%% ===================================================================

t_hitl(Conn) ->
    fixture_reset(Conn),
    %% --- 链 A：approval_required → reject → E13 failed ---
    {_R1, _V1, _A1} = start_running_run(Conn, ?ID_RUN1, <<"ag31d-h1-key">>),
    {ok, #{effect_id := E1, status := waiting_approval}} =
        agent_run_command:authorize_effect(
            Conn, ?ID_RUN1, (eff_ctx(1, <<"ext-h-1">>))#{decision => approval_required}
        ),
    {ok, RunW} = agent_run_command:get_run(Conn, ?ID_RUN1),
    ?assertEqual(waiting_approval, maps:get(status, RunW)),
    %% digest 不匹配拒绝（批准绑定 args_digest，§10.3）
    {error, approval_digest_mismatch} = agent_run_command:approve_effect(
        Conn,
        ?ID_RUN1,
        E1,
        #{
            args_digest => <<"sha256:WRONG">>,
            approval_ref => <<"appr-1">>,
            actor_id => <<"human-1">>,
            now => now0()
        }
    ),
    %% 拒绝（human）→ effect denied + Run E13 failed
    {ok, #{effect_version := _EV1, run_version := _RV1}} = agent_run_command:reject_effect(
        Conn,
        ?ID_RUN1,
        E1,
        #{actor_id => <<"human-1">>, reason_code => approval_rejected, now => now0()}
    ),
    {ok, RunF} = agent_run_command:get_run(Conn, ?ID_RUN1),
    ?assertEqual(failed, maps:get(status, RunF)),
    ?assertEqual(<<"approval_rejected">>, maps:get(reason_code, RunF)),
    {ok, EffDenied} = agent_run_pg:get_effect(Conn, E1),
    ?assertEqual(denied, maps:get(status, EffDenied)),
    %% --- 链 B：批准 → E12 回 queued → 重新 lease → dispatch 成功 ---
    {_R2, _V2, _A2} = start_running_run(Conn, ?ID_RUN2, <<"ag31d-h2-key">>),
    {ok, #{effect_id := E2}} = agent_run_command:authorize_effect(
        Conn, ?ID_RUN2, (eff_ctx(1, <<"ext-h-2">>))#{decision => approval_required}
    ),
    put(ag31d_calls, 0),
    {ok, #{effect_version := _EV2, run_version := _RV2}} = agent_run_command:approve_effect(
        Conn,
        ?ID_RUN2,
        E2,
        #{
            args_digest => <<"sha256:args">>,
            approval_ref => <<"appr-2">>,
            actor_id => <<"human-1">>,
            now => now0()
        }
    ),
    {ok, RunQ} = agent_run_command:get_run(Conn, ?ID_RUN2),
    ?assertEqual(queued, maps:get(status, RunQ)),
    {ok, _} = agent_run_command:acquire_lease(
        Conn, ?ID_RUN2, <<"w1">>, 60, #{now => now0()}
    ),
    {ok, #{status := succeeded}} = agent_run_command:dispatch_effect(
        Conn,
        ?ID_RUN2,
        E2,
        fun(_Input) ->
            put(ag31d_calls, get(ag31d_calls) + 1),
            {ok, <<"result-h">>}
        end,
        #{now => now0()}
    ),
    ?assertEqual(1, get(ag31d_calls)),
    {ok, EffAuth} = agent_run_pg:get_effect(Conn, E2),
    ?assertEqual(<<"appr-2">>, maps:get(approval_ref, EffAuth)),
    ?assertEqual(1, maps:get(grant_version_checked, EffAuth)),
    erase(ag31d_calls),
    %% --- 链 C：批准期间 Grant 被撤销 → 批准不能覆盖（effect denied + E13 failed）---
    {_R3, _V3, _A3} = start_running_run(Conn, ?ID_RUN3, <<"ag31d-h3-key">>),
    {ok, #{effect_id := E3}} = agent_run_command:authorize_effect(
        Conn, ?ID_RUN3, (eff_ctx(1, <<"ext-h-3">>))#{decision => approval_required}
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET status = 'revoked', revoked_at = now(), "
        "revoked_by_user_id = $2, version = version + 1 "
        "WHERE id = $1 AND status = 'active' AND version = 1",
        [?ID_GRANT, ?ID_DELEGATOR]
    ),
    {error, grant_revoked} = agent_run_command:approve_effect(
        Conn,
        ?ID_RUN3,
        E3,
        #{
            args_digest => <<"sha256:args">>,
            approval_ref => <<"appr-3">>,
            actor_id => <<"human-1">>,
            now => now0()
        }
    ),
    {ok, RunF3} = agent_run_command:get_run(Conn, ?ID_RUN3),
    ?assertEqual(failed, maps:get(status, RunF3)),
    ?assertEqual(<<"grant_revoked">>, maps:get(reason_code, RunF3)),
    {ok, EffD3} = agent_run_pg:get_effect(Conn, E3),
    ?assertEqual(denied, maps:get(status, EffD3)),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% i: 链头 00000133 非 dirty + 结构复断言（up/down/up 时间线库端锚点）
%% ===================================================================

t_chain_head(Conn) ->
    {ok, _, [{Version, Dirty}]} = epgsql:equery(
        Conn, "SELECT version, dirty FROM schema_migrations", []
    ),
    ?assertEqual(?CHAIN_HEAD, Version),
    ?assertEqual(false, Dirty),
    fixture_reset(Conn),
    {ok, 1} = insert_fixture_run(Conn, ?ID_RUN1, <<"ag31d-i-key">>, <<"queued">>),
    {ok, _, [Row]} = epgsql:equery(
        Conn,
        "SELECT status, version, attempt, idempotency_key, "
        "(queued_at IS NOT NULL) FROM agent_run WHERE id = $1",
        [?ID_RUN1]
    ),
    ?assertEqual({<<"queued">>, 1, 0, <<"ag31d-i-key">>, true}, Row),
    %% 结构锚点复跑（与 up1 断言同源）
    lists:foreach(
        fun(T) ->
            {ok, _, [{Exists}]} = epgsql:equery(
                Conn, "SELECT to_regclass('public.' || $1) IS NOT NULL", [T]
            ),
            ?assertEqual(true, Exists)
        end,
        [<<"agent_run">>, <<"agent_run_event">>, <<"agent_effect">>]
    ),
    ?assertEqual(
        3,
        count_existing_indexes(Conn, [
            <<"i_ar_status_lease">>, <<"i_ar_agent_org_created">>, <<"i_ar_grant_status">>
        ])
    ),
    fixture_reset(Conn),
    ok.

%% ===================================================================
%% 夹具（自建自清；replica 旁路仅用于测试清理）
%% ===================================================================

fixture_reset(Conn) ->
    fixture_cleanup(Conn),
    fixture_insert(Conn).

fixture_insert(Conn) ->
    {ok, 3} = epgsql:equery(
        Conn,
        "INSERT INTO \"user\" (id, account, password, reg_ip, reg_cosv, status) "
        "VALUES ($1,'ag31d_eunit_delegator','x','127.0.0.1','e2e',1), "
        "($2,'ag31d_eunit_agent','x','127.0.0.1','e2e',1), "
        "($3,'ag31d_eunit_human','x','127.0.0.1','e2e',1)",
        [?ID_DELEGATOR, ?ID_AGENT, ?ID_HUMAN]
    ),
    {ok, 1} = epgsql:equery(
        Conn, "UPDATE \"user\" SET account_type = 1 WHERE id = $1", [?ID_AGENT]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO organization (id, name, owner_id) VALUES ($1,'ag31d-eunit-org',$2)",
        [?ID_ORG, ?ID_DELEGATOR]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO workspace (id, name, owner_id, organization_id) "
        "VALUES ($1,'ag31d-eunit-ws',$2,$3)",
        [?ID_WS, ?ID_DELEGATOR, ?ID_ORG]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant (id, agent_id, organization_id, delegator_user_id, "
        "workspace_scope_kind, status, valid_from, expires_at, version, idempotency_key) "
        "VALUES ($1,$2,$3,$4,'none','active', now() - interval '1 hour', "
        "now() + interval '1 day', 1, 'ag31d-grant-k1')",
        [?ID_GRANT, ?ID_AGENT, ?ID_ORG, ?ID_DELEGATOR]
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
    %% 按 agent_id 级联式清理：覆盖负例断言遗留的任意 id 行（99129x 等），残留零容忍
    AgentId = integer_to_list(?ID_AGENT),
    [
        "DELETE FROM agent_effect WHERE run_id IN "
        "(SELECT id FROM agent_run WHERE agent_id = " ++
            AgentId ++ ")",
        "DELETE FROM agent_run_event WHERE run_id IN "
        "(SELECT id FROM agent_run WHERE agent_id = " ++
            AgentId ++ ")",
        "DELETE FROM agent_run WHERE agent_id = " ++ AgentId,
        "DELETE FROM agent_grant WHERE id IN (" ++ id_list([?ID_GRANT]) ++ ")",
        "DELETE FROM workspace WHERE id IN (" ++ id_list([?ID_WS]) ++ ")",
        "DELETE FROM organization WHERE id IN (" ++ id_list([?ID_ORG]) ++ ")",
        "DELETE FROM \"user\" WHERE id IN (" ++
            id_list([?ID_DELEGATOR, ?ID_AGENT, ?ID_HUMAN]) ++ ")"
    ].

%% 直接 SQL 插一行 agent_run（结构断言用）
insert_fixture_run(Conn, RunId, Key, Status) ->
    epgsql:equery(
        Conn,
        "INSERT INTO agent_run (id, agent_id, organization_id, workspace_id, grant_id, "
        "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
        "runtime_type, status, context_digest, idempotency_key, queued_at) "
        "VALUES ($1,$2,$3,NULL,$4,1,$5,'message',$6,'mock',$7,'sha256:d',$8, "
        "CASE WHEN $7 = 'queued' THEN now() END)",
        [RunId, ?ID_AGENT, ?ID_ORG, ?ID_GRANT, ?ID_DELEGATOR, trigger_id_of(RunId), Status, Key]
    ).

%% 域命令路径：create → queued → running（默认 worker w0），返回 {RunId, V, Attempt}
start_running_run(Conn, RunId, Key) ->
    start_running_run(Conn, RunId, Key, <<"w0">>).

start_running_run(Conn, RunId, Key, Worker) ->
    {ok, _} = agent_run_command:create_run(Conn, run_ctx(RunId, Key)),
    {ok, _} = agent_run_command:transition(
        Conn,
        RunId,
        created,
        queued,
        #{expected_version => 1, now => now0(), actor_id => <<"system:trigger">>}
    ),
    {ok, #{version := V, attempt := Attempt}} = agent_run_command:acquire_lease(
        Conn, RunId, Worker, 60, #{now => now0()}
    ),
    {RunId, V, Attempt}.

run_ctx(RunId, Key) ->
    #{
        id => RunId,
        agent_id => ?ID_AGENT,
        organization_id => ?ID_ORG,
        workspace_id => undefined,
        grant_id => ?ID_GRANT,
        grant_version_at_start => 1,
        delegating_principal_id => ?ID_DELEGATOR,
        trigger_type => message,
        trigger_id => trigger_id_of(RunId),
        runtime_type => mock,
        context_digest => <<"sha256:ctx-", (integer_to_binary(RunId))/binary>>,
        idempotency_key => Key,
        now => now0()
    }.

eff_ctx(Seq, ExtKey) ->
    #{
        sequence => Seq,
        tool_id => <<"tool.demo.v1">>,
        capability => <<"demo.write">>,
        action => <<"create">>,
        resource_digest => <<"sha256:res">>,
        args_digest => <<"sha256:args">>,
        external_idempotency_key => ExtKey,
        decision => allow,
        now => now0()
    }.

trigger_id_of(RunId) ->
    <<"trigger-", (integer_to_binary(RunId))/binary>>.

now0() ->
    calendar:universal_time().

%% ===================================================================
%% 断言辅助（与 02B 同口径）
%% ===================================================================

squery_ok(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        Other -> erlang:error({squery_failed, Sql, Other})
    end.

%% equery 出错时 `{error, Err}` 解构出的 Err 即 #error{} record（编译后 tuple 形状）
err_code({error, _Severity, Code, _Codename, _Msg, _Extra}) ->
    Code;
err_code(Unexpected) ->
    erlang:error({expected_violation_not_raised, Unexpected}).

err_constraint({error, _S, _C, _N, _M, Extra}) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> erlang:error({no_constraint_name_in_error, Extra})
    end;
err_constraint(Unexpected) ->
    erlang:error({expected_violation_not_raised, Unexpected}).

assert_check({error, _S, <<"23514">>, _N, _M, Extra} = Err, ExpectConstraint) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, ExpectConstraint} -> ok;
        {constraint_name, Other} -> erlang:error({wrong_constraint, ExpectConstraint, Other, Err})
    end;
assert_check(Unexpected, ExpectConstraint) ->
    erlang:error({expected_check_violation_not_raised, ExpectConstraint, Unexpected}).

count_existing_constraints(Conn, Table, Names) ->
    Placeholders = placeholders(2, length(Names)),
    Sql =
        "SELECT conname FROM pg_constraint "
        "WHERE conrelid = to_regclass('public.' || $1) AND conname IN (" ++
            Placeholders ++ ")",
    {ok, _, Rows} = epgsql:equery(Conn, Sql, [Table | Names]),
    Found = [N || {N} <- Rows],
    case Names -- Found of
        [] -> length(Names);
        Missing -> erlang:error({missing_constraints, Table, Missing})
    end.

count_existing_indexes(Conn, Names) ->
    Placeholders = placeholders(1, length(Names)),
    Sql = "SELECT indexname FROM pg_indexes WHERE indexname IN (" ++ Placeholders ++ ")",
    {ok, _, Rows} = epgsql:equery(Conn, Sql, Names),
    Found = [N || {N} <- Rows],
    case Names -- Found of
        [] -> length(Names);
        Missing -> erlang:error({missing_indexes, Missing})
    end.

str_contains(Haystack, Needle) ->
    binary:match(Haystack, Needle) =/= nomatch.

placeholders(Start, N) ->
    string:join(["$" ++ integer_to_list(I) || I <- lists:seq(Start, Start + N - 1)], ",").

id_list(Ids) ->
    string:join([integer_to_list(I) || I <- Ids], ",").
