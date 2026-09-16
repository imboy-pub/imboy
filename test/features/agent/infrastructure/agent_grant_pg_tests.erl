%% @doc AG31-02B：agent_grant 四表（迁移 00000132，架构合同 §7.2 Frozen Grant
%% Schema Contract）的隔离 PG 验收测试。
%%
%% == 运行前置 ==
%%
%% 隔离一次性 PG（非 4323 共享库、非生产），库内已应用全链迁移到 00000132。
%% 连接参数只经进程环境变量注入（不写入任何文件/日志）：
%%
%%   AG31_PG_HOST / AG31_PG_PORT / AG31_PG_DB / AG31_PG_USER / AG31_PG_PASSWORD(可空)
%%
%% == 铁律 ==
%%
%% 环境变量缺失或连不上库 → **显式 FAIL**（erlang:error），禁止静默 skip/pass。
%% 测试夹具自建自清（固定高位 id 区段 990000+，不 TRUNCATE 任何共享表）；清理走
%% session_replication_role=replica 一次性旁路——append-only 触发器按合同拒绝一切
%% DELETE（含夹具行），且该模式下 FK CASCADE 同样被绕过，故按子表→父表顺序显式删除。
-module(agent_grant_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHAIN_HEAD, 132).
%% 夹具固定 id 区段（与 psql 侧行为断言脚本同区段，互不污染共享表）
-define(ID_DELEGATOR, 990010).
-define(ID_AGENT, 990011).
-define(ID_ORG1, 990001).
-define(ID_ORG2, 990002).
-define(ID_WS1, 990020).
-define(ID_WS2, 990030).
-define(ID_GRANT, 990100).
-define(ID_GRANT_ALT, 990198).
-define(ID_EVENT, 990300).
-define(ID_EVENT_ALT, 990301).

%% ===================================================================
%% 套件组织
%% ===================================================================

agent_grant_pg_test_() ->
    {setup, fun connect_required/0, fun disconnect/1, fun(Conn) ->
        [
            {"a: four tables + key constraints exist (pg_catalog)", fun() ->
                t_structure(Conn)
            end},
            {"b: cross-org workspace insert rejected by composite FK", fun() ->
                t_cross_org_fk(Conn)
            end},
            {"c: (organization_id, delegator_user_id, idempotency_key) duplicate rejected", fun() ->
                    t_idempotency_unique(Conn)
                end},
            {"d: agent_grant_event UPDATE/DELETE rejected by append-only guard (23514)", fun() ->
                t_append_only(Conn)
            end},
            {"e: CHECK group (revoked match / validity / scope_kind / version / non-empty / enum)",
                fun() ->
                    t_checks(Conn)
                end},
            {"f: chain head at 00000132 not dirty + data roundtrip + structure re-assert", fun() ->
                t_chain_head_and_roundtrip(Conn)
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

disconnect(Conn) ->
    try epgsql:close(Conn) of
        _ -> ok
    catch
        _:_ -> ok
    end,
    ok.

%% ===================================================================
%% a: 结构断言（pg_catalog / information_schema）
%% ===================================================================

t_structure(Conn) ->
    %% 四表存在
    Tables = [
        <<"agent_grant">>,
        <<"agent_grant_workspace">>,
        <<"agent_grant_capability">>,
        <<"agent_grant_event">>
    ],
    lists:foreach(
        fun(T) ->
            {ok, _, [{Exists}]} = epgsql:equery(
                Conn,
                "SELECT to_regclass('public.' || $1) IS NOT NULL",
                [T]
            ),
            ?assertEqual(true, Exists)
        end,
        Tables
    ),
    %% agent_grant 列契约（14 列，类型/可空性逐列）
    {ok, _, Cols} = epgsql:equery(
        Conn,
        "SELECT column_name, data_type, is_nullable FROM information_schema.columns "
        "WHERE table_name = 'agent_grant' ORDER BY ordinal_position",
        []
    ),
    ?assertMatch(
        [
            {<<"id">>, <<"bigint">>, <<"NO">>},
            {<<"agent_id">>, <<"bigint">>, <<"NO">>},
            {<<"organization_id">>, <<"bigint">>, <<"NO">>},
            {<<"delegator_user_id">>, <<"bigint">>, <<"NO">>},
            {<<"workspace_scope_kind">>, <<"text">>, <<"NO">>},
            {<<"status">>, <<"text">>, <<"NO">>},
            {<<"valid_from">>, <<"timestamp with time zone">>, <<"NO">>},
            {<<"expires_at">>, <<"timestamp with time zone">>, <<"NO">>},
            {<<"revoked_at">>, <<"timestamp with time zone">>, <<"YES">>},
            {<<"revoked_by_user_id">>, <<"bigint">>, <<"YES">>},
            {<<"version">>, <<"integer">>, <<"NO">>},
            {<<"idempotency_key">>, <<"text">>, <<"NO">>},
            {<<"created_at">>, <<"timestamp with time zone">>, <<"NO">>},
            {<<"updated_at">>, <<"timestamp with time zone">>, <<"NO">>}
        ],
        Cols
    ),
    %% agent_grant 关键约束 12 项（PK/UNIQUE×2/CHECK×4/FK×4）
    ?assertEqual(
        12,
        count_existing_constraints(Conn, <<"agent_grant">>, [
            <<"pk_agent_grant">>,
            <<"uq_ag_org_id">>,
            <<"uq_ag_org_delegator_idempotency">>,
            <<"ck_ag_workspace_scope_kind">>,
            <<"ck_ag_status">>,
            <<"ck_ag_validity">>,
            <<"ck_ag_status_revoked_match">>,
            <<"ck_ag_version">>,
            <<"fk_ag_agent">>,
            <<"fk_ag_organization">>,
            <<"fk_ag_delegator">>,
            <<"fk_ag_revoked_by">>
        ])
    ),
    %% workspace 复合 FK = RESTRICT；grant 复合 FK = CASCADE
    {ok, _, FkRows} = epgsql:equery(
        Conn,
        "SELECT conname, confdeltype::text FROM pg_constraint "
        "WHERE conrelid = 'agent_grant_workspace'::regclass AND contype = 'f'",
        []
    ),
    FkActions = maps:from_list([{N, A} || {N, A} <- FkRows]),
    ?assertEqual(<<"r">>, maps:get(<<"fk_agw_workspace">>, FkActions)),
    ?assertEqual(<<"c">>, maps:get(<<"fk_agw_grant">>, FkActions)),
    %% append-only 触发器 + 守卫函数
    {ok, _, [{Trg}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM pg_trigger "
        "WHERE tgrelid = 'agent_grant_event'::regclass "
        "AND tgname = 'trg_agent_grant_event_append_only' AND NOT tgisinternal",
        []
    ),
    ?assertEqual(1, Trg),
    {ok, _, [{Fn}]} = epgsql:equery(
        Conn,
        "SELECT to_regprocedure('fn_agent_grant_event_append_only()') IS NOT NULL",
        []
    ),
    ?assertEqual(true, Fn),
    %% 合同 4 条 INDEX
    ?assertEqual(
        4,
        count_existing_indexes(Conn, [
            <<"i_ag_agent_org_status_expires">>,
            <<"i_ag_delegator_org_status">>,
            <<"i_agw_workspace_grant">>,
            <<"i_age_grant_created">>
        ])
    ),
    ok.

%% ===================================================================
%% b: 跨 Org workspace 复合 FK 拒绝
%% ===================================================================

t_cross_org_fk(Conn) ->
    fixture_cleanup(Conn),
    fixture_insert(Conn),
    %% 负例：org1 的 grant 指向 org2 的 workspace → 23503 fk_agw_workspace
    {error, Err1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant_workspace (organization_id, grant_id, workspace_id) "
        "VALUES ($1, $2, $3)",
        [?ID_ORG1, ?ID_GRANT, ?ID_WS2]
    ),
    ?assertEqual(<<"23503">>, err_code(Err1)),
    ?assertEqual(<<"fk_agw_workspace">>, err_constraint(Err1)),
    %% 正例：同 Org workspace 可写
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant_workspace (organization_id, grant_id, workspace_id) "
        "VALUES ($1, $2, $3)",
        [?ID_ORG1, ?ID_GRANT, ?ID_WS1]
    ),
    fixture_cleanup(Conn),
    ok.

%% ===================================================================
%% c: 幂等键唯一域 (organization_id, delegator_user_id, idempotency_key)
%% ===================================================================

t_idempotency_unique(Conn) ->
    fixture_cleanup(Conn),
    fixture_insert(Conn),
    {error, Err} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant (id, agent_id, organization_id, delegator_user_id, "
        "workspace_scope_kind, status, valid_from, expires_at, version, idempotency_key) "
        "VALUES ($1,$2,$3,$4,'none','active', now() - interval '1 hour', "
        "now() + interval '1 day', 1, $5)",
        [?ID_GRANT_ALT, ?ID_AGENT, ?ID_ORG1, ?ID_DELEGATOR, <<"ag31b-idem-k1">>]
    ),
    ?assertEqual(<<"23505">>, err_code(Err)),
    ?assertEqual(<<"uq_ag_org_delegator_idempotency">>, err_constraint(Err)),
    fixture_cleanup(Conn),
    ok.

%% ===================================================================
%% d: agent_grant_event append-only（23514）
%% ===================================================================

t_append_only(Conn) ->
    fixture_cleanup(Conn),
    fixture_insert(Conn),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant_event (id, grant_id, event_type, actor_kind, actor_user_id, "
        "from_version, to_version, detail_json, idempotency_key) "
        "VALUES ($1,$2,'issued','human',$3,NULL,1,'{}'::jsonb,$4)",
        [?ID_EVENT, ?ID_GRANT, ?ID_DELEGATOR, <<"ag31b-eunit-event-k1">>]
    ),
    {error, ErrUpd} = epgsql:equery(
        Conn,
        "UPDATE agent_grant_event SET detail_json = '{\"tampered\":true}'::jsonb "
        "WHERE id = $1",
        [?ID_EVENT]
    ),
    ?assertEqual(<<"23514">>, err_code(ErrUpd)),
    ?assertEqual(<<"trg_agent_grant_event_append_only">>, err_constraint(ErrUpd)),
    {error, ErrDel} = epgsql:equery(
        Conn,
        "DELETE FROM agent_grant_event WHERE id = $1",
        [?ID_EVENT]
    ),
    ?assertEqual(<<"23514">>, err_code(ErrDel)),
    ?assertEqual(<<"trg_agent_grant_event_append_only">>, err_constraint(ErrDel)),
    fixture_cleanup(Conn),
    ok.

%% ===================================================================
%% e: CHECK 组
%% ===================================================================

t_checks(Conn) ->
    fixture_cleanup(Conn),
    fixture_insert(Conn),
    %% E1: active + revoked_at 非空
    {error, E1} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET revoked_at = now() WHERE id = $1",
        [?ID_GRANT]
    ),
    assert_check(E1, <<"ck_ag_status_revoked_match">>),
    %% E2: revoked 但 revoked_at 为 NULL
    {error, E2} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET status = 'revoked' WHERE id = $1",
        [?ID_GRANT]
    ),
    assert_check(E2, <<"ck_ag_status_revoked_match">>),
    %% E3: revoked_at 非空但 revoked_by_user_id 为 NULL（同空/同非空被破坏）
    {error, E3} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET status = 'revoked', revoked_at = now() WHERE id = $1",
        [?ID_GRANT]
    ),
    assert_check(E3, <<"ck_ag_status_revoked_match">>),
    %% E4: expires_at = valid_from（合同为严格大于）
    {error, E4} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET expires_at = valid_from WHERE id = $1",
        [?ID_GRANT]
    ),
    assert_check(E4, <<"ck_ag_validity">>),
    %% E5: workspace_scope_kind = 'all'（V3.1 禁止通配）
    {error, E5} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET workspace_scope_kind = 'all' WHERE id = $1",
        [?ID_GRANT]
    ),
    assert_check(E5, <<"ck_ag_workspace_scope_kind">>),
    %% E6: version = 0
    {error, E6} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET version = 0 WHERE id = $1",
        [?ID_GRANT]
    ),
    assert_check(E6, <<"ck_ag_version">>),
    %% E7: capability 空串
    {error, E7} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant_capability (grant_id, capability, action, resource_type) "
        "VALUES ($1, '', 'read', 'message')",
        [?ID_GRANT]
    ),
    assert_check(E7, <<"ck_agc_capability">>),
    %% E8: event_type 非法值
    {error, E8} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant_event (id, grant_id, event_type, actor_kind, "
        "to_version, detail_json, idempotency_key) "
        "VALUES ($1,$2,'mutated','system',1,'{}'::jsonb,$3)",
        [?ID_EVENT_ALT, ?ID_GRANT, <<"ag31b-eunit-event-bad">>]
    ),
    assert_check(E8, <<"ck_age_event_type">>),
    fixture_cleanup(Conn),
    ok.

%% ===================================================================
%% f: 链头状态 + 数据往返 + 结构复断言（up/down/up 时间线的库端锚点）
%% ===================================================================

t_chain_head_and_roundtrip(Conn) ->
    %% 迁移链头 = 00000132 且非 dirty（up/down/up 后状态一致）
    {ok, _, [{Version, Dirty}]} = epgsql:equery(
        Conn,
        "SELECT version, dirty FROM schema_migrations",
        []
    ),
    ?assertEqual(?CHAIN_HEAD, Version),
    ?assertEqual(false, Dirty),
    %% 数据往返：CAS 撤销流正例 + 读回字段一致
    fixture_cleanup(Conn),
    fixture_insert(Conn),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE agent_grant SET status = 'revoked', revoked_at = now(), "
        "revoked_by_user_id = $2, version = 2 WHERE id = $1 AND version = 1",
        [?ID_GRANT, ?ID_DELEGATOR]
    ),
    {ok, _, [Row]} = epgsql:equery(
        Conn,
        "SELECT status, version, idempotency_key, revoked_by_user_id, "
        "(revoked_at IS NOT NULL) FROM agent_grant WHERE id = $1",
        [?ID_GRANT]
    ),
    ?assertEqual({<<"revoked">>, 2, <<"ag31b-idem-k1">>, ?ID_DELEGATOR, true}, Row),
    %% 结构断言复跑（"结构一致"锚点）
    ok = t_structure(Conn),
    fixture_cleanup(Conn),
    ok.

%% ===================================================================
%% 夹具（自建自清；replica 旁路仅用于测试清理）
%% ===================================================================

fixture_insert(Conn) ->
    {ok, 2} = epgsql:equery(
        Conn,
        "INSERT INTO \"user\" (id, account, password, reg_ip, reg_cosv, status) "
        "VALUES ($1,'ag31b_eunit_delegator','x','127.0.0.1','e2e',1), "
        "($2,'ag31b_eunit_agent','x','127.0.0.1','e2e',1)",
        [?ID_DELEGATOR, ?ID_AGENT]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE \"user\" SET account_type = 1 WHERE id = $1",
        [?ID_AGENT]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        "INSERT INTO organization (id, name, owner_id) VALUES ($1,'ag31b-eunit-org1',$3), "
        "($2,'ag31b-eunit-org2',$3)",
        [?ID_ORG1, ?ID_ORG2, ?ID_DELEGATOR]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        "INSERT INTO workspace (id, name, owner_id, organization_id) "
        "VALUES ($1,'ag31b-eunit-ws1',$3,$5), ($2,'ag31b-eunit-ws2',$3,$4)",
        [?ID_WS1, ?ID_WS2, ?ID_DELEGATOR, ?ID_ORG2, ?ID_ORG1]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant (id, agent_id, organization_id, delegator_user_id, "
        "workspace_scope_kind, status, valid_from, expires_at, version, idempotency_key) "
        "VALUES ($1,$2,$3,$4,'explicit','active', now() - interval '1 hour', "
        "now() + interval '1 day', 1, 'ag31b-idem-k1')",
        [?ID_GRANT, ?ID_AGENT, ?ID_ORG1, ?ID_DELEGATOR]
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

%% squery 成功形状：DML={ok, Count}；SET/无行语句={ok, [], []}
squery_ok(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        Other -> erlang:error({squery_failed, Sql, Other})
    end.

cleanup_stmts() ->
    EventIds = id_list([?ID_EVENT_ALT, ?ID_EVENT]),
    GrantIds = id_list([?ID_GRANT, ?ID_GRANT_ALT]),
    UserIds = id_list([?ID_DELEGATOR, ?ID_AGENT]),
    OrgIds = id_list([?ID_ORG1, ?ID_ORG2]),
    WsIds = id_list([?ID_WS1, ?ID_WS2]),
    [
        "DELETE FROM agent_grant_event WHERE id IN (" ++ EventIds ++ ")",
        "DELETE FROM agent_grant_capability WHERE grant_id IN (" ++ GrantIds ++ ")",
        "DELETE FROM agent_grant_workspace WHERE grant_id IN (" ++ GrantIds ++ ")",
        "DELETE FROM agent_grant WHERE id IN (" ++ GrantIds ++ ")",
        "DELETE FROM workspace WHERE id IN (" ++ WsIds ++ ")",
        "DELETE FROM organization WHERE id IN (" ++ OrgIds ++ ")",
        "DELETE FROM \"user\" WHERE id IN (" ++ UserIds ++ ")"
    ].

id_list(Ids) ->
    string:join([integer_to_list(I) || I <- Ids], ",").

%% ===================================================================
%% 断言辅助
%% ===================================================================

%% equery 出错时 `{error, Err} = epgsql:equery(...)` 解构出的 Err 即 #error{}
%% record；这里按其编译后 tuple 形状匹配
%%（epgsql.hrl：{error, Severity, Code, Codename, Message, Extra}），避免 include path 依赖。
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
    Sql =
        "SELECT indexname FROM pg_indexes WHERE indexname IN (" ++
            Placeholders ++ ")",
    {ok, _, Rows} = epgsql:equery(Conn, Sql, Names),
    Found = [N || {N} <- Rows],
    case Names -- Found of
        [] -> length(Names);
        Missing -> erlang:error({missing_indexes, Missing})
    end.

placeholders(Start, N) ->
    string:join(["$" ++ integer_to_list(I) || I <- lists:seq(Start, Start + N - 1)], ",").
