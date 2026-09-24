%% enterprise_application_grant_pg_tests
%% FULL-01 — Enterprise Application Grant（迁移 00000139）真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL01_MIG_INTTEST）：
%%   空库全量迁移 up（erlang_migrate strict，含 00000139）→ schema oracle
%%   → 广州候选数据升级兼容（goto 136 → GZ 五表 fixture → up 139）
%%   → down 139 → 再次 up 139。
%% 业务用例每条 BEGIN ... ROLLBACK，不留数据；迁移版本变更（goto/down/up）
%% 用独立连接并排在最后（erlang_migrate 需要事务自治连接）。
%%
%% oracle 覆盖（plan-full §5 数据扩展 / §3.1 授权语义 / §7 安全硬门）：
%%   ⓪ 空库全量 up 至版本 139（3 表 + 2 只读视图 + 删除守卫函数就位）
%%   ① enterprise_application_grant：合法签发、幂等键唯一、validity / status
%%      一致性 CHECK、跨 Org application 引用 23503、未登记 scope 双层拒绝
%%   ② enterprise_application_grant_scope：固定 scope 枚举（wildcard /
%%      未登记值 / 大小写变体 / 空串一律 23514）、PK 去重
%%   ③ enterprise_application_grant_workspace：显式 workspace 行、
%%      kind 闸门（none 类型 Grant 挂 workspace 行 23503）、跨 Org workspace
%%      23503、workspace 物理删除被授权引用阻断（RESTRICT）
%%   ④ 授权行物理删除守卫：DELETE 一律 23514（撤销走 status）
%%   ⑤ effective 视图读时求值：未生效（valid_from 未来）/ 已过期 / 已撤销
%%      都不出现在视图内；撤销后同一事务内下一次读取即消失
%%   ⑥ 广州候选（GZ）数据升级兼容：GZ 五表 fixture 在 up 139 前写入，
%%      up 139 后数据逐字段不变，且可为**既有** GZ Application 签发/评估 Grant
%%   ⑦ down 139 → 三表 + 两视图 + 守卫函数全消、GZ 数据不变 → up 139 重建
%%      → 重建后约束/守卫/读面复验
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% ID 段：996xxx（本 run 独立 marker 库，跨套件不共享数据）。

-module(enterprise_application_grant_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(OWNER_A, 996001).
-define(OWNER_B, 996002).
-define(ORG_A, 996101).
-define(ORG_B, 996102).
-define(WS_A1, 996301).
-define(WS_A2, 996302).
-define(WS_B1, 996303).

-define(FAR_FUTURE, <<"2099-12-31T00:00:00+00:00">>).

-define(TABLES, [
    <<"enterprise_application_grant">>,
    <<"enterprise_application_grant_scope">>,
    <<"enterprise_application_grant_workspace">>
]).

-define(VIEWS, [
    <<"v_enterprise_effective_application_grant">>,
    <<"v_enterprise_effective_application_grant_scope">>
]).

-define(GZ_TABLES, [
    <<"enterprise_application">>,
    <<"enterprise_application_credential">>,
    <<"enterprise_external_identity">>,
    <<"enterprise_internal_idempotency">>,
    <<"enterprise_oa_sso_code">>
]).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    %% 直连模式未起 imboy 应用：本测试仅需 default TSID 生成器
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    inttest_marker_db:provision(#{
        env_prefix => <<"FULL01_MIG_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

%% 迁移版本变更（goto/down/up）用独立连接（erlang_migrate 需事务自治）。
connect_marker(State) ->
    #{host := Host, port := Port, username := User, password := Pass} = maps:get(server, State),
    {ok, Conn} =
        epgsql:connect(#{
            host => Host,
            port => Port,
            username => User,
            password => Pass,
            database => maps:get(db, State),
            timeout => 10000,
            codecs => [{epgsql_codec_rfc3339_bin, []}]
        }),
    Conn.

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

%% 负例包装：预期 SQL 错误会 abort 事务，在 SAVEPOINT 内执行并回退。
in_savepoint(C, Fun) ->
    ok = exec(C, <<"SAVEPOINT full01_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT full01_sp">>),
        exec(C, <<"RELEASE SAVEPOINT full01_sp">>)
    end.

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

foundation_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [
                {"empty_db_full_up_reaches_139", empty_db_full_up_test(C)},
                {"grant_row_constraints_and_cross_org_fk", with_tx(C, fun grant_row_oracle/1)},
                {"grant_scope_fixed_enum_and_pk", with_tx(C, fun grant_scope_oracle/1)},
                {"grant_workspace_kind_gate_and_org_boundary",
                    with_tx(C, fun grant_workspace_oracle/1)},
                {"grant_delete_guard_blocks_physical_delete",
                    with_tx(C, fun grant_delete_guard_oracle/1)},
                {"effective_views_read_time_evaluation", with_tx(C, fun effective_view_oracle/1)},
                {"gz_fixture_upgrade_compat_to_139", {timeout, 300, gz_upgrade_compat_test(State)}},
                {"migration_139_down_then_up_cycle", {timeout, 300, migration_cycle_test(State)}}
            ]
        end}}.

%% 当前迁移 head = priv/migrations 下 *.up.sql 的最大 8 位版本号
%% （与 test/repo/enterprise_internal_foundation_pg_tests 同款口径：只读文件
%% 系统、不查库，避免与断言目标同源）。
migration_head() ->
    {ok, Files} = file:list_dir("priv/migrations"),
    Versions = [
        list_to_integer(Ver)
     || F <- Files,
        {match, [Ver]} <- [re:run(F, "^(\\d{8})_.*\\.up\\.sql$", [{capture, all_but_first, list}])]
    ],
    ?assertNotEqual([], Versions),
    lists:max(Versions).

%%%===================================================================
%%% ⓪ 空库全量迁移 up
%%%===================================================================

empty_db_full_up_test(C) ->
    ?_test(begin
        {ok, Version, Dirty} = erlang_migrate:version(#{conn => C, dir => "priv/migrations"}),
        ?assertEqual(false, Dirty),
        %% FULL-02：断言目标从写死 139 改为「priv/migrations 在册最大版本」
        %% （head 前移的必然结果；与 foundation 套件 migration_head/0 同款口径，
        %% 全仓已因同类写死被打红过三次：EPGZ-08 cs_pg_widget / FULL-01 foundation）
        ?assertEqual(migration_head(), Version),
        lists:foreach(fun(T) -> ?assertNot(table_missing(C, T)) end, ?TABLES),
        lists:foreach(fun(V) -> ?assertNot(view_missing(C, V)) end, ?VIEWS),
        ?assertNot(function_missing(C, <<"fn_enterprise_application_grant_no_delete">>))
    end).

%%%===================================================================
%%% ① enterprise_application_grant（Grant 行约束）
%%%===================================================================

grant_row_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    AppB = maps:get(app_b, Fx),
    %% 合法签发：status=active、version=1、kind=none、scopes 落库
    {ok, Grant} = issue(C, ?ORG_A, AppA, <<"k-none">>, [<<"groups:write">>]),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Grant)),
    ?assertEqual(1, maps:get(<<"version">>, Grant)),
    ?assertEqual(<<"none">>, maps:get(<<"workspace_scope_kind">>, Grant)),
    ?assertEqual([<<"groups:write">>], maps:get(<<"scopes">>, Grant)),

    %% 幂等键在 (org, application) 内唯一 → key_conflict（不静默再发一份）
    ?assertEqual(
        {error, key_conflict},
        in_savepoint(C, fun() -> issue(C, ?ORG_A, AppA, <<"k-none">>, [<<"files:write">>]) end)
    ),

    %% validity CHECK：expires_at 必须晚于 valid_from
    ?assertMatch(
        {error, {check_violation, <<"ck_eag_validity">>}},
        in_savepoint(C, fun() ->
            raw_insert_grant(
                C, ?ORG_A, AppA, <<"k-bad-validity">>, <<"now()">>, <<"now() + interval '1 day'">>
            )
        end)
    ),

    %% status/revoked 一致性 CHECK：active 不允许带 revoked_at
    ?assertMatch(
        {error, {check_violation, <<"ck_eag_status_revoked_match">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO enterprise_application_grant (id, organization_id,"
                        " application_id, workspace_scope_kind, status, valid_from, expires_at,"
                        " revoked_at, idempotency_key) VALUES (996901, $1, $2, 'none', 'active',"
                        " CURRENT_TIMESTAMP, CURRENT_TIMESTAMP + interval '1 hour',"
                        " CURRENT_TIMESTAMP, 'k-status-mismatch')"
                    >>,
                    [?ORG_A, AppA]
                )
            )
        end)
    ),

    %% 跨 Org 的 application 引用被复合 FK 拒绝（Org B 引用 Org A 的 Application）；
    %% ops 层把 FK 归一为 application_not_found（不给存在性 oracle）
    ?assertEqual(
        {error, application_not_found},
        in_savepoint(C, fun() -> issue(C, ?ORG_B, AppA, <<"k-cross">>, [<<"groups:write">>]) end)
    ),

    %% 未登记 scope：ops 治理面先拦（invalid_scope），DB CHECK 兜底
    ?assertEqual(
        {error, invalid_scope},
        in_savepoint(C, fun() ->
            issue(C, ?ORG_A, AppA, <<"k-bad-scope">>, [<<"groups:bogus">>])
        end)
    ),
    ?assertMatch(
        {error, {check_violation, <<"ck_eags_scope_fixed">>}},
        in_savepoint(C, fun() ->
            repo_create(C, ?ORG_A, AppA, #{
                scopes => [<<"groups:bogus">>],
                idempotency_key => <<"k-db-bad-scope">>,
                expires_at => ?FAR_FUTURE
            })
        end)
    ),

    %% 受管判定基于「是否存在任何 Grant 行」（含 revoked/过期），另一 Application 不受影响
    ?assertEqual({ok, true}, grant_governed(C, ?ORG_A, AppA)),
    ?assertEqual({ok, false}, grant_governed(C, ?ORG_A, AppB)),
    %% 跨 Org 查询同一 AppId 不串号（Org 边界在 SQL 内强制）
    ?assertEqual({ok, false}, grant_governed(C, ?ORG_B, AppA)).

%%%===================================================================
%%% ② enterprise_application_grant_scope（固定 scope 枚举）
%%%===================================================================

grant_scope_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    {ok, Grant} = issue(C, ?ORG_A, AppA, <<"k-scope">>, [<<"application:read">>]),
    GrantId = maps:get(<<"id">>, Grant),

    %% 固定枚举恰 14 个（V2.1 冻结目录，迁移 144）；其余 13 个成员可逐条写入
    Fixed = enterprise_internal_scope:all(),
    ?assertEqual(14, length(Fixed)),
    Rest = [S || S <- Fixed, S =/= <<"application:read">>],
    ?assertEqual(13, length(Rest)),
    lists:foreach(
        fun(Scope) -> ?assertMatch({ok, _}, insert_scope_row(C, GrantId, Scope)) end,
        Rest
    ),
    {ok, GrantRow} = enterprise_application_grant_repo:find_tx(C, ?ORG_A, AppA, GrantId),
    ?assertEqual(lists:sort(Fixed), maps:get(<<"scopes">>, GrantRow)),

    %% wildcard / 未登记值 / 大小写变体 / 空串一律 23514（授权表无法被写成通配）
    BadScopes = [<<"*">>, <<"groups:writeX">>, <<"GROUPS:WRITE">>, <<>>, <<"messages:send ">>],
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, {check_violation, <<"ck_eags_scope_fixed">>}},
                in_savepoint(C, fun() -> insert_scope_row(C, GrantId, Bad) end)
            )
        end,
        BadScopes
    ),

    %% 主键去重：同一 (grant, scope) 二次插入 23505
    ?assertMatch(
        {error, {unique_violation, <<"pk_enterprise_application_grant_scope">>}},
        in_savepoint(C, fun() -> insert_scope_row(C, GrantId, <<"application:read">>) end)
    ).

%%%===================================================================
%%% ③ enterprise_application_grant_workspace（显式 Workspace Grant）
%%%===================================================================

grant_workspace_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    %% explicit 类型：显式 workspace 行可插入（同 Org），聚合读回有序
    {ok, GrantWs} =
        issue_ws(C, ?ORG_A, AppA, <<"k-ws">>, [<<"files:write">>], [?WS_A1, ?WS_A2]),
    WsId = maps:get(<<"id">>, GrantWs),
    ?assertEqual([?WS_A1, ?WS_A2], maps:get(<<"workspace_ids">>, GrantWs)),

    %% kind 闸门：none 类型 Grant 挂 workspace 行 → 三列复合 FK 拒绝
    {ok, GrantNone} = issue(C, ?ORG_A, AppA, <<"k-none-gate">>, [<<"groups:write">>]),
    NoneId = maps:get(<<"id">>, GrantNone),
    ?assertMatch(
        {error, {foreign_key_violation, <<"fk_eagw_grant">>}},
        in_savepoint(C, fun() -> insert_ws_row(C, ?ORG_A, NoneId, ?WS_A1) end)
    ),

    %% 跨 Org workspace 引用 → 23503（Org A 的 Grant 引用 Org B 的 workspace）
    ?assertMatch(
        {error, {foreign_key_violation, <<"fk_eagw_workspace">>}},
        in_savepoint(C, fun() -> insert_ws_row(C, ?ORG_A, WsId, ?WS_B1) end)
    ),
    ?assertEqual(
        {error, workspace_not_found},
        in_savepoint(C, fun() ->
            insert_ws_via_ops(C, ?ORG_A, AppA, <<"k-ws-cross-ops">>, [?WS_B1])
        end)
    ),

    %% ops 层语义拒绝：explicit 必须非空 / none 必须为空
    ?assertEqual(
        {error, invalid_workspaces},
        issue_ws(C, ?ORG_A, AppA, <<"k-ws-empty">>, [<<"files:write">>], [])
    ),
    ?assertEqual(
        {error, invalid_workspaces},
        repo_create(C, ?ORG_A, AppA, #{
            scopes => [<<"files:write">>],
            workspace_scope_kind => none,
            workspace_ids => [?WS_A1],
            idempotency_key => <<"k-none-with-ws">>,
            expires_at => ?FAR_FUTURE
        })
    ),

    %% workspace 被授权引用时不可物理删除（RESTRICT）
    ?assertEqual(
        {error, {restrict_violation, <<"fk_eagw_workspace">>}},
        in_savepoint(C, fun() ->
            map_pg(elib_pg:query(C, <<"DELETE FROM workspace WHERE id = $1">>, [?WS_A1]))
        end)
    ).

%%%===================================================================
%%% ④ 授权行物理删除守卫
%%%===================================================================

grant_delete_guard_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    {ok, Grant} = issue(C, ?ORG_A, AppA, <<"k-delete">>, [<<"groups:write">>]),
    GrantId = maps:get(<<"id">>, Grant),
    %% 删除守卫：唯一合法路径是 status=revoked（保留 lineage，撤权即时生效）
    ?assertEqual(
        {error, {check_violation, <<"trg_enterprise_application_grant_no_delete">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C, <<"DELETE FROM enterprise_application_grant WHERE id = $1">>, [GrantId]
                )
            )
        end)
    ),
    %% 行仍在，且仍可撤销（守卫不阻断合法生命周期）
    ?assertEqual(ok, revoke(C, ?ORG_A, AppA, GrantId, 1, ?OWNER_A)),
    ?assertMatch(
        {ok, [#{<<"status">> := <<"revoked">>, <<"version">> := 2} | _]},
        elib_pg:query(
            C,
            <<"SELECT status, version FROM enterprise_application_grant WHERE id = $1">>,
            [GrantId]
        )
    ),
    %% 撤销后仍受管（禁止删除 ⇒ 永不退回更宽的未受管边界）
    ?assertEqual({ok, true}, grant_governed(C, ?ORG_A, AppA)).

%%%===================================================================
%%% ⑤ effective 视图（读时求值 / 撤权即时生效）
%%%===================================================================

effective_view_oracle(C) ->
    Fx = seed_fixture(C),
    AppA = maps:get(app_a, Fx),
    {ok, Active} = issue(C, ?ORG_A, AppA, <<"k-eff-active">>, [<<"groups:write">>]),
    ActiveId = maps:get(<<"id">>, Active),
    {ok, Future} =
        repo_create(C, ?ORG_A, AppA, #{
            scopes => [<<"files:write">>],
            idempotency_key => <<"k-eff-future">>,
            valid_from => <<"2099-01-01T00:00:00+00:00">>,
            expires_at => <<"2099-06-30T00:00:00+00:00">>
        }),
    {ok, Expired} =
        repo_create(C, ?ORG_A, AppA, #{
            scopes => [<<"identities:read">>],
            idempotency_key => <<"k-eff-expired">>,
            valid_from => <<"2020-01-01T00:00:00+00:00">>,
            expires_at => <<"2020-12-31T00:00:00+00:00">>
        }),
    {ok, RevokedGrant} = issue(C, ?ORG_A, AppA, <<"k-eff-revoked">>, [<<"webhooks:manage">>]),
    RevokedId = maps:get(<<"id">>, RevokedGrant),
    ?assertEqual(ok, revoke(C, ?ORG_A, AppA, RevokedId, 1, ?OWNER_A)),
    %% 四条 Grant 的 id 互不相同（夹具自检）
    ?assertEqual(
        4,
        length(
            lists:usort([
                ActiveId,
                maps:get(<<"id">>, Future),
                maps:get(<<"id">>, Expired),
                RevokedId
            ])
        )
    ),

    %% 只有「active 且在有效期内」的 Grant 出现在 effective 视图
    {ok, Effective} = enterprise_application_grant_repo:effective_grants_tx(C, ?ORG_A, AppA),
    ?assertEqual([ActiveId], [maps:get(<<"grant_id">>, G) || G <- Effective]),
    {ok, EffectiveScopes} = enterprise_application_grant_repo:effective_scopes_tx(C, ?ORG_A, AppA),
    ?assertEqual([<<"groups:write">>], EffectiveScopes),

    %% 撤权即时生效：同一事务内改状态，下一次读取即消失（无缓存、无后台任务）
    ?assertEqual(ok, revoke(C, ?ORG_A, AppA, ActiveId, 1, ?OWNER_A)),
    ?assertEqual({ok, []}, enterprise_application_grant_repo:effective_grants_tx(C, ?ORG_A, AppA)),
    ?assertEqual({ok, []}, enterprise_application_grant_repo:effective_scopes_tx(C, ?ORG_A, AppA)),
    %% 受管状态不回退
    ?assertEqual({ok, true}, grant_governed(C, ?ORG_A, AppA)).

%%%===================================================================
%%% ⑥ 广州候选（GZ）数据升级兼容
%%%===================================================================

gz_upgrade_compat_test(State) ->
    ?_test(gz_upgrade_compat(State)).

gz_upgrade_compat(State) ->
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    Conn = connect_marker(State),
    try
        MigConfig = #{conn => Conn, dir => "priv/migrations", strict => true},
        %% 1) 回到 GZ 候选 head（136）：空库全量 up 至 136
        ok = erlang_migrate:goto(MigConfig, 136),
        ?assertMatch({ok, 136, false}, erlang_migrate:version(MigConfig)),
        lists:foreach(fun(T) -> ?assertNot(table_missing(Conn, T)) end, ?GZ_TABLES),
        ?assert(table_missing(Conn, <<"enterprise_application_grant">>)),

        %% 2) 写入 GZ 期形态数据（模拟广州候选已有数据；提交，供后续用例复用）
        ok = exec(Conn, <<"BEGIN">>),
        _Fx = seed_fixture(Conn),
        {ok, _GzApp} = enterprise_application_repo:create_tx(
            Conn, ?ORG_A, <<"gz-oa">>, <<"gz"/utf8>>
        ),
        %% GZ 期既有 Application（与夹具的 t996-app-a 并列存在）
        {ok, GzAppRow} = enterprise_application_repo:find_by_key_tx(Conn, ?ORG_A, <<"gz-oa">>),
        GzAppId = maps:get(<<"id">>, GzAppRow),
        ok = exec(Conn, [
            <<"INSERT INTO enterprise_application_credential (id, organization_id,">>,
            <<" application_id, credential_prefix, secret_digest, status, created_at)">>,
            <<" VALUES (996401, ">>,
            integer_to_binary(?ORG_A),
            <<", ">>,
            integer_to_binary(GzAppId),
            <<", 'ib_int_gz1', repeat('a', 64), 'active', CURRENT_TIMESTAMP)">>
        ]),
        ok = exec(Conn, <<"COMMIT">>),

        %% 3) 升级到当前 head（139 起；FULL-02 追加 140，故断言改为 head 推导）
        ok = erlang_migrate:up(MigConfig),
        Head = migration_head(),
        ?assertMatch({ok, Head, false}, erlang_migrate:version(MigConfig)),
        lists:foreach(fun(T) -> ?assertNot(table_missing(Conn, T)) end, ?TABLES),
        lists:foreach(fun(V) -> ?assertNot(view_missing(Conn, V)) end, ?VIEWS),

        %% 4) GZ 数据逐字段不变（升级不触碰既有数据）
        {ok, [CredRow | _]} = elib_pg:query(
            Conn,
            <<
                "SELECT application_id, credential_prefix, status FROM"
                " enterprise_application_credential WHERE id = 996401"
            >>,
            []
        ),
        ?assertEqual(GzAppId, maps:get(<<"application_id">>, CredRow)),
        ?assertEqual(<<"ib_int_gz1">>, maps:get(<<"credential_prefix">>, CredRow)),
        ?assertEqual(<<"active">>, maps:get(<<"status">>, CredRow)),
        {ok, [OrgRow | _]} = elib_pg:query(
            Conn, <<"SELECT name, status FROM organization WHERE id = $1">>, [?ORG_A]
        ),
        ?assertEqual(<<"t996_org_a">>, maps:get(<<"name">>, OrgRow)),

        %% 5) 既有 GZ Application 可正常签发/评估 Grant（新表与既有数据兼容）
        ok = exec(Conn, <<"BEGIN">>),
        {ok, Grant} =
            issue_ws(Conn, ?ORG_A, GzAppId, <<"k-gz-upgrade">>, [<<"groups:write">>], [?WS_A1]),
        ?assertEqual(<<"active">>, maps:get(<<"status">>, Grant)),
        ?assertEqual(
            {ok, true},
            enterprise_application_grant_repo:workspace_covered_tx(
                Conn, ?ORG_A, GzAppId, ?WS_A1, <<"groups:write">>
            )
        ),
        ?assertEqual(
            {ok, false},
            enterprise_application_grant_repo:workspace_covered_tx(
                Conn, ?ORG_A, GzAppId, ?WS_A2, <<"groups:write">>
            )
        ),
        ok = exec(Conn, <<"ROLLBACK">>)
    after
        epgsql:close(Conn)
    end.

%%%===================================================================
%%% ⑦ migration 139 down/up 循环
%%%===================================================================

migration_cycle_test(State) ->
    ?_test(migration_cycle(State)).

migration_cycle(State) ->
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    Conn = connect_marker(State),
    try
        MigConfig = #{conn => Conn, dir => "priv/migrations", strict => true},
        {ok, AppA} = enterprise_application_repo:find_by_key_tx(Conn, ?ORG_A, <<"t996-app-a">>),
        AppAId = maps:get(<<"id">>, AppA),

        %% 回滚 139：三表 + 两视图 + 守卫函数全部消失，版本回到 136，
        %% 广州期数据（五表 + organization + workspace）不受影响。
        %% FULL-02：从 `down 1` 改为 `goto 136`（意图是「退到 139 之前」；
        %% 140 入库后 `down 1` 只回滚 140，断言会错位——与 FULL-01 对
        %% foundation 套件 migration_cycle 的同款修法一致）
        ok = erlang_migrate:goto(MigConfig, 136),
        ?assertMatch({ok, 136, false}, erlang_migrate:version(MigConfig)),
        lists:foreach(fun(T) -> ?assert(table_missing(Conn, T)) end, ?TABLES),
        lists:foreach(fun(V) -> ?assert(view_missing(Conn, V)) end, ?VIEWS),
        ?assert(function_missing(Conn, <<"fn_enterprise_application_grant_no_delete">>)),
        lists:foreach(fun(T) -> ?assertNot(table_missing(Conn, T)) end, ?GZ_TABLES),
        {ok, [_ | _]} = elib_pg:query(
            Conn, <<"SELECT id FROM organization WHERE id = $1">>, [?ORG_A]
        ),

        %% 再次全量 up：三表与视图重建，版本回到当前 head
        ok = erlang_migrate:up(MigConfig),
        Head2 = migration_head(),
        ?assertMatch({ok, Head2, false}, erlang_migrate:version(MigConfig)),
        lists:foreach(fun(T) -> ?assertNot(table_missing(Conn, T)) end, ?TABLES),
        lists:foreach(fun(V) -> ?assertNot(view_missing(Conn, V)) end, ?VIEWS),
        ?assertNot(function_missing(Conn, <<"fn_enterprise_application_grant_no_delete">>)),

        %% 重建后 oracle 复验：约束 / 守卫 / 读面仍在
        ok = exec(Conn, <<"BEGIN">>),
        {ok, Grant} = issue(Conn, ?ORG_A, AppAId, <<"k-after-rebuild">>, [<<"groups:write">>]),
        GrantId = maps:get(<<"id">>, Grant),
        ?assertMatch(
            {error, {check_violation, <<"ck_eags_scope_fixed">>}},
            in_savepoint(Conn, fun() -> insert_scope_row(Conn, GrantId, <<"*">>) end)
        ),
        ?assertEqual(
            {error, {check_violation, <<"trg_enterprise_application_grant_no_delete">>}},
            in_savepoint(Conn, fun() ->
                map_pg(
                    elib_pg:query(
                        Conn,
                        <<"DELETE FROM enterprise_application_grant WHERE id = $1">>,
                        [GrantId]
                    )
                )
            end)
        ),
        ?assertEqual(
            {ok, [<<"groups:write">>]},
            enterprise_application_grant_repo:effective_scopes_tx(Conn, ?ORG_A, AppAId)
        ),
        ok = exec(Conn, <<"ROLLBACK">>)
    after
        epgsql:close(Conn)
    end.

%%%===================================================================
%%% 夹具与辅助
%%%===================================================================

%% 双 Org + 双 Application + 三 workspace（A1/A2 ∈ ORG_A，B1 ∈ ORG_B）。
%% 返回应用 id（TSID 由 repo 生成，不硬编码）。
seed_fixture(C) ->
    ok = seed_user(C, ?OWNER_A, <<"t996_owner_a">>),
    ok = seed_user(C, ?OWNER_B, <<"t996_owner_b">>),
    ok = seed_org(C, ?ORG_A, <<"t996_org_a">>, ?OWNER_A),
    ok = seed_org(C, ?ORG_B, <<"t996_org_b">>, ?OWNER_B),
    {ok, AppA} = enterprise_application_repo:create_tx(C, ?ORG_A, <<"t996-app-a">>, <<"a"/utf8>>),
    {ok, AppB} = enterprise_application_repo:create_tx(C, ?ORG_B, <<"t996-app-b">>, <<"b"/utf8>>),
    ok = seed_ws(C, ?WS_A1, <<"t996-ws-a1">>, ?ORG_A),
    ok = seed_ws(C, ?WS_A2, <<"t996-ws-a2">>, ?ORG_A),
    ok = seed_ws(C, ?WS_B1, <<"t996-ws-b1">>, ?ORG_B),
    #{
        app_a => maps:get(<<"id">>, AppA),
        app_b => maps:get(<<"id">>, AppB),
        app_a_key => <<"t996-app-a">>
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

%% 经 ops 治理面签发（稳定 API，非裸 SQL）
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

insert_ws_via_ops(C, OrgId, AppId, IdemKey, Workspaces) ->
    enterprise_internal_ops:issue_grant_tx(C, OrgId, AppId, #{
        scopes => [<<"files:write">>],
        workspace_scope_kind => explicit,
        workspace_ids => Workspaces,
        idempotency_key => IdemKey,
        expires_at => ?FAR_FUTURE
    }).

repo_create(C, OrgId, AppId, Spec) ->
    enterprise_application_grant_repo:create_tx(C, OrgId, AppId, Spec).

revoke(C, OrgId, AppId, GrantId, ExpectedVersion, RevokedBy) ->
    enterprise_internal_ops:revoke_grant_tx(C, OrgId, AppId, GrantId, ExpectedVersion, RevokedBy).

grant_governed(C, OrgId, AppId) ->
    enterprise_application_grant_repo:grant_governed_tx(C, OrgId, AppId).

%% 裸 INSERT（绕开 ops 校验，用于 DB 层负例）
raw_insert_grant(C, OrgId, AppId, IdemKey, ExpiresAtSql, ValidFromSql) ->
    Sql =
        iolist_to_binary([
            <<"INSERT INTO enterprise_application_grant (id, organization_id, application_id,">>,
            <<" workspace_scope_kind, status, valid_from, expires_at, idempotency_key)">>,
            <<" VALUES (">>,
            integer_to_binary(enterprise_application_grant_repo:next_id()),
            <<", ">>,
            integer_to_binary(OrgId),
            <<", ">>,
            integer_to_binary(AppId),
            <<", 'none', 'active', ">>,
            ValidFromSql,
            <<", ">>,
            ExpiresAtSql,
            <<", '">>,
            IdemKey,
            <<"')">>
        ]),
    map_pg(elib_pg:query(C, Sql, [])).

insert_scope_row(C, GrantId, Scope) ->
    map_pg_simple(
        elib_pg:query(
            C,
            <<"INSERT INTO enterprise_application_grant_scope (grant_id, scope) VALUES ($1, $2)">>,
            [GrantId, Scope]
        )
    ).

insert_ws_row(C, OrgId, GrantId, WsId) ->
    map_pg_simple(
        elib_pg:query(
            C,
            <<
                "INSERT INTO enterprise_application_grant_workspace"
                " (organization_id, grant_id, workspace_id) VALUES ($1, $2, $3)"
            >>,
            [OrgId, GrantId, WsId]
        )
    ).

%% repo 已在内部归一错误；此两处为裸 SQL 断言路径，归一化保持一致口径。
map_pg_simple({ok, _} = Ok) ->
    Ok;
map_pg_simple({error, #error{code = <<"23514">>, extra = Extra}}) ->
    {error, {check_violation, constraint_name(Extra)}};
map_pg_simple({error, #error{code = <<"23505">>, extra = Extra}}) ->
    {error, {unique_violation, constraint_name(Extra)}};
map_pg_simple({error, #error{code = <<"23503">>, extra = Extra}}) ->
    {error, {foreign_key_violation, constraint_name(Extra)}};
map_pg_simple({error, #error{code = <<"23001">>, extra = Extra}}) ->
    {error, {restrict_violation, constraint_name(Extra)}};
map_pg_simple({error, Reason}) ->
    {error, Reason}.

map_pg({error, #error{code = <<"23514">>, extra = Extra}}) ->
    {error, {check_violation, constraint_name(Extra)}};
map_pg({error, #error{code = <<"23503">>, extra = Extra}}) ->
    {error, {foreign_key_violation, constraint_name(Extra)}};
map_pg({error, #error{code = <<"23505">>, extra = Extra}}) ->
    {error, {unique_violation, constraint_name(Extra)}};
map_pg({error, #error{code = <<"23001">>, extra = Extra}}) ->
    {error, {restrict_violation, constraint_name(Extra)}};
map_pg(Other) ->
    Other.

constraint_name(Extra) when is_list(Extra) -> proplists:get_value(constraint_name, Extra);
constraint_name(_Extra) -> undefined.

table_missing(C, Table) ->
    bool(C, <<"SELECT to_regclass('public.", Table/binary, "') IS NULL AS missing">>).

view_missing(C, View) ->
    bool(C, <<"SELECT to_regclass('public.", View/binary, "') IS NULL AS missing">>).

function_missing(C, Function) ->
    bool(C, <<"SELECT to_regprocedure('public.", Function/binary, "()') IS NULL AS missing">>).

bool(C, Sql) ->
    case elib_pg:query(C, Sql, []) of
        {ok, [#{<<"missing">> := True}]} -> True;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.
