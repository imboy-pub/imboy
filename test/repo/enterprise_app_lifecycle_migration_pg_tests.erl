%% enterprise_app_lifecycle_migration_pg_tests
%% FULL-08 — Enterprise Application 生命周期治理 + Grant 撤销归因双通道
%% （迁移 00000143）真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL08_INTTEST）：
%%   空库全量迁移 up（erlang_migrate strict，含 00000143）→ schema oracle
%%   → 业务 oracle（CAS / 状态值域 / 跨 Org IDOR / 双 actor 互斥 / 撤销归因）
%%   → down 143（回到 142）→ 残留归零 + 生命周期收敛 + **既有 fn_% 函数体
%%     md5 逐条不变** → up 143 重建 → oracle 复现。
%%
%% oracle（plan-full §3.1 Application 生命周期 / §3.2 Grant / §7 安全硬门）：
%%   ⓪ head schema：version 列 + ck_ea_version + 四值 ck_ea_status +
%%      i_ea_org_created + revoked_by_adm_user_id + 双通道归因约束
%%   ① CAS：expected_version 命中才 +1；旧版本号 0 行；并发二写只有赢家
%%   ② 状态值域：draft/active/disabled/archived 合法，第五值非法（零 SQL 生效）
%%   ③ 跨 Org IDOR：以 A 组织的 org_id 更新 B 组织的应用 ⇒ 0 行
%%   ④ Grant 撤销归因：user 通道与 adm 通道**恰有一个**；皆空 / 皆非空都违规
%%   ⑤ down/up 对称：列/索引/约束残留归零，draft/archived 收敛为 disabled，
%%      非本域 fn_% 函数体 md5 集合不变
%%   ⑥ 重建后业务 oracle 全部复现（①-④ 在 up 回 143 后再跑一遍）
%%
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% ID 段：993xxx（本 run 独立 marker 库，跨套件不共享数据）。

-module(enterprise_app_lifecycle_migration_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(OWNER_A, 993001).
-define(PRIN_A, 993002).
-define(PRIN_B, 993003).
-define(TENANT_U, 993004).
-define(ORG_A, 993101).
-define(ORG_B, 993102).
-define(APP_A_ID, 993201).
-define(APP_B_ID, 993202).
-define(APP_DRAFT_ID, 993203).
-define(APP_ARCH_ID, 993204).
-define(GRANT_A1, 993301).
-define(GRANT_A2, 993302).
-define(GRANT_A3, 993303).
-define(ADM_USER, 993401).

%% 本套件被测迁移的版本号与回滚目标（显式表达，不写死步数）
-define(THIS_MIGRATION, 143).
-define(DOWN_TARGET, 142).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    State = inttest_marker_db:provision(#{
        env_prefix => <<"FULL08_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_fixture(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    State.

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

%% 迁移版本变更（down/up）用独立连接（erlang_migrate 需事务自治）。
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

seed_fixture(C) ->
    seed_user(C, ?OWNER_A),
    seed_user(C, ?PRIN_A),
    seed_user(C, ?PRIN_B),
    seed_user(C, ?TENANT_U),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"eal993-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_A, <<"eal993-org-b">>),
    seed_app(C, ?APP_A_ID, ?ORG_A, ?PRIN_A, <<"eal993-app-a">>, <<"active">>),
    seed_app(C, ?APP_B_ID, ?ORG_B, ?PRIN_B, <<"eal993-app-b">>, <<"active">>),
    seed_app(C, ?APP_DRAFT_ID, ?ORG_A, ?PRIN_A, <<"eal993-app-draft">>, <<"draft">>),
    seed_app(C, ?APP_ARCH_ID, ?ORG_A, ?PRIN_A, <<"eal993-app-arch">>, <<"archived">>),
    seed_grant(C, ?GRANT_A1, ?ORG_A, ?APP_A_ID, <<"eal993-idem-1">>),
    seed_grant(C, ?GRANT_A2, ?ORG_A, ?APP_A_ID, <<"eal993-idem-2">>),
    seed_grant(C, ?GRANT_A3, ?ORG_A, ?APP_A_ID, <<"eal993-idem-3">>),
    ok.

seed_user(C, Uid) ->
    ok = exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv)">>,
        <<" VALUES (">>,
        integer_to_binary(Uid),
        <<", 'x', 't993_u">>,
        integer_to_binary(Uid),
        <<"', 0, 1, '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid, Name) ->
    ok = exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings,">>,
        <<" created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        <<", '">>,
        Name,
        <<"', ">>,
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_app(C, AppId, OrgId, PrinUid, Key, Status) ->
    ok = exec(C, [
        <<"INSERT INTO enterprise_application (id, organization_id, principal_user_id,">>,
        <<" application_key, name, status) VALUES (">>,
        integer_to_binary(AppId),
        <<", ">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(PrinUid),
        <<", '">>,
        Key,
        <<"', 'eal993 ">>,
        Key,
        <<"', '">>,
        Status,
        <<"')">>
    ]).

seed_grant(C, GrantId, OrgId, AppId, Idem) ->
    ok = exec(C, [
        <<"INSERT INTO enterprise_application_grant (id, organization_id, application_id,">>,
        <<" workspace_scope_kind, status, valid_from, expires_at, idempotency_key) VALUES (">>,
        integer_to_binary(GrantId),
        <<", ">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(AppId),
        <<", 'none', 'active', NOW(), NOW() + interval '30 days', '">>,
        Idem,
        <<"')">>
    ]).

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
    ok = exec(C, <<"SAVEPOINT eal993_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT eal993_sp">>),
        exec(C, <<"RELEASE SAVEPOINT eal993_sp">>)
    end.

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

scalar(C, Sql, Params) ->
    Row = one(C, Sql, Params),
    case maps:values(Row) of
        [V | _] -> V;
        [] -> undefined
    end.

%% 受影响行数。elib_pg:query/3 会把 epgsql 的 Count 丢掉（只回 rows），
%% 故 CAS 的「命中=1 / 未命中=0」必须直接读 epgsql:equery/3 的 Count。
affected(C, Sql, Params) ->
    case epgsql:equery(C, iolist_to_binary(Sql), Params) of
        {ok, N} when is_integer(N) -> N;
        {ok, N, _Cols, _Rows} when is_integer(N) -> N;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

migration_head() ->
    {ok, Files} = file:list_dir("priv/migrations"),
    Versions = [
        list_to_integer(Ver)
     || F <- Files,
        {match, [Ver]} <- [re:run(F, "^(\\d{8})_.*\\.up\\.sql$", [{capture, all_but_first, list}])]
    ],
    ?assertNotEqual([], Versions),
    lists:max(Versions).

%% 裸 SQL 错误归一（与既有企业域套件同口径）
map_pg({ok, _} = Ok) ->
    Ok;
map_pg({error, #error{code = <<"23514">>, extra = Extra}}) ->
    {error, {check_violation, constraint_name(Extra)}};
map_pg({error, #error{code = <<"23505">>, extra = Extra}}) ->
    {error, {unique_violation, constraint_name(Extra)}};
map_pg({error, #error{code = <<"23503">>, extra = Extra}}) ->
    {error, {foreign_key_violation, constraint_name(Extra)}};
map_pg({error, Reason}) ->
    {error, Reason}.

constraint_name(Extra) when is_list(Extra) -> proplists:get_value(constraint_name, Extra);
constraint_name(_) -> undefined.

has_column(C, Table, Col) ->
    scalar(
        C,
        <<
            "SELECT count(*) FROM information_schema.columns"
            " WHERE table_name = $1 AND column_name = $2"
        >>,
        [Table, Col]
    ) > 0.

has_constraint(C, Name) ->
    scalar(
        C,
        <<"SELECT count(*) FROM pg_constraint WHERE conname = $1">>,
        [Name]
    ) > 0.

has_index(C, Idx) ->
    scalar(C, <<"SELECT count(*) FROM pg_indexes WHERE indexname = $1">>, [Idx]) > 0.

%% 既有 fn_% 函数体快照（本迁移不建函数，故全量比对）: name -> md5(prosrc)
fn_body_snapshot(C) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT p.proname AS name, md5(p.prosrc) AS body_md5 FROM pg_proc p"
                " JOIN pg_namespace n ON n.oid = p.pronamespace"
                " WHERE n.nspname = 'public' AND p.proname LIKE 'fn\\_%'"
                " ORDER BY p.proname"
            >>,
            []
        )
    of
        {ok, Rows} ->
            maps:from_list([{maps:get(<<"name">>, R), maps:get(<<"body_md5">>, R)} || R <- Rows]);
        {error, Reason} ->
            erlang:error({sql_error, Reason})
    end.

%%%===================================================================
%%% 套件入口
%%%===================================================================

all_test_() ->
    {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
        C = maps:get(conn, State),
        %% 串行：down_up_cycle 会整库改 schema，与其余用例并发会互相踩。
        {inorder, [
            {"head_schema_and_version_column", {timeout, 900, head_schema_test(C)}},
            {"cas_status_write_single_winner", with_tx(C, fun cas_status_oracle/1)},
            {"cas_scopes_write_single_winner", with_tx(C, fun cas_scopes_oracle/1)},
            {"status_domain_four_values_only", with_tx(C, fun status_domain_oracle/1)},
            {"cross_org_update_is_zero_rows", with_tx(C, fun cross_org_oracle/1)},
            {"grant_revoke_attribution_exactly_one_actor",
                with_tx(C, fun grant_attribution_oracle/1)},
            {"grant_revoke_both_actors_rejected", with_tx(C, fun grant_both_actors_oracle/1)},
            {"grant_revoke_no_actor_rejected", with_tx(C, fun grant_no_actor_oracle/1)},
            {"down_up_cycle_residue_zero_and_rebuild", {timeout, 900, down_up_cycle_test(State)}},
            {"down_with_adm_revoked_grant_converges",
                {timeout, 900, down_with_adm_revoked_test(State)}}
        ]}
    end}.

%%%===================================================================
%%% ⓪ head schema
%%%===================================================================

head_schema_test(C) ->
    ?_test(begin
        {ok, Version, Dirty} = erlang_migrate:version(#{conn => C, dir => "priv/migrations"}),
        ?assertEqual(false, Dirty),
        ?assertEqual(migration_head(), Version),
        %% head 动态（144/145 入列后 ≥ 本迁移）：本套件只钉「143 已应用」，不钉 head。
        ?assert(Version >= ?THIS_MIGRATION),
        %% Application：version 列 + 两条约束 + 列表索引
        ?assert(has_column(C, <<"enterprise_application">>, <<"version">>)),
        ?assert(has_constraint(C, <<"ck_ea_version">>)),
        ?assert(has_constraint(C, <<"ck_ea_status">>)),
        ?assert(has_index(C, <<"i_ea_org_created">>)),
        %% Grant：平台管理员归因列
        ?assert(
            has_column(C, <<"enterprise_application_grant">>, <<"revoked_by_adm_user_id">>)
        ),
        ?assert(has_constraint(C, <<"ck_eag_status_revoked_match">>)),
        %% 既有行被 DEFAULT 1 回填（夹具行在 143 之前写入，迁移时已存在）
        ?assertEqual(
            1,
            scalar(
                C,
                <<"SELECT version FROM enterprise_application WHERE id = $1">>,
                [?APP_A_ID]
            )
        )
    end).

%%%===================================================================
%%% ① CAS：expected_version 命中才 +1；旧版本 0 行
%%%===================================================================

cas_status_oracle(C) ->
    %% 起始 version=1
    ?assertEqual(
        1,
        scalar(C, <<"SELECT version FROM enterprise_application WHERE id = $1">>, [?APP_A_ID])
    ),
    %% 命中 CAS：status active -> disabled，version 1 -> 2
    ?assertEqual(
        1,
        affected(
            C,
            <<
                "UPDATE enterprise_application SET status = 'disabled',"
                " version = version + 1, updated_at = NOW()"
                " WHERE organization_id = $1 AND id = $2 AND version = $3"
            >>,
            [?ORG_A, ?APP_A_ID, 1]
        )
    ),
    ?assertEqual(<<"disabled">>, app_status(C, ?APP_A_ID)),
    ?assertEqual(2, app_version(C, ?APP_A_ID)),
    %% 旧版本号重放（并发输家）：0 行，且状态/版本不被覆盖
    ?assertEqual(
        0,
        affected(
            C,
            <<
                "UPDATE enterprise_application SET status = 'archived',"
                " version = version + 1, updated_at = NOW()"
                " WHERE organization_id = $1 AND id = $2 AND version = $3"
            >>,
            [?ORG_A, ?APP_A_ID, 1]
        )
    ),
    ?assertEqual(<<"disabled">>, app_status(C, ?APP_A_ID)),
    ?assertEqual(2, app_version(C, ?APP_A_ID)),
    %% 当前版本命中：disabled -> archived（终态）
    ?assertEqual(
        1,
        affected(
            C,
            <<
                "UPDATE enterprise_application SET status = 'archived',"
                " version = version + 1, updated_at = NOW()"
                " WHERE organization_id = $1 AND id = $2 AND version = $3"
            >>,
            [?ORG_A, ?APP_A_ID, 2]
        )
    ),
    ?assertEqual(3, app_version(C, ?APP_A_ID)).

cas_scopes_oracle(C) ->
    ?assertEqual(
        1,
        affected(
            C,
            <<
                "UPDATE enterprise_application SET allowed_scopes = $3::jsonb,"
                " version = version + 1, updated_at = NOW()"
                " WHERE organization_id = $1 AND id = $2 AND version = $4"
            >>,
            [?ORG_A, ?APP_A_ID, <<"[\"messages:send\"]">>, 1]
        )
    ),
    ?assertEqual(
        2,
        app_version(C, ?APP_A_ID)
    ),
    %% 重复提交（同 expected_version）：0 行，scope 不被二次写入
    ?assertEqual(
        0,
        affected(
            C,
            <<
                "UPDATE enterprise_application SET allowed_scopes = $3::jsonb,"
                " version = version + 1, updated_at = NOW()"
                " WHERE organization_id = $1 AND id = $2 AND version = $4"
            >>,
            [?ORG_A, ?APP_A_ID, <<"[\"identities:write\"]">>, 1]
        )
    ),
    %% jsonb 在 epgsql 侧是原文 text；用 ->>0 取首元素做等价断言
    ?assertEqual(
        <<"messages:send">>,
        scalar(
            C,
            <<"SELECT allowed_scopes ->> 0 FROM enterprise_application WHERE id = $1">>,
            [?APP_A_ID]
        )
    ).

%%%===================================================================
%%% ② 状态值域：四值合法，第五值非法
%%%===================================================================

status_domain_oracle(C) ->
    %% 四值全部合法（draft / archived 在夹具已写入，此处补 active / disabled 的往返）
    lists:foreach(
        fun(S) ->
            ?assertEqual(
                1,
                affected(
                    C,
                    <<
                        "UPDATE enterprise_application SET status = $3, version = version + 1"
                        " WHERE organization_id = $1 AND id = $2"
                    >>,
                    [?ORG_B, ?APP_B_ID, S]
                )
            )
        end,
        [<<"draft">>, <<"active">>, <<"disabled">>, <<"archived">>]
    ),
    %% 第五值非法：零 SQL 生效（不是「写了再回滚」）
    in_savepoint(C, fun() ->
        ?assertEqual(
            {error, {check_violation, <<"ck_ea_status">>}},
            map_pg(
                elib_pg:query(
                    C,
                    <<"UPDATE enterprise_application SET status = 'deleted' WHERE id = $1">>,
                    [?APP_A_ID]
                )
            )
        )
    end),
    ?assertNotEqual(<<"deleted">>, app_status(C, ?APP_A_ID)).

%%%===================================================================
%%% ③ 跨 Org IDOR：以 A 的 org_id 更新 B 的应用 ⇒ 0 行
%%%===================================================================

cross_org_oracle(C) ->
    Before = app_version(C, ?APP_B_ID),
    ?assertEqual(
        0,
        affected(
            C,
            <<
                "UPDATE enterprise_application SET status = 'archived', version = version + 1"
                " WHERE organization_id = $1 AND id = $2 AND version = $3"
            >>,
            [?ORG_A, ?APP_B_ID, Before]
        )
    ),
    %% B 组织的应用不受影响（其真实 org_id 才是唯一可写路径）
    ?assertEqual(Before, app_version(C, ?APP_B_ID)).

%%%===================================================================
%%% ④ Grant 撤销归因：user 通道 / adm 通道恰有一个
%%%===================================================================

grant_attribution_oracle(C) ->
    %% 平台管理员通道：adm 列非空、user 列为空 ⇒ 合法
    ?assertEqual(
        1,
        affected(
            C,
            <<
                "UPDATE enterprise_application_grant SET status = 'revoked',"
                " revoked_at = NOW(), revoked_by_adm_user_id = $4,"
                " version = version + 1, updated_at = NOW()"
                " WHERE organization_id = $1 AND application_id = $2 AND id = $3"
                " AND version = 1 AND status = 'active'"
            >>,
            [?ORG_A, ?APP_A_ID, ?GRANT_A1, ?ADM_USER]
        )
    ),
    Row = one(
        C,
        <<
            "SELECT status, revoked_by_user_id, revoked_by_adm_user_id, version"
            " FROM enterprise_application_grant WHERE id = $1"
        >>,
        [?GRANT_A1]
    ),
    ?assertEqual(<<"revoked">>, maps:get(<<"status">>, Row)),
    ?assertEqual(?ADM_USER, maps:get(<<"revoked_by_adm_user_id">>, Row)),
    ?assertEqual(null, maps:get(<<"revoked_by_user_id">>, Row)),
    ?assertEqual(2, maps:get(<<"version">>, Row)),
    %% 租户 user 通道仍然合法（既有行为不变）
    ?assertEqual(
        1,
        affected(
            C,
            <<
                "UPDATE enterprise_application_grant SET status = 'revoked',"
                " revoked_at = NOW(), revoked_by_user_id = $4,"
                " version = version + 1, updated_at = NOW()"
                " WHERE organization_id = $1 AND application_id = $2 AND id = $3"
                " AND version = 1 AND status = 'active'"
            >>,
            [?ORG_A, ?APP_A_ID, ?GRANT_A2, ?TENANT_U]
        )
    ),
    Row2 = one(
        C,
        <<"SELECT revoked_by_user_id, revoked_by_adm_user_id FROM enterprise_application_grant WHERE id = $1">>,
        [?GRANT_A2]
    ),
    ?assertEqual(?TENANT_U, maps:get(<<"revoked_by_user_id">>, Row2)),
    ?assertEqual(null, maps:get(<<"revoked_by_adm_user_id">>, Row2)).

grant_both_actors_oracle(C) ->
    %% 两个 actor 都非空 ⇒ 归因歧义，拒绝
    in_savepoint(C, fun() ->
        ?assertEqual(
            {error, {check_violation, <<"ck_eag_status_revoked_match">>}},
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "UPDATE enterprise_application_grant SET status = 'revoked',"
                        " revoked_at = NOW(), revoked_by_user_id = $2,"
                        " revoked_by_adm_user_id = $3"
                        " WHERE id = $1"
                    >>,
                    [?GRANT_A3, ?TENANT_U, ?ADM_USER]
                )
            )
        )
    end),
    ?assertEqual(<<"active">>, grant_status(C, ?GRANT_A3)).

grant_no_actor_oracle(C) ->
    %% 两个 actor 都空 ⇒ 无归因撤权，拒绝
    in_savepoint(C, fun() ->
        ?assertEqual(
            {error, {check_violation, <<"ck_eag_status_revoked_match">>}},
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "UPDATE enterprise_application_grant SET status = 'revoked',"
                        " revoked_at = NOW() WHERE id = $1"
                    >>,
                    [?GRANT_A3]
                )
            )
        )
    end),
    ?assertEqual(<<"active">>, grant_status(C, ?GRANT_A3)).

%%%===================================================================
%%% ⑤ down/up 对称回滚与重建
%%%===================================================================

down_up_cycle_test(State) ->
    C = maps:get(conn, State),
    ?_test(begin
        Before = fn_body_snapshot(C),
        MigConn = connect_marker(State),
        MigConfig = #{conn => MigConn, dir => "priv/migrations"},
        try
            %% --- down 到 142 ---
            ok = erlang_migrate:goto(MigConfig, ?DOWN_TARGET),
            ?assertEqual({ok, ?DOWN_TARGET, false}, erlang_migrate:version(MigConfig)),
            %% 残留归零
            ?assertNot(has_column(C, <<"enterprise_application">>, <<"version">>)),
            ?assertNot(has_constraint(C, <<"ck_ea_version">>)),
            ?assertNot(has_index(C, <<"i_ea_org_created">>)),
            ?assertNot(
                has_column(C, <<"enterprise_application_grant">>, <<"revoked_by_adm_user_id">>)
            ),
            %% 状态值域回到二值：draft/archived 被收敛为 disabled（安全方向）
            ?assertEqual(<<"disabled">>, app_status(C, ?APP_DRAFT_ID)),
            ?assertEqual(<<"disabled">>, app_status(C, ?APP_ARCH_ID)),
            ?assertEqual(<<"active">>, app_status(C, ?APP_B_ID)),
            %% 非本域函数体未被改写
            ?assertEqual(Before, fn_body_snapshot(C)),
            %% --- up 回 143 ---
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig)),
            %% head 动态：只要求 ≥ 本迁移（144/145 入列后 head 前移）。
            ?assert(migration_head() >= ?THIS_MIGRATION),
            %% 重建后对象复现
            ?assert(has_column(C, <<"enterprise_application">>, <<"version">>)),
            ?assert(has_constraint(C, <<"ck_ea_version">>)),
            ?assert(has_index(C, <<"i_ea_org_created">>)),
            ?assert(
                has_column(C, <<"enterprise_application_grant">>, <<"revoked_by_adm_user_id">>)
            ),
            ?assertEqual(Before, fn_body_snapshot(C)),
            %% 重建后业务 oracle 复现：CAS 命中 + 旧版本 0 行 + 跨 Org 0 行
            V = app_version(C, ?APP_A_ID),
            ?assertEqual(
                1,
                affected(
                    C,
                    <<
                        "UPDATE enterprise_application SET status = 'disabled',"
                        " version = version + 1"
                        " WHERE organization_id = $1 AND id = $2 AND version = $3"
                    >>,
                    [?ORG_A, ?APP_A_ID, V]
                )
            ),
            ?assertEqual(
                0,
                affected(
                    C,
                    <<
                        "UPDATE enterprise_application SET status = 'archived',"
                        " version = version + 1"
                        " WHERE organization_id = $1 AND id = $2 AND version = $3"
                    >>,
                    [?ORG_A, ?APP_A_ID, V]
                )
            ),
            ?assertEqual(
                0,
                affected(
                    C,
                    <<
                        "UPDATE enterprise_application SET status = 'archived',"
                        " version = version + 1"
                        " WHERE organization_id = $1 AND id = $2"
                    >>,
                    [?ORG_A, ?APP_B_ID]
                )
            )
        after
            try
                epgsql:close(MigConn)
            catch
                _:_ -> ok
            end
        end
    end).

%% A0-REV review 发现的 down 卡死场景回归钉：
%% adm 通道撤销行（revoked + user 列 NULL + adm 列非空，143 合法态）存在时，
%% down 的「归因收敛到 org owner」必须走通 —— 行保留、status 仍 revoked
%% （不复活授权）、归因=owner（139 窄约束可过），up 回 143 后保持。
down_with_adm_revoked_test(State) ->
    C = maps:get(conn, State),
    ?_test(begin
        MigConn = connect_marker(State),
        MigConfig = #{conn => MigConn, dir => "priv/migrations"},
        try
            %% 造 adm 通道撤销行：恰一个 actor（adm），user 列为空
            ?assertEqual(
                1,
                affected(
                    C,
                    <<
                        "UPDATE enterprise_application_grant"
                        " SET status = 'revoked', revoked_at = NOW(),"
                        " revoked_by_adm_user_id = 995001"
                        " WHERE id = $1"
                    >>,
                    [?GRANT_A1]
                )
            ),
            %% down 穿过 143：不再因 ADD 窄约束撞存量行而卡死
            ok = erlang_migrate:goto(MigConfig, ?DOWN_TARGET),
            ?assertEqual({ok, ?DOWN_TARGET, false}, erlang_migrate:version(MigConfig)),
            ?assertEqual(
                1,
                scalar(
                    C,
                    <<"SELECT count(*) FROM enterprise_application_grant WHERE id = $1">>,
                    [?GRANT_A1]
                )
            ),
            ?assertEqual(
                <<"revoked">>,
                scalar(
                    C,
                    <<"SELECT status FROM enterprise_application_grant WHERE id = $1">>,
                    [?GRANT_A1]
                )
            ),
            ?assertEqual(
                ?OWNER_A,
                scalar(
                    C,
                    <<
                        "SELECT revoked_by_user_id FROM enterprise_application_grant"
                        " WHERE id = $1"
                    >>,
                    [?GRANT_A1]
                )
            ),
            %% up 回 head：行与归因保持，adm 列恢复存在且为 NULL（未写入）
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig)),
            ?assertEqual(
                ?OWNER_A,
                scalar(
                    C,
                    <<
                        "SELECT revoked_by_user_id FROM enterprise_application_grant"
                        " WHERE id = $1"
                    >>,
                    [?GRANT_A1]
                )
            ),
            ?assertEqual(
                null,
                scalar(
                    C,
                    <<
                        "SELECT revoked_by_adm_user_id FROM enterprise_application_grant"
                        " WHERE id = $1"
                    >>,
                    [?GRANT_A1]
                )
            )
        after
            epgsql:close(MigConn)
        end
    end).

%%%===================================================================
%%% 读取助手
%%%===================================================================

app_status(C, Id) ->
    scalar(C, <<"SELECT status FROM enterprise_application WHERE id = $1">>, [Id]).

app_version(C, Id) ->
    scalar(C, <<"SELECT version FROM enterprise_application WHERE id = $1">>, [Id]).

grant_status(C, Id) ->
    scalar(C, <<"SELECT status FROM enterprise_application_grant WHERE id = $1">>, [Id]).
