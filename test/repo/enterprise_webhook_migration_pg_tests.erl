%% enterprise_webhook_migration_pg_tests
%% FULL-03 — Enterprise Webhook 投递账本（迁移 00000141）真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL03_MIG_INTTEST）：
%%   空库全量迁移 up（erlang_migrate strict，含 00000141）→ schema/守卫 oracle
%%   → down 141 → 残留归零 + **既有 fn_% 函数体 md5 逐条不变**（迁移自证的外部复核）
%%   → 广州期形态（140，无 owner 列）eapp 行升级兼容 + backfill
%%   → up 141 重建。
%%
%% oracle（plan-full §5 数据扩展 / §3.1 账本 / §7 安全硬门）：
%%   ⓪ head schema：新列/约束/索引/守卫函数/触发器就位
%%   ① 账本守卫（企业行）：终态不可回退、终态行冻结、快照不可变、版本守卫独占、
%%      attempt_count 单调、owner 三元一致、跨 Org owner 23503、在途重放唯一
%%   ② **bot 域行零行为变化**（非 eapp 行不受守卫管辖：dead->pending 手工重放仍可、
%%      ewh_* 列保持 NULL/0、ledger_version 恒 1）
%%   ③ claim：租约前推 + ewh_claimed_at 打点 + 二次 claim 空集 + 真双连接并发唯一
%%   ④ down/up 对称回滚与重建；迁移自证的外部复核（非 ewh 函数体 md5 集合不变）
%%   ⑤ 广州期数据升级兼容：无 owner 列的既有 eapp 行被 backfill，且随后受守卫管辖
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% ID 段：994xxx（本 run 独立 marker 库，跨套件不共享数据）。

-module(enterprise_webhook_migration_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(OWNER_A, 994001).
-define(PRIN_A, 994002).
-define(PRIN_B, 994003).
-define(ORG_A, 994101).
-define(ORG_B, 994102).
-define(APP_A_ID, 994201).
-define(APP_B_ID, 994202).
-define(LEGACY_DID, <<"ewh994-legacy">>).
-define(BOT_ROW_DID, <<"ewh994-bot">>).
-define(NEW_COLUMNS, [
    <<"ewh_owner_organization_id">>,
    <<"ewh_owner_application_id">>,
    <<"ewh_replay_of">>,
    <<"ewh_endpoint_generation">>,
    <<"ewh_ledger_version">>,
    <<"ewh_claimed_at">>
]).
-define(NEW_CONSTRAINTS, [
    <<"ck_ewh_delivery_owner_pair">>,
    <<"ck_ewh_delivery_ledger_version">>,
    <<"ck_ewh_delivery_endpoint_generation">>,
    <<"ck_ewh_delivery_replay_not_self">>,
    <<"fk_ewh_delivery_owner">>
]).
-define(NEW_INDEXES, [
    <<"bot_delivery_ewh_owner_idx">>,
    <<"uq_ewh_delivery_replay_inflight">>
]).

%% 本套件被测迁移的版本号（FULL-06 起回滚目标用 goto 显式表达，不写死步数）
-define(EWH_MIGRATION_VERSION, 141).
-define(EWH_DOWN_TARGET, 140).

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
        env_prefix => <<"FULL03_MIG_INTTEST">>,
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
        inttest_marker_db:safe_connect(#{
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
    seed_org(C, ?ORG_A, ?OWNER_A, <<"ewh994-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_A, <<"ewh994-org-b">>),
    ok = exec(C, [
        <<"INSERT INTO enterprise_application (id, organization_id, principal_user_id,">>,
        <<" application_key, name) VALUES (">>,
        integer_to_binary(?APP_A_ID),
        <<", ">>,
        integer_to_binary(?ORG_A),
        <<", ">>,
        integer_to_binary(?PRIN_A),
        <<", 'ewh994-app-a', 'ewh994 app a')">>
    ]),
    ok = exec(C, [
        <<"INSERT INTO enterprise_application (id, organization_id, principal_user_id,">>,
        <<" application_key, name) VALUES (">>,
        integer_to_binary(?APP_B_ID),
        <<", ">>,
        integer_to_binary(?ORG_B),
        <<", ">>,
        integer_to_binary(?PRIN_B),
        <<", 'ewh994-app-b', 'ewh994 app b')">>
    ]),
    %% 两个 principal 的 bot 行（企业 endpoint 配置载体）
    seed_bot(C, ?PRIN_A, <<"eapp_ewh994-app-a">>),
    seed_bot(C, ?PRIN_B, <<"eapp_ewh994-app-b">>),
    %% 企业投递行（在途）+ bot 域投递行（纯数字 bot_id）
    insert_enterprise_delivery(C, ?LEGACY_DID, ?PRIN_A, ?ORG_A, ?APP_A_ID, <<"pending">>),
    ok = exec(C, [
        <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
        <<" correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip)">>,
        <<" VALUES ('ewh994-bot', '994501', 'message', '{}', 'corr994bot00000001',">>,
        <<" 'ewh994-bot-idem', 'https://bot.example/', 'bot.example', '93.184.216.34')">>
    ]),
    ok.

seed_user(C, Uid) ->
    ok = exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv)">>,
        <<" VALUES (">>,
        integer_to_binary(Uid),
        <<", 'x', 't994_u">>,
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

seed_bot(C, PrincipalUid, Username) ->
    ok = exec(C, [
        <<"INSERT INTO bot (user_id, name, username, owner_uid, webhook_url, events,">>,
        <<" is_public, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(PrincipalUid),
        <<", 'ewh994', '">>,
        Username,
        <<"', ">>,
        integer_to_binary(PrincipalUid),
        <<", 'https://oa.example.com/hook', '[]'::jsonb, false, 1, NOW(), NOW())">>
    ]).

insert_enterprise_delivery(C, Did, PrincipalUid, OrgId, AppId, Status) ->
    ok = exec(C, [
        <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
        <<" correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,">>,
        <<" status, ewh_owner_organization_id, ewh_owner_application_id) VALUES ('">>,
        Did,
        <<"', 'eapp:">>,
        integer_to_binary(PrincipalUid),
        <<"', 'file.confirmed', '{\"event_id\":\"evt-994\"}'::jsonb, 'corr994ewh0000001',">>,
        <<" 'ewh994-idem-">>,
        Did,
        <<"', 'https://oa.example.com/hook', 'oa.example.com', '93.184.216.34', '">>,
        Status,
        <<"', ">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(AppId),
        <<")">>
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
    ok = exec(C, <<"SAVEPOINT ewh994_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT ewh994_sp">>),
        exec(C, <<"RELEASE SAVEPOINT ewh994_sp">>)
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

query_rows(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} when is_list(Rows) -> Rows;
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

%% 迁移前后既有 fn_% 函数体快照（排除本域 fn_ewh_%）：name -> md5(prosrc)
fn_body_snapshot(C) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT p.proname AS name, md5(p.prosrc) AS body_md5 FROM pg_proc p"
                " JOIN pg_namespace n ON n.oid = p.pronamespace"
                " WHERE n.nspname = 'public' AND p.proname LIKE 'fn\\_%'"
                " AND p.proname NOT LIKE 'fn\\_ewh\\_%' ORDER BY p.proname"
            >>,
            []
        )
    of
        {ok, Rows} ->
            maps:from_list([{maps:get(<<"name">>, R), maps:get(<<"body_md5">>, R)} || R <- Rows]);
        {error, Reason} ->
            erlang:error({sql_error, Reason})
    end.

%% 函数/触发器/索引/列存在性
has_function(C, Fn) ->
    scalar(
        C,
        <<"SELECT to_regprocedure('public.", Fn/binary, "()') IS NOT NULL AS present">>,
        []
    ).

has_trigger(C, Trg) ->
    scalar(C, <<"SELECT count(*) FROM pg_trigger WHERE tgname = $1">>, [Trg]) > 0.

has_index(C, Idx) ->
    scalar(
        C,
        <<"SELECT count(*) FROM pg_indexes WHERE indexname = $1">>,
        [Idx]
    ) > 0.

has_column(C, Col) ->
    scalar(
        C,
        <<
            "SELECT count(*) FROM information_schema.columns"
            " WHERE table_name = 'bot_delivery' AND column_name = $1"
        >>,
        [Col]
    ) > 0.

has_bot_column(C, Col) ->
    scalar(
        C,
        <<
            "SELECT count(*) FROM information_schema.columns"
            " WHERE table_name = 'bot' AND column_name = $1"
        >>,
        [Col]
    ) > 0.

has_constraint(C, Name) ->
    scalar(
        C,
        <<
            "SELECT count(*) FROM pg_constraint"
            " WHERE conrelid = 'bot_delivery'::regclass AND conname = $1"
        >>,
        [Name]
    ) > 0.

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_webhook_migration_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            {inorder, [
                {"empty_db_full_up_reaches_head_with_ledger_schema", head_schema_test(C)},
                {"ledger_guard_terminal_and_snapshot", with_tx(C, fun ledger_guard_oracle/1)},
                {"ledger_guard_ownership_and_replay_unique",
                    with_tx(C, fun ledger_ownership_oracle/1)},
                {"bot_domain_rows_unaffected_by_guard", with_tx(C, fun bot_domain_oracle/1)},
                {"claim_lease_and_claimed_at", with_tx(C, fun claim_oracle/1)},
                {"concurrent_claim_single_winner", {timeout, 120, concurrent_claim_test(State)}},
                {"migration_down_residue_zero_and_fn_md5_unchanged",
                    {timeout, 400, down_up_cycle_test(State)}},
                {"gz_era_row_upgrade_compat_backfill_and_guard",
                    {timeout, 400, gz_upgrade_compat_test(State)}}
            ]}
        end}}.

%%%===================================================================
%%% ⓪ head schema
%%%===================================================================

head_schema_test(C) ->
    ?_test(begin
        {ok, Version, Dirty} = erlang_migrate:version(#{
            conn => C, dir => "priv/migrations"
        }),
        ?assertEqual(false, Dirty),
        ?assertEqual(migration_head(), Version),
        lists:foreach(fun(Col) -> ?assert(has_column(C, Col)) end, ?NEW_COLUMNS),
        lists:foreach(fun(N) -> ?assert(has_constraint(C, N)) end, ?NEW_CONSTRAINTS),
        lists:foreach(fun(I) -> ?assert(has_index(C, I)) end, ?NEW_INDEXES),
        ?assert(has_function(C, <<"fn_ewh_delivery_guard">>)),
        ?assert(has_trigger(C, <<"trg_ewh_delivery_guard">>)),
        ?assert(has_bot_column(C, <<"ewh_endpoint_generation">>)),
        %% 本域函数名集合封闭（只有 fn_ewh_delivery_guard 一个 ewh 函数）
        ?assertEqual(
            [<<"fn_ewh_delivery_guard">>],
            [
                maps:get(<<"proname">>, R)
             || R <- query_rows(
                    C,
                    <<
                        "SELECT proname FROM pg_proc p JOIN pg_namespace n"
                        " ON n.oid = p.pronamespace WHERE n.nspname = 'public'"
                        " AND proname LIKE 'fn\\_ewh\\_%' ORDER BY 1"
                    >>,
                    []
                )
            ]
        )
    end).

%%%===================================================================
%%% ① 账本守卫：终态 / 快照 / 版本 / attempt 单调
%%%===================================================================

ledger_guard_oracle(C) ->
    Did = ?LEGACY_DID,
    %% 起始态（夹具写入）：pending / version 1 / 无认领
    Start = one(
        C,
        <<
            "SELECT status, ewh_ledger_version, ewh_claimed_at, ewh_endpoint_generation,"
            " ewh_replay_of FROM bot_delivery WHERE delivery_id = $1"
        >>,
        [Did]
    ),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Start)),
    ?assertEqual(1, maps:get(<<"ewh_ledger_version">>, Start)),
    ?assertEqual(null, maps:get(<<"ewh_claimed_at">>, Start)),

    %% 合法：pending -> retry（记 attempt 1）
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'retry', attempt_count = 1, ewh_ledger_version = 999,">>,
        <<" next_retry_at = NOW() + INTERVAL '5 seconds' WHERE delivery_id = '">>,
        Did,
        <<"'">>
    ]),
    AfterRetry = one(
        C,
        <<"SELECT status, attempt_count, ewh_ledger_version FROM bot_delivery WHERE delivery_id = $1">>,
        [Did]
    ),
    ?assertEqual(<<"retry">>, maps:get(<<"status">>, AfterRetry)),
    %% 版本守卫独占：客户端传 999 被覆盖为 OLD+1 = 2
    ?assertEqual(2, maps:get(<<"ewh_ledger_version">>, AfterRetry)),

    %% 合法：retry -> success（终态）
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'success', attempt_count = 1 WHERE delivery_id = '">>,
        Did,
        <<"'">>
    ]),
    Terminal = one(
        C,
        <<"SELECT status, ewh_ledger_version FROM bot_delivery WHERE delivery_id = $1">>,
        [Did]
    ),
    ?assertEqual(3, maps:get(<<"ewh_ledger_version">>, Terminal)),

    %% 终态不可回退：success -> dead / retry / pending 一律 23514
    lists:foreach(
        fun(Target) ->
            ?assertEqual(
                {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
                in_savepoint(C, fun() ->
                    map_pg(
                        elib_pg:query(
                            C,
                            <<"UPDATE bot_delivery SET status = $1 WHERE delivery_id = $2">>,
                            [Target, Did]
                        )
                    )
                end)
            )
        end,
        [<<"dead">>, <<"retry">>, <<"pending">>]
    ),

    %% 终态行冻结：attempt_count / next_retry_at 也不得再动
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "UPDATE bot_delivery SET attempt_count = attempt_count + 1"
                        " WHERE delivery_id = $1"
                    >>,
                    [Did]
                )
            )
        end)
    ),

    %% 端点快照不可变：URL / host / pin / payload / ownership / 代际 / created_at
    lists:foreach(
        fun({Col, Val}) ->
            ?assertEqual(
                {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
                in_savepoint(C, fun() ->
                    map_pg(
                        elib_pg:query(
                            C,
                            iolist_to_binary([
                                <<"UPDATE bot_delivery SET ">>,
                                Col,
                                <<" = ">>,
                                Val,
                                <<" WHERE delivery_id = '">>,
                                Did,
                                <<"'">>
                            ]),
                            []
                        )
                    )
                end),
                {col, Col}
            )
        end,
        [
            {<<"webhook_url">>, <<"'https://evil.example/'">>},
            {<<"webhook_host">>, <<"'evil.example'">>},
            {<<"pinned_ip">>, <<"'10.0.0.1'">>},
            {<<"payload">>, <<"'{\"event_id\":\"tampered\"}'::jsonb">>},
            {<<"ewh_owner_organization_id">>, integer_to_binary(?ORG_B)},
            {<<"ewh_owner_application_id">>, integer_to_binary(?APP_B_ID)},
            {<<"ewh_endpoint_generation">>, <<"7">>},
            {<<"ewh_replay_of">>, <<"'other-delivery'">>}
        ]
    ),

    %% attempt_count 不可倒退（在途行）
    ok = exec(
        C, [
            <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
            <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status,">>,
            <<"attempt_count, ewh_owner_organization_id, ewh_owner_application_id)">>,
            <<"VALUES ('ewh994-monotonic', 'eapp:994002', 'file.confirmed', '{}',">>,
            <<"'corr994mono0000001', 'ewh994-mono-idem', 'https://oa.example.com/hook',">>,
            <<"'oa.example.com', '93.184.216.34', 'pending', 3, 994101, 994201)">>
        ]
    ),
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "UPDATE bot_delivery SET attempt_count = 2"
                        " WHERE delivery_id = 'ewh994-monotonic'"
                    >>,
                    []
                )
            )
        end)
    ),
    %% 企业行必须生而 pending / 必须带 owner
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " status, ewh_owner_organization_id, ewh_owner_application_id)"
                        " VALUES ('ewh994-born-success', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994born0000001', 'ewh994-born-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 'success', 994101, 994201)"
                    >>,
                    []
                )
            )
        end)
    ),
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip)"
                        " VALUES ('ewh994-no-owner', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994noown0000001', 'ewh994-no-owner-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34')"
                    >>,
                    []
                )
            )
        end)
    ).

%%%===================================================================
%%% ①b ownership 三元一致 / 跨 Org FK / 在途重放唯一
%%%===================================================================

ledger_ownership_oracle(C) ->
    %% 跨 Org owner（Org A 的 bot 前缀 + Org B 的 application）→ 三元一致拒绝
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " ewh_owner_organization_id, ewh_owner_application_id)"
                        " VALUES ('ewh994-cross-owner', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994cross000001', 'ewh994-cross-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 994102, 994202)"
                    >>,
                    []
                )
            )
        end)
    ),
    %% 不存在的 application：守卫三元一致先拦（预期）。FK 是**第二道**防线
    %% ——暂时禁用守卫触发器即可看到复合 FK 自己拒绝（23503）。
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " ewh_owner_organization_id, ewh_owner_application_id)"
                        " VALUES ('ewh994-no-app', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994noapp000001', 'ewh994-no-app-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 994101, 994999)"
                    >>,
                    []
                )
            )
        end)
    ),
    ?assertEqual(
        {error, {foreign_key_violation, <<"fk_ewh_delivery_owner">>}},
        in_savepoint(C, fun() ->
            ok = exec(
                C, <<"ALTER TABLE bot_delivery DISABLE TRIGGER trg_ewh_delivery_guard">>
            ),
            R = map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " ewh_owner_organization_id, ewh_owner_application_id)"
                        " VALUES ('ewh994-fk-only', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994fkonly00001', 'ewh994-fk-only-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 994101, 994999)"
                    >>,
                    []
                )
            ),
            %% 失败语句已 abort 事务：先回滚到 savepoint，再恢复触发器。
            %% （DISABLE TRIGGER 本身是事务性 DDL，savepoint 回滚也会撤销它——
            %%  这里的 ENABLE 是显式双保险。）
            ok = exec(C, <<"ROLLBACK TO SAVEPOINT ewh994_sp">>),
            ok = exec(
                C, <<"ALTER TABLE bot_delivery ENABLE TRIGGER trg_ewh_delivery_guard">>
            ),
            R
        end)
    ),
    %% owner 成对 CHECK：只给 org 不给 app。守卫先拦（owner 必填），
    %% 成对 CHECK 是**第二道**（禁用触发器即可看到它自己拒绝）。
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " ewh_owner_organization_id)"
                        " VALUES ('ewh994-half-owner', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994half0000001', 'ewh994-half-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 994101)"
                    >>,
                    []
                )
            )
        end)
    ),
    ?assertEqual(
        {error, {check_violation, <<"ck_ewh_delivery_owner_pair">>}},
        in_savepoint(C, fun() ->
            ok = exec(
                C, <<"ALTER TABLE bot_delivery DISABLE TRIGGER trg_ewh_delivery_guard">>
            ),
            R = map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " ewh_owner_organization_id)"
                        " VALUES ('ewh994-half-owner2', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994half2000001', 'ewh994-half2-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 994101)"
                    >>,
                    []
                )
            ),
            ok = exec(C, <<"ROLLBACK TO SAVEPOINT ewh994_sp">>),
            ok = exec(
                C, <<"ALTER TABLE bot_delivery ENABLE TRIGGER trg_ewh_delivery_guard">>
            ),
            R
        end)
    ),
    %% 重放自指拒绝：守卫 INSERT 分支先拦，表级 CHECK 是第二道
    ?assertEqual(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " ewh_owner_organization_id, ewh_owner_application_id, ewh_replay_of)"
                        " VALUES ('ewh994-self', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994self0000001', 'ewh994-self-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 994101, 994201, 'ewh994-self')"
                    >>,
                    []
                )
            )
        end)
    ),
    ?assertEqual(
        {error, {check_violation, <<"ck_ewh_delivery_replay_not_self">>}},
        in_savepoint(C, fun() ->
            ok = exec(
                C, <<"ALTER TABLE bot_delivery DISABLE TRIGGER trg_ewh_delivery_guard">>
            ),
            R = map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " ewh_owner_organization_id, ewh_owner_application_id, ewh_replay_of)"
                        " VALUES ('ewh994-self2', 'eapp:994002', 'file.confirmed', '{}',"
                        " 'corr994self2000001', 'ewh994-self2-idem', 'https://oa.example.com/',"
                        " 'oa.example.com', '93.184.216.34', 994101, 994201, 'ewh994-self2')"
                    >>,
                    []
                )
            ),
            ok = exec(C, <<"ROLLBACK TO SAVEPOINT ewh994_sp">>),
            ok = exec(
                C, <<"ALTER TABLE bot_delivery ENABLE TRIGGER trg_ewh_delivery_guard">>
            ),
            R
        end)
    ),
    %% 在途重放唯一：同一原行两条 pending 重放 → 第二次 23505
    ok = exec(
        C, [
            <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
            <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status,">>,
            <<"ewh_owner_organization_id, ewh_owner_application_id, ewh_replay_of)">>,
            <<"VALUES ('ewh994-replay-1', 'eapp:994002', 'file.confirmed',">>,
            <<"'{\"event_id\":\"evt-994\"}'::jsonb, 'corr994rp10000001', 'ewh994-rp1-idem',">>,
            <<"'https://oa.example.com/hook', 'oa.example.com', '93.184.216.34', 'pending',">>,
            <<"994101, 994201, 'ewh994-legacy')">>
        ]
    ),
    ?assertEqual(
        {error, {unique_violation, <<"uq_ewh_delivery_replay_inflight">>}},
        in_savepoint(C, fun() ->
            map_pg(
                elib_pg:query(
                    C,
                    <<
                        "INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,"
                        " correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,"
                        " status, ewh_owner_organization_id, ewh_owner_application_id,"
                        " ewh_replay_of) VALUES ('ewh994-replay-2', 'eapp:994002',"
                        " 'file.confirmed', '{\"event_id\":\"evt-994\"}'::jsonb,"
                        " 'corr994rp20000001', 'ewh994-rp2-idem', 'https://oa.example.com/hook',"
                        " 'oa.example.com', '93.184.216.34', 'pending', 994101, 994201,"
                        " 'ewh994-legacy')"
                    >>,
                    []
                )
            )
        end)
    ),
    %% 原行终结后可再重放（唯一索引是**在途**限定）
    ok = exec(
        C,
        <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = 'ewh994-replay-1'">>
    ),
    ok = exec(
        C, [
            <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
            <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status,">>,
            <<"ewh_owner_organization_id, ewh_owner_application_id, ewh_replay_of)">>,
            <<"VALUES ('ewh994-replay-3', 'eapp:994002', 'file.confirmed',">>,
            <<"'{\"event_id\":\"evt-994\"}'::jsonb, 'corr994rp30000001', 'ewh994-rp3-idem',">>,
            <<"'https://oa.example.com/hook', 'oa.example.com', '93.184.216.34', 'pending',">>,
            <<"994101, 994201, 'ewh994-legacy')">>
        ]
    ),
    ?assertEqual(
        1,
        scalar(
            C,
            <<
                "SELECT count(*) FROM bot_delivery WHERE ewh_replay_of = 'ewh994-legacy'"
                " AND status IN ('pending','retry')"
            >>,
            []
        )
    ).

%%%===================================================================
%%% ② bot 域行零行为变化
%%%===================================================================

bot_domain_oracle(C) ->
    Did = ?BOT_ROW_DID,
    %% bot 域行的新列保持 NULL / 0（既不回填也不打点）
    Before = one(
        C,
        <<
            "SELECT ewh_owner_organization_id, ewh_owner_application_id, ewh_ledger_version,"
            " ewh_claimed_at, ewh_endpoint_generation, ewh_replay_of, status"
            " FROM bot_delivery WHERE delivery_id = $1"
        >>,
        [Did]
    ),
    ?assertEqual(null, maps:get(<<"ewh_owner_organization_id">>, Before)),
    ?assertEqual(null, maps:get(<<"ewh_owner_application_id">>, Before)),
    ?assertEqual(1, maps:get(<<"ewh_ledger_version">>, Before)),

    %% 既有 bot 生命周期（pending -> retry -> success）不受守卫影响
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'retry', attempt_count = 1,">>,
        <<" next_retry_at = NOW() + INTERVAL '5 seconds' WHERE delivery_id = '">>,
        Did,
        <<"'">>
    ]),
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '">>,
        Did,
        <<"'">>
    ]),
    %% bot 管理面手工重放语义：dead -> pending **仍然允许**（企业守卫只看 eapp 行）
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'pending', next_retry_at = NOW()">>,
        <<" WHERE delivery_id = '">>,
        Did,
        <<"'">>
    ]),
    After = one(
        C,
        <<
            "SELECT status, ewh_ledger_version, ewh_claimed_at, ewh_owner_application_id"
            " FROM bot_delivery WHERE delivery_id = $1"
        >>,
        [Did]
    ),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, After)),
    %% 版本号不被守卫改写（非企业行完全不经守卫逻辑）
    ?assertEqual(1, maps:get(<<"ewh_ledger_version">>, After)),
    ?assertEqual(null, maps:get(<<"ewh_claimed_at">>, After)),
    %% bot 域行的快照列同样不被守卫约束（守卫不越界）
    ok = exec(C, [
        <<"UPDATE bot_delivery SET webhook_url = 'https://bot2.example/', webhook_host = 'bot2.example'">>,
        <<" WHERE delivery_id = '">>,
        Did,
        <<"'">>
    ]),
    ?assertEqual(
        <<"https://bot2.example/">>,
        scalar(C, <<"SELECT webhook_url FROM bot_delivery WHERE delivery_id = $1">>, [Did])
    ).

%%%===================================================================
%%% ③ claim：租约 / 打点 / 二次空集
%%%===================================================================

claim_oracle(C) ->
    Did = <<"ewh994-claim">>,
    ok = exec(
        C, [
            <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
            <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status,">>,
            <<"next_retry_at, ewh_owner_organization_id, ewh_owner_application_id)">>,
            <<"VALUES ('ewh994-claim', 'eapp:994002', 'file.confirmed', '{}', 'corr994claim000001',">>,
            <<"'ewh994-claim-idem', 'https://oa.example.com/hook', 'oa.example.com',">>,
            <<"'93.184.216.34', 'pending', NOW() - INTERVAL '1 minute', 994101, 994201)">>
        ]
    ),
    {ok, Rows} = bot_webhook_delivery_repo:claim_due_tx(C, 5),
    ClaimedRows = [R || R <- Rows, maps:get(<<"delivery_id">>, R) =:= Did],
    ?assertEqual(1, length(ClaimedRows)),
    [Row] = ClaimedRows,
    ?assertEqual(?ORG_A, maps:get(<<"ewh_owner_organization_id">>, Row)),
    ?assertEqual(?APP_A_ID, maps:get(<<"ewh_owner_application_id">>, Row)),
    Claimed = one(
        C,
        <<
            "SELECT status, ewh_claimed_at, next_retry_at > NOW() AS leased,"
            " ewh_ledger_version FROM bot_delivery WHERE delivery_id = $1"
        >>,
        [Did]
    ),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Claimed)),
    ?assertEqual(true, maps:get(<<"leased">>, Claimed)),
    ?assertNotEqual(null, maps:get(<<"ewh_claimed_at">>, Claimed)),
    %% 认领也是一次账本写（版本 +1）
    ?assertEqual(2, maps:get(<<"ewh_ledger_version">>, Claimed)),
    %% 同一事务内二次 claim：租约已在未来 → 本行不可能被再次认领
    {ok, Again} = bot_webhook_delivery_repo:claim_due_tx(C, 10),
    ?assertEqual([], [R || R <- Again, maps:get(<<"delivery_id">>, R) =:= Did]),
    %% bot 域行即使被认领也不打 ewh_claimed_at（守卫不越界）
    ok = exec(
        C, [
            <<"UPDATE bot_delivery SET status = 'pending',">>,
            <<"next_retry_at = NOW() - INTERVAL '1 minute' WHERE delivery_id = 'ewh994-bot'">>
        ]
    ),
    {ok, Rows2} = bot_webhook_delivery_repo:claim_due_tx(C, 10),
    ?assert(lists:any(fun(R) -> maps:get(<<"delivery_id">>, R) =:= <<"ewh994-bot">> end, Rows2)),
    ?assertEqual(
        null,
        scalar(C, <<"SELECT ewh_claimed_at FROM bot_delivery WHERE delivery_id = $1">>, [
            <<"ewh994-bot">>
        ])
    ).

%%%===================================================================
%%% ③b 真并发 claim：唯一认领者
%%%===================================================================

concurrent_claim_test(State) ->
    ?_test(begin
        C2 = connect_marker(State),
        C3 = connect_marker(State),
        C1 = maps:get(conn, State),
        try
            Did = <<"ewh994-conc">>,
            ok = exec(C1, <<"BEGIN">>),
            ok = exec(
                C1, [
                    <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
                    <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status,">>,
                    <<"next_retry_at, ewh_owner_organization_id, ewh_owner_application_id)">>,
                    <<"VALUES ('ewh994-conc', 'eapp:994002', 'file.confirmed', '{}',">>,
                    <<"'corr994conc0000001', 'ewh994-conc-idem', 'https://oa.example.com/hook',">>,
                    <<"'oa.example.com', '93.184.216.34', 'pending', NOW() - INTERVAL '1 minute',">>,
                    <<"994101, 994201)">>
                ]
            ),
            %% T1 认领并**持有事务**（行锁未释放）
            {ok, Rows1} = bot_webhook_delivery_repo:claim_due_tx(C1, 5),
            ?assertEqual(1, length([R || R <- Rows1, maps:get(<<"delivery_id">>, R) =:= Did])),

            %% T2 并发认领：FOR UPDATE SKIP LOCKED 跳过被锁行 → 本行不在结果里
            ok = exec(C2, <<"BEGIN">>),
            {ok, Rows2} = bot_webhook_delivery_repo:claim_due_tx(C2, 5),
            ?assertEqual([], [R || R <- Rows2, maps:get(<<"delivery_id">>, R) =:= Did]),
            ok = exec(C2, <<"COMMIT">>),

            %% T1 提交（租约生效）→ T3 再认领：本行仍不在结果里（next_retry_at 已在未来）
            ok = exec(C1, <<"COMMIT">>),
            ok = exec(C3, <<"BEGIN">>),
            {ok, Rows3} = bot_webhook_delivery_repo:claim_due_tx(C3, 5),
            ?assertEqual([], [R || R <- Rows3, maps:get(<<"delivery_id">>, R) =:= Did]),
            ok = exec(C3, <<"COMMIT">>),

            %% 唯一认领者：该行在整段并发里只被认领过一次（版本只 +1）
            ok = exec(C1, <<"BEGIN">>),
            Row = one(
                C1,
                <<
                    "SELECT ewh_ledger_version, ewh_claimed_at IS NOT NULL AS claimed"
                    " FROM bot_delivery WHERE delivery_id = $1"
                >>,
                [Did]
            ),
            ?assertEqual(2, maps:get(<<"ewh_ledger_version">>, Row)),
            ?assertEqual(true, maps:get(<<"claimed">>, Row))
        after
            _ = exec_quiet(C1, <<"ROLLBACK">>),
            epgsql:close(C2),
            epgsql:close(C3)
        end
    end).

%%%===================================================================
%%% ④ down 对称回滚 + 迁移自证的外部复核（fn 体 md5）
%%%===================================================================

down_up_cycle_test(State) ->
    ?_test(begin
        Conn = connect_marker(State),
        try
            MigConfig = #{conn => Conn, dir => "priv/migrations", strict => true},
            Before = fn_body_snapshot(Conn),
            ?assert(length(maps:keys(Before)) > 20),

            %% 回滚到 141 之前（= 140）：只回滚本迁移（00000141）的对象。
            %% FULL-06 口径更新：目标版本用 goto 显式表达，不再写死 `down 1`——
            %% head 一变，写死的步数就会回滚到别的迁移上，本套件的字段断言随即恒红
            %% （与 GZ 期 cs_pg_widget_tests 写死 head 的缺陷同类，同一修法；见
            %% GZ_CANDIDATE.md §5「cs_pg_widget_tests 写死迁移 head」）。
            ?assertEqual(141, ?EWH_MIGRATION_VERSION),
            ok = erlang_migrate:goto(MigConfig, ?EWH_DOWN_TARGET),
            ?assertEqual({ok, ?EWH_DOWN_TARGET, false}, erlang_migrate:version(MigConfig)),
            lists:foreach(fun(Col) -> ?assertNot(has_column(Conn, Col)) end, ?NEW_COLUMNS),
            lists:foreach(fun(N) -> ?assertNot(has_constraint(Conn, N)) end, ?NEW_CONSTRAINTS),
            lists:foreach(fun(I) -> ?assertNot(has_index(Conn, I)) end, ?NEW_INDEXES),
            ?assertNot(has_function(Conn, <<"fn_ewh_delivery_guard">>)),
            ?assertNot(has_trigger(Conn, <<"trg_ewh_delivery_guard">>)),
            ?assertNot(has_bot_column(Conn, <<"ewh_endpoint_generation">>)),
            %% 既有对象（bot_delivery 本体列 + bot 域既有列）全在
            lists:foreach(
                fun(Col) ->
                    ?assert(
                        scalar(
                            Conn,
                            <<
                                "SELECT count(*) FROM information_schema.columns"
                                " WHERE table_name = 'bot_delivery' AND column_name = $1"
                            >>,
                            [Col]
                        ) > 0
                    )
                end,
                [<<"delivery_id">>, <<"bot_id">>, <<"payload">>, <<"webhook_url">>, <<"pinned_ip">>]
            ),

            %% up 141：重建
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig)),
            lists:foreach(fun(Col) -> ?assert(has_column(Conn, Col)) end, ?NEW_COLUMNS),
            ?assert(has_function(Conn, <<"fn_ewh_delivery_guard">>)),
            ?assert(has_trigger(Conn, <<"trg_ewh_delivery_guard">>)),

            %% 迁移自证的外部复核：非 ewh 的既有 fn_% 函数体 md5 逐条不变
            After = fn_body_snapshot(Conn),
            ?assertEqual(maps:keys(Before), maps:keys(After)),
            Mismatch = [K || K <- maps:keys(Before), maps:get(K, Before) =/= maps:get(K, After)],
            ?assertEqual([], Mismatch),
            %% 二次 up 幂等（无 pending 迁移即 no-op）
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig))
        after
            epgsql:close(Conn)
        end
    end).

%%%===================================================================
%%% ⑤ 广州期（140）形态数据升级兼容
%%%===================================================================

gz_upgrade_compat_test(State) ->
    ?_test(begin
        Conn = connect_marker(State),
        try
            MigConfig = #{conn => Conn, dir => "priv/migrations", strict => true},
            %% 退到 141 之前（140）：owner / 代际 / 版本列都不存在
            ok = erlang_migrate:goto(MigConfig, 140),
            ?assertMatch({ok, 140, false}, erlang_migrate:version(MigConfig)),
            ?assertNot(has_column(Conn, <<"ewh_owner_application_id">>)),

            %% 写入「广州期形态」的企业投递行（无 owner 列）+ 其 bot/app 链
            ok = exec(Conn, <<"BEGIN">>),
            ok = exec(
                Conn, [
                    <<"INSERT INTO \"user\" (id, password, account, account_type, status,">>,
                    <<"reg_ip, reg_cosv) VALUES (994004, 'x', 't994_u994004', 0, 1, '127.0.0.1', 'x')">>
                ]
            ),
            ok = exec(
                Conn, [
                    <<"INSERT INTO enterprise_application (id, organization_id,">>,
                    <<"principal_user_id, application_key, name) VALUES (994203, 994101, 994004,">>,
                    <<"'ewh994-legacy-app', 'ewh994 legacy app')">>
                ]
            ),
            ok = exec(
                Conn, [
                    <<"INSERT INTO bot (user_id, name, username, owner_uid, webhook_url,">>,
                    <<"events, is_public, status, created_at, updated_at) VALUES (994004, 'ewh994',">>,
                    <<"'eapp_ewh994-legacy-app', 994004, 'https://oa.example.com/hook', '[]'::jsonb,">>,
                    <<"false, 1, NOW(), NOW())">>
                ]
            ),
            ok = exec(
                Conn, [
                    <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
                    <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status,">>,
                    <<"attempt_count) VALUES ('ewh994-gz-dead', 'eapp:994004', 'file.confirmed',">>,
                    <<"'{\"event_id\":\"evt-994-gz\"}'::jsonb, 'corr994gz000000001', 'ewh994-gz-idem',">>,
                    <<"'https://oa.example.com/hook', 'oa.example.com', '93.184.216.34', 'dead', 4)">>
                ]
            ),
            ok = exec(Conn, <<"COMMIT">>),

            %% 升级到 head：owner 回填 + 守卫就位
            ok = erlang_migrate:up(MigConfig),
            ?assertEqual({ok, migration_head(), false}, erlang_migrate:version(MigConfig)),
            Backfilled = one(
                Conn,
                <<
                    "SELECT status, attempt_count, ewh_owner_organization_id,"
                    " ewh_owner_application_id, ewh_ledger_version FROM bot_delivery"
                    " WHERE delivery_id = 'ewh994-gz-dead'"
                >>,
                []
            ),
            ?assertEqual(994101, maps:get(<<"ewh_owner_organization_id">>, Backfilled)),
            ?assertEqual(994203, maps:get(<<"ewh_owner_application_id">>, Backfilled)),
            ?assertEqual(1, maps:get(<<"ewh_ledger_version">>, Backfilled)),
            %% 历史数据本身逐字段不变（升级不重写业务列）
            ?assertEqual(<<"dead">>, maps:get(<<"status">>, Backfilled)),
            ?assertEqual(4, maps:get(<<"attempt_count">>, Backfilled)),

            %% 升级后的历史行受守卫管辖：终态不可回退、快照不可变
            %% （savepoint 需要一个活动事务：显式 BEGIN，末尾 ROLLBACK）
            ok = exec(Conn, <<"BEGIN">>),
            ?assertEqual(
                {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
                in_savepoint(Conn, fun() ->
                    map_pg(
                        elib_pg:query(
                            Conn,
                            <<
                                "UPDATE bot_delivery SET status = 'pending'"
                                " WHERE delivery_id = 'ewh994-gz-dead'"
                            >>,
                            []
                        )
                    )
                end)
            ),
            ?assertEqual(
                {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
                in_savepoint(Conn, fun() ->
                    map_pg(
                        elib_pg:query(
                            Conn,
                            <<
                                "UPDATE bot_delivery SET webhook_url = 'https://evil.example/'"
                                " WHERE delivery_id = 'ewh994-gz-dead'"
                            >>,
                            []
                        )
                    )
                end)
            ),
            ok = exec(Conn, <<"ROLLBACK">>),
            %% backfill 幂等：再次 up 不改变 owner、版本仍为 1
            ok = erlang_migrate:up(MigConfig),
            Recheck = one(
                Conn,
                <<
                    "SELECT ewh_owner_application_id, ewh_ledger_version FROM bot_delivery"
                    " WHERE delivery_id = 'ewh994-gz-dead'"
                >>,
                []
            ),
            ?assertEqual(994203, maps:get(<<"ewh_owner_application_id">>, Recheck)),
            ?assertEqual(1, maps:get(<<"ewh_ledger_version">>, Recheck))
        after
            _ = exec_quiet(Conn, <<"ROLLBACK">>),
            epgsql:close(Conn)
        end
    end).
