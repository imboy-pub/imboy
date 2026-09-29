%%% @doc 坐席控制台嵌入真库套件（seat-console-embed SC-BE）：00000153 迁移
%%% up/down 幂等与 fail-closed、CRUD 往返、(Org,WS) 活跃槽位部分唯一、
%%% 吊销释放槽位、全局反查 SQL 形状、铁律 6 租户键机械断言。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/1` 随机 TSID scope；无真实数据。
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）：环境问题不得被当成 PASS。
-module(cs_seat_console_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(FIX, cs_pg_test_fixture).
-define(MIG_DIR, "priv/migrations").
-define(TABLE, <<"customer_service_seat_console">>).

seat_console_pg_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_migration_files_present/0},
        {timeout, 120, fun a01_up_idempotent_and_roundtrip_no_residue/0},
        {timeout, 60, fun tenant_keys_carry_org_in_every_statement/0},
        {timeout, 60, fun global_public_id_sql_shape/0},
        {timeout, 60, fun crud_roundtrip/0},
        {timeout, 60, fun partial_unique_blocks_second_active_slot/0},
        {timeout, 60, fun revoke_frees_slot_and_row_is_kept/0},
        {timeout, 60, fun workspace_fk_cross_tenant_rejected/0},
        {timeout, 60, fun f4_create_is_single_transaction/0},
        {timeout, 60, fun f6_update_expected_version_cas/0},
        {timeout, 120, fun a01_down_with_rows_fails_closed/0}
    ];
cases({error, Reason}) ->
    erlang:error({sc_be_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% A01：迁移文件在册 + up 幂等 + down/up 往返无残留
%% ===================================================================

a01_migration_files_present() ->
    {ok, Files} = file:list_dir(?MIG_DIR),
    ?assert(lists:member("00000153_customer_service_seat_console.up.sql", Files)),
    ?assert(lists:member("00000153_customer_service_seat_console.down.sql", Files)),
    lists:foreach(
        fun(Name) ->
            {ok, Bin} = file:read_file(filename:join(?MIG_DIR, Name)),
            %% 首行注释携带完整文件名（ADR-0002 迁移契约）。
            [FirstLine | _] = binary:split(Bin, <<"\n">>),
            ?assertMatch(
                {match, _}, re:run(FirstLine, <<"(up|down)\\.sql">>)
            )
        end,
        [
            "00000153_customer_service_seat_console.up.sql",
            "00000153_customer_service_seat_console.down.sql"
        ]
    ).

a01_up_idempotent_and_roundtrip_no_residue() ->
    %% 前置：app 启动 migrate 已把 153 应用到 scratch 库；再跑一次 up 全量
    %% 仍全绿（IF NOT EXISTS 幂等）。
    ?assert(table_exists(?TABLE)),
    ok = with_migrate_conn(fun(Conn) ->
        Config = #{conn => Conn, dir => imboy_migrate:get_scripts_path(), strict => true},
        ok = erlang_migrate:up(Config),
        %% down 回滚越过 153（head 落到 152）：down 步数按在册版本推导——
        %% down_steps_to(Target) = count(>Target)+1，head 落到 Target-1，
        %% 随后 up 1 步恰重新应用 Target=153（head 再前移不红）。
        Steps = down_steps_to(153),
        ?assert(Steps >= 1),
        ok = erlang_migrate:down(Config, Steps),
        ?assertNot(table_exists_on(Conn, ?TABLE)),
        %% up 1 步 = 重新应用 153（Target）；再全量 up（幂等）。
        ok = erlang_migrate:up(Config, 1),
        ?assert(table_exists_on(Conn, ?TABLE)),
        ok = erlang_migrate:up(Config)
    end),
    ?assert(column_exists(?TABLE, <<"public_seat_console_id">>)),
    ?assert(column_exists(?TABLE, <<"workspace_id">>)),
    ?assert(column_exists(?TABLE, <<"revoked_at">>)).

%% A01（fail-closed down）：表内有行（含 revoked 行）时 down 必须 RAISE——
%% 已分发的 public_seat_console_id 不得静默丢弃；清空后 down 成功、up 回补。
a01_down_with_rows_fails_closed() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = maps:get(workspace_id, Scope),
    ConsoleId = ?FIX:id(),
    try
        {ok, _} = cs_pg_seat_console:insert_seat_console(Org, console(Org, Ws, ConsoleId)),
        %% 造一个 revoked 行：先吊销首行释放槽位，再插入第二行并吊销——
        %% 表内同时有 revoked 行与（无）active 行，down 仍必须拒绝。
        ok = cs_pg_seat_console:revoke_seat_console(Org, Ws, ConsoleId, now_sec()),
        RevokedId = ?FIX:id(),
        {ok, _} =
            cs_pg_seat_console:insert_seat_console(
                Org, console(Org, Ws, RevokedId, #{public_id => <<"sc_pub_pgdown2">>})
            ),
        ok = cs_pg_seat_console:revoke_seat_console(Org, Ws, RevokedId, now_sec()),
        %% 有行（含 revoked 行）→ down 返回 {error, _} 且表保持原样
        %% （fail-closed：已分发的 public_seat_console_id 不得静默丢弃）。
        {error, _} = with_migrate_down(1),
        ?assert(table_exists(?TABLE)),
        {ok, [_, _]} = cs_pg_seat_console:list_seat_consoles_page(Org, Ws, 0, 50),
        %% 失败的 down 按契约置 dirty（fail-closed 语义的一部分）；force/2
        %% 恢复后（head=153 clean）清空行再 down 即成功；up 回补 head。
        ok = with_migrate_force(153),
        ok = purge_scope(Org),
        ok = with_migrate_down(1),
        ?assertNot(table_exists(?TABLE)),
        ok = with_migrate_up(),
        ?assert(table_exists(?TABLE))
    after
        _ = purge_scope(Org),
        ok = ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 铁律 6 机械断言：冻结语句每条同语句带 organization_id
%% ===================================================================

tenant_keys_carry_org_in_every_statement() ->
    Statements = cs_pg_seat_console:sql_statements(),
    lists:foreach(
        fun(Sql) -> ?assertMatch({match, _}, re:run(Sql, <<"organization_id">>)) end,
        Statements
    ),
    lists:foreach(
        fun(Sql) ->
            ?assertMatch({match, _}, re:run(Sql, <<"organization_id\\s*=\\s*\\$1">>))
        end,
        [S || S <- Statements, not is_insert_statement(S)]
    ).

is_insert_statement(Sql) ->
    match =:= re:run(Sql, <<"^\\s*INSERT\\s+INTO">>, [{capture, none}]).

%% 全局反查语句是模块级宏（不进 sql_statements/0——「同语句带 Org」的机械
%% 断言集语义上不适用）；形状以模块源码冻结（宏原文切片）：谓词零 Org
%% （organization_id 只在 SELECT 投影）、占位符恰为 $1。
global_public_id_sql_shape() ->
    Chunk = global_sql_chunk(),
    ?assertMatch(
        {match, _},
        re:run(Chunk, <<"WHERE\\s+public_seat_console_id = \\$1">>)
    ),
    ?assertMatch(nomatch, re:run(Chunk, <<"organization_id\\s*=">>)),
    ?assertMatch({match, _}, re:run(Chunk, <<"SELECT id, organization_id,">>)),
    ok.

global_sql_chunk() ->
    {ok, Bin} = file:read_file(
        filename:join([
            "src", "features", "customer_service", "infrastructure", "cs_pg_seat_console.erl"
        ])
    ),
    case binary:split(Bin, <<"-define(SQL_FETCH_CONSOLE_BY_PUBLIC_ID_GLOBAL, <<">>) of
        [_Only] ->
            erlang:error(global_sql_missing);
        [_Head, Rest] ->
            case binary:split(Rest, <<">>).">>) of
                [Chunk, _Tail] -> Chunk;
                _ -> erlang:error(global_sql_unterminated)
            end
    end.

%% ===================================================================
%% CRUD 往返 / 槽位唯一 / 吊销语义
%% ===================================================================

crud_roundtrip() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = maps:get(workspace_id, Scope),
    ConsoleId = ?FIX:id(),
    try
        {ok, Row} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ConsoleId, #{public_id => <<"sc_pub_pgcrud1">>})
        ),
        ?assertEqual(ConsoleId, maps:get(id, Row)),
        ?assertEqual(active, maps:get(status, Row)),
        ?assertEqual([<<"https://shop.example.com">>], maps:get(allowed_origins, Row)),
        ?assertEqual(1, maps:get(version, Row)),

        %% 按 (Org, WS, id) 读取；错误作用域 not_found。
        {ok, Fetched} = cs_pg_seat_console:fetch_seat_console(Org, Ws, ConsoleId),
        ?assertEqual(ConsoleId, maps:get(id, Fetched)),
        {error, not_found} = cs_pg_seat_console:fetch_seat_console(Org, Ws + 1, ConsoleId),
        {error, not_found} =
            cs_pg_seat_console:fetch_seat_console(Org + 1, Ws, ConsoleId),

        %% update：只改 allowed_origins；version + 1；不可编辑键不动。
        At = now_sec(),
        {ok, Updated} = cs_pg_seat_console:update_seat_console(
            Org, Ws, ConsoleId, At, #{allowed_origins => [<<"https://docs.example.com">>]}
        ),
        ?assertEqual([<<"https://docs.example.com">>], maps:get(allowed_origins, Updated)),
        ?assertEqual(2, maps:get(version, Updated)),
        ?assertEqual(active, maps:get(status, Updated)),

        %% 列表分页：本 (Org, WS) 命中；其他作用域空页。
        {ok, [Only]} = cs_pg_seat_console:list_seat_consoles_page(Org, Ws, 0, 50),
        ?assertEqual(ConsoleId, maps:get(id, Only)),
        {ok, []} = cs_pg_seat_console:list_seat_consoles_page(Org, Ws, ConsoleId, 50),
        {ok, []} = cs_pg_seat_console:list_seat_consoles_page(Org, Ws + 1, 0, 50),

        %% 全局反查：无 Org 输入，行派生租户；不存在 → not_found。
        {ok, Global} =
            cs_pg_seat_console:fetch_seat_console_by_public_id_global(
                <<"sc_pub_pgcrud1">>
            ),
        ?assertEqual(ConsoleId, maps:get(id, Global)),
        ?assertEqual(Org, maps:get(organization_id, Global)),
        {error, not_found} =
            cs_pg_seat_console:fetch_seat_console_by_public_id_global(<<"sc_pub_absent_pg">>),

        %% 吊销：行保留、status=revoked、revoked_at 落值；反查仍命中行
        %% （active 门由 application 裁决）。
        At2 = now_sec(),
        ok = cs_pg_seat_console:revoke_seat_console(Org, Ws, ConsoleId, At2),
        {ok, Revoked} = cs_pg_seat_console:fetch_seat_console(Org, Ws, ConsoleId),
        ?assertEqual(<<"revoked">>, maps:get(status, Revoked)),
        ?assert(is_integer(maps:get(revoked_at, Revoked))),
        {ok, _} = cs_pg_seat_console:fetch_seat_console_by_public_id_global(<<"sc_pub_pgcrud1">>),
        %% 已吊销行再吊销 → not_found（幂等裁决由 application 用 fetch 区分）。
        {error, not_found} = cs_pg_seat_console:revoke_seat_console(Org, Ws, ConsoleId, At2),
        %% 已吊销行 update 零命中 → seat_console_revoked。
        {error, seat_console_revoked} = cs_pg_seat_console:update_seat_console(
            Org, Ws, ConsoleId, now_sec(), #{allowed_origins => []}
        )
    after
        _ = purge_scope(Org),
        ok = ?FIX:cleanup(Scope)
    end.

%% A03/A04 的 DB 裁决点：uq_cssc_org_ws_active 部分唯一——同 (Org,WS) 第二个
%% active 行 23505 → conflict；不同 (Org,WS) OK；吊销释放槽位。
partial_unique_blocks_second_active_slot() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = maps:get(workspace_id, Scope),
    %% fixture 的 other_workspace 属于 other_org（跨租户负例对）。
    OtherOrg = maps:get(other_org_id, Scope),
    OtherWs = maps:get(other_workspace_id, Scope),
    try
        {ok, _} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_slot1">>})
        ),
        %% 同 (Org, WS) 再插 active → conflict。
        {error, conflict} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_slot2">>})
        ),
        %% 公开 ID 全局唯一：换 workspace 但同公开 ID → conflict。
        {error, conflict} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, OtherWs, ?FIX:id(), #{public_id => <<"sc_pub_slot1">>})
        ),
        %% 不同 (Org, WS) → OK。
        {ok, _} = cs_pg_seat_console:insert_seat_console(
            OtherOrg, console(OtherOrg, OtherWs, ?FIX:id(), #{public_id => <<"sc_pub_slot3">>})
        )
    after
        _ = purge_scope(Org),
        _ = purge_scope(OtherOrg),
        ok = ?FIX:cleanup(Scope)
    end.

revoke_frees_slot_and_row_is_kept() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = maps:get(workspace_id, Scope),
    First = ?FIX:id(),
    try
        {ok, _} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, First, #{public_id => <<"sc_pub_free1">>})
        ),
        ok = cs_pg_seat_console:revoke_seat_console(Org, Ws, First, now_sec()),
        %% revoked 行保留（不删）→ 同公开 ID 仍命中行（行级唯一与 status 无关）。
        {ok, _} = cs_pg_seat_console:fetch_seat_console_by_public_id_global(<<"sc_pub_free1">>),
        %% 槽位被释放：同 (Org, WS) 可建新 active 行。
        {ok, _} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_free2">>})
        ),
        %% 复活旧公开 ID：新行全局唯一冲突 → conflict（行保留的代价，公开 ID
        %% 不回收——已分发的 ID 不换主人）。
        {error, conflict} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_free1">>})
        )
    after
        _ = purge_scope(Org),
        ok = ?FIX:cleanup(Scope)
    end.

%% A03 的作用域 FK 证明：(Org, WS) 复合 FK（fk_cssc_workspace）——workspace
%% 不属于本 Org 的绑定 23503 拒绝（跨租户绑定进不了库）。
workspace_fk_cross_tenant_rejected() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    OtherOrgWs = maps:get(other_workspace_id, Scope),
    try
        {error, {sql, <<"23503">>, _}} =
            cs_pg_seat_console:insert_seat_console(
                Org,
                console(Org, OtherOrgWs, ?FIX:id(), #{public_id => <<"sc_pub_fk1">>})
            )
    after
        _ = purge_scope(Org),
        ok = ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% F-4 / F-6（REVIEW-3）：创建单事务原子 + PUT 可选乐观并发控制
%% ===================================================================

%% F-4 三态（真 DB）：
%%   a) 首次创建成功（INSERT 与回读同事务，行 + 投影同时可读）；
%%   c) 真冲突语义不变：另一 active 控制台已存在 → conflict（23505 → 409）；
%%   b) 事务内回读失败（meck 注入瞬时故障）→ 整体回滚（行不落库，公开 ID
%%      反查 not_found）→ 重试（新 TSID）创建成功——孤儿行窗口不复存在。
f4_create_is_single_transaction() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = maps:get(workspace_id, Scope),
    try
        %% a) 首次创建成功
        {ok, First} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_f4a">>})
        ),
        ?assertEqual(active, maps:get(status, First)),
        ?assertEqual(1, maps:get(version, First)),
        %% c) 真冲突：同 (Org,WS) 另一 active 控制台已存在 → conflict
        {error, conflict} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_f4c">>})
        ),
        %% b) 回读失败 → 回滚：先吊销释放槽位，再注入 fetch_one_conn 瞬时故障
        ok = cs_pg_seat_console:revoke_seat_console(Org, Ws, maps:get(id, First), now_sec()),
        meck:new(cs_pg_common, [passthrough, no_link]),
        meck:expect(
            cs_pg_common,
            fetch_one_conn,
            4,
            fun(_Conn, _Sql, _Params, _Keys) -> {error, {db, transient_fetch_failure}} end
        ),
        {error, {db, transient_fetch_failure}} =
            cs_pg_seat_console:insert_seat_console(
                Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_f4b">>})
            ),
        meck:unload(cs_pg_common),
        %% 回滚证据：行不落库（公开 ID 全局反查 not_found；列表恰含被吊销的
        %% 首行——本套件列表无 status 谓词，revoked 行保留可见，按 id 对齐）。
        {error, not_found} =
            cs_pg_seat_console:fetch_seat_console_by_public_id_global(<<"sc_pub_f4b">>),
        {ok, [Listed]} = cs_pg_seat_console:list_seat_consoles_page(Org, Ws, 0, 50),
        ?assertEqual(maps:get(id, First), maps:get(id, Listed)),
        %% 重试（新 id）天然安全：无孤儿行占位
        {ok, Retried} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, ?FIX:id(), #{public_id => <<"sc_pub_f4b2">>})
        ),
        ?assertEqual(active, maps:get(status, Retried))
    after
        catch meck:unload(cs_pg_common),
        _ = purge_scope(Org),
        ok = ?FIX:cleanup(Scope)
    end.

%% F-6 四态（真 DB）：
%%   1) 无 expected_version = 旧 LWW 行为（更新成功，version 前进）；
%%   2) expected_version 匹配 → 更新成功（version + 1）；
%%   3) 不匹配 → {error, {cas_mismatch, Detail}}（携带当前 version），行不被
%%      改写；
%%   4) revoked 后 PUT 仍安全：带/不带 expected_version 同口径
%%      seat_console_revoked（revoke×PUT 竞争语义未破坏）。
f6_update_expected_version_cas() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = maps:get(workspace_id, Scope),
    Id = ?FIX:id(),
    try
        {ok, _} = cs_pg_seat_console:insert_seat_console(
            Org, console(Org, Ws, Id, #{public_id => <<"sc_pub_f6">>})
        ),
        %% 1) 无 expected_version = 旧 LWW
        {ok, Lww} = cs_pg_seat_console:update_seat_console(
            Org, Ws, Id, now_sec(), #{allowed_origins => [<<"https://lww.example.com">>]}
        ),
        ?assertEqual(2, maps:get(version, Lww)),
        ?assertEqual([<<"https://lww.example.com">>], maps:get(allowed_origins, Lww)),
        %% 2) 匹配 → 成功
        {ok, Matched} = cs_pg_seat_console:update_seat_console(
            Org,
            Ws,
            Id,
            now_sec(),
            #{allowed_origins => [<<"https://matched.example.com">>], expected_version => 2}
        ),
        ?assertEqual(3, maps:get(version, Matched)),
        ?assertEqual([<<"https://matched.example.com">>], maps:get(allowed_origins, Matched)),
        %% 3) 不匹配 → cas_mismatch（409 面，携带当前 version），行不被改写
        {error, {cas_mismatch, Detail}} = cs_pg_seat_console:update_seat_console(
            Org,
            Ws,
            Id,
            now_sec(),
            #{allowed_origins => [<<"https://stale.example.com">>], expected_version => 1}
        ),
        ?assertEqual(#{expected_version => 1, actual_version => 3}, Detail),
        {ok, Unchanged} = cs_pg_seat_console:fetch_seat_console(Org, Ws, Id),
        ?assertEqual([<<"https://matched.example.com">>], maps:get(allowed_origins, Unchanged)),
        ?assertEqual(3, maps:get(version, Unchanged)),
        %% 4) revoked 后 PUT 仍安全（带/不带 expected_version 同口径）
        ok = cs_pg_seat_console:revoke_seat_console(Org, Ws, Id, now_sec()),
        {error, seat_console_revoked} = cs_pg_seat_console:update_seat_console(
            Org, Ws, Id, now_sec(), #{allowed_origins => []}
        ),
        {error, seat_console_revoked} = cs_pg_seat_console:update_seat_console(
            Org, Ws, Id, now_sec(), #{allowed_origins => [], expected_version => 3}
        )
    after
        _ = purge_scope(Org),
        ok = ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

console(Org, Ws, Id) ->
    console(Org, Ws, Id, #{}).

console(_Org, Ws, Id, Opts) ->
    PublicId =
        case maps:get(public_id, Opts, undefined) of
            undefined -> <<"sc_pub_", (integer_to_binary(Id))/binary>>;
            P -> P
        end,
    Base = #{
        id => Id,
        workspace_id => Ws,
        public_seat_console_id => PublicId,
        allowed_origins => [<<"https://shop.example.com">>],
        created_by_user_id => undefined
    },
    case maps:is_key(skip_slot, Opts) of
        true -> Base;
        false -> Base
    end.

org(Scope) ->
    maps:get(org_id, Scope).

now_sec() ->
    erlang:system_time(second).

purge_scope(Org) ->
    _ = ?FIX:exec(
        <<"DELETE FROM customer_service_seat_console WHERE organization_id = $1">>, [Org]
    ),
    ok.

with_migrate_conn(Fun) ->
    {ok, Conn} = inttest_marker_db:safe_connect(config_ds:env(super_account)),
    try
        Fun(Conn)
    after
        _ = epgsql:close(Conn)
    end.

with_migrate_down(Steps) ->
    with_migrate_conn(fun(Conn) ->
        Config = #{conn => Conn, dir => imboy_migrate:get_scripts_path(), strict => true},
        erlang_migrate:down(Config, Steps)
    end).

with_migrate_force(Version) ->
    with_migrate_conn(fun(Conn) ->
        Config = #{conn => Conn, dir => imboy_migrate:get_scripts_path(), strict => true},
        erlang_migrate:force(Config, Version)
    end).

with_migrate_up() ->
    with_migrate_conn(fun(Conn) ->
        Config = #{conn => Conn, dir => imboy_migrate:get_scripts_path(), strict => true},
        erlang_migrate:up(Config)
    end).

down_steps_to(TargetVersion) ->
    {ok, Files} = file:list_dir(?MIG_DIR),
    Versions = lists:usort([
        V
     || F <- Files,
        {ok, V} <- [migration_version_of(F)]
    ]),
    length([V || V <- Versions, V > TargetVersion]) + 1.

migration_version_of(FileName) ->
    case re:run(FileName, <<"^([0-9]{8})_.*\\.(up|down)\\.sql$">>, [{capture, [1], binary}]) of
        {match, [V]} ->
            {ok, binary_to_integer(V)};
        _ ->
            error
    end.

table_exists(Name) ->
    with_migrate_conn(fun(Conn) -> table_exists_on(Conn, Name) end).

table_exists_on(Conn, Name) ->
    {ok, _, [{N}]} =
        epgsql:equery(
            Conn,
            "SELECT count(*)::int AS n FROM information_schema.tables"
            " WHERE table_name = $1",
            [Name]
        ),
    1 =:= N.

column_exists(Table, Column) ->
    with_migrate_conn(fun(Conn) -> column_exists_on(Conn, Table, Column) end).

column_exists_on(Conn, Table, Column) ->
    {ok, _, [{N}]} =
        epgsql:equery(
            Conn,
            "SELECT count(*)::int AS n FROM information_schema.columns"
            " WHERE table_name = $1 AND column_name = $2",
            [Table, Column]
        ),
    1 =:= N.
