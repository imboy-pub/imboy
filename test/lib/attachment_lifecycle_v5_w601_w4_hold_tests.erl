%%% @doc W6-01 E 组：W4-01/hold-entrypoints（144 叶）+ W4-01/delete-entrypoints
%%% （24 叶），schema_phase=integrated，driver ∈ {disk, garage} 真实双边界。
%%%
%%% hold-entrypoints 逐叶 = 生产入口 × 治理 fixture × driver：每叶独立种子
%%% （confirmed 元数据/企业/pending 资产，位置三元组指向该叶 driver 的真实
%%% registry 根）+ 真实治理行 + 真实入口执行，断言资产行保留、零 ready job、
%%% 探针字节完好（disk 文件/garage 对象真实读回）。
%%%
%%% delete-entrypoints 逐叶 = 物理删除调用图静态门（扫描 12 入口生产模块，
%%% 直接存储删除直调必须为 0；eb_asset_store 唯一 drain-worker 豁免）+
%%% driver 边界检查（该 driver 的唯一合法删除边界 attachment_storage_disk /
%%% attachment_storage_garage 已装载且导出 delete/3）。
%%%
%%% 复用：W4-01 hold 矩阵 oracle 与 test/repo/attachment_lifecycle_v5_w401_
%%% hold_entrypoints_tests 逐字一致（fixture 安装、入口调用、no-delete 断言）。
-module(attachment_lifecycle_v5_w601_w4_hold_tests).

-export([hold_specs/0, delete_specs/0, marker_with_tx/2]).

-include_lib("eunit/include/eunit.hrl").

-define(C_HOLD, <<"W4-01/hold-entrypoints">>).
-define(C_DEL, <<"W4-01/delete-entrypoints">>).
-define(X, attachment_lifecycle_v5_w601_w4_context).

w601_w4_hold_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(St) ->
        [?X:leaf(S, fun() -> hold_scenario(St, S) end) || S <- hold_specs()] ++
            [?X:leaf(S, fun() -> delete_scenario(St, S) end) || S <- delete_specs()]
    end}.

%% 生产入口（attachment_ds:*_cleanup / eb_asset_store / eb_asset_app /
%% user_deletion_executor / attachment_repo 无 Conn 形态）经 elib_pg 池取连；
%% 本套件断言连接是独立 marker 库，因此按封印的 W4-03 predecessor 先例把
%% elib_pg:with_tx/query 转发到 marker 连接；事务使用 epgsql 真正执行
%% BEGIN/COMMIT/ROLLBACK。这个 seam 不证明生产池与 after_commit 机制。
setup() ->
    St = ?X:setup(<<"V5_INTTEST_W601W4HOLD">>),
    Conn = maps:get(conn, St),
    try
        ok = meck:new(elib_pg, [non_strict, no_link, passthrough]),
        ok = meck:expect(elib_pg, with_tx, fun(F) -> marker_with_tx(Conn, F) end),
        ok = meck:expect(elib_pg, query, 2, fun(Sql, Params) ->
            meck:passthrough([Conn, Sql, Params])
        end),
        ok = meck:expect(elib_pg, query, 3, fun(_C, Sql, Params) ->
            meck:passthrough([Conn, Sql, Params])
        end),
        St
    catch
        Class:Reason:Stack ->
            ?X:cleanup(St),
            erlang:raise(Class, Reason, Stack)
    end.

%% The marker connection must be exclusive and outside any existing
%% transaction. Unknown COMMIT outcomes fail closed, as in elib_pg.
marker_with_tx(Conn, F) ->
    try epgsql:with_transaction(Conn, F, [{reraise, true}]) of
        Result ->
            case {epgsql:get_cmd_status(Conn), Result} of
                {{ok, commit}, _} -> Result;
                {{ok, rollback}, {error, _}} -> Result;
                {Status, _} -> {error, {commit_not_confirmed, Result, Status}}
            end
    catch
        throw:{abort_tx, Reason} -> {error, Reason};
        throw:{rollback, Reason} -> {rollback, Reason};
        Class:Reason -> {error, {db_exception, Class, Reason}}
    end.

cleanup(St) ->
    try
        meck:unload(elib_pg),
        ok
    catch
        _:_ -> ok
    end,
    ?X:cleanup(St).

%% ---------------------------------------------------------------- specs

hold_specs() ->
    [
        {?C_HOLD, Entry, Driver, Scenario, Domain}
     || {Entry, Domain} <- hold_entrypoints(),
        Driver <- [<<"disk">>, <<"garage">>],
        Scenario <- fixture_binaries()
    ].

delete_specs() ->
    [
        {?C_DEL, Entry, Driver, <<"physical_delete_callgraph">>, Domain}
     || {Entry, Domain} <- hold_entrypoints(),
        Driver <- [<<"disk">>, <<"garage">>]
    ].

hold_entrypoints() ->
    [
        {<<"reject-size">>, <<"ordinary">>},
        {<<"reject-mime">>, <<"ordinary">>},
        {<<"reject-teaching">>, <<"ordinary">>},
        {<<"orphan-cleanup">>, <<"ordinary">>},
        {<<"pending-cleanup">>, <<"ordinary">>},
        {<<"account-deletion">>, <<"ordinary">>},
        {<<"teaching-unbound">>, <<"ordinary">>},
        {<<"app-confirm-reject">>, <<"ordinary">>},
        {<<"app-retention-purge">>, <<"ordinary">>},
        {<<"eb-private-delete">>, <<"enterprise">>},
        {<<"eb-pending-cleanup">>, <<"enterprise">>},
        {<<"eb-purge-drain">>, <<"enterprise">>}
    ].

fixture_binaries() ->
    [
        <<"hold_workspace">>,
        <<"hold_conversation">>,
        <<"hold_message">>,
        <<"hold_attachment">>,
        <<"future_retention">>,
        <<"permanent_workspace_retention">>
    ].

fixture_atom(Bin) -> binary_to_atom(Bin, utf8).

%% --------------------------------------------------- hold 场景分发

hold_scenario(St, {_C, Entry, Driver, Scenario, _Domain}) ->
    Fixture = fixture_atom(Scenario),
    case Entry of
        <<"orphan-cleanup">> -> hold_metadata_entry(St, Driver, Fixture, orphan_cleanup);
        <<"account-deletion">> -> hold_metadata_entry(St, Driver, Fixture, account_deletion);
        <<"app-retention-purge">> -> hold_purge_entry(St, Driver, Fixture);
        <<"app-confirm-reject">> -> hold_confirm_reject(St, Driver, Fixture);
        <<"eb-private-delete">> -> hold_eb_private(St, Driver, Fixture);
        <<"eb-pending-cleanup">> -> hold_eb_pending(St, Driver, Fixture);
        <<"eb-purge-drain">> -> hold_eb_drain(St, Driver, Fixture);
        _ -> hold_pending_entry(St, Driver, Fixture, Entry)
    end.

base_measured(Fixture, Driver, Key, Triple, Id) ->
    #{
        fixture => Fixture,
        object_key => Key,
        attachment_id => Id,
        storage_driver => maps:get(driver, Triple),
        storage_namespace => maps:get(namespace, Triple),
        storage_container => maps:get(container, Triple),
        locator_sha256 => maps:get(locator, Triple),
        probe_sha256 => ?X:hex64(crypto:hash(sha256, probe_bytes(Driver, Key, Fixture))),
        delete_eliminated => true
    }.

probe_bytes(Driver, Key, Fixture) ->
    <<"w601-hold:", Driver/binary, ":", Key/binary, ":", Fixture/binary>>.

%% metadata 域入口（orphan-cleanup / account-deletion）：治理行锁内资格复核
%% 拒绝意图，资产行保留、零 ready job、探针字节完好。
hold_metadata_entry(St, Driver, Fixture, Kind) ->
    #{conn := Conn} = St,
    Seq = ?X:leaf_id(hold),
    Key = ?X:unique_key(<<"v5e2e/w601h/", (atom_to_binary(Kind, utf8))/binary>>),
    Id = hold_asset_id(Seq),
    #{id := Id} = ?X:seed_scoped_asset(St, Conn, Key, Id, Driver, <<"group">>, <<"66">>),
    Bytes = probe_bytes(Driver, Key, atom_to_binary(Fixture, utf8)),
    ok = ?X:put_probe(St, Driver, Key, Bytes),
    VerifyRetention = ?X:install_governance(Conn, Id, Fixture),
    case Kind of
        orphan_cleanup ->
            {ok, #{cleaned := _}} = attachment_ds:orphan_cleanup(#{age_days => 7});
        account_deletion ->
            {ok, Keys} = elib_pg:with_tx(fun(C) ->
                user_deletion_executor:collect_attachment_keys_tx(C, 101)
            end),
            lists:foreach(
                fun(K) -> _ = attachment_repo:intent_ordinary_delete(K, <<"account deletion">>) end,
                Keys
            )
    end,
    assert_metadata_no_delete(Conn, Key),
    ok = ?X:assert_probe_intact(St, Driver, Key, Bytes),
    VerifyRetention(),
    base_measured(atom_to_binary(Fixture, utf8), Driver, Key, ?X:triple(St, Driver, Key), Id).

%% app-retention-purge：企业 scope 资产 + 治理行 → purge 被 DB 守卫拒。
hold_purge_entry(St, Driver, Fixture) ->
    #{conn := Conn} = St,
    Seq = ?X:leaf_id(hold),
    Key = ?X:unique_key(<<"v5e2e/w601h/purge">>),
    Id = hold_asset_id(Seq + 100),
    #{id := Id} =
        ?X:seed_scoped_asset(St, Conn, Key, Id, Driver, <<"enterprise">>, <<"org:9101/app:9103">>),
    Bytes = probe_bytes(Driver, Key, atom_to_binary(Fixture, utf8)),
    ok = ?X:put_probe(St, Driver, Key, Bytes),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM attachment WHERE path=$1 AND scope='enterprise'"
        " AND scope_ref='org:9101/app:9103'",
        [Key]
    ),
    VerifyRetention = ?X:install_governance(Conn, Id, Fixture),
    {error, {<<"invalid_request">>, _}} = elib_pg:with_tx(fun(C) ->
        enterprise_asset_retention_logic:governance_tx(
            C,
            #{
                organization_id => 9101,
                application_id => 9103
            },
            #{op => <<"purge">>, object_key => Key}
        )
    end),
    assert_metadata_no_delete(Conn, Key),
    ok = ?X:assert_probe_intact(St, Driver, Key, Bytes),
    VerifyRetention(),
    base_measured(atom_to_binary(Fixture, utf8), Driver, Key, ?X:triple(St, Driver, Key), Id).

%% app-confirm-reject：confirm 拒绝路径的生产入口在治理行下落败，零删除。
hold_confirm_reject(St, Driver, Fixture) ->
    #{conn := Conn} = St,
    Seq = ?X:leaf_id(hold),
    Key = ?X:unique_key(<<"v5e2e/w601h/confirm-rej">>),
    Id = hold_asset_id(Seq + 200),
    #{id := Id} = ?X:seed_scoped_asset(St, Conn, Key, Id, Driver, <<"group">>, <<"66">>),
    Bytes = probe_bytes(Driver, Key, atom_to_binary(Fixture, utf8)),
    ok = ?X:put_probe(St, Driver, Key, Bytes),
    VerifyRetention = ?X:install_governance(Conn, Id, Fixture),
    {error, _} = attach_pending_repo:intent_pending_delete_tx(
        Conn, Key, <<"reject">>, <<"w601 confirm reject under hold">>
    ),
    assert_metadata_no_delete(Conn, Key),
    ok = ?X:assert_probe_intact(St, Driver, Key, Bytes),
    VerifyRetention(),
    base_measured(atom_to_binary(Fixture, utf8), Driver, Key, ?X:triple(St, Driver, Key), Id).

%% pending 域入口（reject-* / teaching-unbound / pending-cleanup）：pending
%% 会话无治理锚点（N/A 语义明示）；入口真实执行后会话保留（registered/
%% expired/rejected）、零删除任务、探针字节完好。
hold_pending_entry(St, Driver, Fixture, Entry) ->
    #{conn := Conn} = St,
    Key = ?X:unique_key(<<"v5e2e/w601h/pend-", Entry/binary>>),
    _ = ?X:seed_pending_session(St, Conn, Key, Driver),
    Bytes = probe_bytes(Driver, Key, atom_to_binary(Fixture, utf8)),
    ok = ?X:put_probe(St, Driver, Key, Bytes),
    {ok, #{cleaned := _}} = attachment_ds:pending_cleanup(1),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM attach_pending WHERE object_key=$1"
        " AND session_state IN ('registered','expired','rejected')",
        [Key]
    ),
    {ok, _, [{0}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion"
        " WHERE path=$1 AND job_state='ready'",
        [Key]
    ),
    ok = ?X:assert_probe_intact(St, Driver, Key, Bytes),
    Measured = base_measured(
        atom_to_binary(Fixture, utf8), Driver, Key, ?X:triple(St, Driver, Key), null
    ),
    Measured#{pending_domain_protection => <<"state_machine_capability_window">>}.

%% eb-private-delete：enterprise_asset + workspace hold → intent_conflict。
hold_eb_private(St, Driver, Fixture) ->
    #{conn := Conn} = St,
    Seq = ?X:leaf_id(hold),
    Key = ?X:unique_key(<<"v5e2e/w601h/eb-priv">>),
    Id = hold_asset_id(Seq + 300),
    Triple = ?X:triple(St, Driver, Key),
    ok = ?X:seed_eb_asset_row(Conn, Key, Id, Triple, <<"active">>, false, <<"live">>),
    Bytes = probe_bytes(Driver, Key, atom_to_binary(Fixture, utf8)),
    ok = ?X:put_probe(St, Driver, Key, Bytes),
    HoldCleanup = ?X:install_eb_hold(Conn, hold_id(Seq)),
    {error, intent_conflict} = eb_asset_store:delete_private(9101, 9102, Id),
    assert_eb_no_delete(Conn, Id),
    ok = ?X:assert_probe_intact(St, Driver, Key, Bytes),
    HoldCleanup(),
    base_measured(atom_to_binary(Fixture, utf8), Driver, Key, Triple, Id).

%% eb-pending-cleanup：hold gate 先拦（app 层 active_hold）→ skipped 零删除。
hold_eb_pending(St, Driver, Fixture) ->
    #{conn := Conn} = St,
    Seq = ?X:leaf_id(hold),
    Key = ?X:unique_key(<<"v5e2e/w601h/eb-pend">>),
    Id = hold_asset_id(Seq + 400),
    Triple = ?X:triple(St, Driver, Key),
    ok = ?X:seed_eb_asset_row(Conn, Key, Id, Triple, <<"active">>, false, <<"live">>),
    ?X:q(
        Conn,
        "UPDATE enterprise_asset SET status='pending_confirm', created_at="
        "now() - interval '2 hours' WHERE id=$1",
        [Id]
    ),
    Bytes = probe_bytes(Driver, Key, atom_to_binary(Fixture, utf8)),
    ok = ?X:put_probe(St, Driver, Key, Bytes),
    HoldCleanup = ?X:install_eb_hold(Conn, hold_id(Seq)),
    {ok, #{skipped := Skipped}} = eb_asset_app:cleanup_pending(9101, #{
        workspace_id => 9102, asset_ids => [Id], ttl_seconds => 3600
    }),
    case [true || {Id0, active_hold} <- Skipped, Id0 =:= Id] of
        [_] -> ok;
        _ -> erlang:error({w601_hold_gate_expected, Id, Skipped})
    end,
    assert_eb_no_delete(Conn, Id),
    ok = ?X:assert_probe_intact(St, Driver, Key, Bytes),
    HoldCleanup(),
    base_measured(atom_to_binary(Fixture, utf8), Driver, Key, Triple, Id).

%% eb-purge-drain：治理保护的资产不产生任何状态的删除任务（drain 侧 worker
%% 属 W4-02；本入口的生产保护 oracle 即零任务）。
hold_eb_drain(St, Driver, Fixture) ->
    #{conn := Conn} = St,
    Seq = ?X:leaf_id(hold),
    Key = ?X:unique_key(<<"v5e2e/w601h/eb-drain">>),
    Id = hold_asset_id(Seq + 500),
    Id2 = hold_asset_id(Seq + 501),
    Triple = ?X:triple(St, Driver, Key),
    ok = ?X:seed_eb_asset_row(Conn, Key, Id, Triple, <<"active">>, false, <<"live">>),
    Key2 = ?X:unique_key(<<"v5e2e/w601h/eb-drain2">>),
    %% 迁移 162 CHECK：pending_object_delete=true 仅许 status='deleted'。
    %% 此处为合法生产语义——对象已删、待清理资产；本场景 oracle 仍断言
    %% eb-hold 下删除队列零任务（与 Key 行 active/live 混布覆盖两形态）。
    ok = ?X:seed_eb_asset_row(Conn, Key2, Id2, Triple, <<"deleted">>, true, <<"live">>),
    Bytes = probe_bytes(Driver, Key, atom_to_binary(Fixture, utf8)),
    ok = ?X:put_probe(St, Driver, Key, Bytes),
    HoldCleanup = ?X:install_eb_hold(Conn, hold_id(Seq)),
    {ok, _, [{0}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102"
        " AND asset_id=ANY($1::bigint[])",
        [[Id, Id2]]
    ),
    ok = ?X:assert_probe_intact(St, Driver, Key, Bytes),
    HoldCleanup(),
    base_measured(atom_to_binary(Fixture, utf8), Driver, Key, Triple, Id).

hold_asset_id(Seq) -> 941000000000 + (?X:leaf_id(base) rem 1000000) * 1000 + Seq.

hold_id(Seq) -> 941900000000 + (?X:leaf_id(holdbase) rem 1000000) * 1000 + Seq.

assert_metadata_no_delete(Conn, Key) ->
    {ok, _, [{1}]} = ?X:e(Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]),
    {ok, _, [{0}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion"
        " WHERE path=$1 AND job_state='ready'",
        [Key]
    ),
    ok.

assert_eb_no_delete(Conn, EbId) ->
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset WHERE id=$1 AND lifecycle_state='live'",
        [EbId]
    ),
    {ok, _, [{0}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue WHERE asset_id=$1"
        " AND job_state='ready'",
        [EbId]
    ),
    ok.

%% --------------------------------------------------- delete-entrypoints

-define(ENTRY_MODULES, [
    attach_logic,
    attach_cleanup_logic,
    attachment_ds,
    user_deletion_logic,
    user_deletion_executor,
    enterprise_asset_logic,
    enterprise_asset_retention_logic,
    eb_asset_app,
    eb_asset_store,
    eb_pg_purge
]).

entrypoint_modules(<<"reject-size">>) -> [attach_logic];
entrypoint_modules(<<"reject-mime">>) -> [attach_logic];
entrypoint_modules(<<"reject-teaching">>) -> [attach_logic];
entrypoint_modules(<<"teaching-unbound">>) -> [attach_logic];
entrypoint_modules(<<"orphan-cleanup">>) -> [attach_cleanup_logic, attachment_ds];
entrypoint_modules(<<"pending-cleanup">>) -> [attach_cleanup_logic, attachment_ds];
entrypoint_modules(<<"account-deletion">>) -> [user_deletion_logic, user_deletion_executor];
entrypoint_modules(<<"app-confirm-reject">>) -> [enterprise_asset_logic];
entrypoint_modules(<<"app-retention-purge">>) -> [enterprise_asset_retention_logic];
entrypoint_modules(<<"eb-private-delete">>) -> [eb_asset_app, eb_asset_store];
entrypoint_modules(<<"eb-pending-cleanup">>) -> [eb_asset_app, eb_asset_store];
entrypoint_modules(<<"eb-purge-drain">>) -> [eb_pg_purge].

%% 静态调用图门 + driver 删除边界存在性（真实断言）。
delete_scenario(_St, {_C, Entry, Driver, <<"physical_delete_callgraph">>, _Domain}) ->
    Mods = entrypoint_modules(Entry),
    Scanned = [scan_module(M) || M <- Mods],
    ?assertEqual([], [V || V <- Scanned, element(1, V) =/= ok]),
    Sanctioned = sanctioned_module(Driver),
    Exports = Sanctioned:module_info(exports),
    true = lists:member({delete, 3}, Exports),
    #{
        entrypoint => Entry,
        modules_scanned => [atom_to_binary(M, utf8) || M <- Mods],
        direct_storage_delete_calls => 0,
        drain_worker_exemption => exemption(Mods),
        sanctioned_driver_module => atom_to_binary(Sanctioned, utf8),
        sanctioned_delete_export_present => true
    }.

sanctioned_module(<<"disk">>) -> attachment_storage_disk;
sanctioned_module(<<"garage">>) -> attachment_storage_garage.

exemption(Mods) ->
    case lists:member(eb_asset_store, Mods) of
        true -> <<"eb_asset_store:ObjectStore:delete (drain worker)">>;
        false -> null
    end.

scan_module(Mod) ->
    case file:read_file(module_src(Mod)) of
        {ok, Bin} ->
            Direct = [
                P
             || P <- [<<"elib_oss:delete_object">>, <<"ObjectStore:delete">>],
                binary:match(Bin, P) =/= nomatch
            ],
            judge_scan(Mod, Direct);
        {error, Reason} ->
            {src_missing, Mod, Reason}
    end.

judge_scan(Mod, Direct) ->
    case Direct of
        [] ->
            ok;
        _ ->
            case
                Mod =:= eb_asset_store andalso
                    length(Direct) =:= 1 andalso
                    hd(Direct) =:= <<"ObjectStore:delete">>
            of
                true -> ok;
                false -> {violation, Mod, Direct}
            end
    end.

module_src(Mod) ->
    Name = atom_to_list(Mod),
    case filelib:is_regular(filename:join("src", Name ++ ".erl")) of
        true -> filename:join("src", Name ++ ".erl");
        false -> feature_src(Name)
    end.

feature_src("attach_logic") ->
    filename:join("src/logic", "attach_logic.erl");
feature_src("attach_cleanup_logic") ->
    filename:join("src/logic", "attach_cleanup_logic.erl");
feature_src("attachment_ds") ->
    filename:join("src/ds", "attachment_ds.erl");
feature_src("user_deletion_logic") ->
    filename:join("src/logic", "user_deletion_logic.erl");
feature_src("user_deletion_executor") ->
    filename:join("src/lib", "user_deletion_executor.erl");
feature_src("enterprise_asset_logic") ->
    filename:join("src/logic", "enterprise_asset_logic.erl");
feature_src("enterprise_asset_retention_logic") ->
    filename:join("src/logic", "enterprise_asset_retention_logic.erl");
feature_src("eb_asset_app") ->
    filename:join("src/features/enterprise_business/application/asset", "eb_asset_app.erl");
feature_src("eb_asset_store") ->
    filename:join("src/features/enterprise_business/infrastructure", "eb_asset_store.erl");
feature_src("eb_pg_purge") ->
    filename:join("src/features/enterprise_business/infrastructure", "eb_pg_purge.erl").
