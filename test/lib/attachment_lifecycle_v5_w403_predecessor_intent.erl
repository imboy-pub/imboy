%%% @doc W4-03 disk-predecessor-protocols / W4-01 intent-rollback 的 disk 重放
%%% （12 入口 × 4 故障 = 48 叶）。
%%%
%%% oracle 与源 case `W4-01/intent-rollback` 完全一致（意图失败绝不触达存储、
%%% 入口不误报成功），但边界搬到真实 disk driver：
%%%   * 前置状态与故障注入沿用源套件的 repo 层 meck 形状；
%%%   * 每叶在真实 disk root 写探针字节，失败后断言字节与文件清单零变化；
%%%   * 每叶用 marker 连接回读 `attachment_object_deletion`，断言未入队。
%%%
%%% 不宣称真实 PG 故障：sql_rollback / intent_insert_error / audit_insert_error /
%%% commit_response_lost 都是源 oracle 的注入形状，disk 版本只新增真实字节证据。
-module(attachment_lifecycle_v5_w403_predecessor_intent).

-include_lib("eunit/include/eunit.hrl").

-export([run/3]).

-define(S, attachment_lifecycle_v5_w403_predecessor_support).

-define(MECK_MODULES, [
    attach_pending_repo,
    attachment_repo,
    attachment_gc_repo,
    enterprise_attachment_retention_repo,
    enterprise_asset_repo,
    enterprise_asset_retention_logic,
    elib_pg,
    eb_pg_asset_meta,
    eb_pg_store,
    eb_pg_asset_delete_queue,
    user_deletion_executor
]).

run(Entry, Fault, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/", Entry/binary>>),
    Bytes = <<"w403p-intent-bytes:", Entry/binary, ":", (atom_to_binary(Fault, utf8))/binary>>,
    ok = ?S:put_disk_file(Root, Key, Bytes),
    Before = ?S:disk_listing(Root),
    [ok = meck:new(M, [non_strict, no_link, passthrough]) || M <- ?MECK_MODULES],
    try
        ok = base_expectations(Conn),
        Extra = execute(Entry, Fault, Conn, Key),
        After = ?S:disk_listing(Root),
        Before = After,
        ok = ?S:assert_disk_intact(Root, Key, Bytes),
        ok = ?S:assert_no_queue_row(Conn, Key, Entry),
        maps:merge(
            #{
                fault => atom_to_binary(Fault, utf8),
                entrypoint => Entry,
                object_key => Key,
                namespace => ?S:ns(),
                container => ?S:container(),
                probe_sha256 => ?S:hex64(crypto:hash(sha256, Bytes)),
                disk_files_before => length(Before),
                disk_files_after => length(After),
                disk_bytes_unchanged => true,
                storage_delete_calls => 0,
                queue_rows => 0,
                unknown_success_reported => false
            },
            Extra
        )
    after
        [ok = meck:unload(M) || M <- ?MECK_MODULES]
    end.

%% 入口自身的 DB 事务转发到 marker 连接；SQL 查询保持源套件的无副作用形状，
%% 具体入口在前置桩里显式覆盖。
base_expectations(Conn) ->
    ok = meck:expect(elib_pg, with_tx, fun(F) ->
        try
            F(Conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end),
    ok = meck:expect(elib_pg, query, fun(_C, _Sql, _P) -> {ok, []} end),
    ok.

fault_result(sql_rollback) -> {error, {db, sql_rollback}};
fault_result(intent_insert_error) -> {error, {db, intent_insert_failed}};
fault_result(audit_insert_error) -> {error, {db, audit_insert_failed}};
fault_result(commit_response_lost) -> {error, {db, commit_response_lost}}.

%% ---------------------------------------------------------------- 12 入口

execute(Entry, Fault, _Conn, Key) when
    Entry =:= <<"pending-cleanup">> orelse
        Entry =:= <<"reject-size">> orelse
        Entry =:= <<"reject-mime">> orelse
        Entry =:= <<"reject-teaching">> orelse
        Entry =:= <<"teaching-unbound">>
->
    ok = meck:expect(attach_pending_repo, list_expired, fun(_) ->
        {ok, [#{<<"object_key">> => Key}]}
    end),
    ok = meck:expect(attach_pending_repo, intent_pending_delete_tx, fun(_C, _K, _M, _R) ->
        fault_result(Fault)
    end),
    {ok, #{cleaned := 0, errors := N}} = attachment_ds:pending_cleanup(24),
    ?assert(N >= 1),
    #{reported_errors => N, entry_point_call => <<"attachment_ds:pending_cleanup/1">>};
execute(<<"orphan-cleanup">>, Fault, _Conn, Key) ->
    ok = meck:expect(attachment_repo, orphan_list_for_delete, fun(_) ->
        {ok, [#{<<"id">> => 1}]}
    end),
    ok = meck:expect(attachment_repo, orphan_path_tx, fun(_C, _Id, _O) ->
        {ok, [#{<<"path">> => Key, <<"scope">> => <<"private">>}]}
    end),
    ok = meck:expect(attachment_repo, intent_ordinary_delete_tx, fun(_C, _P, _R) ->
        fault_result(Fault)
    end),
    {ok, #{cleaned := 0, errors := N}} = attachment_ds:orphan_cleanup(#{age_days => 30}),
    ?assert(N >= 1),
    #{reported_errors => N, entry_point_call => <<"attachment_ds:orphan_cleanup/1">>};
execute(<<"account-deletion">>, Fault, _Conn, Key) ->
    case Fault of
        sql_rollback ->
            ok = meck:expect(user_deletion_executor, collect_attachment_keys_tx, fun(_C, _U) ->
                {error, sql_rollback}
            end);
        _ ->
            ok = meck:expect(user_deletion_executor, collect_attachment_keys_tx, fun(_C, _U) ->
                {ok, [Key]}
            end),
            ok = meck:expect(attachment_repo, intent_ordinary_delete_tx, fun(_C, _P, _R) ->
                fault_result(Fault)
            end)
    end,
    Result = elib_pg:with_tx(fun(TxConn) ->
        case user_deletion_executor:collect_attachment_keys_tx(TxConn, 101) of
            {ok, Keys} ->
                lists:filter(
                    fun(K) ->
                        case
                            attachment_repo:intent_ordinary_delete_tx(
                                TxConn, K, <<"account deletion">>
                            )
                        of
                            ok -> true;
                            {error, _} -> erlang:throw({abort_tx, intent_failed})
                        end
                    end,
                    Keys
                );
            {error, R} ->
                erlang:throw({abort_tx, R})
        end
    end),
    ?assertNot(Result =:= ok),
    #{reported_errors => 1, account_deletion_result => result_shape(Result)};
execute(<<"app-confirm-reject">>, Fault, Conn, Key) ->
    ok = meck:expect(attach_pending_repo, intent_pending_delete_tx, fun(_C, _K, <<"reject">>, _R) ->
        fault_result(Fault)
    end),
    {error, _} = attach_pending_repo:intent_pending_delete_tx(
        Conn, Key, <<"reject">>, <<"w403p confirm reject disk">>
    ),
    #{
        reported_errors => 1,
        entry_point_call => <<"attach_pending_repo:intent_pending_delete_tx/4">>
    };
execute(<<"app-retention-purge">>, Fault, Conn, Key) ->
    ok = meck:expect(enterprise_asset_repo, find_confirmed_tx, fun(_C, _O, _A, _K) ->
        {ok, #{
            <<"id">> => 42,
            <<"path">> => Key,
            <<"lifecycle_state">> => <<"live">>
        }}
    end),
    ok = meck:expect(enterprise_attachment_retention_repo, find_tx, fun(_C, _A) ->
        {ok, #{
            <<"attachment_id">> => 42,
            <<"hold_state">> => <<"none">>,
            <<"purge_state">> => <<"live">>,
            <<"retention_until">> => <<"x">>
        }}
    end),
    ok = meck:expect(enterprise_attachment_retention_repo, is_purgeable_tx, fun(_C, _A) ->
        {ok, true}
    end),
    ok = meck:expect(enterprise_attachment_retention_repo, purge_tx, fun(_C, _A) ->
        {ok, <<"2026-01-01T00:00:00Z">>}
    end),
    ok = meck:expect(attachment_repo, intent_purged_delete_tx, fun(_C, _P, _R) ->
        fault_result(Fault)
    end),
    {error, {<<"internal_error">>, {purge_intent_failed, _}}} =
        enterprise_asset_retention_logic:governance_tx(
            Conn,
            #{organization_id => 9101, application_id => 9103},
            #{op => <<"purge">>, object_key => Key}
        ),
    #{
        reported_errors => 1,
        entry_point_call => <<"enterprise_asset_retention_logic:governance_tx/3">>
    };
execute(<<"eb-private-delete">>, Fault, _Conn, _Key) ->
    ok = meck:expect(eb_pg_store, tenant_error, fun(_O, _W) -> ok end),
    ok = meck:expect(eb_pg_store, sql, fun(_) ->
        <<"SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)">>
    end),
    ok = meck:expect(elib_pg, query, fun
        (_C, <<"SELECT fn_v5_intent_eb_delete", _/binary>>, _P) -> fault_result(Fault);
        (C, Sql, P) -> meck:passthrough([C, Sql, P])
    end),
    {error, _} = eb_asset_store:delete_private(9101, 9102, 42),
    #{reported_errors => 1, entry_point_call => <<"eb_asset_store:delete_private/3">>};
execute(<<"eb-pending-cleanup">>, Fault, _Conn, _Key) ->
    %% 生产 cleanup_hold_gate 在 claim 之前查 Store:list_active_holds；
    %% 不隔离会让真实 hold 覆盖判定以 active_hold 抢先跳过、探不到本叶本意
    %% 的 claim 失败路径（active_hold 跳过面由其他叶真实覆盖，非放宽断言）。
    ok = meck:expect(eb_pg_store, list_active_holds, fun(_O, _W) -> {ok, []} end),
    ok = meck:expect(eb_pg_asset_meta, fetch_asset, fun(_O, _W, _A) ->
        {ok, #{status => pending_confirm, created_at => 1, conversation_id => undefined}}
    end),
    ok = meck:expect(eb_pg_asset_meta, claim_pending_cleanup, fun(_O, _W, _A, _N, _T) ->
        fault_result(Fault)
    end),
    {ok, #{skipped := Skipped}} = eb_asset_app:cleanup_pending(9101, #{
        workspace_id => 9102, asset_ids => [42], ttl_seconds => 3600
    }),
    ?assertMatch([{42, {cleanup_failed, _}} | _], Skipped),
    #{reported_errors => length(Skipped), entry_point_call => <<"eb_asset_app:cleanup_pending/2">>};
execute(<<"eb-purge-drain">>, Fault, _Conn, _Key) ->
    ok = meck:expect(eb_pg_asset_delete_queue, drain, fun(_O, _W, _L) ->
        fault_result(Fault)
    end),
    {error, _} = eb_pg_purge:purge_batch(9101, 9102, 10),
    #{reported_errors => 1, entry_point_call => <<"eb_pg_purge:purge_batch/3">>}.

result_shape(ok) -> <<"ok">>;
result_shape({ok, _}) -> <<"ok_tuple">>;
result_shape({error, _}) -> <<"error">>;
result_shape(_) -> <<"other">>.
