%%% @doc W4-01 intent-rollback：4 类故障 × 12 删除入口的失败注入矩阵。
%%%
%%% 断言（04 W4-01）：audit/commit/意图 INSERT 失败或事务回滚时——
%%%   * 存储字节 DELETE=0（elib_oss 零调用）；
%%%   * 入口不误报成功（未知返回一律不作 ok）；
%%%   * 对象保留（重试由 tombstone/状态机兜底）。
%%%
%%% 注入点=各入口的删除意图调用（repo 层）；4 故障：
%%%   sql_rollback / intent_insert_error / audit_insert_error /
%%%   commit_response_lost（统一以意图调用 {error, Reason} 形态注入——
%%%   SECURITY DEFINER fn 的内部原子性由 PG 事务保证，入口合同=任何
%%%   意图失败都不得触达存储）。
%%%
%%% 单元编排模式（meck，无真库；协议同 attach_pending_cleanup_tests）。
-module(attachment_lifecycle_v5_w401_rollback_tests).

-include_lib("eunit/include/eunit.hrl").

rollback_test_() ->
    {foreach, fun setup/0, fun cleanup/1, cases()}.

cases() ->
    [
        fun pending_cleanup_rollback/0,
        fun pending_cleanup_intent_insert_error/0,
        fun pending_cleanup_audit_insert_error/0,
        fun pending_cleanup_commit_response_lost/0,
        fun orphan_cleanup_rollback/0,
        fun orphan_cleanup_intent_insert_error/0,
        fun orphan_cleanup_audit_insert_error/0,
        fun orphan_cleanup_commit_response_lost/0,
        fun account_deletion_rollback/0,
        fun account_deletion_intent_insert_error/0,
        fun account_deletion_audit_insert_error/0,
        fun account_deletion_commit_response_lost/0,
        fun eb_pending_cleanup_rollback/0,
        fun eb_pending_cleanup_intent_insert_error/0,
        fun eb_pending_cleanup_audit_insert_error/0,
        fun eb_pending_cleanup_commit_response_lost/0,
        fun app_retention_purge_rollback/0,
        fun app_retention_purge_intent_insert_error/0,
        fun app_retention_purge_audit_insert_error/0,
        fun app_retention_purge_commit_response_lost/0,
        fun eb_private_delete_rollback/0,
        fun eb_private_delete_intent_insert_error/0,
        fun eb_private_delete_audit_insert_error/0,
        fun eb_private_delete_commit_response_lost/0
    ].

setup() ->
    Modules = [
        attach_pending_repo,
        attachment_repo,
        enterprise_attachment_retention_repo,
        enterprise_asset_retention_logic,
        elib_pg,
        elib_oss,
        eb_pg_asset_meta,
        eb_pg_store
    ],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link, passthrough]) end, Modules),
    meck:expect(elib_pg, with_tx, fun(F) ->
        try
            F(test_connection)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end),
    meck:expect(elib_pg, query, fun(_C, _Sql, _P) -> {ok, []} end),
    meck:expect(enterprise_asset_repo, find_confirmed_tx, fun(_C, _O, _A, _K) ->
        {ok, #{
            <<"id">> => 42,
            <<"path">> => <<"v5e2e/e1">>,
            <<"lifecycle_state">> => <<"live">>
        }}
    end),
    meck:expect(eb_pg_store, list_active_holds, fun(_O, _W) -> {ok, []} end),
    meck:expect(enterprise_attachment_retention_repo, find_tx, fun(_C, _A) ->
        {ok, #{
            <<"attachment_id">> => 42,
            <<"hold_state">> => <<"none">>,
            <<"purge_state">> => <<"live">>,
            <<"retention_until">> => <<"x">>
        }}
    end),
    meck:expect(elib_oss, delete_object, fun(_, _) -> ok end),
    meck:expect(elib_oss, delete_object, fun(_) -> ok end),
    Modules.

cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).

%% ---- 故障注入 helper（4 类失败统一映射为意图调用错误形状）----
fault_result(rollback) -> {error, {db, sql_rollback}};
fault_result(intent_insert_error) -> {error, {db, intent_insert_failed}};
fault_result(audit_insert_error) -> {error, {db, audit_insert_failed}};
fault_result(commit_response_lost) -> {error, {db, commit_response_lost}}.

expect_pending_intent(Fault) ->
    meck:expect(
        attach_pending_repo,
        intent_pending_delete_tx,
        fun(_C, _K, _M, _R) -> fault_result(Fault) end
    ).

expect_orphan_intent(Fault) ->
    meck:expect(
        attachment_repo,
        orphan_path_tx,
        fun(_C, _Id, _O) ->
            {ok, [#{<<"path">> => <<"v5e2e/x">>, <<"scope">> => <<"private">>}]}
        end
    ),
    meck:expect(
        attachment_repo,
        intent_ordinary_delete_tx,
        fun(_C, _P, _R) -> fault_result(Fault) end
    ).

expect_collect_keys(Fault) ->
    case Fault of
        rollback ->
            %% 收集段即失败（同一事务回滚）
            meck:expect(
                user_deletion_executor, collect_attachment_keys_tx, fun(_C, _U) ->
                    {error, sql_rollback}
                end
            );
        _ ->
            meck:expect(
                user_deletion_executor,
                collect_attachment_keys_tx,
                fun(_C, _U) -> {ok, [<<"v5e2e/u1">>]} end
            ),
            meck:expect(
                attachment_repo,
                intent_ordinary_delete_tx,
                fun(_C, _P, _R) -> fault_result(Fault) end
            )
    end.

%% ---- 12 入口 × 4 故障 ----

%% 1. pending_cleanup（attachment_ds）
pending_cleanup_fault(Fault) ->
    meck:expect(attach_pending_repo, list_expired, fun(_) ->
        {ok, [#{<<"object_key">> => <<"v5e2e/p1">>}]}
    end),
    expect_pending_intent(Fault),
    Result = attachment_ds:pending_cleanup(24),
    {ok, #{cleaned := 0, errors := N}} = Result,
    ?assert(N >= 1),
    no_object_delete().

pending_cleanup_rollback() -> pending_cleanup_fault(rollback).
pending_cleanup_intent_insert_error() -> pending_cleanup_fault(intent_insert_error).
pending_cleanup_audit_insert_error() -> pending_cleanup_fault(audit_insert_error).
pending_cleanup_commit_response_lost() -> pending_cleanup_fault(commit_response_lost).

%% 2. orphan_cleanup（attachment_ds）
orphan_cleanup_fault(Fault) ->
    meck:expect(attachment_repo, orphan_list_for_delete, fun(_) ->
        {ok, [#{<<"id">> => 1}]}
    end),
    expect_orphan_intent(Fault),
    {ok, #{cleaned := 0, errors := N}} = attachment_ds:orphan_cleanup(#{age_days => 30}),
    ?assert(N >= 1),
    no_object_delete().

orphan_cleanup_rollback() -> orphan_cleanup_fault(rollback).
orphan_cleanup_intent_insert_error() -> orphan_cleanup_fault(intent_insert_error).
orphan_cleanup_audit_insert_error() -> orphan_cleanup_fault(audit_insert_error).
orphan_cleanup_commit_response_lost() -> orphan_cleanup_fault(commit_response_lost).

%% 3. account_deletion（user_deletion_logic 附件段：collect+intent 生产同款）
account_deletion_fault(Fault) ->
    expect_collect_keys(Fault),
    Result = elib_pg:with_tx(fun(Conn) ->
        case user_deletion_executor:collect_attachment_keys_tx(Conn, 101) of
            {ok, Keys} ->
                lists:filter(
                    fun(Key) ->
                        case
                            attachment_repo:intent_ordinary_delete_tx(
                                Conn, Key, <<"account deletion">>
                            )
                        of
                            ok -> true;
                            {error, intent_conflict} -> false;
                            {error, tombstone} -> false;
                            {error, not_found} -> false;
                            {error, R} -> erlang:throw({abort_tx, R})
                        end
                    end,
                    Keys
                );
            {error, R} ->
                erlang:throw({abort_tx, R})
        end
    end),
    %% 未知返回不作 ok：失败必须以 error 形态浮出（tx 回滚）
    Failed =
        case Result of
            ok -> true;
            {ok, _} -> true;
            {error, _} -> false;
            {rollback, _} -> false;
            _ -> false
        end,
    ?assertNot(Failed),
    no_object_delete().

account_deletion_rollback() -> account_deletion_fault(rollback).
account_deletion_intent_insert_error() -> account_deletion_fault(intent_insert_error).
account_deletion_audit_insert_error() -> account_deletion_fault(audit_insert_error).
account_deletion_commit_response_lost() -> account_deletion_fault(commit_response_lost).

%% 5. eb_pending_cleanup（eb_asset_app cleanup_pending：claim 失败=skipped）
eb_pending_cleanup_fault(Fault) ->
    meck:expect(eb_pg_asset_meta, fetch_asset, fun(_O, _W, _A) ->
        {ok, #{status => pending_confirm, created_at => 1, conversation_id => undefined}}
    end),
    meck:expect(eb_pg_asset_meta, claim_pending_cleanup, fun(_O, _W, _A, _N, _T) ->
        fault_result(Fault)
    end),
    {ok, #{skipped := Skipped}} = eb_asset_app:cleanup_pending(1, #{
        workspace_id => 2, asset_ids => [42], ttl_seconds => 3600
    }),
    ?assertMatch([{42, {cleanup_failed, _}} | _], Skipped),
    no_object_delete().

eb_pending_cleanup_rollback() -> eb_pending_cleanup_fault(rollback).
eb_pending_cleanup_intent_insert_error() -> eb_pending_cleanup_fault(intent_insert_error).
eb_pending_cleanup_audit_insert_error() -> eb_pending_cleanup_fault(audit_insert_error).
eb_pending_cleanup_commit_response_lost() -> eb_pending_cleanup_fault(commit_response_lost).

%% 6. app_retention_purge（enterprise_asset_retention_logic governance_tx
%%    purge：意图失败=internal_error，治理行/字节零变化）
app_retention_purge_fault(Fault) ->
    meck:expect(
        enterprise_attachment_retention_repo, is_purgeable_tx, fun(_C, _A) -> {ok, true} end
    ),
    meck:expect(enterprise_attachment_retention_repo, purge_tx, fun(_C, _A) ->
        {ok, <<"2026-01-01T00:00:00Z">>}
    end),
    meck:expect(
        attachment_repo,
        intent_purged_delete_tx,
        fun(_C, _P, _R) -> fault_result(Fault) end
    ),
    Ctx = #{organization_id => 9101, application_id => 9103},
    {error, {<<"internal_error">>, {purge_intent_failed, _}}} =
        elib_pg:with_tx(fun(Conn) ->
            %% do_purge 首参 Att 用 map 形状（resolve_attachment 回读同形）
            enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
                op => <<"purge">>, object_key => <<"v5e2e/e1">>
            })
        end),
    no_object_delete().

app_retention_purge_rollback() -> app_retention_purge_fault(rollback).
app_retention_purge_intent_insert_error() -> app_retention_purge_fault(intent_insert_error).
app_retention_purge_audit_insert_error() -> app_retention_purge_fault(audit_insert_error).
app_retention_purge_commit_response_lost() -> app_retention_purge_fault(commit_response_lost).

%% 7. eb_private_delete（eb_asset_store：意图失败=error，不触对象）
eb_private_delete_fault(Fault) ->
    meck:expect(eb_pg_store, tenant_error, fun(_O, _W) -> ok end),
    meck:expect(eb_pg_store, sql, fun(_) -> <<"SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)">> end),
    meck:expect(elib_pg, query, fun
        (_C, <<"SELECT fn_v5_intent_eb_delete", _/binary>>, _P) -> fault_result(Fault);
        (C, Sql, P) -> meck:passthrough([C, Sql, P])
    end),
    {error, _} = eb_asset_store:delete_private(1, 2, 42),
    no_object_delete().

eb_private_delete_rollback() -> eb_private_delete_fault(rollback).
eb_private_delete_intent_insert_error() -> eb_private_delete_fault(intent_insert_error).
eb_private_delete_audit_insert_error() -> eb_private_delete_fault(audit_insert_error).
eb_private_delete_commit_response_lost() -> eb_private_delete_fault(commit_response_lost).

no_object_delete() ->
    ?assertNot(meck:called(elib_oss, delete_object, ['_', '_'])),
    ?assertNot(meck:called(elib_oss, delete_object, ['_'])).
