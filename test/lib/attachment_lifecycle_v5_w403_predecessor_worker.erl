%%% @doc W4-03 disk-predecessor-protocols / W4-02 四组协议在 disk 边界下的重放
%%% （worker-crash 6 + worker-db-fault 4 + queue-adoption 4 +
%%% ordinary-queue-recovery 4 = 18 叶）。
%%%
%%% 与源 case 的 oracle 一致（无丢任务、旧 token 0 更新、租约回收后可再消费、
%%% done 墓碑拒绝复用、跨队列身份冲突），但队列行与对象位置都指向真实 disk
%%% root，删除经 `attachment_storage:delete_snapshot/1` 真实落盘。
-module(attachment_lifecycle_v5_w403_predecessor_worker).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([run/4]).

-define(S, attachment_lifecycle_v5_w403_predecessor_support).
-define(ORG, 9101).
-define(WS, 9102).

run(<<"worker-crash">>, Scenario, Domain, Ctx) ->
    crash(Scenario, Domain, Ctx);
run(<<"worker-db-fault">>, Scenario, Domain, Ctx) ->
    db_fault(Scenario, Domain, Ctx);
run(<<"queue-adoption">>, Scenario, _Domain, Ctx) ->
    adoption(Scenario, Ctx);
run(<<"ordinary-queue-recovery">>, Scenario, _Domain, Ctx) ->
    recovery(Scenario, Ctx).

%% ------------------------------------------------------------ worker-crash

crash(Scenario, <<"ordinary">> = Domain, Ctx) ->
    ordinary_crash(Scenario, Domain, Ctx);
crash(Scenario, <<"enterprise">> = Domain, Ctx) ->
    enterprise_crash(Scenario, Domain, Ctx).

ordinary_crash(Scenario, Domain, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/crash">>),
    Body = <<"w403p-crash-bytes:", Scenario/binary>>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = reset_ordinary(Conn),
    ok = ?S:seed_ordinary_job(Conn, Key, 0, ready, ?S:locator(Key)),
    case Scenario of
        <<"kill_before_claim_commit">> ->
            {ok, _, [{0, <<"ready">>}]} = ?S:e(
                Conn,
                "SELECT attempts, job_state FROM attachment_object_deletion WHERE path=$1",
                [Key]
            ),
            ok = ?S:assert_disk_intact(Root, Key, Body),
            base(Scenario, Domain, Key, Body, #{
                zero_delete_calls => true, state_after => <<"ready">>
            });
        <<"kill_after_claim_before_delete">> ->
            T = ?S:token(),
            {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, T),
            expire_ordinary(Conn, Key),
            {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
            {ok, _, [{<<"retry_wait">>}]} = ?S:e(
                Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            T2 = ?S:token(),
            {ok, #{<<"out_path">> := Key, <<"out_attempts">> := 2}} =
                attachment_gc_repo:v5_claim(Conn, T2),
            ok = ?S:assert_disk_intact(Root, Key, Body),
            base(Scenario, Domain, Key, Body, #{recovered_after_reap => true, attempts_after => 2});
        <<"kill_after_delete_before_completion">> ->
            T = ?S:token(),
            {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, T),
            ok = attachment_storage:delete_snapshot(frozen(Conn, Key)),
            ?assertEqual({error, enoent}, file:read_file(?S:disk_file(Root, Key))),
            expire_ordinary(Conn, Key),
            {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
            T2 = ?S:token(),
            {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, T2),
            ok = attachment_gc_repo:v5_ack(Conn, Key, T2, true),
            {ok, _, [{<<"done">>}]} = ?S:e(
                Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            base(Scenario, Domain, Key, Body, #{resolved_to_done => true, bytes_gone => true})
    end.

enterprise_crash(Scenario, Domain, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/eb-crash">>),
    Body = <<"w403p-eb-crash-bytes:", Scenario/binary>>,
    ok = ?S:put_disk_file(Root, Key, Body),
    AssetId = asset_id(),
    ok = reset_eb(Conn),
    ok = seed_enterprise(Conn, AssetId, Key, true),
    ok = enqueue_eb(Conn, AssetId, <<"w403p eb crash">>),
    case Scenario of
        <<"kill_before_claim_commit">> ->
            {ok, _, [{0, <<"ready">>}]} = ?S:e(
                Conn,
                "SELECT attempts, job_state FROM enterprise_asset_delete_queue"
                " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
                [?ORG, ?WS, AssetId]
            ),
            ok = ?S:assert_disk_intact(Root, Key, Body),
            base(Scenario, Domain, Key, Body, #{
                zero_delete_calls => true, state_after => <<"ready">>
            });
        <<"kill_after_claim_before_delete">> ->
            {ok, #{attempts := 1}} = claim_eb(Conn),
            expire_eb(Conn, AssetId),
            {ok, _, [{1}]} = ?S:e(Conn, "SELECT fn_v5_reaper_eb(100)", []),
            {ok, _, [{<<"retry_wait">>}]} = eb_state(Conn, AssetId),
            {ok, #{attempts := 2}} = claim_eb_retry_for(Conn, AssetId),
            ok = ?S:assert_disk_intact(Root, Key, Body),
            base(Scenario, Domain, Key, Body, #{recovered_after_reap => true, attempts_after => 2});
        <<"kill_after_delete_before_completion">> ->
            {ok, #{token := T1}} = claim_eb_for(Conn, AssetId),
            ok = attachment_storage:delete_snapshot(eb_frozen(Conn, AssetId)),
            ?assertEqual({error, enoent}, file:read_file(?S:disk_file(Root, Key))),
            expire_eb(Conn, AssetId),
            {ok, _, [{1}]} = ?S:e(Conn, "SELECT fn_v5_reaper_eb(100)", []),
            {ok, #{token := T2}} = claim_eb_retry_for(Conn, AssetId),
            {ok, _, [{<<"done">>}]} = ?S:e(
                Conn,
                "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)",
                [?ORG, ?WS, AssetId, T2]
            ),
            _ = T1,
            base(Scenario, Domain, Key, Body, #{resolved_to_done => true, bytes_gone => true})
    end.

%% --------------------------------------------------------- worker-db-fault

db_fault(<<"db_lost_before_claim_verified">> = Scenario, <<"ordinary">> = Domain, Ctx) ->
    #{conn := Conn, root := Root} = Ctx,
    Key = ?S:unique_key(<<"v5e2e/w403p/fault">>),
    Body = <<"w403p-fault-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = reset_ordinary(Conn),
    ok = ?S:seed_ordinary_job(Conn, Key, 0, ready, ?S:locator(Key)),
    T = ?S:token(),
    {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, T),
    {ok, _, [{<<"stale_token">>}]} = ?S:e(
        Conn, "SELECT fn_v5_ack_ordinary($1, $2, true)", [Key, ?S:token()]
    ),
    {ok, _, [{<<"missing">>}]} = ?S:e(
        Conn,
        "SELECT fn_v5_ack_ordinary($1, $2, true)",
        [<<"v5e2e/w403p/never-existed">>, ?S:token()]
    ),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, Domain, Key, Body, #{stale_ack_zero_rows => true, missing_ack_no_crash => true});
db_fault(<<"db_lost_after_delete">> = Scenario, <<"ordinary">> = Domain, Ctx) ->
    #{conn := Conn, root := Root} = Ctx,
    Key = ?S:unique_key(<<"v5e2e/w403p/fault-after-del">>),
    Body = <<"w403p-fault-after-del-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = reset_ordinary(Conn),
    ok = ?S:seed_ordinary_job(Conn, Key, 0, ready, ?S:locator(Key)),
    T1 = ?S:token(),
    {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, T1),
    ok = attachment_storage:delete_snapshot(frozen(Conn, Key)),
    expire_ordinary(Conn, Key),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    T2 = ?S:token(),
    {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, T2),
    ok = attachment_gc_repo:v5_ack(Conn, Key, T2, true),
    {ok, _, [{<<"done">>, Attempts}]} = ?S:e(
        Conn, "SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ?assertEqual({error, enoent}, file:read_file(?S:disk_file(Root, Key))),
    base(Scenario, Domain, Key, Body, #{same_exact_location => Key, resolved_attempts => Attempts});
db_fault(<<"db_lost_before_claim_verified">> = Scenario, <<"enterprise">> = Domain, Ctx) ->
    #{conn := Conn, root := Root} = Ctx,
    Key = ?S:unique_key(<<"v5e2e/w403p/eb-fault">>),
    Body = <<"w403p-eb-fault-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    AssetId = asset_id(),
    ok = reset_eb(Conn),
    ok = seed_enterprise(Conn, AssetId, Key, false),
    ok = enqueue_eb(Conn, AssetId, <<"w403p eb fault">>),
    {ok, #{attempts := 1}} = claim_eb_for(Conn, AssetId),
    {ok, _, [{<<"stale_token">>}]} = ?S:e(
        Conn,
        "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)",
        [?ORG, ?WS, AssetId, ?S:token()]
    ),
    {ok, _, [{<<"missing">>}]} = ?S:e(
        Conn,
        "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)",
        [?ORG, ?WS, AssetId + 999999, ?S:token()]
    ),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, Domain, Key, Body, #{stale_ack_zero_rows => true, missing_ack_no_crash => true});
db_fault(<<"db_lost_after_delete">> = Scenario, <<"enterprise">> = Domain, Ctx) ->
    #{conn := Conn, root := Root} = Ctx,
    Key = ?S:unique_key(<<"v5e2e/w403p/eb-fault-after-del">>),
    Body = <<"w403p-eb-fault-after-del-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    AssetId = asset_id(),
    ok = reset_eb(Conn),
    ok = seed_enterprise(Conn, AssetId, Key, false),
    ok = enqueue_eb(Conn, AssetId, <<"w403p eb fault after del">>),
    {ok, #{token := T1}} = claim_eb_for(Conn, AssetId),
    ok = attachment_storage:delete_snapshot(eb_frozen(Conn, AssetId)),
    expire_eb(Conn, AssetId),
    {ok, _, [{1}]} = ?S:e(Conn, "SELECT fn_v5_reaper_eb(100)", []),
    {ok, #{token := T2, attempts := Attempts}} = claim_eb_retry_for(Conn, AssetId),
    {ok, _, [{<<"done">>}]} = ?S:e(
        Conn, "SELECT fn_v5_ack_eb($1,$2,$3,$4,true)", [?ORG, ?WS, AssetId, T2]
    ),
    ?assertEqual({error, enoent}, file:read_file(?S:disk_file(Root, Key))),
    _ = T1,
    base(Scenario, Domain, Key, Body, #{same_exact_location => Key, resolved_attempts => Attempts}).

%% ----------------------------------------------------------- queue-adoption

adoption(<<"enterprise_pending_adoption">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/adopt-pending">>),
    Body = <<"w403p-adopt-pending-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    AssetId = asset_id(),
    ok = reset_eb(Conn),
    ok = seed_enterprise(Conn, AssetId, Key, true),
    ok = enqueue_eb(Conn, AssetId, <<"w403p adoption">>),
    {ok, _, [{1}]} = ?S:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
        " AND job_state IN ('blocked','ready')",
        [?ORG, ?WS, AssetId]
    ),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, <<"enterprise">>, Key, Body, #{pending_flag_adopted => true, queue_rows => 1});
adoption(<<"enterprise_intent_replay">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/adopt-replay">>),
    Body = <<"w403p-adopt-replay-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    AssetId = asset_id(),
    ok = reset_eb(Conn),
    ok = seed_enterprise(Conn, AssetId, Key, false),
    ok = enqueue_eb(Conn, AssetId, <<"w403p replay 1">>),
    {error, #error{code = <<"23510">>}} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w403p replay 2')",
        [?ORG, ?WS, AssetId]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?S:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, <<"enterprise">>, Key, Body, #{
        idempotent_replay => true, replay_rejected => <<"23510">>, queue_rows => 1
    });
adoption(<<"ordinary_pk_preserved">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/adopt-pk">>),
    Body = <<"w403p-adopt-pk-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = reset_ordinary(Conn),
    ok = ?S:seed_ordinary_job(Conn, Key, 0, ready, ?S:locator(Key)),
    {ok, _, [{Key, <<"ready">>}]} = ?S:e(
        Conn,
        "SELECT path, job_state FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    {ok, _, [{Key, <<"ready">>}]} = ?S:e(
        Conn,
        "SELECT path, job_state FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, <<"enterprise">>, Key, Body, #{
        ordinary_pk => Key, state => <<"ready">>, untouched => true
    });
adoption(<<"cross_queue_identity_conflict">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/adopt-legacy">>),
    Body = <<"w403p-adopt-legacy-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = reset_ordinary(Conn),
    ?S:q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,attempts,last_error_code)"
        " VALUES ($1,'w403p-bucket','disk',$2,'c',NULL,'blocked',0,"
        " 'legacy_location_unverified') ON CONFLICT (path) DO NOTHING",
        [Key, ?S:ns()]
    ),
    {error, empty} = attachment_gc_repo:v5_claim(Conn, ?S:token()),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, <<"enterprise">>, Key, Body, #{blocked_legacy_not_claimed => true}).

%% ------------------------------------------------- ordinary-queue-recovery

recovery(<<"pending_recovery">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/rec-pending">>),
    Body = <<"w403p-rec-pending-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = ?S:seed_pending(Conn, Key, 48, <<"image/png">>, ?S:locator(Key)),
    {ok, _, _} = ?S:e(
        Conn, "SELECT fn_v5_intent_pending_delete($1, 'w403p rec pending', 'sweep')", [Key]
    ),
    {ok, _, [{1}]} = ?S:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, <<"ordinary">>, Key, Body, #{existing_queue_used => true, queue_rows => 1});
recovery(<<"orphan_recovery">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/rec-orphan">>),
    Body = <<"w403p-rec-orphan-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = ?S:seed_orphan(Conn, Key, orphan_id(), ?S:locator(Key)),
    {ok, _, _} = ?S:e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w403p rec orphan')", [Key]
    ),
    {ok, _, [{1}]} = ?S:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, <<"ordinary">>, Key, Body, #{existing_queue_used => true, queue_rows => 1});
recovery(<<"confirmed_fk_preserved">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/rec-fk">>),
    Body = <<"w403p-rec-fk-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    GfId = group_file_id(),
    ?S:q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    ?S:q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,'group','66',101,'disk',$3,'c',$4,true,NULL,$5,$6)"
        " ON CONFLICT (id) DO NOTHING",
        [
            attachment_id(),
            Key,
            ?S:ns(),
            ?S:locator(Key),
            GfId,
            ?S:hex64(crypto:hash(sha256, Key))
        ]
    ),
    ?S:q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1::bigint,66,$1::text,'101','t.bin','image/png','image','u',16)",
        [GfId]
    ),
    {error, #error{code = <<"23510">>}} = ?S:e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w403p rec fk')", [Key]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?S:e(
        Conn,
        "SELECT COUNT(*) FROM attachment WHERE path=$1 AND group_file_id=$2",
        [Key, GfId]
    ),
    ?S:q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    ok = ?S:assert_disk_intact(Root, Key, Body),
    base(Scenario, <<"ordinary">>, Key, Body, #{fk_intact => true, intent_rejected => <<"23510">>});
recovery(<<"done_path_reuse_rejected">> = Scenario, #{conn := Conn, root := Root}) ->
    Key = ?S:unique_key(<<"v5e2e/w403p/rec-done-reuse">>),
    Body = <<"w403p-rec-done-reuse-bytes">>,
    ok = ?S:put_disk_file(Root, Key, Body),
    ok = reset_ordinary(Conn),
    ok = ?S:seed_ordinary_job(Conn, Key, 0, ready, ?S:locator(Key)),
    T = ?S:token(),
    {ok, #{<<"out_path">> := Key}} = attachment_gc_repo:v5_claim(Conn, T),
    ok = attachment_storage:delete_snapshot(frozen(Conn, Key)),
    ok = attachment_gc_repo:v5_ack(Conn, Key, T, true),
    {ok, _, [{<<"done">>}]} = ?S:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {error, _} = ?S:e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w403p reuse')", [Key]),
    ok = epgsql:sync(Conn),
    ?assertEqual({error, enoent}, file:read_file(?S:disk_file(Root, Key))),
    base(Scenario, <<"ordinary">>, Key, Body, #{done_path => Key, reuse_rejected => true}).

%% ------------------------------------------------------------------ helpers

base(Scenario, Domain, Key, Body, Extra) ->
    maps:merge(
        #{
            source_scenario => Scenario,
            domain => Domain,
            object_key => Key,
            namespace => ?S:ns(),
            container => ?S:container(),
            probe_sha256 => ?S:hex64(crypto:hash(sha256, Body)),
            queue_rows_after => 0
        },
        Extra
    ).

%% 普通队列行的冻结位置快照（worker 的 frozen_location 同形）。
frozen(Conn, Key) ->
    {ok, _, [{Driver, Ns, Container, Locator}]} = ?S:e(
        Conn,
        "SELECT storage_driver, storage_namespace, storage_container,"
        " storage_locator_sha256 FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    {ok, I} = attachment_storage:frozen_location(Key, #{
        <<"storage_driver">> => Driver,
        <<"storage_namespace">> => Ns,
        <<"storage_container">> => Container,
        <<"storage_locator_sha256">> => Locator,
        <<"storage_physical_key">> => null
    }),
    I.

%% 企业队列行的冻结位置快照。
eb_frozen(Conn, AssetId) ->
    {ok, _, [{Driver, Ns, Container, Locator, Key}]} = ?S:e(
        Conn,
        "SELECT storage_driver, storage_namespace, storage_container,"
        " storage_locator_sha256, object_key FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ),
    {ok, I} = attachment_storage:frozen_location(Key, #{
        <<"storage_driver">> => Driver,
        <<"storage_namespace">> => Ns,
        <<"storage_container">> => Container,
        <<"storage_locator_sha256">> => Locator,
        <<"storage_physical_key">> => null
    }),
    I.

expire_ordinary(Conn, Key) ->
    ?S:q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Key]
    ).

expire_eb(Conn, AssetId) ->
    ?S:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET lease_until = now() - interval '1 s'"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ).

eb_state(Conn, AssetId) ->
    ?S:e(
        Conn,
        "SELECT job_state FROM enterprise_asset_delete_queue"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3",
        [?ORG, ?WS, AssetId]
    ).

%% 领一条企业队列任务（claim 返回 10 列 tuple，含 out_storage_physical_key）。
claim_eb(Conn) ->
    T = ?S:token(),
    case ?S:e(Conn, "SELECT * FROM fn_v5_claim_eb($1)", [T]) of
        {ok, _, [{Org, Ws, Asset, Key, Driver, Ns, C, Loc, _Phys, Attempts}]} ->
            {ok, #{
                token => T,
                attempts => Attempts,
                org => Org,
                ws => Ws,
                asset_id => Asset,
                object_key => Key,
                driver => Driver,
                namespace => Ns,
                container => C,
                locator => Loc
            }};
        {ok, _, []} ->
            empty;
        Other ->
            erlang:error({w403p_claim_eb_failed, Other})
    end.

%% 只领指定资产：队列里残留的其他叶行先 ack 收回再继续。
claim_eb_for(Conn, AssetId) ->
    case claim_eb(Conn) of
        {ok, #{asset_id := AssetId} = Row} ->
            {ok, Row};
        {ok, Other} ->
            ok = ack_eb(Conn, Other, true),
            claim_eb_for(Conn, AssetId);
        empty ->
            erlang:error({w403p_target_eb_job_not_claimed, AssetId})
    end.

ack_eb(Conn, #{org := Org, ws := Ws, asset_id := Asset, token := T}, Ok) ->
    case ?S:e(Conn, "SELECT fn_v5_ack_eb($1,$2,$3,$4,$5)", [Org, Ws, Asset, T, Ok]) of
        {ok, _, [State]} -> {ok, State};
        Other -> erlang:error({w403p_ack_eb_failed, Other})
    end.

claim_eb_retry_for(Conn, AssetId) ->
    ?S:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET next_retry_at = now() - interval '1 s'"
        " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
        " AND job_state='retry_wait'",
        [?ORG, ?WS, AssetId]
    ),
    claim_eb_for(Conn, AssetId).

enqueue_eb(Conn, AssetId, Reason) ->
    case
        ?S:e(
            Conn,
            "SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)",
            [?ORG, ?WS, AssetId, Reason]
        )
    of
        {ok, _, _} -> ok;
        {error, #error{code = <<"23510">>}} = Err -> Err
    end.

seed_enterprise(Conn, AssetId, Key, PendingFlag) ->
    ?S:q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,pending_object_delete,storage_driver,storage_namespace,"
        "  storage_container,storage_locator_sha256,lifecycle_state)"
        " VALUES ($1,$2,$3,$4,$5,'text/plain',8,'deleted',$6,'disk',$7,'c',$8,'live')"
        " ON CONFLICT (id) DO NOTHING",
        [
            AssetId,
            ?ORG,
            ?WS,
            Key,
            ?S:hex64(crypto:hash(sha256, Key)),
            PendingFlag,
            ?S:ns(),
            ?S:locator(Key)
        ]
    ).

%% 每叶开始前清掉前序叶/上次重试的队列残留：强制过期租约 → reaper →
%% claim/ack(true) 到 done。done 是终态墓碑，但每叶用新 key/新 asset。
reset_ordinary(Conn) ->
    ?S:q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE job_state='leased'"
    ),
    _ = attachment_gc_repo:v5_reap(Conn, 100),
    drain_ordinary(Conn).

drain_ordinary(Conn) ->
    T = ?S:token(),
    case attachment_gc_repo:v5_claim(Conn, T) of
        {ok, #{<<"out_path">> := P}} ->
            ok = attachment_gc_repo:v5_ack(Conn, P, T, true),
            drain_ordinary(Conn);
        {error, empty} ->
            ok
    end.

reset_eb(Conn) ->
    ?S:q(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET lease_until = now() - interval '1 s'"
        " WHERE job_state='leased'"
    ),
    _ = ?S:e(Conn, "SELECT fn_v5_reaper_eb(200)", []),
    drain_eb(Conn).

drain_eb(Conn) ->
    case claim_eb(Conn) of
        {ok, Row} ->
            {ok, _} = ack_eb(Conn, Row, true),
            drain_eb(Conn);
        empty ->
            ok
    end.

asset_id() -> 950000000000 + rand_suffix().
orphan_id() -> 946000000000 + rand_suffix().
attachment_id() -> 947000000000 + rand_suffix().
group_file_id() -> 948000000000 + rand_suffix().

rand_suffix() ->
    binary:decode_unsigned(crypto:strong_rand_bytes(4)) rem 100000000.
