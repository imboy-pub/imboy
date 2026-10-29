%%% @doc W6-01 E 组：W4-02 队列组（old-attempts-preserved 16 +
%%% worker-db-fault 4 + queue-adoption 4 + ordinary-queue-recovery 4 +
%%% cross-queue-identity 4 = 32 叶），schema_phase=integrated。
%%%
%%% oracle 复用封印源 w402_worker_tests（旧 attempts 保留/legacy 形态不被
%%% 接管、stale token 0 行更新、企业 pending flag 经意图入队、done 墓碑拒绝
%%% 复用、跨队列物理身份锁内查重拒绝）。fn 协议面：ordinary 叶真实执行；
%%% enterprise 叶同面重放并披露 shared_protocol_face。driver 维度 = 队列行/
%%% 资产行冻结身份三元组指向该叶 driver 的真实 registry 根。
-module(attachment_lifecycle_v5_w601_w4_queue_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(X, attachment_lifecycle_v5_w601_w4_context).
-define(QP, attachment_lifecycle_v5_w601_w4_queueproto).
-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).
-define(XDOM, <<"cross_domain">>).

w601_w4_queue_test_() ->
    {setup, fun() -> ?X:setup(<<"V5_INTTEST_W601W4QUEUE">>) end, fun ?X:cleanup/1, fun(St) ->
        [?X:leaf(S, fun() -> scenario(St, S) end) || S <- specs()]
    end}.

specs() ->
    lists:append([
        dual(<<"W4-02/old-attempts-preserved">>, <<"old-attempts-preserved">>, [
            <<"legacy_attempts_above8">>,
            <<"legacy_done">>,
            <<"legacy_nonzero_attempts">>,
            <<"nonzero_proof_exception_rejected">>
        ]),
        garage_dual(<<"W4-02/worker-db-fault">>, <<"worker-db-fault">>, [
            <<"db_lost_before_claim_verified">>,
            <<"db_lost_after_delete">>
        ]),
        solo(<<"W4-02/queue-adoption">>, <<"queue-adoption">>, ?ENT, [
            <<"enterprise_pending_adoption">>,
            <<"enterprise_intent_replay">>,
            <<"ordinary_pk_preserved">>,
            <<"cross_queue_identity_conflict">>
        ]),
        solo(<<"W4-02/ordinary-queue-recovery">>, <<"ordinary-queue-recovery">>, ?ORD, [
            <<"pending_recovery">>,
            <<"orphan_recovery">>,
            <<"confirmed_fk_preserved">>,
            <<"done_path_reuse_rejected">>
        ]),
        cross_specs()
    ]).

dual(Case, Entry, Scenarios) ->
    [
        {Case, Entry, Drv, Scn, Dom}
     || Scn <- Scenarios, Dom <- [?ORD, ?ENT], Drv <- [<<"disk">>, <<"garage">>]
    ].

garage_dual(Case, Entry, Scenarios) ->
    [
        {Case, Entry, <<"garage">>, Scn, Dom}
     || Scn <- Scenarios, Dom <- [?ORD, ?ENT]
    ].

solo(Case, Entry, Dom, Scenarios) ->
    [{Case, Entry, <<"garage">>, Scn, Dom} || Scn <- Scenarios].

cross_specs() ->
    [
        {<<"W4-02/cross-queue-identity">>, <<"cross-queue-identity">>, Drv, Scn, ?XDOM}
     || Scn <- [<<"simultaneous_domain_producers">>, <<"unknown_shared_history">>],
        Drv <- [<<"disk">>, <<"garage">>]
    ].

%% ------------------------------------------------------------- 分发

scenario(St, {_Case, Entry, Drv, Scn, Dom}) ->
    #{conn := Conn} = St,
    N = ((?X:leaf_id(qbase) rem 100000) * 1000 + ?X:leaf_id(queue)),
    Key = <<"v5e2e/w601q/", Entry/binary, "/", (integer_to_binary(N))/binary>>,
    Triple = ?X:triple(St, Drv, Key),
    Measured = run_scenario(St, Conn, Entry, Scn, Key, Triple, N),
    Measured#{
        driver => Drv,
        object_key => Key,
        storage_namespace => maps:get(namespace, Triple),
        storage_container => maps:get(container, Triple),
        locator_sha256 => maps:get(locator, Triple),
        domain_face => Dom,
        shared_protocol_face => Dom =:= ?ENT
    }.

seed_job(Conn, Key, Triple, Attempts, State) ->
    ok = ?X:seed_ordinary_job_row(
        Conn,
        Key,
        Triple,
        Attempts,
        State,
        case State of
            blocked -> <<"max_attempts">>;
            _ -> null
        end
    ).

%% legacy 位置未证实形态：locator 必须为 NULL（协议面如实保留）。
seed_legacy_job(Conn, Key, Triple, Attempts, State, LastError) ->
    ?X:q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,next_retry_at,attempts,last_error_code)"
        " VALUES ($1,'w601-b',$2,$3,$4,NULL,$5,now(),$6,$7)"
        " ON CONFLICT (path) DO UPDATE SET job_state=EXCLUDED.job_state,"
        " attempts=EXCLUDED.attempts, last_error_code=EXCLUDED.last_error_code,"
        " storage_locator_sha256=NULL",
        [
            Key,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            State,
            Attempts,
            LastError
        ]
    ),
    ok.

%% ------------------------------------------------- old-attempts-preserved

run_scenario(St, Conn, Entry, Scn, Key, Triple, N) ->
    case Entry of
        <<"old-attempts-preserved">> -> attempts_cycle(Conn, Scn, Key, Triple);
        <<"worker-db-fault">> -> db_fault_cycle(Conn, Scn, Key, Triple);
        <<"queue-adoption">> -> adoption_cycle(St, Conn, Scn, Key, Triple, N);
        <<"ordinary-queue-recovery">> -> recovery_cycle(St, Conn, Scn, Key, Triple, N);
        <<"cross-queue-identity">> -> cross_cycle(St, Conn, Scn, Key, Triple, N)
    end.

attempts_cycle(Conn, Scn, Key, Triple) ->
    case Scn of
        <<"legacy_nonzero_attempts">> ->
            ?QP:drain_ordinary(Conn),
            seed_job(Conn, Key, Triple, 5, ready),
            T = ?X:token(),
            {ok, #{<<"out_attempts">> := 6}} = ?QP:claim_ordinary(Conn, T),
            ok = attachment_gc_repo:v5_ack(Conn, Key, T, false),
            {ok, _, [{6, <<"delete_failed">>}]} = ?X:e(
                Conn,
                "SELECT attempts, last_error_code FROM attachment_object_deletion"
                " WHERE path=$1",
                [Key]
            ),
            #{attempts_after => 6, not_reset => true};
        <<"legacy_done">> ->
            ?QP:drain_ordinary(Conn),
            seed_job(Conn, Key, Triple, 4, ready),
            T = ?X:token(),
            {ok, _} = ?QP:claim_ordinary(Conn, T),
            ok = attachment_gc_repo:v5_ack(Conn, Key, T, true),
            {ok, _, [{<<"done">>, 5}]} = ?X:e(
                Conn,
                "SELECT job_state, attempts FROM attachment_object_deletion"
                " WHERE path=$1",
                [Key]
            ),
            {ok, _, [{<<"done">>}]} = ?X:e(
                Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            #{stays_done => true, attempts => 5};
        <<"legacy_attempts_above8">> ->
            ?QP:drain_ordinary(Conn),
            seed_job(Conn, Key, Triple, 9, ready),
            {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
            {ok, _, [{9, <<"ready">>}]} = ?X:e(
                Conn,
                "SELECT attempts, job_state FROM attachment_object_deletion"
                " WHERE path=$1",
                [Key]
            ),
            #{attempts_above8_preserved => 9, unchanged => true};
        <<"nonzero_proof_exception_rejected">> ->
            proof_exception_rejected(Conn, Key, Triple)
    end.

%% legacy 位置未证实形态不得用零次尝试证明例外（行保持原样）。
proof_exception_rejected(Conn, Key, Triple) ->
    seed_legacy_job(Conn, Key, Triple, 3, blocked, <<"legacy_location_unverified">>),
    {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
    {ok, _, [{<<"blocked">>, 3, <<"legacy_location_unverified">>}]} = ?X:e(
        Conn,
        "SELECT job_state, attempts, last_error_code"
        " FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    #{blocked_attempts_unchanged => 3, claimed => false}.

%% ------------------------------------------------------ worker-db-fault

db_fault_cycle(Conn, Scn, Key, Triple) ->
    ?QP:drain_ordinary(Conn),
    case Scn of
        <<"db_lost_before_claim_verified">> ->
            seed_job(Conn, Key, Triple, 0, ready),
            T = ?X:token(),
            {ok, _} = ?QP:claim_ordinary(Conn, T),
            {ok, _, [{<<"stale_token">>}]} = ?X:e(
                Conn, "SELECT fn_v5_ack_ordinary($1, $2, true)", [Key, ?X:token()]
            ),
            {ok, _, [{<<"missing">>}]} = ?X:e(
                Conn,
                "SELECT fn_v5_ack_ordinary($1, $2, true)",
                [<<"v5e2e/w601q/never-existed">>, ?X:token()]
            ),
            #{stale_ack_zero_rows => true, missing_ack_no_crash => true};
        <<"db_lost_after_delete">> ->
            seed_job(Conn, Key, Triple, 0, ready),
            T1 = ?X:token(),
            {ok, _} = ?QP:claim_ordinary(Conn, T1),
            ?QP:expire_ordinary(Conn, Key),
            {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
            T2 = ?X:token(),
            {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, T2),
            ok = attachment_gc_repo:v5_ack(Conn, Key, T2, true),
            {ok, _, [{<<"done">>, Attempts}]} = ?X:e(
                Conn,
                "SELECT job_state, attempts FROM attachment_object_deletion"
                " WHERE path=$1",
                [Key]
            ),
            #{same_exact_location => Key, resolved_attempts => Attempts}
    end.

%% ------------------------------------------------------- queue-adoption

adoption_cycle(St, Conn, Scn, Key, Triple, N) ->
    case Scn of
        <<"enterprise_pending_adoption">> ->
            eb_pending_adoption(St, Conn, Key, Triple, N);
        <<"enterprise_intent_replay">> ->
            eb_intent_replay(St, Conn, Key, Triple, N);
        <<"ordinary_pk_preserved">> ->
            ordinary_pk_preserved(Conn, Key, Triple);
        <<"cross_queue_identity_conflict">> ->
            ?QP:reset_ordinary(Conn),
            seed_legacy_job(Conn, Key, Triple, 0, blocked, <<"legacy_location_unverified">>),
            {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
            #{blocked_legacy_not_claimed => true}
    end.

eb_pending_adoption(St, Conn, Key, Triple, N) ->
    EbId = eb_id(N),
    ok = ?X:seed_eb_asset_row(Conn, Key, EbId, Triple, <<"deleted">>, true, <<"live">>),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w601q adoption')", [9101, 9102, EbId]
    ),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1"
        " AND job_state IN ('blocked','ready')",
        [EbId]
    ),
    #{
        pending_flag_adopted => true,
        queue_rows => 1,
        asset_id => EbId,
        scope_note => note(St)
    }.

eb_intent_replay(St, Conn, Key, Triple, N) ->
    EbId = eb_id(N),
    ok = ?X:seed_eb_asset_row(Conn, Key, EbId, Triple, <<"deleted">>, false, <<"live">>),
    {ok, _, _} = ?X:e(
        Conn, "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w601q replay 1')", [9101, 9102, EbId]
    ),
    {error, #error{code = <<"23510">>}} = ?X:e(
        Conn, "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w601q replay 2')", [9101, 9102, EbId]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [EbId]
    ),
    #{
        idempotent_replay => true,
        replay_rejected => <<"23510">>,
        queue_rows => 1,
        scope_note => note(St)
    }.

ordinary_pk_preserved(Conn, Key, Triple) ->
    ?QP:reset_ordinary(Conn),
    seed_job(Conn, Key, Triple, 0, ready),
    {ok, _, [{Key, <<"ready">>}]} = ?X:e(
        Conn, "SELECT path, job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {ok, _, [{Key, <<"ready">>}]} = ?X:e(
        Conn, "SELECT path, job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{ordinary_pk => Key, state => <<"ready">>, untouched => true}.

eb_id(N) -> 949000000000 + N.

note(_St) -> <<"eb identity triple of leaf driver">>.

%% ------------------------------------------------ ordinary-queue-recovery

recovery_cycle(St, Conn, Scn, Key, Triple, N) ->
    case Scn of
        <<"pending_recovery">> ->
            _ = ?X:seed_pending_session(St, Conn, Key, maps:get(driver, Triple)),
            {ok, _, _} = ?X:e(
                Conn,
                "SELECT fn_v5_intent_pending_delete($1,'w601q rec pending','sweep')",
                [Key]
            ),
            {ok, _, [{1}]} = ?X:e(
                Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            #{existing_queue_used => true, queue_rows => 1};
        <<"orphan_recovery">> ->
            ok = ?X:seed_orphan_row(
                Conn, 947000000000 + N, Key, Triple, 0
            ),
            {ok, _, _} = ?X:e(
                Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601q rec orphan')", [Key]
            ),
            {ok, _, [{1}]} = ?X:e(
                Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            #{existing_queue_used => true, queue_rows => 1};
        <<"confirmed_fk_preserved">> ->
            confirmed_fk(St, Conn, Key, Triple, N);
        <<"done_path_reuse_rejected">> ->
            ?QP:reset_ordinary(Conn),
            seed_job(Conn, Key, Triple, 0, ready),
            T = ?X:token(),
            {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, T),
            ok = attachment_storage:delete_snapshot(?QP:frozen_ordinary(Conn, Key)),
            ok = attachment_gc_repo:v5_ack(Conn, Key, T, true),
            {ok, _, [{<<"done">>}]} = ?X:e(
                Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            {error, _} = ?X:e(
                Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601q reuse')", [Key]
            ),
            ok = epgsql:sync(Conn),
            #{done_path => Key, reuse_rejected => true}
    end.

confirmed_fk(_St, Conn, Key, Triple, N) ->
    GfId = 946000000000 + N,
    AttId = 946100000000 + N,
    ?X:q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    ?X:q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,storage_locator_sha256,"
        "gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,'group','66',101,$3,$4,$5,$6,true,NULL,$7,$8)"
        " ON CONFLICT (id) DO NOTHING",
        [
            AttId,
            Key,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple),
            GfId,
            ?X:hex64(crypto:hash(sha256, Key))
        ]
    ),
    ?X:q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,"
        "file_type,file_category,file_url,file_size) VALUES"
        " ($1::bigint,66,$1::text,'101','t.bin','image/png','image','u',16)",
        [GfId]
    ),
    {error, #error{code = <<"23510">>}} = ?X:e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601q rec fk')", [Key]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM attachment WHERE path=$1 AND group_file_id=$2",
        [Key, GfId]
    ),
    ?X:q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    #{fk_intact => true, intent_rejected => <<"23510">>}.

%% -------------------------------------------------- cross-queue-identity

cross_cycle(_St, Conn, Scn, Key, Triple, N) ->
    EbId = 950000000000 + N,
    ?X:q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',8,'active',$4,$5,$6,$7,'live')"
        " ON CONFLICT (id) DO NOTHING",
        [
            EbId,
            Key,
            ?X:hex64(crypto:hash(sha256, Key)),
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    case Scn of
        <<"simultaneous_domain_producers">> ->
            cross_simultaneous(Conn, Key, Triple, N, EbId);
        <<"unknown_shared_history">> ->
            cross_unknown(Conn, Triple)
    end.

cross_simultaneous(Conn, Key, Triple, N, EbId) ->
    {ok, _, [{0}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ok = ?X:seed_orphan_row(Conn, 950100000000 + N, Key, Triple, 0),
    ?X:q(
        Conn,
        "INSERT INTO enterprise_asset_delete_queue"
        " (organization_id,workspace_id,asset_id,object_key,storage_driver,"
        "  storage_namespace,storage_container,storage_locator_sha256,"
        "  job_state,next_retry_at,attempts)"
        " VALUES (9101,9102,$1,$2,$3,$4,$5,$6,'ready',now(),0)"
        " ON CONFLICT (organization_id,workspace_id,asset_id) DO NOTHING",
        [
            EbId,
            Key,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    {error, #error{code = <<"23510">>}} = ?X:e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w601q cross')", [Key]
    ),
    ok = epgsql:sync(Conn),
    #{same_physical_identity_rejected => <<"23510">>, asset_id => EbId}.

cross_unknown(Conn, Triple) ->
    {ok, _, [{OrdRows}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion"
        " WHERE storage_namespace=$1 AND storage_container=$2"
        " AND storage_locator_sha256=$3",
        [
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    {ok, _, [{EbRows}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE storage_namespace=$1 AND storage_container=$2"
        " AND storage_locator_sha256=$3",
        [
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    #{
        no_history_guessed => true,
        pre_check_ordinary_rows => OrdRows,
        pre_check_eb_rows => EbRows,
        identity_triple_of => maps:get(driver, Triple)
    }.
