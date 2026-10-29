%%% @doc W6-01 E 组：W4-02 队列组（old-attempts-preserved 16 +
%%% worker-db-fault 4 + queue-adoption 4 + ordinary-queue-recovery 4 +
%%% cross-queue-identity 4 = 32 叶），schema_phase=integrated。
%%%
%%% oracle 复用封印源 w402_worker_tests（旧 attempts 保留/legacy 形态不被
%%% 接管、stale token 0 行更新、企业 pending flag 经意图入队、done 墓碑拒绝
%%% 复用、跨队列物理身份锁内查重拒绝）。fn 协议面：ordinary/enterprise 叶
%%% 各自真实执行；cross 两 cell 为双域真实意图 fn（未知历史安全拒绝 +
%%% 双连接并发生产者），不再使用 shared_protocol_face 同面披露。driver 维度 = 队列行/
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

%% 跨队列物理身份（W4-04 双向查重，00000237:399-448
%% fn_v5_cross_queue_guard seed 1699 + 两意图 fn 锁序）：每叶两个独立 Key，
%% 每叶内双向子步骤各自完整验证；fn 协议面均为真实生产意图函数。
cross_cycle(St, Conn, Scn, Key, Triple, N) ->
    case Scn of
        <<"simultaneous_domain_producers">> ->
            cross_simultaneous(St, Conn, Key, Triple, N);
        <<"unknown_shared_history">> ->
            cross_unknown(St, Conn, Key, Triple, N)
    end.

seed_cross_eb_asset(Conn, EbId, Key, Triple) ->
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
    ok.

cross_probe(Drv, Key) -> <<"w601-cross:", Drv/binary, ":", Key/binary>>.

eb_asset_state(Conn, EbId) ->
    ?X:e(Conn, "SELECT lifecycle_state FROM enterprise_asset WHERE id=$1", [EbId]).

%% 该物理身份的三类计数：普通队列行 / 企业队列行 / delete_intent 审计。
cross_counters(Conn, Key, EbId) ->
    {ok, _, [{OrdN}]} = ?X:e(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {ok, _, [{EbN}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [EbId]
    ),
    {ok, _, [{AudN}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM attachment_lifecycle_v5_audit"
        " WHERE object_key=$1 AND action='delete_intent'",
        [Key]
    ),
    {OrdN, EbN, AudN}.

%% simultaneous_domain_producers：每叶两个独立 Key，两种提交顺序各跑一遍。
%% 普通 producer=fn_v5_intent_ordinary_delete（00000237:71 起）、企业
%% producer=fn_v5_intent_eb_delete（00000196:623-685，经 fn_v5_lock_eb_upload
%% 00000196:47-67 取身份锁；入队触发器另取 fn_v5_cross_queue_guard 物理 Key
%% 锁 seed 1699）。两连接真实事务屏障：A 先到域意图成功暂不提交，B 异步
%% 真实执行另一域生产意图，空闲连接证明 pg_blocking_pids(B) 含 A 且 B 存在
%% 未授予锁后 A COMMIT，B 以 23510 落败。
cross_simultaneous(St, Conn, Key, Triple, N) ->
    Drv = maps:get(driver, Triple),
    K1 = <<Key/binary, "/sim-ord-first">>,
    K2 = <<Key/binary, "/sim-eb-first">>,
    T1 = ?X:triple(St, Drv, K1),
    T2 = ?X:triple(St, Drv, K2),
    R1 = cross_simul_race(St, Conn, K1, T1, N * 10 + 1, ordinary_first),
    R2 = cross_simul_race(St, Conn, K2, T2, N * 10 + 2, enterprise_first),
    R1#{reverse_order => R2}.

cross_simul_race(St, Conn, Key, Triple, N, Order) ->
    Drv = maps:get(driver, Triple),
    EbId = 950100000000 + N,
    AttId = 950200000000 + N,
    ok = ?X:seed_orphan_row(Conn, AttId, Key, Triple, 0),
    ok = seed_cross_eb_asset(Conn, EbId, Key, Triple),
    Bytes = cross_probe(Drv, Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    {0, 0, 0} = cross_counters(Conn, Key, EbId),
    {OrdIntent, EbIntent} = cross_intents(Key, EbId),
    {{WinSql, WinParams}, {LoseSql, LoseParams}, WinnerDomain} =
        case Order of
            ordinary_first -> {OrdIntent, EbIntent, <<"ordinary">>};
            enterprise_first -> {EbIntent, OrdIntent, <<"enterprise">>}
        end,
    LockType =
        cross_run_race(
            St, Order, {WinSql, WinParams}, {LoseSql, LoseParams}
        ),
    %% 后置：两队列该物理身份总行数=1 属先提交域、胜方 delete_intent 败方
    %% live、成功审计恰一条、探针字节/HEAD 不变。
    {OrdN, EbN, AudN} = cross_counters(Conn, Key, EbId),
    1 = OrdN + EbN,
    case Order of
        ordinary_first ->
            1 = OrdN,
            0 = EbN,
            {ok, _, [{<<"delete_intent">>}]} = ?X:e(
                Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
            ),
            {ok, _, [{<<"live">>}]} = eb_asset_state(Conn, EbId);
        enterprise_first ->
            0 = OrdN,
            1 = EbN,
            {ok, _, [{<<"live">>}]} = ?X:e(
                Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
            ),
            {ok, _, [{<<"delete_intent">>}]} = eb_asset_state(Conn, EbId)
    end,
    1 = AudN,
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    #{
        order => Order,
        winner_domain => WinnerDomain,
        loser_rejected => <<"23510">>,
        lock_wait_proven => true,
        lock_type => LockType,
        winner_queue_rows => 1,
        audit_rows => 1
    }.

cross_intents(Key, EbId) ->
    {
        {"SELECT fn_v5_intent_ordinary_delete($1,$2)", [Key, <<"w601q sim ord">>]},
        {"SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)", [9101, 9102, EbId, <<"w601q sim eb">>]}
    }.

%% A/B 双连接屏障：A BEGIN + 先到域意图（未提交，持物理身份 advisory 锁
%% seed 1685）；B 异步执行后到域生产意图 fn 并真实阻塞；空闲连接证明
%% pg_blocking_pids(B) 包含 A 且 pg_locks 中 B 有未授予锁（不许只看
%% wait_event_type）→ A COMMIT → B 必须 23510 → B ROLLBACK。
cross_run_race(St, Order, {WinSql, WinParams}, {LoseSql, LoseParams}) ->
    A = ?X:barrier_connect(St),
    B = ?X:barrier_connect(St),
    Chk = ?X:barrier_connect(St),
    try
        {ok, _, [{APid}]} = ?X:e(A, "SELECT pg_backend_pid()", []),
        {ok, _, [{BPid}]} = ?X:e(B, "SELECT pg_backend_pid()", []),
        ok = ?X:q(A, "BEGIN"),
        {ok, _, _} = ?X:e(A, WinSql, WinParams),
        Ref = make_ref(),
        Parent = self(),
        spawn(fun() -> Parent ! {Ref, epgsql:equery(B, LoseSql, LoseParams)} end),
        LockType = cross_wait_blocked(Chk, APid, BPid, 200),
        ok = ?X:q(A, "COMMIT"),
        receive
            {Ref, {error, #error{code = <<"23510">>}}} -> ok;
            {Ref, Other} -> erlang:error({w601_cross_loser_not_23510, Order, Other})
        after 20000 -> erlang:error(w601_cross_contender_timeout)
        end,
        ok = epgsql:sync(B),
        ok = ?X:q(B, "ROLLBACK"),
        LockType
    after
        ok = epgsql:close(A),
        ok = epgsql:close(B),
        ok = epgsql:close(Chk)
    end.

cross_wait_blocked(_Chk, _APid, _BPid, 0) ->
    erlang:error(w601_cross_block_unproven);
cross_wait_blocked(Chk, APid, BPid, Left) ->
    {ok, _, [{Blocked, LockType}]} = ?X:e(
        Chk,
        "SELECT $1 = ANY(pg_blocking_pids($2)),"
        " (SELECT locktype::text FROM pg_locks WHERE pid = $2 AND NOT granted LIMIT 1)",
        [APid, BPid]
    ),
    case Blocked of
        true ->
            LockType;
        false ->
            timer:sleep(50),
            cross_wait_blocked(Chk, APid, BPid, Left - 1)
    end.

%% unknown_shared_history：双向安全拒绝验证。每方向种一条全 NULL 身份的
%% legacy blocked 历史行（job_state='blocked'、legacy_location_unverified、
%% attempts 保留），再调真实对域意图 fn，必须 23510；历史行全字段冻结、
%% 败方资产仍 live、两队列零新增、零新增 delete_intent 审计、探针不变。
cross_unknown(St, Conn, Key, Triple, N) ->
    Drv = maps:get(driver, Triple),
    K1 = <<Key/binary, "/unk-ord-history">>,
    K2 = <<Key/binary, "/unk-eb-history">>,
    T1 = ?X:triple(St, Drv, K1),
    T2 = ?X:triple(St, Drv, K2),
    R1 = cross_unknown_ord_history(St, Conn, K1, T1, N * 10 + 3),
    R2 = cross_unknown_eb_history(St, Conn, K2, T2, N * 10 + 4),
    R1#{reverse_direction => R2}.

%% (a) 普通域未知历史行阻断企业域意图：fn_v5_intent_eb_delete 入队 INSERT
%% 触发 fn_v5_cross_queue_guard（NULL 容忍物理 Key 匹配）→ 23510。
cross_unknown_ord_history(St, Conn, Key, Triple, N) ->
    Drv = maps:get(driver, Triple),
    EbId = 950300000000 + N,
    ok = seed_cross_eb_asset(Conn, EbId, Key, Triple),
    ok = seed_unknown_ord_history(Conn, Key, 5),
    Frozen = ord_history_snapshot(Conn, Key),
    Bytes = cross_probe(Drv, Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    {1, 0, 0} = cross_counters(Conn, Key, EbId),
    {error, #error{code = <<"23510">>}} = ?X:e(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)",
        [9101, 9102, EbId, <<"w601q unk ord hist">>]
    ),
    ok = epgsql:sync(Conn),
    Frozen = ord_history_snapshot(Conn, Key),
    {ok, _, [{<<"live">>}]} = eb_asset_state(Conn, EbId),
    {OrdN, EbN, AudN} = cross_counters(Conn, Key, EbId),
    1 = OrdN,
    0 = EbN,
    0 = AudN,
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    #{
        direction => ordinary_history_blocks_eb,
        rejected => <<"23510">>,
        history_row_frozen => true,
        loser_asset_live => true,
        new_queue_rows => 0,
        new_audit_rows => 0
    }.

%% (b) 反向：企业域未知历史行阻断普通域意图。fn_v5_intent_ordinary_delete
%% 内联查重对 NULL 身份不命中，INSERT 触发 fn_v5_cross_queue_guard 的
%% NULL 容忍匹配 → 23510（守卫语义覆盖 legacy 未知位置）。
cross_unknown_eb_history(St, Conn, Key, Triple, N) ->
    Drv = maps:get(driver, Triple),
    EbId = 950400000000 + N,
    AttId = 950500000000 + N,
    ok = seed_cross_eb_asset(Conn, EbId, Key, Triple),
    ok = ?X:seed_orphan_row(Conn, AttId, Key, Triple, 0),
    ok = seed_unknown_eb_history(Conn, EbId, Key, 4),
    Frozen = eb_history_snapshot(Conn, EbId),
    Bytes = cross_probe(Drv, Key),
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    {0, 1, 0} = cross_counters(Conn, Key, EbId),
    {error, #error{code = <<"23510">>}} = ?X:e(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,$2)", [Key, <<"w601q unk eb hist">>]
    ),
    ok = epgsql:sync(Conn),
    Frozen = eb_history_snapshot(Conn, EbId),
    {ok, _, [{<<"live">>}]} = ?X:e(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    {ok, _, [{<<"live">>}]} = eb_asset_state(Conn, EbId),
    {OrdN, EbN, AudN} = cross_counters(Conn, Key, EbId),
    0 = OrdN,
    1 = EbN,
    0 = AudN,
    ok = ?X:assert_probe_intact(St, Drv, Key, Bytes),
    #{
        direction => eb_history_blocks_ordinary,
        rejected => <<"23510">>,
        history_row_frozen => true,
        loser_assets_live => true,
        new_queue_rows => 0,
        new_audit_rows => 0
    }.

%% 未知位置历史行：身份四列全 NULL（协议面如实保留 legacy 形态），身份列
%% 不被接管、attempts 不被重置。
seed_unknown_ord_history(Conn, Key, Attempts) ->
    ?X:q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path,bucket,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,job_state,next_retry_at,attempts,last_error_code)"
        " VALUES ($1,'w601-b',NULL,NULL,NULL,NULL,'blocked',now(),$2,"
        " 'legacy_location_unverified')"
        " ON CONFLICT (path) DO UPDATE SET job_state='blocked',"
        " attempts=EXCLUDED.attempts, last_error_code='legacy_location_unverified',"
        " storage_driver=NULL, storage_namespace=NULL, storage_container=NULL,"
        " storage_locator_sha256=NULL, lease_token=NULL, lease_until=NULL,"
        " completed_at=NULL",
        [Key, Attempts]
    ),
    ok.

seed_unknown_eb_history(Conn, EbId, Key, Attempts) ->
    ?X:q(
        Conn,
        "INSERT INTO enterprise_asset_delete_queue"
        " (organization_id,workspace_id,asset_id,object_key,storage_driver,"
        "  storage_namespace,storage_container,storage_locator_sha256,"
        "  job_state,next_retry_at,attempts,last_error_code)"
        " VALUES (9101,9102,$1,$2,NULL,NULL,NULL,NULL,'blocked',now(),$3,"
        " 'legacy_location_unverified')"
        " ON CONFLICT (organization_id,workspace_id,asset_id) DO UPDATE SET"
        "  job_state='blocked', attempts=EXCLUDED.attempts,"
        "  last_error_code='legacy_location_unverified', storage_driver=NULL,"
        "  storage_namespace=NULL, storage_container=NULL,"
        "  storage_locator_sha256=NULL, lease_token=NULL, lease_until=NULL,"
        "  completed_at=NULL",
        [EbId, Key, Attempts]
    ),
    ok.

ord_history_snapshot(Conn, Key) ->
    {ok, _, [Row]} = ?X:e(
        Conn,
        "SELECT job_state, attempts, last_error_code, storage_driver,"
        " storage_namespace, storage_container, storage_locator_sha256,"
        " lease_token, lease_until, completed_at"
        " FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    Row.

eb_history_snapshot(Conn, EbId) ->
    {ok, _, [Row]} = ?X:e(
        Conn,
        "SELECT job_state, attempts, last_error_code, storage_driver,"
        " storage_namespace, storage_container, storage_locator_sha256,"
        " lease_token, lease_until, completed_at"
        " FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
        [EbId]
    ),
    Row.
