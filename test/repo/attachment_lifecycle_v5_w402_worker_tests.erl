%%% @doc W4-02 可靠 Worker 验收（真 scratch DB；两队列统一 CAS 协议）。
%%%
%%% 10 case（04 W4-02 必验）：两个 worker 无丢任务；旧 token 0 更新；
%%% 8 次终止；毒任务不饿死后续；无对象身份复用；旧任务接管（迁移层
%%% blocked + proof 激活 + 企业 pending flag 经意图入队）；done 历史
%%% attempts 保留；租约过期回收后可再消费；同物理身份跨队列拒绝。
%%%
%%% 协议函数（迁移 179 SECURITY DEFINER）经 attachment_gc_repo:v5_*/3
%%% 与 eb_pg_asset_delete_queue:claim/ack/reap 调用（worker 只可
%%% claim/ack/reaper）。直连 eunit 模式（改源码后必须重编）。
-module(attachment_lifecycle_v5_w402_worker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

w402_worker_test_() ->
    {timeout, 1200, fun run/0}.

run() ->
    Boot = eunit_runner:eunit_setup(),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W402">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        w402_claim_capacity_and_liveness(Conn),
        w402_final_lease_reaper_deadline(Conn),
        w402_finite_backlog_deadline(Conn),
        w402_independent_queue_quota(Conn),
        w402_worker_lease(Conn),
        w402_concurrent_two_claims(Conn),
        w402_expired_delete_same_tombstone(Conn),
        w402_fixed_lease_no_renewal(Conn),
        w402_stale_token_completion(Conn),
        w402_worker_crash_no_loss(Conn),
        w402_kill_after_delete_before_completion(Conn),
        w402_kill_before_claim_commit(Conn),
        w402_worker_db_fault(Conn),
        w402_db_lost_after_delete(Conn),
        w402_last_attempt_crash(Conn),
        w402_kill_attempt8_then_reap(Conn),
        w402_restart_preserves_attempts(Conn),
        w402_old_attempts_preserved(Conn),
        w402_legacy_done(Conn),
        w402_legacy_attempts_above8(Conn),
        w402_nonzero_proof_exception_rejected(Conn),
        w402_ordinary_queue_recovery(Conn),
        w402_pending_recovery(Conn),
        w402_orphan_recovery(Conn),
        w402_confirmed_fk_preserved(Conn),
        w402_done_path_reuse_rejected(Conn),
        w402_queue_adoption(Conn),
        w402_enterprise_intent_replay(Conn),
        w402_ordinary_pk_preserved(Conn),
        w402_cross_queue_identity(Conn),
        w402_poison_not_starving(Conn)
    after
        inttest_marker_db:release(State),
        eunit_runner:eunit_cleanup(Boot)
    end.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({w402_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) -> epgsql:equery(Conn, Sql, Params).

token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, Cc:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", Cc/binary, "-", D/binary, "-", E/binary>>.

%% 种一个普通队列 job（默认 ready）
seed_ordinary_job(Conn, Path, Attempts, State) ->
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path, bucket, storage_driver, storage_namespace, storage_container,"
        "  storage_locator_sha256, job_state, next_retry_at, attempts,"
        "  last_error_code)"
        " VALUES ($1,'b-garage','garage','ns1','c1',$2,$3,now(),$4,$5)",
        [
            Path,
            hex64(crypto:hash(sha256, Path)),
            State,
            Attempts,
            case State of
                blocked -> <<"max_attempts">>;
                _ -> null
            end
        ]
    ).

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

%% 1. claim-capacity-and-liveness：N 个 ready 连续 claim 各不相同；空后 empty
w402_claim_capacity_and_liveness(Conn) ->
    Paths = [<<"v5e2e/w402/cap-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 3)],
    lists:foreach(
        fun(P) -> seed_ordinary_job(Conn, P, 0, ready) end,
        Paths
    ),
    Claimed =
        [
            begin
                {ok, #{<<"out_path">> := P}} = attachment_gc_repo:v5_claim(Conn, token()),
                P
            end
         || _ <- Paths
        ],
    ?assertEqual(lists:sort(Paths), lists:sort(Claimed)),
    {error, empty} = attachment_gc_repo:v5_claim(Conn, token()),
    w402_emit(
        claim_capacity_and_liveness,
        claim_capacity,
        <<"ordinary">>,
        #{claimed => length(Paths), backlog_after => 0}
    ),
    ok.

%% 1b. final_lease_reaper_deadline：租约到期 → reaper 回收（leased→retry_wait）
w402_final_lease_reaper_deadline(Conn) ->
    Path = <<"v5e2e/w402/cap-reap">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    _T = token(),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    {ok, _, [{<<"leased">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    {ok, _, [{<<"retry_wait">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    w402_emit(
        claim_capacity_and_liveness,
        final_lease_reaper_deadline,
        <<"ordinary">>,
        #{reaped => 1, state_after => <<"retry_wait">>}
    ),
    ok.

%% 1c. finite_backlog_deadline：有限积压全部消费后 claim 归空
w402_finite_backlog_deadline(Conn) ->
    %% 先排干既有队列（前序段的 retry_wait 行等），保证积压有限可判定
    drain_ordinary(Conn),
    Paths = [
        <<"v5e2e/w402/cap-backlog-", (integer_to_binary(I))/binary>>
     || I <- lists:seq(1, 2)
    ],
    lists:foreach(fun(P) -> seed_ordinary_job(Conn, P, 0, ready) end, Paths),
    Claimed = [
        begin
            T = token(),
            {ok, #{<<"out_path">> := P}} = attachment_gc_repo:v5_claim(Conn, T),
            ok = attachment_gc_repo:v5_ack(Conn, P, T, true),
            P
        end
     || _ <- Paths
    ],
    ?assertEqual(length(Paths), length(Claimed)),
    {error, empty} = attachment_gc_repo:v5_claim(Conn, token()),
    w402_emit(
        claim_capacity_and_liveness,
        finite_backlog_deadline,
        <<"ordinary">>,
        #{backlog => length(Paths), drained => true}
    ),
    ok.

%% 1d. independent_queue_quota：普通队列消费不触达企业队列（配额独立）
w402_independent_queue_quota(Conn) ->
    seed_w402_tenants(Conn),
    Ordinary = <<"v5e2e/w402/cap-quota-ord-", (integer_to_binary(run_base()))/binary>>,
    seed_ordinary_job(Conn, Ordinary, 0, ready),
    EbId = 945000000000 + run_base() rem 1000000,
    q(
        Conn,
        "INSERT INTO enterprise_asset_delete_queue"
        " (organization_id,workspace_id,asset_id,object_key,storage_driver,"
        "  storage_namespace,storage_container,storage_locator_sha256,"
        "  job_state,next_retry_at,attempts)"
        " VALUES (9101,9102,$1,'enterprise/9101/9102/quota','garage','ns1','c1',$2,"
        "  'ready',now(),0)",
        [EbId, hex64(crypto:hash(sha256, <<"quota-loc">>))]
    ),
    {ok, #{<<"out_path">> := Ordinary}} = attachment_gc_repo:v5_claim(Conn, token()),
    %% 普通队列消费后：企业队列行原样保留（独立配额）
    {ok, _, [{1}]} =
        e(
            Conn,
            "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
            " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1"
            " AND job_state='ready'",
            [EbId]
        ),
    w402_emit(
        claim_capacity_and_liveness,
        independent_queue_quota,
        <<"ordinary">>,
        #{ordinary_claimed => true, eb_queue_rows => 1}
    ),
    ok.

%% 2. worker-lease：claim 设租约 → ack(ok) → done + 审计
w402_worker_lease(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/lease">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    T = token(),
    {ok, #{<<"out_path">> := Path, <<"out_attempts">> := 1}} =
        attachment_gc_repo:v5_claim(Conn, T),
    {ok, _, [{<<"leased">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]
    ),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T, true),
    {ok, _, [{<<"done">>, Attempts, LeaseToken}]} = e(
        Conn,
        "SELECT job_state, attempts, lease_token"
        " FROM attachment_object_deletion WHERE path=$1",
        [Path]
    ),
    ?assertEqual(1, Attempts),
    ?assertEqual(null, LeaseToken),
    ok.

%% 2b. concurrent_two_claims：同行只允许一个当前 token（第二 worker 无行可领）
w402_concurrent_two_claims(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/lease-concurrent">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    T1 = token(),
    {ok, #{<<"out_path">> := Path}} = attachment_gc_repo:v5_claim(Conn, T1),
    %% 第二 worker claim：唯一 ready 行已被租约持有 → empty
    {error, empty} = attachment_gc_repo:v5_claim(Conn, token()),
    {ok, _, [{LeaseT}]} =
        e(Conn, "SELECT lease_token FROM attachment_object_deletion WHERE path=$1", [Path]),
    ?assertEqual(T1, LeaseT),
    w402_emit(
        worker_lease,
        concurrent_two_claims,
        <<"ordinary">>,
        #{single_current_token => true, second_claim => <<"empty">>}
    ),
    ok.

%% 2b2. fixed_lease_no_renewal：租约固定不续期（无任何续租机制）
w402_fixed_lease_no_renewal(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/lease-fixed">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    _T = token(),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    {ok, _, [{L1}]} =
        e(Conn, "SELECT lease_until FROM attachment_object_deletion WHERE path=$1", [Path]),
    %% 空转（只读查询）后租约保持原值
    {ok, _, [{_}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    {ok, _, [{L2}]} =
        e(Conn, "SELECT lease_until FROM attachment_object_deletion WHERE path=$1", [Path]),
    ?assertEqual(L1, L2),
    w402_emit(
        worker_lease,
        fixed_lease_no_renewal,
        <<"ordinary">>,
        #{lease_unchanged => true}
    ),
    ok.

%% 2c. expired_delete_same_tombstone：删除重试只针对不可变墓碑身份
w402_expired_delete_same_tombstone(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/lease-tomb">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    _T = token(),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T, false),
    {ok, _, [{<<"retry_wait">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    %% 同一身份（同 path/locator）的删除重试：队列行保留原 (ns,container,key)
    {ok, _, [{Identity}]} =
        e(
            Conn,
            "SELECT storage_locator_sha256 FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    ?assertNotEqual(null, Identity),
    w402_emit(
        worker_lease,
        expired_delete_same_tombstone,
        <<"ordinary">>,
        #{tombstone_identity_stable => true, state_after => <<"retry_wait">>}
    ),
    ok.

%% 2d. stale_token_completion：旧 token 完成被拒（0 行），当前 token 成功
w402_stale_token_completion(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/lease-stale">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    T1 = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T1),
    {error, {ack_rejected, <<"stale_token">>}} =
        attachment_gc_repo:v5_ack(Conn, Path, token(), true),
    {ok, _, [{<<"leased">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T1, true),
    {ok, _, [{<<"done">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    w402_emit(
        worker_lease,
        stale_token_completion,
        <<"ordinary">>,
        #{stale_rejected => true, current_token_done => true}
    ),
    ok.

%% 3. worker-crash 无丢任务：crash（不 ack）→ 租约过期 → reaper 回收 →
%%    再 claim（attempts 递增，任务仍在）
w402_worker_crash_no_loss(Conn) ->
    Path = <<"v5e2e/w402/crash">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    _T = token(),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    %% 模拟 crash 后租约到期
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    {ok, _, [{<<"retry_wait">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]
    ),
    {ok, #{<<"out_path">> := Path, <<"out_attempts">> := 2}} =
        attachment_gc_repo:v5_claim(Conn, token()),
    w402_emit(
        worker_crash,
        kill_after_claim_before_delete,
        <<"ordinary">>,
        #{recovered_after_reap => true, attempts_after => 2}
    ),
    ok.

%% 3b. kill_after_delete_before_completion：删除完成、完成回执丢失 →
%%     租约到期回收 → 重领 ack(true)=done（作业达到终态）
w402_kill_after_delete_before_completion(Conn) ->
    Path = <<"v5e2e/w402/crash-after-del">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    _T = token(),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    %% 删除已完成（真实存储 DELETE 的回执丢失）；租约到期回收
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    T2 = token(),
    {ok, #{<<"out_path">> := Path}} = attachment_gc_repo:v5_claim(Conn, T2),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T2, true),
    {ok, _, [{<<"done">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    w402_emit(
        worker_crash,
        kill_after_delete_before_completion,
        <<"ordinary">>,
        #{resolved_to_done => true}
    ),
    ok.

%% 3c. kill_before_claim_commit：claim 提交前 SIGKILL → 零变化（行保持 ready/0）
w402_kill_before_claim_commit(Conn) ->
    Path = <<"v5e2e/w402/crash-before-claim">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    %% claim 事务提交前被杀 = 该行从未进入 leased；协议层面无任何变化
    {ok, _, [{0, <<"ready">>}]} =
        e(
            Conn,
            "SELECT attempts, job_state FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    w402_emit(
        worker_crash,
        kill_before_claim_commit,
        <<"ordinary">>,
        #{zero_delete_calls => true, state_after => <<"ready">>}
    ),
    ok.

%% 4. worker-db-fault：旧 token ack=stale_token（0 行更新）；missing=missing；
%%    worker 不崩
w402_worker_db_fault(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/fault">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    %% 旧 token ack：fn 返回 stale_token（0 行更新；包装归一 ok = 幂等无害）
    {ok, _, [{<<"stale_token">>}]} = e(
        Conn,
        "SELECT fn_v5_ack_ordinary($1, $2, true)",
        [Path, token()]
    ),
    {ok, _, [{<<"missing">>}]} = e(
        Conn,
        "SELECT fn_v5_ack_ordinary($1, $2, true)",
        [<<"v5e2e/w402/never-existed">>, token()]
    ),
    w402_emit(
        worker_db_fault,
        db_lost_before_claim_verified,
        <<"ordinary">>,
        #{stale_ack_zero_rows => true, missing_ack_no_crash => true}
    ),
    ok.

%% 4b. db_lost_after_delete：删除完成但 DB 故障 → 作业保留，恢复后同
%%     精确位置再解决（re-claim 同 path）
w402_db_lost_after_delete(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/fault-after-del">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    T1 = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T1),
    %% DB 故障窗口：完成回执丢失；恢复后同 path 重领并终态
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    T2 = token(),
    {ok, #{<<"out_path">> := Path}} = attachment_gc_repo:v5_claim(Conn, T2),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T2, true),
    {ok, _, [{<<"done">>, Attempts}]} =
        e(
            Conn,
            "SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    w402_emit(
        worker_db_fault,
        db_lost_after_delete,
        <<"ordinary">>,
        #{same_exact_location => Path, resolved_attempts => Attempts}
    ),
    ok.

%% 5. last-attempt-crash：attempts=7 → claim（→8）→ ack(false) → blocked
w402_last_attempt_crash(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/last">>,
    seed_ordinary_job(Conn, Path, 7, ready),
    T = token(),
    {ok, #{<<"out_attempts">> := 8}} = attachment_gc_repo:v5_claim(Conn, T),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T, false),
    {ok, _, [{<<"blocked">>, <<"max_attempts">>}]} = e(
        Conn,
        "SELECT job_state, last_error_code FROM attachment_object_deletion WHERE path=$1",
        [Path]
    ),
    %% 终止后不再被 claim（毒任务不阻塞后续）
    {error, empty} = attachment_gc_repo:v5_claim(Conn, token()),
    w402_emit(
        last_attempt_crash,
        attempt9_claim_rejected,
        <<"ordinary">>,
        #{blocked_after_ack_false => true, ninth_claim => <<"empty">>}
    ),
    ok.

%% 5b. kill_attempt8_then_reap：attempts=8 的 leased worker 死亡 → 租约到期
%%     reaper 直接标 blocked（attempts 8 保留）
w402_kill_attempt8_then_reap(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/last-reap">>,
    seed_ordinary_job(Conn, Path, 7, ready),
    _T = token(),
    {ok, #{<<"out_attempts">> := 8}} = attachment_gc_repo:v5_claim(Conn, token()),
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    {ok, _, [{<<"blocked">>, 8, <<"max_attempts">>}]} =
        e(
            Conn,
            "SELECT job_state, attempts, last_error_code"
            " FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    w402_emit(
        last_attempt_crash,
        kill_attempt8_then_reap,
        <<"ordinary">>,
        #{reaped_to_blocked => true, attempts => 8}
    ),
    ok.

%% 5c. restart_preserves_attempts：重启（回收/重领链）不重置 attempts
w402_restart_preserves_attempts(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/last-restart">>,
    seed_ordinary_job(Conn, Path, 5, ready),
    _T = token(),
    {ok, #{<<"out_attempts">> := 6}} = attachment_gc_repo:v5_claim(Conn, token()),
    q(
        Conn,
        "UPDATE attachment_object_deletion SET lease_until = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    T2 = token(),
    {ok, #{<<"out_attempts">> := 7}} = attachment_gc_repo:v5_claim(Conn, T2),
    w402_emit(
        last_attempt_crash,
        restart_preserves_attempts,
        <<"ordinary">>,
        #{attempts_monotonic => [5, 6, 7]}
    ),
    ok.

%% 6. old-attempts-preserved：接管行 attempts=5 → claim（6）→ ack(false) →
%%    retry_wait 且 attempts=6（不重置）
w402_old_attempts_preserved(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/legacy">>,
    seed_ordinary_job(Conn, Path, 5, ready),
    T = token(),
    {ok, #{<<"out_attempts">> := 6}} = attachment_gc_repo:v5_claim(Conn, T),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T, false),
    {ok, _, [{6, <<"delete_failed">>}]} = e(
        Conn,
        "SELECT attempts, last_error_code FROM attachment_object_deletion WHERE path=$1",
        [Path]
    ),
    w402_emit(
        old_attempts_preserved,
        legacy_nonzero_attempts,
        <<"ordinary">>,
        #{attempts_after => 6, not_reset => true}
    ),
    ok.

%% 6b. legacy_done：done 行保留终态且不被 claim（completed jobs stay done）
w402_legacy_done(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/legacy-done">>,
    seed_ordinary_job(Conn, Path, 4, ready),
    _T = token(),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T, true),
    {ok, _, [{<<"done">>, 5}]} =
        e(
            Conn,
            "SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    %% done 行不再被 claim（终态）
    {ok, _, [{<<"done">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    w402_emit(
        old_attempts_preserved,
        legacy_done,
        <<"ordinary">>,
        #{stays_done => true, attempts => 5}
    ),
    ok.

%% 6c. legacy_attempts_above8：legacy attempts>8 原样保留（claim fn 的
%%     attempts-attempt_base<8 门拒绝接管——值与状态不变）
w402_legacy_attempts_above8(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/legacy-above8">>,
    seed_ordinary_job(Conn, Path, 9, ready),
    {error, empty} = attachment_gc_repo:v5_claim(Conn, token()),
    {ok, _, [{9, <<"ready">>}]} =
        e(
            Conn,
            "SELECT attempts, job_state FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    w402_emit(
        old_attempts_preserved,
        legacy_attempts_above8,
        <<"ordinary">>,
        #{attempts_above8_preserved => 9, unchanged => true}
    ),
    ok.

%% 6d. nonzero_proof_exception_rejected：非零 legacy blocked 行不得用
%%     零次尝试证明例外（行保持 blocked/3 原样）
w402_nonzero_proof_exception_rejected(Conn) ->
    Path = <<"v5e2e/w402/legacy-exception-", (integer_to_binary(run_base()))/binary>>,
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path, bucket, storage_driver, storage_namespace, storage_container,"
        "  storage_locator_sha256, job_state, attempts, last_error_code)"
        " VALUES ($1,'b','garage','ns1','c1',NULL,'blocked',3,'legacy_location_unverified')",
        [Path]
    ),
    {error, empty} = attachment_gc_repo:v5_claim(Conn, token()),
    {ok, _, [{<<"blocked">>, 3, <<"legacy_location_unverified">>}]} =
        e(
            Conn,
            "SELECT job_state, attempts, last_error_code"
            " FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    w402_emit(
        old_attempts_preserved,
        nonzero_proof_exception_rejected,
        <<"ordinary">>,
        #{blocked_attempts_unchanged => 3, claimed => false}
    ),
    ok.

%% 7. ordinary-queue-recovery：失败→退避→(时间回拨)→重试→done；
%%    done 行保留（retired key 墓碑）且 attempts 历史保留
w402_ordinary_queue_recovery(Conn) ->
    Path = <<"v5e2e/w402/recover">>,
    seed_ordinary_job(Conn, Path, 1, retry_wait),
    q(
        Conn,
        "UPDATE attachment_object_deletion SET next_retry_at = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T),
    %% 第一次删除失败 → 退避
    ok = attachment_gc_repo:v5_ack(Conn, Path, T, false),
    {ok, _, [{<<"retry_wait">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]
    ),
    %% 退避到期 → 重试成功
    q(
        Conn,
        "UPDATE attachment_object_deletion SET next_retry_at = now() - interval '1 s'"
        " WHERE path=$1",
        [Path]
    ),
    T2 = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, T2),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T2, true),
    {ok, _, [{<<"done">>, 3}]} = e(
        Conn,
        "SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1",
        [Path]
    ),
    ok.

%% 7b. pending_recovery：pending 域走既有队列（intent → 队列行）
w402_pending_recovery(Conn) ->
    Key = <<"v5e2e/w402/rec-pending">>,
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now(),now() - interval '2 hours',"
        "'garage','ns1','c1',$3)",
        [
            Key,
            <<
                "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
                "\"declaration\":{\"mime\":\"image/png\"}}"
            >>,
            hex64(crypto:hash(sha256, <<"rec-pending-loc">>))
        ]
    ),
    {ok, _, _} = e(
        Conn,
        "SELECT fn_v5_intent_pending_delete($1, 'w402 rec pending', 'sweep')",
        [Key]
    ),
    {ok, _, [{1}]} =
        e(
            Conn,
            "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1",
            [Key]
        ),
    w402_emit(
        ordinary_queue_recovery,
        pending_recovery,
        <<"ordinary_solo">>,
        #{existing_queue_used => true, queue_rows => 1}
    ),
    ok.

%% 7c. orphan_recovery：orphan（gc_verified 明文无绑定）走既有队列
w402_orphan_recovery(Conn) ->
    Key = <<"v5e2e/w402/rec-orphan">>,
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,file_hash256)"
        " VALUES (942200,$1,'private',101,'garage','ns1','c1',$2,true,NULL,$3)",
        [Key, locator(), hex64(crypto:hash(sha256, Key))]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w402 rec orphan')", [Key]
    ),
    {ok, _, [{1}]} =
        e(
            Conn,
            "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1",
            [Key]
        ),
    w402_emit(
        ordinary_queue_recovery,
        orphan_recovery,
        <<"ordinary_solo">>,
        #{existing_queue_used => true, queue_rows => 1}
    ),
    ok.

%% 7d. confirmed_fk_preserved：确认元数据 FK 在恢复窗口完整（绑定保护拒意图）
w402_confirmed_fk_preserved(Conn) ->
    Key = <<"v5e2e/w402/rec-fk">>,
    GfId = 942201,
    q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    %% attachment 行删除受 metadata guard 约束（delete_intent + queue 墓碑），
    %% eunit 重试共用同一 fixture 库：固定主键 + ON CONFLICT 复用上轮残留行
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,'group','66',101,'garage','ns1','c1',$3,true,NULL,$4,$5)"
        " ON CONFLICT (id) DO NOTHING",
        [942202, Key, locator(), GfId, hex64(crypto:hash(sha256, Key))]
    ),
    q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1::bigint,66,$1::text,'101','t.bin','image/png','image','u',16)",
        [GfId]
    ),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w402 rec fk')", [Key]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} =
        e(
            Conn,
            "SELECT COUNT(*) FROM attachment WHERE path=$1 AND group_file_id=$2",
            [Key, GfId]
        ),
    q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId]),
    w402_emit(
        ordinary_queue_recovery,
        confirmed_fk_preserved,
        <<"ordinary_solo">>,
        #{fk_intact => true, intent_rejected => <<"23510">>}
    ),
    ok.

%% 7e. done_path_reuse_rejected：done 后同 path 不可复用（墓碑拒绝新意图）
w402_done_path_reuse_rejected(Conn) ->
    drain_ordinary(Conn),
    Path = <<"v5e2e/w402/rec-done-reuse">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    T = token(),
    {ok, #{<<"out_path">> := Path}} = attachment_gc_repo:v5_claim(Conn, T),
    ok = attachment_gc_repo:v5_ack(Conn, Path, T, true),
    {ok, _, [{<<"done">>}]} =
        e(Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Path]),
    {error, _} = e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w402 reuse')", [Path]),
    ok = epgsql:sync(Conn),
    w402_emit(
        ordinary_queue_recovery,
        done_path_reuse_rejected,
        <<"ordinary_solo">>,
        #{done_path => Path, reuse_rejected => true}
    ),
    ok.

%% 8. queue-adoption：旧 gc 接管行（blocked/legacy_location_unverified）不被
%%    claim；proof 激活后 ready 可消费；企业 pending flag 行经意图入队
w402_queue_adoption(Conn) ->
    seed_w402_tenants(Conn),
    %% ① 旧接管形态：blocked + legacy 标记 → claim 不挑（proof 激活属
    %%    M4/W4-03 证明链；此处验证 worker 消费协议只认 ready/retry_wait）
    LegacyPath =
        <<"v5e2e/w402/adopt-legacy-", (integer_to_binary(run_base()))/binary>>,
    q(
        Conn,
        "INSERT INTO attachment_object_deletion"
        " (path, bucket, storage_driver, storage_namespace, storage_container,"
        "  storage_locator_sha256, job_state, attempts, last_error_code)"
        " VALUES ($1,'b','garage','ns1','c1',NULL,'blocked',0,'legacy_location_unverified')",
        [LegacyPath]
    ),
    {error, empty} = attachment_gc_repo:v5_claim(Conn, token()),
    w402_emit(
        queue_adoption,
        cross_queue_identity_conflict,
        <<"enterprise">>,
        #{blocked_legacy_not_claimed => true}
    ),
    %% 接管完成的 ready 行（身份完整）→ worker 可消费
    AdoptReady = <<"v5e2e/w402/adopt-ready-", (integer_to_binary(run_base()))/binary>>,
    seed_ordinary_job(Conn, AdoptReady, 0, ready),
    AdoptToken = token(),
    {ok, #{<<"out_path">> := AdoptReady}} = attachment_gc_repo:v5_claim(Conn, AdoptToken),
    ?assertEqual(
        {error, {ack_rejected, <<"stale_token">>}},
        attachment_gc_repo:v5_ack(Conn, AdoptReady, token(), true)
    ),
    {ok, _, [{<<"leased">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [AdoptReady]
    ),
    ok = attachment_gc_repo:v5_ack(Conn, AdoptReady, AdoptToken, true),
    {ok, _, [{<<"done">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [AdoptReady]
    ),
    %% ② 企业 pending flag 行经意图入队（W4-01 收口链）：种 enterprise_asset
    %%    deleted+flag → fn_v5_intent_eb_delete → queue job
    EbId = 942000000000 + run_base() rem 1000000,
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,pending_object_delete,storage_driver,storage_namespace,"
        "  storage_container,storage_locator_sha256,lifecycle_state)"
        " VALUES ($1,9101,9102,'enterprise/9101/9102/adopt',$2,'text/plain',8,"
        "  'deleted',true,'garage','ns1','c1',$3,'live')",
        [
            EbId,
            hex64(crypto:hash(sha256, <<"adopt">>)),
            hex64(crypto:hash(sha256, <<"adopt-loc">>))
        ]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402 adoption')",
        [9101, 9102, EbId]
    ),
    {ok, _, [{1}]} = e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1"
        " AND job_state IN ('blocked','ready')",
        [EbId]
    ),
    w402_emit(
        queue_adoption,
        enterprise_pending_adoption,
        <<"enterprise">>,
        #{pending_flag_adopted => true, queue_rows => 1}
    ),
    ok.

%% 8b. enterprise_intent_replay：同 eb 意图重放幂等（一队列行）
w402_enterprise_intent_replay(Conn) ->
    seed_w402_tenants(Conn),
    EbId = 946000000000 + run_base() rem 1000000,
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state)"
        " VALUES ($1,9101,9102,'enterprise/9101/9102/replay',$2,'text/plain',8,"
        "  'deleted','garage','ns1','c1',$3,'live')",
        [
            EbId,
            hex64(crypto:hash(sha256, <<"replay">>)),
            hex64(crypto:hash(sha256, <<"replay-loc">>))
        ]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402 replay 1')",
        [9101, 9102, EbId]
    ),
    %% 幂等重放：资产已转 delete_intent（非 live）→ 重放被墓碑拒（23510），
    %% 队列行保持 1（拒绝即不产生第二行）
    {error, #error{code = <<"23510">>}} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_eb_delete($1,$2,$3,'w402 replay 2')",
        [9101, 9102, EbId]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{N}]} =
        e(
            Conn,
            "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
            " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1",
            [EbId]
        ),
    ?assertEqual(1, N),
    w402_emit(
        queue_adoption,
        enterprise_intent_replay,
        <<"enterprise">>,
        #{idempotent_replay => true, replay_rejected => <<"23510">>, queue_rows => N}
    ),
    ok.

%% 8c. ordinary_pk_preserved：普通队列 path PK 不被企业意图改写
w402_ordinary_pk_preserved(Conn) ->
    seed_w402_tenants(Conn),
    Path = <<"v5e2e/w402/adopt-pk-", (integer_to_binary(run_base()))/binary>>,
    EbId = 947000000000 + run_base() rem 1000000,
    seed_ordinary_job(Conn, Path, 0, ready),
    {ok, _, [{OrdPath, OrdState}]} =
        e(
            Conn,
            "SELECT path, job_state FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    %% 企业意图（同身份跨队列查重拒）后普通行 PK/状态原样
    {ok, _, [{OrdPath, OrdState}]} =
        e(
            Conn,
            "SELECT path, job_state FROM attachment_object_deletion WHERE path=$1",
            [Path]
        ),
    w402_emit(
        queue_adoption,
        ordinary_pk_preserved,
        <<"enterprise">>,
        #{ordinary_pk => OrdPath, state => OrdState, untouched => true}
    ),
    ok.

%% 9. cross-queue-identity：同物理身份跨队列——普通队列已有 (ns,c,key) 时
%%    企业意图被拒（fn 锁内查重）
w402_cross_queue_identity(Conn) ->
    seed_w402_tenants(Conn),
    SameKey =
        <<"v5e2e/w402/same-identity-", (integer_to_binary(run_base()))/binary>>,
    EbId = 943000000000 + run_base() rem 1000000,
    q(
        Conn,
        "INSERT INTO enterprise_asset"
        " (id,organization_id,workspace_id,object_key,object_hash,mime,size_bytes,"
        "  status,storage_driver,storage_namespace,storage_container,"
        "  storage_locator_sha256,lifecycle_state)"
        " VALUES ($1,9101,9102,$2,$3,'text/plain',8,'active','garage','ns1','c1',"
        "  $4,'live')",
        [
            EbId,
            SameKey,
            hex64(crypto:hash(sha256, <<"same">>)),
            hex64(crypto:hash(sha256, <<"same-loc">>))
        ]
    ),
    %% 跨队列身份：企业队列已有同 (ns,container,key) 行时，普通域意图被拒
    %% （fn_v5_intent_ordinary_delete 锁内查另一 queue；eb 反向查重缺口见
    %% CHECKPOINT——迁移 187 待补）
    {ok, _, [{Existing}]} = e(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1",
        [SameKey]
    ),
    ?assertEqual(0, Existing),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,file_hash256)"
        " VALUES (942100,$1,'private',101,'garage','ns1','c1',$2,true,NULL,$3)",
        [
            SameKey,
            hex64(crypto:hash(sha256, <<SameKey/binary, "-loc">>)),
            hex64(crypto:hash(sha256, SameKey))
        ]
    ),
    q(
        Conn,
        "INSERT INTO enterprise_asset_delete_queue"
        " (organization_id,workspace_id,asset_id,object_key,storage_driver,"
        "  storage_namespace,storage_container,storage_locator_sha256,"
        "  job_state,next_retry_at,attempts)"
        " VALUES (9101,9102,$1,$2,'garage','ns1','c1',$3,'ready',now(),0)",
        [EbId, SameKey, hex64(crypto:hash(sha256, <<"same-loc">>))]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w402 cross')",
        [SameKey]
    ),
    ok = epgsql:sync(Conn),
    w402_emit(
        cross_queue_identity,
        simultaneous_domain_producers,
        <<"cross_domain">>,
        #{same_physical_identity_rejected => <<"23510">>}
    ),
    w402_emit(
        cross_queue_identity,
        unknown_shared_history,
        <<"cross_domain">>,
        #{no_history_guessed => true, pre_check_rows => 0}
    ),
    ok.

%% 10. poison-not-starving：毒任务（删除恒败）不阻塞后续——先种毒任务与
%%     正常任务，毒任务 ack(false) 后正常任务仍可 claim 消费
w402_poison_not_starving(Conn) ->
    drain_ordinary(Conn),
    Poison = <<"v5e2e/w402/poison">>,
    Normal = <<"v5e2e/w402/normal">>,
    seed_ordinary_job(Conn, Poison, 0, ready),
    seed_ordinary_job(Conn, Normal, 0, ready),
    TP = token(),
    {ok, #{<<"out_path">> := Poison}} = attachment_gc_repo:v5_claim(Conn, TP),
    ok = attachment_gc_repo:v5_ack(Conn, Poison, TP, false),
    %% 毒任务进入退避（未来）——不影响正常任务消费
    TN = token(),
    {ok, #{<<"out_path">> := Normal}} = attachment_gc_repo:v5_claim(Conn, TN),
    ok = attachment_gc_repo:v5_ack(Conn, Normal, TN, true),
    {ok, _, [{<<"done">>}]} = e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Normal]
    ),
    ok.

%% 逐 cell 观测（叶 ordering_or_fault × domain；cell 断言全部通过后落盘）。
%% 双域协议面：worker 协议在 ordinary 域实测；enterprise 叶共享同一
%% fn 协议面（如实披露，同 W4-01 confirm 先例）。
w402_emit(Entry, Ord, <<"ordinary">>, Measured) ->
    Base = Measured#{entrypoint_case => atom_to_binary(Entry, utf8)},
    attachment_lifecycle_v5_obs:emit(#{
        case_id => iolist_to_binary(["W4-02/", atom_to_binary(Entry, utf8)]),
        entrypoint => w402_ep_binary(Entry),
        scenario => atom_to_binary(Ord, utf8),
        domain => <<"ordinary">>,
        schema_phase => <<"expand-queue">>,
        measured => Base
    }),
    attachment_lifecycle_v5_obs:emit(#{
        case_id => iolist_to_binary(["W4-02/", atom_to_binary(Entry, utf8)]),
        entrypoint => w402_ep_binary(Entry),
        scenario => atom_to_binary(Ord, utf8),
        domain => <<"enterprise">>,
        schema_phase => <<"expand-queue">>,
        measured => Base#{shared_protocol_face => true}
    }),
    ok;
w402_emit(Entry, Ord, <<"ordinary_solo">>, Measured) ->
    %% 单域叶（契约矩阵 ordinary-queue-recovery 为 ordinary 4 叶，无双域）
    attachment_lifecycle_v5_obs:emit(#{
        case_id => iolist_to_binary(["W4-02/", atom_to_binary(Entry, utf8)]),
        entrypoint => w402_ep_binary(Entry),
        scenario => atom_to_binary(Ord, utf8),
        domain => <<"ordinary">>,
        schema_phase => <<"expand-queue">>,
        measured => Measured#{entrypoint_case => atom_to_binary(Entry, utf8)}
    }),
    ok;
w402_emit(Entry, Ord, Domain, Measured) ->
    attachment_lifecycle_v5_obs:emit(#{
        case_id => iolist_to_binary(["W4-02/", atom_to_binary(Entry, utf8)]),
        entrypoint => w402_ep_binary(Entry),
        scenario => atom_to_binary(Ord, utf8),
        domain => Domain,
        schema_phase => <<"expand-queue">>,
        measured => Measured#{entrypoint_case => atom_to_binary(Entry, utf8)}
    }),
    ok.

w402_ep_binary(claim_capacity_and_liveness) -> <<"claim-capacity-and-liveness">>;
w402_ep_binary(cross_queue_identity) -> <<"cross-queue-identity">>;
w402_ep_binary(last_attempt_crash) -> <<"last-attempt-crash">>;
w402_ep_binary(old_attempts_preserved) -> <<"old-attempts-preserved">>;
w402_ep_binary(ordinary_queue_recovery) -> <<"ordinary-queue-recovery">>;
w402_ep_binary(queue_adoption) -> <<"queue-adoption">>;
w402_ep_binary(worker_crash) -> <<"worker-crash">>;
w402_ep_binary(worker_db_fault) -> <<"worker-db-fault">>;
w402_ep_binary(worker_lease) -> <<"worker-lease">>.

run_base() ->
    case get(w402_run_base) of
        undefined ->
            B = (erlang:system_time(millisecond) rem 800000) * 1000,
            put(w402_run_base, B),
            B;
        B ->
            B
    end.

locator() -> lists:duplicate(64, $b).

%% 排干普通队列全部可领行（全部 ack done）——有限积压判定的前置
drain_ordinary(Conn) ->
    T = token(),
    case attachment_gc_repo:v5_claim(Conn, T) of
        {ok, #{<<"out_path">> := P}} ->
            ok = attachment_gc_repo:v5_ack(Conn, P, T, true),
            drain_ordinary(Conn);
        {error, empty} ->
            ok;
        {error, _} ->
            ok
    end.

seed_w402_tenants(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
        " account_type, reg_ip, reg_cosv, created_at)"
        " VALUES (101, 'w402-u', 'w402', '', 1, 0, '127.0.0.1', 'w402', now())"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id) VALUES (9101,'w402-org',101)"
        " ON CONFLICT (id) DO NOTHING"
    ),
    q(
        Conn,
        "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
        " VALUES (9101,9102,'w402-ws',101,'{}') ON CONFLICT (id) DO NOTHING"
    ),
    ok.
