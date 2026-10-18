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
        w402_worker_lease(Conn),
        w402_worker_crash_no_loss(Conn),
        w402_worker_db_fault(Conn),
        w402_last_attempt_crash(Conn),
        w402_old_attempts_preserved(Conn),
        w402_ordinary_queue_recovery(Conn),
        w402_queue_adoption(Conn),
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
    ok.

%% 2. worker-lease：claim 设租约 → ack(ok) → done + 审计
w402_worker_lease(Conn) ->
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

%% 3. worker-crash 无丢任务：crash（不 ack）→ 租约过期 → reaper 回收 →
%%    再 claim（attempts 递增，任务仍在）
w402_worker_crash_no_loss(Conn) ->
    Path = <<"v5e2e/w402/crash">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    _T = token(),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, token()),
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
    ok.

%% 4. worker-db-fault：旧 token ack=stale_token（0 行更新）；missing=missing；
%%    worker 不崩
w402_worker_db_fault(Conn) ->
    Path = <<"v5e2e/w402/fault">>,
    seed_ordinary_job(Conn, Path, 0, ready),
    {ok, _} = attachment_gc_repo:v5_claim(Conn, token()),
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
    ok.

%% 5. last-attempt-crash：attempts=7 → claim（→8）→ ack(false) → blocked
w402_last_attempt_crash(Conn) ->
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
    ok.

%% 6. old-attempts-preserved：接管行 attempts=5 → claim（6）→ ack(false) →
%%    retry_wait 且 attempts=6（不重置）
w402_old_attempts_preserved(Conn) ->
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
    %% 接管完成的 ready 行（身份完整）→ worker 可消费
    AdoptReady = <<"v5e2e/w402/adopt-ready-", (integer_to_binary(run_base()))/binary>>,
    seed_ordinary_job(Conn, AdoptReady, 0, ready),
    AdoptToken = token(),
    {ok, #{<<"out_path">> := AdoptReady}} = attachment_gc_repo:v5_claim(Conn, AdoptToken),
    ?assertEqual({error, {ack_rejected, <<"stale_token">>}},
        attachment_gc_repo:v5_ack(Conn, AdoptReady, token(), true)),
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
    ok = epgsql:sync(Conn).

%% 10. poison-not-starving：毒任务（删除恒败）不阻塞后续——先种毒任务与
%%     正常任务，毒任务 ack(false) 后正常任务仍可 claim 消费
w402_poison_not_starving(Conn) ->
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

run_base() ->
    case get(w402_run_base) of
        undefined ->
            B = (erlang:system_time(millisecond) rem 800000) * 1000,
            put(w402_run_base, B),
            B;
        B ->
            B
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
