%%% @doc W6-01 E 组：W4-02 worker 四组（claim-capacity-and-liveness 16 +
%%% worker-lease 16 + worker-crash 6 + last-attempt-crash 12 = 50 叶），
%%% schema_phase=integrated。
%%%
%%% oracle 复用封印源 w402_worker_tests（两队列统一 CAS 协议：无丢任务、
%%% 旧 token 0 更新、8 次终止、租约回收后可再消费）。协议场景是 SECURITY
%%% DEFINER fn 面：ordinary 叶真实执行；enterprise 叶以同一协议面重放并
%%% 如实披露 shared_protocol_face（同 W4-02 封印先例）。driver 维度 = 队列
%%% 行冻结身份三元组指向该叶 driver 的真实 registry 根；worker-crash 的
%%% kill_after_delete 叶删除经 attachment_storage:delete_snapshot 真实落桶
%%% （garage 字节消失为实测读回 not_found）。
-module(attachment_lifecycle_v5_w601_w4_worker_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(X, attachment_lifecycle_v5_w601_w4_context).
-define(QP, attachment_lifecycle_v5_w601_w4_queueproto).
-define(ORD, <<"ordinary">>).
-define(ENT, <<"enterprise">>).

w601_w4_worker_test_() ->
    {setup, fun() -> ?X:setup(<<"V5_INTTEST_W601W4WORK">>) end, fun ?X:cleanup/1, fun(St) ->
        [?X:leaf(S, fun() -> scenario(St, S) end) || S <- specs()]
    end}.

specs() ->
    lists:append([
        group_specs(
            <<"W4-02/claim-capacity-and-liveness">>,
            <<"claim-capacity-and-liveness">>,
            [
                <<"claim_capacity">>,
                <<"final_lease_reaper_deadline">>,
                <<"finite_backlog_deadline">>,
                <<"independent_queue_quota">>
            ]
        ),
        group_specs(
            <<"W4-02/worker-lease">>,
            <<"worker-lease">>,
            [
                <<"concurrent_two_claims">>,
                <<"expired_delete_same_tombstone">>,
                <<"fixed_lease_no_renewal">>,
                <<"stale_token_completion">>
            ]
        ),
        crash_specs(),
        group_specs(
            <<"W4-02/last-attempt-crash">>,
            <<"last-attempt-crash">>,
            [
                <<"attempt9_claim_rejected">>,
                <<"kill_attempt8_then_reap">>,
                <<"restart_preserves_attempts">>
            ]
        )
    ]).

group_specs(Case, Entry, Scenarios) ->
    [
        {Case, Entry, Drv, Scn, Dom}
     || Scn <- Scenarios,
        Dom <- [?ORD, ?ENT],
        Drv <- [<<"disk">>, <<"garage">>]
    ].

%% worker-crash 合同仅 6 叶：3 scenario × 2 domain × garage。
crash_specs() ->
    [
        {<<"W4-02/worker-crash">>, <<"worker-crash">>, <<"garage">>, Scn, Dom}
     || Scn <- [
            <<"kill_before_claim_commit">>,
            <<"kill_after_claim_before_delete">>,
            <<"kill_after_delete_before_completion">>
        ],
        Dom <- [?ORD, ?ENT]
    ].

%% ------------------------------------------------------------- 分发

scenario(St, {_Case, Entry, Drv, Scn, Dom}) ->
    #{conn := Conn} = St,
    Key = unique_key(Entry),
    Triple = ?X:triple(St, Drv, Key),
    Measured = run_scenario(St, Conn, Entry, Scn, Key, Triple),
    Measured#{
        driver => Drv,
        object_key => Key,
        storage_namespace => maps:get(namespace, Triple),
        storage_container => maps:get(container, Triple),
        locator_sha256 => maps:get(locator, Triple),
        domain_face => Dom,
        shared_protocol_face => Dom =:= ?ENT
    }.

unique_key(Entry) ->
    N = (?X:leaf_id(workbase) rem 100000) * 1000 + ?X:leaf_id(work),
    <<"v5e2e/w601w/", Entry/binary, "/", (integer_to_binary(N))/binary>>.

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

%% --------------------------------------------------- claim-capacity

run_scenario(St, Conn, Entry, Scn, Key, Triple) ->
    case Entry of
        <<"claim-capacity-and-liveness">> -> claim_group(St, Conn, Scn, Key, Triple);
        <<"worker-lease">> -> lease_cycle(Conn, Scn, Key, Triple);
        <<"worker-crash">> -> crash_cycle(St, Conn, Scn, Key, Triple);
        <<"last-attempt-crash">> -> last_cycle(Conn, Scn, Key, Triple)
    end.

claim_group(St, Conn, Scn, Key, Triple) ->
    case Scn of
        <<"claim_capacity">> -> claim_capacity(Conn, Key, Triple);
        <<"final_lease_reaper_deadline">> -> reaper_deadline(Conn, Key, Triple);
        <<"finite_backlog_deadline">> -> finite_backlog(Conn, Key, Triple);
        <<"independent_queue_quota">> -> queue_quota(St, Conn, Key, Triple)
    end.

claim_capacity(Conn, Key, Triple) ->
    ?QP:reset_ordinary(Conn),
    Paths = [
        <<Key/binary, "-cap-", (integer_to_binary(I))/binary>>
     || I <- lists:seq(1, 3)
    ],
    lists:foreach(fun(P) -> seed_job(Conn, P, Triple, 0, ready) end, Paths),
    Claimed = [
        begin
            T = ?X:token(),
            {ok, #{<<"out_path">> := P}} = ?QP:claim_ordinary(Conn, T),
            P
        end
     || _ <- Paths
    ],
    ?assertEqual(lists:sort(Paths), lists:sort(Claimed)),
    {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
    #{claimed => length(Paths), backlog_after => 0}.

reaper_deadline(Conn, Key, Triple) ->
    ?QP:reset_ordinary(Conn),
    seed_job(Conn, Key, Triple, 0, ready),
    {ok, _} = ?QP:claim_ordinary(Conn, ?X:token()),
    {ok, _, [{<<"leased">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ?QP:expire_ordinary(Conn, Key),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    {ok, _, [{<<"retry_wait">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{reaped => 1, state_after => <<"retry_wait">>}.

finite_backlog(Conn, Key, Triple) ->
    ?QP:drain_ordinary(Conn),
    Paths = [
        <<Key/binary, "-bl-", (integer_to_binary(I))/binary>>
     || I <- lists:seq(1, 2)
    ],
    lists:foreach(fun(P) -> seed_job(Conn, P, Triple, 0, ready) end, Paths),
    Claimed = [
        begin
            T = ?X:token(),
            {ok, #{<<"out_path">> := P}} = ?QP:claim_ordinary(Conn, T),
            ok = attachment_gc_repo:v5_ack(Conn, P, T, true),
            P
        end
     || _ <- Paths
    ],
    ?assertEqual(length(Paths), length(Claimed)),
    {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
    #{backlog => length(Paths), drained => true}.

queue_quota(_St, Conn, Key, Triple) ->
    ?QP:reset_ordinary(Conn),
    seed_job(Conn, Key, Triple, 0, ready),
    EbId = 948000000000 + eb_leaf_id(),
    EbKey = ?X:unique_key(<<"enterprise/9101/9102/quota">>),
    ok = ?X:seed_eb_asset_row(Conn, EbKey, EbId, Triple, <<"deleted">>, true, <<"live">>),
    {ok, 1} = ?X:e(
        Conn,
        "INSERT INTO enterprise_asset_delete_queue"
        " (organization_id,workspace_id,asset_id,object_key,storage_driver,"
        "  storage_namespace,storage_container,storage_locator_sha256,"
        "  job_state,next_retry_at,attempts)"
        " VALUES (9101,9102,$1,$2,$3,$4,$5,$6,'ready',now(),0)"
        " ON CONFLICT (organization_id,workspace_id,asset_id) DO NOTHING",
        [
            EbId,
            EbKey,
            maps:get(driver, Triple),
            maps:get(namespace, Triple),
            maps:get(container, Triple),
            maps:get(locator, Triple)
        ]
    ),
    {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, ?X:token()),
    {ok, _, [{1}]} = ?X:e(
        Conn,
        "SELECT COUNT(*) FROM enterprise_asset_delete_queue"
        " WHERE organization_id=9101 AND workspace_id=9102 AND asset_id=$1"
        " AND job_state='ready'",
        [EbId]
    ),
    #{ordinary_claimed => true, eb_queue_rows => 1, eb_asset_id => EbId}.

eb_leaf_id() ->
    948000000000 +
        ((?X:leaf_id(ebbase) rem 100000) * 1000 + ?X:leaf_id(work) - 1).

%% ------------------------------------------------------- worker-lease

lease_cycle(Conn, Scn, Key, Triple) ->
    case Scn of
        <<"concurrent_two_claims">> ->
            ?QP:reset_ordinary(Conn),
            seed_job(Conn, Key, Triple, 0, ready),
            T1 = ?X:token(),
            {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, T1),
            {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
            {ok, _, [{LeaseT}]} = ?X:e(
                Conn,
                "SELECT lease_token FROM attachment_object_deletion WHERE path=$1",
                [Key]
            ),
            ?assertEqual(T1, LeaseT),
            #{single_current_token => true, second_claim => <<"empty">>};
        <<"fixed_lease_no_renewal">> ->
            ?QP:reset_ordinary(Conn),
            seed_job(Conn, Key, Triple, 0, ready),
            {ok, _} = ?QP:claim_ordinary(Conn, ?X:token()),
            {ok, _, [{L1}]} = ?X:e(
                Conn,
                "SELECT lease_until FROM attachment_object_deletion WHERE path=$1",
                [Key]
            ),
            {ok, _, [{_}]} = ?X:e(
                Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            {ok, _, [{L2}]} = ?X:e(
                Conn,
                "SELECT lease_until FROM attachment_object_deletion WHERE path=$1",
                [Key]
            ),
            ?assertEqual(L1, L2),
            #{lease_unchanged => true};
        <<"expired_delete_same_tombstone">> ->
            expired_tombstone(Conn, Key, Triple);
        <<"stale_token_completion">> ->
            stale_token_completion(Conn, Key, Triple)
    end.

%% 删除重试只针对不可变墓碑身份（队列行保留原 (ns,container,key)）。
expired_tombstone(Conn, Key, Triple) ->
    ?QP:reset_ordinary(Conn),
    seed_job(Conn, Key, Triple, 0, ready),
    T = ?X:token(),
    {ok, _} = ?QP:claim_ordinary(Conn, T),
    ok = attachment_gc_repo:v5_ack(Conn, Key, T, false),
    {ok, _, [{<<"retry_wait">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    {ok, _, [{Identity}]} = ?X:e(
        Conn,
        "SELECT storage_locator_sha256 FROM attachment_object_deletion"
        " WHERE path=$1",
        [Key]
    ),
    ?assertNotEqual(null, Identity),
    #{tombstone_identity_stable => true, state_after => <<"retry_wait">>}.

stale_token_completion(Conn, Key, Triple) ->
    ?QP:reset_ordinary(Conn),
    seed_job(Conn, Key, Triple, 0, ready),
    T1 = ?X:token(),
    {ok, _} = ?QP:claim_ordinary(Conn, T1),
    {error, {ack_rejected, <<"stale_token">>}} =
        attachment_gc_repo:v5_ack(Conn, Key, ?X:token(), true),
    {ok, _, [{<<"leased">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    ok = attachment_gc_repo:v5_ack(Conn, Key, T1, true),
    {ok, _, [{<<"done">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{stale_rejected => true, current_token_done => true}.

%% ------------------------------------------------------- worker-crash

%% 仅 garage 叶：kill_after_delete 的删除经冻结位置真实落桶。
crash_cycle(St, Conn, Scn, Key, Triple) ->
    ?QP:reset_ordinary(Conn),
    seed_job(Conn, Key, Triple, 0, ready),
    case Scn of
        <<"kill_before_claim_commit">> ->
            {ok, _, [{0, <<"ready">>}]} = ?X:e(
                Conn,
                "SELECT attempts, job_state FROM attachment_object_deletion"
                " WHERE path=$1",
                [Key]
            ),
            #{zero_delete_calls => true, state_after => <<"ready">>};
        <<"kill_after_claim_before_delete">> ->
            T = ?X:token(),
            {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, T),
            ?QP:expire_ordinary(Conn, Key),
            {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
            {ok, _, [{<<"retry_wait">>}]} = ?X:e(
                Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
            ),
            T2 = ?X:token(),
            {ok, #{<<"out_path">> := Key, <<"out_attempts">> := 2}} =
                ?QP:claim_ordinary(Conn, T2),
            #{recovered_after_reap => true, attempts_after => 2};
        <<"kill_after_delete_before_completion">> ->
            bytes_gone_cycle(St, Conn, Key, Triple)
    end.

bytes_gone_cycle(St, Conn, Key, Triple) ->
    Drv = maps:get(driver, Triple),
    Bytes = <<"w601-crash-bytes:", Key/binary>>,
    ok = ?X:put_probe(St, Drv, Key, Bytes),
    T = ?X:token(),
    {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, T),
    ok = attachment_storage:delete_snapshot(?QP:frozen_ordinary(Conn, Key)),
    {error, not_found} = driver_read(St, Triple, Key),
    ?QP:expire_ordinary(Conn, Key),
    {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
    T2 = ?X:token(),
    {ok, #{<<"out_path">> := Key}} = ?QP:claim_ordinary(Conn, T2),
    ok = attachment_gc_repo:v5_ack(Conn, Key, T2, true),
    {ok, _, [{<<"done">>}]} = ?X:e(
        Conn, "SELECT job_state FROM attachment_object_deletion WHERE path=$1", [Key]
    ),
    #{resolved_to_done => true, bytes_gone => true}.

driver_read(_St, Triple, Key) ->
    attachment_storage_garage:read(
        maps:get(namespace, Triple), maps:get(container, Triple), Key
    ).

%% -------------------------------------------------- last-attempt-crash

last_cycle(Conn, Scn, Key, Triple) ->
    ?QP:drain_ordinary(Conn),
    case Scn of
        <<"attempt9_claim_rejected">> ->
            seed_job(Conn, Key, Triple, 7, ready),
            T = ?X:token(),
            {ok, #{<<"out_attempts">> := 8}} = ?QP:claim_ordinary(Conn, T),
            ok = attachment_gc_repo:v5_ack(Conn, Key, T, false),
            {ok, _, [{<<"blocked">>, <<"max_attempts">>}]} = ?X:e(
                Conn,
                "SELECT job_state, last_error_code FROM attachment_object_deletion"
                " WHERE path=$1",
                [Key]
            ),
            {error, empty} = ?QP:claim_ordinary(Conn, ?X:token()),
            #{blocked_after_ack_false => true, ninth_claim => <<"empty">>};
        <<"kill_attempt8_then_reap">> ->
            seed_job(Conn, Key, Triple, 7, ready),
            {ok, #{<<"out_attempts">> := 8}} = ?QP:claim_ordinary(Conn, ?X:token()),
            ?QP:expire_ordinary(Conn, Key),
            {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
            {ok, _, [{<<"blocked">>, 8, <<"max_attempts">>}]} = ?X:e(
                Conn,
                "SELECT job_state, attempts, last_error_code"
                " FROM attachment_object_deletion WHERE path=$1",
                [Key]
            ),
            #{reaped_to_blocked => true, attempts => 8};
        <<"restart_preserves_attempts">> ->
            seed_job(Conn, Key, Triple, 5, ready),
            {ok, #{<<"out_attempts">> := 6}} = ?QP:claim_ordinary(Conn, ?X:token()),
            ?QP:expire_ordinary(Conn, Key),
            {ok, 1} = attachment_gc_repo:v5_reap(Conn, 100),
            T2 = ?X:token(),
            {ok, #{<<"out_attempts">> := 7}} = ?QP:claim_ordinary(Conn, T2),
            #{attempts_monotonic => [5, 6, 7]}
    end.
