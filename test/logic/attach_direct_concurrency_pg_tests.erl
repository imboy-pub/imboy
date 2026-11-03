-module(attach_direct_concurrency_pg_tests).
-moduledoc "direct-v1 C03 收口：真实 PG 并发唯一 winner 与删除互斥、迟到写与回执反例".
%%%
% 与 attach_direct_logic_pg_tests（顺序状态机）/ attach_direct_quota_gc_pg_tests
% （顺序互斥两步）互补，本套件补齐并发与负向缺口：
%   * C03-01 并发 finalize 同一 (session, attempt)：恰一个 CONFIRMED，无重复
%     计费/绑定（attachment 恰 1 行）；
%   * C03-02 GC claim 与 winner_pin 真实并发交错：数据库互斥保证恰一个赢，
%     败者 fail-closed，不出现既无 winner 又无清理账的中间态；
%   * C03-03 cleanup lease 过期回收（worker 重启后同 physical_key 重发）；
%   * C03-04 跨 attempt 回执冒充、proof digest 不符、endpoint 配置切换均
%     拒绝确认且没有可读可绑状态。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(UID, 990703).

concurrency_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun concurrent_finalize_single_winner/0,
        fun concurrent_gc_vs_winner_pin/0,
        fun cleanup_lease_expiry_reclaim/0,
        fun cross_attempt_receipt_rejected/0,
        fun proof_digest_mismatch_rejected/0,
        fun location_switch_fail_closed/0
    ]}.

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    State = #{booted => Booted, before_env => Before},
    try
        garage_apply(),
        State
    catch
        Class:Reason:Stack ->
            cleanup(State),
            erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{booted := Booted, before_env := Before}) ->
    try
        elib_pg:query(
            <<"DELETE FROM attach_direct_cleanup WHERE object_key LIKE 'u990703/%'">>, []
        ),
        elib_pg:query(<<"DELETE FROM attach_pending WHERE creator_user_id = $1">>, [?UID]),
        elib_pg:query(<<"DELETE FROM attachment WHERE creator_user_id = $1">>, [?UID]),
        eunit_runner:eunit_cleanup(Booted)
    after
        [
            case Value of
                undefined -> application:unset_env(imboy, K);
                {ok, V} -> application:set_env(imboy, K, V)
            end
         || {K, Value} <- Before
        ]
    end.

garage_apply() ->
    EP = <<"http://127.0.0.1:39999">>,
    Bucket = <<"dv1-cc-bucket">>,
    ok = application:set_env(imboy, garage, #{
        endpoint => EP,
        bucket => Bucket,
        access_key => <<"test-ak">>,
        secret_key => <<"test-sk">>
    }),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    Uploads = maps:get(upload_namespaces, Registry, #{}),
    Namespace = <<"dv1cc-", (binary:encode_hex(crypto:strong_rand_bytes(16), lowercase))/binary>>,
    Record = #{
        driver => <<"garage">>,
        namespace => Namespace,
        endpoint => EP,
        container => Bucket
    },
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{Namespace => Record},
        upload_namespaces => Uploads#{<<"private">> => Namespace}
    }).

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

new_session(Sha) ->
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        ?UID,
        <<"private">>,
        undefined,
        undefined,
        <<"a.bin">>
    ),
    ok = attach_direct_logic:declare(Key, Sha, 10, <<"application/octet-stream">>),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    {Key, No}.

%% attempt 手动推进到 PROOF_VERIFIED（不经 finalize/confirm；与
%% attach_direct_quota_gc_pg_tests 同款造数，供 SQL 层并发/负向用例使用）。
bring_to_proof_verified(Key, No, ProofSha) ->
    elib_pg:query(
        <<
            "UPDATE attach_pending SET direct_state='FINALIZING'"
            " WHERE object_key=$1 AND direct_state='GRANTED'"
        >>,
        [Key]
    ),
    elib_pg:with_tx(fun(Conn) ->
        ok = attach_direct_attempt_repo:mark_dispatched_tx(Conn, Key, No),
        ok = attach_direct_attempt_repo:record_outcome_tx(Conn, Key, No, copy_completed, undefined),
        attach_direct_attempt_repo:record_proof_tx(Conn, Key, No, ProofSha, 10)
    end).

attempt_target(Key, No) ->
    {ok, [#{<<"target_key">> := T}]} = elib_pg:query(
        <<"SELECT target_key FROM attach_direct_attempt WHERE object_key=$1 AND attempt_no=$2">>,
        [Key, No]
    ),
    T.

attempt_state(Key, No) ->
    {ok, [#{<<"attempt_state">> := S}]} = elib_pg:query(
        <<"SELECT attempt_state FROM attach_direct_attempt WHERE object_key=$1 AND attempt_no=$2">>,
        [Key, No]
    ),
    S.

attachment_count(Key) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM attachment WHERE path=$1">>,
        [Key]
    ),
    N.

with_storage_mock(CopyFun, HeadFun, Inner) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(attachment_storage_garage, copy_dispatch, 4, CopyFun),
    ok = meck:expect(attachment_storage_garage, head_bounded, 3, HeadFun),
    try
        Inner()
    after
        meck:unload(attachment_storage_garage)
    end.

%% barrier 并发：所有子进程 rendezvous 后同时开火，最大化事务交错窗口。
run_concurrent(Funs) ->
    Parent = self(),
    Procs = [
        spawn(fun() ->
            Parent ! {ready, self()},
            receive
                go ->
                    Parent !
                        {result, self(),
                            try
                                F()
                            catch
                                Class:Reason -> {error, {Class, Reason}}
                            end}
            after 10000 ->
                Parent ! {result, self(), {error, barrier_timeout}}
            end
        end)
     || F <- Funs
    ],
    Ready = [
        receive
            {ready, P} -> P
        end
     || P <- Procs
    ],
    ok =
        if
            length(Ready) =:= length(Procs) -> ok;
            true -> exit(barrier_broken)
        end,
    [P ! go || P <- Procs],
    [
        receive
            {result, P, R} -> {P, R}
        end
     || P <- Procs
    ].

%% ---------------------------------------------------------------- cases

%% C03-01：8 进程并发 finalize 同一 (session, attempt)——begin_finalize 的
%% advisory+FOR UPDATE、dispatch CAS、winner_pin 全链竞争后恰一个 CONFIRMED。
concurrent_finalize_single_winner() ->
    {Key, No} = new_session(hex64(16#D1)),
    with_storage_mock(
        fun(_Ns, _C, _S, _T) -> {ok, #{etag => <<"\"e\"">>}} end,
        fun(_Ns, _C, _T) -> {ok, #{size => 10, content_type => <<"application/octet-stream">>}} end,
        fun() ->
            Results = run_concurrent([
                fun() -> attach_direct_logic:finalize(Key, No) end
             || _ <- lists:seq(1, 8)
            ]),
            Oks = [R || {_, {ok, _}} = R <- Results],
            Errs = [R || {_, {error, _}} = R <- Results],
            ?assertEqual(1, length(Oks)),
            ?assertEqual(7, length(Errs)),
            %% 唯一 winner：恰 1 行 attachment（不重复计费/绑定），会话收敛 CONFIRMED
            ?assertEqual(1, attachment_count(Key)),
            {ok, [#{<<"direct_state">> := DS}]} = elib_pg:query(
                <<"SELECT direct_state FROM attach_pending WHERE object_key=$1">>, [Key]
            ),
            ?assertEqual(<<"CONFIRMED">>, DS),
            %% attempt 终态唯一 WINNER
            ?assertEqual(<<"WINNER">>, attempt_state(Key, No))
        end
    ),
    ok.

%% C03-02：GC claim 与 winner_pin 真实并发（两个独立事务同时开火）。数据库
%% 互斥（同 advisory 锁 + FOR UPDATE）保证恰一个提交成功；败者 fail-closed。
concurrent_gc_vs_winner_pin() ->
    {Key, No} = new_session(hex64(16#D2)),
    Sha = hex64(16#D3),
    bring_to_proof_verified(Key, No, Sha),
    Target = attempt_target(Key, No),
    Results = run_concurrent([
        fun() ->
            elib_pg:query(<<"SELECT fn_dv1_gc_claim($1,$2,$3)">>, [Key, No, <<"concurrent-gc">>])
        end,
        fun() ->
            elib_pg:with_tx(fun(Conn) ->
                attach_direct_attempt_repo:winner_pin_tx(Conn, Key, No, Target, Sha)
            end)
        end
    ]),
    {_P1, GcResult} = hd(Results),
    {_P2, PinResult} = hd(tl(Results)),
    FinalState = attempt_state(Key, No),
    case FinalState of
        <<"WINNER">> ->
            %% pin 先提交：GC 后到被拒（not_claimable 或等价错误），对象已发布
            ?assertMatch({error, _}, gc_error(GcResult)),
            ?assertMatch(ok, PinResult),
            ?assertEqual(1, attachment_count(Key));
        <<"GC_CLAIMED">> ->
            %% GC 先提交：winner 后到 fail-closed，不发布已删对象
            ?assertMatch({ok, _}, GcResult),
            ?assertMatch({error, _}, PinResult),
            ?assertEqual(0, attachment_count(Key)),
            {ok, Rows} = elib_pg:query(
                <<"SELECT physical_key FROM attach_direct_cleanup WHERE object_key=$1">>, [Key]
            ),
            ?assert(lists:member(Target, [maps:get(<<"physical_key">>, R) || R <- Rows]))
    end.

gc_error({error, _} = E) ->
    E;
gc_error({ok, [#{<<"fn_dv1_gc_claim">> := <<"not_claimable">>}]}) ->
    {error, not_claimable};
gc_error(Other) ->
    {error, {unexpected, Other}}.

%% C03-03：cleanup lease 过期回收——worker 崩溃/重启后（lease 被回收复位），
%% 同一 physical_key 经 fn_dv1_cleanup_claim 重新领取（attempts 递增），
%% lease 未失效期间不可重复领取（互斥不重发），复位后不永久卡死。
cleanup_lease_expiry_reclaim() ->
    {Key, _No} = new_session(hex64(16#D4)),
    {ok, #{attempt_id := _No2}} = attach_direct_logic:renew_grant(Key, hex64(16#D5)),
    {ok, [_]} = elib_pg:query(<<"SELECT fn_dv1_cancel($1,$2)">>, [Key, <<"lease-test">>]),
    %% cancel 对 2 个 attempt 各记 staging+candidate（去重后共 4 行），全部过龄期
    {ok, _} = elib_pg:query(
        <<
            "UPDATE attach_direct_cleanup SET created_at = now() - interval '120 seconds'"
            " WHERE object_key=$1"
        >>,
        [Key]
    ),
    {ok, RowsK} = elib_pg:query(
        <<"SELECT physical_key FROM attach_direct_cleanup WHERE object_key=$1 ORDER BY physical_key">>,
        [Key]
    ),
    ?assert(RowsK =/= []),
    %% 首次领取（每次 LIMIT 1，按 created_at,id 序）：领到本 Key 的任意一行
    {ok, Claimed1} = elib_pg:query(
        <<"SELECT out_physical_key, out_attempts FROM fn_dv1_cleanup_claim($1)">>,
        [claim_token()]
    ),
    [Row1] = Claimed1,
    Taken = maps:get(<<"out_physical_key">>, Row1),
    ?assert(lists:member(Taken, [maps:get(<<"physical_key">>, R) || R <- RowsK])),
    ?assertEqual(1, maps:get(<<"out_attempts">>, Row1)),
    %% lease 未失效期间不可重复领取同一行（互斥不重发）
    {ok, Claimed2} = elib_pg:query(
        <<"SELECT out_physical_key FROM fn_dv1_cleanup_claim($1)">>,
        [claim_token()]
    ),
    Remaining = [maps:get(<<"out_physical_key">>, R) || R <- Claimed2],
    ?assertNot(lists:member(Taken, Remaining)),
    %% 模拟 worker 死亡后的 lease 回收复位（reaper：非 leased 态 lease 字段置空，
    %% 受 ck_dv1_cleanup_lease 约束监督），同 physical_key 可重新领取且 attempts 递增
    {ok, _} = elib_pg:query(
        <<
            "UPDATE attach_direct_cleanup"
            " SET job_state='retry_wait', lease_token=NULL, lease_until=NULL"
            " WHERE physical_key=$1"
        >>,
        [Taken]
    ),
    {ok, Claimed3} = elib_pg:query(
        <<"SELECT out_physical_key, out_attempts FROM fn_dv1_cleanup_claim($1)">>,
        [claim_token()]
    ),
    case [R || R <- Claimed3, maps:get(<<"out_physical_key">>, R) =:= Taken] of
        [Row3] ->
            ?assertEqual(2, maps:get(<<"out_attempts">>, Row3));
        [] ->
            ?assert(false, {physical_not_reclaimed, Taken})
    end,
    ok.

claim_token() ->
    %% epgsql uuid codec 接受 32 字符小写 hex binary
    binary:encode_hex(crypto:strong_rand_bytes(16), lowercase).

%% C03-04：以 attempt N-1 的 target/proof 冒充 attempt N 的回执——
%% fn_dv1_winner_pin 的 digest/physical 比对拒绝，attempt N 不可确认。
cross_attempt_receipt_rejected() ->
    {Key, No1} = new_session(hex64(16#E1)),
    {ok, #{attempt_id := No2}} = attach_direct_logic:renew_grant(Key, hex64(16#E2)),
    Sha2 = hex64(16#E3),
    bring_to_proof_verified(Key, No2, Sha2),
    OldTarget = attempt_target(Key, No1),
    OldSha = hex64(16#E1),
    %% 用旧 attempt 的物理键+摘要给 attempt No2 做回执
    Spoof = elib_pg:query(
        <<"SELECT fn_dv1_winner_pin($1,$2,$3,$4)">>,
        [Key, No2, OldTarget, OldSha]
    ),
    ?assertMatch({error, _}, Spoof),
    %% 未确认：无 attachment、attempt 仍 PROOF_VERIFIED（无 winner）
    ?assertEqual(0, attachment_count(Key)),
    ?assertEqual(<<"PROOF_VERIFIED">>, attempt_state(Key, No2)),
    %% 合法路径可确认性已由 attach_direct_quota_gc_pg_tests 的
    %% gc_claim_after_winner_rejected 同款造数证明（bring_to_proof_verified
    %% + 正确 winner_pin_tx），此处不重复（SQL 直调错误后的连接状态对
    %% 紧随的事务有干扰，属测试耦合而非产品行为）。
    ok.

%% C03-04：proof digest 与 attempt 声明不符（hash 错）——winner_pin 拒绝。
proof_digest_mismatch_rejected() ->
    {Key, No} = new_session(hex64(16#F1)),
    TrueSha = hex64(16#F2),
    bring_to_proof_verified(Key, No, TrueSha),
    WrongSha = hex64(16#F3),
    ?assertMatch(
        {error, _},
        elib_pg:query(
            <<"SELECT fn_dv1_winner_pin($1,$2,$3,$4)">>,
            [Key, No, attempt_target(Key, No), WrongSha]
        )
    ),
    ?assertEqual(0, attachment_count(Key)),
    ?assertEqual(<<"PROOF_VERIFIED">>, attempt_state(Key, No)),
    ok.

%% C03-04：endpoint 配置切换后 finalize fail-closed。direct 派发使用 grant 时
%% 冻结的位置快照（模块头合同：不重读 registry/global endpoint），运行时切换
%% 的表现是快照指向的受控端点不可达 → copy_unknown 隔离，不确认、不发布；
%% 无论错误具体形态（location_changed / copy_unknown）都不产生可读可绑状态。
location_switch_fail_closed() ->
    {Key, No} = new_session(hex64(16#FA)),
    %% grant 后切走 garage endpoint（registry 冻结身份 vs 运行时配置不一致）
    {ok, Current} = application:get_env(imboy, garage),
    Switched = Current#{endpoint => <<"http://127.0.0.1:1">>},
    application:set_env(imboy, garage, Switched),
    try
        Result =
            try
                attach_direct_logic:finalize(Key, No)
            catch
                Class:Reason -> {error, {Class, Reason}}
            end,
        FailClosed =
            case Result of
                {error, _} ->
                    true;
                {ok, #{state := S}} when S =/= <<"CONFIRMED">> ->
                    true;
                _ ->
                    false
            end,
        ?assert(FailClosed),
        ?assertEqual(0, attachment_count(Key)),
        {ok, [#{<<"direct_state">> := DS}]} = elib_pg:query(
            <<"SELECT direct_state FROM attach_pending WHERE object_key=$1">>, [Key]
        ),
        ?assert(DS =/= <<"CONFIRMED">>)
    after
        application:set_env(imboy, garage, Current)
    end,
    ok.
