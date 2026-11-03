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
%% winner_pin 合同（迁移 00000200 注释 + 00000196 fn_v5_pending_session_guard
%% line 40）：confirm 跳转前 attachment 行必须已存在（正常链由
%% save_confirmed_tx→confirm_if_absent_tx 建立，物理键留 NULL 由 pin 补映射）。
%% 造数因此复用同一 confirm_if_absent_tx 入口建行；缺失即 'v5 confirm
%% unknown asset'（23510，被 winner_pin_tx 折叠为 winner_conflict）。
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
        attach_direct_attempt_repo:record_proof_tx(Conn, Key, No, ProofSha, 10),
        %% 正常链 save_confirmed_tx 同款建行（诊断 v4 实证）：location 四元组
        %% 取 pending register 冻结值、物理键预填 attempt target——与
        %% attach_direct_logic:confirm_attach_and_pin 交付给 save_confirmed_tx
        %% 的 Attach+Location 逐字段一致。缺失行 → 'v5 confirm unknown asset'；
        %% 缺 location → 'v5 attachment location immutable after fill'；两者
        %% 均 23510，被 winner_pin_tx 折叠为 winner_conflict（已确诊）。
        {ok, [Pending]} = elib_pg:query(
            <<
                "SELECT storage_driver, storage_namespace, storage_container,"
                " storage_locator_sha256 FROM attach_pending WHERE object_key=$1"
            >>,
            [Key]
        ),
        {ok, _Id, _First} = attachment_repo:confirm_if_absent_tx(
            Conn,
            elib_dt:now(),
            ?UID,
            #{
                <<"path">> => Key,
                <<"mime_type">> => <<"application/octet-stream">>,
                <<"name">> => <<"a.bin">>,
                <<"url">> => Key,
                <<"size">> => 10,
                <<"scope">> => <<"private">>,
                <<"storage_driver">> => maps:get(<<"storage_driver">>, Pending),
                <<"storage_namespace">> => maps:get(<<"storage_namespace">>, Pending),
                <<"storage_container">> => maps:get(<<"storage_container">>, Pending),
                <<"storage_locator_sha256">> =>
                    maps:get(<<"storage_locator_sha256">>, Pending),
                <<"storage_physical_key">> => attempt_target(Key, No)
            }
        ),
        ok
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

physical_key_row(Key) ->
    elib_pg:query(
        <<"SELECT storage_physical_key AS pk FROM attachment WHERE path=$1">>,
        [Key]
    ).

pending_confirmed(Key) ->
    {ok, [#{<<"direct_state">> := DS}]} = elib_pg:query(
        <<"SELECT direct_state FROM attach_pending WHERE object_key=$1">>, [Key]
    ),
    DS =:= <<"CONFIRMED">>.

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
%% 两个 spawn 的启动顺序在此确定性调度下决定 advisory 锁归属（实证 12/12
%% 先 spawn 者赢），因此正反两种 spawn 顺序各跑一轮，分别覆盖两个合法
%% 终态分支，并证明互斥性与到达顺序无关。
concurrent_gc_vs_winner_pin() ->
    ok = gc_vs_winner_race(gc_first),
    ok = gc_vs_winner_race(pin_first),
    ok.

gc_vs_winner_race(Order) ->
    {Key, No} = new_session(hex64(16#D2)),
    Sha = hex64(16#D3),
    bring_to_proof_verified(Key, No, Sha),
    Target = attempt_target(Key, No),
    GcFun = fun() ->
        elib_pg:query(<<"SELECT fn_dv1_gc_claim($1,$2,$3)">>, [Key, No, <<"concurrent-gc">>])
    end,
    PinFun = fun() ->
        elib_pg:with_tx(fun(Conn) ->
            attach_direct_attempt_repo:winner_pin_tx(Conn, Key, No, Target, Sha)
        end)
    end,
    Funs =
        case Order of
            gc_first -> [GcFun, PinFun];
            pin_first -> [PinFun, GcFun]
        end,
    Results = run_concurrent(Funs),
    %% run_concurrent 保序返回：首元素对应 Funs 的第一项（按 Order 即 GC 或 pin）
    GcResult =
        case Order of
            gc_first -> element(2, hd(Results));
            pin_first -> element(2, lists:nth(2, Results))
        end,
    PinResult =
        case Order of
            gc_first -> element(2, lists:nth(2, Results));
            pin_first -> element(2, hd(Results))
        end,
    FinalState = attempt_state(Key, No),
    case FinalState of
        <<"WINNER">> ->
            %% pin 先提交：GC 后到被拒（not_claimable 或等价错误），物理映射
            %% 补齐、pending CONFIRMED（可发布状态）
            ?assertMatch({error, _}, gc_error(GcResult)),
            ?assertMatch(ok, PinResult),
            ?assertEqual(1, attachment_count(Key)),
            {ok, [#{<<"pk">> := WinPk}]} = physical_key_row(Key),
            ?assertEqual(Target, WinPk),
            ?assert(pending_confirmed(Key));
        <<"GC_CLAIMED">> ->
            %% GC 先提交：winner 后到 fail-closed，不发布已删对象：
            %% 物理键未映射（不可绑定读取）、pending 非 CONFIRMED、清理账入列
            ?assertMatch({ok, _}, GcResult),
            ?assertMatch({error, _}, PinResult),
            ?assertEqual(1, attachment_count(Key)),
            %% 物理键已按正常链预填 target；不可读可绑由 pending 非
            %% CONFIRMED 保证（对象从未进入已确认状态）
            {ok, [#{<<"pk">> := GcPk}]} = physical_key_row(Key),
            ?assertEqual(Target, GcPk),
            ?assertNot(pending_confirmed(Key)),
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
    %% cancel 对 2 个 attempt 各记 staging+candidate（去重后共 4 行）。龄期设
    %% 1 小时使本 Key 的行在 fn_dv1_cleanup_claim 的全表 (created_at,id) 序中
    %% 排最前（共享测试库中他套件残留行不会插队）；循环领取兜底残留插队。
    {ok, _} = elib_pg:query(
        <<
            "UPDATE attach_direct_cleanup SET created_at = now() - interval '1 hour'"
            " WHERE object_key=$1"
        >>,
        [Key]
    ),
    {ok, RowsK} = elib_pg:query(
        <<"SELECT physical_key FROM attach_direct_cleanup WHERE object_key=$1 ORDER BY physical_key">>,
        [Key]
    ),
    ?assert(RowsK =/= []),
    OurKeys = [maps:get(<<"physical_key">>, R) || R <- RowsK],
    %% 循环领取（cap 12）：领到本 Key 的任意一行为止
    {Taken, Row1} = claim_until_ours(OurKeys, 12),
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

%% fn_dv1_cleanup_claim 全表按 (created_at,id) LIMIT 1 领取：循环直到领到
%% 本 Key 的行（cap 内领空即失败——本 Key 行龄期 1h 必在任意外来残留行之前，
%% 连续领空说明表状态异常，属测试环境问题应显式暴露）。
claim_until_ours(OurKeys, N) when N =< 0 ->
    erlang:error({claim_ours_exhausted, OurKeys});
claim_until_ours(OurKeys, N) ->
    {ok, Claimed} = elib_pg:query(
        <<"SELECT out_physical_key, out_attempts FROM fn_dv1_cleanup_claim($1)">>,
        [claim_token()]
    ),
    case [R || R <- Claimed, lists:member(maps:get(<<"out_physical_key">>, R), OurKeys)] of
        [Row] ->
            {maps:get(<<"out_physical_key">>, Row), Row};
        [] ->
            %% 领到外来行或领空：外来行继续消化，领空直接报错
            case Claimed of
                [] -> erlang:error({claim_ours_exhausted, OurKeys});
                _ -> claim_until_ours(OurKeys, N - 1)
            end
    end.

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
    %% 冒充被拒：attempt 仍 PROOF_VERIFIED，attachment 行存在（confirm 建行、
    %% 物理键按正常链预填 target），pending 非 CONFIRMED——无可读可绑状态。
    ?assertEqual(<<"PROOF_VERIFIED">>, attempt_state(Key, No2)),
    ?assertEqual(1, attachment_count(Key)),
    {ok, [#{<<"pk">> := SpoofPk}]} = physical_key_row(Key),
    ?assertEqual(attempt_target(Key, No2), SpoofPk),
    ?assertNot(pending_confirmed(Key)),
    %% 冒充被拒后，同一 attempt 的合法回执仍可确认（fail-closed 不误伤合法
    %% 路径）：digest/物理键门禁放行，物理映射补齐，pending 收敛 CONFIRMED
    %% 且 referer_time=1（不重复计费）。
    ok = elib_pg:with_tx(fun(Conn) ->
        attach_direct_attempt_repo:winner_pin_tx(
            Conn, Key, No2, attempt_target(Key, No2), Sha2
        )
    end),
    ?assertEqual(<<"WINNER">>, attempt_state(Key, No2)),
    {ok, [#{<<"pk">> := MappedPk}]} = physical_key_row(Key),
    ?assertEqual(attempt_target(Key, No2), MappedPk),
    ?assert(pending_confirmed(Key)),
    {ok, [Att]} = elib_pg:query(
        <<"SELECT referer_time FROM attachment WHERE path=$1">>, [Key]
    ),
    ?assertEqual(1, maps:get(<<"referer_time">>, Att)),
    ?assert(pending_confirmed(Key)),
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
    %% digest 拒后：attempt 仍 PROOF_VERIFIED，pending 非 CONFIRMED
    %% （无 winner、无可读可绑状态；物理键按正常链预填 target）
    ?assertEqual(<<"PROOF_VERIFIED">>, attempt_state(Key, No)),
    {ok, [#{<<"pk">> := Pk1}]} = physical_key_row(Key),
    ?assertEqual(attempt_target(Key, No), Pk1),
    ?assertNot(pending_confirmed(Key)),
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
