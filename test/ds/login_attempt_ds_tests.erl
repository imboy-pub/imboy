-module(login_attempt_ds_tests).
-moduledoc "Limiter V1 权威状态 owner 测试：新 API 基础行为、兼容层与 AC-03/04。".
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 时钟注入是唯一 seam；D 与存储不 mock；日志静音沿用项目先例。
-define(LOG_MOCK,
    {elib_log, [
        {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
    ]}
).
-define(CLOCK_MOCK,
    {login_attempt_clock, [{'now_ms', 0, fun limiter_test_support:clock_now/0}]}
).

ensure_server() ->
    limiter_test_support:clock_init(),
    limiter_test_support:ensure_owner().

%% ===================================================================
%% 新 API 基础行为
%% ===================================================================

ready_and_zero_snapshot_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            ?assertEqual(true, login_attempt_ds:ready()),
            I = limiter_test_support:ident(zero),
            {ok, Snap} = login_attempt_ds:snapshot(I),
            ?assertMatch(
                #{
                    count := 0,
                    remaining := 5,
                    locked := false,
                    expires_at_ms := undefined,
                    rule_version := {5, 1800000}
                },
                Snap
            ),
            {ok, Snap2} = login_attempt_ds:check(I),
            ?assertEqual(Snap, Snap2),
            ?assertEqual(ok, login_attempt_ds:reset(I)),
            ?assertEqual(0, limiter_test_support:account_rows())
        end)}.

failure_counts_then_locks_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            I = limiter_test_support:ident(counts),
            application:set_env(imboy, login_max_attempts, 3),
            {ok, S1} = login_attempt_ds:failure(I),
            ?assertMatch(#{count := 1, remaining := 2, locked := false}, S1),
            {ok, S2} = login_attempt_ds:failure(I),
            ?assertMatch(#{count := 2, locked := false}, S2),
            %% 达阈值也记账（不以 locked 拒收在途失败）
            {ok, S3} = login_attempt_ds:failure(I),
            ?assertMatch(#{count := 3, locked := true, remaining := 0}, S3),
            %% check 只读不计数
            {ok, S4} = login_attempt_ds:check(I),
            ?assertMatch(#{count := 3, locked := true}, S4),
            ok = login_attempt_ds:reset(I),
            {ok, S5} = login_attempt_ds:snapshot(I),
            ?assertMatch(#{count := 0, locked := false, remaining := 3}, S5),
            ?assertEqual(0, limiter_test_support:account_rows())
        end)}.

invalid_identity_rejected_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(bad_shape)),
            ?assertEqual({error, unavailable}, login_attempt_ds:check({user, 0})),
            ?assertEqual(
                {error, unavailable},
                login_attempt_ds:check({unknown, other, <<"x">>})
            ),
            ?assertEqual({error, unavailable}, login_attempt_ds:failure({})),
            ?assertEqual({error, unavailable}, login_attempt_ds:reset(nope)),
            ?assertEqual({error, invalid_input}, login_attempt_ds:admin_unlock(1, not_a_uid)),
            ?assertEqual(
                {error, unavailable},
                login_attempt_ds:begin_attempt(bad, self(), default)
            )
        end)}.

admin_unlock_flow_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            I = limiter_test_support:ident(unlock),
            Old = application:get_env(imboy, admin_uids),
            application:set_env(imboy, admin_uids, [42]),
            _ = login_attempt_ds:failure(I),
            ?assertEqual({error, unauthorized}, login_attempt_ds:admin_unlock(43, I)),
            ?assertEqual({ok, unlocked}, login_attempt_ds:admin_unlock(42, I)),
            {ok, Snap} = login_attempt_ds:snapshot(I),
            ?assertMatch(#{count := 0}, Snap),
            ?assertEqual({error, invalid_input}, login_attempt_ds:admin_unlock(<<"x">>, I)),
            case Old of
                undefined -> application:unset_env(imboy, admin_uids);
                {ok, V} -> application:set_env(imboy, admin_uids, V)
            end,
            ok
        end)}.

%% ===================================================================
%% 兼容层（旧导出薄转发；A2/A3 重写调用者后移除）
%% ===================================================================

compat_record_failure_maps_identity_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            Id = <<"compat-user@t.test">>,
            ?assertEqual({ok, 1}, login_attempt_ds:record_failure(Id, <<"1.1.1.1">>)),
            %% 旧 Ip 参数已并入账户域：跨 IP 同一身份（C.1 过渡语义）
            ?assertEqual({ok, 1}, login_attempt_ds:get_attempts(Id, <<"9.9.9.9">>)),
            ?assertEqual(false, login_attempt_ds:is_locked(Id, <<"9.9.9.9">>)),
            ?assertEqual(ok, login_attempt_ds:reset(Id, <<"1.1.1.1">>)),
            ?assertEqual({ok, 0}, login_attempt_ds:get_attempts(Id, <<"1.1.1.1">>)),
            ?assertEqual(
                {ok, 5},
                login_attempt_ds:get_remaining_attempts(Id, <<"1.1.1.1">>)
            ),
            %% 非法输入保留旧语义
            ?assertEqual(
                {error, invalid_input},
                login_attempt_ds:record_failure(<<>>, <<"1.1.1.1">>)
            ),
            ?assertEqual(false, login_attempt_ds:is_locked(<<>>, <<"1.1.1.1">>))
        end)}.

compat_record_failure_locks_at_threshold_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            application:set_env(imboy, login_max_attempts, 2),
            Id = <<"compat-lock@t.test">>,
            {ok, 1} = login_attempt_ds:record_failure(Id, <<"1.1.1.1">>),
            {ok, 2} = login_attempt_ds:record_failure(Id, <<"1.1.1.1">>),
            ?assertEqual(true, login_attempt_ds:is_locked(Id, <<"2.2.2.2">>))
        end)}.

compat_is_locked_conservative_on_unavailable_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            %% degraded（AC-10 置位方式）后兼容 is_locked 保守返回 true；
            %% 容量 env 是 init 读取的，改后必须重启 owner 才生效
            application:set_env(imboy, limiter_max_account_entries, 1),
            limiter_test_support:restart_owner(),
            Filler = limiter_test_support:ident(fill),
            {ok, _} = login_attempt_ds:failure(Filler),
            Victim = limiter_test_support:ident(victim),
            ?assertEqual({error, capacity_lost}, login_attempt_ds:failure(Victim)),
            Id = <<"compat-degraded@t.test">>,
            ?assertEqual(true, login_attempt_ds:is_locked(Id, <<"1.1.1.1">>)),
            ?assertEqual(false, login_attempt_ds:is_locked(<<>>, <<"1.1.1.1">>))
        end)}.

compat_check_ip_rate_limit_forwards_oidc_test_() ->
    {timeout, 30,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            Ip = <<"203.0.113.9">>,
            ?assertEqual({ok, 1}, login_attempt_ds:check_ip_rate_limit(Ip)),
            ?assertEqual({ok, 2}, login_attempt_ds:check_ip_rate_limit(Ip)),
            ?assertEqual({error, invalid_ip}, login_attempt_ds:check_ip_rate_limit(<<>>))
        end)}.

compat_extract_real_ip_and_validate_ip_test_() ->
    ?_test(begin
        %% A3 迁移 elib_req 前保持原行为
        H = #{
            <<"cf-connecting-ip">> => <<"1.2.3.4">>,
            <<"x-real-ip">> => <<"5.6.7.8">>,
            <<"x-forwarded-for">> => <<"9.9.9.9, 10.0.0.1">>
        },
        {ok, <<"1.2.3.4">>} = login_attempt_ds:extract_real_ip(H),
        H2 = #{
            <<"x-real-ip">> => <<"5.6.7.8">>,
            <<"x-forwarded-for">> => <<"9.9.9.9, 10.0.0.1">>
        },
        {ok, <<"5.6.7.8">>} = login_attempt_ds:extract_real_ip(H2),
        H3 = #{<<"x-forwarded-for">> => <<"9.9.9.9, 10.0.0.1">>},
        {ok, <<"9.9.9.9">>} = login_attempt_ds:extract_real_ip(H3),
        {ok, <<"0.0.0.0">>} = login_attempt_ds:extract_real_ip(#{}),
        {ok, <<"::1">>} = login_attempt_ds:validate_ip(<<" ::1 ">>),
        {error, invalid_ip} = login_attempt_ds:validate_ip(<<"nope">>),
        ok
    end).

%% ===================================================================
%% AC-03（limiter_inflight_results）：F−1 初值、K 个 check 全获准后释放
%% K 个 failure → 最终 F−1+K、下一 check 锁定；迟到 reset/failure 两种
%% 提交顺序；跨窗 failure 计新窗。
%% ===================================================================

limiter_inflight_results_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            I = limiter_test_support:ident(ac03),
            lists:foreach(
                fun(C) ->
                    {ok, #{count := C}} = login_attempt_ds:failure(I)
                end,
                lists:seq(1, 4)
            ),
            %% K=3 个并发 check 全部获准（收齐全部结果才继续）
            {RelChk, ColChk} = limiter_test_support:run_parallel_gated(
                3, fun() -> login_attempt_ds:check(I) end
            ),
            RelChk(),
            ChkResults = ColChk(),
            ?assertEqual(3, length([1 || {ok, #{locked := false}} <- ChkResults])),
            %% 一次释放 K 个 failure（异步提交 + 收齐）
            {RelFail, ColFail} = limiter_test_support:run_parallel_gated(
                3, fun() -> login_attempt_ds:failure(I) end
            ),
            RelFail(),
            FailResults = ColFail(),
            %% 并发 worker 的结果收集顺序不代表 owner 的提交顺序。
            ?assertEqual([5, 6, 7], lists:sort([C || {ok, #{count := C}} <- FailResults])),
            {ok, _} = login_attempt_ds:check(I),
            %% 下一 check 锁定
            {ok, LockedSnap} = login_attempt_ds:check(I),
            ?assertMatch(#{count := 7, locked := true, remaining := 0}, LockedSnap),
            %% 迟到 reset 后 failure = 1
            ?assertEqual(ok, login_attempt_ds:reset(I)),
            {ok, #{count := 1}} = login_attempt_ds:failure(I),
            %% 迟到 failure 后 reset = 0
            {ok, #{count := 2}} = login_attempt_ds:failure(I),
            ?assertEqual(ok, login_attempt_ds:reset(I)),
            {ok, #{count := 0}} = login_attempt_ds:snapshot(I),
            %% 跨窗 failure 计新窗（提交时刻归属提交时窗，不回写旧窗）
            T0 = limiter_test_support:clock_now(),
            limiter_test_support:clock_set(T0 + 1800000),
            {ok, NewWin} = login_attempt_ds:failure(I),
            ?assertMatch(
                #{
                    count := 1,
                    locked := false,
                    remaining := 4,
                    rule_version := {5, 1800000}
                },
                NewWin
            ),
            ?assert(maps:get(expires_at_ms, NewWin) =:= T0 + 1800000 + 1800000),
            ok
        end)}.

%% ===================================================================
%% AC-04（limiter_reset_interleaving）：failure 与 reset 并发，强制两种
%% 提交顺序 → 最终只允许 0 或 1 且快照自洽。检测外部 delete 与旧值回写
%% （实现若保留"读本地计数+外部 delete 行"交错，顺序 a 会得到 5）。
%% ===================================================================

limiter_reset_interleaving_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            %% 顺序 a：failure 先提交、reset 后提交 → 0
            Ia = limiter_test_support:ident(ac04a),
            seed_four_failures(Ia),
            {FirstA, SecondA} = interleave_pair(Ia, fail_then_reset),
            ?assertMatch({ok, #{count := 5}}, FirstA),
            ?assertEqual(ok, SecondA),
            {ok, SnapA} = login_attempt_ds:snapshot(Ia),
            assert_consistent(SnapA, 0),
            %% 顺序 b：reset 先提交、failure 后提交 → 1
            Ib = limiter_test_support:ident(ac04b),
            seed_four_failures(Ib),
            {FirstB, SecondB} = interleave_pair(Ib, reset_then_fail),
            ?assertEqual(ok, FirstB),
            ?assertMatch({ok, #{count := 1}}, SecondB),
            {ok, SnapB} = login_attempt_ds:snapshot(Ib),
            assert_consistent(SnapB, 1),
            ok
        end)}.

seed_four_failures(I) ->
    lists:foreach(
        fun(N) ->
            {ok, #{count := N}} = login_attempt_ds:failure(I)
        end,
        lists:seq(1, 4)
    ).

%% @doc 一个 worker 发 failure、一个发 reset；按 Order 强制提交顺序，
%% 第一个 worker 的结果收齐后才放行第二个；全程 monitor 确认正常退出。
interleave_pair(I, Order) ->
    Parent = self(),
    F = spawn_monitor(fun() ->
        receive
            go -> Parent ! {fst, login_attempt_ds:failure(I)}
        end
    end),
    R = spawn_monitor(fun() ->
        receive
            go -> Parent ! {snd, login_attempt_ds:reset(I)}
        end
    end),
    {FPid, FMref} = F,
    {RPid, RMref} = R,
    {FirstTag, FirstPid, FirstMref, SecondTag, SecondPid, SecondMref} =
        case Order of
            fail_then_reset ->
                {fst, FPid, FMref, snd, RPid, RMref};
            reset_then_fail ->
                {snd, RPid, RMref, fst, FPid, FMref}
        end,
    Runner = fun() ->
        FirstPid ! go,
        First =
            receive
                {FirstTag, Res1} -> Res1
            after 15000 ->
                erlang:error({limiter_first_worker_timeout, Order})
            end,
        receive
            {'DOWN', FirstMref, process, FirstPid, normal} -> ok
        after 5000 ->
            erlang:error({limiter_first_worker_down, Order})
        end,
        SecondPid ! go,
        Second =
            receive
                {SecondTag, Res2} -> Res2
            after 15000 ->
                erlang:error({limiter_second_worker_timeout, Order})
            end,
        receive
            {'DOWN', SecondMref, process, SecondPid, normal} -> ok
        after 5000 ->
            erlang:error({limiter_second_worker_down, Order})
        end,
        {First, Second}
    end,
    Runner().

%% @doc 快照自洽：count ∈ {0,1}、remaining 互补、locked 与 count 一致。
assert_consistent(Snap, ExpectCount) ->
    ?assertEqual(ExpectCount, maps:get(count, Snap)),
    ?assertEqual(5 - ExpectCount, maps:get(remaining, Snap)),
    ?assertEqual(ExpectCount >= 5, maps:get(locked, Snap)),
    ok.
