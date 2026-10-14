-module(login_attempt_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

ensure_server() ->
    case whereis(login_attempt_ds) of
        undefined ->
            {ok, _Pid} = login_attempt_ds:start_link(),
            ok;
        _Pid ->
            ok
    end,
    ets:delete_all_objects(login_attempt_ets),
    application:set_env(imboy, login_max_attempts, 5),
    application:set_env(imboy, login_lock_duration_minutes, 30),
    ok.

record_failure_and_reset_flow_test_() ->
    ?WITH_MECK(
        elib_log,
        [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) ->
                ok
            end}
        ],
        fun() ->
            ok = ensure_server(),
            ?assertEqual(
                {ok, 1}, login_attempt_ds:record_failure(<<"user@test.com">>, <<"127.0.0.1">>)
            ),
            ?assertEqual(
                {ok, 1}, login_attempt_ds:get_attempts(<<"user@test.com">>, <<"127.0.0.1">>)
            ),
            ?assertEqual(
                {ok, 4},
                login_attempt_ds:get_remaining_attempts(<<"user@test.com">>, <<"127.0.0.1">>)
            ),
            ?assertEqual(ok, login_attempt_ds:reset(<<"user@test.com">>, <<"127.0.0.1">>)),
            ?assertEqual(
                {ok, 0}, login_attempt_ds:get_attempts(<<"user@test.com">>, <<"127.0.0.1">>)
            )
        end
    ).

record_failure_invalid_input_returns_error_test_() ->
    ?WITH_MECK(
        elib_log,
        [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) ->
                ok
            end}
        ],
        fun() ->
            ?assertEqual(
                {error, invalid_input}, login_attempt_ds:record_failure(<<>>, <<"127.0.0.1">>)
            ),
            ?assertEqual(
                {error, invalid_input}, login_attempt_ds:record_failure(<<"user@test.com">>, <<>>)
            )
        end
    ).

is_locked_after_reaching_limit_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) ->
                    ok
                end}
            ]},
            {elib_dt, [
                {'now', 0, fun() ->
                    1000
                end},
                {'add', 2, fun(1000, {30, minute}) ->
                    2000
                end}
            ]}
        ],
        fun() ->
            ok = ensure_server(),
            application:set_env(imboy, login_max_attempts, 2),
            ?assertEqual(
                {ok, 1}, login_attempt_ds:record_failure(<<"user@test.com">>, <<"127.0.0.1">>)
            ),
            ?assertEqual(
                {ok, 2}, login_attempt_ds:record_failure(<<"user@test.com">>, <<"127.0.0.1">>)
            ),
            ?assertEqual(true, login_attempt_ds:is_locked(<<"user@test.com">>, <<"127.0.0.1">>))
        end
    ).

cache_key_returns_tuple_test() ->
    ?assertEqual(
        {login_attempt, <<"user@test.com">>, <<"127.0.0.1">>},
        login_attempt_ds:cache_key(<<"user@test.com">>, <<"127.0.0.1">>)
    ).

check_ip_rate_limit_success_test_() ->
    ?WITH_MECK(
        imboy_cache,
        [
            {'get', 1, fun({login_attempt_ip_limit, <<"127.0.0.1">>}) ->
                undefined
            end},
            {'set', 3, fun({login_attempt_ip_limit, <<"127.0.0.1">>}, 1, 3600) ->
                ok
            end}
        ],
        fun() ->
            ok = ensure_server(),
            ?assertEqual({ok, 1}, login_attempt_ds:check_ip_rate_limit(<<"127.0.0.1">>))
        end
    ).

%% ===================================================================
%% Limiter V1 L0 缺陷复现（计划 docs/plans/2026-10-13-limiter-v1-*
%% §5/§9.2）。oracle 说明：以下用例断言**旧实现的坏结果**（L0 缺陷
%% 复现成功 = 测试通过），不是产品行为 PASS。L2/L3 实施权威状态改造
%% 时按对应 AC 翻转 oracle（计划 §9.1：L0 复现测试与修复后 oracle
%% 分开）。
%% ===================================================================

-define(L0_NOW_TS, <<"2026-10-14T00:00:00Z">>).
-define(L0_LOCKED_TS, <<"2026-10-14T00:30:00Z">>).
-define(L0_TEST_ID, <<"limiter-l0@e2e.test">>).
-define(L0_TEST_IP, <<"198.51.100.7">>).

mock_l0_log() ->
    {elib_log, [
        {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end}
    ]}.

%% AC-04（F4）旧侧复现：record_failure 在 gen_server 内做
%% lookup→（本地计数）→insert，而 reset/2 是外部进程直接 ets:delete。
%% 允许时序：failure 读到 count=4 → reset 删行 → failure 用本地变量写回 5。
%% 任何完整串行顺序只会得到 0 或 1；5 即违反串行合同的目标非法状态。
%%
%% 调度控制点：meck elib_dt:add/2（D 进程在 lookup 之后、insert 之前调用
%% 它换算锁定到期时间）。计数链本身（lookup/insert/delete）全部是真实
%% D 代码，不 mock。起点状态按计划以 ETS fixture 预置 count=4（真实
%% record_failure 因空 IP 断链写不进 {Account,<<>>} 键之外的历史负担与
%% 本用例无关；预置保证探针触发的确定性，不依赖调用计数巧合）。
limiter_reset_interleaving_test_() ->
    {timeout, 60,
        ?WITH_MECKS(
            [
                mock_l0_log(),
                {elib_dt, [
                    {'now', 0, fun() -> ?L0_NOW_TS end},
                    {'add', 2, fun(_From, _Dur) -> ?L0_LOCKED_TS end}
                ]}
            ],
            fun() ->
                ok = ensure_server(),
                TestPid = self(),
                %% 预置"已 4 次失败"的起点（键即本用例计数键）
                ets:insert(
                    login_attempt_ets,
                    {
                        login_attempt_ds:cache_key(?L0_TEST_ID, ?L0_TEST_IP),
                        #{<<"count">> => 4, <<"first_fail_at">> => ?L0_NOW_TS}
                    }
                ),
                %% 探针体在 D 的 gen_server 进程内执行（elib_dt:add 的调用
                %% 方）：D 在 lookup 之后、insert 之前调用 add。第一次命中
                %% 即本用例的 failure（ETS 只有本键一行、max_attempts=5
                %% 之下不会有其它 add 来源）。
                ok = meck:expect(elib_dt, add, fun
                    (_From, {30, minute}) ->
                        case self() =:= whereis(login_attempt_ds) of
                            true ->
                                %% failure 已完成 lookup（count=4 在本地变量
                                %% 中）：放测试进程执行 reset，再放行。
                                TestPid ! {l0_probe_armed, self()},
                                receive
                                    l0_go_after_reset -> ok
                                after 10000 ->
                                    ok
                                end;
                            false ->
                                ok
                        end,
                        ?L0_LOCKED_TS;
                    (_From, _Dur) ->
                        %% cleanup 的负分钟等其它调用：直接给可比时间
                        ?L0_LOCKED_TS
                end),
                %% 本次 failure 从独立进程发起，D 将停在探针处
                Caller = spawn(fun() ->
                    login_attempt_ds:record_failure(?L0_TEST_ID, ?L0_TEST_IP)
                end),
                DsPid =
                    receive
                        {l0_probe_armed, P} -> P
                    after 10000 ->
                        erlang:error(l0_probe_not_armed)
                    end,
                %% reset 是外部直接 ETS 删除（真实生产代码路径），此刻 D 忙于
                %% 探针，reset 可完成 → 行被删
                ok = login_attempt_ds:reset(?L0_TEST_ID, ?L0_TEST_IP),
                ?assertEqual({ok, 0}, login_attempt_ds:get_attempts(?L0_TEST_ID, ?L0_TEST_IP)),
                %% 放行 failure：D 用本地 count=4 计算出 5 写回
                DsPid ! l0_go_after_reset,
                Mref = erlang:monitor(process, Caller),
                receive
                    {'DOWN', Mref, process, Caller, _Reason} -> ok
                after 10000 ->
                    erlang:error(l0_caller_stuck)
                end,
                %% L0 旧侧 oracle：最终 5 —— 任何串行顺序都不可达的状态
                ?assertEqual({ok, 5}, login_attempt_ds:get_attempts(?L0_TEST_ID, ?L0_TEST_IP)),
                ok
            end
        )}.

%% AC-07（F5）旧侧复现：IP 额度权威在 depcache（可淘汰/可 flush 的普通
%% 缓存）。depcache 清理进程按旧 meta 判定过期后 cast(owner, {flush,Key})，
%% owner 的 handle_cast({flush,Key}) 不比较 serial 直接删——迟到消息会删掉
%% 刚刚续期的新状态。真实 imboy_cache + 真实 depcache，不 mock。
limiter_cache_old_flush_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            [mock_l0_log()],
            fun() ->
                ok = ensure_server(),
                Key = {login_attempt_ip_limit, ?L0_TEST_IP},
                %% 重启 D 清掉其进程 depcache 字典副本（历史运行残留）
                ok = l0_restart_owner(),
                _ = imboy_cache:flush(Key),
                %% 新额度状态建立（真实 check_ip_rate_limit → depcache）
                {ok, 1} = login_attempt_ds:check_ip_rate_limit(?L0_TEST_IP),
                {ok, 2} = login_attempt_ds:check_ip_rate_limit(?L0_TEST_IP),
                %% 测试进程无进程字典副本，读到 server 权威表
                ?assertEqual({ok, 2}, imboy_cache:get(Key)),
                %% 模拟"清理进程基于旧过期 meta 已生成、迟到递送"的 flush
                %% 消息——与 depcache:flush_expired 的 gen_server:cast(Pid,
                %% {flush,Key}) 同构（owner 侧无 serial 版本比较）
                gen_server:cast(imboy_cache, {flush, Key}),
                %% 同步确认该 cast 已处理：对无害键做一次同步 flush call
                _ = depcache:flush({l0_sync_probe, make_ref()}, imboy_cache),
                %% L0 旧侧 oracle：有效新额度被迟到 flush 删除 → undefined
                ?assertEqual(undefined, imboy_cache:get(Key)),
                %% 清理：重启 D 清其进程字典副本，再清 server 残留
                ok = l0_restart_owner(),
                _ = imboy_cache:flush(Key),
                ok
            end
        )}.

%% F5 并发丢更新：计划取证 HEAD（0a5ade3c）时 check_ip_rate_limit 在调用
%% 进程做 get→set；基线 417ba55f 已改为 gen_server:call 串行化（D 模块头
%% 注释与 handle_call({check_ip_rate_limit,...})）。本用例如实记录该修复
%% 事实：并发 N 个 check_ip_rate_limit 不丢数（权威仍是非原子的 Cache 值，
%% 但读改写已串行）。AC-06 的旧侧"get/set 丢数"在此基线不可复现。
limiter_ip_rate_serialized_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            [mock_l0_log()],
            fun() ->
                ok = ensure_server(),
                Key = {login_attempt_ip_limit, ?L0_TEST_IP},
                _ = imboy_cache:flush(Key),
                N = 64,
                Parent = self(),
                Workers = [
                    spawn_monitor(fun() ->
                        {ok, _} = login_attempt_ds:check_ip_rate_limit(?L0_TEST_IP),
                        Parent ! {self(), done}
                    end)
                 || _ <- lists:seq(1, N)
                ],
                [
                    receive
                        {Pid, done} -> ok
                    end
                 || {Pid, _} <- Workers
                ],
                [
                    receive
                        {'DOWN', M, process, P, normal} -> ok
                    end
                 || {P, M} <- Workers
                ],
                %% 串行化修复后：恰 N 次（旧 get→set 并发实现会 < N）
                ?assertEqual({ok, N}, imboy_cache:get(Key)),
                _ = imboy_cache:flush(Key),
                ok
            end
        )}.

%% F8 容量合同缺失：账户失败表没有条目数上限，也没有 Key 长度/数量拒绝
%% 机制。本用例以有限规模（500 个合成身份）证明"无上限接受"这一事实
%% 本身；不是压力测试。AC-11 的 L2 目标（有限容量 + degraded 拒绝）在
%% 当前基线不存在。
limiter_capacity_unbounded_test_() ->
    ?WITH_MECK(
        elib_log,
        [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end}
        ],
        fun() ->
            ok = ensure_server(),
            N = 500,
            %% 全部被接受，无任何容量拒绝
            AllAccepted =
                lists:all(
                    fun(I) ->
                        Id = <<"limiter-l0-cap-", (integer_to_binary(I))/binary>>,
                        {ok, 1} =:= login_attempt_ds:record_failure(Id, ?L0_TEST_IP)
                    end,
                    lists:seq(1, N)
                ),
            ?assert(AllAccepted),
            ?assertEqual(N, ets:info(login_attempt_ets, size)),
            ets:delete_all_objects(login_attempt_ets),
            ok
        end
    ).

%% F8 重启/owner 合同缺失：ETS 由 D 进程持有且无 heir。owner 死亡即丢表；
%% 重启后为空态，已有锁定全部消失（本地易失，无受控恢复合同）。AC-13 的
%% L2/L4 目标（重启不自动无限放行/批准后受控空态）以此为旧侧事实。
%% owner 死亡窗口内 public 表消失、读接口 badarg（而非"无记录"的 0）由
%% 源码直接证明（is_locked/get_attempts 对 named_table 裸 ets:lookup），
%% 不以竞态断言动态化。
limiter_owner_death_loses_state_test_() ->
    {timeout, 30,
        ?WITH_MECK(
            elib_log,
            [
                {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end}
            ],
            fun() ->
                ok = ensure_server(),
                application:set_env(imboy, login_max_attempts, 3),
                %% 真实累积至锁定
                [
                    ?assertEqual({ok, I}, login_attempt_ds:record_failure(?L0_TEST_ID, ?L0_TEST_IP))
                 || I <- lists:seq(1, 3)
                ],
                ?assertEqual(true, login_attempt_ds:is_locked(?L0_TEST_ID, ?L0_TEST_IP)),
                OldPid = whereis(login_attempt_ds),
                Mref = erlang:monitor(process, OldPid),
                exit(OldPid, kill),
                receive
                    {'DOWN', Mref, process, OldPid, killed} -> ok
                after 10000 ->
                    erlang:error(l0_owner_not_dead)
                end,
                %% 等待新实例就绪（imboy_sup permanent 重启或 ensure_server 路径）
                NewPid = l0_await_owner(OldPid, 100),
                true = is_pid(NewPid),
                %% L0 旧侧 oracle：重启后空态——3 次失败与锁定全部丢失
                ?assertEqual({ok, 0}, login_attempt_ds:get_attempts(?L0_TEST_ID, ?L0_TEST_IP)),
                ?assertEqual(false, login_attempt_ds:is_locked(?L0_TEST_ID, ?L0_TEST_IP)),
                ets:delete_all_objects(login_attempt_ets),
                application:set_env(imboy, login_max_attempts, 5),
                ok
            end
        )}.

%% @private 有界等待 D 的新实例（owner 换人且表重建）
l0_await_owner(OldPid, 0) ->
    erlang:error({l0_owner_not_restarted, OldPid});
l0_await_owner(OldPid, N) ->
    case whereis(login_attempt_ds) of
        Pid when is_pid(Pid), Pid =/= OldPid ->
            case ets:info(login_attempt_ets) of
                undefined ->
                    %% 进程已注册但表尚未建完，再等一拍
                    timer:sleep(10),
                    l0_await_owner(OldPid, N - 1);
                _ ->
                    Pid
            end;
        _ ->
            timer:sleep(10),
            l0_await_owner(OldPid, N - 1)
    end.

%% @private 重启 D（kill + 等 imboy_sup permanent 重建）。同时用于清空 D
%% 进程的 depcache 进程字典副本（check_ip_rate_limit 在 D 进程内读写缓存，
%% 其字典副本无法从外部清除）。
l0_restart_owner() ->
    OldPid = whereis(login_attempt_ds),
    Mref = erlang:monitor(process, OldPid),
    exit(OldPid, kill),
    receive
        {'DOWN', Mref, process, OldPid, killed} -> ok
    after 10000 ->
        erlang:error(l0_owner_kill_timeout)
    end,
    _ = l0_await_owner(OldPid, 100),
    ok.
