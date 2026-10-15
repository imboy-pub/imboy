-module(limiter_test_support).
-moduledoc "Limiter V1 DS 测试支撑（A1）：owner 装配、时钟注入、并发收齐。".
%%%===================================================================
%%% 测试纪律（计划 §9.2 AC 执行合同）：
%%% - 时钟是唯一可注入 seam（login_attempt_clock:now_ms/0，meck）；
%%%   D 与实际存储（ETS）不被 mock，测试进程只读 protected 表。
%%% - 并发用例 spawn_monitor 收齐全部参与者结果 + 正常 DOWN，禁止 sleep。
%%% - 每 run 唯一身份串（unique_integer），finally 恢复 owner/env/表。
%%%===================================================================

-export([
    default_env/0,
    ensure_owner/0,
    restart_owner/0,
    detach_owner/0,
    attach_owner/0,
    await_owner/2,
    clock_init/0,
    clock_set/1,
    clock_now/0,
    clock_fini/0,
    ident/1,
    run_parallel/2,
    run_parallel_gated/2,
    collect_workers/2,
    account_rows/0,
    admit_row/2,
    send_cleanup/0,
    send_deadline_check/0,
    fence/0,
    wait_until/2
]).

-define(ETS, login_attempt_ets).

%% ------------------------------------------------------------------
%% owner 装配
%% ------------------------------------------------------------------

%% @doc C.5 默认测试环境；ensure_owner 每次全量重设（测试内改动 env 后
%% 由下一个 ensure_owner 兜底，减少互相污染）。
-spec default_env() -> #{atom() => term()}.
default_env() ->
    #{
        login_max_attempts => 5,
        login_lock_duration_minutes => 30,
        limiter_ip_rates =>
            #{
                passport_per_ip => {10, 60000},
                api_per_ip => {60, 60000},
                oidc_request => {100, 3600000}
            },
        limiter_max_account_entries => 200000,
        limiter_max_ip_entries => 200000,
        limiter_max_key_bytes => 256,
        limiter_cleanup_batch => 1000,
        limiter_auth_deadline_ms => 10000
    }.

%% @doc 重设默认 env 并重启 owner（init 读取 limiter_ip_rates，env 变更后
%% 必须重启才生效）。
-spec ensure_owner() -> ok.
ensure_owner() ->
    maps:foreach(fun(K, V) -> application:set_env(imboy, K, V) end, default_env()),
    restart_owner().

%% @doc kill 现 owner 并等待 supervisor 重建的新 owner ready。
-spec restart_owner() -> ok.
restart_owner() ->
    Old = whereis(login_attempt_ds),
    case Old of
        undefined ->
            ok;
        _ ->
            Mref = erlang:monitor(process, Old),
            exit(Old, kill),
            receive
                {'DOWN', Mref, process, Old, killed} -> ok
            after 10000 ->
                erlang:error(limiter_owner_kill_timeout)
            end
    end,
    await_owner(Old, 300),
    ok = login_attempt_ds:activate_empty(whereis(login_attempt_ds)),
    true = login_attempt_ds:ready(),
    ok.

%% @doc 摘除 owner（owner 不存在的稳定态，AC-12）：terminate_child 显式
%% 停止后 supervisor 不会自动重启 permanent child（child spec 保留），
%% attach 用 restart_child 恢复。不 delete_child（会丢 spec，无法恢复）。
-spec detach_owner() -> ok.
detach_owner() ->
    case whereis(login_attempt_ds) of
        undefined ->
            ok;
        Pid ->
            Mref = erlang:monitor(process, Pid),
            ok = supervisor:terminate_child(imboy_sup, login_attempt_ds),
            receive
                {'DOWN', Mref, process, Pid, _} -> ok
            after 10000 ->
                erlang:error(limiter_owner_terminate_timeout)
            end
    end.

%% @doc 恢复被 detach 的 owner（finally）。
-spec attach_owner() -> ok.
attach_owner() ->
    case whereis(login_attempt_ds) of
        undefined ->
            {ok, _} = supervisor:restart_child(imboy_sup, login_attempt_ds),
            await_owner(undefined, 300),
            ok = login_attempt_ds:activate_empty(whereis(login_attempt_ds)),
            true = login_attempt_ds:ready();
        _ ->
            ok
    end,
    ok.

%% @private 轮询等待注册名出现且异于 OldPid。
await_owner(OldPid, 0) ->
    erlang:error({limiter_owner_not_restarted, OldPid});
await_owner(OldPid, N) ->
    case whereis(login_attempt_ds) of
        P when is_pid(P), P =/= OldPid ->
            %% 同步确认 init 完成；就绪批准由显式测试装配完成。
            _ = sys:get_state(P),
            P;
        _ ->
            timer:sleep(10),
            await_owner(OldPid, N - 1)
    end.

%% ------------------------------------------------------------------
%% 时钟注入（唯一 seam）
%% ------------------------------------------------------------------

%% @doc 建虚拟时钟表并置初值 1000；配合 ?WITH_MECK 的
%% {login_attempt_clock, [{'now_ms', 0, fun ?MODULE:clock_now/0}]}。
-spec clock_init() -> ok.
clock_init() ->
    case ets:info(limiter_test_clock) of
        undefined ->
            _ = ets:new(limiter_test_clock, [named_table, set, public]);
        _ ->
            ok
    end,
    clock_set(1000),
    ok.

-spec clock_set(integer()) -> ok.
clock_set(T) ->
    ets:insert(limiter_test_clock, {now, T}),
    ok.

-spec clock_now() -> integer().
clock_now() ->
    ets:lookup_element(limiter_test_clock, now, 2).

-spec clock_fini() -> ok.
clock_fini() ->
    case ets:info(limiter_test_clock) of
        undefined -> ok;
        _ -> ets:delete(limiter_test_clock)
    end,
    ok.

%% ------------------------------------------------------------------
%% 身份与并发
%% ------------------------------------------------------------------

%% @doc 每 run 唯一账户身份（unknown/account，C.1 过渡语义）。
-spec ident(term()) -> {unknown, account, binary()}.
ident(Tag) ->
    {unknown, account,
        <<"limiter-", (integer_to_binary(erlang:unique_integer([positive])))/binary, "-",
            (to_b(Tag))/binary>>}.

to_b(T) when is_binary(T) -> T;
to_b(T) -> integer_to_binary(erlang:phash2(T)).

%% @doc N 个 worker 收 go 后并发执行 Fun，收齐全部结果与正常 DOWN。
%% 返回 {Release, Collect}。
-spec run_parallel_gated(pos_integer(), fun(() -> term())) ->
    {fun(() -> ok), fun(() -> [term()])}.
run_parallel_gated(N, Fun) ->
    Parent = self(),
    Workers = [
        spawn_monitor(fun() ->
            receive
                go -> Parent ! {self(), Fun()}
            end
        end)
     || _ <- lists:seq(1, N)
    ],
    Release = fun() ->
        [P ! go || {P, _} <- Workers],
        ok
    end,
    Collect = fun() -> collect_workers(Workers, 15000) end,
    {Release, Collect}.

%% @doc 立即并发的 N worker（无 gate）。
-spec run_parallel(pos_integer(), fun(() -> term())) -> [term()].
run_parallel(N, Fun) ->
    {Release, Collect} = run_parallel_gated(N, Fun),
    Release(),
    Collect().

%% @doc 收齐 worker 结果（15s 无结果即失败，不 sleep 等待）。
-spec collect_workers([{pid(), reference()}], timeout()) -> [term()].
collect_workers(Workers, Timeout) ->
    Results = [
        receive
            {P, R} -> R
        after Timeout ->
            erlang:error({limiter_worker_result_timeout, P})
        end
     || {P, _M} <- Workers
    ],
    [
        receive
            {'DOWN', M, process, P, normal} -> ok
        after 5000 ->
            erlang:error({limiter_worker_abnormal_down, P})
        end
     || {P, M} <- Workers
    ],
    Results.

%% ------------------------------------------------------------------
%% 只读观测（protected 表允许读；写仍由 owner 唯一执行）
%% ------------------------------------------------------------------

-spec account_rows() -> non_neg_integer().
account_rows() ->
    ets:select_count(?ETS, [{{{limiter_account, '_'}, '_'}, [], [true]}]).

-spec admit_row(atom(), binary()) -> map() | undefined.
admit_row(Scope, Key) ->
    case ets:lookup(?ETS, {limiter_admit, Scope, Key}) of
        [{_, Row}] -> Row;
        [] -> undefined
    end.

%% @doc 直发 cleanup/deadline 检查消息（不依赖真实 timer 周期，确定性），
%% 并以同步 call 作栅栏确认 owner 已处理完该消息（gen_server 串行）。
send_cleanup() ->
    whereis(login_attempt_ds) ! cleanup_expired,
    fence().

send_deadline_check() ->
    whereis(login_attempt_ds) ! deadline_check,
    fence().

%% @doc 同步栅栏：ready/0 是 gen_server:call，返回时先到的消息均已处理。
fence() ->
    true = login_attempt_ds:ready(),
    ok.

%% @doc 有界轮询等待跨进程确认（DOWN 等异步信号被 owner 处理）；超时失败。
%% 这是完成确认机制本身，不代替任何同步点。
wait_until(_Pred, 0) ->
    erlang:error(limiter_wait_until_timeout);
wait_until(Pred, N) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait_until(Pred, N - 1)
    end.
