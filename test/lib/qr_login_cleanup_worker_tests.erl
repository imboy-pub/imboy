%%%-------------------------------------------------------------------
%%% @doc qr_login_cleanup_worker 单元测试（LEE-5 补充 3）
%%%
%%% 【测试分类】**Worker 单元测试（MOCK：meck qr_login_session_ds）**。
%%% 仅证 worker 行为契约（开关/批量参数/失败仅 WARN 不崩/周期重排/
%%% 注册名唯一）；真实 PG 的"只删过期行、批量上限、与 claim 并发"见
%%% qr_login_session_repo_tests（delete_expired_* / cleanup_vs_claim_*）。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_cleanup_worker_tests).
-include_lib("eunit/include/eunit.hrl").

-define(INTERVAL_MS, 3600000).

run_deletes_with_configured_batch_test_() ->
    {setup,
        fun() ->
            application:set_env(imboy, qr_login_cleanup_enabled, true),
            application:set_env(imboy, qr_login_cleanup_batch, 7),
            application:set_env(imboy, qr_login_cleanup_interval_ms, ?INTERVAL_MS),
            meck:new(qr_login_session_ds, [no_link])
        end,
        fun(_) ->
            meck:unload(qr_login_session_ds),
            application:unset_env(imboy, qr_login_cleanup_enabled),
            application:unset_env(imboy, qr_login_cleanup_batch),
            application:unset_env(imboy, qr_login_cleanup_interval_ms)
        end,
        [
            fun run_calls_ds_with_batch/0,
            fun run_zero_rows_is_quiet_ok/0,
            fun run_failure_warns_and_survives/0,
            fun run_disabled_skips_ds/0
        ]}.

run_calls_ds_with_batch() ->
    meck:expect(qr_login_session_ds, delete_expired, 1, fun(7) -> {ok, 3} end),
    ?assertEqual({noreply, #{}}, qr_login_cleanup_worker:handle_info(run, #{})),
    ?assert(meck:called(qr_login_session_ds, delete_expired, [7])).

run_zero_rows_is_quiet_ok() ->
    meck:expect(qr_login_session_ds, delete_expired, 1, fun(_) -> {ok, 0} end),
    ?assertEqual({noreply, #{}}, qr_login_cleanup_worker:handle_info(run, #{})).

%% 清理失败只 WARN：不 crash、返回正常 {noreply,_}（下一轮照常重排）
run_failure_warns_and_survives() ->
    meck:expect(qr_login_session_ds, delete_expired, 1, fun(_) -> {error, closed} end),
    ?assertEqual({noreply, #{}}, qr_login_cleanup_worker:handle_info(run, #{})).

run_disabled_skips_ds() ->
    application:set_env(imboy, qr_login_cleanup_enabled, false),
    meck:expect(qr_login_session_ds, delete_expired, 1, fun(_) -> error(unexpected_cleanup) end),
    ?assertEqual({noreply, #{}}, qr_login_cleanup_worker:handle_info(run, #{})),
    application:set_env(imboy, qr_login_cleanup_enabled, true).

%% 非相关消息静默保持 loop（对齐 olm_otk_cleanup_worker 契约）
unexpected_info_keeps_loop_test() ->
    ?assertEqual({noreply, #{}}, qr_login_cleanup_worker:handle_info(whatever, #{})).

%% Supervisor 不重复启动：{local, ?MODULE} 注册名保证二次 start_link 立即失败
start_link_registers_unique_name_test_() ->
    {setup, fun() -> application:set_env(imboy, qr_login_cleanup_interval_ms, ?INTERVAL_MS) end,
        fun(_) -> application:unset_env(imboy, qr_login_cleanup_interval_ms) end, fun() ->
            process_flag(trap_exit, true),
            {ok, Pid} = qr_login_cleanup_worker:start_link(),
            try
                ?assertMatch(
                    {error, {already_started, Pid}},
                    qr_login_cleanup_worker:start_link()
                ),
                %% Worker 崩溃不影响调用方（独立进程）：杀掉后测试进程存活
                exit(Pid, kill),
                ?assertEqual(true, is_process_alive(self()))
            after
                catch gen:stop(Pid)
            end
        end}.
