-module(login_ip_atomic_tests).
-moduledoc "Limiter V1 admit 域原子性与并发语义测试（A1，AC-06/AC-11 补充）。".
%%%===================================================================
%%% 历史说明：本文件原名下曾验证 Cache get→set 竞态（deps/depcache）。
%%% Limiter V1 L2 后 admit 权威迁入 login_attempt_ds owner（ETS 单写者，
%%% gen_server 串行），以下用例验证新合同的并发正确性；imboy_cache 不再
%%% 是登录 IP 额度的权威（AC-07 反例见 login_attempt_limiter_ac_tests）。
%%%===================================================================
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(LOG_MOCK,
    {elib_log, [
        {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
    ]}
).
-define(CLOCK_MOCK,
    {login_attempt_clock, [{'now_ms', 0, fun limiter_test_support:clock_now/0}]}
).

ip_atomic_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(concurrent_counts_are_not_lost()) end,
        fun(_) -> ?_test(concurrent_limit_rejects_excess()) end,
        fun(_) -> ?_test(capacity_rejects_new_keys_not_existing()) end,
        fun(_) -> ?_test(compat_layer_routes_through_same_authority()) end
    ]}.

setup() ->
    %% foreach fixture 需自行 boot（幂等； WITH_MECK 路径之外的等价物）
    BootState = eunit_runner:eunit_setup(),
    ok = meck:new(elib_log, [no_link, non_strict]),
    ok = meck:expect(
        elib_log,
        internal_log,
        4,
        fun(_Level, _Msg, _Mod, _Line) -> ok end
    ),
    limiter_test_support:ensure_owner(),
    {BootState, ok}.

cleanup({BootState, _}) ->
    meck:unload(elib_log),
    limiter_test_support:clock_fini(),
    %% 恢复默认 env 供后续模块（下一个 ensure_owner 也会全量重设，双保险）
    maps:foreach(
        fun(K, V) -> application:set_env(imboy, K, V) end,
        limiter_test_support:default_env()
    ),
    eunit_runner:eunit_cleanup(BootState),
    ok.

%% 并发 admit 不丢数：N 个并发全允许、count 恰 N（单 owner 串行 + 原子
%% 判断占用；反例：get→set 读改写会 < N）。
concurrent_counts_are_not_lost() ->
    Scope = oidc_request,
    Ip = <<"c06_parallel_ip">>,
    Results = limiter_test_support:run_parallel(
        40, fun() -> login_attempt_ds:admit(Scope, Ip) end
    ),
    ?assertEqual(40, length([1 || {ok, _} <- Results])),
    Counts = [C || {ok, #{count := C}} <- Results],
    ?assertEqual(lists:seq(1, 40), lists:sort(Counts)),
    ?assertMatch(#{count := 40}, limiter_test_support:admit_row(Scope, Ip)).

%% 阈值内并发：恰 N 允许、余拒绝且不加（反例：N+1 饱和写会把拒绝也计数）。
concurrent_limit_rejects_excess() ->
    %% 默认 {10, 60000}
    Scope = passport_per_ip,
    Ip = <<"c06_limited_ip">>,
    Results = limiter_test_support:run_parallel(
        40, fun() -> login_attempt_ds:admit(Scope, Ip) end
    ),
    ?assertEqual(10, length([1 || {ok, _} <- Results])),
    ?assertEqual(30, length([1 || {error, rate_limited} <- Results])),
    AllowedCounts = [C || {ok, #{count := C}} <- Results],
    ?assertEqual(lists:seq(1, 10), lists:sort(AllowedCounts)),
    ?assertMatch(#{count := 10}, limiter_test_support:admit_row(Scope, Ip)).

%% 容量合同：新 Key 满 → capacity（与 rate_limited 区分）；已有 Key 正常
%% 处理；effective 状态不被淘汰（AC-11）。
capacity_rejects_new_keys_not_existing() ->
    application:set_env(imboy, limiter_max_ip_entries, 2),
    limiter_test_support:restart_owner(),
    Scope = passport_per_ip,
    ?assertMatch({ok, _}, login_attempt_ds:admit(Scope, <<"c06_cap_1">>)),
    ?assertMatch({ok, _}, login_attempt_ds:admit(Scope, <<"c06_cap_2">>)),
    ?assertEqual({error, capacity}, login_attempt_ds:admit(Scope, <<"c06_cap_3">>)),
    %% 已有 Key 仍可继续计数（不淘汰、不拒已有 Key）
    ?assertMatch({ok, #{count := 2}}, login_attempt_ds:admit(Scope, <<"c06_cap_1">>)),
    ?assertMatch({ok, #{count := 2}}, login_attempt_ds:admit(Scope, <<"c06_cap_2">>)),
    %% 新 Key 在腾位前仍拒绝
    ?assertEqual({error, capacity}, login_attempt_ds:admit(Scope, <<"c06_cap_3">>)),
    ok.

%% 兼容层与新 API 同一权威：check_ip_rate_limit（oidc_request 转发）的
%% 计数与 admit 直接调用共享同一行。
compat_layer_routes_through_same_authority() ->
    Scope = oidc_request,
    Ip = <<"c06_compat_ip">>,
    {ok, 1} = login_attempt_ds:check_ip_rate_limit(Ip),
    ?assertMatch({ok, #{count := 2}}, login_attempt_ds:admit(Scope, Ip)),
    {ok, 3} = login_attempt_ds:check_ip_rate_limit(Ip),
    ?assertMatch(#{count := 3}, limiter_test_support:admit_row(Scope, Ip)),
    ok.
