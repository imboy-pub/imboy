-module(login_attempt_limiter_ac_tests).
-moduledoc "Limiter V1 DS 层 AC 扩展（A1）：AC-06/07/08/10/11/12/13。".
%%%===================================================================
%%% 计划 §9.2 AC 表 DS 部分的目标实现测试。oracle 为新合同正确行为；
%%% 旧缺陷复现（L0）已在 docs/qa/limiter-v1/ 归档。
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

ensure_server() ->
    limiter_test_support:clock_init(),
    limiter_test_support:ensure_owner().

%% ===================================================================
%% AC-06（limiter_admit_race）：当前剩 R、K 个同 IP 并发 admit → 恰
%% min(R,K) 允许、count 停 N、拒绝不加、不退款。
%% ===================================================================

limiter_admit_race_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            Ip = <<"198.51.100.10">>,
            %% N=10
            Scope = passport_per_ip,
            %% 预占 3 → 剩 R=7
            lists:foreach(
                fun(C) ->
                    {ok, #{count := C}} = login_attempt_ds:admit(Scope, Ip)
                end,
                lists:seq(1, 3)
            ),
            %% K=8 并发 admit 同 IP：恰 min(7,8)=7 允许、1 拒绝、count 停 N
            {Release, Collect} = limiter_test_support:run_parallel_gated(
                8, fun() -> login_attempt_ds:admit(Scope, Ip) end
            ),
            Release(),
            Results = Collect(),
            Allowed = [C || {ok, #{count := C}} <- Results],
            ?assertEqual(7, length(Allowed)),
            ?assertEqual(1, length([1 || {error, rate_limited} <- Results])),
            ?assertEqual(lists:seq(4, 10), lists:sort(Allowed)),
            ?assertMatch(#{count := 10}, limiter_test_support:admit_row(Scope, Ip)),
            %% 拒绝不加、不退款：再 admit 仍拒，count 不变
            ?assertEqual({error, rate_limited}, login_attempt_ds:admit(Scope, Ip)),
            ?assertMatch(#{count := 10}, limiter_test_support:admit_row(Scope, Ip)),
            ok
        end)}.

limiter_admit_race_at_threshold_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            Ip = <<"198.51.100.11">>,
            %% N=60
            Scope = api_per_ip,
            lists:foreach(
                fun(C) ->
                    {ok, #{count := C}} = login_attempt_ds:admit(Scope, Ip)
                end,
                lists:seq(1, 59)
            ),
            %% 剩 R=1，K=16 并发 → 恰 1 允许、15 拒绝；count 停 N=60
            {Release, Collect} = limiter_test_support:run_parallel_gated(
                16, fun() -> login_attempt_ds:admit(Scope, Ip) end
            ),
            Release(),
            Results = Collect(),
            ?assertEqual(1, length([1 || {ok, _} <- Results])),
            ?assertEqual(15, length([1 || {error, rate_limited} <- Results])),
            ?assertMatch(#{count := 60}, limiter_test_support:admit_row(Scope, Ip)),
            ok
        end)}.

%% ===================================================================
%% AC-07（limiter_cache_old_flush）：迟到 cleanup / Cache flush 不得改变
%% 权威状态；cleanup 只删逻辑过期行；下一 admit/failure 按新 count 判断。
%% （反例：实现若仍以 Cache 为权威，flush 会清掉有效额度。）
%% ===================================================================

limiter_cache_old_flush_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            T0 = limiter_test_support:clock_now(),
            I = limiter_test_support:ident(ac07),
            {ok, _} = login_attempt_ds:failure(I),
            {ok, #{count := 2}} = login_attempt_ds:failure(I),
            %% 真实 Cache flush（旧权威所在处）不影响 ETS 权威
            _ = imboy_cache:flush({login_attempt_ip_limit, <<"9.9.9.9">>}),
            limiter_test_support:fence(),
            ?assertMatch({ok, #{count := 2}}, login_attempt_ds:snapshot(I)),
            %% 未过期的 cleanup 不删有效行
            limiter_test_support:send_cleanup(),
            ?assertMatch({ok, #{count := 2}}, login_attempt_ds:snapshot(I)),
            %% 逻辑过期后：快照视 0，cleanup 删行，failure 计新窗
            limiter_test_support:clock_set(T0 + 1800000),
            ?assertMatch(
                {ok, #{count := 0, locked := false}},
                login_attempt_ds:snapshot(I)
            ),
            limiter_test_support:send_cleanup(),
            ?assertEqual(0, limiter_test_support:account_rows()),
            {ok, NewWin} = login_attempt_ds:failure(I),
            ?assertMatch(#{count := 1}, NewWin),
            ?assertEqual(T0 + 3600000, maps:get(expires_at_ms, NewWin)),
            ok
        end)}.

limiter_cache_old_flush_admit_window_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            %% = 1000（窗 0 内）
            T0 = limiter_test_support:clock_now(),
            limiter_test_support:clock_set(55000),
            Ip = <<"198.51.100.12">>,
            Scope = passport_per_ip,
            {ok, #{count := 1, expires_at_ms := 60000}} = login_attempt_ds:admit(Scope, Ip),
            {ok, #{count := 2}} = login_attempt_ds:admit(Scope, Ip),
            %% Cache flush 与未过期 cleanup 都不动有效额度
            _ = imboy_cache:flush({login_attempt_ip_limit, Ip}),
            limiter_test_support:send_cleanup(),
            ?assertMatch(#{count := 2}, limiter_test_support:admit_row(Scope, Ip)),
            %% 跨边界后：旧行逻辑过期，cleanup 只删过期行；下一 admit 按
            %% 新窗 count 判断（重置 1 起算，不是累计 3）
            limiter_test_support:clock_set(T0 + 60000),
            limiter_test_support:send_cleanup(),
            ?assertEqual(undefined, limiter_test_support:admit_row(Scope, Ip)),
            {ok, New} = login_attempt_ds:admit(Scope, Ip),
            ?assertMatch(#{count := 1}, New),
            ?assertEqual(120000, maps:get(expires_at_ms, New)),
            ok
        end)}.

%% ===================================================================
%% AC-08（limiter_time_rules）：expiry −1/=/+1 边界；HTTP t0=0 对齐换窗
%% （55000 → 60000 换窗，不是 115000）；账户/OIDC 窗口冻结；HTTP pending
%% 边界切换；时钟注入推进三周期。
%% ===================================================================

limiter_time_rules_expiry_boundary_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            application:set_env(imboy, login_max_attempts, 2),
            I = limiter_test_support:ident(ac08exp),
            T0 = limiter_test_support:clock_now(),
            {ok, _} = login_attempt_ds:failure(I),
            {ok, Locked} = login_attempt_ds:failure(I),
            ?assertMatch(#{locked := true}, Locked),
            ?assertEqual(T0 + 1800000, maps:get(expires_at_ms, Locked)),
            %% expiry − 1：窗口仍活，锁定有效
            limiter_test_support:clock_set(T0 + 1800000 - 1),
            ?assertMatch({ok, #{count := 2, locked := true}}, login_attempt_ds:check(I)),
            %% expiry：逻辑过期视 0
            limiter_test_support:clock_set(T0 + 1800000),
            ?assertMatch(
                {ok, #{count := 0, locked := false, expires_at_ms := undefined}},
                login_attempt_ds:check(I)
            ),
            %% expiry + 1：failure 计新窗
            limiter_test_support:clock_set(T0 + 1800000 + 1),
            {ok, NewWin} = login_attempt_ds:failure(I),
            ?assertMatch(#{count := 1}, NewWin),
            ?assertEqual(T0 + 3600000 + 1, maps:get(expires_at_ms, NewWin)),
            ok
        end)}.

limiter_time_rules_http_alignment_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            limiter_test_support:clock_set(55000),
            Ip = <<"198.51.100.13">>,
            Scope = passport_per_ip,
            %% 首请求 t=55000：窗口对齐 t0=0（到期 60000），不是 55000+60000
            ?assertMatch(
                {ok, #{
                    count := 1,
                    expires_at_ms := 60000,
                    rule_version := {10, 60000}
                }},
                login_attempt_ds:admit(Scope, Ip)
            ),
            %% t=60000：换窗（新窗 count=1），不是延续到 115000
            limiter_test_support:clock_set(60000),
            ?assertMatch(
                {ok, #{count := 1, expires_at_ms := 120000}},
                login_attempt_ds:admit(Scope, Ip)
            ),
            %% 时钟推进三周期：60000 → 120000 → 180000，各窗独立从 1 起算
            limiter_test_support:clock_set(120000),
            ?assertMatch(
                {ok, #{count := 1, expires_at_ms := 180000}},
                login_attempt_ds:admit(Scope, Ip)
            ),
            limiter_test_support:clock_set(180001),
            ?assertMatch(
                {ok, #{count := 1, expires_at_ms := 240000}},
                login_attempt_ds:admit(Scope, Ip)
            ),
            %% 同窗内各 Key 版本一致
            ?assertMatch(
                {ok, #{count := 1, rule_version := {10, 60000}}},
                login_attempt_ds:admit(Scope, <<"198.51.100.14">>)
            ),
            ok
        end)}.

limiter_time_rules_account_freeze_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            I = limiter_test_support:ident(ac08frz),
            T0 = limiter_test_support:clock_now(),
            lists:foreach(
                fun(C) ->
                    {ok, #{count := C}} = login_attempt_ds:failure(I)
                end,
                lists:seq(1, 4)
            ),
            %% 配置中途收紧 F: 5→2：旧窗口不复活、不按新规则改判
            application:set_env(imboy, login_max_attempts, 2),
            ?assertMatch(
                {ok, #{
                    count := 4,
                    locked := false,
                    rule_version := {5, 1800000}
                }},
                login_attempt_ds:snapshot(I)
            ),
            ?assertMatch({ok, #{count := 5, locked := true}}, login_attempt_ds:failure(I)),
            %% 跨窗后新窗才采用新规则
            limiter_test_support:clock_set(T0 + 1800000),
            ?assertMatch(
                {ok, #{count := 1, rule_version := {2, 1800000}}},
                login_attempt_ds:failure(I)
            ),
            ok
        end)}.

limiter_time_rules_oidc_freeze_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            limiter_test_support:clock_set(2000),
            IpA = <<"198.51.100.15">>,
            ?assertMatch(
                {ok, #{
                    count := 1,
                    rule_version := {100, 3600000},
                    expires_at_ms := 3602000
                }},
                login_attempt_ds:admit(oidc_request, IpA)
            ),
            %% OIDC 直接换规则（逐 Key 冻结：仅新窗口/新 Key 生效）
            ?assertEqual(ok, login_attempt_ds:set_ip_rates(#{oidc_request => {50, 3600000}})),
            ?assertMatch(
                {ok, #{count := 2, rule_version := {100, 3600000}}},
                login_attempt_ds:admit(oidc_request, IpA)
            ),
            ?assertMatch(
                {ok, #{count := 1, rule_version := {50, 3600000}}},
                login_attempt_ds:admit(oidc_request, <<"198.51.100.16">>)
            ),
            ok
        end)}.

limiter_time_rules_http_pending_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            limiter_test_support:clock_set(55000),
            Ip = <<"198.51.100.17">>,
            Scope = passport_per_ip,
            {ok, #{count := 1}} = login_attempt_ds:admit(Scope, Ip),
            %% 登记新规则：下一 scope 边界（60000）统一切换
            ?assertEqual(ok, login_attempt_ds:set_ip_rates(#{passport_per_ip => {3, 60000}})),
            %% 生效前（同窗）仍按旧规则
            ?assertMatch(
                {ok, #{count := 2, rule_version := {10, 60000}}},
                login_attempt_ds:admit(Scope, Ip)
            ),
            %% 边界后：所有 Key 同版本切新规则
            limiter_test_support:clock_set(60000),
            {ok, S} = login_attempt_ds:admit(Scope, Ip),
            ?assertMatch(#{count := 1, rule_version := {3, 60000}}, S),
            ?assertEqual(120000, maps:get(expires_at_ms, S)),
            lists:foreach(
                fun(N) ->
                    {ok, #{count := N}} = login_attempt_ds:admit(Scope, Ip)
                end,
                [2, 3]
            ),
            ?assertEqual({error, rate_limited}, login_attempt_ds:admit(Scope, Ip)),
            ?assertMatch(
                {ok, #{rule_version := {3, 60000}}},
                login_attempt_ds:admit(Scope, <<"198.51.100.18">>)
            ),
            ok
        end)}.

limiter_time_rules_http_pending_replace_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            limiter_test_support:clock_set(55000),
            Scope = passport_per_ip,
            %% 生效前重复提交：规则按最后一次替换、保留最早边界
            ?assertEqual(ok, login_attempt_ds:set_ip_rates(#{passport_per_ip => {3, 60000}})),
            ?assertEqual(ok, login_attempt_ds:set_ip_rates(#{passport_per_ip => {7, 60000}})),
            limiter_test_support:clock_set(60000),
            ?assertMatch(
                {ok, #{count := 1, rule_version := {7, 60000}}},
                login_attempt_ds:admit(Scope, <<"198.51.100.19">>)
            ),
            %% 非法规则整体拒绝、不清空既有限制
            ?assertEqual(
                {error, invalid_rates},
                login_attempt_ds:set_ip_rates(#{passport_per_ip => {0, 60000}})
            ),
            ?assertEqual({error, invalid_rates}, login_attempt_ds:set_ip_rates(#{a => b})),
            ?assertMatch(
                {ok, #{count := 1, rule_version := {7, 60000}}},
                login_attempt_ds:admit(Scope, <<"198.51.100.20">>)
            ),
            ok
        end)}.

%% ===================================================================
%% AC-10（limiter_capacity_during_auth）：A 无行 check 通过后其他 Key
%% 填满 → A 的 failure 漏记 → capacity_lost + 域 degraded；A 下一次与
%% 另一缺 Key 账户 check 均拒绝；reset/cleanup 腾位后仍拒绝。
%% ===================================================================

limiter_capacity_during_auth_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            application:set_env(imboy, limiter_max_account_entries, 3),
            limiter_test_support:restart_owner(),
            A = limiter_test_support:ident(ac10a),
            %% A 无行 check 通过，认证在途登记（A2 组合：begin → 认证 → 终态）
            {ok, #{count := 0}} = login_attempt_ds:check(A),
            Worker = spawn(fun() -> receive
                after 60000 -> ok
                end end),
            {ok, Ref} = login_attempt_ds:begin_attempt(A, Worker, default),
            %% 其他 Key 填满 M=3
            lists:foreach(
                fun(N) ->
                    {ok, #{count := 1}} =
                        login_attempt_ds:failure(limiter_test_support:ident({ac10, N}))
                end,
                lists:seq(1, 3)
            ),
            ?assertEqual(3, limiter_test_support:account_rows()),
            %% 无法精确记账：本次返回 capacity_lost，后续域拒绝。
            ?assertEqual({error, capacity_lost}, login_attempt_ds:end_attempt(Ref, failure)),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(A)),
            ?assertEqual(
                {error, unavailable},
                login_attempt_ds:check(limiter_test_support:ident(ac10fresh))
            ),
            ?assertEqual({error, unavailable}, login_attempt_ds:snapshot(A)),
            ?assertEqual({error, unavailable}, login_attempt_ds:reset(A)),
            ?assertEqual({error, unavailable}, login_attempt_ds:failure(A)),
            ?assertEqual({error, unavailable}, login_attempt_ds:admin_unlock(1, A)),
            %% 终态已提交，其后 Worker 死亡的 DOWN 查无此 ref，不误伤
            exit(Worker, kill),
            limiter_test_support:fence(),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(A)),
            %% reset/cleanup 腾出位置后仍拒绝（degraded 不自动恢复）
            T0 = limiter_test_support:clock_now(),
            limiter_test_support:clock_set(T0 + 1800000),
            limiter_test_support:send_cleanup(),
            ?assertEqual(0, limiter_test_support:account_rows()),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(A)),
            %% admit 域不受账户域 degraded 影响（同 owner 各域独立）
            {ok, _} = login_attempt_ds:admit(passport_per_ip, <<"198.51.100.30">>),
            ok
        end)}.

limiter_capacity_lost_direct_failure_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            application:set_env(imboy, limiter_max_account_entries, 2),
            limiter_test_support:restart_owner(),
            A = limiter_test_support:ident(ac10d),
            ?assertMatch({ok, #{count := 0}}, login_attempt_ds:check(A)),
            lists:foreach(
                fun(N) ->
                    {ok, #{count := 1}} =
                        login_attempt_ds:failure(limiter_test_support:ident({ac10d, N}))
                end,
                lists:seq(1, 2)
            ),
            %% 直接 failure 路径：返回值明确区分 capacity_lost（不是伪装
            %% {ok,1}，也不是 unavailable 的含糊拒绝）
            ?assertEqual({error, capacity_lost}, login_attempt_ds:failure(A)),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(A)),
            %% ready 仍 true（域 degraded ≠ owner 不可用）
            ?assertEqual(true, login_attempt_ds:ready()),
            ok
        end)}.

%% ===================================================================
%% AC-11（limiter_capacity_bounds）：容量不越上限、有效锁不被挤掉、超长
%% Key 拒绝、IP 新 Key capacity 返回值区分、腾位后可 admit、ETS 有界。
%% ===================================================================

limiter_capacity_bounds_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            application:set_env(imboy, limiter_max_ip_entries, 2),
            application:set_env(imboy, limiter_max_account_entries, 2),
            application:set_env(imboy, login_max_attempts, 2),
            limiter_test_support:restart_owner(),
            Scope = passport_per_ip,
            %% IP 新 Key：第 3 个 capacity（不降级，与 rate_limited 可区分）
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:admit(Scope, <<"10.0.0.1">>)),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:admit(Scope, <<"10.0.0.2">>)),
            ?assertEqual({error, capacity}, login_attempt_ds:admit(Scope, <<"10.0.0.3">>)),
            %% 超长 Key / 空 Key 拒绝（max_key_bytes 默认 256；空 Key 属
            %% 无效请求按 unavailable，超长属资源边界按 capacity）
            ?assertEqual(
                {error, capacity},
                login_attempt_ds:admit(Scope, binary:copy(<<"a">>, 300))
            ),
            ?assertEqual({error, unavailable}, login_attempt_ds:admit(Scope, <<>>)),
            %% 账户域：有效锁不被挤掉（不淘汰）
            X = limiter_test_support:ident(ac11x),
            {ok, #{count := 1}} = login_attempt_ds:failure(X),
            {ok, #{count := 2, locked := true}} = login_attempt_ds:failure(X),
            Y = limiter_test_support:ident(ac11y),
            {ok, _} = login_attempt_ds:failure(Y),
            Z = limiter_test_support:ident(ac11z),
            ?assertEqual({error, capacity_lost}, login_attempt_ds:failure(Z)),
            %% X 的锁定行物理保留（owner 唯一写者未删；degraded 后 API 只读
            %% 拒绝，故直接读表验证）
            ?assertMatch(
                [{_, #{count := 2}}],
                ets:lookup(login_attempt_ets, {limiter_account, X})
            ),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(Y)),
            %% IP 腾位后新请求可 admit（跨窗 + cleanup 删过期行）
            T0 = limiter_test_support:clock_now(),
            limiter_test_support:clock_set(T0 + 60000),
            limiter_test_support:send_cleanup(),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:admit(Scope, <<"10.0.0.3">>)),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:admit(Scope, <<"10.0.0.4">>)),
            ?assertEqual({error, capacity}, login_attempt_ds:admit(Scope, <<"10.0.0.5">>)),
            %% ETS 账户域行数不越上限（degraded 拒建后仍为 2）
            ?assertEqual(2, limiter_test_support:account_rows()),
            ok
        end)}.

%% ===================================================================
%% AC-12 DS 部分：owner 不存在 → 各操作 unavailable；DOWN/deadline →
%% degraded；end_attempt 各 Outcome 正确释放；旧 DOWN 按 ref 核对不误伤。
%% ===================================================================

limiter_lifecycle_owner_missing_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            try
                limiter_test_support:detach_owner(),
                I = limiter_test_support:ident(ac12),
                ?assertEqual(false, login_attempt_ds:ready()),
                ?assertEqual({error, unavailable}, login_attempt_ds:check(I)),
                ?assertEqual({error, unavailable}, login_attempt_ds:failure(I)),
                ?assertEqual({error, unavailable}, login_attempt_ds:reset(I)),
                ?assertEqual({error, unavailable}, login_attempt_ds:snapshot(I)),
                ?assertEqual({error, unavailable}, login_attempt_ds:admin_unlock(1, I)),
                ?assertEqual(
                    {error, unavailable},
                    login_attempt_ds:admit(passport_per_ip, <<"10.1.1.1">>)
                ),
                ?assertEqual(
                    {error, unavailable},
                    login_attempt_ds:begin_attempt(I, self(), default)
                ),
                ?assertEqual({error, unavailable}, login_attempt_ds:end_attempt(mkref(), no_count))
            after
                limiter_test_support:attach_owner()
            end,
            ?assertEqual(true, login_attempt_ds:ready()),
            ok
        end)}.

limiter_lifecycle_down_degrades_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            I = limiter_test_support:ident(ac12down),
            Worker = spawn(fun() -> receive
                after 60000 -> ok
                end end),
            {ok, Ref} = login_attempt_ds:begin_attempt(I, Worker, default),
            ?assertEqual(1, attempt_rows()),
            %% 终态未提交的进程死亡 → degraded（账户域拒绝后续）
            exit(Worker, kill),
            %% 等确认 DOWN 已被 owner 处理（attempts 行清除）
            limiter_test_support:wait_until(fun() -> attempt_rows() =:= 0 end, 200),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(I)),
            %% DOWN 已令域 degraded，终态不能再伪报成功。
            ?assertEqual({error, unavailable}, login_attempt_ds:end_attempt(Ref, no_count)),
            ok
        end)}.

limiter_lifecycle_outcomes_release_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            %% failure 终态：记账 + 释放
            If = limiter_test_support:ident(ac12f),
            Wf = spawn(fun() -> receive
                after 60000 -> ok
                end end),
            {ok, Rf} = login_attempt_ds:begin_attempt(If, Wf, default),
            ok = login_attempt_ds:end_attempt(Rf, failure),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:snapshot(If)),
            exit(Wf, kill),
            limiter_test_support:wait_until(fun() -> attempt_rows() =:= 0 end, 200),
            %% 释放后 DOWN 不降级
            ?assertMatch({ok, _}, login_attempt_ds:check(If)),
            %% reset 终态：清零 + 释放
            Ir = limiter_test_support:ident(ac12r),
            _ = login_attempt_ds:failure(Ir),
            Wr = spawn(fun() -> receive
                after 60000 -> ok
                end end),
            {ok, Rr} = login_attempt_ds:begin_attempt(Ir, Wr, default),
            ok = login_attempt_ds:end_attempt(Rr, reset),
            ?assertMatch({ok, #{count := 0}}, login_attempt_ds:snapshot(Ir)),
            exit(Wr, kill),
            limiter_test_support:wait_until(fun() -> attempt_rows() =:= 0 end, 200),
            ?assertMatch({ok, _}, login_attempt_ds:check(Ir)),
            %% no_count 终态：状态不变 + 释放
            In = limiter_test_support:ident(ac12n),
            _ = login_attempt_ds:failure(In),
            Wn = spawn(fun() -> receive
                after 60000 -> ok
                end end),
            {ok, Rn} = login_attempt_ds:begin_attempt(In, Wn, default),
            ok = login_attempt_ds:end_attempt(Rn, no_count),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:snapshot(In)),
            exit(Wn, kill),
            limiter_test_support:wait_until(fun() -> attempt_rows() =:= 0 end, 200),
            ?assertMatch({ok, _}, login_attempt_ds:check(In)),
            ok
        end)}.

limiter_lifecycle_deadline_degrades_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            I = limiter_test_support:ident(ac12dl),
            {ok, Ref} = login_attempt_ds:begin_attempt(I, self(), 500),
            %% deadline 未到：有界批次检查不降级
            limiter_test_support:send_deadline_check(),
            ?assertMatch({ok, _}, login_attempt_ds:check(I)),
            %% 时钟注入跨 deadline → 批次检查（按 ref 核对仍在表）→ degraded
            T0 = limiter_test_support:clock_now(),
            limiter_test_support:clock_set(T0 + 501),
            limiter_test_support:send_deadline_check(),
            ?assertEqual(0, attempt_rows()),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(I)),
            ?assertEqual({error, unavailable}, login_attempt_ds:end_attempt(Ref, no_count)),
            ok
        end)}.

limiter_lifecycle_stale_down_no_harm_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            I = limiter_test_support:ident(ac12stale),
            %% 多轮 begin → 正常终态 → 杀进程：终态后的 DOWN 按监控引用
            %% 核对查无此 ref，不误伤新请求、不降级
            lists:foreach(
                fun(_) ->
                    W = spawn(fun() -> receive
                        after 60000 -> ok
                        end end),
                    {ok, Ref} = login_attempt_ds:begin_attempt(I, W, default),
                    ok = login_attempt_ds:end_attempt(Ref, no_count),
                    exit(W, kill),
                    limiter_test_support:wait_until(fun() -> attempt_rows() =:= 0 end, 200),
                    ?assertMatch({ok, _}, login_attempt_ds:check(I))
                end,
                lists:seq(1, 5)
            ),
            ?assertMatch({ok, #{count := 0}}, login_attempt_ds:snapshot(I)),
            ok
        end)}.

%% ===================================================================
%% AC-13（limiter_restart_recovery）DS 部分：杀 owner → 表失；重启
%% 新 owner 保持关闭，经 PID 绑定空态批准后重新累计。
%% ===================================================================

limiter_restart_recovery_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            application:set_env(imboy, login_max_attempts, 2),
            I = limiter_test_support:ident(ac13),
            {ok, _} = login_attempt_ds:failure(I),
            ?assertMatch({ok, #{locked := true}}, login_attempt_ds:failure(I)),
            %% 摘除 child：owner 死 → ETS 表随进程消失（无 heir）
            limiter_test_support:detach_owner(),
            ?assertEqual(undefined, whereis(login_attempt_ds)),
            ?assertEqual(undefined, ets:info(login_attempt_ets)),
            %% 原始 restart_child 不包含测试装配的空态批准。
            {ok, NewOwner} = supervisor:restart_child(imboy_sup, login_attempt_ds),
            ?assertEqual(false, login_attempt_ds:ready()),
            ?assertEqual({error, unavailable}, login_attempt_ds:check(I)),
            ?assertEqual(ok, login_attempt_ds:activate_empty(NewOwner)),
            ?assertEqual(true, login_attempt_ds:ready()),
            ?assertMatch({ok, #{count := 0, locked := false}}, login_attempt_ds:check(I)),
            ?assertMatch({ok, #{count := 1}}, login_attempt_ds:failure(I)),
            ok
        end)}.

limiter_restart_recovery_degraded_kept_while_alive_test_() ->
    {timeout, 60,
        ?WITH_MECKS([?LOG_MOCK, ?CLOCK_MOCK], fun() ->
            ok = ensure_server(),
            application:set_env(imboy, limiter_max_account_entries, 1),
            limiter_test_support:restart_owner(),
            A = limiter_test_support:ident(ac13d),
            {ok, _} = login_attempt_ds:failure(A),
            ?assertEqual(
                {error, capacity_lost},
                login_attempt_ds:failure(
                    limiter_test_support:ident(ac13e)
                )
            ),
            %% 同 owner 内：cleanup/reset 腾位均不解锁 degraded；空态解除
            %% 仅走受控重启（见 limiter_restart_recovery_test_）
            ?assertEqual({error, unavailable}, login_attempt_ds:reset(A)),
            limiter_test_support:send_cleanup(),
            ?assertEqual(
                {error, unavailable},
                login_attempt_ds:check(limiter_test_support:ident(ac13f))
            ),
            ok
        end)}.

%% ------------------------------------------------------------------

attempt_rows() ->
    ets:select_count(
        login_attempt_ets,
        [{{{limiter_attempt, '_'}, '_'}, [], [true]}]
    ).

mkref() ->
    erlang:make_ref().
