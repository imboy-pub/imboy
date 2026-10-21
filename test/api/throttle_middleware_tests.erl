-module(throttle_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc throttle_middleware 模块测试
%%%
%%% limiter-v1 L3（AC-14，计划 docs/plans/2026-10-13-limiter-v1-* §9.2）：
%%% 两只 IP scope（passport_per_ip / api_per_ip）的权威 owner 已迁至
%%% login_attempt_ds:admit/2（owner 故障 → 503，不再 rate_not_set fail-open）；
%%% UID 双桶与 cs_widget_per_ip 仍走 throttle 原库。middleware 分支测试
%%% meck admit 观测调用契约（被测单元是 middleware 分支，不是 DS 本体）；
%%% owner 测试用真实 DS（eunit 环境带 imboy app）。

%% @doc login_attempt_ds:admit 放行时返回的快照（middleware 只匹配 {ok, _}）
admit_snap(N) ->
    #{
        count => N,
        remaining => 9 - N,
        locked => false,
        expires_at_ms => undefined,
        rule_version => {10, 60000}
    }.

%% @doc passport 路径命中专用宽松限流（非完全豁免，GAP-09），需要
%% mock elib_req:get_client_ip + login_attempt_ds:admit(passport_per_ip, _)；
%% 同时断言该分支不再触碰 throttle 原库（旧权威已移除）。
whitelist_passport_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/passport/login">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"203.0.113.1">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(passport_per_ip, <<"203.0.113.1">>) -> {ok, admit_snap(1)} end}
            ]},
            {throttle, [
                {'check', 2, fun(_Scope, _Key) ->
                    erlang:error(throttle_must_not_be_called_for_ip_scopes)
                end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc /api/v1/init、/api/v1/ws 完全豁免限流（v0 裸路径已下架，不再测试）
whitelist_init_ws_test_() ->
    Paths = [<<"/api/v1/init">>, <<"/api/v1/ws">>],
    [
        ?WITH_MECKS(
            [
                {cowboy_req, [
                    {'path', 1, fun(_Req) -> Path end}
                ]}
            ],
            fun() ->
                Req = fake_req,
                Env = #{handler_opts => #{}},
                Result = throttle_middleware:execute(Req, Env),
                ?assertMatch({ok, _, _}, Result)
            end
        )
     || Path <- Paths
    ].

whitelist_health_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/health">> end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

whitelist_static_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/static/js/app.js">> end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc 正常请求通过（已认证用户，基于 UID 限流；2026-10 起为
%% api_per_user_burst + api_per_user 双桶串联检查）
normal_uid_request_pass_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/user/info">> end}
            ]},
            {throttle, [
                {'check', 2, fun(Scope, _Key) ->
                    case Scope of
                        api_per_user_burst -> {ok, 29, 1000};
                        api_per_user -> {ok, 299, 60000}
                    end
                end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{current_uid => 12345}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc 正常请求通过（未认证用户，基于 IP，新权威 admit）
normal_ip_request_pass_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/app_version/check">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"192.168.1.1">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(api_per_ip, <<"192.168.1.1">>) -> {ok, admit_snap(1)} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc admit 各返回值 → 对外合同（C.6）：放行 / 429 / 503。
%% passport 分支同样三态（429/503 各验一只即可，分支代码同构）。
passport_admit_rate_limited_429_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/passport/do_login">> end},
                {'reply', 4, fun(429, _H, _B, R) -> R end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"203.0.113.2">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(passport_per_ip, _) -> {error, rate_limited} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

passport_admit_owner_unavailable_503_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/passport/do_login">> end},
                {'reply', 4, fun(503, _H, _B, R) -> R end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"203.0.113.3">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(passport_per_ip, _) -> {error, unavailable} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

%% @doc 超限返回 429（已认证用户，分钟桶超限）
uid_rate_limit_exceeded_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/user/info">> end},
                {'reply', 4, fun(429, _Headers, _Body, Req) -> Req end}
            ]},
            {throttle, [
                {'check', 2, fun(Scope, _Key) ->
                    case Scope of
                        api_per_user_burst -> {ok, 29, 1000};
                        api_per_user -> {limit_exceeded, 0, 5000}
                    end
                end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{current_uid => 12345}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

%% @doc 秒级突发桶超限也返回 429，且不再检查分钟桶（双桶串联，短路返回）
uid_burst_rate_limit_exceeded_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/user/info">> end},
                {'reply', 4, fun(429, _Headers, _Body, Req) -> Req end}
            ]},
            {throttle, [
                {'check', 2, fun(Scope, _Key) ->
                    case Scope of
                        api_per_user_burst ->
                            {limit_exceeded, 0, 900};
                        api_per_user ->
                            self() ! minute_bucket_must_not_be_checked,
                            {ok, 299, 60000}
                    end
                end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{current_uid => 12345}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result),
            receive
                minute_bucket_must_not_be_checked -> ?assert(false)
            after 0 ->
                ok
            end
        end
    ).

%% @doc 超限返回 429（未认证用户，基于 IP，新权威 admit）
ip_rate_limit_exceeded_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/app_version/check">> end},
                {'reply', 4, fun(429, _Headers, _Body, Req) -> Req end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"10.0.0.1">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(api_per_ip, <<"10.0.0.1">>) -> {error, rate_limited} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

%% @doc owner 容量满（capacity）→ 503（不伪装 429、不 fail-open）
ip_limit_owner_capacity_503_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/app_version/check">> end},
                {'reply', 4, fun(503, _Headers, _Body, Req) -> Req end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"10.0.0.2">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(api_per_ip, _) -> {error, capacity} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

%% @doc owner 未 ready（unavailable）→ 503（旧 rate_not_set fail-open 已删，
%% C.8：未 ready 不准入）
ip_limit_owner_not_ready_503_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/app_version/check">> end},
                {'reply', 4, fun(503, _Headers, _Body, Req) -> Req end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"10.0.0.3">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(api_per_ip, _) -> {error, unavailable} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({stop, _}, Result)
        end
    ).

%% @doc UID vs IP 限流选择 - 有 current_uid 走 UID 限流
uid_vs_ip_selection_uid_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/msg/send">> end}
            ]},
            {throttle, [
                {'check', 2, fun(Scope, _Key) ->
                    %% 确保已认证请求走 UID 双桶而非 api_per_ip
                    case Scope of
                        api_per_user_burst -> {ok, 29, 1000};
                        api_per_user -> {ok, 100, 60000};
                        api_per_ip -> {limit_exceeded, 0, 5000}
                    end
                end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{current_uid => 99}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% @doc UID vs IP 限流选择 - 无 current_uid 走 IP 限流（新权威 admit；
%% throttle 任意 scope 若被误调即 429 短路，断言必失败）
uid_vs_ip_selection_ip_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/open/resource">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"172.16.0.1">> end}
            ]},
            {login_attempt_ds, [
                {'admit', 2, fun(api_per_ip, <<"172.16.0.1">>) -> {ok, admit_snap(1)} end}
            ]},
            {throttle, [
                {'check', 2, fun(_Scope, _Key) -> {limit_exceeded, 0, 5000} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
        end
    ).

%% ===================================================================
%% Limiter V1 L3 fixed oracle（计划 docs/plans/2026-10-13-limiter-v1-*
%% §9.2 AC-14，limiter_scope_owner）。原 L0 复现（throttle:setup 重复
%% 装配产生双 worker、旧 reset timer 清新额度）已随 L3 迁移翻转：
%% 两只 IP scope 唯一权威是 login_attempt_ds。
%% ===================================================================

-define(L3_PASSPORT_IP, <<"203.0.113.77">>).

%% AC-14 目标行为：
%% 1) 真实 admit（新 owner）对同 IP 连续计数，额度只随请求消耗；
%% 2) 重复初始化（init_throttle_rates 重跑 + set_ip_rates 同值重放）不得
%%    重置有效额度；
%% 3) 无旧 worker/index/Cache 回落：throttle 的 scope 映射表无两 IP 行；
%% 4) UID / cs_widget 等其它 scope 仍走 throttle 原库。
%% WITH_MECKS 软 boot 不保证 throttle app / login_attempt_ds owner 存活
%% （全量顺序里 app 常被前置套件停掉）——真实 DS 测试自前置拉起。
with_limiter_owner(Fun) ->
    {ok, StartedApps} = application:ensure_all_started(throttle),
    try
        Owned =
            case whereis(login_attempt_ds) of
                undefined ->
                    {ok, Pid} = gen_server:start(
                        {local, login_attempt_ds},
                        login_attempt_ds,
                        [],
                        []
                    ),
                    Pid;
                _ ->
                    undefined
            end,
        try
            case Owned of
                undefined -> ok;
                _ -> ok = login_attempt_ds:activate_empty(Owned)
            end,
            Fun()
        after
            stop_owned_limiter(Owned)
        end
    after
        lists:foreach(
            fun(App) -> ok = application:stop(App) end,
            lists:reverse(StartedApps)
        )
    end.

stop_owned_limiter(undefined) ->
    ok;
stop_owned_limiter(Pid) ->
    Ref = monitor(process, Pid),
    ok = gen_server:stop(Pid, normal, 1000),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 -> error(limiter_fixture_cleanup_timeout)
    end.

limiter_fixture_failure_preserves_worker_ownership_test() ->
    Before = whereis(login_attempt_ds),
    Apps = lists:sort(application:which_applications()),
    ?assertThrow(fixture_failure, with_limiter_owner(fun() -> throw(fixture_failure) end)),
    ?assertEqual(Before, whereis(login_attempt_ds)),
    ?assertEqual(Apps, lists:sort(application:which_applications())).

limiter_scope_owner_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            [],
            fun() ->
                with_limiter_owner(fun() ->
                    ?assert(login_attempt_ds:ready()),
                    Ip = ?L3_PASSPORT_IP,
                    %% 1. 新 owner 装配一次并计数
                    {ok, S1} = login_attempt_ds:admit(passport_per_ip, Ip),
                    C1 = maps:get(count, S1),
                    {ok, S2} = login_attempt_ds:admit(passport_per_ip, Ip),
                    ?assertEqual(C1 + 1, maps:get(count, S2)),
                    %% 2. 重复初始化不重置有效额度：init_throttle_rates 重跑
                    %%    （幂等，不再 setup 两 IP scope）+ set_ip_rates 同值重放
                    %%    （C.4：重复提交在生效前按最后一次替换，不动已有窗口）
                    ok = imboy_app:init_throttle_rates(),
                    ok = login_attempt_ds:set_ip_rates(#{passport_per_ip => {10, 60000}}),
                    {ok, S3} = login_attempt_ds:admit(passport_per_ip, Ip),
                    ?assertEqual(C1 + 2, maps:get(count, S3)),
                    %% 3. 无旧回落：throttle scope→表 映射中不存在两 IP scope
                    ?assertNot(ets:member(throttle_state_table, passport_per_ip)),
                    ?assertNot(ets:member(throttle_state_table, api_per_ip)),
                    %% 4. UID/widget scope 仍原库：cs_widget_per_ip 已注册且可计数
                    ?assert(ets:member(throttle_state_table, cs_widget_per_ip)),
                    ProbeKey = {ac14_probe, erlang:unique_integer()},
                    ?assertMatch({ok, _, _}, throttle:check(cs_widget_per_ip, ProbeKey)),
                    ok
                end)
            end
        )}.

%% AC-14 旧权威移除的直接证据：passport 分支执行成功且全程未触碰
%% throttle 原库（throttle:check 被替换为遇调即错的哨兵）。
middleware_ip_scopes_not_via_throttle_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/passport/meta">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"203.0.113.88">> end}
            ]},
            {throttle, [
                {'check', 2, fun(_Scope, _Key) ->
                    erlang:error(throttle_must_not_be_called_for_ip_scopes)
                end}
            ]}
        ],
        fun() ->
            %% 不 mock login_attempt_ds：真实 DS（新权威）放行；
            %% owner 缺席时 admit 走 unavailable → 503 分支打真实
            %% cowboy reply（fake_req 崩）——自前置保证真实权威在位。
            with_limiter_owner(fun() ->
                Req = fake_req,
                Env = #{handler_opts => #{}},
                ?assertMatch({ok, _, _}, throttle_middleware:execute(Req, Env))
            end)
        end
    ).

%% C.8 启动校验：{throttle, rates} 仍含两 IP scope → 报迁移缺项并拒绝
%% （error 退出阻止旧新混跑）；迁出后（当前 config 状态）→ ok。
limiter_migration_validation_rejects_stale_scopes_test_() ->
    ?WITH_MECKS(
        [],
        fun() ->
            OldRates = application:get_env(throttle, rates),
            try
                application:set_env(throttle, rates, [{passport_per_ip, 10, per_minute}]),
                ?assertError(
                    {limiter_migration_incomplete, [passport_per_ip]},
                    imboy_app:validate_limiter_ip_migration()
                ),
                application:set_env(throttle, rates, [{api_per_ip, 60, per_minute}]),
                ?assertError(
                    {limiter_migration_incomplete, [api_per_ip]},
                    imboy_app:validate_limiter_ip_migration()
                ),
                application:unset_env(throttle, rates),
                ?assertEqual(ok, imboy_app:validate_limiter_ip_migration())
            after
                case OldRates of
                    {ok, R} -> application:set_env(throttle, rates, R);
                    undefined -> application:unset_env(throttle, rates)
                end
            end,
            %% 当前启动配置（已迁出）必须通过
            ?assertEqual(ok, imboy_app:validate_limiter_ip_migration())
        end
    ).
