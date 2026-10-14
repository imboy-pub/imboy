-module(throttle_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc throttle_middleware 模块测试

%% @doc passport 路径命中专用宽松限流（非完全豁免，GAP-09），需要
%% mock elib_req:get_client_ip + throttle:check(passport_per_ip, _)，
%% 否则会真的调用 cowboy_req:header/3 在 fake_req 上崩溃。
whitelist_passport_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/passport/login">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"203.0.113.1">> end}
            ]},
            {throttle, [
                {'check', 2, fun(passport_per_ip, _Key) -> {ok, 9, 60000} end}
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

%% @doc 正常请求通过（未认证用户，基于 IP 限流）
normal_ip_request_pass_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/app_version/check">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"192.168.1.1">> end}
            ]},
            {throttle, [
                {'check', 2, fun(api_per_ip, _Key) -> {ok, 59, 60000} end}
            ]}
        ],
        fun() ->
            Req = fake_req,
            Env = #{handler_opts => #{}},
            Result = throttle_middleware:execute(Req, Env),
            ?assertMatch({ok, _, _}, Result)
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

%% @doc 超限返回 429（未认证用户，基于 IP）
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
            {throttle, [
                {'check', 2, fun(api_per_ip, _Key) -> {limit_exceeded, 0, 5000} end}
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

%% @doc UID vs IP 限流选择 - 无 current_uid 走 IP 限流
uid_vs_ip_selection_ip_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/v1/open/resource">> end}
            ]},
            {elib_req, [
                {'get_client_ip', 1, fun(_Req) -> <<"172.16.0.1">> end}
            ]},
            {throttle, [
                {'check', 2, fun(Scope, _Key) ->
                    %% 确保使用的是 api_per_ip scope
                    case Scope of
                        api_per_ip -> {ok, 50, 60000};
                        api_per_user -> {limit_exceeded, 0, 5000}
                    end
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

%% ===================================================================
%% Limiter V1 L0 缺陷复现（计划 docs/plans/2026-10-13-limiter-v1-*
%% §5.F7、§9.2 AC-14）。真实 throttle 库（deps/throttle，未 mock）。
%% oracle：断言旧实现的坏结果；L3 scope 迁移到唯一 owner 后翻转。
%% ===================================================================

-define(L0_SCOPE, limiter_l0_probe_20261014).

%% 记录当前 throttle_sup children 的 pid 集
l0_scope_workers() ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(throttle_sup), is_pid(Pid)].

%% AC-14 旧侧复现：throttle:setup/3 每次调用都 start_child 一个新 worker
%% 并让新 worker initialize 覆写 scope→表 映射（throttle_ets:initialize/3），
%% 旧 worker 连同它的 reset timer 一起存活。reset 按 scope 查**最新**表
%% （throttle_ets:reset/2）——旧 worker 的 timer 触发 reset_counters 时，
%% 清掉的是新表的全部计数。重复装配不幂等，且旧 timer 持续重置新额度。
%% 生产中 imboy_app:init_throttle_rates/0 与 throttle_app（sys.config rates）
%% 对 api_per_ip / passport_per_ip 各 setup 一次即构成此形态。
limiter_scope_owner_test_() ->
    {timeout, 30,
        ?WITH_MECKS(
            [],
            fun() ->
                Before = l0_scope_workers(),
                %% 第一次装配（N=2/min）
                ok = throttle:setup(?L0_SCOPE, 2, per_minute),
                [WorkerA] = l0_scope_workers() -- Before,
                %% 占满额度（真实 update_counter；N=2，第 3 次拒绝）
                {ok, _, _} = throttle:check(?L0_SCOPE, l0_key),
                {ok, _, _} = throttle:check(?L0_SCOPE, l0_key),
                {limit_exceeded, 0, _} = throttle:check(?L0_SCOPE, l0_key),
                %% 第二次装配（重复 setup，新 worker + 新表，覆写映射）
                AfterA = l0_scope_workers(),
                ok = throttle:setup(?L0_SCOPE, 2, per_minute),
                [WorkerB] = l0_scope_workers() -- AfterA,
                true = (WorkerA =/= WorkerB),
                %% 新表满额度：2 次通过、第 3 次拒绝
                {ok, _, _} = throttle:check(?L0_SCOPE, l0_key),
                {ok, _, _} = throttle:check(?L0_SCOPE, l0_key),
                {limit_exceeded, 0, _} = throttle:check(?L0_SCOPE, l0_key),
                %% 旧 worker 的 reset timer 到期（或任何来源的重复 reset）：
                %% 直接投递 reset_counters 消息（与 timer:send_interval 投递
                %% 的消息一致），旧 worker 按 scope 找到的是**新表**
                WorkerA ! reset_counters,
                %% 同步等待旧 worker 处理完该消息（sys 消息排在普通消息之后，
                %% get_state 返回即表示 reset_counters 已处理完）
                {_, _, _} = sys:get_state(WorkerA),
                %% L0 旧侧 oracle：新表的额度被旧 worker 清零——第 4 次请求
                %% 又被放行（N+1 饱和计数同时被重置）
                ?assertMatch({ok, _, _}, throttle:check(?L0_SCOPE, l0_key)),
                %% 清理：杀掉本用例创建的两个 worker（transient，不重启），
                %% 其拥有的匿名表随之消亡；删除 STATE_TABLE 的 scope 行
                exit(WorkerA, shutdown),
                exit(WorkerB, shutdown),
                true = ets:delete(throttle_state_table, ?L0_SCOPE),
                ok
            end
        )}.
