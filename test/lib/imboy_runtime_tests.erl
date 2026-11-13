-module(imboy_runtime_tests).
-include_lib("eunit/include/eunit.hrl").
-export([init/1]).

init([]) -> {ok, {#{strategy => one_for_one}, []}}.

pool_is_a_supervised_child_test() ->
    %% 2026-11-13 设计裁决：pooler 'pgsql' 池保持 pooler_sup 托管（不挂进
    %% imboy_sup）。原因：全仓 34 个套件通过 pooler:rm_pool/new_pool 接管
    %% 应用池（如 adm_organization_handler_tests:setup_db 的 A1c 接管），
    %% 若池是本树 permanent child，rm_pool 会被 sup 复活、new_pool 恒
    %% already_started，接管机制整体失效（实测）。本测试钉住现契约：
    %%   1) sup init 恰好 new_pool 一次（带 pg_conf）；
    %%   2) 池不是监督树子项（接管兼容）；
    %%   3) user_server/msg_store_sup 关闭预算维持 T02 结论。
    pure_mock_fixture:run(fun() ->
        Modules = [pooler, config_ds, elib_tsid, imboy_feature, attachment_delete_reaper],
        [meck:new(M, [passthrough, no_link]) || M <- Modules],
        meck:expect(pooler, new_pool, fun(_) -> {ok, self()} end),
        meck:expect(config_ds, env, fun(pg_conf) ->
            #{name => pgsql, start_mfa => {epgsql, connect, [#{}]}}
        end),
        meck:expect(config_ds, env, fun(depcache, Default) -> Default end),
        meck:expect(elib_tsid, init, fun(_) -> ok end),
        meck:expect(imboy_feature, enabled, fun(_) -> false end),
        meck:expect(attachment_delete_reaper, child_specs, fun() -> [] end),
        try
            {ok, {_, Specs}} = imboy_sup:init([]),
            ?assertEqual(1, meck:num_calls(pooler, new_pool, 1)),
            [] = [S || #{start := {pooler_pool_sup, start_link, _}} = S <- Specs],
            User = hd([S || #{id := user_server} = S <- Specs]),
            Msg = hd([S || #{id := msg_store_sup} = S <- Specs]),
            ?assertEqual(5000, maps:get(shutdown, User)),
            ?assertEqual(infinity, maps:get(shutdown, Msg))
        after
            [meck:unload(M) || M <- lists:reverse(Modules)]
        end
    end).

runtime_probe_test() ->
    [
        begin
            ?assertEqual(
                {503, ready},
                healthz_handler:probe(
                    ready,
                    #{db => true, tsid => ready, runtime => Phase}
                )
            ),
            ?assertEqual({200, live}, healthz_handler:probe(live, #{runtime => Phase}))
        end
     || Phase <- [starting, draining, degraded]
    ],
    ?assertEqual(
        {200, ready},
        healthz_handler:probe(
            ready,
            #{db => true, tsid => ready, runtime => ready}
        )
    ).

draining_routes_test() ->
    pure_mock_fixture:run(fun() ->
        application:set_env(imboy, lifecycle, draining),
        meck:new(msg_c2c_logic, [passthrough, no_link]),
        meck:expect(msg_c2c_logic, c2c, fun(_, _, _) -> error(new_write_during_drain) end),
        meck:expect(msg_c2c_logic, c2c_revoke_ack, fun(_, _, _) -> ok end),
        try
            ?assertEqual(
                {close, 1012, <<"service_restart">>},
                message_router_logic:route(<<"1">>, 1, #{}, <<"C2C">>, <<"{}">>)
            ),
            ?assertEqual(
                ok,
                message_router_logic:route(
                    <<"1">>,
                    1,
                    #{<<"action">> => <<"message_revoke_ack">>},
                    <<"C2C">>,
                    <<"{}">>
                )
            ),
            ?assertEqual(0, meck:num_calls(msg_c2c_logic, c2c, 3))
        after
            meck:unload(msg_c2c_logic)
        end
    end).

%% T02：drain 配置启动期校验——非法值（负数/非整数/超上界/非数）必须
%% 启动即 error，不允许带非法停机预算进入运行态。纯 env 操作，用后还原。
drain_config_validation_test_() ->
    [
        ?_test(check_drain_config(Value, Expect))
     || {Value, Expect} <- [
            {0, ok},
            {5000, ok},
            {60000, ok},
            {-1, reject},
            {60001, reject},
            {5000.0, reject},
            {<<"5000">>, reject},
            {infinity, reject}
        ]
    ].

check_drain_config(Value, Expect) ->
    Prev = application:get_env(imboy, drain_timeout_ms, undefined),
    application:set_env(imboy, drain_timeout_ms, Value),
    try
        case Expect of
            ok ->
                ?assertEqual(ok, imboy_app:validate_drain_config());
            reject ->
                ?assertError(
                    {invalid_config, drain_timeout_ms, Value},
                    imboy_app:validate_drain_config()
                )
        end
    after
        restore_drain_env(Prev)
    end.

restore_drain_env(undefined) -> application:unset_env(imboy, drain_timeout_ms);
restore_drain_env(Prev) -> application:set_env(imboy, drain_timeout_ms, Prev).

drain_timeout_test() ->
    pure_mock_fixture:run(fun() ->
        application:set_env(imboy, lifecycle, ready),
        application:set_env(imboy, drain_timeout_ms, 40),
        meck:new(config_ds, [passthrough, no_link]),
        meck:expect(config_ds, env, fun(start_mode, http) -> http end),
        meck:new(ranch, [passthrough, no_link]),
        meck:expect(ranch, info, fun(imboy_listener) -> #{all_connections => 1} end),
        meck:expect(ranch, suspend_listener, fun(imboy_listener) ->
            ?assertNot(imboy_app:accepting_requests()),
            ok
        end),
        meck:new(cowboy, [passthrough, no_link]),
        meck:expect(cowboy, stop_listener, fun(imboy_listener) -> ok end),
        try
            Start = erlang:monotonic_time(millisecond),
            ?assertEqual(synthetic_state, imboy_app:prep_stop(synthetic_state)),
            Elapsed = erlang:monotonic_time(millisecond) - Start,
            ?assert(Elapsed >= 40 andalso Elapsed < 1000),
            ?assertEqual(1, meck:num_calls(ranch, suspend_listener, 1)),
            ?assertEqual(1, meck:num_calls(cowboy, stop_listener, 1))
        after
            [meck:unload(M) || M <- [cowboy, ranch, config_ds]]
        end
    end).

middleware_readiness_test() ->
    %% 2026-11-13（R2 review）：入口闸门改读 accepting_requests（lifecycle
    %% env 原子判断），degraded 聚合只在 /readyz 探针侧——中间件不再调用
    %% readiness()，避免每请求 gen_server:call 翻倍热点邮箱。
    pure_mock_fixture:run(fun() ->
        meck:new(imboy_app, [passthrough, no_link]),
        meck:new(elib_response, [passthrough, no_link]),
        meck:expect(elib_response, error_with_status, fun(req, 503, _, 503) -> unavailable_req end),
        try
            [
                begin
                    meck:expect(
                        imboy_app,
                        accepting_requests,
                        fun() -> Accepting end
                    ),
                    case Accepting of
                        true ->
                            ?assertEqual(
                                {ok, req, #{}},
                                lifecycle_middleware:execute(req, #{})
                            );
                        false ->
                            ?assertEqual(
                                {stop, unavailable_req},
                                lifecycle_middleware:execute(req, #{})
                            )
                    end,
                    ?assertEqual(
                        {ok, req, #{handler => healthz_handler}},
                        lifecycle_middleware:execute(req, #{handler => healthz_handler})
                    )
                end
             || Accepting <- [true, false]
            ],
            %% degraded 聚合不再影响入口闸门：readiness()=degraded 仍放行
            meck:expect(imboy_app, accepting_requests, fun() -> true end),
            meck:expect(imboy_app, readiness, fun() -> degraded end),
            ?assertEqual({ok, req, #{}}, lifecycle_middleware:execute(req, #{}))
        after
            meck:unload(elib_response),
            meck:unload(imboy_app)
        end
    end).

trapping_user_worker_shutdown_test() ->
    Parent = self(),
    Victim = spawn(fun() ->
        process_flag(trap_exit, true),
        Parent ! {worker_ready, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {worker_ready, Victim} -> ok
    after 1000 -> error(worker_not_started)
    end,
    {Runner, Ref} = spawn_monitor(fun() -> user_server:terminate(shutdown, [#{pid => Victim}]) end),
    try
        receive
            {'DOWN', Ref, process, Runner, normal} -> ?assertNot(is_process_alive(Victim));
            {'DOWN', Ref, process, Runner, Reason} -> error({shutdown_failed, Reason})
        after 1500 -> error(user_worker_shutdown_unbounded)
        end
    after
        exit(Runner, kill),
        exit(Victim, kill),
        demonitor(Ref, [flush])
    end.

runtime_start_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> check_start(Mode) end))
     || Mode <- [success, core_failure, listener_failure]
    ].

check_start(Mode) ->
    Modules = [imboy_sup, login_attempt_ds, group_event_handler, enterprise_oa_bootstrap, cowboy],
    [meck:new(M, [passthrough, no_link]) || M <- Modules],
    meck:expect(imboy_sup, start_link, fun() ->
        case Mode of
            core_failure ->
                {error, synthetic_core_failure};
            _ ->
                {ok, Sup} = supervisor:start_link(?MODULE, []),
                put(runtime_sup, Sup),
                {ok, Sup}
        end
    end),
    meck:expect(login_attempt_ds, activate_empty, fun(_) -> ok end),
    meck:expect(group_event_handler, attach, fun() -> ok end),
    meck:expect(enterprise_oa_bootstrap, maybe_seed, fun() -> ok end),
    meck:expect(cowboy, start_clear, fun(_, _, _) ->
        ?assert(is_pid(get(runtime_sup))),
        ?assertNot(imboy_app:accepting_requests()),
        case Mode of
            listener_failure -> {error, synthetic_bind_failure};
            _ -> {ok, self()}
        end
    end),
    try
        case Mode of
            success ->
                {ok, Sup} = imboy_app:start_runtime([], http),
                ?assert(is_process_alive(Sup)),
                ?assert(imboy_app:accepting_requests());
            core_failure ->
                ?assertEqual({error, synthetic_core_failure}, imboy_app:start_runtime([], http)),
                ?assertEqual(0, meck:num_calls(cowboy, start_clear, 3));
            listener_failure ->
                ?assertError(
                    {listener_start_failed, http, synthetic_bind_failure},
                    imboy_app:start_runtime([], http)
                ),
                ?assertNot(is_process_alive(get(runtime_sup))),
                ?assertNot(imboy_app:accepting_requests()),
                ?assertEqual(0, meck:num_calls(cowboy, stop_listener, 1))
        end
    after
        case erase(runtime_sup) of
            P when is_pid(P) ->
                case is_process_alive(P) of
                    true ->
                        unlink(P),
                        gen_server:stop(P, shutdown, infinity);
                    false ->
                        ok
                end;
            _ ->
                ok
        end,
        [meck:unload(M) || M <- lists:reverse(Modules)]
    end.
