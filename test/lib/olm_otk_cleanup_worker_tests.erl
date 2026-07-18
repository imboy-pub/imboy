-module(olm_otk_cleanup_worker_tests).

-include_lib("eunit/include/eunit.hrl").

%%% @doc olm_otk_cleanup_worker 行为测试（经 handle_info(run, State) 驱动真实 tick 路径）。
%%% 重点验证行为而非覆盖率：enabled 门、失败不崩、下一轮继续。

%% enabled=false：不调用 Logic，且照常返回 {noreply, State}（下一轮继续调度）
disabled_does_not_call_logic_test() ->
    _ = catch meck:unload([olm_identity_logic]),
    ok = meck:new(olm_identity_logic, [no_link]),
    try
        application:set_env(imboy, olm_otk_cleanup_enabled, false),
        meck:expect(olm_identity_logic, cleanup_consumed_one_time_keys, 1, fun(_) -> {ok, 0} end),
        ?assertEqual({noreply, #{}}, olm_otk_cleanup_worker:handle_info(run, #{})),
        ?assertEqual(0, meck:num_calls(olm_identity_logic, cleanup_consumed_one_time_keys, '_'))
    after
        application:unset_env(imboy, olm_otk_cleanup_enabled),
        meck:unload([olm_identity_logic])
    end.

%% enabled=true 且 cleanup 成功：调用 Logic 一次，返回 {noreply, State}
enabled_success_calls_logic_test() ->
    _ = catch meck:unload([olm_identity_logic]),
    ok = meck:new(olm_identity_logic, [no_link]),
    try
        application:set_env(imboy, olm_otk_cleanup_enabled, true),
        meck:expect(olm_identity_logic, cleanup_consumed_one_time_keys, 1, fun(_) -> {ok, 5} end),
        ?assertEqual({noreply, #{}}, olm_otk_cleanup_worker:handle_info(run, #{})),
        ?assertEqual(1, meck:num_calls(olm_identity_logic, cleanup_consumed_one_time_keys, '_'))
    after
        application:unset_env(imboy, olm_otk_cleanup_enabled),
        meck:unload([olm_identity_logic])
    end.

%% enabled=true 但 cleanup 失败：只 WARN，不 crash，仍返回 {noreply, State}（下一轮继续）
enabled_failure_warns_and_continues_test() ->
    _ = catch meck:unload([olm_identity_logic]),
    ok = meck:new(olm_identity_logic, [no_link]),
    try
        application:set_env(imboy, olm_otk_cleanup_enabled, true),
        meck:expect(
            olm_identity_logic,
            cleanup_consumed_one_time_keys,
            1,
            fun(_) -> {error, <<"internal_error">>} end
        ),
        %% 不抛异常、返回 {noreply, State} 即证明 worker 未崩、tick 已重排
        ?assertEqual({noreply, #{}}, olm_otk_cleanup_worker:handle_info(run, #{})),
        ?assertEqual(1, meck:num_calls(olm_identity_logic, cleanup_consumed_one_time_keys, '_'))
    after
        application:unset_env(imboy, olm_otk_cleanup_enabled),
        meck:unload([olm_identity_logic])
    end.

%% 未知消息不影响 worker：返回 {noreply, State}
ignores_unknown_info_test() ->
    ?assertEqual({noreply, #{}}, olm_otk_cleanup_worker:handle_info(some_other_msg, #{})).
