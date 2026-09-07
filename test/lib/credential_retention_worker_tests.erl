-module(credential_retention_worker_tests).

-include_lib("eunit/include/eunit.hrl").

%%% @doc credential_retention_worker 行为测试（经 handle_info(run, State) 驱动真实 tick 路径，
%%% 对齐 olm_otk_cleanup_worker_tests 风格）：enabled 门、dry-run、分批、失败不崩。

%% enabled=false：不触碰 ds，照常返回 {noreply, State}
disabled_does_not_call_ds_test() ->
    _ = catch meck:unload([verification_code_ds]),
    ok = meck:new(verification_code_ds, [no_link]),
    try
        application:set_env(imboy, credential_retention_enabled, false),
        meck:expect(verification_code_ds, purge_expired, 2, fun(_, _) -> {ok, 0} end),
        ?assertEqual({noreply, #{}}, credential_retention_worker:handle_info(run, #{})),
        ?assertEqual(0, meck:num_calls(verification_code_ds, purge_expired, '_'))
    after
        application:unset_env(imboy, credential_retention_enabled),
        meck:unload([verification_code_ds])
    end.

%% enabled + purge 成功（不足一批）：调用一次后本轮结束
enabled_purge_short_batch_test() ->
    _ = catch meck:unload([verification_code_ds]),
    ok = meck:new(verification_code_ds, [no_link]),
    try
        application:set_env(imboy, credential_retention_enabled, true),
        application:set_env(imboy, credential_retention_batch_limit, 100),
        put(captured, none),
        meck:expect(verification_code_ds, purge_expired, 2, fun(Cutoff, Limit) ->
            put(captured, {Cutoff, Limit}),
            {ok, 3}
        end),
        ?assertEqual({noreply, #{}}, credential_retention_worker:handle_info(run, #{})),
        ?assertEqual(1, meck:num_calls(verification_code_ds, purge_expired, '_')),
        %% cutoff 形态：rfc3339 binary
        {Cutoff, _Limit} = get(captured),
        ?assert(is_binary(Cutoff))
    after
        application:unset_env(imboy, credential_retention_enabled),
        application:unset_env(imboy, credential_retention_batch_limit),
        meck:unload([verification_code_ds])
    end.

%% bounded batches：满批继续，直到出现不足一批（2+2+1=3 批）
enabled_purge_batches_until_short_test() ->
    _ = catch meck:unload([verification_code_ds]),
    ok = meck:new(verification_code_ds, [no_link]),
    try
        application:set_env(imboy, credential_retention_enabled, true),
        application:set_env(imboy, credential_retention_batch_limit, 2),
        put(purge_seq, 0),
        meck:expect(verification_code_ds, purge_expired, 2, fun(_Cutoff, _Limit) ->
            N = get(purge_seq) + 1,
            put(purge_seq, N),
            case N of
                1 -> {ok, 2};
                2 -> {ok, 2};
                _ -> {ok, 1}
            end
        end),
        ?assertEqual({noreply, #{}}, credential_retention_worker:handle_info(run, #{})),
        %% 满批(2)+满批(2)+短批(1) → 本轮结束
        ?assertEqual(3, meck:num_calls(verification_code_ds, purge_expired, '_'))
    after
        application:unset_env(imboy, credential_retention_enabled),
        application:unset_env(imboy, credential_retention_batch_limit),
        meck:unload([verification_code_ds])
    end.

%% dry-run：只统计不删除
dry_run_counts_without_delete_test() ->
    _ = catch meck:unload([verification_code_ds]),
    ok = meck:new(verification_code_ds, [no_link]),
    try
        application:set_env(imboy, credential_retention_enabled, true),
        application:set_env(imboy, credential_retention_dry_run, true),
        meck:expect(verification_code_ds, count_expired, 1, fun(_Cutoff) -> {ok, 7} end),
        meck:expect(verification_code_ds, purge_expired, 2, fun(_, _) ->
            erlang:error(should_not_delete_in_dry_run)
        end),
        ?assertEqual({noreply, #{}}, credential_retention_worker:handle_info(run, #{})),
        ?assertEqual(1, meck:num_calls(verification_code_ds, count_expired, '_')),
        ?assertEqual(0, meck:num_calls(verification_code_ds, purge_expired, '_'))
    after
        application:unset_env(imboy, credential_retention_enabled),
        application:unset_env(imboy, credential_retention_dry_run),
        meck:unload([verification_code_ds])
    end.

%% 失败只 WARN 不崩，tick 已重排
enabled_failure_warns_and_continues_test() ->
    _ = catch meck:unload([verification_code_ds]),
    ok = meck:new(verification_code_ds, [no_link]),
    try
        application:set_env(imboy, credential_retention_enabled, true),
        meck:expect(verification_code_ds, purge_expired, 2, fun(_, _) ->
            {error, <<"internal_error">>}
        end),
        ?assertEqual({noreply, #{}}, credential_retention_worker:handle_info(run, #{})),
        ?assertEqual(1, meck:num_calls(verification_code_ds, purge_expired, '_'))
    after
        application:unset_env(imboy, credential_retention_enabled),
        meck:unload([verification_code_ds])
    end.

%% 未知消息不影响 worker
ignores_unknown_info_test() ->
    ?assertEqual({noreply, #{}}, credential_retention_worker:handle_info(some_other_msg, #{})).

%% cutoff 随 retention 天数左移（fake clock：固定 millisecond，回转断言 3 天窗口）
cutoff_shifts_with_retention_days_test() ->
    _ = catch meck:unload([elib_dt, verification_code_ds]),
    ok = meck:new(elib_dt, [no_link, passthrough]),
    ok = meck:new(verification_code_ds, [no_link]),
    try
        application:set_env(imboy, credential_retention_enabled, true),
        application:set_env(imboy, credential_retention_verification_code_days, 3),
        FixedMs = 1759852800000,
        meck:expect(elib_dt, millisecond, 0, fun() -> FixedMs end),
        put(captured, none),
        meck:expect(verification_code_ds, purge_expired, 2, fun(Cutoff, Limit) ->
            put(captured, {Cutoff, Limit}),
            {ok, 0}
        end),
        _ = credential_retention_worker:handle_info(run, #{}),
        {Cutoff, _Limit} = get(captured),
        ?assert(is_binary(Cutoff)),
        %% rfc3339 回转后应落在 [Fixed-3d-5s, Fixed-3d] 窗口（换算取整误差容忍）
        BackMs = elib_dt:rfc3339_to(Cutoff, millisecond),
        ?assert(is_integer(BackMs)),
        ?assert(BackMs =< FixedMs - 3 * 86400000),
        ?assert(BackMs >= FixedMs - 3 * 86400000 - 5000)
    after
        application:unset_env(imboy, credential_retention_enabled),
        application:unset_env(imboy, credential_retention_verification_code_days),
        meck:unload([elib_dt, verification_code_ds])
    end.
