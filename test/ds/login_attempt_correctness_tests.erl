-module(login_attempt_correctness_tests).
-include_lib("eunit/include/eunit.hrl").

%% 独立 VM 可运行；全量轨道临时让位，结束恢复原应用 owner。
with_owner(Env, Fun) ->
    Config = maps:merge(limiter_test_support:default_env(), Env),
    OldEnv = [{K, application:get_env(imboy, K)} || K <- maps:keys(Config)],
    HadOwner = is_pid(whereis(login_attempt_ds)),
    case HadOwner of
        true -> ok = supervisor:terminate_child(imboy_sup, login_attempt_ds);
        false -> ok
    end,
    ok = meck:new(login_attempt_clock, [passthrough, no_link]),
    Clock = ets:new(limiter_regression_clock, [public]),
    ets:insert(Clock, {now, 1000}),
    ok = meck:expect(login_attempt_clock, now_ms, fun() -> ets:lookup_element(Clock, now, 2) end),
    maps:foreach(fun(K, V) -> application:set_env(imboy, K, V) end, Config),
    try
        start_owner(),
        Fun(fun(T) ->
            true = ets:insert(Clock, {now, T}),
            ok
        end)
    after
        stop_owner(),
        ok = meck:unload(login_attempt_clock),
        ets:delete(Clock),
        [
            case Previous of
                undefined -> application:unset_env(imboy, K);
                {ok, V} -> application:set_env(imboy, K, V)
            end
         || {K, Previous} <- OldEnv
        ],
        case HadOwner of
            true -> limiter_test_support:attach_owner();
            false -> ok
        end
    end.

start_owner() ->
    {ok, Pid} = login_attempt_ds:start_link(),
    unlink(Pid),
    case login_attempt_ds:ready() of
        true -> ok;
        false -> ok = login_attempt_ds:activate_empty(Pid)
    end,
    Pid.

stop_owner() ->
    case whereis(login_attempt_ds) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

admit_expired_row_reuses_capacity_test() ->
    with_owner(#{limiter_max_ip_entries => 2}, fun(SetTime) ->
        lists:foreach(
            fun(T) ->
                SetTime(T),
                ?assertMatch({ok, #{count := 1}}, login_attempt_ds:admit(oidc_request, <<"same">>))
            end,
            [1000, 3601000, 7201000, 10801000]
        ),
        ?assertMatch({ok, _}, login_attempt_ds:admit(oidc_request, <<"other">>)),
        ?assertEqual({error, capacity}, login_attempt_ds:admit(oidc_request, <<"third">>))
    end).

deadline_cannot_starve_behind_live_rows_test() ->
    with_owner(#{limiter_cleanup_batch => 1}, fun(SetTime) ->
        [login_attempt_ds:admit(oidc_request, integer_to_binary(N)) || N <- lists:seq(1, 100)],
        I = {user, 54321},
        {ok, _} = login_attempt_ds:begin_attempt(I, self(), 1),
        SetTime(1002),
        limiter_test_support:send_deadline_check(),
        ?assertEqual(0, ets:info(login_attempt_deadlines, size)),
        ?assertEqual({error, unavailable}, login_attempt_ds:check(I))
    end).

restart_requires_explicit_pid_bound_activation_test() ->
    with_owner(#{}, fun(_) ->
        Old = whereis(login_attempt_ds),
        {ok, _} = login_attempt_ds:failure({user, 1}),
        stop_owner(),
        {ok, New} = login_attempt_ds:start_link(),
        unlink(New),
        ?assertEqual(false, login_attempt_ds:ready()),
        ?assertEqual({error, unavailable}, login_attempt_ds:check({user, 1})),
        ?assertEqual({error, unavailable}, login_attempt_ds:admit(passport_per_ip, <<"ip">>)),
        ?assertEqual({error, unavailable}, login_attempt_ds:activate_empty(Old)),
        ?assertEqual(false, login_attempt_ds:ready()),
        ?assertEqual(ok, login_attempt_ds:activate_empty(New)),
        ?assertMatch({ok, #{count := 0}}, login_attempt_ds:check({user, 1})),
        ?assertEqual({error, unavailable}, login_attempt_ds:activate_empty(New))
    end).

stale_attempt_cannot_complete_in_approved_successor_test() ->
    with_owner(#{}, fun(_) ->
        I = {user, 11},
        {ok, Ref} = login_attempt_ds:begin_attempt(I, self(), default),
        stop_owner(),
        start_owner(),
        {ok, _} = login_attempt_ds:failure(I),
        [
            ?assertEqual({error, unavailable}, login_attempt_ds:end_attempt(Ref, Outcome))
         || Outcome <- [no_count, reset, failure]
        ],
        ?assertMatch({ok, #{count := 1}}, login_attempt_ds:check(I))
    end).

expired_accounts_reclaimed_before_degrading_test() ->
    with_owner(#{limiter_max_account_entries => 1}, fun(SetTime) ->
        {ok, _} = login_attempt_ds:failure({user, 1}),
        SetTime(1801000),
        ?assertMatch({ok, #{count := 1}}, login_attempt_ds:failure({user, 2}))
    end).

negative_clock_window_is_current_test() ->
    with_owner(#{}, fun(SetTime) ->
        SetTime(-65000),
        {ok, Snap} = login_attempt_ds:admit(passport_per_ip, <<"negative">>),
        ?assertEqual(-60000, maps:get(expires_at_ms, Snap)),
        SetTime(-60000),
        ?assertMatch({ok, #{count := 1}}, login_attempt_ds:admit(passport_per_ip, <<"negative">>))
    end).

late_rule_change_uses_new_period_test() ->
    with_owner(#{}, fun(SetTime) ->
        SetTime(55000),
        {ok, _} = login_attempt_ds:admit(passport_per_ip, <<"period-change">>),
        ok = login_attempt_ds:set_ip_rates(#{passport_per_ip => {1, 30000}}),
        %% 新规则在 60000 生效；首个请求迟到 95000，应落入 90000..120000。
        SetTime(95000),
        ?assertMatch(
            {ok, #{count := 1, expires_at_ms := 120000}},
            login_attempt_ds:admit(passport_per_ip, <<"period-change">>)
        ),
        ?assertEqual(
            {error, rate_limited},
            login_attempt_ds:admit(passport_per_ip, <<"period-change">>)
        )
    end).

terminal_failure_propagates_capacity_error_test() ->
    with_owner(#{limiter_max_account_entries => 1}, fun(_) ->
        {ok, _} = login_attempt_ds:failure({user, 1}),
        {ok, Ref} = login_attempt_ds:begin_attempt({user, 2}, self(), default),
        ?assertEqual({error, capacity_lost}, login_attempt_ds:end_attempt(Ref, failure)),
        ?assertEqual({error, unavailable}, login_attempt_ds:check({user, 2}))
    end).

credential_failure_returns_service_unavailable_test_() ->
    [
        {atom_to_list(Mode) ++ "/" ++ atom_to_list(Fault), fun() ->
            with_owner(#{limiter_max_account_entries => 1}, fun(_) ->
                credential_failure(Mode, Fault)
            end)
        end}
     || {Mode, Fault} <-
            [{M, F} || M <- [password, code], F <- [capacity, owner_down]] ++
                [
                    {password_disabled, owner_down},
                    {code_disabled, owner_down},
                    {password, owner_replaced},
                    {code, owner_replaced}
                ]
    ].

credential_failure(Mode, Fault) ->
    Modules = [user_ds, elib_password, verification_code_ds, token_ds],
    [ok = meck:new(M, [passthrough, no_link]) || M <- Modules],
    Status =
        case Mode of
            password_disabled -> 0;
            code_disabled -> 0;
            _ -> 1
        end,
    User = #{<<"id">> => 87654, <<"status">> => Status, <<"password">> => <<"synthetic-hash">>},
    Inject = fun() ->
        case Fault of
            capacity ->
                {ok, _} = login_attempt_ds:failure({user, 999});
            owner_down ->
                stop_owner();
            owner_replaced ->
                stop_owner(),
                start_owner(),
                {ok, _} = login_attempt_ds:failure({user, 87654})
        end,
        case {Mode, Fault} of
            {password, owner_replaced} -> {ok, true};
            {code, owner_replaced} -> {ok, <<"verified">>};
            {password_disabled, _} -> {ok, true};
            {code_disabled, _} -> {ok, <<"verified">>};
            _ -> {error, <<"synthetic bad credential">>}
        end
    end,
    try
        ok = meck:expect(user_ds, find_by_account, fun(_, _) -> User end),
        ok = meck:expect(elib_password, verify, fun(_, _) -> Inject() end),
        ok = meck:expect(verification_code_ds, consume, fun(_, _) -> Inject() end),
        ok = meck:expect(token_ds, encrypt_token, fun(_, _) -> error(unexpected_token) end),
        Result =
            case Mode of
                M when M =:= password; M =:= password_disabled ->
                    passport_logic:do_login(
                        <<"account">>,
                        <<"synthetic">>,
                        <<"wrong">>,
                        <<"android">>,
                        <<"synthetic-device">>
                    );
                _ ->
                    passport_logic:do_login_by_code(
                        <<"account">>,
                        <<"synthetic">>,
                        <<"wrong">>,
                        <<"android">>,
                        <<"synthetic-device">>
                    )
            end,
        ?assertEqual({error, service_unavailable}, Result),
        ?assertEqual(0, meck:num_calls(token_ds, encrypt_token, '_')),
        case Fault of
            owner_replaced ->
                ?assertMatch({ok, #{count := 1}}, login_attempt_ds:check({user, 87654}));
            _ ->
                ok
        end
    after
        [meck:unload(M) || M <- Modules]
    end.

attempt_metadata_is_bounded_and_released_test() ->
    with_owner(#{limiter_max_account_entries => 1}, fun(_) ->
        {ok, Ref} = login_attempt_ds:begin_attempt({user, 1}, self(), default),
        ?assertEqual(
            {error, unavailable}, login_attempt_ds:begin_attempt({user, 2}, self(), default)
        ),
        ?assertEqual(ok, login_attempt_ds:end_attempt(Ref, no_count)),
        ?assertEqual(0, ets:info(login_attempt_deadlines, size)),
        {ok, Next} = login_attempt_ds:begin_attempt({user, 2}, self(), default),
        ?assertEqual(ok, login_attempt_ds:end_attempt(Next, no_count))
    end).

rest_fixture_capacity_is_frozen_before_first_window_test() ->
    with_owner(#{}, fun(_) ->
        stop_owner(),
        ok = rest_fixture:ensure_ct_priv_alias(),
        start_owner(),
        %% 虚拟时间不推进，证明配置在首窗生效，不能等下一分钟碰巧通过。
        lists:foreach(
            fun(N) ->
                ?assertMatch(
                    {ok, #{count := N}},
                    login_attempt_ds:admit(passport_per_ip, <<"fixture-ip">>)
                )
            end,
            lists:seq(1, 20)
        )
    end).
