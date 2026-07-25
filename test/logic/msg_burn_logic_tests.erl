-module(msg_burn_logic_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

%% ===================================================================
%% valid_expire_secs/1 Tests
%% ===================================================================

valid_expire_secs_undefined_test() ->
    ?assert(msg_burn_logic:valid_expire_secs(undefined)).

valid_expire_secs_zero_test() ->
    ?assert(msg_burn_logic:valid_expire_secs(0)).

valid_expire_secs_allowed_values_test() ->
    lists:foreach(
        fun(V) ->
            ?assert(msg_burn_logic:valid_expire_secs(V))
        end,
        [5, 30, 60, 300, 3600, 86400]
    ).

valid_expire_secs_disallowed_test() ->
    ?assertNot(msg_burn_logic:valid_expire_secs(10)),
    ?assertNot(msg_burn_logic:valid_expire_secs(100)),
    ?assertNot(msg_burn_logic:valid_expire_secs(999)),
    ?assertNot(msg_burn_logic:valid_expire_secs(-1)).

valid_expire_secs_non_integer_test() ->
    ?assertNot(msg_burn_logic:valid_expire_secs(<<"60">>)),
    ?assertNot(msg_burn_logic:valid_expire_secs(3.14)).

%% ===================================================================
%% calc_expire_at/2 Tests
%% ===================================================================

calc_expire_at_undefined_test() ->
    ?assertEqual(null, msg_burn_logic:calc_expire_at(<<"2026-01-01T00:00:00Z">>, undefined)).

calc_expire_at_zero_test() ->
    ?assertEqual(null, msg_burn_logic:calc_expire_at(<<"2026-01-01T00:00:00Z">>, 0)).

calc_expire_at_negative_test() ->
    ?assertEqual(null, msg_burn_logic:calc_expire_at(<<"2026-01-01T00:00:00Z">>, -5)).

%% created_at 非法（客户端不可信输入）时必须回退服务端时间，
%% 绝不能返回 null——null 会让阅后即焚静默失效
calc_expire_at_invalid_created_at_fallback_test() ->
    Before = elib_dt:millisecond(),
    Result = msg_burn_logic:calc_expire_at(<<"garbage">>, 5),
    ?assert(is_binary(Result)),
    ExpireMs = elib_dt:rfc3339_to(Result),
    ?assert(is_integer(ExpireMs)),
    ?assert(ExpireMs >= Before + 5000).

calc_expire_at_5s_test() ->
    ?WITH_MECKS([elib_dt], fun() ->
        %% CreatedAt -> 毫秒时间戳
        meck:expect(elib_dt, rfc3339_to, fun(<<"2026-01-01T00:00:00Z">>) -> 1767225600000 end),
        %% ExpireAtMs = 1767225600000 + 5000 = 1767225605000
        meck:expect(elib_dt, to_rfc3339, fun(1767225605000) -> <<"2026-01-01T00:00:05Z">> end),
        Result = msg_burn_logic:calc_expire_at(<<"2026-01-01T00:00:00Z">>, 5),
        ?assertEqual(<<"2026-01-01T00:00:05Z">>, Result)
    end).

calc_expire_at_3600s_test() ->
    ?WITH_MECKS([elib_dt], fun() ->
        meck:expect(elib_dt, rfc3339_to, fun(<<"2026-01-01T00:00:00Z">>) -> 1767225600000 end),
        meck:expect(elib_dt, to_rfc3339, fun(1767229200000) -> <<"2026-01-01T01:00:00Z">> end),
        Result = msg_burn_logic:calc_expire_at(<<"2026-01-01T00:00:00Z">>, 3600),
        ?assertEqual(<<"2026-01-01T01:00:00Z">>, Result)
    end).

%% ===================================================================
%% cleanup GenServer Tests
%% ===================================================================

cleanup_expired_c2c_test() ->
    ?WITH_MECKS([elib_pg, msg_c2c_repo], fun() ->
        meck:expect(msg_c2c_repo, tablename, fun() -> <<"public.msg_c2c">> end),
        meck:expect(elib_pg, execute, fun(_Sql, [_Now, _BatchSize]) -> {ok, 3} end),
        %% 直接调用内部函数无法测试（非导出），通过 cleanup_now 间接测试
        ok
    end).

cleanup_now_test() ->
    ?WITH_MECKS([elib_pg, elib_log, msg_c2c_repo, msg_c2g_repo, elib_dt], fun() ->
        meck:expect(msg_c2c_repo, tablename, fun() -> <<"public.msg_c2c">> end),
        meck:expect(msg_c2g_repo, tablename, fun() -> <<"public.msg_c2g">> end),
        meck:expect(elib_dt, now, fun() -> <<"2026-01-01T00:00:00Z">> end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 2} end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),

        %% 启动 GenServer
        application:set_env(imboy, msg_burn_enabled, true),
        application:set_env(imboy, msg_burn_interval, 60000),
        application:set_env(imboy, msg_burn_batch_size, 100),

        case whereis(msg_burn_logic) of
            undefined ->
                {ok, Pid} = msg_burn_logic:start_link(),
                try
                    {ok, Count} = msg_burn_logic:cleanup_now(),
                    % 2 (c2c) + 2 (c2g)
                    ?assertEqual(4, Count)
                after
                    gen_server:stop(Pid)
                end;
            _Pid ->
                %% 已经在运行（supervisor 启动），直接测试
                {ok, Count} = msg_burn_logic:cleanup_now(),
                ?assert(is_integer(Count))
        end
    end).

get_status_test() ->
    ?WITH_MECKS([elib_pg, elib_log, msg_c2c_repo, msg_c2g_repo, elib_dt], fun() ->
        meck:expect(msg_c2c_repo, tablename, fun() -> <<"public.msg_c2c">> end),
        meck:expect(msg_c2g_repo, tablename, fun() -> <<"public.msg_c2g">> end),
        meck:expect(elib_dt, now, fun() -> <<"2026-01-01T00:00:00Z">> end),
        meck:expect(elib_pg, execute, fun(_Sql, _Params) -> {ok, 0} end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _) -> ok end),
        meck:expect(elib_log, internal_log, fun(_, _, _, _, _) -> ok end),

        application:set_env(imboy, msg_burn_enabled, true),
        application:set_env(imboy, msg_burn_interval, 60000),
        application:set_env(imboy, msg_burn_batch_size, 100),

        case whereis(msg_burn_logic) of
            undefined ->
                {ok, Pid} = msg_burn_logic:start_link(),
                try
                    Status = msg_burn_logic:get_status(),
                    ?assert(is_map(Status)),
                    ?assert(maps:is_key(interval, Status)),
                    ?assert(maps:is_key(batch_size, Status)),
                    ?assert(maps:is_key(total_cleaned, Status))
                after
                    gen_server:stop(Pid)
                end;
            _Pid ->
                Status = msg_burn_logic:get_status(),
                ?assert(is_map(Status))
        end
    end).
