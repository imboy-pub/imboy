-module(elib_retry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

with_log_stub(TestFun) ->
    ?WITH_MECK(elib_log, [
        {'internal_log', 4, fun(_, _, _, _) -> ok end},
        {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
    ], TestFun).

%%%===================================================================
%%% 基础测试
%%%===================================================================

%% @doc 成功场景（无需重试）
success_test() ->
    Fun = fun() -> success_result end,
    ?assertEqual({ok, success_result}, elib_retry:with_retry(Fun)).

%% @doc 重试后成功
retry_success_test_() ->
    with_log_stub(fun() ->
        Attempts = ets:new(retry_success_attempts, [set, private]),
        try
            Fun = fun() ->
                Current = case ets:lookup(Attempts, count) of
                    [] -> 0;
                    [{count, N}] -> N
                end,
                ets:insert(Attempts, {count, Current + 1}),

                case Current of
                    2 -> success_after_2_retries;
                    _ -> erlang:error(not_yet)
                end
            end,

            Result = elib_retry:with_retry(Fun, 5, 10),
            ?assertEqual({ok, success_after_2_retries}, Result),
            ?assertEqual([{count, 3}], ets:lookup(Attempts, count))
        after
            ets:delete(Attempts)
        end
    end).

%% @doc 重试次数用尽后失败
retry_failed_test_() ->
    with_log_stub(fun() ->
        Fun = fun() -> erlang:error(always_fail) end,
        Result = elib_retry:with_retry(Fun, 2, 10),
        ?assertEqual({error, max_retries_exceeded}, Result)
    end).

%% @doc 测试固定延迟策略
fixed_backoff_test_() ->
    with_log_stub(fun() ->
        Attempts = ets:new(fixed_backoff_attempts, [set, private]),
        StartTime = erlang:monotonic_time(millisecond),
        try
            Fun = fun() ->
                Current = case ets:lookup(Attempts, count) of
                    [] -> 0;
                    [{count, N}] -> N
                end,
                ets:insert(Attempts, {count, Current + 1}),
                case Current of
                    0 -> erlang:error(too_soon);
                    _ -> success
                end
            end,

            Result = elib_retry:with_retry(Fun, 3, 50, fixed),
            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
            ?assertEqual({ok, success}, Result),
            ?assert(Elapsed >= 50)
        after
            ets:delete(Attempts)
        end
    end).

%% @doc 测试线性退避策略
linear_backoff_test() ->
    Fun = fun() ->
        timer:sleep(10),
        success
    end,

    ?assertEqual({ok, success}, elib_retry:with_retry(Fun, 2, 10, linear)).

%% @doc 测试指数退避策略
exponential_backoff_test() ->
    Fun = fun() ->
        timer:sleep(10),
        success
    end,

    ?assertEqual({ok, success}, elib_retry:with_retry(Fun, 2, 10, exponential)).

%% @doc 测试抛出不同类型的错误
different_error_types_test_() ->
    TestCases = [
        {fun() -> erlang:error(badarg) end, badarg},
        {fun() -> throw(my_exception) end, my_exception},
        {fun() -> exit(normal) end, normal}
    ],

    with_log_stub(fun() ->
        lists:foreach(fun({Fun, _ExpectedError}) ->
            Result = elib_retry:with_retry(Fun, 1, 10),
            ?assertEqual({error, max_retries_exceeded}, Result)
        end, TestCases)
    end).

%% @doc 测试返回值正确传递
return_value_test() ->
    Fun = fun() -> 42 end,
    ?assertEqual({ok, 42}, elib_retry:with_retry(Fun)).

%% @doc 测试复杂返回值
complex_return_value_test() ->
    Fun = fun() ->
        #{
            <<"id">> => 123,
            <<"name">> => <<"Alice">>,
            <<"nested">> => #{<<"value">> => 100}
        }
    end,

    {ok, Result} = elib_retry:with_retry(Fun),
    ?assertEqual(123, maps:get(<<"id">>, Result)),
    ?assertEqual(<<"Alice">>, maps:get(<<"name">>, Result)).

%%%===================================================================
%%% 超时测试
%%%===================================================================

%% @doc 测试带超时的成功场景
timeout_success_test() ->
    Fun = fun() ->
        timer:sleep(100),
        success
    end,

    ?assertEqual({ok, success},
                 elib_retry:with_retry_and_timeout(Fun, 200, 1)).

%% @doc 测试带超时的失败场景（超时后重试）
timeout_and_retry_test_() ->
    with_log_stub(fun() ->
        Attempts = timeout_retry_attempts,
        ets:new(Attempts, [named_table, public, set]),
        try
            Fun = fun() ->
                Current = case ets:lookup(Attempts, count) of
                    [] -> 0;
                    [{count, N}] -> N
                end,
                ets:insert(Attempts, {count, Current + 1}),

                case Current of
                    0 -> timer:sleep(200), erlang:error(timeout);
                    _ -> success
                end
            end,

            Result = elib_retry:with_retry_and_timeout(Fun, 100, 3, 50),
            ?assertEqual({ok, success}, Result),
            ?assertEqual([{count, 2}], ets:lookup(Attempts, count))
        after
            ets:delete(Attempts)
        end
    end).

%% @doc 测试零重试次数
zero_retry_count_test_() ->
    with_log_stub(fun() ->
        Fun = fun() ->
            erlang:error(should_not_retry)
        end,
        Result = elib_retry:with_retry(Fun, 0, 100),
        ?assertEqual({error, max_retries_exceeded}, Result)
    end).

%% @doc 测试零延迟重试
zero_delay_retry_test_() ->
    with_log_stub(fun() ->
        Attempts = ets:new(zero_delay_attempts, [set, private]),
        try
            Fun = fun() ->
                Current = case ets:lookup(Attempts, count) of
                    [] -> 0;
                    [{count, N}] -> N
                end,
                ets:insert(Attempts, {count, Current + 1}),

                case Current of
                    2 -> success;
                    _ -> erlang:error(retry_me)
                end
            end,

            Result = elib_retry:with_retry(Fun, 5, 0),
            ?assertEqual({ok, success}, Result),
            ?assertEqual([{count, 3}], ets:lookup(Attempts, count))
        after
            ets:delete(Attempts)
        end
    end).

%% @doc 测试非常大的重试次数
very_large_retry_count_test() ->
    Fun = fun() ->
        success_immediately
    end,
    % 第一次就成功，不应该重试
    Result = elib_retry:with_retry(Fun, 1000000, 1000),
    ?assertEqual({ok, success_immediately}, Result).

%% @doc 测试超时为0的情况
zero_timeout_test_() ->
    with_log_stub(fun() ->
        Fun = fun() ->
            timer:sleep(10),
            success
        end,
        Result = elib_retry:with_retry_and_timeout(Fun, 0, 1),
        ?assertEqual({error, max_retries_exceeded}, Result)
    end).

%% @doc 测试退避策略的正确性
backoff_strategy_test_() ->
    TestCases = [
        {fixed, "固定延迟策略"},
        {exponential, "指数退避策略"},
        {linear, "线性退避策略"}
    ],
    lists:map(fun({BackoffType, Desc}) ->
        {Desc, ?_test(begin
            Fun = fun() ->
                timer:sleep(5),
                success
            end,
            Result = elib_retry:with_retry(Fun, 2, 10, BackoffType),
            ?assertEqual({ok, success}, Result)
        end)}
    end, TestCases).

%%%===================================================================
%%% 集成测试
%%%===================================================================

%% @doc 模拟真实场景：数据库连接重试
database_retry_simulation_test_() ->
    with_log_stub(fun() ->
        DbAttempts = ets:new(database_retry_attempts, [set, private]),
        try
            DbFun = fun() ->
                case ets:lookup(DbAttempts, count) of
                    [] ->
                        ets:insert(DbAttempts, {count, 1}),
                        erlang:error(connection_refused);
                    [{count, 1}] ->
                        ets:insert(DbAttempts, {count, 2}),
                        erlang:error(connection_refused);
                    [{count, 2}] ->
                        ets:insert(DbAttempts, {count, 3}),
                        {ok, #{data => <<"result">>}}
                end
            end,

            Result = elib_retry:with_retry(DbFun, 5, 10),
            ?assertEqual({ok, {ok, #{data => <<"result">>}}}, Result),
            ?assertEqual([{count, 3}], ets:lookup(DbAttempts, count))
        after
            ets:delete(DbAttempts)
        end
    end).

%% @doc 模拟真实场景：API 调用重试
api_retry_simulation_test_() ->
    ?_test(begin
        ApiAttempts = ets:new(api_retry_attempts, [set, private]),
        try
            ApiFun = fun() ->
                case ets:lookup(ApiAttempts, count) of
                    [] ->
                        ets:insert(ApiAttempts, {count, 1}),
                        {error, 500};
                    [{count, 1}] ->
                        ets:insert(ApiAttempts, {count, 2}),
                        {ok, #{status => 200, body => <<"success">>}}
                end
            end,

            Result = elib_retry:with_retry(ApiFun, 3, 100),
            ?assertEqual({ok, {error, 500}}, Result),
            ?assertEqual([{count, 1}], ets:lookup(ApiAttempts, count))
        after
            ets:delete(ApiAttempts)
        end
    end).
