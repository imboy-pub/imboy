-module(elib_retry_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% 基础测试
%%%===================================================================

%% @doc 成功场景（无需重试）
success_test() ->
    Fun = fun() -> success_result end,
    ?assertEqual({ok, success_result}, elib_retry:with_retry(Fun)).

%% @doc 重试后成功
retry_success_test() ->
    AttemptsRef = make_ref(),
    ets:new(AttemptsRef, [set, private, named_table]),

    Fun = fun() ->
        Current = case ets:lookup(AttemptsRef, count) of
            [] -> 0;
            [{count, N}] -> N
        end,
        ets:insert(AttemptsRef, {count, Current + 1}),

        case Current of
            2 -> success_after_2_retries;
            _ -> erlang:error(not_yet)
        end
    end,

    Result = elib_retry:with_retry(Fun, 5, 10),
    ?assertEqual({ok, success_after_2_retries}, Result),

    ets:delete(AttemptsRef).

%% @doc 重试次数用尽后失败
retry_failed_test() ->
    Fun = fun() -> erlang:error(always_fail) end,
    Result = elib_retry:with_retry(Fun, 2, 10),
    ?assertEqual({error, max_retries_exceeded}, Result).

%% @doc 测试固定延迟策略
fixed_backoff_test() ->
    StartTime = erlang:monotonic_time(millisecond),

    Fun = fun() ->
        CurrentTime = erlang:monotonic_time(millisecond),
        case CurrentTime - StartTime of
            N when N < 50 -> erlang:error(too_soon);
            _ -> success
        end
    end,

    Result = elib_retry:with_retry(Fun, 3, 50, fixed),
    ?assertEqual({ok, success}, Result).

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
different_error_types_test() ->
    TestCases = [
        {fun() -> erlang:error(badarg) end, badarg},
        {fun() -> throw(my_exception) end, my_exception},
        {fun() -> exit(normal) end, normal}
    ],

    lists:foreach(fun({Fun, _ExpectedError}) ->
        % 应该重试后失败
        Result = elib_retry:with_retry(Fun, 1, 10),
        ?assertEqual({error, max_retries_exceeded}, Result)
    end, TestCases).

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
timeout_and_retry_test() ->
    AttemptsRef = make_ref(),
    ets:new(AttemptsRef, [set, private, named_table]),

    Fun = fun() ->
        Current = case ets:lookup(AttemptsRef, count) of
            [] -> 0;
            [{count, N}] -> N
        end,
        ets:insert(AttemptsRef, {count, Current + 1}),

        case Current of
            0 -> timer:sleep(200), erlang:error(timeout);  % 第一次超时
            _ -> success  % 第二次成功
        end
    end,

    Result = elib_retry:with_retry_and_timeout(Fun, 100, 3, 50),
    ?assertEqual({ok, success}, Result),

    ets:delete(AttemptsRef).

%% @doc 测试零重试次数
zero_retry_count_test() ->
    Fun = fun() ->
        erlang:error(should_not_retry)
    end,
    Result = elib_retry:with_retry(Fun, 0, 100),
    ?assertEqual({error, max_retries_exceeded}, Result).

%% @doc 测试零延迟重试
zero_delay_retry_test() ->
    AttemptsRef = make_ref(),
    ets:new(AttemptsRef, [set, private, named_table]),

    Fun = fun() ->
        Current = case ets:lookup(AttemptsRef, count) of
            [] -> 0;
            [{count, N}] -> N
        end,
        ets:insert(AttemptsRef, {count, Current + 1}),

        case Current of
            2 -> success;
            _ -> erlang:error(retry_me)
        end
    end,

    Result = elib_retry:with_retry(Fun, 5, 0),
    ?assertEqual({ok, success}, Result),

    ets:delete(AttemptsRef).

%% @doc 测试非常大的重试次数
very_large_retry_count_test() ->
    Fun = fun() ->
        success_immediately
    end,
    % 第一次就成功，不应该重试
    Result = elib_retry:with_retry(Fun, 1000000, 1000),
    ?assertEqual({ok, success_immediately}, Result).

%% @doc 测试超时为0的情况
zero_timeout_test() ->
    Fun = fun() ->
        timer:sleep(10),  % 即使很短的延迟也应该超时
        success
    end,
    Result = elib_retry:with_retry_and_timeout(Fun, 0, 1),
    ?assertEqual({error, max_retries_exceeded}, Result).

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
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              % 模拟数据库前两次失败，第三次成功
              DbAttempts = make_ref(),
              ets:new(DbAttempts, [set, private, named_table]),

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

              ets:delete(DbAttempts)
          end)]
     end}.

%% @doc 模拟真实场景：API 调用重试
api_retry_simulation_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              % 模拟 API 第一次返回 500，第二次成功
              ApiAttempts = make_ref(),
              ets:new(ApiAttempts, [set, private, named_table]),

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
              ?assertEqual({ok, {ok, #{status => 200, body => <<"success">>}}}, Result),

              ets:delete(ApiAttempts)
          end)]
     end}.
