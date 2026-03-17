-module(elib_async_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% async/1 测试
%%%===================================================================

%% @doc 测试基本异步执行
async_basic_test() ->
    Parent = self(),

    Pid = elib_async:async(fun() ->
        Parent ! {async_result, hello}
    end),

    ?assert(is_pid(Pid)),

    receive
        {async_result, hello} -> ok
    after 1000 ->
        ?assert(false, timeout)
    end.

%% @doc 测试异步执行返回值
async_return_value_test() ->
    Parent = self(),

    elib_async:async(fun() ->
        Parent ! {result, 42}
    end),

    receive
        {result, 42} -> ok
    after 1000 ->
        ?assert(false, timeout)
    end.

%%%===================================================================
%%% async_retry/1-3 测试
%%%===================================================================

%% @doc 测试异步重试成功
async_retry_success_test() ->
    Parent = self(),

    Fun = fun() ->
        Parent ! {retry_executed, success_after_retry},
        success_after_retry
    end,

    Pid = elib_async:async_retry(Fun, 5, 10),
    ?assert(is_pid(Pid)),

    receive
        {retry_executed, success_after_retry} -> ok
    after 500 ->
        ?assert(false, timeout)
    end,
    ok.

%% @doc 测试异步重试失败
async_retry_failed_test() ->
    Fun = fun() -> erlang:error(always_fail) end,
    Pid = elib_async:async_retry(Fun, 2, 10),
    ?assert(is_pid(Pid)),

    receive
        after 500 -> ok  % 等待异步完成
    end.

%% @doc 测试默认参数的异步重试
async_retry_default_test() ->
    Parent = self(),

    Pid = elib_async:async_retry(fun() ->
        Parent ! retry_success,
        retry_success
    end),

    ?assert(is_pid(Pid)),

    receive
        retry_success -> ok
    after 2000 ->
        ?assert(false, timeout)
    end.

%%%===================================================================
%%% async/2 测试（带超时）
%%%===================================================================

%% @doc 测试带超时的异步执行（成功）
async_with_timeout_success_test() ->
    Parent = self(),

    Pid = elib_async:async(fun() ->
        Parent ! {async_result, success}
    end, 500),

    ?assert(is_pid(Pid)),

    receive
        {async_result, success} -> ok
    after 1000 ->
        ?assert(false, timeout)
    end.

%% @doc 测试带超时的异步执行（超时）
async_with_timeout_fail_test() ->
    Parent = self(),

    Pid = elib_async:async(fun() ->
        timer:sleep(1000),
        Parent ! {async_result, should_not_reach}
    end, 100),

    ?assert(is_pid(Pid)),

    receive
        {async_result, should_not_reach} -> ?assert(false, should_timeout)
    after 300 ->
        ok  % 超时是预期行为
    end.

%% @doc 测试带超时的异步执行（刚好在超时前完成）
async_with_timeout_just_in_time_test() ->
    Parent = self(),

    Pid = elib_async:async(fun() ->
        timer:sleep(80),
        Parent ! {async_result, just_in_time}
    end, 100),

    ?assert(is_pid(Pid)),

    receive
        {async_result, just_in_time} -> ok
    after 500 ->
        ?assert(false, timeout)
    end.

%%%===================================================================
%%% async_with_timeout/2 测试
%%%===================================================================

%% @doc 测试 async_with_timeout 成功
async_with_timeout_api_success_test() ->
    Parent = self(),

    Pid = elib_async:async_with_timeout(fun() ->
        Parent ! {api_result, api_success},
        api_success
    end, 500),

    ?assert(is_pid(Pid)),

    receive
        {api_result, api_success} -> ok
    after 1000 ->
        ?assert(false, timeout)
    end.

%% @doc 测试 async_with_timeout 超时后不会收到迟到消息
async_with_timeout_api_retry_test() ->
    Parent = self(),

    Fun = fun() ->
        timer:sleep(200),
        Parent ! {api_result, should_timeout},
        should_timeout
    end,

    Pid = elib_async:async_with_timeout(Fun, 100),
    ?assert(is_pid(Pid)),

    receive
        {api_result, should_timeout} -> ?assert(false, should_timeout)
    after 400 ->
        ok
    end.

%%%===================================================================
%%% async_with_callback/2 测试
%%%===================================================================

%% @doc 测试异步回调成功
async_with_callback_success_test() ->
    Parent = self(),

    elib_async:async_with_callback(fun() ->
        callback_result
    end, Parent),

    receive
        {async_result, {ok, callback_result}} -> ok;
        {async_result, {error, _}} -> ?assert(false, unexpected_error)
    after 1000 ->
        ?assert(false, timeout)
    end.

%% @doc 测试异步回调失败
async_with_callback_error_test() ->
    Parent = self(),

    elib_async:async_with_callback(fun() ->
        erlang:error(test_error)
    end, Parent),

    receive
        {async_result, {error, test_error}} -> ok;
        {async_result, {ok, _}} -> ?assert(false, unexpected_success)
    after 1000 ->
        ?assert(false, timeout)
    end.

%%%===================================================================
%%% 边界情况测试
%%%===================================================================

%% @doc 测试返回不同类型的值
async_return_different_types_test_() ->
    TestCases = [
        {fun() -> 42 end, 42},
        {fun() -> <<"binary">> end, <<"binary">>},
        {fun() -> [1, 2, 3] end, [1, 2, 3]},
        {fun() -> #{key => value} end, #{key => value}},
        {fun() -> {tuple, ok} end, {tuple, ok}}
    ],

    lists:map(fun({Fun, Expected}) ->
        ?_test(begin
            Parent = self(),
            elib_async:async(fun() ->
                Parent ! {result, Fun()}
            end),

            receive
                {result, Result} -> ?assertEqual(Expected, Result)
            after 1000 ->
                ?assert(false, timeout)
            end
        end)
    end, TestCases).

%% @doc 测试异步函数抛出不同类型的异常
async_throw_different_exceptions_test_() ->
    TestCases = [
        {fun() -> erlang:error(badarg) end, badarg},
        {fun() -> throw(my_exception) end, my_exception},
        {fun() -> exit(normal) end, normal}
    ],

    lists:map(fun({Fun, ExpectedError}) ->
        ?_test(begin
            Parent = self(),
            elib_async:async_with_callback(Fun, Parent),

            receive
                {async_result, {error, Error}} -> ?assertEqual(ExpectedError, Error);
                {async_result, {ok, _}} -> ?assert(false, should_have_failed)
            after 1000 ->
                ?assert(false, timeout)
            end
        end)
    end, TestCases).

%% @doc 测试立即返回的函数
async_immediate_return_test() ->
    Parent = self(),

    elib_async:async(fun() ->
        Parent ! {result, immediate}
    end),

    receive
        {result, immediate} -> ok
    after 100 ->
        ?assert(false, should_be_immediate)
    end.

%% @doc 测试超时为0的情况
async_zero_timeout_test() ->
    Parent = self(),

    Pid = elib_async:async(fun() ->
        timer:sleep(10),
        Parent ! {result, delayed}
    end, 0),

    ?assert(is_pid(Pid)),

    receive
        {result, delayed} -> ?assert(false, should_timeout)
    after 50 ->
        ok  % 超时是预期行为
    end.

%% @doc 测试长超时时间
async_long_timeout_test() ->
    Parent = self(),

    Pid = elib_async:async(fun() ->
        timer:sleep(100),
        Parent ! {result, long_timeout_ok}
    end, 5000),

    ?assert(is_pid(Pid)),

    receive
        {result, long_timeout_ok} -> ok
    after 1000 ->
        ?assert(false, timeout)
    end.

%% @doc 测试在立即成功时会异步执行函数体
async_retry_negative_count_test() ->
    Parent = self(),

    Pid = elib_async:async_retry(fun() ->
        Parent ! retry_negative_count_executed,
        ok
    end, -1, 10),
    ?assert(is_pid(Pid)),

    receive
        retry_negative_count_executed -> ok
    after 500 ->
        ?assert(false, timeout)
    end,
    ok.

%% @doc 测试零延迟重试
async_retry_zero_delay_test() ->
    Parent = self(),

    Fun = fun() ->
        Parent ! {retry_zero_delay, success},
        success
    end,

    Pid = elib_async:async_retry(Fun, 3, 0),
    ?assert(is_pid(Pid)),

    receive
        {retry_zero_delay, success} -> ok
    after 500 ->
        ?assert(false, timeout)
    end,
    ok.

%% @doc 测试非常大的超时值
async_very_large_timeout_test() ->
    Parent = self(),

    Pid = elib_async:async(fun() ->
        Parent ! {result, quick}
    end, 999999999),  % 非常大的超时值

    ?assert(is_pid(Pid)),

    receive
        {result, quick} -> ok
    after 100 ->
        ok  % 应该快速完成
    end.

%%%===================================================================
%%% 并发测试
%%%===================================================================

%% @doc 测试多个异步任务并发执行
async_concurrent_tasks_test() ->
    Parent = self(),
    TaskCount = 10,

    Pids = [elib_async:async(fun() ->
        timer:sleep(10),
        Parent ! {task_result, TaskId}
    end) || TaskId <- lists:seq(1, TaskCount)],

    ?assertEqual(TaskCount, length(Pids)),

    Results = receive_results(TaskCount, []),
    ?assertEqual(TaskCount, length(Results)),
    ?assertEqual(lists:sort(lists:seq(1, TaskCount)), lists:sort(Results)).

%% @doc 测试多个异步重试任务并发执行
async_concurrent_retry_tasks_test() ->
    Parent = self(),
    TaskCount = 5,

    [elib_async:async_retry(fun() ->
        Parent ! {retry_task, ok}
    end, 2, 10) || _ <- lists:seq(1, TaskCount)],

    Results = receive_retry_results(TaskCount, []),
    ?assertEqual(TaskCount, length(Results)).

%% @doc 测试多个带回调的异步任务
async_concurrent_callback_tasks_test() ->
    Parent = self(),
    TaskCount = 5,

    [elib_async:async_with_callback(fun() ->
        {ok, ok}
    end, Parent) || _ <- lists:seq(1, TaskCount)],

    Results = receive_callback_results(TaskCount, []),
    ?assertEqual(TaskCount, length(Results)).

%% @private 辅助函数：接收多个任务结果
receive_results(0, Acc) ->
    Acc;
receive_results(Count, Acc) ->
    receive
        {task_result, N} ->
            receive_results(Count - 1, [N | Acc])
    after 2000 ->
        Acc
    end.

%% @private 辅助函数：接收多个重试任务结果
receive_retry_results(0, Acc) ->
    Acc;
receive_retry_results(Count, Acc) ->
    receive
        {retry_task, N} ->
            receive_retry_results(Count - 1, [N | Acc])
    after 2000 ->
        Acc
    end.

%% @private 辅助函数：接收多个回调结果
receive_callback_results(0, Acc) ->
    Acc;
receive_callback_results(Count, Acc) ->
    receive
        {async_result, {ok, N}} ->
            receive_callback_results(Count - 1, [N | Acc])
    after 2000 ->
        Acc
    end.

%%%===================================================================
%%% 集成测试
%%%===================================================================

%% @doc 模拟真实场景：异步发送消息
async_send_message_simulation_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              Parent = self(),
              MsgId = <<"msg123">>,
              ToId = 12345,

              % 模拟发送消息
              elib_async:async_retry(fun() ->
                  % 模拟消息发送
                  Parent ! {msg_sent, MsgId, ToId},
                  ok
              end, 3, 50),

              receive
                  {msg_sent, MsgId, ToId} -> ok
              after 1000 ->
                  ?assert(false, timeout)
              end
          end)]
     end}.

%% @doc 模拟真实场景：异步带回调处理结果
async_callback_simulation_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(begin
              Parent = self(),
              Uid = 123,

              % 模拟异步查找用户并回调
              elib_async:async_with_callback(fun() ->
                  % 模拟数据库查询
                  {ok, #{id => Uid, name => <<"Alice">>}}
              end, Parent),

              receive
                  {async_result, {ok, {ok, User}}} ->
                      ?assertEqual(Uid, maps:get(id, User));
                  {async_result, {error, _}} ->
                      ?assert(false, unexpected_error)
              after 1000 ->
                  ?assert(false, timeout)
              end
          end)]
     end}.
