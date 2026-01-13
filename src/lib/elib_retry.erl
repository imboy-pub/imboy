-module(elib_retry).
-author("imboy").
-vsn("0.1.0").

-include("log.hrl").

%%%===================================================================
%%% API 导出
%%%===================================================================

-export([with_retry/1]).
-export([with_retry/2]).
-export([with_retry/3]).
-export([with_retry/4]).

-export([with_retry_and_timeout/3]).
-export([with_retry_and_timeout/4]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 带重试的函数执行（使用默认参数）
%% 默认：重试3次，延迟1000ms，指数退避
%%
%% 示例：
%% ```
%% elib_retry:with_retry(fun() ->
%%     risky_operation()
%% end).
%% ```
-spec with_retry(fun()) -> {ok, any()} | {error, any()}.
with_retry(Fun) ->
    with_retry(Fun, 3, 1000, exponential).

%% @doc 带重试的函数执行（自定义重试次数）
%%
%% 示例：
%% ```
%% elib_retry:with_retry(Fun, 5).
%% ```
-spec with_retry(fun(), non_neg_integer()) -> {ok, any()} | {error, any()}.
with_retry(Fun, RetryCount) ->
    with_retry(Fun, RetryCount, 1000, exponential).

%% @doc 带重试的函数执行（自定义重试次数和延迟）
%%
%% 示例：
%% ```
%% elib_retry:with_retry(Fun, 3, 2000).
%% ```
-spec with_retry(fun(), non_neg_integer(), non_neg_integer()) -> {ok, any()} | {error, any()}.
with_retry(Fun, RetryCount, DelayMs) ->
    with_retry(Fun, RetryCount, DelayMs, exponential).

%% @doc 带重试的函数执行（完整参数）
%%
%% BackoffType 选项：
%% - `fixed`: 固定延迟
%% - `exponential`: 指数退避（每次延迟翻倍）
%% - `linear`: 线性退避（每次延迟增加）
%%
%% 示例：
%% ```
%% elib_retry:with_retry(Fun, 3, 1000, exponential).
%% ```
-spec with_retry(fun(), non_neg_integer(), non_neg_integer(), fixed | exponential | linear) ->
          {ok, any()} | {error, any()}.
with_retry(Fun, RetryCount, DelayMs, BackoffType) ->
    do_retry(Fun, RetryCount, DelayMs, BackoffType, 1).

%% @doc 带超时和重试的函数执行
%%
%% Fun 执行超时后会被认为失败，进入重试流程
%%
%% 示例：
%% ```
%% elib_retry:with_retry_and_timeout(Fun, 5000, 3).
%% ```
-spec with_retry_and_timeout(fun(), non_neg_integer(), non_neg_integer()) ->
          {ok, any()} | {error, any()}.
with_retry_and_timeout(Fun, TimeoutMs, RetryCount) ->
    with_retry_and_timeout(Fun, TimeoutMs, RetryCount, 1000).

%% @doc 带超时和重试的函数执行（完整参数）
%%
%% 示例：
%% ```
%% elib_retry:with_retry_and_timeout(Fun, 5000, 3, 2000).
%% ```
-spec with_retry_and_timeout(fun(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
          {ok, any()} | {error, any()}.
with_retry_and_timeout(Fun, TimeoutMs, RetryCount, DelayMs) ->
    do_retry_with_timeout(Fun, TimeoutMs, RetryCount, DelayMs, exponential, 1).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private 重试执行（递归实现）
do_retry(_Fun, 0, _DelayMs, _BackoffType, _Attempt) ->
    ok = ?ERROR_LOG("[RETRY_EXHAUSTED] 所有可能的重试均失败"),
    {error, max_retries_exceeded};
do_retry(Fun, RetryCount, DelayMs, BackoffType, Attempt) ->
    try
        % 执行函数
        Result = Fun(),
        {ok, Result}
    catch
        Type:Error:Stacktrace ->
            ok = ?ERROR_LOG("[RETRY_FAILED] "
                       "尝试: ~p/~p, "
                       "错误: ~p:~p, "
                       "下次重试延迟: ~pms~n"
                       "堆栈: ~p~n",
                       [Attempt, RetryCount + 1, Type, Error,
                        calculate_delay(DelayMs, BackoffType, Attempt),
                        Stacktrace]),

            % 计算下次重试延迟
            NextDelay = calculate_delay(DelayMs, BackoffType, Attempt),

            % 等待后重试
            timer:sleep(NextDelay),
            do_retry(Fun, RetryCount - 1, DelayMs, BackoffType, Attempt + 1)
    end.

%% @private 带超时的重试执行
do_retry_with_timeout(_Fun, _TimeoutMs, 0, _DelayMs, _BackoffType, _Attempt) ->
    ok = ?ERROR_LOG("[RETRY_WITH_TIMEOUT_EXHAUSTED] 所有可能的重试均失败"),
    {error, max_retries_exceeded};
do_retry_with_timeout(Fun, TimeoutMs, RetryCount, DelayMs, BackoffType, Attempt) ->
    try
        % 设置超时执行
        Result = case catch erlang:spawn_monitor(fun() ->
            % 在新进程中执行，支持超时
            exit(Fun())
        end) of
            {Pid, Ref} ->
                receive
                    {'DOWN', Ref, process, Pid, Result2} ->
                        Result2
                after TimeoutMs ->
                    erlang:demonitor(Ref, [flush]),
                    erlang:exit(Pid, kill),
                    erlang:error(timeout)
                end
        end,
        {ok, Result}
    catch
        Type:Error:Stacktrace ->
            ok = ?ERROR_LOG("[RETRY_WITH_TIMEOUT_FAILED] "
                       "尝试: ~p/~p, "
                       "错误: ~p:~p, "
                       "下次重试延迟: ~pms~n"
                       "堆栈: ~p~n",
                       [Attempt, RetryCount + 1, Type, Error,
                        calculate_delay(DelayMs, BackoffType, Attempt),
                        Stacktrace]),

            % 计算下次重试延迟
            NextDelay = calculate_delay(DelayMs, BackoffType, Attempt),

            % 等待后重试
            timer:sleep(NextDelay),
            do_retry_with_timeout(Fun, TimeoutMs, RetryCount - 1,
                                DelayMs, BackoffType, Attempt + 1)
    end.

%% @private 计算延迟时间
%% fixed: 固定延迟
%% linear: 线性增长 (Delay * Attempt)
%% exponential: 指数增长 (Delay * 2^(Attempt-1))
-spec calculate_delay(non_neg_integer(), fixed | exponential | linear, pos_integer()) ->
          non_neg_integer().
calculate_delay(BaseDelay, fixed, _Attempt) ->
    BaseDelay;
calculate_delay(BaseDelay, linear, Attempt) ->
    BaseDelay * Attempt;
calculate_delay(BaseDelay, exponential, Attempt) ->
    BaseDelay * trunc(math:pow(2, Attempt - 1)).
