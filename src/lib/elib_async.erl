-module(elib_async).
-author("imboy").
-vsn("0.1.0").

-include("log.hrl").

%%%===================================================================
%%% API 导出
%%%===================================================================

-export([async/1]).
-export([async/2]).
% ponytail: backward-compat alias for async/1, remove after next full deploy
-export([run/1]).
-export([async_retry/1]).
-export([async_retry/2]).
-export([async_retry/3]).
-export([async_retry/4]).
-export([async_with_timeout/2]).
-export([async_with_callback/2]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 异步执行函数（无重试，无超时）
%%
%% 简单的异步执行，适用于快速操作
%%
%% 示例：
%% ```
%% elib_async:async(fun() ->
%%     io:format("Hello from async~n")
%% end).
%% ```
-spec async(fun(() -> term())) -> pid().
async(Fun) ->
    spawn(Fun).

% ponytail: backward-compat alias — old beams call run/1, remove after next full redeploy
run(Fun) -> async(Fun).

%% @doc 异步执行函数（带超时监控）
%%
%% 如果 Fun 执行超过 TimeoutMs，进程会被杀死
%%
%% 示例：
%% ```
%% elib_async:async(fun() ->
%%     timer:sleep(5000),
%%     success
%% end, 1000).
%% ```
-spec async(fun(() -> term()), timeout()) -> pid().
async(Fun, TimeoutMs) ->
    spawn(fun() ->
        Pid = spawn_link(fun() ->
            exit(Fun())
        end),
        receive
            {'EXIT', Pid, Result} ->
                case Result of
                    normal -> ok;
                    _ -> Result
                end
        after TimeoutMs ->
            erlang:unlink(Pid),
            erlang:exit(Pid, kill),
            ok = ?ERROR_LOG("[ASYNC_TIMEOUT] Fun 执行超时: ~pms", [TimeoutMs]),
            exit(timeout)
        end
    end).

%% @doc 异步执行函数（带重试，使用默认参数）
%%
%% 默认：重试3次，延迟1000ms，指数退避
%%
%% 示例：
%% ```
%% elib_async:async_retry(fun() ->
%%     message_ds:send_next(ToId, MsgId, MsgJson, MsLi)
%% end).
%% ```
-spec async_retry(fun(() -> term())) -> pid().
async_retry(Fun) ->
    async_retry(Fun, 3, 1000).

%% @doc 异步执行函数（带重试，自定义重试次数）
%%
%% 示例：
%% ```
%% elib_async:async_retry(Fun, 5).
%% ```
-spec async_retry(fun(() -> term()), non_neg_integer()) -> pid().
async_retry(Fun, RetryCount) ->
    async_retry(Fun, RetryCount, 1000).

%% @doc 异步执行函数（带重试，3 参数版本）
%%
%% 使用默认的 exponential（指数）退避策略
%%
%% 示例：
%% ```
%% elib_async:async_retry(Fun, 3, 1000).
%% ```
-spec async_retry(fun(() -> term()), pos_integer(), pos_integer()) -> pid().
async_retry(Fun, RetryCount, DelayMs) ->
    spawn(fun() ->
        case elib_retry:with_retry(Fun, RetryCount, DelayMs) of
            {ok, _Result} ->
                ok;
            {error, Reason} ->
                ok = ?ERROR_LOG("[ASYNC_RETRY_FAILED] 所有重试均失败: ~p", [Reason])
        end
    end).

%% @doc 异步执行函数（带重试，完整参数）
%%
%% BackoffType: fixed | exponential | linear
%%
%% 示例：
%% ```
%% elib_async:async_retry(Fun, 3, 1000, exponential).
%% ```
-spec async_retry(fun(() -> term()), pos_integer(), pos_integer(), fixed | exponential | linear) ->
    pid().
async_retry(Fun, RetryCount, DelayMs, BackoffType) ->
    spawn(fun() ->
        case elib_retry:with_retry(Fun, RetryCount, DelayMs, BackoffType) of
            {ok, _Result} ->
                ok;
            {error, Reason} ->
                ok = ?ERROR_LOG("[ASYNC_RETRY_FAILED] 所有重试均失败: ~p", [Reason])
        end
    end).

%% @doc 异步执行函数（带超时和重试）
%%
%% 示例：
%% ```
%% elib_async:async_with_timeout(Fun, 5000).
%% ```
-spec async_with_timeout(fun(() -> term()), timeout()) -> pid().
async_with_timeout(Fun, TimeoutMs) ->
    spawn(fun() ->
        case elib_retry:with_retry_and_timeout(Fun, TimeoutMs, 3) of
            {ok, Result} ->
                {ok, Result};
            {error, Reason} ->
                ok = ?ERROR_LOG("[ASYNC_TIMEOUT_RETRY_FAILED] 执行失败: ~p", [Reason])
        end
    end).

%% @doc 异步执行函数（带回调）
%%
%% 执行完成后，将结果发送给 CallbackPid
%% 消息格式：{async_result, {ok, Result}} | {error, Reason}
%%
%% 示例：
%% ```
%% elib_async:async_with_callback(fun() ->
%%     user_repo:find_by_id(Uid)
%% end, self()).
%%
%% receive
%%     {async_result, {ok, User}} -> process_user(User);
%%     {async_result, {error, Reason}} -> handle_error(Reason)
%% end.
%% ```
-spec async_with_callback(fun(() -> term()), pid()) -> pid().
async_with_callback(Fun, CallbackPid) ->
    spawn(fun() ->
        Result =
            try
                {ok, Fun()}
            catch
                _:Error:_ ->
                    {error, Error}
            end,
        CallbackPid ! {async_result, Result}
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================
