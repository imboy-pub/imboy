-module(ack_retry_cache).

%% ACK 重试专用的节点本地 ETS 缓存。
%%
%% 这条路径只需要短生命周期的 ACK 标志与 timer ref，不需要 depcache 的依赖失效、
%% 跨进程 memo 等通用能力。独立 ETS 表让写入/失效不再经过 depcache 服务进程，
%% 读取也不与通用缓存共用表，同时保留“每节点本地状态 + syn 广播取消”集群语义。

-export([init_table/0]).
-export([get/1, set/3, delete_if_value/2]).
-export([cleanup/0]).

-define(TAB, ack_retry_cache_ets).
-define(CLEANUP_TIMER_PT, ack_retry_cache_cleanup_timer).
-define(CLEANUP_INTERVAL_MS, 60000).

%% @doc 应用启动时由长驻 application master 调用，确保表不会由短命 WS 进程持有。
-spec init_table() -> ok.
init_table() ->
    ok = ensure_table(),
    ensure_cleanup_timer().

%% @doc 写入带毫秒 TTL 的值。TTL=0 用于立即过期语义。
-spec set(term(), term(), non_neg_integer()) -> ok.
set(Key, Value, TTLms) when is_integer(TTLms), TTLms >= 0 ->
    ok = ensure_table(),
    ExpiresAt = now_ms() + TTLms,
    true = ets:insert(?TAB, {Key, Value, ExpiresAt}),
    ok.

%% @doc 读取未过期值；命中过期对象时仅删除本次读到的精确对象，避免误删并发新值。
-spec get(term()) -> {ok, term()} | undefined.
get(Key) ->
    ok = ensure_table(),
    case ets:lookup(?TAB, Key) of
        [{Key, Value, ExpiresAt} = Entry] ->
            case ExpiresAt > now_ms() of
                true ->
                    {ok, Value};
                false ->
                    true = ets:delete_object(?TAB, Entry),
                    undefined
            end;
        [] ->
            undefined
    end.

%% @doc 仅当当前值仍等于 ExpectedValue 时原子删除。
%% timer 消息可能晚于后继 timer 到达；返回 true 代表本次确实消费了当前 ref。
-spec delete_if_value(term(), term()) -> boolean().
delete_if_value(Key, ExpectedValue) ->
    ok = ensure_table(),
    Deleted = ets:select_delete(?TAB, [
        {{Key, ExpectedValue, '_'}, [], [true]}
    ]),
    Deleted =:= 1.

%% @doc 清理全部已过期对象。正确性不依赖定时器，get/1 仍会惰性删除过期值。
-spec cleanup() -> ok.
cleanup() ->
    case ets:whereis(?TAB) of
        undefined ->
            ok;
        _ ->
            Now = now_ms(),
            _ = ets:select_delete(?TAB, [
                {{'_', '_', '$1'}, [{'=<', '$1', Now}], [true]}
            ]),
            ok
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?TAB) of
        undefined ->
            try
                _ = ets:new(?TAB, [
                    set,
                    public,
                    named_table,
                    {keypos, 1},
                    {write_concurrency, true},
                    {read_concurrency, true}
                ]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

-spec ensure_cleanup_timer() -> ok.
ensure_cleanup_timer() ->
    case persistent_term:get(?CLEANUP_TIMER_PT, undefined) of
        Ref when is_reference(Ref) ->
            ok;
        _ ->
            case timer:apply_interval(?CLEANUP_INTERVAL_MS, ?MODULE, cleanup, []) of
                {ok, Ref} ->
                    persistent_term:put(?CLEANUP_TIMER_PT, Ref),
                    ok;
                {error, Reason} ->
                    erlang:error({ack_retry_cache_timer_start_failed, Reason})
            end
    end.

-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).
