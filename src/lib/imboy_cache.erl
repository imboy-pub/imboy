%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010  Marc Worrell, 2014 Arjan Scherpenisse
%%
%% @doc z_depcache interface file for handing depcache functions from
%% the Zotonic context.

%% Copyright 2009-2010 Marc Worrell, 2014 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% copy from https://github.com/zotonic/zotonic/blob/master/apps/zotonic_core/src/support/z_depcache.erl
%% 为了项目风格统一，并且不依赖 zotonic.hrl ，所以修改了module名称
%% The Module name was changed in order to maintain a uniform project style and not rely on zotonic.hrl

-module(imboy_cache).

-include("cache.hrl").
-include("log.hrl").
-include("chat.hrl").

%% gen_server API
-export([start_link/1]).
%% depcache exports
-export([memo/1, memo/2, memo/3, memo/4]).
-export([set/2, set/3, set/4, get/1, get_wait/1, get/2, get_subkey/2]).
-export([flush/0, flush/1, delete/1, size/0, record_depcache_event/1]).
-export([broadcast/1]).
-export([in_process_server/0, in_process_server/1, in_process/1, flush_process_dict/0]).
%% V2.2.1 新增最小接口（计划 4.2）
-export([put/4, put_delete/1, invalidate/1]).

%% @doc 启动基于站点配置的 depcache 实例
%% 启动本地 depcache 服务，分布式缓存同步由独立的 gen_server 处理
%% @param Args 配置参数列表，可包含 depcache_memory_max 等选项
%% @returns {ok, Pid} 成功启动返回进程ID
-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
    % ?DEBUG_LOG(Args),
    MemoryMax =
        case lists:keyfind(depcache_memory_max, 1, Args) of
            {_, V} ->
                V;
            false ->
                undefined
        end,
    % 启动本地depcache服务
    % 必须把真实 depcache 实例 pid 交给 supervisor：此前返回 {ok, self()}
    % （调用方=supervisor 自身），sup 记录的 child pid 失真——实例被杀后
    % sup 永远感知不到，permanent 重启形同虚设，命名表 'm:imboy_cache'
    % 消失 → 所有缓存调用 badarg。已存活的旧实例则收养其 pid 交给 sup 托管。
    case
        depcache:start_link(
            ?DEPCACHE_SERVER,
            #{
                memory_max => MemoryMax,
                callback => {?MODULE, record_depcache_event, [Args]}
            }
        )
    of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

%% @doc 缓存函数执行结果一小时
%% 如果缓存中已有结果则返回缓存值，否则执行函数并缓存结果
%% @param Fun 无参数函数，用于产生要缓存的值
%% @returns 缓存的值或函数执行结果
-spec memo(fun(() -> term())) -> term().
memo(Function) when is_function(Function, 0) ->
    depcache:memo(Function, ?DEPCACHE_SERVER).

%% @doc 根据参数类型缓存函数结果
%% 如果 Fun 是函数，则以 Key 为键缓存一小时；如果 Fun 是 {M,F,A} 元组，
%% 则从元组派生键并缓存 MaxAge 秒
%% @param Function 函数或 {Module, Function, Args} 元组
%% @param MaxAge 缓存时间（秒）
%% @returns 缓存的值或函数执行结果
-spec memo({module(), atom(), list()} | fun(() -> term()), non_neg_integer()) -> term().
memo(Function, MaxAge) when is_tuple(Function) ->
    depcache:memo(Function, undefined, MaxAge, [], ?DEPCACHE_SERVER);
%% @doc 以 Key 为键缓存函数执行结果 MaxAge 秒
%% @param F 无参数函数
%% @param Key 缓存项的键
%% @returns 缓存的值或函数执行结果
%% @equiv memo(F, Key, MaxAge, [], Server)
memo(F, Key) when is_function(F, 0) ->
    depcache:memo(F, Key, ?HOUR, [], ?DEPCACHE_SERVER).

%% @doc 以 Key 为键缓存函数执行结果 MaxAge 秒
%% @param F 无参数函数
%% @param Key 缓存项的键
%% @param MaxAge 缓存时间（秒）
%% @returns 缓存的值或函数执行结果
-spec memo(fun(() -> term()), term(), non_neg_integer()) -> term().
memo(F, Key, MaxAge) ->
    depcache:memo(F, Key, MaxAge, [], ?DEPCACHE_SERVER).

%% @doc 以 Key 为键缓存函数执行结果 MaxAge 秒，如果任何依赖项发生变化则刷新缓存
%% @param F 无参数函数
%% @param Key 缓存项的键
%% @param MaxAge 缓存时间（秒）
%% @param Dep 依赖项列表
%% @returns 缓存的值或函数执行结果
-spec memo(fun(() -> term()), term(), non_neg_integer(), list()) -> term().
memo(F, Key, MaxAge, Dep) ->
    case imboy_cache_policy:is_fenced(Key) of
        true ->
            %% 19.3 generation 栅栏路径：物理键 {cache_v2,Key,Gen} + 版本化依赖标签
            memo_fenced(F, Key, MaxAge, Dep);
        false ->
            depcache:memo(F, Key, MaxAge, Dep, ?DEPCACHE_SERVER)
    end.

%% @doc 19.3 完整并发栅栏：loader 前快照 generation，返回前验证；
%% 不匹配则最多重试一次；连续失效时直接执行权威查询且不缓存
%% （计划 7.2：无法协调时返回权威查询结果，不返回陈旧成功、不无限递归）。
%% 保留 depcache 原生 memo/get_wait 等待合并；不重写 waiter（19.3）。
-spec memo_fenced(fun(() -> term()), term(), non_neg_integer(), list()) -> term().
memo_fenced(F, Key, MaxAge, Dep) ->
    case memo_fenced_attempt(F, Key, MaxAge, Dep) of
        {ok, Value} ->
            Value;
        {stale, Snapshot} ->
            %% 最多一次重试（19.3）
            case memo_fenced_attempt(F, Key, MaxAge, Dep, Snapshot) of
                {ok, Value} ->
                    Value;
                {stale, _} ->
                    %% 连续失效：返回权威结果，不写缓存（旧物理键 TTL 有界自然过期）
                    F()
            end
    end.

-spec memo_fenced_attempt(fun(() -> term()), term(), non_neg_integer(), list()) ->
    {ok, term()} | {stale, list()}.
memo_fenced_attempt(F, Key, MaxAge, Dep) ->
    Snapshot = generation_snapshot(Key, Dep),
    Value = apply_physical_memo(F, Key, MaxAge, Snapshot),
    case generation_stable(Key, Snapshot) of
        true -> {ok, Value};
        false -> {stale, Snapshot}
    end.

-spec memo_fenced_attempt(fun(() -> term()), term(), non_neg_integer(), list(), list()) ->
    {ok, term()} | {stale, list()}.
memo_fenced_attempt(F, Key, MaxAge, Dep, _PrevSnapshot) ->
    %% 重试轮：忽略旧快照，重新取当前代（旧 loader 已被物理键隔离，无需 flush 当前键）
    memo_fenced_attempt(F, Key, MaxAge, Dep).

-spec generation_snapshot(term(), list()) -> [{term(), non_neg_integer(), non_neg_integer()}].
generation_snapshot(Key, Dep) ->
    [{Key, direct, imboy_cache_sync:generation(Key)}] ++
        [{D, dep, imboy_cache_sync:dep_generation(D)} || D <- Dep].

-spec generation_stable(term(), [{term(), non_neg_integer(), non_neg_integer()}]) -> boolean().
generation_stable(_Key, Snapshot) ->
    lists:all(
        fun
            ({K, direct, G}) -> imboy_cache_sync:generation(K) =:= G;
            ({D, dep, G}) -> imboy_cache_sync:dep_generation(D) =:= G
        end,
        Snapshot
    ).

-spec apply_physical_memo(
    fun(() -> term()),
    term(),
    non_neg_integer(),
    [{term(), non_neg_integer(), non_neg_integer()}]
) -> term().
apply_physical_memo(F, _Key, MaxAge, [{Key, direct, G} | Deps]) ->
    PhysKey = {cache_v2, Key, G},
    PhysDeps = [{cache_v2_dep, D, GD} || {D, dep, GD} <- Deps],
    depcache:memo(F, PhysKey, MaxAge, PhysDeps, ?DEPCACHE_SERVER).

%% @doc 添加键值对到缓存，保存3600秒，无依赖项
%% @param Key 缓存键
%% @param Data 要缓存的数据
%% @returns ok
-spec set(term(), term()) -> ok.
set(Key, Data) ->
    set(Key, Data, ?HOUR, [], ?DEPCACHE_SERVER).

%% @doc 添加键值对到缓存，保存 MaxAge 秒，无依赖项
%% @param Key 缓存键
%% @param Data 要缓存的数据
%% @param MaxAge 缓存时间（秒）
%% @returns ok
%% 示例: imboy_cache:set("test", 1, 3), imboy_cache:get("test")
-spec set(term(), term(), non_neg_integer()) -> ok.
set(Key, Data, MaxAge) ->
    % ?DEBUG_LOG(["imboy_cache/set/3", Key, "; ", Data, "; ", MaxAge]),
    set(Key, Data, MaxAge, [], ?DEPCACHE_SERVER).

%% @doc 添加键值对到缓存，保存 MaxAge 秒，检查依赖项
%% @param Key 缓存键
%% @param Data 要缓存的数据
%% @param MaxAge 缓存时间（秒）
%% @param Depend 依赖项列表
%% @returns ok
-spec set(term(), term(), non_neg_integer(), list()) -> ok.
set(Key, Data, MaxAge, Depend) ->
    set(Key, Data, MaxAge, Depend, ?DEPCACHE_SERVER).

%% @doc 设置缓存项，支持本地缓存和分布式缓存
%% 首先设置本地缓存，然后检查是否需要广播到其他节点
%% @param Key 缓存键
%% @param Data 要缓存的数据
%% @param MaxAge 缓存时间（秒）
%% @param Depend 依赖项列表
%% @param Server 缓存服务器标识
%% @returns ok
-spec set(term(), term(), non_neg_integer(), list(), atom()) -> ok.
set(Key, Data, MaxAge, Depend, Server) ->
    % 首先设置本地缓存
    depcache:set(Key, Data, MaxAge, Depend, Server),

    % 检查是否需要广播到其他节点
    _ =
        case should_broadcast(Key) of
            true ->
                broadcast({set, Key, Data, MaxAge, Depend});
            false ->
                ok
        end,
    ok.

%% @doc 判断是否需要广播缓存操作（W1-05/W2 策略化）：
%% 仅未分类的 legacy Key 在 dsync_enabled 时保持旧广播行为（计划 3.3 迁移期兼容）；
%% local / invalidate / blocked（QR 等）Key 一律不广播——
%% 普通数据库查询缓存填充不引发无意义广播（计划 2.1、AC-09），
%% 敏感数据不再经广播复制到其他节点（19.5、W1-05）。
%% 失效传播不在此路径：数据变更后的跨节点失效统一走 invalidate/1。
-spec should_broadcast(term()) -> boolean().
should_broadcast(Key) ->
    application:get_env(imboy, dsync_enabled, false) andalso
        not is_local_cache_key(Key) andalso
        imboy_cache_policy:policy(Key) =:= legacy.

%% @doc 检查是否为本地缓存键
%% @param Key 缓存键
%% @returns true | false
-spec is_local_cache_key(term()) -> boolean().
is_local_cache_key({local_cache, _}) ->
    true;
is_local_cache_key(_) ->
    false.

%% @doc 从缓存获取键值，如果键不存在则锁定条目让调用进程插入值
%% 其他请求该键的进程将等待直到键被更新并接收新值
%% @param Key 缓存键
%% @returns {ok, Data} | undefined
-spec get_wait(term()) -> {ok, term()} | undefined.
get_wait(Key) ->
    depcache:get_wait(Key, ?DEPCACHE_SERVER).

%% @doc V2.2.1 统一失效入口（计划 2.1/4.2/7.2）：
%% 数据变更（事务 COMMIT）成功后调用；本地清理 + generation 推进 + 跨节点失效通知。
%% ROLLBACK 后不得调用（7.5：回滚不广播未提交的数据状态）。
-spec invalidate(term()) -> ok.
invalidate(Key) ->
    imboy_cache_sync:invalidate(Key).

%% @doc V2.2.1 策略化写入（计划 4.2）。
%% put(Key, Value, TTL, local)      -> 只写本地 depcache，不广播；
%% put(Key, Value, TTL, replicate)  -> 校验（白名单/大小/TTL/敏感性，fail-closed）后
%%                                     分配 Lamport revision、本地写、发布 V2 SET；
%%                                     未就绪/未授权/超限返回 {error, Reason}。
%% 不修改现有 set/4 的第 4 参数含义（那是 depcache 依赖列表，计划 4.2）。
-spec put(term(), term(), non_neg_integer(), local | replicate) ->
    ok | {error, term()}.
put(Key, Value, TTL, local) ->
    case imboy_cache_policy:validate_put(Key, Value, TTL) of
        ok ->
            _ = depcache:set(Key, Value, TTL, [], ?DEPCACHE_SERVER),
            ok;
        {error, _} = Err ->
            Err
    end;
put(Key, Value, TTL, replicate) ->
    case imboy_cache_policy:validate_replicate(Key, Value, TTL) of
        ok ->
            imboy_cache_sync:publish_set(Key, Value, TTL);
        {error, _} = Err ->
            Err
    end.

%% @doc replicate 版本化删除（计划 4.3）：带版本删除事件 + 短期墓碑。
%% 普通键仍用 delete/1（flush 别名，legacy 语义）。
-spec put_delete(term()) -> ok | {error, term()}.
put_delete(Key) ->
    imboy_cache_sync:publish_delete(Key).

%% @doc 从缓存获取键值，返回数据或未找到时返回 undefined
%% replicate Key（rindex 登记）额外经过版本/墓碑/同步状态校验（计划 5.5）：
%% 墓碑或未 READY 时不得返回未校验旧值。
-spec get(term()) -> {ok, term()} | undefined.
get(Key) ->
    case imboy_cache_sync:is_replicated(Key) of
        true ->
            case imboy_cache_sync:replicate_state(Key) of
                deleted -> undefined;
                unverified -> undefined;
                _ -> depcache:get(Key, ?DEPCACHE_SERVER)
            end;
        false ->
            depcache:get(Key, ?DEPCACHE_SERVER)
    end.

%% @doc 从缓存中获取子键值
%% @param Key 缓存键
%% @param SubKey 子键
%% @returns {ok, Data} | undefined
-spec get_subkey(term(), term()) -> {ok, term()} | undefined.
get_subkey(Key, SubKey) ->
    depcache:get_subkey(Key, SubKey, ?DEPCACHE_SERVER).

%% @doc 从缓存获取键值，支持子键
%% @param Key 缓存键
%% @param SubKey 子键
%% @returns {ok, Data} | undefined
-spec get(term(), term()) -> {ok, term()} | undefined.
get(Key, SubKey) ->
    depcache:get(Key, SubKey, ?DEPCACHE_SERVER).

%% @doc 清空所有缓存中的键
%% 支持分布式缓存同步，如果启用了 dsync 则广播到其他节点
%% @returns ok
-spec flush() -> ok.
flush() ->
    depcache:flush(?DEPCACHE_SERVER),
    % 检查是否启用分布式缓存
    _ =
        case application:get_env(imboy, dsync_enabled, false) of
            true ->
                % 广播到其他节点
                broadcast(flush);
            false ->
                ok
        end,
    ok.

%% @doc 清空指定键及其所有依赖键
%% 支持分布式缓存同步，如果启用了 dsync 则广播到其他节点
%% @param Key 要清空的缓存键
%% @returns ok
-spec flush(term()) -> ok.
flush(Key) ->
    depcache:flush(Key, ?DEPCACHE_SERVER),
    % 检查是否启用分布式缓存
    _ =
        case application:get_env(imboy, dsync_enabled, false) of
            true ->
                % 广播到其他节点
                broadcast({flush, Key});
            false ->
                ok
        end,
    ok.

%% @doc 返回所有存储项的总内存大小
%% @returns 内存大小（字节）
-spec size() -> non_neg_integer().
size() ->
    depcache:size(?DEPCACHE_SERVER).

%% @doc 检查是否使用本地进程字典缓存
%% @returns boolean()
-spec in_process_server() -> boolean().
in_process_server() ->
    depcache:in_process_server(?DEPCACHE_SERVER).

%% @doc 检查指定服务器是否使用本地进程字典缓存
%% @param Server 服务器标识
%% @returns boolean()
-spec in_process_server(atom()) -> boolean().
in_process_server(Server) ->
    depcache:in_process_server(Server).

%% @doc 启用或禁用使用进程字典进行进程内缓存
%% @param Flag true 启用 | false 禁用
%% @returns 'true' | 'false' | 'undefined' （实际的 depcache 返回值）
-spec in_process(boolean()) -> true | false | undefined.
in_process(Flag) ->
    depcache:in_process(Flag).

%% @doc 清空进程字典中所有缓存项
%% @returns ok | {ok, Count} （实际的 depcache 返回值）
-spec flush_process_dict() -> ok | {ok, non_neg_integer()}.
flush_process_dict() ->
    depcache:flush_process_dict().

%% @doc 记录缓存事件回调函数
%% @param Args 事件参数
%% @returns ok
-spec record_depcache_event(any()) -> ok.
record_depcache_event(Args) ->
    ok = ?DEBUG_LOG(Args),
    ok.

%% @doc 广播消息到所有节点
%% @param Message 要广播的消息
%% @returns {ok, Count} 成功广播的节点数
-spec broadcast(any()) -> {ok, non_neg_integer()}.
broadcast(Message) ->
    imboy_cache_sync:broadcast(Message).

%% @doc 删除单个缓存键（flush 的别名）
%% @param Key 要删除的缓存键
%% @returns ok
%%
%% 此函数作为 flush/1 的别名提供，使代码更具语义性
-spec delete(term()) -> ok.
delete(Key) ->
    flush(Key).
