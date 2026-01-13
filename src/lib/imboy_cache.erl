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

-dialyzer({nowarn_function, [cached_get/3, cached_get/4, cached_update/3, batch_get/1, batch_set/2, batch_flush/1]}).

-include("cache.hrl").
-include("log.hrl").
-include("chat.hrl").

%% gen_server API
-export([start_link/1]).
%% depcache exports
-export([memo/1, memo/2, memo/3, memo/4]).
-export([set/2, set/3, set/4, get/1, get_wait/1, get/2, get_subkey/2]).
-export([flush/0, flush/1, size/0, record_depcache_event/1]).
-export([in_process_server/0, in_process_server/1, in_process/1, flush_process_dict/0]).

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
    _ = depcache:start_link(?DEPCACHE_SERVER,
                            #{memory_max => MemoryMax,
                              callback => {?MODULE, record_depcache_event, [Args]}}),
    % 分布式缓存同步由独立的gen_server处理，不在这里启动
    {ok, self()}.

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
    depcache:memo(F, Key, MaxAge, Dep, ?DEPCACHE_SERVER).

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
    _ = case should_broadcast(Key) of
            true ->
                broadcast({set, Key, Data, MaxAge, Depend});
            false ->
                ok
        end,
    ok.

%% @doc 判断是否需要广播缓存操作
%% @param Key 缓存键
%% @returns true | false
-spec should_broadcast(term()) -> boolean().
should_broadcast(Key) ->
    application:get_env(imboy, dsync_enabled, false) andalso not is_local_cache_key(Key).

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

%% @doc 从缓存获取键值，返回数据或未找到时返回 undefined
%% @param Key 缓存键
%% @returns {ok, Data} | undefined
-spec get(term()) -> {ok, term()} | undefined.
get(Key) ->
    % ?DEBUG_LOG(["imboy_cache/get/`", Key]),
    depcache:get(Key, ?DEPCACHE_SERVER).

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
    _ = case application:get_env(imboy, dsync_enabled, false) of
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
    _ = case application:get_env(imboy, dsync_enabled, false) of
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


%% ===================================================================
%% 标准缓存模式辅助函数
%% ===================================================================

%% @doc 带缓存的获取函数（懒加载模式）
%%
%% 如果缓存命中则返回缓存值，否则执行加载函数并缓存结果
%% 适用于读多写少的场景，如用户信息、配置等
%%
%% @param Key 缓存键
%% @param LoadFun 加载函数（当缓存未命中时执行）
%% @param TTL 缓存过期时间（秒）
%% @returns 加载的数据或缓存值
%%
%% @doc 带缓存的获取函数（懒加载模式）
-spec cached_get(term(), fun(() -> term()), non_neg_integer()) -> term().
cached_get(Key, LoadFun, TTL) ->
    case imboy_cache:get(Key) of
        undefined ->
            Value = LoadFun(),
            set(Key, Value, TTL),
            Value;
        {ok, Cached} ->
            Cached
    end.

%% @doc 带缓存的获取函数（支持默认值）
%%
%% 如果缓存命中则返回缓存值，否则执行加载函数并缓存结果
%% 如果加载失败则返回默认值
%%
%% @param Key 缓存键
%% @param LoadFun 加载函数（返回 {ok, Value} | {error, Reason}）
%% @param TTL 缓存过期时间（秒）
%% @param Default 默认值
%% @returns 加载的数据、缓存值或默认值
%%
%% @doc 带缓存的获取函数（支持默认值）
-spec cached_get(term(), fun(() -> {ok, term()} | {error, any()}), non_neg_integer(), term()) -> term().
cached_get(Key, LoadFun, TTL, Default) ->
    case imboy_cache:get(Key) of
        undefined ->
            case LoadFun() of
                {ok, Value} ->
                    set(Key, Value, TTL),
                    Value;
                {error, _Reason} ->
                    Default
            end;
        {ok, Cached} ->
            Cached
    end.

%% @doc 更新缓存并刷新（写穿透模式）
%%
%% 先更新数据库，然后删除缓存，下次读取时重新加载
%% 适用于需要保证数据一致性的场景
%%
%% @param Key 缓存键
%% @param UpdateFun 更新函数（返回 {ok, UpdatedValue} | {error, Reason}）
%% @param TTL 缓存过期时间（秒，用于更新后的缓存）
%% @returns {ok, UpdatedValue} | {error, Reason}
%%
%% @doc 更新缓存并刷新（写穿透模式）
-spec cached_update(term(), fun(() -> {ok, term()} | {error, any()}), non_neg_integer()) -> {ok, term()} | {error, any()}.
cached_update(Key, UpdateFun, TTL) ->
    case UpdateFun() of
        {ok, UpdatedValue} ->
            % 删除旧缓存，下次读取时重新加载
            flush(Key),
            % 可选：立即设置新缓存（如果希望立即缓存更新后的值）
            set(Key, UpdatedValue, TTL),
            {ok, UpdatedValue};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 批量获取缓存
%%
%% 对于多个键，一次性获取所有缓存，减少缓存访问次数
%%
%% @param Keys 缓存键列表
%% @returns #{Key => Value} 键值映射
%%
%% @doc 批量获取缓存
-spec batch_get([term()]) -> map().
batch_get(Keys) when is_list(Keys) ->
    maps:from_list([{Key, case imboy_cache:get(Key) of
                                undefined -> undefined;
                                {ok, Value} -> Value
                            end} || Key <- Keys]).

%% @doc 批量设置缓存
%%
%% 一次性设置多个缓存项，所有项使用相同的TTL
%%
%% @param KeyValueMap 键值映射 #{Key => Value}
%% @param TTL 缓存过期时间（秒）
%% @returns ok
%%
%% @doc 批量设置缓存
-spec batch_set(map(), non_neg_integer()) -> ok.
batch_set(KeyValueMap, TTL) when is_map(KeyValueMap) ->
    maps:foreach(fun(Key, Value) ->
        set(Key, Value, TTL)
    end, KeyValueMap),
    ok.

%% @doc 批量刷新缓存
%%
%% 一次性删除多个缓存项
%%
%% @param Keys 缓存键列表
%% @returns ok
%%
%% @doc 批量刷新缓存
-spec batch_flush([term()]) -> ok.
batch_flush(Keys) when is_list(Keys) ->
    lists:foreach(fun(Key) ->
        flush(Key)
    end, Keys),
    ok.
