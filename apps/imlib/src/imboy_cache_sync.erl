%% @doc 分布式缓存同步服务器
%% 负责处理节点间的缓存同步消息
-module(imboy_cache_sync).
-behaviour(gen_server).

-include_lib("imlib/include/cache.hrl").
-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").

%% API
-export([start_link/0, broadcast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 启动分布式缓存同步服务器
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 广播消息到所有节点
broadcast(Message) ->
    syn:publish(?CACHE_SCOPE, dsync_handler, {cache_sync, Message}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc 初始化服务器
init([]) ->
    % 注册到syn
    case syn:join(?CACHE_SCOPE, dsync_handler, self(), #{}) of
        ok ->
            ?DEBUG_LOG(["Distributed cache sync server started and registered to syn"]),
            {ok, #state{}};
        {error, Reason} ->
            ?DEBUG_LOG(["Failed to register to syn:", Reason]),
            {stop, {syn_register_failed, Reason}}
    end.

%% @doc 处理同步调用
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc 处理异步调用
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 处理信息消息
handle_info({cache_sync, Message}, State) ->
    handle_sync_message(Message),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc 服务器终止处理
terminate(_Reason, _State) ->
    % 从syn中离开
    syn:leave(?CACHE_SCOPE, dsync_handler, self()),
    ok.

%% @doc 代码更改处理
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 处理同步消息
handle_sync_message({set, Key, Data, MaxAge, Depend}) ->
    % ?DEBUG_LOG({set, Key, Data, MaxAge, Depend}),
    % 在其他节点上设置缓存
    depcache:set(Key, Data, MaxAge, Depend, ?DEPCACHE_SERVER);
handle_sync_message({flush, Key}) ->
    % 在其他节点上清空指定键
    depcache:flush(Key, ?DEPCACHE_SERVER);
handle_sync_message(flush) ->
    % 在其他节点上清空所有缓存
    depcache:flush(?DEPCACHE_SERVER);
handle_sync_message(_) ->
    % 忽略未知消息
    ok.