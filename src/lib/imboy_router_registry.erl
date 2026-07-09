-module(imboy_router_registry).
-behaviour(gen_server).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_router_registry - 插件路由动态注册表
%%% Plugin route dynamic registry
%%%
%%% Phase 2 切片 1：纯注册表（ETS read-optimized），不接 cowboy。
%%% 切片 2 接入 cowboy dispatch 热更（imboy_router.erl 拆分时）。
%%%
%%% 设计 / Design:
%%%   - ETS 表：read_concurrency + decentralized_counters（OTP 25+ 高读优化）
%%%   - 仅本 gen_server 写（protected），所有进程读（O(1) lookup）
%%%   - 注册操作走 gen_server:call 序列化，避免并发写竞争
%%%   - 未来 Phase 4 插件 lifecycle 在 enable/disable 时调 register/unregister
%%%
%%% 路径校验 / Path validation:
%%%   - 所有 route 的 path 必须以 /api/v{n}/<plugin_name>/ 开头（contract.md §3.3）
%%%   - 违反则 register 拒绝并返回 {error, {invalid_route_namespace, Path}}
%%%
%%% Source of truth: docs/plugin/contract.md §3.2 / §6 + roadmap P2
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-include("imboy_plugin.hrl").

%% Public API
-export([
    start_link/0,
    register/2,
    unregister/1,
    plugin_routes/1,
    all_routes/0,
    plugin_names/0,
    clear/0,
    reload_dispatch/0
]).

%% gen_server callbacks (full set per actor-model instinct)
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    format_status/1
]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 注册插件路由（覆盖式：同一 plugin 重复 register 直接覆盖）。
%% 路径强约束：所有 route.path 必须以 /api/v{n}/<plugin_name>/ 开头。
-spec register(plugin_name(), [route_spec()]) ->
    ok | {error, {invalid_route_namespace, binary()} | term()}.
register(PluginName, Routes) when is_atom(PluginName), is_list(Routes) ->
    gen_server:call(?SERVER, {register, PluginName, Routes}).

%% @doc 反注册插件路由。未注册的插件返回 ok（幂等）。
-spec unregister(plugin_name()) -> ok.
unregister(PluginName) when is_atom(PluginName) ->
    gen_server:call(?SERVER, {unregister, PluginName}).

%% @doc 查询插件 routes（O(1) ETS 读，无 gen_server hop）。
-spec plugin_routes(plugin_name()) -> [route_spec()].
plugin_routes(PluginName) when is_atom(PluginName) ->
    case ets:lookup(?TAB, PluginName) of
        [{_, Routes}] -> Routes;
        [] -> []
    end.

%% @doc 列出所有已注册的 (plugin_name, route) 对，扁平化。
-spec all_routes() -> [{plugin_name(), route_spec()}].
all_routes() ->
    ets:foldl(
        fun({Name, Routes}, Acc) ->
            [{Name, R} || R <- Routes] ++ Acc
        end,
        [],
        ?TAB
    ).

%% @doc 已注册插件名列表（O(N) 扫描 ETS keys）。
-spec plugin_names() -> [plugin_name()].
plugin_names() ->
    [Name || {Name, _} <- ets:tab2list(?TAB)].

%% @doc 清空全部注册（仅供测试与 dev）。
-spec clear() -> ok.
clear() ->
    gen_server:call(?SERVER, clear).

%% @doc 重新编译全部路由并热更新 cowboy dispatch。
%% 供外部手动调用或由 register/unregister/clear 自动触发。
%% 无 cowboy listener 时优雅降级返回 ok。
-spec reload_dispatch() -> ok.
reload_dispatch() ->
    try
        Routes = imboy_router:get_routes(),
        Dispatch = cowboy_router:compile(Routes),
        %% 对所有已知的 listener 执行热更
        lists:foreach(
            fun(Listener) ->
                try
                    cowboy:set_env(Listener, dispatch, Dispatch)
                catch
                    _:Err ->
                        logger:warning(#{
                            event => router_hot_reload_failed, listener => Listener, error => Err
                        }),
                        ok
                end
            end,
            [imboy_listener, imboy_listener_tls]
        ),
        ok
    catch
        _:Err ->
            logger:warning(#{event => router_reload_failed, error => Err}),
            ok
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% ETS 表：named, protected, read_concurrency
    %% decentralized_counters 是 OTP 25+ 优化：分散计数器以减少多核 cacheline 争用
    _ = ets:new(?TAB, [
        named_table,
        set,
        protected,
        {read_concurrency, true},
        {decentralized_counters, true}
    ]),
    {ok, #{}}.

handle_call({register, PluginName, Routes}, _From, State) ->
    case validate_routes(PluginName, Routes) of
        ok ->
            true = ets:insert(?TAB, {PluginName, Routes}),
            _ = reload_dispatch(),
            {reply, ok, State};
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call({unregister, PluginName}, _From, State) ->
    true = ets:delete(?TAB, PluginName),
    _ = reload_dispatch(),
    {reply, ok, State};
handle_call(clear, _From, State) ->
    true = ets:delete_all_objects(?TAB),
    _ = reload_dispatch(),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    %% 优雅处理未知消息 / Handle unknown messages gracefully
    {noreply, State}.

terminate(_Reason, _State) ->
    %% ETS 表随 owner 进程消亡自动清理；不主动 ets:delete
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(Status) ->
    %% 暴露表大小，不暴露完整路由内容
    Status#{state => #{ets_size => safe_ets_size()}}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 校验路径强约束：所有 route.path 必须以 /api/v{n}/<plugin_name>/ 开头。
%% 与 imboy_plugin_toml:validate_routes/1 同源（Phase 0 已实现），此处复用相同语义。
validate_routes(PluginName, Routes) ->
    NameBin = atom_to_binary(PluginName, utf8),
    Pattern = <<"^/api/v[0-9]+/", NameBin/binary, "/">>,
    lists:foldl(
        fun
            (#{path := P}, ok) when is_binary(P) ->
                case re:run(P, Pattern) of
                    {match, _} -> ok;
                    nomatch -> {error, {invalid_route_namespace, P}}
                end;
            (_, {error, _} = E) ->
                E;
            (Other, ok) ->
                {error, {invalid_route_spec, Other}}
        end,
        ok,
        Routes
    ).

safe_ets_size() ->
    case ets:info(?TAB, size) of
        undefined -> 0;
        N -> N
    end.
