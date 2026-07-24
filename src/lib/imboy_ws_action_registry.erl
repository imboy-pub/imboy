-module(imboy_ws_action_registry).
-behaviour(gen_server).

%%%-------------------------------------------------------------------
%%% @doc
%%% WS action 动态注册表（数据驱动路由）。
%%%
%%% 与 HTTP 侧 imboy_router_registry 对齐，让 WebSocket action 可被插件扩展。
%%% 取代 message_router_logic 里 route_c2c_action / route_c2g_action 的函数头
%%% 硬编码分派——改为查表 dispatch，新增 action 无需改核心路由代码。
%%%
%%% 设计：
%%%   - ETS 表：named, protected, set, read_concurrency（高读优化）
%%%   - key = {Type, Action}，value = {Module, Function}
%%%   - 仅本 gen_server 写（protected），所有进程 O(1) 读（无 gen_server hop）
%%%   - 写操作走 gen_server:call 序列化，避免并发写竞争
%%%
%%% 启动即注册内置 action（init/1）；init_builtin/0 用于测试与重置（幂等）。
%%%
%%% @end
%%%-------------------------------------------------------------------

%% Public API
-export([
    start_link/0,
    init_builtin/0,
    register/3,
    unregister/2,
    lookup/2,
    builtin_lookup/2,
    clear/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% 内置 action → {Module, Function}（从 message_router_logic 原函数头分派迁移而来）
-define(BUILTIN_ACTIONS, [
    {<<"c2c">>, <<"message_revoke">>, msg_c2c_logic, c2c_revoke},
    {<<"c2c">>, <<"message_revoke_ack">>, msg_c2c_logic, c2c_revoke_ack},
    {<<"c2c">>, <<"message_edit">>, msg_c2c_logic, c2c_edit},
    {<<"c2c">>, <<"message_edit_ack">>, msg_c2c_logic, c2c_edit_ack},
    {<<"c2c">>, <<"message_read">>, msg_c2c_logic, c2c_read},
    {<<"c2c">>, <<"message_read_ack">>, msg_c2c_logic, c2c_read_ack},
    {<<"c2c">>, <<"message_input">>, msg_c2c_logic, c2c_input},
    {<<"c2g">>, <<"message_revoke">>, msg_c2g_logic, c2g_revoke},
    {<<"c2g">>, <<"message_revoke_ack">>, msg_c2g_logic, c2g_revoke_ack},
    {<<"c2g">>, <<"message_edit">>, msg_c2g_logic, c2g_edit},
    {<<"c2g">>, <<"message_edit_ack">>, msg_c2g_logic, c2g_edit_ack}
]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 重置为内置 action 集合（清空后重新注册）。幂等，供测试与运维重置。
-spec init_builtin() -> ok.
init_builtin() ->
    gen_server:call(?SERVER, init_builtin).

%% @doc 注册一个 WS action（插件扩展场景）。覆盖式：同 key 重复注册直接覆盖。
-spec register(binary(), binary(), {atom(), atom()}) -> ok.
register(Type, Action, {Module, Function} = MFA) when
    is_binary(Type), is_binary(Action), is_atom(Module), is_atom(Function)
->
    gen_server:call(?SERVER, {register, Type, Action, MFA}).

%% @doc 反注册一个 WS action。未注册返回 ok（幂等）。
-spec unregister(binary(), binary()) -> ok.
unregister(Type, Action) when is_binary(Type), is_binary(Action) ->
    gen_server:call(?SERVER, {unregister, Type, Action}).

%% @doc 查询 action 对应的 {Module, Function}（O(1) ETS 读，无 gen_server hop）。
%% 表未启动时（如未启动应用的单元测试）返回 undefined，调用方可回退 builtin_lookup/2。
-spec lookup(binary(), binary()) -> {ok, {atom(), atom()}} | undefined.
lookup(Type, Action) when is_binary(Type), is_binary(Action) ->
    case ets:whereis(?TAB) of
        undefined ->
            undefined;
        _ ->
            case ets:lookup(?TAB, {Type, Action}) of
                [{_, MFA}] -> {ok, MFA};
                [] -> undefined
            end
    end.

%% @doc 纯函数查内置 action（不依赖 ETS）。供 lookup 未启动时的回退，
%% 以及 message_router_logic:route_action/5 的兜底（保证单元测试不依赖应用启动）。
-spec builtin_lookup(binary(), binary()) -> {ok, {atom(), atom()}} | undefined.
builtin_lookup(Type, Action) when is_binary(Type), is_binary(Action) ->
    case lists:keyfind({Type, Action}, 1, builtin_action_list()) of
        {_, MFA} -> {ok, MFA};
        false -> undefined
    end.

%% @doc 内置 action 列表（?BUILTIN_ACTIONS 转为 {{Type,Action}, MFA} 形式）。
builtin_action_list() ->
    [{{T, A}, {M, F}} || {T, A, M, F} <- ?BUILTIN_ACTIONS].

%% @doc 清空全部注册（仅供测试与 dev）。
-spec clear() -> ok.
clear() ->
    gen_server:call(?SERVER, clear).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% ETS 表：named, protected, read_concurrency
    _ = ets:new(?TAB, [
        named_table,
        set,
        protected,
        {read_concurrency, true},
        {decentralized_counters, true}
    ]),
    ok = insert_builtin(),
    {ok, #{}}.

handle_call(init_builtin, _From, State) ->
    true = ets:delete_all_objects(?TAB),
    ok = insert_builtin(),
    {reply, ok, State};
handle_call({register, Type, Action, MFA}, _From, State) ->
    true = ets:insert(?TAB, {{Type, Action}, MFA}),
    {reply, ok, State};
handle_call({unregister, Type, Action}, _From, State) ->
    true = ets:delete(?TAB, {Type, Action}),
    {reply, ok, State};
handle_call(clear, _From, State) ->
    true = ets:delete_all_objects(?TAB),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% ETS 表随 owner 进程消亡自动清理
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 将内置 action 列表写入 ETS。
insert_builtin() ->
    true = ets:insert(?TAB, [{{T, A}, {M, F}} || {T, A, M, F} <- ?BUILTIN_ACTIONS]),
    ok.
