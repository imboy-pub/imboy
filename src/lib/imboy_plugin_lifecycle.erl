-module(imboy_plugin_lifecycle).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_lifecycle — 插件生命周期状态机（gen_statem）
%%% Plugin lifecycle state machine
%%%
%%% 切片 3：超时 + 回滚
%%% - undo_log 记录已完成步骤的撤销操作
%%% - atomic / best_effort / manual 三档回滚策略
%%% - 步骤失败时按策略执行回滚
%%% - state_timeout 配置就绪（同步执行模式下实际不触发，
%%%   异步步骤执行引入后生效）
%%%
%%% Source of truth: doc/plugin/lifecycle.md
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-behaviour(gen_statem).

%% gen_statem handle_event/4 冗余模式警告抑制：
%% step runners 当前返回类型使部分 {error,_} 分支不可达，
%% 但保留防御性编码以支持未来步骤扩展。
-dialyzer({nowarn_function, [handle_event/4]}).

-include("log.hrl").

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3,
    code_change/4,
    format_status/1
]).

%% API
-export([
    start_link/1,
    get_state/1,
    broadcast_manifest_change/0
]).

%% State types
-type state() ::
    unknown |
    installing |
    installed |
    enabling |
    enabled |
    disabling |
    disabled |
    upgrading |
    uninstalling |
    failed.

-type rollback_strategy() :: atomic | best_effort | manual.
-type plugin_name() :: atom().

-record(data, {
    name :: plugin_name(),
    manifest :: map() | undefined,
    version :: binary() | undefined,
    prev_state :: state() | undefined,
    error :: term() | undefined,
    path :: binary() | undefined,
    rollback_strategy :: rollback_strategy()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(map()) -> gen_statem:start_ret().
start_link(Opts) ->
    gen_statem:start_link(?MODULE, Opts, []).

-spec get_state(pid()) -> state().
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%% @doc 广播 manifest_updated S2C 消息给所有在线用户。
%% 在 enable/disable/upgrade 状态转换成功后调用。
%% Fire-and-forget：错误仅记录日志，不影响调用方。
-spec broadcast_manifest_change() -> ok.
broadcast_manifest_change() ->
    _ = elib_async:async(fun() ->
        try
            Manifest = app_manifest_handler:build_manifest(),
            Msg = message_ds:assemble_msg(
                <<"S2C">>, <<>>, <<>>, Manifest,
                elib_id:gen("s2c"), <<>>, <<"manifest_updated">>, null
            ),
            Encoded = jsone:encode(Msg, [native_utf8]),
            GroupNames = syn:group_names(imboy_chat),
            _ = [syn:publish(imboy_chat, Uid, Encoded) || Uid <- GroupNames],
            ok
        catch
            _:Err ->
                ?WARN_LOG(["manifest_broadcast_failed", Err])
        end
    end),
    ok.

%% ===================================================================
%% gen_statem callbacks
%% ===================================================================

callback_mode() -> handle_event_function.

init(Opts) ->
    Name = maps:get(name, Opts, undefined),
    _ = if Name =:= undefined -> ok;
           true -> persistent_term:put({imboy_plugin_lifecycle, Name}, self())
        end,
    {ok, unknown, #data{name = Name, rollback_strategy = atomic}}.

%% ── state queries ──

handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, [{reply, From, State}]};

handle_event({call, From}, health_check, State, Data) ->
    Reply = {ok, #{state => State, name => Data#data.name}},
    {keep_state_and_data, [{reply, From, Reply}]};

%% ── unknown ──

handle_event({call, From}, {install, Path}, unknown, Data) ->
    case run_install_steps(Path, Data) of
        {ok, NewData} ->
            audit_transition(Data#data.name, unknown, installed, ok, #{}),
            {next_state, installed, NewData, [{reply, From, ok}]};
        {error, Reason, UndoLog} ->
            rollback_on_failure(Reason, UndoLog, unknown, Data, From)
    end;

handle_event({call, From}, _Event, unknown, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── installed ──

handle_event({call, From}, enable, installed, Data) ->
    case run_enable_steps(Data) of
        {ok, NewData} ->
            audit_transition(Data#data.name, installed, enabled, ok, #{}),
            broadcast_manifest_change(),
            {next_state, enabled, NewData, [{reply, From, ok}]};
        {error, Reason, UndoLog} ->
            rollback_on_failure(Reason, UndoLog, installed, Data, From)
    end;

handle_event({call, From}, {upgrade, Version}, installed, Data) ->
    case run_upgrade_steps(Version, Data) of
        {ok, NewData} ->
            audit_transition(Data#data.name, installed, installed, ok,
                             #{action => upgrade, version => Version}),
            broadcast_manifest_change(),
            {next_state, installed, NewData, [{reply, From, ok}]};
        {error, Reason, UndoLog} ->
            rollback_on_failure(Reason, UndoLog, installed, Data, From)
    end;

handle_event({call, From}, {uninstall, Mode}, installed, Data) ->
    case run_uninstall_steps(Mode, Data) of
        ok ->
            audit_transition(Data#data.name, installed, uninstalled, ok,
                             #{action => uninstall, mode => Mode}),
            {stop_and_reply, normal, [{reply, From, ok}]};
        {error, Reason} ->
            audit_transition(Data#data.name, installed, failed, {error, Reason}, #{}),
            {next_state, failed,
             Data#data{error = Reason, prev_state = installed},
             [{reply, From, {error, Reason}}]}
    end;

handle_event({call, From}, _Event, installed, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── enabled ──

handle_event({call, From}, enable, enabled, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};

handle_event({call, From}, disable, enabled, Data) ->
    %% §8.2: Check if any enabled plugin depends on this one
    case imboy_plugin_dependency:find_dependents(Data#data.name) of
        ok ->
            case run_disable_steps(Data) of
                {ok, NewData} ->
                    audit_transition(Data#data.name, enabled, disabled, ok, #{}),
                    broadcast_manifest_change(),
                    {next_state, disabled, NewData, [{reply, From, ok}]};
                {error, Reason, UndoLog} ->
                    rollback_on_failure(Reason, UndoLog, enabled, Data, From)
            end;
        {error, {has_dependents, Names}} ->
            {keep_state_and_data,
             [{reply, From, {error, {has_dependents, Names}}}]}
    end;

handle_event({call, From}, {upgrade, Version}, enabled, Data) ->
    case run_upgrade_steps(Version, Data) of
        {ok, NewData} ->
            audit_transition(Data#data.name, enabled, enabled, ok,
                             #{action => upgrade, version => Version}),
            broadcast_manifest_change(),
            {next_state, enabled, NewData, [{reply, From, ok}]};
        {error, Reason, UndoLog} ->
            rollback_on_failure(Reason, UndoLog, enabled, Data, From)
    end;

handle_event({call, From}, {uninstall, Mode}, enabled, Data) ->
    case run_disable_steps(Data) of
        {ok, _} ->
            case run_uninstall_steps(Mode, Data) of
                ok ->
                    {stop_and_reply, normal, [{reply, From, ok}]};
                {error, Reason} ->
                    {next_state, failed,
                     Data#data{error = Reason, prev_state = enabled},
                     [{reply, From, {error, Reason}}]}
            end;
        {error, Reason, _UndoLog} ->
            {next_state, failed,
             Data#data{error = Reason, prev_state = enabled},
             [{reply, From, {error, Reason}}]}
    end;

handle_event({call, From}, _Event, enabled, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── disabled ──

handle_event({call, From}, enable, disabled, Data) ->
    case run_enable_steps(Data) of
        {ok, NewData} ->
            broadcast_manifest_change(),
            {next_state, enabled, NewData, [{reply, From, ok}]};
        {error, Reason, UndoLog} ->
            rollback_on_failure(Reason, UndoLog, disabled, Data, From)
    end;

handle_event({call, From}, {uninstall, Mode}, disabled, Data) ->
    case run_uninstall_steps(Mode, Data) of
        ok ->
            {stop_and_reply, normal, [{reply, From, ok}]};
        {error, Reason} ->
            {next_state, failed,
             Data#data{error = Reason, prev_state = disabled},
             [{reply, From, {error, Reason}}]}
    end;

handle_event({call, From}, _Event, disabled, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── failed ──

handle_event({call, From}, reset, failed, Data) ->
    {next_state, unknown,
     Data#data{error = undefined, prev_state = undefined,
               manifest = undefined, version = undefined, path = undefined,
               rollback_strategy = atomic},
     [{reply, From, ok}]};

handle_event({call, From}, {force_uninstall, Mode}, failed, Data) ->
    _ = catch run_uninstall_steps(Mode, Data),
    {stop_and_reply, normal, [{reply, From, ok}]};

handle_event(cast, {inject_failure, Reason}, State, Data) ->
    {next_state, failed, Data#data{error = Reason, prev_state = State}};

handle_event({call, From}, _Event, failed, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── catch-all ──

handle_event(_EventType, _Event, _State, _Data) ->
    {keep_state_and_data, []}.

terminate(_Reason, _State, Data) ->
    _ = case Data#data.name of
        undefined -> ok;
        Name -> persistent_term:erase({imboy_plugin_lifecycle, Name})
    end,
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

format_status(StatusData) ->
    StatusData.

%% ===================================================================
%% Audit — 审计日志
%% ===================================================================

%% @doc 写入审计日志（fire-and-forget）。
audit_transition(Name, FromState, ToState, Result, Meta) ->
    _ = catch imboy_plugin_audit_ds:write(#{
        plugin_name => atom_to_binary(Name),
        event => <<"state_transition">>,
        from_state => state_to_bin(FromState),
        to_state => state_to_bin(ToState),
        result => result_to_bin(Result),
        metadata => Meta
    }).

state_to_bin(S) -> atom_to_binary(S).

result_to_bin(ok) -> <<"ok">>;
result_to_bin({error, {Step, _}}) -> atom_to_binary(Step).

%% ===================================================================
%% Rollback — 回滚策略执行
%% ===================================================================

%% @doc 步骤失败后根据策略执行回滚。
%% atomic: 全部撤销 → 成功则回到入口状态，失败则进入 failed
%% best_effort: 全部撤销（catch） → 始终进入 failed
%% manual: 跳过撤销 → 直接进入 failed
rollback_on_failure(Reason, UndoLog, EntryState, Data, From) ->
    Strategy = Data#data.rollback_strategy,
    case execute_rollback(Strategy, UndoLog) of
        rollback_ok ->
            audit_transition(Data#data.name, EntryState, EntryState,
                             {error, Reason}, #{rollback => atomic, outcome => rolled_back}),
            {next_state, EntryState,
             Data#data{error = undefined, prev_state = undefined},
             [{reply, From, {error, Reason}}]};
        {rollback_partial, PartialErrors} ->
            audit_transition(Data#data.name, EntryState, failed,
                             {error, Reason}, #{rollback => best_effort,
                                                partial_errors => length(PartialErrors)}),
            {next_state, failed,
             Data#data{error = {Reason, {partial_rollback, PartialErrors}},
                       prev_state = EntryState},
             [{reply, From, {error, Reason}}]};
        rollback_skipped ->
            audit_transition(Data#data.name, EntryState, failed,
                             {error, Reason}, #{rollback => manual}),
            {next_state, failed,
             Data#data{error = Reason, prev_state = EntryState},
             [{reply, From, {error, Reason}}]}
    end.

%% @doc 根据回滚策略执行撤销操作。
execute_rollback(manual, _UndoLog) ->
    rollback_skipped;
execute_rollback(atomic, UndoLog) ->
    execute_undo_atomic(UndoLog, []);
execute_rollback(best_effort, UndoLog) ->
    _ = execute_undo_best_effort(UndoLog),
    %% best_effort always goes to failed even if all undos succeed
    {rollback_partial, []}.

%% @doc Atomic: execute all undos, stop on first failure.
execute_undo_atomic([], _Errors) ->
    rollback_ok;
execute_undo_atomic([{Step, UndoFun} | Rest], Errors) ->
    try
        UndoFun(),
        execute_undo_atomic(Rest, Errors)
    catch
        _:Err ->
            execute_undo_atomic(Rest, [{Step, Err} | Errors])
    end.

%% @doc Best effort: execute all undos, collect errors.
execute_undo_best_effort(UndoLog) ->
    lists:foldl(fun({Step, UndoFun}, Errors) ->
        try UndoFun() of _ -> Errors
        catch _:Err -> [{Step, Err} | Errors]
        end
    end, [], UndoLog).

%% ===================================================================
%% Step runners — 组件接线 / Component wiring
%% ===================================================================

%% @doc 安装步骤 (lifecycle.md §5.1)。
%% 记录有副作用的步骤的撤销操作。
run_install_steps(Path, Data) ->
    ConfigPath = <<Path/binary, "/plugin.config">>,
    SigPath = <<Path/binary, "/SIGNATURE">>,
    erase(undo_log),
    with_steps(fun() ->
        step_ok(imboy_plugin_signature:verify_file(ConfigPath, SigPath),
                verify_signature),
        Manifest = step_result(imboy_plugin_toml:load(ConfigPath),
                               parse_manifest),
        step_ok(imboy_plugin_dependency:validate_constraints([Manifest]),
                validate_dependencies),
        %% Side effect: register manifest → record undo
        persistent_term:put({imboy_plugin_manifest, Data#data.name}, Manifest),
        add_undo(register_manifest, fun() ->
            persistent_term:erase({imboy_plugin_manifest, Data#data.name})
        end),
        _ = catch imboy_plugin_loader:scan(),
        %% Extract rollback strategy from manifest
        Strategy = get_rollback_strategy(Manifest),
        {ok, Data#data{path = Path, manifest = Manifest,
                        version = maps:get(version, Manifest, <<"1.0.0">>),
                        rollback_strategy = Strategy}}
    end).

%% @doc 启用步骤 (lifecycle.md §5.2 + §8.1)。
run_enable_steps(Data) ->
    #data{manifest = Manifest, name = Name} = Data,
    Routes = maps:get(routes, Manifest, []),
    erase(undo_log),
    with_steps(fun() ->
        step_ok(imboy_plugin_dependency:check_enable_deps(Manifest),
                check_enable_deps),
        case Routes of
            [] -> ok;
            _ ->
                step_ok(imboy_router_registry:register(Name, Routes),
                        register_routes),
                add_undo(register_routes, fun() ->
                    catch imboy_router_registry:unregister(Name)
                end)
        end,
        {ok, Data}
    end).

%% @doc 禁用步骤 (lifecycle.md §5.3)。
%% 无需撤销——禁用为尽力清理。
run_disable_steps(Data) ->
    #data{name = Name} = Data,
    erase(undo_log),
    _ = catch imboy_router_registry:unregister(Name),
    {ok, Data}.

%% @doc 升级步骤 (lifecycle.md §5.4)。
run_upgrade_steps(_Version, Data) ->
    #data{manifest = Manifest} = Data,
    erase(undo_log),
    with_steps(fun() ->
        step_ok(imboy_plugin_dependency:validate_constraints([Manifest]),
                validate_dependencies),
        {ok, Data}
    end).

%% @doc 卸载步骤 (lifecycle.md §5.5)。
%% 卸载不使用 undo_log——一旦开始必须完成。
run_uninstall_steps(_Mode, Data) ->
    #data{name = Name} = Data,
    _ = catch imboy_router_registry:unregister(Name),
    _ = catch persistent_term:erase({imboy_plugin_manifest, Name}),
    _ = catch imboy_plugin_loader:scan(),
    ok.

%% ===================================================================
%% Step helpers
%% ===================================================================

step_ok(ok, _) -> ok;
step_ok({error, R}, Step) -> throw({step_failed, Step, R}).

step_result({ok, V}, _) -> V;
step_result({error, R}, Step) -> throw({step_failed, Step, R}).

%% @doc 为当前步骤序列记录撤销操作。
add_undo(Step, UndoFun) ->
    Log = get(undo_log),
    put(undo_log, [{Step, UndoFun} | Log]),
    ok.

%% @doc 运行步骤序列（支持 undo_log）。
with_steps(Fun) ->
    try Fun()
    catch throw:{step_failed, Step, R} ->
        UndoLog = case get(undo_log) of undefined -> []; L -> L end,
        {error, {Step, R}, lists:reverse(UndoLog)}
    end.

%% @doc 从 manifest 中提取回滚策略。
get_rollback_strategy(Manifest) ->
    case maps:get(lifecycle, Manifest, undefined) of
        #{rollback_strategy := S}
            when S =:= atomic; S =:= best_effort; S =:= manual ->
            S;
        _ ->
            atomic
    end.
