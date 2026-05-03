-module(imboy_plugin_lifecycle).
-behaviour(gen_statem).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_lifecycle — 插件生命周期状态机（gen_statem）
%%% Plugin lifecycle state machine
%%%
%%% 切片 1：纯状态机骨架，10 个状态，sealed decision。
%%% 不调用外部组件（signature/toml/dependency/migrate/loader/router）。
%%%
%%% 状态转换图（lifecycle.md §4）：
%%%   unknown → installing → installed
%%%   installed → enabling → enabled
%%%   enabled → disabling → disabled
%%%   disabled → enabling → enabled
%%%   enabled/installed → upgrading → enabled/installed
%%%   installed/disabled/enabled → uninstalling → unknown (stop)
%%%   *-ing → failed (step_failed / timeout / cancel)
%%%   failed → retry / force_uninstall / reset
%%%
%%% Source of truth: doc/plugin/lifecycle.md
%%%-------------------------------------------------------------------

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
    get_state/1
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

-type plugin_name() :: atom().

-record(data, {
    name :: plugin_name(),
    manifest :: map() | undefined,
    version :: binary() | undefined,
    prev_state :: state() | undefined,
    error :: term() | undefined,
    path :: binary() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(map()) -> gen_statem:start_ret().
start_link(Opts) ->
    gen_statem:start_link(?MODULE, Opts, []).

-spec get_state(pid()) -> state().
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> handle_event_function.

init(Opts) ->
    Name = maps:get(name, Opts, undefined),
    {ok, unknown, #data{name = Name}}.

%% ── state queries ──

handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, [{reply, From, State}]};

handle_event({call, From}, health_check, State, Data) ->
    Reply = {ok, #{state => State, name => Data#data.name}},
    {keep_state_and_data, [{reply, From, Reply}]};

%% ── unknown ──

handle_event({call, From}, {install, Path}, unknown, Data) ->
    NextData = Data#data{path = Path, version = <<"1.0.0">>},
    {next_state, installed, NextData, [{reply, From, ok}]};

handle_event({call, From}, _Event, unknown, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── installed ──

handle_event({call, From}, enable, installed, Data) ->
    {next_state, enabled, Data, [{reply, From, ok}]};

handle_event({call, From}, {upgrade, _Version}, installed, Data) ->
    {next_state, installed, Data, [{reply, From, ok}]};

handle_event({call, From}, {uninstall, _Mode}, installed, _Data) ->
    {stop_and_reply, normal, [{reply, From, ok}]};

handle_event({call, From}, _Event, installed, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── enabled ──

handle_event({call, From}, enable, enabled, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};

handle_event({call, From}, disable, enabled, Data) ->
    {next_state, disabled, Data, [{reply, From, ok}]};

handle_event({call, From}, {upgrade, _Version}, enabled, Data) ->
    {next_state, enabled, Data, [{reply, From, ok}]};

handle_event({call, From}, {uninstall, _Mode}, enabled, _Data) ->
    {stop_and_reply, normal, [{reply, From, ok}]};

handle_event({call, From}, _Event, enabled, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── disabled ──

handle_event({call, From}, enable, disabled, Data) ->
    {next_state, enabled, Data, [{reply, From, ok}]};

handle_event({call, From}, {uninstall, _Mode}, disabled, _Data) ->
    {stop_and_reply, normal, [{reply, From, ok}]};

handle_event({call, From}, _Event, disabled, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── failed ──

handle_event({call, From}, reset, failed, Data) ->
    {next_state, unknown, Data#data{error = undefined, prev_state = undefined},
     [{reply, From, ok}]};

handle_event({call, From}, {force_uninstall, _Mode}, failed, _Data) ->
    {stop_and_reply, normal, [{reply, From, ok}]};

handle_event(cast, {inject_failure, Reason}, State, Data) ->
    {next_state, failed, Data#data{error = Reason, prev_state = State}};

handle_event({call, From}, _Event, failed, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state_transition}}]};

%% ── catch-all ──

handle_event(_EventType, _Event, _State, _Data) ->
    {keep_state_and_data, []}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

format_status(StatusData) ->
    StatusData.
