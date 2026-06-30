-module(imboy_plugin_manager).
-compile([nowarn_deprecated_catch]).

%% @status FROZEN (roadmap-only, 2026-06)：v2 动态加载子系统暂停投入。
%% 当前生产走配置驱动模块化单体路线（见 product-profile-and-plugin-registry-design.md §3.1）。
%% 修改前请确认是否真要重启动态平台方向。冻结≠移除。
%% FROZEN: v2 dynamic plugin loading subsystem is suspended (roadmap-only).
%% Current production route: config-driven monolith. See §3.1 before resuming.

%%%-------------------------------------------------------------------
%%% @doc
%%% 插件管理 API 层 / Plugin management API layer
%%% 桥接 HTTP handler 与 lifecycle statem。
%%% 使用 persistent_term 进行进程发现。
%%%
%%% lifecycle.md §10 Admin REST API 的后端实现。
%%% @end
%%%-------------------------------------------------------------------

-export([
    list_plugins/0,
    get_plugin/1,
    get_state/1,
    health_check/1,
    install/2,
    enable/1,
    disable/1,
    upgrade/2,
    uninstall/2,
    reset/1,
    force_uninstall/2,
    find_lifecycle/1
]).

%% @doc 列出所有已注册插件及其状态。
-spec list_plugins() -> {ok, [map()]}.
list_plugins() ->
    PT = persistent_term:get(),
    Plugins = lists:filtermap(
        fun({Key, Val}) ->
            case Key of
                {imboy_plugin_manifest, Name} when is_atom(Name) ->
                    {true, #{name => Name, manifest => Val}};
                _ ->
                    false
            end
        end,
        PT
    ),
    Items = lists:map(
        fun(#{name := Name, manifest := MF} = M) ->
            Version = maps:get(version, MF, <<"0.0.0">>),
            Strategy = get_rollback_strategy(MF),
            State =
                case find_lifecycle(Name) of
                    undefined ->
                        installed;
                    Pid ->
                        try
                            gen_statem:call(Pid, get_state, 1000)
                        catch
                            _:Err ->
                                logger:warning(#{
                                    event => plugin_state_call_failed,
                                    name => Name,
                                    function => list_plugins,
                                    error => Err
                                }),
                                installed
                        end
                end,
            maps:without([manifest], M#{
                state => State,
                version => Version,
                rollback_strategy => Strategy
            })
        end,
        Plugins
    ),
    {ok, Items}.

%% @doc 获取单个插件详情。
-spec get_plugin(atom()) -> {ok, map()} | {error, not_found}.
get_plugin(Name) when is_atom(Name) ->
    case persistent_term:get({imboy_plugin_manifest, Name}, undefined) of
        undefined ->
            {error, not_found};
        MF ->
            Version = maps:get(version, MF, <<"0.0.0">>),
            Strategy = get_rollback_strategy(MF),
            State =
                case find_lifecycle(Name) of
                    undefined ->
                        installed;
                    Pid ->
                        try
                            gen_statem:call(Pid, get_state, 1000)
                        catch
                            _:Err ->
                                logger:warning(#{
                                    event => plugin_state_call_failed,
                                    name => Name,
                                    function => get_plugin,
                                    error => Err
                                }),
                                installed
                        end
                end,
            {ok, #{
                name => Name,
                state => State,
                version => Version,
                rollback_strategy => Strategy,
                manifest => MF
            }}
    end.

%% @doc 获取插件 lifecycle 状态（轻量级）。
-spec get_state(atom()) -> {ok, map()} | {error, term()}.
get_state(Name) when is_atom(Name) ->
    case find_lifecycle(Name) of
        undefined ->
            case persistent_term:get({imboy_plugin_manifest, Name}, undefined) of
                undefined ->
                    {error, not_found};
                MF ->
                    {ok, #{
                        name => Name,
                        state => installed,
                        version => maps:get(version, MF, <<"0.0.0">>)
                    }}
            end;
        Pid ->
            try
                State = gen_statem:call(Pid, get_state, 1000),
                case gen_statem:call(Pid, health_check, 1000) of
                    {ok, Info} -> {ok, Info#{state => State}};
                    _ -> {ok, #{name => Name, state => State}}
                end
            catch
                _:Err ->
                    logger:warning(#{
                        event => plugin_health_call_failed,
                        name => Name,
                        function => get_state,
                        error => Err
                    }),
                    {error, process_error}
            end
    end.

%% @doc 触发健康检查。
-spec health_check(atom()) -> {ok, map()} | {error, term()}.
health_check(Name) when is_atom(Name) ->
    case find_lifecycle(Name) of
        undefined ->
            {error, not_found};
        Pid ->
            try
                gen_statem:call(Pid, health_check, 5000)
            catch
                _:Err ->
                    logger:warning(#{
                        event => plugin_health_call_failed,
                        name => Name,
                        function => health_check,
                        error => Err
                    }),
                    {error, process_error}
            end
    end.

%% @doc 安装插件（启动 lifecycle statem 并触发 install）。
-spec install(atom(), binary()) -> {ok, map()} | {error, term()}.
install(Name, Path) when is_atom(Name), is_binary(Path) ->
    case find_lifecycle(Name) of
        undefined ->
            {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => Name}),
            call_and_maybe_stop(Pid, {install, Path}, Name);
        Pid ->
            case safe_get_state(Pid) of
                unknown ->
                    call_and_maybe_stop(Pid, {install, Path}, Name);
                State ->
                    {error, {invalid_state_transition, State}}
            end
    end.

%% @doc 启用插件。
-spec enable(atom()) -> ok | {error, term()}.
enable(Name) when is_atom(Name) ->
    call_lifecycle(Name, enable).

%% @doc 禁用插件。
-spec disable(atom()) -> ok | {error, term()}.
disable(Name) when is_atom(Name) ->
    call_lifecycle(Name, disable).

%% @doc 升级插件。
-spec upgrade(atom(), binary()) -> ok | {error, term()}.
upgrade(Name, Version) when is_atom(Name), is_binary(Version) ->
    call_lifecycle(Name, {upgrade, Version}).

%% @doc 卸载插件。
-spec uninstall(atom(), binary()) -> ok | {error, term()}.
uninstall(Name, Mode) when is_atom(Name) ->
    call_lifecycle(Name, {uninstall, Mode}).

%% @doc 从 failed 状态重置到 unknown。
-spec reset(atom()) -> ok | {error, term()}.
reset(Name) when is_atom(Name) ->
    call_lifecycle(Name, reset).

%% @doc 从 failed 状态强制卸载。
-spec force_uninstall(atom(), binary()) -> ok | {error, term()}.
force_uninstall(Name, Mode) when is_atom(Name) ->
    call_lifecycle(Name, {force_uninstall, Mode}).

%% @doc 查找 lifecycle 进程 Pid。
-spec find_lifecycle(atom()) -> pid() | undefined.
find_lifecycle(Name) when is_atom(Name) ->
    case persistent_term:get({imboy_plugin_lifecycle, Name}, undefined) of
        undefined ->
            undefined;
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true ->
                    Pid;
                false ->
                    catch persistent_term:erase({imboy_plugin_lifecycle, Name}),
                    undefined
            end
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

call_lifecycle(Name, Event) ->
    case find_lifecycle(Name) of
        undefined ->
            {error, not_found};
        Pid ->
            try
                gen_statem:call(Pid, Event, 5000)
            catch
                exit:{noproc, _} -> {error, not_found};
                exit:{timeout, _} -> {error, timeout};
                _:_ -> {error, process_error}
            end
    end.

safe_get_state(Pid) ->
    try
        gen_statem:call(Pid, get_state, 1000)
    catch
        _:Err ->
            logger:warning(#{
                event => plugin_state_call_failed, function => safe_get_state, error => Err
            }),
            unknown
    end.

call_and_maybe_stop(Pid, Event, Name) ->
    try gen_statem:call(Pid, Event, 5000) of
        ok ->
            {ok, #{name => Name, state => safe_get_state(Pid)}};
        {error, Reason} ->
            catch gen_statem:stop(Pid),
            {error, Reason}
    catch
        exit:{noproc, _} -> {error, not_found};
        exit:{timeout, _} -> {error, timeout};
        _:_ -> {error, process_error}
    end.

get_rollback_strategy(Manifest) ->
    case Manifest of
        #{lifecycle := #{rollback_strategy := S}} when
            S =:= atomic; S =:= best_effort; S =:= manual
        ->
            S;
        _ ->
            atomic
    end.
