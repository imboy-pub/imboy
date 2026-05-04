-module(imboy_plugin_telemetry).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_telemetry — 插件 telemetry 事件发射器（P6-T3）
%%% Plugin telemetry event emitter
%%%
%%% 职责 / Responsibilities:
%%%   - 为所有插件操作发射 telemetry 事件
%%%   - 每个事件携带 #{plugin_name => Name} metadata
%%%   - 事件命名约定: [imboy, plugin, Action]
%%%   - 后续可通过 opentelemetry_telemetry 桥接为 OTel spans
%%%
%%% 事件列表 / Events:
%%%   [imboy, plugin, loaded]      — 插件加载完成
%%%   [imboy, plugin, started]     — 插件启动完成
%%%   [imboy, plugin, stopped]     — 插件停止
%%%   [imboy, plugin, error]       — 插件运行时错误
%%%   [imboy, plugin, unloaded]    — 插件卸载
%%%
%%% 用法 / Usage:
%%%   imboy_plugin_telemetry:emit(loaded, my_plugin, #{}),
%%%   imboy_plugin_telemetry:emit(error, my_plugin, #{reason => timeout}).
%%%
%%% @end
%%%-------------------------------------------------------------------

-export([emit/3]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc 发射带插件名的 telemetry 事件。
%% Event: atom() 如 loaded | started | stopped | error | unloaded
%% PluginName: atom() 插件名
%% Extra: map() 额外 metadata + 可选 measurements key
-spec emit(atom(), atom(), map()) -> ok.
emit(Event, PluginName, Extra) when is_atom(Event), is_atom(PluginName), is_map(Extra) ->
    Measurements = maps:get(measurements, Extra, #{}),
    Metadata = maps:without([measurements], Extra),
    telemetry:execute(
        [imboy, plugin, Event],
        Measurements,
        Metadata#{plugin_name => PluginName}
    ),
    ok.
