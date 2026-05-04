-module(imboy_plugin_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_telemetry 的 EUnit 测试（P6-T3）
%%%
%%% 覆盖 / Coverage:
%%%   1. emit 调用不崩溃（无 handler 注册时）
%%%   2. emit 携带正确的 plugin_name metadata
%%%   3. emit 支持 measurements
%%%   4. emit 合并 extra metadata
%%%
%%% 使用 ETS 存储收到的事件（避免 EUnit 进程隔离问题）
%%% @end
%%%-------------------------------------------------------------------

-define(ETS, imboy_plugin_telemetry_test_ets).

%% ===================================================================
%% Helpers
%% ===================================================================

ensure_ets() ->
    case ets:info(?ETS) of
        undefined -> ets:new(?ETS, [named_table, public, set]);
        _ -> ets:delete_all_objects(?ETS)
    end.

attach_handler(EventAtom) ->
    HandlerRef = {?MODULE, erlang:unique_integer()},
    telemetry:attach(HandlerRef, [imboy, plugin, EventAtom],
        fun(_Name, Measurements, Metadata, _Config) ->
            ets:insert(?ETS, {{event, EventAtom}, Measurements, Metadata})
        end, []),
    HandlerRef.

get_event(EventAtom) ->
    case ets:lookup(?ETS, {event, EventAtom}) of
        [{{event, EventAtom}, Measurements, Metadata}] ->
            {ok, Measurements, Metadata};
        [] ->
            not_found
    end.

%% ===================================================================
%% 1. emit 不崩溃（无 handler）
%% ===================================================================

telemetry_emit_no_handler_test_() ->
    ?_test(begin
        ok = application:ensure_started(telemetry),
        ?assertEqual(ok, imboy_plugin_telemetry:emit(loaded, channel_plugin, #{}))
    end).

%% ===================================================================
%% 2. emit 携带 plugin_name metadata
%% ===================================================================

telemetry_emit_with_metadata_test_() ->
    {setup,
     fun() ->
         ok = application:ensure_started(telemetry),
         ensure_ets(),
         attach_handler(loaded)
     end,
     fun(HandlerRef) ->
         telemetry:detach(HandlerRef)
     end,
     fun(_) ->
         [?_test(begin
             ok = imboy_plugin_telemetry:emit(loaded, channel_plugin, #{}),
             {ok, _M, Metadata} = get_event(loaded),
             ?assertEqual(channel_plugin, maps:get(plugin_name, Metadata))
         end)]
     end}.

%% ===================================================================
%% 3. emit 支持 measurements
%% ===================================================================

telemetry_emit_with_measurements_test_() ->
    {setup,
     fun() ->
         ok = application:ensure_started(telemetry),
         ensure_ets(),
         attach_handler(started)
     end,
     fun(HandlerRef) ->
         telemetry:detach(HandlerRef)
     end,
     fun(_) ->
         [?_test(begin
             ok = imboy_plugin_telemetry:emit(started, my_plugin,
                 #{measurements => #{duration_ms => 150}}),
             {ok, Measurements, Metadata} = get_event(started),
             ?assertEqual(150, maps:get(duration_ms, Measurements)),
             ?assertEqual(my_plugin, maps:get(plugin_name, Metadata))
         end)]
     end}.

%% ===================================================================
%% 4. emit 合并 extra metadata
%% ===================================================================

telemetry_emit_merges_extra_test_() ->
    {setup,
     fun() ->
         ok = application:ensure_started(telemetry),
         ensure_ets(),
         attach_handler(error)
     end,
     fun(HandlerRef) ->
         telemetry:detach(HandlerRef)
     end,
     fun(_) ->
         [?_test(begin
             ok = imboy_plugin_telemetry:emit(error, beta_plugin,
                 #{reason => timeout, step => init}),
             {ok, _Measurements, Metadata} = get_event(error),
             ?assertEqual(beta_plugin, maps:get(plugin_name, Metadata)),
             ?assertEqual(timeout, maps:get(reason, Metadata)),
             ?assertEqual(init, maps:get(step, Metadata))
         end)]
     end}.
