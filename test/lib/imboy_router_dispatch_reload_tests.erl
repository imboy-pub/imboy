-module(imboy_router_dispatch_reload_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% P2-T4: 路由注册变更后自动触发 cowboy dispatch 热更
%%%
%%% 覆盖 / Coverage:
%%%   1. reload_dispatch 无 cowboy listener 时不崩溃
%%%   2. register 后触发 reload_dispatch（无 listener 优雅降级）
%%%   3. unregister 后触发 reload_dispatch
%%%   4. clear 后触发 reload_dispatch
%%%   5. reload_dispatch 手动调用后 ETS 内容不变
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% Test helpers
%% ===================================================================

setup() ->
    {ok, Pid} = imboy_router_registry:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            gen_server:stop(Pid);
        false ->
            ok
    end.

valid_route(PluginName, Path) ->
    #{
        method => <<"GET">>,
        path => Path,
        handler => list_to_atom(atom_to_list(PluginName) ++ "_handler"),
        action => list
    }.

%% ===================================================================
%% 1. reload_dispatch 无 cowboy listener 时不崩溃
%% ===================================================================

reload_dispatch_no_listener_no_crash_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ?_test(begin
            ?assertEqual(ok, imboy_router_registry:reload_dispatch())
        end)
    end}.

%% ===================================================================
%% 2. register 后调用 reload_dispatch 不崩溃
%% ===================================================================

register_triggers_reload_no_crash_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ?_test(begin
            R = valid_route(channel, <<"/api/v1/channel/x">>),
            ?assertEqual(ok, imboy_router_registry:register(channel, [R]))
        end)
    end}.

%% ===================================================================
%% 3. unregister 后调用 reload_dispatch 不崩溃
%% ===================================================================

unregister_triggers_reload_no_crash_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ?_test(begin
            R = valid_route(channel, <<"/api/v1/channel/x">>),
            ok = imboy_router_registry:register(channel, [R]),
            ?assertEqual(ok, imboy_router_registry:unregister(channel))
        end)
    end}.

%% ===================================================================
%% 4. clear 后调用 reload_dispatch 不崩溃
%% ===================================================================

clear_triggers_reload_no_crash_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ?_test(begin
            R = valid_route(channel, <<"/api/v1/channel/x">>),
            ok = imboy_router_registry:register(channel, [R]),
            ?assertEqual(ok, imboy_router_registry:clear())
        end)
    end}.

%% ===================================================================
%% 5. reload_dispatch 手动调用后 ETS 内容不变
%% ===================================================================

reload_preserves_ets_content_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ?_test(begin
            R = valid_route(moment, <<"/api/v1/moment/feed">>),
            ok = imboy_router_registry:register(moment, [R]),
            ok = imboy_router_registry:reload_dispatch(),
            ?assertEqual([R], imboy_router_registry:plugin_routes(moment))
        end)
    end}.
