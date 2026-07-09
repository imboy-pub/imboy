-module(imboy_router_plugin_routes_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_router:plugin_routes/0 集成测试（P2 切片 2）
%%% Integration test for imboy_router:plugin_routes/0 (P2 slice 2)
%%%
%%% 验证 / Verifies:
%%%   1. registry 未启动时 plugin_routes/0 返回 []（兼容启动早期）
%%%   2. 注册后 plugin_routes/0 返回 cowboy {Path, Handler, Opts} tuple
%%%   3. required_feature 字段透传到 Opts
%%%   4. 多插件路由扁平合并
%%%   5. unregister 后路由消失
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

%% ===================================================================
%% 1. registry 未启动时返回 []
%% ===================================================================

plugin_routes_returns_empty_when_registry_not_started_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 确保 registry 未启动
        case erlang:whereis(imboy_router_registry) of
            undefined ->
                ok;
            P ->
                unlink(P),
                gen_server:stop(P),
                timer:sleep(50)
        end,
        ?assertEqual(undefined, erlang:whereis(imboy_router_registry)),
        ?assertEqual([], imboy_router:plugin_routes())
    end).

%% ===================================================================
%% 2. 注册后 plugin_routes/0 返回 cowboy tuple
%% ===================================================================

plugin_routes_returns_cowboy_tuples_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        R = #{
            method => <<"GET">>,
            path => <<"/api/v1/channel/discover">>,
            handler => channel_handler,
            action => discover
        },
        ok = imboy_router_registry:register(channel, [R]),
        Result = imboy_router:plugin_routes(),
        [
            ?_assertEqual(1, length(Result)),
            %% cowboy tuple: {string Path, atom Handler, map Opts}
            ?_assertMatch(
                [{"/api/v1/channel/discover", channel_handler, #{action := discover}}],
                Result
            )
        ]
    end}.

%% ===================================================================
%% 3. required_feature 透传到 Opts
%% ===================================================================

plugin_routes_propagates_required_feature_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        R = #{
            method => <<"POST">>,
            path => <<"/api/v1/channel/invitation">>,
            handler => channel_handler,
            action => create_invitation,
            required_feature => channel_invitation
        },
        ok = imboy_router_registry:register(channel, [R]),
        Result = imboy_router:plugin_routes(),
        ?_assertMatch(
            [
                {"/api/v1/channel/invitation", channel_handler, #{
                    action := create_invitation, required_feature := channel_invitation
                }}
            ],
            Result
        )
    end}.

%% ===================================================================
%% 4. 多插件路由扁平合并
%% ===================================================================

plugin_routes_flattens_multiple_plugins_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        RChan = #{
            method => <<"GET">>,
            path => <<"/api/v1/channel/x">>,
            handler => channel_handler,
            action => x
        },
        RMom = #{
            method => <<"GET">>,
            path => <<"/api/v1/moment/y">>,
            handler => moment_handler,
            action => y
        },
        ok = imboy_router_registry:register(channel, [RChan]),
        ok = imboy_router_registry:register(moment, [RMom]),
        Result = imboy_router:plugin_routes(),
        Paths = [P || {P, _, _} <- Result],
        [
            ?_assertEqual(2, length(Result)),
            ?_assert(lists:member("/api/v1/channel/x", Paths)),
            ?_assert(lists:member("/api/v1/moment/y", Paths))
        ]
    end}.

%% ===================================================================
%% 5. unregister 后路由消失
%% ===================================================================

plugin_routes_drops_after_unregister_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        R = #{
            method => <<"GET">>,
            path => <<"/api/v1/channel/x">>,
            handler => channel_handler,
            action => x
        },
        ok = imboy_router_registry:register(channel, [R]),
        ?assertEqual(1, length(imboy_router:plugin_routes())),
        ok = imboy_router_registry:unregister(channel),
        ?_assertEqual([], imboy_router:plugin_routes())
    end}.
