-module(imboy_router_registry_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_router_registry 的 EUnit 测试（P2 切片 1）
%%%
%%% 覆盖 / Coverage:
%%%   1. start_link 成功 + ETS 表创建
%%%   2. register/2 注册成功 + plugin_routes/1 查询返回相同 routes
%%%   3. 重复 register 覆盖式更新
%%%   4. unregister 移除（幂等）
%%%   5. all_routes/0 扁平化
%%%   6. plugin_names/0 列表
%%%   7. clear/0 清空
%%%   8. 路径强约束：违反 /v{n}/<name>/ 拒绝
%%%   9. 非法 route_spec 拒绝
%%%  10. 未知消息优雅处理（actor-model instinct）
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
%% 1. start_link + ETS 表
%% ===================================================================

start_link_creates_ets_table_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_assertNotEqual(undefined, ets:info(imboy_router_registry)),
            ?_assertEqual(0, ets:info(imboy_router_registry, size))
        ]
    end}.

%% ===================================================================
%% 2. register + plugin_routes
%% ===================================================================

register_then_lookup_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        R1 = valid_route(channel, <<"/api/v1/channel/x">>),
        R2 = valid_route(channel, <<"/api/v1/channel/y">>),
        ok = imboy_router_registry:register(channel, [R1, R2]),
        [
            ?_assertEqual([R1, R2], imboy_router_registry:plugin_routes(channel)),
            ?_assertEqual([], imboy_router_registry:plugin_routes(unknown_plugin))
        ]
    end}.

%% ===================================================================
%% 3. 重复 register 覆盖
%% ===================================================================

register_overwrites_existing_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        OldR = valid_route(channel, <<"/api/v1/channel/old">>),
        NewR = valid_route(channel, <<"/api/v1/channel/new">>),
        ok = imboy_router_registry:register(channel, [OldR]),
        ok = imboy_router_registry:register(channel, [NewR]),
        ?_assertEqual([NewR], imboy_router_registry:plugin_routes(channel))
    end}.

%% ===================================================================
%% 4. unregister 幂等
%% ===================================================================

unregister_is_idempotent_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        R = valid_route(channel, <<"/api/v1/channel/x">>),
        ok = imboy_router_registry:register(channel, [R]),
        ok = imboy_router_registry:unregister(channel),
        %% 重复 unregister 不应报错
        ok = imboy_router_registry:unregister(channel),
        %% 未注册的也 ok
        ok = imboy_router_registry:unregister(never_registered),
        ?_assertEqual([], imboy_router_registry:plugin_routes(channel))
    end}.

%% ===================================================================
%% 5. all_routes 扁平化
%% ===================================================================

all_routes_flattens_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        RChan = valid_route(channel, <<"/api/v1/channel/x">>),
        RMom = valid_route(moment, <<"/api/v1/moment/y">>),
        ok = imboy_router_registry:register(channel, [RChan]),
        ok = imboy_router_registry:register(moment, [RMom]),
        All = imboy_router_registry:all_routes(),
        [
            ?_assertEqual(2, length(All)),
            ?_assert(lists:member({channel, RChan}, All)),
            ?_assert(lists:member({moment, RMom}, All))
        ]
    end}.

%% ===================================================================
%% 6. plugin_names
%% ===================================================================

plugin_names_lists_registered_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ok = imboy_router_registry:register(channel, [valid_route(channel, <<"/api/v1/channel/x">>)]),
        ok = imboy_router_registry:register(moment, [valid_route(moment, <<"/api/v1/moment/y">>)]),
        ?_assertEqual(
            lists:sort([channel, moment]),
            lists:sort(imboy_router_registry:plugin_names())
        )
    end}.

%% ===================================================================
%% 7. clear 清空
%% ===================================================================

clear_removes_all_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ok = imboy_router_registry:register(channel, [valid_route(channel, <<"/api/v1/channel/x">>)]),
        ok = imboy_router_registry:clear(),
        [
            ?_assertEqual([], imboy_router_registry:plugin_routes(channel)),
            ?_assertEqual([], imboy_router_registry:plugin_names())
        ]
    end}.

%% ===================================================================
%% 8. 路径强约束 — 违反返回 error
%% ===================================================================

register_rejects_invalid_namespace_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        %% path 不以 /v{n}/channel/ 开头
        BadRoute = #{
            method => <<"GET">>,
            path => <<"/api/v1/wrong/x">>,
            handler => h,
            action => a
        },
        ?_assertMatch(
            {error, {invalid_route_namespace, <<"/api/v1/wrong/x">>}},
            imboy_router_registry:register(channel, [BadRoute])
        )
    end}.

%% ===================================================================
%% 9. 非法 route_spec 拒绝
%% ===================================================================

register_rejects_malformed_spec_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        BadSpec = not_a_map,
        ?_assertMatch(
            {error, {invalid_route_spec, _}},
            imboy_router_registry:register(channel, [BadSpec])
        )
    end}.

%% ===================================================================
%% 10. handle_info / handle_cast / unknown call
%% ===================================================================

handles_unknown_messages_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        [
            ?_test(begin
                Pid ! {random_info, 42},
                ?assert(is_process_alive(Pid))
            end),
            ?_test(begin
                gen_server:cast(Pid, random_cast),
                ?assert(is_process_alive(Pid))
            end),
            ?_test(begin
                ?assertEqual(
                    {error, unknown_call},
                    gen_server:call(Pid, random_call)
                )
            end)
        ]
    end}.
