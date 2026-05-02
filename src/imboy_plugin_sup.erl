-module(imboy_plugin_sup).
-behaviour(supervisor).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_sup - 插件总监督树
%%% Plugin top-level supervisor
%%%
%%% 监督 4 个生产插件的 supervisor（channel/moment/location/group_collab）。
%%% 总监督策略 one_for_one：单插件 sup 崩溃仅自身重启，不影响兄弟。
%%%
%%% 启动顺序：由 imboy_sup 在 PluginLoader **之后**启动，确保
%%% loader 已加载 manifest 到 persistent_term，插件 sup 启动时可安全查询。
%%%
%%% Phase 1 切片 1：4 个 plugin sup 当前均无 children worker，骨架先就位；
%%% Phase 1 切片 2+ 当真实 plugin worker 实施时，挂载到对应 sup 下。
%%%
%%% Source of truth: doc/plugin/contract.md §8 + roadmap P1-T1/T2
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    Children = [
        plugin_sup_spec(channel_sup),
        plugin_sup_spec(moment_sup),
        plugin_sup_spec(location_sup),
        plugin_sup_spec(group_collab_sup)
    ],
    {ok, {SupFlags, Children}}.

%% @doc 通用 plugin sup child spec：复用 imboy_plugin_generic_sup 注册不同 local name。
plugin_sup_spec(Name) when is_atom(Name) ->
    #{
        id => Name,
        start => {imboy_plugin_generic_sup, start_link, [Name]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [imboy_plugin_generic_sup]
    }.
