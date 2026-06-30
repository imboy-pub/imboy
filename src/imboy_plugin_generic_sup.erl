-module(imboy_plugin_generic_sup).
-behaviour(supervisor).

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_generic_sup - 通用空插件 supervisor 模板
%%% Generic empty plugin supervisor template
%%%
%%% Phase 1 切片 1：4 个插件（channel/moment/location/group_collab）
%%% 当前均无专属 worker，使用本通用模板注册不同 local name 即可。
%%%
%%% Phase 1+ 当真实插件 worker 出现时，可：
%%%   (a) 把 worker 直接挂载（动态 supervisor:start_child/2）
%%%   (b) 拆出独立模块替代本模板
%%%
%%% 监督策略 / Supervision strategy:
%%%   - one_for_one: 单 worker 崩溃仅自身重启，不影响兄弟
%%%   - intensity=5/period=10: 10 秒内 5 次重启即终止本 sup
%%%   - 与 contract.md §8.1 + roadmap §5 设计原则对齐
%%%
%%% Source of truth: docs/plugin/contract.md §8 + roadmap P1
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-export([start_link/1, init/1]).

-spec start_link(Name :: atom()) -> {ok, pid()} | {error, term()}.
start_link(Name) when is_atom(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, [Name]).

init([Name]) ->
    _ = elib_metric:increment(plugin_sup_starts, 1, #{plugin => Name}),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    Children = [],
    {ok, {SupFlags, Children}}.
