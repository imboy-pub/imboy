-module(imboy_plugin_dependency).

%% @status FROZEN (roadmap-only, 2026-06)：v2 动态加载子系统暂停投入。
%% 当前生产走配置驱动模块化单体路线（见 product-profile-and-plugin-registry-design.md §3.1）。
%% 修改前请确认是否真要重启动态平台方向。冻结≠移除。
%% FROZEN: v2 dynamic plugin loading subsystem is suspended (roadmap-only).
%% Current production route: config-driven monolith. See §3.1 before resuming.

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_dependency - 插件依赖解析（纯函数）
%%% Plugin dependency resolver (pure functions)
%%%
%%% 提供 / Provides:
%%%   - parse_semver/1: 解析 X.Y.Z[-prerelease] 字符串
%%%   - parse_constraint/1: 解析 ^X.Y[.Z] 或 X.Y[.Z]（exact）
%%%   - check_constraint/2: 验证版本满足约束
%%%   - validate_constraints/1: 校验所有 manifests 的 depends_on 引用 + 版本约束
%%%   - topological_sort/1: 按依赖关系排序（含循环检测）
%%%
%%% Phase 0 限制（contract.md §3.3）/ Phase 0 limits:
%%%   - 仅支持 ^X.Y.Z 与精确版本（不支持 ~> >= < ||）
%%%   - prerelease 字段记录但不参与 caret 比较语义（KISS）
%%%
%%% Source of truth: docs/plugin/contract.md §3 §9 / roadmap P4-T2
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

-include("imboy_plugin.hrl").

-export([
    parse_semver/1,
    parse_constraint/1,
    check_constraint/2,
    validate_constraints/1,
    topological_sort/1,
    check_enable_deps/1,
    find_dependents/1
]).

-type version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer(), [string()]}.
-type constraint() :: {caret | exact, version()}.

%% ===================================================================
%% Public API — semver / constraint
%% ===================================================================

%% @doc 解析 semver 字符串 X.Y.Z 或 X.Y.Z-prerelease。
-spec parse_semver(binary()) -> {ok, version()} | {error, invalid}.
parse_semver(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<"-">>, [global]) of
        [Core] ->
            parse_strict_core(Core, []);
        [Core | PreParts] ->
            PreStrs = [binary_to_list(P) || P <- PreParts],
            JoinedPre = string:join(PreStrs, "-"),
            ExpandedPre = string:split(JoinedPre, ".", all),
            parse_strict_core(Core, ExpandedPre)
    end;
parse_semver(_) ->
    {error, invalid}.

%% @doc 解析约束 ^X.Y[.Z] 或精确版本 X.Y[.Z]。
%% 接受宽松的 X.Y（缺 patch 默认为 0），与 contract.md §3.2 example 一致。
-spec parse_constraint(binary()) -> {ok, constraint()} | {error, invalid}.
parse_constraint(<<"^", Rest/binary>>) when byte_size(Rest) > 0 ->
    case parse_lenient(Rest) of
        {ok, V} -> {ok, {caret, V}};
        E -> E
    end;
parse_constraint(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    case parse_lenient(Bin) of
        {ok, V} -> {ok, {exact, V}};
        E -> E
    end;
parse_constraint(_) ->
    {error, invalid}.

%% @doc 验证 version 是否满足 constraint。
%% caret: major 相同 + (minor, patch) ≥ (Cmin, Cpat)
%% exact: 版本三元组完全相等（prerelease 也参与比较）
-spec check_constraint(version(), constraint()) -> boolean().
check_constraint({Vmaj, Vmin, Vpat, _}, {caret, {Cmaj, Cmin, Cpat, _}}) ->
    Vmaj =:= Cmaj andalso {Vmin, Vpat} >= {Cmin, Cpat};
check_constraint(V, {exact, V}) ->
    true;
check_constraint(_, {exact, _}) ->
    false.

%% ===================================================================
%% Public API — manifest-level
%% ===================================================================

%% @doc 校验所有 manifests 的 depends_on 引用与版本约束。
%% 失败返回 {error, {Plugin, missing_dep | version_mismatch | invalid_constraint, Detail}}
-spec validate_constraints([map()]) ->
    ok | {error, {atom(), atom(), term()}}.
validate_constraints(Manifests) when is_list(Manifests) ->
    NameToVersion = lists:foldl(
        fun(#{name := N, version := V}, Acc) ->
            case parse_semver(V) of
                {ok, Parsed} -> Acc#{N => Parsed};
                {error, _} -> Acc#{N => {error, invalid_version}}
            end
        end,
        #{},
        Manifests
    ),
    do_validate(Manifests, NameToVersion).

do_validate([], _) ->
    ok;
do_validate([#{name := Name, depends_on := Deps} | Rest], Versions) when
    is_map(Deps)
->
    case check_each_dep(Name, maps:to_list(Deps), Versions) of
        ok -> do_validate(Rest, Versions);
        {error, _} = E -> E
    end;
do_validate([_ | Rest], Versions) ->
    %% manifest 无 depends_on 字段（已过 P0 schema 校验默认 #{}）
    do_validate(Rest, Versions).

check_each_dep(_From, [], _) ->
    ok;
check_each_dep(From, [{DepName, ConstraintBin} | Rest], Versions) when
    is_atom(DepName), is_binary(ConstraintBin)
->
    case maps:find(DepName, Versions) of
        error ->
            {error, {From, missing_dep, DepName}};
        {ok, {error, invalid_version}} ->
            {error, {From, version_mismatch, DepName}};
        {ok, DepVersion} ->
            case parse_constraint(ConstraintBin) of
                {ok, Constraint} ->
                    case check_constraint(DepVersion, Constraint) of
                        true -> check_each_dep(From, Rest, Versions);
                        false -> {error, {From, version_mismatch, DepName}}
                    end;
                {error, _} ->
                    {error, {From, invalid_constraint, ConstraintBin}}
            end
    end.

%% @doc 拓扑排序：按依赖顺序输出 plugin name（被依赖者在前）。
%% Topological sort: dependencies first.
%% 循环依赖返回 {error, {cycle, [PluginName]}}（cycle 中涉及的 plugins）。
-spec topological_sort([map()]) ->
    {ok, [atom()]} | {error, {cycle, [atom()]}}.
topological_sort(Manifests) when is_list(Manifests) ->
    Names = [maps:get(name, M) || M <- Manifests],
    AdjOut = build_adj_out(Manifests),
    InDegree = build_in_degree(AdjOut, Names),
    kahn_sort(AdjOut, InDegree, Names).

%% ===================================================================
%% Internal — semver parse helpers
%% ===================================================================

%% strict: X.Y.Z（必须 3 段）
parse_strict_core(Core, Pre) ->
    case binary:split(Core, <<".">>, [global]) of
        [MajB, MinB, PatB] ->
            ints_to_version(MajB, MinB, PatB, Pre);
        _ ->
            {error, invalid}
    end.

%% lenient: 接受 X.Y 或 X.Y.Z
parse_lenient(Bin) ->
    case binary:split(Bin, <<"-">>, [global]) of
        [Core] -> parse_lenient_core(Core, []);
        %% prerelease 在约束中忽略
        [Core | _] -> parse_lenient_core(Core, [])
    end.

parse_lenient_core(Core, Pre) ->
    case binary:split(Core, <<".">>, [global]) of
        [MajB, MinB] ->
            ints_to_version(MajB, MinB, <<"0">>, Pre);
        [MajB, MinB, PatB] ->
            ints_to_version(MajB, MinB, PatB, Pre);
        _ ->
            {error, invalid}
    end.

ints_to_version(MajB, MinB, PatB, Pre) ->
    try
        Maj = binary_to_integer(MajB),
        Min = binary_to_integer(MinB),
        Pat = binary_to_integer(PatB),
        if
            Maj >= 0, Min >= 0, Pat >= 0 ->
                {ok, {Maj, Min, Pat, Pre}};
            true ->
                {error, invalid}
        end
    catch
        error:badarg -> {error, invalid}
    end.

%% ===================================================================
%% Internal — topological sort (Kahn's algorithm)
%% ===================================================================

%% 邻接表：对每个 manifest M, depends_on 的每个 dep D：D 出边指向 M
%% （即 M 依赖 D，D 完成后 M 才能进入）
build_adj_out(Manifests) ->
    lists:foldl(
        fun(#{name := Self, depends_on := Deps}, Acc) ->
            lists:foldl(
                fun(DepName, A) ->
                    Dependents = maps:get(DepName, A, []),
                    A#{DepName => [Self | Dependents]}
                end,
                Acc,
                maps:keys(Deps)
            )
        end,
        #{},
        Manifests
    ).

build_in_degree(AdjOut, Names) ->
    %% 每个 node 的入度 = 它依赖了多少个其他 plugin
    InitMap = maps:from_list([{N, 0} || N <- Names]),
    maps:fold(
        fun(_DepName, Dependents, Acc) ->
            lists:foldl(
                fun(Dependent, A) ->
                    A#{Dependent => maps:get(Dependent, A, 0) + 1}
                end,
                Acc,
                Dependents
            )
        end,
        InitMap,
        AdjOut
    ).

kahn_sort(AdjOut, InDegree, AllNames) ->
    Queue = [N || N <- AllNames, maps:get(N, InDegree, 0) =:= 0],
    kahn_loop(Queue, AdjOut, InDegree, [], length(AllNames)).

kahn_loop([], _AdjOut, InDegree, Acc, Total) ->
    case length(Acc) =:= Total of
        true ->
            {ok, lists:reverse(Acc)};
        false ->
            Cycle = [N || {N, D} <- maps:to_list(InDegree), D > 0],
            {error, {cycle, lists:sort(Cycle)}}
    end;
kahn_loop([Node | Rest], AdjOut, InDegree, Acc, Total) ->
    Dependents = maps:get(Node, AdjOut, []),
    {NewInDegree, NewQueue} = lists:foldl(
        fun(D, {IDAcc, QAcc}) ->
            NewD = maps:get(D, IDAcc) - 1,
            IDAcc2 = IDAcc#{D => NewD},
            QAcc2 =
                case NewD of
                    0 -> QAcc ++ [D];
                    _ -> QAcc
                end,
            {IDAcc2, QAcc2}
        end,
        {InDegree, Rest},
        Dependents
    ),
    kahn_loop(NewQueue, AdjOut, NewInDegree, [Node | Acc], Total).

%% ===================================================================
%% Cascade — 依赖联动 (lifecycle.md §8)
%% ===================================================================

%% @doc 检查 enable 前置依赖是否满足 (§8.1)。
%% 返回 ok 或 {error, {deps_not_enabled, [Name]}}。
-spec check_enable_deps(map()) -> ok | {error, {deps_not_enabled, [atom()]}}.
check_enable_deps(Manifest) ->
    Deps = maps:get(depends_on, Manifest, #{}),
    NotEnabled = lists:filter(
        fun(DepName) ->
            case get_plugin_state(DepName) of
                enabled -> false;
                _ -> true
            end
        end,
        maps:keys(Deps)
    ),
    case NotEnabled of
        [] -> ok;
        _ -> {error, {deps_not_enabled, NotEnabled}}
    end.

%% @doc 查找哪些已启用插件依赖当前插件 (§8.2)。
%% 返回 [] 或 {error, {has_dependents, [Name]}}。
-spec find_dependents(atom()) -> ok | {error, {has_dependents, [atom()]}}.
find_dependents(PluginName) ->
    AllManifests = get_all_manifests(),
    Dependents = lists:filtermap(
        fun({Name, Manifest}) ->
            Deps = maps:get(depends_on, Manifest, #{}),
            case maps:is_key(PluginName, Deps) of
                true ->
                    case get_plugin_state(Name) of
                        enabled -> {true, Name};
                        _ -> false
                    end;
                false ->
                    false
            end
        end,
        AllManifests
    ),
    case Dependents of
        [] -> ok;
        _ -> {error, {has_dependents, Dependents}}
    end.

%% @doc 从 persistent_term 获取所有已注册的插件 manifest。
get_all_manifests() ->
    PT = persistent_term:get(),
    lists:filtermap(
        fun({Key, Val}) ->
            case Key of
                {imboy_plugin_manifest, Name} -> {true, {Name, Val}};
                _ -> false
            end
        end,
        PT
    ).

%% @doc 获取插件 lifecycle 进程的当前状态（如果存在）。
get_plugin_state(Name) ->
    case persistent_term:get({imboy_plugin_lifecycle, Name}, undefined) of
        undefined ->
            case persistent_term:get({imboy_plugin_manifest, Name}, undefined) of
                undefined -> unknown;
                _ -> installed
            end;
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                false ->
                    catch persistent_term:erase({imboy_plugin_lifecycle, Name}),
                    case persistent_term:get({imboy_plugin_manifest, Name}, undefined) of
                        undefined -> unknown;
                        _ -> installed
                    end;
                true ->
                    try
                        gen_statem:call(Pid, get_state, 1000)
                    catch
                        _:Err ->
                            logger:warning(#{
                                event => plugin_state_call_failed,
                                module => imboy_plugin_dependency,
                                error => Err
                            }),
                            unknown
                    end
            end
    end.
