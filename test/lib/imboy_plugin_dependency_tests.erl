-module(imboy_plugin_dependency_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_dependency 的 EUnit 测试（P4 切片 1：依赖解析纯函数）
%%%
%%% 覆盖 / Coverage:
%%%   - parse_semver/1: 6 项（合法/prerelease/拒绝）
%%%   - parse_constraint/1: 6 项（caret/exact/lenient/拒绝）
%%%   - check_constraint/2: 6 项（caret 满足/不满足/exact）
%%%   - validate_constraints/1: 5 项（pass/missing_dep/version_mismatch/invalid_constraint）
%%%   - topological_sort/1: 6 项（线性/扇出/扇入/独立/循环 2/循环 3）
%%% @end
%%%-------------------------------------------------------------------

%% Helper: 构造最小 manifest
m(Name, Version) ->
    m(Name, Version, #{}).
m(Name, Version, Deps) ->
    #{name => Name, version => list_to_binary(Version), depends_on => Deps}.

%% ===================================================================
%% parse_semver/1
%% ===================================================================

parse_semver_basic_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, {1, 0, 0, []}},
                     imboy_plugin_dependency:parse_semver(<<"1.0.0">>))
    end).

parse_semver_high_numbers_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, {12, 34, 567, []}},
                     imboy_plugin_dependency:parse_semver(<<"12.34.567">>))
    end).

parse_semver_with_prerelease_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, {1, 0, 0, ["rc", "2"]}},
                     imboy_plugin_dependency:parse_semver(<<"1.0.0-rc.2">>))
    end).

parse_semver_two_segments_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, invalid},
                     imboy_plugin_dependency:parse_semver(<<"1.0">>))
    end).

parse_semver_non_numeric_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, invalid},
                     imboy_plugin_dependency:parse_semver(<<"a.b.c">>))
    end).

parse_semver_non_binary_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, invalid},
                     imboy_plugin_dependency:parse_semver("1.0.0"))
    end).

%% ===================================================================
%% parse_constraint/1
%% ===================================================================

parse_constraint_caret_full_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, {caret, {1, 0, 0, []}}},
                     imboy_plugin_dependency:parse_constraint(<<"^1.0.0">>))
    end).

parse_constraint_caret_lenient_test_() ->
    %% contract.md §3.2 example: moment = "^1.0"（缺 patch）
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, {caret, {1, 0, 0, []}}},
                     imboy_plugin_dependency:parse_constraint(<<"^1.0">>))
    end).

parse_constraint_exact_full_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, {exact, {2, 5, 9, []}}},
                     imboy_plugin_dependency:parse_constraint(<<"2.5.9">>))
    end).

parse_constraint_exact_lenient_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, {exact, {1, 5, 0, []}}},
                     imboy_plugin_dependency:parse_constraint(<<"1.5">>))
    end).

parse_constraint_empty_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, invalid},
                     imboy_plugin_dependency:parse_constraint(<<"">>))
    end).

parse_constraint_caret_alone_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({error, invalid},
                     imboy_plugin_dependency:parse_constraint(<<"^">>))
    end).

%% ===================================================================
%% check_constraint/2
%% ===================================================================

check_caret_same_minor_passes_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(imboy_plugin_dependency:check_constraint(
            {1, 5, 3, []},
            {caret, {1, 0, 0, []}}
        ))
    end).

check_caret_higher_minor_passes_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(imboy_plugin_dependency:check_constraint(
            {1, 99, 0, []},
            {caret, {1, 5, 0, []}}
        ))
    end).

check_caret_lower_minor_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertNot(imboy_plugin_dependency:check_constraint(
            {1, 0, 0, []},
            {caret, {1, 5, 0, []}}
        ))
    end).

check_caret_diff_major_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertNot(imboy_plugin_dependency:check_constraint(
            {2, 0, 0, []},
            {caret, {1, 0, 0, []}}
        ))
    end).

check_exact_match_passes_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(imboy_plugin_dependency:check_constraint(
            {1, 0, 0, []},
            {exact, {1, 0, 0, []}}
        ))
    end).

check_exact_mismatch_fails_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertNot(imboy_plugin_dependency:check_constraint(
            {1, 0, 1, []},
            {exact, {1, 0, 0, []}}
        ))
    end).

%% ===================================================================
%% validate_constraints/1
%% ===================================================================

validate_no_deps_passes_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [m(channel, "1.0.0"), m(moment, "1.0.0")],
        ?assertEqual(ok, imboy_plugin_dependency:validate_constraints(Manifests))
    end).

validate_satisfied_caret_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(moment, "1.5.0"),
            m(channel, "1.0.0", #{moment => <<"^1.0">>})
        ],
        ?assertEqual(ok, imboy_plugin_dependency:validate_constraints(Manifests))
    end).

validate_missing_dep_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(channel, "1.0.0", #{moment => <<"^1.0">>})  %% moment 不在列表
        ],
        ?assertMatch({error, {channel, missing_dep, moment}},
                     imboy_plugin_dependency:validate_constraints(Manifests))
    end).

validate_version_mismatch_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(moment, "2.0.0"),
            m(channel, "1.0.0", #{moment => <<"^1.0">>})
        ],
        ?assertMatch({error, {channel, version_mismatch, moment}},
                     imboy_plugin_dependency:validate_constraints(Manifests))
    end).

validate_invalid_constraint_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(moment, "1.0.0"),
            m(channel, "1.0.0", #{moment => <<"not-a-version">>})
        ],
        ?assertMatch({error, {channel, invalid_constraint, _}},
                     imboy_plugin_dependency:validate_constraints(Manifests))
    end).

%% ===================================================================
%% topological_sort/1
%% ===================================================================

topo_linear_chain_test_() ->
    %% c -> b -> a (a 是叶，c 依赖 b 依赖 a)
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(c, "1.0.0", #{b => <<"^1.0">>}),
            m(b, "1.0.0", #{a => <<"^1.0">>}),
            m(a, "1.0.0")
        ],
        {ok, Order} = imboy_plugin_dependency:topological_sort(Manifests),
        APos = index_of(a, Order),
        BPos = index_of(b, Order),
        CPos = index_of(c, Order),
        ?assert(APos < BPos),
        ?assert(BPos < CPos)
    end).

topo_independent_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [m(x, "1.0.0"), m(y, "1.0.0"), m(z, "1.0.0")],
        {ok, Order} = imboy_plugin_dependency:topological_sort(Manifests),
        ?assertEqual(lists:sort([x, y, z]), lists:sort(Order))
    end).

topo_fan_in_test_() ->
    %% a 被 b/c/d 共同依赖
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(a, "1.0.0"),
            m(b, "1.0.0", #{a => <<"^1.0">>}),
            m(c, "1.0.0", #{a => <<"^1.0">>}),
            m(d, "1.0.0", #{a => <<"^1.0">>})
        ],
        {ok, Order} = imboy_plugin_dependency:topological_sort(Manifests),
        APos = index_of(a, Order),
        BPos = index_of(b, Order),
        CPos = index_of(c, Order),
        DPos = index_of(d, Order),
        ?assert(APos < BPos),
        ?assert(APos < CPos),
        ?assert(APos < DPos)
    end).

topo_fan_out_test_() ->
    %% a 同时依赖 b/c/d
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(a, "1.0.0", #{b => <<"^1.0">>, c => <<"^1.0">>, d => <<"^1.0">>}),
            m(b, "1.0.0"),
            m(c, "1.0.0"),
            m(d, "1.0.0")
        ],
        {ok, Order} = imboy_plugin_dependency:topological_sort(Manifests),
        APos = index_of(a, Order),
        BPos = index_of(b, Order),
        CPos = index_of(c, Order),
        DPos = index_of(d, Order),
        ?assert(BPos < APos),
        ?assert(CPos < APos),
        ?assert(DPos < APos)
    end).

topo_two_node_cycle_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(a, "1.0.0", #{b => <<"^1.0">>}),
            m(b, "1.0.0", #{a => <<"^1.0">>})
        ],
        ?assertMatch({error, {cycle, [a, b]}},
                     imboy_plugin_dependency:topological_sort(Manifests))
    end).

topo_three_node_cycle_test_() ->
    ?TEST_SIMPLE(fun() ->
        Manifests = [
            m(a, "1.0.0", #{b => <<"^1.0">>}),
            m(b, "1.0.0", #{c => <<"^1.0">>}),
            m(c, "1.0.0", #{a => <<"^1.0">>})
        ],
        ?assertMatch({error, {cycle, [a, b, c]}},
                     imboy_plugin_dependency:topological_sort(Manifests))
    end).

%% ===================================================================
%% Helpers
%% ===================================================================

index_of(Elem, List) ->
    index_of_loop(Elem, List, 0).

index_of_loop(_, [], _) -> -1;
index_of_loop(E, [E | _], N) -> N;
index_of_loop(E, [_ | T], N) -> index_of_loop(E, T, N + 1).
