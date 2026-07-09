-module(imboy_plugin_toml_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_toml 模块的 EUnit 测试（切片 1：纯函数）
%%% EUnit tests for imboy_plugin_toml (slice 1: pure functions)
%%%
%%% 覆盖 / Coverage:
%%%   - parse_contract_version/1: 10 个 happy/sad path
%%%   - validate_manifest/1: 1 个 happy + 12 个 negative + 1 个 audience-all happy
%%%   - load/1 (切片 2): 5 个 file I/O case (合法/缺失/多 term/非 map/校验失败)
%%%
%%% Mock baseline: imboy_plugin_dummy:manifest()
%%% Source of truth: docs/plugin/contract.md §3.3
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% parse_contract_version/1
%% ===================================================================

parse_contract_version_valid_one_zero_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({1, 0}, imboy_plugin_toml:parse_contract_version(<<"1.0">>))
    end).

parse_contract_version_valid_two_three_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({2, 3}, imboy_plugin_toml:parse_contract_version(<<"2.3">>))
    end).

parse_contract_version_valid_high_minor_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({1, 99}, imboy_plugin_toml:parse_contract_version(<<"1.99">>))
    end).

parse_contract_version_zero_major_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_format},
            imboy_plugin_toml:parse_contract_version(<<"0.5">>)
        )
    end).

parse_contract_version_negative_minor_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_format},
            imboy_plugin_toml:parse_contract_version(<<"1.-1">>)
        )
    end).

parse_contract_version_missing_minor_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_format},
            imboy_plugin_toml:parse_contract_version(<<"1">>)
        )
    end).

parse_contract_version_three_segments_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_format},
            imboy_plugin_toml:parse_contract_version(<<"1.0.0">>)
        )
    end).

parse_contract_version_non_integer_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_format},
            imboy_plugin_toml:parse_contract_version(<<"a.b">>)
        )
    end).

parse_contract_version_empty_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_format},
            imboy_plugin_toml:parse_contract_version(<<"">>)
        )
    end).

parse_contract_version_non_binary_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            {error, invalid_format},
            imboy_plugin_toml:parse_contract_version("1.0")
        )
    end).

%% ===================================================================
%% validate_manifest/1 — happy path
%% ===================================================================

validate_manifest_dummy_passes_test_() ->
    %% imboy_plugin_dummy:manifest/0 是契约 §4.3 的合规实现，应 PASS
    ?TEST_SIMPLE(fun() ->
        M = imboy_plugin_dummy:manifest(),
        ?assertMatch({ok, _}, imboy_plugin_toml:validate_manifest(M))
    end).

%% ===================================================================
%% validate_manifest/1 — negative cases
%% ===================================================================

validate_manifest_non_map_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch(
            {error, {manifest, not_a_map}},
            imboy_plugin_toml:validate_manifest(undefined)
        )
    end).

validate_manifest_missing_name_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = maps:remove(name, imboy_plugin_dummy:manifest()),
        ?assertMatch({error, {name, _}}, imboy_plugin_toml:validate_manifest(M))
    end).

validate_manifest_invalid_name_format_rejected_test_() ->
    %% 大写字母不合规
    ?TEST_SIMPLE(fun() ->
        M = (imboy_plugin_dummy:manifest())#{name := 'BadName'},
        ?assertMatch(
            {error, {name, invalid_format}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_invalid_version_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = (imboy_plugin_dummy:manifest())#{version := <<"not-semver">>},
        ?assertMatch(
            {error, {version, invalid_semver}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_invalid_contract_version_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = (imboy_plugin_dummy:manifest())#{contract_version := {0, 0}},
        ?assertMatch(
            {error, {contract_version, _}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_invalid_kind_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = (imboy_plugin_dummy:manifest())#{kind := unknown_kind},
        ?assertMatch(
            {error, {kind, _}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_table_prefix_mismatch_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        Bad = #{
            dir => <<"priv/migrations">>,
            table_prefix => <<"wrong_prefix_">>,
            preserve_on_uninstall => true
        },
        M = (imboy_plugin_dummy:manifest())#{migrations := Bad},
        ?assertMatch(
            {error, {migrations_table_prefix, _}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_route_namespace_violation_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        BadRoute = #{
            method => <<"GET">>,
            %% path 不以 /api/v{n}/<name>/ 开头
            path => <<"/api/v1/wrong/x">>,
            handler => some_handler,
            action => some_action
        },
        M = (imboy_plugin_dummy:manifest())#{routes := [BadRoute]},
        ?assertMatch(
            {error, {routes, _}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_invalid_degrade_enum_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = (imboy_plugin_dummy:manifest())#{
            degrade := #{on_unhealthy => crash}
        },
        ?assertMatch(
            {error, {degrade_on_unhealthy, _}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_buckets_too_many_rejected_test_() ->
    %% 长度 101，触发 too_many（优先于 duplicates）
    ?TEST_SIMPLE(fun() ->
        Buckets = lists:duplicate(101, 0),
        Audience = #{kind => uid_hash, buckets => Buckets},
        Feature = #{
            default => false,
            description => <<"x">>,
            rollout => canary,
            percentage => 1,
            audience => Audience
        },
        M = (imboy_plugin_dummy:manifest())#{features := #{f => Feature}},
        ?assertMatch(
            {error, {audience_buckets, too_many}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_buckets_duplicates_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        Audience = #{kind => uid_hash, buckets => [1, 1, 2]},
        Feature = #{
            default => false,
            description => <<"x">>,
            rollout => canary,
            percentage => 1,
            audience => Audience
        },
        M = (imboy_plugin_dummy:manifest())#{features := #{f => Feature}},
        ?assertMatch(
            {error, {audience_buckets, duplicates}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_buckets_out_of_range_rejected_test_() ->
    %% 100 超出 [0..99]
    ?TEST_SIMPLE(fun() ->
        Audience = #{kind => uid_hash, buckets => [100]},
        Feature = #{
            default => false,
            description => <<"x">>,
            rollout => canary,
            percentage => 1,
            audience => Audience
        },
        M = (imboy_plugin_dummy:manifest())#{features := #{f => Feature}},
        ?assertMatch(
            {error, {audience_buckets, out_of_range}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_audience_all_passes_test_() ->
    %% audience.kind = all 不触发 buckets 校验
    ?TEST_SIMPLE(fun() ->
        Feature = #{
            default => true,
            description => <<"x">>,
            rollout => always,
            audience => #{kind => all}
        },
        M = (imboy_plugin_dummy:manifest())#{features := #{f => Feature}},
        ?assertMatch({ok, _}, imboy_plugin_toml:validate_manifest(M))
    end).

%% ===================================================================
%% V2 评审 HIGH-2 修复：required keys 完整性校验
%% Validators must reject manifests missing any := field
%% ===================================================================

validate_manifest_missing_limits_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = maps:remove(limits, imboy_plugin_dummy:manifest()),
        ?assertMatch(
            {error, {limits, missing}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_missing_budget_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = maps:remove(budget, imboy_plugin_dummy:manifest()),
        ?assertMatch(
            {error, {budget, missing}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_missing_circuit_breaker_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = maps:remove(circuit_breaker, imboy_plugin_dummy:manifest()),
        ?assertMatch(
            {error, {circuit_breaker, missing}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_missing_entries_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = maps:remove(entries, imboy_plugin_dummy:manifest()),
        ?assertMatch(
            {error, {entries, missing}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_missing_publishes_meta_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = maps:remove(publishes_meta, imboy_plugin_dummy:manifest()),
        ?assertMatch(
            {error, {publishes_meta, missing}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

validate_manifest_missing_audit_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = maps:remove(audit, imboy_plugin_dummy:manifest()),
        ?assertMatch(
            {error, {audit, missing}},
            imboy_plugin_toml:validate_manifest(M)
        )
    end).

%% ===================================================================
%% load/1 — 切片 2：从文件读取并解析（Erlang terms 格式）
%% load/1 — slice 2: load and parse from file (Erlang terms format)
%%
%% 测试策略 / Strategy:
%%   - 在 /tmp 下生成临时 .config 文件，断言 load/1 行为
%%   - cleanup 阶段删除临时文件，避免残留
%%   - 不依赖 fixture 目录布局，可在任意 cwd 下跑
%% ===================================================================

%% Helper：生成唯一临时文件路径 / Generate unique temp file path
tmp_config_path() ->
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    filename:join("/tmp", "imboy_plugin_toml_test_" ++ Suffix ++ ".config").

%% Helper：将 term 写为 Erlang term 文件
write_term(Path, Term) ->
    Content = io_lib:format("~p.~n", [Term]),
    ok = file:write_file(Path, Content).

%% Helper：写任意原始字符串
write_raw(Path, Content) ->
    ok = file:write_file(Path, Content).

load_valid_manifest_returns_ok_test_() ->
    {setup,
        fun() ->
            Path = tmp_config_path(),
            write_term(Path, imboy_plugin_dummy:manifest()),
            Path
        end,
        fun(Path) -> file:delete(Path) end, fun(Path) ->
            ?_assertMatch({ok, _}, imboy_plugin_toml:load(Path))
        end}.

load_file_not_found_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch(
            {error, {load, file_not_found}},
            imboy_plugin_toml:load("/tmp/imboy_plugin_toml_does_not_exist_xyz.config")
        )
    end).

load_multiple_terms_returns_error_test_() ->
    {setup,
        fun() ->
            Path = tmp_config_path(),
            %% 两个 term，违反单一 manifest 约定
            write_raw(Path, "{a, 1}.\n{b, 2}.\n"),
            Path
        end,
        fun(Path) -> file:delete(Path) end, fun(Path) ->
            ?_assertMatch({error, {load, multiple_terms}}, imboy_plugin_toml:load(Path))
        end}.

load_non_map_term_returns_error_test_() ->
    {setup,
        fun() ->
            Path = tmp_config_path(),
            write_term(Path, {not_a_map, 42}),
            Path
        end,
        fun(Path) -> file:delete(Path) end, fun(Path) ->
            ?_assertMatch({error, {load, {not_a_map, _}}}, imboy_plugin_toml:load(Path))
        end}.

load_invalid_manifest_propagates_validation_error_test_() ->
    %% 文件可读且是 map，但 schema 校验失败（name 大写非法）
    {setup,
        fun() ->
            Path = tmp_config_path(),
            M = (imboy_plugin_dummy:manifest())#{name := 'BadName'},
            write_term(Path, M),
            Path
        end,
        fun(Path) -> file:delete(Path) end, fun(Path) ->
            ?_assertMatch({error, {name, invalid_format}}, imboy_plugin_toml:load(Path))
        end}.
