%%% @doc R4-②：eb_retention_purge_worker 的扫批决策套件（纯函数，无 DB）。
%%%
%%% 覆盖：目标清单解析（合法/非法混列）、未启用 no-op、空目标 no-op、
%%% 单目标失败隔离（错误不拖垮整批）、错误/成功计数。
-module(eb_retention_purge_worker_tests).

-include_lib("eunit/include/eunit.hrl").

purge_worker_test_() ->
    {setup, fun setup_env/0, fun restore_env/1, fun cases/1}.

setup_env() ->
    Saved = #{
        enabled => application:get_env(imboy, eb_retention_purge_enabled),
        targets => application:get_env(imboy, eb_retention_purge_targets)
    },
    application:unset_env(imboy, eb_retention_purge_enabled),
    application:unset_env(imboy, eb_retention_purge_targets),
    Saved.

restore_env(Saved) ->
    restore(enabled, maps:get(enabled, Saved)),
    restore(targets, maps:get(targets, Saved)),
    ok.

restore(_Key, undefined) ->
    ok;
restore(Key, {ok, Val}) ->
    application:set_env(imboy, env_key(Key), Val).

env_key(enabled) -> eb_retention_purge_enabled;
env_key(targets) -> eb_retention_purge_targets.

cases(_Saved) ->
    [
        fun parse_targets_splits_valid_and_invalid_entries/0,
        fun run_sweep_disabled_is_noop_even_with_targets/0,
        fun run_sweep_enabled_empty_targets_is_noop/0,
        fun run_sweep_isolates_per_target_errors/0,
        fun run_sweep_counts_invalid_targets_without_calling_them/0
    ].

%% ------------------------------------------------------------------

parse_targets_splits_valid_and_invalid_entries() ->
    Valid = #{org_id => 501, workspace_id => 601},
    Env = [
        Valid,
        #{org_id => 502, workspace_id => 602},
        #{org_id => 0, workspace_id => 603},
        #{org_id => <<"503">>, workspace_id => 604},
        not_a_map,
        #{org_id => 504}
    ],
    {Pairs, Bad} = eb_retention_purge_worker:parse_targets(Env),
    ?assertEqual([{501, 601}, {502, 602}], lists:sort(Pairs)),
    ?assertEqual(4, Bad),
    %% 非列表形状整体 fail-closed：零合法目标
    ?assertEqual({[], 1}, eb_retention_purge_worker:parse_targets(#{org_id => 1})).

run_sweep_disabled_is_noop_even_with_targets() ->
    application:set_env(imboy, eb_retention_purge_enabled, false),
    application:set_env(imboy, eb_retention_purge_targets, [#{org_id => 501, workspace_id => 601}]),
    Never = fun(_O, _W) -> erlang:error(purge_must_not_be_called) end,
    {ok, Summary} = eb_retention_purge_worker:run_sweep(Never),
    ?assertEqual(0, maps:get(swept, Summary)),
    ?assertEqual(0, maps:get(ok, Summary)),
    ?assertEqual(0, maps:get(failed, Summary)).

run_sweep_enabled_empty_targets_is_noop() ->
    application:set_env(imboy, eb_retention_purge_enabled, true),
    application:set_env(imboy, eb_retention_purge_targets, []),
    Never = fun(_O, _W) -> erlang:error(purge_must_not_be_called) end,
    {ok, Summary} = eb_retention_purge_worker:run_sweep(Never),
    ?assertEqual(0, maps:get(swept, Summary)).

run_sweep_isolates_per_target_errors() ->
    application:set_env(imboy, eb_retention_purge_enabled, true),
    application:set_env(
        imboy,
        eb_retention_purge_targets,
        [
            #{org_id => 501, workspace_id => 601},
            #{org_id => 502, workspace_id => 602},
            #{org_id => 503, workspace_id => 603}
        ]
    ),
    Me = self(),
    PurgeFun = fun
        (501, 601) ->
            {ok, #{purged => []}};
        (502, 602) ->
            {error, {clock_unavailable, x}};
        (503, 603) ->
            Me ! swept_third,
            {ok, #{purged => []}}
    end,
    {ok, Summary} = eb_retention_purge_worker:run_sweep(PurgeFun),
    ?assertEqual(3, maps:get(swept, Summary)),
    ?assertEqual(2, maps:get(ok, Summary)),
    ?assertEqual(1, maps:get(failed, Summary)),
    %% 第三个目标在第二个失败后仍被执行（互不拖累）
    receive
        swept_third -> ok
    after 0 ->
        erlang:error(third_target_skipped)
    end.

run_sweep_counts_invalid_targets_without_calling_them() ->
    application:set_env(imboy, eb_retention_purge_enabled, true),
    application:set_env(
        imboy,
        eb_retention_purge_targets,
        [#{org_id => 501, workspace_id => 601}, invalid_entry]
    ),
    PurgeFun = fun
        (501, 601) -> {ok, #{}};
        (_O, _W) -> erlang:error(purge_must_not_be_called_for_invalid_target)
    end,
    {ok, Summary} = eb_retention_purge_worker:run_sweep(PurgeFun),
    ?assertEqual(1, maps:get(swept, Summary)),
    ?assertEqual(1, maps:get(invalid_targets, Summary)).
