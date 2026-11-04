-module(imboy_quality_full_summary_tests).
-include_lib("eunit/include/eunit.hrl").

skip_is_counted_test() ->
    Counts = #{pass => 2, fail => 0, skip => 3, cancel => 0},
    Roots = #{synthetic_root => #{exit => ok, counts => Counts}},
    Summary = imboy_quality_full:summarize(Roots, [synthetic_root]),
    ?assertEqual(3, maps:get(<<"skip">>, Summary)),
    ?assertEqual([{synthetic_root, 5}], imboy_quality_full:root_identity_set(Roots)).

unknown_skip_is_not_zero_test() ->
    Roots = #{synthetic_root => #{exit => ok, counts => #{pass => 2, fail => 0, cancel => 0}}},
    ?assertException(error, {badkey, skip}, imboy_quality_full:summarize(Roots, [synthetic_root])).

all_counts_are_required_and_nonnegative_test() ->
    Counts = #{pass => 1, fail => 0, skip => 0, cancel => 0},
    lists:foreach(
        fun(Key) ->
            Roots = #{synthetic_root => #{exit => ok, counts => maps:remove(Key, Counts)}},
            ?assertException(
                error, {badkey, Key}, imboy_quality_full:summarize(Roots, [synthetic_root])
            )
        end,
        [pass, fail, skip, cancel]
    ),
    lists:foreach(
        fun(Key) ->
            Invalid = Counts#{Key := -1},
            Roots = #{synthetic_root => #{exit => ok, counts => Invalid}},
            ?assertException(
                error,
                {invalid_counts, Invalid},
                imboy_quality_full:summarize(Roots, [synthetic_root])
            )
        end,
        [pass, fail, skip, cancel]
    ),
    InvalidType = Counts#{pass := 1.0},
    ?assertException(
        error,
        {invalid_counts, InvalidType},
        imboy_quality_full:summarize(
            #{synthetic_root => #{exit => ok, counts => InvalidType}}, [synthetic_root]
        )
    ).

zero_leaf_root_is_not_success_test() ->
    Counts = #{pass => 0, fail => 0, skip => 0, cancel => 0},
    Summary = imboy_quality_full:summarize(
        #{synthetic_root => #{exit => ok, counts => Counts}}, [synthetic_root]
    ),
    ?assertMatch(
        #{
            <<"roots_ok">> := 0,
            <<"roots_failed">> := 0,
            <<"roots_zero_leaf">> := 1,
            <<"zero_leaf_roots">> := [<<"synthetic_root">>]
        },
        Summary
    ),
    ?assertNotEqual(1, maps:get(<<"roots_ok">>, Summary)).

known_auxiliary_root_is_reported_without_omission_test() ->
    Helper = channel_creation_tx_pg_checks,
    Caller = workspace_creation_pg_tests,
    Roots = [Helper, Caller],
    PerRoot = #{
        Helper => #{exit => ok, counts => #{pass => 0, fail => 0, skip => 0, cancel => 0}},
        Caller => #{exit => ok, counts => #{pass => 3, fail => 0, skip => 0, cancel => 0}}
    },
    Summary = imboy_quality_full:summarize(PerRoot, Roots),
    ?assertMatch(
        #{
            <<"roots_ok">> := 1,
            <<"roots_failed">> := 0,
            <<"roots_zero_leaf">> := 1,
            <<"zero_leaf_roots">> := [<<"channel_creation_tx_pg_checks">>]
        },
        Summary
    ),
    ?assertEqual(
        [{channel_creation_tx_pg_checks, 0}, {workspace_creation_pg_tests, 3}],
        imboy_quality_full:root_identity_set(PerRoot)
    ),
    ?assertNotEqual(length(Roots), maps:get(<<"roots_ok">>, Summary)).

missing_root_is_reported_test() ->
    Summary = imboy_quality_full:summarize(#{}, [synthetic_root]),
    ?assertEqual(1, maps:get(<<"roots_missing">>, Summary)).

cleanup_requires_resources_and_configs_test() ->
    Good = #{<<"ok">> => true, <<"clones_dropped">> => [<<"owned">>]},
    ?assertMatch(
        #{<<"ok">> := true, <<"config_cleanup">> := <<"ok">>},
        imboy_quality_full:combine_cleanup(Good, ok)
    ),
    ?assertMatch(
        #{<<"ok">> := false},
        imboy_quality_full:combine_cleanup(Good, {error, ownership_not_verified})
    ),
    ?assertMatch(
        #{<<"ok">> := false},
        imboy_quality_full:combine_cleanup(Good#{<<"ok">> := false}, ok)
    ),
    ?assertMatch(
        #{<<"ok">> := false},
        imboy_quality_full:combine_cleanup({error, cleanup_crash}, ok)
    ).

existing_run_evidence_is_not_overwritten_test() ->
    Ctrl = filename:join("/private/tmp", "imboy-full-existing-" ++ os:getpid()),
    Existing = filename:join([Ctrl, "evidence", "full-duplicate", "per_root.term"]),
    ok = filelib:ensure_dir(Existing),
    ok = file:write_file(Existing, <<"preserve existing evidence">>, [exclusive]),
    try
        ?assertException(
            error,
            {badmatch, {error, unsafe_or_existing_path}},
            imboy_quality_full:run([], #{repo => Ctrl, ctrl => Ctrl}, "duplicate")
        ),
        ?assertEqual({ok, <<"preserve existing evidence">>}, file:read_file(Existing))
    after
        file:delete(Existing),
        file:del_dir(filename:dirname(Existing)),
        file:del_dir(filename:join(Ctrl, "evidence")),
        file:del_dir(Ctrl)
    end.

terminal_result_is_exclusive_test() ->
    OutDir = filename:join("/private/tmp", "imboy-final-result-" ++ os:getpid()),
    ok = file:make_dir(OutDir),
    Path = filename:join(OutDir, "result.json"),
    try
        ok = imboy_quality_full:write_terminal_result(OutDir, #{<<"ok">> => false}),
        {ok, Before} = file:read_file(Path),
        ?assertException(
            error,
            {badmatch, {error, eexist}},
            imboy_quality_full:write_terminal_result(OutDir, #{<<"ok">> => true})
        ),
        ?assertEqual({ok, Before}, file:read_file(Path))
    after
        file:delete(Path),
        file:del_dir(OutDir)
    end.

final_result_waits_for_both_cleanups_test() ->
    OutDir = filename:join("/private/tmp", "imboy-final-cleanup-" ++ os:getpid()),
    ok = file:make_dir(OutDir),
    Path = filename:join(OutDir, "result.json"),
    try
        Release = fun() ->
            ?assertNot(filelib:is_regular(Path)),
            put(cleanup_order, [resource]),
            #{<<"ok">> => true}
        end,
        Config = fun() ->
            ?assertNot(filelib:is_regular(Path)),
            ?assertEqual([resource], get(cleanup_order)),
            put(cleanup_order, [resource, config]),
            {error, unverified_config}
        end,
        {Result, OutDir} = imboy_quality_full:finalize_run(
            OutDir, #{<<"run_id">> => <<"test">>}, Release, Config
        ),
        ?assertMatch(#{<<"cleanup">> := #{<<"ok">> := false}}, Result),
        ?assertEqual([resource, config], get(cleanup_order)),
        Cleanup = maps:get(<<"cleanup">>, Result),
        ?assertEqual(<<"{error,unverified_config}">>, maps:get(<<"config_cleanup">>, Cleanup)),
        ?assertNot(maps:is_key(<<"resource_cleanup_error">>, Cleanup)),
        {ok, Bytes} = file:read_file(Path),
        ?assertMatch(#{<<"cleanup">> := #{<<"ok">> := false}}, json:decode(Bytes))
    after
        erase(cleanup_order),
        file:delete(Path),
        file:del_dir(OutDir)
    end.

config_cleanup_runs_after_resource_failure_test() ->
    OutDir = filename:join("/private/tmp", "imboy-final-error-" ++ os:getpid()),
    ok = file:make_dir(OutDir),
    Path = filename:join(OutDir, "result.json"),
    try
        Release = fun() -> error(simulated_failure) end,
        Config = fun() ->
            put(config_cleanup_called, true),
            ok
        end,
        {Result, OutDir} = imboy_quality_full:finalize_run(OutDir, #{}, Release, Config),
        ?assertEqual(true, get(config_cleanup_called)),
        ?assertMatch(#{<<"cleanup">> := #{<<"ok">> := false}}, Result),
        ?assert(filelib:is_regular(Path))
    after
        erase(config_cleanup_called),
        file:delete(Path),
        file:del_dir(OutDir)
    end.

alias_created_is_removed_and_existing_is_preserved_test() ->
    with_alias_repo(fun(Repo, Alias) ->
        Created = imboy_quality_full:ensure_lib_alias(Repo),
        ?assertMatch({created, _}, Created),
        ?assertEqual(ok, imboy_quality_full:maybe_remove_lib_alias(Repo, Created)),
        ?assertEqual({error, enoent}, file:read_link_info(Alias)),
        ok = file:make_symlink(".", Alias),
        ?assertEqual(existing, imboy_quality_full:ensure_lib_alias(Repo)),
        ?assertEqual(ok, imboy_quality_full:maybe_remove_lib_alias(Repo, existing)),
        ?assertEqual({ok, "."}, file:read_link(Alias))
    end).

alias_replaced_is_not_removed_test() ->
    with_alias_repo(fun(Repo, Alias) ->
        Created = imboy_quality_full:ensure_lib_alias(Repo),
        Saved = Alias ++ ".saved",
        ok = file:rename(Alias, Saved),
        ok = file:make_symlink(".", Alias),
        ?assertEqual(
            {error, alias_identity_changed},
            imboy_quality_full:maybe_remove_lib_alias(Repo, Created)
        ),
        ?assertEqual({ok, "."}, file:read_link(Alias)),
        ok = file:delete(Saved)
    end).

alias_delete_failure_is_reported_test() ->
    with_alias_repo(fun(Repo, Alias) ->
        Created = imboy_quality_full:ensure_lib_alias(Repo),
        Delete = fun(Path) ->
            ?assertEqual(Alias, Path),
            {error, eacces}
        end,
        ?assertEqual(
            {error, eacces}, imboy_quality_full:maybe_remove_lib_alias(Repo, Created, Delete)
        ),
        ?assertEqual({ok, "."}, file:read_link(Alias))
    end).

alias_failure_prevents_final_cleanup_success_test() ->
    OutDir = filename:join("/private/tmp", "imboy-final-alias-" ++ os:getpid()),
    ok = file:make_dir(OutDir),
    Path = filename:join(OutDir, "result.json"),
    try
        Input = #{<<"alias_cleanup">> => #{<<"ok">> => false, <<"error">> => <<"eacces">>}},
        {Result, OutDir} = imboy_quality_full:finalize_run(
            OutDir,
            Input,
            fun() -> #{<<"ok">> => true} end,
            fun() -> ok end
        ),
        ?assertMatch(
            #{<<"cleanup">> := #{<<"ok">> := false, <<"alias">> := #{<<"ok">> := false}}}, Result
        )
    after
        file:delete(Path),
        file:del_dir(OutDir)
    end.

verified_alias_allows_cleanup_success_test() ->
    OutDir = filename:join("/private/tmp", "imboy-final-alias-ok-" ++ os:getpid()),
    ok = file:make_dir(OutDir),
    Path = filename:join(OutDir, "result.json"),
    try
        Input = #{<<"alias_cleanup">> => #{<<"ok">> => true}},
        {Result, OutDir} = imboy_quality_full:finalize_run(
            OutDir,
            Input,
            fun() -> #{<<"ok">> => true} end,
            fun() -> ok end
        ),
        ?assertMatch(
            #{<<"cleanup">> := #{<<"ok">> := true, <<"alias">> := #{<<"ok">> := true}}}, Result
        )
    after
        file:delete(Path),
        file:del_dir(OutDir)
    end.

with_alias_repo(Fun) ->
    Repo = filename:join("/private/tmp", "imboy-alias-test-" ++ os:getpid()),
    ok = file:make_dir(Repo),
    Alias = filename:join(Repo, "imboy"),
    try
        Fun(Repo, Alias)
    after
        file:delete(Alias),
        file:delete(Alias ++ ".saved"),
        file:del_dir(Repo)
    end.
