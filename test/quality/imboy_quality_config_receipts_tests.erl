-module(imboy_quality_config_receipts_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

verified_cleanup_is_idempotent_test() ->
    with_run(fun(Ctrl, Repo) ->
        Config =
            imboy_quality_resources:write_config(Repo, "base", "one", 1, {"old", "new"}) ++
                ".config",
        {ok, #file_info{mode = Mode, links = 1}} = file:read_link_info(Config),
        ?assertEqual(0, Mode band 8#077),
        {ok, <<"private test content">>} = file:read_file(Config),
        Journal = filename:join([Ctrl, "evidence", "config-receipts", "test-run"]),
        {ok, Names} = file:list_dir(Journal),
        lists:foreach(
            fun(N) ->
                {ok, Data} = file:read_file(filename:join(Journal, N)),
                ?assertEqual(nomatch, binary:match(Data, <<"private test content">>))
            end,
            Names
        ),
        ?assertEqual(ok, imboy_quality_resources:cleanup_config_receipts()),
        ?assertEqual({error, enoent}, file:read_link_info(Config)),
        ?assertEqual(ok, imboy_quality_resources:cleanup_config_receipts())
    end).

changed_config_is_retained_test() ->
    with_run(fun(_Ctrl, Repo) ->
        Config =
            imboy_quality_resources:write_config(Repo, "base", "changed", 1, {"old", "new"}) ++
                ".config",
        ok = file:write_file(Config, <<"changed by another writer">>),
        ?assertMatch({error, _}, imboy_quality_resources:cleanup_config_receipts()),
        ?assertEqual({ok, <<"changed by another writer">>}, file:read_file(Config)),
        ?assertMatch({error, _}, imboy_quality_resources:cleanup_config_receipts())
    end).

existing_and_duplicate_paths_are_not_overwritten_test() ->
    with_run(fun(_Ctrl, Repo) ->
        Config =
            imboy_quality_resources:write_config(Repo, "base", "same", 1, {"old", "new"}) ++
                ".config",
        ?assertException(
            error,
            {badmatch, false},
            imboy_quality_resources:write_config(Repo, "base", "same", 1, {"old", "new"})
        ),
        ?assertEqual({ok, <<"private test content">>}, file:read_file(Config)),
        ?assertEqual(ok, imboy_quality_resources:cleanup_config_receipts())
    end).

unconfirmed_intent_retains_unknown_file_test() ->
    with_run(fun(Ctrl, Repo) ->
        Config = filename:join([Repo, "config", "sys.quality.orphan.config"]),
        ok = file:write_file(Config, <<"unknown file">>),
        Hex = string:lowercase(
            binary:encode_hex(crypto:hash(sha256, unicode:characters_to_binary(Config)))
        ),
        Intent = filename:join([
            Ctrl, "evidence", "config-receipts", "test-run", binary_to_list(Hex) ++ ".intent"
        ]),
        ok = file:write_file(
            Intent,
            term_to_binary(#{
                kind => intent, run_id => <<"test-run">>, path => list_to_binary(Config)
            })
        ),
        ?assertMatch({error, _}, imboy_quality_resources:cleanup_config_receipts()),
        ?assertEqual({ok, <<"unknown file">>}, file:read_file(Config)),
        ?assertMatch({error, _}, imboy_quality_resources:cleanup_config_receipts())
    end).

corrupt_completed_receipt_retains_config_test() ->
    with_run(fun(Ctrl, Repo) ->
        Config =
            imboy_quality_resources:write_config(Repo, "base", "corrupt", 1, {"old", "new"}) ++
                ".config",
        Dir = filename:join([Ctrl, "evidence", "config-receipts", "test-run"]),
        [Done] = filelib:wildcard(filename:join(Dir, "*.done")),
        ok = file:write_file(Done, <<"corrupt">>),
        ?assertMatch({error, _}, imboy_quality_resources:cleanup_config_receipts()),
        ?assertEqual({ok, <<"private test content">>}, file:read_file(Config))
    end).

replaced_journal_directory_is_rejected_test() ->
    with_run(fun(Ctrl, Repo) ->
        Config =
            imboy_quality_resources:write_config(Repo, "base", "moved", 1, {"old", "new"}) ++
                ".config",
        Dir = filename:join([Ctrl, "evidence", "config-receipts", "test-run"]),
        ok = file:rename(Dir, Dir ++ "-saved"),
        ok = file:make_dir(Dir),
        ?assertMatch({error, _}, imboy_quality_resources:cleanup_config_receipts()),
        ?assertEqual({ok, <<"private test content">>}, file:read_file(Config))
    end).

concurrent_writers_and_sealing_test() ->
    with_run(fun(_Ctrl, Repo) ->
        Parent = self(),
        [
            spawn(fun() ->
                Result =
                    try
                        imboy_quality_resources:write_config(
                            Repo, "base", integer_to_list(N), N, {"old", "new"}
                        )
                    catch
                        C:R -> {error, C, R}
                    end,
                Parent ! {writer_result, Result}
            end)
         || N <- lists:seq(1, 4)
        ],
        Paths = [
            receive
                {writer_result, P} when is_list(P) -> P ++ ".config";
                {writer_result, E} -> error({writer_failed, E})
            after 3000 -> error(writer_timeout)
            end
         || _ <- lists:seq(1, 4)
        ],
        ?assertEqual(4, length(lists:usort(Paths))),
        ?assertEqual(ok, imboy_quality_resources:cleanup_config_receipts()),
        lists:foreach(fun(P) -> ?assertEqual({error, enoent}, file:read_link_info(P)) end, Paths),
        ?assertException(
            error,
            {badmatch, true},
            imboy_quality_resources:write_config(Repo, "base", "late", 9, {"old", "new"})
        ),
        ?assertEqual(ok, imboy_quality_resources:cleanup_config_receipts())
    end).

existing_json_is_not_overwritten_test() ->
    with_run(fun(_Ctrl, Repo) ->
        Path = filename:join(Repo, "existing.json"),
        ok = file:write_file(Path, <<"preserve">>),
        ?assertException(
            error,
            {badmatch, {error, eexist}},
            imboy_quality_resources:write_json(Path, #{test => true})
        ),
        ?assertEqual({ok, <<"preserve">>}, file:read_file(Path))
    end).

with_run(Fun) ->
    Root = filename:join(
        "/tmp",
        "imboy-config-receipts-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    Ctrl = filename:join(Root, "ctrl"),
    Repo = filename:join(Root, "repo"),
    ok = filelib:ensure_dir(filename:join([Ctrl, "evidence", "x"])),
    ok = filelib:ensure_dir(filename:join([Repo, "config", "x"])),
    ok = file:write_file(filename:join(Repo, "base.config"), <<"private test content">>),
    ok = imboy_quality_config_receipts:begin_run(Ctrl, "test-run", Repo),
    try
        Fun(Ctrl, Repo)
    after
        ets:delete(imboy_quality_config_receipts),
        ok = file:del_dir_r(Root)
    end.
