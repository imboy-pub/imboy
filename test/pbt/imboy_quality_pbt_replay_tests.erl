-module(imboy_quality_pbt_replay_tests).
-export([fixture_property/0]).

-include_lib("eunit/include/eunit.hrl").

fixture_property() -> ok.

cross_vm_known_and_unknown_atoms_test() ->
    with_counterexample(fun(_Dir, Path) ->
        Entry = #{
            schema => 1,
            id => pbt_cross_vm_example,
            case_kind => random,
            mfa => {?MODULE, fixture_property, 0},
            input => []
        },
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, term_to_binary(Entry, [compressed])),
        BeamDir = filename:dirname(code:which(?MODULE)),
        Eval =
            "case catch binary_to_existing_atom(<<\"pbt_cross_vm_example\">>,utf8) of {'EXIT',_}->ok; _->halt(5) end, "
            "{module,imboy_quality_pbt}=code:ensure_loaded(imboy_quality_pbt), "
            "{module,imboy_quality_pbt_replay_tests}=code:ensure_loaded(imboy_quality_pbt_replay_tests), "
            "{ok,E}=imboy_quality_pbt:load_counterexample(os:getenv(\"PBT_TEST_FILE\")), "
            "<<\"pbt_cross_vm_example\">>=atom_to_binary(maps:get(id,E)), "
            "halt(0).",
        ?assertEqual(0, child_exit(BeamDir, Path, Eval)),
        Name = <<"imboy_pbt_unknown_cross_vm_794618">>,
        ok = file:write_file(Path, <<131, 118, (byte_size(Name)):16/big, Name/binary>>),
        BadEval =
            "{module,imboy_quality_pbt}=code:ensure_loaded(imboy_quality_pbt), "
            "{error,corrupt_or_unsafe_etf}=imboy_quality_pbt:load_counterexample(os:getenv(\"PBT_TEST_FILE\")), "
            "halt(0).",
        ?assertEqual(0, child_exit(BeamDir, Path, BadEval))
    end).

child_exit(BeamDir, Path, Eval) ->
    Port = open_port({spawn_executable, os:find_executable("erl")}, [
        exit_status,
        stderr_to_stdout,
        binary,
        {args, [
            "+S",
            "1:1",
            "+SDcpu",
            "1",
            "+SDio",
            "1",
            "+A",
            "1",
            "-noshell",
            "-pa",
            BeamDir,
            "-eval",
            Eval
        ]},
        {env, [{"PBT_TEST_FILE", Path}]}
    ]),
    child_wait(Port).

child_wait(Port) ->
    receive
        {Port, {data, _}} -> child_wait(Port);
        {Port, {exit_status, Status}} -> Status
    after 10000 ->
        port_close(Port),
        error(child_timeout)
    end.

valid_compressed_counterexample_loads_test() ->
    with_counterexample(fun(Dir, Path) ->
        Entry = #{
            id => missing_property,
            case_kind => random,
            mfa => {imboy_quality_missing_property, missing, 0},
            input => binary:copy(<<0>>, 4096)
        },
        {ok, Path} = imboy_quality_pbt:save_counterexample(Dir, Entry),
        {ok, <<131, 80, _/binary>>} = file:read_file(Path),
        {ok, Loaded} = imboy_quality_pbt:load_counterexample(Path),
        ?assertEqual(Entry, maps:with(maps:keys(Entry), Loaded))
    end).

missing_property_is_not_recorded_test() ->
    with_counterexample(fun(Dir, Path) ->
        MFA = {imboy_quality_missing_property, missing, 0},
        {ok, Path} = imboy_quality_pbt:save_counterexample(Dir, #{
            id => missing_property, case_kind => random, mfa => MFA, input => []
        }),
        {ok, Before} = file:read_file(Path),
        ?assertMatch(
            {error, {replay_crash, error, undef}},
            imboy_quality_pbt:replay_next(Dir, [MFA])
        ),
        ?assertEqual({ok, Before}, file:read_file(Path))
    end).

corrupt_etf_is_not_ignored_test() ->
    with_counterexample(fun(Dir, Path) ->
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, <<"not an ETF">>),
        ?assertMatch(
            {error, {invalid_counterexample, _, corrupt_or_unsafe_etf}},
            imboy_quality_pbt:replay_next(Dir, [])
        ),
        ?assertEqual({ok, <<"not an ETF">>}, file:read_file(Path))
    end).

oversized_etf_is_rejected_test() ->
    with_counterexample(fun(Dir, Path) ->
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, binary:copy(<<0>>, 1048577)),
        ?assertMatch(
            {error, {invalid_counterexample, _, too_large}},
            imboy_quality_pbt:replay_next(Dir, [])
        )
    end).

compressed_expansion_limit_is_checked_test() ->
    with_counterexample(fun(Dir, Path) ->
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, <<131, 80, 1048577:32/big, 0>>),
        ?assertMatch(
            {error, {invalid_counterexample, _, expanded_too_large}},
            imboy_quality_pbt:replay_next(Dir, [])
        )
    end).

new_atoms_are_rejected_test() ->
    with_counterexample(fun(Dir, Path) ->
        ok = filelib:ensure_dir(Path),
        Name = <<"imboy_pbt_untrusted_atom_not_in_vm_8461">>,
        ?assertException(error, badarg, binary_to_existing_atom(Name, utf8)),
        ok = file:write_file(Path, <<131, 118, (byte_size(Name)):16/big, Name/binary>>),
        ?assertMatch(
            {error, {invalid_counterexample, _, corrupt_or_unsafe_etf}},
            imboy_quality_pbt:replay_next(Dir, [])
        )
    end).

unlisted_mfa_is_rejected_test() ->
    ?assertEqual(
        {error, mfa_not_allowed},
        imboy_quality_pbt:replay_check(
            #{mfa => {imboy_quality_missing_property, missing, 0}, input => []}, []
        )
    ).

invalid_replay_receipt_is_not_ignored_test() ->
    with_counterexample(fun(Dir, Path) ->
        ok = filelib:ensure_dir(Path),
        Entry = #{
            schema => 1,
            id => missing_property,
            case_kind => random,
            mfa => {imboy_quality_missing_property, missing, 0},
            input => [],
            replayed_at => 1,
            replay_result => {error, undef}
        },
        Bin = term_to_binary(Entry),
        ok = file:write_file(Path, Bin),
        ?assertMatch(
            {error, {invalid_counterexample, _, invalid_replay_receipt}},
            imboy_quality_pbt:replay_next(Dir, [])
        ),
        ?assertEqual({ok, Bin}, file:read_file(Path))
    end).

with_counterexample(Fun) ->
    Dir = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "imboy-pbt-replay-" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))
    ),
    ok = file:make_dir(Dir),
    Counterexamples = filename:join(Dir, "counterexamples"),
    Path = filename:join(Counterexamples, "missing_property.etf"),
    try
        Fun(Dir, Path)
    after
        _ = file:delete(Path),
        _ = file:del_dir(Counterexamples),
        ok = file:del_dir(Dir)
    end.
