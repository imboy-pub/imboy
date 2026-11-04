-module(imboy_quality_admission_tests).
-include_lib("eunit/include/eunit.hrl").

admission_binding_test() ->
    with_input(fun(Repo, _Src) -> admission_binding(Repo) end).

admission_binding(Repo) ->
    Head = <<"0123456789012345678901234567890123456789">>,
    Gate = #{<<"G-IMPL">> => <<"ADMITTED">>},
    A = #{
        <<"status">> => <<"ADMITTED">>,
        <<"candidate_sha">> => Head,
        <<"profile">> => <<"PHASE2_INTEGRATED">>,
        <<"input_fingerprint">> => imboy_quality_state:candidate_input_fingerprint(Repo)
    },
    State = #{
        <<"current_gate_status">> => Gate,
        <<"current_admission">> => A,
        <<"worktree">> => list_to_binary(Repo)
    },
    ?assertEqual(phase2_integrated, imboy_quality_state:admission_profile(State, Head)),
    ?assertEqual(not_admitted, imboy_quality_state:admission_profile(State, <<"different">>)),
    ?assertEqual(
        not_admitted,
        imboy_quality_state:admission_profile(
            State#{<<"current_gate_status">> := #{<<"G-IMPL">> => <<"NOT_REVALIDATED">>}}, Head
        )
    ),
    ?assertEqual(
        not_admitted,
        imboy_quality_state:admission_profile(
            State#{<<"current_admission">> := maps:remove(<<"status">>, A)}, Head
        )
    ),
    ?assertEqual(
        not_admitted,
        imboy_quality_state:admission_profile(
            State#{<<"current_admission">> := A#{<<"profile">> := <<"UNKNOWN">>}}, Head
        )
    ).

same_head_changed_input_is_rejected_test() ->
    with_input(fun(Repo, Src) ->
        Head = <<"0123456789012345678901234567890123456789">>,
        A = #{
            <<"status">> => <<"ADMITTED">>,
            <<"candidate_sha">> => Head,
            <<"profile">> => <<"PHASE2_INTEGRATED">>,
            <<"input_fingerprint">> => imboy_quality_state:candidate_input_fingerprint(Repo)
        },
        S = #{
            <<"current_gate_status">> => #{<<"G-IMPL">> => <<"ADMITTED">>},
            <<"current_admission">> => A,
            <<"worktree">> => list_to_binary(Repo)
        },
        ?assertEqual(phase2_integrated, imboy_quality_state:admission_profile(S, Head)),
        ok = file:write_file(Src, <<"changed source">>),
        ?assertEqual(not_admitted, imboy_quality_state:admission_profile(S, Head))
    end).

with_input(Fun) ->
    Repo = filename:join("/tmp", "imboy-admission-input-" ++ os:getpid()),
    Src = filename:join([Repo, "src", "fixture.erl"]),
    ok = filelib:ensure_dir(Src),
    ok = file:write_file(Src, <<"original source">>),
    Config = filename:join([Repo, "config", "sys.local.config"]),
    ok = filelib:ensure_dir(Config),
    ok = file:write_file(Config, <<"[].">>),
    try
        Fun(Repo, Src)
    after
        file:delete(Src),
        file:delete(Config),
        file:del_dir(filename:join(Repo, "config")),
        file:del_dir(filename:join(Repo, "src")),
        file:del_dir(Repo)
    end.

missing_selected_config_is_rejected_test() ->
    with_input(fun(Repo, _Src) ->
        Old = os:getenv("EUNIT_CONFIG"),
        try
            true = os:putenv("EUNIT_CONFIG", filename:join(Repo, "absent.config")),
            ?assertException(
                error,
                {badmatch, false},
                imboy_quality_state:candidate_input_fingerprint(Repo)
            )
        after
            case Old of
                false -> os:unsetenv("EUNIT_CONFIG");
                _ -> os:putenv("EUNIT_CONFIG", Old)
            end
        end
    end).

independent_gates_test() ->
    Base = #{
        task_id => <<"IMBOY_ERLANG_QUALITY">>,
        execution_mode => integrated_execution,
        execution_goal => all_phases,
        owner_verified => true,
        heavy_admitted => true
    },
    ?assertMatch(
        {error, {g_impl_not_admitted, eunit_full}}, imboy_quality_cli:permit(Base, eunit_full)
    ),
    ?assertMatch({error, {g_impl_not_admitted, pbt}}, imboy_quality_cli:permit(Base, pbt)),
    P1 = Base#{gimpl_profile => phase1_snapshot},
    ?assertEqual(ok, imboy_quality_cli:permit(P1, eunit_full)),
    ?assertMatch({error, {g_impl_not_admitted, pbt}}, imboy_quality_cli:permit(P1, pbt)),
    P2 = Base#{gimpl_profile => phase2_integrated},
    ?assertEqual(ok, imboy_quality_cli:permit(P2, pbt)),
    ?assertEqual(ok, imboy_quality_cli:permit(P2, replay)),
    ?assertMatch(
        {error, {g_heavy_not_admitted, pbt}},
        imboy_quality_cli:permit(P2#{heavy_admitted => false}, pbt)
    ).

header_and_protocol_inputs_are_bound_test() ->
    with_input(fun(Repo, _Src) ->
        lists:foreach(
            fun(Name) ->
                Path = filename:join([Repo, "src", Name]),
                try
                    ok = file:write_file(Path, <<"original">>),
                    Before = imboy_quality_state:candidate_input_fingerprint(Repo),
                    ok = file:write_file(Path, <<"changed">>),
                    ?assertNotEqual(Before, imboy_quality_state:candidate_input_fingerprint(Repo))
                after
                    file:delete(Path)
                end
            end,
            ["fixture.hrl", "fixture.proto"]
        )
    end).

build_output_does_not_change_source_identity_test() ->
    with_input(fun(Repo, _Src) ->
        Beam = filename:join([Repo, "test", "generated.beam"]),
        Before = imboy_quality_state:candidate_input_fingerprint(Repo),
        try
            ok = filelib:ensure_dir(Beam),
            ok = file:write_file(Beam, <<"generated beam">>),
            ?assertEqual(Before, imboy_quality_state:candidate_input_fingerprint(Repo))
        after
            file:delete(Beam),
            file:del_dir(filename:join(Repo, "test"))
        end
    end).

selected_runtime_config_is_bound_test() ->
    with_input(fun(Repo, _Src) ->
        Config = filename:join(Repo, "custom.config"),
        Old = os:getenv("EUNIT_CONFIG"),
        try
            true = os:putenv("EUNIT_CONFIG", Config),
            ok = file:write_file(Config, <<"original config">>),
            Before = imboy_quality_state:candidate_input_fingerprint(Repo),
            ok = file:write_file(Config, <<"changed config">>),
            ?assertNotEqual(Before, imboy_quality_state:candidate_input_fingerprint(Repo))
        after
            file:delete(Config),
            case Old of
                false -> os:unsetenv("EUNIT_CONFIG");
                _ -> os:putenv("EUNIT_CONFIG", Old)
            end
        end
    end).
