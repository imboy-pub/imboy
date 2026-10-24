-module(talk_record_owned_eunit_fixture_tests).
-include_lib("eunit/include/eunit.hrl").

mapping_test() ->
    Expected = [
        talk_record_assembly_relay_tests,
        talk_record_capture_tests,
        talk_record_content_tests,
        talk_record_departure_tests,
        talk_record_ds_pg_tests,
        talk_record_ds_split_tests,
        talk_record_export_relay_tests,
        talk_record_export_tests,
        talk_record_gateway_http_tests,
        talk_record_governance_relay_tests,
        talk_record_grant_tests,
        talk_record_handler,
        talk_record_marks_tests,
        talk_record_participant_tests,
        talk_record_processor_relay_tests,
        talk_record_results_relay_tests,
        talk_record_t07_handler_tests,
        talk_record_t12_tests,
        talk_record_t15_handler_tests,
        talk_record_t17_handler_tests,
        talk_record_t19_handler_tests,
        talk_record_t20_handler_tests,
        talk_record_attachment_lifecycle_tests,
        talk_record_parent_fixture_tests,
        attachment_reference_guard_relay_tests,
        talk_record_client_audio_http_pg_tests
    ],
    ?assertEqual(Expected, [M || {M, _} <- talk_record_owned_eunit_fixture:budgets()]),
    ?assertEqual(unrelated_tests, talk_record_owned_eunit_fixture:entry(unrelated_tests)),
    lists:foreach(
        fun({M, N}) ->
            ?assert(N >= 900),
            ?assertMatch({timeout, N, _}, talk_record_owned_eunit_fixture:entry(M))
        end,
        talk_record_owned_eunit_fixture:budgets()
    ).

summary_test() ->
    Good = [{pass, 23}, {fail, 0}, {skip, 0}, {cancel, 0}],
    ?assertEqual(ok, talk_record_owned_eunit_fixture:assert_summary(ok, Good)),
    lists:foreach(
        fun({Key, Value}) ->
            Bad = lists:keystore(Key, 1, Good, {Key, Value}),
            ?assertError(
                {owned_talk_incomplete, ok, Bad},
                talk_record_owned_eunit_fixture:assert_summary(ok, Bad)
            )
        end,
        [{pass, 0}, {pass, -1}, {fail, 1}, {skip, 1}, {cancel, 1}]
    ),
    ?assertError(
        {owned_talk_incomplete, error, Good},
        talk_record_owned_eunit_fixture:assert_summary(error, Good)
    ),
    Missing = lists:keydelete(cancel, 1, Good),
    ?assertError(
        {owned_talk_incomplete, ok, Missing},
        talk_record_owned_eunit_fixture:assert_summary(ok, Missing)
    ).

missing_env_test() ->
    with_env("TALK_OWNED_GUARD_TEST_MISSING", false, fun() ->
        ?assertError(
            {owned_talk_missing_env, "TALK_OWNED_GUARD_TEST_MISSING"},
            talk_record_owned_eunit_fixture:required("TALK_OWNED_GUARD_TEST_MISSING")
        )
    end).

empty_marker_test() ->
    with_env("TALK_OWNED_PG_MARKER", "", fun() ->
        ?assertError(owned_talk_empty_marker, talk_record_owned_eunit_fixture:marker())
    end).

invalid_roots_test() ->
    %% Existing filesystem roots only: no directories or database are created.
    lists:foreach(
        fun({Run, Scratch}) ->
            ?assertException(error, _, talk_record_owned_eunit_fixture:validate_roots(Run, Scratch))
        end,
        [{"/", "/"}, {"/", "/private"}, {"relative", "/"}, {"/private/../private", "/"}]
    ).

with_env(Name, Value, Fun) ->
    Previous = os:getenv(Name),
    set_env(Name, Value),
    try
        Fun()
    after
        set_env(Name, Previous)
    end.
set_env(Name, false) -> os:unsetenv(Name);
set_env(Name, Value) -> os:putenv(Name, Value).

fixture_binding_test() ->
    with_env("TALK_OWNED_PG_HOST", "127.0.0.1", fun() ->
        with_env("TALK_OWNED_PG_PORT", "56624", fun() ->
            with_env("TALK_OWNED_PG_USER", "synthetic", fun() ->
                with_env("TALK_OWNED_PG_PASSWORD", "", fun() ->
                    Env = talk_record_owned_eunit_fixture:fixture_env("inttest_synthetic"),
                    lists:foreach(
                        fun(Prefix) ->
                            ?assertEqual("56624", proplists:get_value(Prefix ++ "PORT", Env)),
                            ?assertEqual("", proplists:get_value(Prefix ++ "PASSWORD", Env))
                        end,
                        ["T05_PG_", "T06_PG_", "T12_PG_"]
                    ),
                    lists:foreach(
                        fun(Key) ->
                            ?assertEqual("inttest_synthetic", proplists:get_value(Key, Env))
                        end,
                        [
                            "T05_PG_DB",
                            "T12_PG_DB",
                            "T06_PG_DATABASE",
                            "TALK_PARENT_FIXTURE_DATABASE"
                        ]
                    )
                end)
            end)
        end)
    end).

registered_handler_mapping_test() ->
    Module = talk_record_t20_handler_tests,
    ?assertEqual(
        {Module, 1380},
        lists:keyfind(
            Module,
            1,
            talk_record_owned_eunit_fixture:budgets()
        )
    ),
    ?assertMatch(
        {timeout, 1380, Fun} when is_function(Fun, 0),
        talk_record_owned_eunit_fixture:entry(Module)
    ).

attachment_relay_mapping_test() ->
    Module = attachment_reference_guard_relay_tests,
    ?assertEqual(
        {Module, 960},
        lists:keyfind(
            Module,
            1,
            talk_record_owned_eunit_fixture:budgets()
        )
    ),
    ?assertMatch(
        {timeout, 960, Fun} when is_function(Fun, 0),
        talk_record_owned_eunit_fixture:entry(Module)
    ).

registered_handler_summary_test() ->
    Module = talk_record_t20_handler_tests,
    Good = [{pass, 4}, {fail, 0}, {skip, 0}, {cancel, 0}],
    ?assertEqual(ok, talk_record_owned_eunit_fixture:assert_summary(Module, ok, Good)),
    lists:foreach(
        fun(Count) ->
            Bad = lists:keystore(pass, 1, Good, {pass, Count}),
            ?assertError(
                {owned_talk_case_count, Module, 4, Bad},
                talk_record_owned_eunit_fixture:assert_summary(Module, ok, Bad)
            )
        end,
        [1, 3, 5]
    ),
    lists:foreach(
        fun({Key, Value}) ->
            Bad = lists:keystore(Key, 1, Good, {Key, Value}),
            ?assertError(
                {owned_talk_incomplete, ok, Bad},
                talk_record_owned_eunit_fixture:assert_summary(Module, ok, Bad)
            )
        end,
        [
            {pass, 0},
            {pass, -1},
            {pass, 4.0},
            {pass, undefined},
            {fail, 1},
            {skip, 1},
            {cancel, 1}
        ]
    ),
    lists:foreach(
        fun(Key) ->
            Bad = lists:keydelete(Key, 1, Good),
            ?assertError(
                {owned_talk_incomplete, ok, Bad},
                talk_record_owned_eunit_fixture:assert_summary(Module, ok, Bad)
            )
        end,
        [pass, fail, skip, cancel]
    ),
    ?assertError(
        {owned_talk_incomplete, error, Good},
        talk_record_owned_eunit_fixture:assert_summary(Module, error, Good)
    ),
    %% Other existing modules retain the positive-count contract.
    ?assertEqual(
        ok,
        talk_record_owned_eunit_fixture:assert_summary(
            talk_record_t19_handler_tests, ok, lists:keystore(pass, 1, Good, {pass, 3})
        )
    ).

%% Resource-bound authorization: no network connection or database is created.
%% epgsql has no passthrough, so every unexpected call (including DDL) fails.
owned_identity_accepts_exact_child_test() ->
    with_owned_guard([], fun(P) -> P end, #{}, fun(Db) ->
        ?assertEqual(
            ok,
            talk_record_owned_eunit_fixture:assert_owned_database(
                guard_connection, Db
            )
        ),
        assert_guard_queries(1)
    end).

owned_identity_rejects_before_query_test_() ->
    Cases = [
        {"missing proof", [], fun(_) -> missing end, identity, undefined},
        {"ordinary database", [], fun(P) -> P end, ordinary, nomatch},
        {"unowned inttest prefix", [], fun(P) -> P end, other_prefix, nomatch},
        {"wrong configured child", [{"T05_PG_DB", "inttest_talk_owned_11_23"}], fun(P) -> P end,
            identity, <<"inttest_talk_owned_11_23">>},
        {"nonloopback host", [{"T05_PG_HOST", "192.0.2.1"}], fun(P) -> P end, identity,
            "192.0.2.1"},
        {"owned host mismatch", [{"TALK_OWNED_PG_HOST", "localhost"}], fun(P) -> P end, identity,
            "localhost"},
        {"host mapped port mismatch", [{"T05_PG_PORT", "56625"}], fun(P) -> P end, identity, 56624},
        {"configured user mismatch", [{"T05_PG_USER", "other_role"}], fun(P) -> P end, identity,
            <<"synthetic_role">>}
    ],
    [
        {Name, fun() ->
            with_owned_guard(Env, Proof, #{}, fun(Db) ->
                Argument = guard_db_argument(Kind, Db),
                ?assertError(
                    {badmatch, Reason},
                    talk_record_owned_eunit_fixture:assert_owned_database(
                        guard_connection, Argument
                    )
                ),
                assert_guard_queries(0)
            end)
        end}
     || {Name, Env, Proof, Kind, Reason} <- Cases
    ].

owned_identity_rejects_wrong_proof_test_() ->
    Cases = [
        {db, <<"inttest_talk_owned_99_99">>},
        {host, "localhost"},
        {port, 56625},
        {user, <<"other_role">>},
        {system_id, <<"999999">>},
        {marker, <<"different-task-marker">>}
    ],
    [
        {"proof mismatch: " ++ atom_to_list(Key), fun() ->
            Mutate = fun(P) -> P#{Key => Value} end,
            with_owned_guard([], Mutate, #{}, fun(Db) ->
                BadProof = Mutate(owned_guard_proof()),
                ?assertError(
                    {badmatch, BadProof},
                    talk_record_owned_eunit_fixture:assert_owned_database(
                        guard_connection, Db
                    )
                ),
                assert_guard_queries(0)
            end)
        end}
     || {Key, Value} <- Cases
    ].

owned_identity_rejects_wrong_actual_resource_test_() ->
    Cases = [
        {db, <<"inttest_talk_owned_99_99">>},
        {system_id, <<"999999">>},
        {user, <<"other_role">>},
        {server_port, 5433},
        {marker, <<"different-task-marker">>},
        {marker, null}
    ],
    [
        {"actual resource mismatch: " ++ atom_to_list(Key), fun() ->
            with_owned_guard([], fun(P) -> P end, #{Key => Value}, fun(Db) ->
                ExpectedIdentity = owned_guard_identity(),
                ?assertError(
                    {badmatch, ExpectedIdentity},
                    talk_record_owned_eunit_fixture:assert_owned_database(
                        guard_connection, Db
                    )
                ),
                assert_guard_queries(1)
            end)
        end}
     || {Key, Value} <- Cases
    ].

guard_db_argument(identity, Db) -> Db;
guard_db_argument(ordinary, _) -> <<"shared_business">>;
guard_db_argument(other_prefix, _) -> <<"inttest_unrelated_11_22">>.

owned_guard_identity() ->
    #{
        db => <<"inttest_talk_owned_11_22">>,
        system_id => <<"123456789">>,
        user => <<"synthetic_role">>,
        server_port => 5432,
        marker => <<"synthetic-owned-task-marker">>
    }.
owned_guard_proof() ->
    (owned_guard_identity())#{host => "127.0.0.1", port => 56624}.

with_owned_guard(Changes, ProofFun, ActualChanges, Fun) ->
    Env = [
        {"T05_PG_DB", "inttest_talk_owned_11_22"},
        {"T05_PG_HOST", "127.0.0.1"},
        {"T05_PG_PORT", "56624"},
        {"T05_PG_USER", "synthetic_role"},
        {"TALK_OWNED_PG_HOST", "127.0.0.1"},
        {"TALK_OWNED_PG_PORT", "56624"},
        {"TALK_OWNED_PG_USER", "synthetic_role"},
        {"TALK_OWNED_PG_SYSTEM_ID", "123456789"},
        {"TALK_OWNED_PG_MARKER", "synthetic-owned-task-marker"}
    ],
    Saved = [{K, os:getenv(K)} || {K, _} <- Env],
    SavedProof = application:get_env(imboy, talk_owned_child_proof),
    ok = meck:new(epgsql, [no_link]),
    try
        lists:foreach(fun({K, V}) -> set_env(K, V) end, Env ++ Changes),
        set_guard_proof(ProofFun(owned_guard_proof())),
        Actual = maps:merge(owned_guard_identity(), ActualChanges),
        ok = meck:expect(epgsql, equery, fun(guard_connection, Sql, []) ->
            ?assertMatch({match, _}, re:run(Sql, "^SELECT ")),
            ?assertEqual(
                nomatch,
                re:run(
                    Sql,
                    "(?i)\\b(CREATE|ALTER|DROP|COMMENT|INSERT|UPDATE|DELETE)\\b"
                )
            ),
            #{
                db := Db,
                system_id := Id,
                user := User,
                server_port := Port,
                marker := Marker
            } = Actual,
            {ok, [], [{Db, Id, User, Port, Marker}]}
        end),
        Fun(maps:get(db, owned_guard_identity()))
    after
        try
            ok = meck:unload(epgsql)
        after
            lists:foreach(fun({K, V}) -> set_env(K, V) end, Saved),
            restore_guard_proof(SavedProof)
        end
    end.

set_guard_proof(missing) ->
    application:unset_env(imboy, talk_owned_child_proof);
set_guard_proof(Proof) ->
    application:set_env(imboy, talk_owned_child_proof, Proof).
restore_guard_proof(undefined) -> set_guard_proof(missing);
restore_guard_proof({ok, Proof}) -> set_guard_proof(Proof).

assert_guard_queries(Expected) ->
    ?assertEqual(Expected, meck:num_calls(epgsql, equery, '_')),
    ?assertEqual(0, meck:num_calls(epgsql, squery, '_')),
    ?assert(meck:validate(epgsql)).

legacy_public_setup_preserves_id_sequence_test() ->
    Db = <<"talk_record_legacy_guard_test">>,
    with_legacy_public_setup(Db, fun() ->
        State = talk_record_t07_support:setup_all(),
        ?assertEqual(#{conn => self()}, State),
        Id = elib_tsid:generate(1),
        ?assertEqual(900000000000000000, Id),
        ?assert(is_integer(Id) andalso Id > 0 andalso Id =< 9223372036854775807),
        ?assertEqual(1, meck:num_calls(epgsql, squery, [self(), legacy_sequence_sql()])),
        ?assertEqual(
            1,
            meck:num_calls(
                epgsql,
                squery,
                [self(), "SELECT nextval('talk_record_test_entity_id')"]
            )
        ),
        ?assertEqual(0, meck:num_calls(talk_record_t05_support, teardown_all, '_')),
        ?assertEqual(ok, talk_record_t07_support:teardown_all(State)),
        ?assertEqual(1, meck:num_calls(talk_record_t05_support, teardown_all, [State])),
        assert_legacy_mocks()
    end).

ordinary_database_public_setup_rejected_test() ->
    with_legacy_public_setup(<<"shared_database_name">>, fun() ->
        ?assertError({badmatch, nomatch}, talk_record_t07_support:setup_all()),
        ?assertEqual(
            1,
            meck:num_calls(
                epgsql,
                squery,
                [self(), "SELECT current_database()"]
            )
        ),
        ?assertEqual(0, meck:num_calls(epgsql, squery, [self(), legacy_sequence_sql()])),
        ?assertEqual(
            1,
            meck:num_calls(
                talk_record_t05_support,
                teardown_all,
                [#{conn => self()}]
            )
        ),
        ?assertEqual(0, meck:num_calls(elib_tsid, generate, '_')),
        assert_legacy_mocks()
    end).

with_legacy_public_setup(Db, Fun) ->
    OwnedNames = [
        hd(string:split(Value, "="))
     || Value <- os:getenv(),
        lists:prefix("TALK_OWNED", Value)
    ],
    Names = lists:usort(["T05_PG_DB" | OwnedNames]),
    Saved = [{Name, os:getenv(Name)} || Name <- Names],
    SavedProof = application:get_env(imboy, talk_owned_child_proof),
    try
        lists:foreach(fun(Name) -> set_env(Name, false) end, OwnedNames),
        set_env("T05_PG_DB", binary_to_list(Db)),
        set_guard_proof(missing),
        with_legacy_mocks(
            [
                talk_record_t05_support,
                talk_record_upload_test_support,
                epgsql,
                elib_tsid
            ],
            fun() ->
                configure_legacy_mocks(Db),
                Fun()
            end
        )
    after
        lists:foreach(fun({Name, Value}) -> set_env(Name, Value) end, Saved),
        restore_guard_proof(SavedProof)
    end.

with_legacy_mocks([], Fun) ->
    Fun();
with_legacy_mocks([Module | Rest], Fun) ->
    ok = meck:new(Module, [no_link]),
    try
        with_legacy_mocks(Rest, Fun)
    after
        ok = meck:unload(Module)
    end.

configure_legacy_mocks(Db) ->
    Owner = self(),
    ok = meck:expect(
        talk_record_t05_support,
        setup_all,
        fun() -> #{conn => Owner} end
    ),
    ok = meck:expect(
        talk_record_t05_support,
        teardown_all,
        fun(#{conn := C}) when C =:= Owner -> ok end
    ),
    ok = meck:expect(talk_record_upload_test_support, configure, fun() -> ok end),
    SequenceSql = legacy_sequence_sql(),
    ok = meck:expect(epgsql, squery, fun
        (C, "SELECT current_database()") when C =:= Owner -> {ok, [], [{Db}]};
        (C, Sql) when C =:= Owner, Sql =:= SequenceSql -> {ok, [], []};
        (C, "SELECT nextval('talk_record_test_entity_id')") when C =:= Owner ->
            {ok, [], [{<<"900000000000000000">>}]}
    end).

legacy_sequence_sql() ->
    "CREATE SEQUENCE IF NOT EXISTS talk_record_test_entity_id"
    " AS bigint START WITH 900000000000000000 NO CYCLE".

assert_legacy_mocks() ->
    lists:foreach(
        fun(Module) -> ?assert(meck:validate(Module)) end,
        [talk_record_t05_support, talk_record_upload_test_support, epgsql, elib_tsid]
    ),
    ?assertEqual(0, meck:num_calls(epgsql, equery, '_')).

client_audio_http_mapping_test() ->
    Module = talk_record_client_audio_http_pg_tests,
    ?assertEqual(
        {Module, 9300},
        lists:keyfind(
            Module,
            1,
            talk_record_owned_eunit_fixture:budgets()
        )
    ),
    ?assertMatch(
        {timeout, 9300, Fun} when is_function(Fun, 0),
        talk_record_owned_eunit_fixture:entry(Module)
    ).
