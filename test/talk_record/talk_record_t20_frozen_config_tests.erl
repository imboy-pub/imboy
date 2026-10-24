-module(talk_record_t20_frozen_config_tests).
-include_lib("eunit/include/eunit.hrl").

config() ->
    #{
        <<"approved">> => true,
        <<"purpose">> => <<"t20_case_execution">>,
        <<"valid_from_unix">> => 1000,
        <<"expires_at_unix">> => 2000,
        <<"max_lease_seconds">> => 1000,
        <<"controller_epoch">> => 7,
        <<"controller_pid">> => 123,
        <<"host">> => <<"127.0.0.1">>,
        <<"connection_port">> => 64839,
        <<"server_port">> => 5432,
        <<"migration_head">> => 223,
        <<"migration_count">> => 3,
        <<"migration_versions">> => [1, 221, 223],
        <<"owner">> => <<"offline-test">>,
        <<"controller_process_start">> => <<"start">>,
        <<"controller_thread_id">> => <<"thread">>,
        <<"controller_host">> => <<"host">>,
        <<"system_identifier">> => <<"physical-instance">>,
        <<"user">> => <<"test">>,
        <<"candidate_commit">> => <<"candidate">>,
        <<"source_sha256">> => #{},
        <<"case_authorizations">> => #{},
        <<"repo">> => <<"/offline/repo">>,
        <<"runtime_root">> => <<"/offline/runtime">>,
        <<"registry_path">> => <<"/offline/registry">>,
        <<"lease_path">> => <<"/offline/lease">>,
        <<"controller_lock_path">> => <<"/offline/lock">>,
        <<"controller_state_path">> => <<"/offline/state">>
    }.

approved_config_test() ->
    ?assertEqual(ok, talk_record_t20_frozen_config:verify(config(), 1100)).

invalid_config_test_() ->
    [
        ?_assertException(error, _, talk_record_t20_frozen_config:verify((config())#{K => V}, 1100))
     || {K, V} <- [
            {<<"approved">>, false},
            {<<"purpose">>, <<"other">>},
            {<<"valid_from_unix">>, 1101},
            {<<"expires_at_unix">>, 1219},
            {<<"max_lease_seconds">>, 14401},
            {<<"max_lease_seconds">>, 999},
            {<<"controller_epoch">>, 0},
            {<<"controller_pid">>, 0},
            {<<"host">>, <<"remote">>},
            {<<"connection_port">>, 0},
            {<<"migration_versions">>, [1, 223, 221]},
            {<<"migration_versions">>, [1, 221, 221]},
            {<<"migration_count">>, 2},
            {<<"migration_head">>, 221},
            {<<"controller_thread_id">>, <<>>},
            {<<"repo">>, <<"relative">>}
        ]
    ].

binding_rejects_each_divergent_identity_test_() ->
    C = config(),
    [
        ?_assertException(
            error,
            _,
            talk_record_t20_frozen_config:same(
                C, C#{K => invalid}, talk_record_t20_frozen_config:binding_keys()
            )
        )
     || K <- talk_record_t20_frozen_config:binding_keys()
    ].

missing_binding_field_test() ->
    ?assertException(
        error,
        _,
        talk_record_t20_frozen_config:same(
            config(),
            maps:remove(<<"controller_thread_id">>, config()),
            talk_record_t20_frozen_config:binding_keys()
        )
    ).

lease_window_test_() ->
    C = config(),
    Good = #{<<"issued_at_unix">> => 1000, <<"expires_at_unix">> => 1500},
    [?_assertEqual(ok, talk_record_t20_frozen_config:lease_window(Good, C, 1100))] ++
        [
            ?_assertException(error, _, talk_record_t20_frozen_config:lease_window(Bad, C, 1100))
         || Bad <- [
                Good#{<<"issued_at_unix">> => 999},
                Good#{<<"issued_at_unix">> => 1101},
                Good#{<<"expires_at_unix">> => 1219},
                Good#{<<"expires_at_unix">> => 2001},
                maps:remove(<<"issued_at_unix">>, Good)
            ]
        ].

controller_identity_test_() ->
    C = config(),
    Owner = #{
        <<"controller_epoch">> => 7,
        <<"pid">> => 123,
        <<"process_start">> => <<"start">>,
        <<"session">> => <<"offline-test">>,
        <<"thread_id">> => <<"thread">>,
        <<"host">> => <<"host">>,
        <<"status">> => <<"RUNNING">>
    },
    Controller = maps:without([<<"controller_epoch">>, <<"status">>], Owner),
    State = #{
        <<"controller_epoch">> => 7,
        <<"status">> => <<"RUNNING">>,
        <<"controller">> => Controller#{<<"lock_path">> => <<"/offline/lock">>}
    },
    [?_assertEqual(ok, talk_record_t20_frozen_config:controller_identity(C, Owner, State))] ++
        [
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:controller_identity(
                    C, Owner#{K => V}, State
                )
            )
         || {K, V} <- [
                {<<"controller_epoch">>, 3},
                {<<"pid">>, 124},
                {<<"process_start">>, <<"old">>},
                {<<"thread_id">>, <<"foreign">>},
                {<<"session">>, <<"old">>},
                {<<"status">>, <<"PAUSED">>},
                {<<"host">>, <<"foreign">>}
            ]
        ] ++
        [
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:controller_identity(
                    C, Owner, State#{<<"controller_epoch">> => 3}
                )
            ),
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:controller_identity(
                    C, Owner, State#{<<"status">> => <<"PAUSED">>}
                )
            ),
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:controller_identity(
                    C, Owner, State#{
                        <<"controller">> => Controller#{<<"lock_path">> => <<"/foreign">>}
                    }
                )
            )
        ].

v3_controller_identity_test_() ->
    C = (config())#{
        <<"controller_schema">> => <<"imboy_delivery_v3">>,
        <<"controller_run_id">> => <<"owned-run">>
    },
    Lock = #{<<"type">> => <<"regular">>, <<"path">> => <<"/offline/lock">>},
    Controller = #{<<"epoch">> => 7, <<"pid">> => 123, <<"started">> => <<"start">>},
    State = #{
        <<"run_id">> => <<"owned-run">>,
        <<"status">> => <<"ACTIVE">>,
        <<"controller">> => Controller
    },
    [
        ?_assertEqual(ok, talk_record_t20_frozen_config:verify(C, 1100)),
        ?_assertEqual(ok, talk_record_t20_frozen_config:controller_identity(C, Lock, State))
    ] ++
        [
            ?_assertException(
                error, _, talk_record_t20_frozen_config:controller_identity(C, Lock, Bad)
            )
         || Bad <- [
                State#{<<"status">> => <<"USER_PAUSED">>},
                State#{<<"status">> => <<"RUNNING">>},
                State#{<<"run_id">> => <<"foreign">>},
                State#{<<"controller">> => Controller#{<<"epoch">> => 8}},
                State#{<<"controller">> => Controller#{<<"pid">> => 124}},
                State#{<<"controller">> => Controller#{<<"started">> => <<"old">>}},
                State#{<<"controller">> => Controller#{<<"released">> => true}},
                State#{<<"controller">> => maps:remove(<<"started">>, Controller)},
                maps:remove(<<"run_id">>, State)
            ]
        ] ++
        [
            ?_assertException(
                error, _, talk_record_t20_frozen_config:controller_identity(C, Bad, State)
            )
         || Bad <- [
                #{},
                Lock#{<<"type">> => <<"directory">>},
                Lock#{<<"path">> => <<"/foreign/lock">>}
            ]
        ] ++
        [
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:controller_identity(
                    config(), Lock, State
                )
            ),
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:verify(
                    C#{<<"controller_schema">> => <<"unknown">>}, 1100
                )
            ),
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:verify(
                    maps:remove(<<"controller_run_id">>, C), 1100
                )
            ),
            ?_assertException(
                error,
                _,
                talk_record_t20_frozen_config:verify(
                    C#{<<"controller_run_id">> => <<>>}, 1100
                )
            )
        ].

v3_lease_stays_required_test() ->
    C = (config())#{
        <<"controller_schema">> => <<"imboy_delivery_v3">>,
        <<"controller_run_id">> => <<"owned-run">>
    },
    ?assertException(
        error,
        _,
        talk_record_t20_frozen_config:lease_window(
            #{<<"issued_at_unix">> => 1000, <<"expires_at_unix">> => 1099}, C, 1100
        )
    ),
    ?assertException(error, _, talk_record_t20_frozen_config:lease_window(#{}, C, 1100)).
