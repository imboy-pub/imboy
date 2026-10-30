-module(inttest_marker_db_tests).
-include_lib("eunit/include/eunit.hrl").

schema_mode_contract_test() ->
    ?assertEqual(full, inttest_marker_db:schema_mode(#{})),
    ?assertEqual(full, inttest_marker_db:schema_mode(#{schema_mode => full})),
    ?assertEqual(empty, inttest_marker_db:schema_mode(#{schema_mode => empty})),
    ?assertError(
        {marker_db_invalid_schema_mode, typo},
        inttest_marker_db:schema_mode(#{schema_mode => typo})
    ),
    ?assertError(
        marker_db_empty_schema_cannot_reuse_or_target,
        inttest_marker_db:schema_mode(#{schema_mode => empty, reuse => true})
    ),
    ?assertError(
        marker_db_empty_schema_cannot_reuse_or_target,
        inttest_marker_db:schema_mode(#{schema_mode => empty, migration_target => 1})
    ),
    %% Invalid modes fail before configuration, application startup or PG access.
    ?assertError(
        {marker_db_invalid_schema_mode, typo},
        inttest_marker_db:provision(#{schema_mode => typo})
    ),
    ?assertError(
        marker_db_empty_schema_cannot_reuse_or_target,
        inttest_marker_db:provision(#{schema_mode => empty, reuse => true})
    ).

connection_override_rejected_before_setup_test() ->
    lists:foreach(
        fun(Key) ->
            ?assertError(
                {marker_db_forbidden_connect_override, [Key]},
                inttest_marker_db:provision(#{connect_extra => #{Key => "other"}})
            ),
            ?assertError(
                {marker_db_forbidden_connect_override, [Key]},
                inttest_marker_db:provision(#{reuse => true, connect_extra => #{Key => "other"}})
            )
        end,
        [host, port, username, password, database]
    ),
    ?assertError(
        marker_db_invalid_connect_extra,
        inttest_marker_db:provision(#{connect_extra => []})
    ).

v5_marker_without_bound_identity_rejected_before_setup_test() ->
    lists:foreach(
        fun(Prefix) ->
            ?assertError(
                {contract_conflict, marker_db_identity_and_slot_unverified},
                inttest_marker_db:provision(#{env_prefix => Prefix})
            )
        end,
        [<<"V5_INTTEST">>, <<"V5_INTTEST_W301">>]
    ).

v5_run_rejects_non_v5_prefix_before_setup_test() ->
    Previous = os:getenv("V5_RUN_DIR"),
    try
        true = os:putenv("V5_RUN_DIR", "/isolated/v5/run"),
        ?assertError(
            {contract_conflict, marker_db_identity_and_slot_unverified},
            inttest_marker_db:provision(#{env_prefix => <<"INTTEST">>})
        )
    after
        case Previous of
            false -> os:unsetenv("V5_RUN_DIR");
            Value -> os:putenv("V5_RUN_DIR", Value)
        end
    end.

v5_run_rejects_direct_safe_connect_before_network_test() ->
    Previous = os:getenv("V5_RUN_DIR"),
    try
        true = os:putenv("V5_RUN_DIR", "/isolated/v5/run"),
        ?assertError(
            {contract_conflict, marker_db_identity_and_slot_unverified},
            inttest_marker_db:safe_connect(#{})
        )
    after
        case Previous of
            false -> os:unsetenv("V5_RUN_DIR");
            Value -> os:putenv("V5_RUN_DIR", Value)
        end
    end.

v5_reserved_database_rejected_without_run_environment_test() ->
    Previous = os:getenv("V5_RUN_DIR"),
    try
        os:unsetenv("V5_RUN_DIR"),
        lists:foreach(
            fun(Db) ->
                ?assertError(
                    {contract_conflict, marker_db_identity_and_slot_unverified},
                    inttest_marker_db:safe_connect(#{database => Db})
                )
            end,
            [<<"attv5_slot1_pool">>, "attv5_chain_slot1_w403", <<"inttest_v5_inttest_123">>]
        )
    after
        case Previous of
            false -> os:unsetenv("V5_RUN_DIR");
            Value -> os:putenv("V5_RUN_DIR", Value)
        end
    end.

v5_implicit_database_rejected_before_network_test() ->
    Previous = os:getenv("V5_RUN_DIR"),
    try
        os:unsetenv("V5_RUN_DIR"),
        lists:foreach(
            fun(User) ->
                ?assertError(
                    {contract_conflict, marker_db_identity_and_slot_unverified},
                    inttest_marker_db:safe_connect(#{username => User})
                )
            end,
            [<<"attv5_slot1_pool">>, "attv5_chain_slot1_w403", <<"inttest_v5_inttest_123">>]
        )
    after
        case Previous of
            false -> os:unsetenv("V5_RUN_DIR");
            Value -> os:putenv("V5_RUN_DIR", Value)
        end
    end.

v5_tls_policy_rejects_downgrades_test() ->
    Defaults = #{
        ssl => required,
        ssl_opts => [
            {verify, verify_peer}, {cacerts, [<<1, 2, 3>>]}, {server_name_indication, "localhost"}
        ]
    },
    Secure = inttest_marker_db:v5_tls_extra(Defaults, #{codecs => []}),
    ?assertEqual(required, maps:get(ssl, Secure)),
    ?assertEqual(verify_peer, proplists:get_value(verify, maps:get(ssl_opts, Secure))),
    ?assertEqual([<<1, 2, 3>>], proplists:get_value(cacerts, maps:get(ssl_opts, Secure))),
    lists:foreach(
        fun(Key) ->
            ?assertError(
                {marker_db_forbidden_tls_override, [Key]},
                inttest_marker_db:v5_tls_extra(Defaults, #{Key => false})
            )
        end,
        [ssl, ssl_opts]
    ),
    lists:foreach(
        fun(Bad) ->
            ?assertError(
                {contract_conflict, marker_db_tls_config_unverified},
                inttest_marker_db:v5_tls_extra(Bad, #{})
            )
        end,
        [
            #{},
            Defaults#{ssl => true},
            Defaults#{
                ssl_opts => [
                    {verify, verify_none},
                    {cacerts, [<<1>>]},
                    {server_name_indication, "localhost"}
                ]
            },
            Defaults#{
                ssl_opts => [
                    {verify, verify_peer},
                    {verify, verify_none},
                    {cacerts, [<<1>>]},
                    {server_name_indication, "localhost"}
                ]
            },
            Defaults#{
                ssl_opts => [
                    {verify, verify_peer},
                    {cacerts, []},
                    {server_name_indication, "localhost"}
                ]
            }
        ]
    ).

v5_slot_shaped_and_maintenance_targets_rejected_before_network_test() ->
    PreviousRun = os:getenv("V5_RUN_DIR"),
    PreviousSlot = os:getenv("V5_SLOT"),
    try
        true = os:putenv("V5_RUN_DIR", "/isolated/v5/run"),
        true = os:putenv("V5_SLOT", "slot1"),
        lists:foreach(
            fun(Db) ->
                ?assertError(
                    {contract_conflict, marker_db_identity_and_slot_unverified},
                    inttest_marker_db:safe_connect(#{database => Db, host => "127.0.0.1"})
                )
            end,
            [<<"attv5_slot1_pool">>, <<"postgres">>]
        )
    after
        restore_env("V5_RUN_DIR", PreviousRun),
        restore_env("V5_SLOT", PreviousSlot)
    end.

restore_env(Key, false) -> os:unsetenv(Key);
restore_env(Key, Value) -> os:putenv(Key, Value).

connect_worker_timeout_stops_worker_test() ->
    {Worker, Monitor} = spawn_monitor(fun() ->
        receive
            never -> ok
        end
    end),
    ?assertError(
        {safe_connect_worker_timeout, synthetic},
        inttest_marker_db:await_connect(Worker, Monitor, 10, synthetic)
    ),
    ?assertNot(is_process_alive(Worker)).

connect_worker_crash_is_reported_test() ->
    {Worker, Monitor} = spawn_monitor(fun() -> exit(synthetic_failure) end),
    ?assertEqual(
        {error, {safe_connect_worker_down, synthetic_failure}},
        inttest_marker_db:await_connect(Worker, Monitor, 1000, synthetic)
    ).
