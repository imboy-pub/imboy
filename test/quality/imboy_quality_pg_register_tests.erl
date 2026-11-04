-module(imboy_quality_pg_register_tests).
-include_lib("eunit/include/eunit.hrl").

fixture() ->
    Root = <<"/synthetic/isolated">>,
    Hash = binary:copy(<<"a">>, 64),
    PG = #{
        <<"kind">> => <<"ISOLATED_POSTGRESQL">>,
        <<"status">> => <<"PREPARED">>,
        <<"host">> => <<"127.0.0.1">>,
        <<"port">> => 21458,
        <<"data_dir">> => <<Root/binary, "/pgdata">>,
        <<"executable">> => <<Root/binary, "/postgres-toolchain/bin/postgres">>,
        <<"executable_sha256">> => Hash,
        <<"system_identifier">> => <<"71234567890">>
    },
    Volume = #{<<"kind">> => <<"ISOLATED_APFS_VOLUME">>, <<"path">> => Root},
    Id = #{<<"major_device">> => 1, <<"minor_device">> => 2, <<"inode">> => 3},
    Meta = maps:merge(
        maps:without([<<"kind">>, <<"status">>], PG),
        #{
            <<"executable_identity">> => Id,
            <<"data_dir_identity">> => Id,
            <<"log_path">> => <<Root/binary, "/postgres-owner-abc.log">>,
            <<"owner_nonce_sha256">> => Hash
        }
    ),
    {#{<<"owned_resources">> => [Volume, PG], <<"history">> => unchanged}, Meta}.

register_and_stop_test() ->
    {S, M} = fixture(),
    {ok, Running} = imboy_quality_pg_register:transition(register, S, {1234, M}),
    ?assertEqual(unchanged, maps:get(<<"history">>, Running)),
    [_, PG] = maps:get(<<"owned_resources">>, Running),
    ?assertEqual(<<"RUNNING">>, maps:get(<<"status">>, PG)),
    ?assertEqual(1234, maps:get(<<"pid">>, PG)),
    ?assertEqual({ok, Running}, imboy_quality_pg_register:transition(register, Running, {1234, M})),
    ?assertMatch(
        {error, _},
        imboy_quality_pg_register:transition(
            record_exit,
            Running,
            {1235, #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => true}}
        )
    ),
    {ok, Stopped} = imboy_quality_pg_register:transition(
        record_exit,
        Running,
        {1234, #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => true}}
    ),
    [_, Final] = maps:get(<<"owned_resources">>, Stopped),
    ?assertEqual(<<"STOPPED">>, maps:get(<<"status">>, Final)),
    ?assertEqual(
        {ok, Stopped},
        imboy_quality_pg_register:transition(
            record_exit,
            Stopped,
            {1234, #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => true}}
        )
    ),
    {ok, BadLog} = imboy_quality_pg_register:transition(
        record_exit,
        Running,
        {1234, #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => false}}
    ),
    [_, FailedEvidence] = maps:get(<<"owned_resources">>, BadLog),
    ?assertEqual(<<"STOPPED_WITH_EVIDENCE_ERROR">>, maps:get(<<"status">>, FailedEvidence)).

nonzero_exit_and_log_error_are_not_clean_test() ->
    {S, M} = fixture(),
    {ok, Running} = imboy_quality_pg_register:transition(register, S, {1234, M}),
    lists:foreach(
        fun(Exit = #{postgres_exit := P, pg_ctl_exit := C, log_ok := LogOk}) ->
            {ok, Updated} = imboy_quality_pg_register:transition(
                record_exit, Running, {1234, Exit}
            ),
            [_, PG] = maps:get(<<"owned_resources">>, Updated),
            ?assertEqual(<<"STOPPED_WITH_EVIDENCE_ERROR">>, maps:get(<<"status">>, PG)),
            ?assertEqual(P, maps:get(<<"postgres_exit">>, PG)),
            ?assertEqual(C, maps:get(<<"pg_ctl_exit">>, PG)),
            ?assertEqual(LogOk, maps:get(<<"log_ok">>, PG))
        end,
        [
            #{postgres_exit => 3, pg_ctl_exit => 0, log_ok => true},
            #{postgres_exit => 0, pg_ctl_exit => 7, log_ok => true},
            #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => false}
        ]
    ).

persistence_failure_does_not_consume_receipt_test() ->
    put(receipt_commits, 0),
    Commit = fun() ->
        put(receipt_commits, get(receipt_commits) + 1),
        ok
    end,
    Good = fun() -> ok end,
    BadBackup = fun() -> error(injected_backup_failure) end,
    BadWrite = fun() -> error(injected_write_failure) end,
    ?assertMatch(
        {error, _}, imboy_quality_pg_register:persist_then_commit(BadBackup, Good, Commit)
    ),
    ?assertEqual(0, get(receipt_commits)),
    ?assertMatch({error, _}, imboy_quality_pg_register:persist_then_commit(Good, BadWrite, Commit)),
    ?assertEqual(0, get(receipt_commits)),
    ?assertEqual(ok, imboy_quality_pg_register:persist_then_commit(Good, Good, Commit)),
    ?assertEqual(1, get(receipt_commits)),
    erase(receipt_commits).

invalid_registration_is_rejected_test() ->
    {S, M} = fixture(),
    [Volume, PG] = maps:get(<<"owned_resources">>, S),
    BadStates = [
        #{},
        S#{<<"owned_resources">> := [Volume]},
        S#{<<"owned_resources">> := [Volume, PG, PG]}
    ],
    lists:foreach(
        fun(Bad) ->
            ?assertMatch({error, _}, imboy_quality_pg_register:transition(register, Bad, {1234, M}))
        end,
        BadStates
    ),
    Id = maps:get(<<"data_dir_identity">>, M),
    BadMeta = [
        maps:remove(<<"host">>, M),
        M#{<<"port">> := 21459},
        M#{<<"data_dir">> := <<"/other/pgdata">>},
        M#{<<"host">> := <<"remote">>},
        M#{<<"executable_sha256">> := <<"short">>},
        M#{<<"system_identifier">> := <<"1x">>},
        M#{<<"data_dir_identity">> := Id#{<<"inode">> := 0}},
        M#{<<"log_path">> := <<"/synthetic/isolated/postgres-owner-../../bad">>},
        M#{<<"password">> => <<"not allowed">>}
    ],
    lists:foreach(
        fun(Bad) ->
            ?assertMatch({error, _}, imboy_quality_pg_register:transition(register, S, {1234, Bad}))
        end,
        BadMeta
    ),
    ?assertMatch({error, _}, imboy_quality_pg_register:transition(register, S, {0, M})).

natural_exit_is_preserved_as_failure_test() ->
    {S, M} = fixture(),
    {ok, Running} = imboy_quality_pg_register:transition(register, S, {1234, M}),
    {ok, Updated} = imboy_quality_pg_register:transition(
        record_exit,
        Running,
        {1234, #{postgres_exit => 0, pg_ctl_exit => <<"NOT_INVOKED">>, log_ok => true}}
    ),
    [_, PG] = maps:get(<<"owned_resources">>, Updated),
    ?assertEqual(<<"STOPPED_WITH_EVIDENCE_ERROR">>, maps:get(<<"status">>, PG)),
    ?assertEqual(<<"NOT_INVOKED">>, maps:get(<<"pg_ctl_exit">>, PG)).

port_receipt_rejects_forgery_and_live_exit_test() ->
    Exec = os:find_executable("erl"),
    Port = open_port({spawn_executable, Exec}, [
        binary,
        exit_status,
        stderr_to_stdout,
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
            "-eval",
            "receive after 800 -> halt(0) end."
        ]}
    ]),
    {os_pid, Pid} = erlang:port_info(Port, os_pid),
    Nonce = string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(16))),
    Hash = string:lowercase(binary:encode_hex(crypto:hash(sha256, Nonce))),
    PG = #{
        <<"executable">> => list_to_binary(Exec),
        <<"pid">> => Pid,
        <<"owner_nonce_sha256">> => Hash
    },
    Meta = #{<<"owner_nonce_sha256">> => Hash},
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        ?assertEqual(
            ok,
            imboy_quality_pg_register:verify_port_receipt(
                register,
                Pid,
                Meta,
                PG,
                {self(), Port},
                Nonce
            )
        ),
        ?assertMatch(
            {error, _},
            imboy_quality_pg_register:verify_port_receipt(
                register,
                Pid,
                Meta,
                PG,
                {Other, Port},
                Nonce
            )
        ),
        ?assertMatch(
            {error, _},
            imboy_quality_pg_register:verify_port_receipt(
                record_exit,
                Pid,
                #{},
                PG,
                {self(), Port},
                Nonce
            )
        ),
        receive
            {Port, {data, _}} -> ok
        after 0 -> ok
        end,
        receive
            {Port, {exit_status, 0}} -> ok
        after 4000 -> error(otp_child_timeout)
        end,
        ?assertEqual(
            ok,
            imboy_quality_pg_register:verify_port_receipt(
                record_exit,
                Pid,
                #{},
                PG,
                {self(), Port},
                Nonce
            )
        ),
        ?assertMatch(
            {error, _},
            imboy_quality_pg_register:verify_port_receipt(
                record_exit,
                Pid,
                #{},
                PG,
                {self(), Port},
                <<"00000000000000000000000000000000">>
            )
        )
    after
        Other ! stop,
        case erlang:port_info(Port) of
            undefined -> ok;
            _ -> port_close(Port)
        end
    end.

invalid_exit_is_rejected_test() ->
    {S, M} = fixture(),
    {ok, R} = imboy_quality_pg_register:transition(register, S, {1234, M}),
    lists:foreach(
        fun(Exit) ->
            ?assertMatch(
                {error, _}, imboy_quality_pg_register:transition(record_exit, R, {1234, Exit})
            )
        end,
        [
            #{postgres_exit => 0},
            #{postgres_exit => 0, pg_ctl_exit => 0},
            #{postgres_exit => -1, pg_ctl_exit => 0, log_ok => true},
            #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => true, unexpected => true},
            #{postgres_exit => undefined, pg_ctl_exit => 0, log_ok => true},
            #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => unknown}
        ]
    ).
