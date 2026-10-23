-module(talk_record_client_audio_authorization_tests).
-include_lib("eunit/include/eunit.hrl").

flag_cannot_replace_missing_durable_authority_test() ->
    with_domain(fun() ->
        ?assertEqual({error, audio_source_denied}, issue()),
        ?assertEqual(1, meck:num_calls(talk_record_client_audio_repo, source_authority_tx, '_')),
        ?assertEqual(0, meck:num_calls(attachment_derived_artifact, with_range, '_'))
    end).

issue_and_read_recheck_current_authority_and_hide_bearer_test() ->
    with_domain(fun() ->
        enable_durable_fixture(),
        {ok, #{url := URL, expires_at := Exp}} = issue(),
        #{query := Q} = uri_string:parse(URL),
        [{<<"ticket">>, Ticket}] = uri_string:dissect_query(Q),
        ?assertEqual(
            maps:get(<<"source_expires_at">>, maps:get(provider_task, get(audio_authority))), Exp
        ),
        {ok, Meta} = talk_record_client_audio_logic:metadata(Ticket),
        ?assertEqual(
            <<"private, no-store">>, maps:get(<<"cache-control">>, maps:get(headers, Meta))
        ),
        ?assertNot(maps:is_key(url, Meta)),
        ?assertNot(maps:is_key(ticket, Meta)),
        ?assertEqual(1, meck:num_calls(talk_record_client_audio_repo, consent_tx, '_')),
        ?assertEqual(2, meck:num_calls(talk_record_client_gate, record_tx, '_')),
        put(audio_device_active, false),
        ?assertEqual(denied(), talk_record_client_audio_logic:metadata(Ticket)),
        put(audio_device_active, true),
        put(audio_epoch, 2),
        ?assertEqual(denied(), talk_record_client_audio_logic:metadata(Ticket))
    end).

source_replacement_deleted_record_and_policy_change_revoke_test() ->
    with_domain(fun() ->
        enable_durable_fixture(),
        {ok, #{url := URL}} = issue(),
        #{query := Q} = uri_string:parse(URL),
        [{_, Ticket}] = uri_string:dissect_query(Q),
        Original = get(audio_authority),
        R = maps:get(record, Original),
        Artifact = maps:get(artifact, Original),
        lists:foreach(
            fun(Changed) ->
                put(audio_authority, Changed),
                ?assertEqual(denied(), talk_record_client_audio_logic:metadata(Ticket))
            end,
            [
                Original#{record => R#{<<"deleted_at">> => <<"synthetic-deleted">>}},
                Original#{record => R#{<<"acl_revision">> => 2}},
                Original#{artifact => Artifact#{<<"file_hash256">> => binary:copy(<<"b">>, 64)}},
                Original#{artifact => Artifact#{<<"reference_valid">> => false}}
            ]
        ),
        put(audio_authority, Original),
        put(audio_enabled, false),
        ?assertEqual(
            {error, audio_source_not_ready}, talk_record_client_audio_logic:metadata(Ticket)
        ),
        ?assertEqual(0, meck:num_calls(attachment_derived_artifact, with_range, '_'))
    end).

issue() ->
    talk_record_client_audio_logic:issue(
        #{uid => 10, did => <<"synthetic-device">>},
        #{rid => 7, jid => 20, run_id => 30, seq => 0},
        #{token => binary:copy(<<"t">>, 44), if_match => <<"client-run:30:1">>}
    ).
denied() -> {error, audio_source_denied}.

with_domain(F) ->
    Modules = [
        talk_record_client_policy,
        config_ds,
        elib_pg,
        talk_record_client_gate,
        talk_record_client_repo,
        talk_record_client_audio_repo,
        user_device_ds,
        auth_session_ds,
        attachment_derived_artifact
    ],
    lists:foreach(
        fun(M) ->
            Opts =
                case M of
                    elib_pg -> [no_link];
                    _ -> [passthrough, no_link]
                end,
            meck:new(M, Opts)
        end,
        Modules
    ),
    try
        A = talk_record_client_audio_contract_tests:authority(),
        Run = (maps:get(run, A))#{
            <<"device_session_id">> => <<"synthetic-device">>, <<"revision">> => 1
        },
        put(audio_authority, A#{run => Run}),
        put(audio_epoch, 1),
        put(audio_device_active, true),
        put(audio_enabled, true),
        setup_domain(),
        F()
    after
        lists:foreach(fun meck:unload/1, Modules),
        lists:foreach(fun erase/1, [
            audio_authority, audio_epoch, audio_device_active, audio_enabled
        ])
    end.

setup_domain() ->
    meck:expect(talk_record_client_policy, get, fun() ->
        C = maps:get(config, get(audio_authority)),
        {ok, C#{
            audio_source_enabled => get(audio_enabled), signing_key => binary:copy(<<"x">>, 32)
        }}
    end),
    meck:expect(config_ds, env, fun(base_url, _) -> <<"https://example.com">> end),
    meck:expect(elib_pg, with_tx, fun(F, _) ->
        try
            F(conn)
        catch
            throw:E -> {rollback, E}
        end
    end),
    meck:expect(user_device_ds, is_active, fun(10, <<"synthetic-device">>) ->
        get(audio_device_active)
    end),
    meck:expect(auth_session_ds, current_epoch, fun(10) -> {ok, get(audio_epoch)} end),
    meck:expect(talk_record_client_gate, record_tx, fun(
        conn, #{uid := 10, did := <<"synthetic-device">>}, 7, write
    ) ->
        maps:get(record, get(audio_authority))
    end),
    meck:expect(talk_record_client_gate, pins_tx, fun(_, _, _) ->
        maps:get(config, get(audio_authority))
    end),
    meck:expect(talk_record_client_gate, run_tx, fun(_, _, _, _, _) -> ok end),
    setup_rows().

setup_rows() ->
    meck:expect(talk_record_client_repo, lock_job_tx, fun(conn, 7, 20) ->
        {ok, maps:get(job, get(audio_authority))}
    end),
    meck:expect(talk_record_client_repo, lock_run_tx, fun(conn, 7, 20, 30) ->
        {ok, maps:get(run, get(audio_authority))}
    end),
    meck:expect(talk_record_client_repo, parts_tx, fun(conn, 30) ->
        {ok, maps:get(parts, get(audio_authority))}
    end),
    meck:expect(talk_record_client_audio_repo, assembly_tx, fun(conn, 7) ->
        {ok, maps:get(assembly, get(audio_authority))}
    end),
    meck:expect(talk_record_client_audio_repo, artifact_tx, fun(conn, 7, 80) ->
        {ok, maps:get(artifact, get(audio_authority))}
    end),
    meck:expect(talk_record_client_audio_repo, run_identity_tx, fun(conn, 7, 20, 30) ->
        {ok, #{<<"actor_user_id">> => 10, <<"device_session_id">> => <<"synthetic-device">>}}
    end),
    meck:expect(talk_record_client_audio_repo, consent_tx, fun(conn, 10, _, <<"transcribe">>) ->
        {ok, #{<<"id">> => 31}}
    end).

enable_durable_fixture() ->
    %% Synthetic durable row for isolated domain fences; real PG/HTTP is a separate acceptance gate.
    meck:expect(talk_record_client_audio_repo, source_authority_tx, fun(
        conn, #{rid := 7, jid := 20, run_id := 30, seq := 0}
    ) ->
        {ok, maps:get(provider_task, get(audio_authority))}
    end).
