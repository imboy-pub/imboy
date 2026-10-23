-module(talk_record_provider_repo_tests).
-include_lib("eunit/include/eunit.hrl").
source_fact_projection_from_durable_task_test() ->
    meck:new(talk_record_provider_repo, [passthrough, no_link]),
    try
        Profile = talk_record_provider_contract:profile(),
        Base = #{
            <<"id">> => 50,
            <<"cancelled_at">> => null,
            <<"source_run_id">> => 30,
            <<"request_profile">> => jsone:encode(Profile),
            <<"assembly">> => jsone:encode(#{<<"id">> => 80, <<"sha256">> => <<"synthetic">>}),
            <<"input_manifest_hash">> => <<"manifest">>,
            <<"input_content_revision">> => 3,
            <<"input_transcript_version">> => null
        },
        put(task_row, Base),
        meck:expect(talk_record_provider_repo, origin_tx, fun(conn, 7, 20, 30) ->
            {ok, get(task_row)}
        end),
        Ids = #{rid => 7, jid => 20, run_id => 30, seq => 0},
        {ok, Projected} = talk_record_client_audio_repo:source_authority_tx(conn, Ids),
        ?assertEqual(30, maps:get(<<"run_id">>, Projected)),
        ?assertEqual(80, maps:get(<<"attachment_id">>, Projected)),
        ?assertEqual(<<"300">>, maps:get(<<"ssd_version">>, Projected)),
        ?assertEqual(false, maps:get(<<"enable_ddc">>, Projected)),
        put(task_row, Base#{<<"cancelled_at">> => 1}),
        ?assertEqual(
            {error, audio_source_denied},
            talk_record_client_audio_repo:source_authority_tx(conn, Ids)
        )
    after
        meck:unload(talk_record_provider_repo),
        erase(task_row)
    end.
update_rejects_secret_or_identity_fields_before_sql_test() ->
    lists:foreach(
        fun(K) ->
            ?assertError(
                {badmatch, false}, talk_record_provider_repo:update_tx(no_db, 1, #{K => <<"x">>})
            )
        end,
        [
            <<"api_key">>,
            <<"source_url">>,
            <<"client_request_id">>,
            <<"assembly">>,
            <<"profile_hash">>
        ]
    ).
