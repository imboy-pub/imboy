-module(talk_record_client_claim).
-moduledoc "Exclusive authenticated-device claims and explicit unknown-charge successors.".
-export([create_tx/6, insert_unprepared_tx/7]).

create_tx(C, Context, R, J, Headers, Body) ->
    talk_record_client_gate:ensure(
        talk_record_client_contract:allowed(Body, [
            <<"processing_consent">>, <<"previous_run_id">>, <<"ack_possible_duplicate_charge">>
        ]),
        client_schema_invalid
    ),
    talk_record_client_gate:ensure(
        lists:member(maps:get(<<"state">>, J), [<<"queued">>, <<"retry_wait">>, <<"failed">>]),
        client_execution_conflict
    ),
    first_async_claim(C, J),
    Previous = previous(C, J, Headers, Body),
    talk_record_client_admission:claim_tx(C, R, J),
    Config = talk_record_client_gate:require(talk_record_client_policy:get()),
    Pin = talk_record_client_gate:json(maps:get(<<"processor_input">>, J)),
    Kind = maps:get(<<"kind">>, J),
    Request = maps:with(
        [<<"provider">>, <<"model">>, <<"policy_version">>, <<"config_version">>], Pin
    ),
    Parsed = talk_record_client_gate:require(
        talk_record_client_contract:request(
            Request#{
                <<"mode">> => Kind,
                <<"execution_mode">> => <<"client_byok">>,
                <<"processing_consent">> => maps:get(<<"processing_consent">>, Body)
            },
            maps:get(public, Config)
        )
    ),
    Sources = talk_record_client_gate:require(
        sources(C, maps:get(<<"id">>, R), Kind, Pin)
    ),
    true = byte_size(jsone:encode(Sources)) =< 1048576,
    insert(C, Context, R, J, Config, Parsed, Sources, Previous).

previous(C, J, Headers, Body) ->
    case talk_record_client_repo:latest_tx(C, maps:get(<<"id">>, J)) of
        {error, not_found} ->
            safe_successor(Body),
            null;
        {ok, #{<<"state">> := <<"provider_outcome_unknown">>} = Old} ->
            talk_record_client_gate:ensure(
                maps:get(<<"ack_possible_duplicate_charge">>, Body, false) =:= true,
                client_execution_conflict
            ),
            talk_record_client_gate:ensure(
                maps:get(<<"previous_run_id">>, Body, undefined) =:= maps:get(<<"id">>, Old),
                client_execution_conflict
            ),
            talk_record_client_ds:revision(Old, maps:get(if_match, Headers, undefined)),
            maps:get(<<"id">>, Old);
        {ok, #{<<"state">> := State}} when State =:= <<"expired">>; State =:= <<"failed">> ->
            safe_successor(Body),
            null;
        {ok, _} ->
            talk_record_client_gate:abort(client_execution_conflict);
        {error, Reason} ->
            talk_record_client_gate:abort(Reason)
    end.

insert(C, #{uid := Uid, did := Did}, R, J, Config, P, Sources, Previous) ->
    Id = elib_tsid:generate(attachment),
    Attempt = maps:get(<<"attempt">>, J) + 1,
    Token = talk_record_client_policy:token(maps:get(signing_key, Config), Id, Attempt, Uid, Did),
    {ok, Consent} = talk_record_client_repo:consent_tx(
        C,
        maps:get(<<"id">>, R),
        Uid,
        maps:get(<<"kind">>, J),
        {maps:get(policy_version, P), maps:get(config_version, P)}
    ),
    Data = #{
        <<"id">> => Id,
        <<"record_id">> => maps:get(<<"id">>, R),
        <<"job_id">> => maps:get(<<"id">>, J),
        <<"attempt">> => Attempt,
        <<"actor_user_id">> => Uid,
        <<"device_session_id">> => Did,
        <<"lease_token_hash">> => binary:encode_hex(crypto:hash(sha256, Token), lowercase),
        <<"lease_until">> => elib_dt:add(elib_dt:now(), {300, second}),
        <<"provider">> => maps:get(provider, P),
        <<"model">> => maps:get(model, P),
        <<"policy_version">> => maps:get(policy_version, P),
        <<"config_version">> => maps:get(config_version, P),
        <<"consent_event_id">> => Consent,
        <<"input_manifest_hash">> => maps:get(<<"input_manifest_hash">>, J),
        <<"input_content_revision">> => maps:get(<<"input_content_revision">>, J),
        <<"input_transcript_version">> => maps:get(<<"input_transcript_version">>, J),
        <<"input_snapshot">> => jsone:encode(Sources),
        <<"replaces_run_id">> => Previous,
        <<"ack_possible_duplicate_charge">> => Previous =/= null
    },
    {ok, 1} = talk_record_client_repo:claim_job_tx(C, maps:get(<<"id">>, J), Attempt),
    {ok, 1} = talk_record_client_repo:insert_run_tx(C, Data),
    Run = talk_record_client_gate:require(
        talk_record_client_repo:lock_run_tx(C, maps:get(<<"id">>, R), maps:get(<<"id">>, J), Id)
    ),
    {ok, created, talk_record_client_ds:view(Run, [])}.

safe_successor(Body) ->
    talk_record_client_gate:ensure(
        maps:get(<<"ack_possible_duplicate_charge">>, Body, false) =:= false andalso
            not maps:is_key(<<"previous_run_id">>, Body),
        client_schema_invalid
    ).

sources(C, Rid, <<"transcribe">>, #{<<"provider">> := <<"doubao">>}) ->
    talk_record_provider_repo:snapshot_tx(C, Rid);
sources(C, Rid, Kind, _) ->
    talk_record_client_repo:sources_tx(C, Rid, Kind).

first_async_claim(C, J) ->
    Pin = talk_record_client_gate:json(maps:get(<<"processor_input">>, J, #{})),
    case maps:get(<<"provider">>, Pin, undefined) of
        <<"doubao">> ->
            case talk_record_client_repo:latest_tx(C, maps:get(<<"id">>, J)) of
                {error, not_found} -> ok;
                {ok, _} -> talk_record_client_gate:abort(async_protocol_required);
                {error, R} -> talk_record_client_gate:abort(R)
            end;
        _ ->
            ok
    end.

%% Only the dedicated recovery transaction may call this after whole-job no-grant proof.
insert_unprepared_tx(C, Ctx, R, J, Config, Parsed, Source) ->
    insert(C, Ctx, R, J, Config, Parsed, Source, null).
