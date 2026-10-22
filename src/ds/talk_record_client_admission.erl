-module(talk_record_client_admission).
-moduledoc "Record-locked cross-job paid-request fence and bounded client queue admission.".
-export([choose_tx/4, claim_tx/3, dedupe_key/3]).

%% Caller holds the live record row lock. Device and HTTP idempotency identities
%% deliberately do not enter this logical input identity.
choose_tx(C, R, P, I) ->
    Rid = maps:get(<<"id">>, R),
    Kind = maps:get(kind, P),
    Jobs = talk_record_client_gate:require(
        talk_record_client_repo:same_input_tx(
            C,
            Rid,
            Kind,
            maps:get(manifest, I),
            maps:get(revision, I),
            maps:get(transcript_version, I)
        )
    ),
    case Jobs of
        [] ->
            capacity(C, Rid),
            new;
        [J] ->
            compatible(J, P),
            {existing, J};
        _ ->
            talk_record_client_gate:abort(client_execution_conflict)
    end.

compatible(J, P) ->
    Pin = talk_record_client_gate:json(maps:get(<<"processor_input">>, J)),
    Expected = #{
        <<"provider">> => maps:get(provider, P),
        <<"model">> => maps:get(model, P),
        <<"policy_version">> => maps:get(policy_version, P),
        <<"config_version">> => maps:get(config_version, P),
        <<"processing_mode">> => maps:get(kind, P)
    },
    Same = maps:with(maps:keys(Expected), Pin) =:= Expected,
    talk_record_client_gate:ensure(
        Same andalso maps:get(<<"state">>, J) =/= <<"cancelled">>,
        client_execution_conflict
    ).

%% A failed/expired job's new attempt occupies a fresh active slot. A queued or
%% retry_wait job was already counted, so claiming it cannot double-count itself.
claim_tx(C, R, #{<<"state">> := <<"failed">>}) -> capacity(C, maps:get(<<"id">>, R));
claim_tx(_, _, _) -> ok.

capacity(C, Rid) ->
    Limit = maps:get(<<"queue_limit">>, talk_record_ds:capabilities(), 2),
    talk_record_client_gate:ensure(is_integer(Limit) andalso Limit >= 0, processing_unavailable),
    #{<<"count">> := Count} = talk_record_client_gate:require(
        talk_record_client_repo:active_count_tx(C, Rid)
    ),
    talk_record_client_gate:ensure(Count < Limit, client_queue_full).

dedupe_key(Rid, Kind, I) ->
    Hash = talk_record_client_contract:hash(#{
        <<"record_id">> => Rid,
        <<"kind">> => Kind,
        <<"manifest">> => maps:get(manifest, I),
        <<"content_revision">> => maps:get(revision, I),
        <<"transcript_version">> => maps:get(transcript_version, I)
    }),
    <<"client-input-v1:", Hash/binary>>.
