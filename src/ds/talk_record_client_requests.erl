-module(talk_record_client_requests).
-moduledoc "Create a device-owned job without local-model availability checks or worker kicks.".
-export([create/4, create_tx/5]).

create(Context, Rid, Headers, Body) ->
    case talk_record_client_policy:get() of
        {ok, Config} ->
            case talk_record_client_contract:request(Body, maps:get(public, Config)) of
                {ok, Parsed} ->
                    talk_record_client_ds:tx(fun(C) ->
                        create_tx(C, Context, Rid, Headers, {Body, Parsed})
                    end);
                E ->
                    E
            end;
        E ->
            E
    end.

create_tx(C, Context, Rid, Headers, {Body, _Parsed}) ->
    R = talk_record_client_gate:record_tx(C, Context, Rid, write),
    Config = talk_record_client_gate:require(talk_record_client_policy:get()),
    Parsed = talk_record_client_gate:require(
        talk_record_client_contract:request(Body, maps:get(public, Config))
    ),
    case talk_record_client_policy:allowed(R, Config) of
        true -> ok;
        false -> talk_record_client_gate:abort(forbidden)
    end,
    Resource = <<"/api/v1/talk-records/", (integer_to_binary(Rid))/binary, "/processing-requests">>,
    talk_record_client_ds:idempotent(C, Context, Resource, Headers, Body, fun() ->
        Expected = talk_record_client_gate:require(
            talk_record_logic:parse_if_match(maps:get(if_match, Headers, undefined))
        ),
        case Expected =:= maps:get(<<"revision">>, R) of
            true -> ok;
            false -> talk_record_client_gate:abort(precondition_failed)
        end,
        ready(C, Context, R, Parsed)
    end).

ready(C, Context, R, P) ->
    case maps:get(<<"media_status">>, R) of
        <<"available">> -> ok;
        _ -> talk_record_client_gate:abort(media_not_ready)
    end,
    Rid = maps:get(<<"id">>, R),
    Kind = maps:get(kind, P),
    A = talk_record_client_gate:require(talk_record_assembly_logic:find_tx(C, Rid)),
    Input = #{
        manifest => maps:get(<<"manifest_hash">>, A),
        revision => maps:get(<<"content_revision">>, R),
        transcript_version => transcript_version(C, Rid, Kind)
    },
    Jid =
        case talk_record_client_admission:choose_tx(C, R, P, Input) of
            {existing, Job} -> maps:get(<<"id">>, Job);
            new -> enqueue(C, Context, R, P, Input)
        end,
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE talk_record SET processing_mode=$2,revision=revision+1,updated_at=now() WHERE id=$1">>,
        [Rid, Kind]
    ),
    {ok, created, #{
        <<"record_id">> => Rid,
        <<"job_id">> => Jid,
        <<"execution_mode">> => <<"client_byok">>,
        <<"revision">> => maps:get(<<"revision">>, R) + 1
    }}.

enqueue(C, #{uid := Uid}, R, P, Input) ->
    Rid = maps:get(<<"id">>, R),
    Kind = maps:get(kind, P),
    {ok, ConsentId} = talk_record_client_repo:consent_tx(
        C,
        Rid,
        Uid,
        Kind,
        {maps:get(policy_version, P), maps:get(config_version, P)}
    ),
    ProviderPin = maps:get(
        Kind,
        maps:get(
            <<"providers">>,
            maps:get(
                public,
                talk_record_client_gate:require(talk_record_client_policy:get())
            )
        )
    ),
    Pin = ProviderPin#{
        <<"provider">> => maps:get(provider, P),
        <<"model">> => maps:get(model, P),
        <<"processing_mode">> => Kind,
        <<"policy_version">> => maps:get(policy_version, P),
        <<"config_version">> => maps:get(config_version, P),
        <<"consent_event_id">> => ConsentId,
        <<"summary_snapshot">> => talk_record_job_support:summary_snapshot_tx(C, Rid)
    },
    Jid = elib_tsid:generate(attachment),
    {ok, created, Jid} = talk_record_job_repo:enqueue_tx(C, #{
        id => Jid,
        record_id => Rid,
        kind => Kind,
        execution_mode => <<"client_byok">>,
        input_content_revision => maps:get(revision, Input),
        input_manifest_hash => maps:get(manifest, Input),
        input_transcript_version => maps:get(transcript_version, Input),
        processor_input => Pin,
        dedupe_key => talk_record_client_admission:dedupe_key(Rid, Kind, Input)
    }),
    Jid.

transcript_version(C, Rid, <<"summarize">>) ->
    case talk_record_transcript_repo:find_current_tx(C, Rid, <<"t.version,t.status">>) of
        {ok, #{<<"version">> := V, <<"status">> := <<"ready">>}} -> V;
        _ -> talk_record_client_gate:abort(media_not_ready)
    end;
transcript_version(_, _, _) ->
    null.
