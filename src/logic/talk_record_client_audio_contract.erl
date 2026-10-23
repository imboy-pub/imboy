-module(talk_record_client_audio_contract).
-moduledoc "Pure assembly-only external-audio authority; inactive until the async protocol is pinned.".
-export([
    authorize/2,
    binding/2,
    validate_binding/1,
    range/2,
    max_bytes/0,
    enabled/1,
    origin/1,
    build_url/2
]).
-define(MAX_BYTES, 134217728).

max_bytes() -> ?MAX_BYTES.

authorize(A, Now) ->
    checked(fun() ->
        #{
            record := R,
            job := J,
            run := Run,
            parts := [P],
            assembly := Assembly,
            artifact := Artifact,
            config := Config,
            auth_epoch := Epoch,
            provider_task := Task
        } = A,
        Kind = maps:get(<<"kind">>, J),
        <<"transcribe">> = Kind,
        <<"client_byok">> = maps:get(<<"execution_mode">>, J),
        true = maps:get(<<"processing_mode">>, R) =:= Kind,
        true = maps:get(<<"deleted_at">>, R) =:= null,
        <<"active">> = maps:get(<<"custody_status">>, R),
        <<"available">> = maps:get(<<"media_status">>, R),
        true = maps:get(<<"state">>, J) =/= <<"cancelled">>,
        task(Task, R, J, Run, Artifact, Now),
        true = maps:get(<<"intent_id">>, Task) =:= maps:get(<<"dispatch_intent_id">>, P),
        true = maps:get(<<"request_hash">>, Task) =:= maps:get(<<"request_hash">>, P),
        Pin = talk_record_client_gate:json(maps:get(<<"processor_input">>, J)),
        provider(Config, Pin, Run),
        input(R, J, Run, Assembly),
        artifact(Artifact, Assembly),
        plan(P, Assembly, Artifact),
        true = maps:get(<<"run_id">>, P) =:= maps:get(<<"id">>, Run),
        true = is_integer(Epoch) andalso Epoch > 0,
        B = binding(A, Pin),
        true = validate_binding(B),
        B
    end).

provider(#{public := Public} = Config, Pin, Run) ->
    true = maps:get(audio_source_enabled, Config, false) =:= true,
    #{<<"transcribe">> := Provider} = maps:get(<<"providers">>, Public),
    Expected = #{
        <<"provider">> => <<"doubao">>,
        <<"resource_id">> => <<"volc.seedasr.auc">>,
        <<"protocol">> => <<"doubao_auc_v1">>,
        <<"source_delivery">> => <<"assembly_url_v1">>,
        <<"model_name">> => <<"bigmodel">>,
        <<"enable_speaker_info">> => true,
        <<"show_utterances">> => true,
        <<"ssd_version">> => <<"300">>,
        <<"enable_itn">> => true,
        <<"enable_punc">> => true,
        <<"enable_ddc">> => false
    },
    true = maps:with(maps:keys(Expected), Provider) =:= Expected,
    true = maps:with(maps:keys(Expected), Pin) =:= Expected,
    true = maps:get(<<"processing_mode">>, Pin) =:= <<"transcribe">>,
    lists:foreach(
        fun(K) -> true = maps:get(K, Pin) =:= maps:get(K, Run) end,
        [<<"provider">>, <<"model">>, <<"policy_version">>, <<"config_version">>]
    ),
    true = maps:get(<<"model">>, Provider) =:= maps:get(<<"model">>, Run),
    true = maps:get(<<"policy_version">>, Public) =:= maps:get(<<"policy_version">>, Run),
    true = maps:get(<<"config_version">>, Public) =:= maps:get(<<"config_version">>, Run).

input(R, J, Run, A) ->
    Id = maps:get(<<"id">>, R),
    Id = maps:get(<<"record_id">>, J),
    Id = maps:get(<<"record_id">>, Run),
    Jid = maps:get(<<"id">>, J),
    Jid = maps:get(<<"job_id">>, Run),
    Revision = maps:get(<<"content_revision">>, R),
    Revision = maps:get(<<"input_content_revision">>, J),
    Revision = maps:get(<<"input_content_revision">>, Run),
    Hash = maps:get(<<"manifest_hash">>, A),
    Hash = maps:get(<<"input_manifest_hash">>, J),
    Hash = maps:get(<<"input_manifest_hash">>, Run),
    true =
        maps:get(<<"input_transcript_version">>, J) =:=
            maps:get(<<"input_transcript_version">>, Run),
    Duration = maps:get(<<"total_duration_ms">>, A),
    true = is_integer(Duration) andalso Duration > 0 andalso Duration =< 7200000.

artifact(Artifact, A) ->
    Id = maps:get(<<"audio_attachment_id">>, A),
    Id = maps:get(<<"id">>, Artifact),
    <<"live">> = maps:get(<<"lifecycle_state">>, Artifact),
    <<"complete">> = maps:get(<<"write_state">>, Artifact),
    <<"bound">> = maps:get(<<"ref_state">>, Artifact),
    <<"transcoded_audio">> = maps:get(<<"ref_role">>, Artifact),
    <<"audio/aac">> = maps:get(<<"mime_type">>, Artifact),
    null = maps:get(<<"cipher">>, Artifact),
    true = maps:get(<<"reference_valid">>, Artifact),
    false = maps:get(<<"governance_purged">>, Artifact),
    N = maps:get(<<"size">>, Artifact),
    true = is_integer(N) andalso N > 0 andalso N < ?MAX_BYTES.

plan(P, A, Artifact) ->
    0 = maps:get(<<"seq">>, P),
    0 = maps:get(<<"offset_ms">>, P),
    true = lists:member(maps:get(<<"provider_state">>, P), [
        <<"dispatching">>, <<"provider_outcome_unknown">>
    ]),
    true = maps:get(<<"duration_ms">>, P) =:= maps:get(<<"total_duration_ms">>, A),
    true = maps:get(<<"byte_size">>, P) =:= maps:get(<<"size">>, Artifact),
    true = maps:get(<<"source_part_manifest_hash">>, P) =:= maps:get(<<"manifest_hash">>, A),
    Parts = talk_record_client_gate:json(maps:get(<<"segments">>, A)),
    true = is_list(Parts) andalso Parts =/= [] andalso length(Parts) =< 2000,
    Expected = [
        #{
            <<"part_id">> => maps:get(<<"part_id">>, S),
            <<"sha256">> => maps:get(<<"sha256">>, S),
            <<"start_ms">> => 0,
            <<"end_ms">> => maps:get(<<"duration_ms">>, S)
        }
     || S <- Parts
    ],
    true = talk_record_client_gate:json(maps:get(<<"source_ranges">>, P)) =:= Expected.

binding(
    #{
        record := R,
        job := J,
        run := Run,
        parts := [P],
        assembly := A,
        artifact := Artifact,
        auth_epoch := Epoch,
        config := Config,
        provider_task := Task
    },
    Pin
) ->
    Base = maps:with(
        [
            <<"provider">>,
            <<"model">>,
            <<"resource_id">>,
            <<"protocol">>,
            <<"source_delivery">>,
            <<"policy_version">>,
            <<"config_version">>,
            <<"model_name">>,
            <<"enable_speaker_info">>,
            <<"show_utterances">>,
            <<"ssd_version">>,
            <<"enable_itn">>,
            <<"enable_punc">>,
            <<"enable_ddc">>
        ],
        Pin
    ),
    Base#{
        <<"profile_hash">> => maps:get(<<"profile_hash">>, Task),
        <<"provider_task_ref">> => maps:get(<<"id">>, Task),
        <<"source_authorization_id">> => maps:get(<<"source_authorization_id">>, Task),
        <<"source_issued_at">> => maps:get(<<"source_issued_at">>, Task),
        <<"source_expires_at">> => maps:get(<<"source_expires_at">>, Task),
        <<"context_hash">> => context_hash(R),
        <<"transcript_version">> => maps:get(<<"input_transcript_version">>, Run),
        <<"deployment_id">> => maps:get(deployment_id, Config),
        <<"uid">> => maps:get(<<"actor_user_id">>, Run),
        <<"rid">> => maps:get(<<"id">>, R),
        <<"jid">> => maps:get(<<"id">>, J),
        <<"run_id">> => maps:get(<<"id">>, Run),
        <<"attempt">> => maps:get(<<"attempt">>, Run),
        <<"seq">> => maps:get(<<"seq">>, P),
        <<"consent_event_id">> => maps:get(<<"consent_event_id">>, Run),
        <<"intent_id">> => maps:get(<<"dispatch_intent_id">>, P),
        <<"request_hash">> => maps:get(<<"request_hash">>, P),
        <<"manifest_hash">> => maps:get(<<"manifest_hash">>, A),
        <<"content_revision">> => maps:get(<<"content_revision">>, R),
        <<"chunk_plan_hash">> => maps:get(<<"chunk_plan_hash">>, Run),
        <<"attachment_id">> => maps:get(<<"id">>, Artifact),
        <<"attachment_hash">> => maps:get(<<"file_hash256">>, Artifact),
        <<"size">> => maps:get(<<"size">>, Artifact),
        <<"acl_revision">> => maps:get(<<"acl_revision">>, R),
        <<"auth_epoch">> => Epoch
    }.

validate_binding(B) ->
    try
        Ids = [
            <<"uid">>,
            <<"rid">>,
            <<"jid">>,
            <<"run_id">>,
            <<"attempt">>,
            <<"consent_event_id">>,
            <<"intent_id">>,
            <<"content_revision">>,
            <<"attachment_id">>,
            <<"acl_revision">>,
            <<"auth_epoch">>,
            <<"provider_task_ref">>,
            <<"source_authorization_id">>,
            <<"source_issued_at">>,
            <<"source_expires_at">>
        ],
        true = lists:all(
            fun(K) ->
                N = maps:get(K, B),
                is_integer(N) andalso N > 0 andalso N =< 9223372036854775807
            end,
            Ids
        ),
        Hashes = [
            <<"request_hash">>,
            <<"manifest_hash">>,
            <<"chunk_plan_hash">>,
            <<"attachment_hash">>,
            <<"config_version">>,
            <<"deployment_id">>,
            <<"context_hash">>,
            <<"profile_hash">>
        ],
        true = lists:all(fun(K) -> hash(maps:get(K, B)) end, Hashes),
        <<"doubao">> = maps:get(<<"provider">>, B),
        <<"volc.seedasr.auc">> = maps:get(<<"resource_id">>, B),
        <<"doubao_auc_v1">> = maps:get(<<"protocol">>, B),
        <<"assembly_url_v1">> = maps:get(<<"source_delivery">>, B),
        <<"bigmodel">> = maps:get(<<"model_name">>, B),
        true = maps:get(<<"enable_speaker_info">>, B),
        true = maps:get(<<"enable_itn">>, B),
        true = maps:get(<<"enable_punc">>, B),
        false = maps:get(<<"enable_ddc">>, B),
        true = maps:get(<<"show_utterances">>, B),
        <<"300">> = maps:get(<<"ssd_version">>, B),
        true = text(maps:get(<<"model">>, B), 64),
        true = text(maps:get(<<"policy_version">>, B), 64),
        TV = maps:get(<<"transcript_version">>, B),
        true = TV =:= null orelse (is_integer(TV) andalso TV > 0),
        Issued = maps:get(<<"source_issued_at">>, B),
        Expires = maps:get(<<"source_expires_at">>, B),
        true = Expires > Issued andalso Expires =< Issued + 300,
        0 = maps:get(<<"seq">>, B),
        N = maps:get(<<"size">>, B),
        true = is_integer(N) andalso N > 0 andalso N < ?MAX_BYTES,
        Keys =
            Ids ++ Hashes ++
                [
                    <<"provider">>,
                    <<"resource_id">>,
                    <<"protocol">>,
                    <<"source_delivery">>,
                    <<"model">>,
                    <<"policy_version">>,
                    <<"seq">>,
                    <<"size">>,
                    <<"model_name">>,
                    <<"enable_speaker_info">>,
                    <<"show_utterances">>,
                    <<"ssd_version">>,
                    <<"enable_itn">>,
                    <<"enable_punc">>,
                    <<"enable_ddc">>,
                    <<"transcript_version">>
                ],
        true = maps:without(Keys, B) =:= #{},
        true
    catch
        _:_ -> false
    end.

range({Offset, Length}, Size) when
    is_integer(Offset),
    is_integer(Length),
    is_integer(Size),
    Offset >= 0,
    Length > 0,
    Size > 0,
    Size < ?MAX_BYTES,
    Offset + Length =< Size
->
    {ok, {Offset, Length}};
range(_, _) ->
    {error, audio_source_range_invalid}.

hash(B) when is_binary(B), byte_size(B) =:= 64 ->
    re:run(B, <<"^[0-9a-f]{64}$">>, [{capture, none}]) =:= match;
hash(_) ->
    false.
text(B, N) -> is_binary(B) andalso byte_size(B) > 0 andalso byte_size(B) =< N.
checked(F) ->
    try
        {ok, F()}
    catch
        _:_ -> {error, audio_source_denied}
    end.

enabled(Config) ->
    try
        true = maps:get(audio_source_enabled, Config, false),
        #{
            <<"provider">> := <<"doubao">>,
            <<"resource_id">> := <<"volc.seedasr.auc">>,
            <<"protocol">> := <<"doubao_auc_v1">>,
            <<"source_delivery">> := <<"assembly_url_v1">>,
            <<"model_name">> := <<"bigmodel">>,
            <<"enable_speaker_info">> := true,
            <<"show_utterances">> := true,
            <<"ssd_version">> := <<"300">>
        } = maps:get(
            <<"transcribe">>,
            maps:get(<<"providers">>, maps:get(public, Config))
        ),
        true
    catch
        _:_ -> false
    end.

origin(Url) when is_binary(Url), byte_size(Url) =< 2048 ->
    checked(fun() ->
        #{scheme := <<"https">>, host := Host} = U = uri_string:parse(Url),
        true = is_binary(Host) andalso byte_size(Host) > 0,
        true =
            not maps:is_key(userinfo, U) andalso not maps:is_key(query, U) andalso
                not maps:is_key(fragment, U),
        true = lists:member(maps:get(path, U, <<>>), [<<>>, <<"/">>]),
        Port = maps:get(port, U, 443),
        true = is_integer(Port) andalso Port > 0 andalso Port =< 65535,
        talk_record_client_contract:hash(#{
            <<"scheme">> => <<"https">>, <<"host">> => string:lowercase(Host), <<"port">> => Port
        })
    end);
origin(_) ->
    {error, audio_source_denied}.

%% A provider task outlives a device lease. These must be durable source-authority
%% facts from the future task repository, never request JSON or a runtime flag.
task(T, R, J, Run, Artifact, Now) ->
    Pairs = [
        {<<"record_id">>, maps:get(<<"id">>, R)},
        {<<"job_id">>, maps:get(<<"id">>, J)},
        {<<"run_id">>, maps:get(<<"id">>, Run)},
        {<<"attempt">>, maps:get(<<"attempt">>, Run)},
        {<<"seq">>, 0},
        {<<"actor_user_id">>, maps:get(<<"actor_user_id">>, Run)},
        {<<"attachment_id">>, maps:get(<<"id">>, Artifact)},
        {<<"attachment_hash">>, maps:get(<<"file_hash256">>, Artifact)},
        {<<"manifest_hash">>, maps:get(<<"input_manifest_hash">>, Run)},
        {<<"content_revision">>, maps:get(<<"input_content_revision">>, Run)},
        {<<"consent_event_id">>, maps:get(<<"consent_event_id">>, Run)},
        {<<"policy_version">>, maps:get(<<"policy_version">>, Run)},
        {<<"config_version">>, maps:get(<<"config_version">>, Run)},
        {<<"model">>, maps:get(<<"model">>, Run)},
        {<<"transcript_version">>, maps:get(<<"input_transcript_version">>, Run)},
        {<<"provider">>, <<"doubao">>},
        {<<"resource_id">>, <<"volc.seedasr.auc">>},
        {<<"model_name">>, <<"bigmodel">>},
        {<<"enable_speaker_info">>, true},
        {<<"show_utterances">>, true},
        {<<"ssd_version">>, <<"300">>}
    ],
    lists:foreach(fun({K, V}) -> V = maps:get(K, T) end, Pairs),
    true =
        maps:get(<<"profile_hash">>, T) =:=
            talk_record_client_contract:hash(talk_record_provider_contract:profile()),
    true = lists:member(maps:get(<<"state">>, T), [
        <<"prepared">>, <<"submit_outcome_unknown">>, <<"accepted">>, <<"processing">>
    ]),
    Issued = maps:get(<<"source_issued_at">>, T),
    Expires = maps:get(<<"source_expires_at">>, T),
    true =
        is_integer(Issued) andalso is_integer(Expires) andalso Issued =< Now andalso Now < Expires,
    true = Expires > Issued andalso Expires =< Issued + 300.

context_hash(R) ->
    talk_record_client_contract:hash(
        maps:with(
            [<<"scope">>, <<"organization_id">>, <<"workspace_id">>, <<"owner_user_id">>], R
        )
    ).

build_url(Origin, Ticket) when
    is_binary(Ticket), byte_size(Ticket) > 0, byte_size(Ticket) =< 8192
->
    checked(fun() ->
        {ok, _} = origin(Origin),
        true = re:run(Ticket, <<"^[A-Za-z0-9_.-]+$">>, [{capture, none}]) =:= match,
        U = uri_string:parse(Origin),
        Canonical = U#{
            host => string:lowercase(maps:get(host, U)),
            path => <<"/api/v1/talk-records/client-audio-source">>,
            query => uri_string:compose_query([{<<"ticket">>, Ticket}])
        },
        URL =
            case maps:get(port, Canonical, 443) of
                443 -> maps:remove(port, Canonical);
                _ -> Canonical
            end,
        uri_string:recompose(URL)
    end);
build_url(_, _) ->
    {error, audio_source_denied}.
