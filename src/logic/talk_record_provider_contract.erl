-module(talk_record_provider_contract).
-moduledoc "Pure asynchronous supplier protocol, immutable identifiers and paid-intent fences.".
-export([
    profile/0,
    plan/1,
    validate/2,
    transition/4,
    view/1,
    etag/1,
    can_resubmit/1,
    can_resume_preparation/1
]).

profile() ->
    #{
        <<"audio">> => #{
            <<"format">> => <<"aac">>,
            <<"rate">> => 16000,
            <<"bits">> => 16,
            <<"channel">> => 1
        },
        <<"request">> => #{
            <<"model_name">> => <<"bigmodel">>,
            <<"enable_speaker_info">> => true,
            <<"show_utterances">> => true,
            <<"ssd_version">> => <<"300">>,
            <<"enable_itn">> => true,
            <<"enable_punc">> => true,
            <<"enable_ddc">> => false
        }
    }.

plan(#{<<"assembly">> := A, <<"parts">> := Parts}) ->
    #{
        <<"overlap_rule">> => <<"none">>,
        <<"chunks">> => [
            #{
                <<"seq">> => 0,
                <<"source_part_manifest_hash">> => maps:get(<<"manifest_hash">>, A),
                <<"source_ranges">> => [
                    #{
                        <<"part_id">> => maps:get(<<"part_id">>, P),
                        <<"sha256">> => maps:get(<<"sha256">>, P),
                        <<"start_ms">> => 0,
                        <<"end_ms">> => maps:get(<<"duration_ms">>, P)
                    }
                 || P <- Parts
                ],
                <<"offset_ms">> => 0,
                <<"duration_ms">> => maps:get(<<"duration_ms">>, A),
                <<"byte_size">> => maps:get(<<"byte_size">>, A)
            }
        ]
    }.

validate(Action, B) ->
    try
        true = is_map(B),
        true = byte_size(jsone:encode(B)) =< 2097152,
        ok = body(Action, B),
        ok
    catch
        _:_ -> {error, client_schema_invalid}
    end.
body(A, B) when A =:= get; A =:= prepare; A =:= query_intent; A =:= complete; A =:= cancel ->
    true = B =:= #{},
    ok;
body(submit_intent, B) ->
    keys(B, [<<"profile_hash">>]),
    hash(maps:get(<<"profile_hash">>, B)),
    ok;
body(A, B) when A =:= submission_receipt; A =:= reconcile_receipt ->
    keys(B, [<<"client_request_id">>, <<"provider_task_id">>, <<"state">>, <<"status_code">>]),
    uuid(maps:get(<<"client_request_id">>, B)),
    State = maps:get(<<"state">>, B),
    true = lists:member(State, [<<"accepted">>, <<"unknown">>, <<"rejected">>]),
    case maps:get(<<"provider_task_id">>, B, null) of
        null -> true = State =/= <<"accepted">>;
        Id -> task_id(Id)
    end,
    case maps:find(<<"status_code">>, B) of
        error ->
            ok;
        {ok, Code} ->
            true = lists:member(
                Code,
                [
                    <<"20000000">>,
                    <<"20000001">>,
                    <<"20000002">>,
                    <<"45000001">>,
                    <<"45000002">>,
                    <<"55000000">>
                ]
            )
    end,
    case A of
        reconcile_receipt -> true = State =:= <<"accepted">>;
        _ -> ok
    end,
    ok;
body(query_receipt, B) ->
    keys(B, [<<"provider_task_id">>, <<"state">>, <<"result">>, <<"result_hash">>]),
    task_id(maps:get(<<"provider_task_id">>, B)),
    State = maps:get(<<"state">>, B),
    true = lists:member(State, [<<"processing">>, <<"unknown">>, <<"result_ready">>]),
    case State of
        <<"result_ready">> ->
            true = is_map(maps:get(<<"result">>, B)),
            hash(maps:get(<<"result_hash">>, B));
        _ ->
            true = not maps:is_key(<<"result">>, B),
            true = not maps:is_key(<<"result_hash">>, B)
    end,
    ok;
body(query_execution, B) ->
    keys(B, [<<"processing_consent">>]),
    true = is_map(maps:get(<<"processing_consent">>, B)),
    ok;
body(resubmission, B) ->
    keys(B, [<<"processing_consent">>, <<"ack_possible_duplicate_charge">>]),
    true = is_boolean(maps:get(<<"ack_possible_duplicate_charge">>, B)),
    true = is_map(maps:get(<<"processing_consent">>, B)),
    ok.
keys(B, Keys) -> true = talk_record_client_contract:allowed(B, Keys).
hash(H) ->
    true = is_binary(H),
    true = re:run(H, <<"^[0-9a-f]{64}$">>, [{capture, none}]) =:= match.
uuid(U) ->
    true = is_binary(U),
    true =
        re:run(U, <<"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>, [
            {capture, none}
        ]) =:= match.
task_id(I) ->
    true = is_binary(I),
    true = byte_size(I) > 0 andalso byte_size(I) =< 128,
    true = re:run(I, <<"^[A-Za-z0-9_.:-]+$">>, [{capture, none}]) =:= match.

transition(Action, T, B, Now) ->
    try
        ok = validate(Action, B),
        true = maps:get(<<"cancelled_at">>, T, null) =:= null,
        advance(Action, T, B, Now)
    catch
        _:_ -> {error, client_intent_conflict}
    end.
advance(submit_intent, #{<<"state">> := <<"prepared">>} = T, B, Now) ->
    true = maps:get(<<"profile_hash">>, T) =:= maps:get(<<"profile_hash">>, B),
    {ok,
        #{
            <<"state">> => <<"submit_outcome_unknown">>,
            <<"source_issued_at">> => Now,
            <<"source_expires_at">> => Now + 300
        },
        true};
advance(submit_intent, T, B, _) ->
    true = maps:get(<<"profile_hash">>, T) =:= maps:get(<<"profile_hash">>, B),
    {ok, #{}, false};
advance(A, T, B, _) when A =:= submission_receipt; A =:= reconcile_receipt ->
    true = maps:get(<<"intent_id">>, T, null) =/= null,
    true = maps:get(<<"client_request_id">>, T) =:= maps:get(<<"client_request_id">>, B),
    Old = maps:get(<<"provider_task_id">>, T, null),
    New = maps:get(<<"provider_task_id">>, B, null),
    true = Old =:= null orelse Old =:= New,
    State = maps:get(<<"state">>, B),
    true = lists:member(maps:get(<<"state">>, T), [<<"submit_outcome_unknown">>, <<"accepted">>]),
    Next =
        case State of
            <<"unknown">> -> <<"submit_outcome_unknown">>;
            X -> X
        end,
    {ok, #{<<"provider_task_id">> => New, <<"state">> => Next}, false};
advance(query_intent, T, _, _) ->
    true = maps:get(<<"provider_task_id">>, T, null) =/= null,
    true = lists:member(maps:get(<<"state">>, T), [
        <<"accepted">>, <<"processing">>, <<"submit_outcome_unknown">>
    ]),
    {ok, #{}, false};
advance(query_receipt, T, B, _) ->
    true = maps:get(<<"provider_task_id">>, T) =:= maps:get(<<"provider_task_id">>, B),
    true = lists:member(maps:get(<<"state">>, T), [
        <<"accepted">>, <<"processing">>, <<"submit_outcome_unknown">>
    ]),
    State =
        case maps:get(<<"state">>, B) of
            <<"unknown">> -> <<"submit_outcome_unknown">>;
            X -> X
        end,
    {ok, (maps:with([<<"result">>, <<"result_hash">>], B))#{<<"state">> => State}, false};
advance(cancel, _, _, _) ->
    {ok, #{}, false}.

etag(T) ->
    <<"provider-task:", (integer_to_binary(maps:get(<<"id">>, T)))/binary, ":",
        (integer_to_binary(maps:get(<<"revision">>, T)))/binary>>.
view(T) ->
    Keys = [
        <<"id">>,
        <<"job_id">>,
        <<"source_run_id">>,
        <<"attempt">>,
        <<"seq">>,
        <<"client_request_id">>,
        <<"provider_task_id">>,
        <<"provider">>,
        <<"model">>,
        <<"resource_id">>,
        <<"model_name">>,
        <<"profile_hash">>,
        <<"input_manifest_hash">>,
        <<"input_content_revision">>,
        <<"input_transcript_version">>,
        <<"state">>,
        <<"revision">>
    ],
    (maps:with(Keys, T))#{
        <<"request_profile">> => json(maps:get(<<"request_profile">>, T)),
        <<"assembly">> => json(maps:get(<<"assembly">>, T)),
        <<"etag">> => etag(T),
        <<"source_authorization">> => #{
            <<"id">> => maps:get(<<"source_authorization_id">>, T),
            <<"issued_at">> => maps:get(<<"source_issued_at">>, T, null),
            <<"expires_at">> => maps:get(<<"source_expires_at">>, T, null)
        },
        <<"allowed_actions">> => actions(T)
    }.
actions(#{<<"cancelled_at">> := C}) when C =/= null -> [];
actions(#{<<"state">> := <<"published">>}) ->
    [];
actions(#{<<"state">> := <<"result_ready">>}) ->
    [<<"query_execution">>, <<"complete">>, <<"cancel">>];
actions(#{<<"state">> := <<"prepared">>} = T) ->
    case can_resume_preparation(T) of
        true -> [<<"submit_intent">>, <<"resume_preparation">>, <<"cancel">>];
        false -> [<<"cancel">>]
    end;
actions(#{<<"state">> := State}) when State =:= <<"rejected">> ->
    [<<"resubmission">>, <<"cancel">>];
actions(#{<<"state">> := <<"failed">>}) ->
    [<<"cancel">>];
actions(#{<<"provider_task_id">> := null}) ->
    [<<"reconcile_receipt">>, <<"resubmission">>, <<"cancel">>];
actions(#{<<"state">> := <<"submit_outcome_unknown">>}) ->
    [<<"query_execution">>, <<"cancel">>];
actions(_) ->
    [<<"query_execution">>, <<"cancel">>].
json(B) when is_binary(B) -> jsone:decode(B);
json(V) -> V.

can_resubmit(#{<<"state">> := <<"rejected">>}) ->
    true;
can_resubmit(#{<<"state">> := <<"submit_outcome_unknown">>, <<"provider_task_id">> := null}) ->
    true;
can_resubmit(_) ->
    false.

can_resume_preparation(#{<<"state">> := <<"prepared">>} = T) ->
    lists:all(fun(K) -> maps:get(K, T, missing) =:= null end, [
        <<"intent_id">>,
        <<"request_hash">>,
        <<"provider_task_id">>,
        <<"source_issued_at">>,
        <<"source_expires_at">>,
        <<"result">>,
        <<"result_hash">>,
        <<"outcome_unknown_at">>,
        <<"cancelled_at">>
    ]);
can_resume_preparation(_) ->
    false.
