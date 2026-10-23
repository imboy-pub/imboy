-module(talk_record_client_operations).
-moduledoc "Frozen plan, durable intent, normalized result and exact completion transitions.".
-export([perform/7, reconstructed_plan/2, request_hash/3]).

perform(C, plan, R, J, Run, _Seq, Body) ->
    Snapshot = talk_record_client_gate:json(maps:get(<<"input_snapshot">>, Run)),
    Valid = talk_record_client_gate:require(
        talk_record_client_contract:plan(maps:get(<<"kind">>, J), Body, Snapshot)
    ),
    freeze(C, J, Run, Valid),
    reply(C, R, J, Run);
perform(C, heartbeat, R, J, Run, _, Body) ->
    talk_record_client_gate:ensure(Body =:= #{}, client_schema_invalid),
    {ok, 1} = talk_record_client_repo:heartbeat_tx(C, maps:get(<<"id">>, Run)),
    reply(C, R, J, Run);
perform(C, dispatch, R, J, Run, Seq, Body) ->
    P = part(C, Run, Seq),
    talk_record_client_gate:ensure(
        talk_record_client_contract:allowed(Body, [<<"request_hash">>, <<"chunk_plan_hash">>]),
        client_schema_invalid
    ),
    Hash = request_hash(Run, P, Seq),
    talk_record_client_gate:ensure(
        Hash =:= maps:get(<<"request_hash">>, Body, undefined), client_intent_conflict
    ),
    PlanHash = maps:get(<<"chunk_plan_hash">>, Run),
    talk_record_client_gate:ensure(
        PlanHash =:= maps:get(<<"chunk_plan_hash">>, Body, undefined), client_plan_conflict
    ),
    CanDispatch = dispatch(C, Run, P, Hash),
    {ok, created, View} = reply(C, R, J, Run),
    {ok, Parts} = talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run)),
    [Updated] = [X || X <- Parts, maps:get(<<"seq">>, X) =:= Seq],
    {ok, created, View#{
        <<"may_dispatch">> => CanDispatch,
        <<"dispatch_intent_id">> => maps:get(<<"dispatch_intent_id">>, Updated)
    }};
perform(C, result, R, J, Run, Seq, Body) ->
    P = part(C, Run, Seq),
    Snapshot = talk_record_client_gate:json(maps:get(<<"input_snapshot">>, Run)),
    Valid = talk_record_client_gate:require(
        talk_record_client_contract:result(maps:get(<<"kind">>, J), Body, P, Snapshot)
    ),
    confirm(C, Run, P, Valid),
    reply(C, R, J, Run);
perform(C, complete, R, J, Run, _, Body) ->
    talk_record_client_gate:ensure(Body =:= #{}, client_schema_invalid),
    {ok, Parts} = talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run)),
    exact_completion(J, Run, Parts),
    ok = talk_record_client_publish:publish_tx(C, R, J, Run, Parts),
    {ok, 1} = talk_record_client_repo:complete_tx(C, maps:get(<<"id">>, Run)),
    {ok, 1} = talk_record_client_repo:finish_job_tx(C, maps:get(<<"id">>, J), <<"succeeded">>),
    reply(C, R, J, Run);
perform(C, settle, R, J, Run, _, Body) ->
    talk_record_client_gate:ensure(
        talk_record_client_contract:allowed(Body, [<<"state">>, <<"error_code">>]),
        client_schema_invalid
    ),
    Requested = maps:get(<<"state">>, Body, undefined),
    Code = maps:get(<<"error_code">>, Body, undefined),
    talk_record_client_gate:ensure(
        lists:member(Requested, [<<"failed">>, <<"cancelled">>, <<"provider_outcome_unknown">>]),
        client_schema_invalid
    ),
    talk_record_client_gate:ensure(
        lists:member(Code, [
            <<"key_invalid">>,
            <<"insufficient_balance">>,
            <<"rate_limited">>,
            <<"network_unknown">>,
            <<"cancelled">>,
            <<"response_invalid">>,
            <<"device_stopped">>
        ]),
        client_schema_invalid
    ),
    {ok, Parts} = talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run)),
    Unknown = lists:any(
        fun(P) -> maps:get(<<"provider_state">>, P) =:= <<"dispatching">> end, Parts
    ),
    State =
        case Unknown of
            true -> <<"provider_outcome_unknown">>;
            false -> Requested
        end,
    Error =
        case State of
            <<"provider_outcome_unknown">> -> State;
            _ -> Code
        end,
    {ok, 1} = talk_record_client_repo:settle_tx(C, maps:get(<<"id">>, Run), State, Error),
    reply(C, R, J, Run).

freeze(C, J, #{<<"chunk_plan_hash">> := null} = Run, #{chunks := Chunks, hash := Hash}) ->
    Id = maps:get(<<"id">>, Run),
    {ok, 1} = talk_record_client_repo:freeze_plan_tx(C, Id, Hash),
    lists:foreach(
        fun(Chunk) ->
            {ok, 1} = talk_record_client_repo:insert_part_tx(C, stored_chunk(Id, Chunk))
        end,
        Chunks
    ),
    reuse(C, J, Run, Hash);
freeze(_, _, #{<<"chunk_plan_hash">> := Hash}, #{hash := Hash}) ->
    ok;
freeze(_, _, _, _) ->
    talk_record_client_gate:abort(client_plan_conflict).

stored_chunk(Id, #{<<"source_ranges">> := _} = Chunk) ->
    (maps:remove(<<"source_ranges">>, Chunk))#{
        <<"run_id">> => Id,
        <<"source_ranges">> => jsone:encode(maps:get(<<"source_ranges">>, Chunk))
    };
stored_chunk(Id, #{<<"seq">> := Seq, <<"source_segment_ids">> := Ids, <<"source_hash">> := Hash}) ->
    #{
        <<"run_id">> => Id,
        <<"seq">> => Seq,
        <<"source_part_manifest_hash">> => Hash,
        <<"source_ranges">> => jsone:encode([#{<<"segment_id">> => I} || I <- Ids]),
        <<"offset_ms">> => 0,
        <<"duration_ms">> => 0,
        <<"byte_size">> => 0
    }.

reuse(C, J, Run, Hash) ->
    reuse(C, J, Run, Hash, Run).

reuse(C, J, Run, Hash, Child) ->
    case talk_record_client_repo:previous_tx(C, maps:get(<<"id">>, J), maps:get(<<"id">>, Child)) of
        {ok, Old} ->
            talk_record_client_gate:ensure(
                maps:get(<<"job_id">>, Old) =:= maps:get(<<"id">>, J) andalso
                    maps:get(<<"record_id">>, Old) =:= maps:get(<<"record_id">>, Run) andalso
                    maps:get(<<"attempt">>, Old) =:= maps:get(<<"attempt">>, Child) - 1,
                client_execution_conflict
            ),
            Parts = talk_record_client_gate:require(
                talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Old))
            ),
            legal_predecessor(Child, Old, Parts),
            Keys = [
                <<"provider">>,
                <<"model">>,
                <<"policy_version">>,
                <<"config_version">>,
                <<"input_manifest_hash">>,
                <<"input_content_revision">>,
                <<"input_transcript_version">>,
                <<"input_snapshot">>
            ],
            case
                {
                    maps:with(Keys, Run) =:= maps:with(Keys, Old),
                    maps:get(<<"chunk_plan_hash">>, Old)
                }
            of
                {true, Hash} ->
                    {ok, _} = talk_record_client_repo:reuse_tx(
                        C, maps:get(<<"id">>, Run), maps:get(<<"id">>, Old)
                    ),
                    reuse(C, J, Run, Hash, Old);
                {true, null} ->
                    talk_record_client_gate:ensure(Parts =:= [], client_plan_conflict),
                    reuse(C, J, Run, Hash, Old);
                {false, _} ->
                    talk_record_client_gate:abort(client_execution_conflict);
                {true, _} ->
                    talk_record_client_gate:abort(client_plan_conflict)
            end;
        {error, not_found} ->
            talk_record_client_gate:ensure(
                maps:get(<<"attempt">>, Child) =:= 1 andalso
                    maps:get(<<"replaces_run_id">>, Child, null) =:= null andalso
                    maps:get(<<"ack_possible_duplicate_charge">>, Child, false) =:= false,
                client_execution_conflict
            );
        {error, E} ->
            talk_record_client_gate:abort(E)
    end.

legal_predecessor(Child, Old, Parts) ->
    State = maps:get(<<"state">>, Old),
    Unknown = lists:any(
        fun(P) ->
            lists:member(
                maps:get(<<"provider_state">>, P),
                [<<"dispatching">>, <<"provider_outcome_unknown">>]
            )
        end,
        Parts
    ),
    talk_record_client_gate:ensure(
        not Unknown orelse State =:= <<"provider_outcome_unknown">>, client_execution_conflict
    ),
    Legal =
        case State of
            <<"provider_outcome_unknown">> ->
                maps:get(<<"replaces_run_id">>, Child, null) =:= maps:get(<<"id">>, Old) andalso
                    maps:get(<<"ack_possible_duplicate_charge">>, Child, false) =:= true;
            Safe when Safe =:= <<"expired">>; Safe =:= <<"failed">> ->
                maps:get(<<"replaces_run_id">>, Child, null) =:= null andalso
                    maps:get(<<"ack_possible_duplicate_charge">>, Child, false) =:= false;
            _ ->
                false
        end,
    talk_record_client_gate:ensure(Legal, client_execution_conflict).

part(C, Run, Seq) ->
    {ok, Parts} = talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run)),
    case [P || P <- Parts, maps:get(<<"seq">>, P) =:= Seq] of
        [P] -> P;
        _ -> talk_record_client_gate:abort(client_plan_required)
    end.

dispatch(C, Run, #{<<"provider_state">> := <<"not_dispatched">>} = P, Hash) ->
    {ok, 1} = talk_record_client_repo:dispatch_tx(
        C, maps:get(<<"id">>, Run), maps:get(<<"seq">>, P), elib_tsid:generate(attachment), Hash
    ),
    {ok, 1} = talk_record_client_repo:touch_tx(C, maps:get(<<"id">>, Run)),
    true;
dispatch(_, _, #{<<"request_hash">> := Hash}, Hash) ->
    false;
dispatch(_, _, _, _) ->
    talk_record_client_gate:abort(client_intent_conflict).

request_hash(Run, P, Seq) ->
    talk_record_client_contract:hash(#{
        <<"provider">> => maps:get(<<"provider">>, Run),
        <<"model">> => maps:get(<<"model">>, Run),
        <<"config_version">> => maps:get(<<"config_version">>, Run),
        <<"chunk_plan_hash">> => maps:get(<<"chunk_plan_hash">>, Run),
        <<"seq">> => Seq,
        <<"source_part_manifest_hash">> => maps:get(<<"source_part_manifest_hash">>, P)
    }).

confirm(C, Run, #{<<"provider_state">> := <<"dispatching">>} = P, V) ->
    {ok, 1} = talk_record_client_repo:result_tx(
        C,
        maps:get(<<"id">>, Run),
        maps:get(<<"seq">>, P),
        maps:get(hash, V),
        {maps:get(wire_result, V), maps:get(provider_request_id, V)}
    ),
    {ok, 1} = talk_record_client_repo:touch_tx(C, maps:get(<<"id">>, Run)),
    ok;
confirm(_, _, #{<<"provider_state">> := <<"confirmed">>, <<"result_hash">> := H}, #{hash := H}) ->
    ok;
confirm(_, _, _, _) ->
    talk_record_client_gate:abort(client_result_conflict).

exact_completion(J, Run, Parts) ->
    Snapshot = talk_record_client_gate:json(maps:get(<<"input_snapshot">>, Run)),
    Body = reconstructed_plan(maps:get(<<"kind">>, J), Parts),
    V = talk_record_client_gate:require(
        talk_record_client_contract:plan(maps:get(<<"kind">>, J), Body, Snapshot)
    ),
    talk_record_client_gate:ensure(
        maps:get(hash, V) =:= maps:get(<<"chunk_plan_hash">>, Run), client_plan_conflict
    ),
    talk_record_client_gate:ensure(
        lists:all(fun(P) -> maps:get(<<"provider_state">>, P) =:= <<"confirmed">> end, Parts),
        client_result_conflict
    ).

reconstructed_plan(<<"transcribe">>, Parts) ->
    #{
        <<"overlap_rule">> => <<"none">>,
        <<"chunks">> => [
            (maps:with(
                [
                    <<"seq">>,
                    <<"source_part_manifest_hash">>,
                    <<"offset_ms">>,
                    <<"duration_ms">>,
                    <<"byte_size">>
                ],
                P
            ))#{
                <<"source_ranges">> =>
                    talk_record_client_gate:json(maps:get(<<"source_ranges">>, P))
            }
         || P <- Parts
        ]
    };
reconstructed_plan(<<"summarize">>, Parts) ->
    #{
        <<"overlap_rule">> => <<"none">>,
        <<"chunks">> => [
            #{
                <<"seq">> => maps:get(<<"seq">>, P),
                <<"source_hash">> => maps:get(<<"source_part_manifest_hash">>, P),
                <<"source_segment_ids">> =>
                    [
                        maps:get(<<"segment_id">>, I)
                     || I <- talk_record_client_gate:json(maps:get(<<"source_ranges">>, P))
                    ]
            }
         || P <- Parts
        ]
    }.

reply(C, R, J, Run) ->
    Updated = talk_record_client_gate:require(
        talk_record_client_repo:lock_run_tx(
            C, maps:get(<<"id">>, R), maps:get(<<"id">>, J), maps:get(<<"id">>, Run)
        )
    ),
    {ok, Parts} = talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run)),
    {ok, created, talk_record_client_ds:view(Updated, Parts)}.
