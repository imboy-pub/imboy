-module(talk_record_provider_recovery).
-moduledoc "New device query-only leases never recreate an old paid submission.".
-export([create/9]).
create(C, A, Ctx, R, J, T, H, B, _Ids) ->
    ensure(maps:get(<<"state">>, T) =/= <<"published">>, client_execution_conflict),
    case A of
        query_execution ->
            ensure(
                maps:get(<<"provider_task_id">>, T) =/= null andalso
                    lists:member(maps:get(<<"state">>, T), [
                        <<"accepted">>,
                        <<"processing">>,
                        <<"submit_outcome_unknown">>,
                        <<"result_ready">>
                    ]),
                client_intent_conflict
            );
        resubmission ->
            resubmit_permission(T, B),
            Latest = require(talk_record_provider_repo:latest_task_tx(C, maps:get(<<"id">>, J))),
            ensure(maps:get(<<"id">>, Latest) =:= maps:get(<<"id">>, T), client_execution_conflict)
    end,
    _ = H,
    Config = require(talk_record_client_policy:get()),
    Pin = json(maps:get(<<"processor_input">>, J)),
    Request = (maps:with(
        [<<"provider">>, <<"model">>, <<"policy_version">>, <<"config_version">>], Pin
    ))#{
        <<"mode">> => <<"transcribe">>,
        <<"execution_mode">> => <<"client_byok">>,
        <<"processing_consent">> => maps:get(<<"processing_consent">>, B)
    },
    _ = require(talk_record_client_contract:request(Request, maps:get(public, Config))),
    safe_preparation(C, A, J, T, B),
    case available(C, A, Ctx, R, J, T, Config) of
        {reuse, Run} ->
            ready_result(C, Run, T),
            {ok, created, #{
                <<"task">> => talk_record_provider_contract:view(T),
                <<"run">> => talk_record_provider_ds:response(C, R, J, Run),
                <<"may_submit">> => false
            }};
        new ->
            new_run(C, A, Ctx, R, J, T, Config)
    end.
new_run(C, A, Ctx, R, J, T, Config) ->
    CurrentJob = require(
        talk_record_client_repo:lock_job_tx(C, maps:get(<<"id">>, R), maps:get(<<"id">>, J))
    ),
    talk_record_client_admission:claim_tx(C, R, CurrentJob),
    Origin = require(
        talk_record_client_repo:lock_run_tx(
            C, maps:get(<<"id">>, R), maps:get(<<"id">>, J), maps:get(<<"source_run_id">>, T)
        )
    ),
    make_run(C, A, Ctx, R, J, T, Origin, Config).
available(C, A, Ctx, R, J, T, Config) ->
    case talk_record_client_repo:latest_tx(C, maps:get(<<"id">>, J)) of
        {ok, #{<<"state">> := <<"active">>, <<"lease_valid">> := true} = Run} when
            A =:= query_execution
        ->
            reuse(C, Ctx, R, J, T, Run, Config),
            {reuse, Run};
        {ok, #{<<"state">> := <<"active">>, <<"lease_valid">> := true} = Run} when
            A =:= resubmission
        ->
            reuse(C, Ctx, R, J, T, Run, Config),
            {State, Code} =
                case talk_record_provider_contract:can_resume_preparation(T) of
                    true -> {<<"expired">>, <<"device_stopped">>};
                    false -> {<<"provider_outcome_unknown">>, <<"network_unknown">>}
                end,
            {ok, 1} = talk_record_client_repo:settle_tx(C, maps:get(<<"id">>, Run), State, Code),
            new;
        {ok, #{<<"state">> := <<"active">>} = Run} ->
            case talk_record_client_repo:expire_tx(C, Run) of
                current -> talk_record_client_gate:abort(client_execution_conflict);
                {expired, _} -> new
            end;
        {ok, _} ->
            new;
        {error, not_found} ->
            new;
        {error, Reason} ->
            talk_record_client_gate:abort(Reason)
    end.
reuse(C, #{uid := Uid, did := Did} = Ctx, R, J, T, Run, Config) ->
    ensure(maps:get(<<"record_id">>, Run) =:= maps:get(<<"id">>, R), client_execution_conflict),
    ensure(maps:get(<<"job_id">>, Run) =:= maps:get(<<"id">>, J), client_execution_conflict),
    ensure(
        Uid =:= maps:get(<<"actor_user_id">>, Run) andalso
            Did =:= maps:get(<<"device_session_id">>, Run),
        client_execution_conflict
    ),
    Bound =
        case maps:get(<<"execution_purpose">>, Run) of
            <<"submit">> -> maps:get(<<"id">>, Run) =:= maps:get(<<"source_run_id">>, T);
            <<"query_only">> -> maps:get(<<"provider_task_id">>, Run) =:= maps:get(<<"id">>, T)
        end,
    ensure(Bound, client_execution_conflict),
    Token = talk_record_client_policy:token(
        maps:get(signing_key, Config),
        maps:get(<<"id">>, Run),
        maps:get(<<"attempt">>, Run),
        Uid,
        Did
    ),
    talk_record_client_gate:run_tx(C, Ctx, J, Run, Token).
ready_result(C, Run, #{<<"state">> := <<"result_ready">>} = T) ->
    [P] = require(talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run))),
    ensure(
        maps:get(<<"provider_state">>, P) =:= <<"confirmed">> andalso
            maps:get(<<"result_hash">>, P) =:= maps:get(<<"result_hash">>, T),
        client_result_conflict
    );
ready_result(_, _, _) ->
    ok.
make_run(C, A, #{uid := Uid, did := Did}, R, J, T, Origin, Config) ->
    Id = elib_tsid:generate(attachment),
    Attempt = maps:get(<<"attempt">>, J) + 1,
    Token = talk_record_client_policy:token(maps:get(signing_key, Config), Id, Attempt, Uid, Did),
    Consent = require(
        talk_record_client_repo:consent_tx(
            C,
            maps:get(<<"id">>, R),
            Uid,
            <<"transcribe">>,
            {maps:get(<<"policy_version">>, T), maps:get(<<"config_version">>, T)}
        )
    ),
    Base = maps:with(
        [
            <<"record_id">>,
            <<"job_id">>,
            <<"provider">>,
            <<"model">>,
            <<"policy_version">>,
            <<"config_version">>,
            <<"input_manifest_hash">>,
            <<"input_content_revision">>,
            <<"input_transcript_version">>
        ],
        Origin
    ),
    Query = A =:= query_execution,
    D = Base#{
        <<"id">> => Id,
        <<"attempt">> => Attempt,
        <<"actor_user_id">> => Uid,
        <<"device_session_id">> => Did,
        <<"lease_token_hash">> => binary:encode_hex(crypto:hash(sha256, Token), lowercase),
        <<"lease_until">> => elib_dt:add(elib_dt:now(), {300, second}),
        <<"consent_event_id">> => Consent,
        <<"input_snapshot">> => maps:get(<<"input_snapshot">>, Origin),
        <<"execution_purpose">> =>
            case Query of
                true -> <<"query_only">>;
                false -> <<"submit">>
            end,
        <<"provider_task_id">> =>
            case Query of
                true -> maps:get(<<"id">>, T);
                false -> null
            end,
        <<"replaces_run_id">> =>
            case Query of
                true -> null;
                false -> maps:get(<<"id">>, Origin)
            end,
        <<"ack_possible_duplicate_charge">> => not Query andalso
            not talk_record_provider_contract:can_resume_preparation(T)
    },
    {ok, 1} = talk_record_client_repo:claim_job_tx(C, maps:get(<<"id">>, J), Attempt),
    {ok, 1} = talk_record_client_repo:insert_run_tx(C, D),
    Run = require(
        talk_record_client_repo:lock_run_tx(C, maps:get(<<"id">>, R), maps:get(<<"id">>, J), Id)
    ),
    finalize(C, A, R, J, T, Run, Origin, Token).
finalize(C, resubmission, R, J, T, Run, _Origin, Token) ->
    {ok, created, P} = talk_record_provider_prepare:create(C, R, J, Run),
    New = maps:get(<<"task">>, P),
    %% These immutable predecessor facts must be inserted, never patched after creation.
    ensure(maps:get(<<"id">>, New) =/= maps:get(<<"id">>, T), client_execution_conflict),
    _ = Token,
    {ok, created, P};
finalize(C, query_execution, R, J, T, Run, Origin, _Token) ->
    Id = maps:get(<<"id">>, Run),
    {ok, 1} = talk_record_client_repo:freeze_plan_tx(
        C, Id, maps:get(<<"chunk_plan_hash">>, Origin)
    ),
    [P] = require(talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Origin))),
    D = (maps:with(
        [
            <<"seq">>,
            <<"source_part_manifest_hash">>,
            <<"source_ranges">>,
            <<"offset_ms">>,
            <<"duration_ms">>,
            <<"byte_size">>
        ],
        P
    ))#{
        <<"run_id">> => Id,
        <<"source_ranges">> => jsone:encode(json(maps:get(<<"source_ranges">>, P))),
        <<"dispatch_intent_id">> => maps:get(<<"intent_id">>, T),
        <<"request_hash">> => maps:get(<<"request_hash">>, T),
        <<"provider_state">> => <<"dispatching">>
    },
    {ok, 1} = talk_record_client_repo:insert_part_tx(C, D),
    restore_result(C, Id, T),
    {ok, created, #{
        <<"task">> => talk_record_provider_contract:view(T),
        <<"run">> => talk_record_provider_ds:response(C, R, J, Run),
        <<"may_submit">> => false
    }}.
restore_result(C, Id, #{<<"state">> := <<"result_ready">>} = T) ->
    {ok, 1} = talk_record_client_repo:result_tx(
        C,
        Id,
        0,
        maps:get(<<"result_hash">>, T),
        {json(maps:get(<<"result">>, T)), maps:get(<<"provider_task_id">>, T)}
    ),
    ok;
restore_result(_, _, _) ->
    ok.
require(V) -> talk_record_client_gate:require(V).
ensure(V, E) -> talk_record_client_gate:ensure(V, E).
json(V) -> talk_record_client_gate:json(V).

resubmit_permission(T, B) ->
    case maps:get(<<"ack_possible_duplicate_charge">>, B) of
        true ->
            ensure(talk_record_provider_contract:can_resubmit(T), client_execution_conflict);
        false ->
            ensure(
                talk_record_provider_contract:can_resume_preparation(T), client_execution_conflict
            )
    end.
safe_preparation(C, resubmission, J, T, #{<<"ack_possible_duplicate_charge">> := false}) ->
    Run = require(
        talk_record_client_repo:lock_run_tx(
            C,
            maps:get(<<"record_id">>, T),
            maps:get(<<"id">>, J),
            maps:get(<<"source_run_id">>, T)
        )
    ),
    [P] = require(talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run))),
    ensure(
        maps:get(<<"seq">>, P) =:= 0 andalso
            maps:get(<<"provider_state">>, P) =:= <<"not_dispatched">> andalso
            maps:get(<<"dispatch_intent_id">>, P, null) =:= null andalso
            maps:get(<<"request_hash">>, P, null) =:= null,
        client_intent_conflict
    );
safe_preparation(_, _, _, _, _) ->
    ok.
