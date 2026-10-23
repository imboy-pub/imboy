-module(talk_record_client_recovery_ds).
-moduledoc "Job-level discovery recovers lost claim/prepare responses without granting a supplier submission.".
-export([execute/5]).
execute(A, Ctx, Rid, Jid, {H, B}) ->
    Result = talk_record_client_ds:tx(fun(C) -> locked(C, A, Ctx, Rid, Jid, H, B) end),
    attach_token(A, Ctx, Rid, Jid, Result).
locked(C, A, Ctx, Rid, Jid, H, B) ->
    R = talk_record_client_gate:record_tx(C, Ctx, Rid, read),
    J = require(talk_record_client_repo:lock_job_tx(C, Rid, Jid)),
    Pin = talk_record_client_gate:json(maps:get(<<"processor_input">>, J)),
    ensure(
        maps:get(<<"provider">>, Pin) =:= <<"doubao">> andalso
            maps:get(<<"kind">>, J) =:= <<"transcribe">> andalso
            maps:get(<<"execution_mode">>, J) =:= <<"client_byok">>,
        async_protocol_required
    ),
    Task = optional(talk_record_provider_repo:latest_task_tx(C, Jid)),
    Run = optional(talk_record_client_repo:latest_tx(C, Jid)),
    case A =:= latest andalso terminal(Task) of
        true ->
            reply_view(C, Ctx, Run, Task, Jid);
        false ->
            _ = talk_record_client_gate:record_tx(C, Ctx, Rid, write),
            Config = talk_record_client_gate:pins_tx(C, R, J),
            case A of
                latest -> reply_view(C, Ctx, Run, Task, Jid);
                recover_unprepared -> recover(C, Ctx, R, J, Run, Task, Config, H, B)
            end
    end.
reply_view(C, Ctx, Run, Task, Jid) ->
    Parts =
        case Run of
            null -> [];
            _ -> require(talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run)))
        end,
    Facts = require(talk_record_client_recovery_repo:facts_tx(C, Jid)),
    {ok, read, talk_record_client_recovery_contract:view(Run, Parts, Task, Ctx, Facts)}.
recover(C, Ctx, R, J, Run, Task, Config, H, B) ->
    ensure(Task =:= null andalso Run =/= null, client_execution_conflict),
    Facts = require(talk_record_client_recovery_repo:facts_tx(C, maps:get(<<"id">>, J))),
    ensure(talk_record_client_recovery_contract:safe(Facts), client_execution_conflict),
    Parsed = consent(J, Config, B),
    ensure(
        lists:member(maps:get(<<"state">>, J), [
            <<"queued">>, <<"retry_wait">>, <<"running">>, <<"failed">>
        ]),
        client_execution_conflict
    ),
    Resource = resource(J, Run),
    talk_record_client_ds:idempotent(C, Ctx, Resource, H, B, fun() ->
        talk_record_client_ds:revision(Run, maps:get(if_match, H, undefined)),
        Source = require(talk_record_provider_repo:snapshot_tx(C, maps:get(<<"id">>, R))),
        ensure(byte_size(jsone:encode(Source)) =< 1048576, client_schema_invalid),
        View = recover_run(C, Ctx, R, J, Run, Config, Parsed, Source),
        {ok, created, #{<<"run">> => View, <<"task">> => null, <<"may_submit">> => false}}
    end).
consent(J, Config, B) ->
    Pin = talk_record_client_gate:json(maps:get(<<"processor_input">>, J)),
    Request = (maps:with(
        [<<"provider">>, <<"model">>, <<"policy_version">>, <<"config_version">>], Pin
    ))#{
        <<"mode">> => <<"transcribe">>,
        <<"execution_mode">> => <<"client_byok">>,
        <<"processing_consent">> => maps:get(<<"processing_consent">>, B)
    },
    require(talk_record_client_contract:request(Request, maps:get(public, Config))).
recover_run(
    C,
    #{uid := U, did := D} = Ctx,
    R,
    J,
    #{<<"state">> := <<"active">>, <<"lease_valid">> := true} = Run,
    Config,
    _,
    Source
) ->
    ensure(
        U =:= maps:get(<<"actor_user_id">>, Run) andalso
            D =:= maps:get(<<"device_session_id">>, Run),
        client_execution_conflict
    ),
    ensure(
        Source =:= talk_record_client_gate:json(maps:get(<<"input_snapshot">>, Run)), input_stale
    ),
    run_pins(J, Run),
    Token = token(Config, Run, U, D),
    talk_record_client_gate:run_tx(C, Ctx, J, Run, Token),
    talk_record_provider_ds:response(C, R, J, Run);
recover_run(C, Ctx, R, J, Run, Config, Parsed, Source) ->
    case maps:get(<<"state">>, Run) of
        <<"active">> ->
            case talk_record_client_repo:expire_tx(C, Run) of
                {expired, <<"expired">>} -> ok;
                _ -> talk_record_client_gate:abort(client_execution_conflict)
            end;
        S ->
            ensure(
                lists:member(S, [<<"expired">>, <<"failed">>, <<"cancelled">>]),
                client_execution_conflict
            )
    end,
    talk_record_client_admission:claim_tx(C, R, #{<<"state">> => <<"failed">>}),
    {ok, created, View} = talk_record_client_claim:insert_unprepared_tx(
        C, Ctx, R, J, Config, Parsed, Source
    ),
    View.
attach_token(
    recover_unprepared, #{uid := U, did := D} = Ctx, Rid, Jid, {ok, Mark, #{<<"run">> := View} = P}
) ->
    Checked = talk_record_client_ds:tx(fun(C) ->
        R = talk_record_client_gate:record_tx(C, Ctx, Rid, write),
        J = require(talk_record_client_repo:lock_job_tx(C, Rid, Jid)),
        Config = talk_record_client_gate:pins_tx(C, R, J),
        Run = require(talk_record_client_repo:lock_run_tx(C, Rid, Jid, maps:get(<<"id">>, View))),
        talk_record_client_ds:active(Run),
        run_pins(J, Run),
        T = token(Config, Run, U, D),
        talk_record_client_gate:run_tx(C, Ctx, J, Run, T),
        {ok, T}
    end),
    case Checked of
        {ok, T} -> {ok, Mark, P#{<<"run_token">> => T}};
        E -> E
    end;
attach_token(_, _, _, _, R) ->
    R.
resource(J, R) ->
    <<"/talk-client-recovery/", (integer_to_binary(maps:get(<<"id">>, J)))/binary, "/",
        (integer_to_binary(maps:get(<<"id">>, R)))/binary, ":",
        (integer_to_binary(maps:get(<<"revision">>, R)))/binary>>.
token(Config, Run, U, D) ->
    talk_record_client_policy:token(
        maps:get(signing_key, Config), maps:get(<<"id">>, Run), maps:get(<<"attempt">>, Run), U, D
    ).
terminal(#{<<"state">> := <<"published">>}) -> true;
terminal(#{<<"cancelled_at">> := C}) when C =/= null -> true;
terminal(_) -> false.
optional({ok, V}) -> V;
optional({error, not_found}) -> null;
optional({error, R}) -> talk_record_client_gate:abort(R).
require(V) -> talk_record_client_gate:require(V).
ensure(V, E) -> talk_record_client_gate:ensure(V, E).

run_pins(J, Run) ->
    Pin = talk_record_client_gate:json(maps:get(<<"processor_input">>, J)),
    Keys = [
        <<"input_manifest_hash">>, <<"input_content_revision">>, <<"input_transcript_version">>
    ],
    ensure(maps:with(Keys, J) =:= maps:with(Keys, Run), input_stale),
    ensure(
        lists:all(
            fun(K) -> maps:get(K, Pin) =:= maps:get(K, Run) end,
            [<<"provider">>, <<"model">>, <<"policy_version">>, <<"config_version">>]
        ),
        processing_consent_required
    ).
