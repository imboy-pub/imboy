-module(talk_record_provider_actions).
-moduledoc "Durable submit and query receipts, fenced publication, and explicit cancellation.".
-export([perform/9, owned_run/7]).

perform(C, A, Ctx, R, J, T, Ids, H, B) when A =:= query_execution; A =:= resubmission ->
    talk_record_provider_recovery:create(C, A, Ctx, R, J, T, H, B, Ids);
perform(C, reconcile_receipt, Ctx, R, J, T, _Ids, _H, B) ->
    ensure(maps:get(uid, Ctx) =:= maps:get(<<"actor_user_id">>, T), forbidden),
    _ = require(
        talk_record_client_audio_repo:consent_tx(
            C,
            maps:get(uid, Ctx),
            T#{<<"record_id">> => maps:get(<<"id">>, R)},
            <<"transcribe">>
        )
    ),
    change(C, reconcile_receipt, T, B),
    reply(C, R, J, T, undefined, #{});
perform(C, cancel, _Ctx, R, J, T, _Ids, _H, _) ->
    {ok, 1} = talk_record_provider_repo:cancel_tx(C, maps:get(<<"id">>, T)),
    reply(C, R, J, T, undefined, #{});
perform(C, A, Ctx, R, J, T, Ids, H, B) ->
    Run = owned_run(C, Ctx, R, J, T, Ids, H),
    apply_action(C, A, R, J, T, Run, B).

owned_run(C, Ctx, R, J, T, #{run_id := Id, jid := Jid}, H) ->
    ensure(Jid =:= maps:get(<<"id">>, J), not_found),
    Run = require(talk_record_client_repo:lock_run_tx(C, maps:get(<<"id">>, R), Jid, Id)),
    talk_record_client_gate:run_tx(C, Ctx, J, Run, maps:get(token, H, undefined)),
    talk_record_client_ds:active(Run),
    case maps:get(<<"execution_purpose">>, Run) of
        <<"query_only">> ->
            ensure(maps:get(<<"provider_task_id">>, Run) =:= maps:get(<<"id">>, T), forbidden);
        <<"submit">> ->
            ensure(Id =:= maps:get(<<"source_run_id">>, T), forbidden)
    end,
    Run.

apply_action(C, submit_intent, R, J, T, Run, B) ->
    ensure(maps:get(<<"execution_purpose">>, Run) =:= <<"submit">>, client_intent_conflict),
    Config = require(talk_record_client_policy:get()),
    ensure(talk_record_client_audio_contract:enabled(Config), audio_source_not_ready),
    _ = require(talk_record_client_audio_contract:origin(config_ds:env(base_url, <<>>))),
    {ok, D, May} = require_transition(submit_intent, T, B),
    case May of
        true ->
            Id = elib_tsid:generate(attachment),
            Hash = talk_record_client_contract:hash(#{
                <<"task_id">> => maps:get(<<"id">>, T),
                <<"client_request_id">> => maps:get(<<"client_request_id">>, T),
                <<"profile_hash">> => maps:get(<<"profile_hash">>, T),
                <<"assembly">> => json(maps:get(<<"assembly">>, T)),
                <<"source_authorization_id">> => maps:get(<<"source_authorization_id">>, T),
                <<"expires_at">> => maps:get(<<"source_expires_at">>, D)
            }),
            {ok, 1} = talk_record_provider_repo:update_tx(C, maps:get(<<"id">>, T), D#{
                <<"intent_id">> => Id, <<"request_hash">> => Hash
            }),
            {ok, 1} = talk_record_client_repo:dispatch_tx(C, maps:get(<<"id">>, Run), 0, Id, Hash),
            ok;
        false ->
            ok
    end,
    reply(C, R, J, T, Run, #{<<"may_submit">> => May});
apply_action(C, submission_receipt, R, J, T, Run, B) ->
    ensure(maps:get(<<"execution_purpose">>, Run) =:= <<"submit">>, client_intent_conflict),
    change(C, submission_receipt, T, B),
    reply(C, R, J, T, Run, #{});
apply_action(C, query_intent, R, J, T, Run, B) ->
    {ok, _, false} = require_transition(query_intent, T, B),
    case talk_record_provider_repo:query_intent_tx(C, maps:get(<<"id">>, T)) of
        {ok, 1} -> ok;
        {ok, 0} -> talk_record_client_gate:abort(client_execution_conflict);
        {error, Reason} -> talk_record_client_gate:abort(Reason)
    end,
    reply(C, R, J, T, Run, #{
        <<"may_query">> => true, <<"query_task_id">> => maps:get(<<"provider_task_id">>, T)
    });
apply_action(C, query_receipt, R, J, T, Run, B) ->
    {ok, D0, false} = require_transition(query_receipt, T, B),
    D = validated_result(C, J, Run, T, D0, B),
    {ok, 1} = talk_record_provider_repo:update_tx(C, maps:get(<<"id">>, T), D),
    reply(C, R, J, T, Run, #{});
apply_action(C, complete, R, J, T, Run, _) ->
    complete(C, R, J, T, Run).

complete(C, R, J, T, Run) ->
    ensure(maps:get(<<"state">>, T) =:= <<"result_ready">>, client_result_conflict),
    {ok, Parts} = talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run)),
    ensure(length(Parts) =:= 1, client_plan_conflict),
    [P] = Parts,
    Plan = talk_record_provider_contract:plan(json(maps:get(<<"input_snapshot">>, Run))),
    ensure(
        maps:get(<<"chunk_plan_hash">>, Run) =:= talk_record_client_contract:hash(Plan),
        client_plan_conflict
    ),
    ensure(
        talk_record_client_operations:reconstructed_plan(<<"transcribe">>, Parts) =:= Plan,
        client_plan_conflict
    ),
    ensure(maps:get(<<"provider_state">>, P) =:= <<"confirmed">>, client_result_conflict),
    ensure(
        maps:get(<<"result_hash">>, P) =:= maps:get(<<"result_hash">>, T), client_result_conflict
    ),
    ok = talk_record_client_publish:publish_tx(C, R, J, Run, Parts),
    {ok, 1} = talk_record_client_repo:complete_tx(C, maps:get(<<"id">>, Run)),
    {ok, 1} = talk_record_client_repo:finish_job_tx(C, maps:get(<<"id">>, J), <<"succeeded">>),
    {ok, 1} = talk_record_provider_repo:update_tx(C, maps:get(<<"id">>, T), #{
        <<"state">> => <<"published">>
    }),
    reply(C, R, J, T, Run, #{}).

validated_result(C, J, Run, T, #{<<"state">> := <<"result_ready">>} = D, B) ->
    [P] = require(talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run))),
    ResultBody = (maps:with([<<"result">>, <<"result_hash">>], B))#{
        <<"provider_request_id">> => maps:get(<<"provider_task_id">>, T)
    },
    V = require(
        talk_record_client_contract:result(
            maps:get(<<"kind">>, J),
            ResultBody,
            P,
            json(maps:get(<<"input_snapshot">>, Run))
        )
    ),
    _ =
        case maps:get(<<"provider_state">>, P) of
            <<"dispatching">> ->
                {ok, 1} = talk_record_client_repo:result_tx(
                    C,
                    maps:get(<<"id">>, Run),
                    0,
                    maps:get(hash, V),
                    {maps:get(wire_result, V), maps:get(provider_request_id, V)}
                );
            <<"confirmed">> ->
                ensure(maps:get(<<"result_hash">>, P) =:= maps:get(hash, V), client_result_conflict)
        end,
    D;
validated_result(_, _, _, _, D, _) ->
    D.
change(C, A, T, B) ->
    {ok, D, false} = require_transition(A, T, B),
    {ok, 1} = talk_record_provider_repo:update_tx(C, maps:get(<<"id">>, T), D),
    ok.
require_transition(A, T, B) ->
    case talk_record_provider_contract:transition(A, T, B, erlang:system_time(second)) of
        {ok, _, _} = V -> V;
        {error, E} -> talk_record_client_gate:abort(E)
    end.
reply(C, R, J, T, Run, Extra) ->
    Current = talk_record_provider_ds:task(C, maps:get(<<"id">>, R), maps:get(<<"id">>, T)),
    P = Extra#{<<"task">> => talk_record_provider_contract:view(Current)},
    case Run of
        undefined -> {ok, created, P};
        _ -> {ok, created, P#{<<"run">> => talk_record_provider_ds:response(C, R, J, Run)}}
    end.
require(V) -> talk_record_client_gate:require(V).
ensure(V, E) -> talk_record_client_gate:ensure(V, E).
json(V) -> talk_record_client_gate:json(V).
