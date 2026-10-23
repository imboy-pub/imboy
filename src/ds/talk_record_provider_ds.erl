-module(talk_record_provider_ds).
-moduledoc "Locked provider-task control plane, never sends supplier requests or stores credentials.".
-export([execute/5, task/3, response/4, revision/2, require/1, ensure/2]).

execute(A, Ctx, Ids, H, B) ->
    Result = talk_record_client_ds:tx(fun(C) -> locked(C, A, Ctx, Ids, H, B) end),
    source_response(A, Ctx, Ids, H, token_response(A, Ctx, Ids, Result)).

locked(C, A, Ctx, #{rid := Rid} = Ids, H, B) ->
    R = talk_record_client_gate:record_tx(C, Ctx, Rid, write),
    case A of
        prepare ->
            prepare(C, Ctx, R, Ids, H, B);
        _ ->
            T = task(C, Rid, maps:get(task_id, Ids)),
            J = require(talk_record_client_repo:lock_job_tx(C, Rid, maps:get(<<"job_id">>, T))),
            authority(C, Ctx, R, J, T, A),
            case A of
                get -> {ok, read, #{<<"task">> => talk_record_provider_contract:view(T)}};
                _ -> mutate(C, A, Ctx, R, J, T, Ids, H, B)
            end
    end.

prepare(C, Ctx, R, #{jid := Jid, run_id := RunId}, H, B) ->
    Rid = maps:get(<<"id">>, R),
    J = require(talk_record_client_repo:lock_job_tx(C, Rid, Jid)),
    _ = talk_record_client_gate:pins_tx(C, R, J),
    Run = require(talk_record_client_repo:lock_run_tx(C, Rid, Jid, RunId)),
    talk_record_client_gate:run_tx(C, Ctx, J, Run, maps:get(token, H, undefined)),
    ensure(maps:get(<<"provider">>, Run) =:= <<"doubao">>, async_protocol_required),
    ensure(
        maps:get(<<"execution_purpose">>, Run, <<"submit">>) =:= <<"submit">>,
        client_execution_conflict
    ),
    Resource = resource(Rid, RunId, prepare),
    talk_record_client_ds:idempotent(C, Ctx, Resource, H, B, fun() ->
        talk_record_client_ds:active(Run),
        talk_record_client_ds:revision(Run, maps:get(if_match, H, undefined)),
        talk_record_provider_prepare:create(C, R, J, Run)
    end).

authority(C, Ctx, R, J, T, A) ->
    case maps:get(<<"state">>, T) of
        <<"published">> ->
            talk_record_client_gate:mode(R, J);
        _ ->
            _ = talk_record_client_gate:pins_tx(C, R, J),
            ensure(
                maps:get(<<"input_content_revision">>, T) =:= maps:get(<<"content_revision">>, R),
                input_stale
            )
    end,
    Config = require(talk_record_client_policy:get()),
    _ = require(
        talk_record_client_audio_repo:consent_tx(
            C, maps:get(<<"actor_user_id">>, T), T, <<"transcribe">>
        )
    ),
    ensure(talk_record_client_policy:allowed(R, Config), forbidden),
    ensure(
        maps:get(<<"config_version">>, maps:get(public, Config)) =:=
            maps:get(<<"config_version">>, T),
        processing_consent_required
    ),
    ensure(maps:get(<<"provider">>, T) =:= <<"doubao">>, async_protocol_required),
    ensure(maps:get(<<"cancelled_at">>, T) =:= null orelse A =:= get, forbidden),
    ensure(
        maps:get(<<"input_manifest_hash">>, T) =:= maps:get(<<"input_manifest_hash">>, J),
        input_stale
    ),
    ensure(
        maps:get(<<"config_version">>, T) =:=
            maps:get(
                <<"config_version">>,
                talk_record_client_gate:json(maps:get(<<"processor_input">>, J))
            ),
        processing_consent_required
    ),
    Snapshot = require(talk_record_provider_repo:snapshot_tx(C, maps:get(<<"id">>, R))),
    ensure(
        maps:remove(<<"manifest_hash">>, maps:get(<<"assembly">>, Snapshot)) =:=
            talk_record_client_gate:json(maps:get(<<"assembly">>, T)),
        input_stale
    ),
    %% Reading an existing supplier task does not grant a new submit permission.
    ensure(is_integer(maps:get(uid, Ctx)), forbidden),
    Profile = talk_record_provider_contract:profile(),
    ensure(
        Profile =:= talk_record_client_gate:json(maps:get(<<"request_profile">>, T)), input_stale
    ),
    ensure(
        talk_record_client_contract:hash(Profile) =:= maps:get(<<"profile_hash">>, T), input_stale
    ).

mutate(C, A, Ctx, R, J, T, Ids, H, B) ->
    Resource = resource(maps:get(<<"id">>, R), maps:get(<<"id">>, T), A),
    Out = talk_record_client_ds:idempotent(C, Ctx, Resource, H, B, fun() ->
        revision(T, maps:get(if_match, H, undefined)),
        talk_record_provider_actions:perform(C, A, Ctx, R, J, T, Ids, H, B)
    end),
    suppress_replay(A, Out).
suppress_replay(_, {ok, replay, P}) ->
    {ok, replay,
        maps:without([<<"source_url">>], P#{<<"may_submit">> => false, <<"may_query">> => false})};
suppress_replay(_, R) ->
    R.

source_response(submit_intent, Ctx, Ids, H, {ok, created, #{<<"may_submit">> := true} = P}) ->
    Run = maps:get(<<"run">>, P),
    SourceIds = maps:without([task_id], Ids#{seq => 0}),
    case
        talk_record_client_audio_ds:issue(Ctx, SourceIds, H#{if_match => maps:get(<<"etag">>, Run)})
    of
        {ok, #{url := URL}} -> {ok, created, P#{<<"source_url">> => URL}};
        _ -> {error, audio_source_not_ready}
    end;
source_response(_, _, _, _, R) ->
    R.

task(C, Rid, Tid) -> require(talk_record_provider_repo:task_tx(C, Rid, Tid)).
response(C, R, J, Run) ->
    Updated = require(
        talk_record_client_repo:lock_run_tx(
            C, maps:get(<<"id">>, R), maps:get(<<"id">>, J), maps:get(<<"id">>, Run)
        )
    ),
    Parts = require(talk_record_client_repo:parts_tx(C, maps:get(<<"id">>, Run))),
    talk_record_client_ds:view(Updated, Parts).
revision(T, H) ->
    N = require(
        talk_record_content_common:parse_resource_if_match(
            <<"provider-task">>, maps:get(<<"id">>, T), H
        )
    ),
    ensure(N =:= maps:get(<<"revision">>, T), precondition_failed).
resource(Rid, Id, A) ->
    <<"/talk-provider/", (integer_to_binary(Rid))/binary, "/", (integer_to_binary(Id))/binary, "/",
        (atom_to_binary(A))/binary>>.
require(V) -> talk_record_client_gate:require(V).
ensure(V, E) -> talk_record_client_gate:ensure(V, E).

token_response(
    A, #{uid := Uid, did := Did}, #{rid := Rid}, {ok, Mark, #{<<"run">> := View} = P}
) when
    A =:= query_execution; A =:= resubmission
->
    Config = require(talk_record_client_policy:get()),
    Checked = talk_record_client_ds:tx(fun(C) ->
        Run = require(
            talk_record_client_repo:lock_run_tx(
                C, Rid, maps:get(<<"job_id">>, View), maps:get(<<"id">>, View)
            )
        ),
        talk_record_client_ds:active(Run),
        ensure(
            Uid =:= maps:get(<<"actor_user_id">>, Run) andalso
                Did =:= maps:get(<<"device_session_id">>, Run),
            forbidden
        ),
        {ok,
            talk_record_client_policy:token(
                maps:get(signing_key, Config),
                maps:get(<<"id">>, Run),
                maps:get(<<"attempt">>, Run),
                Uid,
                Did
            )}
    end),
    case Checked of
        {ok, Token} -> {ok, Mark, P#{<<"run_token">> => Token}};
        E -> E
    end;
token_response(_, _, _, R) ->
    R.
