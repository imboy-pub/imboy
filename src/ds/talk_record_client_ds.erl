-module(talk_record_client_ds).
-moduledoc "Device-owned execution transactions; no model invocation or server-worker retry.".
-export([execute/7, tx/1, idempotent/6, view/2, active/1, revision/2]).

execute(Action, Context, Rid, Jid, RunId, Seq, {Headers, Body}) ->
    tx(fun(C) ->
        Access =
            case Action of
                get -> read;
                _ -> write
            end,
        Record = talk_record_client_gate:record_tx(C, Context, Rid, Access),
        Job = talk_record_client_gate:require(talk_record_client_repo:lock_job_tx(C, Rid, Jid)),
        case maybe_expire(C, Job) of
            expired -> {error, client_lease_expired};
            current -> dispatch(C, Action, Context, Record, Job, RunId, Seq, Headers, Body)
        end
    end).

maybe_expire(C, J) ->
    case talk_record_client_repo:latest_tx(C, maps:get(<<"id">>, J)) of
        {ok, Run} ->
            case talk_record_client_repo:expire_tx(C, Run) of
                {expired, _} -> expired;
                current -> current
            end;
        {error, not_found} ->
            current;
        {error, R} ->
            talk_record_client_gate:abort(R)
    end.

dispatch(C, claim, Context, Record, Job, _, _, Headers, Body) ->
    _ = talk_record_client_gate:pins_tx(C, Record, Job),
    Payload = idempotent(C, Context, resource(Job, 0, claim, 0), Headers, Body, fun() ->
        talk_record_client_claim:create_tx(C, Context, Record, Job, Headers, Body)
    end),
    token_response(C, Context, Record, Job, Payload);
dispatch(C, get, _Context, Record, Job, RunId, _, _, _) ->
    Run = talk_record_client_gate:require(
        talk_record_client_repo:lock_run_tx(
            C, maps:get(<<"id">>, Record), maps:get(<<"id">>, Job), RunId
        )
    ),
    {ok, Parts} = talk_record_client_repo:parts_tx(C, RunId),
    {ok, read, view(Run, Parts)};
dispatch(C, Action, Context, Record, Job, RunId, Seq, Headers, Body) ->
    Run = talk_record_client_gate:require(
        talk_record_client_repo:lock_run_tx(
            C, maps:get(<<"id">>, Record), maps:get(<<"id">>, Job), RunId
        )
    ),
    talk_record_client_gate:mode(Record, Job),
    talk_record_client_gate:run_tx(C, Context, Job, Run, maps:get(token, Headers, undefined)),
    Config = talk_record_client_gate:require(talk_record_client_policy:get()),
    talk_record_client_gate:ensure(talk_record_client_policy:allowed(Record, Config), forbidden),
    Public = maps:get(public, Config),
    talk_record_client_gate:ensure(
        maps:get(<<"policy_version">>, Public) =:= maps:get(<<"policy_version">>, Run),
        processing_consent_required
    ),
    talk_record_client_gate:ensure(
        maps:get(<<"config_version">>, Public) =:= maps:get(<<"config_version">>, Run),
        processing_consent_required
    ),
    idempotent(C, Context, resource(Job, RunId, Action, Seq), Headers, Body, fun() ->
        active(Run),
        revision(Run, maps:get(if_match, Headers, undefined)),
        _ = talk_record_client_gate:pins_tx(C, Record, Job),
        talk_record_client_operations:perform(C, Action, Record, Job, Run, Seq, Body)
    end).

token_response(C, #{uid := Uid, did := Did}, Record, Job, {ok, Mark, Payload}) ->
    Run = talk_record_client_gate:require(
        talk_record_client_repo:lock_run_tx(
            C, maps:get(<<"id">>, Record), maps:get(<<"id">>, Job), maps:get(<<"id">>, Payload)
        )
    ),
    active(Run),
    Uid = maps:get(<<"actor_user_id">>, Run),
    Did = maps:get(<<"device_session_id">>, Run),
    Config = talk_record_client_gate:require(talk_record_client_policy:get()),
    Token = talk_record_client_policy:token(
        maps:get(signing_key, Config),
        maps:get(<<"id">>, Run),
        maps:get(<<"attempt">>, Run),
        Uid,
        Did
    ),
    Hash = binary:encode_hex(crypto:hash(sha256, Token), lowercase),
    true = talk_record_client_contract:constant_equal(Hash, maps:get(<<"lease_token_hash">>, Run)),
    {ok, Mark, Payload#{
        <<"run_token">> => Token,
        <<"input_snapshot">> => talk_record_client_gate:json(maps:get(<<"input_snapshot">>, Run))
    }};
token_response(_, _, _, _, E) ->
    E.

active(#{<<"state">> := <<"active">>, <<"lease_valid">> := true}) -> ok;
active(_) -> talk_record_client_gate:abort(lease_lost).
revision(Run, Header) ->
    N = talk_record_client_gate:require(
        talk_record_content_common:parse_resource_if_match(
            <<"client-run">>, maps:get(<<"id">>, Run), Header
        )
    ),
    case N =:= maps:get(<<"revision">>, Run) of
        true -> ok;
        false -> talk_record_client_gate:abort(precondition_failed)
    end.

idempotent(C, #{uid := Uid, did := Did}, Resource, Headers, Body, Fun) ->
    Key = maps:get(idempotency_key, Headers, undefined),
    case talk_record_logic:valid_idempotency_key(Key) of
        true -> ok;
        false -> talk_record_client_gate:abort(idempotency_key_required)
    end,
    Digest = talk_record_client_contract:hash(#{
        <<"body">> => Body, <<"authenticated_device">> => Did
    }),
    Expiry = elib_dt:add(elib_dt:now(), {talk_record_job_support:idempotency_ttl_seconds(), second}),
    case
        talk_record_idempotency_repo:record_tx(C, Uid, <<"POST">>, Resource, Key, Digest, Expiry)
    of
        {ok, inserted} ->
            {ok, created, P} = Fun(),
            ok = talk_record_client_gate:require(
                talk_record_idempotency_repo:complete_tx(
                    C,
                    Uid,
                    <<"POST">>,
                    Resource,
                    Key,
                    jsone:encode(#{<<"payload">> => P})
                )
            ),
            {ok, created, P};
        {ok, existing, Row} ->
            replay(talk_record_job_support:replay_idem(Row, Digest));
        E ->
            talk_record_client_gate:require(E)
    end.

replay({ok, replay, #{<<"may_dispatch">> := _} = P}) ->
    {ok, replay, P#{<<"may_dispatch">> => false}};
replay(E) ->
    E.

resource(Job, Run, Action, Seq) ->
    <<"/talk-client/", (integer_to_binary(maps:get(<<"record_id">>, Job)))/binary, "/",
        (integer_to_binary(maps:get(<<"id">>, Job)))/binary, "/", (integer_to_binary(Run))/binary,
        "/", (atom_to_binary(Action))/binary, "/", (integer_to_binary(Seq))/binary>>.

view(Run, Parts) ->
    Base = maps:with(
        [
            <<"id">>,
            <<"record_id">>,
            <<"job_id">>,
            <<"attempt">>,
            <<"provider">>,
            <<"model">>,
            <<"policy_version">>,
            <<"config_version">>,
            <<"input_manifest_hash">>,
            <<"input_content_revision">>,
            <<"input_transcript_version">>,
            <<"chunk_plan_hash">>,
            <<"state">>,
            <<"revision">>,
            <<"lease_expires_at_unix">>
        ],
        Run
    ),
    Base#{
        <<"etag">> =>
            <<"client-run:", (integer_to_binary(maps:get(<<"id">>, Run)))/binary, ":",
                (integer_to_binary(maps:get(<<"revision">>, Run)))/binary>>,
        <<"parts">> => [
            maps:with(
                [<<"seq">>, <<"provider_state">>, <<"result_hash">>, <<"dispatch_intent_id">>], P
            )
         || P <- Parts
        ]
    }.

tx(Fun) ->
    case elib_pg:with_tx(Fun, [{reraise, false}]) of
        {rollback, {talk_client_abort, R}} -> {error, R};
        {rollback, _} -> {error, db_error};
        E -> E
    end.
