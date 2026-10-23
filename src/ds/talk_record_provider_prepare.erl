-module(talk_record_provider_prepare).
-moduledoc "Server-frozen full assembly plan and one durable task per submit run.".
-export([create/4]).
create(C, R, J, Run) ->
    Rid = maps:get(<<"id">>, R),
    Jid = maps:get(<<"id">>, J),
    RunId = maps:get(<<"id">>, Run),
    case talk_record_provider_repo:origin_tx(C, Rid, Jid, RunId) of
        {ok, T} -> reply(C, R, J, Run, T);
        {error, not_found} -> new(C, R, J, Run);
        {error, Reason} -> talk_record_client_gate:abort(Reason)
    end.
new(C, R, J, Run) ->
    Snapshot = talk_record_client_gate:json(maps:get(<<"input_snapshot">>, Run)),
    Current = talk_record_client_gate:require(
        talk_record_provider_repo:snapshot_tx(C, maps:get(<<"id">>, R))
    ),
    talk_record_client_gate:ensure(Current =:= Snapshot, input_stale),
    Plan = talk_record_provider_contract:plan(Snapshot),
    freeze(C, Run, Plan),
    Profile = talk_record_provider_contract:profile(),
    Previous = predecessor(C, R, J, Run),
    T = (maps:with(
        [
            <<"record_id">>,
            <<"job_id">>,
            <<"attempt">>,
            <<"actor_user_id">>,
            <<"provider">>,
            <<"model">>,
            <<"policy_version">>,
            <<"config_version">>,
            <<"consent_event_id">>,
            <<"input_manifest_hash">>,
            <<"input_content_revision">>,
            <<"input_transcript_version">>
        ],
        Run
    ))#{
        <<"id">> => elib_tsid:generate(attachment),
        <<"source_run_id">> => maps:get(<<"id">>, Run),
        <<"seq">> => 0,
        <<"resource_id">> => <<"volc.seedasr.auc">>,
        <<"model_name">> => <<"bigmodel">>,
        <<"request_profile">> => Profile,
        <<"profile_hash">> => talk_record_client_contract:hash(Profile),
        <<"assembly">> => maps:remove(<<"manifest_hash">>, maps:get(<<"assembly">>, Snapshot)),
        <<"client_request_id">> => uuid(),
        <<"source_authorization_id">> => elib_tsid:generate(attachment),
        <<"previous_task_id">> => Previous,
        <<"ack_possible_duplicate_charge">> => maps:get(
            <<"ack_possible_duplicate_charge">>, Run, false
        )
    },
    {ok, 1} = talk_record_provider_repo:insert_tx(C, T),
    reply(
        C, R, J, Run, talk_record_provider_ds:task(C, maps:get(<<"id">>, R), maps:get(<<"id">>, T))
    ).
freeze(C, #{<<"chunk_plan_hash">> := null} = Run, Plan) ->
    Id = maps:get(<<"id">>, Run),
    Hash = talk_record_client_contract:hash(Plan),
    {ok, 1} = talk_record_client_repo:freeze_plan_tx(C, Id, Hash),
    [Chunk] = maps:get(<<"chunks">>, Plan),
    D = Chunk#{
        <<"run_id">> => Id,
        <<"source_ranges">> => jsone:encode(maps:get(<<"source_ranges">>, Chunk))
    },
    {ok, 1} = talk_record_client_repo:insert_part_tx(C, D),
    ok;
freeze(_, Run, Plan) ->
    talk_record_client_gate:ensure(
        maps:get(<<"chunk_plan_hash">>, Run) =:= talk_record_client_contract:hash(Plan),
        client_plan_conflict
    ).
reply(C, R, J, Run, T) ->
    {ok, created, #{
        <<"task">> => talk_record_provider_contract:view(T),
        <<"run">> => talk_record_provider_ds:response(C, R, J, Run),
        <<"may_submit">> => false
    }}.
uuid() ->
    <<A:48, _:4, B:12, _:2, D:62>> = crypto:strong_rand_bytes(16),
    H = binary:encode_hex(<<A:48, 4:4, B:12, 2:2, D:62>>, lowercase),
    <<P:8/binary, Q:4/binary, R:4/binary, S:4/binary, T:12/binary>> = H,
    <<P/binary, "-", Q/binary, "-", R/binary, "-", S/binary, "-", T/binary>>.

predecessor(C, R, J, Run) ->
    case maps:get(<<"replaces_run_id">>, Run, null) of
        null ->
            Paid = talk_record_client_gate:require(
                talk_record_provider_repo:paid_tasks_tx(C, maps:get(<<"id">>, J))
            ),
            talk_record_client_gate:ensure(Paid =:= [], client_execution_conflict),
            null;
        Id ->
            Old = talk_record_client_gate:require(
                talk_record_provider_repo:origin_tx(
                    C, maps:get(<<"id">>, R), maps:get(<<"id">>, J), Id
                )
            ),
            Latest = talk_record_client_gate:require(
                talk_record_provider_repo:latest_task_tx(C, maps:get(<<"id">>, J))
            ),
            talk_record_client_gate:ensure(
                maps:get(<<"id">>, Latest) =:= maps:get(<<"id">>, Old), client_execution_conflict
            ),
            Allowed =
                case maps:get(<<"ack_possible_duplicate_charge">>, Run) of
                    true -> talk_record_provider_contract:can_resubmit(Old);
                    false -> talk_record_provider_contract:can_resume_preparation(Old)
                end,
            talk_record_client_gate:ensure(Allowed, client_execution_conflict),
            maps:get(<<"id">>, Old)
    end.
