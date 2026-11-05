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
    %% no-backend-ffmpeg V1：rate 来自 assemble 校验通过的冻结成品参数。
    Rate = final_rate(C, maps:get(<<"id">>, R)),
    Profile = talk_record_provider_contract:profile(Rate),
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

%% 冻结成品真实采样率：complete 事务准入把 final_audio 钉在 assemble job
%% 的 processor_input（BYOK transcribe job 的 processor_input 不含此键），
%% 故从本记录 succeeded 的 assemble job 读证据——单一事实源，不在
%% processing-requests 处重复成品准入逻辑。
final_rate(C, Rid) ->
    Pin =
        case
            elib_pg:query(
                C,
                %% 重录换 manifest 会再建 assemble job（旧行仍 succeeded）——
                %% 取最新 succeeded 行对应当前发布成品。
                <<
                    "SELECT processor_input FROM talk_record_job"
                    " WHERE record_id=$1 AND kind='assemble' AND state='succeeded'"
                    " ORDER BY id DESC LIMIT 1"
                >>,
                [Rid]
            )
        of
            {ok, [Row]} -> maps:get(<<"processor_input">>, Row, null);
            _ -> null
        end,
    Final =
        case Pin of
            Bin when is_binary(Bin) -> jsone:decode(Bin);
            M when is_map(M) -> M;
            %% BYOK transcribe job 无 assemble 成品（processor_input 缺
            %% final_audio 键）：走 final_input_missing，不因 null 容器崩。
            _ -> #{}
        end,
    case maps:get(<<"final_audio">>, Final, undefined) of
        #{<<"sample_rate">> := Rate} when is_integer(Rate), Rate > 0 ->
            case lists:member(Rate, talk_record_provider_contract:rates()) of
                true -> Rate;
                false -> talk_record_client_gate:abort(final_input_invalid)
            end;
        _ ->
            talk_record_client_gate:abort(final_input_missing)
    end.
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
