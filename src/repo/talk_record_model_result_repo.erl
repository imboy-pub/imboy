-module(talk_record_model_result_repo).
-moduledoc "受控模型结果持久化；调用方持有record/job锁并完成CAS门禁。".
-export([publish_transcript_tx/4, publish_summary_tx/4]).

publish_transcript_tx(C, Record, Job, R) ->
    Rid = maps:get(<<"id">>, Record),
    {ok, [#{<<"version">> := V}]} = elib_pg:query(
        C,
        <<"SELECT coalesce(max(version),0)+1 AS version FROM talk_record_transcript WHERE record_id=$1">>,
        [Rid]
    ),
    Tid = elib_tsid:generate(attachment),
    insert(C, <<"talk_record_transcript">>, #{
        <<"id">> => Tid,
        <<"record_id">> => Rid,
        <<"version">> => V,
        <<"source_manifest_hash">> => maps:get(<<"input_manifest_hash">>, Job),
        <<"source_revision">> => maps:get(<<"content_revision">>, Record),
        <<"language">> => maps:get(<<"language">>, Record, <<>>),
        <<"status">> => maps:get(status, R),
        <<"model_version">> => maps:get(model_version, R)
    }),
    Speakers = lists:foldl(
        fun(S, A) ->
            Label = maps:get(<<"label">>, S),
            Id = speaker(C, Rid, Label, V, maps:get(<<"confidence">>, S, null)),
            A#{Label => Id}
        end,
        #{},
        maps:get(speakers, R)
    ),
    _ = lists:foldl(
        fun(S, Seq) ->
            insert(C, <<"talk_record_segment">>, #{
                <<"id">> => elib_tsid:generate(attachment),
                <<"record_id">> => Rid,
                <<"transcript_version">> => V,
                <<"seq">> => Seq,
                <<"speaker_id">> => maps:get(maps:get(<<"speaker_label">>, S), Speakers),
                <<"start_ms">> => maps:get(<<"start_ms">>, S),
                <<"end_ms">> => maps:get(<<"end_ms">>, S),
                <<"text">> => maps:get(<<"text">>, S),
                <<"confidence">> => maps:get(<<"confidence">>, S, null),
                <<"origin">> => <<"model">>,
                <<"is_final">> => true
            }),
            Seq + 1
        end,
        0,
        maps:get(segments, R)
    ),
    {ok, _} = talk_record_summary_repo:mark_stale_tx(C, Rid),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "UPDATE talk_record SET current_transcript_id=$2,"
            "transcript_status='ready',content_revision=content_revision+1,updated_at=now() WHERE id=$1"
        >>,
        [Rid, Tid]
    ),
    {ok, V, maps:get(<<"content_revision">>, Record) + 1}.

speaker(C, Rid, Label, V, Confidence) ->
    Id = elib_tsid:generate(attachment),
    %% Version-scoped cluster labels never silently attach a new model cluster
    %% to a previously manually confirmed person's identity.
    Local = <<"v", (integer_to_binary(V))/binary, ":", Label/binary>>,
    insert(C, <<"talk_record_speaker">>, #{
        <<"id">> => Id,
        <<"record_id">> => Rid,
        <<"local_label">> => Local,
        <<"identity_source">> => <<"anonymous">>,
        <<"confidence">> => Confidence
    }),
    Id.

publish_summary_tx(C, Record, Job, R) ->
    Rid = maps:get(<<"id">>, Record),
    insert(C, <<"talk_record_summary">>, #{
        <<"id">> => elib_tsid:generate(attachment),
        <<"record_id">> => Rid,
        <<"source_transcript_version">> => maps:get(<<"input_transcript_version">>, Job),
        <<"source_content_revision">> => maps:get(<<"content_revision">>, Record),
        <<"status">> => <<"draft">>,
        <<"overview">> => maps:get(overview, R),
        <<"decisions">> => jsone:encode(maps:get(decisions, R)),
        <<"actions">> => jsone:encode(maps:get(actions, R)),
        <<"model_version">> => maps:get(model_version, R)
    }),
    {ok, 1} = elib_pg:execute(
        C, <<"UPDATE talk_record SET summary_status='ready',updated_at=now() WHERE id=$1">>, [Rid]
    ),
    ok.

insert(C, Table, Data) ->
    {Sql, Args} = elib_pg_sql:insert(Table, Data),
    case elib_pg:execute(C, Sql, Args) of
        {ok, 1} -> ok;
        E -> throw({talk_model_abort, E})
    end.
