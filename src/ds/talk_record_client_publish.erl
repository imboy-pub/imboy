-module(talk_record_client_publish).
-moduledoc "Publish only complete normalized inputs through existing content repositories.".
-export([publish_tx/5, transcript/2]).

publish_tx(C, R, #{<<"kind">> := <<"transcribe">>} = J, Run, Parts) ->
    Result = transcript(Parts, maps:get(<<"model">>, Run)),
    {ok, _, _} = talk_record_model_result_repo:publish_transcript_tx(C, R, J, Result),
    ok;
publish_tx(C, R, #{<<"kind">> := <<"summarize">>} = J, Run, [Part]) ->
    Wire = talk_record_client_gate:json(maps:get(<<"result">>, Part)),
    Snapshot = talk_record_client_gate:json(maps:get(<<"input_snapshot">>, Run)),
    Ids = [maps:get(<<"id">>, S) || S <- maps:get(<<"segments">>, Snapshot)],
    {ok, Result} = talk_record_processor_result:validate(
        <<"summarize">>,
        Wire,
        #{model_version => maps:get(<<"model">>, Run)},
        #{segment_ids => Ids}
    ),
    talk_record_model_result_repo:publish_summary_tx(C, R, J, Result).

transcript(Parts, Model) ->
    {Speakers, Segments} = lists:foldl(fun part/2, {[], []}, Parts),
    true = length(Speakers) =< 10000 andalso length(Segments) =< 20000,
    true = byte_size(jsone:encode(Segments)) =< 8388608,
    #{
        status =>
            case Segments of
                [] -> <<"no_speech">>;
                _ -> <<"ready">>
            end,
        speakers => Speakers,
        segments => Segments,
        model_version => Model
    }.

part(P, {Speakers, Segments}) ->
    Raw = talk_record_client_gate:json(maps:get(<<"result">>, P)),
    Seq = maps:get(<<"seq">>, P),
    Offset = maps:get(<<"offset_ms">>, P),
    NewSpeakers = [
        S#{<<"label">> => label(Seq, maps:get(<<"label">>, S))}
     || S <- maps:get(<<"speakers">>, Raw)
    ],
    NewSegments = [
        S#{
            <<"speaker_label">> => label(Seq, maps:get(<<"speaker_label">>, S)),
            <<"start_ms">> => maps:get(<<"start_ms">>, S) + Offset,
            <<"end_ms">> => maps:get(<<"end_ms">>, S) + Offset
        }
     || S <- maps:get(<<"segments">>, Raw)
    ],
    {Speakers ++ NewSpeakers, Segments ++ NewSegments}.

label(Seq, Label) -> <<(integer_to_binary(Seq))/binary, ":", Label/binary>>.
