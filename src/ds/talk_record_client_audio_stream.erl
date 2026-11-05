-module(talk_record_client_audio_stream).
-moduledoc "Bounded attachment spool streaming; a live source-authority guard runs before IO and each 64KiB chunk.".
-export([fold/5]).

%% Guard is trusted server code returning a freshly authorized immutable binding,
%% never a function supplied by request JSON. No unguarded file/path API exists.
fold(Aid, Range, Guard, Fun, Acc) when
    is_integer(Aid), Aid > 0, is_function(Guard, 0), is_function(Fun, 2)
->
    try
        {ok, B} = Guard(),
        Aid = maps:get(<<"attachment_id">>, B),
        {ok, {Offset, Length}} = talk_record_client_audio_contract:range(
            Range, maps:get(<<"size">>, B)
        ),
        case
            with_published_range(Aid, Offset, Length, fun(Source) ->
                drain(Source, 0, Length, B, Guard, Fun, Acc)
            end)
        of
            {ok, _} = Ok -> Ok;
            _ -> {error, audio_source_denied}
        end
    catch
        _:_ -> {error, audio_source_denied}
    end;
fold(_, _, _, _, _) ->
    {error, audio_source_denied}.

drain(_, _, 0, _, _, _, Acc) ->
    {ok, Acc};
drain(Source, Offset, Left, B, Guard, Fun, Acc) ->
    {ok, B} = Guard(),
    {ok, Bytes} = attachment_upload_spool:read_at(Source, Offset, min(65536, Left)),
    true = byte_size(Bytes) > 0 andalso byte_size(Bytes) =< Left,
    Next = Fun(Bytes, Acc),
    drain(Source, Offset + byte_size(Bytes), Left - byte_size(Bytes), B, Guard, Fun, Next).

%% no-backend-ffmpeg V1：读取权威 = assembly 已发布音频（直传成品附件，
%% assemble job succeeded 锚）——不再要求 derived_write 完成行。
with_published_range(Aid, Offset, Length, Fun) ->
    {ok, Row} =
        elib_pg:with_conn(fun(C) ->
            case
                elib_pg:query(
                    C,
                    <<
                        "SELECT a.path, a.size, a.file_hash256,"
                        " a.storage_driver, a.storage_namespace, a.storage_container,"
                        " a.storage_locator_sha256, a.storage_physical_key"
                        " FROM talk_record_assembly m"
                        " JOIN attachment a ON a.id = m.audio_attachment_id"
                        " JOIN talk_record_job j ON j.record_id = m.record_id"
                        " AND j.kind='assemble' AND j.state='succeeded'"
                        " WHERE m.audio_attachment_id = $1"
                        " AND a.lifecycle_state='live' AND a.status=1 AND a.size>0"
                    >>,
                    [Aid]
                )
            of
                {ok, [R]} -> {ok, R};
                _ -> {error, audio_source_denied}
            end
        end),
    attachment_artifact_reader:with_range(Row, Offset, Length, Fun).
