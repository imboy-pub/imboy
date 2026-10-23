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
            attachment_derived_artifact:with_range(Aid, Offset, Length, fun(Source) ->
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
