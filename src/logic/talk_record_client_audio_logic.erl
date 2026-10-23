-module(talk_record_client_audio_logic).
-moduledoc "Purpose-specific source authority; JWT issuance and bearer-only provider read.".
-export([issue/3, metadata/1, fold/4]).

%% Context must come from the existing JWT/device middleware, not request JSON.
issue(#{uid := Uid, did := Did} = Context, Ids, Headers) ->
    case valid(Context, Ids, Headers) andalso Uid > 0 andalso byte_size(Did) > 0 of
        true -> talk_record_client_audio_ds:issue(Context, Ids, Headers);
        false -> {error, audio_source_denied}
    end;
issue(_, _, _) ->
    {error, audio_source_denied}.

metadata(Ticket) -> talk_record_client_audio_ds:metadata(Ticket).
fold(Ticket, Range, Fun, Acc) -> talk_record_client_audio_ds:fold(Ticket, Range, Fun, Acc).

valid(
    #{uid := Uid, did := Did},
    #{rid := Rid, jid := Jid, run_id := RunId, seq := 0} = Ids,
    #{token := Token, if_match := ETag} = Headers
) ->
    lists:all(fun(N) -> is_integer(N) andalso N > 0 andalso N =< 9223372036854775807 end, [
        Uid, Rid, Jid, RunId
    ]) andalso
        is_binary(Did) andalso byte_size(Did) > 0 andalso byte_size(Did) =< 128 andalso
        is_binary(Token) andalso byte_size(Token) >= 32 andalso byte_size(Token) =< 128 andalso
        is_binary(ETag) andalso byte_size(ETag) > 0 andalso byte_size(ETag) =< 128 andalso
        map_size(Ids) =:= 4 andalso map_size(Headers) =:= 2;
valid(_, _, _) ->
    false.
