-module(talk_record_client_logic).
-moduledoc "Authenticated BYOK entry, bounded wire values and transaction delegation.".
-export([execute/7, processing_request/4]).

processing_request(Context, Rid, Headers, Body) ->
    case valid_context(Context) andalso valid_body(Body) of
        true -> talk_record_client_requests:create(Context, Rid, Headers, Body);
        false -> {error, forbidden}
    end.

execute(Action, Context, Rid, Jid, RunId, Seq, {Headers, Body}) ->
    case valid_context(Context) andalso valid_body(Body) andalso is_integer(Seq) andalso Seq >= 0 of
        true ->
            talk_record_client_ds:execute(Action, Context, Rid, Jid, RunId, Seq, {Headers, Body});
        false ->
            {error, client_schema_invalid}
    end.

valid_context(#{uid := Uid, did := Did}) ->
    is_integer(Uid) andalso Uid > 0 andalso is_binary(Did) andalso
        byte_size(Did) > 0 andalso byte_size(Did) =< 128;
valid_context(_) ->
    false.

valid_body(Body) when is_map(Body) ->
    try
        {ok, Bytes} = enterprise_cursor_v2:canonical_json(Body),
        byte_size(Bytes) =< 2097152
    catch
        _:_ -> false
    end;
valid_body(_) ->
    false.
