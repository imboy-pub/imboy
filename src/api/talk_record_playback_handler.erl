-module(talk_record_playback_handler).
-moduledoc "受JWT和短票据双重保护的回放流，单Range、有界spool读取和逐块撤权重验。".
-export([init/2, byte_range/2]).

init(Req, State) ->
    Result =
        case cowboy_req:method(Req) of
            <<"GET">> -> open(Req, State);
            <<"HEAD">> -> open(Req, State);
            _ -> cowboy_req:reply(405, #{<<"allow">> => <<"GET, HEAD">>}, <<>>, Req)
        end,
    {ok, Result, State}.

open(Req, State) ->
    Uid = maps:get(current_uid, State, 0),
    case {Uid, talk_record_http:path_id(Req), ticket(Req)} of
        {U, {ok, Rid}, Ticket} when
            is_integer(U), U > 0, is_binary(Ticket), byte_size(Ticket) > 0
        ->
            case talk_record_playback_stream:metadata(U, Rid, Ticket) of
                {ok, Meta} ->
                    case respond(Req, Meta, U, Rid, Ticket) of
                        {ok, Response} -> Response;
                        {error, Reason} -> deny(Req, Reason)
                    end;
                {error, Reason} ->
                    deny(Req, Reason)
            end;
        _ ->
            deny(Req, playback_ticket_invalid)
    end.

ticket(Req) ->
    case [V || {<<"ticket">>, V} <- cowboy_req:parse_qs(Req)] of
        [T] when is_binary(T), byte_size(T) =< 8192 -> T;
        _ -> undefined
    end.

respond(Req, #{size := Size, attachment_id := Aid}, Uid, Rid, Ticket) ->
    case byte_range(cowboy_req:header(<<"range">>, Req), Size) of
        {ok, Status, Start, Length} ->
            Headers = headers(Status, Start, Length, Size),
            case cowboy_req:method(Req) of
                <<"HEAD">> ->
                    case talk_record_playback_stream:recheck(Uid, Rid, Ticket) of
                        ok -> {ok, cowboy_req:stream_reply(Status, Headers, Req)};
                        Error -> Error
                    end;
                _ ->
                    talk_record_playback_stream:with_range(
                        Uid, Rid, Ticket, Aid, Start, Length, fun(Source) ->
                            Resp = cowboy_req:stream_reply(Status, Headers, Req),
                            send(Source, 0, Length, Resp, fun() ->
                                talk_record_playback_stream:recheck(Uid, Rid, Ticket)
                            end),
                            {ok, Resp}
                        end
                    )
            end;
        {error, range_not_satisfiable} ->
            {ok,
                cowboy_req:reply(
                    416,
                    #{
                        <<"content-range">> =>
                            <<"bytes */", (integer_to_binary(Size))/binary>>,
                        <<"cache-control">> => <<"no-store">>
                    },
                    <<>>,
                    Req
                )}
    end.

headers(Status, Start, Length, Size) ->
    Base = #{
        <<"content-type">> => <<"audio/aac">>,
        <<"content-length">> => integer_to_binary(Length),
        <<"accept-ranges">> => <<"bytes">>,
        <<"cache-control">> => <<"private, no-store">>,
        <<"x-content-type-options">> => <<"nosniff">>
    },
    case Status of
        206 ->
            Base#{
                <<"content-range">> =>
                    <<"bytes ", (integer_to_binary(Start))/binary, "-",
                        (integer_to_binary(Start + Length - 1))/binary, "/",
                        (integer_to_binary(Size))/binary>>
            };
        200 ->
            Base
    end.

send(_, _, 0, Req, _) ->
    cowboy_req:stream_body(<<>>, fin, Req);
send(Source, Offset, Remaining, Req, Recheck) ->
    %% A revoked/deleted/expired session cannot drain an already opened spool.
    case Recheck() of
        ok -> ok;
        _ -> exit(playback_authorization_changed)
    end,
    case attachment_upload_spool:read_at(Source, Offset, min(65536, Remaining)) of
        {ok, Bytes} when byte_size(Bytes) > 0 ->
            ok = cowboy_req:stream_body(Bytes, nofin, Req),
            send(Source, Offset + byte_size(Bytes), Remaining - byte_size(Bytes), Req, Recheck);
        _ ->
            exit(playback_artifact_truncated)
    end.

deny(Req, Reason) ->
    Status =
        case Reason of
            not_found -> 404;
            playback_media_unavailable -> 409;
            _ -> 403
        end,
    cowboy_req:reply(Status, #{<<"cache-control">> => <<"no-store">>}, <<>>, Req).

%% RFC single-byte range only. Multiple/malformed ranges are rejected, never ignored.
byte_range(undefined, Size) when is_integer(Size), Size > 0 -> {ok, 200, 0, Size};
byte_range(<<"bytes=", Rest/binary>>, Size) when
    byte_size(Rest) =< 100, is_integer(Size), Size > 0
->
    case binary:split(Rest, <<"-">>, [global]) of
        [<<>>, Suffix] ->
            case decimal(Suffix) of
                {ok, N} when N > 0 ->
                    Length = min(N, Size),
                    {ok, 206, Size - Length, Length};
                _ ->
                    {error, range_not_satisfiable}
            end;
        [First, Last] ->
            case {decimal(First), Last} of
                {{ok, Start}, <<>>} when Start < Size -> {ok, 206, Start, Size - Start};
                {{ok, Start}, _} when Start < Size ->
                    case decimal(Last) of
                        {ok, End} when End >= Start ->
                            {ok, 206, Start, min(End, Size - 1) - Start + 1};
                        _ ->
                            {error, range_not_satisfiable}
                    end;
                _ ->
                    {error, range_not_satisfiable}
            end;
        _ ->
            {error, range_not_satisfiable}
    end;
byte_range(_, _) ->
    {error, range_not_satisfiable}.

decimal(<<>>) ->
    error;
decimal(B) ->
    case lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(B)) of
        true -> {ok, binary_to_integer(B)};
        false -> error
    end.
