-module(talk_record_playback_parts_handler).
-moduledoc "Short-lived, authenticated source-part playback.".
-export([init/2]).

init(Req, State) ->
    Result = handle(Req, State),
    {ok, Result, State}.

handle(Req, #{action := list} = State) ->
    case {cowboy_req:method(Req), maps:get(current_uid, State, 0), talk_record_http:path_id(Req)} of
        {<<"POST">>, Uid, {ok, Rid}} when is_integer(Uid), Uid > 0 ->
            talk_record_http:respond_ok(Req, talk_record_playback_parts:open(Uid, Rid));
        _ ->
            talk_record_http:method_not_allowed(Req)
    end;
handle(Req, State) ->
    Method = cowboy_req:method(Req),
    Uid = maps:get(current_uid, State, 0),
    case {Method, Uid, talk_record_http:path_id(Req), seq(Req), ticket(Req)} of
        {M, U, {ok, Rid}, {ok, Seq}, Ticket} when
            (M =:= <<"GET">> orelse M =:= <<"HEAD">>),
            is_integer(U),
            U > 0,
            is_binary(Ticket),
            byte_size(Ticket) > 0
        ->
            stream(Req, U, Rid, Ticket, Seq);
        _ ->
            deny(Req, playback_ticket_invalid)
    end.

seq(Req) ->
    S = cowboy_req:binding(seq, Req, <<>>),
    case
        byte_size(S) > 0 andalso byte_size(S) =< 6 andalso
            lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(S))
    of
        true -> {ok, binary_to_integer(S)};
        false -> error
    end.

ticket(Req) ->
    case [V || {<<"ticket">>, V} <- cowboy_req:parse_qs(Req)] of
        [T] when is_binary(T), byte_size(T) =< 8192 -> T;
        _ -> undefined
    end.

stream(Req, Uid, Rid, Ticket, Seq) ->
    case talk_record_playback_parts:metadata(Uid, Rid, Ticket, Seq) of
        {ok, P} -> respond(Req, Uid, Rid, Ticket, Seq, P);
        {error, Reason} -> deny(Req, Reason)
    end.

respond(Req, Uid, Rid, Ticket, Seq, P) ->
    Size = maps:get(<<"byte_size">>, P),
    case talk_record_playback_handler:byte_range(cowboy_req:header(<<"range">>, Req), Size) of
        {ok, Status, Start, Length} ->
            H = headers(Status, Start, Length, Size),
            case cowboy_req:method(Req) of
                <<"HEAD">> ->
                    case talk_record_playback_parts:recheck_part(Uid, Rid, Ticket, Seq, P) of
                        ok -> cowboy_req:stream_reply(Status, H, Req);
                        {error, Reason} -> deny(Req, Reason)
                    end;
                _ ->
                    send_source(Req, Uid, Rid, Ticket, Seq, P, Status, Start, Length, H)
            end;
        _ ->
            cowboy_req:reply(
                416,
                #{
                    <<"cache-control">> => <<"no-store">>,
                    <<"content-range">> => <<"bytes */", (integer_to_binary(Size))/binary>>
                },
                <<>>,
                Req
            )
    end.

send_source(Req, Uid, Rid, Ticket, Seq, Expected, Status, Start, Length, H) ->
    case
        talk_record_playback_parts:with_source(Uid, Rid, Ticket, Seq, fun(Source, Size, P) ->
            case P =:= Expected andalso Start + Length =< Size of
                true ->
                    Reply = cowboy_req:stream_reply(Status, H, Req),
                    send(Source, Start, Length, Reply, Uid, Rid, Ticket, Seq, P),
                    {ok, Reply};
                false ->
                    {error, playback_media_unavailable}
            end
        end)
    of
        {ok, Reply} -> Reply;
        {error, Reason} -> deny(Req, Reason)
    end.

headers(Status, Start, Length, Size) ->
    H = #{
        <<"content-type">> => <<"audio/aac">>,
        <<"content-length">> => integer_to_binary(Length),
        <<"accept-ranges">> => <<"bytes">>,
        <<"cache-control">> => <<"private, no-store">>,
        <<"x-content-type-options">> => <<"nosniff">>
    },
    case Status of
        206 ->
            H#{
                <<"content-range">> =>
                    <<"bytes ", (integer_to_binary(Start))/binary, "-",
                        (integer_to_binary(Start + Length - 1))/binary, "/",
                        (integer_to_binary(Size))/binary>>
            };
        _ ->
            H
    end.

send(_, _, 0, Req, _, _, _, _, _) ->
    cowboy_req:stream_body(<<>>, fin, Req);
send(Source, Offset, Remaining, Req, Uid, Rid, Ticket, Seq, P) ->
    case talk_record_playback_parts:recheck_part(Uid, Rid, Ticket, Seq, P) of
        ok ->
            case attachment_upload_spool:read_at(Source, Offset, min(65536, Remaining)) of
                {ok, Bytes} when byte_size(Bytes) > 0 ->
                    ok = cowboy_req:stream_body(Bytes, nofin, Req),
                    send(
                        Source,
                        Offset + byte_size(Bytes),
                        Remaining - byte_size(Bytes),
                        Req,
                        Uid,
                        Rid,
                        Ticket,
                        Seq,
                        P
                    );
                _ ->
                    exit(playback_artifact_truncated)
            end;
        _ ->
            exit(playback_authorization_changed)
    end.

deny(Req, Reason) ->
    Status =
        case Reason of
            not_found -> 404;
            playback_media_unavailable -> 409;
            playback_busy -> 429;
            _ -> 403
        end,
    Headers =
        case Status of
            429 -> #{<<"cache-control">> => <<"no-store">>, <<"retry-after">> => <<"1">>};
            _ -> #{<<"cache-control">> => <<"no-store">>}
        end,
    cowboy_req:reply(Status, Headers, <<>>, Req).
