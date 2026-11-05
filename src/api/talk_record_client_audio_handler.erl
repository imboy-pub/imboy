-module(talk_record_client_audio_handler).
-moduledoc "Purpose-only third-party audio read, bounded ticket query and single byte range.".
-export([init/2, ticket/1, byte_range/2]).
init(Req, State) ->
    Response =
        case cowboy_req:method(Req) of
            <<"GET">> ->
                safe_open(Req);
            <<"HEAD">> ->
                safe_open(Req);
            _ ->
                cowboy_req:reply(
                    405,
                    #{<<"allow">> => <<"GET, HEAD">>, <<"cache-control">> => <<"no-store">>},
                    <<>>,
                    Req
                )
        end,
    {ok, Response, State}.
open(Req) ->
    T = ticket(cowboy_req:parse_qs(Req)),
    case talk_record_client_audio_logic:metadata(T) of
        {ok, M} -> respond(Req, T, M);
        _ -> deny(Req)
    end.
ticket([{<<"ticket">>, T}]) when is_binary(T), byte_size(T) > 0, byte_size(T) =< 8192 -> T;
ticket(_) -> undefined.
respond(Req, T, #{size := N, headers := Base}) ->
    case byte_range(cowboy_req:header(<<"range">>, Req), N) of
        {ok, Status, Start, Length} ->
            H = headers(Status, Start, Length, N, Base),
            case cowboy_req:method(Req) of
                <<"HEAD">> ->
                    case talk_record_client_audio_logic:metadata(T) of
                        {ok, _} -> cowboy_req:stream_reply(Status, H, Req);
                        _ -> deny(Req)
                    end;
                _ ->
                    cowboy_req:reply(503, #{<<"cache-control">> => <<"no-store">>}, <<>>, Req)
            end;
        _ ->
            cowboy_req:reply(
                416,
                #{
                    <<"cache-control">> => <<"no-store">>,
                    <<"content-range">> => <<"bytes */", (integer_to_binary(N))/binary>>
                },
                <<>>,
                Req
            )
    end.
safe_open(Req) ->
    try
        open(Req)
    catch
        _:_ -> deny(Req)
    end.

headers(200, _, Length, _, H) ->
    H#{<<"content-length">> => integer_to_binary(Length)};
headers(206, Start, Length, N, H) ->
    H#{
        <<"content-length">> => integer_to_binary(Length),
        <<"content-range">> =>
            <<"bytes ", (integer_to_binary(Start))/binary, "-",
                (integer_to_binary(Start + Length - 1))/binary, "/", (integer_to_binary(N))/binary>>
    }.
deny(Req) -> cowboy_req:reply(403, #{<<"cache-control">> => <<"no-store">>}, <<>>, Req).
%% Reuse the established pure RFC single-range parser; no playback authorization is delegated.
byte_range(H, N) -> talk_record_playback_handler:byte_range(H, N).
