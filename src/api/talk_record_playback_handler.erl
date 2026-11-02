-module(talk_record_playback_handler).
-moduledoc "受JWT和短票据双重保护的回放入口：HEAD 有界元数据；GET 302 边缘承载（V3-18 P3）".
%%% V3-18 P3 边缘化：GET 正文不再经 BEAM spool（attachment_artifact_reader
%%% with_range/range_spool 收口）——旧接口保留有界鉴权壳（JWT+票据双重
%%% 守卫、逐请求 recheck、416/HEAD 语义不变、302 不泄私有签名），字节由
%%% 边缘 OS 进程承载（attachment_edge_proxy_handler）。
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

respond(Req, #{size := Size}, Uid, Rid, Ticket) ->
    case byte_range(cowboy_req:header(<<"range">>, Req), Size) of
        {ok, Status, Start, Length} ->
            Headers = headers(Status, Start, Length, Size),
            case cowboy_req:method(Req) of
                <<"HEAD">> ->
                    %% HEAD 语义不变：只读元数据（零字节），逐请求 recheck。
                    case talk_record_playback_stream:recheck(Uid, Rid, Ticket) of
                        ok -> {ok, cowboy_req:stream_reply(Status, Headers, Req)};
                        Error -> Error
                    end;
                _ ->
                    %% V3-18 P3 边缘承载：GET 正文不再经 BEAM spool
                    %% （attachment_artifact_reader:with_range 收口）——同一
                    %% 回放票据 302 到边缘 /edge?ticket=；边缘逐请求回调
                    %% authorize 重放 ACL 后承载 Range/206/416（
                    %% attachment_edge_proxy_handler，语义与原 spool 形态
                    %% 一致）。fail-closed：边缘未配置 → 503，不回退 BEAM。
                    case attachment_edge_redirect:edge_ready() of
                        true ->
                            {ok, attachment_edge_redirect:redirect(Req, Ticket)};
                        false ->
                            {ok,
                                cowboy_req:reply(
                                    503,
                                    #{<<"cache-control">> => <<"private, no-store">>},
                                    <<>>,
                                    Req
                                )}
                    end
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
