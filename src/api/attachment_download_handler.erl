-module(attachment_download_handler).
-moduledoc "附件下载代理端点 —— GET/HEAD 经授权 ticket 取 Garage S3 签名 URL 并流式转发。".
-export([init/2]).

init(Req, State) ->
    Reply =
        case cowboy_req:method(Req) of
            <<"GET">> -> serve(Req);
            <<"HEAD">> -> serve(Req);
            _ -> cowboy_req:reply(405, #{}, Req)
        end,
    {ok, Reply, State}.

serve(Req) ->
    Query = cowboy_req:parse_qs(Req),
    Ticket = proplists:get_value(<<"ticket">>, Query, <<>>),
    case attach_logic:download(Ticket) of
        {ok, {disk, I}} -> disk_response(Req, I, Ticket);
        {ok, Url} when is_binary(Url) -> proxy(Req, Url, Ticket);
        _ -> cowboy_req:reply(403, private_headers(), Req)
    end.

proxy(Req, Url, Ticket) ->
    Range = cowboy_req:header(<<"range">>, Req),
    case range_headers(Range) of
        {ok, Headers} ->
            case
                httpc:request(
                    get,
                    {binary_to_list(Url), Headers},
                    [{timeout, 30000}, {connect_timeout, 10000}, {autoredirect, false}],
                    [{sync, false}, {stream, {self, once}}]
                )
            of
                {ok, Id} ->
                    try
                        start_stream(Req, Id, Ticket)
                    after
                        httpc:cancel_request(Id)
                    end;
                _ ->
                    cowboy_req:reply(502, private_headers(), Req)
            end;
        error ->
            cowboy_req:reply(416, private_headers(), Req)
    end.

range_headers(undefined) ->
    {ok, []};
range_headers(Value) when byte_size(Value) =< 100 ->
    case re:run(Value, <<"^bytes=([0-9]+-[0-9]*|-[0-9]+)$">>, [{capture, none}]) of
        match -> {ok, [{"range", binary_to_list(Value)}]};
        _ -> error
    end;
range_headers(_) ->
    error.

start_stream(Req, Id, Ticket) ->
    receive
        {http, {Id, stream_start, Headers, Pid}} ->
            Out = maps:merge(private_headers(), response_headers(Headers)),
            Code =
                case maps:is_key(<<"content-range">>, Out) of
                    true -> 206;
                    false -> 200
                end,
            case attach_logic:download(Ticket) of
                {ok, _} ->
                    case cowboy_req:method(Req) of
                        <<"HEAD">> ->
                            cowboy_req:stream_reply(Code, Out, Req);
                        _ ->
                            Req1 = cowboy_req:stream_reply(Code, Out, Req),
                            httpc:stream_next(Pid),
                            stream(Req1, Id, Pid)
                    end;
                _ ->
                    cowboy_req:reply(403, private_headers(), Req)
            end;
        {http, {Id, {{_, 404, _}, _, _}}} ->
            cowboy_req:reply(404, private_headers(), Req);
        {http, {Id, {{_, 416, _}, Headers, _}}} ->
            cowboy_req:reply(416, maps:merge(private_headers(), response_headers(Headers)), Req);
        {http, {Id, _}} ->
            cowboy_req:reply(502, private_headers(), Req)
    after 30000 -> cowboy_req:reply(504, private_headers(), Req)
    end.

stream(Req, Id, Pid) ->
    receive
        {http, {Id, stream, Bytes}} ->
            ok = cowboy_req:stream_body(Bytes, nofin, Req),
            httpc:stream_next(Pid),
            stream(Req, Id, Pid);
        {http, {Id, stream_end, _}} ->
            ok = cowboy_req:stream_body(<<>>, fin, Req),
            Req;
        {http, {Id, {error, _}}} ->
            exit(attachment_stream_failed)
    after 30000 -> exit(attachment_stream_timeout)
    end.

response_headers(Headers) ->
    maps:from_list([
        {list_to_binary(Name), list_to_binary(Value)}
     || {Name, Value} <- Headers,
        lists:member(Name, ["content-type", "content-length", "content-range", "accept-ranges"])
    ]).

private_headers() ->
    #{
        <<"cache-control">> => <<"private, no-store">>,
        <<"x-content-type-options">> => <<"nosniff">>,
        <<"content-security-policy">> => <<"sandbox">>
    }.

%% 所有 Disk 读取都经过 ticket 与当前 ACL，Range 仅允许单区间。
disk_response(Req, I, Ticket) ->
    case attachment_storage_disk:read_snapshot(I) of
        {ok, Bytes} ->
            case disk_range(cowboy_req:header(<<"range">>, Req), byte_size(Bytes)) of
                {ok, Start, Count, Code, Extra} ->
                    case attach_logic:download(Ticket) of
                        {ok, {disk, _}} ->
                            Headers = maps:merge(private_headers(), Extra#{
                                <<"accept-ranges">> => <<"bytes">>,
                                <<"content-type">> => maps:get(
                                    mime, I, <<"application/octet-stream">>
                                ),
                                <<"content-length">> => integer_to_binary(Count)
                            }),
                            %% Cowboy 按 HEAD 自动省略响应体；传真实 body 计算正确 Content-Length。
                            Body = binary:part(Bytes, Start, Count),
                            cowboy_req:reply(Code, Headers, Body, Req);
                        _ ->
                            cowboy_req:reply(403, private_headers(), Req)
                    end;
                error ->
                    cowboy_req:reply(
                        416,
                        (private_headers())#{
                            <<"content-range">> =>
                                <<"bytes */", (integer_to_binary(byte_size(Bytes)))/binary>>
                        },
                        Req
                    )
            end;
        {error, not_found} ->
            cowboy_req:reply(404, private_headers(), Req);
        _ ->
            cowboy_req:reply(502, private_headers(), Req)
    end.
disk_range(undefined, Size) ->
    {ok, 0, Size, 200, #{}};
disk_range(Range, Size) when is_binary(Range), byte_size(Range) =< 100 ->
    try
        <<"bytes=", Spec/binary>> = Range,
        [A, B] = binary:split(Spec, <<"-">>),
        {Start, End} =
            case {A, B} of
                {<<>>, Tail} ->
                    N = binary_to_integer(Tail),
                    true = N > 0,
                    {max(0, Size - N), Size - 1};
                {Head, <<>>} ->
                    {binary_to_integer(Head), Size - 1};
                {Head, Tail} ->
                    {binary_to_integer(Head), min(Size - 1, binary_to_integer(Tail))}
            end,
        true = Start >= 0 andalso Start < Size andalso End >= Start,
        ContentRange = iolist_to_binary([
            "bytes ", integer_to_list(Start), "-", integer_to_list(End), "/", integer_to_list(Size)
        ]),
        {ok, Start, End - Start + 1, 206, #{<<"content-range">> => ContentRange}}
    catch
        _:_ -> error
    end;
disk_range(_, _) ->
    error.
