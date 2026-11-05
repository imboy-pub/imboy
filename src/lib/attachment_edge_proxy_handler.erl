-module(attachment_edge_proxy_handler).
-moduledoc "V3-08 下载边缘代理 handler（运行于边缘 OS 进程 VM，非主 BEAM）".
%%%
%% 由 priv/workers/download_edge.escript（独立长驻进程）加载执行：
%%   * 逐请求回调主 BEAM authorize（loopback + 内部共享密钥）；
%%   * 字节仅在边缘进程流动（Garage → 客户端流式转发）；
%%   * 隐藏 origin：上游 URL 永不出现在响应；无权限/不存在统一 404。
%% 本模块代码住主仓（编译进 ebin），但不在主 BEAM 请求路径上执行。

-export([init/2]).

init(Req0, Ctx) ->
    {ok, handle(Req0, Ctx), Ctx}.

handle(Req, {AuthorizeUrl, Token, client_audio}) ->
    case {cowboy_req:method(Req), cowboy_req:parse_qs(Req)} of
        {M, [{<<"ticket">>, Ticket}]} when
            (M =:= <<"GET">> orelse M =:= <<"HEAD">>),
            byte_size(Ticket) > 0,
            byte_size(Ticket) =< 8192
        ->
            case authorize(AuthorizeUrl, Token, <<"client_audio:", Ticket/binary>>, M) of
                {ok, UpstreamUrl} ->
                    proxy(Req, UpstreamUrl, cowboy_req:header(<<"range">>, Req), undefined);
                _ ->
                    hidden_deny(Req)
            end;
        {M, _} when M =:= <<"GET">>; M =:= <<"HEAD">> -> hidden_deny(Req);
        _ ->
            reply(Req, 405, #{<<"allow">> => <<"GET, HEAD">>}, <<>>)
    end;
handle(Req, {AuthorizeUrl, Token, avatar}) ->
    case cowboy_req:method(Req) of
        M when M =:= <<"GET">>; M =:= <<"HEAD">> ->
            Alias = cowboy_req:binding(alias, Req, <<>>),
            Ticket = <<"avatar:", Alias/binary>>,
            case authorize(AuthorizeUrl, Token, Ticket, M) of
                {ok, UpstreamUrl} ->
                    proxy(Req, UpstreamUrl, cowboy_req:header(<<"range">>, Req), undefined);
                _ ->
                    hidden_deny(Req)
            end;
        _ ->
            reply(Req, 405, #{}, <<>>)
    end;
handle(Req, {AuthorizeUrl, Token}) ->
    case cowboy_req:method(Req) of
        M when M =:= <<"GET">>; M =:= <<"HEAD">> ->
            Query = cowboy_req:parse_qs(Req),
            Ticket = proplists:get_value(<<"ticket">>, Query, <<>>),
            Range = cowboy_req:header(<<"range">>, Req),
            case authorize(AuthorizeUrl, Token, Ticket, M) of
                {ok, UpstreamUrl} ->
                    proxy(Req, UpstreamUrl, Range, undefined);
                {ok, UpstreamUrl, {verified_part, Size, Hash}} ->
                    verified_part(
                        Req, UpstreamUrl, Range, Size, Hash, AuthorizeUrl, Token, Ticket, M
                    );
                {ok, UpstreamUrl, Filename} ->
                    proxy(Req, UpstreamUrl, Range, Filename);
                deny ->
                    hidden_deny(Req)
            end;
        _ ->
            reply(Req, 405, #{}, <<>>)
    end.

authorize(AuthorizeUrl, Token, Ticket, Method) ->
    Body = jsone:encode(#{<<"ticket">> => Ticket, <<"method">> => Method}),
    ReqHeaders = [
        {"content-type", "application/json"},
        {"x-edge-internal-token", Token}
    ],
    case
        httpc:request(
            post,
            {AuthorizeUrl, ReqHeaders, "application/json", binary_to_list(Body)},
            [{timeout, 10000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, 200, _}, _, RespBody}} ->
            try
                case jsone:decode(RespBody) of
                    #{<<"url">> := Url, <<"size">> := Size, <<"sha256">> := Hash} when
                        is_binary(Url),
                        is_integer(Size),
                        Size > 0,
                        Size =< 134217728,
                        is_binary(Hash),
                        byte_size(Hash) =:= 64
                    ->
                        {ok, Url, {verified_part, Size, Hash}};
                    #{<<"url">> := Url, <<"filename">> := Filename} when
                        is_binary(Url),
                        is_binary(Filename),
                        byte_size(Filename) > 0,
                        byte_size(Filename) =< 255
                    ->
                        %% V3-18 P3/EXC-2：导出域文件名（白名单单头透传）。
                        {ok, Url, sanitize_filename(Filename)};
                    #{<<"url">> := Url} when is_binary(Url) ->
                        {ok, Url};
                    _ ->
                        deny
                end
            catch
                _:_ -> deny
            end;
        _ ->
            deny
    end.

%% Content-Disposition filename 白名单化：仅留文件名字符，禁路径/引号注入。
sanitize_filename(Raw) ->
    Keep = [
        C
     || C <- binary_to_list(Raw),
        (C >= $a andalso C =< $z) orelse
            (C >= $A andalso C =< $Z) orelse (C >= $0 andalso C =< $9) orelse
            C =:= $. orelse C =:= $- orelse C =:= $_
    ],
    list_to_binary(Keep).

proxy(Req, Url, Range, Filename) ->
    case normalize_range(Range) of
        invalid ->
            reply(Req, 416, #{}, <<>>);
        Headers ->
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
                        start_stream(Req, Id, Filename)
                    after
                        httpc:cancel_request(Id)
                    end;
                _ ->
                    reply(Req, 502, #{}, <<>>)
            end
    end.

%% Verify the entire bounded source before returning any part of a historical object.
verified_part(Req, Url, Range, Size, Hash, AuthorizeUrl, Token, Ticket, Method) ->
    Lock = {{?MODULE, verified_part}, self()},
    case global:set_lock(Lock, [node()], 0) of
        true ->
            try
                verified_part_locked(
                    Req, Url, Range, Size, Hash, AuthorizeUrl, Token, Ticket, Method
                )
            after
                global:del_lock(Lock, [node()])
            end;
        false ->
            reply(Req, 429, #{<<"retry-after">> => <<"1">>}, <<>>)
    end.

verified_part_locked(Req, Url, Range, Size, Hash, AuthorizeUrl, Token, Ticket, Method) ->
    Deadline = erlang:monotonic_time(millisecond) + 30000,
    case talk_record_playback_handler:byte_range(Range, Size) of
        {ok, Status, Start, Length} ->
            case
                httpc:request(
                    get,
                    {binary_to_list(Url), []},
                    [{timeout, 30000}, {connect_timeout, 10000}, {autoredirect, false}],
                    [{sync, false}, {stream, {self, once}}]
                )
            of
                {ok, Id} ->
                    try
                        case verified_collect(Id, Size, Deadline) of
                            {ok, Bytes} ->
                                Actual = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
                                case crypto:hash_equals(Actual, Hash) of
                                    true ->
                                        case authorize(AuthorizeUrl, Token, Ticket, Method) of
                                            {ok, _, {verified_part, Size, Hash}} ->
                                                verified_reply(
                                                    Req, Status, Start, Length, Size, Bytes
                                                );
                                            _ ->
                                                hidden_deny(Req)
                                        end;
                                    false ->
                                        hidden_deny(Req)
                                end;
                            _ ->
                                hidden_deny(Req)
                        end
                    after
                        httpc:cancel_request(Id)
                    end;
                _ ->
                    hidden_deny(Req)
            end;
        _ ->
            reply(
                Req,
                416,
                #{<<"content-range">> => <<"bytes */", (integer_to_binary(Size))/binary>>},
                <<>>
            )
    end.

verified_collect(Id, Size, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {http, {Id, stream_start, _, Pid}} ->
            httpc:stream_next(Pid),
            verified_collect(Id, Pid, Size, 0, [], Deadline);
        {http, {Id, _}} ->
            {error, upstream}
    after Remaining -> {error, timeout}
    end.

verified_collect(Id, Pid, Size, Seen, Acc, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {http, {Id, stream, Bytes}} when Seen + byte_size(Bytes) =< Size ->
            httpc:stream_next(Pid),
            verified_collect(Id, Pid, Size, Seen + byte_size(Bytes), [Bytes | Acc], Deadline);
        {http, {Id, stream_end, _}} when Seen =:= Size ->
            {ok, iolist_to_binary(lists:reverse(Acc))};
        {http, {Id, _}} ->
            {error, upstream}
    after Remaining -> {error, timeout}
    end.

verified_reply(Req, Status, Start, Length, Size, Bytes) ->
    H0 = #{
        <<"content-type">> => <<"audio/aac">>,
        <<"accept-ranges">> => <<"bytes">>,
        <<"content-length">> => integer_to_binary(Length)
    },
    H =
        case Status of
            206 ->
                H0#{
                    <<"content-range">> =>
                        <<"bytes ", (integer_to_binary(Start))/binary, "-",
                            (integer_to_binary(Start + Length - 1))/binary, "/",
                            (integer_to_binary(Size))/binary>>
                };
            _ ->
                H0
        end,
    Headers = maps:merge(private_headers(), H),
    Reply = cowboy_req:stream_reply(Status, Headers, Req),
    case cowboy_req:method(Req) of
        <<"HEAD">> ->
            Reply;
        _ ->
            Owner = self(),
            Watch = spawn(fun() -> send_deadline(Owner) end),
            try
                send_verified(Reply, Bytes, Start, Length),
                Reply
            after
                Watch ! stop
            end
    end.

%% stream_body can wait forever for a stalled connection's data_ack.
send_deadline(Owner) ->
    Monitor = erlang:monitor(process, Owner),
    receive
        stop ->
            erlang:demonitor(Monitor, [flush]),
            ok;
        {'DOWN', Monitor, process, Owner, _} ->
            ok
    after 30000 -> exit(Owner, kill)
    end.

%% Copy each bounded block: Cowboy must not retain the complete verified binary.
send_verified(Req, _Bytes, _Offset, 0) ->
    cowboy_req:stream_body(<<>>, fin, Req);
send_verified(Req, Bytes, Offset, Remaining) ->
    N = min(65536, Remaining),
    Chunk = binary:copy(binary:part(Bytes, Offset, N)),
    ok = cowboy_req:stream_body(Chunk, nofin, Req),
    send_verified(Req, Bytes, Offset + N, Remaining - N).

normalize_range(undefined) ->
    [];
normalize_range(Value) when byte_size(Value) =< 100 ->
    case re:run(Value, <<"^bytes=([0-9]+-[0-9]*|-[0-9]+)$">>, [{capture, none}]) of
        match -> [{"range", binary_to_list(Value)}];
        _ -> invalid
    end;
normalize_range(_) ->
    invalid.

start_stream(Req, Id, Filename) ->
    receive
        {http, {Id, stream_start, Headers, Pid}} ->
            BaseHeaders = maps:merge(private_headers(), response_headers(Headers)),
            Out =
                case Filename of
                    undefined ->
                        BaseHeaders;
                    _ ->
                        %% V3-18 P3/EXC-2：附件下载契约（文件名已白名单化）。
                        BaseHeaders#{
                            <<"content-disposition">> =>
                                <<"attachment; filename=\"", Filename/binary, "\"">>
                        }
                end,
            Code =
                case maps:is_key(<<"content-range">>, Out) of
                    true -> 206;
                    false -> 200
                end,
            case cowboy_req:method(Req) of
                <<"HEAD">> ->
                    cowboy_req:stream_reply(Code, Out, Req);
                _ ->
                    Req1 = cowboy_req:stream_reply(Code, Out, Req),
                    httpc:stream_next(Pid),
                    stream(Req1, Id, Pid)
            end;
        {http, {Id, {{_, 404, _}, _, _}}} ->
            reply(Req, 404, #{}, <<>>);
        {http, {Id, {{_, 416, _}, Headers, _}}} ->
            reply(Req, 416, maps:remove(<<"content-length">>, response_headers(Headers)), <<>>);
        {http, {Id, _}} ->
            reply(Req, 502, #{}, <<>>)
    after 30000 -> reply(Req, 504, #{}, <<>>)
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
            exit(edge_stream_failed)
    after 30000 -> exit(edge_stream_timeout)
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
        <<"content-security-policy">> => <<"sandbox">>,
        %% V3-17：业务端点 302 到边缘后，浏览器 fetch 跨域跟随重定向需要
        %% 边缘响应带 CORS 头。票据走 URL 查询串、请求无凭证（credentials
        %% omit），通配 ACAO 不放大授权面（鉴权仍逐请求回调 authorize）。
        <<"access-control-allow-origin">> => <<"*">>
    }.

reply(Req, Code, ExtraHeaders, Body) ->
    cowboy_req:reply(Code, maps:merge(private_headers(), ExtraHeaders), Body, Req).

hidden_deny(Req) ->
    %% 无权限与不存在统一隐藏 404（不泄露对象存在性）。
    reply(Req, 404, #{}, <<>>).
