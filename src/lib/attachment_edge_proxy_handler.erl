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

handle(Req, {AuthorizeUrl, Token}) ->
    case cowboy_req:method(Req) of
        M when M =:= <<"GET">>; M =:= <<"HEAD">> ->
            Query = cowboy_req:parse_qs(Req),
            Ticket = proplists:get_value(<<"ticket">>, Query, <<>>),
            Range = cowboy_req:header(<<"range">>, Req),
            case authorize(AuthorizeUrl, Token, Ticket, M) of
                {ok, UpstreamUrl} -> proxy(Req, UpstreamUrl, Range, undefined);
                {ok, UpstreamUrl, Filename} -> proxy(Req, UpstreamUrl, Range, Filename);
                deny -> hidden_deny(Req)
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

normalize_range(undefined) ->
    [];
normalize_range(Value) when byte_size(Value) =< 100 ->
    case re:run(Value, <<"^bytes=([0-9]+-[0-9]*|-[0-9]+)$">>, [{capture, none}]) of
        match -> [{"range", binary_to_list(Value)}];
        _ -> invalid
    end;
normalize_range(_) ->
    invalid.

start_stream(Req, Id) ->
    start_stream(Req, Id, undefined).

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
