-module(bot_webhook_delivery_sender).

%%%
% WH-01 出站投递 sender：连接 pinned IP（防 DNS rebinding），TLS SNI/Host 用原域名，
% 证书 verify_peer + 系统 CA 池（OTP public_key:cacerts_get/0）；
% http 明文仅 test/local profile 的 loopback fixture 可用（guard 已限制）。
% 手写 HTTP/1.1 POST（content-length 定长），返回 {ok, StatusCode} | {error, Reason}；
% 不解析/不保存响应正文。
%%%

-export([post/7]).

-define(CONNECT_TIMEOUT_MS, 8000).
-define(RESP_TIMEOUT_MS, 8000).

%% @doc 发送 POST。IP/Port/PathQS/Host 来自 guard 的 pin 结果。
-spec post(
    inet:ip_address(),
    inet:port_number(),
    boolean(),
    binary(),
    binary(),
    [{binary(), binary()}],
    binary()
) ->
    {ok, pos_integer()} | {error, term()}.
post(IP, Port, IsTls, PathQS, Host, Headers, Body) ->
    case
        gen_tcp:connect(
            IP,
            Port,
            [binary, {active, false}, {nodelay, true}],
            ?CONNECT_TIMEOUT_MS
        )
    of
        {ok, Sock} when IsTls ->
            case ssl:connect(Sock, ssl_opts(Host), ?CONNECT_TIMEOUT_MS) of
                {ok, TlsSock} ->
                    R = request(TlsSock, PathQS, Host, Headers, Body),
                    try
                        ssl:close(TlsSock)
                    catch
                        _:_ -> ok
                    end,
                    R;
                {error, Reason} ->
                    try
                        gen_tcp:close(Sock)
                    catch
                        _:_ -> ok
                    end,
                    {error, {tls_connect, Reason}}
            end;
        {ok, Sock} ->
            R = request(Sock, PathQS, Host, Headers, Body),
            try
                gen_tcp:close(Sock)
            catch
                _:_ -> ok
            end,
            R;
        {error, Reason} ->
            {error, {connect, Reason}}
    end.

request(Sock, PathQS, Host, ExtraHeaders, Body) ->
    Hdrs = [
        {<<"host">>, Host},
        {<<"user-agent">>, <<"IMBoy-Webhook/1.0">>},
        {<<"content-type">>, <<"application/json">>},
        {<<"content-length">>, integer_to_binary(byte_size(Body))},
        {<<"connection">>, <<"close">>}
        | ExtraHeaders
    ],
    Req = iolist_to_binary(
        [
            <<"POST ">>,
            PathQS,
            <<" HTTP/1.1\r\n">>,
            [[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- Hdrs],
            <<"\r\n">>,
            Body
        ]
    ),
    case gen_tcp:send(Sock, Req) of
        ok -> read_status(Sock);
        {error, Reason} -> {error, {send, Reason}}
    end.

send_all(Sock, Bin) ->
    case gen_tcp:send(Sock, Bin) of
        ok -> ok;
        {error, Reason} -> {error, {send, Reason}}
    end.

%% 读状态行 + 头，取 status code（正文按 connection: close 由 close 兜底，不读）。
read_status(Sock) ->
    case read_head(Sock, <<>>, ?RESP_TIMEOUT_MS) of
        {ok, HeadBin} ->
            case
                re:run(
                    HeadBin,
                    <<"HTTP/[0-9.]+ ([0-9]{3})">>,
                    [{capture, [1], binary}]
                )
            of
                {match, [CodeBin]} ->
                    {ok, binary_to_integer(CodeBin)};
                _ ->
                    {error, bad_status_line}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

read_head(Sock, Acc, Timeout) ->
    case gen_tcp:recv(Sock, 0, Timeout) of
        {ok, Data} ->
            Acc2 = <<Acc/binary, Data/binary>>,
            case binary:match(Acc2, <<"\r\n\r\n">>) of
                nomatch when byte_size(Acc2) < 65536 -> read_head(Sock, Acc2, Timeout);
                nomatch -> {error, head_too_large};
                _ -> {ok, Acc2}
            end;
        {error, Reason} ->
            {error, {recv, Reason}}
    end.

ssl_opts(Host) ->
    Hostname = binary_to_list(Host),
    Cacerts =
        try public_key:cacerts_get() of
            Cs when is_list(Cs) -> Cs;
            _ -> []
        catch
            _:_ -> []
        end,
    [
        {server_name_indication, Hostname},
        {verify, verify_peer},
        {cacerts, Cacerts},
        {depth, 4}
    ].
