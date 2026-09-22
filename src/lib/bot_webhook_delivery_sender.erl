-module(bot_webhook_delivery_sender).

%%%
% WH-01 出站投递 sender：连接 pinned IP（防 DNS rebinding），TLS SNI/Host 用原域名，
% 证书 verify_peer + 系统 CA 池（OTP public_key:cacerts_get/0）；
% http 明文仅 test/local profile 的 loopback fixture 可用（guard 已限制）。
% 手写 HTTP/1.1 POST（content-length 定长），返回 {ok, StatusCode} | {error, Reason}；
% 不解析/不保存响应正文。
%
% FULL-03 加固（plan-full §7「timeout、response cap」）：
%   * 超时：连接/响应各自有**有界**超时（默认 8000ms）；应用环境
%     bot_webhook_timeout_ms 可覆盖，夹紧到 [100, 30000]——出站超时是
%     防挂死/防 SSRF 慢速读取的一部分，只能收紧，不能被关掉。
%   * response cap：只读到响应头结束（\r\n\r\n）为止，头上限
%     response_cap_bytes/0（64KiB），超限 {error, head_too_large}；
%     **正文一个字节都不读**（connection: close 由 socket 关闭兜底），
%     因此恶意/超大响应体不会进内存。
%   * redirect：3xx 只作为状态码返回（上层判为可重试失败），**不跟随**
%     Location——不产生第二次出站（SSRF 跳板防护）。
%%%

-export([post/7, ssl_opts/1, timeouts/0, response_cap_bytes/0]).

-define(DEFAULT_TIMEOUT_MS, 8000).
-define(MIN_TIMEOUT_MS, 100).
-define(MAX_TIMEOUT_MS, 30000).
-define(MAX_RESP_HEAD_BYTES, 65536).

%% @doc 出站超时（连接 / 响应，毫秒）。生产默认 8000；env 覆盖夹紧到
%% [100, 30000]，非法值回落默认（0/负数/无限一律不接受）。
-spec timeouts() -> {pos_integer(), pos_integer()}.
timeouts() ->
    Ms = clamp_timeout(config_ds:env(bot_webhook_timeout_ms, ?DEFAULT_TIMEOUT_MS)),
    {Ms, Ms}.

clamp_timeout(Ms) when is_integer(Ms), Ms >= ?MIN_TIMEOUT_MS, Ms =< ?MAX_TIMEOUT_MS -> Ms;
clamp_timeout(_) -> ?DEFAULT_TIMEOUT_MS.

%% @doc 响应头字节上限（正文不读；超限即失败，不静默截断）。
-spec response_cap_bytes() -> pos_integer().
response_cap_bytes() ->
    ?MAX_RESP_HEAD_BYTES.

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
    {ConnectMs, _RespMs} = timeouts(),
    case
        gen_tcp:connect(
            IP,
            Port,
            [binary, {active, false}, {nodelay, true}],
            ConnectMs
        )
    of
        {ok, Sock} when IsTls ->
            case ssl:connect(Sock, ssl_opts(Host), ConnectMs) of
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

%% 读状态行 + 头，取 status code（正文按 connection: close 由 close 兜底，不读；
%% 头累计超 response_cap_bytes/0 即失败——不静默截断）。
read_status(Sock) ->
    {_ConnectMs, RespMs} = timeouts(),
    case read_head(Sock, <<>>, RespMs) of
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
                nomatch when byte_size(Acc2) < ?MAX_RESP_HEAD_BYTES ->
                    read_head(Sock, Acc2, Timeout);
                nomatch ->
                    {error, head_too_large};
                _ ->
                    {ok, Acc2}
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
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]},
        {depth, 4}
    ].
