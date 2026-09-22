-module(bot_webhook_guard).

%%%
% 出站 Webhook SSRF 防护与 IP Pinning（WH-01，PDT-01 webhook 契约 §2.5）。
%
% 规则（fail-closed）：
%   - 仅 HTTPS；loopback 的 http 仅在 test/local profile 显式开放（E2E fixture 用）；
%   - 解析域名得到全部 IP：任一命中 私网/loopback/link-local/保留段 → 拒绝
%     （fail-closed：混合解析全拒，防 DNS rebinding 绕过）；
%   - 选定 pinned IP 返回给 sender：连接只连 pinned IP，TLS SNI/HTTP Host 仍用
%     原域名（证书校验按域名）；
%   - redirect V1 不跟随（上层把 3xx 判为失败）。
%%%

-export([validate_and_pin/1, validate_pinned/2, is_private_ip/1]).

-include("log.hrl").

%% @doc 校验并解析出站目标。
%% 返回 {ok, #{ip => tuple(), host => binary(), port => pos_integer(), path => binary()}}
%% | {error, Reason}。
-spec validate_and_pin(binary()) ->
    {ok, map()} | {error, invalid_scheme | invalid_url | forbidden_host | dns_failure}.
validate_and_pin(Url) when is_binary(Url), Url =/= <<>> ->
    case endpoint_shape(Url) of
        {ok, Endpoint, AllowLoopback} ->
            case resolve_pin(maps:get(host, Endpoint), AllowLoopback) of
                {ok, Pin} -> {ok, Endpoint#{ip => Pin}};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end;
validate_and_pin(_) ->
    {error, invalid_url}.

%% @doc 从 outbox 的不可变 URL/IP 快照恢复目标，不执行 DNS。
-spec validate_pinned(binary(), binary()) ->
    {ok, map()} | {error, invalid_scheme | invalid_url | invalid_pin | forbidden_host}.
validate_pinned(Url, PinnedIP) when is_binary(PinnedIP), PinnedIP =/= <<>> ->
    case endpoint_shape(Url) of
        {ok, Endpoint, AllowLoopback} ->
            case inet:parse_address(binary_to_list(PinnedIP)) of
                {ok, IP} -> validate_stored_pin(Endpoint, IP, AllowLoopback);
                {error, _} -> {error, invalid_pin}
            end;
        {error, _} = Error ->
            Error
    end;
validate_pinned(_, _) ->
    {error, invalid_pin}.

endpoint_shape(Url) ->
    try uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Map ->
            endpoint_scheme(Map, ec_cnv:to_binary(Scheme), ec_cnv:to_binary(Host));
        _ ->
            {error, invalid_url}
    catch
        _:_ -> {error, invalid_url}
    end.

endpoint_scheme(Map, <<"https">>, Host) when Host =/= <<>> ->
    make_endpoint(Map, Host, 443, true, false);
endpoint_scheme(Map, <<"http">>, Host) when Host =/= <<>> ->
    AllowLoopback = loopback_allowed() andalso is_loopback_host(Host),
    case AllowLoopback of
        true -> make_endpoint(Map, Host, 80, false, true);
        false -> {error, invalid_scheme}
    end;
endpoint_scheme(_, _, _) ->
    {error, invalid_scheme}.

make_endpoint(Map, HostB, DefPort, IsTls, AllowLoopback) ->
    HostB = ec_cnv:to_binary(maps:get(host, Map)),
    Port = maps:get(port, Map, DefPort),
    Path =
        case maps:find(path, Map) of
            {ok, P} when P =/= <<>> -> ec_cnv:to_binary(P);
            _ -> <<"/">>
        end,
    QS =
        case maps:find(query, Map) of
            {ok, Q} when Q =/= <<>> -> [<<"?">>, ec_cnv:to_binary(Q)];
            _ -> []
        end,
    case is_integer(Port) andalso Port > 0 andalso Port =< 65535 of
        true ->
            {ok,
                #{
                    host => HostB,
                    port => Port,
                    tls => IsTls,
                    path => iolist_to_binary([Path, QS])
                },
                AllowLoopback};
        false ->
            {error, invalid_url}
    end.

validate_stored_pin(Endpoint, IP, AllowLoopback) ->
    case is_private_ip(IP) andalso not (AllowLoopback andalso is_loopback_ip(IP)) of
        true -> {error, forbidden_host};
        false -> {ok, Endpoint#{ip => IP}}
    end.

%% 解析并全量校验：任一 IP 落禁区即整体拒绝（防 rebinding 混合解析）。
resolve_pin(Host, AllowLoopback) ->
    case inet:getaddrs(binary_to_list(Host), inet) of
        {ok, IPs} when is_list(IPs), IPs =/= [] ->
            Bad = [
                IP
             || IP <- IPs,
                is_private_ip(IP),
                not (AllowLoopback andalso is_loopback_ip(IP))
            ],
            case Bad of
                [] ->
                    {ok, hd(IPs)};
                _ ->
                    ?WARN_LOG(
                        "[WH01] forbidden host ~ts resolved into ~p banned "
                        "ips~n",
                        [Host, length(Bad)]
                    ),
                    {error, forbidden_host}
            end;
        _ ->
            {error, dns_failure}
    end.

is_loopback_host(Host) ->
    lists:member(Host, [<<"127.0.0.1">>, <<"localhost">>, <<"::1">>]).

is_loopback_ip({127, _, _, _}) -> true;
is_loopback_ip({0, 0, 0, 0, 0, 0, 0, 1}) -> true;
is_loopback_ip(_) -> false.

%% loopback 仅 test/local profile 显式开放（E2E fixture）
loopback_allowed() ->
    case imboy_env:current() of
        <<"test">> -> true;
        <<"local">> -> true;
        <<"dev">> -> true;
        _ -> false
    end.

%% @doc 私网/保留段判定。V1 出站只解析 IPv4；IPv6 在完整分类与发送链
%% 支持前统一 fail closed，避免把未覆盖的 ULA/link-local/组播地址当公网。
is_private_ip({A, _, _, _}) when A =:= 0; A =:= 10; A =:= 127 -> true;
is_private_ip({172, O2, _, _}) when O2 >= 16, O2 =< 31 -> true;
is_private_ip({192, 168, _, _}) -> true;
is_private_ip({169, 254, _, _}) -> true;
%% CGNAT
is_private_ip({100, B, _, _}) when B >= 64, B =< 127 -> true;
is_private_ip({192, 0, 0, _}) -> true;
%% 6to4 relay anycast 192.88.99.0/24（FULL-03 补：既是保留段也是中继跳板）
is_private_ip({192, 88, 99, _}) -> true;
%% IANA documentation ranges
is_private_ip({192, 0, 2, _}) -> true;
is_private_ip({198, 51, 100, _}) -> true;
is_private_ip({203, 0, 113, _}) -> true;
%% benchmark 198.18.0.0/15
is_private_ip({198, B, _, _}) when B =:= 18; B =:= 19 -> true;
%% multicast and reserved 224.0.0.0/4
is_private_ip({A, _, _, _}) when A >= 224 -> true;
is_private_ip({_, _, _, _, _, _, _, _}) -> true;
is_private_ip(_) -> false.
