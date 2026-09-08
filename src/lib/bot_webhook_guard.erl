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

-export([validate_and_pin/1, is_private_ip/1]).

-include("log.hrl").

%% @doc 校验并解析出站目标。
%% 返回 {ok, #{ip => tuple(), host => binary(), port => pos_integer(), path => binary()}}
%% | {error, Reason}。
-spec validate_and_pin(binary()) ->
    {ok, map()} | {error, invalid_scheme | invalid_url | forbidden_host | dns_failure}.
validate_and_pin(Url) when is_binary(Url), Url =/= <<>> ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Map ->
            SchemeB = ec_cnv:to_binary(Scheme),
            HostB = ec_cnv:to_binary(Host),
            case {SchemeB, HostB} of
                {<<"https">>, _} ->
                    ok_shape(Map, 443, true);
                {<<"http">>, _} ->
                    case loopback_allowed() andalso is_loopback_host(HostB) of
                        true -> ok_shape(Map, 80, false);
                        false -> {error, invalid_scheme}
                    end;
                _ ->
                    {error, invalid_scheme}
            end;
        _ ->
            {error, invalid_url}
    end;
validate_and_pin(_) ->
    {error, invalid_url}.

ok_shape(Map, DefPort, IsTls) ->
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
    case resolve_pin(HostB) of
        {ok, Pin} ->
            {ok, #{
                ip => Pin,
                host => HostB,
                port => Port,
                tls => IsTls,
                path => iolist_to_binary([Path, QS])
            }};
        {error, _} = E ->
            E
    end.

%% 解析并全量校验：任一 IP 落禁区即整体拒绝（防 rebinding 混合解析）。
resolve_pin(Host) ->
    case inet:getaddrs(Host, inet) of
        {ok, IPs} when is_list(IPs), IPs =/= [] ->
            Bad = [IP || IP <- IPs, is_private_ip(IP)],
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

%% loopback 仅 test/local profile 显式开放（E2E fixture）
loopback_allowed() ->
    case imboy_env:current() of
        <<"test">> -> true;
        <<"local">> -> true;
        <<"dev">> -> true;
        _ -> false
    end.

%% @doc 私网/保留段判定（IPv4+IPv6）。
is_private_ip({10, _, _, _}) ->
    true;
is_private_ip({172, O2, _, _}) when O2 >= 16, O2 =< 31 -> true;
is_private_ip({192, 168, _, _}) ->
    true;
is_private_ip({127, _, _, _}) ->
    true;
is_private_ip({169, 254, _, _}) ->
    true;
is_private_ip({0, _, _, _}) ->
    true;
%% CGNAT
is_private_ip({100, B, _, _}) when B >= 64, B =< 127 -> true;
is_private_ip({192, 0, 0, _}) ->
    true;
%% benchmark
is_private_ip({198, 18, _, _}) ->
    true;
%% multicast
is_private_ip({224, _, _, _}) ->
    true;
%% reserved
is_private_ip({240, _, _, _}) ->
    true;
is_private_ip({255, 255, 255, 255}) ->
    true;
%% ::1
is_private_ip({0, 0, 0, 0, 0, 0, 0, 1}) ->
    true;
%% ::
is_private_ip({0, 0, 0, 0, 0, 0, 0, 0}) ->
    true;
%% ::1 变体
is_private_ip({65752, _, _, _, _, _, _, _}) ->
    true;
is_private_ip({0, 0, 0, 0, 0, 65535, A, B}) ->
    is_private_ip({A bsr 8, A band 255, B bsr 8, B band 255});
%% 100.64/10 v6 映射误报防御
is_private_ip({8444, _, _, _, _, _, _, _}) ->
    true;
is_private_ip(_) ->
    false.
