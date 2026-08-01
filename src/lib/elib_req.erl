-module(elib_req).

%%% @doc HTTP 请求处理模块
%%% 提供 GET/POST 请求、参数解析、IP 获取等功能

-export([peer_ip/1]).
-export([get_client_ip/1]).
-export([ip_in_allowlist/2]).
-export([cookie/2]).
-export([get/1, get/2]).
-export([post/2, post/3]).
-export([body/2]).
-export([post_params/1]).
-export([parse_urlencoded_body/1, parse_key_value_pairs/1, parse_json_body/1, add_value/3]).

-include("log.hrl").

-define(ReqHeaders, [
    {"content-type", "application/json"},
    {"client", "imboy-req"}
]).

-spec get(binary() | list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
get(Url) ->
    req(get, Url, #{}, ?ReqHeaders).

-spec get(binary() | list(), list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
get(Url, Headers) ->
    req(get, Url, #{}, Headers).

-spec post(binary() | list(), map() | list()) ->
    {ok, map()} | {error, any()} | {error, integer(), map()}.
post(Url, Params) ->
    req(post, Url, Params, ?ReqHeaders).

-spec post(binary() | list(), map() | list(), list()) ->
    {ok, map()} | {error, any()} | {error, integer(), map()}.
post(Url, Params, Headers) ->
    req(post, Url, Params, Headers).

-spec body(cowboy_req:req(), list()) -> {ok, map(), cowboy_req:req()}.
body(Req0, _Opts) ->
    case read_full_body(Req0, <<>>) of
        {ok, <<>>, Req1} ->
            {ok, #{}, Req1};
        {ok, BodyBin, Req1} ->
            ContentType = cowboy_req:header(<<"content-type">>, Req1, <<>>),
            case parse_body_by_content_type(BodyBin, ContentType) of
                {ok, Params} when is_map(Params) ->
                    {ok, Params, Req1};
                _ ->
                    {ok, #{}, Req1}
            end
    end.

-spec post_params(cowboy_req:req()) -> map().
post_params(Req0) ->
    case body(Req0, []) of
        {ok, Params, _Req1} ->
            Params
    end.

-spec read_full_body(cowboy_req:req(), binary()) -> {ok, binary(), cowboy_req:req()}.
read_full_body(Req0, Acc0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req1} ->
            {ok, <<Acc0/binary, (ensure_binary(Data))/binary>>, Req1};
        {more, Data, Req1} ->
            read_full_body(Req1, <<Acc0/binary, (ensure_binary(Data))/binary>>)
    end.

-spec parse_body_by_content_type(binary(), binary()) -> {ok, map()} | {error, atom()}.
parse_body_by_content_type(Body, ContentType) ->
    case
        {
            has_content_type(ContentType, <<"application/x-www-form-urlencoded">>),
            has_content_type(ContentType, <<"multipart/form-data">>)
        }
    of
        {true, _} ->
            parse_urlencoded_body(Body);
        {false, true} ->
            {ok, #{}};
        {false, false} ->
            case parse_json_body(Body) of
                {ok, Map} when is_map(Map) ->
                    {ok, Map};
                {ok, _Other} ->
                    {ok, #{}};
                {error, _} ->
                    parse_key_value_pairs(Body)
            end
    end.

-spec parse_urlencoded_body(binary()) -> {ok, map()} | {error, atom()}.
parse_urlencoded_body(<<>>) ->
    {ok, #{}};
parse_urlencoded_body(Body) when is_binary(Body) ->
    parse_query_pairs(binary:split(Body, <<"&">>, [global])).

-spec parse_key_value_pairs(binary()) -> {ok, map()} | {error, atom()}.
parse_key_value_pairs(<<>>) ->
    {ok, #{}};
parse_key_value_pairs(Body) when is_binary(Body) ->
    parse_query_pairs(binary:split(Body, <<"&">>, [global])).

-spec parse_json_body(binary()) -> {ok, term()} | {error, atom()}.
parse_json_body(<<>>) ->
    {error, empty_body};
parse_json_body(Body) when is_binary(Body) ->
    try
        {ok, jsone:decode(Body, [{object_format, map}])}
    catch
        _:_ ->
            {error, invalid_json}
    end.

-spec parse_query_pairs([binary()]) -> {ok, map()} | {error, atom()}.
parse_query_pairs(Pairs) ->
    try
        Params = lists:foldl(fun parse_query_pair/2, #{}, Pairs),
        {ok, Params}
    catch
        _:_ ->
            {error, parse_failed}
    end.

parse_query_pair(<<>>, Acc) ->
    Acc;
parse_query_pair(Pair, Acc) ->
    {RawKey, RawValue} = split_pair(Pair),
    Key = decode_query_component(RawKey),
    Value = decode_query_component(RawValue),
    add_value(Key, Value, Acc).

-spec split_pair(binary()) -> {binary(), binary()}.
split_pair(Pair) ->
    case binary:match(Pair, <<"=">>) of
        {Pos, 1} ->
            <<Key:Pos/binary, $=, Value/binary>> = Pair,
            {Key, Value};
        nomatch ->
            {Pair, <<>>}
    end.

-spec decode_query_component(binary()) -> binary().
decode_query_component(Value) when is_binary(Value) ->
    Value1 = binary:replace(Value, <<"+">>, <<" ">>, [global]),
    try
        unicode:characters_to_binary(uri_string:unquote(binary_to_list(Value1)))
    catch
        _:_ ->
            Value1
    end.

-spec add_value(binary(), binary(), map()) -> map().
add_value(Key, Value, Acc) ->
    case maps:get(Key, Acc, undefined) of
        undefined ->
            maps:put(Key, Value, Acc);
        Existing when is_list(Existing) ->
            maps:put(Key, Existing ++ [Value], Acc);
        Existing ->
            maps:put(Key, [Existing, Value], Acc)
    end.

-spec cookie(binary(), cowboy_req:req()) -> binary() | false.
cookie(Key, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    maps:get(Key, maps:from_list(Cookies), false).

-spec peer_ip(map()) -> binary().
peer_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    list_to_binary(inet:ntoa(IP)).

%% @doc 获取可信任的真实客户端 IP（全站唯一实现）。
%%
%% 只有当**直连对端**本身在受信代理白名单内时，才采信 x-forwarded-for；
%% 否则一律用直连 IP。
%%
%% 此前的实现无条件采信 XFF 首段，而本函数是 throttle_middleware 的
%% passport_per_ip(10/min) 与 api_per_ip(60/min) 两个限流桶的 key 来源：
%% 每个请求带一个随机 `X-Forwarded-For: 1.2.3.x` 就能让桶 key 每次都新，
%% 全站 IP 维度限流 100% 失效 —— 登录爆破、验证码轰炸、注册刷量的防护
%% 一起归零。这个洞同时是验证码爆破链的放大器。
%%
%% 本实现原为 passport_handler:get_real_ip/1（写法是对的，只是没被复用），
%% 现提升到 lib 层作为唯一真源，passport_handler 改为委托调用。
%%
%% 默认白名单 [127.0.0.1, ::1] 与 deploy/nginx 的 proxy_pass
%% http://127.0.0.1:9800 一致；多层代理/云 LB 需在 trusted_proxy_ips
%% 中显式列出各跳的出口 IP，并把 trusted_proxy_hops 设为实际代理层数
%% （默认 1 = 单层 nginx）。
-spec get_client_ip(cowboy_req:req()) -> binary().
get_client_ip(Req) ->
    {PeerIp, _Port} = cowboy_req:peer(Req),
    PeerIpBin = ec_cnv:to_binary(inet:ntoa(PeerIp)),
    TrustedProxies = config_ds:env(trusted_proxy_ips, [<<"127.0.0.1">>, <<"::1">>]),
    case lists:member(PeerIpBin, TrustedProxies) of
        true ->
            forwarded_ip_at_hop(
                cowboy_req:header(<<"x-forwarded-for">>, Req, PeerIpBin),
                PeerIpBin,
                config_ds:env(trusted_proxy_hops, 1)
            );
        false ->
            PeerIpBin
    end.

%% @private 从 XFF **右**数第 Hops 段。
%%
%% 取最左段是错的：deploy/nginx/templates/imboy.conf.template 用
%% `proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for`，其语义是
%% 「客户端自带的 XFF 原样留在左边 ++ 本跳看到的 remote_addr 追加到右边」。
%% 因此最左段 100% 由攻击者控制：每个请求带一个随机 `X-Forwarded-For: 1.2.3.x`
%% 就能让限流桶 key 每次都新，IP 维度限流全站失效。
%% 只有**代理自己追加的那些右侧段**不可伪造。
%%
%% 用固定跳数而不是"丢弃所有受信 IP"：后者可被 `XFF: evil, 127.0.0.1` 这类
%% 受信 IP 填充绕过（贪心丢完就把 evil 当成了客户端）。
%% Hops = 请求到达前经过的代理层数：单 nginx（默认部署）= 1 → 取最后一段；
%% 云 LB → nginx = 2 → 取倒数第二段。段数不足时回退直连 IP（不可伪造）。
%% 空头 / 全分隔符同样回退 —— 原实现用 hd/1，空值头会 badarg 崩掉请求。
-spec forwarded_ip_at_hop(binary(), binary(), integer()) -> binary().
forwarded_ip_at_hop(ForwardedFor, Fallback, Hops) when
    is_binary(ForwardedFor), is_integer(Hops), Hops >= 1
->
    Segments = binary:split(ForwardedFor, [<<",">>, <<" ">>], [trim_all, global]),
    case length(Segments) >= Hops of
        true -> lists:nth(Hops, lists:reverse(Segments));
        false -> Fallback
    end;
forwarded_ip_at_hop(_, Fallback, _) ->
    Fallback.

%% @doc 判断客户端 IP 是否命中白名单（全站唯一实现）。
%%
%% 支持精确匹配（`"192.168.1.1"'）与前缀匹配（`"10.0.0."' 命中 `"10.0.0.1"'）——
%% 精确匹配是前缀匹配的特例，无需分开判断。条目可为 binary 或 string。
%%
%% **空白名单一律返回 false**：`"未配置 = 不启用"' 是调用方的策略，不在此兜底。
%% 若在此把空列表当"全部放行"，调用方漏判空时就会把"没配置"静默变成"全放行"。
%%
%% 空串条目同样不命中 —— 空前缀会匹配任意 IP，配置里多一个 `""' 就等于关掉整道门。
-spec ip_in_allowlist(binary() | undefined, [binary() | string() | term()]) -> boolean().
ip_in_allowlist(Ip, Allowlist) when is_binary(Ip), is_list(Allowlist) ->
    lists:any(
        fun(Entry) ->
            case allowlist_entry(Entry) of
                <<>> -> false;
                Prefix -> binary:longest_common_prefix([Ip, Prefix]) =:= byte_size(Prefix)
            end
        end,
        Allowlist
    );
ip_in_allowlist(_Ip, _Allowlist) ->
    false.

%% @private 白名单条目归一化；非字符串条目归一为 <<>> 从而永不命中
-spec allowlist_entry(term()) -> binary().
allowlist_entry(Entry) when is_binary(Entry) ->
    Entry;
allowlist_entry(Entry) when is_list(Entry) ->
    case unicode:characters_to_binary(Entry) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<>>
    end;
allowlist_entry(_Entry) ->
    <<>>.

-spec req(atom(), binary() | list(), map() | list(), list()) ->
    {ok, map()} | {error, any()} | {error, integer(), map()}.
req(Method, Url, Params, Headers) ->
    _ = application:ensure_started(ssl),
    _ = application:ensure_started(inets),
    ContentType = maps:get("content-type", maps:from_list(Headers), "application/json"),
    Request =
        case Method of
            post ->
                Bin = jsone:encode(Params, [native_utf8]),
                {Url, Headers, ContentType, Bin};
            get ->
                {Url, Headers}
        end,
    Response = httpc:request(Method, Request, [], []),
    ok = ?DEBUG_LOG([response, Response]),
    case Response of
        {ok, {{_, 200, _}, _RespHeaders, Body}} ->
            {ok, decode_response_body(Body)};
        {ok, {{_, StatusCode, _}, _RespHeaders, Body}} ->
            {error, StatusCode, decode_response_body(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec has_content_type(binary(), binary()) -> boolean().
has_content_type(ContentType, Expected) when is_binary(ContentType) ->
    binary:match(ContentType, Expected) =/= nomatch.

-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_list(Value) ->
    iolist_to_binary(Value);
ensure_binary(Value) ->
    ec_cnv:to_binary(Value).

-spec decode_response_body(term()) -> map().
decode_response_body(Body) ->
    case parse_json_body(ensure_binary(Body)) of
        {ok, Payload} when is_map(Payload) ->
            Payload;
        {ok, Payload} ->
            #{<<"data">> => Payload};
        {error, _} ->
            #{}
    end.
