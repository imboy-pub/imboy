%%% @doc Widget 接入的领域纯函数（CSB-02；application `cs_widget_app` 的判定真源）。
%%%
%%% 纯净性（铁律 4）：无 I/O、无进程、无隐式时间/随机源；时钟（`Now`）、HMAC
%%% 密钥材料（`Key`）一律由调用方显式传入（服务端事实，绝不来自浏览器申报）。
%%% 所有函数可零 mock 单测。
%%%
%%% 冻结的判定：
%%%   * Origin 判定全在服务端：申报 Origin 与 allowlist 双方都做
%%%     scheme+host+port 归一（同源 sibling：缺省端口 80/443 折叠、host 小写、
%%%     scheme 大小写折叠），然后**精确**匹配——无前缀/后缀/子域通融；
%%%     v1.1 追加同源对等（`origin_allowed/3`，GAP-4 裁决）：放行集合 =
%%%     allowlist ∪ 与请求 Host 头 scheme+host:port 归一相同，放行来源可区分；
%%%   * 匿名 subject 只以 HMAC 形态存在：`subject_hmac/3` 以安装级数据域
%%%     （public_widget_id + 浏览器随机 ID）+ 注入密钥计算 sha256-HMAC hex；
%%%     签名身份 sub 同口径但数据域前缀不同（与匿名域永不相交）；
%%%   * 签名断言 claims 全查：iss/aud/widget_id/sub/exp/iat/jti 任一缺失或
%%%     不符即拒（失败原因唯一可复现）；jti 的一次性消费由 store 的
%%%     nonce 唯一约束裁决（DB 裁决点），本模块只做形状与时间窗判定；
%%%   * branding 响应白名单：installation.branding 原文里的其余键一律不出
%%%     application 层。
-module(cs_widget).

-export([
    normalize_origin/1,
    origin_allowed/2,
    origin_allowed/3,
    subject_hmac/3,
    verified_subject_hmac/3,
    assertion_claims/2,
    branding_view/1
]).

-define(DEFAULT_HTTP_PORT, 80).
-define(DEFAULT_HTTPS_PORT, 443).

%% branding 响应白名单：**逐字**（installation.branding jsonb 里其余键一律
%% 不出 application 层——branding 是租户配置原文，白名单外的键不得进响应）。
-define(BRANDING_KEYS, [
    <<"primary_color">>,
    <<"logo_url">>,
    <<"welcome_text">>,
    <<"display_name">>
]).

%% ===================================================================
%% Origin 归一化与 allowlist 精确匹配
%% ===================================================================

%% @doc 把申报/配置 Origin 归一为 `scheme://host[:port]`（缺省端口折叠、
%% scheme/host 小写）。合同 S4 六禁形状门：scheme 非 http(s)、通配 `*`、
%% userinfo、path/query/fragment、空白/控制字符、非法字符集（host 白名单外
%% 与端口非纯数字）一律 `{error, {invalid_origin, V}}`——fail-closed，不做
%% 容错截断（CSP frame-ancestors 输入链的唯一入口，形状非法值绝不上头）。
-spec normalize_origin(term()) -> {ok, binary()} | {error, term()}.
normalize_origin(V) when is_binary(V) ->
    case binary:split(V, <<"://">>) of
        [Scheme, Rest] when Scheme =/= <<>>, Rest =/= <<>> ->
            normalize_scheme(lower(Scheme), Rest, V);
        _ ->
            {error, {invalid_origin, V}}
    end;
normalize_origin(V) ->
    {error, {invalid_origin, V}}.

%% 六禁之一：scheme 无条件仅 {http,https}——与端口无关（`ftp://h:21`、
%% `javascript://h:80` 同拒）；scheme 段的任何空白/控制/通配形态都进不了
%% 这两个逐字匹配，天然封闭。
normalize_scheme(<<"http">>, Rest, Raw) ->
    reject_forbidden_authority(<<"http">>, Rest, Raw);
normalize_scheme(<<"https">>, Rest, Raw) ->
    reject_forbidden_authority(<<"https">>, Rest, Raw);
normalize_scheme(_Scheme, _Rest, Raw) ->
    {error, {invalid_origin, Raw}}.

%% 六禁（authority 任意位置）：通配 `*`、path/query/fragment 前导 `/`、`?`、
%% `#`、userinfo `@`、空白与控制字符（<0x21 与 0x7F）一律拒——
%% `https://a.com\r\nEvil`（无冒号 CRLF 形态）与 `https://a.com X` 在此拦截。
reject_forbidden_authority(Scheme, Rest, Raw) ->
    case has_forbidden_authority_byte(Rest) of
        true -> {error, {invalid_origin, Raw}};
        false -> normalize_authority(Scheme, Rest, Raw)
    end.

has_forbidden_authority_byte(<<C, _/binary>>) when C < 16#21; C =:= 16#7F -> true;
has_forbidden_authority_byte(<<$*, _/binary>>) -> true;
has_forbidden_authority_byte(<<$/, _/binary>>) -> true;
has_forbidden_authority_byte(<<$?, _/binary>>) -> true;
has_forbidden_authority_byte(<<$#, _/binary>>) -> true;
has_forbidden_authority_byte(<<$@, _/binary>>) -> true;
has_forbidden_authority_byte(<<_, Rest/binary>>) -> has_forbidden_authority_byte(Rest);
has_forbidden_authority_byte(<<>>) -> false.

%% authority 里的 `/` 一定意味着 path（query/fragment 亦然）——Origin 没有这些
%% （前置禁字节扫描已拦，此处保留为结构兜底）。
normalize_authority(Scheme, Rest, Raw) ->
    case binary:match(Rest, <<"/">>) of
        nomatch -> normalize_hostport(Scheme, Rest, Raw);
        _ -> {error, {invalid_origin, Raw}}
    end.

%% userinfo（`user@host`）不是 origin 的形状——拒绝，不做容错剥离。
normalize_hostport(Scheme, Authority, Raw) ->
    case binary:split(Authority, <<"@">>) of
        [_Only] -> split_hostport(Scheme, Authority, Raw);
        _ -> {error, {invalid_origin, Raw}}
    end.

%% host 非空、字符集白名单内；端口缺省折叠（http:80 / https:443），否则
%% 1..65535（纯数字）。
split_hostport(Scheme, Authority, Raw) ->
    case split_authority(Authority) of
        {ok, Host0, DefaultPort} ->
            Host = lower(Host0),
            case valid_host_charset(Host) of
                true ->
                    case port_or_default(DefaultPort, Scheme) of
                        {ok, Port} -> {ok, join_origin(Scheme, Host, Port)};
                        {error, _} -> {error, {invalid_origin, Raw}}
                    end;
                false ->
                    {error, {invalid_origin, Raw}}
            end;
        error ->
            {error, {invalid_origin, Raw}}
    end.

%% 六禁之「非法字符集」：host 白名单——非方括号形态仅 `[a-z0-9._-]`（小写
%% 归一后判定，大小写输入不受影响）；IPv6 方括号形态保留 `[<hex>:.]`
%% （`::1` 与 IPv4-mapped 同口径），空方括号/白名单外一律拒。
valid_host_charset(<<>>) ->
    false;
valid_host_charset(<<$[, Rest/binary>>) ->
    case binary:split(Rest, <<"]">>) of
        [Body, <<>>] when Body =/= <<>> -> ipv6_chars(Body);
        _ -> false
    end;
valid_host_charset(Host) ->
    hostname_chars(Host).

hostname_chars(<<>>) ->
    true;
hostname_chars(<<C, Rest/binary>>) when
    (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse
        C =:= $. orelse C =:= $- orelse C =:= $_
->
    hostname_chars(Rest);
hostname_chars(_) ->
    false.

ipv6_chars(<<>>) ->
    true;
ipv6_chars(<<C, Rest/binary>>) when
    (C >= $a andalso C =< $f) orelse (C >= $0 andalso C =< $9) orelse C =:= $: orelse C =:= $.
->
    ipv6_chars(Rest);
ipv6_chars(_) ->
    false.

%% 拆 host:port；IPv6 字面量按 `[...]:port` 处理（方括号内原样保留）。
split_authority(<<"[", _/binary>> = Authority) ->
    case binary:split(Authority, <<"]">>) of
        [Inside, Rest] ->
            Host = <<Inside/binary, "]">>,
            case Rest of
                <<>> -> {ok, Host, undefined};
                <<":", PortBin/binary>> -> {ok, Host, binary_to_port(PortBin)};
                _ -> error
            end;
        _ ->
            error
    end;
split_authority(Authority) ->
    case binary:split(Authority, <<":">>) of
        [Host] when Host =/= <<>> -> {ok, Host, undefined};
        [Host, PortBin] when Host =/= <<>> -> {ok, Host, binary_to_port(PortBin)};
        _ -> error
    end.

%% 端口仅接受纯数字 1..65535（`+80`、`0x50` 等非法字符集形态同拒；
%% 归一输出恒为规范十进制，坏字符不进 CSP 值）。
binary_to_port(Bin) ->
    case port_digits(Bin) of
        true ->
            try binary_to_integer(Bin) of
                N when N >= 1, N =< 65535 -> N;
                _ -> invalid
            catch
                _:_ -> invalid
            end;
        false ->
            invalid
    end.

port_digits(<<>>) ->
    false;
port_digits(<<C, Rest/binary>>) when C >= $0, C =< $9 -> port_digits_rest(Rest);
port_digits(_) ->
    false.

port_digits_rest(<<>>) ->
    true;
port_digits_rest(<<C, Rest/binary>>) when C >= $0, C =< $9 -> port_digits_rest(Rest);
port_digits_rest(_) ->
    false.

port_or_default(undefined, <<"http">>) -> {ok, ?DEFAULT_HTTP_PORT};
port_or_default(undefined, <<"https">>) -> {ok, ?DEFAULT_HTTPS_PORT};
port_or_default(undefined, _Scheme) -> {error, missing_port};
port_or_default(invalid, _Scheme) -> {error, bad_port};
port_or_default(Port, _Scheme) when is_integer(Port) -> {ok, Port}.

%% 非缺省端口保留；缺省端口折叠（同源 sibling 归一的核心）。
join_origin(Scheme, Host, Port) ->
    Default =
        case Scheme of
            <<"http">> -> ?DEFAULT_HTTP_PORT;
            <<"https">> -> ?DEFAULT_HTTPS_PORT;
            _ -> undefined
        end,
    case Port =:= Default of
        true ->
            <<Scheme/binary, "://", Host/binary>>;
        false ->
            PortBin = integer_to_binary(Port),
            <<Scheme/binary, "://", Host/binary, ":", PortBin/binary>>
    end.

lower(Bin) ->
    Lower = fun
        (C) when C >= $A, C =< $Z -> C + 32;
        (C) -> C
    end,
    <<<<(Lower(C))>> || <<C>> <= Bin>>.

%% @doc 申报 Origin 是否被 installation allowlist 精确允许。
%%
%% 双方都归一后再做逐字相等；allowlist 里的非法条目 fail-closed（配置错误
%% 必须显式暴露，不允许静默跳过某一条）。allowlist 为空 = 未配置 = 全拒。
-spec origin_allowed(term(), [binary()]) -> ok | {error, term()}.
origin_allowed(Declared, AllowedOrigins) when is_list(AllowedOrigins) ->
    case normalize_origin(Declared) of
        {error, _} = Err ->
            Err;
        {ok, Norm} ->
            case normalize_all(AllowedOrigins, []) of
                {error, _} = Err2 -> Err2;
                {ok, Norms} -> exact_member(Norm, Norms)
            end
    end;
origin_allowed(_Declared, _AllowedOrigins) ->
    {error, {invalid_origin, undefined}}.

%% @doc v1.1 同源对等放行（CSD-BE-01S，hosted-widget-contract S3 GAP-4 裁决）：
%% 放行集合 = installation `allowed_origins`（宿主面）∪ **与请求 Host 头
%% scheme+host:port 归一相同**（同源 iframe 面——iframe 内 fetch 的 Origin
%% 恒为 Widget 网关自身，嵌入合法性由 /w/ 的 frame-ancestors CSP 保证）。
%%
%% 返回可区分两种放行来源（测试/日志面）：
%%   * `ok`                —— allowlist 精确命中（allowlist 优先，语义不变）；
%%   * `{ok, same_origin}` —— Origin 与 Host 头归一相等（同源对等）；
%%   * `{error, _}`        —— 两者皆否 / 输入形状非法 / allowlist 配置错误
%%     （fail-closed 原样上抛）。
%%
%% `HostOrigin` 是 handler 由 Host 头 + 客户端侧 scheme 派生的归一 origin
%% （`undefined` = 头缺失/形状非法 → 同源分支不生效，仅剩 allowlist 判定）。
-spec origin_allowed(term(), [binary()], term()) -> ok | {ok, same_origin} | {error, term()}.
origin_allowed(Declared, AllowedOrigins, HostOrigin) when is_list(AllowedOrigins) ->
    case origin_allowed(Declared, AllowedOrigins) of
        ok ->
            ok;
        {error, origin_not_allowed} = NotAllowed ->
            case same_origin_as(Declared, HostOrigin) of
                true -> {ok, same_origin};
                false -> NotAllowed
            end;
        {error, _} = Err ->
            Err
    end;
origin_allowed(_Declared, _AllowedOrigins, _HostOrigin) ->
    {error, {invalid_origin, undefined}}.

%% 同源判定：双方各自归一后逐字相等（scheme+host+port 同一口径）；Host 侧
%% 形状非法 = 不构成同源放行（fail-closed，不做容错截断）。
same_origin_as(Declared, HostOrigin) when is_binary(Declared), is_binary(HostOrigin) ->
    case {normalize_origin(Declared), normalize_origin(HostOrigin)} of
        {{ok, Norm}, {ok, Norm}} -> true;
        _ -> false
    end;
same_origin_as(_Declared, _HostOrigin) ->
    false.

normalize_all([], Acc) ->
    {ok, lists:usort(Acc)};
normalize_all([O | Rest], Acc) ->
    case normalize_origin(O) of
        {ok, Norm} -> normalize_all(Rest, [Norm | Acc]);
        {error, _} = Err -> Err
    end.

exact_member(Norm, Norms) ->
    case lists:member(Norm, Norms) of
        true -> ok;
        false -> {error, origin_not_allowed}
    end.

%% ===================================================================
%% 匿名 subject / 签名身份 sub 的 HMAC 归一
%% ===================================================================

%% @doc 匿名 subject HMAC：数据域 = installation 公开标识 + 浏览器随机 ID，
%% 密钥 = 注入的服务端材料。同一 (installation, subject) 恒得同一 HMAC——
%% 这是「重放不重复 contact」幂等映射的锚；裸 subject/Key 永不出本函数。
-spec subject_hmac(binary(), binary(), binary()) -> binary().
subject_hmac(PublicWidgetId, SubjectId, Key) when
    is_binary(PublicWidgetId), is_binary(SubjectId), is_binary(Key), Key =/= <<>>
->
    binary:encode_hex(
        crypto:mac(hmac, sha256, Key, <<PublicWidgetId/binary, ":", SubjectId/binary>>)
    );
subject_hmac(_PublicWidgetId, _SubjectId, _Key) ->
    erlang:error(badarg).

%% @doc 签名身份 sub 的 HMAC 归一：数据域前缀 `identity:` 与匿名域永不相交。
-spec verified_subject_hmac(binary(), binary(), binary()) -> binary().
verified_subject_hmac(PublicWidgetId, Sub, Key) when
    is_binary(PublicWidgetId), is_binary(Sub), is_binary(Key), Key =/= <<>>
->
    Data = <<"identity:", PublicWidgetId/binary, ":", Sub/binary>>,
    binary:encode_hex(crypto:mac(hmac, sha256, Key, Data));
verified_subject_hmac(_PublicWidgetId, _Sub, _Key) ->
    erlang:error(badarg).

%% ===================================================================
%% 签名断言 claims 全查（iss/aud/widget_id/sub/exp/iat/jti）
%% ===================================================================

%% @doc 判定签名断言的 claims 是否可用（签名本身由注入验证器裁决后才进来）。
%%
%% Expectation：`#{aud := PublicWidgetId, widget_id := PublicWidgetId, now := Now}`。
%% 判定顺序固定（失败原因唯一可复现）：
%%   1. `now` 必须是注入的整数时钟；
%%   2. iss 非空二进制（签发方标识由验证器/配置锚定，这里只做形状）；
%%   3. aud =:= 本 installation 的 public_widget_id → 否则 assertion_aud_mismatch；
%%   4. widget_id =:= 本 installation 的 public_widget_id → 否则
%%      assertion_widget_mismatch；
%%   5. sub 非空二进制；
%%   6. exp 整数且 > Now → 否则 assertion_expired；
%%   7. iat 整数且 =< Now → 否则 assertion_iat_in_future；
%%   8. jti 非空二进制（一次性消费由 store nonce 唯一裁决）。
-spec assertion_claims(map(), map()) -> ok | {error, term()}.
assertion_claims(Claims, Expectation) when is_map(Claims), is_map(Expectation) ->
    case maps:get(now, Expectation, undefined) of
        Now when is_integer(Now) ->
            claims_iss(Claims, Expectation, Now);
        _ ->
            {error, {invalid_argument, assertion_now}}
    end;
assertion_claims(_Claims, _Expectation) ->
    {error, {invalid_argument, assertion_claims}}.

claims_iss(Claims, Expectation, Now) ->
    case claims_get(Claims, iss) of
        Iss when is_binary(Iss), Iss =/= <<>> -> claims_aud(Claims, Expectation, Now);
        _ -> {error, {invalid_claim, iss}}
    end.

claims_aud(Claims, Expectation, Now) ->
    Aud = claims_get(Claims, aud),
    Expected = maps:get(aud, Expectation, undefined),
    case is_binary(Aud) andalso Aud =/= <<>> andalso Aud =:= Expected of
        true -> claims_widget(Claims, Expectation, Now);
        false -> {error, assertion_aud_mismatch}
    end.

claims_widget(Claims, Expectation, Now) ->
    Widget = claims_get(Claims, widget_id),
    Expected = maps:get(widget_id, Expectation, undefined),
    case is_binary(Widget) andalso Widget =/= <<>> andalso Widget =:= Expected of
        true -> claims_sub(Claims, Now);
        false -> {error, assertion_widget_mismatch}
    end.

claims_sub(Claims, Now) ->
    case claims_get(Claims, sub) of
        Sub when is_binary(Sub), Sub =/= <<>> -> claims_exp(Claims, Now);
        _ -> {error, {invalid_claim, sub}}
    end.

claims_exp(Claims, Now) ->
    case claims_get(Claims, exp) of
        Exp when is_integer(Exp), Exp > Now -> claims_iat(Claims, Now);
        _ -> {error, assertion_expired}
    end.

claims_iat(Claims, Now) ->
    case claims_get(Claims, iat) of
        Iat when is_integer(Iat), Iat =< Now -> claims_jti(Claims);
        _ -> {error, assertion_iat_in_future}
    end.

claims_jti(Claims) ->
    case claims_get(Claims, jti) of
        Jti when is_binary(Jti), Jti =/= <<>> -> ok;
        _ -> {error, {invalid_claim, jti}}
    end.

claims_get(Claims, Key) ->
    maps:get(Key, Claims, undefined).

%% ===================================================================
%% branding 响应白名单
%% ===================================================================

%% @doc 只保留白名单键（且值必须是 binary）的 branding 视图；非 map 输入
%% （NULL/形状不符）归一为空 map——品牌缺失不是错误，泄漏才是。
-spec branding_view(term()) -> map().
branding_view(Branding) when is_map(Branding) ->
    Allowed = maps:with(?BRANDING_KEYS, Branding),
    maps:filter(fun(_K, V) -> is_binary(V) end, Allowed);
branding_view(_Other) ->
    #{}.
