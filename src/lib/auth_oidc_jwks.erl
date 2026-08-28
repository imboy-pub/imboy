-module(auth_oidc_jwks).

%%%
% OIDC id_token 签名验签（SEC-01）
% JWS 验签：discovery/JWKS 拉取 + kid 选择 + 算法白名单 + 缓存与轮换
%
% 流程（verify_id_token/2）：
%   1. JWT 三段拆分 -> header alg/kid
%   2. 算法白名单（默认仅 RS256/ES256；none/HS* 硬拒绝，配置亦不可放开）
%   3. 解析 JWKS：配置直给 jwks_uri，否则 discovery(issuer) 取 jwks_uri
%   4. 按 kid 选 key（kty/alg 必须与 token header 匹配，防 key confusion）
%   5. RS256: public_key:verify PKCS#1 v1.5 SHA-256
%      ES256: JWS raw r||s 转 DER 后 public_key:verify ECDSA-SHA256
%   6. 验签通过才解码 payload claims 返回
%
% 安全边界（fail-closed：任何异常/未知一律拒绝，绝不放行）：
%   - 算法白名单外（none/HS*/RS384/...）-> 拒绝
%   - 未知 kid -> 强制刷新一次 JWKS 再试，仍未知 -> 拒绝（支持密钥轮换）
%   - 签名不匹配 / JWK 材料缺失 / crv 非 P-256 / use 非 sig -> 拒绝或跳过该 key
%   - discovery/JWKS 拉取失败（网络错/非 0/坏 JSON）-> 拒绝（不用陈旧 key 兜底）
%   - 出站强制 TLS verify_peer（同 auth_oidc_logic 先例）；
%     http 仅白名单 127.0.0.1/localhost（本地 E2E）
%   - JWKS 缓存 TTL 默认 300s（oidc_jwks_cache_ttl 可配）；
%     未知 kid 触发的强制刷新受最小间隔约束（oidc_jwks_min_refresh_ms
%     默认 1000ms），防未知 kid 洪水打爆 IdP —— 被限流同样拒绝（fail-closed）
%%%

-export([verify_id_token/2, init_table/0, clear_cache/0]).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-define(JWKS_TAB, imboy_oidc_jwks_cache).
%% JWKS 缓存 TTL（秒）
-define(DEFAULT_CACHE_TTL, 300).
%% 算法白名单默认值（可用 oidc_allowed_algs 收窄；none/HS* 永久硬拒）
-define(DEFAULT_ALGS, [<<"RS256">>, <<"ES256">>]).
%% 未知 kid 触发强制刷新的最小间隔（毫秒）
-define(DEFAULT_MIN_REFRESH_MS, 1000).
-define(HTTP_TIMEOUT, 5000).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 验签 id_token：成功返回 {ok, Claims}（payload 解码后的 map），
%% 任何失败返回 {error, Reason}（细节只进服务端日志）。
-spec verify_id_token(binary(), map()) -> {ok, map()} | {error, term()}.
verify_id_token(IdToken, Cfg) when is_binary(IdToken), is_map(Cfg) ->
    StartMs = erlang:system_time(millisecond),
    case split_token(IdToken) of
        {ok, HeaderB64, PayloadB64, SigB64, SigningInput} ->
            case decode_json_map(HeaderB64) of
                {ok, Header} when is_map(Header) ->
                    Alg = maps:get(<<"alg">>, Header, <<>>),
                    Kid = kid_of_token(Header),
                    case alg_allowed(Alg) of
                        true ->
                            verify_sig_and_decode(
                                SigningInput, SigB64, PayloadB64, Alg, Kid, Cfg, StartMs
                            );
                        false ->
                            {error, {alg_not_allowed, Alg}}
                    end;
                _ ->
                    {error, bad_id_token}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
verify_id_token(_, _) ->
    {error, bad_id_token}.

%% @doc 启动期由长驻进程（imboy_app）建表。
%% 与 auth_oidc_logic 的 ONETIME_TAB 同理：表若被首个惰性建表的短命请求进程
%% 持有，请求结束即销毁 -> JWKS 缓存反复失效 -> 验签退化为每次外呼。
-spec init_table() -> ok.
init_table() ->
    ensure_table().

%% @doc 清空 JWKS 缓存（测试与运维排障用）。
-spec clear_cache() -> ok.
clear_cache() ->
    case ets:whereis(?JWKS_TAB) of
        undefined ->
            ok;
        _ ->
            _ = ets:delete_all_objects(?JWKS_TAB),
            ok
    end.

%% ===================================================================
%% Internal: 验签主流程
%% ===================================================================

verify_sig_and_decode(SigningInput, SigB64, PayloadB64, Alg, Kid, Cfg, StartMs) ->
    case select_key(Kid, Alg, Cfg, StartMs) of
        {ok, KeyInfo} ->
            case verify_sig(Alg, SigningInput, SigB64, KeyInfo) of
                true ->
                    case decode_json_map(PayloadB64) of
                        {ok, Claims} when is_map(Claims) -> {ok, Claims};
                        _ -> {error, bad_id_token}
                    end;
                false ->
                    {error, signature_invalid}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc kid 选 key；未知 kid 时强制刷新一次重试（密钥轮换支持）
select_key(Kid, Alg, Cfg, StartMs) ->
    case jwks(Cfg, StartMs) of
        {ok, #{keys := Keys} = Entry} ->
            case pick_key(Kid, Alg, Keys) of
                {ok, KeyInfo} ->
                    {ok, KeyInfo};
                {error, PickErr} when Kid =/= <<>> ->
                    %% 轮换：未知/不匹配 kid -> 强制刷新 JWKS 再试一次；
                    %% 刷新后仍失败 -> 以最终 pick 错误拒绝
                    case force_refresh(Cfg, Entry, StartMs) of
                        {ok, #{keys := Keys2}} -> pick_key(Kid, Alg, Keys2);
                        refresh_limited -> {error, PickErr};
                        {error, Reason} -> {error, Reason}
                    end;
                {error, PickErr} ->
                    {error, PickErr}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc token 无 kid：仅当 JWKS 恰好一把 key 时才允许（多 key 则歧义拒绝）
pick_key(<<>>, Alg, Keys) ->
    case maps:size(Keys) of
        1 -> key_matches(Alg, hd(maps:values(Keys)));
        _ -> {error, kid_ambiguous}
    end;
pick_key(Kid, Alg, Keys) ->
    case maps:get(Kid, Keys, undefined) of
        undefined -> {error, unknown_kid};
        KeyInfo -> key_matches(Alg, KeyInfo)
    end.

%% @doc key 族别与 JWK alg 声明（若有）必须与 token header alg 一致，
%% 防 ES256/RSA 混用等 key confusion
key_matches(Alg, #{kty := Kty} = KeyInfo) ->
    FamilyOk = alg_family(Alg) =:= Kty,
    JwkAlgOk =
        case KeyInfo of
            #{jwk_alg := A} -> A =:= Alg;
            _ -> true
        end,
    case FamilyOk andalso JwkAlgOk of
        true -> {ok, KeyInfo};
        false -> {error, key_type_mismatch}
    end.

%% @doc 签名验证（任何异常按不匹配处理，fail-closed）
verify_sig(<<"RS256">>, SigningInput, SigB64, #{key := PubKey}) ->
    try
        public_key:verify(SigningInput, sha256, b64url_decode(SigB64), PubKey)
    catch
        _:_ -> false
    end;
verify_sig(<<"ES256">>, SigningInput, SigB64, #{key := ECPoint}) ->
    try
        case b64url_decode(SigB64) of
            <<R:32/binary, S:32/binary>> ->
                Der = der_ecdsa_sig(binary:decode_unsigned(R), binary:decode_unsigned(S)),
                ECKey = {ECPoint, {namedCurve, ?secp256r1}},
                public_key:verify(SigningInput, sha256, Der, ECKey);
            _ ->
                false
        end
    catch
        _:_ -> false
    end;
verify_sig(_, _, _, _) ->
    false.

%% ===================================================================
%% Internal: JWKS 解析与缓存
%% ===================================================================

%% 缓存命中直接用；未命中/过期 -> 拉取（discovery + jwks）
jwks(Cfg, _StartMs) ->
    ok = ensure_table(),
    case cache_lookup(cache_key(Cfg)) of
        {ok, Entry} -> {ok, Entry};
        miss -> refresh(cache_key(Cfg), Cfg)
    end.

%% 未知 kid 的强制刷新：仅当上次拉取早于本次 verify 开始（非本次刚拉）
%% 且距上次拉取超过最小间隔（防未知 kid 洪水）；否则按限流拒绝
force_refresh(Cfg, #{fetched_ms := FetchedMs}, StartMs) ->
    MinMs = min_refresh_ms(),
    case FetchedMs < StartMs andalso erlang:system_time(millisecond) - FetchedMs >= MinMs of
        true -> refresh(cache_key(Cfg), Cfg);
        false -> refresh_limited
    end.

refresh(CacheKey, Cfg) ->
    case resolve_jwks_uri(Cfg) of
        {ok, JwksUri} ->
            case fetch_jwks(JwksUri) of
                {ok, Keys} ->
                    Entry = #{
                        jwks_uri => JwksUri,
                        keys => Keys,
                        fetched_ms => erlang:system_time(millisecond)
                    },
                    ok = cache_put(CacheKey, Entry),
                    {ok, Entry};
                {error, Reason} ->
                    {error, {jwks_fetch_failed, sanitize_http_error(Reason)}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc jwks_uri 解析：配置直给优先，否则走 OIDC discovery
resolve_jwks_uri(Cfg) ->
    case maps:get(<<"jwks_uri">>, Cfg, <<>>) of
        Uri when is_binary(Uri), Uri =/= <<>> ->
            check_url(Uri, insecure_jwks_url);
        _ ->
            discovery_jwks_uri(Cfg)
    end.

discovery_jwks_uri(Cfg) ->
    case issuer(Cfg) of
        <<>> ->
            {error, issuer_missing};
        Issuer ->
            Url = <<(trim_trailing_slash(Issuer))/binary, "/.well-known/openid-configuration">>,
            case check_url(Url, insecure_discovery_url) of
                {ok, _} ->
                    case http_get_json(Url) of
                        {ok, Doc} ->
                            case maps:get(<<"jwks_uri">>, Doc, <<>>) of
                                Uri when is_binary(Uri), Uri =/= <<>> ->
                                    check_url(Uri, insecure_jwks_url);
                                _ ->
                                    {error, bad_discovery_doc}
                            end;
                        {error, Reason} ->
                            {error, {discovery_failed, sanitize_http_error(Reason)}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

issuer(Cfg) ->
    trim_trailing_slash(maps:get(<<"issuer">>, Cfg, <<>>)).

%% 缓存键：优先 issuer（discovery 结果随 issuer 缓存），无 issuer 用 jwks_uri
cache_key(#{<<"issuer">> := Issuer}) when is_binary(Issuer), Issuer =/= <<>> ->
    {issuer, trim_trailing_slash(Issuer)};
cache_key(Cfg) ->
    {jwks_uri, maps:get(<<"jwks_uri">>, Cfg, <<>>)}.

%% @doc 拉取并解析 JWKS：RSA(n/e) 与 EC P-256(x/y)；
%% 单条 JWK 材料缺失/不支持的曲线/use 非 sig -> 跳过该条（不拖垮整份 JWKS）
fetch_jwks(Uri) ->
    case http_get_json(Uri) of
        {ok, Doc} ->
            case maps:get(<<"keys">>, Doc, undefined) of
                KeyList when is_list(KeyList) ->
                    {ok, parse_keys([K || K <- KeyList, is_map(K)])};
                _ ->
                    {error, bad_jwks}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_keys(KeyMaps) ->
    lists:foldl(fun parse_key/2, #{}, KeyMaps).

parse_key(#{<<"kty">> := <<"RSA">>} = K, Acc) ->
    case sig_use(K) andalso rsa_material(K) of
        {true, N, E} ->
            Key = #'RSAPublicKey'{
                modulus = binary:decode_unsigned(N),
                publicExponent = binary:decode_unsigned(E)
            },
            Acc#{kid_of(K) => #{kty => rsa, key => Key, jwk_alg => jwk_alg(K)}};
        _ ->
            Acc
    end;
parse_key(#{<<"kty">> := <<"EC">>} = K, Acc) ->
    case sig_use(K) andalso ec_material(K) of
        {true, X, Y} ->
            %% 仅支持 P-256（crv 白名单外的曲线在 ec_material 拒绝）
            Acc#{
                kid_of(K) => #{
                    kty => ec,
                    key => #'ECPoint'{point = <<4, X/binary, Y/binary>>},
                    jwk_alg => jwk_alg(K)
                }
            };
        _ ->
            Acc
    end;
parse_key(_, Acc) ->
    Acc.

%% use 缺省视为 sig；非 sig（如 enc）的 key 不用于验签
sig_use(K) ->
    case maps:get(<<"use">>, K, <<"sig">>) of
        <<"sig">> -> true;
        _ -> false
    end.

rsa_material(K) ->
    case {b64url_bin(maps:get(<<"n">>, K, <<>>)), b64url_bin(maps:get(<<"e">>, K, <<>>))} of
        {N, E} when is_binary(N), N =/= <<>>, is_binary(E), E =/= <<>> -> {true, N, E};
        _ -> false
    end.

%% 仅 P-256；x/y 必须各 32 字节（未压缩点坐标定长）
ec_material(K) ->
    CrvOk = maps:get(<<"crv">>, K, <<>>) =:= <<"P-256">>,
    case {CrvOk, b64url_bin(maps:get(<<"x">>, K, <<>>)), b64url_bin(maps:get(<<"y">>, K, <<>>))} of
        {true, X, Y} when byte_size(X) =:= 32, byte_size(Y) =:= 32 -> {true, X, Y};
        _ -> false
    end.

kid_of(K) ->
    case maps:get(<<"kid">>, K, <<>>) of
        Kid when is_binary(Kid) -> Kid;
        _ -> <<>>
    end.

jwk_alg(K) ->
    case maps:get(<<"alg">>, K, <<>>) of
        A when is_binary(A), A =/= <<>> -> A;
        _ -> undefined
    end.

%% ===================================================================
%% Internal: 缓存表（自有 ETS，惰性建表 + imboy_app 显式建表）
%% ===================================================================

ensure_table() ->
    case ets:whereis(?JWKS_TAB) of
        undefined ->
            try
                _ = ets:new(?JWKS_TAB, [
                    set,
                    public,
                    named_table,
                    {read_concurrency, true},
                    {write_concurrency, true}
                ]),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

cache_lookup(Key) ->
    Now = erlang:system_time(second),
    case ets:lookup(?JWKS_TAB, Key) of
        [{_, Entry, ExpireAt}] when ExpireAt > Now -> {ok, Entry};
        _ -> miss
    end.

cache_put(Key, Entry) ->
    ExpireAt = erlang:system_time(second) + cache_ttl(),
    true = ets:insert(?JWKS_TAB, {Key, Entry, ExpireAt}),
    ok.

%% ===================================================================
%% Internal: 出站 HTTP（同 auth_oidc_logic：强制 TLS verify_peer）
%% ===================================================================

%% @doc 出站 HTTPS 请求选项：强制校验证书链 + 主机名（防 MITM 伪造 JWKS）。
%% 安全：JWKS 是 id_token 验签的信任根，信道被劫持即可伪造任意 token，
%%       故必须 verify_peer；与 auth_oidc_logic:https_request_opts/0 同口径。
https_request_opts() ->
    SslOpts = [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {depth, 9},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ],
    [
        {timeout, ?HTTP_TIMEOUT},
        {connect_timeout, ?HTTP_TIMEOUT},
        {autoredirect, false},
        {ssl, SslOpts}
    ].

http_get_json(Url) ->
    UrlL = binary_to_list(Url),
    case httpc:request(get, {UrlL, []}, https_request_opts(), [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            decode_plain_json(Body);
        {ok, {{_, HttpCode, _}, _, _}} ->
            {error, {http_status, HttpCode}};
        {error, Reason} ->
            {error, Reason}
    end.

%% HTTP 响应体（明文 JSON）
decode_plain_json(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        Map when is_map(Map) -> {ok, Map};
        _ -> {error, bad_json}
    catch
        _:_ -> {error, bad_json}
    end.

%% @doc 脱敏出站错误：剥离响应体（JWKS 端点异常时可能回显敏感信息）
sanitize_http_error({http_status, Code, Body}) when is_binary(Body) ->
    {http_status, Code, {body_bytes, byte_size(Body)}};
sanitize_http_error(Other) ->
    Other.

%% ===================================================================
%% Internal: 算法白名单与配置
%% ===================================================================

%% @doc 算法白名单：none/HS*（HMAC 对称签名，JWKS 体系无 secret，且对
%% RS->HS 算法混淆攻击免疫必须硬拒）即使配置进白名单也永久拒绝；
%% 其余算法以 oidc_allowed_algs 配置为准（默认仅 RS256/ES256）。
alg_allowed(<<"none">>) ->
    false;
alg_allowed(<<"HS", _/binary>>) ->
    false;
alg_allowed(Alg) when is_binary(Alg) ->
    lists:member(Alg, allowed_algs());
alg_allowed(_) ->
    false.

allowed_algs() ->
    case application:get_env(imboy, oidc_allowed_algs, ?DEFAULT_ALGS) of
        L when is_list(L) -> [A || A <- L, is_binary(A)];
        _ -> ?DEFAULT_ALGS
    end.

alg_family(<<"RS256">>) -> rsa;
alg_family(<<"ES256">>) -> ec;
alg_family(_) -> undefined.

cache_ttl() ->
    int_env(oidc_jwks_cache_ttl, ?DEFAULT_CACHE_TTL).

min_refresh_ms() ->
    int_env(oidc_jwks_min_refresh_ms, ?DEFAULT_MIN_REFRESH_MS).

int_env(Key, Default) ->
    case application:get_env(imboy, Key, Default) of
        N when is_integer(N), N >= 0 -> N;
        _ -> Default
    end.

%% ===================================================================
%% Internal: 小工具
%% ===================================================================

%% JWT 三段拆分；签名段不可为空（alg=none 攻击面在白名单层已拒，
%% 这里再要求 Sig 段存在，双保险）
split_token(Token) when is_binary(Token) ->
    case binary:split(Token, <<".">>, [global]) of
        [_H, _P, _S] = Parts ->
            [HeaderB64, PayloadB64, SigB64] = Parts,
            case SigB64 of
                <<>> -> {error, bad_id_token};
                _ -> {ok, HeaderB64, PayloadB64, SigB64, join_signing_input(HeaderB64, PayloadB64)}
            end;
        _ ->
            {error, bad_id_token}
    end;
split_token(_) ->
    {error, bad_id_token}.

join_signing_input(HeaderB64, PayloadB64) ->
    <<HeaderB64/binary, ".", PayloadB64/binary>>.

kid_of_token(Header) ->
    case maps:get(<<"kid">>, Header, <<>>) of
        Kid when is_binary(Kid) -> Kid;
        _ -> <<>>
    end.

decode_json_map(Bin) ->
    try jsone:decode(b64url_decode(Bin), [{object_format, map}]) of
        Map when is_map(Map) -> {ok, Map};
        _ -> {error, bad_json}
    catch
        _:_ -> {error, bad_json}
    end.

b64url_bin(Bin) when is_binary(Bin), Bin =/= <<>> ->
    try
        Dec = base64:decode(Bin, #{mode => urlsafe, padding => false}),
        case Dec of
            <<>> -> error;
            _ -> Dec
        end
    catch
        _:_ -> error
    end;
b64url_bin(_) ->
    error.

b64url_decode(Bin) ->
    base64:decode(Bin, #{mode => urlsafe, padding => false}).

%% issuer 去尾部斜杠（discovery 拼接规范：issuer + 固定路径）
trim_trailing_slash(<<>>) ->
    <<>>;
trim_trailing_slash(Issuer) ->
    binary:replace(Issuer, <<"/">>, <<>>, [{scope, {byte_size(Issuer) - 1, 1}}]).

%% URL 限定 https；http 仅 127.0.0.1/localhost 白名单（本地 E2E，同
%% auth_oidc_logic:allowed_url/1 口径 —— 明文信道只允许打本机 fake IdP）
check_url(<<"https://", _/binary>> = Url, _Err) ->
    {ok, Url};
check_url(<<"http://", _/binary>> = Url, Err) ->
    case uri_string:parse(Url) of
        #{host := H} when H =:= <<"127.0.0.1">>; H =:= <<"localhost">> -> {ok, Url};
        #{host := H} when H =:= "127.0.0.1"; H =:= "localhost" -> {ok, Url};
        _ -> {error, Err}
    end;
check_url(_, Err) ->
    {error, Err}.

%% JWS ES256 签名是 raw r||s（各 32 字节），public_key 需要 DER 编码的
%% ECDSA-Sig-Value（SEQUENCE { INTEGER r, INTEGER s }），手工转换
der_ecdsa_sig(R, S) when R >= 0, S >= 0 ->
    Body = <<(der_int(R))/binary, (der_int(S))/binary>>,
    der_tlv(16#30, Body).

%% 非负 INTEGER DER 编码：最高位置位时补 0x00 前缀保正
der_int(N) when N >= 0 ->
    B = binary:encode_unsigned(N),
    case binary:first(B) band 16#80 of
        0 -> <<2, (byte_size(B)), B/binary>>;
        _ -> <<2, (byte_size(B) + 1), 0, B/binary>>
    end.

der_tlv(Tag, Body) ->
    <<Tag, (der_len(byte_size(Body)))/binary, Body/binary>>.

der_len(L) when L < 128 ->
    <<L>>;
der_len(L) ->
    Lb = binary:encode_unsigned(L),
    <<((byte_size(Lb) band 16#7f) bor 16#80), Lb/binary>>.
