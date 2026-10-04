-module(imboy_jwt).

-moduledoc "imboy JWT 签发/验签（纯 jose 方案，取代 jwerl shim）。".
%%%
% imboy JWT 签发/验签（纯 jose 方案，取代 jwerl shim）
% Pure-jose HS256 JWT sign/verify for imboy.
%
% 设计约束 / Design:
%   * 仅支持 HS256 —— 项目唯一使用算法（token_ds / rtc_room_logic / smoke 脚本），
%     不实现其他算法，杜绝 alg=none 与算法混淆路径。
%   * 验签走 jose_jwt:verify_strict 白名单，header 声明非白名单算法直接拒绝。
%   * jose 只验签名不校验时间 claims；exp/nbf/iat 在本模块显式校验（可配 leeway）。
%   * claims 的 key 一律 binary（jose JSON 层原生形态；旧 jwerl shim 为 atom key，
%     调用方已同步改为 binary key 读取）。
%   * 畸形 token 会让 jose 抛异常，此处统一收敛为 {error, invalid}，调用方无需 try。
%
% 与旧 jwerl 链路的错误语义对齐（净收紧点已标注）/ Error-shape vs the jwerl shim:
%   * exp 过期                -> {error, expired}            （token_ds 归 705 可刷新）
%   * 签名不匹配              -> {error, invalid_signature}   （706）
%   * nbf/iat 未生效          -> {error, {invalid_claim, _}}  （706）
%   * 畸形 token / 非法时间值 -> {error, invalid}             （706）
%   * 两处较旧链路收紧：缺 exp 旧链路实际放行（项序 `<<>> > Now` 恒 true），
%     本模块归 expired；alg=none 旧 shim 不验签直接放行，本模块一律拒绝。
%
% 存量 token 兼容：HS256 下 jwerl shim 与 jose 底层同为 crypto:mac(hmac, ...)，
% 签名字节级一致；jwerl 时代签发的 compact token 在本模块验签通过（金丝雀测试覆盖）。
%%%

-export([sign/2, verify/2, verify/3]).

-include("log.hrl").

%% 白名单算法：仅 HS256
-define(HS256_ONLY, [<<"HS256">>]).

-type claims() :: #{binary() => term()}.
-type verify_result() ::
    {ok, claims()}
    | {error, expired}
    | {error, {invalid_claim, nbf | iat}}
    | {error, invalid_signature}
    | {error, invalid}.

%% @doc 以 HS256 签发 compact JWS token
% jose 会自动补 header 的 typ=JWT；header/payload 的 JSON 序列化由 jose 完成，
% 调用方只给 binary-key claims map。atom key/value 均不被 jose JSON 层可靠支持，
% 本函数不做 try 收敛，入参类型由调用方保证（现有调用方均为 binary key/value）。
% 签出的 token 与旧 jwerl 链路格式互验兼容。
-spec sign(claims(), binary()) -> binary().
sign(Claims, Secret) when is_map(Claims), is_binary(Secret) ->
    JWK = jose_jwk:from_oct(Secret),
    {_, SignedMap} = jose_jwt:sign(JWK, #{<<"alg">> => <<"HS256">>}, Claims),
    {_, Compact} = jose_jws:compact(SignedMap),
    Compact.

%% @equiv verify(Token, Secret, #{})
-spec verify(binary(), binary()) -> verify_result().
verify(Token, Secret) ->
    verify(Token, Secret, #{}).

%% @doc 验签并校验时间 claims
% Opts（均为秒，默认 0）：
%   exp_leeway —— 允许 exp 已过 N 秒内仍放行（时钟偏差容忍）
%   nbf_leeway —— 允许 nbf 尚在未来 N 秒内仍放行
%   iat_leeway —— 允许 iat 尚在未来 N 秒内仍放行
%
% 时间基准 os:system_time(second)，与 jose/jwerl 生态一致。
-spec verify(binary(), binary(), map()) -> verify_result().
verify(Token, Secret, Opts) when is_binary(Token), is_binary(Secret), is_map(Opts) ->
    JWK = jose_jwk:from_oct(Secret),
    try jose_jwt:verify_strict(JWK, ?HS256_ONLY, Token) of
        {true, JWT, _JWS} ->
            %% #jose_jwt{} 的第 2 元素即 claims map（binary key）
            check_exp(element(2, JWT), Opts);
        {false, _JWT, _JWS} ->
            {error, invalid_signature}
    catch
        Class:Reason ->
            %% 畸形 token（非 3 段 / base64url 非法 / payload 非法 JSON 等）jose 会
            %% 抛异常；统一收敛为 invalid。留 debug 日志便于大面积验签失败时排障
            %% （不记 token 内容，避免日志注入面）。
            _ = ?DEBUG_LOG(['JWT_VERIFY_CRASH', Class, Reason]),
            {error, invalid}
    end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% exp：缺失视为 expired（较旧链路净收紧——旧 token_ds 手动判定 `<<>> > Now`
%% 在 Erlang 项序下恒为 true，缺 exp 的 token 实际被放行，属安全缺陷；自家
%% 签发端恒写 exp，无兼容影响），非整数视为 invalid（旧链路 badarith -> 706）。
check_exp(Claims, Opts) ->
    case Claims of
        #{<<"exp">> := Exp} when is_integer(Exp) ->
            Now = os:system_time(second),
            case Now < Exp + leeway(exp_leeway, Opts) of
                true -> check_nbf(Claims, Now, Opts);
                false -> {error, expired}
            end;
        #{<<"exp">> := _} ->
            {error, invalid};
        _ ->
            {error, expired}
    end.

%% nbf：缺失忽略；非整数 invalid；未生效 {invalid_claim, nbf}。
check_nbf(Claims, Now, Opts) ->
    case Claims of
        #{<<"nbf">> := Nbf} when is_integer(Nbf) ->
            case Nbf - leeway(nbf_leeway, Opts) =< Now of
                true -> check_iat(Claims, Now, Opts);
                false -> {error, {invalid_claim, nbf}}
            end;
        #{<<"nbf">> := _} ->
            {error, invalid};
        _ ->
            check_iat(Claims, Now, Opts)
    end.

%% iat：缺失忽略；非整数 invalid；签发时间在未来 {invalid_claim, iat}。
check_iat(Claims, Now, Opts) ->
    case Claims of
        #{<<"iat">> := Iat} when is_integer(Iat) ->
            case Iat - leeway(iat_leeway, Opts) =< Now of
                true -> {ok, Claims};
                false -> {error, {invalid_claim, iat}}
            end;
        #{<<"iat">> := _} ->
            {error, invalid};
        _ ->
            {ok, Claims}
    end.

leeway(Key, Opts) ->
    case Opts of
        #{Key := V} when is_integer(V), V >= 0 -> V;
        #{} -> 0
    end.
