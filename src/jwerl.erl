%% jwerl.erl — drop-in shim over jose (potatosalad/erlang-jose 1.11.12)
%%
%% 目的 / Purpose:
%%   保持 `jwerl` 模块名与原 API 形状不变，内部改用 jose 实现签名/验签，
%%   使 imboy 无需改动任何调用点即可从 jwerl 依赖切换到 jose 依赖。
%%   Keep the `jwerl` module name and API shape; implement sign/verify on top
%%   of jose so imboy can drop the jwerl dependency without touching callers.
%%
%% 语义基线 / Semantic baseline:
%%   逐条对照 jwerl 源码（deps/jwerl/src/jwerl.erl）复刻：
%%   * sign/1,2,3 与 verify/1..5、header/1 的元数与守卫
%%   * {ok, Claims} | {error, Reason} 的确切形状（含 {error, [sub, exp]} 列表顺序）
%%   * {error, {invalid_algorithm, HeaderAlg, RequestedAlg}}
%%   * {error, invalid_signature}
%%   * exp/iat/nbf 检查与 exp_leeway/iat_leeway Opts（os:system_time(seconds)）
%%   * 自定义 claim 的 Required 匹配（claim_match 的 string-or-uri 规则）
%%   * base64url 手工实现（urlencode/urldecode + 重补 padding），保证字节级兼容
%%   * -on_load conveniece_keys/0 预注册 JWT 保留字 atom（jsx attempt_atom 依赖）
%%   * `none` 算法：无签名 token 的签发与"不验签直接解码"（jwerl 原有行为，
%%     见下方安全说明）
%%
%% 与 jwerl 的已知偏差 / Known deviations from jwerl:
%%   1. HS*：jose_jws_alg_hmac 底层同为 crypto:mac(hmac, shaN, Key, Data)，
%%      签名字节级一致；同 VM 内签出的 token 与 jwerl 完全相同（差分测试证明）。
%%   2. RS*：jose_jws_alg_rsa_pkcs1_v1_5 底层同为 public_key:sign/verify
%%      （RSASSA-PKCS1-v1_5，确定性签名），token 字节级一致。
%%   3. ES*：jwerl_es 的签名是 public_key:sign 的 DER 编码
%%      （'ECDSA-Sig-Value'，非 JOSE 标准的 raw r||s）；jose 输出 raw。
%%      shim 在 ES 路径做 raw<->DER 翻译，保证与 jwerl 签发的 token 互验兼容
%%      （与 jwerl 同为"DER 格式"，即保持 jwerl 的既有线上格式不变）。
%%      唯一行为差异：对被篡改的 ES 签名，jwerl 可能在 public_key 内部 crash，
%%      shim 统一返回 {error, invalid_signature}（更安全，且 imboy 不使用 ES）。
%%   4. 存量 token 兼容性：HS256 是 imboy 唯一使用的算法（token_ds.erl、
%%      rtc_room_logic.erl），其 HMAC-SHA256 验签与算法名匹配逻辑和 jwerl
%%      完全一致，故所有存量 token 在切换后仍可正常验签。
%%
%% 安全说明 / Security note:
%%   jwerl 支持 alg=none 且验签时不校验（原样复刻）。若线上存在用 none 签发
%%   的 token，切换不改变其（不安全的）行为。建议后续单独清理 none 用法。
%%
-module(jwerl).

-export([sign/1, sign/2, sign/3,
         verify/1, verify/2, verify/3, verify/4, verify/5,
         header/1]).

-on_load(conveniece_keys/0).

-include_lib("public_key/include/public_key.hrl").

-define(DEFAULT_ALG, <<"HS256">>).
-define(DEFAULT_HEADER, #{typ => <<"JWT">>,
                          alg => ?DEFAULT_ALG}).

-type algorithm() :: hs256 | hs384 | hs512 |
                     rs256 | rs384 | rs512 |
                     es256 | es384 | es512 |
                     none.

%% @equiv sign(Data, hs256, <<"">>)
-spec sign(Data :: map()) -> binary().
sign(Data) ->
    sign(Data, hs256, <<"">>).
% @equiv sign(Data, Algorithm, <<"">>)
-spec sign(Data :: map(), Algorithm :: algorithm()) -> binary().
sign(Data, Algorithm) ->
    sign(Data, Algorithm, <<"">>).
% @doc
% Sign <tt>Data</tt> with the given <tt>Algorithm</tt> and <tt>KeyOrPem</tt>.
%
% Supported algorithms :
% <ul>
% <li>hs256, hs384, hs512</li>
% <li>rs256, rs384, rs512</li>
% <li>es256, es384, es512</li>
% <li>none</li>
% </ul>
%
% This function support ext, nbt, iat, iss, sub, aud and jti.
%
% Example:
%
% <pre>
% Token = jwerl:sign(#{key =&gt; &lt;&lt;"Hello World"&gt;&gt;}, hs256, &lt;&lt;"s3cr3t k3y"&gt;&gt;).
% </pre>
% @end
-spec sign(Data :: map() | list(), Algorithm :: algorithm(), KeyOrPem :: binary()) -> binary().
sign(Data, Algorithm, KeyOrPem) when (is_map(Data) orelse is_list(Data)), is_atom(Algorithm), is_binary(KeyOrPem) ->
    encode(jsx:encode(Data), config_headers(#{alg => algorithm_to_binary(Algorithm)}), KeyOrPem).

% @equiv verify(Data, <<"">>, hs256, #{}, #{})
verify(Data) ->
    verify(Data, hs256, <<"">>, #{}, #{}).
% @equiv verify(Data, Algorithm, <<"">>, #{}, #{})
verify(Data, Algorithm) ->
    verify(Data, Algorithm, <<"">>, #{}, #{}).
% @equiv verify(Data, Algorithm, KeyOrPem, #{}, #{})
verify(Data, Algorithm, KeyOrPem) ->
  verify(Data, Algorithm, KeyOrPem, #{}, #{}).
% @equiv verify(Data, Algorithm, KeyOrPem, #{}, #{})
verify(Data, Algorithm, KeyOrPem, Claims) ->
  verify(Data, Algorithm, KeyOrPem, Claims, #{}).
% @doc
% Verify a JWToken according to the given <tt>Algorithm</tt>, <tt>KeyOrPem</tt> and <tt>Claims</tt>.
% This verifycation can ignore (<tt>CheckClaims =:= false</tt>) claims.
%
% This function support ext, nbt, iat, iss, sub, aud and jti.
%
% Options:
%
% <ul>
% <li><tt>exp_leeway</tt> : <tt>integer()</tt></li>
% <li><tt>iat_leeway</tt> : <tt>integer()</tt></li>
% </ul>
%
% Example :
%
% <pre>
% jwerl:verify(Token, hs256, &lt;&lt;"s3cr3t k3y"&gt;&gt;, #{sub =&gt; &lt;&lt;"hello"&gt;&gt;,
%                                                            aud =&gt; [&lt;&lt;"world"&gt;&gt;, &lt;&lt;"aliens"&gt;&gt;]}).
% </pre>
% @end
-spec verify(Data :: binary(), Algorithm :: algorithm(), KeyOrPem :: binary(), CheckClaims :: map() | list() | false, Opts :: map() | list()) ->
  {ok, map()} | {error, term()}.
verify(Data, Algorithm, KeyOrPem, Claims, Opts) ->
  case decode(Data, KeyOrPem, Algorithm) of
    {ok, TokenData} when is_map(Claims) orelse is_list(Claims) ->
      case check_claims(TokenData, Claims, Opts) of
        ok ->
          {ok, TokenData};
        Error ->
          Error
      end;
    Result ->
      Result
  end.

% @doc
% Return the header for a given <tt>JWToken</tt>.
%
% Example:
%
% <pre>
% jwerl:header(Token).
% </pre>
% @end
-spec header(Data :: binary()) -> map().
header(Data) ->
  decode_header(Data).

check_claims(TokenData, Claims, Opts) when is_map(Opts) ->
  check_claims(TokenData, Claims, maps:to_list(Opts));
check_claims(TokenData, Claims, Opts) when is_list(Opts) ->
  Now = os:system_time(seconds),
  claims_errors(
    [
     check_claim(TokenData, exp, false, fun(ExpireTime) ->
                                            ExpLeeway = proplists:get_value(exp_leeway, Opts, 0),
                                            Now < ExpireTime + ExpLeeway
                                        end, exp),
     check_claim(TokenData, iat, false, fun(IssuedAt) ->
                                            IatLeeway = proplists:get_value(iat_leeway, Opts, 0),
                                            IssuedAt - IatLeeway =< Now
                                        end, iat),
     check_claim(TokenData, nbf, false, fun(NotBefore) ->
                                            NotBefore =< Now
                                        end, nbf)
     | [
        check_claim(
          TokenData,
          Claim,
          true,
          fun(Value) ->
              claim_match(Expected, Value)
          end, Claim)
        || {Claim, Expected} <- get_claims(Claims)
       ]
    ], []);
check_claims(TokenData, Claims, _Opts) ->
  check_claims(TokenData, Claims, []).

claims_errors([], []) -> ok;
claims_errors([], List) -> {error, List};
claims_errors([ok|Rest], Acc) -> claims_errors(Rest, Acc);
claims_errors([{error, Error}|Rest], Acc) -> claims_errors(Rest, [Error|Acc]).

claim_match(Expected, Value) ->
  case is_string_or_uri(Value) of
    true ->
      case Expected of
        Value ->
          true;
        List when is_list(List) ->
          lists:member(Value, List);
        _Other ->
          false
      end;
    false ->
      false
  end.

check_claim(TokenData, Key, Required, F, FailReason) ->
  case get_claim(Key, TokenData) of
    error when Required =:= false ->
      %% Ignore if missing. If it has been correctly signed,
      %% this was intended.
      ok;
    error ->
      {error, FailReason};
    {ok, Value} ->
      %% Call back if found for custom checking logic
      case F(Value) of
        true -> ok;
        false -> {error, FailReason}
      end
  end.

get_claim(Claim, Map) when is_map(Map) ->
  maps:find(Claim, Map);
get_claim(Claim, List) when is_list(List) ->
  case lists:keyfind(Claim, 1, List) of
    {Claim, Value} -> {ok, Value};
    false -> error
  end.

get_claims(Map) when is_map(Map) ->
  maps:to_list(Map);
get_claims(List) when is_list(List) ->
  List.

encode(Data, #{alg := <<"none">>} = Options, _) ->
  encode_input(Data, Options);
encode(Data, Options, Key) ->
  Input = encode_input(Data, Options),
  <<Input/binary, ".", (signature(maps:get(alg, Options), Key, Input))/binary>>.

decode(Data, KeyOrPem, Algorithm) ->
  Header = decode_header(Data),
  case algorithm_to_atom(maps:get(alg, Header)) of
    Algorithm -> payload(Data, Algorithm, KeyOrPem);
    Algorithm1 -> {error, {invalid_algorithm, Algorithm1, Algorithm}}
  end.

base64_encode(Data) ->
  Data1 = base64_encode_strip(lists:reverse(base64:encode_to_string(Data))),
  << << (urlencode_digit(D)) >> || <<D>> <= Data1 >>.
base64_encode_strip([$=|Rest]) ->
  base64_encode_strip(Rest);
base64_encode_strip(Result) ->
  list_to_binary(lists:reverse(Result)).

base64_decode(Data) ->
  Data1 = << << (urldecode_digit(D)) >> || <<D>> <= Data >>,
  Data2 = case byte_size(Data1) rem 4 of
            2 -> <<Data1/binary, "==">>;
            3 -> <<Data1/binary, "=">>;
            _ -> Data1
          end,
  base64:decode(Data2).

urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.

urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.

config_headers(Options) ->
  maps:merge(?DEFAULT_HEADER, Options).

decode_header(Data) ->
  [Header|_] = binary:split(Data, <<".">>, [global]),
  jsx:decode(base64_decode(Header), [return_maps, {labels, attempt_atom}]).

payload(Data, none, _) ->
  [_, Data1|_] = binary:split(Data, <<".">>, [global]),
  {ok, jsx:decode(base64_decode(Data1), [return_maps, {labels, attempt_atom}])};
payload(Data, Algorithm, Key) ->
  [Header, Data1, Signature] = binary:split(Data, <<".">>, [global]),
  case verify_signature(Algorithm,
                        Key,
                        <<Header/binary, ".", Data1/binary>>,
                        base64_decode(Signature)) of
    true ->
      {ok, jsx:decode(base64_decode(Data1), [return_maps, {labels, attempt_atom}])};
    _ ->
      {error, invalid_signature}
  end.

encode_input(Data, Options) ->
  <<(base64_encode(jsx:encode(Options)))/binary, ".", (base64_encode(Data))/binary>>.

%% -------------------------------------------------------------------------
%% jose-based signature primitives
%% -------------------------------------------------------------------------

signature(Algorithm, Key, Data) ->
  base64_encode(sign_jws(Algorithm, Key, Data)).

%% 返回原始签名字节 / Return raw signature bytes via jose
sign_jws(Algorithm, Key, Data) ->
  case algorithm_to_binary(Algorithm) of
    <<"HS", _/binary>> = HsAlg ->
      jose_jws_alg_hmac:sign(jose_jwk:from_oct(Key), Data, jose_alg_atom(HsAlg));
    <<"RS", _/binary>> = RsAlg ->
      jose_jws_alg_rsa_pkcs1_v1_5:sign(jose_jwk:from_pem(Key), Data, jose_alg_atom(RsAlg));
    <<"ES", _/binary>> = EsAlg ->
      Raw = jose_jws_alg_ecdsa:sign(jose_jwk:from_pem(Key), Data, jose_alg_atom(EsAlg)),
      raw_to_der(Raw);
    _ ->
      %% 与 jwerl:algorithm_to_infos 的 exit(invalid_algorithme) 一致
      %%（拼写错误一并保留，保持行为兼容）
      exit(invalid_algorithme)
  end.

%% 返回 boolean / Return boolean via jose
verify_signature(Algorithm, Key, Data, Signature) ->
  case algorithm_to_binary(Algorithm) of
    <<"HS", _/binary>> = HsAlg ->
      jose_jws_alg_hmac:verify(jose_jwk:from_oct(Key), Data, Signature, jose_alg_atom(HsAlg));
    <<"RS", _/binary>> = RsAlg ->
      jose_jws_alg_rsa_pkcs1_v1_5:verify(jose_jwk:from_pem(Key), Data, Signature, jose_alg_atom(RsAlg));
    <<"ES", _/binary>> = EsAlg ->
      case der_to_raw(Signature, EsAlg) of
        false ->
          false;
        Raw ->
          jose_jws_alg_ecdsa:verify(jose_jwk:from_pem(Key), Data, Raw, jose_alg_atom(EsAlg))
      end;
    _ ->
      exit(invalid_algorithme)
  end.

jose_alg_atom(<<"HS256">>) -> 'HS256';
jose_alg_atom(<<"HS384">>) -> 'HS384';
jose_alg_atom(<<"HS512">>) -> 'HS512';
jose_alg_atom(<<"RS256">>) -> 'RS256';
jose_alg_atom(<<"RS384">>) -> 'RS384';
jose_alg_atom(<<"RS512">>) -> 'RS512';
jose_alg_atom(<<"ES256">>) -> 'ES256';
jose_alg_atom(<<"ES384">>) -> 'ES384';
jose_alg_atom(<<"ES512">>) -> 'ES512'.

%% ES: jose raw r||s -> jwerl 线上格式 DER('ECDSA-Sig-Value')
raw_to_der(Raw) ->
  Len = byte_size(Raw) div 2,
  <<RB:Len/binary, SB:Len/binary>> = Raw,
  R = crypto:bytes_to_integer(RB),
  S = crypto:bytes_to_integer(SB),
  public_key:der_encode('ECDSA-Sig-Value', #'ECDSA-Sig-Value'{r = R, s = S}).

%% ES: jwerl 线上格式 DER -> jose raw r||s（按算法固定宽度补零）
%% ES256 -> 32 字节/半, ES384 -> 48, ES512 -> 66（对齐 jose_jws_alg_ecdsa:jws_alg_to_r_s_size）
der_to_raw(DERSignature, EsAlg) ->
  try public_key:der_decode('ECDSA-Sig-Value', DERSignature) of
    #'ECDSA-Sig-Value'{r = R, s = S} when is_integer(R), R >= 0,
                                          is_integer(S), S >= 0 ->
      Size = es_r_s_size(EsAlg),
      case R < (1 bsl (Size * 8)) andalso S < (1 bsl (Size * 8)) of
        true ->
          <<R:(Size * 8)/big-unsigned-integer, S:(Size * 8)/big-unsigned-integer>>;
        false ->
          false
      end;
    _ ->
      false
  catch
    _:_ ->
      %% 与 jwerl 的差异：篡改/损坏的 ES 签名不 crash，返回 invalid_signature
      false
  end.

es_r_s_size(<<"ES256">>) -> 32;
es_r_s_size(<<"ES384">>) -> 48;
es_r_s_size(<<"ES512">>) -> 66.

algorithm_to_atom(<<"HS256">>) -> hs256;
algorithm_to_atom(<<"RS256">>) -> rs256;
algorithm_to_atom(<<"ES256">>) -> es256;
algorithm_to_atom(<<"HS384">>) -> hs384;
algorithm_to_atom(<<"RS384">>) -> rs384;
algorithm_to_atom(<<"ES384">>) -> es384;
algorithm_to_atom(<<"HS512">>) -> hs512;
algorithm_to_atom(<<"RS512">>) -> rs512;
algorithm_to_atom(<<"ES512">>) -> es512;
algorithm_to_atom(A) when is_atom(A) -> A;
algorithm_to_atom(_) -> none.

algorithm_to_binary(hs256) -> <<"HS256">>;
algorithm_to_binary(rs256) -> <<"RS256">>;
algorithm_to_binary(es256) -> <<"ES256">>;
algorithm_to_binary(hs384) -> <<"HS384">>;
algorithm_to_binary(rs384) -> <<"RS384">>;
algorithm_to_binary(es384) -> <<"ES384">>;
algorithm_to_binary(hs512) -> <<"HS512">>;
algorithm_to_binary(rs512) -> <<"RS512">>;
algorithm_to_binary(es512) -> <<"ES512">>;
algorithm_to_binary(A) when is_binary(A) -> A;
algorithm_to_binary(_) -> <<"none">>.

conveniece_keys() ->
    registered_claim_names(),
    header_parameters(),
    miscellaneous(),
    ok.

registered_claim_names() ->
    iss,
    sub,
    aud,
    exp,
    nbf,
    iat,
    jti.

header_parameters() ->
    typ,
    cty.

miscellaneous() ->
    alg.

is_string_or_uri(Value) when is_binary(Value) ->
  size(trim(Value, both)) > 0;
is_string_or_uri(_Value) ->
  false.

trim(Binary, left) ->
  trim_left(Binary);
trim(Binary, right) ->
  trim_right(Binary);
trim(Binary, both) ->
  trim_left(trim_right(Binary)).

trim_left(<<C, Rest/binary>>) when C =:= $\s orelse
                                   C =:= $\n orelse
                                   C =:= $\r orelse
                                   C =:= $\t ->
  trim_left(Rest);
trim_left(Binary) -> Binary.

trim_right(Binary) ->
  trim_right(Binary, size(Binary)-1).

trim_right(Binary, Size) ->
  case Binary of
    <<Rest:Size/binary, C>> when C =:= $\s
                                 orelse C =:= $\t
                                 orelse C =:= $\n
                                 orelse C =:= $\r ->
      trim_right(Rest, Size - 1);
    Other ->
      Other
  end.
