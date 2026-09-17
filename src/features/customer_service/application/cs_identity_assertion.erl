%%% @doc 签名身份断言的验签器（CSB-02R）：`cs_widget_app:identity_exchange/2`
%%% 的 `assertion_verifier` 注入缺省实现。真钥材料只存在于 env 配置
%%% （`cs_widget_identity_keys`，[{Version, Key}]）；DB 侧 identity key 行只存
%%% digest——本模块以「材料摘要 = 行 digest」锚定版本，再以 HMAC 复核断言签名。
%%%
%%% 断言契约（HTTP 面 assertion 对象）：
%%%   `#{key_version := pos_integer(), claims := map(), sig := binary()}`
%%%   其中 `sig = hex(HMAC-SHA256(Key, canonical(claims)))`、
%%%   `canonical(claims) = jsx(键名字典序的键值对数组)`。
%%%
%%% 判定顺序（失败原因唯一可复现，全部 fail-closed）：
%%%   1. 形状：key_version 正整数 + claims 对象 + sig 非空 binary；
%%%   2. 材料：`cs_widget_identity_keys` 里必须有该 version 的条目——缺失即
%%%      `{error, identity_key_not_configured}`（与 DB 行语义同词，422）；
%%%   3. digest 锚定：sha256(Key) hex ≠ KeyDigest 即
%%%      `{error, identity_key_digest_mismatch}`（服务端配置漂移，500）；
%%%   4. 签名：HMAC 复核失败即 `{error, assertion_signature_mismatch}`（401）；
%%%   5. claims 归一（binary 键 → claims 白名单原子）后返回 `{ok, Claims}`——
%%%      iss/aud/widget_id/sub/exp/iat/jti 的取值全查在 domain
%%%      `cs_widget:assertion_claims/2`，jti 一次性消费在 store nonce。
%%%
%%% **本模块不做**：不做 claims 取值判定、不读库、不写日志（材料/sig 零落盘）。
-module(cs_identity_assertion).

-export([verify/2]).

-define(CLAIM_KEYS, [
    <<"iss">>, <<"aud">>, <<"widget_id">>, <<"sub">>, <<"exp">>, <<"iat">>, <<"jti">>
]).

%% @doc 验签入口（`assertion_verifier` 的缺省实现）。
-spec verify(term(), binary()) -> {ok, map()} | {error, term()}.
verify(Assertion, KeyDigest) when is_map(Assertion), is_binary(KeyDigest) ->
    case shape(Assertion) of
        {error, _} = Err ->
            Err;
        {ok, Kv, Claims, Sig} ->
            case key_material(Kv) of
                {error, _} = Err2 ->
                    Err2;
                {ok, Key} ->
                    verify_digest(Key, KeyDigest, Claims, Sig)
            end
    end;
verify(_Assertion, _KeyDigest) ->
    {error, {invalid_argument, assertion}}.

shape(#{key_version := Kv, claims := Claims, sig := Sig}) when
    is_integer(Kv), Kv > 0, is_map(Claims), is_binary(Sig), Sig =/= <<>>
->
    {ok, Kv, Claims, Sig};
shape(_Other) ->
    {error, {invalid_argument, assertion}}.

key_material(Kv) ->
    case provisioned_keys(config_ds:env(cs_widget_identity_keys, []), []) of
        Keys when is_list(Keys), Keys =/= [] ->
            case maps:find(Kv, maps:from_list(Keys)) of
                {ok, Key} -> {ok, Key};
                error -> {error, identity_key_not_configured}
            end;
        _ ->
            {error, identity_key_not_configured}
    end.

%% 配置形态双轨：sys.config 原生 `[{Version, Key}]`；或 env 注入的
%% `"1:k1,2:k2"` 字符串（容器/编排注入 secret 的常规形态）。未识别条目忽略。
provisioned_keys([{V, Key} | Rest], Acc) when is_integer(V), is_binary(Key), Key =/= <<>> ->
    provisioned_keys(Rest, [{V, Key} | Acc]);
provisioned_keys([_Other | Rest], Acc) ->
    provisioned_keys(Rest, Acc);
provisioned_keys([], Acc) ->
    Acc;
provisioned_keys(Bin, Acc) when is_binary(Bin) ->
    Parts = binary:split(Bin, <<",">>, [global, trim_all]),
    provisioned_keys_pairs(Parts, Acc);
provisioned_keys(_Other, Acc) ->
    Acc.

provisioned_keys_pairs([], Acc) ->
    Acc;
provisioned_keys_pairs([Pair | Rest], Acc) ->
    case binary:split(Pair, <<":">>) of
        [VBin, Key] ->
            try binary_to_integer(VBin, 10) of
                V when V > 0, Key =/= <<>> ->
                    provisioned_keys_pairs(Rest, [{V, Key} | Acc]);
                _ ->
                    provisioned_keys_pairs(Rest, Acc)
            catch
                _:_ ->
                    provisioned_keys_pairs(Rest, Acc)
            end;
        _ ->
            provisioned_keys_pairs(Rest, Acc)
    end.

verify_digest(Key, KeyDigest, Claims, Sig) ->
    %% encode_hex/2 lowercase：digest 惯例小写（shasum/xxd 同口径，DB 行亦然）；
    %% encode_hex/1 缺省大写会与任何小写 provisioned digest 恒不匹配
    %% （CSX-01 E2E 实测 identity_key_digest_mismatch 500 的根因）。
    case binary:encode_hex(crypto:hash(sha256, Key), lowercase) of
        KeyDigest ->
            verify_signature(Key, Claims, Sig);
        _Other ->
            %% provisioned material 与 DB 行 digest 不符 = 服务端配置漂移。
            {error, identity_key_digest_mismatch}
    end.

verify_signature(Key, Claims, Sig) ->
    %% 同 digest 口径：断言方签名 hex 惯例小写（node crypto digest('hex') 等）。
    Expected =
        binary:encode_hex(crypto:mac(hmac, sha256, Key, canonical_claims(Claims)), lowercase),
    case secure_equal(Expected, Sig) of
        true -> {ok, normalized_claims(Claims)};
        false -> {error, assertion_signature_mismatch}
    end.

%% canonical(claims)：键名一律 binary、按字典序排序的键值对数组再 JSON 编码
%% （jsx 对 proplist 编码为对象）——同 claims 恒得同一字节串。
canonical_claims(Claims) ->
    Pairs = [{key_bin(K), V} || {K, V} <- maps:to_list(Claims)],
    jsx:encode(lists:sort(Pairs)).

key_bin(K) when is_binary(K) -> K;
key_bin(K) when is_atom(K) -> atom_to_binary(K, utf8).

%% claims 键归一：仅白名单键（binary/atom 均接受）进 application 的 claims
%% 全查；白名单外的键丢弃（断言面最小暴露）。binary_to_atom 只作用于冻结
%% 白名单，无无界原子生长。
normalized_claims(Claims) ->
    maps:from_list([
        {claim_atom(K), V}
     || {K, V} <- maps:to_list(Claims),
        lists:member(key_bin(K), ?CLAIM_KEYS)
    ]).

claim_atom(K) when is_binary(K) -> binary_to_atom(K, utf8);
claim_atom(K) when is_atom(K) -> K.

%% 定长摘要的等值比较：先各自 sha256 再比对，避免逐字节早退。
secure_equal(A, B) when is_binary(A), is_binary(B) ->
    crypto:hash(sha256, A) =:= crypto:hash(sha256, B);
secure_equal(_A, _B) ->
    false.
