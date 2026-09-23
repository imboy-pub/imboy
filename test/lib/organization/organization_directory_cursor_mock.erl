%%% @doc CURSOR-V2 游标模块的测试替身（test-only；A4 单元/PG 测试注入用）。
%%%
%%% 与 A1 的 src/lib/enterprise_cursor_v2.erl 保持**同名 4 函数、相同语义**
%%% （接口冻结）：
%%%   canonical_json/1 —— 对象 key 按 UTF-8 字节序递归排序的紧凑 JSON
%%%   sign/2           —— base64url(canonical) + "." + base64url(HMAC-SHA256)
%%%   verify/2         —— 验签 + 过期检查（issued_at + 24h）→ {ok, Payload}
%%%   signing_key/0    —— 读 config enterprise_internal_cursor_signing_key
%%%                       （≥32 bytes；缺失/不合规 → {error, missing}）
%%%
%%% 只用 OTP crypto/base64 + jsone，不新增依赖（§10.1 同款约束）。
%%% W1.5 集成时 A4 的 application 层换回默认 enterprise_cursor_v2 即接真实现。
-module(organization_directory_cursor_mock).

-export([canonical_json/1, sign/2, verify/2, signing_key/0]).

-define(MIN_KEY_BYTES, 32).
-define(TTL_SECONDS, 24 * 3600).

%% ===================================================================
%% signing_key/0
%% ===================================================================

-spec signing_key() -> {ok, binary()} | {error, missing}.
signing_key() ->
    Key = application:get_env(imboy, enterprise_internal_cursor_signing_key, undefined),
    case is_binary(Key) andalso byte_size(Key) >= ?MIN_KEY_BYTES of
        true -> {ok, Key};
        false -> {error, missing}
    end.

%% ===================================================================
%% canonical_json/1
%% ===================================================================

-spec canonical_json(term()) -> {ok, binary()} | {error, not_canonicalizable}.
canonical_json(Term) ->
    try
        {ok, encode(Term)}
    catch
        throw:not_canonicalizable -> {error, not_canonicalizable}
    end.

%% 紧凑编码：对象 key 升序；数组保序；integer/boolean/null/string 按规范。
encode(Map) when is_map(Map) ->
    Keys = lists:sort(maps:keys(Map)),
    Parts = [
        <<(key_bin(K))/binary, $:, (encode(V))/binary>>
     || K <- Keys, V <- [maps:get(K, Map)]
    ],
    <<${, (join_parts(Parts))/binary, $}>>;
encode(List) when is_list(List) ->
    %% 仅视为 JSON array；Erlang 字符串一律走 binary（本合同无 string 列表输入）。
    Parts = [encode(V) || V <- List],
    <<$[, (join_parts(Parts))/binary, $]>>;
encode(Int) when is_integer(Int) ->
    integer_to_binary(Int);
encode(true) ->
    <<"true">>;
encode(false) ->
    <<"false">>;
encode(null) ->
    <<"null">>;
encode(Bin) when is_binary(Bin) ->
    jsone:encode(Bin);
encode(_Other) ->
    %% 本合同请求字段禁止浮点/NaN/Infinity/atom 等（§10.1）。
    throw(not_canonicalizable).

key_bin(K) when is_binary(K) ->
    jsone:encode(K);
key_bin(K) when is_atom(K) ->
    jsone:encode(atom_to_binary(K, utf8)).

join_parts([]) ->
    <<>>;
join_parts(Parts) ->
    iolist_to_binary(lists:join(<<$,>>, Parts)).

%% ===================================================================
%% sign/2
%% ===================================================================

-spec sign(map(), binary()) -> {ok, binary()} | {error, term()}.
sign(Payload, Key) when is_map(Payload), is_binary(Key) ->
    case canonical_json(Payload) of
        {ok, Canonical} ->
            Mac = crypto:mac(hmac, sha256, Key, Canonical),
            {ok, <<(b64url_encode(Canonical))/binary, ".", (b64url_encode(Mac))/binary>>};
        {error, Reason} ->
            {error, Reason}
    end;
sign(_Payload, _Key) ->
    {error, invalid_argument}.

%% ===================================================================
%% verify/2
%% ===================================================================

-spec verify(binary(), binary()) -> {ok, map()} | {error, invalid | tampered | expired}.
verify(Cursor, Key) when is_binary(Cursor), is_binary(Key) ->
    case binary:split(Cursor, <<".">>) of
        [B64Json, B64Mac] ->
            case {b64url_decode(B64Json), b64url_decode(B64Mac)} of
                {{ok, Canonical}, {ok, Mac}} ->
                    Expected = crypto:mac(hmac, sha256, Key, Canonical),
                    case crypto:hash_equals(Mac, Expected) of
                        true -> decode_payload(Canonical);
                        false -> {error, tampered}
                    end;
                _ ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end;
verify(_Cursor, _Key) ->
    {error, invalid}.

decode_payload(Canonical) ->
    try
        #{<<"issued_at">> := IssuedAt} =
            Payload = jsone:decode(Canonical, [
                {object_format, map}
            ]),
        case is_integer(IssuedAt) andalso os:system_time(second) - IssuedAt < ?TTL_SECONDS of
            true -> {ok, Payload};
            false -> {error, expired}
        end
    catch
        _:_ -> {error, invalid}
    end.

%% ===================================================================
%% base64url（无 padding；解码时按需补齐）
%% ===================================================================

b64url_encode(Bin) ->
    Base64 = base64:encode(Bin),
    <<<<(b64url_char(C))>> || <<C>> <= Base64, C =/= $=>>.

b64url_char($+) ->
    $-;
b64url_char($/) ->
    $_;
b64url_char(C) ->
    C.

b64url_decode(Bin) ->
    try
        Base64 = <<<<(b64_std_char(C))>> || <<C>> <= Bin>>,
        case (4 - byte_size(Base64) rem 4) rem 4 of
            0 -> {ok, base64:decode(Base64)};
            Pad -> {ok, base64:decode(<<Base64/binary, (binary:copy(<<$=>>, Pad))/binary>>)}
        end
    catch
        _:_ -> {error, invalid}
    end.

b64_std_char($-) ->
    $+;
b64_std_char($_) ->
    $/;
b64_std_char(C) ->
    C.
