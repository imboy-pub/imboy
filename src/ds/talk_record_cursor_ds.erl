-module(talk_record_cursor_ds).
-moduledoc "Extracted talk-record service responsibility; public entry remains talk_record_ds.".
-export([encode_cursor/3, decode_page_cursor/3]).
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-define(CURSOR_TTL_SECONDS, 86400).

%% @private 首页（无游标）直通；有游标则验签+绑定比对+TTL。
-spec decode_page_cursor(binary() | undefined, binary(), map()) ->
    {ok, term()} | {error, invalid | expired | key_unavailable}.
decode_page_cursor(undefined, _Family, _Binding) ->
    {ok, undefined};
decode_page_cursor(<<>>, _Family, _Binding) ->
    {ok, undefined};
decode_page_cursor(CursorBin, Family, Binding) ->
    case cursor_secret() of
        {ok, Key} -> verify_cursor(CursorBin, Key, Family, Binding);
        {error, key_unavailable} = E -> E
    end.

-spec encode_cursor(binary(), map(), [term()] | integer()) ->
    {ok, binary()} | {error, term()}.
encode_cursor(Family, Binding, SortVals) ->
    case cursor_secret() of
        {ok, Key} ->
            Payload = #{
                <<"v">> => 1,
                <<"family">> => Family,
                <<"b">> => Binding,
                <<"s">> => SortVals,
                <<"iat">> => os:system_time(second)
            },
            case enterprise_cursor_v2:canonical_json(Payload) of
                {ok, Canonical} ->
                    Mac = crypto:mac(hmac, sha256, Key, Canonical),
                    {ok, <<(b64url_encode(Canonical))/binary, ".", (b64url_encode(Mac))/binary>>};
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.

-spec verify_cursor(binary(), binary(), binary(), map()) ->
    {ok, term()} | {error, invalid | expired}.
verify_cursor(CursorBin, Key, Family, Binding) ->
    case binary:split(CursorBin, <<".">>) of
        [EncPayload, EncMac] ->
            case {b64url_decode(EncPayload), b64url_decode(EncMac)} of
                {{ok, Canonical}, {ok, Mac}} when byte_size(Mac) =:= 32 ->
                    Expected = crypto:mac(hmac, sha256, Key, Canonical),
                    case crypto:hash_equals(Mac, Expected) of
                        true -> accept_cursor(Canonical, Family, Binding);
                        false -> {error, invalid}
                    end;
                _ ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end.

-spec accept_cursor(binary(), binary(), map()) -> {ok, term()} | {error, invalid | expired}.
accept_cursor(Canonical, Family, Binding) ->
    try jsone:decode(Canonical) of
        #{
            <<"v">> := 1,
            <<"family">> := Family,
            <<"b">> := Binding,
            <<"s">> := SortVals,
            <<"iat">> := IssuedAt
        } when is_integer(IssuedAt) ->
            case os:system_time(second) - IssuedAt > ?CURSOR_TTL_SECONDS of
                true -> {error, expired};
                false -> {ok, SortVals}
            end;
        _ ->
            {error, invalid}
    catch
        _:_ -> {error, invalid}
    end.

%% @private 游标签名密钥：优先专用 key；缺失时从 postgre_aes_key 派生
%% 域分离子密钥（HMAC-SHA256），不引入新部署必配项。
-spec cursor_secret() -> {ok, binary()} | {error, key_unavailable}.
cursor_secret() ->
    Dedicated = config_ds:env(imboy, talk_record_cursor_signing_key, <<>>),
    Master = config_ds:env(imboy, postgre_aes_key, <<>>),
    case {key_ok(Dedicated), key_ok(Master)} of
        {true, _} -> {ok, Dedicated};
        {_, true} -> {ok, crypto:mac(hmac, sha256, Master, <<"talk_record-cursor-v1">>)};
        _ -> {error, key_unavailable}
    end.

-spec key_ok(term()) -> boolean().
key_ok(K) when is_binary(K), byte_size(K) >= 32 -> true;
key_ok(_) -> false.

-spec b64url_encode(binary()) -> binary().
b64url_encode(Bin) ->
    <<<<(b64url_char(C))/binary>> || <<C>> <= base64:encode(Bin)>>.

-spec b64url_char(byte()) -> binary().
b64url_char($+) -> <<"-">>;
b64url_char($/) -> <<"_">>;
b64url_char($=) -> <<>>;
b64url_char(C) -> <<C>>.

-spec b64url_decode(binary()) -> {ok, binary()} | error.
b64url_decode(B64Url) ->
    B641 = binary:replace(
        binary:replace(B64Url, <<"-">>, <<"+">>, [global]),
        <<"_">>,
        <<"/">>,
        [global]
    ),
    %% base64:decode 需要标准 padding；编码侧已去 padding，这里补齐到 4 倍长
    Pad =
        case byte_size(B641) rem 4 of
            2 -> <<"==">>;
            3 -> <<"=">>;
            _ -> <<>>
        end,
    try
        {ok, base64:decode(<<B641/binary, Pad/binary>>)}
    catch
        _:_ -> error
    end.
