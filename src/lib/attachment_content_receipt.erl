-module(attachment_content_receipt).
-moduledoc "direct-v1 内容 Worker 认证 receipt（step-12，协议 §5.4 / T30/T31）".
%%%
% Worker 扫描产物 receipt：HMAC-SHA256 绑定精确字节与 attempt——
%   fields = attempt_key || object_key || sha256hex || size || mime ||
%            ruleset_digest || verdict || expires_at
% 验收合同（T31，全部 fail-closed）：
%   * 伪造签名 / 任一字节被改 → 拒绝；
%   * 跨 attempt / 跨 object 绑定不符 → 拒绝；
%   * 过期（expires_at < now）→ 拒绝；
%   * ruleset 指纹不符（规则集在签发后被改）→ 拒绝；
%   * verdict 非 pass → 永不通过（worker 失败停待验证，不视通过）。
% Secret 沿 config_ds:env(jwt_key)（本实例内；不外发）。
%%%

-export([
    issue/2,
    verify/3,
    ruleset_digest/0
]).

-ifdef(TEST).
-export([canonical_fields/1]).
-endif.

-spec issue(map(), binary()) -> binary().
issue(
    F = #{
        attempt_key := AK,
        object_key := OK,
        sha256 := SHA,
        size := Size,
        mime := Mime,
        verdict := Verdict
    },
    Secret
) ->
    ExpiresAt = maps:get(expires_at, F, erlang:system_time(second) + 600),
    Ruleset = maps:get(ruleset_digest, F, ruleset_digest()),
    Fields = canonical_fields(#{
        attempt_key => AK,
        object_key => OK,
        sha256 => SHA,
        size => Size,
        mime => Mime,
        ruleset_digest => Ruleset,
        verdict => Verdict,
        expires_at => ExpiresAt
    }),
    Sig = hmac(Secret, Fields),
    body(Fields, Sig).

body(Fields, Sig) ->
    Payload = binary:split(Fields, <<"\n">>, [global]),
    %% 签名是原始随机字节（非 UTF-8）：base64 文本化后进 JSON
    Body = jsone:encode(#{
        <<"f">> => Payload,
        <<"s">> => base64:encode(Sig)
    }),
    base64:encode(Body).

hmac(Secret, Data) ->
    crypto:mac(hmac, sha256, Secret, Data).

-spec verify(binary(), binary(), map()) ->
    ok | {error, forged | expired | binding_mismatch | verdict_not_pass}.
verify(ReceiptB64, Secret, #{
    attempt_key := EAK,
    object_key := EOK,
    sha256 := ESHA,
    size := ESize,
    mime := EMime,
    ruleset_digest := ERules
}) ->
    try
        #{<<"f">> := FieldsList, <<"s">> := SigB64} =
            jsone:decode(base64:decode(ReceiptB64)),
        Sig = base64:decode(SigB64),
        Fields = iolist_to_binary(lists:join(<<"\n">>, FieldsList)),
        %% 1. 签名（伪造/篡改第一道门）
        case crypto:hash_equals(hmac(Secret, Fields), Sig) of
            false ->
                {error, forged};
            true ->
                M = fields_map(FieldsList),
                %% 2. 过期
                Now = erlang:system_time(second),
                Expires = binary_to_integer(maps:get(<<"expires_at">>, M)),
                case Expires >= Now of
                    false ->
                        {error, expired};
                    true ->
                        %% 3. ruleset 指纹（规则集漂移拒绝）
                        case
                            maps:get(<<"ruleset_digest">>, M) =:= ERules andalso
                                ERules =:= ruleset_digest()
                        of
                            false ->
                                {error, binding_mismatch};
                            true ->
                                %% 4. 绑定（attempt/object/字节精确匹配）
                                case
                                    maps:get(<<"attempt_key">>, M) =:= EAK andalso
                                        maps:get(<<"object_key">>, M) =:= EOK andalso
                                        maps:get(<<"sha256">>, M) =:= ESHA andalso
                                        maps:get(<<"size">>, M) =:= integer_to_binary(ESize) andalso
                                        maps:get(<<"mime">>, M) =:= EMime
                                of
                                    false ->
                                        {error, binding_mismatch};
                                    true ->
                                        %% 5. verdict 必须显式 pass
                                        case maps:get(<<"verdict">>, M) of
                                            <<"pass">> -> ok;
                                            _ -> {error, verdict_not_pass}
                                        end
                                end
                        end
                end
        end
    catch
        _:_ -> {error, forged}
    end.

canonical_fields(#{
    attempt_key := AK,
    object_key := OK,
    sha256 := SHA,
    size := Size,
    mime := Mime,
    ruleset_digest := Rules,
    verdict := Verdict,
    expires_at := Exp
}) ->
    iolist_to_binary(
        lists:join(<<"\n">>, [
            <<"attempt_key=", AK/binary>>,
            <<"object_key=", OK/binary>>,
            <<"sha256=", SHA/binary>>,
            <<"size=", (integer_to_binary(Size))/binary>>,
            <<"mime=", Mime/binary>>,
            <<"ruleset_digest=", Rules/binary>>,
            <<"verdict=", Verdict/binary>>,
            <<"expires_at=", (integer_to_binary(Exp))/binary>>
        ])
    ).

fields_map(List) ->
    maps:from_list([
        begin
            [K, V] = binary:split(Item, <<"=">>),
            {K, V}
        end
     || Item <- List
    ]).

%% @doc 内容规则集指纹：规则模块的语义版本锚（魔数表 + 文本判定的字节级
%% 摘要）。规则集被改 → digest 变 → 所有在途 receipt 绑定失效（T31）。
-spec ruleset_digest() -> binary().
ruleset_digest() ->
    Semantics = #{
        %% has_magic 前缀表（与 eb_asset_content 同步演进；漂移即换 digest）
        <<"magic_prefixes">> => [
            [<<"image/png">>, <<"hex:89504E470D0A1A0A">>],
            [<<"image/jpeg">>, <<"hex:FFD8FF">>],
            [<<"image/gif">>, [<<"GIF87a">>, <<"GIF89a">>]],
            [<<"image/webp">>, <<"riff_webp@offset8">>],
            [<<"image/bmp">>, <<"BM">>],
            [<<"image/svg+xml">>, <<"text_head_2k_svg">>],
            [<<"application/pdf">>, <<"%PDF-">>],
            [<<"text/plain">>, <<"text_full_clean">>],
            [<<"text/markdown">>, <<"text_full_clean">>],
            [<<"text/csv">>, <<"text_full_clean">>],
            [<<"application/zip">>, <<"hex:504B0304">>],
            [<<"application/x-7z-compressed">>, <<"hex:377ABCAF271C">>],
            [<<"application/gzip">>, <<"hex:1F8B">>],
            [<<"application/msword">>, <<"hex:D0CF11E0A1B11AE1">>],
            [<<"ooxml">>, <<"hex:504B0304">>],
            [<<"video/mp4">>, <<"ftyp_offset_4">>],
            [<<"video/webm">>, <<"hex:1A45DFA3">>],
            [<<"audio/mpeg">>, [<<"id3_or_sync">>, <<"mpeg_frame_sync">>]]
        ],
        %% 现行规则模块字节摘要（同步演进锚）
        <<"rule_module_sha256">> => rule_module_sha()
    },
    binary:encode_hex(crypto:hash(sha256, jsone:encode(Semantics))).

rule_module_sha() ->
    Src = code:which(eb_asset_content),
    case Src of
        preloaded ->
            <<"preloaded">>;
        _ ->
            case file:read_file(Src) of
                {ok, Bin} -> binary:encode_hex(crypto:hash(sha256, Bin));
                _ -> <<"unknown">>
            end
    end.
