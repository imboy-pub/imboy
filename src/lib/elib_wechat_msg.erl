-module(elib_wechat_msg).

%%%===================================================================
%%% @doc 微信「消息推送」加解密原语 / WeChat message-push crypto primitives
%%%
%%% 小程序与公众号的「消息推送」共用同一套规范（官方 WXBizMsgCrypt）：
%%%
%%%   1. **签名**：把 Token 与请求里的 Timestamp、Nonce 一起按**字典序**
%%%      排序后拼接，取 sha1 的十六进制（小写）。密文场景下第四个参与
%%%      排序的元素是请求体里的 `Encrypt` 字段（不是时间戳或别的）。
%%%   2. **密钥**：AESKey = base64:decode(EncodingAESKey ++ "=")，恒 32 字节；
%%%      **IV 取 AESKey 的前 16 字节**（不是随机值、也不是全零）；
%%%      分组模式 AES-256-CBC。
%%%   3. **明文结构**（加密前的字节流，顺序固定）：
%%%        random(16) || MsgLen(4, big-endian) || Msg || AppId
%%%      外层补位为 PKCS#7（不足整块则填满整块）。
%%%
%%% ⚠️ 为什么**不复用** elib_cipher:aes_encrypt/4 + aes_decrypt/4：
%%%   那一对是「手写补位 + crypto 隐式补位」的**双重补位**自洽方案 —— 加密时
%%%   补两次、解密时 crypto 拆一次再手剥一次，因此只与它自己那一侧配对。
%%%   微信是**单次** PKCS#7，两者混用会在解密端多剥一次：明文尾部若干字节被
%%%   吃掉，而且**不报错**（盐值指纹、appid 都可能被吃掉后仍解析成功）。
%%%   本模块的补位全部手写，并显式用 `{padding, none}` 关掉 crypto 的隐式
%%%   补位，使「补位口径」在本模块内只有一个来源。
%%%
%%% 本模块是**纯原语**：不读配置、不碰网络、不落日志，便于用固定向量测试。
%%% @end
%%%===================================================================

-export([signature/3, signature/4, verify/4, verify/5, secure_equal/2]).
-export([aes_key/1, decrypt/2, encrypt/3]).

-define(BLOCK_SIZE, 16).
-define(AES_KEY_SIZE, 32).
%% EncodingAESKey 的文本形态：43 字符（微信后台给出的形态，本身少一个 "="）
-define(AES_KEY_TEXT_SIZE, 43).
-define(RANDOM_PREFIX_SIZE, 16).
-define(MSG_LEN_SIZE, 4).

-type config_key() :: binary() | string().
-type aes_key() :: <<_:256>>.

%%%===================================================================
%%% 签名
%%%===================================================================

%% @doc 明文场景签名：sha1(字典序排序后的 [Token, Timestamp, Nonce])，十六进制小写。
%% 首次保存配置时微信发的 GET 校验走这一条。
-spec signature(binary(), binary(), binary()) -> binary().
signature(Token, Timestamp, Nonce) ->
    sha1_hex(sorted_concat([Token, Timestamp, Nonce])).

%% @doc 密文场景签名：参与字典序排序的第四个元素是请求体的 `Encrypt` 字段。
-spec signature(binary(), binary(), binary(), binary()) -> binary().
signature(Token, Timestamp, Nonce, Encrypt) ->
    sha1_hex(sorted_concat([Token, Timestamp, Nonce, Encrypt])).

%% @doc 校验明文场景签名。签名缺失或为空 → false（不发散、不抛错）。
-spec verify(binary(), binary(), binary(), binary()) -> boolean().
verify(Token, Timestamp, Nonce, Signature) when is_binary(Signature), Signature =/= <<>> ->
    secure_equal(signature(Token, Timestamp, Nonce), Signature);
verify(_Token, _Timestamp, _Nonce, _Signature) ->
    false.

%% @doc 校验密文场景签名（第四个签名元素是 Encrypt）。
-spec verify(binary(), binary(), binary(), binary(), binary()) -> boolean().
verify(Token, Timestamp, Nonce, Encrypt, Signature) when
    is_binary(Signature), Signature =/= <<>>
->
    secure_equal(signature(Token, Timestamp, Nonce, Encrypt), Signature);
verify(_Token, _Timestamp, _Nonce, _Encrypt, _Signature) ->
    false.

%% @doc 定长比较：先各自哈希再比，避免按字节早退泄漏「前几位猜对了」的信息。
-spec secure_equal(binary(), binary()) -> boolean().
secure_equal(A, B) when is_binary(A), is_binary(B), byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(crypto:hash(sha256, A), crypto:hash(sha256, B));
secure_equal(_A, _B) ->
    false.

%%%===================================================================
%%% 密钥
%%%===================================================================

%% @doc EncodingAESKey（43 字符的 base64 文本）→ 32 字节 AES 密钥。
%%
%% 微信给的是 base64 文本**去掉了一个 "="** 的形态，所以补一个 "=" 再解。
%% 同时接受已带 "=" 的 44 字符形态（个别后台导出工具会保留），但解码后
%% 必须恰好 32 字节，否则 fail-closed —— 少一个字符会静默解出错误密钥，
%% 表现为「解密出来的东西看似合理实则错位」，比直接失败难查得多。
-spec aes_key(config_key()) -> {ok, aes_key()} | {error, invalid_aes_key}.
aes_key(Key0) ->
    Key = to_binary(Key0),
    Padded =
        case byte_size(Key) of
            ?AES_KEY_TEXT_SIZE -> <<Key/binary, "=">>;
            %% 44 字符 = 已自带 "="（个别后台导出工具会保留）
            (?AES_KEY_TEXT_SIZE + 1) -> Key;
            _ -> undefined
        end,
    case Padded of
        undefined ->
            {error, invalid_aes_key};
        _ ->
            try base64:decode(Padded) of
                Decoded when byte_size(Decoded) =:= ?AES_KEY_SIZE -> {ok, Decoded};
                _ -> {error, invalid_aes_key}
            catch
                _:_ -> {error, invalid_aes_key}
            end
    end.

%%%===================================================================
%%% 解密 / 加密
%%%===================================================================

%% @doc 解密推送消息体的 `Encrypt` 字段。
%%
%% 入参 Encrypted 是 base64 **文本**（微信推送的原始形态），出参已按明文结构
%% 拆好：`#{msg := 业务报文, appid := 报文中携带的 AppId}`。
%%
%% ⚠️ appid **不由本函数裁决**。调用方必须与自己环境配置的 appid 比对：
%% AESKey 若在多个应用间复用（或攻击者拿到了别人的 EncodingAESKey），
%% 解出来的报文结构一样合法，只有 appid 能区分「这条消息本来发给谁」。
-spec decrypt(binary(), config_key()) ->
    {ok, #{msg := binary(), appid := binary()}} | {error, term()}.
decrypt(Encrypted, EncodingAESKey) ->
    case aes_key(EncodingAESKey) of
        {error, Reason} ->
            {error, Reason};
        {ok, Key} ->
            IV = binary:part(Key, 0, ?BLOCK_SIZE),
            case decode_ciphertext(Encrypted) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Cipher} ->
                    try crypto_one_time(Key, IV, Cipher, false) of
                        {ok, Padded} -> parse_plaintext(unpad(Padded));
                        {error, Reason} -> {error, Reason}
                    catch
                        _:_ -> {error, decrypt_failed}
                    end
            end
    end.

%% @doc 加密业务报文（安全模式下被动回复用；兼容模式回明文即可）。
%% 入参 Msg 是业务报文本身（JSON/XML 文本），本函数负责加盐、加长度前缀、
%% 拼 appid、补位、分组加密、base64。
-spec encrypt(binary(), config_key(), binary()) -> {ok, binary()} | {error, term()}.
encrypt(Msg, EncodingAESKey, AppId) when is_binary(Msg), is_binary(AppId) ->
    case aes_key(EncodingAESKey) of
        {error, Reason} ->
            {error, Reason};
        {ok, Key} ->
            IV = binary:part(Key, 0, ?BLOCK_SIZE),
            Plain =
                <<
                    (crypto:strong_rand_bytes(?RANDOM_PREFIX_SIZE))/binary,
                    (byte_size(Msg)):32/big,
                    Msg/binary,
                    AppId/binary
                >>,
            try crypto_one_time(Key, IV, pad(Plain), true) of
                {ok, Cipher} -> {ok, base64:encode(Cipher)};
                {error, Reason} -> {error, Reason}
            catch
                _:_ -> {error, encrypt_failed}
            end
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec sorted_concat([binary()]) -> binary().
sorted_concat(Items) ->
    iolist_to_binary(lists:sort([to_binary(I) || I <- Items])).

-spec sha1_hex(binary()) -> binary().
sha1_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha, Bin), lowercase).

-spec decode_ciphertext(binary()) -> {ok, binary()} | {error, invalid_ciphertext}.
decode_ciphertext(Encrypted) when is_binary(Encrypted), Encrypted =/= <<>> ->
    %% 用 re 而非 binary:replace，避免把 base64 文本里合法的 "+" 也动到；
    %% 只需把 URL-safe 的两个字符归一化回标准字母表。
    Normalized = url_safe_to_standard(Encrypted),
    try base64:decode(Normalized) of
        Cipher when byte_size(Cipher) > 0, byte_size(Cipher) rem ?BLOCK_SIZE =:= 0 ->
            {ok, Cipher};
        _ ->
            {error, invalid_ciphertext}
    catch
        _:_ -> {error, invalid_ciphertext}
    end;
decode_ciphertext(_) ->
    {error, invalid_ciphertext}.

-spec url_safe_to_standard(binary()) -> binary().
url_safe_to_standard(Bin) ->
    binary:replace(
        binary:replace(Bin, <<"-">>, <<"+">>, [global]),
        <<"_">>,
        <<"/">>,
        [global]
    ).

%% @doc AES-256-CBC 单次分组运算。补位由本模块手写，故显式关掉 crypto 的隐式补位。
-spec crypto_one_time(aes_key(), binary(), binary(), boolean()) ->
    {ok, binary()} | {error, term()}.
crypto_one_time(Key, IV, Data, Encrypt) ->
    try
        {ok,
            crypto:crypto_one_time(aes_256_cbc, Key, IV, Data, [
                {encrypt, Encrypt}, {padding, none}
            ])}
    catch
        _:_ -> {error, crypto_failed}
    end.

-spec pad(binary()) -> binary().
pad(Bin) ->
    PadLen = ?BLOCK_SIZE - (byte_size(Bin) rem ?BLOCK_SIZE),
    <<Bin/binary, (binary:copy(<<PadLen>>, PadLen))/binary>>.

%% @doc 剥 PKCS#7 补位，并**校验**补位自洽。
%% 不校验的话，密文被截断或密钥错误时也会「成功」返回一段垃圾，
%% 把问题推迟到下游解析 appid 时才暴露，甚至不暴露。
-spec unpad(binary()) -> {ok, binary()} | {error, bad_padding}.
unpad(<<>>) ->
    {error, bad_padding};
unpad(Bin) ->
    PadLen = binary:last(Bin),
    Size = byte_size(Bin),
    case PadLen >= 1 andalso PadLen =< ?BLOCK_SIZE andalso PadLen =< Size of
        true ->
            PayloadSize = Size - PadLen,
            <<_Payload:PayloadSize/binary, Pad:PadLen/binary>> = Bin,
            case Pad =:= binary:copy(<<PadLen>>, PadLen) of
                true -> {ok, binary:part(Bin, 0, PayloadSize)};
                false -> {error, bad_padding}
            end;
        false ->
            {error, bad_padding}
    end.

%% @doc 拆明文结构：random(16) || MsgLen(4, big) || Msg || AppId。
-spec parse_plaintext({ok, binary()} | {error, term()}) ->
    {ok, #{msg := binary(), appid := binary()}} | {error, term()}.
parse_plaintext({error, Reason}) ->
    {error, Reason};
parse_plaintext({ok, Plain}) ->
    MinSize = ?RANDOM_PREFIX_SIZE + ?MSG_LEN_SIZE,
    case Plain of
        <<_Random:?RANDOM_PREFIX_SIZE/binary, MsgLen:32/big, Rest/binary>> when
            byte_size(Rest) >= MsgLen
        ->
            <<Msg:MsgLen/binary, AppId/binary>> = Rest,
            case AppId of
                <<>> -> {error, missing_appid};
                _ -> {ok, #{msg => Msg, appid => AppId}}
            end;
        _ when byte_size(Plain) < MinSize ->
            {error, malformed_plaintext};
        _ ->
            {error, length_prefix_mismatch}
    end.

-spec to_binary(binary() | string() | atom()) -> binary().
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).
