-module(elib_cipher).

-include("common.hrl").
-include("log.hrl").

%% @doc 加解密工具模块
%% 提供 AES 加密和 RSA 加密/解密功能
-export([
    aes_encrypt/3,
    aes_decrypt/3
]).
-export([
    aes_encrypt/4,
    aes_decrypt/4
]).
-export([
    rsa_encrypt/1,
    rsa_decrypt/1
]).
-export([
    rsa_encrypt/2,
    rsa_decrypt/2
]).
-export([safe_rsa_decrypt/2]).
-export([num_random/1]).

%% 密钥备份相关功能
-export([
    derive_master_password/2,
    encrypt_private_key/2,
    decrypt_private_key/2
]).
-export([encrypt_rsa_oaep/2, decrypt_rsa_oaep/2]).

-define(SHA_256_BLOCKSIZE, 64).

%% ===================================================================
%% AES 加密/解密
%% ===================================================================

%% @doc AES 加密（使用默认的 aes_256_cbc 模式和 PKCS#7 填充）
%% @param Bin 要加密的二进制数据
%% @param Key 加密密钥（二进制或列表，长度32字节）
%% @param IV 初始化向量（二进制或列表，长度16字节）
%% @returns Base64 编码的加密数据
%%
%% @example
%% CipherText = elib_cipher:aes_encrypt(<<"admin8889">>, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa").
%% PlainText = elib_cipher:aes_decrypt(CipherText, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa").
-spec aes_encrypt(binary(), binary() | list(), binary()) -> binary().
aes_encrypt(Bin, Key, IV) ->
    aes_encrypt(aes_256_cbc, Bin, Key, IV).

%% PlainText = elib_cipher:aes_decrypt(elib_cipher:aes_encrypt(<<"admin8889">>, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"), "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa").
-spec aes_decrypt(binary(), binary() | list(), binary()) -> binary().
aes_decrypt(Bin, Key, IV) ->
    aes_decrypt(aes_256_cbc, Bin, Key, IV).

%% @doc AES 加密（指定加密类型）
%% @param Type 加密类型（如 aes_256_cbc）
%% @param Bin 要加密的二进制数据
%% @param Key 加密密钥（二进制或列表）
%% @param IV 初始化向量（二进制或列表）
%% @returns Base64 编码的加密数据
-spec aes_encrypt(atom(), binary(), binary() | list(), binary()) -> binary().
aes_encrypt(Type, Bin, Key, IV) when is_binary(Key) ->
    aes_encrypt(Type, Bin, binary_to_list(Key), IV);
aes_encrypt(Type, Bin, Key, IV) ->
    Len = erlang:size(Bin),
    Value = 16 - (Len rem 16),
    % 将<<Value>>复制Value份赋值出来
    PadBin = binary:copy(<<Value>>, Value),
    Bin2 = <<Bin/binary, PadBin/binary>>,
    StateEnc = crypto:crypto_init(Type, Key, IV, true),
    EncodeB = crypto:crypto_update(StateEnc, Bin2),
    base64:encode(EncodeB).

%% @doc AES 解密（指定加密类型）
%% @param Type 解密类型（如 aes_256_cbc）
%% @param Bin Base64 编码的加密数据
%% @param Key 解密密钥（二进制或列表）
%% @param IV 初始化向量（二进制或列表）
%% @returns 解密后的原始二进制数据
-spec aes_decrypt(atom(), binary(), binary() | list(), binary()) -> binary().
aes_decrypt(Type, Bin, Key, IV) when is_binary(Key) ->
    aes_decrypt(Type, Bin, binary_to_list(Key), IV);
aes_decrypt(Type, Bin, Key, IV) ->
    Bin1 = base64:decode(Bin),
    StateDec = crypto:crypto_init(Type, Key, IV, false),
    Bin2 = crypto_update(StateDec, Bin1, size(Bin1), <<>>),
    binary:part(Bin2, {0, size(Bin2) - binary:last(Bin2)}).

%% ===================================================================
%% RSA 加密/解密
%% ===================================================================

%% @doc RSA 公钥加密（使用配置中的公钥）
%% @param PlainText 要加密的明文（二进制或列表）
%% @returns Base64 编码的加密数据或错误信息
%%
%% 此函数从配置中获取 login_rsa_pub_key 进行加密
-spec rsa_encrypt(binary() | list()) -> binary() | {error, term()}.
rsa_encrypt(PlainText) when is_binary(PlainText) ->
    %%公钥加密
    PemBin = config_ds:env(login_rsa_pub_key),
    true = is_binary(PemBin),
    rsa_encrypt(PlainText, PemBin);
rsa_encrypt(PlainText) when is_list(PlainText) ->
    %%公钥加密
    PemBin = config_ds:env(login_rsa_pub_key),
    true = is_binary(PemBin),
    BinData = list_to_binary(PlainText),
    rsa_encrypt(BinData, PemBin).

%% @doc RSA 公钥加密（指定公钥）
%% @param BinData 要加密的二进制数据
%% @param PemBin PEM 格式的公钥
%% @returns Base64 编码的加密数据或错误信息
%%
%% 🔒 安全升级：使用 RSA-OAEP-SHA256 填充
%% - 从 PKCS#1 v1.5 升级到 RSA-OAEP-SHA256
%% - 与前端 Dart/JS 加密方案保持一致
%% - 抗 Bleichenbacher 攻击
-spec rsa_encrypt(binary(), binary()) -> binary() | {error, term()}.
rsa_encrypt(BinData, PemBin) ->
    %%公钥加密
    try
        PublicKey = get_rsa_key_str(PemBin),
        % 🔒 使用 RSA-OAEP-SHA256 填充（与前端一致）
        Cipher = public_key:encrypt_public(
            BinData,
            PublicKey,
            [
                {rsa_padding, rsa_pkcs1_oaep_padding},
                {rsa_oaep_md, sha256},
                {rsa_mgf1_md, sha256}
            ]
        ),
        base64:encode(Cipher)
    catch
        _:_ -> {error, encrypt_failed}
    end.

%% @doc RSA 私钥解密（使用配置中的私钥）
%% @param CipherText Base64 编码的加密数据（二进制或列表）
%% @returns 解密后的明文，或配置错误时返回错误信息
%%
%% 此函数从配置中获取 login_rsa_priv_key 进行解密
-spec rsa_decrypt(binary() | list()) -> binary() | term().
rsa_decrypt(CipherText) ->
    %%私钥解密
    case config_ds:env(login_rsa_priv_key) of
        PemBin when is_binary(PemBin) ->
            rsa_decrypt(CipherText, PemBin);
        Error ->
            Error
    end.

%% @doc RSA 私钥解密（指定私钥）
%% @param CipherText Base64 编码的加密数据（支持 URL-safe Base64）
%% @param PrivKey PEM 格式的私钥
%% @returns 解密后的明文
%%
%% 支持特性：
%% - URL-safe Base64 格式（- 替换为 +，_ 替换为 /）
%% - 自动添加 Base64 填充
%% - URL 编码处理
%% - **OAEP v2.0/v2.1 兼容**：自动检测并处理两种格式
%%
%% 🔒 安全升级：使用 RSA-OAEP-SHA256 填充
%% - 从 PKCS#1 v1.5 升级到 RSA-OAEP-SHA256
%% - 支持 pointycastle (OAEP v2.0) 和 Erlang public_key (OAEP v2.1)
-spec rsa_decrypt(binary(), binary()) -> binary().
rsa_decrypt(CipherText, PrivKey) ->
    %%私钥解密
    % 处理可能的URL编码和URL-safe Base64格式
    DecodedText = decode_base64_text(CipherText),
    BinData = base64:decode(DecodedText),
    PrivateKey = get_rsa_key_str(PrivKey),

    % 优先使用 Erlang 标准 OAEP-SHA256 解密（兼容 WebCrypto / SubtleCrypto）
    case rsa_decrypt_oaep_v21(BinData, PrivateKey) of
        {ok, Result} ->
            Result;
        {error, _} ->
            rsa_decrypt_oaep_compat(BinData, PrivateKey, DecodedText)
    end.

%% @private OAEP v2.0/v2.1 兼容解密（历史兼容路径）
-spec rsa_decrypt_oaep_compat(binary(), term(), binary()) -> binary().
rsa_decrypt_oaep_compat(BinData, PrivateKey, _DecodedText) ->
    % 先进行裸 RSA 解密
    % pointycastle (OAEP v2.0): EM = maskedSeed || maskedDB (256 字节)
    % Erlang public_key (OAEP v2.1): EM = 0x00 || maskedSeed || maskedDB (256 字节)
    RawEM = public_key:decrypt_private(
        BinData,
        PrivateKey,
        [{rsa_padding, rsa_no_padding}]
    ),

    % RSA 解密可能返回比模数短的字节数（前导零被去除）
    % 2048 位 RSA
    KeySize = 256,
    RawEMLen = byte_size(RawEM),
    <<FirstByte:8, RestEM/binary>> = RawEM,

    case FirstByte of
        0 ->
            % 第一个字节是 0：尝试 v2.0（pointycastle 可能产生以 0 开头的 maskedSeed），失败则降级 v2.1
            try
                decode_oaep_common(RawEM)
            catch
                _:_:_ ->
                    % v2.0 失败，跳过第一个 0x00 字节尝试 v2.1
                    case RawEMLen of
                        KeySize ->
                            decode_oaep_common(RestEM);
                        _ ->
                            % 长度不足时先补齐再跳过第一个字节
                            PadLen = KeySize - RawEMLen,
                            PaddedEM = <<0:PadLen/unit:8, RawEM/binary>>,
                            <<0:8, FinalRest/binary>> = PaddedEM,
                            decode_oaep_common(FinalRest)
                    end
            end;
        _ ->
            % 第一个字节非 0，一定是 OAEP v2.0 (pointycastle)
            decode_oaep_common(RawEM)
    end.

%% @private 解码 Base64 文本（处理 URL 编码和 URL-safe 格式）
-spec decode_base64_text(binary()) -> binary().
decode_base64_text(CipherText) ->
    % 首先进行URL解码（如果包含%编码）
    DecodedText0 =
        case binary:match(CipherText, <<"%">>) of
            nomatch -> CipherText;
            _ -> uri_string:unquote(CipherText)
        end,
    % 某些 form-urlencoded 请求会把 '+' 还原为空格，需转回标准 Base64
    DecodedText = binary:replace(DecodedText0, <<" ">>, <<"+">>, [global]),
    % 确保Base64填充正确
    PaddingSize = (4 - (byte_size(DecodedText) rem 4)) rem 4,
    PaddedText =
        case PaddingSize of
            0 -> DecodedText;
            _ -> <<DecodedText/binary, (binary:copy(<<"=">>, PaddingSize))/binary>>
        end,
    % 将URL-safe Base64转换为标准Base64
    binary:replace(
        binary:replace(PaddedText, <<"-">>, <<"+">>, [global]),
        <<"_">>,
        <<"/">>,
        [global]
    ).

%% @private 使用 OAEP v2.1 (RFC 8017) 解密
-spec rsa_decrypt_oaep_v21(binary(), term()) -> {ok, binary()} | {error, term()}.
rsa_decrypt_oaep_v21(BinData, PrivateKey) ->
    try
        Result = public_key:decrypt_private(
            BinData,
            PrivateKey,
            [
                {rsa_padding, rsa_pkcs1_oaep_padding},
                {rsa_oaep_md, sha256},
                {rsa_mgf1_md, sha256}
            ]
        ),
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            _ = ?WARN_LOG(
                "OAEP v2.1 decrypt failed: ~p:~p~nStacktrace: ~p",
                [Class, Reason, Stacktrace]
            ),
            {error, oaep_v21_failed}
    end.

%% @private 通用的 OAEP 解码逻辑
-spec decode_oaep_common(binary()) -> binary().
decode_oaep_common(EM) ->
    % SHA-256
    HashLen = 32,

    case byte_size(EM) of
        Size when Size < 2 * HashLen + 1 ->
            throw({error, invalid_em_length});
        _ ->
            % 提取 maskedSeed 和 maskedDB
            <<MaskedSeed:HashLen/binary, MaskedDB/binary>> = EM,

            % 计算 seedMask = MGF(maskedDB, hLen)
            SeedMask = mgf1(MaskedDB, HashLen, sha256),

            % 计算 seed = maskedSeed XOR seedMask
            Seed = xor_bytes(MaskedSeed, SeedMask),

            % 计算 dbMask = MGF(seed, emLen - hLen)
            DBMask = mgf1(Seed, byte_size(MaskedDB), sha256),

            % 计算 DB = maskedDB XOR dbMask
            DB = xor_bytes(MaskedDB, DBMask),

            % 解析 DB = pHash || PS || 0x01 || M
            <<_PHash:HashLen/binary, Rest/binary>> = DB,

            % 找到 0x01 分隔符并提取消息
            extract_message(Rest, 0)
    end.

%% @private MGF1 (Mask Generation Function 1)
-spec mgf1(binary(), non_neg_integer(), atom()) -> binary().
mgf1(Seed, MaskLen, HashAlg) ->
    mgf1_loop(Seed, MaskLen, HashAlg, 0, <<>>).

%% @private MGF1 循环
-spec mgf1_loop(binary(), non_neg_integer(), atom(), non_neg_integer(), binary()) -> binary().
mgf1_loop(_Seed, MaskLen, _HashAlg, _Counter, Acc) when byte_size(Acc) >= MaskLen ->
    <<Result:MaskLen/binary, _/binary>> = Acc,
    Result;
mgf1_loop(Seed, MaskLen, HashAlg, Counter, Acc) ->
    % C = I2OSP(Counter, 4)
    C = <<Counter:32/big-unsigned-integer>>,
    % T = T || Hash(Seed || C)
    Hash = crypto:hash(HashAlg, <<Seed/binary, C/binary>>),
    mgf1_loop(Seed, MaskLen, HashAlg, Counter + 1, <<Acc/binary, Hash/binary>>).

%% @private 字节异或
-spec xor_bytes(binary(), binary()) -> binary().
xor_bytes(A, B) ->
    list_to_binary([X bxor Y || {X, Y} <- lists:zip(binary_to_list(A), binary_to_list(B))]).

%% @private 提取消息（跳过 PS 和 0x01 分隔符）
-spec extract_message(binary(), non_neg_integer()) -> binary().
extract_message(<<>>, _Pos) ->
    throw({error, invalid_padding});
extract_message(<<0, Rest/binary>>, Pos) ->
    extract_message(Rest, Pos + 1);
extract_message(<<1, Rest/binary>>, _Pos) ->
    Rest;
extract_message(_, _Pos) ->
    throw({error, invalid_padding}).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec crypto_update(term(), binary(), non_neg_integer(), binary()) -> binary().
crypto_update(StateDec, Bin, BinSize, OutBin) when BinSize > 16 ->
    Bin2 = binary:part(Bin, {0, 16}),
    OutBin2 = crypto:crypto_update(StateDec, Bin2),
    OutBin3 = <<OutBin/binary, OutBin2/binary>>,
    Bin3 = binary:part(Bin, {16, BinSize - 16}),
    crypto_update(StateDec, Bin3, BinSize - 16, OutBin3);
crypto_update(StateDec, Bin, _BinSize, OutBin) ->
    OutBin2 = crypto:crypto_update(StateDec, Bin),
    <<OutBin/binary, OutBin2/binary>>.

%% @fun 拿密钥内容
-spec get_rsa_key_str(binary()) -> term().
get_rsa_key_str(PemBin) ->
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).

%% ===================================================================
%% 安全的 RSA 解密
%% ===================================================================

%% @doc 安全的 RSA 解密，失败时返回空二进制
%% 用于处理 RSA 加密的密码，避免在每个 Handler 中重复 try-catch 逻辑
%% @param Password 加密的密码数据
%% @param AuthVersion 认证版本（<<"1">> 使用 RSA 解密，其他版本直接返回）
%% @returns 解密后的密码或空二进制
%%
%% @example
%% case elib_cipher:safe_rsa_decrypt(Password, <<"1">>) of
%%     <<>> -> error(invalid_password);
%%     DecryptedPwd -> process_password(DecryptedPwd)
%% end.
-spec safe_rsa_decrypt(binary(), binary()) -> binary().
safe_rsa_decrypt(<<>>, _) ->
    <<>>;
safe_rsa_decrypt(Password, <<"1">>) ->
    try rsa_decrypt(Password) of
        Pwd when is_binary(Pwd) -> Pwd;
        _ -> <<>>
    catch
        Class:Reason:Stacktrace ->
            _ = ?WARN_LOG(
                "RSA decrypt failed: ~p:~p~nStacktrace: ~p",
                [Class, Reason, Stacktrace]
            ),
            <<>>
    end;
safe_rsa_decrypt(Password, _) ->
    Password.

%% ===================================================================
%% 随机数生成
%% ===================================================================

%% @doc 生成指定位数的密码学安全随机整数
%% @param Len 位数（如 6 → 100000..999999）
%% @returns Len 位随机正整数
-spec num_random(pos_integer()) -> pos_integer().
num_random(Len) ->
    Min = round(math:pow(10, Len - 1)),
    Range = Min * 9,
    <<RandInt:64/unsigned-big>> = crypto:strong_rand_bytes(8),
    Min + (RandInt rem Range).

%% ===================================================================
%% 密钥备份相关功能
%% ===================================================================

%% @doc 使用 PBKDF2-SHA256 从主密码派生加密密钥
%% @param MasterPassword 用户主密码（二进制或列表）
%% @param Salt 盐值（至少 16 字节）
%% @returns {ok, DerivedKey} | {error, Reason}
%%
%% 使用 PBKDF2-SHA256 算法，100,000 次迭代
%% 派生 32 字节密钥（用于 AES-256 加密）
%%
%% @example
%% Salt = crypto:strong_rand_bytes(16),
%% {ok, Key} = elib_cipher:derive_master_password(<<"MyPassword123">>, Salt).
-spec derive_master_password(binary() | list(), binary()) -> {ok, binary()} | {error, term()}.
derive_master_password(MasterPassword, Salt) when is_list(MasterPassword) ->
    derive_master_password(list_to_binary(MasterPassword), Salt);
derive_master_password(<<>>, _Salt) ->
    {error, empty_password};
derive_master_password(_MasterPassword, Salt) when byte_size(Salt) < 16 ->
    {error, invalid_salt};
derive_master_password(MasterPassword, Salt) when is_binary(MasterPassword), is_binary(Salt) ->
    try
        % PBKDF2-HMAC-SHA256，100,000 次迭代，派生 32 字节密钥
        Iterations = 100000,
        DerivedKey = crypto:pbkdf2_hmac(sha256, MasterPassword, Salt, Iterations, 32),
        {ok, DerivedKey}
    catch
        _:_:_ ->
            {error, derivation_failed}
    end.

%% @doc 使用 AES-256-GCM 加密私钥
%% @param PrivateKey 要加密的私钥（PEM 格式或二进制）
%% @param EncryptionKey 加密密钥（32 字节）
%% @returns {ok, EncryptedData} | {error, Reason}
%%
%% 使用 AES-256-GCM 模式加密，每次加密使用随机 IV 和盐值
%% 返回格式: base64(salt + iv + ciphertext + tag)
%%
%% @example
%% Key = crypto:strong_rand_bytes(32),
%% {ok, Encrypted} = elib_cipher:encrypt_private_key(PrivateKeyPEM, Key).
-spec encrypt_private_key(binary(), binary()) -> {ok, binary()} | {error, term()}.
encrypt_private_key(PrivateKey, EncryptionKey) when
    is_binary(PrivateKey), byte_size(EncryptionKey) =:= 32
->
    try
        % 生成随机 IV (12 字节，GCM 推荐值)
        IV = crypto:strong_rand_bytes(12),
        % 生成随机盐值（用于密钥派生，增加安全性，也作为 AAD）
        Salt = crypto:strong_rand_bytes(16),

        % 使用 AES-256-GCM 加密（AEAD）
        % 返回: {Ciphertext, Tag}
        {Ciphertext, Tag} = crypto:crypto_one_time_aead(
            aes_256_gcm,
            EncryptionKey,
            IV,
            PrivateKey,
            % AAD (Additional Authenticated Data)
            Salt,
            % Tag 长度 (16 字节)
            16,
            % encrypt flag
            true
        ),

        % 组合: Salt(16) + IV(12) + Ciphertext + Tag(16)
        Combined = <<Salt/binary, IV/binary, Ciphertext/binary, Tag/binary>>,

        % Base64 编码
        {ok, base64:encode(Combined)}
    catch
        _:_:_ ->
            {error, encryption_failed}
    end;
encrypt_private_key(_PrivateKey, EncryptionKey) when byte_size(EncryptionKey) =/= 32 ->
    {error, invalid_key_length};
encrypt_private_key(_PrivateKey, _EncryptionKey) ->
    {error, invalid_input}.

%% @doc 使用 AES-256-GCM 解密私钥
%% @param EncryptedData 加密的私钥数据（Base64 编码）
%% @param EncryptionKey 解密密钥（32 字节）
%% @returns {ok, PrivateKey} | {error, Reason}
%%
%% @example
%% {ok, Decrypted} = elib_cipher:decrypt_private_key(EncryptedBase64, Key).
-spec decrypt_private_key(binary(), binary()) -> {ok, binary()} | {error, term()}.
decrypt_private_key(EncryptedData, EncryptionKey) when
    is_binary(EncryptedData), byte_size(EncryptionKey) =:= 32
->
    try
        % Base64 解码
        Combined = base64:decode(EncryptedData),

        % 提取各部分
        SaltSize = 16,
        IVSize = 12,
        TagSize = 16,
        TotalHeaderSize = SaltSize + IVSize,

        case byte_size(Combined) of
            Size when Size > TotalHeaderSize + TagSize ->
                <<Salt:SaltSize/binary, IV:IVSize/binary, Rest/binary>> = Combined,
                CipherTextSize = byte_size(Rest) - TagSize,
                <<CipherText:CipherTextSize/binary, Tag:TagSize/binary>> = Rest,

                % 使用 AES-256-GCM 解密
                case
                    crypto:crypto_one_time_aead(
                        aes_256_gcm,
                        EncryptionKey,
                        IV,
                        CipherText,
                        Salt,
                        Tag,
                        % decrypt flag
                        false
                    )
                of
                    error ->
                        % 认证失败（错误的密钥或数据被篡改）
                        {error, authentication_failed};
                    Plaintext when is_binary(Plaintext) ->
                        {ok, Plaintext}
                end;
            _ ->
                {error, invalid_data_format}
        end
    catch
        _:_ ->
            {error, decryption_failed}
    end;
decrypt_private_key(_EncryptedData, EncryptionKey) when byte_size(EncryptionKey) =/= 32 ->
    {error, invalid_key_length};
decrypt_private_key(_EncryptedData, _EncryptionKey) ->
    {error, invalid_input}.

%% ===================================================================
%% RSA-OAEP 加密/解密
%% ===================================================================

%% @doc RSA-OAEP 公钥加密
%% @param PlainText 要加密的明文（二进制）
%% @param PublicKeyPem PEM 格式的公钥
%% @returns Base64 编码的加密数据
%%
%% 使用 RSA-OAEP-SHA256 填充模式进行加密
%% @example
%% PublicKeyPem = <<"-----BEGIN PUBLIC KEY...">>,
%% {ok, Encrypted} = elib_cipher:encrypt_rsa_oaep(<<"secret">>, PublicKeyPem).
-spec encrypt_rsa_oaep(binary(), binary()) -> {ok, binary()} | {error, term()}.
encrypt_rsa_oaep(PlainText, PublicKeyPem) when is_binary(PlainText), is_binary(PublicKeyPem) ->
    try
        PublicKey = get_rsa_key_str(PublicKeyPem),
        % 使用 RSA-OAEP-SHA256 填充
        Encrypted = public_key:encrypt_public(
            PlainText,
            PublicKey,
            [
                {rsa_padding, rsa_pkcs1_oaep_padding},
                {rsa_oaep_md, sha256},
                {rsa_mgf1_md, sha256}
            ]
        ),
        {ok, base64:encode(Encrypted)}
    catch
        _:_:_ ->
            {error, encryption_failed}
    end;
encrypt_rsa_oaep(_PlainText, _PublicKeyPem) ->
    {error, invalid_input}.

%% @doc RSA-OAEP 私钥解密
%% @param CipherText Base64 编码的加密数据
%% @param PrivateKeyPem PEM 格式的私钥
%% @returns 解密后的明文
%%
%% 使用 RSA-OAEP-SHA256 填充模式进行解密
%% @example
%% PrivateKeyPem = <<"-----BEGIN PRIVATE KEY...">>,
%% {ok, Decrypted} = elib_cipher:decrypt_rsa_oaep(EncryptedBase64, PrivateKeyPem).
-spec decrypt_rsa_oaep(binary(), binary()) -> {ok, binary()} | {error, term()}.
decrypt_rsa_oaep(CipherText, PrivateKeyPem) when is_binary(CipherText), is_binary(PrivateKeyPem) ->
    try
        Encrypted = base64:decode(CipherText),
        PrivateKey = get_rsa_key_str(PrivateKeyPem),
        % 使用 RSA-OAEP-SHA256 填充解密
        Decrypted = public_key:decrypt_private(
            Encrypted,
            PrivateKey,
            [
                {rsa_padding, rsa_pkcs1_oaep_padding},
                {rsa_oaep_md, sha256},
                {rsa_mgf1_md, sha256}
            ]
        ),
        {ok, Decrypted}
    catch
        _:_:_ ->
            {error, decryption_failed}
    end;
decrypt_rsa_oaep(_CipherText, _PrivateKeyPem) ->
    {error, invalid_input}.
