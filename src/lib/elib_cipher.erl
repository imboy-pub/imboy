-module(elib_cipher).

-include("common.hrl").
-include("log.hrl").

%% @doc 加解密工具模块
%% 提供 AES 加密和 RSA 加密/解密功能
-export([aes_encrypt/3,
         aes_decrypt/3]).
-export([aes_encrypt/4,
         aes_decrypt/4]).
-export([rsa_encrypt/1,
         rsa_decrypt/1]).
-export([rsa_encrypt/2,
         rsa_decrypt/2]).
-export([safe_rsa_decrypt/2]).
-export([num_random/1]).

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
    PemBin = config_ds:get(<<"login_rsa_pub_key">>),
    true = is_binary(PemBin),
    rsa_encrypt(PlainText, PemBin);
rsa_encrypt(PlainText) when is_list(PlainText) ->
    %%公钥加密
    PemBin = config_ds:get(<<"login_rsa_pub_key">>),
    true = is_binary(PemBin),
    BinData = list_to_binary(PlainText),
    rsa_encrypt(BinData, PemBin).


%% @doc RSA 公钥加密（指定公钥）
%% @param BinData 要加密的二进制数据
%% @param PemBin PEM 格式的公钥
%% @returns Base64 编码的加密数据或错误信息
-spec rsa_encrypt(binary(), binary()) -> binary() | {error, term()}.
rsa_encrypt(BinData, PemBin) ->
    %%公钥加密
    try
        PublicKey = get_rsa_key_str(PemBin),
        Cipher = public_key:encrypt_public(BinData, PublicKey),
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
    case config_ds:get(<<"login_rsa_priv_key">>) of
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
-spec rsa_decrypt(binary(), binary()) -> binary().
rsa_decrypt(CipherText, PrivKey) ->
    %%私钥解密
    % 处理可能的URL编码和URL-safe Base64格式
    % 首先进行URL解码（如果包含%编码）
    DecodedText = case binary:match(CipherText, <<"%">>) of
        nomatch ->
            CipherText;
        _ ->
            % 包含URL编码，先解码
            list_to_binary(uri_string:unquote(binary_to_list(CipherText)))
    end,
    % 确保Base64填充正确（missing_padding错误）
    % 计算需要的填充
    PaddingSize = (4 - (byte_size(DecodedText) rem 4)) rem 4,
    PaddedText = case PaddingSize of
        0 -> DecodedText;
        _ -> <<DecodedText/binary, (binary:copy(<<"=">>, PaddingSize))/binary>>
    end,
    % 将URL-safe Base64转换为标准Base64（处理可能存在的-case）
    StandardBase64 = binary:replace(
        binary:replace(PaddedText, <<"-">>, <<"+">>, [global]),
        <<"_">>, <<"/">>, [global]
    ),
    BinData = base64:decode(StandardBase64),
    PrivateKey = get_rsa_key_str(PrivKey),
    Result = public_key:decrypt_private(BinData, PrivateKey),
    Result.


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
safe_rsa_decrypt(Password, <<"1">>) ->
    try rsa_decrypt(Password) of
        Pwd when is_binary(Pwd) -> Pwd;
        _ -> <<>>
    catch
        Class:Reason:Stacktrace ->
            _ = ?WARN_LOG("RSA decrypt failed: ~p:~p~nStacktrace: ~p",
                          [Class, Reason, Stacktrace]),
            <<>>
    end;
safe_rsa_decrypt(Password, _) ->
    Password.

%% ===================================================================
%% 随机数生成
%% ===================================================================

%% @doc 生成指定位数的随机数
%% 用于生成安全相关的随机值，如密码盐值、验证码等
%% @param Len 随机数的位数
%% @returns 生成的随机数
%%
%% @example
%% Salt = elib_cipher:num_random(40).  %% 生成 40 位随机数（盐值）
%% Code = elib_cipher:num_random(6).   %% 生成 6 位随机数（验证码）
-spec num_random(pos_integer()) -> pos_integer().
num_random(Len) ->
    Prefix = rand:uniform(9),
    MinNum = round(math:pow(10, Len - 1)),
    Num = rand:uniform(MinNum),
    case Num > MinNum of
        true ->
            Num;
        _ ->
            MinNum * Prefix + Num
    end.
