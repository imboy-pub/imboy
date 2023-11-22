-module(imboy_cipher).

-include_lib("imlib/include/common.hrl").

-export([aes_encrypt/3,
         aes_decrypt/3]).
-export([aes_encrypt/4,
         aes_decrypt/4]).
-export([rsa_encrypt/1,
         rsa_decrypt/1]).
-export([rsa_encrypt/2,
         rsa_decrypt/2]).

-define(SHA_256_BLOCKSIZE, 64).


% aes_cbc + pkcs#7填充
% io:format("~s~n", [imboy_cipher:aes_encrypt(<<"admin8889">>, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa")]).
aes_encrypt(Bin, Key, IV) ->
    aes_encrypt(aes_256_cbc, Bin, Key, IV).


% imboy_cipher:aes_decrypt(base64:decode(Va)l, config_ds:env(postgre_aes_key), <<>>).
aes_decrypt(Bin, Key, IV) ->
    aes_decrypt(aes_256_cbc, Bin, Key, IV).


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


aes_decrypt(Type, Bin, Key, IV) when is_binary(Key) ->
    aes_decrypt(Type, Bin, binary_to_list(Key), IV);
aes_decrypt(Type, Bin, Key, IV) ->
    Bin1 = base64:decode(Bin),
    IV = config_ds:env(solidified_key_iv),
    StateDec = crypto:crypto_init(Type, Key, IV, false),
    Bin2 = crypto_update(StateDec, Bin1, size(Bin1), <<>>),
    binary:part(Bin2, {0, size(Bin2) - binary:last(Bin2)}).


-spec rsa_encrypt(CipherText :: binary(), PrivKey :: binary()) -> PlainText :: binary().


-spec rsa_decrypt(PlainText :: list(), PubKey :: binary()) -> CipherText :: binary().
rsa_encrypt(PlainText) when is_binary(PlainText) ->
    %%公钥加密
    PemBin = config_ds:get("login_rsa_pub_key"),
    rsa_encrypt(PlainText, PemBin);
rsa_encrypt(PlainText) ->
    %%公钥加密
    PemBin = config_ds:get("login_rsa_pub_key"),
    BinData = list_to_binary(PlainText),
    rsa_encrypt(BinData, PemBin).


rsa_encrypt(BinData, PemBin) ->
    %%公钥加密
    PublicKey = get_rsa_key_str(PemBin),
    Cipher = public_key:encrypt_public(BinData, PublicKey),
    base64:encode(Cipher).


-spec rsa_decrypt(CipherText :: binary()) -> any().
rsa_decrypt(CipherText) ->
    %%私钥解密
    PemBin = config_ds:get("login_rsa_priv_key"),
    rsa_decrypt(CipherText, PemBin).


rsa_decrypt(CipherText, PrivKey) ->
    %%私钥解密
    BinData = base64:decode(CipherText),
    PrivateKey = get_rsa_key_str(PrivKey),
    Result = public_key:decrypt_private(BinData, PrivateKey),
    Result.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


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
get_rsa_key_str(PemBin) ->
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).
