-module(imboy_cipher).

-export([aes_encrypt/1,
         aes_decrypt/1]).
-export([aes_encrypt/2,
         aes_decrypt/2]).
-export([rsa_encrypt/1,
         rsa_decrypt/1]).
-export([rsa_encrypt/2,
         rsa_decrypt/2]).

-include("common.hrl").

-define(SHA_256_BLOCKSIZE, 64).


-spec rsa_encrypt(CipherText :: binary(), PrivKey :: binary()) ->
          PlainText :: binary().


-spec rsa_decrypt(PlainText :: list(), PubKey :: binary()) ->
          CipherText :: binary().

% aes_cbc + pkcs#7填充
% io:format("~s~n", [imboy_cipher:rsa_encrypt(<<"admin8889">>)]).
aes_encrypt(Bin) ->
    aes_encrypt(aes_256_cbc, Bin).


aes_decrypt(Bin) ->
    aes_decrypt(aes_256_cbc, Bin).


aes_encrypt(Type, Bin) ->
    Len = erlang:size(Bin),
    Value = 16 - (Len rem 16),
    % 将<<Value>>复制Value份赋值出来
    PadBin = binary:copy(<<Value>>, Value),
    Bin2 = <<Bin/binary, PadBin/binary>>,
    StateEnc = crypto:crypto_init(Type, ?AES_KEY, ?AES_IV, true),
    EncodeB = crypto:crypto_update(StateEnc, Bin2),
    base64:encode(EncodeB).


aes_decrypt(Type, Bin) ->
    Bin1 = base64:decode(Bin),
    StateDec = crypto:crypto_init(Type, ?AES_KEY, ?AES_IV, false),
    Bin2 = crypto_update(StateDec, Bin1, size(Bin1), <<>>),
    binary:part(Bin2, {0, size(Bin2) - binary:last(Bin2)}).


crypto_update(StateDec, Bin, BinSize, OutBin) when BinSize > 16 ->
    Bin2 = binary:part(Bin, {0, 16}),
    OutBin2 = crypto:crypto_update(StateDec, Bin2),
    OutBin3 = <<OutBin/binary, OutBin2/binary>>,
    Bin3 = binary:part(Bin, {16, BinSize - 16}),
    crypto_update(StateDec, Bin3, BinSize - 16, OutBin3);
crypto_update(StateDec, Bin, _BinSize, OutBin) ->
    OutBin2 = crypto:crypto_update(StateDec, Bin),
    <<OutBin/binary, OutBin2/binary>>.


rsa_encrypt(PlainText) when is_binary(PlainText) ->
    %%公钥加密
    PemBin = logic_config:get("login_rsa_pub_key"),
    rsa_encrypt(PlainText, PemBin);
rsa_encrypt(PlainText) ->
    %%公钥加密
    PemBin = logic_config:get("login_rsa_pub_key"),
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
    PemBin = logic_config:get("login_rsa_priv_key"),
    rsa_decrypt(CipherText, PemBin).


rsa_decrypt(CipherText, PrivKey) ->
    %%私钥解密
    BinData = base64:decode(CipherText),
    PrivateKey = get_rsa_key_str(PrivKey),
    Result = public_key:decrypt_private(BinData, PrivateKey),
    Result.


%% @fun 拿密钥内容
get_rsa_key_str(PemBin) ->
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).
