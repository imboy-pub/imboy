-module (imboy_cipher).

-export([aes_encrypt/1, aes_decrypt/1]).
-export([rsa_encrypt/1, rsa_decrypt/1]).
-export([rsa_encrypt/2, rsa_decrypt/2]).
-export([password_hash/1, password_verify/2]).

-include("imboy.hrl").

-spec rsa_encrypt(CipherText :: binary(), PrivKey :: binary()) -> PlainText :: binary().
-spec rsa_decrypt(PlainText :: list(), PubKey :: binary()) -> CipherText :: binary().

% aes128 + pkcs#7填充
aes_encrypt(Bin) ->
    Len = erlang:size(Bin),
    Value = 16 - (Len rem 16),
    PadBin = binary:copy(<<Value>>, Value),
    % io:format("~p, ~p~n", [?AES_KEY, ?AES_IV]),
    EncodeB = crypto:block_encrypt(aes_cbc128, list_to_binary(?AES_KEY), list_to_binary(?AES_IV), <<Bin/binary, PadBin/binary>>),
    base64:encode(EncodeB).

% aes128 + pkcs#7填充
aes_decrypt(Bin) ->
    Bin1 = base64:decode(Bin),
    case erlang:size(Bin1) rem 16 of
        0 ->
            Bin2 = crypto:block_decrypt(aes_cbc128, list_to_binary(?AES_KEY), list_to_binary(?AES_IV), Bin1),
            binary:part(Bin2, {0, byte_size(Bin2) - binary:last(Bin2)});
        _ ->
            {error, 1102}
    end.

password_hash(Pwd) ->
    sha512(Pwd).

password_verify(Pwd, Hash) when is_binary(Hash)  ->
    sha512(Pwd) == Hash;
password_verify(Pwd, Hash) when is_list(Hash)  ->
    sha512(Pwd) == list_to_binary(Hash).

sha512(PlainText) ->
    {ok, Key} = application:get_env(imboy, imboy_cipher_key),
    sha512(PlainText, Key).
sha512(PlainText, Key) ->
    Bin = crypto:hmac(sha512, Key, PlainText),
    base64:encode(Bin).



rsa_encrypt(PlainText) when is_binary(PlainText) ->
    %%公钥加密
    PemBin = config_as:get("login_rsa_pub_key"),
    rsa_encrypt(PlainText, PemBin);
rsa_encrypt(PlainText) ->
    %%公钥加密
    PemBin = config_as:get("login_rsa_pub_key"),
    BinData = list_to_binary(PlainText),
    rsa_encrypt(BinData, PemBin).

rsa_encrypt(BinData, PemBin) ->
    %%公钥加密
    PublicKey = get_rsa_key_str(PemBin),
    Cipher = public_key:encrypt_public(BinData, PublicKey),
    base64:encode(Cipher).

rsa_decrypt(CipherText) ->
    %%私钥解密
    PemBin = config_as:get("login_rsa_priv_key"),
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
