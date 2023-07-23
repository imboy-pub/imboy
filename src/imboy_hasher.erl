-module(imboy_hasher).

-export([md5/1]).
-export([hmac_sha512/2]).

-export([encoded_val/1, decoded_payload/0, decoded_field/1]).

-define(SHA_256_BLOCKSIZE, 64).


%% ===================================================================
%% API
%% ===================================================================

% Payload2 = imboy_hasher:encoded_val(Val),
-spec encoded_val(list() | binary() | map()) ->binary().
encoded_val(Val) ->
    Key = config_ds:env(postgre_aes_key),
    Val1 = base64:encode(Val),
    <<"encode(encrypt('", Val1/binary, "', '", Key/binary, "', 'aes-cbc/pad:pkcs'), 'base64')">>.

% P = imboy_hasher:decoded_payload(),
-spec decoded_payload() ->binary().
decoded_payload() ->
    decoded_field(<<"payload">>).

decoded_field(Field) ->
    %在 public.config 表里面的 value 加密时使用了前缀 aes_cbc_
    Key = config_ds:env(postgre_aes_key),
    <<"decode(encode(decrypt(decode(replace(", Field/binary, ", 'aes_cbc_', ''),'base64'), '", Key/binary, "', 'aes-cbc/pad:pkcs') , 'escape'), 'base64') as ", Field/binary>>.

%% erlang md5 16进制字符串
md5(Str) ->
    Sig = erlang:md5(Str),
    iolist_to_binary([io_lib:format("~2.16.0b", [S]) ||
                         S <- binary_to_list(Sig)]).


-spec hmac_sha512(PlainText :: list(), Key :: list()) ->
          Ciphertext :: binary().
hmac_sha512(PlainText, Key) ->
    Bin = crypto:macN(hmac, sha512, Key, PlainText, ?SHA_256_BLOCKSIZE),
    base64:encode(Bin).
