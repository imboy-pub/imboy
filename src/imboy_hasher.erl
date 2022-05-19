-module(imboy_hasher).

-export([md5/1]).
-export([hmac_sha512/2]).

-define(SHA_256_BLOCKSIZE, 64).


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
