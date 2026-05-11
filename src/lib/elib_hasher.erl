-module(elib_hasher).

-export([md5/1]).
-export([hmac_sha256/2,
         hmac_sha512/2]).

-export([encoded_val/1,
         decoded_payload/0,
         decoded_field/1]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================


% Payload2 = elib_hasher:encoded_val(Val),
-spec encoded_val(list() | binary() | map()) -> binary().
encoded_val(Val) when is_map(Val); is_list(Val) ->
    encoded_val(jsone:encode(Val, [native_utf8]));
encoded_val(Val) ->
    Key = config_ds:env(postgre_aes_key),
    Val1 = base64:encode(Val),
    % Val1 = ec_cnv:to_binary(Val),
    % ?DEBUG_LOG(["encoded_val ", Val, " val1 ", Val1]),
    <<"encode(encrypt('", Val1/binary, "', '", Key/binary, "', 'aes-cbc/pad:pkcs'), 'base64')">>.


% P = elib_hasher:decoded_payload(),
-spec decoded_payload() -> binary().
decoded_payload() ->
    decoded_field(<<"payload">>).

-spec decoded_field(binary()) -> binary().
decoded_field(Field) ->
    %在 public.config 表里面的 value 加密时使用了前缀 aes_cbc_
    Key = config_ds:env(postgre_aes_key),
    <<"decode(encode(decrypt(decode(replace(", Field/binary, ", 'aes_cbc_', ''),'base64'), '", Key/binary,
      "', 'aes-cbc/pad:pkcs') , 'escape'), 'base64') as ", Field/binary>>.


%% erlang md5 16进制字符串
% elib_hasher:md5("ddd").
-spec md5(binary() | list()) -> binary().
md5(Str) ->
    Sig = erlang:md5(Str),
    iolist_to_binary([ io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig) ]).

%% 兼容历史调用入口，保留旧的 hash/1 名称。
-spec hash(binary() | list()) -> binary().
hash(Str) ->
    md5(Str).

% elib_hasher:hmac_sha512("", "").
-spec hmac_sha512(binary(), binary() | iolist()) -> binary().
hmac_sha512(PlainText, Key) ->
    % Bin = crypto:macN(hmac, sha512, Key, PlainText, ?SHA_256_BLOCKSIZE),
    Bin = crypto:mac(hmac, sha512, Key, PlainText),
    base64:encode(Bin).


% elib_hasher:hmac_sha256("", "").
-spec hmac_sha256(binary(), binary() | iolist()) -> binary().
hmac_sha256(PlainText, Key) ->
    Bin = crypto:mac(hmac, sha256, Key, PlainText),
    base64:encode(Bin).
