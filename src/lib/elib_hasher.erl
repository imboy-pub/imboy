-module(elib_hasher).

-export([md5/1]).
-export([
    hmac_sha256/2,
    hmac_sha512/2
]).

-export([
    encoded_val/1,
    decoded_val/1,
    decode_list_field/2
]).

-include("log.hrl").

%% 应用层密文前缀（AES-256-GCM，随机 IV，base64）
-define(CIPHER_PREFIX, "aesg1_").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 把任意值加密成可直接存入普通 text 列的密文（A-05，审计缺陷 #2）
%%
%% 历史实现拼的是一段含真实 `postgre_aes_key` 的 pgcrypto SQL 表达式字符串。
%% 由于 elib_pg_sql:unzip_map/1 只把 {raw, Sql} 元组拼进 SQL、普通 binary 一律
%% 走绑定参数，那段字符串被当**字面值**写进了 user_collect.info：
%%   1) 任意用户收藏一次 → SELECT info 即得全站主密钥；
%%   2) 加密同时静默失效 —— 存的一直是明文表达式。
%% 现改为应用层 AES-256-GCM（AEAD + 随机 IV），密钥不再出现在 SQL 文本里。
%%
%% ⚠️ 产出是密文：调用方对该列的 LIKE 检索恒不命中（改造前存的是 base64
%%    明文表达式，LIKE 同样不命中，故非回归）。需要检索请另建明文索引列。
%%
%% 密钥缺失或长度非法时直接 error（fail-closed），绝不回落明文落库。
-spec encoded_val(list() | binary() | map()) -> binary().
encoded_val(Val) when is_map(Val); is_list(Val) ->
    encoded_val(jsone:encode(Val, [native_utf8]));
encoded_val(Val) when is_binary(Val) ->
    case elib_cipher:aes_gcm_encrypt(Val, aes_key()) of
        {ok, Cipher} ->
            <<?CIPHER_PREFIX, Cipher/binary>>;
        {error, Reason} ->
            erlang:error({encoded_val_failed, Reason})
    end.

%% @doc encoded_val/1 的逆运算，兼容三种历史形态
%%
%% 1. <<"aesg1_", _/binary>>        → A-05 之后的应用层密文
%% 2. <<"encode(encrypt('", _>>     → A-05 之前的脏数据：从未真正加密，
%%                                    内层就是 base64(明文)，这里只做读兼容；
%%                                    落库清洗见迁移 00000053 与
%%                                    scripts/recrypt_user_collect.escript
%% 3. 其它                          → 明文（迁移清洗后的形态）原样返回
-spec decoded_val(binary()) -> binary().
decoded_val(<<?CIPHER_PREFIX, Cipher/binary>>) ->
    case elib_cipher:aes_gcm_decrypt(Cipher, aes_key()) of
        {ok, Plain} ->
            Plain;
        {error, Reason} ->
            % 认证失败/密钥不匹配一律返回空，不回落密文给客户端
            _ = ?WARN_LOG(["decoded_val failed ", Reason]),
            <<>>
    end;
decoded_val(<<"encode(encrypt('", Rest/binary>>) ->
    legacy_literal_plaintext(Rest);
decoded_val(Val) when is_binary(Val) ->
    Val.

%% @doc 对结果集每一行的 Field 字段做 decoded_val/1
%% 用于替换历史的 decoded_field/1（把密钥内联进 SQL 列表达式，审计 #26）
-spec decode_list_field(list(), binary()) -> list().
decode_list_field(List, Field) when is_list(List) ->
    [decode_row_field(Row, Field) || Row <- List];
decode_list_field(List, _Field) ->
    List.

%% erlang md5 16进制字符串
% elib_hasher:md5("ddd").
-spec md5(binary() | list()) -> binary().
md5(Str) ->
    Sig = erlang:md5(Str),
    iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)]).

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

%% ===================================================================
%% Internal
%% ===================================================================

%% @private 取主密钥，fail-closed：缺失或长度非 32 字节直接 error
%% 注意：任何分支都不得把密钥本身写进日志或异常 reason
-spec aes_key() -> binary().
aes_key() ->
    case config_ds:env(postgre_aes_key) of
        Key when is_binary(Key), byte_size(Key) =:= 32 ->
            Key;
        Key when is_list(Key), length(Key) =:= 32 ->
            list_to_binary(Key);
        _ ->
            erlang:error(invalid_postgre_aes_key)
    end.

%% @private 从历史脏数据里抠出明文
%% 形态：encode(encrypt('<base64(明文)>', '<主密钥>', 'aes-cbc/pad:pkcs'), 'base64')
-spec legacy_literal_plaintext(binary()) -> binary().
legacy_literal_plaintext(Rest) ->
    %% 与迁移 00000053 的 split_part(substr(info, 17), '''', 1) 语义对齐：
    %% 取到下一个单引号为止；截断行没有收尾引号时取全部剩余，而不是判定失败。
    %% 不对齐会出现「迁移前读不出、迁移后读得出」的窗口期不一致。
    [B64 | _] = binary:split(Rest, <<"'">>),
    try
        base64:decode(B64)
    catch
        _:_ -> <<>>
    end.

-spec decode_row_field(term(), binary()) -> term().
decode_row_field(Row, Field) when is_map(Row) ->
    case maps:find(Field, Row) of
        {ok, Val} when is_binary(Val) ->
            Row#{Field => decoded_val(Val)};
        _ ->
            Row
    end;
decode_row_field(Row, _Field) ->
    Row.
