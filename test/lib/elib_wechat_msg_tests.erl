%% elib_wechat_msg_tests
%%
%% 微信「消息推送」加解密原语的**跨实现**验证。
%%
%% 关键点：固定向量不是本模块自产的，而是由 /tmp 侧一份用 OpenSSL CLI
%% （`openssl enc -aes-256-cbc -nopad`）+ Python base64/hashlib 独立实现的脚本
%% 生成（gen_wechat_msg_vectors.py）。同一套规范由两条完全不同的技术栈各自
%% 实现，再断言逐字节相等 —— 这样证明的是「与规范一致」，而不是「与自己的
%% 加密端一致」。自往返（encrypt→decrypt）只能证明自洽，一个 IV 用错或补位
%% 口径错位的实现同样能自往返通过。
%%
%% 另有一条独立检查：encrypt/3 的产物用**手写 CBC**（ECB 单块 + 手工异或链）
%% 反解并逐字段核对明文结构。ECB 单块解密是与本模块不同的代码路径，能捕获
%% IV 取错、链式异或写反、补位漏做这类错。

-module(elib_wechat_msg_tests).

-include_lib("eunit/include/eunit.hrl").

%% 全部为测试专用常量，与生产无关（生产 Token/AESKey 只存在于 gitignored
%% 的 config/sys.pro.config，绝不入库）。
-define(TOKEN, <<"testtoken1234567890">>).
%% base64(bytes(range(1,33))) 去掉尾部 "=" ⇒ 43 字符，与微信后台给出的形态一致
-define(AES_KEY_TEXT, <<"AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyA">>).
-define(AES_KEY_HEX, <<"0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20">>).
%% 另一把结构合法但内容不同的密钥，用于「密钥错必须失败」的用例
-define(WRONG_AES_KEY_TEXT, <<"ISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0A">>).
-define(APPID, <<"wxzzzzzzzzzzzzzzzz">>).
-define(TIMESTAMP, <<"1789531800">>).
-define(NONCE, <<"a1b2c3d4">>).
-define(SIG_PLAIN, <<"eaee55865d33d15605f1e134dc8472da48108874">>).
-define(SIG_ENCRYPT, <<"038a7aef52cf621e2566d0c88e7fa91b700fd064">>).
-define(BLOCK_SIZE, 16).

%%%===================================================================
%%% 固定向量（OpenSSL 侧产出，逐字节比对）
%%%===================================================================

signature_plain_vector_test() ->
    ?assertEqual(
        ?SIG_PLAIN,
        elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE)
    ).

signature_encrypt_vector_test() ->
    ?assertEqual(
        ?SIG_ENCRYPT,
        elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE, encrypt_b64())
    ).

%% 字典序排序语义：Token / Timestamp / Nonce 三者的位置不影响结果。
%% 这条独立于固定向量 —— 即使向量整体被换掉，排序语义错了它也会红。
signature_is_order_invariant_test() ->
    Expected = elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE),
    ?assertEqual(Expected, elib_wechat_msg:signature(?NONCE, ?TOKEN, ?TIMESTAMP)),
    ?assertEqual(Expected, elib_wechat_msg:signature(?TIMESTAMP, ?NONCE, ?TOKEN)).

%% 密文场景下第四个参与排序的是 Encrypt，不是别的。改一个字符必须变。
signature_changes_with_encrypt_test() ->
    ?assertNotEqual(
        elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE, encrypt_b64()),
        elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE, <<"other">>)
    ).

%%%===================================================================
%%% 解密：OpenSSL 侧密文 → Erlang 侧明文
%%%===================================================================

decrypt_openssl_vector_test() ->
    ?assertEqual(
        {ok, #{msg => msg_binary(), appid => ?APPID}},
        elib_wechat_msg:decrypt(encrypt_b64(), ?AES_KEY_TEXT)
    ).

%% 微信在个别通道会把 base64 转成 URL-safe 字母表（-/_ 替代 +//）。
%% 归一化必须只动这两个字符，不能顺手把合法的 "+" 也换掉。
decrypt_url_safe_ciphertext_test() ->
    UrlSafe = binary:replace(
        binary:replace(encrypt_b64(), <<"+">>, <<"-">>, [global]),
        <<"/">>,
        <<"_">>,
        [global]
    ),
    ?assertNotEqual(encrypt_b64(), UrlSafe),
    ?assertEqual(
        {ok, #{msg => msg_binary(), appid => ?APPID}},
        elib_wechat_msg:decrypt(UrlSafe, ?AES_KEY_TEXT)
    ).

%% 44 字符（自带 "="）形态要能解出同一把密钥
decrypt_accepts_padded_key_text_test() ->
    Padded = <<?AES_KEY_TEXT/binary, "=">>,
    ?assertEqual(byte_size(Padded), 44),
    ?assertEqual(
        {ok, #{msg => msg_binary(), appid => ?APPID}},
        elib_wechat_msg:decrypt(encrypt_b64(), Padded)
    ).

%%%===================================================================
%%% 失败必须 fail-closed（不能返回一段「看起来合理」的垃圾）
%%%===================================================================

%% 密文被改一个字符：签名校验早已拦下，但即便直调 decrypt 也不能成功
decrypt_tampered_ciphertext_test() ->
    Tampered = tamper_first_char(encrypt_b64()),
    ?assertMatch({error, _}, elib_wechat_msg:decrypt(Tampered, ?AES_KEY_TEXT)).

%% 密钥错：补位自洽校验必须拦住（否则会解出结构合法但内容错位的报文）
decrypt_wrong_key_test() ->
    ?assertMatch({error, _}, elib_wechat_msg:decrypt(encrypt_b64(), ?WRONG_AES_KEY_TEXT)).

decrypt_empty_and_garbage_test() ->
    ?assertMatch({error, _}, elib_wechat_msg:decrypt(<<>>, ?AES_KEY_TEXT)),
    ?assertMatch({error, _}, elib_wechat_msg:decrypt(<<"not base64 at all!!!">>, ?AES_KEY_TEXT)).

%%%===================================================================
%%% 密钥解析
%%%===================================================================

aes_key_decodes_43_chars_test() ->
    ?assertEqual({ok, hex_to_bin(?AES_KEY_HEX)}, elib_wechat_msg:aes_key(?AES_KEY_TEXT)).

%% 长度不对必须直接失败：少一个字符会**静默**解出另一把 31 字节密钥，
%% 表现为「解密出来的东西看似合理实则错位」，比报错难查得多。
aes_key_rejects_wrong_length_test() ->
    ?assertEqual(
        {error, invalid_aes_key},
        elib_wechat_msg:aes_key(binary:part(?AES_KEY_TEXT, 0, 42))
    ),
    ?assertEqual({error, invalid_aes_key}, elib_wechat_msg:aes_key(<<>>)),
    ?assertEqual({error, invalid_aes_key}, elib_wechat_msg:aes_key(<<"short">>)).

%% 尺寸检查看的是**解码后**的字节数，不是输入文本长度：
%%   43 字符（微信形态）→ 补 "=" 后 44 字符，尾组带 1 个 "=" ⇒ 32 字节 ✅
%%   44 字符且**不带** "=" ⇒ 11 个完整分组 ⇒ 33 字节 ❌ 必须拒
%% 两者一起证明长度检查没有偷懒成「只看字符串长度」。
aes_key_checks_decoded_size_test() ->
    ?assertMatch(
        {ok, Key} when byte_size(Key) =:= 32,
        elib_wechat_msg:aes_key(binary:copy(<<"A">>, 43))
    ),
    ?assertEqual({error, invalid_aes_key}, elib_wechat_msg:aes_key(binary:copy(<<"A">>, 44))).

%%%===================================================================
%%% 验签（含定长比较）
%%%===================================================================

verify_plain_test() ->
    ?assert(elib_wechat_msg:verify(?TOKEN, ?TIMESTAMP, ?NONCE, ?SIG_PLAIN)),
    ?assertNot(elib_wechat_msg:verify(?TOKEN, ?TIMESTAMP, ?NONCE, <<"deadbeef">>)),
    %% 签名缺失/为空一律 false，不发散、不抛错
    ?assertNot(elib_wechat_msg:verify(?TOKEN, ?TIMESTAMP, ?NONCE, <<>>)),
    ?assertNot(elib_wechat_msg:verify(?TOKEN, ?TIMESTAMP, ?NONCE, undefined)).

verify_encrypt_test() ->
    ?assert(elib_wechat_msg:verify(?TOKEN, ?TIMESTAMP, ?NONCE, encrypt_b64(), ?SIG_ENCRYPT)),
    ?assertNot(elib_wechat_msg:verify(?TOKEN, ?TIMESTAMP, ?NONCE, encrypt_b64(), <<"x">>)),
    ?assertNot(
        elib_wechat_msg:verify(?TOKEN, ?TIMESTAMP, ?NONCE, <<"other">>, ?SIG_ENCRYPT)
    ).

secure_equal_test() ->
    ?assert(elib_wechat_msg:secure_equal(<<"abc">>, <<"abc">>)),
    ?assertNot(elib_wechat_msg:secure_equal(<<"abc">>, <<"abd">>)),
    %% 长度不同直接 false（不等长走不进 crypto:hash_equals）
    ?assertNot(elib_wechat_msg:secure_equal(<<"abc">>, <<"abcd">>)).

%%%===================================================================
%%% 加密：自产密文的结构由「手写 CBC」独立反解核对
%%%===================================================================

encrypt_roundtrip_test() ->
    {ok, Enc} = elib_wechat_msg:encrypt(msg_binary(), ?AES_KEY_TEXT, ?APPID),
    ?assertEqual(
        {ok, #{msg => msg_binary(), appid => ?APPID}},
        elib_wechat_msg:decrypt(Enc, ?AES_KEY_TEXT)
    ).

%% 用 ECB 单块 + 手工异或链反解（与本模块的 crypto CBC 调用是不同的代码路径），
%% 再逐字段核对明文结构 random(16)||len(4)|msg||appid + PKCS#7。
encrypt_wire_structure_test() ->
    {ok, Enc} = elib_wechat_msg:encrypt(msg_binary(), ?AES_KEY_TEXT, ?APPID),
    Cipher = base64:decode(Enc),
    ?assertEqual(0, byte_size(Cipher) rem ?BLOCK_SIZE),
    Plain = manual_cbc_decrypt(Cipher, ?AES_KEY_TEXT),
    PadLen = binary:last(Plain),
    ?assert(PadLen >= 1 andalso PadLen =< ?BLOCK_SIZE),
    Body = binary:part(Plain, 0, byte_size(Plain) - PadLen),
    ?assertEqual(binary:copy(<<PadLen>>, PadLen), binary:part(Plain, byte_size(Body), PadLen)),
    MsgLen = byte_size(msg_binary()),
    <<_Random:16/binary, MsgLen:32/big, Rest/binary>> = Body,
    ?assertEqual(msg_binary(), binary:part(Rest, 0, MsgLen)),
    ?assertEqual(?APPID, binary:part(Rest, MsgLen, byte_size(Rest) - MsgLen)).

encrypt_fails_on_bad_key_test() ->
    ?assertEqual(
        {error, invalid_aes_key},
        elib_wechat_msg:encrypt(msg_binary(), <<"short">>, ?APPID)
    ).

%% 两块密文：确认链式异或（CBC 而非 ECB）真的生效 —— 同样的明文前缀、
%% 不同的盐值，密文必须不同，否则等于退化成了 ECB。
encrypt_uses_random_iv_prefix_test() ->
    {ok, A} = elib_wechat_msg:encrypt(msg_binary(), ?AES_KEY_TEXT, ?APPID),
    {ok, B} = elib_wechat_msg:encrypt(msg_binary(), ?AES_KEY_TEXT, ?APPID),
    ?assertNotEqual(A, B).

%%%===================================================================
%%% 固定向量构造：与 Python 侧 `b'...\\u8001...'` 逐字节一致
%%%===================================================================

msg_binary() ->
    <<
        "{\"ToUserName\":\"wxzzzzzzzzzzzzzzzz\","
        "\"FromUserName\":\"oABCDEFGHIJKLMNOP\","
        "\"CreateTime\":1789531800,"
        "\"MsgType\":\"text\","
        "\"Content\":\"\\u8001\\u5e08\\u597d\"}"
    >>.

encrypt_b64() ->
    <<
        "Y+UxJ4jP5TNqCOYYFgFq6eafkTDbTKIrZabyqgtRLNEEnXBSfNhsL+wlN9dwpmeZe8aX/kthOZKBs"
        "/eJIhAEhw7jNrsjLH9/wamNH3tQDkCE98F66J3OvK8+rIJeIXwdftfIdQcDJenuOMJn9EAFZONF5zow"
        "KGPQQ+8/QtwzExjF5/9yc/fcmMRUJPWYQgYla7RSWxOZULQWeYld+NUryn+vGGsFL6F+6RvH+coTQtDd"
        "LyALpUHiukehaSm7JUGx"
    >>.

%%%===================================================================
%%% Internal：手写 CBC（ECB 单块 + 异或链），独立于 elib_wechat_msg 的实现
%%%===================================================================

manual_cbc_decrypt(Cipher, KeyText) ->
    {ok, Key} = elib_wechat_msg:aes_key(KeyText),
    IV = binary:part(Key, 0, ?BLOCK_SIZE),
    Blocks = split_blocks(Cipher),
    {_, PlainBlocks} = lists:foldl(
        fun(Block, {Prev, Acc}) ->
            Decrypted = ecb_decrypt_block(Key, Block),
            Plain = crypto:exor(Decrypted, Prev),
            {Block, [Plain | Acc]}
        end,
        {IV, []},
        Blocks
    ),
    iolist_to_binary(lists:reverse(PlainBlocks)).

split_blocks(Bin) ->
    [
        binary:part(Bin, I, ?BLOCK_SIZE)
     || I <- lists:seq(0, byte_size(Bin) - ?BLOCK_SIZE, ?BLOCK_SIZE)
    ].

ecb_decrypt_block(Key, Block) ->
    crypto:crypto_one_time(aes_256_ecb, Key, <<>>, Block, [{encrypt, false}, {padding, none}]).

%%%===================================================================
%%% Internal：杂项
%%%===================================================================

%% 改第一个字符且保证确实变了（避免"改完还是原值"的假绿）
tamper_first_char(Bin) ->
    <<Head:1/binary, Rest/binary>> = Bin,
    Replacement =
        case Head of
            <<"A">> -> <<"B">>;
            _ -> <<"A">>
        end,
    case <<Replacement/binary, Rest/binary>> of
        Bin -> <<"Z", Rest/binary>>;
        Tampered -> Tampered
    end.

hex_to_bin(Hex) ->
    binary:decode_hex(Hex).
