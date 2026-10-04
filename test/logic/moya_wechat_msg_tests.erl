%% moya_wechat_msg_tests
%%
%% 微信「消息推送」业务逻辑：GET 首次验签（原样回 echostr）与 POST 事件解包。
%%
%% 全部配置项与外部依赖均 meck，无真实网络。密文用的是 elib_wechat_msg_tests
%% 里那份由 OpenSSL 独立实现的固定向量（同一份常量），因此这一层测的是
%% 「业务裁决」而不是「密码学是否正确」：
%%   - 未配置 → provider_unconfigured（fail-closed，不静默降级为「不验签」）
%%   - 密文必须验 msg_signature，且解密出的 appid 必须等于本环境 wechat_mini_appid
%%   - 明文模式（body 无 Encrypt）**同样验 URL 上的 signature**：官方文档
%%     「解密方式为明文模式」第 4 步要求用它判断请求是否来自微信服务器
%%   - 被动回复默认关闭

-module(moya_wechat_msg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 与 elib_wechat_msg_tests 同一份测试常量（非生产）
-define(TOKEN, <<"testtoken1234567890">>).
-define(AES_KEY_TEXT, <<"AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyA">>).
-define(APPID, <<"wxzzzzzzzzzzzzzzzz">>).
-define(OTHER_APPID, <<"wxother0000000000">>).
-define(TIMESTAMP, <<"1789531800">>).
-define(NONCE, <<"a1b2c3d4">>).
-define(SIG_PLAIN, <<"eaee55865d33d15605f1e134dc8472da48108874">>).
-define(SIG_ENCRYPT, <<"038a7aef52cf621e2566d0c88e7fa91b700fd064">>).

%%%===================================================================
%%% 未配置：必须 fail-closed
%%%===================================================================

verify_url_unconfigured_test_() ->
    ?WITH_MECKS(
        [cfg(<<>>, <<>>, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, provider_unconfigured},
                moya_wechat_msg_logic:verify_url(valid_query(<<"echo">>))
            )
        end
    ).

%% 只配了 Token 没配 AESKey：仍然算未配置。若这里放过，POST 会在解密时才炸，
%% 而 GET 校验会「假装成功」—— 后台显示接入成功但消息全收不到。
verify_url_token_only_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, <<>>, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, provider_unconfigured},
                moya_wechat_msg_logic:verify_url(valid_query(<<"echo">>))
            )
        end
    ).

handle_push_unconfigured_test_() ->
    ?WITH_MECKS(
        [cfg(<<>>, <<>>, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, provider_unconfigured},
                moya_wechat_msg_logic:handle_push(encrypted_query(), event_body())
            )
        end
    ).

%%%===================================================================
%%% GET 首次验签
%%%===================================================================

verify_url_ok_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            ?assertEqual(
                {ok, <<"echo-12345">>},
                moya_wechat_msg_logic:verify_url(valid_query(<<"echo-12345">>))
            )
        end
    ).

verify_url_bad_signature_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            Query = (valid_query(<<"echo">>))#{<<"signature">> => <<"deadbeef">>},
            ?assertEqual({error, bad_signature}, moya_wechat_msg_logic:verify_url(Query))
        end
    ).

%% 签名缺失（微信的签名参数名写错时就是这个现象）→ bad_signature，不能放行
verify_url_missing_signature_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            Query = maps:remove(<<"signature">>, valid_query(<<"echo">>)),
            ?assertEqual({error, bad_signature}, moya_wechat_msg_logic:verify_url(Query))
        end
    ).

%% 验签通过但没带 echostr：不能当成功回空串 —— 后台会报 Token 验证失败，
%% 这里给出明确原因便于定位（是参数名写错，不是 Token 错）
verify_url_missing_echostr_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            Query = maps:remove(<<"echostr">>, valid_query(<<"echo">>)),
            ?assertEqual({error, missing_echostr}, moya_wechat_msg_logic:verify_url(Query))
        end
    ).

%%%===================================================================
%%% POST 密文事件（兼容模式的实际路径）
%%%===================================================================

handle_push_encrypted_ok_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            %% 未配置被动回复 ⇒ 空串（微信视作「已收到，无需回复」）
            ?assertEqual(
                {ok, <<>>},
                moya_wechat_msg_logic:handle_push(encrypted_query(), event_body())
            )
        end
    ).

handle_push_bad_msg_signature_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            Query = (encrypted_query())#{<<"msg_signature">> => <<"deadbeef">>},
            ?assertEqual(
                {error, bad_msg_signature},
                moya_wechat_msg_logic:handle_push(Query, event_body())
            )
        end
    ).

%% 密文无法解密（Token 对但 AESKey 与微信后台不一致的典型症状）
handle_push_wrong_aes_key_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, <<"ISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0A">>, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, decrypt_failed},
                moya_wechat_msg_logic:handle_push(encrypted_query(), event_body())
            )
        end
    ).

%% appid 这一关不能省：EncodingAESKey 一旦在多应用间复用（或泄漏），
%% 别人发来的报文结构同样合法，只有 appid 能区分「这条本来发给谁」。
handle_push_appid_mismatch_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?OTHER_APPID)],
        fun() ->
            ?assertEqual(
                {error, appid_mismatch},
                moya_wechat_msg_logic:handle_push(encrypted_query(), event_body())
            )
        end
    ).

%% appid 未配置（漏配 wechat_mini_appid）→ provider_unconfigured，而不是 appid_mismatch。
%% 两者都拒绝，但归因方向完全不同：前者查「配置漏了」，后者查「AESKey 是否被复用/泄漏」。
handle_push_appid_unconfigured_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, <<>>)],
        fun() ->
            ?assertEqual(
                {error, provider_unconfigured},
                moya_wechat_msg_logic:handle_push(encrypted_query(), event_body())
            )
        end
    ).

%% 能解密、appid 也对，但报文不是 JSON 对象 → malformed_event
handle_push_non_json_payload_test_() ->
    {ok, Enc} = elib_wechat_msg:encrypt(<<"not-a-json-payload">>, ?AES_KEY_TEXT, ?APPID),
    Sig = elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE, Enc),
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, malformed_event},
                moya_wechat_msg_logic:handle_push(
                    #{
                        <<"timestamp">> => ?TIMESTAMP,
                        <<"nonce">> => ?NONCE,
                        <<"msg_signature">> => Sig
                    },
                    #{<<"Encrypt">> => Enc}
                )
            )
        end
    ).

%% 解密后是 JSON 数组（不是对象）→ 同样 malformed_event
handle_push_json_array_payload_test_() ->
    {ok, Enc} = elib_wechat_msg:encrypt(<<"[1,2,3]">>, ?AES_KEY_TEXT, ?APPID),
    Sig = elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE, Enc),
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, malformed_event},
                moya_wechat_msg_logic:handle_push(
                    #{
                        <<"timestamp">> => ?TIMESTAMP,
                        <<"nonce">> => ?NONCE,
                        <<"msg_signature">> => Sig
                    },
                    #{<<"Encrypt">> => Enc}
                )
            )
        end
    ).

%%%===================================================================
%%% POST 明文模式（body 无 Encrypt）：仍必须验 URL 上的 signature
%%%===================================================================

%% 明文模式消息体不加密，但微信在 URL 上给了 signature/timestamp/nonce，
%% 官方《消息推送》「解密方式为明文模式」第 4 步要求据此判断请求是否来自微信。
%% 带正确签名 → 放行（?SIG_PLAIN 即 signature(TOKEN, TIMESTAMP, NONCE)）。
handle_push_plaintext_ok_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            ?assertEqual(
                {ok, <<>>},
                moya_wechat_msg_logic:handle_push(plain_query(?SIG_PLAIN), plain_body())
            )
        end
    ).

%% 签名不符 → 拒绝。这一条是防「退化成不验签」的守卫：
%% 早前版本正是直接放行这一支，等于任何人 POST 都能让服务端处理并记录事件。
handle_push_plaintext_bad_signature_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, bad_signature},
                moya_wechat_msg_logic:handle_push(plain_query(<<"bogus">>), plain_body())
            )
        end
    ).

%% 完全不签名（URL 少参数 / 参数名写错）→ 与「签名不符」分开报，便于定位
handle_push_plaintext_missing_signature_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            Query = maps:remove(<<"signature">>, plain_query(?SIG_PLAIN)),
            ?assertEqual(
                {error, missing_signature},
                moya_wechat_msg_logic:handle_push(Query, plain_body())
            )
        end
    ).

%% 非 map 的 body（适配层解码失败时可能传下来）→ malformed_event
handle_push_non_map_body_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID)],
        fun() ->
            ?assertEqual(
                {error, malformed_event},
                moya_wechat_msg_logic:handle_push(#{}, <<"not a map">>)
            )
        end
    ).

%%%===================================================================
%%% 被动回复
%%%===================================================================

handle_push_reply_text_test_() ->
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID, ~B'收到啦')],
        fun() ->
            {ok, Raw} = moya_wechat_msg_logic:handle_push(encrypted_query(), event_body()),
            Reply = jsone:decode(Raw),
            %% 收发双方**互换**：回复的收件人是发消息的用户
            ?assertEqual(<<"oABCDEFGHIJKLMNOP">>, maps:get(<<"ToUserName">>, Reply)),
            ?assertEqual(?APPID, maps:get(<<"FromUserName">>, Reply)),
            ?assertEqual(<<"text">>, maps:get(<<"MsgType">>, Reply)),
            ?assertEqual(~B'收到啦', maps:get(<<"Content">>, Reply)),
            ?assert(is_integer(maps:get(<<"CreateTime">>, Reply)))
        end
    ).

%% 配了回复文案，但来的是事件（不是文本消息）→ 仍回空串。
%% 给事件回一段文本是微信侧非法形状，会触发重试。
handle_push_reply_skipped_for_event_test_() ->
    {ok, Enc} = elib_wechat_msg:encrypt(
        <<"{\"MsgType\":\"event\",\"Event\":\"user_enter_tempsession\",\"FromUserName\":\"oX\",\"ToUserName\":\"wxzzzzzzzzzzzzzzzz\"}">>,
        ?AES_KEY_TEXT,
        ?APPID
    ),
    Sig = elib_wechat_msg:signature(?TOKEN, ?TIMESTAMP, ?NONCE, Enc),
    ?WITH_MECKS(
        [cfg(?TOKEN, ?AES_KEY_TEXT, ?APPID, ~B'收到啦')],
        fun() ->
            ?assertEqual(
                {ok, <<>>},
                moya_wechat_msg_logic:handle_push(
                    #{
                        <<"timestamp">> => ?TIMESTAMP,
                        <<"nonce">> => ?NONCE,
                        <<"msg_signature">> => Sig
                    },
                    #{<<"Encrypt">> => Enc}
                )
            )
        end
    ).

%%%===================================================================
%%% Fixtures
%%%===================================================================

%% 密文事件的 query：timestamp / nonce / msg_signature 三者齐备
encrypted_query() ->
    #{
        <<"timestamp">> => ?TIMESTAMP,
        <<"nonce">> => ?NONCE,
        <<"msg_signature">> => ?SIG_ENCRYPT
    }.

event_body() ->
    #{<<"Encrypt">> => encrypt_b64()}.

valid_query(EchoStr) ->
    #{
        <<"signature">> => ?SIG_PLAIN,
        <<"timestamp">> => ?TIMESTAMP,
        <<"nonce">> => ?NONCE,
        <<"echostr">> => EchoStr
    }.

%% 明文模式事件的 query：签名参数名是 `signature`（密文分支才是 `msg_signature`）
plain_query(Signature) ->
    #{
        <<"signature">> => Signature,
        <<"timestamp">> => ?TIMESTAMP,
        <<"nonce">> => ?NONCE
    }.

plain_body() ->
    #{
        <<"MsgType">> => <<"text">>,
        <<"FromUserName">> => <<"oABCDEFGHIJKLMNOP">>,
        <<"ToUserName">> => ?APPID
    }.

%% OpenSSL 侧固定向量（与 elib_wechat_msg_tests 同一份）
encrypt_b64() ->
    <<
        "Y+UxJ4jP5TNqCOYYFgFq6eafkTDbTKIrZabyqgtRLNEEnXBSfNhsL+wlN9dwpmeZe8aX/kthOZKBs"
        "/eJIhAEhw7jNrsjLH9/wamNH3tQDkCE98F66J3OvK8+rIJeIXwdftfIdQcDJenuOMJn9EAFZONF5zow"
        "KGPQQ+8/QtwzExjF5/9yc/fcmMRUJPWYQgYla7RSWxOZULQWeYld+NUryn+vGGsFL6F+6RvH+coTQtDd"
        "LyALpUHiukehaSm7JUGx"
    >>.

cfg(Token, AesKey, AppId) ->
    cfg(Token, AesKey, AppId, <<>>).

cfg(Token, AesKey, AppId, ReplyText) ->
    {config_ds, [
        {'env', 2, fun
            (wechat_mini_msg_push_token, _) -> Token;
            (wechat_mini_msg_push_aes_key, _) -> AesKey;
            (wechat_mini_appid, _) -> AppId;
            (wechat_mini_msg_push_reply, _) -> ReplyText;
            (_, Default) -> Default
        end}
    ]}.
