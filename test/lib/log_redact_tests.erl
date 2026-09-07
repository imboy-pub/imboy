-module(log_redact_tests).

%%%===================================================================
%%% @doc log_redact 日志脱敏测试（V-02）
%%%
%%% 覆盖计划测试条款：嵌套 map、headers、URL、异常文本、E2EE 明文/密文、
%%% 支付回调、sink 级断言（原始秘密在 lager 入口缺席）。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% term/1 —— 键脱敏（精确匹配，递归）
%%%===================================================================

%% 嵌套 map 里的敏感键逐层剥离
nested_map_redacted_test() ->
    In = #{
        <<"uid">> => 7,
        <<"req">> => #{
            <<"headers">> => #{
                <<"authorization">> => <<"Bearer abc.def">>,
                <<"cookie">> => <<"sid=1">>,
                <<"content-type">> => <<"application/json">>
            },
            <<"body">> => #{
                <<"access_token">> => <<"tok">>,
                <<"nickname">> => <<"n">>
            }
        }
    },
    Out = log_redact:term(In),
    Headers = maps:get(<<"headers">>, maps:get(<<"req">>, Out)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"authorization">>, Headers)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"cookie">>, Headers)),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)),
    Body = maps:get(<<"body">>, maps:get(<<"req">>, Out)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"access_token">>, Body)),
    ?assertEqual(<<"n">>, maps:get(<<"nickname">>, Body)).

%% E2EE 明文键必须剥离；密文本身不是秘密，保留可诊断
e2ee_plaintext_redacted_ciphertext_kept_test() ->
    In = #{
        <<"plaintext">> => <<"你好，这是明文消息"/utf8>>,
        <<"msg_content">> => <<"机密"/utf8>>,
        <<"ciphertext">> => <<"base64blob">>
    },
    Out = log_redact:term(In),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"plaintext">>, Out)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"msg_content">>, Out)),
    ?assertEqual(<<"base64blob">>, maps:get(<<"ciphertext">>, Out)).

%% 支付回调 proplist：sign/openid 是凭据级字段，剥离
payment_callback_proplist_redacted_test() ->
    In = [
        {<<"out_trade_no">>, <<"T123">>},
        {<<"sign">>, <<"ABCD1234">>},
        {<<"openid">>, <<"oX-123456">>},
        {<<"total_fee">>, 100}
    ],
    Out = log_redact:term(In),
    ?assertEqual({<<"out_trade_no">>, <<"T123">>}, lists:nth(1, Out)),
    ?assertEqual({<<"sign">>, <<"[REDACTED]">>}, lists:nth(2, Out)),
    ?assertEqual({<<"openid">>, <<"[REDACTED]">>}, lists:nth(3, Out)),
    ?assertEqual({<<"total_fee">>, 100}, lists:nth(4, Out)).

%% 键精确匹配：design/assignment 不得误伤（sign 子串陷阱）
exact_key_no_false_positive_test() ->
    In = #{<<"design">> => <<"ok">>, <<"assignment">> => 1, <<"sign">> => <<"x">>},
    Out = log_redact:term(In),
    ?assertEqual(<<"ok">>, maps:get(<<"design">>, Out)),
    ?assertEqual(1, maps:get(<<"assignment">>, Out)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"sign">>, Out)).

%% atom 键与大小写变体
atom_and_case_keys_test() ->
    In = #{password => <<"h">>, <<"Token">> => <<"t">>},
    Out = log_redact:term(In),
    ?assertEqual(<<"[REDACTED]">>, maps:get(password, Out)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"Token">>, Out)).

%%%===================================================================
%%% text/1 —— 值模式兜底
%%%===================================================================

url_query_token_redacted_test() ->
    In = <<"GET https://pro.imboy.pub/api/v1/user?token=abc123&uid=7">>,
    Out = log_redact:text(In),
    ?assertEqual(nomatch, binary:match(Out, <<"abc123">>)),
    ?assertNotEqual(nomatch, binary:match(Out, <<"token=[REDACTED]">>)),
    ?assertNotEqual(nomatch, binary:match(Out, <<"uid=7">>)).

exception_text_jwt_redacted_test() ->
    In =
        <<
            "auth failed with JWT eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxIn0.SflKxwRJSMeKKF2QT4f"
            "wpMeJf36POk6yJV_adQssw5c"
        >>,
    Out = log_redact:text(In),
    ?assertEqual(nomatch, binary:match(Out, <<"eyJhbGciOiJIUzI1NiJ9">>)),
    ?assertNotEqual(nomatch, binary:match(Out, <<"[REDACTED]">>)).

bearer_header_text_redacted_test() ->
    In = <<"request header authorization: Bearer sk_live_9f8e7d6c5b4a">>,
    Out = log_redact:text(In),
    ?assertEqual(nomatch, binary:match(Out, <<"sk_live_9f8e7d6c5b4a">>)).

mobile_and_email_redacted_test() ->
    In = <<"user 13812345678 mail foo.bar@example.com ok"/utf8>>,
    Out = log_redact:text(In),
    ?assertEqual(nomatch, binary:match(Out, <<"13812345678">>)),
    ?assertEqual(nomatch, binary:match(Out, <<"foo.bar@example.com">>)),
    %% 相邻字符不被吞（手机号捕获组保持前后缀）
    ?assertNotEqual(nomatch, binary:match(Out, <<"user [REDACTED] mail"/utf8>>)).

plain_text_without_secrets_untouched_test() ->
    In = <<"消息发送成功 msg_id=42"/utf8>>,
    ?assertEqual(In, log_redact:text(In)).

text_non_string_passthrough_test() ->
    ?assertEqual(42, log_redact:text(42)),
    ?assertEqual(foo, log_redact:text(foo)).

%%%===================================================================
%%% sink 级：elib_log 交给 lager 的最终文本不得含原始秘密
%%%===================================================================

sink_redaction_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun sink_strips_secret_from_term_list/0,
        fun sink_strips_secret_from_fmt_args/0
    ]}.

setup() ->
    meck:new(lager, [non_strict, no_link]),
    meck:expect(lager, log, fun(_Level, _Meta, _Msg) -> ok end),
    ok.

cleanup(_) ->
    catch meck:unload(lager),
    ok.

captured_messages() ->
    [Msg || {_P, {lager, log, [_L, _M, Msg]}, _R} <- meck:history(lager)].

%% term list 路径：值里的手机号与 token 键在 lager 入口缺席
sink_strips_secret_from_term_list() ->
    elib_log:internal_log(
        error,
        [payment_notify_failed, <<"https://cb/pay?sign=SECRETVALUE&mobile=13912345678">>],
        ?MODULE,
        1
    ),
    [Msg] = captured_messages(),
    Text = unicode:characters_to_binary(Msg),
    ?assertEqual(nomatch, binary:match(Text, <<"SECRETVALUE">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"13912345678">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"[REDACTED]">>)).

%% fmt/args 路径：参数先脱敏再格式化
sink_strips_secret_from_fmt_args() ->
    elib_log:internal_log(
        error,
        "notify failed: ~ts",
        [<<"https://callback/pay?sign=SECRETVALUE&ts=1">>],
        ?MODULE,
        2
    ),
    [Msg] = captured_messages(),
    Text = unicode:characters_to_binary(Msg),
    ?assertEqual(nomatch, binary:match(Text, <<"SECRETVALUE">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"sign=[REDACTED]">>)).
