%% teaching_ai_worker_view_url_tests
%% 附件视频 URL 开关（默认关闭）——纯 logic 用例：零 DB、零网络、零真实密钥。
%%
%% 语义：teaching_ai_worker:maybe_attach_view_url/1 关闭时**原样返回**（模型只能拿到
%% object_key，回课内容盲但链路完整）；开启时才签名补 url；签名失败降级为不带 url
%% （不崩溃、不阻断回课）。开关默认关闭的理由见该函数注释：开启即产生「未成年人媒体
%% 内容外发给第三方模型」的数据流，必须显式打开。

-module(teaching_ai_worker_view_url_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

att() ->
    #{
        <<"id">> => 978001,
        <<"path">> => <<"u1/teaching/20260912/a.mp4">>,
        <<"mime_type">> => <<"video/mp4">>,
        <<"size">> => 1024,
        <<"scope">> => <<"teaching">>
    }.

%% 开关值可注入；签名函数可注入。
%% ⚠️ 断言「不该签名」的用例，mock 必须**返回可识别的 URL**而不是抛异常——
%% 实现里有 try/catch 兜底，抛异常会被吞掉、即使漏了开关判断也照样绿（假绿）。
mocks(FlagValue, PresignFun) ->
    [
        {config_ds, [
            {'env', 2, fun
                (teaching_ai_attach_video_url, _Default) -> FlagValue;
                (_, Default) -> Default
            end}
        ]},
        {elib_oss, [
            {'presign_get_for_key', 3, PresignFun}
        ]}
    ].

%% 探针 URL：出现即说明真去签名了
-define(LEAK, <<"https://s3.imboy.pub/MUST_NOT_BE_USED">>).

%% 开关关闭（false）→ 附件原样、**无 url 键**（即未签名）
flag_false_keeps_attachment_test_() ->
    ?WITH_MECKS(
        mocks(false, fun(_, _, _) -> ?LEAK end),
        fun() ->
            Got = teaching_ai_worker:maybe_attach_view_url(att()),
            ?assertEqual(att(), Got),
            ?assertEqual(false, maps:is_key(<<"url">>, Got))
        end
    ).

%% 配置缺失（undefined）→ 同样按关闭处理
flag_undefined_keeps_attachment_test_() ->
    ?WITH_MECKS(
        mocks(undefined, fun(_, _, _) -> ?LEAK end),
        fun() ->
            Got = teaching_ai_worker:maybe_attach_view_url(att()),
            ?assertEqual(att(), Got),
            ?assertEqual(false, maps:is_key(<<"url">>, Got))
        end
    ).

%% 开关打开 → 补 url，且其余字段不受影响
flag_true_attaches_url_test_() ->
    ?WITH_MECKS(
        mocks(true, fun(_Bucket, _Key, _Expires) -> <<"https://s3.imboy.pub/signed">> end),
        fun() ->
            Got = teaching_ai_worker:maybe_attach_view_url(att()),
            ?assertEqual(<<"https://s3.imboy.pub/signed">>, maps:get(<<"url">>, Got)),
            ?assertEqual(maps:get(<<"path">>, att()), maps:get(<<"path">>, Got)),
            ?assertEqual(maps:get(<<"mime_type">>, att()), maps:get(<<"mime_type">>, Got))
        end
    ).

%% 开关打开但签名抛异常（如 garage 未配置）→ 降级为不带 url，不崩
flag_true_sign_failure_degrades_test_() ->
    ?WITH_MECKS(
        mocks(true, fun(_, _, _) -> erlang:error(garage_not_configured) end),
        fun() ->
            Got = teaching_ai_worker:maybe_attach_view_url(att()),
            ?assertEqual(att(), Got),
            ?assertEqual(false, maps:is_key(<<"url">>, Got))
        end
    ).

%% 开关打开但签名返回空串 → 不写入 url
flag_true_empty_url_degrades_test_() ->
    ?WITH_MECKS(
        mocks(true, fun(_, _, _) -> <<>> end),
        fun() ->
            Got = teaching_ai_worker:maybe_attach_view_url(att()),
            ?assertEqual(att(), Got),
            ?assertEqual(false, maps:is_key(<<"url">>, Got))
        end
    ).

%% 开关打开但 path 为空 → 不去签名、原样返回
flag_true_empty_path_keeps_attachment_test_() ->
    ?WITH_MECKS(
        mocks(true, fun(_, _, _) -> ?LEAK end),
        fun() ->
            Base = att(),
            Empty = Base#{<<"path">> => <<>>},
            Got = teaching_ai_worker:maybe_attach_view_url(Empty),
            ?assertEqual(Empty, Got),
            ?assertEqual(false, maps:is_key(<<"url">>, Got))
        end
    ).
