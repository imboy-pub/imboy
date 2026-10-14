-module(elib_s3_sign_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% elib_s3_sign 单元测试
%%% 纯函数，无 DB/网络依赖
%%%===================================================================

presign_put_contains_amz_algorithm_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            endpoint => <<"http://127.0.0.1:3900">>,
            region => <<"garage">>,
            bucket => <<"imboy">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        Url = elib_s3_sign:presign_put(
            <<"http://127.0.0.1:3900">>,
            <<"imboy">>,
            <<"file_1/test.jpg">>,
            <<"image/jpeg">>,
            3600
        ),
        ?assert(binary:match(Url, <<"AWS4-HMAC-SHA256">>) =/= nomatch)
    end).

presign_put_contains_access_key_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            region => <<"garage">>,
            access_key => <<"GKtestkey">>,
            secret_key => <<"testsecret">>
        }),
        Url = elib_s3_sign:presign_put(
            <<"http://127.0.0.1:3900">>,
            <<"imboy">>,
            <<"file_1/test.jpg">>,
            <<"image/jpeg">>,
            3600
        ),
        ?assert(binary:match(Url, <<"GKtestkey">>) =/= nomatch)
    end).

presign_put_contains_expires_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        Url = elib_s3_sign:presign_put(
            <<"http://127.0.0.1:3900">>,
            <<"imboy">>,
            <<"file.jpg">>,
            <<"image/jpeg">>,
            7200
        ),
        ?assert(binary:match(Url, <<"X-Amz-Expires=7200">>) =/= nomatch)
    end).

presign_put_contains_signature_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        Url = elib_s3_sign:presign_put(
            <<"http://127.0.0.1:3900">>,
            <<"imboy">>,
            <<"file.jpg">>,
            <<"image/jpeg">>,
            3600
        ),
        ?assert(binary:match(Url, <<"X-Amz-Signature=">>) =/= nomatch)
    end).

format_date_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_s3_sign:format_date({{2026, 5, 28}, {10, 30, 0}}),
        ?assertEqual(<<"20260528">>, Result)
    end).

format_amz_date_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_s3_sign:format_amz_date({{2026, 5, 28}, {10, 30, 0}}),
        ?assertEqual(<<"20260528T103000Z">>, Result)
    end).

format_date_pads_zeros_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_s3_sign:format_date({{2026, 1, 5}, {0, 0, 0}}),
        ?assertEqual(<<"20260105">>, Result)
    end).

presign_get_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        Url = elib_s3_sign:presign_get(
            <<"http://127.0.0.1:3900">>, <<"imboy">>, <<"obj/key">>, 3600
        ),
        ?assert(is_binary(Url)),
        ?assert(binary:match(Url, <<"X-Amz-Signature=">>) =/= nomatch)
    end).

authorization_header_starts_with_aws4_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            endpoint => <<"http://127.0.0.1:3900">>,
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        Header = elib_s3_sign:authorization_header(
            <<"PUT">>,
            <<"imboy">>,
            <<"dir/file.jpg">>,
            <<"image/jpeg">>,
            <<"20260528T103000Z">>,
            <<"GKtest">>,
            <<"testsecret">>
        ),
        ?assert(binary:match(Header, <<"AWS4-HMAC-SHA256">>) =/= nomatch),
        ?assert(binary:match(Header, <<"Credential=">>) =/= nomatch),
        ?assert(binary:match(Header, <<"Signature=">>) =/= nomatch)
    end).

authorization_header_delete_no_content_type_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            endpoint => <<"http://127.0.0.1:3900">>,
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        Header = elib_s3_sign:authorization_header(
            <<"DELETE">>,
            <<"imboy">>,
            <<"dir/file.jpg">>,
            <<>>,
            <<"20260528T103000Z">>,
            <<"GKtest">>,
            <<"testsecret">>
        ),
        ?assert(is_binary(Header)),
        ?assert(binary:match(Header, <<"AWS4-HMAC-SHA256">>) =/= nomatch)
    end).

%%%===================================================================
%%% W2-01（02 §15）：冻结签名 Context 的 presigned PUT
%%% 断言：同 Context 重复签名产出同一 URL（底层不重新计时）、
%%% X-Amz-Date/X-Amz-Expires 严格取自传入的绝对 issued_at/expires_at、
%%% 非法窗口（过期/超 7 天上限）拒绝。
%%%===================================================================

ctx_sign() ->
    elib_s3_sign:presign_put_ctx(
        <<"http://127.0.0.1:3900">>,
        <<"imboy">>,
        <<"u1/file/x.bin">>,
        <<"application/octet-stream">>,
        {{2026, 10, 14}, {8, 0, 0}},
        {{2026, 10, 14}, {9, 0, 0}}
    ).

presign_put_ctx_is_deterministic_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            endpoint => <<"http://127.0.0.1:3900">>,
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        {ok, Url1} = ctx_sign(),
        {ok, Url2} = ctx_sign(),
        ?assertEqual(Url1, Url2)
    end).

presign_put_ctx_uses_frozen_times_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        {ok, Url} = ctx_sign(),
        ?assert(binary:match(Url, <<"X-Amz-Date=20261014T080000Z">>) =/= nomatch),
        ?assert(binary:match(Url, <<"X-Amz-Expires=3600">>) =/= nomatch)
    end).

presign_put_ctx_rejects_expired_window_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        ?assertEqual(
            {error, invalid_context},
            elib_s3_sign:presign_put_ctx(
                <<"http://127.0.0.1:3900">>,
                <<"imboy">>,
                <<"u1/file/x.bin">>,
                <<"application/octet-stream">>,
                {{2026, 10, 14}, {8, 0, 0}},
                {{2026, 10, 14}, {8, 0, 0}}
            )
        ),
        ?assertEqual(
            {error, invalid_context},
            elib_s3_sign:presign_put_ctx(
                <<"http://127.0.0.1:3900">>,
                <<"imboy">>,
                <<"u1/file/x.bin">>,
                <<"application/octet-stream">>,
                {{2026, 10, 14}, {9, 0, 0}},
                {{2026, 10, 14}, {8, 0, 0}}
            )
        )
    end).

presign_put_ctx_rejects_over_7d_window_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, garage, #{
            region => <<"garage">>,
            access_key => <<"GKtest">>,
            secret_key => <<"testsecret">>
        }),
        ?assertEqual(
            {error, invalid_context},
            elib_s3_sign:presign_put_ctx(
                <<"http://127.0.0.1:3900">>,
                <<"imboy">>,
                <<"u1/file/x.bin">>,
                <<"application/octet-stream">>,
                {{2026, 10, 14}, {8, 0, 0}},
                {{2026, 10, 22}, {8, 0, 1}}
            )
        )
    end).
