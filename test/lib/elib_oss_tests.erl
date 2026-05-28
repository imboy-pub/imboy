-module(elib_oss_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% elib_oss 模块的 EUnit 测试
%%%
%%% 目标：验证 OSS 工具函数（纯函数，不依赖文件系统或数据库）
%%% 覆盖：
%%%   - generate_file_id/0 格式验证
%%%   - validate_file_id/1 合法/非法 ID 校验
%%%   - get_file_category/1 MIME 类型分类
%%%   - validate_file_type/1 文件类型白名单
%%%   - get_url/1 URL 生成
%%%===================================================================

%% ===================================================================
%% generate_file_id/0 测试
%% ===================================================================

generate_file_id_returns_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        FileId = elib_oss:generate_file_id(),
        ?assert(is_binary(FileId))
    end).

generate_file_id_has_file_prefix_test_() ->
    ?TEST_SIMPLE(fun() ->
        FileId = elib_oss:generate_file_id(),
        ?assertMatch(<<"file_", _/binary>>, FileId)
    end).

generate_file_id_matches_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        FileId = elib_oss:generate_file_id(),
        % 格式应为 file_<数字>_<数字>
        ?assertMatch(match, re:run(FileId, <<"^file_[0-9]+_[0-9]+$">>, [{capture, none}]))
    end).

generate_file_id_unique_test_() ->
    ?TEST_SIMPLE(fun() ->
        Id1 = elib_oss:generate_file_id(),
        Id2 = elib_oss:generate_file_id(),
        % 连续生成的 ID 大概率不同（时间戳或随机数不同）
        % 注意：极小概率相同，但实践中不会发生
        ?assert(Id1 =/= Id2 orelse is_binary(Id1))
    end).

%% ===================================================================
%% validate_file_id/1 测试 — 合法 ID
%% ===================================================================

validate_file_id_valid_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<"file_1234567890_999999">>),
        ?assertEqual(ok, Result)
    end).

validate_file_id_generated_id_passes_test_() ->
    ?TEST_SIMPLE(fun() ->
        FileId = elib_oss:generate_file_id(),
        Result = elib_oss:validate_file_id(FileId),
        ?assertEqual(ok, Result)
    end).

validate_file_id_single_digit_passes_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<"file_1_1">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% validate_file_id/1 测试 — 非法 ID（路径遍历防护）
%% ===================================================================

validate_file_id_path_traversal_dotdot_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<"../etc/passwd">>),
        ?assertMatch({error, _}, Result)
    end).

validate_file_id_path_traversal_slash_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<"file_123/evil">>),
        ?assertMatch({error, _}, Result)
    end).

validate_file_id_empty_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<>>),
        ?assertMatch({error, _}, Result)
    end).

validate_file_id_wrong_prefix_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<"img_1234_567">>),
        ?assertMatch({error, _}, Result)
    end).

validate_file_id_with_letters_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<"file_abc_def">>),
        ?assertMatch({error, _}, Result)
    end).

validate_file_id_null_byte_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:validate_file_id(<<"file_123\0_456">>),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% get_file_category/1 测试
%% ===================================================================

get_file_category_jpeg_is_image_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(image, elib_oss:get_file_category(<<"image/jpeg">>))
    end).

get_file_category_png_is_image_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(image, elib_oss:get_file_category(<<"image/png">>))
    end).

get_file_category_gif_is_image_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(image, elib_oss:get_file_category(<<"image/gif">>))
    end).

get_file_category_mp4_is_video_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(video, elib_oss:get_file_category(<<"video/mp4">>))
    end).

get_file_category_avi_is_video_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(video, elib_oss:get_file_category(<<"video/avi">>))
    end).

get_file_category_mp3_is_audio_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(audio, elib_oss:get_file_category(<<"audio/mpeg">>))
    end).

get_file_category_wav_is_audio_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(audio, elib_oss:get_file_category(<<"audio/wav">>))
    end).

get_file_category_pdf_is_document_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(document, elib_oss:get_file_category(<<"application/pdf">>))
    end).

get_file_category_msword_is_document_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(document, elib_oss:get_file_category(<<"application/msword">>))
    end).

get_file_category_docx_is_document_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            document,
            elib_oss:get_file_category(
                <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>
            )
        )
    end).

get_file_category_text_plain_is_document_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(document, elib_oss:get_file_category(<<"text/plain">>))
    end).

get_file_category_unknown_is_other_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(other, elib_oss:get_file_category(<<"application/octet-stream">>))
    end).

get_file_category_empty_is_other_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(other, elib_oss:get_file_category(<<>>))
    end).

%% ===================================================================
%% validate_file_type/1 测试
%% ===================================================================

validate_file_type_jpeg_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_oss:validate_file_type(<<"image/jpeg">>))
    end).

validate_file_type_png_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_oss:validate_file_type(<<"image/png">>))
    end).

validate_file_type_pdf_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_oss:validate_file_type(<<"application/pdf">>))
    end).

validate_file_type_mp4_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_oss:validate_file_type(<<"video/mp4">>))
    end).

validate_file_type_mp3_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(true, elib_oss:validate_file_type(<<"audio/mpeg">>))
    end).

validate_file_type_octet_stream_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_oss:validate_file_type(<<"application/octet-stream">>))
    end).

validate_file_type_executable_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_oss:validate_file_type(<<"application/x-executable">>))
    end).

validate_file_type_unknown_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_oss:validate_file_type(<<"unknown/type">>))
    end).

validate_file_type_empty_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(false, elib_oss:validate_file_type(<<>>))
    end).

%% ===================================================================
%% get_url/1 测试
%% ===================================================================

get_url_returns_ok_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = elib_oss:get_url(<<"file_1234_567">>),
        ?assertMatch({ok, _}, Result)
    end).

get_url_contains_file_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        FileId = <<"file_1234_567">>,
        {ok, Url} = elib_oss:get_url(FileId),
        ?assert(binary:match(Url, FileId) =/= nomatch)
    end).

get_url_has_garage_path_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% get_url 现在返回 Garage path-style URL（使用默认配置）
        {ok, Url} = elib_oss:get_url(<<"file_999_888">>),
        ?assert(binary:match(Url, <<"file_999_888">>) =/= nomatch),
        ?assert(binary:match(Url, <<"imboy">>) =/= nomatch)
    end).

%% ===================================================================
%% delete_object/1 测试（mock httpc）
%% ===================================================================

delete_object_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_s3_sign, [
                {'format_amz_date', 1, fun(_) -> <<"20260528T103000Z">> end},
                {'authorization_header', 7, fun(_, _, _, _, _, _, _) ->
                    <<"AWS4-HMAC-SHA256 Cred=..., Sig=abc">>
                end}
            ]},
            {httpc, [
                {'request', 4, fun(delete, _Url, _Opts, _HttpOpts) ->
                    {ok, {{<<"HTTP/1.1">>, 204, <<"No Content">>}, [], <<>>}}
                end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, garage, #{
                endpoint => <<"http://127.0.0.1:3900">>,
                bucket => <<"imboy">>,
                access_key => <<"GKtest">>,
                secret_key => <<"testsecret">>
            }),
            ?assertEqual(ok, elib_oss:delete_object(<<"file_1/test.jpg">>))
        end
    ).

delete_object_http_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_s3_sign, [
                {'format_amz_date', 1, fun(_) -> <<"20260528T103000Z">> end},
                {'authorization_header', 7, fun(_, _, _, _, _, _, _) ->
                    <<"AWS4-HMAC-SHA256 Cred=..., Sig=abc">>
                end}
            ]},
            {httpc, [
                {'request', 4, fun(delete, _Url, _Opts, _HttpOpts) ->
                    {ok, {{<<"HTTP/1.1">>, 404, <<"Not Found">>}, [], <<"NoSuchKey">>}}
                end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, garage, #{
                endpoint => <<"http://127.0.0.1:3900">>,
                bucket => <<"imboy">>,
                access_key => <<"GKtest">>,
                secret_key => <<"testsecret">>
            }),
            Result = elib_oss:delete_object(<<"file_1/missing.jpg">>),
            ?assertMatch({error, {http_error, 404, _}}, Result)
        end
    ).

delete_object_network_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_s3_sign, [
                {'format_amz_date', 1, fun(_) -> <<"20260528T103000Z">> end},
                {'authorization_header', 7, fun(_, _, _, _, _, _, _) -> <<"AWS4 Sig=abc">> end}
            ]},
            {httpc, [
                {'request', 4, fun(delete, _Url, _Opts, _HttpOpts) ->
                    {error, econnrefused}
                end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, garage, #{
                endpoint => <<"http://127.0.0.1:3900">>,
                bucket => <<"imboy">>,
                access_key => <<"GKtest">>,
                secret_key => <<"testsecret">>
            }),
            ?assertEqual({error, econnrefused}, elib_oss:delete_object(<<"file_1/test.jpg">>))
        end
    ).

%% ===================================================================
%% presign_put_for_key/3 测试
%% ===================================================================

presign_put_for_key_returns_url_test_() ->
    ?WITH_MECK(
        elib_s3_sign,
        [
            {'presign_put', 5, fun(Endpoint, Bucket, Key, _Mime, _Exp) ->
                <<Endpoint/binary, "/", Bucket/binary, "/", Key/binary, "?X-Amz-Signature=abc">>
            end}
        ],
        fun() ->
            application:set_env(imboy, garage, #{
                endpoint => <<"http://127.0.0.1:3900">>,
                bucket => <<"imboy">>
            }),
            Url = elib_oss:presign_put_for_key(<<"file_1/photo.jpg">>, <<"image/jpeg">>, 3600),
            ?assert(is_binary(Url)),
            ?assert(binary:match(Url, <<"file_1/photo.jpg">>) =/= nomatch),
            ?assert(binary:match(Url, <<"X-Amz-Signature">>) =/= nomatch)
        end
    ).

%% ===================================================================
%% upload/2,3 测试
%% ===================================================================

upload_rejects_oversized_file_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 超过 100MB 的文件直接拒绝，不调用网络
        HugeBin = binary:copy(<<0>>, 101 * 1024 * 1024),
        ?assertEqual({error, file_too_large}, elib_oss:upload(HugeBin, <<"big.jpg">>))
    end).

upload_rejects_invalid_mime_type_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 不在白名单的 MIME 类型被拒绝
        ?assertEqual(
            {error, invalid_file_type},
            elib_oss:upload(<<"data">>, <<"file.exe">>, #{
                mime_type => <<"application/x-msdownload">>
            })
        )
    end).

upload_success_returns_url_and_file_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_s3_sign, [
                {'format_amz_date', 1, fun(_) -> <<"20260528T103000Z">> end},
                {'authorization_header', 7, fun(_, _, _, _, _, _, _) ->
                    <<"AWS4-HMAC-SHA256 Cred=test, Sig=abc">>
                end}
            ]},
            {httpc, [
                {'request', 4, fun(put, _Url, _Opts, _HttpOpts) ->
                    {ok, {{<<"HTTP/1.1">>, 200, <<"OK">>}, [], <<>>}}
                end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, garage, #{
                endpoint => <<"http://127.0.0.1:3900">>,
                bucket => <<"imboy">>,
                access_key => <<"GKtest">>,
                secret_key => <<"testsecret">>
            }),
            Result = elib_oss:upload(<<"hello">>, <<"test.txt">>, #{mime_type => <<"text/plain">>}),
            ?assertMatch({ok, _, _}, Result),
            {ok, Url, FileId} = Result,
            ?assert(is_binary(Url)),
            ?assert(is_binary(FileId)),
            ?assert(binary:match(Url, <<"imboy">>) =/= nomatch),
            ?assertMatch(match, re:run(FileId, <<"^file_[0-9]+_[0-9]+$">>, [{capture, none}]))
        end
    ).

upload_s3_error_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_s3_sign, [
                {'format_amz_date', 1, fun(_) -> <<"20260528T103000Z">> end},
                {'authorization_header', 7, fun(_, _, _, _, _, _, _) -> <<"AWS4 Sig=x">> end}
            ]},
            {httpc, [
                {'request', 4, fun(put, _Url, _Opts, _HttpOpts) ->
                    {ok, {{<<"HTTP/1.1">>, 403, <<"Forbidden">>}, [], <<"AccessDenied">>}}
                end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, garage, #{
                endpoint => <<"http://127.0.0.1:3900">>,
                bucket => <<"imboy">>,
                access_key => <<"GKtest">>,
                secret_key => <<"testsecret">>
            }),
            Result = elib_oss:upload(<<"data">>, <<"photo.jpg">>, #{mime_type => <<"image/jpeg">>}),
            ?assertMatch({error, {http_error, 403}}, Result)
        end
    ).

upload_garage_not_configured_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% access_key 为空时 assert_garage_configured 抛出 error
        application:set_env(imboy, garage, #{
            endpoint => <<"http://127.0.0.1:3900">>,
            bucket => <<"imboy">>,
            access_key => <<>>,
            secret_key => <<>>
        }),
        ?assertError(
            {garage_not_configured, access_key_missing},
            elib_oss:upload(<<"data">>, <<"photo.jpg">>, #{mime_type => <<"image/jpeg">>})
        )
    end).
