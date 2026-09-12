-module(attachment_upload_logic_tests).
%%%
% attachment_upload_logic（multipart 直传端点逻辑）单元测试
% 覆盖：成功路径（mock elib_oss）、越权 object_key（owner 前缀/ pending 归属）、
% 未 presign 拒、mime 白名单拒（teaching 白名单 + 全局白名单）、超限拒、
% Garage 写失败透传。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_setup.hrl").

-define(KEY, <<"u7/file/20260911/file_1/x.jpg">>).

pending_ok() ->
    fun(_K) ->
        {ok, #{
            <<"object_key">> => ?KEY,
            <<"bucket">> => <<"imboy">>,
            <<"scope">> => <<"private">>,
            <<"creator_user_id">> => 7,
            <<"created_at">> => null
        }}
    end.

%% ===================================================================

upload_success_puts_bucket_object_from_file_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [{'get_by_key', 1, pending_ok()}]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            {ok, Data} = attachment_upload_logic:upload_multipart(
                7, ?KEY, <<"image/jpeg">>, "/tmp/fake.part", 99
            ),
            ?assertEqual(<<"u7/file/20260911/file_1/x.jpg">>, maps:get(<<"object_key">>, Data)),
            ?assertEqual(<<"image/jpeg">>, maps:get(<<"mime_type">>, Data)),
            ?assertEqual(99, maps:get(<<"size">>, Data)),
            ?assertEqual(1, meck:num_calls(elib_oss, put_object_from_file, 4)),
            ?assert(
                meck:called(
                    elib_oss, put_object_from_file, [
                        <<"imboy">>, ?KEY, "/tmp/fake.part", <<"image/jpeg">>
                    ]
                )
            )
        end
    ).

%% object_key 前缀不是 u<Uid>/ → 直接拒（pending 都不查）
upload_rejects_other_owner_key_prefix_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [{'get_by_key', 1, pending_ok()}]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            OtherKey = <<"u8/file/20260911/file_1/x.jpg">>,
            ?assertEqual(
                {error, forbidden_key},
                attachment_upload_logic:upload_multipart(
                    7, OtherKey, <<"image/jpeg">>, "/tmp/fake.part", 10
                )
            ),
            ?assertEqual(0, meck:num_calls(attach_pending_repo, get_by_key, 1)),
            ?assertEqual(0, meck:num_calls(elib_oss, put_object_from_file, 4))
        end
    ).

%% pending 行归属他人 → 拒（防写他人对象）
upload_rejects_pending_owner_mismatch_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'get_by_key', 1, fun(_K) ->
                    {ok, #{
                        <<"bucket">> => <<"imboy">>,
                        <<"scope">> => <<"private">>,
                        <<"creator_user_id">> => 8
                    }}
                end}
            ]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, forbidden_key},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"image/jpeg">>, "/tmp/fake.part", 10
                )
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, put_object_from_file, 4))
        end
    ).

%% 未 presign（无 pending 登记）→ 拒
upload_rejects_unpresigned_key_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'get_by_key', 1, fun(_K) -> {error, not_found} end}
            ]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, object_not_found},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"image/jpeg">>, "/tmp/fake.part", 10
                )
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, put_object_from_file, 4))
        end
    ).

%% teaching scope 走教学白名单（gif 不在 teaching 白名单内）
upload_teaching_mime_whitelist_rejects_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'get_by_key', 1, fun(_K) ->
                    {ok, #{
                        <<"bucket">> => <<"imboy">>,
                        <<"scope">> => <<"teaching">>,
                        <<"creator_user_id">> => 7
                    }}
                end}
            ]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, invalid_file_type},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"image/gif">>, "/tmp/fake.part", 10
                )
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, put_object_from_file, 4))
        end
    ).

%% teaching scope 白名单内的 mime 正常放行
upload_teaching_mime_whitelist_allows_jpeg_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'get_by_key', 1, fun(_K) ->
                    {ok, #{
                        <<"bucket">> => <<"imboy">>,
                        <<"scope">> => <<"teaching">>,
                        <<"creator_user_id">> => 7
                    }}
                end}
            ]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, _},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"image/jpeg">>, "/tmp/fake.part", 10
                )
            )
        end
    ).

%% 全局白名单拒（application/zip 不在 ALLOWED_TYPES）
upload_global_mime_whitelist_rejects_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [{'get_by_key', 1, pending_ok()}]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, invalid_file_type},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"application/zip">>, "/tmp/fake.part", 10
                )
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, put_object_from_file, 4))
        end
    ).

%% 超过 max_file_size 拒（快速失败，不落 Garage）
upload_rejects_oversize_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [{'get_by_key', 1, pending_ok()}]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, file_too_large},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"image/jpeg">>, "/tmp/fake.part", 101
                )
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, put_object_from_file, 4))
        end
    ).

%% Garage 写失败 → {storage_error,_} 标签（handler 映射 5xx；客户端可重传，
%% 对象幂等覆盖）
upload_garage_error_tagged_storage_error_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [{'get_by_key', 1, pending_ok()}]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) ->
                    {error, {http_status, 500, <<"boom">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {storage_error, {http_status, 500, <<"boom">>}}},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"image/jpeg">>, "/tmp/fake.part", 10
                )
            )
        end
    ).

%% pending 查询 DB 故障（非 not_found）→ {db_error,_} 标签，不落 4xx
upload_db_fault_tagged_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'get_by_key', 1, fun(_K) -> {error, pool_down} end}
            ]},
            {elib_oss, [
                {'max_file_size', 0, fun() -> 100 end},
                {'put_object_from_file', 4, fun(_B, _K, _P, _M) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {db_error, pool_down}},
                attachment_upload_logic:upload_multipart(
                    7, ?KEY, <<"image/jpeg">>, "/tmp/fake.part", 10
                )
            ),
            ?assertEqual(0, meck:num_calls(elib_oss, put_object_from_file, 4))
        end
    ).
