-module(attachment_upload_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% 使用真实私有文件和 hash，模拟数据库准入与 Garage 写入。
-define(KEY, <<"u7/file/20260911/file_1/x.jpg">>).
-define(MIME, <<"image/jpeg">>).

upload_success_pins_and_puts_frozen_garage_test() ->
    with_garage(fun(C) ->
        ?assertEqual(
            {ok, #{
                <<"object_key">> => ?KEY,
                <<"mime_type">> => ?MIME,
                <<"size">> => byte_size(maps:get(bytes, C))
            }},
            upload(C)
        ),
        ?assertEqual(maps:get(bytes, C), get(garage_bytes)),
        ?assertEqual(receipt(maps:get(bytes, C)), get(multipart_receipt)),
        ?assertEqual(1, meck:num_calls(attach_pending_repo, register_content_pin, 2)),
        ?assertEqual(1, meck:num_calls(attach_pending_repo, recheck_content_pin, 3)),
        ?assertEqual(1, meck:num_calls(attachment_storage_garage, put_snapshot, 4))
    end).

%% V3-19 退役守卫（C09）：elib_oss 不再提供磁盘文件→对象存储的 BEAM 内
%% 写通道；该面唯一合法路径是受限 Worker（attachment_worker_port:upload_file
%% → assembly_io_worker.escript，Worker 进程内自签 presign PUT+checksum）。
%% meck 无法 mock 不存在的函数，退役以导出面断言承载（mock 卸载后亦真）。
put_object_from_file_retired_test() ->
    meck_unload_guard(),
    {module, elib_oss} = code:ensure_loaded(elib_oss),
    ?assertNot(erlang:function_exported(elib_oss, put_object_from_file, 4)),
    ?assertNot(erlang:function_exported(elib_oss, put_object_from_file_ctx, 5)),
    assert_put_object_from_file_retired().

meck_unload_guard() ->
    try
        meck:unload(elib_oss)
    catch
        _:_ -> ok
    end,
    ok.

assert_put_object_from_file_retired() ->
    ?assertNot(erlang:function_exported(elib_oss, put_object_from_file, 4)),
    ?assertNot(erlang:function_exported(elib_oss, put_object_from_file_ctx, 5)).

upload_rejects_other_owner_key_prefix_test() ->
    with_fixture(fun(C) ->
        ?assertEqual(
            {error, forbidden_key},
            attachment_upload_logic:upload_multipart(
                7, <<"u8/file/20260911/file_1/x.jpg">>, ?MIME, maps:get(path, C), 8
            )
        ),
        ?assertEqual(0, meck:num_calls(attach_pending_repo, status_lookup, 1)),
        assert_unwritten(C)
    end).

upload_rejects_pending_owner_mismatch_test() ->
    with_fixture(fun(C) ->
        pending((maps:get(row, C))#{<<"creator_user_id">> => 8}),
        ?assertEqual({error, forbidden_key}, upload(C)),
        assert_unwritten(C)
    end).

upload_rejects_unpresigned_key_test() ->
    with_fixture(fun(C) ->
        meck:expect(attach_pending_repo, status_lookup, fun(_) -> {error, not_found} end),
        ?assertEqual({error, object_not_found}, upload(C)),
        assert_unwritten(C)
    end).

retired_scope_rejects_upload_test() ->
    lists:foreach(
        fun(Scope) ->
            with_fixture(fun(C) ->
                pending(scope_row(maps:get(row, C), Scope, ?MIME)),
                ?assertEqual({error, forbidden}, upload(C)),
                assert_unwritten(C)
            end)
        end,
        [<<"teaching">>, <<"moya">>]
    ).

upload_global_mime_whitelist_rejects_test() ->
    with_fixture(fun(C) ->
        ?assertEqual(
            {error, invalid_file_type},
            attachment_upload_logic:upload_multipart(
                7, ?KEY, <<"application/zip">>, maps:get(path, C), 8
            )
        ),
        assert_unwritten(C)
    end).

upload_rejects_oversize_test() ->
    with_fixture(fun(C) ->
        ?assertEqual({error, file_too_large}, upload(C, 101)),
        assert_unwritten(C)
    end).

upload_rechecks_actual_size_before_pin_test() ->
    with_fixture(fun(C) ->
        ok = file:write_file(maps:get(path, C), binary:copy(<<"x">>, 101)),
        ?assertEqual({error, file_too_large}, upload(C, 1)),
        assert_unwritten(C)
    end).

upload_receipt_and_response_use_actual_size_test() ->
    with_garage(fun(C) ->
        {ok, Data} = upload(C, 1),
        ?assertEqual(byte_size(maps:get(bytes, C)), maps:get(<<"size">>, Data)),
        ?assertEqual(receipt(maps:get(bytes, C)), get(multipart_receipt))
    end).

upload_rejects_session_mime_substitution_test() ->
    with_fixture(fun(C) ->
        ?assertEqual(
            {error, content_mismatch},
            attachment_upload_logic:upload_multipart(
                7, ?KEY, <<"image/png">>, maps:get(path, C), 8
            )
        ),
        assert_unwritten(C)
    end).

upload_rejects_session_subject_and_scope_substitution_test() ->
    with_fixture(fun(C) ->
        Row = maps:get(row, C),
        Registration = maps:get(<<"upload_registration">>, Row),
        pending(Row#{
            <<"upload_registration">> => Registration#{
                <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"8">>}
            }
        }),
        ?assertEqual({error, forbidden_key}, upload(C)),
        pending(Row#{<<"scope">> => <<"public">>}),
        ?assertEqual({error, scope_conflict}, upload(C)),
        assert_unwritten(C)
    end).

upload_rejects_closed_or_expired_session_test() ->
    with_fixture(fun(C) ->
        Row = maps:get(row, C),
        lists:foreach(
            fun(Change) ->
                pending(maps:merge(Row, Change)),
                ?assertEqual({error, session_not_renewable}, upload(C))
            end,
            [
                #{<<"session_state">> => <<"confirmed">>},
                #{<<"session_state">> => <<"delete_intent">>},
                #{<<"client_upload_id">> => null},
                #{<<"upload_expires_at">> => timestamp(erlang:system_time(second) - 1)},
                #{<<"session_deadline">> => timestamp(erlang:system_time(second) - 1)},
                #{<<"upload_expires_at">> => <<"invalid timestamp">>}
            ]
        ),
        assert_unwritten(C)
    end).

upload_rejects_unknown_and_changed_locations_test() ->
    with_fixture(fun(C) ->
        Row = maps:get(row, C),
        pending(
            maps:without(
                [
                    <<"storage_driver">>,
                    <<"storage_namespace">>,
                    <<"storage_container">>,
                    <<"storage_locator_sha256">>
                ],
                Row
            )
        ),
        ?assertEqual({error, historical_location_unverified}, upload(C)),
        pending(Row#{<<"storage_locator_sha256">> => binary:copy(<<"0">>, 64)}),
        ?assertEqual({error, location_changed}, upload(C)),
        pending(Row#{<<"storage_driver">> => null}),
        ?assertEqual({error, location_unverified}, upload(C)),
        assert_unwritten(C)
    end).

upload_path_and_registry_replacement_cannot_change_pinned_bytes_test() ->
    with_garage(fun(C) ->
        meck:expect(attach_pending_repo, register_content_pin, fun(?KEY, Receipt) ->
            ?assertEqual(receipt(maps:get(bytes, C)), Receipt),
            put(multipart_receipt, Receipt),
            ok = file:delete(maps:get(path, C)),
            ok = file:write_file(maps:get(path, C), <<"replacement bytes">>),
            application:set_env(imboy, attachment_storage_registry, #{version => 1, roots => #{}}),
            ok
        end),
        ?assertMatch({ok, _}, upload(C)),
        ?assertEqual(maps:get(bytes, C), get(garage_bytes)),
        ?assertEqual({ok, <<"replacement bytes">>}, file:read_file(maps:get(path, C)))
    end).

upload_identical_retry_and_different_content_conflict_test() ->
    with_garage(fun(C) ->
        ?assertMatch({ok, _}, upload(C)),
        ?assertMatch({ok, _}, upload(C)),
        ok = file:write_file(maps:get(path, C), <<"different">>),
        ?assertEqual({error, content_mismatch}, upload(C)),
        ?assertEqual(maps:get(bytes, C), get(garage_bytes)),
        ?assertEqual(2, meck:num_calls(attachment_storage_garage, put_snapshot, 4)),
        ?assertEqual(2, meck:num_calls(attach_pending_repo, recheck_content_pin, 3))
    end).

upload_pin_rejections_never_reach_storage_test() ->
    with_fixture(fun(C) ->
        lists:foreach(
            fun({Reason, Expected}) ->
                meck:expect(
                    attach_pending_repo,
                    register_content_pin,
                    fun(_, _) -> {error, Reason} end
                ),
                ?assertEqual({error, Expected}, upload(C)),
                %% V3-20 disk 退役：target 文件断言改为 garage put 计数为 0。
                ?assertEqual(0, meck:num_calls(attachment_storage_garage, put_snapshot, 4))
            end,
            [
                {content_mismatch, content_mismatch},
                {tombstone, tombstone},
                {session_not_renewable, session_not_renewable},
                {location_changed, location_changed},
                {not_found, object_not_found},
                {pool_down, {db_error, pool_down}}
            ]
        ),
        ?assertEqual(0, meck:num_calls(attach_pending_repo, recheck_content_pin, 3))
    end).

upload_garage_live_recheck_rejections_never_publish_test() ->
    with_garage(fun(C) ->
        lists:foreach(
            fun({Reason, Expected}) ->
                meck:expect(attach_pending_repo, recheck_content_pin, fun(?KEY, Receipt, Identity) ->
                    ?assertEqual(get(multipart_receipt), Receipt),
                    ?assertEqual(maps:get(garage_identity, C), Identity),
                    {error, Reason}
                end),
                ?assertEqual({error, Expected}, upload(C)),
                ?assertEqual(0, meck:num_calls(attachment_storage_garage, put_snapshot, 4))
            end,
            [
                {content_mismatch, content_mismatch},
                {tombstone, tombstone},
                {session_not_renewable, session_not_renewable},
                {location_changed, location_changed},
                {not_found, object_not_found},
                {pool_down, {db_error, pool_down}}
            ]
        )
    end).

upload_garage_recheck_cannot_extend_frozen_expiry_test_() ->
    {timeout, 15, fun upload_garage_recheck_cannot_extend_frozen_expiry/0}.

upload_garage_recheck_cannot_extend_frozen_expiry() ->
    with_fixture(fun(C) ->
        Garage = garage_session(C),
        Expiry = erlang:system_time(second) + 4,
        pending((maps:get(row, Garage))#{<<"upload_expires_at">> => timestamp(Expiry)}),
        meck:expect(attach_pending_repo, recheck_content_pin, fun(_, _, _) ->
            pending(maps:get(row, Garage)),
            timer:sleep(max(0, Expiry * 1000 - erlang:system_time(millisecond) + 20)),
            ok
        end),
        meck:new(httpc, [passthrough, no_link, unstick]),
        try
            %% 公共路径默认拦截 put_snapshot；本用例验证过期签名上下文在
            %% 真实 put_snapshot 入口 fail-closed（不发 http），恢复 passthrough。
            meck:expect(
                attachment_storage_garage,
                put_snapshot,
                fun(I, Source, Mime, Ctx) -> meck:passthrough([I, Source, Mime, Ctx]) end
            ),
            ?assertEqual({error, invalid_signature_context}, upload(C)),
            ?assertEqual(0, meck:num_calls(httpc, request, 4))
        after
            meck:unload(httpc)
        end
    end).

upload_retired_disk_fails_closed_test() ->
    %% V3-20 disk 退役：独立 disk root fixture（不经共享 with_fixture 的
    %% frozen_location 成功路径），拒绝点前移到 storage 层 frozen_location。
    with_fixture(fun(C) ->
        RootId = <<"multipart-test-disk-retired">>,
        Rec = #{
            driver => <<"disk">>,
            namespace => <<"multipart-test-disk">>,
            container => <<"frozen-bucket">>,
            endpoint => <<"file:///nonexistent/multipart-test-disk">>
        },
        application:set_env(
            imboy,
            attachment_storage_registry,
            #{version => 1, roots => #{RootId => Rec}}
        ),
        pending(location_row(base_row(maps:get(expiry, C)), RootId, Rec)),
        ?assertEqual({error, unsupported_driver}, upload(C)),
        %% 失败发生在 pin 之前：receipt 未写入、存储与 recheck 均未触达。
        ?assertEqual(undefined, get(multipart_receipt)),
        ?assertEqual(0, meck:num_calls(attach_pending_repo, register_content_pin, 2)),
        ?assertEqual(0, meck:num_calls(attach_pending_repo, recheck_content_pin, 3)),
        ?assertEqual(0, meck:num_calls(attachment_storage_garage, put_snapshot, 4))
    end).

upload_db_fault_tagged_test() ->
    with_fixture(fun(C) ->
        meck:expect(attach_pending_repo, status_lookup, fun(_) -> {error, pool_down} end),
        ?assertEqual({error, {db_error, pool_down}}, upload(C)),
        assert_unwritten(C)
    end).

upload_spool_error_tagged_storage_error_test() ->
    with_fixture(fun(C) ->
        ok = file:delete(maps:get(path, C)),
        ?assertEqual({error, {storage_error, enoent}}, upload(C)),
        assert_unwritten(C)
    end).

upload_garage_uses_frozen_identity_original_expiry_and_retained_bytes_test() ->
    with_fixture(fun(C) ->
        Garage = garage_session(C),
        pending(maps:get(row, Garage)),
        meck:expect(attach_pending_repo, register_content_pin, fun(?KEY, Receipt) ->
            put(multipart_receipt, Receipt),
            ok = file:write_file(maps:get(path, C), <<"replacement">>),
            application:set_env(imboy, attachment_storage_registry, #{version => 1, roots => #{}}),
            application:set_env(imboy, garage, #{endpoint => <<"http://wrong.invalid">>}),
            ok
        end),
        meck:expect(attach_pending_repo, recheck_content_pin, fun(?KEY, Receipt, Identity) ->
            ?assertEqual(get(multipart_receipt), Receipt),
            ?assertEqual(maps:get(identity, Garage), Identity),
            ok
        end),
        meck:new(elib_s3_sign, [passthrough, no_link]),
        meck:new(httpc, [passthrough, no_link, unstick]),
        try
            %% 公共路径默认拦截 put_snapshot；本用例验证真实 put_snapshot 的
            %% presign/producer 路径（网络由 httpc meck 拦截），恢复 passthrough。
            meck:expect(
                attachment_storage_garage,
                put_snapshot,
                fun(I, Source, Mime, Ctx) -> meck:passthrough([I, Source, Mime, Ctx]) end
            ),
            meck:expect(
                elib_s3_sign,
                presign_url_at,
                fun(Method, EP, Bucket, Key, Mime, Issued, Ttl, Config) ->
                    ?assertEqual(
                        {<<"PUT">>, <<"http://garage.invalid:3900">>, <<"frozen-bucket">>, ?KEY,
                            ?MIME},
                        {Method, EP, Bucket, Key, Mime}
                    ),
                    ?assertEqual(maps:get(config, Garage), Config),
                    ?assertEqual(
                        maps:get(expiry, C),
                        calendar:datetime_to_gregorian_seconds(Issued) - 62167219200 + Ttl
                    ),
                    meck:passthrough([Method, EP, Bucket, Key, Mime, Issued, Ttl, Config])
                end
            ),
            meck:expect(httpc, request, fun(put, {Url, Headers, "image/jpeg", {Producer, 0}}, _, _) ->
                ?assert(lists:prefix("http://garage.invalid:3900/frozen-bucket/", Url)),
                ?assertEqual(
                    integer_to_list(byte_size(maps:get(bytes, C))),
                    proplists:get_value("content-length", Headers)
                ),
                ?assertEqual(maps:get(bytes, C), read_producer(Producer, 0, [])),
                ?assertEqual(maps:get(bytes, C), read_producer(Producer, 0, [])),
                {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}
            end),
            ?assertMatch({ok, _}, upload(C)),
            ?assertEqual(1, meck:num_calls(httpc, request, 4)),
            ?assert(meck:validate(elib_s3_sign)),
            ?assert(meck:validate(httpc))
        after
            meck:unload([elib_s3_sign, httpc])
        end
    end).

upload_garage_error_tagged_storage_error_test() ->
    with_fixture(fun(C) ->
        Garage = garage_session(C),
        pending(maps:get(row, Garage)),
        meck:expect(attach_pending_repo, recheck_content_pin, fun(_, _, _) -> ok end),
        meck:new(httpc, [passthrough, no_link, unstick]),
        try
            %% 公共路径默认拦截 put_snapshot；本用例验证 httpc 500 错误经真实
            %% put_snapshot 打标签，恢复 passthrough（网络仍被 httpc meck 拦截）。
            meck:expect(
                attachment_storage_garage,
                put_snapshot,
                fun(I, Source, Mime, Ctx) -> meck:passthrough([I, Source, Mime, Ctx]) end
            ),
            meck:expect(httpc, request, fun(put, _, _, _) ->
                {ok, {{"HTTP/1.1", 500, "Error"}, [], <<"boom">>}}
            end),
            ?assertEqual({error, {storage_error, {storage_status, 500}}}, upload(C)),
            ?assertEqual(receipt(maps:get(bytes, C)), get(multipart_receipt)),
            ?assert(meck:validate(httpc))
        after
            meck:unload(httpc)
        end
    end).

upload_garage_recheck_rejection_prevents_http_test() ->
    with_fixture(fun(C) ->
        Garage = garage_session(C),
        pending(maps:get(row, Garage)),
        meck:expect(
            attach_pending_repo,
            recheck_content_pin,
            fun(_, _, _) -> {error, tombstone} end
        ),
        meck:new(httpc, [passthrough, no_link, unstick]),
        try
            ?assertEqual({error, tombstone}, upload(C)),
            ?assertEqual(0, meck:num_calls(httpc, request, 4))
        after
            meck:unload(httpc)
        end
    end).

with_garage(Fun) ->
    with_fixture(fun(C) ->
        Garage = garage_session(C),
        Identity = maps:get(identity, Garage),
        pending(maps:get(row, Garage)),
        meck:expect(attach_pending_repo, recheck_content_pin, fun(?KEY, Receipt, I) ->
            ?assertEqual(get(multipart_receipt), Receipt),
            ?assertEqual(Identity, I),
            ok
        end),
        %% put_snapshot 模块级 meck 已上提 with_fixture；此处覆盖为记录字节的
        %% 拦截（garage root 迁移后语义不变）。
        try
            meck:expect(attachment_storage_garage, put_snapshot, fun(I, Source, ?MIME, _) ->
                ?assertEqual(Identity, I),
                put(garage_bytes, read_spool(Source, 0, [])),
                ok
            end),
            Fun(C#{garage_identity => Identity})
        after
            erase(garage_bytes)
        end
    end).

read_spool(Source, Offset, Acc) ->
    case attachment_upload_spool:read(Source, Offset) of
        {ok, Bytes} -> read_spool(Source, Offset + byte_size(Bytes), [Bytes | Acc]);
        eof -> iolist_to_binary(lists:reverse(Acc))
    end.

with_fixture(Fun) ->
    Dir = filename:join(
        tmp_root(),
        "imboy-multipart-test-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, 8#700),
    Saved = [{K, application:get_env(imboy, K)} || K <- [attachment_storage_registry, garage]],
    erase(multipart_receipt),
    %% V3-20 disk 退役：garage put_snapshot 拦截上提公共路径，任何用例不会真发网络。
    meck:new([attach_pending_repo, elib_oss, attachment_storage_garage], [passthrough, no_link]),
    try
        Bytes = <<"verified">>,
        Path = filename:join(Dir, "input.part"),
        ok = file:write_file(Path, Bytes),
        %% V3-20 disk 退役：共享根从 disk root（file:// endpoint）迁移为 garage root
        %% （与 garage_session/1 同一模板），frozen_location 走 garage 分支成功。
        RootId = <<"multipart-test-garage-root">>,
        Config = #{
            endpoint => <<"http://garage.invalid:3900">>,
            region => <<"garage">>,
            access_key => <<"test-access">>,
            secret_key => <<"test-secret">>
        },
        Rec = #{
            driver => <<"garage">>,
            namespace => <<"multipart-test-garage">>,
            container => <<"frozen-bucket">>,
            endpoint => maps:get(endpoint, Config)
        },
        application:set_env(
            imboy,
            attachment_storage_registry,
            #{version => 1, roots => #{RootId => Rec}}
        ),
        application:set_env(imboy, garage, Config),
        Expiry = erlang:system_time(second) + 300,
        Row = location_row(base_row(Expiry), RootId, Rec),
        {ok, Identity} = attachment_storage:frozen_location(?KEY, Row),
        C = #{
            path => Path,
            bytes => Bytes,
            row => Row,
            identity => Identity,
            expiry => Expiry
        },
        pending(Row),
        meck:expect(attach_pending_repo, register_content_pin, fun(?KEY, Receipt) ->
            case get(multipart_receipt) of
                undefined ->
                    put(multipart_receipt, Receipt),
                    ok;
                Receipt ->
                    ok;
                _ ->
                    {error, content_mismatch}
            end
        end),
        meck:expect(attach_pending_repo, recheck_content_pin, fun(?KEY, Receipt, I) ->
            ?assertEqual(get(multipart_receipt), Receipt),
            ?assertEqual(Identity, I),
            ok
        end),
        meck:expect(elib_oss, max_file_size, fun() -> 100 end),
        %% 公共默认拦截：返回 ok（真实 put_snapshot 路径用例按需覆盖为 passthrough）。
        meck:expect(attachment_storage_garage, put_snapshot, fun(_I, _Source, _Mime, _Ctx) ->
            ok
        end),
        Fun(C),
        ?assert(meck:validate(attach_pending_repo)),
        ?assert(meck:validate(elib_oss)),
        ?assert(meck:validate(attachment_storage_garage)),
        assert_put_object_from_file_retired()
    after
        meck:unload([attach_pending_repo, elib_oss, attachment_storage_garage]),
        erase(multipart_receipt),
        lists:foreach(
            fun
                ({K, undefined}) -> application:unset_env(imboy, K);
                ({K, {ok, V}}) -> application:set_env(imboy, K, V)
            end,
            Saved
        ),
        file:del_dir_r(Dir)
    end.

%% macOS 将 /tmp 软链到 /private/tmp；no_symlinks 逐段校验会拒绝 /tmp 路径。
tmp_root() ->
    case file:read_link_all("/tmp") of
        {ok, Real} -> filename:join("/", unicode:characters_to_list(Real));
        {error, _} -> "/tmp"
    end.

base_row(Expiry) ->
    #{
        <<"object_key">> => ?KEY,
        <<"bucket">> => <<"legacy-bucket">>,
        <<"scope">> => <<"private">>,
        <<"creator_user_id">> => 7,
        <<"session_state">> => <<"registered">>,
        <<"client_upload_id">> => <<"00000000-0000-4000-8000-000000000007">>,
        <<"upload_expires_at">> => timestamp(Expiry),
        <<"session_deadline">> => timestamp(Expiry + 300),
        <<"upload_registration">> => #{
            <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"7">>},
            <<"declaration">> => #{<<"mime">> => ?MIME, <<"scope">> => <<"private">>}
        }
    }.

location_row(Row, RootId, Rec) ->
    Row#{
        <<"storage_driver">> => maps:get(driver, Rec),
        <<"storage_namespace">> => maps:get(namespace, Rec),
        <<"storage_container">> => maps:get(container, Rec),
        <<"storage_locator_sha256">> => attachment_storage:locator_digest(
            maps:get(namespace, Rec), Rec#{root_id => RootId}
        )
    }.

scope_row(Row, Scope, Mime) ->
    Registration = maps:get(<<"upload_registration">>, Row),
    Declaration = maps:get(<<"declaration">>, Registration),
    Row#{
        <<"scope">> => Scope,
        <<"upload_registration">> => Registration#{
            <<"declaration">> => Declaration#{<<"scope">> => Scope, <<"mime">> => Mime}
        }
    }.

pending(Row) -> meck:expect(attach_pending_repo, status_lookup, fun(?KEY) -> {ok, Row} end).
upload(C) -> upload(C, byte_size(maps:get(bytes, C))).
upload(C, Size) ->
    attachment_upload_logic:upload_multipart(7, ?KEY, ?MIME, maps:get(path, C), Size).
assert_unwritten(C) ->
    %% V3-20 disk 退役：磁盘 target 断言改为 garage put 计数为 0（未触存储）。
    ?assertEqual(0, meck:num_calls(attachment_storage_garage, put_snapshot, 4)),
    ?assertEqual(0, meck:num_calls(attach_pending_repo, register_content_pin, 2)),
    ?assertEqual(0, meck:num_calls(attach_pending_repo, recheck_content_pin, 3)).
receipt(Bytes) ->
    #{
        <<"version">> => 1,
        <<"sha256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
        <<"size_bytes">> => integer_to_binary(byte_size(Bytes)),
        <<"mime">> => ?MIME
    }.
timestamp(Seconds) -> list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{offset, "Z"}])).

garage_session(C) ->
    RootId = <<"multipart-test-garage-root">>,
    Config = #{
        endpoint => <<"http://garage.invalid:3900">>,
        region => <<"garage">>,
        access_key => <<"test-access">>,
        secret_key => <<"test-secret">>
    },
    Rec = #{
        driver => <<"garage">>,
        namespace => <<"multipart-test-garage">>,
        container => <<"frozen-bucket">>,
        endpoint => maps:get(endpoint, Config)
    },
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{RootId => Rec}
    }),
    application:set_env(imboy, garage, Config),
    Row = location_row(maps:get(row, C), RootId, Rec),
    {ok, Identity} = attachment_storage:frozen_location(?KEY, Row),
    #{row => Row, identity => Identity, config => Config}.
read_producer(Producer, Offset, Acc) ->
    case Producer(Offset) of
        {ok, Bytes, Next} -> read_producer(Producer, Next, [Bytes | Acc]);
        eof -> iolist_to_binary(lists:reverse(Acc))
    end.
