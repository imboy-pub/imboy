-module(group_file_ds_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% group_file_ds 模块的 EUnit 测试
%%%
%%% 测试群文件数据服务层的所有功能
%%%===================================================================

%% C08（V3-12）：upload_file/5 及其用例已随 multipart 正文上传链（"登记即
%% 绑定"）退役；群文件唯一引用入口为 bind_file（direct-v1 confirmed 绑定），
%% 其资格/幂等/元数据合同由下方 bind_* 用例与 attach_direct_bind_tests 覆盖。

%% ===================================================================
%% download_file/2 测试
%% ===================================================================

download_file_success_test_() ->
    with_scope(fun() ->
        FileId = 1,
        CurrentUid = 100,
        FileUrl = <<"http://example.com/file.pdf">>,
        Parent = self(),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"status">> => 1, <<"id">> => 1, <<"group_id">> => 1, <<"file_url">> => FileUrl}
        end),
        meck:expect(group_file_repo, increment_download_tx, fun(_Conn, DownloadFileId) ->
            Parent ! {increment_download_called, DownloadFileId},
            {ok, 1}
        end),
        %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑（下载计数 freeze 路径）
        meck:new(workspace_resolver, [passthrough]),
        meck:expect(workspace_resolver, resolve_workspace, fun(_Target) -> personal end),
        meck:new(elib_pg, [passthrough]),
        meck:expect(elib_pg, with_tx, fun(F) -> F(fake_conn) end),

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        Result = group_file_ds:download_file(FileId, CurrentUid),

        ?assertEqual({ok, FileUrl}, Result),
        receive
            {increment_download_called, 1} -> ok
        after 200 ->
            ?assert(false)
        end,

        meck:unload(group_ds),
        meck:unload(elib_pg),
        meck:unload(workspace_resolver),
        meck:unload(group_file_repo),
        ok
    end).

download_file_not_member_test_() ->
    with_scope(fun() ->
        FileId = 1,
        CurrentUid = 999,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{
                <<"status">> => 1,
                <<"id">> => 1,
                <<"group_id">> => 1,
                <<"file_url">> => <<"http://example.com/file.pdf">>
            }
        end),

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> false end),

        Result = group_file_ds:download_file(FileId, CurrentUid),

        meck:unload(group_ds),
        meck:unload(group_file_repo),

        ?assertMatch({error, not_member}, Result)
    end).

download_file_not_found_test_() ->
    with_scope(fun() ->
        FileId = 999999,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) -> #{} end),

        Result = group_file_ds:download_file(FileId, CurrentUid),

        meck:unload(group_file_repo),

        ?assertMatch({error, not_found}, Result)
    end).

%% ===================================================================
%% delete_file/2 测试
%% ===================================================================

delete_file_as_uploader_test_() ->
    with_scope(fun() ->
        FileId = 115787467376035840,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"status">> => 1, <<"id">> => FileId, <<"group_id">> => 1, <<"uploader_id">> => 100}
        end),
        meck:expect(group_file_repo, soft_delete_tx, fun(_Conn, Id) ->
            ?assertEqual(FileId, Id),
            {ok, 1}
        end),
        %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑
        meck:new(workspace_resolver, [passthrough]),
        meck:expect(workspace_resolver, resolve_workspace, fun(_Target) -> personal end),
        meck:new(elib_pg, [passthrough]),
        meck:expect(elib_pg, with_tx, fun(F) -> F(fake_conn) end),

        Results = [
            group_file_ds:delete_file(Id, CurrentUid)
         || Id <- [FileId, integer_to_binary(FileId)]
        ],

        meck:unload(elib_pg),
        meck:unload(workspace_resolver),
        meck:unload(group_file_repo),

        ?assertEqual([ok, ok], Results)
    end).

delete_file_as_admin_test_() ->
    with_scope(fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"status">> => 1, <<"id">> => 1, <<"group_id">> => 1, <<"uploader_id">> => 200}
        end),
        meck:expect(group_file_repo, soft_delete_tx, fun(_Conn, _) -> {ok, 1} end),
        %% T7 归档写守卫收口适配：personal 直通 + with_tx 直跑
        meck:new(workspace_resolver, [passthrough]),
        meck:expect(workspace_resolver, resolve_workspace, fun(_Target) -> personal end),
        meck:new(elib_pg, [passthrough]),
        meck:expect(elib_pg, with_tx, fun(F) -> F(fake_conn) end),

        meck:new(group_member_repo, [passthrough]),
        meck:expect(group_member_repo, find, fun(_, _, _) ->
            % 管理员
            #{<<"role">> => 3}
        end),

        Result = group_file_ds:delete_file(FileId, CurrentUid),

        meck:unload(group_member_repo),
        meck:unload(elib_pg),
        meck:unload(workspace_resolver),
        meck:unload(group_file_repo),

        ?assertMatch(ok, Result)
    end).

delete_file_permission_denied_test_() ->
    with_scope(fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"status">> => 1, <<"id">> => 1, <<"group_id">> => 1, <<"uploader_id">> => 200}
        end),

        meck:new(group_member_repo, [passthrough]),
        meck:expect(group_member_repo, find, fun(_, _, _) ->
            % 普通成员
            #{<<"role">> => 1}
        end),

        Result = group_file_ds:delete_file(FileId, CurrentUid),

        meck:unload(group_member_repo),
        meck:unload(group_file_repo),

        ?assertMatch({error, permission_denied}, Result)
    end).

%% ===================================================================
%% list_files/4 测试
%% ===================================================================

list_files_success_test_() ->
    with_scope(fun() ->
        Gid = 1,
        CurrentUid = 100,
        Page = 1,
        Size = 20,
        ExpectedFiles = [
            #{<<"status">> => 1, <<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
        ],

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, list_by_group, fun(Gid0, Page0, Size0, Options0) ->
            ?assertEqual(Gid, Gid0),
            ?assertEqual(Page, Page0),
            ?assertEqual(Size, Size0),
            ?assertEqual(#{}, Options0),
            {ok, ExpectedFiles}
        end),

        Result = group_file_ds:list_files(Gid, CurrentUid, Page, Size),

        meck:unload(group_file_repo),
        meck:unload(group_ds),

        ?assertEqual({ok, ExpectedFiles}, Result)
    end).

list_files_not_member_test_() ->
    with_scope(fun() ->
        Gid = 1,
        CurrentUid = 999,
        Page = 1,
        Size = 20,

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> false end),

        Result = group_file_ds:list_files(Gid, CurrentUid, Page, Size),

        meck:unload(group_ds),

        ?assertMatch({error, not_member}, Result)
    end).

list_files_with_category_test_() ->
    with_scope(fun() ->
        Gid = 1,
        CurrentUid = 100,
        Page = 1,
        Size = 20,
        Options = #{category => <<"document">>},
        ExpectedFiles = [
            #{
                <<"status">> => 1,
                <<"id">> => 1,
                <<"file_name">> => <<"test.pdf"/utf8>>,
                <<"file_category">> => <<"document">>
            }
        ],

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, list_by_group, fun(Gid0, Page0, Size0, Options0) ->
            ?assertEqual(Gid, Gid0),
            ?assertEqual(Page, Page0),
            ?assertEqual(Size, Size0),
            ?assertEqual(Options, Options0),
            {ok, ExpectedFiles}
        end),

        Result = group_file_ds:list_files(Gid, CurrentUid, Page, Size, Options),

        meck:unload(group_file_repo),
        meck:unload(group_ds),

        ?assertEqual({ok, ExpectedFiles}, Result)
    end).

%% ===================================================================
%% search_files/4 测试
%% ===================================================================

search_files_success_test_() ->
    with_scope(fun() ->
        Gid = 1,
        Keyword = <<"test"/utf8>>,
        Page = 1,
        Size = 20,
        CurrentUid = 42,
        ExpectedFiles = [
            #{<<"status">> => 1, <<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
        ],

        meck:new(group_file_repo, [passthrough]),
        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> true end),
        meck:expect(group_file_repo, search_by_name, fun(Gid0, Keyword0, Page0, Size0) ->
            ?assertEqual(Gid, Gid0),
            ?assertEqual(Keyword, Keyword0),
            ?assertEqual(Page, Page0),
            ?assertEqual(Size, Size0),
            {ok, ExpectedFiles}
        end),

        Result = group_file_ds:search_files(Gid, Keyword, Page, Size, CurrentUid),

        meck:unload(group_file_repo),
        meck:unload(group_ds),

        ?assertEqual({ok, ExpectedFiles}, Result)
    end).

%% 安全回归：非群成员不能搜索群文件（曾经代码注释承认跳过成员校验）
search_files_not_member_test_() ->
    with_scope(fun() ->
        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> false end),

        Result = group_file_ds:search_files(1, <<"test"/utf8>>, 1, 20, 999),

        meck:unload(group_ds),

        ?assertEqual({error, not_member}, Result)
    end).

search_files_repo_error_test_() ->
    with_scope(fun() ->
        Gid = 1,
        Keyword = <<"test"/utf8>>,
        Page = 1,
        Size = 20,

        meck:new(group_file_repo, [passthrough]),
        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> true end),
        meck:expect(group_file_repo, search_by_name, fun(_, _, _, _) -> {error, db_unavailable} end),

        Result = group_file_ds:search_files(Gid, Keyword, Page, Size, 42),

        meck:unload(group_file_repo),
        meck:unload(group_ds),

        ?assertEqual({error, db_unavailable}, Result)
    end).

%% ===================================================================
%% get_file_categories/1 测试
%% ===================================================================

get_file_categories_success_test_() ->
    with_scope(fun() ->
        Gid = 1,
        ExpectedStats = [
            #{<<"file_category">> => <<"document">>, <<"count">> => 10, <<"total_size">> => 1024000}
        ],

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, category_stats, fun(_) ->
            {ok, ExpectedStats}
        end),

        Result = group_file_ds:get_file_categories(Gid, 100),

        meck:unload(group_file_repo),

        ?assertEqual({ok, ExpectedStats}, Result)
    end).

get_file_categories_empty_test_() ->
    with_scope(fun() ->
        Gid = 999999,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, category_stats, fun(_) ->
            {ok, []}
        end),

        Result = group_file_ds:get_file_categories(Gid, 100),

        meck:unload(group_file_repo),

        ?assertEqual({ok, []}, Result)
    end).

with_scope_cleans_failure_test() ->
    Test = with_scope(fun() ->
        meck:new(group_file_repo, [passthrough]),
        error(injected_failure)
    end),
    ?assertError(injected_failure, Test()),
    lists:foreach(
        fun(Module) ->
            ?assertNot(lists:member(Module, meck:mocked()))
        end,
        [group_file_repo, attachment_ds, enterprise_audit_event_repo]
    ).

bind_existing_reference_under_lock_test_() ->
    bind_locked_test(
        #{
            <<"group_file_id">> => 42,
            <<"anchor_msg_id">> => null,
            <<"anchor_conv_seq">> => null
        },
        {ok, <<"existing">>}
    ).

bind_message_anchor_rejected_under_lock_test_() ->
    bind_locked_test(
        #{
            <<"group_file_id">> => null,
            <<"anchor_msg_id">> => <<"m1">>,
            <<"anchor_conv_seq">> => null
        },
        {error, attachment_anchor_conflict}
    ).

bind_negative_size_rejected_test_() ->
    bind_metadata_test(-1, <<"text/plain">>, <<>>, {error, file_size_mismatch}).

bind_wrong_size_rejected_test_() ->
    bind_metadata_test(2, <<"text/plain">>, <<>>, {error, file_size_mismatch}).

bind_wrong_type_rejected_test_() ->
    bind_metadata_test(1, <<"application/pdf">>, <<>>, {error, file_type_mismatch}).

bind_unverifiable_md5_rejected_test_() ->
    bind_metadata_test(1, <<"text/plain">>, <<"forged">>, {error, unverifiable_file_md5}).

partial_storage_identity_rejected_test() ->
    ?assertThrow(
        {abort_tx, incomplete_storage_identity},
        group_file_ds:location_columns(#{driver => garage})
    ),
    ?assertEqual(#{}, group_file_ds:location_columns(#{})).

bind_metadata_test(Size, Type, Md5, Expected) ->
    bind_locked_test(
        #{
            <<"group_file_id">> => 42,
            <<"anchor_msg_id">> => null,
            <<"anchor_conv_seq">> => null
        },
        Size,
        Type,
        Md5,
        Expected
    ).

bind_locked_test(Fields, Expected) ->
    bind_locked_test(Fields, 1, <<"text/plain">>, <<>>, Expected).

bind_locked_test(Fields, Size, Type, Md5, Expected) ->
    with_scope(fun() ->
        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(100, 1) -> true end),
        meck:new(elib_oss, [passthrough]),
        meck:expect(elib_oss, validate_file_type, fun(_) -> true end),
        meck:new(workspace_resolver, [passthrough]),
        meck:expect(workspace_resolver, resolve_workspace, fun(_) -> personal end),
        meck:expect(attachment_ds, ensure_upload_scope_tx_retryable, fun(_, _, _) -> ok end),
        meck:new(attachment_repo, [passthrough]),
        meck:expect(attachment_repo, bind_reference_lock_tx, fun(conn, <<"key">>) ->
            put(group_file_locked, true),
            ok
        end),
        meck:expect(attachment_repo, verify_direct_winner_tx, fun(conn, <<"key">>) ->
            ?assertEqual(true, get(group_file_locked)),
            ok
        end),
        meck:new(elib_pg, [passthrough]),
        meck:expect(elib_pg, with_deadlock_retry_tx, fun(F) ->
            try
                F(conn)
            catch
                throw:{abort_tx, Reason} -> {error, Reason}
            end
        end),
        meck:expect(elib_pg, query, fun(conn, Sql, _Params) ->
            ?assertEqual(true, get(group_file_locked)),
            case binary:match(iolist_to_binary(Sql), <<"SELECT creator_user_id">>) of
                {_, _} ->
                    {ok, [
                        maps:merge(
                            #{
                                <<"creator_user_id">> => 100,
                                <<"scope">> => <<"group">>,
                                <<"scope_ref">> => <<"1">>,
                                <<"lifecycle_state">> => <<"live">>,
                                <<"size">> => 1,
                                <<"mime_type">> => <<"text/plain">>,
                                <<"file_hash256">> => <<"confirmed-sha256">>
                            },
                            Fields
                        )
                    ]};
                nomatch ->
                    {ok, [#{<<"file_id">> => <<"existing">>}]}
            end
        end),
        ?assertEqual(
            Expected,
            group_file_ds:bind_file(
                1,
                100,
                <<"a.txt">>,
                Type,
                <<"key">>,
                Size,
                Md5
            )
        ),
        erase(group_file_locked)
    end).

with_scope(Test) ->
    fun() ->
        Existing = meck:mocked(),
        Modules = [
            attachment_ds,
            attachment_repo,
            enterprise_audit_event_repo,
            group_ds,
            elib_oss,
            elib_dt,
            group_file_repo,
            workspace_resolver,
            elib_pg,
            group_member_repo,
            workspace_guard,
            attach_logic,
            attach_pending_repo,
            attachment_upload_logic
        ],
        try
            meck:new(enterprise_audit_event_repo, [passthrough]),
            meck:expect(enterprise_audit_event_repo, append_scope_file_tx, fun(_, _, _, _) -> ok end),
            meck:new(attachment_ds, [passthrough]),
            meck:expect(attachment_ds, authorize_group_scope, fun(_, _) -> true end),
            meck:expect(attachment_ds, find_path_by_group_file, fun(_, _) ->
                {ok, <<"http://example.com/file.pdf">>}
            end),
            meck:expect(attachment_ds, ensure_upload_scope_tx, fun(_, _, _) -> ok end),
            Test()
        after
            lists:foreach(
                fun(Module) ->
                    case
                        lists:member(Module, Existing) orelse
                            not lists:member(Module, meck:mocked())
                    of
                        true -> ok;
                        false -> meck:unload(Module)
                    end
                end,
                Modules
            )
        end
    end.
