-module(group_file_ds_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% group_file_ds 模块的 EUnit 测试
%%%
%%% 测试群文件数据服务层的所有功能
%%%===================================================================

%% ===================================================================
%% upload_file/5 测试
%% ===================================================================

upload_file_success_test_() ->
    fun() ->
        Gid = 1,
        UploaderId = 100,
        FileName = <<"test.pdf"/utf8>>,
        FileBinary = <<<<"test">> || _ <- lists:seq(1, 100)>>,
        FileType = <<"application/pdf">>,
        FileUrl = <<"https://oss.example.com/group/test.pdf">>,
        FileId = <<"file_123">>,
        NowTs = 1700000000,

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(UploaderId0, Gid0) ->
            ?assertEqual(UploaderId, UploaderId0),
            ?assertEqual(Gid, Gid0),
            true
        end),

        meck:new(elib_oss, [passthrough]),
        meck:expect(elib_oss, validate_file_type, fun(Type) ->
            ?assertEqual(FileType, Type),
            true
        end),
        meck:expect(elib_oss, upload, fun(Binary, Name, Opts) ->
            ?assertEqual(FileBinary, Binary),
            ?assertEqual(FileName, Name),
            ?assertEqual(#{mime_type => FileType}, Opts),
            {ok, FileUrl, FileId}
        end),
        meck:expect(elib_oss, get_file_category, fun(Type) ->
            ?assertEqual(FileType, Type),
            document
        end),

        meck:new(elib_dt, [passthrough]),
        meck:expect(elib_dt, now, fun() -> NowTs end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, insert, fun(Data) ->
            ExpectedHashHex = binary:encode_hex(erlang:md5(FileBinary)),
            ?assertEqual(Gid, maps:get(group_id, Data)),
            ?assertEqual(FileId, maps:get(file_id, Data)),
            ?assertEqual(FileName, maps:get(file_name, Data)),
            ?assertEqual(byte_size(FileBinary), maps:get(file_size, Data)),
            ?assertEqual(FileType, maps:get(file_type, Data)),
            ?assertEqual(<<"document">>, maps:get(file_category, Data)),
            ?assertEqual(FileUrl, maps:get(file_url, Data)),
            ?assertEqual(ExpectedHashHex, maps:get(file_hash, Data)),
            ?assertEqual(UploaderId, maps:get(uploader_id, Data)),
            ?assertEqual(0, maps:get(download_count, Data)),
            ?assertEqual(1, maps:get(status, Data)),
            ?assertEqual(NowTs, maps:get(created_at, Data)),
            ?assertEqual(NowTs, maps:get(updated_at, Data)),
            % 真实契约：group_file_repo:insert/1 返回 {ok, FileId} 二元组
            % （曾 mock 成三元组 {ok, 1, #{}} 与实现一起漂移，掩盖了
            % group_file_ds:upload_file/5 的 no case clause 生产 500）
            {ok, FileId}
        end),

        Result = group_file_ds:upload_file(Gid, UploaderId, FileName, FileBinary, FileType),

        meck:unload(group_file_repo),
        meck:unload(elib_dt),
        meck:unload(elib_oss),
        meck:unload(group_ds),

        ?assertEqual({ok, FileId}, Result)
    end.

upload_file_invalid_type_test_() ->
    fun() ->
        Gid = 1,
        UploaderId = 100,
        FileName = <<"test.exe">>,
        FileBinary = <<<<"test">> || _ <- lists:seq(1, 100)>>,
        FileType = <<"application/x-msdownload">>,

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        meck:new(elib_oss, [passthrough]),
        meck:expect(elib_oss, validate_file_type, fun(_) -> false end),

        Result = group_file_ds:upload_file(Gid, UploaderId, FileName, FileBinary, FileType),

        meck:unload(elib_oss),
        meck:unload(group_ds),

        ?assertEqual({error, invalid_file_type}, Result)
    end.

upload_file_too_large_test_() ->
    fun() ->
        Gid = 1,
        UploaderId = 100,
        FileName = <<"large.pdf"/utf8>>,
        FileBinary = <<<<"test">> || _ <- lists:seq(1, 100)>>,
        FileType = <<"application/pdf">>,

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        meck:new(elib_oss, [passthrough]),
        meck:expect(elib_oss, validate_file_type, fun(_) -> true end),
        meck:expect(elib_oss, upload, fun(_, _, _) -> {error, file_too_large} end),

        Result = group_file_ds:upload_file(Gid, UploaderId, FileName, FileBinary, FileType),

        meck:unload(elib_oss),
        meck:unload(group_ds),

        ?assertEqual({error, file_too_large}, Result)
    end.

%% ===================================================================
%% download_file/2 测试
%% ===================================================================

download_file_success_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 100,
        FileUrl = <<"http://example.com/file.pdf">>,
        Parent = self(),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"id">> => 1, <<"group_id">> => 1, <<"file_url">> => FileUrl}
        end),
        meck:expect(group_file_repo, increment_download, fun(DownloadFileId) ->
            Parent ! {increment_download_called, DownloadFileId},
            {ok, 1}
        end),

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
        meck:unload(group_file_repo),
        ok
    end.

download_file_not_member_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 999,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{
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
    end.

download_file_not_found_test_() ->
    fun() ->
        FileId = 999999,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) -> #{} end),

        Result = group_file_ds:download_file(FileId, CurrentUid),

        meck:unload(group_file_repo),

        ?assertMatch({error, not_found}, Result)
    end.

%% ===================================================================
%% delete_file/2 测试
%% ===================================================================

delete_file_as_uploader_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"id">> => 1, <<"group_id">> => 1, <<"uploader_id">> => 100}
        end),
        meck:expect(group_file_repo, soft_delete, fun(_) -> {ok, 1} end),

        Result = group_file_ds:delete_file(FileId, CurrentUid),

        meck:unload(group_file_repo),

        ?assertMatch(ok, Result)
    end.

delete_file_as_admin_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"id">> => 1, <<"group_id">> => 1, <<"uploader_id">> => 200}
        end),
        meck:expect(group_file_repo, soft_delete, fun(_) -> {ok, 1} end),

        meck:new(group_member_repo, [passthrough]),
        meck:expect(group_member_repo, find, fun(_, _, _) ->
            % 管理员
            #{<<"role">> => 3}
        end),

        Result = group_file_ds:delete_file(FileId, CurrentUid),

        meck:unload(group_member_repo),
        meck:unload(group_file_repo),

        ?assertMatch(ok, Result)
    end.

delete_file_permission_denied_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"id">> => 1, <<"group_id">> => 1, <<"uploader_id">> => 200}
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
    end.

%% ===================================================================
%% list_files/4 测试
%% ===================================================================

list_files_success_test_() ->
    fun() ->
        Gid = 1,
        CurrentUid = 100,
        Page = 1,
        Size = 20,
        ExpectedFiles = [
            #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
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
    end.

list_files_not_member_test_() ->
    fun() ->
        Gid = 1,
        CurrentUid = 999,
        Page = 1,
        Size = 20,

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> false end),

        Result = group_file_ds:list_files(Gid, CurrentUid, Page, Size),

        meck:unload(group_ds),

        ?assertMatch({error, not_member}, Result)
    end.

list_files_with_category_test_() ->
    fun() ->
        Gid = 1,
        CurrentUid = 100,
        Page = 1,
        Size = 20,
        Options = #{category => <<"document">>},
        ExpectedFiles = [
            #{
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
    end.

%% ===================================================================
%% search_files/4 测试
%% ===================================================================

search_files_success_test_() ->
    fun() ->
        Gid = 1,
        Keyword = <<"test"/utf8>>,
        Page = 1,
        Size = 20,
        CurrentUid = 42,
        ExpectedFiles = [
            #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
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
    end.

%% 安全回归：非群成员不能搜索群文件（曾经代码注释承认跳过成员校验）
search_files_not_member_test_() ->
    fun() ->
        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_Uid, _Gid) -> false end),

        Result = group_file_ds:search_files(1, <<"test"/utf8>>, 1, 20, 999),

        meck:unload(group_ds),

        ?assertEqual({error, not_member}, Result)
    end.

search_files_repo_error_test_() ->
    fun() ->
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
    end.

%% ===================================================================
%% get_file_categories/1 测试
%% ===================================================================

get_file_categories_success_test_() ->
    fun() ->
        Gid = 1,
        ExpectedStats = [
            #{<<"file_category">> => <<"document">>, <<"count">> => 10, <<"total_size">> => 1024000}
        ],

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, category_stats, fun(_) ->
            {ok, ExpectedStats}
        end),

        Result = group_file_ds:get_file_categories(Gid),

        meck:unload(group_file_repo),

        ?assertEqual({ok, ExpectedStats}, Result)
    end.

get_file_categories_empty_test_() ->
    fun() ->
        Gid = 999999,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, category_stats, fun(_) ->
            {ok, []}
        end),

        Result = group_file_ds:get_file_categories(Gid),

        meck:unload(group_file_repo),

        ?assertEqual({ok, []}, Result)
    end.
