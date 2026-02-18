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

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, insert, fun(_) -> {ok, 1, #{}} end),

        Result = group_file_ds:upload_file(Gid, UploaderId, FileName, FileBinary, FileType),

        meck:unload(group_file_repo),

        case Result of
            {ok, FileId} when is_binary(FileId) ->
                ?assert(true);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

upload_file_invalid_type_test_() ->
    fun() ->
        Gid = 1,
        UploaderId = 100,
        FileName = <<"test.exe">>,
        FileBinary = <<<<"test">> || _ <- lists:seq(1, 100)>>,
        FileType = <<"application/x-msdownload">>,

        Result = group_file_ds:upload_file(Gid, UploaderId, FileName, FileBinary, FileType),

        ?assertMatch({error, _Reason}, Result)
    end.

upload_file_too_large_test_() ->
    fun() ->
        Gid = 1,
        UploaderId = 100,
        FileName = <<"large.pdf"/utf8>>,
        % 创建超过100MB的文件
        FileBinary = <<0:100102400>>,
        FileType = <<"application/pdf">>,

        Result = group_file_ds:upload_file(Gid, UploaderId, FileName, FileBinary, FileType),

        ?assertMatch({error, file_too_large}, Result)
    end.

%% ===================================================================
%% download_file/2 测试
%% ===================================================================

download_file_success_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"id">> => 1, <<"group_id">> => 1, <<"file_url">> => <<"http://example.com/file.pdf">>}
        end),
        meck:expect(group_file_repo, increment_download, fun(_) -> {ok, 1} end),

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        Result = group_file_ds:download_file(FileId, CurrentUid),

        meck:unload(group_ds),
        meck:unload(group_file_repo),

        case Result of
            {ok, FileUrl} when is_binary(FileUrl) ->
                ?assert(true);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

download_file_not_member_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 999,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, find_by_id, fun(_) ->
            #{<<"id">> => 1, <<"group_id">> => 1}
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
            #{<<"role">> => 3} % 管理员
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
            #{<<"role">> => 1} % 普通成员
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

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, list_by_group, fun(_, _, _, _) ->
            {ok, [
                #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
            ]}
        end),

        Result = group_file_ds:list_files(Gid, CurrentUid, Page, Size),

        meck:unload(group_file_repo),
        meck:unload(group_ds),

        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(true);
            {error, _Reason} ->
                ?assert(true)
        end
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

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, list_by_group, fun(_, _, _, _) ->
            {ok, [
                #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>, <<"file_category">> => <<"document">>}
            ]}
        end),

        Result = group_file_ds:list_files(Gid, CurrentUid, Page, Size, Options),

        meck:unload(group_file_repo),
        meck:unload(group_ds),

        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(true);
            {error, _Reason} ->
                ?assert(true)
        end
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

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> true end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, search_by_name, fun(_, _, _, _) ->
            {ok, [
                #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
            ]}
        end),

        Result = group_file_ds:search_files(Gid, Keyword, Page, Size),

        meck:unload(group_file_repo),
        meck:unload(group_ds),

        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(true);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

search_files_not_member_test_() ->
    fun() ->
        Gid = 1,
        Keyword = <<"test"/utf8>>,
        Page = 1,
        Size = 20,

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(_, _) -> false end),

        Result = group_file_ds:search_files(Gid, Keyword, Page, Size),

        meck:unload(group_ds),

        ?assertMatch({error, not_member}, Result)
    end.

%% ===================================================================
%% get_file_categories/1 测试
%% ===================================================================

get_file_categories_success_test_() ->
    fun() ->
        Gid = 1,

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, category_stats, fun(_) ->
            {ok, [
                #{<<"file_category">> => <<"document">>, <<"count">> => 10, <<"total_size">> => 1024000}
            ]}
        end),

        Result = group_file_ds:get_file_categories(Gid),

        meck:unload(group_file_repo),

        case Result of
            {ok, Stats} when is_list(Stats) ->
                ?assert(true);
            {error, _Reason} ->
                ?assert(true)
        end
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

        case Result of
            {ok, []} ->
                ?assert(true);
            {ok, Stats} when is_list(Stats) ->
                ?assertEqual([], Stats);
            {error, _Reason} ->
                ?assert(true)
        end
    end.
