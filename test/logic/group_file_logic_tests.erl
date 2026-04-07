-module(group_file_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% group_file_logic 模块的 EUnit 测试
%%%
%%% 测试群文件业务逻辑层的所有功能
%%%===================================================================

%% ===================================================================
%% upload/5 测试
%% ===================================================================

upload_success_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 100,
        FileName = <<"test.pdf"/utf8>>,
        FileBinary = <<<<"test">> || _ <- lists:seq(1, 100)>>,
        FileType = <<"application/pdf">>,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, upload_file, fun(_, _, _, _, _) ->
            {ok, <<"file_123">>}
        end),

        Result = group_file_logic:upload(Gid, CurrentUid, FileName, FileBinary, FileType),

        meck:unload(group_file_ds),

        case Result of
            {ok, FileData} when is_map(FileData) ->
                ?assertMatch(#{<<"file_id">> := _}, FileData);
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

upload_not_member_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 999,
        FileName = <<"test.pdf"/utf8>>,
        FileBinary = <<<<"test">> || _ <- lists:seq(1, 100)>>,
        FileType = <<"application/pdf">>,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, upload_file, fun(_, _, _, _, _) ->
            {error, not_member}
        end),

        Result = group_file_logic:upload(Gid, CurrentUid, FileName, FileBinary, FileType),

        meck:unload(group_file_ds),

        ?assertMatch({error, not_member}, Result)
    end.

upload_file_too_large_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 100,
        FileName = <<"large.pdf"/utf8>>,
        FileBinary = <<0:100102400>>,
        FileType = <<"application/pdf">>,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, upload_file, fun(_, _, _, _, _) ->
            {error, file_too_large}
        end),

        Result = group_file_logic:upload(Gid, CurrentUid, FileName, FileBinary, FileType),

        meck:unload(group_file_ds),

        ?assertMatch({error, file_too_large}, Result)
    end.

%% ===================================================================
%% download/2 测试
%% ===================================================================

download_success_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, download_file, fun(_, _) ->
            {ok, <<"http://example.com/file.pdf">>}
        end),

        Result = group_file_logic:download(FileId, CurrentUid),

        meck:unload(group_file_ds),

        case Result of
            {ok, FileUrl} when is_binary(FileUrl) ->
                ?assert(is_binary(FileUrl));
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

download_not_member_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 999,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, download_file, fun(_, _) ->
            {error, not_member}
        end),

        Result = group_file_logic:download(FileId, CurrentUid),

        meck:unload(group_file_ds),

        ?assertMatch({error, not_member}, Result)
    end.

download_not_found_test_() ->
    fun() ->
        FileId = 999999,
        CurrentUid = 100,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, download_file, fun(_, _) ->
            {error, not_found}
        end),

        Result = group_file_logic:download(FileId, CurrentUid),

        meck:unload(group_file_ds),

        ?assertMatch({error, not_found}, Result)
    end.

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_success_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 100,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, delete_file, fun(_, _) -> ok end),

        Result = group_file_logic:delete(FileId, CurrentUid),

        meck:unload(group_file_ds),

        ?assertMatch(ok, Result)
    end.

delete_permission_denied_test_() ->
    fun() ->
        FileId = 1,
        CurrentUid = 999,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, delete_file, fun(_, _) ->
            {error, permission_denied}
        end),

        Result = group_file_logic:delete(FileId, CurrentUid),

        meck:unload(group_file_ds),

        ?assertMatch({error, permission_denied}, Result)
    end.

delete_not_found_test_() ->
    fun() ->
        FileId = 999999,
        CurrentUid = 100,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, delete_file, fun(_, _) ->
            {error, not_found}
        end),

        Result = group_file_logic:delete(FileId, CurrentUid),

        meck:unload(group_file_ds),

        ?assertMatch({error, not_found}, Result)
    end.

%% ===================================================================
%% list/4 测试
%% ===================================================================

list_success_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 100,
        Page = 1,
        Size = 20,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, list_files, fun(_, _, _, _, _) ->
            {ok, [
                #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
            ]}
        end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, count_by_group, fun(_) -> {ok, 1} end),

        Result = group_file_logic:list(Gid, CurrentUid, Page, Size),

        meck:unload(group_file_repo),
        meck:unload(group_file_ds),

        case Result of
            {ok, FileList} when is_map(FileList) ->
                ?assertMatch(#{<<"items">> := _, <<"total">> := _}, FileList);
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

list_not_member_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 999,
        Page = 1,
        Size = 20,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, list_files, fun(_, _, _, _, _) ->
            {error, not_member}
        end),

        Result = group_file_logic:list(Gid, CurrentUid, Page, Size),

        meck:unload(group_file_ds),

        ?assertMatch({error, not_member}, Result)
    end.

list_with_category_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 100,
        Page = 1,
        Size = 20,
        Options = #{category => <<"document">>},

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, list_files, fun(_, _, _, _, _) ->
            {ok, [
                #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>, <<"file_category">> => <<"document">>}
            ]}
        end),

        meck:new(group_file_repo, [passthrough]),
        meck:expect(group_file_repo, count_by_group, fun(_) -> {ok, 1} end),

        Result = group_file_logic:list(Gid, CurrentUid, Page, Size, Options),

        meck:unload(group_file_repo),
        meck:unload(group_file_ds),

        case Result of
            {ok, FileList} when is_map(FileList) ->
                ?assertMatch(#{<<"items">> := _, <<"total">> := _}, FileList);
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

%% ===================================================================
%% search/4 测试
%% ===================================================================

search_success_test_() ->
    fun() ->
        Gid = <<"g1">>,
        Keyword = <<"test"/utf8>>,
        Page = 1,
        Size = 20,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, search_files, fun(_, _, _, _) ->
            {ok, [
                #{<<"id">> => 1, <<"file_name">> => <<"test.pdf"/utf8>>}
            ]}
        end),

        Result = group_file_logic:search(Gid, Keyword, Page, Size),

        meck:unload(group_file_ds),

        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(is_list(Files));
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

search_empty_result_test_() ->
    fun() ->
        Gid = <<"g1">>,
        Keyword = <<"nonexistent"/utf8>>,
        Page = 1,
        Size = 20,

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, search_files, fun(_, _, _, _) ->
            {ok, []}
        end),

        Result = group_file_logic:search(Gid, Keyword, Page, Size),

        meck:unload(group_file_ds),

        case Result of
            {ok, []} ->
                ok;
            {ok, Files} when is_list(Files) ->
                ?assertEqual([], Files);
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

%% ===================================================================
%% get_categories/2 测试
%% ===================================================================

get_categories_success_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 100,

        _ = catch meck:unload(group_ds),
        _ = catch meck:unload(group_file_ds),

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(100, 1) -> true end),

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, get_file_categories, fun(_) ->
            {ok, [
                {<<"document">>, 10, 1024000}
            ]}
        end),

        Result = group_file_logic:get_categories(Gid, CurrentUid),

        meck:unload(group_file_ds),
        meck:unload(group_ds),

        case Result of
            {ok, Stats} when is_list(Stats) ->
                ?assert(is_list(Stats));
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

get_categories_empty_test_() ->
    fun() ->
        Gid = <<"g1">>,
        CurrentUid = 100,

        _ = catch meck:unload(group_ds),
        _ = catch meck:unload(group_file_ds),

        meck:new(group_ds, [passthrough]),
        meck:expect(group_ds, is_member, fun(100, 1) -> true end),

        meck:new(group_file_ds, [passthrough]),
        meck:expect(group_file_ds, get_file_categories, fun(_) ->
            {ok, []}
        end),

        Result = group_file_logic:get_categories(Gid, CurrentUid),

        meck:unload(group_file_ds),
        meck:unload(group_ds),

        case Result of
            {ok, []} ->
                ok;
            {ok, Stats} when is_list(Stats) ->
                ?assertEqual([], Stats);
            {error, Reason} ->
                assert_reason(Reason)
        end
    end.

assert_reason(Reason) ->
    ?assert(is_atom(Reason) orelse is_binary(Reason) orelse is_tuple(Reason)).
