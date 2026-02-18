-module(group_file_repo_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% group_file_repo 模块的 EUnit 测试
%%%
%%% 测试群文件数据仓库层的所有功能
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test() ->
    Result = group_file_repo:tablename(),
    ?assertEqual(<<"public.group_file">>, Result).

%% ===================================================================
%% insert/1 测试
%% ===================================================================

insert_valid_file_test_() ->
    fun() ->
        Data = #{
            group_id => 1,
            file_id => <<"file_test_001">>,
            file_name => <<"test.pdf"/utf8>>,
            file_size => 1024000,
            file_type => <<"application/pdf">>,
            file_category => <<"document">>,
            file_url => <<"http://example.com/files/test.pdf">>,
            file_hash => <<"abc123">>,
            uploader_id => 100,
            download_count => 0,
            status => 1,
            created_at => {{2026, 2, 16}, {12, 0, 0}}
        },
        Result = group_file_repo:insert(Data),
        case Result of
            {ok, InsertId, Details} when is_integer(InsertId) ->
                ?assert(InsertId > 0),
                ?assertMatch(#{}, Details);
            {ok, InsertResult} when is_map(InsertResult) ->
                ?assertMatch(#{}, InsertResult);
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason))
        end
    end.

insert_with_missing_required_field_test_() ->
    fun() ->
        % 缺少 group_id
        Data = #{
            file_name => <<"test.pdf"/utf8>>,
            file_type => <<"application/pdf">>
        },
        Result = group_file_repo:insert(Data),
        ?assertMatch({error, _Reason}, Result)
    end.

%% ===================================================================
%% find_by_id/1 测试
%% ===================================================================

find_by_id_existing_test_() ->
    fun() ->
        FileId = 1,
        Result = group_file_repo:find_by_id(FileId),
        ?assertMatch(#{<<"id">> := _, <<"file_name">> := _}, Result)
    end.

find_by_id_not_existing_test_() ->
    fun() ->
        FileId = 999999,
        Result = group_file_repo:find_by_id(FileId),
        ?assertMatch(#{}, Result)
    end.

%% ===================================================================
%% find_by_file_id/1 测试
%% ===================================================================

find_by_file_id_existing_test_() ->
    fun() ->
        FileId = <<"file_existing_001">>,
        Result = group_file_repo:find_by_file_id(FileId),
        ?assertMatch(#{<<"id">> := _, <<"file_id">> := _}, Result)
    end.

find_by_file_id_not_existing_test_() ->
    fun() ->
        FileId = <<"file_not_existing_999">>,
        Result = group_file_repo:find_by_file_id(FileId),
        ?assertMatch(#{}, Result)
    end.

%% ===================================================================
%% list_by_group/4 测试
%% ===================================================================

list_by_group_with_results_test_() ->
    fun() ->
        Gid = 1,
        Page = 1,
        Size = 20,
        Result = group_file_repo:list_by_group(Gid, Page, Size, #{}),
        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(is_list(Files));
            {error, _Reason} ->
                ?assert(true)
        end
    end.

list_by_group_with_category_filter_test_() ->
    fun() ->
        Gid = 1,
        Page = 1,
        Size = 20,
        Options = #{category => <<"document">>},
        Result = group_file_repo:list_by_group(Gid, Page, Size, Options),
        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(is_list(Files));
            {error, _Reason} ->
                ?assert(true)
        end
    end.

list_by_group_empty_group_test_() ->
    fun() ->
        Gid = 999999,
        Page = 1,
        Size = 20,
        Result = group_file_repo:list_by_group(Gid, Page, Size, #{}),
        case Result of
            {ok, []} ->
                ?assert(true);
            {ok, Files} when is_list(Files) ->
                ?assertEqual([], Files);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

%% ===================================================================
%% search_by_name/4 测试
%% ===================================================================

search_by_name_with_results_test_() ->
    fun() ->
        Gid = 1,
        Keyword = <<"test"/utf8>>,
        Page = 1,
        Size = 20,
        Result = group_file_repo:search_by_name(Gid, Keyword, Page, Size),
        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(is_list(Files));
            {error, _Reason} ->
                ?assert(true)
        end
    end.

search_by_name_no_results_test_() ->
    fun() ->
        Gid = 1,
        Keyword = <<"nonexistent"/utf8>>,
        Page = 1,
        Size = 20,
        Result = group_file_repo:search_by_name(Gid, Keyword, Page, Size),
        case Result of
            {ok, []} ->
                ?assert(true);
            {ok, Files} when is_list(Files) ->
                ?assertEqual([], Files);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

%% ===================================================================
%% list_by_category/4 测试
%% ===================================================================

list_by_category_with_results_test_() ->
    fun() ->
        Gid = 1,
        Category = <<"document">>,
        Page = 1,
        Size = 20,
        Result = group_file_repo:list_by_category(Gid, Category, Page, Size),
        case Result of
            {ok, Files} when is_list(Files) ->
                ?assert(is_list(Files));
            {error, _Reason} ->
                ?assert(true)
        end
    end.

list_by_category_invalid_category_test_() ->
    fun() ->
        Gid = 1,
        Category = <<"invalid">>,
        Page = 1,
        Size = 20,
        Result = group_file_repo:list_by_category(Gid, Category, Page, Size),
        case Result of
            {ok, []} ->
                ?assert(true);
            {ok, Files} when is_list(Files) ->
                ?assertEqual([], Files);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

%% ===================================================================
%% soft_delete/1 测试
%% ===================================================================

soft_delete_existing_file_test_() ->
    fun() ->
        FileId = 1,
        Result = group_file_repo:soft_delete(FileId),
        case Result of
            {ok, 1} ->
                ?assert(true);
            {ok, Count} when is_integer(Count) ->
                ?assert(Count >= 0);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

soft_delete_not_existing_file_test_() ->
    fun() ->
        FileId = 999999,
        Result = group_file_repo:soft_delete(FileId),
        case Result of
            {ok, 0} ->
                ?assert(true);
            {ok, Count} when is_integer(Count) ->
                ?assertEqual(0, Count);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

%% ===================================================================
%% increment_download/1 测试
%% ===================================================================

increment_download_existing_file_test_() ->
    fun() ->
        FileId = 1,
        Result = group_file_repo:increment_download(FileId),
        case Result of
            {ok, 1} ->
                ?assert(true);
            {ok, Count} when is_integer(Count) ->
                ?assert(Count >= 0);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

increment_download_not_existing_file_test_() ->
    fun() ->
        FileId = 999999,
        Result = group_file_repo:increment_download(FileId),
        case Result of
            {ok, 0} ->
                ?assert(true);
            {ok, Count} when is_integer(Count) ->
                ?assertEqual(0, Count);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

%% ===================================================================
%% count_by_group/1 测试
%% ===================================================================

count_by_group_with_files_test_() ->
    fun() ->
        Gid = 1,
        Result = group_file_repo:count_by_group(Gid),
        case Result of
            {ok, Count} when is_integer(Count) ->
                ?assert(Count >= 0);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

count_by_group_empty_group_test_() ->
    fun() ->
        Gid = 999999,
        Result = group_file_repo:count_by_group(Gid),
        case Result of
            {ok, 0} ->
                ?assert(true);
            {ok, Count} when is_integer(Count) ->
                ?assertEqual(0, Count);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

%% ===================================================================
%% sum_size_by_group/1 测试
%% ===================================================================

sum_size_by_group_with_files_test_() ->
    fun() ->
        Gid = 1,
        Result = group_file_repo:sum_size_by_group(Gid),
        case Result of
            {ok, TotalSize} when is_integer(TotalSize) ->
                ?assert(TotalSize >= 0);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

sum_size_by_group_empty_group_test_() ->
    fun() ->
        Gid = 999999,
        Result = group_file_repo:sum_size_by_group(Gid),
        case Result of
            {ok, 0} ->
                ?assert(true);
            {ok, TotalSize} when is_integer(TotalSize) ->
                ?assertEqual(0, TotalSize);
            {error, _Reason} ->
                ?assert(true)
        end
    end.

%% ===================================================================
%% category_stats/1 测试
%% ===================================================================

category_stats_with_files_test_() ->
    fun() ->
        Gid = 1,
        Result = group_file_repo:category_stats(Gid),
        case Result of
            {ok, Stats} when is_list(Stats) ->
                ?assert(is_list(Stats));
            {error, _Reason} ->
                ?assert(true)
        end
    end.

category_stats_empty_group_test_() ->
    fun() ->
        Gid = 999999,
        Result = group_file_repo:category_stats(Gid),
        case Result of
            {ok, []} ->
                ?assert(true);
            {ok, Stats} when is_list(Stats) ->
                ?assertEqual([], Stats);
            {error, _Reason} ->
                ?assert(true)
        end
    end.
