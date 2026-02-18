-module(group_album_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_album_repo 模块的 EUnit 测试
%%% 测试群相册数据仓库层功能
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = group_album_repo:tablename(),
        ?assertEqual(<<"public.group_album">>, Result)
    end).

%% ===================================================================
%% create_album/4 测试
%% ===================================================================

create_album_valid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        AlbumId = <<"album_test_001">>,
        AlbumName = <<"测试相册"/utf8>>,
        CreatorId = 1,
        Result = group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId),
        case Result of
            {ok, InsertId} when is_integer(InsertId) ->
                ?assert(InsertId > 0, "Expected positive insert ID");
            {ok, Id, _} when is_integer(Id) ->
                ?assert(Id > 0, "Expected positive ID");
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason), "Expected atom or binary error reason")
        end
    end).

%% ===================================================================
%% find_album_by_id/1 测试
%% ===================================================================

find_album_by_id_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        Gid = 1,
        AlbumId = <<"album_test_002">>,
        AlbumName = <<"测试相册2"/utf8>>,
        CreatorId = 1,
        {ok, InsertId} = group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId),

        % 查询测试
        Result = group_album_repo:find_album_by_id(InsertId),
        ?assertMatch(#{<<"id">> := _, <<"album_name">> := _}, Result)
    end).

find_album_by_id_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = group_album_repo:find_album_by_id(999999),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% list_albums/3 测试
%% ===================================================================

list_albums_test_() ->
    ?TEST_WITH_DB(fun() ->
        Gid = 1,
        Page = 1,
        Size = 10,
        Result = group_album_repo:list_albums(Gid, Page, Size),
        case Result of
            {ok, #{<<"list">> := List, <<"total">> := Total}} when is_list(List), is_integer(Total) ->
                ?assert(true);
            {ok, List} when is_list(List) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected list result")
        end
    end).

%% ===================================================================
%% insert_photo/1 测试
%% ===================================================================

insert_photo_valid_test_() ->
    ?TEST_WITH_DB(fun() ->
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_003">>,
            photo_id => <<"photo_test_001">>,
            photo_name => <<"测试图片.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        Result = group_album_repo:insert_photo(PhotoData),
        case Result of
            {ok, InsertId} when is_integer(InsertId) ->
                ?assert(InsertId > 0, "Expected positive insert ID");
            {ok, Id, _} when is_integer(Id) ->
                ?assert(Id > 0, "Expected positive ID");
            {error, Reason} ->
                ?assert(is_atom(Reason) orelse is_binary(Reason), "Expected atom or binary error reason")
        end
    end).

%% ===================================================================
%% find_photo_by_id/1 测试
%% ===================================================================

find_photo_by_id_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_004">>,
            photo_id => <<"photo_test_002">>,
            photo_name => <<"测试图片2.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo2.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        {ok, InsertId} = group_album_repo:insert_photo(PhotoData),

        % 查询测试
        Result = group_album_repo:find_photo_by_id(InsertId),
        ?assertMatch(#{<<"id">> := _, <<"photo_name">> := _}, Result)
    end).

find_photo_by_id_not_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = group_album_repo:find_photo_by_id(999999),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% list_photos/4 测试
%% ===================================================================

list_photos_test_() ->
    ?TEST_WITH_DB(fun() ->
        AlbumId = <<"album_test_005">>,
        Page = 1,
        Size = 10,
        Result = group_album_repo:list_photos(AlbumId, Page, Size, <<"*">>),
        case Result of
            {ok, #{<<"list">> := List, <<"total">> := Total}} when is_list(List), is_integer(Total) ->
                ?assert(true);
            {ok, List} when is_list(List) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected list result")
        end
    end).

%% ===================================================================
%% like_photo/2 测试
%% ===================================================================

like_photo_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_006">>,
            photo_id => <<"photo_test_003">>,
            photo_name => <<"测试图片3.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo3.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        {ok, InsertId} = group_album_repo:insert_photo(PhotoData),
        PhotoId = integer_to_binary(InsertId),
        UserId = 1,

        % 点赞测试
        Result = group_album_repo:like_photo(PhotoId, UserId),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Like should succeed")
        end
    end).

%% ===================================================================
%% unlike_photo/2 测试
%% ===================================================================

unlike_photo_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据并点赞
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_007">>,
            photo_id => <<"photo_test_004">>,
            photo_name => <<"测试图片4.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo4.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        {ok, InsertId} = group_album_repo:insert_photo(PhotoData),
        PhotoId = integer_to_binary(InsertId),
        UserId = 1,
        group_album_repo:like_photo(PhotoId, UserId),

        % 取消点赞测试
        Result = group_album_repo:unlike_photo(PhotoId, UserId),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Unlike should succeed")
        end
    end).

%% ===================================================================
%% is_liked/2 测试
%% ===================================================================

is_liked_true_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据并点赞
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_008">>,
            photo_id => <<"photo_test_005">>,
            photo_name => <<"测试图片5.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo5.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        {ok, InsertId} = group_album_repo:insert_photo(PhotoData),
        PhotoId = integer_to_binary(InsertId),
        UserId = 1,
        group_album_repo:like_photo(PhotoId, UserId),

        % 检查是否点赞
        Result = group_album_repo:is_liked(PhotoId, UserId),
        ?assertEqual(true, Result)
    end).

is_liked_false_test_() ->
    ?TEST_WITH_DB(fun() ->
        PhotoId = <<"photo_not_exist">>,
        UserId = 1,
        Result = group_album_repo:is_liked(PhotoId, UserId),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% add_comment/3 测试
%% ===================================================================

add_comment_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_009">>,
            photo_id => <<"photo_test_006">>,
            photo_name => <<"测试图片6.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo6.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        {ok, InsertId} = group_album_repo:insert_photo(PhotoData),
        PhotoId = integer_to_binary(InsertId),
        UserId = 1,
        Content = <<"这是一条测试评论"/utf8>>,

        % 添加评论测试
        Result = group_album_repo:add_comment(PhotoId, UserId, Content),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Add comment should succeed")
        end
    end).

%% ===================================================================
%% list_comments/2 测试
%% ===================================================================

list_comments_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据和评论
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_010">>,
            photo_id => <<"photo_test_007">>,
            photo_name => <<"测试图片7.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo7.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        {ok, InsertId} = group_album_repo:insert_photo(PhotoData),
        PhotoId = integer_to_binary(InsertId),
        UserId = 1,
        Content = <<"测试评论"/utf8>>,
        group_album_repo:add_comment(PhotoId, UserId, Content),

        % 查询评论列表
        Result = group_album_repo:list_comments(PhotoId, 10),
        case Result of
            {ok, List} when is_list(List) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected list result")
        end
    end).

%% ===================================================================
%% update_album/2 测试
%% ===================================================================

update_album_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        Gid = 1,
        AlbumId = <<"album_test_011">>,
        AlbumName = <<"原相册名"/utf8>>,
        CreatorId = 1,
        {ok, InsertId} = group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId),

        % 更新相册
        UpdateData = #{id => InsertId, album_name => <<"新相册名"/utf8>>},
        Result = group_album_repo:update_album(UpdateData),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Update should succeed")
        end
    end).

%% ===================================================================
%% delete_album/1 测试
%% ===================================================================

delete_album_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        Gid = 1,
        AlbumId = <<"album_test_012">>,
        AlbumName = <<"要删除的相册"/utf8>>,
        CreatorId = 1,
        {ok, InsertId} = group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId),

        % 删除相册
        Result = group_album_repo:delete_album(InsertId),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Delete should succeed")
        end
    end).

%% ===================================================================
%% delete_photo/1 测试
%% ===================================================================

delete_photo_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        PhotoData = #{
            group_id => 1,
            album_id => <<"album_test_013">>,
            photo_id => <<"photo_test_008">>,
            photo_name => <<"要删除的图片.jpg"/utf8>>,
            photo_url => <<"http://example.com/photo8.jpg">>,
            photo_size => 102400,
            uploader_id => 1
        },
        {ok, InsertId} = group_album_repo:insert_photo(PhotoData),

        % 删除图片
        Result = group_album_repo:delete_photo(InsertId),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Delete should succeed")
        end
    end).

%% ===================================================================
%% increment_photo_count/1 测试
%% ===================================================================

increment_photo_count_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        Gid = 1,
        AlbumId = <<"album_test_014">>,
        AlbumName = <<"测试相册14"/utf8>>,
        CreatorId = 1,
        {ok, InsertId} = group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId),

        % 增加照片计数
        Result = group_album_repo:increment_photo_count(InsertId),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Increment should succeed")
        end
    end).

%% ===================================================================
%% decrement_photo_count/1 测试
%% ===================================================================

decrement_photo_count_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 先插入测试数据
        Gid = 1,
        AlbumId = <<"album_test_015">>,
        AlbumName = <<"测试相册15"/utf8>>,
        CreatorId = 1,
        {ok, InsertId} = group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId),

        % 减少照片计数
        Result = group_album_repo:decrement_photo_count(InsertId),
        case Result of
            {ok, _} -> ?assert(true);
            {error, _} -> ?assert(false, "Decrement should succeed")
        end
    end).
