-module(group_album_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_album_repo 模块的 EUnit 测试
%%% 测试群相册数据仓库层功能（使用 meck mock，不依赖真实数据库）
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        Result = group_album_repo:tablename(),
        ?assertEqual(<<"public.group_album">>, Result)
    end).

%% ===================================================================
%% create_album/4 测试
%% ===================================================================

create_album_valid_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 100001 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        Gid = 1,
        AlbumId = <<"album_test_001">>,
        AlbumName = <<"测试相册"/utf8>>,
        CreatorId = 1,
        Result = group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId),
        ?assertEqual({ok, 100001}, Result)
    end).

%% ===================================================================
%% find_album_by_id/1 测试
%% ===================================================================

find_album_by_id_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"id">> => 100, <<"album_name">> => <<"测试相册"/utf8>>}}
            end}
        ]}
    ], fun() ->
        Result = group_album_repo:find_album_by_id(100),
        ?assertMatch(#{<<"id">> := _, <<"album_name">> := _}, Result)
    end).

find_album_by_id_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
        ]}
    ], fun() ->
        Result = group_album_repo:find_album_by_id(999999),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% list_albums/3 测试
%% ===================================================================

list_albums_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Col, _Where, _Order, _Page, _Size) ->
                {ok, #{<<"list">> => [], <<"total">> => 0}}
            end}
        ]}
    ], fun() ->
        Gid = 1,
        Page = 1,
        Size = 10,
        Result = group_album_repo:list_albums(Gid, Page, Size),
        case Result of
            {ok, #{<<"list">> := List, <<"total">> := Total}} when is_list(List), is_integer(Total) ->
                ?assert(Total >= 0);
            _ ->
                ?assert(false, "Expected list result")
        end
    end).

%% ===================================================================
%% insert_photo/1 测试
%% ===================================================================

insert_photo_valid_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 200001 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
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
        ?assertEqual({ok, 200001}, Result)
    end).

%% ===================================================================
%% find_photo_by_id/1 测试
%% ===================================================================

find_photo_by_id_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) ->
                {ok, #{<<"id">> => 200, <<"photo_name">> => <<"测试图片.jpg"/utf8>>}}
            end}
        ]}
    ], fun() ->
        Result = group_album_repo:find_photo_by_id(200),
        ?assertMatch(#{<<"id">> := _, <<"photo_name">> := _}, Result)
    end).

find_photo_by_id_not_existing_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
        ]}
    ], fun() ->
        Result = group_album_repo:find_photo_by_id(999999),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% list_photos/4 测试
%% ===================================================================

list_photos_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Col, _Where, _Order, _Page, _Size) ->
                {ok, #{<<"list">> => [], <<"total">> => 0}}
            end}
        ]}
    ], fun() ->
        AlbumId = <<"album_test_005">>,
        Page = 1,
        Size = 10,
        Result = group_album_repo:list_photos(AlbumId, Page, Size, <<"*">>),
        case Result of
            {ok, #{<<"list">> := List, <<"total">> := Total}} when is_list(List), is_integer(Total) ->
                ?assert(Total >= 0);
            _ ->
                ?assert(false, "Expected list result")
        end
    end).

%% ===================================================================
%% like_photo/2 测试
%% ===================================================================

like_photo_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 300001 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end},
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        PhotoId = <<"200">>,
        UserId = 1,
        Result = group_album_repo:like_photo(PhotoId, UserId),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% unlike_photo/2 测试
%% ===================================================================

unlike_photo_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        PhotoId = <<"200">>,
        UserId = 1,
        Result = group_album_repo:unlike_photo(PhotoId, UserId),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% is_liked/2 测试
%% ===================================================================

is_liked_true_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"1">> => 1}} end}
        ]}
    ], fun() ->
        PhotoId = <<"200">>,
        UserId = 1,
        Result = group_album_repo:is_liked(PhotoId, UserId),
        ?assertEqual(true, Result)
    end).

is_liked_false_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {error, not_found} end}
        ]}
    ], fun() ->
        PhotoId = <<"photo_not_exist">>,
        UserId = 1,
        Result = group_album_repo:is_liked(PhotoId, UserId),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% add_comment/3 测试
%% ===================================================================

add_comment_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun(_Table) -> 400001 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end},
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        PhotoId = <<"200">>,
        UserId = 1,
        Content = <<"这是一条测试评论"/utf8>>,
        Result = group_album_repo:add_comment(PhotoId, UserId, Content),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% list_comments/2 测试
%% ===================================================================

list_comments_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) ->
                {ok, [#{<<"id">> => 1, <<"content">> => <<"测试评论"/utf8>>}]}
            end}
        ]}
    ], fun() ->
        PhotoId = <<"200">>,
        Result = group_album_repo:list_comments(PhotoId, 10),
        case Result of
            {ok, List} when is_list(List) ->
                ?assert(is_list(List));
            _ ->
                ?assert(false, "Expected list result")
        end
    end).

%% ===================================================================
%% update_album/2 测试
%% ===================================================================

update_album_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        UpdateData = #{<<"id">> => 100, <<"album_name">> => <<"新相册名"/utf8>>},
        Result = group_album_repo:update_album(UpdateData),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete_album/1 测试
%% ===================================================================

delete_album_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        Result = group_album_repo:delete_album(100),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% delete_photo/1 测试
%% ===================================================================

delete_photo_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'update', 4, fun(_Tb, _Data, _Where, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        Result = group_album_repo:delete_photo(200),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% increment_photo_count/1 测试
%% ===================================================================

increment_photo_count_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        Result = group_album_repo:increment_photo_count(100),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% decrement_photo_count/1 测试
%% ===================================================================

decrement_photo_count_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        Result = group_album_repo:decrement_photo_count(100),
        ?assertMatch({ok, _}, Result)
    end).
