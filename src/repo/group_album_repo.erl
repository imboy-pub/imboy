-module(group_album_repo).
%%%
% group_album_repo 是群相册 repository 缩写
% 群相册数据仓库层，提供群相册数据的基础数据库操作
%%%

-export([tablename/0]).

%% 相册操作
-export([create_album/4]).
-export([find_album_by_id/1]).
-export([find_album_by_album_id/1]).
-export([list_albums/3]).
-export([update_album/1]).
-export([delete_album/1]).
-export([increment_photo_count/1]).
-export([decrement_photo_count/1]).

%% 图片操作
-export([insert_photo/1]).
-export([find_photo_by_id/1]).
-export([list_photos/4]).
-export([delete_photo/1]).

%% 点赞操作
-export([like_photo/2]).
-export([unlike_photo/2]).
-export([is_liked/2]).

%% 评论操作
-export([add_comment/3]).
-export([list_comments/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取群相册表的表名
%% @return 返回群相册表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"group_album">>).

%% @doc 获取相册图片表的表名
-spec photo_tablename() -> binary().
photo_tablename() ->
    elib_pg_sql:public_tablename(<<"group_album_photo">>).

%% @doc 获取点赞表的表名
-spec like_tablename() -> binary().
like_tablename() ->
    elib_pg_sql:public_tablename(<<"group_album_photo_like">>).

%% @doc 获取评论表的表名
-spec comment_tablename() -> binary().
comment_tablename() ->
    elib_pg_sql:public_tablename(<<"group_album_photo_comment">>).

%% ===================================================================
%% 相册操作
%% ===================================================================

%% @doc 创建相册
%% @param Gid 群组ID
%% @param AlbumId 相册ID（唯一标识）
%% @param AlbumName 相册名称
%% @param CreatorId 创建者ID
%% @return {ok, Id} | {error, Reason}
-spec create_album(integer(), binary(), binary(), integer()) -> {ok, integer()} | {error, term()}.
create_album(Gid, AlbumId, AlbumName, CreatorId) ->
    Tb = tablename(),
    Data = #{
        group_id => Gid,
        album_id => AlbumId,
        album_name => AlbumName,
        creator_id => CreatorId,
        created_at => elib_dt:now()
    },
    Id = elib_tsid:generate(group_album),
    Data2 = Data#{id => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 根据ID查找相册
%% @param Id 相册ID
%% @return Map 查询成功返回相册信息map，未找到返回空map
-spec find_album_by_id(integer()) -> map().
find_album_by_id(Id) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [Id]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 根据相册ID（业务ID）查找相册
%% @param AlbumId 相册ID（业务ID）
%% @return Map 查询成功返回相册信息map，未找到返回空map
-spec find_album_by_album_id(binary()) -> map().
find_album_by_album_id(AlbumId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE album_id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [AlbumId]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 查询相册列表
%% @param Gid 群组ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, PageMap} | {error, Reason}
-spec list_albums(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_albums(Gid, Page, Size) ->
    Tb = tablename(),
    Column = <<"id, group_id, album_id, album_name, album_cover, creator_id, photo_count, created_at">>,
    elib_pg:page_with_total(Tb, Column, #{group_id => Gid, status => 1}, <<"created_at DESC">>, Page, Size).

%% @doc 更新相册
%% @param Data 包含更新数据的map（必须包含id字段）
%% @return {ok, Count} | {error, Reason}
-spec update_album(map()) -> {ok, non_neg_integer()} | {error, term()}.
update_album(Data) ->
    Tb = tablename(),
    Id = maps:get(<<"id">>, Data),
    UpdateData = maps:without([<<"id">>], Data),
    UpdateData2 = UpdateData#{updated_at => elib_dt:now()},
    elib_pg:update(Tb, UpdateData2, <<"id = $1">>, [Id]).

%% @doc 删除相册（软删除）
%% @param Id 相册ID
%% @return {ok, Count} | {error, Reason}
-spec delete_album(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_album(Id) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => 0, updated_at => elib_dt:now()}, <<"id = $1">>, [Id]).

%% @doc 增加相册照片计数
%% @param Id 相册ID
%% @return {ok, Count} | {error, Reason}
-spec increment_photo_count(integer()) -> {ok, non_neg_integer()} | {error, term()}.
increment_photo_count(Id) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET photo_count = photo_count + 1, updated_at = $1 WHERE id = $2">>,
    elib_pg:execute(Sql, [elib_dt:now(), Id]).

%% @doc 减少相册照片计数
%% @param Id 相册ID
%% @return {ok, Count} | {error, Reason}
-spec decrement_photo_count(integer()) -> {ok, non_neg_integer()} | {error, term()}.
decrement_photo_count(Id) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET photo_count = GREATEST(photo_count - 1, 0), updated_at = $1 WHERE id = $2">>,
    elib_pg:execute(Sql, [elib_dt:now(), Id]).

%% ===================================================================
%% 图片操作
%% ===================================================================

%% @doc 插入图片
%% @param Data 图片数据map
%% @return {ok, Id} | {error, Reason}
-spec insert_photo(map()) -> {ok, integer()} | {error, term()}.
insert_photo(Data) ->
    Tb = photo_tablename(),
    Id = elib_tsid:generate(group_album_photo),
    Data2 = Data#{id => Id, created_at => elib_dt:now()},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 根据ID查找图片
%% @param Id 图片ID
%% @return Map 查询成功返回图片信息map，未找到返回空map
-spec find_photo_by_id(integer()) -> map().
find_photo_by_id(Id) ->
    Tb = photo_tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 AND status = 1">>,
    case elib_pg:one(Sql, [Id]) of
        {ok, Row} -> Row;
        _ -> #{}
    end.

%% @doc 查询图片列表
%% @param AlbumId 相册ID
%% @param Page 页码
%% @param Size 每页数量
%% @param Column 查询的列
%% @return {ok, PageMap} | {error, Reason}
-spec list_photos(binary(), integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
list_photos(AlbumId, Page, Size, Column) ->
    Tb = photo_tablename(),
    elib_pg:page_with_total(Tb, Column, #{album_id => AlbumId, status => 1}, <<"created_at DESC">>, Page, Size).

%% @doc 删除图片（软删除）
%% @param Id 图片ID
%% @return {ok, Count} | {error, Reason}
-spec delete_photo(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_photo(Id) ->
    Tb = photo_tablename(),
    elib_pg:update(Tb, #{status => 0}, <<"id = $1">>, [Id]).

%% ===================================================================
%% 点赞操作
%% ===================================================================

%% @doc 点赞图片
%% @param PhotoId 图片ID
%% @param UserId 用户ID
%% @return {ok, Count} | {error, Reason}
-spec like_photo(binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
like_photo(PhotoId, UserId) ->
    TbPhoto = photo_tablename(),
    TbLike = like_tablename(),
    Now = elib_dt:now(),

    % 插入点赞记录
    Id = elib_tsid:generate(group_album),
    LikeData = #{id => Id, photo_id => PhotoId, user_id => UserId, created_at => Now},
    {SqlInsert, ParamsInsert} = elib_pg_sql:insert(TbLike, LikeData),
    case elib_pg:query(SqlInsert, ParamsInsert) of
        {ok, _Count} ->
            % 增加点赞计数
            Sql = <<"UPDATE ", TbPhoto/binary, " SET like_count = like_count + 1 WHERE id = $1">>,
            elib_pg:execute(Sql, [PhotoId]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 取消点赞
%% @param PhotoId 图片ID
%% @param UserId 用户ID
%% @return {ok, Count} | {error, Reason}
-spec unlike_photo(binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
unlike_photo(PhotoId, UserId) ->
    TbPhoto = photo_tablename(),
    TbLike = like_tablename(),

    % 删除点赞记录
    Sql = <<"DELETE FROM ", TbLike/binary, " WHERE photo_id = $1 AND user_id = $2">>,
    case elib_pg:execute(Sql, [PhotoId, UserId]) of
        {ok, _} ->
            % 减少点赞计数
            Sql2 = <<"UPDATE ", TbPhoto/binary, " SET like_count = GREATEST(like_count - 1, 0) WHERE id = $1">>,
            elib_pg:execute(Sql2, [PhotoId]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 检查是否已点赞
%% @param PhotoId 图片ID
%% @param UserId 用户ID
%% @return true | false
-spec is_liked(binary(), integer()) -> boolean().
is_liked(PhotoId, UserId) ->
    Tb = like_tablename(),
    Sql = <<"SELECT 1 FROM ", Tb/binary, " WHERE photo_id = $1 AND user_id = $2 LIMIT 1">>,
    case elib_pg:one(Sql, [PhotoId, UserId]) of
        {ok, _} -> true;
        _ -> false
    end.

%% ===================================================================
%% 评论操作
%% ===================================================================

%% @doc 添加评论
%% @param PhotoId 图片ID
%% @param UserId 用户ID
%% @param Content 评论内容
%% @return {ok, Id} | {error, Reason}
-spec add_comment(binary(), integer(), binary()) -> {ok, integer()} | {error, term()}.
add_comment(PhotoId, UserId, Content) ->
    TbComment = comment_tablename(),
    TbPhoto = photo_tablename(),
    Now = elib_dt:now(),

    % 插入评论记录
    CommentId = elib_tsid:generate(group_album_comment),
    CommentData = #{id => CommentId, photo_id => PhotoId, user_id => UserId, content => Content, created_at => Now},
    {SqlInsert, ParamsInsert} = elib_pg_sql:insert(TbComment, CommentData),
    case elib_pg:query(SqlInsert, ParamsInsert) of
        {ok, _Count} ->
            % 增加评论计数
            Sql = <<"UPDATE ", TbPhoto/binary, " SET comment_count = comment_count + 1 WHERE id = $1">>,
            elib_pg:execute(Sql, [PhotoId]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询评论列表
%% @param PhotoId 图片ID
%% @param Limit 返回数量限制
%% @return {ok, List} | {error, Reason}
-spec list_comments(binary(), integer()) -> {ok, list(map())} | {error, term()}.
list_comments(PhotoId, Limit) ->
    Tb = comment_tablename(),
    Sql = <<"SELECT id, photo_id, user_id, content, created_at FROM ", Tb/binary,
            " WHERE photo_id = $1 AND status = 1 ORDER BY created_at ASC LIMIT $2">>,
    elib_pg:query(Sql, [PhotoId, Limit]).

%% @doc 删除评论
%% @param Id 评论ID
%% @return {ok, Count} | {error, Reason}
-spec delete_comment(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_comment(Id) ->
    Tb = comment_tablename(),
    elib_pg:update(Tb, #{status => 0}, <<"id = $1">>, [Id]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
