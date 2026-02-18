-module(group_album_logic).
%%%
% group_album_logic 是群相册 logic 缩写
% 群相册业务逻辑层，处理群相册相关的业务逻辑
%%%

-export([create_album/4]).
-export([upload_photo/5]).
-export([batch_upload_photos/3]).
-export([delete_photo/2]).
-export([like_photo/2]).
-export([unlike_photo/2]).
-export([add_comment/3]).
-export([list_albums/4]).
-export([list_photos/4]).
-export([get_photo_detail/2]).
-export([update_album_cover/2]).
-export([rename_album/2]).

-include("log.hrl").

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 创建相册
%% @param Gid 群组ID（HashID编码）
%% @param CurrentUid 当前用户ID
%% @param AlbumName 相册名称
%% @param CoverPhotoId 封面图片ID（可选）
%% @return {ok, AlbumData} | {error, Reason}
-spec create_album(binary(), integer(), binary(), binary() | undefined) -> {ok, map()} | {error, term()}.
create_album(Gid, CurrentUid, AlbumName, CoverPhotoId) ->
    % 1. 解码群组ID
    Gid2 = elib_hashids:decode(Gid),

    % 2. 调用DS层创建相册
    case group_album_ds:create_album(Gid2, CurrentUid, AlbumName, CoverPhotoId) of
        {ok, AlbumData} ->
            % 3. 编码ID
            CreatorId = maps:get(<<"id">>, AlbumData),

            AlbumData2 = AlbumData#{
                <<"id">> => elib_hashids:encode(CreatorId),
                <<"gid">> => Gid
            },
            {ok, AlbumData2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 上传图片
%% @param Gid 群组ID（HashID编码）
%% @param CurrentUid 当前用户ID
%% @param AlbumId 相册ID
%% @param PhotoBinary 图片二进制数据
%% @param PhotoName 图片名称
%% @return {ok, PhotoData} | {error, Reason}
-spec upload_photo(binary(), integer(), binary(), binary(), binary()) -> {ok, map()} | {error, term()}.
upload_photo(Gid, CurrentUid, AlbumId, PhotoBinary, PhotoName) ->
    % 1. 解码群组ID
    Gid2 = elib_hashids:decode(Gid),

    % 2. 调用DS层上传图片
    case group_album_ds:upload_photo(Gid2, CurrentUid, AlbumId, PhotoBinary, PhotoName) of
        {ok, PhotoData} ->
            % 3. 编码ID
            UploaderId = maps:get(<<"id">>, PhotoData),

            PhotoData2 = PhotoData#{
                <<"id">> => elib_hashids:encode(UploaderId),
                <<"gid">> => Gid,
                <<"album_id">> => AlbumId
            },
            {ok, PhotoData2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 批量上传图片
%% @param Gid 群组ID（HashID编码）
%% @param CurrentUid 当前用户ID
%% @param Photos 图片列表 [{AlbumId, PhotoBinary, PhotoName}]
%% @return {ok, Results} | {error, Reason}
-spec batch_upload_photos(binary(), integer(), list({binary(), binary(), binary()})) ->
    {ok, list(map())} | {error, term()}.
batch_upload_photos(Gid, CurrentUid, Photos) ->
    % 1. 解码群组ID
    Gid2 = elib_hashids:decode(Gid),

    % 2. 构建完整的图片数据列表
    Photos2 = [{Gid2, AlbumId, PhotoBinary, PhotoName} || {AlbumId, PhotoBinary, PhotoName} <- Photos],

    % 3. 调用DS层批量上传
    case group_album_ds:batch_upload_photos(Photos2, CurrentUid) of
        {ok, Results} ->
            % 4. 编码结果中的ID
            Results2 = lists:map(fun(Result) ->
                case Result of
                    {ok, PhotoData} ->
                        UploaderId = maps:get(<<"id">>, PhotoData),
                        PhotoData2 = PhotoData#{
                            <<"id">> => elib_hashids:encode(UploaderId),
                            <<"gid">> => Gid
                        },
                        {ok, PhotoData2};
                    {error, Reason} ->
                        {error, Reason}
                end
            end, Results),
            {ok, Results2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除图片
%% @param PhotoId 图片ID（主键）
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec delete_photo(integer(), integer()) -> ok | {error, term()}.
delete_photo(PhotoId, CurrentUid) ->
    group_album_ds:delete_photo(PhotoId, CurrentUid).

%% @doc 点赞图片
%% @param PhotoId 图片ID
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec like_photo(binary(), integer()) -> ok | {error, term()}.
like_photo(PhotoId, CurrentUid) ->
    group_album_ds:like_photo(PhotoId, CurrentUid).

%% @doc 取消点赞
%% @param PhotoId 图片ID
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec unlike_photo(binary(), integer()) -> ok | {error, term()}.
unlike_photo(PhotoId, CurrentUid) ->
    group_album_ds:unlike_photo(PhotoId, CurrentUid).

%% @doc 添加评论
%% @param PhotoId 图片ID
%% @param CurrentUid 当前用户ID
%% @param Content 评论内容
%% @return ok | {error, Reason}
-spec add_comment(binary(), integer(), binary()) -> ok | {error, term()}.
add_comment(PhotoId, CurrentUid, Content) ->
    group_album_ds:add_comment(PhotoId, CurrentUid, Content).

%% @doc 查询相册列表
%% @param Gid 群组ID（HashID编码）
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, AlbumList} | {error, Reason}
-spec list_albums(binary(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_albums(Gid, CurrentUid, Page, Size) ->
    % 1. 解码群组ID
    Gid2 = elib_hashids:decode(Gid),

    % 2. 调用DS层查询相册列表
    group_album_ds:list_albums(Gid2, CurrentUid, Page, Size).

%% @doc 查询图片列表
%% @param AlbumId 相册ID
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, PhotoList} | {error, Reason}
-spec list_photos(binary(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_photos(AlbumId, CurrentUid, Page, Size) ->
    group_album_ds:list_photos(AlbumId, CurrentUid, Page, Size).

%% @doc 获取图片详情
%% @param PhotoId 图片ID（主键）
%% @param CurrentUid 当前用户ID
%% @return {ok, PhotoDetail} | {error, Reason}
-spec get_photo_detail(integer(), integer()) -> {ok, map()} | {error, term()}.
get_photo_detail(PhotoId, CurrentUid) ->
    group_album_ds:get_photo_detail(PhotoId, CurrentUid).

%% @doc 更新相册封面
%% @param AlbumId 相册ID
%% @param PhotoId 图片ID
%% @return ok | {error, Reason}
-spec update_album_cover(binary(), binary()) -> ok | {error, term()}.
update_album_cover(AlbumId, PhotoId) ->
    group_album_ds:update_album_cover(AlbumId, PhotoId).

%% @doc 重命名相册
%% @param AlbumId 相册ID
%% @param NewName 新相册名称
%% @return ok | {error, Reason}
-spec rename_album(binary(), binary()) -> ok | {error, term()}.
rename_album(AlbumId, NewName) ->
    case group_album_repo:find_album_by_album_id(AlbumId) of
        #{<<"id">> := Id} ->
            UpdateData = #{id => Id, album_name => NewName},
            case group_album_repo:update_album(UpdateData) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, <<"相册不存在"/utf8>>}
    end.
