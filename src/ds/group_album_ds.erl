-module(group_album_ds).
%%%
% group_album_ds 是群相册 domain service 缩写
% 群相册数据服务层，封装群相册业务逻辑和数据访问
%%%

-export([create_album/4]).
-export([upload_photo/5]).
-export([batch_upload_photos/2]).
-export([delete_photo/2]).
-export([like_photo/2]).
-export([unlike_photo/2]).
-export([add_comment/3]).
-export([list_albums/4]).
-export([list_photos/4]).
-export([get_photo_detail/2]).
-export([update_album_cover/2]).

-include("cache.hrl").
-include("log.hrl").

-define(MAX_PHOTO_SIZE, 10 * 1024 * 1024). % 10MB
-define(ALLOWED_IMAGE_TYPES, [
    <<"image/jpeg">>,
    <<"image/png">>,
    <<"image/gif">>,
    <<"image/webp">>
]).

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 创建相册
%% @param Gid 群组ID
%% @param CreatorId 创建者ID
%% @param AlbumName 相册名称
%% @param CoverPhotoId 封面图片ID（可选）
%% @return {ok, AlbumData} | {error, Reason}
-spec create_album(integer(), integer(), binary(), binary() | undefined) -> {ok, map()} | {error, term()}.
create_album(Gid, CreatorId, AlbumName, CoverPhotoId) ->
    % 1. 验证群成员身份
    case group_ds:is_member(CreatorId, Gid) of
        false ->
            {error, <<"非群组成员"/utf8>>};
        true ->
            % 2. 验证相册名称
            case validate_album_name(AlbumName) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    % 3. 生成相册ID
                    AlbumId = generate_album_id(),

                    % 4. 创建相册
                    case group_album_repo:create_album(Gid, AlbumId, AlbumName, CreatorId) of
                        {ok, Id} ->
                            AlbumData = #{
                                <<"id">> => Id,
                                <<"album_id">> => AlbumId,
                                <<"album_name">> => AlbumName,
                                <<"album_cover">> => CoverPhotoId,
                                <<"photo_count">> => 0,
                                <<"created_at">> => elib_dt:timestamp()
                            },
                            {ok, AlbumData};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 上传图片
%% @param Gid 群组ID
%% @param UploaderId 上传者ID
%% @param AlbumId 相册ID
%% @param PhotoBinary 图片二进制数据
%% @param PhotoName 图片名称
%% @return {ok, PhotoData} | {error, Reason}
-spec upload_photo(integer(), integer(), binary(), binary(), binary()) -> {ok, map()} | {error, term()}.
upload_photo(Gid, UploaderId, AlbumId, PhotoBinary, PhotoName) ->
    % 1. 验证群成员身份
    case group_ds:is_member(UploaderId, Gid) of
        false ->
            {error, <<"非群组成员"/utf8>>};
        true ->
            % 2. 验证图片大小
            PhotoSize = byte_size(PhotoBinary),
            case PhotoSize > ?MAX_PHOTO_SIZE of
                true ->
                    {error, <<"图片大小超出限制"/utf8>>};
                false ->
                    % 3. 验证图片类型
                    MimeType = guess_mime_type(PhotoName),
                    case lists:member(MimeType, ?ALLOWED_IMAGE_TYPES) of
                        false ->
                            {error, <<"图片类型无效"/utf8>>};
                        true ->
                            % 4. 上传到OSS
                            case elib_oss:upload(PhotoBinary, PhotoName, #{mime_type => MimeType}) of
                                {error, Reason} ->
                                    {error, Reason};
                                {ok, PhotoUrl, FileId} ->
                                    % 5. 生成缩略图URL（占位符实现）
                                    ThumbnailUrl = generate_thumbnail_url(PhotoUrl),

                                    % 6. 生成图片ID
                                    PhotoId = FileId,

                                    % 7. 保存图片记录
                                    PhotoData = #{
                                        group_id => Gid,
                                        album_id => AlbumId,
                                        photo_id => PhotoId,
                                        photo_name => PhotoName,
                                        photo_url => PhotoUrl,
                                        thumbnail_url => ThumbnailUrl,
                                        photo_size => PhotoSize,
                                        width => 0,  % 占位符，实际应从图片中提取
                                        height => 0, % 占位符，实际应从图片中提取
                                        uploader_id => UploaderId
                                    },

                                    case group_album_repo:insert_photo(PhotoData) of
                                        {ok, InsertId} ->
                                            % 8. 增加相册照片计数（通过相册ID查找）
                                            case group_album_repo:find_album_by_album_id(AlbumId) of
                                                #{<<"id">> := AlbumRecordId} ->
                                                    group_album_repo:increment_photo_count(AlbumRecordId);
                                                _ ->
                                                    ok
                                            end,

                                            Result = #{
                                                <<"id">> => InsertId,
                                                <<"photo_id">> => PhotoId,
                                                <<"photo_name">> => PhotoName,
                                                <<"photo_url">> => PhotoUrl,
                                                <<"thumbnail_url">> => ThumbnailUrl,
                                                <<"photo_size">> => PhotoSize,
                                                <<"width">> => 0,
                                                <<"height">> => 0,
                                                <<"created_at">> => elib_dt:timestamp()
                                            },
                                            {ok, Result};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end
                            end
                    end
            end
    end.

%% @doc 批量上传图片
%% @param Photos 图片列表 [{AlbumId, PhotoBinary, PhotoName}]
%% @param UploaderId 上传者ID
%% @return {ok, Results} | {error, Reason}
-spec batch_upload_photos(list({integer(), integer(), binary(), binary(), binary()}), integer()) ->
    {ok, list(map())} | {error, term()}.
batch_upload_photos(Photos, UploaderId) ->
    % 逐个上传图片
    Results = lists:map(fun({Gid, AlbumId, PhotoBinary, PhotoName}) ->
        case upload_photo(Gid, UploaderId, AlbumId, PhotoBinary, PhotoName) of
            {ok, PhotoData} ->
                {ok, PhotoData};
            {error, Reason} ->
                {error, Reason}
        end
    end, Photos),

    % 检查是否有失败的情况
    Failed = [R || R <- Results, element(1, R) =:= error],
    case Failed of
        [] ->
            {ok, Results};
        _ ->
            {ok, Results}  % 返回所有结果，包括失败的
    end.

%% @doc 删除图片
%% @param PhotoId 图片ID（主键）
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec delete_photo(integer(), integer()) -> ok | {error, term()}.
delete_photo(PhotoId, CurrentUid) ->
    % 1. 查询图片信息
    case group_album_repo:find_photo_by_id(PhotoId) of
        #{<<"id">> := _, <<"group_id">> := Gid, <<"uploader_id">> := UploaderId, <<"album_id">> := AlbumId} ->
            % 2. 验证权限
            case check_delete_permission(CurrentUid, UploaderId, Gid) of
                {ok, true} ->
                    % 3. 软删除图片
                    case group_album_repo:delete_photo(PhotoId) of
                        {ok, _} ->
                            % 4. 减少相册照片计数（通过相册ID查找）
                            case group_album_repo:find_album_by_album_id(AlbumId) of
                                #{<<"id">> := AlbumRecordId} ->
                                    group_album_repo:decrement_photo_count(AlbumRecordId);
                                _ ->
                                    ok
                            end,
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, <<"图片不存在"/utf8>>}
    end.

%% @doc 点赞图片
%% @param PhotoId 图片ID
%% @param UserId 用户ID
%% @return ok | {error, Reason}
-spec like_photo(binary(), integer()) -> ok | {error, term()}.
like_photo(PhotoId, UserId) ->
    % 检查是否已点赞
    case group_album_repo:is_liked(PhotoId, UserId) of
        true ->
            {error, <<"已点赞该图片"/utf8>>};
        false ->
            case group_album_repo:like_photo(PhotoId, UserId) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 取消点赞
%% @param PhotoId 图片ID
%% @param UserId 用户ID
%% @return ok | {error, Reason}
-spec unlike_photo(binary(), integer()) -> ok | {error, term()}.
unlike_photo(PhotoId, UserId) ->
    case group_album_repo:unlike_photo(PhotoId, UserId) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 添加评论
%% @param PhotoId 图片ID
%% @param UserId 用户ID
%% @param Content 评论内容
%% @return ok | {error, Reason}
-spec add_comment(binary(), integer(), binary()) -> ok | {error, term()}.
add_comment(PhotoId, UserId, Content) ->
    % 验证评论内容
    case validate_comment_content(Content) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case group_album_repo:add_comment(PhotoId, UserId, Content) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 查询相册列表
%% @param Gid 群组ID
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, AlbumList} | {error, Reason}
-spec list_albums(integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_albums(Gid, CurrentUid, Page, Size) ->
    % 验证群成员身份
    case group_ds:is_member(CurrentUid, Gid) of
        false ->
            {error, <<"非群组成员"/utf8>>};
        true ->
            case group_album_repo:list_albums(Gid, Page, Size) of
                {ok, #{<<"list">> := List, <<"total">> := Total}} ->
                    AlbumList = #{
                        <<"items">> => List,
                        <<"total">> => Total,
                        <<"page">> => Page,
                        <<"size">> => Size
                    },
                    {ok, AlbumList};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 查询图片列表
%% @param AlbumId 相册ID
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, PhotoList} | {error, Reason}
-spec list_photos(binary(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_photos(AlbumId, CurrentUid, Page, Size) ->
    % 1. 查询相册信息获取群组ID
    case group_album_repo:find_album_by_album_id(AlbumId) of
        #{<<"group_id">> := Gid} ->
            % 2. 验证群成员身份
            case group_ds:is_member(CurrentUid, Gid) of
                false ->
                    {error, <<"非群组成员"/utf8>>};
                true ->
                    case group_album_repo:list_photos(AlbumId, Page, Size, <<"*">>) of
                        {ok, #{<<"list">> := List, <<"total">> := Total}} ->
                            PhotoList = #{
                                <<"items">> => List,
                                <<"total">> => Total,
                                <<"page">> => Page,
                                <<"size">> => Size
                            },
                            {ok, PhotoList};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        _ ->
            {error, <<"相册不存在"/utf8>>}
    end.

%% @doc 获取图片详情
%% @param PhotoId 图片ID（主键）
%% @param CurrentUid 当前用户ID
%% @return {ok, PhotoDetail} | {error, Reason}
-spec get_photo_detail(integer(), integer()) -> {ok, map()} | {error, term()}.
get_photo_detail(PhotoId, CurrentUid) ->
    case group_album_repo:find_photo_by_id(PhotoId) of
        #{<<"id">> := _, <<"group_id">> := Gid} = Photo ->
            % 验证群成员身份
            case group_ds:is_member(CurrentUid, Gid) of
                false ->
                    {error, <<"非群组成员"/utf8>>};
                true ->
                    % 查询是否已点赞
                    PhotoIdBin = maps:get(<<"photo_id">>, Photo, <<>>),
                    IsLiked = group_album_repo:is_liked(PhotoIdBin, CurrentUid),
                    Photo2 = Photo#{<<"is_liked">> => IsLiked},

                    {ok, Photo2}
            end;
        _ ->
            {error, <<"图片不存在"/utf8>>}
    end.

%% @doc 更新相册封面
%% @param AlbumId 相册ID
%% @param PhotoId 图片ID
%% @return ok | {error, Reason}
-spec update_album_cover(binary(), binary()) -> ok | {error, term()}.
update_album_cover(AlbumId, PhotoId) ->
    case group_album_repo:find_album_by_album_id(AlbumId) of
        #{<<"id">> := Id} ->
            UpdateData = #{id => Id, album_cover => PhotoId},
            case group_album_repo:update_album(UpdateData) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, <<"相册不存在"/utf8>>}
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 验证相册名称
-spec validate_album_name(binary()) -> ok | {error, binary()}.
validate_album_name(<<>>) ->
    {error, <<"相册名称不能为空"/utf8>>};
validate_album_name(Name) when byte_size(Name) > 100 ->
    {error, <<"相册名称过长"/utf8>>};
validate_album_name(_Name) ->
    ok.

%% @doc 验证评论内容
-spec validate_comment_content(binary()) -> ok | {error, binary()}.
validate_comment_content(<<>>) ->
    {error, <<"评论内容不能为空"/utf8>>};
validate_comment_content(Content) when byte_size(Content) > 500 ->
    {error, <<"评论内容过长"/utf8>>};
validate_comment_content(_Content) ->
    ok.

%% @doc 检查删除权限
-spec check_delete_permission(integer(), integer(), integer()) -> {ok, true} | {error, term()}.
check_delete_permission(CurrentUid, UploaderId, _Gid) when CurrentUid =:= UploaderId ->
    {ok, true};
check_delete_permission(CurrentUid, _UploaderId, Gid) ->
    % 检查是否为群主或管理员
    case group_member_repo:find(Gid, CurrentUid, <<"role">>) of
        #{<<"role">> := Role} when Role >= 3 -> % 管理员或群主
            {ok, true};
        _ ->
            {error, <<"相册权限不足"/utf8>>}
    end.

%% @doc 生成相册ID
-spec generate_album_id() -> binary().
generate_album_id() ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<"album_", Timestamp/binary, "_", Random/binary>>.

%% @doc 猜测图片MIME类型
-spec guess_mime_type(binary()) -> binary().
guess_mime_type(FileName) ->
    case filename:extension(FileName) of
        <<".jpg">> -> <<"image/jpeg">>;
        <<".jpeg">> -> <<"image/jpeg">>;
        <<".png">> -> <<"image/png">>;
        <<".gif">> -> <<"image/gif">>;
        <<".webp">> -> <<"image/webp">>;
        _ -> <<"image/jpeg">>
    end.


%% @doc 生成缩略图URL（占位符实现）
-spec generate_thumbnail_url(binary()) -> binary().
generate_thumbnail_url(PhotoUrl) ->
    % 占位符实现：实际应该生成真实的缩略图
    <<PhotoUrl/binary, "?thumbnail=1">>.
