-module(group_album_logic).
-dialyzer({nowarn_function, [batch_upload_photos/3]}).
%%%
% group_album_logic 是群相册 logic 缩写
% 群相册业务逻辑层，处理群相册相关的业务逻辑
%%%

-export([create_album/4]).
-export([upload_photo/5]).
-export([batch_upload_photos/3]).
-export([delete_album/2]).
-export([delete_photo/2]).
-export([like_photo/2]).
-export([unlike_photo/2]).
-export([add_comment/3]).
-export([list_albums/4]).
-export([list_photos/4]).
-export([get_photo_detail/2]).
-export([update_album_cover/3]).
-export([rename_album/3]).
-export([list_comments/3]).

-include("log.hrl").

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 创建相册
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @param AlbumName 相册名称
%% @param CoverPhotoId 封面图片ID（可选）
%% @return {ok, AlbumData} | {error, Reason}
-spec create_album(binary(), integer(), binary(), binary() | undefined) ->
    {ok, map()} | {error, term()}.
create_album(Gid, CurrentUid, AlbumName, CoverPhotoId) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

    % 2. 调用DS层创建相册
    case group_album_ds:create_album(Gid2, CurrentUid, AlbumName, CoverPhotoId) of
        {ok, AlbumData} ->
            % 3. 编码ID
            AlbumId = maps:get(<<"id">>, AlbumData),

            AlbumData2 = AlbumData#{
                <<"id">> => AlbumId,
                <<"gid">> => Gid
            },
            {ok, AlbumData2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 上传图片
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @param AlbumId 相册ID
%% @param PhotoBinary 图片二进制数据
%% @param PhotoName 图片名称
%% @return {ok, PhotoData} | {error, Reason}
-spec upload_photo(binary(), integer(), binary(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
upload_photo(Gid, CurrentUid, AlbumId, PhotoBinary, PhotoName) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

    % 2. 调用DS层上传图片
    case group_album_ds:upload_photo(Gid2, CurrentUid, AlbumId, PhotoBinary, PhotoName) of
        {ok, PhotoData} ->
            % 3. 编码ID
            PhotoId = maps:get(<<"id">>, PhotoData),

            PhotoData2 = PhotoData#{
                <<"id">> => PhotoId,
                <<"gid">> => Gid,
                <<"album_id">> => AlbumId
            },
            {ok, PhotoData2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 批量上传图片
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @param Photos 图片列表 [{AlbumId, PhotoBinary, PhotoName}]
%% @return {ok, Results} | {error, Reason}
-spec batch_upload_photos(binary(), integer(), list({binary(), binary(), binary()})) ->
    {ok, list(map())} | {error, term()}.
batch_upload_photos(Gid, CurrentUid, Photos) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

    % 2. 构建完整的图片数据列表
    Photos2 = [
        {Gid2, AlbumId, PhotoBinary, PhotoName}
     || {AlbumId, PhotoBinary, PhotoName} <- Photos
    ],

    % 3. 调用DS层批量上传
    case group_album_ds:batch_upload_photos(Photos2, CurrentUid) of
        {ok, Results} ->
            % 4. 编码结果中的ID
            Results2 = lists:map(
                fun(Result) ->
                    case Result of
                        {ok, PhotoData} ->
                            PhotoId2 = maps:get(<<"id">>, PhotoData),
                            PhotoData2 = PhotoData#{
                                <<"id">> => PhotoId2,
                                <<"gid">> => Gid
                            },
                            {ok, PhotoData2};
                        {error, Reason} ->
                            {error, Reason}
                    end
                end,
                Results
            ),
            {ok, Results2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除相册
%% @param AlbumId 相册ID（业务ID）
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec delete_album(binary(), integer()) -> ok | {error, term()}.
delete_album(AlbumId, CurrentUid) ->
    case group_album_ds:find_album_by_album_id(AlbumId) of
        #{<<"id">> := Id, <<"group_id">> := Gid, <<"creator_id">> := CreatorId} ->
            case check_album_delete_permission(CurrentUid, CreatorId, Gid) of
                ok ->
                    case group_album_ds:delete_album(Id) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, <<"相册不存在"/utf8>>}
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
    case ensure_photo_group_member(PhotoId, CurrentUid) of
        ok -> group_album_ds:like_photo(PhotoId, CurrentUid);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取消点赞
%% @param PhotoId 图片ID
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec unlike_photo(binary(), integer()) -> ok | {error, term()}.
unlike_photo(PhotoId, CurrentUid) ->
    case ensure_photo_group_member(PhotoId, CurrentUid) of
        ok -> group_album_ds:unlike_photo(PhotoId, CurrentUid);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 校验当前用户是否为图片所在群组的成员
%% 注意：group_ds:is_member/2 参数顺序为 (Uid, Gid)，与 group_member_ds/group_logic
%% 同名函数的参数顺序不同，调用时务必确认。
-spec ensure_photo_group_member(binary(), integer()) -> ok | {error, binary()}.
ensure_photo_group_member(PhotoId, CurrentUid) ->
    case group_album_ds:find_photo_group_id(PhotoId) of
        {ok, Gid} ->
            case group_ds:is_member(CurrentUid, Gid) of
                true -> ok;
                false -> {error, <<"你不是该群成员"/utf8>>}
            end;
        {error, not_found} ->
            {error, <<"图片不存在"/utf8>>}
    end.

%% @doc 添加评论
%% @param PhotoId 图片ID
%% @param CurrentUid 当前用户ID
%% @param Content 评论内容
%% @return ok | {error, Reason}
-spec add_comment(binary(), integer(), binary()) -> ok | {error, term()}.
add_comment(PhotoId, CurrentUid, Content) ->
    case ensure_photo_group_member(PhotoId, CurrentUid) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            group_album_ds:add_comment(PhotoId, CurrentUid, Content)
    end.

%% @doc 查询相册列表
%% @param Gid 群组ID（整数）
%% @param CurrentUid 当前用户ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, AlbumList} | {error, Reason}
-spec list_albums(binary(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
list_albums(Gid, CurrentUid, Page, Size) ->
    % 1. 解码群组ID
    Gid2 = ec_cnv:to_integer(Gid),

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
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec update_album_cover(binary(), binary(), integer()) -> ok | {error, term()}.
update_album_cover(AlbumId, PhotoId, CurrentUid) ->
    case group_album_ds:find_album_by_album_id(AlbumId) of
        #{<<"group_id">> := Gid, <<"creator_id">> := CreatorId} ->
            case check_album_delete_permission(CurrentUid, CreatorId, Gid) of
                ok -> group_album_ds:update_album_cover(AlbumId, PhotoId);
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, <<"相册不存在"/utf8>>}
    end.

%% @doc 重命名相册
%% @param AlbumId 相册ID
%% @param NewName 新相册名称
%% @param CurrentUid 当前用户ID
%% @return ok | {error, Reason}
-spec rename_album(binary(), binary(), integer()) -> ok | {error, term()}.
rename_album(AlbumId, NewName, CurrentUid) ->
    case group_album_ds:find_album_by_album_id(AlbumId) of
        #{<<"id">> := Id, <<"group_id">> := Gid, <<"creator_id">> := CreatorId} ->
            case check_album_delete_permission(CurrentUid, CreatorId, Gid) of
                ok ->
                    UpdateData = #{id => Id, album_name => NewName},
                    case group_album_ds:update_album(UpdateData) of
                        {ok, _} -> ok;
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, <<"相册不存在"/utf8>>}
    end.

%% @doc 查询图片评论列表
%% IDOR 防御：CurrentUid 必须是该图片所属群的成员，否则任意登录用户传任意
%% photo_id 都能读取评论内容（该函数唯一调用方为用户侧 handler，无管理端
%% 用例，故直接在此收口，无需另设无鉴权变体）。
%% @param PhotoId 图片ID
%% @param CurrentUid 当前用户ID
%% @param Limit 返回条数上限
%% @return {ok, CommentList} | {error, Reason}
-spec list_comments(binary(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
list_comments(PhotoId, CurrentUid, Limit) ->
    case ensure_photo_group_member(PhotoId, CurrentUid) of
        ok -> group_album_ds:list_comments(PhotoId, Limit);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查相册删除权限
-spec check_album_delete_permission(integer(), integer(), integer()) -> ok | {error, binary()}.
check_album_delete_permission(CurrentUid, CreatorId, _Gid) when CurrentUid =:= CreatorId ->
    ok;
check_album_delete_permission(CurrentUid, _CreatorId, Gid) ->
    case group_member_ds:find_by_gid_and_uid(Gid, CurrentUid, <<"role">>) of
        #{<<"role">> := Role} when Role >= 3 ->
            ok;
        _ ->
            {error, <<"相册权限不足"/utf8>>}
    end.
