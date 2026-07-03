-module(group_album_handler).
%%%
% group_album_handler 是群相册 REST API 处理器
% 处理群相册相关的 HTTP 请求
%%%

-export([init/2]).
-export([handle_action/3]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 初始化相册处理器
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
%% @param Action 动作类型
%% @param Req Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create_album, Req, State) -> create_album(Req, State);
handle_action(list_albums, Req, State) -> list_albums(Req, State);
handle_action(rename_album, Req, State) -> rename_album(Req, State);
handle_action(delete_album, Req, State) -> delete_album(Req, State);
handle_action(upload_photo, Req, State) -> upload_photo(Req, State);
handle_action(batch_upload, Req, State) -> batch_upload(Req, State);
handle_action(list_photos, Req, State) -> list_photos(Req, State);
handle_action(photo_detail, Req, State) -> photo_detail(Req, State);
handle_action(delete_photo, Req, State) -> delete_photo(Req, State);
handle_action(like_photo, Req, State) -> like_photo(Req, State);
handle_action(unlike_photo, Req, State) -> unlike_photo(Req, State);
handle_action(add_comment, Req, State) -> add_comment(Req, State);
handle_action(list_comments, Req, State) -> list_comments(Req, State);
handle_action(update_cover, Req, State) -> update_cover(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% 相册操作
%% ===================================================================

%% @doc 创建相册
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec create_album(cowboy_req:req(), map()) -> cowboy_req:req().
create_album(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    Gid = maps:get(<<"gid">>, Body, <<>>),
    AlbumName = maps:get(<<"album_name">>, Body, <<>>),
    CoverPhotoId = maps:get(<<"cover_photo_id">>, Body, undefined),

    % 验证参数
    case {Gid, AlbumName} of
        {<<>>, _} ->
            elib_response:error(Req1, <<"群组ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        {_, <<>>} ->
            elib_response:error(Req1, <<"相册名称不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:create_album(Gid, CurrentUid, AlbumName, CoverPhotoId) of
                {ok, AlbumData} ->
                    elib_response:success(Req1, AlbumData, <<"创建相册成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"创建相册失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 查询相册列表
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec list_albums(cowboy_req:req(), map()) -> cowboy_req:req().
list_albums(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),

    Gid = proplists:get_value(<<"gid">>, Qs, <<>>),
    Page = ec_cnv:to_integer(proplists:get_value(<<"page">>, Qs, 1)),
    Size = ec_cnv:to_integer(proplists:get_value(<<"size">>, Qs, 20)),

    % 验证参数
    case Gid of
        <<>> ->
            elib_response:error(Req0, <<"群组ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:list_albums(Gid, CurrentUid, Page, Size) of
                {ok, AlbumList} ->
                    elib_response:success(Req0, AlbumList, <<"查询成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 重命名相册
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec rename_album(cowboy_req:req(), map()) -> cowboy_req:req().
rename_album(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    AlbumId = maps:get(<<"album_id">>, Body, <<>>),
    NewName = maps:get(<<"album_name">>, Body, <<>>),

    % 验证参数
    case {AlbumId, NewName} of
        {<<>>, _} ->
            elib_response:error(Req1, <<"相册ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        {_, <<>>} ->
            elib_response:error(Req1, <<"相册名称不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:rename_album(AlbumId, NewName, CurrentUid) of
                ok ->
                    elib_response:success(Req1, #{}, <<"重命名成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"重命名失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 删除相册
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec delete_album(cowboy_req:req(), map()) -> cowboy_req:req().
delete_album(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    AlbumId = maps:get(<<"album_id">>, Body, <<>>),

    % 验证参数
    case AlbumId of
        <<>> ->
            elib_response:error(Req1, <<"相册ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:delete_album(AlbumId, CurrentUid) of
                ok ->
                    elib_response:success(Req1, #{}, <<"删除成功"/utf8>>);
                {error, <<"相册不存在"/utf8>>} ->
                    elib_response:error(Req1, <<"相册不存在"/utf8>>, ?ERR_ALBUM_NOT_FOUND);
                {error, <<"相册权限不足"/utf8>>} ->
                    elib_response:error(Req1, <<"相册权限不足"/utf8>>, ?ERR_ALBUM_PERMISSION_DENIED);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"删除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% ===================================================================
%% 图片操作
%% ===================================================================

%% @doc 上传图片
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec upload_photo(cowboy_req:req(), map()) -> cowboy_req:req().
upload_photo(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),

    ContentType = cowboy_req:header(<<"content-type">>, Req0, <<>>),
    case binary:match(ContentType, <<"multipart/form-data">>) of
        nomatch ->
            % 处理 JSON 请求（base64 编码的图片）
            upload_photo_json(Req0, CurrentUid);
        _ ->
            upload_photo_multipart(Req0, CurrentUid)
    end.

%% @doc 上传图片（multipart/form-data）
-spec upload_photo_multipart(cowboy_req:req(), integer()) -> cowboy_req:req().
upload_photo_multipart(Req0, CurrentUid) ->
    case read_upload_multipart(Req0, #{}) of
        {ok, Parts, Req1} ->
            Gid = maps:get(gid, Parts, <<>>),
            AlbumId = maps:get(album_id, Parts, <<>>),
            PhotoBinary = maps:get(photo_binary, Parts, <<>>),
            PhotoName = maps:get(photo_name, Parts, <<"photo.jpg">>),

            case {Gid, AlbumId, PhotoBinary} of
                {<<>>, _, _} ->
                    elib_response:error(Req1, <<"群组ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                {_, <<>>, _} ->
                    elib_response:error(Req1, <<"相册ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                {_, _, <<>>} ->
                    elib_response:error(Req1, <<"请上传图片文件"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    case
                        group_album_logic:upload_photo(
                            Gid, CurrentUid, AlbumId, PhotoBinary, PhotoName
                        )
                    of
                        {ok, PhotoData} ->
                            elib_response:success(Req1, PhotoData, <<"上传成功"/utf8>>);
                        {error, <<"相册不存在"/utf8>>} ->
                            elib_response:error(Req1, <<"相册不存在"/utf8>>, ?ERR_ALBUM_NOT_FOUND);
                        {error, <<"相册权限不足"/utf8>>} ->
                            elib_response:error(
                                Req1, <<"相册权限不足"/utf8>>, ?ERR_ALBUM_PERMISSION_DENIED
                            );
                        {error, Reason} when is_binary(Reason) ->
                            elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                        {error, _Reason} ->
                            elib_response:error(Req1, <<"上传失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end;
        {error, malformed_multipart, Req1} ->
            elib_response:error(Req1, <<"请求体格式错误"/utf8>>, ?ERR_BAD_REQUEST)
    end.

%% @doc 上传图片（JSON + base64）
-spec upload_photo_json(cowboy_req:req(), integer()) -> cowboy_req:req().
upload_photo_json(Req0, CurrentUid) ->
    {ok, Body, Req1} = elib_req:body(Req0, []),

    Gid = maps:get(<<"gid">>, Body, <<>>),
    AlbumId = maps:get(<<"album_id">>, Body, <<>>),
    PhotoBase64 = maps:get(<<"photo">>, Body, <<>>),
    PhotoName = maps:get(<<"photo_name">>, Body, <<"photo.jpg">>),

    % 验证参数
    case {Gid, AlbumId, PhotoBase64} of
        {<<>>, _, _} ->
            elib_response:error(Req1, <<"群组ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        {_, <<>>, _} ->
            elib_response:error(Req1, <<"相册ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        {_, _, <<>>} ->
            elib_response:error(Req1, <<"图片数据不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 解码 base64
            try base64:decode(PhotoBase64) of
                PhotoBinary ->
                    case
                        group_album_logic:upload_photo(
                            Gid, CurrentUid, AlbumId, PhotoBinary, PhotoName
                        )
                    of
                        {ok, PhotoData} ->
                            elib_response:success(Req1, PhotoData, <<"上传成功"/utf8>>);
                        {error, <<"相册不存在"/utf8>>} ->
                            elib_response:error(Req1, <<"相册不存在"/utf8>>, ?ERR_ALBUM_NOT_FOUND);
                        {error, <<"相册权限不足"/utf8>>} ->
                            elib_response:error(
                                Req1, <<"相册权限不足"/utf8>>, ?ERR_ALBUM_PERMISSION_DENIED
                            );
                        {error, Reason} when is_binary(Reason) ->
                            elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                        {error, _Reason} ->
                            elib_response:error(Req1, <<"上传失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            catch
                _:_ ->
                    elib_response:error(Req1, <<"图片数据格式错误"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end.

%% @doc 读取 multipart 上传字段
-spec read_upload_multipart(cowboy_req:req(), map()) ->
    {ok, map(), cowboy_req:req()} | {error, malformed_multipart, cowboy_req:req()}.
read_upload_multipart(Req0, Acc0) ->
    case cowboy_req:read_part(Req0) of
        {done, Req1} ->
            {ok, Acc0, Req1};
        {ok, Headers, Req1} ->
            case read_part_body_all(Req1, <<>>) of
                {ok, Data, Req2} ->
                    try cow_multipart:form_data(Headers) of
                        {data, <<"gid">>} ->
                            read_upload_multipart(Req2, Acc0#{gid => Data});
                        {data, <<"album_id">>} ->
                            read_upload_multipart(Req2, Acc0#{album_id => Data});
                        {data, <<"photo_name">>} ->
                            read_upload_multipart(Req2, Acc0#{photo_name => Data});
                        {file, _Field, FileName, _ContentType} ->
                            Acc1 = Acc0#{
                                photo_binary => Data,
                                photo_name => maps:get(photo_name, Acc0, FileName)
                            },
                            read_upload_multipart(Req2, Acc1);
                        _ ->
                            read_upload_multipart(Req2, Acc0)
                    catch
                        _:_ ->
                            {error, malformed_multipart, Req2}
                    end;
                {error, malformed_multipart, Req2} ->
                    {error, malformed_multipart, Req2}
            end
    end.

%% @doc 读取完整 part body（处理 read_part_body/2 的 more 场景）
-spec read_part_body_all(cowboy_req:req(), binary()) ->
    {ok, binary(), cowboy_req:req()} | {error, malformed_multipart, cowboy_req:req()}.
read_part_body_all(Req0, Acc0) ->
    try cowboy_req:read_part_body(Req0, #{length => 20971520, period => 5000}) of
        {ok, Data, Req1} ->
            {ok, <<Acc0/binary, Data/binary>>, Req1};
        {more, Data, Req1} ->
            read_part_body_all(Req1, <<Acc0/binary, Data/binary>>)
    catch
        _:_ ->
            {error, malformed_multipart, Req0}
    end.

%% @doc 批量上传图片
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec batch_upload(cowboy_req:req(), map()) -> cowboy_req:req().
batch_upload(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    Gid = maps:get(<<"gid">>, Body, <<>>),
    Photos = maps:get(<<"photos">>, Body, []),

    % 验证参数
    case Gid of
        <<>> ->
            elib_response:error(Req1, <<"群组ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:batch_upload_photos(Gid, CurrentUid, Photos) of
                {ok, Results} ->
                    elib_response:success(Req1, #{results => Results}, <<"批量上传完成"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"批量上传失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 查询图片列表
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec list_photos(cowboy_req:req(), map()) -> cowboy_req:req().
list_photos(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),

    AlbumId = proplists:get_value(<<"album_id">>, Qs, <<>>),
    Page = ec_cnv:to_integer(proplists:get_value(<<"page">>, Qs, 1)),
    Size = ec_cnv:to_integer(proplists:get_value(<<"size">>, Qs, 20)),

    % 验证参数
    case AlbumId of
        <<>> ->
            elib_response:error(Req0, <<"相册ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:list_photos(AlbumId, CurrentUid, Page, Size) of
                {ok, PhotoList} ->
                    elib_response:success(Req0, PhotoList, <<"查询成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 获取图片详情
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec photo_detail(cowboy_req:req(), map()) -> cowboy_req:req().
photo_detail(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),

    PhotoId = proplists:get_value(<<"photo_id">>, Qs, <<>>),

    % 验证参数
    case PhotoId of
        <<>> ->
            elib_response:error(Req0, <<"图片ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            PhotoIdInt = ec_cnv:to_integer(PhotoId),
            case group_album_logic:get_photo_detail(PhotoIdInt, CurrentUid) of
                {ok, PhotoDetail} ->
                    elib_response:success(Req0, PhotoDetail, <<"查询成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 删除图片
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec delete_photo(cowboy_req:req(), map()) -> cowboy_req:req().
delete_photo(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    PhotoId = maps:get(<<"photo_id">>, Body, <<>>),

    % 验证参数
    case PhotoId of
        <<>> ->
            elib_response:error(Req1, <<"图片ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            PhotoIdInt = ec_cnv:to_integer(PhotoId),
            case group_album_logic:delete_photo(PhotoIdInt, CurrentUid) of
                ok ->
                    elib_response:success(Req1, #{}, <<"删除成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"删除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 点赞图片
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec like_photo(cowboy_req:req(), map()) -> cowboy_req:req().
like_photo(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    PhotoId = maps:get(<<"photo_id">>, Body, <<>>),

    % 验证参数
    case PhotoId of
        <<>> ->
            elib_response:error(Req1, <<"图片ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:like_photo(PhotoId, CurrentUid) of
                ok ->
                    elib_response:success(Req1, #{}, <<"点赞成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"点赞失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 取消点赞
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec unlike_photo(cowboy_req:req(), map()) -> cowboy_req:req().
unlike_photo(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    PhotoId = maps:get(<<"photo_id">>, Body, <<>>),

    % 验证参数
    case PhotoId of
        <<>> ->
            elib_response:error(Req1, <<"图片ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:unlike_photo(PhotoId, CurrentUid) of
                ok ->
                    elib_response:success(Req1, #{}, <<"取消点赞成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"取消点赞失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 添加评论
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec add_comment(cowboy_req:req(), map()) -> cowboy_req:req().
add_comment(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    PhotoId = maps:get(<<"photo_id">>, Body, <<>>),
    Content = maps:get(<<"content">>, Body, <<>>),

    % 验证参数
    case {PhotoId, Content} of
        {<<>>, _} ->
            elib_response:error(Req1, <<"图片ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        {_, <<>>} ->
            elib_response:error(Req1, <<"评论内容不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:add_comment(PhotoId, CurrentUid, Content) of
                ok ->
                    elib_response:success(Req1, #{}, <<"评论成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"评论失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 查询评论列表
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec list_comments(cowboy_req:req(), map()) -> cowboy_req:req().
list_comments(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),

    PhotoId = proplists:get_value(<<"photo_id">>, Qs, <<>>),
    Limit = ec_cnv:to_integer(proplists:get_value(<<"limit">>, Qs, 20)),

    % 验证参数
    case PhotoId of
        <<>> ->
            elib_response:error(Req0, <<"图片ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:list_comments(PhotoId, CurrentUid, Limit) of
                {ok, Comments} ->
                    % 编码用户ID
                    Comments2 = lists:map(
                        fun(Comment) ->
                            UserId = maps:get(<<"user_id">>, Comment, 0),
                            Comment#{<<"user_id">> => UserId}
                        end,
                        Comments
                    ),
                    elib_response:success(Req0, #{comments => Comments2}, <<"查询成功"/utf8>>);
                {error, <<"你不是该群成员"/utf8>>} ->
                    elib_response:error(Req0, <<"你不是该群成员"/utf8>>, ?ERR_FORBIDDEN);
                {error, <<"图片不存在"/utf8>>} ->
                    elib_response:error(Req0, <<"图片不存在"/utf8>>, ?ERR_NOT_FOUND);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 更新相册封面
%% @param Req0 Cowboy请求对象
%% @param State 状态映射
%% @return 处理后的请求对象
-spec update_cover(cowboy_req:req(), map()) -> cowboy_req:req().
update_cover(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {ok, Body, Req1} = elib_req:body(Req0, []),

    AlbumId = maps:get(<<"album_id">>, Body, <<>>),
    PhotoId = maps:get(<<"photo_id">>, Body, <<>>),

    % 验证参数
    case {AlbumId, PhotoId} of
        {<<>>, _} ->
            elib_response:error(Req1, <<"相册ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        {_, <<>>} ->
            elib_response:error(Req1, <<"图片ID不能为空"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case group_album_logic:update_album_cover(AlbumId, PhotoId, CurrentUid) of
                ok ->
                    elib_response:success(Req1, #{}, <<"更新封面成功"/utf8>>);
                {error, Reason} when is_binary(Reason) ->
                    elib_response:error(Req1, Reason, ?ERR_INTERNAL_SERVER_ERROR);
                {error, _Reason} ->
                    elib_response:error(Req1, <<"更新封面失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.
