-module(channel_comment_logic).
%%%
% channel_comment_logic — 频道评论业务逻辑层
% 镜像 channel_logic_message 模式：校验访问权 → 编排 ds → 转换输出。
%%%

-export([create/5]).
-export([list_by_message/5]).
-export([delete/2]).
-export([like/2]).
-export([unlike/2]).
-export([count_by_message/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 角色：>=3 为管理者（admin/creator），可删除他人评论
-define(CHANNEL_ROLE_ADMIN, 3).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 创建评论
-spec create(integer(), binary(), binary(), binary(), integer()) ->
    {ok, map()} | {error, binary()}.
create(Uid, ChannelIdBin, MessageIdBin, Content, ParentId) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case Content of
                <<>> ->
                    {error, <<"评论内容不能为空"/utf8>>};
                _ ->
                    case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                        ok ->
                            do_create_comment(Uid, ChannelId, MessageIdBin, Content, ParentId);
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc 查询消息评论列表
-spec list_by_message(integer(), binary(), binary(), integer(), integer()) ->
    {ok, list(map())} | {error, binary()}.
list_by_message(Uid, ChannelIdBin, MessageIdBin, Cursor, Limit) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    MessageId = elib_cnv:safe_to_integer(MessageIdBin),
                    case channel_comment_ds:list_by_message(MessageId, Cursor, Limit) of
                        {ok, Comments} when is_list(Comments) ->
                            {ok, [comment_transfer(C) || C <- Comments, is_map(C)]};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 删除评论（仅作者或管理者）
-spec delete(integer(), integer()) -> ok | {error, binary()}.
delete(Uid, CommentId) ->
    case channel_comment_ds:find_by_id(CommentId) of
        #{<<"user_id">> := Uid} ->
            do_delete(CommentId);
        #{<<"channel_id">> := ChannelId} ->
            Role = channel_logic_common:get_user_role(ChannelId, Uid),
            case Role >= ?CHANNEL_ROLE_ADMIN of
                true -> do_delete(CommentId);
                false -> {error, <<"无权删除该评论"/utf8>>}
            end;
        {error, _} ->
            {error, <<"评论不存在"/utf8>>}
    end.

%% @doc 点赞评论
-spec like(integer(), integer()) -> ok | {error, binary()}.
like(Uid, CommentId) ->
    case ensure_comment_access(Uid, CommentId) of
        {ok, _ChannelId} ->
            case channel_comment_ds:like(CommentId) of
                {ok, _} -> ok;
                {error, _} -> {error, <<"操作失败"/utf8>>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 取消点赞
-spec unlike(integer(), integer()) -> ok | {error, binary()}.
unlike(Uid, CommentId) ->
    case ensure_comment_access(Uid, CommentId) of
        {ok, _ChannelId} ->
            case channel_comment_ds:unlike(CommentId) of
                {ok, _} -> ok;
                {error, _} -> {error, <<"操作失败"/utf8>>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 评论数
-spec count_by_message(binary()) -> {ok, non_neg_integer()} | {error, binary()}.
count_by_message(MessageIdBin) ->
    MessageId = elib_cnv:safe_to_integer(MessageIdBin),
    channel_comment_ds:count_by_message(MessageId).

%% 点赞/取消点赞属于付费频道互动，必须与评论读取使用同一访问门。
%% 不能只依赖 comment_id 存在，否则未购买用户可通过枚举 ID 修改付费内容数据。
-spec ensure_comment_access(integer(), integer()) -> {ok, integer()} | {error, binary()}.
ensure_comment_access(Uid, CommentId) ->
    case channel_comment_ds:find_by_id(CommentId) of
        #{<<"channel_id">> := ChannelId} when is_integer(ChannelId), ChannelId > 0 ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok -> {ok, ChannelId};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, <<"评论不存在"/utf8>>}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

do_create_comment(Uid, ChannelId, MessageIdBin, Content, ParentId) ->
    MessageId = elib_cnv:safe_to_integer(MessageIdBin),
    case channel_message_ds:find_by_id(MessageId) of
        #{<<"channel_id">> := ChannelId} ->
            UserMap = user_ds:find_by_id(Uid, <<"nickname,avatar">>),
            UserName = maps:get(<<"nickname">>, UserMap, <<>>),
            UserAvatar = maps:get(<<"avatar">>, UserMap, <<>>),
            Data = #{
                <<"channel_id">> => ChannelId,
                <<"message_id">> => MessageId,
                <<"user_id">> => Uid,
                <<"user_name">> => UserName,
                <<"user_avatar">> => UserAvatar,
                <<"content">> => Content,
                <<"parent_id">> => ParentId
            },
            case channel_comment_ds:add(Data) of
                {ok, CommentId} ->
                    Comment = channel_comment_ds:find_by_id(CommentId),
                    {ok, comment_transfer(Comment)};
                {error, _} ->
                    {error, <<"评论失败"/utf8>>}
            end;
        _ ->
            {error, <<"消息不存在"/utf8>>}
    end.

do_delete(CommentId) ->
    case channel_comment_ds:delete(CommentId) of
        {ok, _} -> ok;
        {error, _} -> {error, <<"删除失败"/utf8>>}
    end.

-spec comment_transfer(map()) -> map().
comment_transfer(Comment) when is_map(Comment) ->
    Comment.
