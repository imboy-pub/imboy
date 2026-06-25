-module(mention_repo).
%%%
% mention_repo 是 @提及数据仓库层
% 提供 @提及记录的基础数据库操作
%%%
-include("chat.hrl").
-include("log.hrl").

-export([tablename/0]).
-export([insert/4]).
-export([find_by_msg_id/1]).
-export([find_msg_id_by_mention_id/2]).
-export([find_by_uid/2]).
-export([find_by_group_and_uid/3]).
-export([mark_as_read/2]).
-export([mark_all_as_read/1]).
-export([mark_group_as_read/2]).
-export([count_unread/1]).
-export([count_unread_in_group/2]).
-export([delete_by_msg_id/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取@提及记录表的表名
%% @return 返回@提及记录表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_mention">>).

%% @doc 插入@提及记录
%% @param MsgId 消息ID
%% @param Gid 群组ID
%% @param MentionedUid 被@的用户ID
%% @param FromUid 发送者ID
%% @return ok | {error, Reason}
-spec insert(binary(), integer(), integer(), integer()) -> ok | {error, term()}.
insert(MsgId, Gid, MentionedUid, FromUid) when
    MsgId =:= <<>>; Gid =:= 0; MentionedUid =:= 0; FromUid =:= 0
->
    {error, invalid_params};
insert(MsgId, Gid, MentionedUid, FromUid) ->
    Tb = tablename(),
    Data = #{
        msg_id => MsgId,
        group_id => Gid,
        mentioned_uid => MentionedUid,
        from_uid => FromUid,
        is_read => false
    },
    Id = elib_tsid:generate(msg_mention),
    Data2 = Data#{id => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据消息ID查找所有@提及记录
%% @param MsgId 消息ID
%% @return {ok, list(map())} | {error, Reason}
-spec find_by_msg_id(binary()) -> {ok, list(map())} | {error, term()}.
find_by_msg_id(MsgId) ->
    Tb = tablename(),
    Column = <<"id, msg_id, group_id, mentioned_uid, from_uid, is_read, created_at">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE msg_id = $1">>,
    elib_pg:query(Sql, [MsgId]).

%% @doc 根据 mention 记录ID查找消息ID（用于兼容客户端传 mention_id 标记已读）
%% @param MentionId mention 记录ID
%% @param Uid 当前用户ID（mentioned_uid）
%% @return {ok, MsgId} | {error, not_found | Reason}
-spec find_msg_id_by_mention_id(integer(), integer()) -> {ok, binary()} | {error, term()}.
find_msg_id_by_mention_id(MentionId, Uid) when MentionId > 0 ->
    Tb = tablename(),
    Sql = <<"SELECT msg_id FROM ", Tb/binary, " WHERE id = $1 AND mentioned_uid = $2">>,
    case elib_pg:one(Sql, [MentionId, Uid]) of
        {ok, #{<<"msg_id">> := MsgId}} ->
            {ok, MsgId};
        _ ->
            {error, not_found}
    end;
find_msg_id_by_mention_id(_, _) ->
    {error, invalid_params}.

%% @doc 查询用户的所有@提及（支持已读/未读过滤）
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @return {ok, list(map())} | {error, Reason}
-spec find_by_uid(integer(), boolean() | undefined) -> {ok, list(map())} | {error, term()}.
find_by_uid(Uid, undefined) ->
    Tb = tablename(),
    Column = <<"id, msg_id, group_id, mentioned_uid, from_uid, is_read, created_at">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE mentioned_uid = $1 ORDER BY created_at DESC">>,
    elib_pg:query(Sql, [Uid]);
find_by_uid(Uid, IsRead) ->
    Tb = tablename(),
    Column = <<"id, msg_id, group_id, mentioned_uid, from_uid, is_read, created_at">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE mentioned_uid = $1 AND is_read = $2 ORDER BY created_at DESC">>,
    elib_pg:query(Sql, [Uid, IsRead]).

%% @doc 查询用户在指定群组的@提及
%% @param Gid 群组ID
%% @param Uid 用户ID
%% @param IsRead 已读状态（true/false/undefined 表示全部）
%% @return {ok, list(map())} | {error, Reason}
-spec find_by_group_and_uid(integer(), integer(), boolean() | undefined) ->
    {ok, list(map())} | {error, term()}.
find_by_group_and_uid(Gid, Uid, undefined) ->
    Tb = tablename(),
    Column = <<"id, msg_id, group_id, mentioned_uid, from_uid, is_read, created_at">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE group_id = $1 AND mentioned_uid = $2 ORDER BY created_at DESC">>,
    elib_pg:query(Sql, [Gid, Uid]);
find_by_group_and_uid(Gid, Uid, IsRead) ->
    Tb = tablename(),
    Column = <<"id, msg_id, group_id, mentioned_uid, from_uid, is_read, created_at">>,
    Sql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE group_id = $1 AND mentioned_uid = $2 AND is_read = $3 ORDER BY created_at DESC">>,
    elib_pg:query(Sql, [Gid, Uid, IsRead]).

%% @doc 标记@消息为已读
%% @param MsgId 消息ID
%% @param Uid 被@的用户ID
%% @return ok | {error, Reason}
-spec mark_as_read(binary(), integer()) -> ok | {error, term()}.
mark_as_read(MsgId, Uid) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET is_read = true WHERE msg_id = $1 AND mentioned_uid = $2">>,
    case elib_pg:execute(Sql, [MsgId, Uid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记用户所有@消息为已读
%% @param Uid 被@的用户ID
%% @return ok | {error, Reason}
-spec mark_all_as_read(integer()) -> ok | {error, term()}.
mark_all_as_read(Uid) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET is_read = true WHERE mentioned_uid = $1 AND is_read = false">>,
    case elib_pg:execute(Sql, [Uid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记用户在指定群组中的@消息为已读
%% @param Gid 群组ID
%% @param Uid 被@的用户ID
%% @return ok | {error, Reason}
-spec mark_group_as_read(integer(), integer()) -> ok | {error, term()}.
mark_group_as_read(Gid, Uid) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET is_read = true WHERE group_id = $1 AND mentioned_uid = $2 AND is_read = false">>,
    case elib_pg:execute(Sql, [Gid, Uid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 统计用户未读的@消息数量
%% @param Uid 用户ID
%% @return 未读数量
-spec count_unread(integer()) -> non_neg_integer().
count_unread(Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE mentioned_uid = $1 AND is_read = false">>,
    case elib_pg:one(Sql, [Uid]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 统计用户在指定群组中的未读@消息数量
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @return 未读数量
-spec count_unread_in_group(integer(), integer()) -> non_neg_integer().
count_unread_in_group(Uid, Gid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE mentioned_uid = $1 AND group_id = $2 AND is_read = false">>,
    case elib_pg:one(Sql, [Uid, Gid]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 根据消息ID删除所有@提及记录
%% @param MsgId 消息ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_msg_id(binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_msg_id(MsgId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE msg_id = $1">>,
    elib_pg:execute(Sql, [MsgId]).
