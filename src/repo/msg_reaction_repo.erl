-module(msg_reaction_repo).
%%%
% msg_reaction_repo 是消息表情回应数据仓库层
% 提供消息表情回应的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([add/4]).              % (MsgId, MsgType, UserId, Emoji)
-export([remove/4]).           % (MsgId, MsgType, UserId, Emoji)
-export([find_by_msg/2]).      % (MsgId, MsgType)
-export([find_by_msg_emoji/3]).% (MsgId, MsgType, Emoji)
-export([count_by_msg/2]).     % (MsgId, MsgType)
-export([count_by_emoji/3]).   % (MsgId, MsgType, Emoji)
-export([find_user_reactions/3]). % (UserId, Page, Size)
-export([remove_all_by_msg/2]). % (MsgId, MsgType) - 用于测试

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取表情回应表的表名
%% @return 返回表情回应表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_reaction">>).

%% @doc 添加表情回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param UserId 用户ID
%% @param Emoji emoji字符
%% @return ok | {error, Reason}
-spec add(binary(), binary(), integer(), binary()) -> ok | {error, term()}.
add(MsgId, MsgType, UserId, Emoji) ->
    % 验证参数
    case Emoji of
        <<>> ->
            {error, empty_emoji};
        _ when byte_size(Emoji) > 100 ->
            {error, emoji_too_long};
        _ ->
            Tb = tablename(),
            Sql = <<"INSERT INTO ", Tb/binary,
                   " (msg_id, msg_type, user_id, emoji) "
                   "VALUES ($1, $2, $3, $4) "
                   "ON CONFLICT (msg_id, msg_type, user_id, emoji) DO NOTHING">>,
            case elib_pg:query(Sql, [MsgId, MsgType, UserId, Emoji]) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc 移除表情回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param UserId 用户ID
%% @param Emoji emoji字符
%% @return ok
-spec remove(binary(), binary(), integer(), binary()) -> ok.
remove(MsgId, MsgType, UserId, Emoji) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary,
           " WHERE msg_id = $1 AND msg_type = $2 "
           "AND user_id = $3 AND emoji = $4">>,
    case elib_pg:query(Sql, [MsgId, MsgType, UserId, Emoji]) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc 查询消息的所有表情回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @return {ok, list(map())} | {error, Reason}
-spec find_by_msg(binary(), binary()) -> {ok, list(map())} | {error, term()}.
find_by_msg(MsgId, MsgType) ->
    Tb = tablename(),
    Sql = <<"SELECT msg_id, msg_type, user_id, emoji, created_at "
           "FROM ", Tb/binary,
           " WHERE msg_id = $1 AND msg_type = $2 "
           "ORDER BY created_at ASC">>,
    case elib_pg:query(Sql, [MsgId, MsgType]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查询消息的特定emoji回应
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param Emoji emoji字符
%% @return {ok, list(map())} | {error, Reason}
-spec find_by_msg_emoji(binary(), binary(), binary()) -> {ok, list(map())} | {error, term()}.
find_by_msg_emoji(MsgId, MsgType, Emoji) ->
    Tb = tablename(),
    Sql = <<"SELECT msg_id, msg_type, user_id, emoji, created_at "
           "FROM ", Tb/binary,
           " WHERE msg_id = $1 AND msg_type = $2 AND emoji = $3 "
           "ORDER BY created_at ASC">>,
    case elib_pg:query(Sql, [MsgId, MsgType, Emoji]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 统计消息的表情总数
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @return integer()
-spec count_by_msg(binary(), binary()) -> integer().
count_by_msg(MsgId, MsgType) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary,
           " WHERE msg_id = $1 AND msg_type = $2">>,
    case elib_pg:query(Sql, [MsgId, MsgType]) of
        {ok, [#{<<"count">> := Count}]} -> Count;
        _ -> 0
    end.

%% @doc 统计特定emoji的数量
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @param Emoji emoji字符
%% @return integer()
-spec count_by_emoji(binary(), binary(), binary()) -> integer().
count_by_emoji(MsgId, MsgType, Emoji) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as count FROM ", Tb/binary,
           " WHERE msg_id = $1 AND msg_type = $2 AND emoji = $3">>,
    case elib_pg:query(Sql, [MsgId, MsgType, Emoji]) of
        {ok, [#{<<"count">> := Count}]} -> Count;
        _ -> 0
    end.

%% @doc 查询用户的表情回应历史
%% @param UserId 用户ID
%% @param Page 页码（从1开始）
%% @param Size 每页大小
%% @return {ok, list(map())} | {error, Reason}
-spec find_user_reactions(integer(), integer(), integer()) -> {ok, list(map())} | {error, term()}.
find_user_reactions(UserId, Page, Size) when Page >= 1, Size > 0 ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    Sql = <<"SELECT msg_id, msg_type, user_id, emoji, created_at "
           "FROM ", Tb/binary,
           " WHERE user_id = $1 "
           "ORDER BY created_at DESC "
           "LIMIT $2 OFFSET $3">>,
    case elib_pg:query(Sql, [UserId, Size, Offset]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end;
find_user_reactions(_, _, _) ->
    {error, invalid_params}.

%% @doc 删除消息的所有表情回应（用于测试和消息删除）
%% @param MsgId 消息ID
%% @param MsgType 消息类型 (c2c/c2g)
%% @return ok
-spec remove_all_by_msg(binary(), binary()) -> ok.
remove_all_by_msg(MsgId, MsgType) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary,
           " WHERE msg_id = $1 AND msg_type = $2">>,
    case elib_pg:query(Sql, [MsgId, MsgType]) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.
