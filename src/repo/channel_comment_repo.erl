-module(channel_comment_repo).
%%%
% channel_comment_repo — 频道评论数据仓库层
% 镜像 channel_message_repo 模式，职责单一：纯 SQL。
%%%
%% Internal persistence detail for the channel_content domain.

-export([tablename/0]).
-export([add/1]).
-export([find_by_id/1]).
-export([list_by_message/3]).
-export([list_by_channel/3]).
-export([delete/1]).
-export([count_by_message/1]).
-export([increment_like/1]).
-export([decrement_like/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"channel_comment">>).

%% @doc 添加评论
-spec add(map()) -> {ok, integer()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(channel_comment),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 按 ID 查询
-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(CommentId) ->
    Tb = tablename(),
    Sql = <<"SELECT * FROM ", Tb/binary, " WHERE id = $1 AND status = 1">>,
    case elib_pg:query(Sql, [CommentId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row]} -> Row;
        {error, _} = Err -> Err
    end.

%% @doc 按消息查评论（游标分页，镜像 list_by_channel）
-spec list_by_message(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_message(MessageId, 0, Limit) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE message_id = $1 AND status = 1 "
            "ORDER BY created_at ASC LIMIT $2">>,
    elib_pg:query(Sql, [MessageId, Limit]);
list_by_message(MessageId, Cursor, Limit) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE message_id = $1 AND status = 1 AND id > $2 "
            "ORDER BY created_at ASC LIMIT $3">>,
    elib_pg:query(Sql, [MessageId, Cursor, Limit]).

%% @doc 按频道查最新评论（频道评论总览）
-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, 0, Limit) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 "
            "ORDER BY created_at DESC LIMIT $2">>,
    elib_pg:query(Sql, [ChannelId, Limit]);
list_by_channel(ChannelId, Cursor, Limit) ->
    Tb = tablename(),
    Sql =
        <<"SELECT * FROM ", Tb/binary,
            " WHERE channel_id = $1 AND status = 1 AND id < $2 "
            "ORDER BY created_at DESC LIMIT $3">>,
    elib_pg:query(Sql, [ChannelId, Cursor, Limit]).

%% @doc 软删除评论
-spec delete(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete(CommentId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET status = 0 WHERE id = $1">>,
    elib_pg:query(Sql, [CommentId]).

%% @doc 统计消息评论数
-spec count_by_message(integer()) -> {ok, non_neg_integer()} | {error, any()}.
count_by_message(MessageId) ->
    Tb = tablename(),
    Sql = <<"SELECT COUNT(*) as cnt FROM ", Tb/binary,
            " WHERE message_id = $1 AND status = 1">>,
    case elib_pg:query(Sql, [MessageId]) of
        {ok, [#{<<"cnt">> := Count}]} -> {ok, Count};
        {error, _} = Err -> Err
    end.

%% @doc 点赞 +1
-spec increment_like(integer()) -> {ok, non_neg_integer()} | {error, any()}.
increment_like(CommentId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET like_count = like_count + 1 WHERE id = $1 AND status = 1">>,
    elib_pg:query(Sql, [CommentId]).

%% @doc 点赞 -1（下限 0）
-spec decrement_like(integer()) -> {ok, non_neg_integer()} | {error, any()}.
decrement_like(CommentId) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET like_count = GREATEST(like_count - 1, 0) WHERE id = $1 AND status = 1">>,
    elib_pg:query(Sql, [CommentId]).
