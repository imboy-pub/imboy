-module(channel_message_ds).
%%%
% channel_message_ds — G3 架构治理：channel_logic_message 不应直调 channel_message_repo
% G3: thin DS wrapper for channel messages
%%%

-include("log.hrl").

%% ==================== API ====================
-export([find_by_id/1]).
-export([list_by_channel/3]).
-export([update/2]).
-export([delete/1]).
-export([revoke/3]).
-export([get_stats/1]).
-export([page/5]).
-export([list_pinned/1]).

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(MessageId) -> channel_message_repo:find_by_id(MessageId).

-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, Cursor, Limit) ->
    channel_message_repo:list_by_channel(ChannelId, Cursor, Limit).

-spec update(integer(), map()) -> {ok, integer()} | {error, any()}.
update(MessageId, Data) -> channel_message_repo:update(MessageId, Data).

-spec delete(integer()) -> {ok, integer()} | {error, any()}.
delete(MessageId) -> channel_message_repo:delete(MessageId).

-spec revoke(integer(), integer(), binary()) -> ok | {error, any()}.
revoke(MessageId, RevokedBy, RevokedAt) ->
    channel_message_repo:revoke(MessageId, RevokedBy, RevokedAt).

%% G3: channel_logic_stats 不应直调 channel_message_repo:tablename()
-spec get_stats(integer()) -> {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
get_stats(ChannelId) ->
    Tb = channel_message_repo:tablename(),
    Sql = <<
        "SELECT COUNT(*) as total_messages, COALESCE(SUM(view_count), 0) as total_views "
        "FROM ",
        Tb/binary,
        " WHERE channel_id = $1 AND status = 1"
    >>,
    case elib_pg:one(Sql, [ChannelId]) of
        {ok, Row} when is_map(Row) ->
            TotalMsgs = maps:get(<<"total_messages">>, Row, 0),
            TotalViews = maps:get(<<"total_views">>, Row, 0),
            {ok, TotalMsgs, TotalViews};
        _ ->
            {ok, 0, 0}
    end.

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_message_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).

-spec list_pinned(integer()) -> {ok, list(map())} | {error, any()}.
list_pinned(ChannelId) -> channel_message_repo:list_pinned(ChannelId).
