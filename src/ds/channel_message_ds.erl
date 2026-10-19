-module(channel_message_ds).
-moduledoc "频道消息数据服务（G3）—— logic 层不应直调 channel_message_repo。".
%%%
% channel_message_ds — G3 架构治理：channel_logic_message 不应直调 channel_message_repo
% G3: thin DS wrapper for channel messages
%%%

-include("log.hrl").

%% ==================== API ====================
-export([find_by_id/1]).
-export([list_by_channel/3]).
-export([update/2]).
-export([update_checked/3, delete_checked/2]).
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
update(MessageId, Data) ->
    %% T7 归档写守卫（P0 收口）：置顶/编辑等消息更新与守卫同事务
    %% （{channel_message, Id} → channel → workspace 行锁）。
    workspace_guard:write_tx({channel_message, MessageId}, fun(Conn) ->
        channel_message_repo:update_tx(Conn, MessageId, Data)
    end).

update_checked(MessageId, Data, Expected) ->
    workspace_guard:write_tx({channel_message, MessageId}, fun(Conn) ->
        channel_message_repo:update_checked_tx(Conn, MessageId, Data, Expected)
    end).

delete_checked(MessageId, Content) ->
    workspace_guard:write_tx({channel_message, MessageId}, fun(Conn) ->
        channel_message_repo:delete_checked_tx(Conn, MessageId, Content)
    end).

-spec delete(integer()) -> {ok, integer()} | {error, any()}.
delete(MessageId) ->
    %% T7 归档写守卫（P0 收口）：消息软删与守卫同事务
    workspace_guard:write_tx({channel_message, MessageId}, fun(Conn) ->
        channel_message_repo:delete_tx(Conn, MessageId)
    end).

-spec revoke(integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, any()}.
revoke(MessageId, RevokedBy, RevokedAt) ->
    %% T7 归档写守卫（P0 收口）：消息撤回与守卫同事务
    workspace_guard:write_tx({channel_message, MessageId}, fun(Conn) ->
        channel_message_repo:revoke_tx(Conn, MessageId, RevokedBy, RevokedAt)
    end).

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
        {ok, #{<<"total_messages">> := TotalMsgs0, <<"total_views">> := TotalViews0}} ->
            case {stats_count(TotalMsgs0), stats_count(TotalViews0)} of
                {{ok, TotalMsgs}, {ok, TotalViews}} -> {ok, TotalMsgs, TotalViews};
                _ -> {error, invalid_channel_message_stats}
            end;
        {error, _} = Error ->
            Error;
        _ ->
            {error, invalid_channel_message_stats}
    end.

%% SUM(bigint) is PostgreSQL numeric, returned as exact integer text by epgsql.
%% Keep arbitrary precision; malformed counts must not look like empty statistics.
stats_count(Value) when is_integer(Value), Value >= 0 ->
    {ok, Value};
stats_count(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Count when Count >= 0 -> {ok, Count};
        _ -> {error, invalid_count}
    catch
        error:badarg -> {error, invalid_count}
    end;
stats_count(_) ->
    {error, invalid_count}.

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_message_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).

-spec list_pinned(integer()) -> {ok, list(map())} | {error, any()}.
list_pinned(ChannelId) -> channel_message_repo:list_pinned(ChannelId).
