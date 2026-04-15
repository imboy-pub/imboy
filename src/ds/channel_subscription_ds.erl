-module(channel_subscription_ds).
%%%
% channel_subscription_ds — G3 架构治理
% G3: thin DS wrapper for channel subscriptions
%%%

-include("log.hrl").

%% ==================== API ====================
-export([list_by_uid/1]).
%% G3 thin wrappers for channel_logic_*
-export([is_subscribed/2]).
-export([count_unread/1]).
-export([count_unread_channels/1]).
-export([list_unread_by_uid/1]).
-export([list_by_channel/3]).
-export([delete/3]).
-export([clear_unread/2]).
-export([list_unread_counts_by_channel/1]).
-export([page/5]).

%% @doc 查询用户订阅列表（pass-through 到 repo）
-spec list_by_uid(integer()) -> {ok, list(map())} | {error, any()}.
list_by_uid(Uid) ->
    channel_subscription_repo:list_by_uid(Uid).

-spec is_subscribed(integer(), integer()) -> boolean().
is_subscribed(ChannelId, Uid) -> channel_subscription_repo:is_subscribed(ChannelId, Uid).

-spec count_unread(integer()) -> integer().
count_unread(Uid) -> channel_subscription_repo:count_unread(Uid).

-spec count_unread_channels(integer()) -> integer().
count_unread_channels(Uid) -> channel_subscription_repo:count_unread_channels(Uid).

-spec list_unread_by_uid(integer()) -> {ok, list(map())} | {error, any()}.
list_unread_by_uid(Uid) -> channel_subscription_repo:list_unread_by_uid(Uid).

-spec list_by_channel(integer(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId, Cursor, Limit) ->
    channel_subscription_repo:list_by_channel(ChannelId, Cursor, Limit).

-spec delete(epgsql:connection(), integer(), integer()) -> {ok, integer()} | {error, any()}.
delete(Conn, ChannelId, Uid) -> channel_subscription_repo:delete(Conn, ChannelId, Uid).

-spec clear_unread(integer(), integer()) -> ok | {error, any()}.
clear_unread(ChannelId, Uid) -> channel_subscription_repo:clear_unread(ChannelId, Uid).

-spec list_unread_counts_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_unread_counts_by_channel(ChannelId) ->
    channel_subscription_repo:list_unread_counts_by_channel(ChannelId).

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_subscription_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).
