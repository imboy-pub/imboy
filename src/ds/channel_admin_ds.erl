-module(channel_admin_ds).
%%%
% channel_admin_ds — G3 架构治理：channel_logic_message/common 不应直调 channel_admin_repo
% G3: thin DS wrapper for channel admins
%%%

-include("log.hrl").

%% ==================== API ====================
-export([add/1]).
-export([add/2]).
-export([delete/2]).
-export([list_by_channel/1]).
-export([update_role/3]).
-export([get_role/2]).
-export([find/2]).
-export([page/5]).

-spec add(map()) -> {ok, integer(), map()} | {error, any()}.
add(Data) -> channel_admin_repo:add(Data).

-spec add(epgsql:connection(), map()) -> {ok, integer(), map()} | {error, any()}.
add(Conn, Data) -> channel_admin_repo:add(Conn, Data).

-spec delete(integer(), integer()) -> {ok, integer()} | {error, any()}.
delete(ChannelId, AdminUid) -> channel_admin_repo:delete(ChannelId, AdminUid).

-spec list_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId) -> channel_admin_repo:list_by_channel(ChannelId).

-spec update_role(integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
update_role(ChannelId, Uid, Role) -> channel_admin_repo:update_role(ChannelId, Uid, Role).

-spec get_role(integer(), integer()) -> integer() | {error, any()}.
get_role(ChannelId, Uid) -> channel_admin_repo:get_role(ChannelId, Uid).

-spec find(integer(), integer()) -> map() | {error, any()}.
find(ChannelId, Uid) -> channel_admin_repo:find(ChannelId, Uid).

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_admin_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).
