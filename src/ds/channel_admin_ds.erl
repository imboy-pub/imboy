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

-spec add(map()) -> {ok, integer()} | {error, any()}.
add(Data) ->
    %% T7 归档写守卫（R3 #13 收口）：添加频道管理员与守卫同事务
    %% （{channel, Id} → workspace 行锁）；create_channel 事务内路径用 add/2。
    ChannelId = maps:get(channel_id, Data, 0),
    workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
        channel_admin_repo:add(Conn, Data)
    end).

-spec add(epgsql:connection(), map()) -> {ok, integer()} | {error, any()}.
add(Conn, Data) -> channel_admin_repo:add(Conn, Data).

-spec delete(integer(), integer()) -> {ok, integer()} | {error, any()}.
delete(ChannelId, AdminUid) ->
    %% T7 归档写守卫（R3 #13 收口）：移除频道管理员与守卫同事务
    workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
        channel_admin_repo:delete_tx(Conn, ChannelId, AdminUid)
    end).

-spec list_by_channel(integer()) -> {ok, list(map())} | {error, any()}.
list_by_channel(ChannelId) -> channel_admin_repo:list_by_channel(ChannelId).

-spec update_role(integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
update_role(ChannelId, Uid, Role) ->
    %% T7 归档写守卫（R3 #13 收口）：管理员角色变更与守卫同事务
    workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
        channel_admin_repo:update_role_tx(Conn, ChannelId, Uid, Role)
    end).

-spec get_role(integer(), integer()) -> integer() | {error, any()}.
get_role(ChannelId, Uid) -> channel_admin_repo:get_role(ChannelId, Uid).

-spec find(integer(), integer()) -> map() | {error, any()}.
find(ChannelId, Uid) -> channel_admin_repo:find(ChannelId, Uid).

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_admin_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).
