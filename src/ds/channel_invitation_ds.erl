-module(channel_invitation_ds).
%%%
% channel_invitation_ds — G3 架构治理：channel_logic_invitation 不应直调 channel_invitation_repo
% G3: thin DS wrapper for channel invitations
%%%

-include("log.hrl").

%% ==================== API ====================
-export([create/1]).
-export([find_by_id/1]).
-export([find_pending_by_channel_and_invitee/2]).
-export([is_invited/2]).
-export([accept/2]).
-export([reject/2]).
-export([list_pending_by_invitee/1]).
-export([list_by_inviter/2]).
-export([page/5]).

-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data) ->
    %% T7 归档写守卫（P0 收口）：邀请创建与守卫同事务（{channel, Id} 行锁）
    ChannelId = maps:get(channel_id, Data, 0),
    workspace_guard:write_tx({channel, ChannelId}, fun(Conn) ->
        channel_invitation_repo:create_tx(Conn, Data)
    end).

-spec find_by_id(integer()) -> {ok, map()} | {error, any()}.
find_by_id(Id) -> channel_invitation_repo:find_by_id(Id).

-spec find_pending_by_channel_and_invitee(integer(), integer()) ->
    {ok, map()} | {error, not_found}.
find_pending_by_channel_and_invitee(ChannelId, InviteeUid) ->
    channel_invitation_repo:find_pending_by_channel_and_invitee(ChannelId, InviteeUid).

-spec is_invited(integer(), integer()) -> boolean().
is_invited(ChannelId, InviteeUid) ->
    channel_invitation_repo:is_invited(ChannelId, InviteeUid).

-spec accept(integer(), integer()) -> ok | {error, term()}.
accept(InvitationId, Uid) ->
    %% T7 归档写守卫（P0 收口）：接受邀请与守卫同事务（{channel_invitation, Id}）
    workspace_guard:write_tx({channel_invitation, InvitationId}, fun(Conn) ->
        channel_invitation_repo:accept_tx(Conn, InvitationId, Uid)
    end).

-spec reject(integer(), integer()) -> ok | {error, term()}.
reject(InvitationId, Uid) ->
    %% T7 归档写守卫（P0 收口）：拒绝邀请与守卫同事务
    workspace_guard:write_tx({channel_invitation, InvitationId}, fun(Conn) ->
        channel_invitation_repo:reject_tx(Conn, InvitationId, Uid)
    end).

-spec list_pending_by_invitee(integer()) -> {ok, list(map())} | {error, any()}.
list_pending_by_invitee(InviteeUid) ->
    channel_invitation_repo:list_pending_by_invitee(InviteeUid).

-spec list_by_inviter(integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_inviter(InviterUid, Limit) ->
    channel_invitation_repo:list_by_inviter(InviterUid, Limit).

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_invitation_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).
