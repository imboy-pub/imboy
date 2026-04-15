-module(channel_invitation_ds).
%%%
% channel_invitation_ds — G3 架构治理：channel_logic_invitation 不应直调 channel_invitation_repo
% G3: thin DS wrapper for channel invitations
%%%

-include("log.hrl").

%% ==================== API ====================
-export([find_by_id/1]).
-export([list_pending_by_invitee/1]).
-export([list_by_inviter/2]).
-export([page/5]).

-spec find_by_id(integer()) -> map() | {error, any()}.
find_by_id(Id) -> channel_invitation_repo:find_by_id(Id).

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
