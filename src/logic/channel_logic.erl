-module(channel_logic).
%%%
% channel_logic facade
% 对外保持兼容签名，内部按领域子模块分发
%%%
%% Stable domain boundary for channel_content.
%% API and admin adapters should call this module instead of repo internals.

%% ==================== Specs ====================

%% channel_logic_message delegates
-spec create_channel(integer(), binary(), map(), integer()) -> {ok, map()} | {error, binary()}.
-spec get_channel(binary(), integer()) -> {ok, map()} | {error, binary()}.
-spec get_channel_by_custom_id(binary(), integer()) -> {ok, map()} | {error, binary()}.
-spec update_channel(integer(), binary(), map()) -> {ok, map()} | {error, binary()}.
-spec delete_channel(integer(), binary()) -> ok | {error, binary()}.
-spec publish_message(integer(), binary(), binary(), binary(), map()) ->
    {ok, map()} | {error, binary()}.
-spec publish_message(integer(), binary(), binary(), binary(), map(), binary()) ->
    {ok, map()} | {error, binary()}.
-spec get_messages(integer(), binary(), integer(), integer()) ->
    {ok, list(map())} | {error, binary()}.
-spec mark_as_read(integer(), binary(), binary()) -> ok | {error, binary()}.
-spec pin_message(integer(), binary(), boolean()) -> {ok, map()} | {error, binary()}.
-spec delete_message(integer(), binary()) -> ok | {error, binary()}.
-spec revoke_message(integer(), binary(), binary()) -> ok | {error, binary()}.
-spec search_channels(binary(), integer()) -> {ok, list(map())} | {error, binary()}.
-spec get_discover_channels(integer()) -> {ok, list(map())} | {error, binary()}.
-spec add_admin(integer(), binary(), integer(), integer()) -> ok | {error, binary()}.
-spec remove_admin(integer(), binary(), integer()) -> ok | {error, binary()}.
-spec get_admins(integer() | binary()) -> {ok, list(map())} | {error, binary()}.
-spec update_admin_role(integer(), integer() | binary(), integer(), integer()) ->
    ok | {error, binary()}.

%% channel_logic_subscription delegates
-spec subscribe(integer(), binary()) -> ok | {error, binary()}.
-spec unsubscribe(integer(), binary()) -> ok | {error, binary()}.
-spec get_subscribed_channels(integer()) -> {ok, list(map())} | {error, binary()}.
-spec get_managed_channels(integer()) -> {ok, list(map())} | {error, binary()}.
-spec get_unread_summary(integer()) -> {ok, map()} | {error, binary()}.
-spec get_subscribers(binary(), integer(), integer()) -> {ok, list(map())} | {error, binary()}.
-spec remove_subscriber(integer(), integer() | binary(), integer()) -> ok | {error, binary()}.

%% channel_logic_stats delegates
-spec get_channel_stats(integer(), binary()) -> {ok, map()} | {error, binary()}.
-spec record_message_view(integer(), binary(), binary()) -> ok | {error, binary()}.
-spec add_reaction(integer(), binary(), binary(), binary()) -> ok | {error, binary()}.
-spec remove_reaction(integer(), binary(), binary(), binary()) -> ok | {error, binary()}.
-spec get_daily_stats(integer(), binary(), integer()) -> {ok, list(map())} | {error, binary()}.

%% channel_logic_invitation delegates
-spec create_invitation(integer(), binary(), integer()) ->
    {ok, map()} | {error, binary() | {integer(), binary()}}.
-spec accept_invitation(integer(), integer()) -> ok | {error, binary() | {integer(), binary()}}.
-spec reject_invitation(integer(), integer()) -> ok | {error, binary() | {integer(), binary()}}.
-spec get_my_invitations(integer()) -> {ok, [map()]} | {error, binary()}.
-spec get_sent_invitations(integer()) -> {ok, [map()]} | {error, binary()}.

%% channel_logic_order delegates
-spec create_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
-spec create_order(integer(), binary(), binary()) -> {ok, map()} | {error, binary()}.
-spec pay_order(integer(), binary()) -> {ok, map()} | {error, binary()}.
-spec cancel_order(integer(), binary()) -> ok | {error, binary()}.
-spec get_my_orders(integer()) -> {ok, [map()]} | {error, binary()}.
-spec get_order(integer(), binary()) -> {ok, map()} | {error, binary()}.

%% channel_logic_sync delegates
-spec sync_channels(integer(), integer()) -> {ok, map()} | {error, binary()}.

%% ==================== Exports ====================

-export([create_channel/4]).
-export([get_channel/2]).
-export([get_channel_by_custom_id/2]).
-export([update_channel/3]).
-export([delete_channel/2]).

-export([subscribe/2]).
-export([unsubscribe/2]).
-export([get_subscribed_channels/1]).
-export([get_managed_channels/1]).
-export([get_unread_summary/1]).
-export([get_subscribers/3]).
-export([remove_subscriber/3]).

-export([publish_message/5]).
-export([publish_message/6]).
-export([get_messages/4]).
-export([mark_as_read/3]).
-export([pin_message/3]).
-export([delete_message/2]).
-export([revoke_message/3]).

-export([search_channels/2]).
-export([get_discover_channels/1]).

-export([add_admin/4]).
-export([remove_admin/3]).
-export([get_admins/1]).
-export([update_admin_role/4]).

-export([get_channel_stats/2]).
-export([get_channel_stats_admin/1]).
-export([record_message_view/3]).
-export([add_reaction/4]).
-export([remove_reaction/4]).
-export([get_daily_stats/3]).
-export([get_pinned_messages/2]).
-export([get_message_reactions/3]).
-export([refund_order/2]).
-export([refund_order/3]).

-export([create_invitation/3]).
-export([accept_invitation/2]).
-export([reject_invitation/2]).
-export([get_my_invitations/1]).
-export([get_sent_invitations/1]).

-export([create_order/2]).
-export([create_order/3]).
-export([pay_order/2]).
-export([cancel_order/2]).
-export([get_my_orders/1]).
-export([get_order/2]).

-export([sync_channels/2]).

%% ==================== T5 scope 感知（双体验 v2.5.2 WP3/T5）====================
%% 新增函数，不改上方任何既有函数签名；personal 路径行为零变化。
-export([create_channel/5]).
-export([update_channel_checked/3]).
-export([list_workspace_channels/2]).

%% ==================== Delegates ====================

create_channel(Uid, Name, Opts, MaxChannels) ->
    channel_logic_message:create_channel(Uid, Name, Opts, MaxChannels).

get_channel(ChannelIdBin, Uid) ->
    channel_logic_message:get_channel(ChannelIdBin, Uid).

get_channel_by_custom_id(CustomId, Uid) ->
    channel_logic_message:get_channel_by_custom_id(CustomId, Uid).

update_channel(Uid, ChannelIdBin, Data) ->
    channel_logic_message:update_channel(Uid, ChannelIdBin, Data).

delete_channel(Uid, ChannelIdBin) ->
    channel_logic_message:delete_channel(Uid, ChannelIdBin).

subscribe(Uid, ChannelIdBin) ->
    channel_logic_subscription:subscribe(Uid, ChannelIdBin).

unsubscribe(Uid, ChannelIdBin) ->
    channel_logic_subscription:unsubscribe(Uid, ChannelIdBin).

get_subscribed_channels(Uid) ->
    channel_logic_subscription:get_subscribed_channels(Uid).

get_managed_channels(Uid) ->
    channel_logic_subscription:get_managed_channels(Uid).

get_unread_summary(Uid) ->
    channel_logic_subscription:get_unread_summary(Uid).

get_subscribers(ChannelIdBin, Cursor, Limit) ->
    channel_logic_subscription:get_subscribers(ChannelIdBin, Cursor, Limit).

remove_subscriber(Uid, ChannelId, TargetUid) ->
    channel_logic_subscription:remove_subscriber(Uid, ChannelId, TargetUid).

publish_message(Uid, ChannelIdBin, Content, MsgType, Payload) ->
    channel_logic_message:publish_message(Uid, ChannelIdBin, Content, MsgType, Payload).

publish_message(Uid, ChannelIdBin, Content, MsgType, Payload, RequestId) ->
    channel_logic_message:publish_message(
        Uid, ChannelIdBin, Content, MsgType, Payload, RequestId
    ).

get_messages(Uid, ChannelIdBin, Cursor, Limit) ->
    channel_logic_message:get_messages(Uid, ChannelIdBin, Cursor, Limit).

mark_as_read(Uid, ChannelIdBin, MessageIdBin) ->
    channel_logic_message:mark_as_read(Uid, ChannelIdBin, MessageIdBin).

pin_message(Uid, MessageIdBin, IsPinned) ->
    channel_logic_message:pin_message(Uid, MessageIdBin, IsPinned).

delete_message(Uid, MessageIdBin) ->
    channel_logic_message:delete_message(Uid, MessageIdBin).

revoke_message(Uid, ChannelIdBin, MessageIdBin) ->
    channel_logic_message:revoke_message(Uid, ChannelIdBin, MessageIdBin).

search_channels(Keyword, Limit) ->
    channel_logic_message:search_channels(Keyword, Limit).

get_discover_channels(Limit) ->
    channel_logic_message:get_discover_channels(Limit).

add_admin(Uid, ChannelIdBin, NewAdminUid, Role) ->
    channel_logic_message:add_admin(Uid, ChannelIdBin, NewAdminUid, Role).

remove_admin(Uid, ChannelIdBin, AdminUid) ->
    channel_logic_message:remove_admin(Uid, ChannelIdBin, AdminUid).

get_admins(ChannelId) ->
    channel_logic_message:get_admins(ChannelId).

update_admin_role(Uid, ChannelId, TargetUid, Role) ->
    channel_logic_message:update_admin_role(Uid, ChannelId, TargetUid, Role).

get_channel_stats(Uid, ChannelIdBin) ->
    channel_logic_stats:get_channel_stats(Uid, ChannelIdBin).

-spec get_channel_stats_admin(binary()) -> {ok, map()} | {error, binary()}.
get_channel_stats_admin(ChannelIdBin) ->
    channel_logic_stats:get_channel_stats_admin(ChannelIdBin).

record_message_view(Uid, ChannelIdBin, MessageIdBin) ->
    channel_logic_stats:record_message_view(Uid, ChannelIdBin, MessageIdBin).

add_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType) ->
    channel_logic_stats:add_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType).

remove_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType) ->
    channel_logic_stats:remove_reaction(Uid, ChannelIdBin, MessageIdBin, ReactionType).

get_daily_stats(Uid, ChannelIdBin, Days) ->
    channel_logic_stats:get_daily_stats(Uid, ChannelIdBin, Days).

create_invitation(Uid, ChannelIdBin, InviteeUid) ->
    channel_logic_invitation:create_invitation(Uid, ChannelIdBin, InviteeUid).

accept_invitation(Uid, InvitationId) ->
    channel_logic_invitation:accept_invitation(Uid, InvitationId).

reject_invitation(Uid, InvitationId) ->
    channel_logic_invitation:reject_invitation(Uid, InvitationId).

get_my_invitations(Uid) ->
    channel_logic_invitation:get_my_invitations(Uid).

get_sent_invitations(Uid) ->
    channel_logic_invitation:get_sent_invitations(Uid).

create_order(Uid, ChannelIdBin) ->
    channel_logic_order:create_order(Uid, ChannelIdBin).

create_order(Uid, ChannelIdBin, PaymentMethod) ->
    channel_logic_order:create_order(Uid, ChannelIdBin, PaymentMethod).

pay_order(Uid, OrderNo) ->
    channel_logic_order:pay_order(Uid, OrderNo).

cancel_order(Uid, OrderNo) ->
    channel_logic_order:cancel_order(Uid, OrderNo).

get_my_orders(Uid) ->
    channel_logic_order:get_my_orders(Uid).

get_order(Uid, OrderNo) ->
    channel_logic_order:get_order(Uid, OrderNo).

sync_channels(Uid, Since) ->
    channel_logic_sync:sync_channels(Uid, Since).

-spec get_pinned_messages(integer(), binary() | integer()) ->
    {ok, list(map())} | {error, binary()}.
get_pinned_messages(Uid, ChannelIdBin) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                ok ->
                    case channel_message_ds:list_pinned(ChannelId) of
                        {ok, Messages} when is_list(Messages) ->
                            {ok, channel_logic_message:attach_my_reactions(Uid, Messages)};
                        Other ->
                            Other
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec get_message_reactions(integer(), binary(), binary()) ->
    {ok, list(map())} | {error, binary()}.
get_message_reactions(Uid, ChannelIdBin, MessageId) ->
    ChannelId = channel_logic_common:resolve_channel_id(ChannelIdBin),
    case ChannelId of
        0 ->
            {error, <<"频道不存在"/utf8>>};
        _ ->
            case channel_logic_common:ensure_channel_content_access(Uid, ChannelId) of
                {error, Reason} ->
                    {error, Reason};
                ok ->
                    case channel_message_ds:find_by_id(MessageId) of
                        #{<<"channel_id">> := MsgChannelId} when MsgChannelId =:= ChannelId ->
                            msg_reaction_ds:get_reactions(MessageId, <<"channel">>);
                        _ ->
                            {error, <<"消息不属于该频道"/utf8>>}
                    end
            end
    end.

-spec refund_order(integer(), binary()) -> ok | {error, binary()}.
refund_order(Uid, OrderNo) ->
    channel_logic_order:refund_order(Uid, OrderNo).

-spec refund_order(integer(), binary(), binary()) -> ok | {error, binary()}.
refund_order(Uid, OrderNo, Reason) ->
    channel_logic_order:refund_order(Uid, OrderNo, Reason).

%% ===================================================================
%% T5 scope 感知函数（双体验 v2.5.2 WP3/T5）
%% ===================================================================

%% @doc scope 感知的频道创建（§1.4.2 授权规则：创建 Workspace Channel 须为
%% 该 Workspace 的 Owner/Member；Guest 403；非成员 403）。
%% ScopeCtx：
%%   {personal, 0}            → 完全走既有 create_channel/4 路径（零行为变化）
%%   {workspace, WsId}        → 校验角色后带 scope 字段创建
%%   {OtherScope, _}          → 400
%% scope/workspace_id 创建后不可变（§1.4.2 规则 9，更新接口拒绝改这两字段）。
-spec create_channel(integer(), binary(), map(), integer(), {binary(), integer()}) ->
    {ok, map()} | {error, binary()} | {error, {integer(), binary()}}.
create_channel(Uid, Name, Opts, MaxChannels, {Scope, WorkspaceId}) ->
    case Scope of
        <<"personal">> ->
            create_channel(Uid, Name, Opts, MaxChannels);
        <<"workspace">> ->
            case is_integer(WorkspaceId) andalso WorkspaceId > 0 of
                false ->
                    {error, {400, <<"scope=workspace 时 workspace_id 必须"/utf8>>}};
                true ->
                    %% 仅 Owner/Member 可建（Guest 拒建，§1.4.2 矩阵）
                    case workspace_logic:ensure_can_create_resource(WorkspaceId, Uid) of
                        {error, {Code, Msg}} ->
                            {error, {Code, Msg}};
                        ok ->
                            Opts2 = Opts#{
                                scope => <<"workspace">>,
                                workspace_id => WorkspaceId
                            },
                            create_channel(Uid, Name, Opts2, MaxChannels)
                    end
            end;
        _ ->
            {error, {400, <<"scope 仅支持 personal|workspace"/utf8>>}}
    end.

%% @doc 更新入口守卫：scope 与 workspace_id 创建后不可变（§1.4.2 规则 9）。
%% 提交了这两个字段之一即拒绝（不静默忽略，防止客户端误以为已改归属）。
-spec update_channel_checked(integer(), binary(), map()) ->
    {ok, map()} | {error, binary()} | {error, {400, binary()}}.
update_channel_checked(Uid, ChannelIdBin, Data) ->
    case maps:is_key(<<"scope">>, Data) orelse maps:is_key(<<"workspace_id">>, Data) of
        true ->
            {error, {400, <<"scope 与 workspace_id 创建后不可修改"/utf8>>}};
        false ->
            update_channel(Uid, ChannelIdBin, Data)
    end.

%% @doc 工作区频道列表（scope 严格分区：仅 scope='workspace' 且 status=1，
%% personal 频道列表接口零行为变化）。
-spec list_workspace_channels(integer(), integer()) -> {ok, [map()]} | {error, binary()}.
list_workspace_channels(WorkspaceId, Limit) ->
    case channel_ds:list_workspace_channels(WorkspaceId, Limit) of
        {ok, Channels} -> {ok, [channel_logic_common:channel_transfer(C) || C <- Channels]};
        {error, Reason} -> {error, elib_cnv:safe_to_binary(Reason)}
    end.
