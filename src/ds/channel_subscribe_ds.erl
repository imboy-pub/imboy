-module(channel_subscribe_ds).
%%%===================================================================
%%% @doc 频道订阅数据服务层
%%%
%%% 封装频道订阅相关的复杂业务逻辑（含缓存）
%%% 处理私有频道邀请和付费频道购买
%%% @end
%%%===================================================================

-include("log.hrl").

%%===================================================================
%%% API Functions
%%===================================================================

-export([check_subscription_permission/2]).
-export([subscribe_private/3]).
-export([subscribe_paid/3]).
-export([is_invited/2]).
-export([has_purchased/2]).
-export([get_invitation_status/2]).
-export([get_purchase_status/2]).
-export([create_invitation/3]).
-export([accept_invitation/2]).
-export([reject_invitation/2]).
-export([create_order/3]).
-export([pay_order/2]).

%%===================================================================
%%% API Implementation
%%===================================================================

%% @doc 检查订阅权限
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @returns {ok, can_subscribe} | {error, Reason}
-spec check_subscription_permission(integer(), integer()) ->
    {ok, can_subscribe} | {ok, need_invitation} | {ok, need_purchase} | {error, term()}.
check_subscription_permission(ChannelId, Uid) ->
    case channel_repo:find_by_id(ChannelId, <<"id,type">>) of
        {error, _} ->
            {error, <<"频道不存在"/utf8>>};
        Channel ->
            Type = maps:get(<<"type">>, Channel, 0),
            case Type of
                1 -> % 私有频道
                    case is_invited(ChannelId, Uid) of
                        true -> {ok, can_subscribe};
                        false -> {ok, need_invitation}
                    end;
                2 -> % 付费频道
                    case has_purchased(ChannelId, Uid) of
                        true -> {ok, can_subscribe};
                        false -> {ok, need_purchase}
                    end;
                _ -> % 公开频道
                    {ok, can_subscribe}
            end
    end.

%% @doc 订阅私有频道
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @param InvitationId 邀请ID（可选，如果有邀请码则自动查找）
%% @returns ok | {error, Reason}
-spec subscribe_private(integer(), integer(), integer() | undefined) -> ok | {error, term()}.
subscribe_private(ChannelId, Uid, InvitationId) ->
    % 检查是否被邀请
    case is_invited(ChannelId, Uid) of
        true ->
            % 接受邀请（会自动创建订阅关系）
            case accept_invitation_internal(ChannelId, Uid, InvitationId) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {error, <<"私有频道需要邀请才能订阅"/utf8>>}
    end.

%% @doc 订阅付费频道
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @param OrderNo 订单号
%% @returns ok | {error, Reason}
-spec subscribe_paid(integer(), integer(), binary()) -> ok | {error, term()}.
subscribe_paid(ChannelId, Uid, OrderNo) ->
    % 检查订单是否有效
    case channel_order_repo:find_by_order_no(OrderNo) of
        {ok, Order} ->
            OrderChannelId = maps:get(<<"channel_id">>, Order),
            OrderUserId = maps:get(<<"user_id">>, Order),
            Status = maps:get(<<"status">>, Order),
            if
                OrderChannelId =/= ChannelId ->
                    {error, <<"订单频道不匹配"/utf8>>};
                OrderUserId =/= Uid ->
                    {error, <<"订单用户不匹配"/utf8>>};
                Status =/= 1 ->
                    {error, <<"订单未支付"/utf8>>};
                true ->
                    % 创建订阅关系（pay/2 已经处理了，这里再确认一下）
                    case channel_subscription_repo:is_subscribed(ChannelId, Uid) of
                        true -> ok;
                        false ->
                            Sql = <<"INSERT INTO channel_subscription (channel_id, user_id, subscribed_at, status) ",
                                    "VALUES ($1, $2, NOW(), 1) ",
                                    "ON CONFLICT (channel_id, user_id) DO UPDATE SET status = 1, subscribed_at = NOW()">>,
                            elib_pg:execute(Sql, [ChannelId, Uid]),
                            ok
                    end
            end;
        {error, _} ->
            {error, <<"订单不存在"/utf8>>}
    end.

%% @doc 检查用户是否被邀请
-spec is_invited(integer(), integer()) -> boolean().
is_invited(ChannelId, Uid) ->
    channel_invitation_repo:is_invited(ChannelId, Uid).

%% @doc 检查用户是否已购买
-spec has_purchased(integer(), integer()) -> boolean().
has_purchased(ChannelId, Uid) ->
    channel_order_repo:has_purchased(ChannelId, Uid).

%% @doc 获取邀请状态
-spec get_invitation_status(integer(), integer()) -> {ok, map()} | {error, not_found}.
get_invitation_status(ChannelId, Uid) ->
    channel_invitation_repo:find_by_channel_and_invitee(ChannelId, Uid).

%% @doc 获取购买状态
-spec get_purchase_status(integer(), integer()) -> {ok, map()} | {error, not_found}.
get_purchase_status(ChannelId, Uid) ->
    channel_order_repo:get_active_subscription(ChannelId, Uid).

%% @doc 创建邀请
%% @param ChannelId 频道ID
%% @param InviterUid 邀请人ID
%% @param InviteeUid 被邀请人ID
%% @returns {ok, InvitationId} | {error, Reason}
-spec create_invitation(integer(), integer(), integer()) -> {ok, integer()} | {error, term()}.
create_invitation(ChannelId, InviterUid, InviteeUid) ->
    % 检查邀请人是否有权限邀请（必须是订阅者）
    case channel_subscription_repo:is_subscribed(ChannelId, InviterUid) of
        true ->
            Data = #{
                channel_id => ChannelId,
                inviter_uid => InviterUid,
                invitee_uid => InviteeUid
            },
            channel_invitation_repo:create(Data);
        false ->
            {error, <<"您不是频道订阅者，无法邀请他人"/utf8>>}
    end.

%% @doc 接受邀请
-spec accept_invitation(integer(), integer()) -> ok | {error, term()}.
accept_invitation(InvitationId, Uid) ->
    case channel_invitation_repo:find_by_id(InvitationId) of
        {ok, Invitation} ->
            ChannelId = maps:get(<<"channel_id">>, Invitation),
            InviteeUid = maps:get(<<"invitee_uid">>, Invitation),
            if
                InviteeUid =/= Uid ->
                    {error, <<"无权接受此邀请"/utf8>>};
                true ->
                    accept_invitation_internal(ChannelId, Uid, InvitationId)
            end;
        {error, _} ->
            {error, <<"邀请不存在"/utf8>>}
    end.

%% @doc 拒绝邀请
-spec reject_invitation(integer(), integer()) -> ok | {error, term()}.
reject_invitation(InvitationId, Uid) ->
    channel_invitation_repo:reject(InvitationId, Uid).

%% @doc 创建订单
%% @param ChannelId 频道ID
%% @param Uid 用户ID
%% @param Opts 可选参数 #{amount => Amount, ...}
-spec create_order(integer(), integer(), map()) -> {ok, binary()} | {error, term()}.
create_order(ChannelId, Uid, Opts) ->
    % 检查是否已购买
    case has_purchased(ChannelId, Uid) of
        true ->
            {error, <<"您已购买此频道"/utf8>>};
        false ->
            % 获取价格信息
            case get_channel_price(ChannelId) of
                {ok, Price} ->
                    Amount = maps:get(amount, Opts, maps:get(<<"price">>, Price, 0)),
                    Currency = maps:get(<<"currency">>, Price, <<"CNY">>),
                    Data = #{
                        channel_id => ChannelId,
                        user_id => Uid,
                        amount => Amount,
                        currency => Currency
                    },
                    channel_order_repo:create_order(Data);
                {error, _} ->
                    {error, <<"频道价格未配置"/utf8>>}
            end
    end.

%% @doc 支付订单（模拟支付）
-spec pay_order(binary(), map()) -> ok | {error, term()}.
pay_order(OrderNo, PaymentData) ->
    channel_order_repo:pay(OrderNo, PaymentData).

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 内部接受邀请函数
accept_invitation_internal(ChannelId, Uid, InvitationId) ->
    % 先获取邀请信息
    case channel_invitation_repo:find_by_channel_and_invitee(ChannelId, Uid) of
        {ok, Invitation} ->
            InvId = maps:get(<<"id">>, Invitation),
            case channel_invitation_repo:accept(InvId, Uid) of
                ok ->
                    % 触发器会自动创建订阅关系，这里确保一下
                    case channel_subscription_repo:is_subscribed(ChannelId, Uid) of
                        true -> ok;
                        false ->
                            % 手动创建订阅关系
                            Sql = <<"INSERT INTO channel_subscription (channel_id, user_id, subscribed_at, status) ",
                                    "VALUES ($1, $2, NOW(), 1) ",
                                    "ON CONFLICT (channel_id, user_id) DO UPDATE SET status = 1, subscribed_at = NOW()">>,
                            elib_pg:execute(Sql, [ChannelId, Uid]),
                            ok
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} when is_integer(InvitationId) ->
            % 尝试使用指定的邀请ID
            channel_invitation_repo:accept(InvitationId, Uid);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取频道价格
get_channel_price(ChannelId) ->
    Sql = <<"SELECT id, channel_id, price, currency, subscription_type, original_price, description ",
            "FROM channel_price WHERE channel_id = $1 AND status = 1">>,
    case elib_pg:query(Sql, [ChannelId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.
