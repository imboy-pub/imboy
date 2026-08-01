-module(channel_order_ds).
%%%
% channel_order_ds — G3 架构治理：channel_logic_order 不应直调 channel_order_repo
% G3: thin DS wrapper for channel orders
%%%

-include("log.hrl").

%% ==================== API ====================
-export([create_order/1]).
-export([find_by_order_no/1]).
-export([pay/2, pay/3]).
-export([refund/3]).
-export([cancel/1]).
-export([has_purchased/2]).
-export([get_price/1]).
-export([list_by_user/2]).
-export([page/5]).

-spec create_order(map()) -> {ok, binary()} | {error, term()}.
create_order(Data) -> channel_order_repo:create_order(Data).

-spec find_by_order_no(binary()) -> {ok, map()} | {error, any()}.
find_by_order_no(OrderNo) -> channel_order_repo:find_by_order_no(OrderNo).

-spec pay(binary(), map()) -> ok | {error, term()}.
pay(OrderNo, PaymentData) -> channel_order_repo:pay(OrderNo, PaymentData).

%% @doc 带过期宽限的支付（B-08）：仅第三方回调侧使用，见 channel_order_repo:pay/3。
-spec pay(binary(), map(), non_neg_integer()) -> ok | {error, term()}.
pay(OrderNo, PaymentData, GraceMinutes) ->
    channel_order_repo:pay(OrderNo, PaymentData, GraceMinutes).

%% @doc 退款：将已支付(1)订单更新为已退款(2)，并记录退款原因/时间
-spec refund(binary(), integer(), binary()) -> ok | {error, term()}.
refund(OrderNo, OperatorUid, Reason) ->
    channel_order_repo:refund(OrderNo, OperatorUid, Reason).

%% @doc 取消：将待支付(0)订单更新为已取消(3)
-spec cancel(binary()) -> ok | {error, term()}.
cancel(OrderNo) -> channel_order_repo:cancel(OrderNo).

-spec has_purchased(integer(), integer()) -> boolean().
has_purchased(ChannelId, Uid) -> channel_order_repo:has_purchased(ChannelId, Uid).

%% @doc 获取频道价格配置（启用状态的价格记录）
-spec get_price(integer()) -> {ok, map()} | {error, not_found | term()}.
get_price(ChannelId) ->
    Sql =
        <<"SELECT id, channel_id, price, currency, subscription_type, original_price, description ",
            "FROM channel_price WHERE channel_id = $1 AND status = 1">>,
    case elib_pg:query(Sql, [ChannelId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

-spec list_by_user(integer(), integer()) -> {ok, list(map())} | {error, any()}.
list_by_user(UserId, Limit) -> channel_order_repo:list_by_user(UserId, Limit).

-spec page(binary(), map(), binary(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, Where, Order, Page, Size) ->
    Tb = channel_order_repo:tablename(),
    elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size).
