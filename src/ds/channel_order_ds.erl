-module(channel_order_ds).
-moduledoc "频道订单数据服务（G3）—— logic 层不应直调 channel_order_repo。".
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
-export([set_gateway_params/3]).
-export([mark_refunding/1, finalize_refund/3, release_refunding/1]).

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
refund(OrderNo, _OperatorUid, Reason) ->
    refund_transition(OrderNo, 1, 2, Reason).

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

-spec set_gateway_params(binary(), binary(), map()) -> ok | {error, term()}.
set_gateway_params(OrderNo, PayNo, Extra) ->
    channel_order_repo:set_gateway_params(OrderNo, PayNo, Extra).

%% @doc B-09 CAS 退款占位：paid(1) → refunding(5)
-spec mark_refunding(binary()) -> {ok, 0 | 1} | {error, term()}.
mark_refunding(OrderNo) ->
    refund_transition(OrderNo, 1, 5, <<>>).

%% @doc B-09 CAS 退款收尾：refunding(5) → refunded(2)
-spec finalize_refund(binary(), integer(), binary()) -> ok | {error, term()}.
finalize_refund(OrderNo, _Uid, Reason) ->
    refund_transition(OrderNo, 5, 2, Reason).

%% @doc B-09 CAS 释放退款占位：refunding(5) → paid(1)
-spec release_refunding(binary()) -> {ok, 0 | 1} | {error, term()}.
release_refunding(OrderNo) ->
    channel_order_repo:release_refunding(OrderNo).

%% Channel -> group sequence -> order CAS. Revocation and successful refund's
%% subscription deletion commit with the order; a failed step aborts all writes.
refund_transition(OrderNo, From, To, Reason) ->
    Result = elib_pg:with_tx(fun(Conn) ->
        case channel_order_repo:find_for_transition_tx(Conn, OrderNo) of
            {ok, [#{<<"channel_id">> := Cid, <<"user_id">> := Uid}]} ->
                _ = micro_community_access_repo:lock_channel_tx(Conn, Cid),
                case channel_order_repo:transition_tx(Conn, OrderNo, From, {To, Reason}) of
                    {ok, 0} ->
                        {ok, 0};
                    {ok, 1} ->
                        case To of
                            2 ->
                                ok = unsubscribe_refunded_tx(Conn, Cid, Uid),
                                ok = micro_community_access_ds:revoke_channel_tx(Conn, Cid, Uid);
                            %% Status 5 temporarily rejects new entitlement checks. A
                            %% failed gateway refund can return to paid without losing
                            %% membership or its existing history generation.
                            5 ->
                                ok
                        end,
                        {ok, 1};
                    {error, Err} ->
                        throw({abort_tx, Err})
                end;
            {ok, []} ->
                {ok, 0};
            {error, Err} ->
                throw({abort_tx, Err})
        end
    end),
    case {To, Result} of
        {5, _} -> Result;
        {2, {ok, 1}} -> ok;
        {2, {ok, 0}} when From =:= 5 -> {error, not_found_or_not_refunding};
        {2, {ok, 0}} -> {error, not_found_or_not_paid};
        _ -> Result
    end.

unsubscribe_refunded_tx(Conn, Cid, Uid) ->
    case channel_subscription_repo:delete(Conn, Cid, Uid) of
        {ok, 0} ->
            ok;
        {ok, N} when N > 0 ->
            case channel_repo:increment_subscribers(Conn, Cid, -1) of
                {ok, _} -> ok;
                {error, Err} -> throw({abort_tx, Err})
            end;
        {error, Err} ->
            throw({abort_tx, Err})
    end.
