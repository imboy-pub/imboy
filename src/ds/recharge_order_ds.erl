-module(recharge_order_ds).
%%%
% recharge_order_ds — 充值订单数据服务层 / Recharge order data service layer
%
% 职责：薄封装 recharge_order_repo，遵循 Handler→Logic→DS→Repo 单向架构。
%       recharge_logic 不直调 recharge_order_repo。
%%%

-include("log.hrl").

-export([create/1]).
-export([find_by_order_no/1]).
-export([mark_paid/2]).
-export([update_status/2]).
-export([page_by_user/3]).

%% @doc 创建充值订单（status=0）
-spec create(map()) -> {ok, binary()} | {error, term()}.
create(Data) -> recharge_order_repo:create(Data).

%% @doc 根据订单号查询
-spec find_by_order_no(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_order_no(OrderNo) -> recharge_order_repo:find_by_order_no(OrderNo).

%% @doc 标记已支付（条件更新，幂等保护）
-spec mark_paid(binary(), binary()) -> ok | {error, not_found_or_paid | term()}.
mark_paid(OrderNo, GatewayPaymentNo) ->
    recharge_order_repo:mark_paid(OrderNo, GatewayPaymentNo).

%% @doc 更新订单状态
-spec update_status(binary(), integer()) -> ok | {error, not_found | term()}.
update_status(OrderNo, Status) ->
    recharge_order_repo:update_status(OrderNo, Status).

%% @doc 分页查询用户充值订单
-spec page_by_user(integer(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page_by_user(Uid, Page, Size) ->
    recharge_order_repo:page_by_user(Uid, Page, Size).
