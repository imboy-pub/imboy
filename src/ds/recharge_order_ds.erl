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
-export([page/3]).
-export([credit_in_tx/4]).

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

%% @doc 跨用户充值订单分页查询（运营后台用，薄封装）
-spec page(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page(WhereMap, Page, Size) ->
    recharge_order_repo:page(WhereMap, Page, Size).

%% @doc 单事务充值入账（订单状态+钱包余额+流水三表原子，幂等）
-spec credit_in_tx(binary(), binary(), integer(), integer()) ->
    {ok, integer()}
    | {rollback, already_credited | order_not_payable | wallet_not_found}
    | {error, term()}.
credit_in_tx(OrderNo, GwPayNo, Uid, Amount) ->
    recharge_order_repo:credit_in_tx(OrderNo, GwPayNo, Uid, Amount).
