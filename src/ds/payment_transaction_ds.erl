-module(payment_transaction_ds).
%%%
% payment_transaction_ds — 统一支付流水数据服务层 / Payment transaction data service
%
% 职责：payment_transaction_repo 的薄封装 + 幂等记录编排。
%   Logic（payment_callback_logic）不直调 repo，遵循 Handler→Logic→DS→Repo 单向架构。
%
% 幂等核心：record_success/2 先按 (gateway, gateway_payment_no) 探测，
%   命中则视为重复回调直接返回 {ok, already}，不再二次入账。
%%%

-export([create/1]).
-export([find_by_trade_no/1]).
-export([find_by_gateway_no/2]).
-export([update_status/3]).
-export([mark_refunded/1]).
-export([mark_refunding/1, release_refunding/1]).
-export([mark_success/2]).
-export([mark_failed/2]).
-export([page/5]).
-export([reconcile_list/3]).

-include("log.hrl").

-define(STATUS_SUCCESS, 1).
-define(STATUS_FAILED, 2).

%% @doc 创建支付流水（薄封装）
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) ->
    payment_transaction_repo:create(Data).

%% @doc 按内部交易号查询
-spec find_by_trade_no(binary()) -> map().
find_by_trade_no(TradeNo) ->
    payment_transaction_repo:find_by_trade_no(TradeNo).

%% @doc 按 (gateway, gateway_payment_no) 查询 —— 回调幂等探测主路径
-spec find_by_gateway_no(binary(), binary()) -> map().
find_by_gateway_no(Gateway, GatewayPaymentNo) ->
    payment_transaction_repo:find_by_gateway_no(Gateway, GatewayPaymentNo).

%% @doc 更新状态（薄封装）
-spec update_status(binary(), integer(), map()) ->
    {ok, non_neg_integer()} | {error, term()}.
update_status(TradeNo, Status, Extra) ->
    payment_transaction_repo:update_status(TradeNo, Status, Extra).

%% @doc 幂等标记退款（仅 status=1->3 的 CAS，薄封装）
-spec mark_refunded(binary()) -> {ok, 0 | 1} | {error, term()}.
mark_refunded(TradeNo) ->
    payment_transaction_repo:mark_refunded(TradeNo).

%% @doc B-09 退款占位（1→5）/ 释放占位（5→1），见 payment_transaction_repo。
-spec mark_refunding(binary()) -> {ok, 0 | 1} | {error, term()}.
mark_refunding(TradeNo) ->
    payment_transaction_repo:mark_refunding(TradeNo).

-spec release_refunding(binary()) -> {ok, 0 | 1} | {error, term()}.
release_refunding(TradeNo) ->
    payment_transaction_repo:release_refunding(TradeNo).

%% @doc 标记成功 —— 记录 gateway_payment_no / notify_data，落 paid_at
-spec mark_success(binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
mark_success(TradeNo, Extra) ->
    payment_transaction_repo:update_status(TradeNo, ?STATUS_SUCCESS, Extra).

%% @doc 标记失败
-spec mark_failed(binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
mark_failed(TradeNo, Extra) ->
    payment_transaction_repo:update_status(TradeNo, ?STATUS_FAILED, Extra).

%% @doc 分页查询（对账/后台）
-spec page(binary(), map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(Column, WhereMap, Order, Page, Size) ->
    payment_transaction_repo:page(Column, WhereMap, Order, Page, Size).

%% @doc 对账查询：时间段 + 可选状态
-spec reconcile_list(binary(), binary(), integer() | all) ->
    {ok, [map()]} | {error, term()}.
reconcile_list(FromTs, ToTs, Status) ->
    payment_transaction_repo:reconcile_list(FromTs, ToTs, Status).
