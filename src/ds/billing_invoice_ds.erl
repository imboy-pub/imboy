-module(billing_invoice_ds).
%%%
% billing_invoice_ds — 账单数据服务层 / SaaS invoice data service
%
% 职责：薄封装 billing_invoice_repo，遵循 Handler→Logic→DS→Repo 单向架构。
%%%

-include("log.hrl").

-export([create/1]).
-export([find_by_id/1]).
-export([find_by_invoice_no/1]).
-export([mark_paid/2]).
-export([mark_overdue/1]).
-export([list_by_subscription/1]).
-export([page/4]).

%% @doc 创建账单
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) -> billing_invoice_repo:create(Data).

%% @doc 按 id 查询账单
-spec find_by_id(integer()) -> map().
find_by_id(Id) -> billing_invoice_repo:find_by_id(Id).

%% @doc 按 invoice_no 查询账单
-spec find_by_invoice_no(binary()) -> map().
find_by_invoice_no(InvoiceNo) ->
    billing_invoice_repo:find_by_invoice_no(InvoiceNo).

%% @doc 标记已支付（幂等）
-spec mark_paid(binary(), binary()) -> ok | {error, not_found_or_paid | term()}.
mark_paid(InvoiceNo, PaymentNo) ->
    billing_invoice_repo:mark_paid(InvoiceNo, PaymentNo).

%% @doc 标记逾期
-spec mark_overdue(binary()) -> ok | {error, term()}.
mark_overdue(InvoiceNo) -> billing_invoice_repo:mark_overdue(InvoiceNo).

%% @doc 列出订阅账单
-spec list_by_subscription(integer()) -> [map()].
list_by_subscription(SubId) ->
    billing_invoice_repo:list_by_subscription(SubId).

%% @doc 分页查询
-spec page(map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(WhereMap, Order, Page, Size) ->
    billing_invoice_repo:page(WhereMap, Order, Page, Size).
