-module(billing_subscription_ds).
%%%
% billing_subscription_ds — 订阅数据服务层 / SaaS subscription data service
%
% 职责：薄封装 billing_subscription_repo，遵循 Handler→Logic→DS→Repo 单向架构。
%%%

-include("log.hrl").

-export([create/1]).
-export([find_by_id/1]).
-export([find_active_by_tenant/1]).
-export([update/2]).
-export([renew/3]).
-export([list_expiring/1]).
-export([page/4]).

%% @doc 创建订阅
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) -> billing_subscription_repo:create(Data).

%% @doc 按 id 查询订阅
-spec find_by_id(integer()) -> map().
find_by_id(Id) -> billing_subscription_repo:find_by_id(Id).

%% @doc 查询租户当前生效/试用订阅
-spec find_active_by_tenant(integer()) -> map().
find_active_by_tenant(TenantId) ->
    billing_subscription_repo:find_active_by_tenant(TenantId).

%% @doc 更新订阅
-spec update(integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Id, ChangeMap) -> billing_subscription_repo:update(Id, ChangeMap).

%% @doc 续费（推进周期 + 置生效）
-spec renew(integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
renew(Id, PeriodStart, PeriodEnd) ->
    billing_subscription_repo:renew(Id, PeriodStart, PeriodEnd).

%% @doc 列出到期订阅
-spec list_expiring(integer()) -> [map()].
list_expiring(BeforeTs) -> billing_subscription_repo:list_expiring(BeforeTs).

%% @doc 分页查询
-spec page(map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(WhereMap, Order, Page, Size) ->
    billing_subscription_repo:page(WhereMap, Order, Page, Size).
