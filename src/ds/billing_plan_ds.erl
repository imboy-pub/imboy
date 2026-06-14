-module(billing_plan_ds).
%%%
% billing_plan_ds — 套餐数据服务层 / SaaS plan data service layer
%
% 职责：薄封装 billing_plan_repo，遵循 Handler→Logic→DS→Repo 单向架构。
%       billing_logic 不直调 billing_plan_repo。
%%%

-include("log.hrl").

-export([create/1]).
-export([find_by_id/1]).
-export([find_by_code/1]).
-export([update/2]).
-export([list_active/0]).
-export([page/4]).

%% @doc 创建套餐
-spec create(map()) -> {ok, integer()} | {error, duplicate | term()}.
create(Data) -> billing_plan_repo:create(Data).

%% @doc 按 id 查询套餐
-spec find_by_id(integer()) -> map().
find_by_id(Id) -> billing_plan_repo:find_by_id(Id).

%% @doc 按 code 查询套餐
-spec find_by_code(binary()) -> map().
find_by_code(Code) -> billing_plan_repo:find_by_code(Code).

%% @doc 更新套餐
-spec update(integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Id, ChangeMap) -> billing_plan_repo:update(Id, ChangeMap).

%% @doc 列出上架套餐
-spec list_active() -> [map()].
list_active() -> billing_plan_repo:list_active().

%% @doc 分页查询
-spec page(map(), iodata(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(WhereMap, Order, Page, Size) ->
    billing_plan_repo:page(WhereMap, Order, Page, Size).
