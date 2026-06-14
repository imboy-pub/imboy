-module(billing_usage_ds).
%%%
% billing_usage_ds — 用量数据服务层 / SaaS usage data service
%
% 职责：薄封装 billing_usage_repo，遵循 Handler→Logic→DS→Repo 单向架构。
%%%

-include("log.hrl").

-export([incr/4]).
-export([get_used/3]).
-export([list_by_period/2]).

%% @doc 累加用量（upsert），返回累加后总量
-spec incr(integer(), binary(), binary(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
incr(SubId, Metric, Period, Delta) ->
    billing_usage_repo:incr(SubId, Metric, Period, Delta).

%% @doc 查询已用量
-spec get_used(integer(), binary(), binary()) -> non_neg_integer().
get_used(SubId, Metric, Period) ->
    billing_usage_repo:get_used(SubId, Metric, Period).

%% @doc 列出某周期全部指标用量
-spec list_by_period(integer(), binary()) -> [map()].
list_by_period(SubId, Period) ->
    billing_usage_repo:list_by_period(SubId, Period).
