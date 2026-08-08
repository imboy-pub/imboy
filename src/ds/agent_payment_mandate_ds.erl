-module(agent_payment_mandate_ds).

%%%
% Agent 受控支付授权数据服务 / Agent payment mandate data service
% 薄封装 agent_payment_mandate_repo，供 agent_payment_logic 编排三道闸门。
% 屏蔽 repo 存储细节；额度的原子预留/释放语义集中在 repo 的单条 SQL。
%%%

-export([create/1]).
-export([get/1]).
-export([find_active/1]).
-export([try_reserve/2]).
-export([try_reserve_with_compensation/3]).
-export([revoke/1]).

%% @doc 创建授权（见 repo:create/1 的 Data 键说明）
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data) ->
    agent_payment_mandate_repo:create(Data).

%% @doc 按 id 读取授权
-spec get(integer()) -> {ok, map()} | {error, notfound | term()}.
get(Id) ->
    agent_payment_mandate_repo:find(Id).

%% @doc 闸门①：取 agent 当前有效授权（未过期 + status=1）
-spec find_active(integer()) -> {ok, map()} | {error, notfound | term()}.
find_active(AgentUid) ->
    agent_payment_mandate_repo:find_active(AgentUid).

%% @doc 闸门②：原子预留周期额度（超上限返回 exceeds_total_limit，绝不扣款）
-spec try_reserve(integer(), integer()) ->
    {ok, integer()} | {error, exceeds_total_limit | term()}.
try_reserve(MandateId, AmountFen) ->
    agent_payment_mandate_repo:reserve(MandateId, AmountFen).

%% @doc 原子预留周期额度并创建失败补偿 outbox。
-spec try_reserve_with_compensation(integer(), integer(), binary()) ->
    {ok, integer(), integer()} | {error, exceeds_total_limit | term()}.
try_reserve_with_compensation(MandateId, AmountFen, RefNo) ->
    agent_payment_mandate_repo:reserve_with_compensation(MandateId, AmountFen, RefNo).

%% @doc 撤销授权
-spec revoke(integer()) -> {ok, non_neg_integer()} | {error, term()}.
revoke(Id) ->
    agent_payment_mandate_repo:set_status(Id, 0).
