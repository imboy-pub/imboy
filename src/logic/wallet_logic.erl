-module(wallet_logic).

%%%
% 钱包业务逻辑层 / Wallet business logic layer
%
% 职责：作为 wallet_handler 与 wallet_ds 之间的标准中转层，
%       遵循 Handler→Logic→DS→Repo 四层架构。
%
% This module is the standard logic layer between wallet_handler and wallet_ds,
% enforcing the Handler→Logic→DS→Repo single-direction dependency rule.
%%%

-export([find_by_uid/1, ensure_wallet/1, page_transactions/3, topup/3]).

-include("log.hrl").
-include("common.hrl").

%% @doc 查询钱包信息（不自动创建）
-spec find_by_uid(integer()) -> map().
find_by_uid(Uid) ->
    wallet_ds:find_by_uid(Uid).

%% @doc 确保钱包存在；若不存在则自动创建并返回新建后的记录
-spec ensure_wallet(integer()) -> map().
ensure_wallet(Uid) ->
    wallet_ds:ensure_wallet(Uid).

%% @doc 分页查询流水
-spec page_transactions(pos_integer(), pos_integer(), integer()) ->
    {ok, map()} | {error, term()}.
page_transactions(Page, Size, Uid) ->
    wallet_ds:page_transactions(Page, Size, Uid).

%% @doc 充值：原子事务（余额更新 + 流水写入）
%% 调用方需自行做金额合法性校验
%% @returns {ok, NewBalance} | {rollback, Reason} | {error, Reason}
-spec topup(integer(), integer(), binary()) ->
    {ok, integer()} | {rollback, term()} | {error, term()}.
topup(Uid, Amount, RefNo) ->
    wallet_ds:topup(Uid, Amount, RefNo).
