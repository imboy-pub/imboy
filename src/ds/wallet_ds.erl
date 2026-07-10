-module(wallet_ds).

%%%
% wallet_ds — 钱包数据服务层 / Wallet data service layer
%
% 职责：封装 wallet_repo 的访问 + 自动建钱包 / 充值的业务编排，
%       让 wallet_handler 回归纯 HTTP 入口职责。
%
% 设计动机 / Why this layer:
% - 原 wallet_handler 直接调 wallet_repo:find_by_uid/create/atomic_balance_change/page_transactions
%   共 7 处，违反 Handler→Logic→DS→Repo 单向架构（详见 G2-a 治理）
% - 业务规则（首次访问自动创建钱包、充值的事务封装）属于"数据服务"语义，
%   既不上升到 Logic（无复杂跨域编排），也不应散落在 Handler
%
% Per the four-layer architecture, handlers should not call repo directly.
% This module consolidates wallet data services in 2026-04 (G2-a remediation).
%%%

-export([find_by_uid/1, ensure_wallet/1, page_transactions/3]).
-export([page/3]).
-export([create/1]).
-export([topup/3]).
%% G3 治理：payment_wallet_gateway 不再直调 repo / G3 remediation
-export([atomic_balance_change/4]).
%% 原子两腿结算（借记付款方 + 贷记收款方，单事务无资金丢失窗口）
-export([atomic_transfer/2]).
%% 幂等查询：payment_wallet_gateway 用 reference_no 防止重复入账
-export([find_transaction_by_ref/1]).
%% 运营后台：提现流水分页 + 状态更新
-export([page_withdrawals/3, update_tx_status/2, reject_and_refund/1]).
%% 运营后台：钱包冻结/解冻
-export([freeze/2, unfreeze/2]).

-include("log.hrl").

%% @doc 查询钱包信息（不自动创建）
-spec find_by_uid(integer()) -> map().
find_by_uid(Uid) ->
    wallet_repo:find_by_uid(Uid).

%% @doc 确保钱包存在；若不存在则自动创建并返回新建后的记录
%% Idempotent: returns the existing wallet, or creates one and returns it.
-spec ensure_wallet(integer()) -> map().
ensure_wallet(Uid) ->
    Wallet = wallet_repo:find_by_uid(Uid),
    case map_size(Wallet) =:= 0 of
        true ->
            case wallet_repo:create(#{user_id => Uid}) of
                {ok, _} ->
                    wallet_repo:find_by_uid(Uid);
                {error, Reason} ->
                    ?WARN_LOG(
                        "wallet_ds:ensure_wallet create error uid=~p reason=~p",
                        [Uid, Reason]
                    ),
                    %% 极端情况下创建失败仍返回空 map，由调用方判断
                    %% On rare creation failure, return empty map; caller decides
                    #{}
            end;
        false ->
            Wallet
    end.

%% @doc 分页查询流水
-spec page_transactions(pos_integer(), pos_integer(), integer()) ->
    {ok, map()} | {error, term()}.
page_transactions(Page, Size, Uid) ->
    wallet_repo:page_transactions(Page, Size, Uid).

%% @doc 跨用户钱包分页查询（运营后台用，薄封装）
-spec page(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page(WhereMap, Page, Size) ->
    wallet_repo:page(WhereMap, Page, Size).

%% @doc 充值：原子事务（余额更新 + 流水写入）
%% 调用方需自行做参数校验（金额范围等 HTTP 输入校验）
%% Caller is responsible for HTTP-level input validation (amount range, etc.)
%%
%% @returns {ok, NewBalance} | {rollback, Reason} | {error, Reason}
-spec topup(integer(), integer(), binary()) ->
    {ok, integer()} | {rollback, term()} | {error, term()}.
topup(Uid, Amount, RefNo) ->
    Wallet = ensure_wallet(Uid),
    case map_size(Wallet) =:= 0 of
        true ->
            {error, wallet_unavailable};
        false ->
            WalletId = maps:get(<<"id">>, Wallet),
            TxData = #{
                <<"wallet_id">> => WalletId,
                <<"user_id">> => Uid,
                <<"amount">> => Amount,
                <<"tx_type">> => 1,
                <<"remark">> => <<"充值"/utf8>>,
                <<"status">> => 1
            },
            wallet_repo:atomic_balance_change(Amount, Uid, TxData, RefNo)
    end.

%% @doc 原子余额变更（薄封装）/ Atomic balance change (thin pass-through).
%% G3 治理：payment_wallet_gateway 不再直调 repo / G3 remediation.
-spec atomic_balance_change(integer(), integer(), map(), binary()) ->
    {ok, integer()} | {rollback, term()} | {error, term()}.
atomic_balance_change(Amount, Uid, TxData, RefNo) ->
    wallet_repo:atomic_balance_change(Amount, Uid, TxData, RefNo).

%% @doc 原子两腿结算（薄封装）：同一事务借记付款方 + 贷记收款方 + 双流水。
%% Debit/Credit map：#{user_id, wallet_id, amount(正整数分), tx_type, remark, reference_no}
-spec atomic_transfer(map(), map()) ->
    {ok, #{debit_balance := integer(), credit_balance := integer()}}
    | {rollback, term()}
    | {error, term()}.
atomic_transfer(Debit, Credit) ->
    wallet_repo:atomic_transfer(Debit, Credit).

%% @doc 按外部业务号查询流水（幂等性保护）/ Find transaction by external ref (idempotency).
%% G3 治理：payment_wallet_gateway 不再直调 repo / G3 remediation.
-spec find_transaction_by_ref(binary()) -> map() | undefined.
find_transaction_by_ref(RefNo) ->
    wallet_repo:find_transaction_by_ref(RefNo).

%% G3: wallet_handler 不应直调 wallet_repo:create
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data) -> wallet_repo:create(Data).

%% @doc 提现流水分页（运营后台，跨用户，固定 tx_type=10）
-spec page_withdrawals(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page_withdrawals(WhereMap, Page, Size) ->
    wallet_repo:page_withdrawals(WhereMap, Page, Size).

%% @doc 更新提现流水状态（1=完成，2=拒绝；仅操作 status=0 的待处理记录）
-spec update_tx_status(integer(), 1 | 2) -> {ok, non_neg_integer()} | {error, term()}.
update_tx_status(TxId, Status) ->
    wallet_repo:update_tx_status(TxId, Status).

-spec reject_and_refund(integer()) -> {ok, 0 | 1} | {error, term()}.
reject_and_refund(TxId) ->
    wallet_repo:reject_and_refund(TxId).

%% @doc 冻结钱包资金（可用余额 -> frozen；单行原子 UPDATE，薄封装）
-spec freeze(integer(), integer()) -> {ok, 0 | 1} | {error, term()}.
freeze(Uid, Amount) ->
    wallet_repo:freeze(Uid, Amount).

%% @doc 解冻钱包资金（frozen -> 可用余额；freeze/2 逆操作，薄封装）
-spec unfreeze(integer(), integer()) -> {ok, 0 | 1} | {error, term()}.
unfreeze(Uid, Amount) ->
    wallet_repo:unfreeze(Uid, Amount).
