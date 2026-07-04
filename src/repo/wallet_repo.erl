-module(wallet_repo).
%%%
% wallet 钱包数据仓库层，提供钱包及流水的基础数据库操作
%%%

-export([find_by_uid/1]).
-export([create/1]).
-export([atomic_balance_change/4]).
-export([find_transaction_by_ref/1]).
-export([page_transactions/3]).
-export([page/3]).
-export([page_withdrawals/3]).
-export([update_tx_status/2]).
-export([reject_and_refund/1]).
-export([freeze/2, unfreeze/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取钱包表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"wallet">>).

%% @doc 获取钱包流水表名
-spec tx_tablename() -> binary().
tx_tablename() ->
    elib_pg_sql:public_tablename(<<"wallet_transaction">>).

%% @doc 根据用户ID查询钱包信息
%% @param Uid 用户ID（整数）
%% @return map | #{} 不存在时返回空map
-spec find_by_uid(integer()) -> map().
find_by_uid(Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, user_id, balance, frozen, version, status FROM ", Tb/binary,
            " WHERE user_id = $1 AND status = 1 LIMIT 1">>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 创建钱包记录
%% @param Data 包含 user_id 的map
%% @return {ok, Count} | {error, Reason}
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(wallet),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 根据流水单号查询流水记录（用于退款查询原始支付）
%% @param RefNo 流水参考单号（如 WPY_xxxxx）
%% @return map | #{} 不存在时返回空map
-spec find_transaction_by_ref(binary()) -> map().
find_transaction_by_ref(RefNo) ->
    Tb = tx_tablename(),
    Sql =
        <<"SELECT id, wallet_id, user_id, amount, balance_after, tx_type, reference_no FROM ",
            Tb/binary, " WHERE reference_no = $1 AND status = 1 LIMIT 1">>,
    case elib_pg:query(Sql, [RefNo]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 分页查询用户流水记录
%% @param Page 页码（从1开始）
%% @param Size 每页条数
%% @param Uid 用户ID
%% @return {ok, Payload} Payload包含 list, page, size, total, total_page
-spec page_transactions(integer(), integer(), integer()) -> {ok, map()}.
page_transactions(Page, Size, Uid) ->
    Tb = tx_tablename(),
    Column =
        <<"id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status, created_at">>,
    WhereMap = #{user_id => Uid, status => 1},
    Order = <<"id desc">>,
    elib_pg:page_with_total(Tb, Column, WhereMap, Order, Page, Size).

%% @doc 跨用户钱包分页查询（运营后台用）
%% @param WhereMap 等值筛选条件（如 #{user_id => Uid, status => Status}），由调用方组装
%% @param Page 页码（从1开始）
%% @param Size 每页条数
%% @return {ok, Payload} Payload 含 list/total/page/size | {error, Reason}
-spec page(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page(WhereMap, Page, Size) ->
    Tb = tablename(),
    Column = <<"id, user_id, balance, frozen, version, status, created_at, updated_at">>,
    elib_pg:page_with_total(Tb, Column, WhereMap, <<"id desc">>, Page, Size).

%% @doc 原子性余额变动（事务内完成余额更新+流水写入）
%% @param Amount 变动金额（正=增加，负=扣减）
%% @param Uid 用户ID
%% @param TxData 流水数据 map（不含 balance_after，由事务内计算）
%% @return {ok, NewBalance} | {rollback, insufficient_balance} | {error, term()}
-spec atomic_balance_change(integer(), integer(), map(), binary()) ->
    {ok, integer()} | {rollback, term()} | {error, term()}.
atomic_balance_change(Amount, Uid, TxData, RefNo) ->
    Tb = tablename(),
    TxTb = tx_tablename(),
    elib_pg:with_tx(fun(Conn) ->
        %% 1. 行锁 + 原子更新余额
        UpdateSql =
            <<"UPDATE ", Tb/binary,
                " SET balance = balance + $1, version = version + 1, updated_at = NOW()"
                " WHERE user_id = $2 AND balance + $1 >= 0"
                " RETURNING balance">>,
        case elib_pg:execute(Conn, UpdateSql, [Amount, Uid]) of
            {ok, 1, [{NewBalance}]} ->
                %% 2. 在同一事务内写入流水
                TxData2 = TxData#{
                    <<"balance_after">> => NewBalance,
                    <<"reference_no">> => RefNo
                },
                TxId = elib_tsid:generate(wallet_transaction),
                TxData3 = TxData2#{<<"id">> => TxId},
                {TxSql, TxParams} = elib_pg_sql:insert(TxTb, TxData3),
                case elib_pg:execute(Conn, TxSql, TxParams) of
                    {ok, _} -> ok;
                    {error, TxReason} -> throw({rollback, TxReason})
                end,
                {ok, NewBalance};
            {ok, 0} ->
                %% 余额不足或用户不存在
                throw({rollback, insufficient_balance});
            {error, Reason} ->
                throw({rollback, Reason})
        end
    end).

%% @doc 提现流水分页（运营后台跨用户，固定 tx_type=10）
-spec page_withdrawals(map(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page_withdrawals(WhereMap, Page, Size) ->
    Tb = tx_tablename(),
    Column =
        <<"id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status, created_at">>,
    Where2 = WhereMap#{tx_type => 10},
    elib_pg:page_with_total(Tb, Column, Where2, <<"id desc">>, Page, Size).

%% @doc 更新提现流水状态（仅操作 tx_type=10 且 status=0 的待处理记录）
%% Status: 1=完成 2=拒绝
-spec update_tx_status(integer(), 1 | 2) -> {ok, non_neg_integer()} | {error, term()}.
update_tx_status(TxId, Status) ->
    Tb = tx_tablename(),
    elib_pg:update(Tb, #{<<"status">> => Status}, <<"id = $1 AND tx_type = 10 AND status = 0">>, [
        TxId
    ]).

%% @doc 原子拒绝提现并退款：status=2 + 退还余额 + 写退款流水（tx_type=11）
%% 返回 {ok,1} 成功，{ok,0} 记录不存在或已处理，{error,Reason} DB 错误
-spec reject_and_refund(integer()) -> {ok, 0 | 1} | {error, term()}.
reject_and_refund(TxId) ->
    Tb = tablename(),
    TxTb = tx_tablename(),
    elib_pg:with_tx(fun(Conn) ->
        MarkSql =
            <<"UPDATE ", TxTb/binary,
                " SET status = 2 WHERE id = $1 AND tx_type = 10 AND status = 0"
                " RETURNING amount, user_id, wallet_id">>,
        case elib_pg:execute(Conn, MarkSql, [TxId]) of
            {ok, 0} ->
                {ok, 0};
            {ok, 1, [{Amount, Uid, WalletId}]} ->
                Refund = erlang:abs(Amount),
                BalSql =
                    <<"UPDATE ", Tb/binary,
                        " SET balance = balance + $1, version = version + 1, updated_at = NOW()"
                        " WHERE user_id = $2"
                        " RETURNING balance">>,
                case elib_pg:execute(Conn, BalSql, [Refund, Uid]) of
                    {ok, 1, [{NewBalance}]} ->
                        RefNo = <<"WRF_", (integer_to_binary(TxId))/binary>>,
                        RefundTxId = elib_tsid:generate(wallet_transaction),
                        {TxSql, TxParams} = elib_pg_sql:insert(TxTb, #{
                            <<"id">> => RefundTxId,
                            <<"wallet_id">> => WalletId,
                            <<"user_id">> => Uid,
                            <<"amount">> => Refund,
                            <<"balance_after">> => NewBalance,
                            <<"tx_type">> => 11,
                            <<"reference_no">> => RefNo,
                            <<"remark">> => <<"提现拒绝退款"/utf8>>,
                            <<"status">> => 1
                        }),
                        case elib_pg:execute(Conn, TxSql, TxParams) of
                            {ok, _} -> {ok, 1};
                            {error, Reason2} -> throw({rollback, Reason2})
                        end;
                    {error, Reason} ->
                        throw({rollback, Reason})
                end;
            {error, Reason} ->
                throw({rollback, Reason})
        end
    end).

%% @doc 冻结钱包资金：把 Amount（分）从可用余额移入 frozen（冻结额）。
%% 单行原子 UPDATE（PG 语句级原子，无需显式事务），version+1 体现乐观锁纪律。
%% 不变量守卫：status=1(正常) 且 `balance - frozen >= Amount`（仅可冻结可用余额），
%% 配合表级 CHECK(frozen>=0, balance>=0) 保证 0 <= frozen <= balance 恒成立。
%% 与 unfreeze/2 对称可逆。frozen 只是「持有标记」，不写 wallet_transaction 流水。
%% @returns {ok, 1} 成功 | {ok, 0} 钱包不存在/已停用/可用余额不足 | {error, term()}
-spec freeze(integer(), integer()) -> {ok, 0 | 1} | {error, term()}.
freeze(Uid, Amount) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET frozen = frozen + $1, version = version + 1, updated_at = NOW()"
            " WHERE user_id = $2 AND status = 1 AND balance - frozen >= $1">>,
    case elib_pg:execute(Sql, [Amount, Uid]) of
        {ok, N} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 解冻钱包资金：把 Amount（分）从 frozen 移回可用余额（freeze/2 的逆操作）。
%% 单行原子 UPDATE + version+1。守卫：status=1 且 `frozen >= Amount`（不能解冻超过已冻结额）。
%% @returns {ok, 1} 成功 | {ok, 0} 钱包不存在/已停用/冻结额不足 | {error, term()}
-spec unfreeze(integer(), integer()) -> {ok, 0 | 1} | {error, term()}.
unfreeze(Uid, Amount) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET frozen = frozen - $1, version = version + 1, updated_at = NOW()"
            " WHERE user_id = $2 AND status = 1 AND frozen >= $1">>,
    case elib_pg:execute(Sql, [Amount, Uid]) of
        {ok, N} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
