-module(wallet_repo).
%%%
% wallet 钱包数据仓库层，提供钱包及流水的基础数据库操作
%%%

-export([tablename/0]).
-export([find_by_uid/1]).
-export([create/1]).
-export([update_balance/3]).
-export([atomic_balance_change/4]).
-export([tx_tablename/0]).
-export([add_transaction/1]).
-export([find_transaction_by_ref/1]).
-export([page_transactions/3]).

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
    Sql = <<"SELECT id, user_id, balance, frozen, version, status FROM ",
            Tb/binary, " WHERE user_id = $1 AND status = 1 LIMIT 1">>,
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

%% @doc 用乐观锁更新余额
%% @param NewBalance 新余额（分）
%% @param Uid 用户ID
%% @param Version 当前版本号（乐观锁）
%% @return {ok, Count} Count=1表示成功，Count=0表示版本冲突 | {error, Reason}
-spec update_balance(integer(), integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
update_balance(NewBalance, Uid, Version) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
            " SET balance = $1, version = version + 1, updated_at = NOW()"
            " WHERE user_id = $2 AND version = $3">>,
    case elib_pg:execute(Sql, [NewBalance, Uid, Version]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 插入钱包流水记录
%% @param Data 流水数据map，包含 wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark
%% @return {ok, Count} | {error, Reason}
-spec add_transaction(map()) -> {ok, integer()} | {error, term()}.
add_transaction(Data) ->
    Tb = tx_tablename(),
    Id = elib_tsid:generate(wallet_transaction),
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
    Sql = <<"SELECT id, wallet_id, user_id, amount, balance_after, tx_type, reference_no FROM ",
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
    Column = <<"id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status, created_at">>,
    WhereMap = #{user_id => Uid, status => 1},
    Order = <<"id desc">>,
    elib_pg:page_with_total(Tb, Column, WhereMap, Order, Page, Size).

%% @doc 原子性余额变动（事务内完成余额更新+流水写入）
%% @param Amount 变动金额（正=增加，负=扣减）
%% @param Uid 用户ID
%% @param TxData 流水数据 map（不含 balance_after，由事务内计算）
%% @return {ok, NewBalance} | {error, insufficient_balance} | {error, term()}
-spec atomic_balance_change(integer(), integer(), map(), binary()) ->
    {ok, integer()} | {error, term()}.
atomic_balance_change(Amount, Uid, TxData, RefNo) ->
    Tb = tablename(),
    TxTb = tx_tablename(),
    elib_pg:with_tx(fun(Conn) ->
        %% 1. 行锁 + 原子更新余额
        UpdateSql = <<"UPDATE ", Tb/binary,
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
                elib_pg:execute(Conn, TxSql, TxParams),
                {ok, NewBalance};
            {ok, 0} ->
                %% 余额不足或用户不存在
                throw({rollback, insufficient_balance});
            {error, Reason} ->
                throw({rollback, Reason})
        end
    end).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
