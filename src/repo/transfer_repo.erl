-module(transfer_repo).
%%%===================================================================
%%% @doc 转账订单仓库层 / P2P Transfer repository
%%%===================================================================

-include("log.hrl").

-export([tablename/0]).
-export([create/4, find_by_id/1, accept/2, refund/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"transfer_order">>).

%% @doc 创建转账（原子扣减发送者钱包 + 创单）
-spec create(integer(), integer(), integer(), binary()) -> {ok, integer()} | {error, term()}.
create(SenderUid, ReceiverUid, Amount, Remark) ->
    Tb = tablename(),
    WalletTb = elib_pg_sql:public_tablename(<<"wallet">>),
    TxTb = elib_pg_sql:public_tablename(<<"wallet_transaction">>),
    TransferId = elib_tsid:generate(transfer_order),
    RefNo = gen_ref_no(<<"TFO_">>),

    elib_pg:with_tx(fun(Conn) ->
        %% 1. 行锁并扣除发送方钱包余额
        DeductSql =
            <<"UPDATE ", WalletTb/binary,
                " SET balance = balance - $1, version = version + 1, updated_at = NOW() WHERE user_id = $2 AND balance >= $1 RETURNING balance, id">>,
        case elib_pg:execute(Conn, DeductSql, [Amount, SenderUid]) of
            {ok, 1, [[NewBalance, WalletId]]} ->
                %% 2. 写入发送方钱包流水（tx_type=5：转账转出）
                TxSql =
                    <<"INSERT INTO ", TxTb/binary,
                        " (id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status) VALUES ($1, $2, $3, $4, $5, 5, $6, $7, 1)">>,
                TxId = elib_tsid:generate(wallet_transaction),
                {ok, 1} = elib_pg:execute(Conn, TxSql, [
                    TxId, WalletId, SenderUid, -Amount, NewBalance, RefNo, <<"转账给好友"/utf8>>
                ]),

                %% 3. 创建转账订单
                Data = #{
                    <<"id">> => TransferId,
                    <<"sender_uid">> => SenderUid,
                    <<"receiver_uid">> => ReceiverUid,
                    <<"amount">> => Amount,
                    <<"remark">> => Remark,
                    <<"status">> => <<"pending">>
                },
                {Sql, Params} = elib_pg_sql:insert(Tb, Data),
                {ok, _} = elib_pg:execute(Conn, Sql, Params),
                {ok, TransferId};
            {ok, 0} ->
                throw({rollback, insufficient_balance});
            {error, Reason} ->
                throw({rollback, Reason})
        end
    end).

%% @doc 根据ID查询转账订单
-spec find_by_id(integer()) -> map().
find_by_id(Id) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, sender_uid, receiver_uid, amount, remark, status, created_at, completed_at FROM ",
            Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 接收转账（原子更新转账单 + 充值接收方钱包）
-spec accept(integer(), integer()) -> {ok, integer()} | {error, term()}.
accept(TransferId, ReceiverUid) ->
    Tb = tablename(),
    WalletTb = elib_pg_sql:public_tablename(<<"wallet">>),
    TxTb = elib_pg_sql:public_tablename(<<"wallet_transaction">>),
    RefNo = gen_ref_no(<<"TFI_">>),

    elib_pg:with_tx(fun(Conn) ->
        %% 1. 悲观锁定并检查转账订单
        LockSql =
            <<"SELECT sender_uid, receiver_uid, amount, status FROM ", Tb/binary,
                " WHERE id = $1 FOR UPDATE">>,
        case elib_pg:execute(Conn, LockSql, [TransferId]) of
            {ok, 1, [[SenderUid, TargetReceiverUid, Amount, Status]]} ->
                %% 2. 验证合法性
                case Status =:= <<"pending">> andalso TargetReceiverUid =:= ReceiverUid of
                    true ->
                        %% 3. 更新转账状态
                        UpSql =
                            <<"UPDATE ", Tb/binary,
                                " SET status = 'accepted', completed_at = NOW() WHERE id = $1">>,
                        {ok, 1} = elib_pg:execute(Conn, UpSql, [TransferId]),

                        %% 4. 给接收方充值
                        CreditSql =
                            <<"UPDATE ", WalletTb/binary,
                                " SET balance = balance + $1, version = version + 1, updated_at = NOW() WHERE user_id = $2 RETURNING balance, id">>,
                        {ok, 1, [[NewBalance, WalletId]]} = elib_pg:execute(Conn, CreditSql, [
                            Amount, ReceiverUid
                        ]),

                        %% 5. 写入接收方钱包流水（tx_type=6：转账转入）
                        TxSql =
                            <<"INSERT INTO ", TxTb/binary,
                                " (id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status) VALUES ($1, $2, $3, $4, $5, 6, $6, $7, 1)">>,
                        TxId = elib_tsid:generate(wallet_transaction),
                        {ok, 1} = elib_pg:execute(Conn, TxSql, [
                            TxId, WalletId, ReceiverUid, Amount, NewBalance, RefNo, <<"收取转账"/utf8>>
                        ]),
                        {ok, Amount};
                    false ->
                        throw({rollback, invalid_status})
                end;
            _ ->
                throw({rollback, not_found})
        end
    end).

%% @doc 退回转账（逾期退回 / 拒收退回）
-spec refund(integer()) -> {ok, integer()} | {error, term()}.
refund(TransferId) ->
    Tb = tablename(),
    WalletTb = elib_pg_sql:public_tablename(<<"wallet">>),
    TxTb = elib_pg_sql:public_tablename(<<"wallet_transaction">>),
    RefNo = gen_ref_no(<<"TFR_">>),

    elib_pg:with_tx(fun(Conn) ->
        %% 1. 锁定订单
        LockSql =
            <<"SELECT sender_uid, amount, status FROM ", Tb/binary, " WHERE id = $1 FOR UPDATE">>,
        case elib_pg:execute(Conn, LockSql, [TransferId]) of
            {ok, 1, [[SenderUid, Amount, Status]]} ->
                case Status =:= <<"pending">> of
                    true ->
                        %% 2. 更新状态
                        UpSql =
                            <<"UPDATE ", Tb/binary,
                                " SET status = 'refunded', completed_at = NOW() WHERE id = $1">>,
                        {ok, 1} = elib_pg:execute(Conn, UpSql, [TransferId]),

                        %% 3. 退还资金给发送者
                        RefundSql =
                            <<"UPDATE ", WalletTb/binary,
                                " SET balance = balance + $1, version = version + 1, updated_at = NOW() WHERE user_id = $2 RETURNING balance, id">>,
                        {ok, 1, [[NewBalance, WalletId]]} = elib_pg:execute(Conn, RefundSql, [
                            Amount, SenderUid
                        ]),

                        %% 4. 写入发送者钱包退款流水（tx_type=9：红包/转账退回）
                        TxSql =
                            <<"INSERT INTO ", TxTb/binary,
                                " (id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status) VALUES ($1, $2, $3, $4, $5, 9, $6, $7, 1)">>,
                        TxId = elib_tsid:generate(wallet_transaction),
                        {ok, 1} = elib_pg:execute(Conn, TxSql, [
                            TxId, WalletId, SenderUid, Amount, NewBalance, RefNo, <<"转账退回"/utf8>>
                        ]),
                        {ok, Amount};
                    false ->
                        throw({rollback, invalid_status})
                end;
            _ ->
                throw({rollback, not_found})
        end
    end).

%% ===================================================================
%% Internal Functions
%% ===================================================================

gen_ref_no(Prefix) ->
    Id = elib_tsid:generate(wallet_transaction),
    <<Prefix/binary, (integer_to_binary(Id))/binary>>.
