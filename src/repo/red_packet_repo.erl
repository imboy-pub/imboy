-module(red_packet_repo).
%%%===================================================================
%%% @doc 红包仓库层 / Red packet repository
%%%===================================================================

-include("log.hrl").

-export([tablename/0, receive_tablename/0]).
-export([create/6, find_by_id/1, get_receivers/1, find_receive_by_user/2, grab/2]).

%% ===================================================================
%% API Functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"red_packet">>).

-spec receive_tablename() -> binary().
receive_tablename() ->
    elib_pg_sql:public_tablename(<<"red_packet_receive">>).

%% @doc 创建红包
-spec create(integer(), binary(), integer(), integer(), binary(), integer()) ->
    {ok, integer()} | {error, term()}.
create(SenderUid, Type, Amount, Count, Greeting, ExpiresAt) ->
    Tb = tablename(),
    Id = elib_tsid:generate(red_packet),
    Data = #{
        <<"id">> => Id,
        <<"sender_uid">> => SenderUid,
        <<"type">> => Type,
        <<"amount">> => Amount,
        <<"count">> => Count,
        <<"remain_amount">> => Amount,
        <<"remain_count">> => Count,
        <<"greeting">> => Greeting,
        <<"status">> => <<"active">>,
        <<"expires_at">> => elib_dt:to_rfc3339(ExpiresAt)
    },
    {Sql, Params} = elib_pg_sql:insert(Tb, Data),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查询红包信息
-spec find_by_id(integer()) -> map().
find_by_id(Id) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, sender_uid, type, amount, count, remain_amount, remain_count, greeting, status, created_at, expires_at FROM ",
            Tb/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 获取红包领取记录列表
-spec get_receivers(integer()) -> list().
get_receivers(PacketId) ->
    Tb = receive_tablename(),
    Sql =
        <<"SELECT id, red_packet_id, receiver_uid, amount, received_at FROM ", Tb/binary,
            " WHERE red_packet_id = $1 ORDER BY received_at DESC">>,
    case elib_pg:query(Sql, [PacketId]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc 查询指定用户是否已领取该红包
-spec find_receive_by_user(integer(), integer()) -> map().
find_receive_by_user(PacketId, ReceiverUid) ->
    Tb = receive_tablename(),
    Sql =
        <<"SELECT id, red_packet_id, receiver_uid, amount, received_at FROM ", Tb/binary,
            " WHERE red_packet_id = $1 AND receiver_uid = $2 LIMIT 1">>,
    case elib_pg:query(Sql, [PacketId, ReceiverUid]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 抢红包：悲观锁事务
-spec grab(integer(), integer()) -> {ok, integer()} | {rollback, term()} | {error, term()}.
grab(PacketId, ReceiverUid) ->
    Tb = tablename(),
    RecvTb = receive_tablename(),
    WalletTb = elib_pg_sql:public_tablename(<<"wallet">>),
    TxTb = elib_pg_sql:public_tablename(<<"wallet_transaction">>),

    elib_pg:with_tx(fun(Conn) ->
        %% 1. 排他锁锁定红包记录 / Lock red packet row
        LockSql =
            <<"SELECT type, amount, count, remain_amount, remain_count, status FROM ", Tb/binary,
                " WHERE id = $1 FOR UPDATE">>,
        case elib_pg:execute(Conn, LockSql, [PacketId]) of
            {ok, 1, [{Type, _TotalAmount, _TotalCount, RemainAmount, RemainCount, Status}]} ->
                %% 2. 检查状态 / Check status
                case Status =:= <<"active">> andalso RemainCount > 0 of
                    true ->
                        %% 3. 检查是否重复领取 / Check duplicate claim
                        CheckSql =
                            <<"SELECT id FROM ", RecvTb/binary,
                                " WHERE red_packet_id = $1 AND receiver_uid = $2 LIMIT 1">>,
                        case elib_pg:execute(Conn, CheckSql, [PacketId, ReceiverUid]) of
                            {ok, 1, [_]} ->
                                throw({rollback, already_received});
                            _ ->
                                %% 4. 计算领取金额 / Calculate random/fixed amount
                                GrabAmount = calculate_amount(Type, RemainAmount, RemainCount),
                                %% 5. 更新红包状态 / Decrement remain amount & count
                                NewStatus =
                                    case RemainCount =:= 1 of
                                        true -> <<"finished">>;
                                        false -> <<"active">>
                                    end,
                                UpSql =
                                    <<"UPDATE ", Tb/binary,
                                        " SET remain_amount = remain_amount - $1, remain_count = remain_count - 1, status = $2 WHERE id = $3">>,
                                {ok, 1} = elib_pg:execute(Conn, UpSql, [
                                    GrabAmount, NewStatus, PacketId
                                ]),
                                %% 6. 往钱包充钱 / Credit receiver's wallet
                                CreditSql =
                                    <<"UPDATE ", WalletTb/binary,
                                        " SET balance = balance + $1, version = version + 1, updated_at = NOW() WHERE user_id = $2 RETURNING balance, id">>,
                                {ok, 1, [{NewBalance, WalletId}]} = elib_pg:execute(
                                    Conn, CreditSql, [GrabAmount, ReceiverUid]
                                ),
                                %% 7. 写入钱包流水（tx_type=8：领红包） / Add wallet tx (tx_type=8)
                                TxId = elib_tsid:generate(wallet_transaction),
                                RefNo = gen_ref_no(),
                                TxSql =
                                    <<"INSERT INTO ", TxTb/binary,
                                        " (id, wallet_id, user_id, amount, balance_after, tx_type, reference_no, remark, status) VALUES ($1, $2, $3, $4, $5, 8, $6, $7, 1)">>,
                                {ok, 1} = elib_pg:execute(Conn, TxSql, [
                                    TxId,
                                    WalletId,
                                    ReceiverUid,
                                    GrabAmount,
                                    NewBalance,
                                    RefNo,
                                    <<"抢红包"/utf8>>
                                ]),
                                %% 8. 写入红包领取详情 / Add red_packet_receive
                                RecvId = elib_tsid:generate(red_packet_receive),
                                RecvSql =
                                    <<"INSERT INTO ", RecvTb/binary,
                                        " (id, red_packet_id, receiver_uid, amount) VALUES ($1, $2, $3, $4)">>,
                                {ok, 1} = elib_pg:execute(Conn, RecvSql, [
                                    RecvId, PacketId, ReceiverUid, GrabAmount
                                ]),
                                {ok, GrabAmount}
                        end;
                    false ->
                        throw({rollback, red_packet_unavailable})
                end;
            _ ->
                throw({rollback, not_found})
        end
    end).

%% ===================================================================
%% Internal Functions
%% ===================================================================

calculate_amount(<<"fixed">>, RemainAmount, RemainCount) ->
    case RemainCount of
        1 -> RemainAmount;
        _ -> RemainAmount div RemainCount
    end;
calculate_amount(<<"random">>, RemainAmount, RemainCount) ->
    case RemainCount of
        1 ->
            RemainAmount;
        _ ->
            Avg = RemainAmount div RemainCount,
            Max = erlang:min(RemainAmount, Avg * 2),
            case Max =< 2 of
                true -> 1;
                false -> rand:uniform(Max - 1)
            end
    end.

gen_ref_no() ->
    Id = elib_tsid:generate(wallet_transaction),
    <<"RPI_", (integer_to_binary(Id))/binary>>.
