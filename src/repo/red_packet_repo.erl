-module(red_packet_repo).
%%%===================================================================
%%% @doc 红包仓库层 / Red packet repository
%%%===================================================================

-include("log.hrl").

-export([tablename/0, receive_tablename/0]).
-export([create/6, create/7, find_by_id/1, get_receivers/1, find_receive_by_user/2, grab/2]).
-export([list_expired_active/1, expire_and_refund/1]).

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
    create(SenderUid, Type, Amount, Count, Greeting, ExpiresAt, #{}).

%% @doc 创建红包并绑定会话作用域（B-11）。
%% Scope :: #{scope_type => binary(), scope_id => integer()}；空 map 表示不绑定
%% （旧客户端），此时 open/2 沿用旧的"任何人凭 id 可领"行为。
-spec create(integer(), binary(), integer(), integer(), binary(), integer(), map()) ->
    {ok, integer()} | {error, term()}.
create(SenderUid, Type, Amount, Count, Greeting, ExpiresAt, Scope) ->
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
    Data2 = apply_scope(Data, Scope),
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查询红包信息
-spec find_by_id(integer()) -> map().
find_by_id(Id) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, sender_uid, type, amount, count, remain_amount, remain_count, greeting,"
            " status, created_at, expires_at, scope_type, scope_id FROM ",
            Tb/binary,
            " WHERE id = $1 LIMIT 1"
        >>,
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
        %% expires_at 一起取出：此前只看 status，**过期红包照样能抢**（错误文案写着
        %% "已过期"但代码里从来没实现过）。B-10 的过期退款会把 remain_amount 退回
        %% 发送者，若这里不挡，同一笔钱既退回又被领走。
        LockSql =
            <<"SELECT type, amount, count, remain_amount, remain_count, status,",
                " (expires_at > NOW()) AS alive FROM ", Tb/binary, " WHERE id = $1 FOR UPDATE">>,
        %% ⚠️ 事务内 SELECT 必须用 query/3（equery）：epgsql 的 execute_batch 对
        %% SELECT 只返回 {ok, Rows}（decode_complete 把 SELECT tag 解码成原子 select），
        %% 三元组 {ok, Count, Rows} 匹配必败 → 曾致抢红包永远报「红包不存在」。
        case elib_pg:query(Conn, LockSql, [PacketId]) of
            {ok, [
                #{
                    <<"type">> := Type,
                    <<"amount">> := _TotalAmount,
                    <<"count">> := _TotalCount,
                    <<"remain_amount">> := RemainAmount,
                    <<"remain_count">> := RemainCount,
                    <<"status">> := Status,
                    <<"alive">> := Alive
                }
            ]} ->
                %% 2. 检查状态 / Check status
                case Status =:= <<"active">> andalso RemainCount > 0 andalso Alive =:= true of
                    true ->
                        %% 3. 检查是否重复领取 / Check duplicate claim
                        CheckSql =
                            <<"SELECT id FROM ", RecvTb/binary,
                                " WHERE red_packet_id = $1 AND receiver_uid = $2 LIMIT 1">>,
                        case elib_pg:query(Conn, CheckSql, [PacketId, ReceiverUid]) of
                            {ok, [_]} ->
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

%% @doc B-10：列出已过期但仍是 active 且有余额的红包（待退回发送者）。
%% remain_amount = 0 的不返回：没钱可退，只是状态没收尾，不值得单独跑一趟事务。
-spec list_expired_active(pos_integer()) -> [map()].
list_expired_active(Limit) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, sender_uid, remain_amount FROM ", Tb/binary,
            " WHERE status = 'active' AND expires_at <= NOW() AND remain_amount > 0",
            " ORDER BY expires_at ASC LIMIT $1">>,
    case elib_pg:query(Sql, [Limit]) of
        {ok, Rows} -> Rows;
        _ -> []
    end.

%% @doc B-10：单个红包过期退款（一个事务内完成，幂等）。
%%
%% 顺序刻意与 grab/2 一致：**先对红包行加排他锁再动钱**。否则"退款"与"最后一次抢"
%% 会并发跑，同一笔 remain_amount 既退给发送者又发给领取者。
%%
%% 幂等由两层保证：
%%   - status = 'active' 的 CAS（第二次跑命中 0 行）
%%   - wallet_transaction.reference_no UNIQUE，refno 固定为 RPE_<PacketId>
%% @returns {ok, RefundAmount} | {rollback, term()} | {error, term()}
-spec expire_and_refund(integer()) -> {ok, integer()} | {rollback, term()} | {error, term()}.
expire_and_refund(PacketId) ->
    Tb = tablename(),
    WalletTb = elib_pg_sql:public_tablename(<<"wallet">>),
    TxTb = elib_pg_sql:public_tablename(<<"wallet_transaction">>),
    elib_pg:with_tx(fun(Conn) ->
        LockSql =
            <<"SELECT sender_uid, remain_amount, status FROM ", Tb/binary,
                " WHERE id = $1 AND expires_at <= NOW() FOR UPDATE">>,
        case elib_pg:query(Conn, LockSql, [PacketId]) of
            {ok, [
                #{
                    <<"sender_uid">> := SenderUid,
                    <<"remain_amount">> := RemainAmount,
                    <<"status">> := <<"active">>
                }
            ]} when RemainAmount > 0 ->
                do_expire_refund(Conn, Tb, WalletTb, TxTb, PacketId, SenderUid, RemainAmount);
            {ok, [#{<<"status">> := <<"active">>}]} ->
                %% 余额已为 0：只收尾状态，不动钱
                ExpSql = <<"UPDATE ", Tb/binary, " SET status = 'expired' WHERE id = $1">>,
                {ok, 1} = elib_pg:execute(Conn, ExpSql, [PacketId]),
                {ok, 0};
            {ok, [_]} ->
                %% 已被并发处理成 expired/finished
                throw({rollback, already_settled});
            _ ->
                throw({rollback, not_found})
        end
    end).

-spec do_expire_refund(
    term(), binary(), binary(), binary(), integer(), integer(), integer()
) -> {ok, integer()}.
do_expire_refund(Conn, Tb, WalletTb, TxTb, PacketId, SenderUid, RemainAmount) ->
    ExpSql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'expired', remain_amount = 0 WHERE id = $1 AND status = 'active'">>,
    {ok, 1} = elib_pg:execute(Conn, ExpSql, [PacketId]),
    CreditSql =
        <<"UPDATE ", WalletTb/binary,
            " SET balance = balance + $1, version = version + 1, updated_at = NOW()",
            " WHERE user_id = $2 RETURNING balance, id">>,
    case elib_pg:execute(Conn, CreditSql, [RemainAmount, SenderUid]) of
        {ok, 1, [{NewBalance, WalletId}]} ->
            TxId = elib_tsid:generate(wallet_transaction),
            %% refno 由红包 id 决定（不是随机）：UNIQUE 约束因此成为第二道幂等闸门
            RefNo = <<"RPE_", (integer_to_binary(PacketId))/binary>>,
            TxSql =
                <<"INSERT INTO ", TxTb/binary,
                    " (id, wallet_id, user_id, amount, balance_after, tx_type, reference_no,"
                    " remark, status) VALUES ($1, $2, $3, $4, $5, 9, $6, $7, 1)">>,
            {ok, 1} = elib_pg:execute(Conn, TxSql, [
                TxId,
                WalletId,
                SenderUid,
                RemainAmount,
                NewBalance,
                RefNo,
                <<"红包过期退回"/utf8>>
            ]),
            {ok, RemainAmount};
        _ ->
            %% 发送者钱包不存在：整个事务回滚，红包保持 active 等下一轮
            throw({rollback, sender_wallet_not_found})
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 作用域两列同进同退 —— 迁移 00000056 的 chk_red_packet_scope_pair 也是这么约束的，
%% 只写一半会被 PG 直接拒掉。
-spec apply_scope(map(), map()) -> map().
apply_scope(Data, Scope) ->
    ScopeType = maps:get(scope_type, Scope, undefined),
    ScopeId = maps:get(scope_id, Scope, undefined),
    case is_binary(ScopeType) andalso is_integer(ScopeId) andalso ScopeId > 0 of
        true -> Data#{<<"scope_type">> => ScopeType, <<"scope_id">> => ScopeId};
        false -> Data
    end.

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
