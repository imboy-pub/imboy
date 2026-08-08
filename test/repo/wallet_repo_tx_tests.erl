-module(wallet_repo_tx_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc wallet_repo 钱路径事务测试：atomic_balance_change/4 与 reject_and_refund/1
%%% 钱路径必须有回归保护：余额不足拒绝、原子入账、拒绝退款幂等(no-op)、退款成功。
%%% with_tx 测试替身复刻真实事务语义：事务体抛 {rollback,R} → 整体 {error,R}。

%% with_tx 测试替身：执行事务体，按真实语义把 rollback throw 转成 {error,_}
with_tx_stub() ->
    {'with_tx', 1, fun(F) ->
        try
            F(mock_conn)
        catch
            throw:{rollback, Reason} -> {error, Reason}
        end
    end}.

%% tablename/insert 替身：避免依赖真实 schema 配置与 TSID 生成
sql_stub() ->
    {elib_pg_sql, [
        {'public_tablename', 1, fun(T) -> T end},
        {'insert', 2, fun(_T, _D) -> {<<"INSERT INTO wallet_transaction">>, []} end}
    ]}.

tsid_stub() ->
    {elib_tsid, [{'generate', 1, fun(_) -> 123456 end}]}.

%% atomic_balance_change：余额不足时 UPDATE 影响 0 行 → 事务回滚 {error, insufficient_balance}
atomic_balance_change_insufficient_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [with_tx_stub(), {'execute', 3, fun(_C, _Sql, _P) -> {ok, 0} end}]},
            {elib_pg_sql, [{'public_tablename', 1, fun(T) -> T end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, insufficient_balance},
                wallet_repo:atomic_balance_change(-100, 200, #{<<"tx_type">> => 1}, <<"REF1">>)
            )
        end
    ).

%% atomic_balance_change：成功 → {ok, NewBalance}，余额更新与流水写入在同一事务内完成
atomic_balance_change_ok_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                with_tx_stub(),
                {'execute', 3, fun(_C, Sql, _P) ->
                    case binary:match(Sql, <<"INSERT">>) of
                        %% UPDATE 余额 RETURNING balance
                        nomatch -> {ok, 1, [{500}]};
                        %% INSERT 流水
                        _ -> {ok, 1}
                    end
                end}
            ]},
            tsid_stub(),
            sql_stub()
        ],
        fun() ->
            ?assertEqual(
                {ok, 500},
                wallet_repo:atomic_balance_change(100, 200, #{<<"tx_type">> => 1}, <<"REF2">>)
            )
        end
    ).

%% reject_and_refund：记录不存在或已处理(MARK 影响 0 行) → {ok, 0}，幂等不退款
reject_and_refund_noop_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [with_tx_stub(), {'execute', 3, fun(_C, _Sql, _P) -> {ok, 0} end}]},
            {elib_pg_sql, [{'public_tablename', 1, fun(T) -> T end}]}
        ],
        fun() ->
            ?assertEqual({ok, 0}, wallet_repo:reject_and_refund(999))
        end
    ).

%% reject_and_refund：成功拒绝并退款 → {ok, 1}，三步(标记/退款/退款流水)在同一事务内
reject_and_refund_ok_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                with_tx_stub(),
                {'execute', 3, fun(_C, Sql, _P) ->
                    HasInsert = binary:match(Sql, <<"INSERT">>) =/= nomatch,
                    HasMark = binary:match(Sql, <<"status = 2">>) =/= nomatch,
                    if
                        %% INSERT 退款流水
                        HasInsert -> {ok, 1};
                        %% MARK：标记拒绝 RETURNING amount,user_id,wallet_id（提现金额存负）
                        HasMark -> {ok, 1, [{-100, 200, 300}]};
                        %% BalSql：退款后余额 RETURNING balance
                        true -> {ok, 1, [{900}]}
                    end
                end}
            ]},
            tsid_stub(),
            sql_stub()
        ],
        fun() ->
            ?assertEqual({ok, 1}, wallet_repo:reject_and_refund(555))
        end
    ).

%% atomic_transfer：贷记流水成功后，补偿状态回调仍失败 → 整个钱包事务回滚
atomic_transfer_after_transfer_callback_test_() ->
    Debit = #{
        user_id => 200,
        wallet_id => 300,
        amount => 100,
        tx_type => 20,
        reference_no => <<"D1">>
    },
    Credit = #{
        user_id => 201,
        wallet_id => 301,
        amount => 100,
        tx_type => 21,
        reference_no => <<"C1">>,
        after_transfer => fun(_Conn) -> {error, compensation_not_settling} end
    },
    ?WITH_MECKS(
        [
            {elib_pg, [
                with_tx_stub(),
                {'execute', 3, fun(_Conn, Sql, _Params) ->
                    case binary:match(Sql, <<"INSERT">>) of
                        nomatch ->
                            case binary:match(Sql, <<"balance = balance -">>) of
                                nomatch -> {ok, 1, [{600}]};
                                _ -> {ok, 1, [{900}]}
                            end;
                        _ ->
                            {ok, 1}
                    end
                end}
            ]},
            sql_stub(),
            tsid_stub()
        ],
        fun() ->
            ?assertEqual(
                {error, compensation_not_settling},
                wallet_repo:atomic_transfer(Debit, Credit)
            )
        end
    ).

%% atomic_transfer：补偿状态回调在钱事务提交前成功执行
atomic_transfer_after_transfer_callback_ok_test_() ->
    Debit = #{
        user_id => 200,
        wallet_id => 300,
        amount => 100,
        tx_type => 20,
        reference_no => <<"D2">>
    },
    Credit = #{
        user_id => 201,
        wallet_id => 301,
        amount => 100,
        tx_type => 21,
        reference_no => <<"C2">>,
        after_transfer => fun(mock_conn) ->
            put(callback_conn, mock_conn),
            ok
        end
    },
    ?WITH_MECKS(
        [
            {elib_pg, [
                with_tx_stub(),
                {'execute', 3, fun(_Conn, Sql, _Params) ->
                    case binary:match(Sql, <<"INSERT">>) of
                        nomatch ->
                            case binary:match(Sql, <<"balance = balance -">>) of
                                nomatch -> {ok, 1, [{600}]};
                                _ -> {ok, 1, [{900}]}
                            end;
                        _ ->
                            {ok, 1}
                    end
                end}
            ]},
            sql_stub(),
            tsid_stub()
        ],
        fun() ->
            erase(callback_conn),
            ?assertMatch({ok, _}, wallet_repo:atomic_transfer(Debit, Credit)),
            ?assertEqual(mock_conn, get(callback_conn))
        end
    ).
