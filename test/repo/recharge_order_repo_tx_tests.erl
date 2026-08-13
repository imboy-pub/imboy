-module(recharge_order_repo_tx_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc 充值退款会从用户钱包扣回余额，因此必须遵守停用钱包不可借记的不变量。

with_tx_stub() ->
    {'with_tx', 1, fun(F) ->
        try
            F(mock_conn)
        catch
            throw:{rollback, Reason} -> {rollback, Reason}
        end
    end}.

refund_debit_guard_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                with_tx_stub(),
                {'execute', 3, fun(_Conn, Sql, _Params) ->
                    case binary:match(Sql, <<"RETURNING user_id, amount">>) of
                        nomatch ->
                            case binary:match(Sql, <<"SET balance = balance - $1">>) of
                                nomatch ->
                                    {ok, 1, [{1}]};
                                _ ->
                                    put(recharge_refund_debit_sql, Sql),
                                    {ok, 0}
                            end;
                        _ ->
                            {ok, 1, [{200, 100}]}
                    end
                end}
            ]},
            {elib_pg_sql, [{'public_tablename', 1, fun(T) -> T end}]}
        ],
        fun() ->
            erase(recharge_refund_debit_sql),
            ?assertEqual(
                {rollback, insufficient_available},
                recharge_order_repo:refund_in_tx(<<"RCH-GUARD">>)
            ),
            Sql = get(recharge_refund_debit_sql),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"balance - frozen >= $1">>))
        end
    ).
