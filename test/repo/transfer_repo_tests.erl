-module(transfer_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc transfer_repo 的直接借记路径不能绕过钱包可用余额不变量。

with_tx_stub() ->
    {'with_tx', 1, fun(F) ->
        try
            F(mock_conn)
        catch
            throw:{rollback, Reason} -> {rollback, Reason}
        end
    end}.

transfer_create_debit_guard_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                with_tx_stub(),
                %% 扣减语句带 RETURNING，生产代码走 query/3；命中 0 行返回
                %% {ok, []}（execute_batch 语义下为 {ok, 0}），对应余额不足。
                {'query', 3, fun(_Conn, Sql, _Params) ->
                    put(transfer_create_debit_sql, Sql),
                    {ok, []}
                end},
                {'execute', 3, fun(_Conn, _Sql, _Params) ->
                    {ok, 0}
                end}
            ]},
            {elib_pg_sql, [{'public_tablename', 1, fun(T) -> T end}]},
            {elib_tsid, [{'generate', 1, fun(_Type) -> 123456 end}]}
        ],
        fun() ->
            erase(transfer_create_debit_sql),
            ?assertEqual(
                {rollback, insufficient_balance},
                transfer_repo:create(200, 201, 100, <<"guard-test">>)
            ),
            Sql = get(transfer_create_debit_sql),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"status = 1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"balance - frozen >= $1">>))
        end
    ).
