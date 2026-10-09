-module(user_collect_persistence_contract_tests).
-include_lib("eunit/include/eunit.hrl").

transaction_failure_is_not_success_test_() ->
    [
        persistence_case(Kind, {error, invalid_postgre_aes_key}, error)
     || Kind <- [<<"1">>, <<"7">>]
    ].

transaction_success_test_() ->
    [persistence_case(Kind, ok, ok) || Kind <- [<<"1">>, <<"7">>]].

sql_failure_aborts_transaction_test_() ->
    sql_case({error, injected_sql_failure}, error).

sql_insert_has_generated_id_test_() ->
    sql_case({ok, 1}, ok).

sql_case(SqlResult, Expected) ->
    {setup,
        fun() ->
            Modules = [
                user_collect_ds,
                elib_dt,
                elib_pg,
                attachment_ds,
                elib_hasher,
                elib_pg_sql,
                elib_tsid
            ],
            [meck:new(M, [no_link, non_strict]) || M <- Modules],
            meck:expect(user_collect_ds, count_by_uid_kind_id, fun(_, _) -> 0 end),
            meck:expect(elib_dt, now, fun() -> 1000000 end),
            meck:expect(attachment_ds, save, fun(_, _, _, []) -> ok end),
            meck:expect(elib_hasher, encoded_val, fun(_) -> <<"opaque">> end),
            meck:expect(elib_tsid, generate, fun(user_collect) -> 123456789 end),
            meck:expect(elib_pg_sql, insert, fun(_, Data, _) ->
                ?assertEqual(123456789, maps:get(<<"id">>, Data)),
                {<<"INSERT">>, []}
            end),
            meck:expect(elib_pg, execute, fun(_, _, _) -> SqlResult end),
            meck:expect(elib_pg, with_tx, fun(F) ->
                try
                    F(owned_connection)
                catch
                    error:Reason -> {error, Reason}
                end
            end),
            Modules
        end,
        fun(Modules) -> [meck:unload(M) || M <- Modules] end, fun(_) ->
            ?_test(begin
                Result = user_collect_logic:add(
                    100,
                    <<"1">>,
                    <<"owned-collect">>,
                    #{<<"text">> => <<"fixture">>},
                    <<"owned-test">>,
                    <<>>
                ),
                case Expected of
                    error -> ?assertMatch({error, _}, Result);
                    ok -> ?assertMatch({ok, _}, Result)
                end
            end)
        end}.

persistence_case(Kind, TransactionResult, Expected) ->
    {setup,
        fun() ->
            Modules = [user_collect_ds, elib_dt, elib_pg],
            [meck:new(M, [no_link, non_strict]) || M <- Modules],
            meck:expect(user_collect_ds, count_by_uid_kind_id, fun(_, _) -> 0 end),
            meck:expect(elib_dt, now, fun() -> 1000000 end),
            meck:expect(elib_pg, with_tx, fun(_) -> TransactionResult end),
            Modules
        end,
        fun(Modules) -> [meck:unload(M) || M <- Modules] end, fun(_) ->
            ?_test(begin
                Result = user_collect_logic:add(
                    100,
                    Kind,
                    <<"owned-collect">>,
                    #{<<"text">> => <<"fixture">>},
                    <<"owned-test">>,
                    <<>>
                ),
                case Expected of
                    error -> ?assertMatch({error, _}, Result);
                    ok -> ?assertMatch({ok, _}, Result)
                end
            end)
        end}.
