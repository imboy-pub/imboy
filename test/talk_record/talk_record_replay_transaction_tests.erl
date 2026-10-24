-module(talk_record_replay_transaction_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

transaction_test_() ->
    [
        {atom_to_list(Path) ++ "_" ++ atom_to_list(Mode), fun() -> scenario(Path, Mode) end}
     || Path <- [record, part],
        Mode <- [unique, savepoint_fail, rollback_fail, release_fail, other_error, inserted, zero]
    ].

scenario(Path, Mode) ->
    put(tx_aborted, false),
    put(select_count, 0),
    put(execute_count, 0),
    put(saves, []),
    put(releases, 0),
    put(rollbacks, 0),
    ok = meck:new(elib_pg, [non_strict, no_link]),
    ok = meck:new(elib_pg_sql, [non_strict, no_link]),
    try
        ok = meck:expect(elib_pg_sql, public_tablename, fun(T) -> T end),
        ok = meck:expect(elib_pg_sql, insert, fun(_, _) -> {<<"INSERT">>, []} end),
        ok = meck:expect(elib_pg, execute, fun(_, _, _) ->
            put(execute_count, get(execute_count) + 1),
            case Mode of
                inserted ->
                    {ok, 1};
                zero ->
                    {ok, 0};
                _ ->
                    put(tx_aborted, true),
                    Code =
                        case Mode of
                            other_error -> <<"23503">>;
                            _ -> <<"23505">>
                        end,
                    {error, #error{
                        severity = error,
                        code = Code,
                        codename = synthetic,
                        message = <<"synthetic">>,
                        extra = []
                    }}
            end
        end),
        ok = meck:expect(elib_pg, query, fun(_, Sql, _) -> query(iolist_to_binary(Sql), Mode) end),
        Result = invoke(Path),
        case Mode of
            inserted ->
                Expected =
                    case Path of
                        record -> {ok, 1};
                        part -> {ok, inserted, #{}}
                    end,
                ?assertEqual(Expected, Result),
                ?assertEqual(1, get(releases)),
                ?assertEqual(0, get(rollbacks)),
                ?assertEqual(0, get(select_count)),
                ?assertEqual(false, get(tx_aborted));
            zero ->
                Expected =
                    case Path of
                        record -> {ok, #{<<"id">> => 9}};
                        part -> {ok, existing, #{<<"id">> => 9, <<"sha256">> => <<"same">>}}
                    end,
                ?assertEqual(Expected, Result),
                ?assertEqual(1, get(releases)),
                ?assertEqual(0, get(rollbacks)),
                ?assertEqual(1, get(select_count)),
                ?assertEqual(false, get(tx_aborted));
            unique ->
                Expected =
                    case Path of
                        record -> {ok, #{<<"id">> => 9}};
                        part -> {ok, existing, #{<<"id">> => 9, <<"sha256">> => <<"same">>}}
                    end,
                ?assertEqual(Expected, Result),
                ?assertEqual(false, get(tx_aborted)),
                ?assertEqual(1, get(select_count)),
                [Save] = get(saves),
                ?assert(is_binary(Save));
            savepoint_fail ->
                ?assertEqual({error, savepoint_failed}, Result),
                ?assertEqual(0, get(execute_count)),
                ?assertEqual(0, get(select_count));
            rollback_fail ->
                ?assertEqual({error, rollback_failed}, Result),
                ?assertEqual(0, get(select_count));
            release_fail ->
                ?assertEqual({error, release_failed}, Result),
                ?assertEqual(0, get(select_count));
            other_error ->
                ?assertMatch({error, #error{code = <<"23503">>}}, Result),
                ?assertEqual(true, get(tx_aborted)),
                ?assertEqual(0, get(select_count))
        end
    after
        meck:unload(elib_pg_sql),
        meck:unload(elib_pg),
        [erase(K) || K <- [tx_aborted, select_count, execute_count, saves, releases, rollbacks]]
    end.

query(<<"SAVEPOINT ", Name/binary>>, Mode) ->
    case Mode of
        savepoint_fail ->
            {error, savepoint_failed};
        _ ->
            put(saves, [Name | get(saves)]),
            {ok, []}
    end;
query(<<"ROLLBACK TO SAVEPOINT ", Name/binary>>, Mode) ->
    put(rollbacks, get(rollbacks) + 1),
    ?assertEqual([Name], get(saves)),
    case Mode of
        rollback_fail ->
            {error, rollback_failed};
        _ ->
            put(tx_aborted, false),
            {ok, []}
    end;
query(<<"RELEASE SAVEPOINT ", Name/binary>>, Mode) ->
    put(releases, get(releases) + 1),
    ?assertEqual([Name], get(saves)),
    ?assertEqual(false, get(tx_aborted)),
    case Mode of
        release_fail -> {error, release_failed};
        _ -> {ok, []}
    end;
query(_, _) ->
    put(select_count, get(select_count) + 1),
    case get(tx_aborted) of
        true ->
            {error, #error{
                severity = error,
                code = <<"25P02">>,
                codename = in_failed_sql_transaction,
                message = <<"synthetic aborted">>,
                extra = []
            }};
        false ->
            {ok, [#{<<"id">> => 9, <<"sha256">> => <<"same">>}]}
    end.

invoke(record) ->
    case talk_record_repo:insert_tx(synthetic, #{<<"id">> => 1}) of
        {error, unique_violation} ->
            case talk_record_repo:find_by_creator_client_tx(synthetic, 2, <<"client">>) of
                {ok, Row} -> {ok, maps:with([<<"id">>], Row)};
                Error -> Error
            end;
        Error ->
            Error
    end;
invoke(part) ->
    talk_record_capture_repo:insert_part_tx(
        synthetic,
        #{
            <<"id">> => 1,
            <<"record_id">> => 2,
            <<"capture_id">> => 3,
            <<"seq">> => 1,
            <<"start_ms">> => 0,
            <<"end_ms">> => 10,
            <<"capture_offset_ms">> => 0,
            <<"attachment_id">> => 4,
            <<"sha256">> => <<"same">>,
            <<"byte_size">> => 5
        }
    ).
