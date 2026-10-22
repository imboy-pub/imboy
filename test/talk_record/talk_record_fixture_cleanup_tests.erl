-module(talk_record_fixture_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").

empty_retry_ledger_is_named_test() ->
    pure_mock_fixture:run(fun() -> check(empty) end).

missing_legacy_retry_table_is_supported_test() ->
    pure_mock_fixture:run(fun() -> check(missing) end).

nonempty_retry_ledger_is_preserved_test() ->
    pure_mock_fixture:run(fun() -> check(nonempty) end).

client_execution_fixture_test_() ->
    [
        {atom_to_list(Mode), fun() ->
            pure_mock_fixture:run(fun() -> check_client_execution(Mode) end)
        end}
     || Mode <- [empty, missing, nonempty_run, nonempty_part, run_only, part_only]
    ].

check_client_execution(Mode) ->
    Parent = self(),
    ok = meck:new(elib_pg, [non_strict]),
    try
        meck:expect(elib_pg, query, fun(conn, Sql, []) ->
            client_response(Mode, Parent, Sql)
        end),
        case Mode of
            Half when Half =:= run_only; Half =:= part_only ->
                ?assertException(
                    error,
                    {client_execution_fixture_lookup, _},
                    talk_record_t05_support:truncate_talk(conn)
                ),
                receive
                    {truncate, _} -> error(client_history_erased)
                after 0 -> ok
                end;
            Nonempty when Nonempty =:= nonempty_run; Nonempty =:= nonempty_part ->
                ?assertException(
                    error,
                    {client_execution_fixture_not_empty, _},
                    talk_record_t05_support:truncate_talk(conn)
                ),
                receive
                    {truncate, _} -> error(client_history_erased)
                after 0 -> ok
                end;
            _ ->
                ok = talk_record_t05_support:truncate_talk(conn),
                receive
                    {truncate, Sql} ->
                        ?assertEqual(
                            Mode =:= empty,
                            binary:match(
                                Sql, <<"talk_record_client_run_part, talk_record_client_run, ">>
                            ) =/= nomatch
                        ),
                        ?assertEqual(nomatch, binary:match(Sql, <<"CASCADE">>))
                after 0 -> error(missing_truncate)
                end
        end
    after
        meck:unload(elib_pg)
    end.

client_response(Mode, _, <<"SELECT to_regclass('talk_record_client_run') AS run, ", _/binary>>) ->
    Run =
        case Mode of
            missing -> null;
            part_only -> null;
            _ -> <<"present">>
        end,
    Part =
        case Mode of
            missing -> null;
            run_only -> null;
            _ -> <<"present">>
        end,
    {ok, [#{<<"run">> => Run, <<"part">> => Part}]};
client_response(
    Mode, _, <<"SELECT (SELECT count(*) FROM talk_record_client_run) AS runs, ", _/binary>>
) ->
    {ok, [
        #{
            <<"runs">> =>
                case Mode of
                    nonempty_run -> 1;
                    _ -> 0
                end,
            <<"parts">> =>
                case Mode of
                    nonempty_part -> 1;
                    _ -> 0
                end
        }
    ]};
client_response(_, Parent, Sql) ->
    response(empty, Parent, Sql).

check(Mode) ->
    Parent = self(),
    ok = meck:new(elib_pg, [non_strict]),
    try
        meck:expect(elib_pg, query, fun(conn, Sql, []) ->
            response(Mode, Parent, Sql)
        end),
        case Mode of
            nonempty ->
                ?assertException(
                    error,
                    {cleanup_retry_fixture_not_empty, _},
                    talk_record_t05_support:truncate_talk(conn)
                ),
                receive
                    {truncate, _} -> error(ledger_erased)
                after 0 -> ok
                end;
            _ ->
                ok = talk_record_t05_support:truncate_talk(conn),
                receive
                    {truncate, Sql} ->
                        HasRetry = binary:match(Sql, <<"talk_record_cleanup_retry, ">>) =/= nomatch,
                        ?assertEqual(Mode =:= empty, HasRetry),
                        ?assertEqual(nomatch, binary:match(Sql, <<"CASCADE">>))
                after 0 ->
                    error(missing_truncate)
                end
        end
    after
        meck:unload(elib_pg)
    end.

response(_, _, <<"SELECT to_regclass('talk_record_processor_run') AS name">>) ->
    {ok, [#{<<"name">> => null}]};
response(_, _, <<"SELECT to_regclass('talk_record_client_run') AS run, ", _/binary>>) ->
    {ok, [#{<<"run">> => null, <<"part">> => null}]};
response(missing, _, <<"SELECT to_regclass('talk_record_cleanup_retry') AS name">>) ->
    {ok, [#{<<"name">> => null}]};
response(_, _, <<"SELECT to_regclass('talk_record_cleanup_retry') AS name">>) ->
    {ok, [#{<<"name">> => <<"talk_record_cleanup_retry">>}]};
response(Mode, _, <<"SELECT count(*) n FROM talk_record_cleanup_retry">>) ->
    {ok, [
        #{
            <<"n">> =>
                case Mode of
                    empty -> 0;
                    nonempty -> 1
                end
        }
    ]};
response(_, _, <<"SELECT EXISTS", _/binary>>) ->
    {ok, [#{<<"enabled">> => false}]};
response(_, Parent, <<"TRUNCATE ", _/binary>> = Sql) ->
    Parent ! {truncate, Sql},
    {ok, []};
response(_, _, <<"DELETE FROM ", _/binary>>) ->
    {ok, []}.
