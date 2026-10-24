-module(talk_record_repo_sqlstate_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

sqlstate_paths_test_() ->
    [
        {atom_to_list(Shape) ++ "_" ++ atom_to_list(Path), fun() ->
            with_mocks(fun() -> conflict_path(Shape, Path) end)
        end}
     || Shape <- [direct, wrapped],
        Path <- [record, capture, mark, event, part_replay, part_conflict]
    ] ++
        [
            {"preserve_" ++ atom_to_list(Path), fun() ->
                with_mocks(fun() -> preserve_error(Path) end)
            end}
         || Path <- [record, capture, mark, event, part_replay]
        ].

with_mocks(Fun) ->
    put(sql_select_count, 0),
    put(tx_query_count, 0),
    ok = meck:new(elib_pg, [non_strict, no_link]),
    ok = meck:new(elib_pg_sql, [non_strict, no_link]),
    try
        ok = meck:expect(elib_pg_sql, public_tablename, fun(T) -> T end),
        ok = meck:expect(elib_pg_sql, insert, fun(_, _) -> {<<"INSERT">>, []} end),
        Fun(),
        ?assert(meck:validate(elib_pg)),
        ?assert(meck:validate(elib_pg_sql))
    after
        meck:unload(elib_pg_sql),
        meck:unload(elib_pg),
        erase(sql_select_count),
        erase(tx_query_count)
    end.

error_record(Code) ->
    #error{
        severity = error,
        code = Code,
        codename = unique_violation,
        message = <<"synthetic constraint">>,
        extra = []
    }.

conflict_path(Shape, Path) ->
    Direct = error_record(<<"23505">>),
    Reason =
        case Shape of
            direct -> Direct;
            wrapped -> {error, Direct}
        end,
    ok = meck:expect(elib_pg, execute, fun(_, _, _) -> {error, Reason} end),
    configure_query(Path),
    Expected =
        case Path of
            event -> {error, event_seq_conflict};
            part_replay -> {ok, existing, #{<<"sha256">> => <<"same">>}};
            part_conflict -> {error, part_seq_conflict};
            _ -> {error, unique_violation}
        end,
    ?assertEqual(Expected, invoke(Path)),
    ?assertEqual(1, meck:num_calls(elib_pg, execute, ['_', '_', '_'])),
    case Path of
        event -> ?assertEqual(2, get(sql_select_count));
        part_replay -> ?assertEqual(1, get(sql_select_count));
        part_conflict -> ?assertEqual(1, get(sql_select_count));
        _ -> ok
    end.

preserve_error(Path) ->
    Reasons = [
        error_record(<<"23503">>),
        error_record(<<"bad">>),
        {error, malformed},
        unexpected
    ],
    lists:foreach(
        fun(Reason) ->
            ok = meck:expect(elib_pg, execute, fun(_, _, _) -> {error, Reason} end),
            configure_query(Path),
            ?assertEqual({error, Reason}, invoke(Path))
        end,
        Reasons
    ),
    case Path of
        part_replay -> ?assertEqual(0, get(sql_select_count));
        _ -> ok
    end.

configure_query(Path) ->
    meck:expect(elib_pg, query, fun(_, Sql, _) ->
        case iolist_to_binary(Sql) of
            <<"SAVEPOINT ", _/binary>> ->
                transaction_query();
            <<"ROLLBACK TO SAVEPOINT ", _/binary>> ->
                transaction_query();
            <<"RELEASE SAVEPOINT ", _/binary>> ->
                transaction_query();
            <<"SELECT ", _/binary>> ->
                put(sql_select_count, get(sql_select_count) + 1),
                case Path of
                    event -> {ok, []};
                    part_replay -> {ok, [#{<<"sha256">> => <<"same">>}]};
                    part_conflict -> {ok, [#{<<"sha256">> => <<"different">>}]};
                    _ -> error(unexpected_business_select)
                end;
            _ ->
                error(unexpected_statement)
        end
    end).

transaction_query() ->
    put(tx_query_count, get(tx_query_count) + 1),
    {ok, []}.

invoke(record) ->
    talk_record_repo:insert_tx(synthetic_conn, #{<<"id">> => 1});
invoke(capture) ->
    talk_record_capture_repo:insert_capture_tx(synthetic_conn, #{<<"id">> => 1});
invoke(mark) ->
    talk_record_marks_repo:insert_tx(synthetic_conn, #{<<"id">> => 1});
invoke(event) ->
    talk_record_capture_repo:insert_event_tx(
        synthetic_conn,
        #{
            <<"id">> => 1,
            <<"record_id">> => 2,
            <<"capture_id">> => 3,
            <<"device_session_id">> => <<"synthetic">>,
            <<"client_event_id">> => <<"event">>,
            <<"seq">> => 1,
            <<"event">> => <<"pause">>,
            <<"position_ms">> => 5,
            <<"reported_at">> => 10
        }
    );
invoke(Path) when Path =:= part_replay; Path =:= part_conflict ->
    talk_record_capture_repo:insert_part_tx(
        synthetic_conn,
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
