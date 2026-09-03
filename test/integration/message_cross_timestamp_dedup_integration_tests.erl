-module(message_cross_timestamp_dedup_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TABLES, [<<"msg_c2c">>, <<"msg_c2g">>, <<"msg_c2s">>, <<"msg_s2c">>]).

cross_timestamp_dedup_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {timeout, 60, fun verifies_cross_timestamp_dedup/0};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

verifies_cross_timestamp_dedup() ->
    verifies_existing_duplicate_precheck(),
    Result = elib_pg:with_tx(fun(Conn) ->
        ok = apply_migration(
            Conn, "priv/migrations/00000083_message_cross_timestamp_dedup.down.sql"
        ),
        ok = apply_runtime_statements(Conn),
        lists:foreach(fun(Table) -> verifies_table(Conn, Table) end, ?TABLES),
        verifies_same_timestamp_conflict(Conn),
        verifies_year_scale_chunks(Conn),
        verifies_down_migration(Conn),
        throw({rollback, verified})
    end),
    ?assertEqual({rollback, verified}, Result).

verifies_existing_duplicate_precheck() ->
    MsgId = unique_msg_id(<<"precheck">>),
    Result = elib_pg:with_tx(fun(Conn) ->
        ok = apply_migration(
            Conn, "priv/migrations/00000083_message_cross_timestamp_dedup.down.sql"
        ),
        {ok, 1} = insert_message(Conn, <<"msg_c2c">>, MsgId, <<"2026-09-03T06:00:00Z">>),
        {ok, 1} = insert_message(Conn, <<"msg_c2c">>, MsgId, <<"2026-09-20T06:00:00Z">>),
        apply_migration(Conn, "priv/migrations/00000083_message_cross_timestamp_dedup.up.sql")
    end),
    ?assertMatch({error, _}, Result).

verifies_table(Conn, Table) ->
    MsgId = unique_msg_id(Table),
    {ok, 1} = insert_message(Conn, Table, MsgId, <<"2026-09-03T06:00:00Z">>),
    {ok, 0} = insert_message(Conn, Table, MsgId, <<"2026-09-20T06:00:00Z">>),
    {ok, [#{<<"count">> := 1}]} = elib_pg:query(
        Conn,
        iolist_to_binary(["SELECT count(*) AS count FROM public.", Table, " WHERE msg_id = $1"]),
        [MsgId]
    ).

verifies_same_timestamp_conflict(Conn) ->
    MsgId = unique_msg_id(<<"same_timestamp">>),
    Timestamp = <<"2026-09-03T06:00:00Z">>,
    {ok, 1} = insert_message_on_conflict(Conn, MsgId, Timestamp),
    {ok, 0} = insert_message_on_conflict(Conn, MsgId, Timestamp).

verifies_year_scale_chunks(Conn) ->
    MsgId = unique_msg_id(<<"year_scale">>),
    {ok, 52} = elib_pg:execute(
        Conn,
        <<
            "INSERT INTO public.msg_c2c "
            "(id, from_id, to_id, msg_id, msg_type, payload, created_at) "
            "SELECT $1::bigint + n, 1, 2, $2 || '_' || n, 'text', '{}', "
            "current_timestamp - n * interval '7 days' FROM generate_series(0, 51) n"
        >>,
        [elib_tsid:generate(), MsgId]
    ),
    {ok, [#{<<"count">> := ChunkCount}]} = elib_pg:query(
        Conn,
        <<
            "SELECT count(*) AS count FROM timescaledb_information.chunks "
            "WHERE hypertable_schema = 'public' AND hypertable_name = 'msg_c2c'"
        >>,
        []
    ),
    ?assert(ChunkCount >= 40),
    {ok, 1} = insert_message(Conn, <<"msg_c2c">>, MsgId, <<"2026-09-03T07:00:00Z">>).

verifies_down_migration(Conn) ->
    MsgId = unique_msg_id(<<"down">>),
    ok = apply_migration(Conn, "priv/migrations/00000083_message_cross_timestamp_dedup.down.sql"),
    {ok, 1} = insert_message(Conn, <<"msg_c2c">>, MsgId, <<"2026-09-03T06:00:00Z">>),
    {ok, 1} = insert_message(Conn, <<"msg_c2c">>, MsgId, <<"2026-09-20T06:00:00Z">>).

insert_message(Conn, Table, MsgId, Timestamp) ->
    Sql =
        case Table of
            <<"msg_s2c">> ->
                "INSERT INTO public.msg_s2c "
                "(id, from_id, to_id, msg_id, action, msg_type, payload, created_at) "
                "VALUES ($1, 1, 2, $2, 'message', 'text', '{}', $3)";
            _ ->
                iolist_to_binary([
                    "INSERT INTO public.",
                    Table,
                    " (id, from_id, to_id, msg_id, msg_type, payload, created_at) ",
                    "VALUES ($1, 1, 2, $2, 'text', '{}', $3)"
                ])
        end,
    elib_pg:execute(Conn, Sql, [elib_tsid:generate(), MsgId, Timestamp]).

insert_message_on_conflict(Conn, MsgId, Timestamp) ->
    elib_pg:execute(
        Conn,
        <<
            "INSERT INTO public.msg_c2c "
            "(id, from_id, to_id, msg_id, msg_type, payload, created_at) "
            "VALUES ($1, 1, 2, $2, 'text', '{}', $3) "
            "ON CONFLICT (msg_id, created_at) DO NOTHING"
        >>,
        [elib_tsid:generate(), MsgId, Timestamp]
    ).

unique_msg_id(Suffix) ->
    <<"dedup_", Suffix/binary, "_", (integer_to_binary(elib_tsid:generate()))/binary>>.

apply_runtime_statements(Conn) ->
    {ok, Sql} = file:read_file("priv/migrations/00000083_message_cross_timestamp_dedup.up.sql"),
    Statements = [
        Statement
     || Statement <- binary:split(Sql, <<"--;">>, [global]),
        string:trim(Statement) =/= <<>>
    ],
    apply_statements(Conn, lists:droplast(Statements)).

apply_migration(Conn, Path) ->
    {ok, Sql} = file:read_file(Path),
    apply_statements(Conn, binary:split(Sql, <<"--;">>, [global])).

apply_statements(Conn, RawStatements) ->
    Statements = [
        string:trim(Statement)
     || Statement <- RawStatements,
        string:trim(Statement) =/= <<>>
    ],
    lists:foreach(
        fun(Statement) ->
            case elib_pg:execute(Conn, Statement, []) of
                {ok, _} -> ok;
                {ok, _, _} -> ok
            end
        end,
        Statements
    ).
