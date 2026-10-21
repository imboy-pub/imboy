-module(elib_pg_commit_real_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% Explicit, dedicated synthetic clone only. Never discover or alter a shared database.
commit_real_pg_test_() ->
    case os:getenv("RELAY_DELETE_PG") of
        "1" ->
            {timeout, 120,
                {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(S) ->
                    {timeout, 60, ?_test(run(S))}
                end}};
        _ ->
            []
    end.
setup() ->
    S = talk_record_t19_support:setup_all(),
    C = maps:get(conn, S),
    {ok, [#{<<"system_identifier">> := Identity, <<"db">> := Db}]} = elib_pg:query(
        C,
        <<"SELECT system_identifier,current_database() db FROM pg_control_system()">>,
        []
    ),
    true = integer_to_binary(Identity) =:= list_to_binary(os:getenv("RELAY_TX_TEST_SYSID")),
    true = Db =:= <<"talk_record_delete_repro">>,
    ok = talk_record_t19_support:seed(S),
    {ok, [#{<<"base">> := Base}]} = elib_pg:query(
        C,
        <<
            "SELECT greatest(9000000000000,"
            "coalesce((SELECT max(id) FROM attachment),0),coalesce((SELECT max(id) FROM talk_record),0),"
            "coalesce((SELECT max(id) FROM talk_record_job),0),coalesce((SELECT max(id) FROM talk_record_audit),0)) base"
        >>,
        []
    ),
    meck:expect(elib_tsid, generate, fun(_) ->
        Base + erlang:unique_integer([positive, monotonic])
    end),
    S.
run(S) ->
    C = maps:get(conn, S),
    {ok, _} = elib_pg:execute(
        C, <<"CREATE TABLE IF NOT EXISTS relay_commit_probe(id integer PRIMARY KEY)">>, []
    ),
    {ok, _} = elib_pg:execute(C, <<"TRUNCATE relay_commit_probe">>, []),
    ordinary_commit(C),
    swallowed_error(C, <<"SELECT 1/0">>, 2),
    swallowed_error(C, <<"SELECT 1 FROM relay_absent_projection">>, 3),
    rollback_actions(C),
    returned_sql_error(C),
    commit_actions(C),
    receipt_loss(C),
    missing_projection_delete(C),
    audit_failed_delete(C),
    fence_failed_delete(C),
    cancel_failed_delete(C),
    ok.
ordinary_commit(C) ->
    ?assertEqual(
        {ok, committed},
        elib_pg:with_tx(fun(X) ->
            insert(X, 1),
            {ok, committed}
        end)
    ),
    ?assertEqual(1, count(C, 1)).
swallowed_error(C, Sql, Id) ->
    ?assertMatch(
        {error, {commit_not_confirmed, {ok, misleading_body}, {ok, rollback}}},
        elib_pg:with_tx(fun(X) ->
            insert(X, Id),
            {error, _} = elib_pg:query(X, Sql, []),
            {ok, misleading_body}
        end)
    ),
    ?assertEqual(0, count(C, Id)).
rollback_actions(C) ->
    Parent = self(),
    ?assertMatch(
        {error, {commit_not_confirmed, _, {ok, rollback}}},
        elib_pg:with_tx(fun(X) ->
            insert(X, 4),
            elib_pg:after_commit(X, never, fun() ->
                Parent ! incorrect_action,
                ok
            end),
            {error, _} = elib_pg:query(X, <<"SELECT 1/0">>, []),
            {ok, misleading_body}
        end)
    ),
    ?assertEqual(
        {rollback, explicit},
        elib_pg:with_tx(
            fun(X) ->
                insert(X, 5),
                elib_pg:after_commit(X, never, fun() ->
                    Parent ! incorrect_action,
                    ok
                end),
                throw(explicit)
            end,
            [{reraise, false}]
        )
    ),
    ?assertEqual(0, count(C, 4)),
    ?assertEqual(0, count(C, 5)),
    no_action().
returned_sql_error(C) ->
    Parent = self(),
    Result = elib_pg:with_tx(fun(X) ->
        insert(X, 8),
        elib_pg:after_commit(X, never, fun() ->
            Parent ! incorrect_action,
            ok
        end),
        Error = elib_pg:query(X, <<"SELECT 1/0">>, []),
        put(original_sql_error, Error),
        Error
    end),
    ?assertMatch({error, #error{code = <<"22012">>}}, Result),
    ?assertEqual(erase(original_sql_error), Result),
    ?assertEqual(0, count(C, 8)),
    no_action().
commit_actions(C) ->
    Parent = self(),
    ?assertEqual(
        {ok, committed},
        elib_pg:with_tx(fun(X) ->
            insert(X, 6),
            elib_pg:after_commit(X, confirmed, fun() ->
                Parent ! confirmed_action,
                ok
            end),
            {ok, committed}
        end)
    ),
    ?assertEqual(1, count(C, 6)),
    receive
        confirmed_action -> ok
    after 1000 -> error(missing_action)
    end.
receipt_loss(C) ->
    Parent = self(),
    put(transaction_body_calls, 0),
    ok = meck:new(epgsql, [passthrough, no_link]),
    meck:expect(epgsql, get_cmd_status, fun(_) -> {error, synthetic_receipt_loss} end),
    try
        ?assertMatch(
            {error, {commit_not_confirmed, _, {error, synthetic_receipt_loss}}},
            elib_pg:with_tx(fun(X) ->
                put(transaction_body_calls, get(transaction_body_calls) + 1),
                insert(X, 7),
                elib_pg:after_commit(X, never, fun() ->
                    Parent ! incorrect_action,
                    ok
                end),
                {ok, body_completed}
            end)
        )
    after
        meck:unload(epgsql)
    end,
    %% Real COMMIT occurred; only its client receipt is injected as unavailable. Never replay it.
    ?assertEqual(1, get(transaction_body_calls)),
    ?assertEqual(1, count(C, 7)),
    no_action().
insert(C, Id) ->
    {ok, 1} = elib_pg:execute(C, <<"INSERT INTO relay_commit_probe(id) VALUES($1)">>, [Id]),
    ok.
count(C, Id) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        C, <<"SELECT count(*) n FROM relay_commit_probe WHERE id=$1">>, [Id]
    ),
    N.
no_action() ->
    receive
        incorrect_action -> error(action_before_confirmed_commit)
    after 0 -> ok
    end.

missing_projection_delete(C) ->
    U = 700001,
    Rev = 1,
    {ok, R} = talk_record_t15_support:create_record(
        U, test_number(), <<"synthetic missing projection">>
    ),
    Restore = remove_projection_fixture(C),
    try
        ?assertMatch(
            {error, {commit_not_confirmed, _, {ok, rollback}}},
            talk_record_delete_logic:delete(U, R, integer_to_binary(Rev))
        ),
        {ok, [#{<<"deleted_at">> := null}]} = elib_pg:query(
            C, <<"SELECT deleted_at FROM talk_record WHERE id=$1">>, [R]
        )
    after
        Restore()
    end.
remove_projection_fixture(C) ->
    case elib_pg:query(C, <<"SELECT to_regclass('talk_record_processor_run') t">>, []) of
        {ok, [#{<<"t">> := null}]} ->
            fun() ->
                {ok, Sql} = file:read_file(
                    "priv/migrations/00000219_talk_record_processor_run.up.sql"
                ),
                Results = epgsql:squery(C, Sql),
                true = is_list(Results),
                [] = [E || E = {error, _} <- Results],
                ok
            end;
        {ok, [_]} ->
            {ok, _} = elib_pg:query(
                C,
                <<"ALTER TABLE talk_record_processor_run RENAME TO relay_saved_processor_run">>,
                []
            ),
            fun() ->
                {ok, _} = elib_pg:query(
                    C,
                    <<"ALTER TABLE relay_saved_processor_run RENAME TO talk_record_processor_run">>,
                    []
                ),
                ok
            end
    end.
audit_failed_delete(C) ->
    {ok, R} = talk_record_t15_support:create_record(
        700001, test_number(), <<"synthetic audit failure">>
    ),
    install_failure(C, <<"talk_record_audit">>, <<"INSERT">>),
    try
        ?assertEqual({error, audit_failed}, talk_record_delete_logic:delete(700001, R, <<"1">>))
    after
        drop_failure(C, <<"talk_record_audit">>)
    end,
    assert_not_deleted(C, R),
    ?assertMatch({ok, deleted, _}, talk_record_delete_logic:delete(700001, R, <<"1">>)),
    {ok, [#{<<"deleted">> := true}]} = elib_pg:query(
        C, <<"SELECT deleted_at IS NOT NULL deleted FROM talk_record WHERE id=$1">>, [R]
    ).
fence_failed_delete(C) ->
    {ok, R} = talk_record_t15_support:create_record(
        700001, test_number(), <<"synthetic fence failure">>
    ),
    install_failure(C, <<"talk_record_capture">>, <<"UPDATE">>),
    try
        ?assertMatch({error, _}, talk_record_delete_logic:delete(700001, R, <<"1">>))
    after
        drop_failure(C, <<"talk_record_capture">>)
    end,
    assert_not_deleted(C, R).
cancel_failed_delete(C) ->
    {ok, R} = talk_record_t15_support:create_record(
        700001, test_number(), <<"synthetic cancel failure">>
    ),
    install_failure(C, <<"talk_record_job">>, <<"UPDATE">>),
    try
        ?assertEqual(
            {error, cancel_jobs_failed}, talk_record_delete_logic:delete(700001, R, <<"1">>)
        )
    after
        drop_failure(C, <<"talk_record_job">>)
    end,
    assert_not_deleted(C, R).
assert_not_deleted(C, R) ->
    {ok, [#{<<"deleted_at">> := null, <<"revision">> := 1}]} = elib_pg:query(
        C,
        <<"SELECT deleted_at,revision FROM talk_record WHERE id=$1">>,
        [R]
    ),
    ?assertEqual(
        {ok, []},
        elib_pg:query(C, <<"SELECT 1 FROM talk_record_job WHERE record_id=$1 AND kind='purge'">>, [
            R
        ])
    ).
install_failure(C, Table, Op) ->
    {ok, _} = elib_pg:query(
        C,
        <<
            "CREATE OR REPLACE FUNCTION relay_injected_failure() RETURNS trigger LANGUAGE plpgsql AS $$"
            " BEGIN RAISE EXCEPTION 'synthetic forced SQL failure' USING ERRCODE='P0001'; END $$"
        >>,
        []
    ),
    {ok, _} = elib_pg:query(
        C,
        [
            <<"CREATE TRIGGER relay_injected_failure BEFORE ">>,
            Op,
            <<" ON ">>,
            Table,
            <<" FOR EACH STATEMENT EXECUTE FUNCTION relay_injected_failure()">>
        ],
        []
    ),
    ok.
drop_failure(C, Table) ->
    {ok, _} = elib_pg:query(C, [<<"DROP TRIGGER relay_injected_failure ON ">>, Table], []),
    ok.

test_number() -> binary:decode_unsigned(crypto:strong_rand_bytes(4)).
