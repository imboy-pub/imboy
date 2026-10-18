-module(attachment_intent_transaction_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 真 PG 同事务回归：测试侧不包 SAVEPOINT，必须由生产 repo 恢复已知拒绝。
%% Real PG regression: no test-side savepoint may hide a poisoned transaction.
ordinary_intent_transaction_test_() ->
    {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.

run() ->
    State = inttest_marker_db:provision(#{env_prefix => <<"INTENT_TX_INTTEST">>}),
    C = maps:get(conn, State),
    try
        {ok, Observer} = inttest_marker_db:safe_connect(
            (maps:get(server, State))#{database => maps:get(db, State)}
        ),
        try
            exec(C, <<"CREATE TABLE inttest_intent_sentinel (id integer PRIMARY KEY)">>),
            success_commits(C, Observer),
            success_obeys_outer_rollback(C, Observer),
            known_errors_preserve_outer_transaction(C, Observer),
            unknown_error_aborts_outer_transaction(C, Observer),
            requires_outer_transaction(C, Observer)
        after
            epgsql:close(Observer)
        end
    after
        inttest_marker_db:release(State)
    end.

success_commits(C, Observer) ->
    Key = seed(C, 1, true),
    in_transaction(C, fun() ->
        sentinel(C, 1),
        ?assertEqual(ok, intent(C, Key)),
        ?assertEqual({<<"delete_intent">>, 1, 1}, snapshot(C, Key)),
        ?assertEqual({<<"live">>, 0, 0}, snapshot(Observer, Key)),
        commit(C)
    end),
    ?assertEqual({<<"delete_intent">>, 1, 1}, snapshot(Observer, Key)),
    ?assertEqual(1, sentinel_count(Observer, 1)).

success_obeys_outer_rollback(C, Observer) ->
    Key = seed(C, 2, true),
    in_transaction(C, fun() ->
        sentinel(C, 2),
        ?assertEqual(ok, intent(C, Key)),
        ?assertEqual({<<"delete_intent">>, 1, 1}, snapshot(C, Key))
    end),
    ?assertEqual({<<"live">>, 0, 0}, snapshot(Observer, Key)),
    ?assertEqual(0, sentinel_count(Observer, 2)).

known_errors_preserve_outer_transaction(C, Observer) ->
    Earlier = seed(C, 3, true),
    Unverified = seed(C, 4, false),
    Tombstone = seed(C, 5, true),
    Later = seed(C, 6, true),
    %% 已存在队列墓碑且 metadata 仍 live：覆盖函数的 23505 分支。
    %% Existing tombstone with live metadata exercises the distinct 23505 branch.
    exec(
        C,
        <<"INSERT INTO attachment_object_deletion "
          "(path,bucket,storage_driver,storage_namespace,storage_container,"
          "storage_locator_sha256,job_state,last_error_code) "
          "VALUES ($1,'test-bucket','garage','intent-test','test-bucket',$2,'ready',NULL)">>,
        [Tombstone, locator()]
    ),
    in_transaction(C, fun() ->
        sentinel(C, 3),
        ?assertEqual(ok, intent(C, Earlier)),
        lists:foreach(
            fun({Key, Expected}) ->
                ?assertEqual({error, Expected}, intent(C, Key)),
                %% No rollback/sync here: the repo must leave this same tx usable.
                ?assertEqual({ok, [#{<<"probe">> => 42}]},
                    elib_pg:query(C, <<"SELECT 42 AS probe">>, [])),
                ?assertEqual(1, sentinel_count(C, 3)),
                ?assertEqual({<<"delete_intent">>, 1, 1}, snapshot(C, Earlier))
            end,
            [{Unverified, intent_conflict}, {path(99), not_found}, {Tombstone, tombstone}]
        ),
        ?assertEqual({<<"live">>, 0, 0}, snapshot(C, Unverified)),
        ?assertEqual({<<"live">>, 1, 0}, snapshot(C, Tombstone)),
        ?assertEqual(ok, intent(C, Later)),
        commit(C)
    end),
    ?assertEqual(1, sentinel_count(Observer, 3)),
    ?assertEqual({<<"delete_intent">>, 1, 1}, snapshot(Observer, Earlier)),
    ?assertEqual({<<"delete_intent">>, 1, 1}, snapshot(Observer, Later)),
    ?assertEqual({<<"live">>, 0, 0}, snapshot(Observer, Unverified)),
    ?assertEqual({<<"live">>, 1, 0}, snapshot(Observer, Tombstone)).

unknown_error_aborts_outer_transaction(C, Observer) ->
    Key = seed(C, 7, true),
    Earlier = seed(C, 8, true),
    %% 只在一次性 marker 库注入 audit 错误，发生于状态变更和入队之后。
    %% Inject after state/queue writes, without replacing the production function.
    exec(C, <<
        "CREATE FUNCTION inttest_intent_audit_failure() RETURNS trigger "
        "LANGUAGE plpgsql AS $$ BEGIN "
        "IF NEW.object_key = 'inttest/ordinary-intent/7' THEN "
        "RAISE EXCEPTION 'intent audit failure probe' USING ERRCODE='XX000'; "
        "END IF; RETURN NEW; END $$"
    >>),
    exec(C, <<
        "CREATE TRIGGER inttest_intent_audit_failure BEFORE INSERT "
        "ON attachment_lifecycle_v5_audit FOR EACH ROW "
        "EXECUTE FUNCTION inttest_intent_audit_failure()"
    >>),
    try
        in_transaction(C, fun() ->
            sentinel(C, 4),
            ?assertEqual(ok, intent(C, Earlier)),
            ?assertMatch(
                {error, #error{code = <<"XX000">>, message = <<"intent audit failure probe">>}},
                intent(C, Key)
            ),
            %% Unknown errors remain fatal; they cannot silently commit prior work.
            ?assertMatch(
                {error, #error{code = <<"25P02">>}},
                elib_pg:query(C, <<"SELECT 42 AS probe">>, [])
            )
        end),
        ?assertEqual(0, sentinel_count(Observer, 4)),
        ?assertEqual({<<"live">>, 0, 0}, snapshot(Observer, Earlier)),
        ?assertEqual({<<"live">>, 0, 0}, snapshot(Observer, Key))
    after
        exec(C, <<"DROP TRIGGER inttest_intent_audit_failure ON attachment_lifecycle_v5_audit">>),
        exec(C, <<"DROP FUNCTION inttest_intent_audit_failure()">>)
    end.

requires_outer_transaction(C, Observer) ->
    Key = seed(C, 9, true),
    ?assertMatch({error, #error{code = <<"25P01">>}}, intent(C, Key)),
    ?assertEqual({<<"live">>, 0, 0}, snapshot(Observer, Key)).

in_transaction(C, Fun) ->
    exec(C, <<"BEGIN">>),
    try
        Fun()
    after
        %% Also safe after an explicit COMMIT; fixture cleanup never masks recovery.
        exec(C, <<"ROLLBACK">>)
    end.

commit(C) ->
    exec(C, <<"COMMIT">>),
    ?assertEqual({ok, commit}, epgsql:get_cmd_status(C)).

intent(C, Key) ->
    attachment_repo:intent_ordinary_delete_tx(C, Key, <<"transaction regression">>).

seed(C, N, Verified) ->
    Key = path(N),
    exec(
        C,
        <<"INSERT INTO attachment "
          "(id,path,url,scope,creator_user_id,storage_driver,storage_namespace,"
          "storage_container,storage_locator_sha256,gc_verified) "
          "VALUES ($1,$2,'test-bucket','private',101,'garage','intent-test',"
          "'test-bucket',$3,$4)">>,
        [901194000 + N, Key, locator(), Verified]
    ),
    Key.

path(N) -> <<"inttest/ordinary-intent/", (integer_to_binary(N))/binary>>.
locator() -> binary:copy(<<"a">>, 64).

snapshot(C, Key) ->
    {ok, [Row]} = elib_pg:query(
        C,
        <<"SELECT a.lifecycle_state, "
          "(SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1) AS jobs, "
          "(SELECT COUNT(*) FROM attachment_lifecycle_v5_audit "
          "WHERE object_key=$1 AND action='delete_intent') AS audits "
          "FROM attachment a WHERE a.path=$1">>,
        [Key]
    ),
    {maps:get(<<"lifecycle_state">>, Row), maps:get(<<"jobs">>, Row),
        maps:get(<<"audits">>, Row)}.

sentinel(C, N) ->
    exec(C, <<"INSERT INTO inttest_intent_sentinel (id) VALUES ($1)">>, [N]).

sentinel_count(C, N) ->
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(
        C, <<"SELECT COUNT(*) FROM inttest_intent_sentinel WHERE id=$1">>, [N]
    ),
    Count.

exec(C, Sql) -> exec(C, Sql, []).
exec(C, Sql, Params) ->
    {ok, _} = elib_pg:query(C, Sql, Params),
    ok.
