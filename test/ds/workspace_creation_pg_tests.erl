%%% Actual template creation, owner quota races and rollback on synthetic PG.
-module(workspace_creation_pg_tests).
-include_lib("eunit/include/eunit.hrl").

creation_test_() ->
    {timeout, 900,
        {setup, fun intbe02_http_support:setup_all/0, fun intbe02_http_support:teardown_all/1, fun(
            S
        ) ->
            {timeout, 900, fun() -> creation(S) end}
        end}}.

creation(S) ->
    [elib_tsid:register(T) || T <- [workspace, channel, channel_admin, channel_subscription]],
    C = maps:get(conn, S),
    organization_invite_race_pg_checks:run(C),
    channel_creation_tx_pg_checks:run(C),
    {ok, 2} = workspace_repo:count_by_owner_tx(C, 995001),
    assert_same_request(C),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"INSERT INTO workspace(id,name,owner_id,organization_id,status,branding) SELECT 996000+n,'synthetic-quota-'||n,995001,995101,'active','{}'::jsonb FROM generate_series(1,96) n">>
    ),
    {ok, 99} = workspace_repo:count_by_owner_tx(C, 995001),
    Results = race([<<"Quota A">>, <<"Quota B">>]),
    ?assertEqual(1, length([R || {ok, R, created} <- Results]), {creation_results, Results}),
    [Created] = [R || {ok, R, created} <- Results],
    ?assertEqual(1, length([R || R = {error, owner_workspace_limit} <- Results])),
    {ok, 100} = workspace_repo:count_by_owner_tx(C, 995001),
    WsId = maps:get(workspace_id, Created),
    Name = maps:get(<<"name">>, maps:get(workspace, Created)),
    ?assertMatch(
        {ok, #{workspace_id := WsId}, existing},
        workspace_ds:create_template(995001, 995101, Name, undefined)
    ),
    ?assertMatch(
        {ok, #{workspace_id := WsId}, existing},
        workspace_ds:create_template(995001, 995101, <<"Renamed retry">>, Name)
    ),
    assert_template(C, Created),
    assert_count_failure(C),
    assert_archive(C, Created),
    organization_membership_journey_pg_checks:run(C),
    cs_session_open_pg_checks:run(C),
    customer_service_seat_http_checks:run(S),
    ReportDir = filename:join(os:getenv("IMBOY_GATE_RUN_DIR", "/tmp"), "cs-journey"),
    ok = filelib:ensure_dir(filename:join(ReportDir, "placeholder")),
    ?assertEqual(
        ok,
        eunit:test(
            {"Customer service marker journey", cs_pg_tests:cases({ok, C})},
            [verbose, {report, {eunit_surefire, [{dir, ReportDir}]}}]
        )
    ).

assert_archive(C, #{workspace_id := W} = Created) ->
    ?assertMatch({ok, _}, organization_default_workspace_app:set(995001, 995101, W)),
    ?assertMatch(
        {error, {default_workspace_handover_required, _}},
        elib_pg:with_tx(fun(Tx) -> workspace_ds:archive_tx(Tx, W, null, undefined) end)
    ),
    #{<<"status">> := <<"active">>, <<"archived_by">> := null} =
        intbe02_http_support:one(C, <<"SELECT status, archived_by FROM workspace WHERE id=$1">>, [W]),
    ?assertMatch(
        {error, {default_workspace_handover_invalid, cross_org}},
        elib_pg:with_tx(fun(Tx) -> workspace_ds:archive_tx(Tx, W, null, 995211) end)
    ),
    {ok, W} = organization_default_workspace_app:get(995101),
    assert_archive_db_failure(C, W),
    ?assertMatch(
        {ok, #{status := <<"archived">>, archived_by := null}},
        workspace_logic:admin_archive(995999, W, #{replacement_workspace_id => 995201})
    ),
    {ok, 995201} = organization_default_workspace_app:get(995101),
    #{<<"status">> := <<"archived">>, <<"archived_by">> := null} =
        intbe02_http_support:one(C, <<"SELECT status, archived_by FROM workspace WHERE id=$1">>, [W]),
    ?assertMatch({error, {409, _}}, workspace_logic:admin_archive(995999, W)),
    assert_template(C, Created).

assert_archive_db_failure(C, W) ->
    ok = intbe02_http_support:sql_exec(
        C,
        <<"ALTER TABLE workspace ADD CONSTRAINT synthetic_archive_reject CHECK(status <> 'archived') NOT VALID">>
    ),
    try
        ?assertMatch(
            {error, _},
            elib_pg:with_tx(fun(Tx) -> workspace_ds:archive_tx(Tx, W, null, 995201) end)
        ),
        #{<<"status">> := <<"active">>} = intbe02_http_support:one(
            C,
            <<"SELECT status FROM workspace WHERE id=$1">>,
            [W]
        ),
        {ok, W} = organization_default_workspace_app:get(995101)
    after
        ok = intbe02_http_support:sql_exec(
            C,
            <<"ALTER TABLE workspace DROP CONSTRAINT synthetic_archive_reject">>
        )
    end.

race(Names) ->
    Parent = self(),
    Workers = [
        spawn(fun() ->
            receive
                go ->
                    Parent ! {self(), workspace_ds:create_template(995001, 995101, Name, Name)}
            end
        end)
     || Name <- Names
    ],
    [Pid ! go || Pid <- Workers],
    [
        receive
            {Pid, R} -> R
        after 10000 -> error(workspace_creation_timeout)
        end
     || Pid <- Workers
    ].

assert_same_request(C) ->
    Results = race([<<"Same request">>, <<"Same request">>]),
    ?assertEqual(1, length([R || {ok, R, created} <- Results])),
    ?assertEqual(1, length([R || {ok, R, existing} <- Results])),
    [W, W] = [maps:get(workspace_id, R) || {ok, R, _} <- Results],
    {ok, 3} = workspace_repo:count_by_owner_tx(C, 995001).

assert_template(C, #{workspace_id := W, group_id := G, channel_id := Ch}) ->
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM workspace_member WHERE workspace_id=$1 AND user_id=995001 AND role='owner' AND status='active'">>,
        [W]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM \"group\" WHERE id=$1 AND workspace_id=$2 AND scope='workspace'">>,
        [G, W]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel WHERE id=$1 AND workspace_id=$2 AND scope='workspace'">>,
        [Ch, W]
    ).

assert_count_failure(C) ->
    ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"CREATE ROLE synthetic_quota_count_reader NOLOGIN">>
    ),
    try
        ok = intbe02_http_support:sql_exec(C, <<"SET LOCAL ROLE synthetic_quota_count_reader">>),
        ?assertMatch({error, _}, workspace_repo:count_by_owner_tx(C, 995001))
    after
        ok = intbe02_http_support:sql_exec(C, <<"ROLLBACK">>)
    end,
    {ok, 100} = workspace_repo:count_by_owner_tx(C, 995001).
