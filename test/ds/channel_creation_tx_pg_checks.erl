%% Caller-owned transaction: real channel writes, creator identity and rollback.
-module(channel_creation_tx_pg_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(C) ->
    Name = <<"synthetic-channel-tx">>,
    Opts = #{scope => <<"workspace">>, workspace_id => 995201},
    {ok, Ch} = elib_pg:with_tx(fun(Tx) ->
        {ok, Id} = channel_ds:create_channel_tx(Tx, 995001, Name, Opts),
        assert_creator(Tx, Id),
        {ok, Id}
    end),
    assert_creator(C, Ch),
    ?assertEqual(
        {error, synthetic_audit_failure},
        elib_pg:with_tx(fun(Tx) ->
            {ok, Id} = channel_ds:create_channel_tx(Tx, 995001, <<"synthetic-rollback">>, Opts),
            assert_creator(Tx, Id),
            throw({abort_tx, synthetic_audit_failure})
        end)
    ),
    assert_absent(C, <<"synthetic-rollback">>),
    reject_admin(C, Opts),
    reject_archived_workspace(C, Opts),
    assert_creator(C, Ch),
    assert_quota(C),
    ok.

assert_creator(C, Ch) ->
    #{<<"subscriber_count">> := 1, <<"creator_uid">> := 995001} =
        intbe02_http_support:one(
            C,
            <<"SELECT subscriber_count, creator_uid FROM channel WHERE id=$1">>,
            [Ch]
        ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel_subscription WHERE channel_id=$1 AND user_id=995001 AND status=1">>,
        [Ch]
    ),
    #{<<"n">> := 1} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel_admin WHERE channel_id=$1 AND user_id=995001 AND role=3">>,
        [Ch]
    ).

assert_absent(C, Name) ->
    #{<<"n">> := 0} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel WHERE name=$1">>,
        [Name]
    ),
    #{<<"n">> := 0} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel_subscription s LEFT JOIN channel c ON c.id=s.channel_id WHERE c.id IS NULL">>,
        []
    ),
    #{<<"n">> := 0} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM channel_admin a LEFT JOIN channel c ON c.id=a.channel_id WHERE c.id IS NULL">>,
        []
    ).

reject_admin(C, Opts) ->
    ok = intbe02_http_support:sql_exec(
        C,
        <<"ALTER TABLE channel_admin ADD CONSTRAINT synthetic_creator_reject CHECK(role<>3) NOT VALID">>
    ),
    try
        ?assertMatch(
            {error, _},
            elib_pg:with_tx(fun(Tx) ->
                channel_ds:create_channel_tx(Tx, 995001, <<"synthetic-admin-reject">>, Opts)
            end)
        ),
        assert_absent(C, <<"synthetic-admin-reject">>)
    after
        ok = intbe02_http_support:sql_exec(
            C,
            <<"ALTER TABLE channel_admin DROP CONSTRAINT synthetic_creator_reject">>
        )
    end.

reject_archived_workspace(C, Opts) ->
    ?assertMatch(
        {error, {980, _}},
        elib_pg:with_tx(fun(Tx) ->
            {ok, 1} = elib_pg:query(
                Tx,
                <<"UPDATE workspace SET status='archived' WHERE id=$1">>,
                [995201]
            ),
            channel_ds:create_channel_tx(Tx, 995001, <<"synthetic-archived-reject">>, Opts)
        end)
    ),
    assert_absent(C, <<"synthetic-archived-reject">>),
    #{<<"status">> := <<"active">>} = intbe02_http_support:one(
        C,
        <<"SELECT status FROM workspace WHERE id=$1">>,
        [995201]
    ).

assert_quota(C) ->
    {ok, Before} = channel_repo:count_managed_tx(C, 995001),
    Parent = self(),
    Opts = #{max_channels => Before + 1},
    Workers = [
        spawn(fun() ->
            receive
                go -> Parent ! {self(), channel_ds:create_channel(995001, Name, Opts)}
            end
        end)
     || Name <- [<<"synthetic-quota-a">>, <<"synthetic-quota-b">>]
    ],
    [P ! go || P <- Workers],
    Results = [
        receive
            {P, R} -> R
        after 10000 -> error(channel_quota_race_timeout)
        end
     || P <- Workers
    ],
    ?assertEqual(1, length([Id || {ok, Id} <- Results])),
    ?assertEqual(1, length([E || E = {error, <<"已达频道创建上限"/utf8>>} <- Results])),
    {ok, After} = channel_repo:count_managed_tx(C, 995001),
    ?assertEqual(Before + 1, After),
    [Id] = [I || {ok, I} <- Results],
    assert_creator(C, Id),
    ?assertEqual(
        {error, channel_creation_limit},
        elib_pg:with_tx(fun(Tx) ->
            channel_ds:create_channel_tx(Tx, 995001, <<"synthetic-quota-full">>, Opts)
        end)
    ),
    assert_absent(C, <<"synthetic-quota-full">>),
    ?assertEqual(
        {error, invalid_channel_limit},
        elib_pg:with_tx(fun(Tx) ->
            channel_ds:create_channel_tx(Tx, 995001, <<"synthetic-quota-invalid">>, #{
                max_channels => 0
            })
        end)
    ),
    assert_absent(C, <<"synthetic-quota-invalid">>),
    assert_count_failure(C).

assert_count_failure(C) ->
    ok = intbe02_http_support:sql_exec(C, <<"CREATE ROLE synthetic_channel_count_reader NOLOGIN">>),
    try
        ?assertMatch(
            {error, {channel_quota_count_failed, _}},
            elib_pg:with_tx(fun(Tx) ->
                {ok, _} = elib_pg:query(
                    Tx, <<"SET LOCAL ROLE synthetic_channel_count_reader">>, []
                ),
                channel_ds:create_channel_tx(Tx, 995001, <<"synthetic-quota-count-error">>, #{})
            end)
        ),
        assert_absent(C, <<"synthetic-quota-count-error">>)
    after
        ok = intbe02_http_support:sql_exec(C, <<"DROP ROLE synthetic_channel_count_reader">>)
    end.
