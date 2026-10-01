%%% Real synthetic PG: invitation -> default resources -> departure -> rejoin.
-module(organization_membership_journey_pg_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-define(ORG, 995902).
-define(OWNER, 995021).
-define(TARGET, 995011).

run(C) ->
    ?assertEqual(undefined, whereis(imboy_domain_event)),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES(995902,'Synthetic full membership',995021,'active')">>
    ),
    {ok, Resources, created} = workspace_ds:create_template(
        ?OWNER, ?ORG, <<"Synthetic default resources">>, undefined
    ),
    #{workspace_id := W, group_id := G, channel_id := Ch} = Resources,
    {ok, W} = organization_default_workspace_app:get(?ORG),
    seed_invitation(C, 995906),
    assert_atomic_failure(C, Resources),
    ?assertMatch({ok, #{already_accepted := false}}, accept()),
    assert_counts(C, Resources, 1),
    ?assertMatch({ok, _}, workspace_logic:detail(?TARGET, W)),
    ?assertMatch({ok, _}, organization_directory_app:list_members(?ORG, ?TARGET, #{})),
    ?assertMatch({ok, #{already_accepted := true}}, accept()),
    assert_generations(C, G, 1, 1),
    ?assertMatch({error, {409, _}}, organization_member_logic:leave(?OWNER, ?ORG)),
    ?assertMatch({ok, #{status := <<"removed">>}}, organization_member_logic:leave(?TARGET, ?ORG)),
    assert_counts(C, Resources, 0),
    ?assertMatch({error, {403, _}}, workspace_logic:detail(?TARGET, W)),
    ?assertEqual(
        {error, <<"insufficient_scope">>},
        organization_directory_app:list_members(?ORG, ?TARGET, #{})
    ),
    assert_generations(C, G, 1, 0),
    ?assertMatch({ok, _}, organization_member_repo:find_active(995101, ?TARGET, <<"role">>)),
    %% An already accepted invitation is historical proof, not a new membership grant.
    ?assertMatch({ok, #{already_accepted := true}}, accept()),
    assert_counts(C, Resources, 0),
    seed_invitation(C, 995907),
    ?assertMatch({ok, #{already_accepted := false}}, accept()),
    assert_counts(C, Resources, 1),
    assert_generations(C, G, 2, 1),
    #{<<"workspace_id">> := W} = intbe02_http_support:one(
        C, <<"SELECT workspace_id FROM channel WHERE id=$1">>, [Ch]
    ),
    ok.

seed_invitation(C, Id) ->
    Digest = organization_invitation:token_digest(integer_to_binary(Id)),
    intbe02_http_support:sql_exec(
        C,
        <<"INSERT INTO organization_invitation(id,organization_id,target_user_id,invited_by,token_digest,status,expires_at) VALUES($1,995902,995011,995021,$2,'pending',clock_timestamp()+interval '1 hour')">>,
        [Id, Digest]
    ).

accept() ->
    organization_invitation_app:accept_targeted(?TARGET, ?ORG, #{
        membership_hook => fun organization_join_orchestrator:membership_hook/2
    }).

assert_atomic_failure(C, Resources) ->
    ok = intbe02_http_support:sql_exec(
        C,
        <<"ALTER TABLE channel_subscription ADD CONSTRAINT synthetic_membership_subscribe_fail CHECK(user_id <> 995011) NOT VALID">>
    ),
    try
        ?assertMatch({error, {500, _}}, accept()),
        assert_counts(C, Resources, 0),
        #{<<"status">> := <<"pending">>} = intbe02_http_support:one(
            C, <<"SELECT status FROM organization_invitation WHERE id=995906">>
        ),
        assert_generations(C, maps:get(group_id, Resources), 0, 0)
    after
        ok = intbe02_http_support:sql_exec(
            C,
            <<"ALTER TABLE channel_subscription DROP CONSTRAINT synthetic_membership_subscribe_fail">>
        )
    end.

assert_counts(C, #{workspace_id := W, group_id := G, channel_id := Ch}, Expected) ->
    Sql =
        <<"SELECT 'org' AS kind,count(*) AS n FROM organization_member WHERE organization_id=995902 AND user_id=995011 AND status='active' UNION ALL SELECT 'workspace',count(*) FROM workspace_member WHERE workspace_id=$1 AND user_id=995011 AND status='active' UNION ALL SELECT 'group',count(*) FROM group_member WHERE group_id=$2 AND user_id=995011 AND status=1 UNION ALL SELECT 'channel',count(*) FROM channel_subscription WHERE channel_id=$3 AND user_id=995011 AND status=1">>,
    {ok, Rows} = elib_pg:query(C, Sql, [W, G, Ch]),
    ?assertEqual(4, length(Rows)),
    [?assertEqual(Expected, maps:get(<<"n">>, Row), Row) || Row <- Rows].

assert_generations(C, G, Total, Open) ->
    #{<<"total">> := ActualTotal, <<"open">> := ActualOpen} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS total,count(*) FILTER(WHERE end_seq IS NULL) AS open FROM group_member_generation WHERE group_id=$1 AND user_id=995011">>,
        [G]
    ),
    ?assertEqual(Total, ActualTotal),
    ?assertEqual(Open, ActualOpen).
