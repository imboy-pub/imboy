%%% Synthetic PG: an invite read cannot outlive revocation while waiting for org locks.
-module(organization_invite_race_pg_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(C) ->
    S = intbe02_http_support,
    ok = S:sql_exec(
        C,
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES (995901,'Synthetic invite race',995001,'active')">>
    ),
    try
        [
            race(C, Entry, Mutation)
         || Entry <- [scoped, global, preview], Mutation <- [revoke, expire]
        ],
        [invitation_accept_race(C, Entry) || Entry <- [token, targeted]],
        concurrent_invitation(C),
        concurrent_join(C),
        ok
    after
        _ = epgsql:squery(C, "ROLLBACK"),
        ok = S:sql_exec(C, <<"DELETE FROM organization WHERE id=995901">>)
    end.

race(C, Entry, Mutation) ->
    S = intbe02_http_support,
    ok = S:sql_exec(C, <<"DELETE FROM organization_invite_code WHERE organization_id=995901">>),
    ok = S:sql_exec(
        C,
        <<"INSERT INTO organization_invite_code(id,organization_id,code,created_by,role,expires_at,status) VALUES(995904,995901,'RACE2345',995001,'member',clock_timestamp()+interval '1 hour','active')">>
    ),
    #{<<"pid">> := Blocker} = S:one(C, <<"SELECT pg_backend_pid() AS pid">>),
    {ok, _, _} = epgsql:squery(C, "BEGIN"),
    _ = S:one(C, <<"SELECT id FROM organization WHERE id=995901 FOR UPDATE">>),
    Ref = make_ref(),
    Parent = self(),
    spawn(fun() -> Parent ! {Ref, join(Entry)} end),
    wait_for_lock(C, Blocker, 1, 100),
    Sql =
        case Mutation of
            revoke ->
                <<"UPDATE organization_invite_code SET status='revoked' WHERE id=995904">>;
            expire ->
                <<"UPDATE organization_invite_code SET expires_at=clock_timestamp()+interval '100 milliseconds' WHERE id=995904">>
        end,
    ok = S:sql_exec(C, Sql),
    case Mutation of
        expire -> timer:sleep(160);
        revoke -> ok
    end,
    {ok, _, _} = epgsql:squery(C, "COMMIT"),
    Result =
        receive
            {Ref, R} -> R
        after 3000 -> error(invite_join_timeout)
        end,
    Expected =
        case Mutation of
            revoke -> 981;
            expire -> 982
        end,
    ?assertMatch({error, {Expected, _}}, Result, {Entry, Mutation, Result}),
    #{<<"n">> := Count} = S:one(
        C,
        <<"SELECT count(*) AS n FROM organization_member WHERE organization_id=995901 AND user_id=995021">>
    ),
    ?assertEqual(0, Count).

join(scoped) -> organization_invite_code_app:join_by_code(995021, 995901, <<"RACE2345">>);
join(global) -> organization_invite_code_app:join_by_code_only(995021, <<"RACE2345">>);
join(preview) -> organization_invite_code_app:preview_by_code(995021, <<"RACE2345">>).

invitation_accept_race(C, Entry) ->
    S = intbe02_http_support,
    seed_invitation(C),
    #{<<"pid">> := Blocker} = S:one(C, <<"SELECT pg_backend_pid() AS pid">>),
    {ok, _, _} = epgsql:squery(C, "BEGIN"),
    _ = S:one(C, <<"SELECT id FROM organization_invitation WHERE id=995905 FOR UPDATE">>),
    Ref = make_ref(),
    Parent = self(),
    spawn(fun() -> Parent ! {Ref, accept_invitation(Entry)} end),
    wait_for_lock(C, Blocker, 1, 100),
    ok = S:sql_exec(
        C,
        <<"UPDATE organization_invitation SET expires_at=clock_timestamp()+interval '100 milliseconds' WHERE id=995905">>
    ),
    timer:sleep(160),
    {ok, _, _} = epgsql:squery(C, "COMMIT"),
    Result =
        receive
            {Ref, R} -> R
        after 3000 -> error(invitation_accept_timeout)
        end,
    ?assertMatch({error, {_, _}}, Result, {Entry, Result}),
    #{<<"status">> := Status} = S:one(
        C, <<"SELECT status FROM organization_invitation WHERE id=995905">>
    ),
    ?assertNotEqual(<<"accepted">>, Status),
    #{<<"n">> := Count} = S:one(
        C,
        <<"SELECT count(*) AS n FROM organization_member WHERE organization_id=995901 AND user_id=995021">>
    ),
    ?assertEqual(0, Count).

seed_invitation(C) ->
    S = intbe02_http_support,
    ok = S:sql_exec(C, <<"DELETE FROM organization_invitation WHERE organization_id=995901">>),
    Digest = organization_invitation:token_digest(<<"SYNTHETIC-INVITE-TOKEN">>),
    ok = S:sql_exec(
        C,
        <<"INSERT INTO organization_invitation(id,organization_id,target_user_id,invited_by,token_digest,status,expires_at) VALUES(995905,995901,995021,995001,$1,'pending',clock_timestamp()+interval '1 hour')">>,
        [Digest]
    ).

concurrent_invitation(C) ->
    S = intbe02_http_support,
    seed_invitation(C),
    #{<<"pid">> := Blocker} = S:one(C, <<"SELECT pg_backend_pid() AS pid">>),
    {ok, _, _} = epgsql:squery(C, "BEGIN"),
    _ = S:one(C, <<"SELECT id FROM organization WHERE id=995901 FOR UPDATE">>),
    Ref = make_ref(),
    Parent = self(),
    [spawn(fun() -> Parent ! {Ref, accept_invitation(E)} end) || E <- [token, targeted]],
    wait_for_lock(C, Blocker, 2, 100),
    {ok, _, _} = epgsql:squery(C, "COMMIT"),
    Results = [
        receive
            {Ref, R} -> R
        after 3000 -> error(invitation_accept_timeout)
        end
     || _ <- [1, 2]
    ],
    [?assertMatch({ok, #{status := <<"accepted">>}}, R) || R <- Results],
    ?assertEqual(
        [false, true], lists:sort([maps:get(already_accepted, V, false) || {ok, V} <- Results])
    ),
    #{<<"n">> := Count} = S:one(
        C,
        <<"SELECT count(*) AS n FROM organization_member WHERE organization_id=995901 AND user_id=995021 AND status='active'">>
    ),
    ?assertEqual(1, Count),
    ?assertMatch({ok, #{already_accepted := true}}, accept_invitation(token)),
    ?assertMatch({ok, #{already_accepted := true}}, accept_invitation(targeted)),
    ok = S:sql_exec(
        C, <<"DELETE FROM organization_member WHERE organization_id=995901 AND user_id=995021">>
    ).

accept_invitation(token) ->
    organization_invitation_app:accept(995021, 995901, <<"SYNTHETIC-INVITE-TOKEN">>, #{
        membership_hook => fun organization_join_orchestrator:membership_hook/2
    });
accept_invitation(targeted) ->
    organization_invitation_app:accept_targeted(995021, 995901, #{
        membership_hook => fun organization_join_orchestrator:membership_hook/2
    }).

concurrent_join(C) ->
    S = intbe02_http_support,
    ok = S:sql_exec(
        C,
        <<"UPDATE organization_invite_code SET status='active',expires_at=clock_timestamp()+interval '1 hour' WHERE id=995904">>
    ),
    #{<<"pid">> := Blocker} = S:one(C, <<"SELECT pg_backend_pid() AS pid">>),
    {ok, _, _} = epgsql:squery(C, "BEGIN"),
    _ = S:one(C, <<"SELECT id FROM organization WHERE id=995901 FOR UPDATE">>),
    Ref = make_ref(),
    Parent = self(),
    [spawn(fun() -> Parent ! {Ref, join(Entry)} end) || Entry <- [scoped, global]],
    wait_for_lock(C, Blocker, 2, 100),
    {ok, _, _} = epgsql:squery(C, "COMMIT"),
    Results = [
        receive
            {Ref, R} -> R
        after 3000 -> error(invite_join_timeout)
        end
     || _ <- [1, 2]
    ],
    [?assertMatch({ok, _, #{organization_id := 995901}}, R) || R <- Results],
    #{<<"n">> := Count} = S:one(
        C,
        <<"SELECT count(*) AS n FROM organization_member WHERE organization_id=995901 AND user_id=995021 AND status='active'">>
    ),
    ?assertEqual(1, Count),
    ?assertMatch({ok, unchanged, _}, join(global)).

wait_for_lock(_, _, _, 0) ->
    error(invite_join_did_not_wait_for_org_lock);
wait_for_lock(C, Blocker, Expected, Attempts) ->
    {ok, [#{<<"n">> := Waiting}]} = elib_pg:query(
        C,
        <<"SELECT count(*) AS n FROM pg_stat_activity WHERE $1 = ANY(pg_blocking_pids(pid))">>,
        [Blocker]
    ),
    case Waiting >= Expected of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait_for_lock(C, Blocker, Expected, Attempts - 1)
    end.
