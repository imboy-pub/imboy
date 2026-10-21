-module(talk_record_departure_tests).
-include_lib("eunit/include/eunit.hrl").
-define(ORG, 9791000).
-define(U0, 9791001).
-define(U1, 9791002).
-define(U2, 9791003).
-define(WS, 9791004).
-define(R, 9791010).

departure_test_() ->
    {timeout, 180,
        {setup, fun setup/0, fun cleanup/1, fun(S) ->
            [
                ?_test(registry_mandatory()),
                ?_test(preflight_projection(S)),
                ?_test(cleanup_resources_bounded(S)),
                ?_test(voluntary_and_history(S)),
                ?_test(forced_suspend(S)),
                ?_test(forced_remove(S)),
                ?_test(final_transaction_guard(S)),
                ?_test(create_before_leave(S)),
                ?_test(leave_before_create(S)),
                ?_test(transfer_before_suspend(S)),
                ?_test(suspend_before_transfer(S)),
                ?_test(workspace_revoke_before_transfer(S)),
                ?_test(transfer_before_workspace_revoke(S)),
                ?_test(required_provider_unavailable(S)),
                ?_test(disposition_contract())
            ]
        end}}.

setup() ->
    S = talk_record_t05_support:setup_all(),
    C = maps:get(conn, S),
    q(
        C,
        <<"ALTER TABLE organization_member ADD COLUMN IF NOT EXISTS updated_at timestamptz DEFAULT now()">>,
        []
    ),
    q(C, <<"ALTER TABLE workspace ADD COLUMN IF NOT EXISTS owner_id bigint">>, []),
    lists:foreach(
        fun({Uid, Name}) ->
            ok = talk_record_parent_fixture:user(C, Uid, #{
                <<"nickname">> => Name,
                <<"status">> => 1,
                <<"account_type">> => 0
            })
        end,
        [
            {?U0, <<"departure-owner">>},
            {?U1, <<"departure-member">>},
            {?U2, <<"departure-target">>}
        ]
    ),
    q(
        C,
        <<"INSERT INTO organization(id,name,status,owner_id) VALUES ($1,'departure-test','active',$2)">>,
        [?ORG, ?U0]
    ),
    q(
        C,
        <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES ($1,$2,'owner','active'),($1,$3,'member','active'),($1,$4,'member','active') ON CONFLICT (organization_id,user_id) DO UPDATE SET role=EXCLUDED.role,status=EXCLUDED.status">>,
        [?ORG, ?U0, ?U1, ?U2]
    ),
    q(
        C,
        <<"INSERT INTO workspace(id,organization_id,name,status,owner_id) VALUES ($1,$2,'departure-ws','active',$3)">>,
        [?WS, ?ORG, ?U0]
    ),
    q(
        C,
        <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'member','active'),($1,$3,'member','active')">>,
        [?WS, ?U1, ?U2]
    ),
    S.

cleanup(S) ->
    unload(),
    C = maps:get(conn, S),
    clear_records(C),
    q(C, <<"DELETE FROM workspace_member WHERE workspace_id=$1">>, [?WS]),
    q(C, <<"DELETE FROM workspace WHERE id=$1">>, [?WS]),
    q(C, <<"DELETE FROM organization WHERE id=$1">>, [?ORG]),
    q(C, <<"DELETE FROM organization_member WHERE organization_id=$1">>, [?ORG]),
    q(C, <<"DELETE FROM \"user\" WHERE id IN ($1,$2,$3)">>, [?U0, ?U1, ?U2]),
    talk_record_t05_support:teardown_all(S).

unload() ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [
            talk_record_repo,
            talk_record_delete_logic,
            organization_member_repo,
            talk_record_delete_preflight,
            organization_preflight_facts_pg,
            eb_preflight_facts_pg,
            cs_preflight_facts_pg,
            agent_preflight_facts
        ]
    ),
    application:unset_env(imboy, deletion_preflight_providers).

clear_records(C) ->
    q(
        C,
        <<"DELETE FROM talk_record_audit WHERE record_id IN (SELECT id FROM talk_record WHERE created_by IN ($1,$2))">>,
        [?U1, ?U2]
    ),
    q(
        C,
        <<"DELETE FROM talk_record_grant WHERE record_id IN (SELECT id FROM talk_record WHERE created_by IN ($1,$2))">>,
        [?U1, ?U2]
    ),
    q(C, <<"DELETE FROM talk_record WHERE created_by IN ($1,$2)">>, [?U1, ?U2]),
    q(C, <<"DELETE FROM talk_record_idempotency WHERE actor_id IN ($1,$2)">>, [?U1, ?U2]).
reset(S) ->
    unload(),
    C = maps:get(conn, S),
    clear_records(C),
    q(C, <<"UPDATE organization_member SET status='active' WHERE organization_id=$1">>, [?ORG]),
    q(C, <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1">>, [?WS]),
    ok.
record(S, Ws) ->
    C = maps:get(conn, S),
    q(
        C,
        <<"INSERT INTO talk_record(id,owner_user_id,created_by,client_recording_id,scope,organization_id,workspace_id) VALUES($1,$2,$2,'97910000-0000-4000-8000-000000000001','organization',$3,$4)">>,
        [?R, ?U1, ?ORG, Ws]
    ),
    ok.
q(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, R} -> R;
        E -> error({sql, Sql, E})
    end.
row(S) ->
    [R] = q(
        maps:get(conn, S),
        <<"SELECT owner_user_id,created_by,custody_status,revision,acl_revision FROM talk_record WHERE id=$1">>,
        [?R]
    ),
    R.
codes(F) -> [maps:get(code, B) || B <- maps:get(blockers, F)].

registry_mandatory() ->
    application:unset_env(imboy, deletion_preflight_providers),
    Registry = organization_deletion_preflight:registry(),
    ?assert(lists:member({talk_record, talk_record_delete_preflight, facts}, Registry)),
    ?assert(lists:member(talk_record, organization_deletion_preflight:required_domains())),
    application:set_env(imboy, deletion_preflight_providers, [
        E
     || {D, _, _} = E <- Registry, D =/= talk_record
    ]),
    ?assertMatch(
        {error, #{reason := provider_unregistered, detail := [talk_record]}},
        organization_deletion_preflight:run(?U1)
    ),
    application:unset_env(imboy, deletion_preflight_providers).

preflight_projection(S) ->
    reset(S),
    record(S, null),
    C = maps:get(conn, S),
    q(
        C,
        <<"UPDATE organization_member SET status='suspended' WHERE organization_id=$1 AND user_id=$2">>,
        [?ORG, ?U1]
    ),
    {ok, F} = talk_record_delete_preflight:facts(?U1),
    ?assertEqual(1, maps:get(organization_handover_required, maps:get(counts, F))),
    ?assert(lists:member(<<"TALK_RECORD_ORGANIZATION_OWNER_PENDING">>, codes(F))),
    ?assertEqual(<<"active">>, maps:get(<<"custody_status">>, row(S))).

cleanup_resources_bounded(S) ->
    reset(S),
    C = maps:get(conn, S),
    q(
        C,
        <<"INSERT INTO talk_record(id,owner_user_id,created_by,client_recording_id,scope,deleted_at,cleanup_status) ",
            "SELECT $1+n,$2,$2,('97910000-0000-4000-8000-'||lpad(n::text,12,'0'))::uuid,'personal',now(),'pending' FROM generate_series(1,21) n">>,
        [?R, ?U1]
    ),
    {ok, F} = talk_record_delete_preflight:facts(?U1),
    Resources = maps:get(resources, F),
    ?assertEqual(20, length(Resources)),
    ?assertEqual(#{record_id => ?R + 1}, hd(Resources)),
    ?assertEqual(true, maps:get(cleanup_resources_truncated, F)),
    ?assert(
        lists:all(fun(B) -> maps:get(resource_type, B) =:= <<"account">> end, maps:get(blockers, F))
    ),
    q(C, <<"DELETE FROM talk_record WHERE id=$1">>, [?R + 21]),
    {ok, F20} = talk_record_delete_preflight:facts(?U1),
    ?assertEqual(false, maps:get(cleanup_resources_truncated, F20)).

transfer() ->
    talk_record_transfer_logic:transfer(
        ?U1,
        ?R,
        <<"1">>,
        <<"departure-transfer">>,
        #{<<"target_user_id">> => ?U2, <<"keep_access">> => false}
    ).
voluntary_and_history(S) ->
    reset(S),
    record(S, null),
    ?assertMatch({error, {409, _}}, organization_member_logic:leave(?U1, ?ORG)),
    ?assertMatch({ok, created, _}, transfer()),
    ?assertEqual(?U1, maps:get(<<"created_by">>, row(S))),
    {ok, F} = talk_record_delete_preflight:facts(?U1),
    ?assertNot(lists:member(<<"TALK_RECORD_ORGANIZATION_OWNER_PENDING">>, codes(F))),
    ?assert(lists:member(<<"TALK_RECORD_RETAINED_IDENTITY_PENDING">>, codes(F))),
    ?assertMatch({ok, _}, organization_member_logic:leave(?U1, ?ORG)),
    ?assertEqual(?U2, maps:get(<<"owner_user_id">>, row(S))).

forced_suspend(S) ->
    reset(S),
    record(S, null),
    ?assertMatch({ok, _}, organization_member_logic:suspend(?U0, ?ORG, ?U1)),
    R = row(S),
    ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, R)),
    ?assertEqual(2, maps:get(<<"acl_revision">>, R)),
    ?assertEqual({error, not_found}, talk_record_repo:find_visible(?U1, ?R)),
    ?assertMatch({error, {409, _}}, organization_member_logic:suspend(?U0, ?ORG, ?U0)).

forced_remove(S) ->
    reset(S),
    record(S, null),
    ?assertMatch({ok, _}, organization_member_logic:remove(?U0, ?ORG, ?U1)),
    ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, row(S))),
    ?assertEqual({error, not_found}, talk_record_repo:find_visible(?U1, ?R)).

empty_fact(Uid, D) ->
    {ok, #{
        subject_user_id => Uid,
        domain => D,
        observed_at => os:system_time(millisecond),
        fact_version => 1,
        blockers => [],
        counts => #{}
    }}.
mock_other_facts() ->
    lists:foreach(
        fun({M, F, D}) ->
            case lists:member(M, meck:mocked()) of
                false -> meck:new(M, [passthrough, unstick, no_link]);
                true -> ok
            end,
            meck:expect(M, F, fun(U) -> empty_fact(U, D) end)
        end,
        [
            {organization_preflight_facts_pg, facts_organization, organization},
            {organization_preflight_facts_pg, facts_workspace, workspace},
            {eb_preflight_facts_pg, facts_enterprise_business, enterprise_business},
            {cs_preflight_facts_pg, facts_customer_service, customer_service},
            {agent_preflight_facts, facts_agent, agent}
        ]
    ).
final_transaction_guard(S) ->
    reset(S),
    record(S, null),
    mock_other_facts(),
    {ok, #{blockers := B}} = organization_deletion_preflight:run(?U1),
    ?assert(B =/= []),
    meck:new(talk_record_delete_preflight, [passthrough, unstick, no_link]),
    meck:expect(talk_record_delete_preflight, facts, fun(U) -> empty_fact(U, talk_record) end),
    C = maps:get(conn, S),
    q(C, <<"BEGIN">>, []),
    try
        try user_deletion_executor:execute_main_tx(C, ?U1) of
            Result -> error({unexpected_delete, Result})
        catch
            throw:{abort_tx, {deletion_preflight_blockers, Blockers}} -> ?assert(Blockers =/= [])
        end
    after
        q(C, <<"ROLLBACK">>, [])
    end,
    ?assertEqual(?U1, maps:get(<<"owner_user_id">>, row(S))),
    unload().

validated() ->
    #{
        scope => <<"organization">>,
        organization_id => ?ORG,
        workspace_id => 0,
        client_recording_id => <<"97910000-0000-4000-8000-000000000002">>,
        title => <<"barrier">>,
        language => <<>>,
        processing_mode => <<"record_only">>
    }.
create() ->
    talk_record_ds:create(
        ?U1,
        validated(),
        <<"departure-create">>,
        binary:copy(<<"a">>, 64)
    ).
worker(Label, Fun) ->
    P = self(),
    spawn(fun() ->
        P !
            {Label,
                try
                    Fun()
                catch
                    Class:Reason -> {caught, Class, Reason}
                end}
    end).
result(Label) ->
    receive
        {Label, R} -> R
    after 8000 -> error({worker_timeout, Label})
    end.
barrier(C, Name) ->
    Pid = maps:get(<<"pid">>, hd(q(C, <<"SELECT pg_backend_pid() AS pid">>, []))),
    Owner = whereis(departure_barrier_owner),
    Owner ! {Name, self(), Pid},
    receive
        release -> ok
    after 8000 -> error({barrier_timeout, Name})
    end.
ready(Name) ->
    receive
        {Name, P, Db} -> {P, Db}
    after 8000 -> error({not_at_barrier, Name})
    end.
blocked(S, DbPid) -> blocked(S, DbPid, erlang:monotonic_time(millisecond) + 6000).
blocked(S, DbPid, Deadline) ->
    [#{<<"blocked">> := Waiting}] = q(
        maps:get(conn, S),
        <<"SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND $1::int=ANY(pg_blocking_pids(pid))) AS blocked">>,
        [DbPid]
    ),
    case Waiting of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    erlang:yield(),
                    blocked(S, DbPid, Deadline);
                false ->
                    error(no_lock_wait)
            end
    end.
owner() ->
    case whereis(departure_barrier_owner) of
        undefined -> ok;
        _ -> unregister(departure_barrier_owner)
    end,
    register(departure_barrier_owner, self()).

create_before_leave(S) ->
    reset(S),
    owner(),
    meck:new(talk_record_repo, [passthrough, unstick, no_link]),
    meck:expect(talk_record_repo, insert_tx, fun(C, D) ->
        barrier(C, create_locked),
        meck:passthrough([C, D])
    end),
    worker(create_result, fun create/0),
    {P, Db} = ready(create_locked),
    worker(leave_result, fun() -> organization_member_logic:leave(?U1, ?ORG) end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, created, _, _}, result(create_result)),
    ?assertMatch({error, {409, _}}, result(leave_result)),
    unload().

leave_before_create(S) ->
    reset(S),
    owner(),
    meck:new(talk_record_delete_logic, [passthrough, unstick, no_link]),
    meck:expect(talk_record_delete_logic, ensure_departure_responsibility_tx, fun(C, U, O) ->
        R = meck:passthrough([C, U, O]),
        barrier(C, leave_locked),
        R
    end),
    worker(leave_result, fun() -> organization_member_logic:leave(?U1, ?ORG) end),
    {P, Db} = ready(leave_locked),
    worker(create_result, fun create/0),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, _}, result(leave_result)),
    ?assertEqual({error, context_changed}, result(create_result)),
    unload().

transfer_before_suspend(S) ->
    reset(S),
    record(S, null),
    owner(),
    meck:new(talk_record_repo, [passthrough, unstick, no_link]),
    meck:expect(talk_record_repo, lock_for_transfer_tx, fun(C, R, V) ->
        Locked = meck:passthrough([C, R, V]),
        barrier(C, transfer_locked),
        Locked
    end),
    worker(transfer_result, fun transfer/0),
    {P, Db} = ready(transfer_locked),
    worker(suspend_result, fun() -> organization_member_logic:suspend(?U0, ?ORG, ?U2) end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, created, _}, result(transfer_result)),
    ?assertMatch({ok, _}, result(suspend_result)),
    ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, row(S))),
    unload().

suspend_before_transfer(S) ->
    reset(S),
    record(S, null),
    owner(),
    meck:new(organization_member_repo, [passthrough, unstick, no_link]),
    meck:expect(organization_member_repo, find_for_update_tx, fun(C, O, U, Cols) ->
        Found = meck:passthrough([C, O, U, Cols]),
        case U of
            ?U2 -> barrier(C, suspend_locked);
            _ -> ok
        end,
        Found
    end),
    worker(suspend_result, fun() -> organization_member_logic:suspend(?U0, ?ORG, ?U2) end),
    {P, Db} = ready(suspend_locked),
    worker(transfer_result, fun transfer/0),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, _}, result(suspend_result)),
    ?assertEqual({error, target_not_in_directory}, result(transfer_result)),
    ?assertEqual(?U1, maps:get(<<"owner_user_id">>, row(S))),
    unload().

workspace_revoke_before_transfer(S) ->
    reset(S),
    record(S, ?WS),
    C = maps:get(conn, S),
    q(C, <<"UPDATE workspace_member SET status='active' WHERE workspace_id=$1">>, [?WS]),
    q(C, <<"BEGIN">>, []),
    q(C, <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>, [
        ?WS, ?U2
    ]),
    [#{<<"pid">> := Db}] = q(C, <<"SELECT pg_backend_pid() AS pid">>, []),
    worker(transfer_result, fun transfer/0),
    blocked(S, Db),
    q(C, <<"COMMIT">>, []),
    ?assertEqual({error, target_not_in_directory}, result(transfer_result)),
    ?assertEqual(?U1, maps:get(<<"owner_user_id">>, row(S))).

transfer_before_workspace_revoke(S) ->
    reset(S),
    record(S, ?WS),
    owner(),
    C = maps:get(conn, S),
    q(C, <<"UPDATE workspace_member SET status='active' WHERE workspace_id=$1">>, [?WS]),
    meck:new(talk_record_repo, [passthrough, unstick, no_link]),
    meck:expect(talk_record_repo, lock_for_transfer_tx, fun(Conn, R, V) ->
        Locked = meck:passthrough([Conn, R, V]),
        barrier(Conn, transfer_ws_locked),
        Locked
    end),
    worker(transfer_result, fun transfer/0),
    {P, Db} = ready(transfer_ws_locked),
    worker(revoke_result, fun() ->
        elib_pg:with_tx(fun(Conn) ->
            q(
                Conn,
                <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>,
                [?WS, ?U2]
            )
        end)
    end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, created, _}, result(transfer_result)),
    result(revoke_result),
    ?assertEqual(?U2, maps:get(<<"owner_user_id">>, row(S))),
    ?assertEqual({error, not_found}, talk_record_repo:find_visible(?U2, ?R)),
    {ok, F} = talk_record_delete_preflight:facts(?U2),
    ?assertEqual(1, maps:get(organization_handover_required, maps:get(counts, F))),
    unload().

required_provider_unavailable(S) ->
    reset(S),
    mock_other_facts(),
    meck:new(talk_record_delete_preflight, [passthrough, unstick, no_link]),
    meck:expect(talk_record_delete_preflight, facts, fun(_) -> {error, unavailable} end),
    ?assertMatch(
        {error, #{domain := talk_record, reason := unavailable}},
        organization_deletion_preflight:run(?U1)
    ),
    unload().

disposition_contract() ->
    {ok, Items} = data_disposition:parse_file("docs/compliance/data-disposition.yml"),
    lists:foreach(
        fun(Name) ->
            [Entry] = [I || I <- Items, maps:get(table, I) =:= Name],
            ?assertEqual(retain, maps:get(action, Entry))
        end,
        [
            <<"talk_record">>,
            <<"talk_record_capture">>,
            <<"talk_record_consent_event">>,
            <<"talk_record_audit">>
        ]
    ).
