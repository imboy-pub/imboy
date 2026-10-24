-module(talk_record_departure_tests).
-include_lib("eunit/include/eunit.hrl").

departure_test_() ->
    {timeout, 180,
        {foreach, fun setup/0, fun cleanup/1, [
            fun(S) -> ?_test(registry_mandatory(S)) end,
            fun(S) -> ?_test(preflight_projection(S)) end,
            fun(S) -> ?_test(cleanup_resources_bounded(S)) end,
            fun(S) -> ?_test(voluntary_and_history(S)) end,
            fun(S) -> ?_test(forced_suspend(S)) end,
            fun(S) -> ?_test(forced_remove(S)) end,
            fun(S) -> ?_test(final_transaction_guard(S)) end,
            fun(S) -> ?_test(create_before_leave(S)) end,
            fun(S) -> ?_test(leave_before_create(S)) end,
            fun(S) -> ?_test(transfer_before_suspend(S)) end,
            fun(S) -> ?_test(suspend_before_transfer(S)) end,
            fun(S) -> ?_test(workspace_revoke_before_transfer(S)) end,
            fun(S) -> ?_test(transfer_before_workspace_revoke(S)) end,
            fun(S) -> ?_test(required_provider_unavailable(S)) end,
            fun(_S) -> ?_test(disposition_contract()) end
        ]}}.

setup() ->
    State = talk_record_t05_support:setup_all(),
    S = State#{ids => new_ids()},
    talk_record_setup_fixture:prepare(
        S,
        fun() ->
            seed_parents(S),
            S
        end,
        fun talk_record_t05_support:teardown_all/1
    ).

new_ids() ->
    %% Separate from the T05 allocator's 9e12 range; one disjoint block per case.
    Base = 8000000000000000 + 256 * erlang:unique_integer([positive, monotonic]),
    #{
        org => Base,
        u0 => Base + 1,
        u1 => Base + 2,
        u2 => Base + 3,
        ws => Base + 4,
        record_id => Base + 10,
        record_client_uuid => client_uuid(Base + 10),
        create_client_uuid => client_uuid(Base + 200),
        create_key => <<"departure-create-", (integer_to_binary(Base))/binary>>,
        transfer_key => <<"departure-transfer-", (integer_to_binary(Base))/binary>>
    }.

id(S, Name) -> maps:get(Name, maps:get(ids, S)).

client_uuid(Id) ->
    <<High:4/binary, Low:12/binary>> = binary:encode_hex(<<Id:64>>, lowercase),
    <<"97910000-", High/binary, "-4000-8000-", Low/binary>>.

seed_parents(S) ->
    C = maps:get(conn, S),
    [] = q(
        C,
        <<"SELECT id FROM \"user\" WHERE id IN ($1,$2,$3)">>,
        [id(S, u0), id(S, u1), id(S, u2)]
    ),
    lists:foreach(
        fun({Uid, Name}) ->
            ok = talk_record_parent_fixture:user(C, Uid, #{
                <<"nickname">> => Name, <<"status">> => 1, <<"account_type">> => 0
            })
        end,
        [
            {id(S, u0), <<"departure-owner">>},
            {id(S, u1), <<"departure-member">>},
            {id(S, u2), <<"departure-target">>}
        ]
    ),
    q(
        C,
        <<"INSERT INTO organization(id,name,status,owner_id) VALUES ($1,'departure-test','active',$2)">>,
        [id(S, org), id(S, u0)]
    ),
    ?assertMatch(
        [#{<<"role">> := <<"owner">>, <<"status">> := <<"active">>}],
        q(
            C,
            <<"SELECT role,status FROM organization_member WHERE organization_id=$1 AND user_id=$2">>,
            [id(S, org), id(S, u0)]
        )
    ),
    q(
        C,
        <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES ($1,$2,'member','active'),($1,$3,'member','active')">>,
        [id(S, org), id(S, u1), id(S, u2)]
    ),
    q(
        C,
        <<"INSERT INTO workspace(id,organization_id,name,status,owner_id) VALUES ($1,$2,'departure-ws','active',$3)">>,
        [id(S, ws), id(S, org), id(S, u0)]
    ),
    q(
        C,
        <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'member','active'),($1,$3,'member','active')">>,
        [id(S, ws), id(S, u1), id(S, u2)]
    ),
    ok.

cleanup(S) ->
    unload(),
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

reset(S) ->
    unload(),
    C = maps:get(conn, S),
    q(C, <<"UPDATE organization_member SET status='active' WHERE organization_id=$1">>, [id(S, org)]),
    q(C, <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1">>, [id(S, ws)]),
    ok.

record(S, Ws) ->
    q(
        maps:get(conn, S),
        <<"INSERT INTO talk_record(id,owner_user_id,created_by,client_recording_id,scope,organization_id,workspace_id) VALUES($1,$2,$2,$3,'organization',$4,$5)">>,
        [id(S, record_id), id(S, u1), id(S, record_client_uuid), id(S, org), Ws]
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
        [id(S, record_id)]
    ),
    R.
codes(F) -> [maps:get(code, B) || B <- maps:get(blockers, F)].

registry_mandatory(S) ->
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
        organization_deletion_preflight:run(id(S, u1))
    ),
    application:unset_env(imboy, deletion_preflight_providers).

preflight_projection(S) ->
    reset(S),
    record(S, null),
    C = maps:get(conn, S),
    q(
        C,
        <<"UPDATE organization_member SET status='suspended' WHERE organization_id=$1 AND user_id=$2">>,
        [id(S, org), id(S, u1)]
    ),
    {ok, F} = talk_record_delete_preflight:facts(id(S, u1)),
    ?assertEqual(1, maps:get(organization_handover_required, maps:get(counts, F))),
    ?assert(lists:member(<<"TALK_RECORD_ORGANIZATION_OWNER_PENDING">>, codes(F))),
    ?assertEqual(<<"active">>, maps:get(<<"custody_status">>, row(S))).

cleanup_resources_bounded(S) ->
    reset(S),
    %% Independent 21/20-record cohorts retain both histories instead of deleting
    %% the 21st record to change a previous observation for the same actor.
    R = id(S, record_id),
    Rows =
        [{R + N, id(S, u1)} || N <- lists:seq(1, 21)] ++
            [{R + 100 + N, id(S, u2)} || N <- lists:seq(1, 20)],
    insert_cleanup_cohorts(S, Rows),
    {ok, F} = talk_record_delete_preflight:facts(id(S, u1)),
    Resources = maps:get(resources, F),
    ?assertEqual(20, length(Resources)),
    ?assertEqual(#{record_id => R + 1}, hd(Resources)),
    ?assertEqual(true, maps:get(cleanup_resources_truncated, F)),
    ?assert(
        lists:all(fun(B) -> maps:get(resource_type, B) =:= <<"account">> end, maps:get(blockers, F))
    ),
    {ok, F20} = talk_record_delete_preflight:facts(id(S, u2)),
    ?assertEqual(20, length(maps:get(resources, F20))),
    ?assertEqual(false, maps:get(cleanup_resources_truncated, F20)).

insert_cleanup_cohorts(S, Rows) ->
    Values = [
        io_lib:format(
            "($~B,$~B,$~B,$~B::uuid,'personal',now(),'pending')", [N, N + 1, N + 1, N + 2]
        )
     || N <- lists:seq(1, length(Rows) * 3, 3)
    ],
    Sql = iolist_to_binary([
        "INSERT INTO talk_record(id,owner_user_id,created_by,client_recording_id,scope,deleted_at,cleanup_status) VALUES ",
        lists:join(",", Values)
    ]),
    Params = lists:append([[Rid, Uid, client_uuid(Rid)] || {Rid, Uid} <- Rows]),
    q(maps:get(conn, S), Sql, Params).

transfer(S) ->
    talk_record_transfer_logic:transfer(
        id(S, u1),
        id(S, record_id),
        <<"1">>,
        id(S, transfer_key),
        #{<<"target_user_id">> => id(S, u2), <<"keep_access">> => false}
    ).
voluntary_and_history(S) ->
    reset(S),
    record(S, null),
    ?assertMatch({error, {409, _}}, organization_member_logic:leave(id(S, u1), id(S, org))),
    ?assertMatch({ok, created, _}, transfer(S)),
    ?assertEqual(id(S, u1), maps:get(<<"created_by">>, row(S))),
    {ok, F} = talk_record_delete_preflight:facts(id(S, u1)),
    ?assertNot(lists:member(<<"TALK_RECORD_ORGANIZATION_OWNER_PENDING">>, codes(F))),
    ?assert(lists:member(<<"TALK_RECORD_RETAINED_IDENTITY_PENDING">>, codes(F))),
    ?assertMatch({ok, _}, organization_member_logic:leave(id(S, u1), id(S, org))),
    ?assertEqual(id(S, u2), maps:get(<<"owner_user_id">>, row(S))).

forced_suspend(S) ->
    reset(S),
    record(S, null),
    ?assertMatch({ok, _}, organization_member_logic:suspend(id(S, u0), id(S, org), id(S, u1))),
    R = row(S),
    ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, R)),
    ?assertEqual(2, maps:get(<<"acl_revision">>, R)),
    ?assertEqual({error, not_found}, talk_record_repo:find_visible(id(S, u1), id(S, record_id))),
    ?assertMatch(
        {error, {409, _}}, organization_member_logic:suspend(id(S, u0), id(S, org), id(S, u0))
    ).

forced_remove(S) ->
    reset(S),
    record(S, null),
    ?assertMatch({ok, _}, organization_member_logic:remove(id(S, u0), id(S, org), id(S, u1))),
    ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, row(S))),
    ?assertEqual({error, not_found}, talk_record_repo:find_visible(id(S, u1), id(S, record_id))).

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
    {ok, #{blockers := B}} = organization_deletion_preflight:run(id(S, u1)),
    ?assert(B =/= []),
    meck:new(talk_record_delete_preflight, [passthrough, unstick, no_link]),
    meck:expect(talk_record_delete_preflight, facts, fun(U) -> empty_fact(U, talk_record) end),
    C = maps:get(conn, S),
    q(C, <<"BEGIN">>, []),
    try
        try user_deletion_executor:execute_main_tx(C, id(S, u1)) of
            Result -> error({unexpected_delete, Result})
        catch
            throw:{abort_tx, {deletion_preflight_blockers, Blockers}} -> ?assert(Blockers =/= [])
        end
    after
        q(C, <<"ROLLBACK">>, [])
    end,
    ?assertEqual(id(S, u1), maps:get(<<"owner_user_id">>, row(S))),
    unload().

validated(S) ->
    #{
        scope => <<"organization">>,
        organization_id => id(S, org),
        workspace_id => 0,
        client_recording_id => id(S, create_client_uuid),
        title => <<"barrier">>,
        language => <<>>,
        policy_version => maps:get(<<"policy_version">>, talk_record_ds:capabilities()),
        processing_mode => <<"record_only">>
    }.
create(S) ->
    talk_record_ds:create(
        id(S, u1),
        validated(S),
        id(S, create_key),
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
barrier(Owner, C, Name) ->
    Pid = maps:get(<<"pid">>, hd(q(C, <<"SELECT pg_backend_pid() AS pid">>, []))),
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
create_before_leave(S) ->
    reset(S),
    Owner = self(),
    meck:new(talk_record_repo, [passthrough, unstick, no_link]),
    meck:expect(talk_record_repo, insert_tx, fun(C, D) ->
        barrier(Owner, C, create_locked),
        meck:passthrough([C, D])
    end),
    worker(create_result, fun() -> create(S) end),
    {P, Db} = ready(create_locked),
    worker(leave_result, fun() -> organization_member_logic:leave(id(S, u1), id(S, org)) end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, created, _, _}, result(create_result)),
    ?assertMatch({error, {409, _}}, result(leave_result)),
    unload().

leave_before_create(S) ->
    reset(S),
    Owner = self(),
    meck:new(talk_record_delete_logic, [passthrough, unstick, no_link]),
    meck:expect(talk_record_delete_logic, ensure_departure_responsibility_tx, fun(C, U, O) ->
        R = meck:passthrough([C, U, O]),
        barrier(Owner, C, leave_locked),
        R
    end),
    worker(leave_result, fun() -> organization_member_logic:leave(id(S, u1), id(S, org)) end),
    {P, Db} = ready(leave_locked),
    worker(create_result, fun() -> create(S) end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, _}, result(leave_result)),
    ?assertEqual({error, context_changed}, result(create_result)),
    unload().

transfer_before_suspend(S) ->
    reset(S),
    record(S, null),
    Owner = self(),
    meck:new(talk_record_repo, [passthrough, unstick, no_link]),
    meck:expect(talk_record_repo, lock_for_transfer_tx, fun(C, R, V) ->
        Locked = meck:passthrough([C, R, V]),
        barrier(Owner, C, transfer_locked),
        Locked
    end),
    worker(transfer_result, fun() -> transfer(S) end),
    {P, Db} = ready(transfer_locked),
    worker(suspend_result, fun() ->
        organization_member_logic:suspend(id(S, u0), id(S, org), id(S, u2))
    end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, created, _}, result(transfer_result)),
    ?assertMatch({ok, _}, result(suspend_result)),
    ?assertEqual(<<"handover_required">>, maps:get(<<"custody_status">>, row(S))),
    unload().

suspend_before_transfer(S) ->
    reset(S),
    record(S, null),
    Owner = self(),
    meck:new(organization_member_repo, [passthrough, unstick, no_link]),
    meck:expect(organization_member_repo, find_for_update_tx, fun(C, O, U, Cols) ->
        Found = meck:passthrough([C, O, U, Cols]),
        case U =:= id(S, u2) of
            true -> barrier(Owner, C, suspend_locked);
            false -> ok
        end,
        Found
    end),
    worker(suspend_result, fun() ->
        organization_member_logic:suspend(id(S, u0), id(S, org), id(S, u2))
    end),
    {P, Db} = ready(suspend_locked),
    worker(transfer_result, fun() -> transfer(S) end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, _}, result(suspend_result)),
    ?assertEqual({error, target_not_in_directory}, result(transfer_result)),
    ?assertEqual(id(S, u1), maps:get(<<"owner_user_id">>, row(S))),
    unload().

workspace_revoke_before_transfer(S) ->
    reset(S),
    record(S, id(S, ws)),
    C = maps:get(conn, S),
    q(C, <<"UPDATE workspace_member SET status='active' WHERE workspace_id=$1">>, [id(S, ws)]),
    q(C, <<"BEGIN">>, []),
    q(C, <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>, [
        id(S, ws), id(S, u2)
    ]),
    [#{<<"pid">> := Db}] = q(C, <<"SELECT pg_backend_pid() AS pid">>, []),
    worker(transfer_result, fun() -> transfer(S) end),
    blocked(S, Db),
    q(C, <<"COMMIT">>, []),
    ?assertEqual({error, target_not_in_directory}, result(transfer_result)),
    ?assertEqual(id(S, u1), maps:get(<<"owner_user_id">>, row(S))).

transfer_before_workspace_revoke(S) ->
    reset(S),
    record(S, id(S, ws)),
    Owner = self(),
    C = maps:get(conn, S),
    q(C, <<"UPDATE workspace_member SET status='active' WHERE workspace_id=$1">>, [id(S, ws)]),
    meck:new(talk_record_repo, [passthrough, unstick, no_link]),
    meck:expect(talk_record_repo, lock_for_transfer_tx, fun(Conn, R, V) ->
        Locked = meck:passthrough([Conn, R, V]),
        barrier(Owner, Conn, transfer_ws_locked),
        Locked
    end),
    worker(transfer_result, fun() -> transfer(S) end),
    {P, Db} = ready(transfer_ws_locked),
    worker(revoke_result, fun() ->
        elib_pg:with_tx(fun(Conn) ->
            q(
                Conn,
                <<"UPDATE workspace_member SET status='removed' WHERE workspace_id=$1 AND user_id=$2">>,
                [id(S, ws), id(S, u2)]
            )
        end)
    end),
    blocked(S, Db),
    P ! release,
    ?assertMatch({ok, created, _}, result(transfer_result)),
    result(revoke_result),
    ?assertEqual(id(S, u2), maps:get(<<"owner_user_id">>, row(S))),
    ?assertEqual({error, not_found}, talk_record_repo:find_visible(id(S, u2), id(S, record_id))),
    {ok, F} = talk_record_delete_preflight:facts(id(S, u2)),
    ?assertEqual(1, maps:get(organization_handover_required, maps:get(counts, F))),
    unload().

required_provider_unavailable(S) ->
    reset(S),
    mock_other_facts(),
    meck:new(talk_record_delete_preflight, [passthrough, unstick, no_link]),
    meck:expect(talk_record_delete_preflight, facts, fun(_) -> {error, unavailable} end),
    ?assertMatch(
        {error, #{domain := talk_record, reason := unavailable}},
        organization_deletion_preflight:run(id(S, u1))
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
