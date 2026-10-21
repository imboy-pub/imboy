-module(enterprise_source_person_r20_default_preflight_pg_tests).
-include_lib("eunit/include/eunit.hrl").

default_preflight_executor_test_() ->
    {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.

run() ->
    ?assertEqual(undefined, whereis(pgsql)),
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"R20_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Providers = application:get_env(imboy, deletion_preflight_providers),
    try
        application:unset_env(imboy, deletion_preflight_providers),
        {ok, _} = application:ensure_all_started(pooler),
        start_pool(State),
        try
            exercise(maps:get(conn, State))
        after
            pooler:rm_pool(pgsql)
        end
    after
        restore_env(Providers),
        inttest_marker_db:release(State)
    end.

start_pool(State) ->
    Server = maps:get(server, State),
    Options = maps:merge(
        maps:with([host, port, username, password], Server),
        maps:get(connect_extra, Server, #{})
    ),
    {ok, Pool} = pooler:new_pool(#{
        name => pgsql,
        init_count => 2,
        max_count => 4,
        start_mfa => {epgsql, connect, [Options#{database => maps:get(db, State)}]}
    }),
    ?assert(is_pid(Pool)).

exercise(Conn) ->
    assert_default_registry(),
    AppId = seed(Conn),
    UserId = 977002,
    ExternalId = <<"default-preflight-deletion">>,
    {ok, _} = enterprise_source_person_repo:claim_tx(Conn, AppId, ExternalId, UserId),
    {ok, _} = enterprise_external_identity_repo:bind_tx(Conn, 977100, AppId, ExternalId, UserId),
    {ok, #{blockers := Blockers, facts := Facts}} = organization_deletion_preflight:run(UserId),
    ?assertEqual(
        organization_deletion_preflight:required_domains(),
        [maps:get(domain, F) || F <- Facts]
    ),
    ?assert(
        lists:any(
            fun(Blocker) -> maps:get(code, Blocker) =:= <<"ORG_MEMBERSHIP_ACTIVE">> end, Blockers
        )
    ),
    assert_blocked(Conn, UserId),
    {ok, [_]} = query(Conn, <<"SELECT id FROM \"user\" WHERE id=$1">>, [UserId]),
    {ok, [_]} = query(Conn, <<"SELECT id FROM enterprise_external_identity WHERE user_id=$1">>, [
        UserId
    ]),
    {ok, _} = query(Conn, <<"UPDATE organization_member SET status='removed' WHERE user_id=$1">>, [
        UserId
    ]),
    {ok, #{blockers := [], facts := ReadyFacts}} = organization_deletion_preflight:run(UserId),
    ?assertEqual(
        organization_deletion_preflight:required_domains(),
        [maps:get(domain, F) || F <- ReadyFacts]
    ),
    assert_deleted(Conn, AppId, ExternalId, UserId).

assert_default_registry() ->
    ?assertEqual(
        [
            {organization, organization_preflight_facts_pg, facts_organization},
            {workspace, organization_preflight_facts_pg, facts_workspace},
            {enterprise_business, eb_preflight_facts_pg, facts_enterprise_business},
            {customer_service, cs_preflight_facts_pg, facts_customer_service},
            {agent, agent_preflight_facts, facts_agent},
            {talk_record, talk_record_delete_preflight, facts}
        ],
        organization_deletion_preflight:registry()
    ).

assert_blocked(Conn, UserId) ->
    {ok, _} = query(Conn, <<"BEGIN">>, []),
    try
        try user_deletion_executor:execute_main_tx(Conn, UserId) of
            Result -> error({active_membership_deletion_allowed, Result})
        catch
            throw:{abort_tx, {deletion_preflight_blockers, Blockers}} ->
                ?assert(Blockers =/= [])
        end
    after
        {ok, _} = query(Conn, <<"ROLLBACK">>, [])
    end.

assert_deleted(Conn, AppId, ExternalId, UserId) ->
    {ok, _} = query(Conn, <<"BEGIN">>, []),
    try
        ok = user_deletion_executor:execute_main_tx(Conn, UserId),
        {ok, []} = query(Conn, <<"SELECT id FROM \"user\" WHERE id=$1">>, [UserId]),
        {ok, []} = query(
            Conn, <<"SELECT id FROM enterprise_external_identity WHERE user_id=$1">>, [UserId]
        ),
        {ok, Person} = enterprise_source_person_repo:find_tx(Conn, AppId, ExternalId),
        ?assertEqual(true, maps:get(<<"binding_retired">>, Person)),
        ?assertEqual(null, maps:get(<<"user_id">>, Person)),
        ?assertEqual(<<"removed">>, maps:get(<<"status">>, Person)),
        {ok, _} = query(Conn, <<"COMMIT">>, [])
    catch
        Class:Reason:Stack ->
            query(Conn, <<"ROLLBACK">>, []),
            erlang:raise(Class, Reason, Stack)
    end,
    {ok, []} = query(Conn, <<"SELECT id FROM \"user\" WHERE id=$1">>, [UserId]),
    {ok, #{<<"binding_retired">> := true}} = enterprise_source_person_repo:find_tx(
        Conn, AppId, ExternalId
    ).

seed(Conn) ->
    lists:foreach(
        fun(UserId) ->
            {ok, _} = query(
                Conn,
                <<"INSERT INTO \"user\" (id,password,account,account_type,status,reg_ip,reg_cosv) VALUES ($1,'x',$2,0,1,'127.0.0.1','x')">>,
                [UserId, <<"r20-default-", (integer_to_binary(UserId))/binary>>]
            )
        end,
        [977001, 977002]
    ),
    {ok, _} = query(
        Conn,
        <<"INSERT INTO organization (id,name,owner_id,status,branding,settings) VALUES (977100,'r20-default',977001,'active','{}','{}')">>,
        []
    ),
    {ok, _} = query(
        Conn,
        <<"INSERT INTO organization_member (organization_id,user_id,role,status) VALUES (977100,977002,'member','active')">>,
        []
    ),
    {ok, _} = enterprise_application_repo:create_tx(
        Conn, 977100, <<"r20-default-source">>, <<"R20 default">>
    ),
    {ok, [#{<<"id">> := AppId}]} = query(
        Conn,
        <<"SELECT id FROM enterprise_application WHERE application_key='r20-default-source'">>,
        []
    ),
    AppId.

restore_env(undefined) -> application:unset_env(imboy, deletion_preflight_providers);
restore_env({ok, Providers}) -> application:set_env(imboy, deletion_preflight_providers, Providers).

query(Conn, Sql, Params) -> elib_pg:query(Conn, Sql, Params).
