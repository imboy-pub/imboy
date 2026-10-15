-module(eb_pg_asset_delete_queue_tests).
-include_lib("eunit/include/eunit.hrl").

worker_result_test_() ->
    [
        {timeout, 35,
            ?_test(pure_mock_fixture:run(fun() -> worker_case(Delete, Ack, Expected) end))}
     || {Delete, Ack, Expected} <- [
            {ok, <<"done">>, {ok, []}},
            {{error, not_found}, <<"done">>, {ok, []}},
            {{error, denied}, <<"retry_wait">>, {error, {object_delete_failed, {error, denied}}}},
            {ok, <<"stale_token">>, {error, {object_ack_failed, <<"stale_token">>}}},
            {identity_changed, <<"retry_wait">>,
                {error, {object_delete_failed, {error, storage_identity_changed}}}}
        ]
    ].

worker_case(Delete, Ack, Expected) ->
    Row = #{
        <<"out_organization_id">> => 10,
        <<"out_workspace_id">> => 20,
        <<"out_asset_id">> => 30,
        <<"out_object_key">> => <<"synthetic/key">>,
        <<"out_storage_namespace">> => <<"alias">>,
        <<"out_storage_container">> => <<"bucket">>,
        <<"out_storage_driver">> => <<"garage">>,
        <<"out_storage_locator_sha256">> => <<"digest">>
    },
    Query = fun
        (<<"SELECT * FROM fn_v5_claim_eb($1, $2, $3)">>, [Token, 10, 20]) ->
            ?assertMatch(
                {match, _},
                re:run(
                    Token, "^[0-9a-f]{8}-[0-9a-f]{4}-7[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
                )
            ),
            put(lease_token, Token),
            {ok, [Row]};
        (<<"SELECT fn_v5_ack_eb($1, $2, $3, $4, $5)">>, [10, 20, 30, Token, Ok]) ->
            ?assertEqual(get(lease_token), Token),
            ?assertEqual(Delete =:= ok orelse Delete =:= {error, not_found}, Ok),
            put(ack_seen, true),
            {ok, [#{<<"fn_v5_ack_eb">> => Ack}]}
    end,
    pure_mock_fixture:with_mocks(
        [
            {elib_pg, [{query, 2, Query}]},
            {attachment_storage, [
                {identity, 3, fun(<<"alias">>, <<"bucket">>, <<"synthetic/key">>) ->
                    {ok, #{
                        driver => garage,
                        namespace => <<"ns">>,
                        container => <<"bucket">>,
                        locator_digest =>
                            case Delete of
                                identity_changed -> <<"changed">>;
                                _ -> <<"digest">>
                            end
                    }}
                end},
                {driver_module, 1, fun(<<"garage">>) -> attachment_storage_garage end}
            ]},
            {attachment_storage_garage, [
                {delete, 3, fun(<<"ns">>, <<"bucket">>, <<"synthetic/key">>) ->
                    ?assertNotEqual(identity_changed, Delete),
                    Delete
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(Expected, eb_pg_asset_delete_queue:drain(10, 20, 1)),
            ?assertEqual(true, get(ack_seen))
        end
    ).

scoped_claim_db_test_() ->
    {setup,
        fun() ->
            {ok, Conn} = eunit_runner:eunit_setup_with_db(),
            Conn
        end,
        fun eunit_runner:eunit_cleanup_db/1, fun(_) -> {timeout, 60, fun scoped_claim/0} end}.

scoped_claim() ->
    Scope = eb_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    OtherOrg = maps:get(other_org_id, Scope),
    OtherWs = maps:get(other_workspace_id, Scope),
    try
        Result = elib_pg:with_tx(fun(Conn) ->
            seed(Conn, OtherOrg, OtherWs, 1),
            seed(Conn, Org, Ws, 2),
            {ok, [Row]} = elib_pg:query(
                Conn,
                <<"SELECT * FROM fn_v5_claim_eb($1,$2,$3)">>,
                [elib_uuid:gen_v7(), Org, Ws]
            ),
            ?assertEqual(Org, maps:get(<<"out_organization_id">>, Row)),
            ?assertEqual(Ws, maps:get(<<"out_workspace_id">>, Row)),
            {ok, [#{<<"job_state">> := <<"ready">>, <<"attempts">> := 0}]} =
                elib_pg:query(
                    Conn,
                    <<"SELECT job_state, attempts FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2">>,
                    [OtherOrg, OtherWs]
                ),
            throw({rollback, verified})
        end),
        ?assertEqual({rollback, verified}, Result)
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

seed(Conn, Org, Ws, Asset) ->
    Key = <<"synthetic/", (integer_to_binary(Org))/binary, "/", (integer_to_binary(Asset))/binary>>,
    Hash = binary:encode_hex(crypto:hash(sha256, Key), lowercase),
    {ok, _} = elib_pg:execute(
        Conn,
        <<"INSERT INTO enterprise_asset_delete_queue (organization_id,workspace_id,asset_id,object_key,storage_driver,storage_namespace,storage_container,storage_locator_sha256,job_state,next_retry_at) VALUES ($1,$2,$3,$4,'garage','synthetic','synthetic',$5,'ready',now()-interval '1 hour')">>,
        [Org, Ws, Asset, Key, Hash]
    ),
    ok.
