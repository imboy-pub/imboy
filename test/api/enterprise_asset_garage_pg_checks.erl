%%% Real Garage plus disposable PostgreSQL; production default assembly, no stub.
-module(enterprise_asset_garage_pg_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    {ok, _} = application:ensure_all_started(inets),
    H = intbe02_http_support:setup_all(),
    try
        application:unset_env(imboy, eb_asset_object_store),
        stub_regression(),
        Config = #{
            endpoint => env("IMBOY_TEST_ENDPOINT"),
            bucket => <<"synthetic-enterprise-assets">>,
            region => <<"garage">>,
            access_key => env("IMBOY_TEST_ACCESS"),
            secret_key => env("IMBOY_TEST_SECRET")
        },
        application:set_env(imboy, garage, Config),
        S = cs_pg_test_fixture:new_scope(),
        ok = cs_pg_test_fixture:exec(
            <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,function_key='customer_service' WHERE organization_id=$1 AND user_id=$2">>,
            [maps:get(org_id, S), maps:get(actor_user_id, S), maps:get(service_identity_id, S)]
        ),
        journey(S),
        rollback_object(S),
        application:unset_env(imboy, garage),
        Org = maps:get(org_id, S),
        Ws = maps:get(workspace_id, S),
        FailedId = cs_pg_test_fixture:id(),
        ?assertEqual(
            {error, {object_store, storage_not_configured}},
            eb_asset_store:put_private(Org, Ws, #{
                id => FailedId,
                payload => <<"x">>,
                object_hash => eb_asset_content:sha256_hex(<<"x">>)
            })
        ),
        {ok, #{status := pending_confirm}} = eb_asset_store:fetch_asset(Org, Ws, FailedId),
        application:set_env(imboy, eb_asset_object_store, invalid),
        ?assertEqual({error, invalid_object_store}, eb_asset_store:put_private(Org, Ws, #{})),
        io:format(
            "PASS: default Garage assembly; real PG presign/PUT/confirm/content, ACL, duplicate/concurrent PUT safety, metadata reservation, missing and invalid config~n"
        )
    after
        intbe02_http_support:teardown_all(H),
        inttest_marker_db:release(H)
    end.

journey(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Bytes = <<"synthetic enterprise asset in real Garage">>,
    Base = #{
        workspace_id => Ws,
        conversation_id => maps:get(conversation_id, S),
        actor_user_id => maps:get(actor_user_id, S),
        key_ref => cs_pg_test_fixture:key_ref()
    },
    {ok, Presign} = enterprise_business_facade:request_presign(Org, Base#{
        mime => <<"text/plain">>,
        size_bytes => byte_size(Bytes),
        object_hash => eb_asset_content:sha256_hex(Bytes)
    }),
    Ref = maps:get(upload_ref, Presign),
    Id = maps:get(asset_id, Presign),
    ?assertEqual(<<"private_object_store">>, maps:get(adapter, maps:get(upload, Presign))),
    concurrent_put(Org, Base#{upload_ref => Ref, payload => Bytes}),
    ?assertMatch(
        {error, _},
        enterprise_business_facade:put_object(
            Org, Base#{upload_ref => Ref, payload => Bytes}
        )
    ),
    {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id),
    {ok, _} = enterprise_business_facade:confirm_asset(Org, Base#{upload_ref => Ref}),
    {ok, #{body := Bytes}} = enterprise_business_facade:content_stream(Org, Base#{asset_id => Id}),
    ?assertEqual(
        {error, not_found},
        enterprise_business_facade:content_stream(
            maps:get(other_org_id, S), Base#{
                workspace_id => maps:get(other_workspace_id, S), asset_id => Id
            }
        )
    ),
    ?assertMatch(
        {error, _},
        enterprise_business_facade:content_stream(
            Org, Base#{actor_user_id => maps:get(peer_user_id, S), asset_id => Id}
        )
    ),
    {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, Id),
    ?assertEqual(active, maps:get(status, Row)),
    ok = eb_asset_store:delete_private(Org, Ws, Id).

rollback_object(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Id = cs_pg_test_fixture:id(),
    ?assertEqual(
        {error, invalid_asset_descriptor},
        eb_asset_store:put_private(
            Org, Ws, #{id => Id, payload => <<"synthetic rollback">>, mime => <<"text/plain">>}
        )
    ),
    ?assertEqual(
        {error, not_found},
        eb_asset_object_garage:get(
            eb_asset_store:scope_key(Org, Ws, Id), eb_pg_asset_meta:key_prefix(Org, Ws)
        )
    ).

env(Name) -> list_to_binary(os:getenv(Name)).

stub_regression() ->
    Previous = eb_pg_test_fixture:select_asset_stub(),
    try
        ok = eunit:test(eb_asset_store_tests:cases({ok, synthetic}), [verbose])
    after
        eb_asset_object_stub:reset(),
        eb_pg_test_fixture:restore_asset_store(Previous)
    end,
    ?assertEqual(undefined, application:get_env(imboy, eb_asset_object_store)).

concurrent_put(Org, Params) ->
    Parent = self(),
    Workers = [
        spawn(fun() ->
            receive
                start -> Parent ! {self(), enterprise_business_facade:put_object(Org, Params)}
            end
        end)
     || _ <- [1, 2]
    ],
    [Pid ! start || Pid <- Workers],
    Results = [
        receive
            {Pid, Result} -> Result
        after 10000 -> error(upload_timeout)
        end
     || Pid <- Workers
    ],
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([error || {error, _} <- Results])).
