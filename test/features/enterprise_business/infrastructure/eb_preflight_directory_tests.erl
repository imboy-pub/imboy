-module(eb_preflight_directory_tests).
-include_lib("eunit/include/eunit.hrl").

pending_owner_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        meck:expect(elib_pg, query, fun
            (_, []) -> {ok, [#{<<"present">> => true}]};
            (<<"SELECT id, organization_id", _/binary>>, [101]) -> {ok, []};
            (_, [101]) -> {ok, [#{<<"application_id">> => 17}]}
        end),
        ?assertMatch(
            {ok, #{
                blockers := [
                    #{
                        code := <<"DIRECTORY_INITIAL_OWNER_PENDING">>,
                        resource_id := <<"17">>,
                        organization_id := null
                    }
                ]
            }},
            eb_preflight_facts_pg:facts_enterprise_business(101)
        ),
        meck:expect(elib_pg, query, fun
            (_, []) -> {ok, [#{<<"present">> => false}]};
            (_, [101]) -> {ok, []}
        end),
        ?assertMatch({ok, #{blockers := []}}, eb_preflight_facts_pg:facts_enterprise_business(101)),
        meck:expect(elib_pg, query, fun
            (_, []) -> {error, unavailable};
            (_, [101]) -> {ok, []}
        end),
        ?assertEqual({error, unavailable}, eb_preflight_facts_pg:facts_enterprise_business(101))
    after
        meck:unload(elib_pg)
    end.
