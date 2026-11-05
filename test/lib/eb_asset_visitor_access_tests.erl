-module(eb_asset_visitor_access_tests).
-include_lib("eunit/include/eunit.hrl").

visitor_revalidation_test() ->
    Claims = #{
        org_id => 1,
        workspace_id => 2,
        actor_id => 3,
        conversation_id => 4,
        visitor_session_id => 5,
        visitor_token_id => 6,
        visitor_installation_id => 7
    },
    meck:new(elib_pg, [passthrough]),
    try
        meck:expect(elib_pg, query, fun(_, [1, 2, 3, 4, 5, 6, 7]) ->
            {ok, [#{<<"?column?">> => 1}]}
        end),
        ?assertEqual(ok, eb_asset_visitor_access:authorize(Claims)),
        meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
        ?assertEqual({error, not_found}, eb_asset_visitor_access:authorize(Claims)),
        ?assertEqual(
            {error, not_found},
            eb_asset_visitor_access:authorize(maps:remove(visitor_token_id, Claims))
        ),
        meck:expect(elib_pg, query, fun(_, _) -> {error, unavailable} end),
        ?assertEqual({error, not_found}, eb_asset_visitor_access:authorize(Claims))
    after
        meck:unload(elib_pg)
    end.
