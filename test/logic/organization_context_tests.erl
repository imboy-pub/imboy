-module(organization_context_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

organization_admin_context_and_switch_test_() ->
    ?WITH_MECKS(
        [
            {moya_context_repo, [
                {'guardian_contexts', 1, fun(_) -> {ok, []} end},
                {'staff_contexts', 1, fun(_) -> {ok, []} end},
                {'organization_contexts', 1, fun(_) ->
                    {ok, [
                        #{
                            <<"org_id">> => 101,
                            <<"org_name">> => <<"Acme">>,
                            <<"role">> => <<"admin">>
                        }
                    ]}
                end}
            ]},
            {organization_member_repo, [
                {'find_active', 3, fun(101, 202, <<"role">>) ->
                    {ok, #{<<"role">> => <<"admin">>}}
                end}
            ]}
        ],
        fun() ->
            {ok, #{contexts := [Ctx]}} = moya_context_logic:contexts(202, organization),
            ?assertEqual(<<"organization">>, maps:get(<<"context_type">>, Ctx)),
            ?assertEqual(<<"admin">>, maps:get(<<"role">>, Ctx)),
            ?assertEqual(
                {ok, Ctx},
                moya_context_logic:switch(202, #{
                    <<"context_type">> => <<"organization">>,
                    <<"organization_id">> => <<"101">>
                })
            )
        end
    ).

legacy_org_owner_switch_is_accepted_test_() ->
    ?WITH_MECKS(
        [
            {moya_context_repo, [
                {'guardian_contexts', 1, fun(_) -> {ok, []} end},
                {'staff_contexts', 1, fun(_) -> {ok, []} end},
                {'owner_contexts', 1, fun(_) ->
                    {ok, [
                        #{
                            <<"org_id">> => 101,
                            <<"org_name">> => <<"Acme">>
                        }
                    ]}
                end},
                {'org_owner_uid', 1, fun(101) -> {ok, 202} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{<<"context_type">> := <<"org_owner">>}},
                moya_context_logic:switch(202, #{
                    <<"context_type">> => <<"org_owner">>,
                    <<"organization_id">> => <<"101">>
                })
            )
        end
    ).

legacy_contexts_keep_owner_only_wire_shape_test_() ->
    ?WITH_MECKS(
        [
            {moya_context_repo, [
                {'guardian_contexts', 1, fun(_) -> {ok, []} end},
                {'staff_contexts', 1, fun(_) -> {ok, []} end},
                {'owner_contexts', 1, fun(_) ->
                    {ok, [#{<<"org_id">> => 101, <<"org_name">> => <<"Acme">>}]}
                end}
            ]}
        ],
        fun() ->
            {ok, #{contexts := [Ctx]}} = moya_context_logic:contexts(202),
            ?assertEqual(<<"org_owner">>, maps:get(<<"context_type">>, Ctx)),
            ?assertEqual(false, maps:is_key(<<"role">>, Ctx))
        end
    ).
