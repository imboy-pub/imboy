-module(enterprise_directory_member_logic_tests).
-include_lib("eunit/include/eunit.hrl").

transition_provenance_test() ->
    R = row(),
    ?assertEqual(ok, transition(R, <<"suspended">>, 42)),
    ?assertEqual({error, version_conflict}, transition(R, <<"suspended">>, 41)),
    Paused = R#{<<"member_status">> => <<"suspended">>},
    ?assertEqual({error, resource_conflict}, transition(Paused, <<"active">>, 42)),
    Owned = Paused#{<<"suspension_member_version">> => 42},
    ?assertEqual(ok, transition(Owned, <<"active">>, 42)),
    ?assertEqual(
        {error, resource_conflict},
        transition(
            Owned#{<<"member_version">> => 43}, <<"active">>, 43
        )
    ),
    ?assertEqual(
        {error, resource_conflict},
        transition(
            Owned#{<<"member_status">> => <<"removed">>}, <<"active">>, 42
        )
    ).

protected_members_test() ->
    [
        begin
            ?assertEqual(
                {error, resource_conflict}, transition((row())#{K => V}, <<"suspended">>, 42)
            )
        end
     || {K, V} <- [
            {<<"role">>, <<"owner">>},
            {<<"role">>, <<"admin">>},
            {<<"managed">>, false},
            {<<"valid_user">>, false},
            {<<"binding_retired">>, true},
            {<<"projected_user_id">>, 2},
            {<<"membership_status">>, <<"removed">>},
            {<<"source_status">>, <<"removed">>}
        ]
    ].

validation_test() ->
    Q = #{<<"external_organization_id">> => <<"oa">>, <<"external_user_id">> => <<"employee">>},
    B = Q#{<<"status">> => <<"suspended">>, <<"expected_member_version">> => 42},
    ?assertEqual(ok, enterprise_directory_member_logic:validate(read, Q)),
    ?assertEqual(ok, enterprise_directory_member_logic:validate(write, B)),
    [
        ?assertEqual(
            {error, invalid_request}, enterprise_directory_member_logic:validate(write, Bad)
        )
     || Bad <- [
            B#{<<"user_id">> => 1},
            B#{<<"role">> => <<"admin">>},
            B#{<<"status">> => <<"removed">>},
            B#{<<"expected_member_version">> => 0},
            B#{<<"expected_member_version">> => <<"42">>},
            maps:remove(<<"external_user_id">>, B)
        ]
    ].

transition(R, S, V) -> enterprise_directory_member_logic:transition(R, S, V).
row() ->
    #{
        <<"managed">> => true,
        <<"membership_status">> => <<"active">>,
        <<"source_status">> => <<"active">>,
        <<"binding_retired">> => false,
        <<"valid_user">> => true,
        <<"role">> => <<"member">>,
        <<"user_id">> => 1,
        <<"projected_user_id">> => 1,
        <<"member_status">> => <<"active">>,
        <<"member_version">> => 42,
        <<"suspension_member_version">> => null
    }.
