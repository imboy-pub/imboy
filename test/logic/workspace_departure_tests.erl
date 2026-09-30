-module(workspace_departure_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

channel_removal_test_() ->
    [
        removal_case(success),
        removal_case(channel_error),
        removal_case(parent_error),
        removal_case(channel_owner)
    ].

removal_case(Mode) ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {find_by_id, 1, fun(_) -> #{<<"owner_id">> => 1} end},
                {find_by_id, 2, fun(_, _) -> #{<<"status">> => <<"active">>} end}
            ]},
            {workspace_member_repo, [
                {find, 3, fun(_, _, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end},
                {owned_projects_of_user, 3, fun(_, _, _) -> {ok, []} end},
                {unfinished_tasks_of_user, 3, fun(_, _, _) -> {ok, []} end},
                {list_active_workspace_groups_of_user, 3, fun(_, _, _) -> {ok, []} end},
                {remove_channels_tx, 3, fun(conn, 10, 2) -> remove_channels(Mode) end},
                {remove_tx, 3, fun(conn, 10, 2) ->
                    put(parent_removed, true),
                    case Mode of
                        parent_error -> {error, disconnected};
                        _ -> ok
                    end
                end}
            ]},
            {elib_pg, [
                {with_tx, 1, fun(F) ->
                    try
                        F(conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason};
                        Class:Reason -> {error, {Class, Reason}}
                    end
                end}
            ]},
            {imboy_cache, [
                {flush, 1, fun(Key) ->
                    put(flushed, [
                        Key
                        | case get(flushed) of
                            undefined -> [];
                            X -> X
                        end
                    ]),
                    ok
                end}
            ]},
            {imboy_domain_event, [
                {publish, 1, fun(_) ->
                    put(published, true),
                    ok
                end}
            ]}
        ],
        fun() -> verify_removal(Mode) end
    ).

remove_channels(channel_error) ->
    {error, disconnected};
remove_channels(channel_owner) ->
    throw({abort_tx, {membership_conflict, #{owned_channels => [#{<<"id">> => 20}]}}});
remove_channels(_) ->
    put(channels_removed, true),
    {ok, [#{<<"channel_id">> => 20}, #{<<"channel_id">> => 21}]}.

verify_removal(success) ->
    ?assertMatch(
        {ok, #{affected_channels := [#{channel_id := 20}, #{channel_id := 21}]}},
        workspace_logic:remove_member(1, 10, 2)
    ),
    ?assertEqual(true, erase(channels_removed)),
    ?assertEqual(true, erase(parent_removed)),
    ?assertEqual(
        lists:sort([{channel_subs, 20}, {channel, 20}, {channel_subs, 21}, {channel, 21}]),
        lists:sort(erase(flushed))
    ),
    ?assertEqual(true, erase(published));
verify_removal(Mode) ->
    Code =
        case Mode of
            channel_owner -> 409;
            _ -> 500
        end,
    ?assertMatch({error, {Code, _}}, workspace_logic:remove_member(1, 10, 2)),
    ?assertEqual(undefined, erase(flushed)),
    ?assertEqual(undefined, erase(published)),
    ExpectedParent =
        case Mode of
            parent_error -> true;
            _ -> undefined
        end,
    ?assertEqual(ExpectedParent, erase(parent_removed)).
