-module(organization_departure_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

departure_test_() ->
    [
        departure_case(Mode)
     || Mode <- [
            member,
            admin,
            suspended,
            archived,
            owner,
            workspace_owner,
            group_owner,
            resource_error,
            parent_error,
            admin_removal,
            non_member
        ]
    ].

departure_case(Mode) ->
    ?WITH_MECKS(mocks(Mode), fun() -> verify(Mode) end).

mocks(Mode) ->
    [
        {elib_pg, [
            {with_tx, 1, fun(F) ->
                put(committed, false),
                try F(conn) of
                    R ->
                        put(committed, true),
                        R
                catch
                    throw:{abort_tx, Reason} -> {error, Reason}
                end
            end},
            {execute, 3, fun(conn, _, _) -> {ok, 1} end}
        ]},
        {organization_member_repo, [
            {find_organization_for_share_tx, 3, fun(conn, 10, _) ->
                Status =
                    case Mode of
                        archived -> <<"archived">>;
                        _ -> <<"active">>
                    end,
                {ok, #{<<"status">> => Status, <<"owner_id">> => 1}}
            end},
            {find_active_for_share_tx, 4, fun(conn, 10, 1, _) ->
                {ok, #{<<"role">> => <<"owner">>}}
            end},
            {find_for_update_tx, 4, fun(conn, 10, 2, _) -> member(Mode) end},
            {remove_tx, 3, fun(conn, 10, 2) ->
                ?assertEqual([21, 20], get(workspace_writes)),
                case Mode of
                    parent_error -> {error, unavailable};
                    _ -> ok
                end
            end}
        ]},
        {workspace_member_repo, [
            {lock_organization_memberships_tx, 3, fun(conn, 10, 2) ->
                Owner =
                    case Mode of
                        workspace_owner -> 2;
                        _ -> 1
                    end,
                {ok, [
                    #{<<"workspace_id">> => 20, <<"owner_id">> => Owner},
                    #{<<"workspace_id">> => 21, <<"owner_id">> => 1}
                ]}
            end},
            {owned_projects_of_user, 3, fun(conn, _, 2) -> {ok, []} end},
            {unfinished_tasks_of_user, 3, fun(conn, _, 2) -> {ok, []} end},
            {owned_groups_of_user, 3, fun(conn, _, 2) ->
                case Mode of
                    group_owner -> {ok, [#{<<"id">> => 30}]};
                    _ -> {ok, []}
                end
            end},
            {list_active_workspace_groups_of_user, 3, fun(conn, _, 2) -> {ok, []} end},
            {remove_channels_tx, 3, fun(conn, WsId, 2) ->
                case {Mode, WsId} of
                    {resource_error, 21} -> {error, unavailable};
                    _ -> {ok, []}
                end
            end},
            {remove_tx, 3, fun(conn, WsId, 2) ->
                put(workspace_writes, [
                    WsId
                    | case get(workspace_writes) of
                        undefined -> [];
                        X -> X
                    end
                ]),
                ok
            end}
        ]},
        {workspace_ds, [
            {member_removed, 1, fun(#{workspace_id := WsId}) ->
                ?assertEqual(true, get(committed)),
                put(notified, [
                    WsId
                    | case get(notified) of
                        undefined -> [];
                        X -> X
                    end
                ]),
                ok
            end}
        ]}
    ].

member(non_member) ->
    {error, not_found};
member(Mode) ->
    Role =
        case Mode of
            owner -> <<"owner">>;
            admin -> <<"admin">>;
            _ -> <<"member">>
        end,
    Status =
        case Mode of
            suspended -> <<"suspended">>;
            _ -> <<"active">>
        end,
    {ok, #{<<"role">> => Role, <<"status">> => Status}}.

verify(Mode) ->
    Result =
        case Mode of
            admin_removal -> organization_member_logic:remove(1, 10, 2);
            _ -> organization_member_logic:leave(2, 10)
        end,
    case Mode of
        owner ->
            blocked(Result, 409);
        workspace_owner ->
            blocked(Result, 409);
        group_owner ->
            blocked(Result, 409);
        resource_error ->
            blocked(Result, 500);
        parent_error ->
            blocked(Result, 500);
        non_member ->
            blocked(Result, 403);
        _ ->
            ?assertMatch({ok, #{status := <<"removed">>, affected_workspaces := [_, _]}}, Result),
            ?assertEqual([21, 20], erase(notified)),
            ?assertEqual(true, erase(committed))
    end.

blocked(Result, Code) ->
    ?assertMatch({error, {Code, _}}, Result),
    ?assertEqual(false, erase(committed)),
    ?assertEqual(undefined, erase(notified)).
