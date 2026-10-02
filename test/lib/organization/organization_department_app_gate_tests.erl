%%% @doc ORG-BACKEND-GAP3：部门读路径授权门。目录三个读入口
%%% （list/detail/members）要求同 Org active member；结构写另要求 Org owner/admin。
%%% 非成员/已离场成员一律 {actor_not_member,_} / {actor_not_active,_,_}，
%%% 且门先于存在性判定（不得借 404 探测租户目录）。
-module(organization_department_app_gate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORG, 301).
-define(ACTOR, 305).
-define(DEPT, 401).

-define(WITH_PG_MOCKS(Body),
    {setup,
        fun() ->
            meck:new(organization_department_pg, [non_strict]),
            ok
        end,
        fun(_) -> meck:unload() end, Body}
).

structure_requires_org_manager_test_() ->
    [
        {
            lists:flatten(io_lib:format("~p rejects ~p role", [Op, Role])),
            ?WITH_PG_MOCKS(fun() ->
                Facts =
                    case Role of
                        undefined -> #{status => active};
                        _ -> #{role => Role, status => active}
                    end,
                meck:expect(organization_department_pg, org_role_of, fun(?ORG, ?ACTOR) ->
                    {ok, Facts}
                end),
                Params = #{
                    actor_user_id => ?ACTOR,
                    department_id => ?DEPT,
                    name => <<"dev">>,
                    parent_id => null,
                    expected_version => 1
                },
                ?assertEqual({error, {actor_not_permitted, ?ACTOR}}, structure_call(Op, Params))
            end)
        }
     || Op <- [create, rename, move, archive], Role <- [member, unknown, undefined]
    ].

structure_call(create, Params) -> organization_department_app:create_department(?ORG, Params);
structure_call(rename, Params) -> organization_department_app:update_department(?ORG, Params);
structure_call(move, Params) -> organization_department_app:move_department(?ORG, Params);
structure_call(archive, Params) -> organization_department_app:archive_department(?ORG, Params).

%% ------------------------------------------------------------------
%% list_departments
%% ------------------------------------------------------------------

list_rejects_non_member_before_any_read_test_() ->
    ?WITH_PG_MOCKS(
        fun() ->
            meck:expect(organization_department_pg, org_role_of, fun(_O, _U) ->
                {error, not_found}
            end),
            %% 门必须先于任何目录读：pg 读被调即视为破防
            meck:expect(organization_department_pg, list_departments, fun(_O, _S, _C) ->
                erlang:error(ungated_directory_read)
            end),
            ?assertEqual(
                {error, {actor_not_member, ?ACTOR}},
                organization_department_app:list_departments(?ORG, #{actor_user_id => ?ACTOR})
            )
        end
    ).

list_rejects_offboarded_member_test_() ->
    ?WITH_PG_MOCKS(
        fun() ->
            meck:expect(organization_department_pg, org_role_of, fun(_O, _U) ->
                {ok, #{role => member, status => removed}}
            end),
            ?assertEqual(
                {error, {actor_not_active, ?ACTOR, removed}},
                organization_department_app:list_departments(?ORG, #{actor_user_id => ?ACTOR})
            )
        end
    ).

list_allows_active_member_and_projects_rows_test_() ->
    ?WITH_PG_MOCKS(
        fun() ->
            meck:expect(organization_department_pg, org_role_of, fun(_O, _U) ->
                {ok, #{role => member, status => active}}
            end),
            meck:expect(organization_department_pg, list_departments, fun(?ORG, active, _C) ->
                {ok, [dept_row()]}
            end),
            ?assertMatch(
                {ok, [#{id := ?DEPT, name := <<"dev">>}]},
                organization_department_app:list_departments(?ORG, #{
                    actor_user_id => ?ACTOR, status => active
                })
            )
        end
    ).

%% ------------------------------------------------------------------
%% get_department / list_members
%% ------------------------------------------------------------------

detail_rejects_non_member_before_existence_probe_test_() ->
    ?WITH_PG_MOCKS(
        fun() ->
            meck:expect(organization_department_pg, org_role_of, fun(_O, _U) ->
                {error, not_found}
            end),
            %% 存在性探测（fetch_department）不得发生在门之前
            meck:expect(organization_department_pg, fetch_department, fun(_O, _D, _C) ->
                erlang:error(ungated_existence_probe)
            end),
            ?assertEqual(
                {error, {actor_not_member, ?ACTOR}},
                organization_department_app:get_department(?ORG, #{
                    actor_user_id => ?ACTOR, department_id => ?DEPT
                })
            )
        end
    ).

detail_allows_active_member_with_members_test_() ->
    ?WITH_PG_MOCKS(
        fun() ->
            meck:expect(organization_department_pg, org_role_of, fun(_O, _U) ->
                {ok, #{role => member, status => active}}
            end),
            meck:expect(organization_department_pg, fetch_department, fun(?ORG, ?DEPT, _C) ->
                {ok, dept_row()}
            end),
            meck:expect(organization_department_pg, list_members, fun(?DEPT, _C) ->
                {ok, [member_row()]}
            end),
            ?assertMatch(
                {ok, #{id := ?DEPT, members := [#{user_id := 306}]}},
                organization_department_app:get_department(?ORG, #{
                    actor_user_id => ?ACTOR, department_id => ?DEPT
                })
            )
        end
    ).

dept_members_reject_offboarded_member_test_() ->
    ?WITH_PG_MOCKS(
        fun() ->
            meck:expect(organization_department_pg, org_role_of, fun(_O, _U) ->
                {ok, #{role => member, status => removed}}
            end),
            meck:expect(organization_department_pg, fetch_department, fun(_O, _D, _C) ->
                erlang:error(ungated_existence_probe)
            end),
            ?assertEqual(
                {error, {actor_not_active, ?ACTOR, removed}},
                organization_department_app:list_members(?ORG, #{
                    actor_user_id => ?ACTOR, department_id => ?DEPT
                })
            )
        end
    ).

dept_members_allows_active_member_test_() ->
    ?WITH_PG_MOCKS(
        fun() ->
            meck:expect(organization_department_pg, org_role_of, fun(_O, _U) ->
                {ok, #{role => admin, status => active}}
            end),
            meck:expect(organization_department_pg, fetch_department, fun(?ORG, ?DEPT, _C) ->
                {ok, dept_row()}
            end),
            meck:expect(organization_department_pg, list_members, fun(?DEPT, _C) ->
                {ok, [member_row()]}
            end),
            ?assertMatch(
                {ok, [#{user_id := 306}]},
                organization_department_app:list_members(?ORG, #{
                    actor_user_id => ?ACTOR, department_id => ?DEPT
                })
            )
        end
    ).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

dept_row() ->
    #{
        id => ?DEPT,
        organization_id => ?ORG,
        parent_id => null,
        name => <<"dev">>,
        status => active,
        version => 1,
        created_at => 0,
        updated_at => 0,
        archive_idempotent => false
    }.

member_row() ->
    #{
        id => 1,
        department_id => ?DEPT,
        user_id => 306,
        is_admin => false,
        added_by_user_id => ?ACTOR,
        created_at => 0,
        updated_at => 0,
        idempotent => false
    }.
