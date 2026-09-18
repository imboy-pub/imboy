-module(organization_owner_invariant_tests).

%% C04 Owner 领域规则纯函数测试（无 IO / 无 mock）。

-include_lib("eunit/include/eunit.hrl").

validate_self_transfer_test_() ->
    [
        {"同一用户自转移被 400 拒绝",
            ?_assertEqual(
                {error, {400, <<"新主 Owner 不能是当前主 Owner"/utf8>>}},
                organization_owner_invariant:validate_self_transfer(7, 7)
            )},
        {"不同用户允许通过", ?_assertEqual(ok, organization_owner_invariant:validate_self_transfer(7, 8))}
    ].

validate_current_owner_test_() ->
    [
        {"actor 等于投影 owner 放行",
            ?_assertEqual(
                ok,
                organization_owner_invariant:validate_current_owner(
                    #{<<"owner_id">> => 7, <<"status">> => <<"active">>}, 7
                )
            )},
        {"actor 非当前 owner 被 403 拒绝",
            ?_assertMatch(
                {error, {403, _}},
                organization_owner_invariant:validate_current_owner(#{<<"owner_id">> => 7}, 8)
            )}
    ].

validate_actor_membership_test_() ->
    [
        {"active owner 行放行",
            ?_assertEqual(
                ok,
                organization_owner_invariant:validate_actor_membership(
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                )
            )},
        {"已降级（admin）行被 403 拒绝",
            ?_assertMatch(
                {error, {403, _}},
                organization_owner_invariant:validate_actor_membership(
                    #{<<"role">> => <<"admin">>, <<"status">> => <<"active">>}
                )
            )},
        {"removed 行被 403 拒绝",
            ?_assertMatch(
                {error, {403, _}},
                organization_owner_invariant:validate_actor_membership(
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"removed">>}
                )
            )}
    ].

validate_target_membership_test_() ->
    [
        {"active admin 可成为 target",
            ?_assertEqual(
                ok,
                organization_owner_invariant:validate_target_membership(
                    #{
                        <<"role">> => <<"admin">>,
                        <<"status">> => <<"active">>,
                        <<"account_type">> => 0
                    }
                )
            )},
        {"active member 可成为 target",
            ?_assertEqual(
                ok,
                organization_owner_invariant:validate_target_membership(
                    #{
                        <<"role">> => <<"member">>,
                        <<"status">> => <<"active">>,
                        <<"account_type">> => 0
                    }
                )
            )},
        {"Agent（account_type=1）作 target 被 409 拒绝",
            ?_assertMatch(
                {error, {409, _}},
                organization_owner_invariant:validate_target_membership(
                    #{
                        <<"role">> => <<"member">>,
                        <<"status">> => <<"active">>,
                        <<"account_type">> => 1
                    }
                )
            )},
        {"非 owner 账号类型（2/3 过渡值）作 target 被 409 拒绝",
            ?_assertMatch(
                {error, {409, _}},
                organization_owner_invariant:validate_target_membership(
                    #{
                        <<"role">> => <<"member">>,
                        <<"status">> => <<"active">>,
                        <<"account_type">> => 2
                    }
                )
            )},
        {"已是 owner 的 target 被 409 拒绝",
            ?_assertMatch(
                {error, {409, _}},
                organization_owner_invariant:validate_target_membership(
                    #{
                        <<"role">> => <<"owner">>,
                        <<"status">> => <<"active">>,
                        <<"account_type">> => 0
                    }
                )
            )},
        {"removed 成员被 409 拒绝",
            ?_assertMatch(
                {error, {409, _}},
                organization_owner_invariant:validate_target_membership(
                    #{
                        <<"role">> => <<"member">>,
                        <<"status">> => <<"removed">>,
                        <<"account_type">> => 0
                    }
                )
            )},
        {"非成员（not_found 空 map）被 409 拒绝",
            ?_assertMatch(
                {error, {409, _}},
                organization_owner_invariant:validate_target_membership(#{})
            )}
    ].
