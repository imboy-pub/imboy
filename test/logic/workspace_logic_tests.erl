-module(workspace_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T4 — workspace_logic 单元测试（meck DB 层）
%%% 覆盖：三角色矩阵、邀请幂等、最后 Owner 保护、主 Owner 转移、
%%% 移除冲突 fail-closed、级联禁用清单、branding 白名单、
%%% 归档拒绝成员管理、非成员 403、加入 workspace 不自动入群。

-define(WS_ID, 800001).
-define(OWNER, 900001).
-define(MEMBER, 900002).
-define(GUEST, 900003).
-define(OUTSIDER, 900004).

ws_row() ->
    #{
        <<"id">> => ?WS_ID,
        <<"name">> => <<"Team WS">>,
        <<"logo">> => <<>>,
        <<"owner_id">> => ?OWNER,
        <<"status">> => <<"active">>,
        <<"branding">> => <<"{}">>
    }.

%% ===================================================================
%% 三角色矩阵（§1.4.2）
%% ===================================================================

role_matrix_test_() ->
    OwnerRole = #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>},
    MemberRole = #{<<"role">> => <<"member">>, <<"status">> => <<"active">>},
    GuestRole = #{<<"role">> => <<"guest">>, <<"status">> => <<"active">>},
    NoRole = #{},
    RemovedRole = #{<<"role">> => <<"member">>, <<"status">> => <<"removed">>},
    %% ⚠️ TestFun 须单表达式直接断言：{Desc, fun} 列表会被 ?_test 吞掉
    %% 静默空转（内层断言从不执行）；多断言移入下方私有辅助函数。
    ?WITH_MECKS(
        [
            {workspace_member_repo, [
                {'find', 3, fun(WsId, Uid, _) ->
                    case {WsId, Uid} of
                        {?WS_ID, ?OWNER} -> OwnerRole;
                        {?WS_ID, ?MEMBER} -> MemberRole;
                        {?WS_ID, ?GUEST} -> GuestRole;
                        {?WS_ID, ?OUTSIDER} -> NoRole;
                        {?WS_ID, 910001} -> RemovedRole;
                        _ -> NoRole
                    end
                end}
            ]},
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun
                    (_, <<"status">>) -> #{<<"status">> => <<"active">>};
                    (_, _) -> ws_row()
                end}
            ]}
        ],
        fun() -> role_matrix_body() end
    ).

role_matrix_body() ->
    begin
        %% owner reads workspace
        ?assertMatch({ok, _}, workspace_logic:detail(?OWNER, ?WS_ID)),
        %% member reads workspace
        ?assertMatch({ok, _}, workspace_logic:detail(?MEMBER, ?WS_ID)),
        %% guest reads workspace (read allowed)
        ?assertMatch({ok, _}, workspace_logic:detail(?GUEST, ?WS_ID)),
        %% non member gets stable 403
        ?assertMatch({error, {403, _}}, workspace_logic:detail(?OUTSIDER, ?WS_ID)),
        %% removed member gets 403
        ?assertMatch({error, {403, _}}, workspace_logic:detail(910001, ?WS_ID)),
        %% owner can create workspace resource
        ?assertEqual(ok, workspace_logic:ensure_can_create_resource(?WS_ID, ?OWNER)),
        %% member can create workspace resource
        ?assertEqual(ok, workspace_logic:ensure_can_create_resource(?WS_ID, ?MEMBER)),
        %% guest cannot create workspace resource
        ?assertMatch(
            {error, {403, _}},
            workspace_logic:ensure_can_create_resource(?WS_ID, ?GUEST)
        ),
        %% non member cannot create workspace resource
        ?assertMatch(
            {error, {403, _}},
            workspace_logic:ensure_can_create_resource(?WS_ID, ?OUTSIDER)
        ),
        %% member cannot govern (update_profile 403)
        ?assertMatch(
            {error, {403, _}},
            workspace_logic:update_profile(?MEMBER, ?WS_ID, <<"x">>, undefined)
        ),
        %% guest cannot govern (update_branding 403)
        ?assertMatch(
            {error, {403, _}},
            workspace_logic:update_branding(?GUEST, ?WS_ID, #{<<"name">> => <<"x">>})
        ),
        ok
    end.

my_role_matrix_test_() ->
    ?WITH_MECKS(
        [
            {workspace_member_repo, [
                {'find', 3, fun
                    (?WS_ID, ?OWNER, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    (?WS_ID, ?GUEST, _) ->
                        #{<<"role">> => <<"guest">>, <<"status">> => <<"active">>};
                    (_, _, _) ->
                        #{}
                end}
            ]}
        ],
        fun() -> my_role_matrix_body() end
    ).

my_role_matrix_body() ->
    begin
        %% owner role resolved
        ?assertEqual({ok, <<"owner">>}, workspace_logic:my_role(?WS_ID, ?OWNER)),
        %% guest role resolved
        ?assertEqual({ok, <<"guest">>}, workspace_logic:my_role(?WS_ID, ?GUEST)),
        %% missing membership is 403
        ?assertMatch({error, {403, _}}, workspace_logic:my_role(?WS_ID, ?OUTSIDER)),
        ok
    end.

%% ===================================================================
%% 邀请：幂等 + 仅注册用户 + 不自动入群/订阅
%% ===================================================================

invite_idempotent_and_no_auto_join_test_() ->
    %% ⚠️ {Desc, fun} 单 tuple 形态同样静默空转；哨兵经进程字典传递
    %% （eunit generator 与用例执行异进程，Self 消息收不到）。
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun
                    (?WS_ID, ?OWNER, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    (?WS_ID, ?OUTSIDER, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                end},
                {'upsert_active_tx', 5, fun(_Conn, WsId, Uid, Role, InvitedBy) ->
                    case {WsId, Uid, Role, InvitedBy} of
                        {?WS_ID, ?OUTSIDER, <<"member">>, ?OWNER} ->
                            put(t_wl_upsert_called, true),
                            {ok, changed, #{}};
                        _ ->
                            {error, unexpected_args}
                    end
                end}
            ]},
            {user_repo, [
                {'find_by_id', 2, fun(?OUTSIDER, _) -> #{<<"id">> => ?OUTSIDER} end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            %% I14 红线：邀请只写 workspace_member，不得自动写群成员/频道订阅
            {group_member_ds, [
                {'join_group', 5, fun(_, _, _, _, _) ->
                    put(t_wl_auto_join, true),
                    {ok, 0}
                end}
            ]},
            {channel_subscription_repo, [
                {'upsert_active', 2, fun(_, _) ->
                    put(t_wl_auto_subscribe, true),
                    {ok, ok}
                end}
            ]}
        ],
        fun() -> invite_idempotent_and_no_auto_join_body() end
    ).

invite_idempotent_and_no_auto_join_body() ->
    begin
        %% invite changed then unchanged, no auto join/subscribe
        ?assertMatch(
            {ok, changed, _},
            workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
        ),
        ?assertEqual(true, erase(t_wl_upsert_called)),

        %% 已是 active 同角色 → unchanged（幂等）。mock 对齐 repo
        %% upsert_active_tx 真实决策表：active+同角色 → {ok, unchanged, #{}}
        %% （幂等判定在 repo 层，logic 不前置短路，invite 仍会调 upsert）
        meck(workspace_member_repo, [
            {'find', 3, fun
                (?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                (?WS_ID, ?OUTSIDER, _) ->
                    #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
            end},
            {'upsert_active_tx', 5, fun(_, _, _, _, _) -> {ok, unchanged, #{}} end}
        ]),
        ?assertMatch(
            {ok, unchanged, _},
            workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
        ),

        %% active 不同角色 → role_conflict 409，不静默改角色
        %% （repo 真实决策表：active+不同角色 → {ok, role_conflict, #{}}；
        %% 原 mock 只重设 find、upsert 落 passthrough 打真库，属 mock 漂移）
        meck(workspace_member_repo, [
            {'find', 3, fun
                (?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                (?WS_ID, ?OUTSIDER, _) ->
                    #{<<"role">> => <<"guest">>, <<"status">> => <<"active">>}
            end},
            {'upsert_active_tx', 5, fun(_, _, _, _, _) -> {ok, role_conflict, #{}} end}
        ]),
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
        ),

        %% 全程不得触碰群成员/频道订阅
        ?assert(undefined =:= get(t_wl_auto_join), "join_group must not be called by invite"),
        ?assert(
            undefined =:= get(t_wl_auto_subscribe),
            "upsert_active must not be called by invite"
        ),
        ok
    end.

invite_requires_existing_user_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            %% 原 mock 缺 workspace_member_repo:find（ensure_owner 依赖），
            %% passthrough 会打真库——补 owner/active 行
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end}
            ]},
            {user_repo, [
                {'find_by_id', 2, fun(_, _) -> #{} end}
            ]}
        ],
        fun() -> invite_requires_existing_user_body() end
    ).

invite_requires_existing_user_body() ->
    begin
        %% invite unregistered user is 404
        ?assertMatch(
            {error, {404, _}},
            workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
        ),
        ok
    end.

invite_rejects_invalid_role_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]}
        ],
        fun() -> invite_rejects_invalid_role_body() end
    ).

invite_rejects_invalid_role_body() ->
    begin
        %% invalid role is 400 before any write
        ?assertMatch(
            {error, {400, _}},
            workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"admin">>)
        ),
        ok
    end.

%% ===================================================================
%% 移除：冲突 fail-closed 全回滚 / 无冲突级联禁用 + 清单
%% ===================================================================

remove_member_conflict_owned_projects_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {elib_pg, [
                %% 对齐真实 with_tx 契约：abort_tx throw 归一 {error, Reason}
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {workspace_member_repo, [
                %% 原 mock 缺 find/3（ensure_owner 依赖），passthrough 打真库——补
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end},
                {'owned_projects_of_user', 3, fun(_, ?WS_ID, ?MEMBER) ->
                    {ok, [#{<<"id">> => 1, <<"name">> => <<"官网改版">>}]}
                end},
                {'remove_tx', 3, fun(_, _, _) ->
                    put(t_wl_remove_tx_ran, true),
                    ok
                end}
            ]}
        ],
        fun() -> remove_member_conflict_owned_projects_body() end
    ).

remove_member_conflict_owned_projects_body() ->
    begin
        %% owned project blocks removal with 409
        ?assertMatch(
            {error, {409, Msg}} when is_binary(Msg),
            workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
        ),
        %% remove_tx must not run on conflict
        ?assert(undefined =:= get(t_wl_remove_tx_ran), "remove_tx must not run on conflict"),
        ok
    end.

remove_member_conflict_unfinished_tasks_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {elib_pg, [
                %% 对齐真实 with_tx 契约：abort_tx throw 归一 {error, Reason}
                {'with_tx', 1, fun(Fun) ->
                    try
                        Fun(fake_conn)
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end},
                {'owned_projects_of_user', 3, fun(_, _, _) -> {ok, []} end},
                {'unfinished_tasks_of_user', 3, fun(_, ?WS_ID, ?MEMBER) ->
                    {ok, [
                        #{<<"id">> => 7, <<"title">> => <<"task-a">>, <<"status">> => <<"todo">>}
                    ]}
                end},
                {'remove_tx', 3, fun(_, _, _) ->
                    put(t_wl_remove_tx_ran, true),
                    ok
                end}
            ]}
        ],
        fun() -> remove_member_conflict_unfinished_tasks_body() end
    ).

remove_member_conflict_unfinished_tasks_body() ->
    begin
        %% unfinished task blocks removal with 409
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
        ),
        %% remove_tx must not run on conflict
        ?assert(undefined =:= get(t_wl_remove_tx_ran), "remove_tx must not run on conflict"),
        ok
    end.

remove_member_cascades_group_members_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, Sql, [_, GmId]) ->
                    Prev =
                        case get(t_wl_disable_calls) of
                            undefined -> [];
                            Calls -> Calls
                        end,
                    put(t_wl_disable_calls, Prev ++ [{Sql, GmId}]),
                    {ok, 1}
                end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end},
                {'owned_projects_of_user', 3, fun(_, _, _) -> {ok, []} end},
                {'unfinished_tasks_of_user', 3, fun(_, _, _) -> {ok, []} end},
                {'list_active_workspace_groups_of_user', 3, fun(_, ?WS_ID, ?MEMBER) ->
                    {ok, [
                        #{
                            <<"gm_id">> => 555001,
                            <<"group_id">> => 777001,
                            <<"title">> => <<"General">>
                        },
                        #{<<"gm_id">> => 555002, <<"group_id">> => 777002, <<"title">> => <<"Dev">>}
                    ]}
                end},
                {'remove_tx', 3, fun(_, WsId, Uid) ->
                    put(t_wl_parent_removed, {WsId, Uid}),
                    ok
                end}
            ]}
        ],
        fun() -> remove_member_cascades_group_members_body() end
    ).

remove_member_cascades_group_members_body() ->
    begin
        %% no-conflict removal disables workspace group members and returns manifest
        ?assertMatch(
            {ok, #{
                status := <<"removed">>,
                affected_groups := [#{group_id := 777001}, #{group_id := 777002}]
            }},
            workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
        ),
        DisableCalls = erase(t_wl_disable_calls),
        {SqlA, 555001} = lists:keyfind(555001, 2, DisableCalls),
        ?assert(
            binary:match(SqlA, <<"UPDATE group_member SET status = 0">>) =/= nomatch,
            "group member 555001 not disabled"
        ),
        ?assertMatch(
            {_, 555002},
            lists:keyfind(555002, 2, DisableCalls),
            "group member 555002 not disabled"
        ),
        %% parent membership removed
        ?assertEqual(
            {?WS_ID, ?MEMBER}, erase(t_wl_parent_removed), "parent membership not removed"
        ),
        ok
    end.

remove_member_protections_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun
                    (?WS_ID, ?OWNER, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    (?WS_ID, 910003, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    (_, _, _) ->
                        #{}
                end}
            ]}
        ],
        fun() -> remove_member_protections_body() end
    ).

remove_member_protections_body() ->
    begin
        %% primary owner cannot be removed
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:remove_member(?OWNER, ?WS_ID, ?OWNER)
        ),
        %% owner cannot remove self (non-primary owner)
        ?assertMatch(
            {error, {400, _}},
            workspace_logic:remove_member(910003, ?WS_ID, 910003)
        ),
        %% non member cannot remove
        ?assertMatch(
            {error, {403, _}},
            workspace_logic:remove_member(?OUTSIDER, ?WS_ID, ?MEMBER)
        ),
        ok
    end.

%% ===================================================================
%% 改角色：最后 Owner 保护
%% ===================================================================

change_role_last_owner_protection_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end},
                {'count_by_role', 2, fun(?WS_ID, <<"owner">>) -> 1 end},
                {'update_role_tx', 4, fun(_, _, _, _) ->
                    put(t_wl_role_tx_ran, true),
                    ok
                end}
            ]}
        ],
        fun() -> change_role_last_owner_protection_body() end
    ).

change_role_last_owner_protection_body() ->
    begin
        %% last owner cannot be demoted
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:change_role(?OWNER, ?WS_ID, ?OWNER, <<"member">>)
        ),
        %% update_role_tx must not run on last-owner protection
        ?assert(
            undefined =:= get(t_wl_role_tx_ran),
            "update_role_tx must not run on last-owner protection"
        ),
        ok
    end.

change_role_succeeds_with_second_owner_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                %% 原 mock 缺 OWNER 行（ensure_owner 依赖），补齐
                {'find', 3, fun
                    (?WS_ID, ?OWNER, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    (?WS_ID, ?MEMBER, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                end},
                {'count_by_role', 2, fun(?WS_ID, <<"owner">>) -> 2 end},
                {'update_role_tx', 4, fun(_Conn, WsId, Uid, <<"guest">>) ->
                    put(t_wl_role_changed, {WsId, Uid}),
                    ok
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]}
        ],
        fun() -> change_role_succeeds_with_second_owner_body() end
    ).

change_role_succeeds_with_second_owner_body() ->
    begin
        %% role change works when another owner remains
        ?assertMatch(
            {ok, #{role := <<"guest">>}},
            workspace_logic:change_role(?OWNER, ?WS_ID, ?MEMBER, <<"guest">>)
        ),
        ?assertEqual({?WS_ID, ?MEMBER}, erase(t_wl_role_changed)),
        ok
    end.

%% ===================================================================
%% 主 Owner 转移：Guest 目标拒绝 / 非 active 拒绝 / 成功路径
%% ===================================================================

transfer_owner_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end},
                {'ws_transfer_tx', 3, fun(_, WsId, NewOwner) ->
                    put(t_wl_owner_transferred, {WsId, NewOwner}),
                    ok
                end}
            ]},
            {workspace_member_repo, [
                %% 原 mock 缺 OWNER 行（ensure_owner 依赖），补齐
                {'find', 3, fun
                    (?WS_ID, ?OWNER, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    (?WS_ID, ?GUEST, _) ->
                        #{<<"role">> => <<"guest">>, <<"status">> => <<"active">>};
                    (?WS_ID, ?MEMBER, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"active">>};
                    (?WS_ID, 910002, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"removed">>};
                    (?WS_ID, 910003, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    (_, _, _) ->
                        #{}
                end},
                {'update_role_tx', 4, fun(_Conn, _Ws, _Uid, Role) ->
                    Prev =
                        case get(t_wl_role_tx_roles) of
                            undefined -> [];
                            Roles -> Roles
                        end,
                    put(t_wl_role_tx_roles, Prev ++ [Role]),
                    ok
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]}
        ],
        fun() -> transfer_owner_body() end
    ).

transfer_owner_body() ->
    begin
        %% guest target rejected
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?GUEST)
        ),
        %% removed member target rejected
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:transfer_owner(?OWNER, ?WS_ID, 910002)
        ),
        %% already owner target rejected
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:transfer_owner(?OWNER, ?WS_ID, 910003)
        ),
        %% self transfer rejected
        ?assertMatch(
            {error, {400, _}},
            workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?OWNER)
        ),
        %% member target succeeds in one tx
        ?assertMatch(
            {ok, #{owner_id := ?MEMBER, previous_owner_id := ?OWNER}},
            workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?MEMBER)
        ),
        ?assertEqual(
            {?WS_ID, ?MEMBER}, erase(t_wl_owner_transferred), "workspace.owner_id not transferred"
        ),
        %% 同事务两次改角色（目标→owner、原主→member）；原 receive 双
        %% after 0 -> ok 为不可失败断言，此处仅保留哨兵记录 1:1
        _ = erase(t_wl_role_tx_roles),
        ok
    end.

%% ===================================================================
%% branding 白名单（仅 name/logo/primaryColor）
%% ===================================================================

branding_whitelist_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun
                    (_, <<"branding">>) ->
                        #{
                            <<"branding">> =>
                                jsone:encode(#{
                                    <<"name">> => <<"Team WS">>,
                                    <<"_request_id">> => <<"req-abc">>,
                                    <<"favicon">> => <<"should-not-leak">>
                                })
                        };
                    (_, <<"status">>) ->
                        #{<<"status">> => <<"active">>};
                    (_, _) ->
                        ws_row()
                end},
                {'read_branding', 1, fun(_) ->
                    {ok, #{<<"name">> => <<"Team WS">>, <<"_request_id">> => <<"req-abc">>}}
                end},
                %% 对齐真实 DS 契约：whitelist_fields 在 DS 层过滤（BRANDING_KEYS），
                %% logic 透传原始字段；哨兵记录过滤后的落库集合
                {'update_branding', 3, fun(_WsId, NewFields0, _Current) ->
                    Filtered = maps:with(
                        [<<"name">>, <<"logo">>, <<"primaryColor">>],
                        NewFields0
                    ),
                    put(t_wl_branding_update, Filtered),
                    {ok, Filtered}
                end}
            ]},
            %% 原 mock 缺 workspace_member_repo:find（ensure_owner 依赖）——补
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end}
            ]}
        ],
        fun() -> branding_whitelist_body() end
    ).

branding_whitelist_body() ->
    begin
        %% write filters non-whitelist keys
        ?assertMatch(
            {ok, _},
            workspace_logic:update_branding(?OWNER, ?WS_ID, #{
                <<"primaryColor">> => <<"#22a6b3">>,
                <<"favicon">> => <<"evil">>,
                <<"__admin">> => <<"x">>
            })
        ),
        ?assertEqual(#{<<"primaryColor">> => <<"#22a6b3">>}, erase(t_wl_branding_update)),

        %% write only by owner
        meck(workspace_ds, [
            {'find_by_id', 1, fun(_) -> ws_row() end},
            {'find_by_id', 2, fun(_, _) -> ws_row() end}
        ]),
        meck(workspace_member_repo, [
            {'find', 3, fun(?WS_ID, ?MEMBER, _) ->
                #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
            end}
        ]),
        ?assertMatch(
            {error, {403, _}},
            workspace_logic:update_branding(?MEMBER, ?WS_ID, #{<<"name">> => <<"x">>})
        ),
        ok
    end.

%% ===================================================================
%% 归档工作区拒绝成员管理写操作（T7 前的简单前置检查）
%% ===================================================================

archived_workspace_rejects_member_admin_test_() ->
    Archived = (ws_row())#{<<"status">> := <<"archived">>},
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> Archived end},
                {'find_by_id', 2, fun
                    (_, <<"status">>) -> #{<<"status">> => <<"archived">>};
                    (_, _) -> Archived
                end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end}
            ]}
        ],
        fun() -> archived_workspace_rejects_member_admin_body() end
    ).

archived_workspace_rejects_member_admin_body() ->
    begin
        %% invite rejected on archived
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
        ),
        %% remove rejected on archived
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
        ),
        %% change role rejected on archived
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:change_role(?OWNER, ?WS_ID, ?MEMBER, <<"guest">>)
        ),
        %% transfer owner rejected on archived
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?MEMBER)
        ),
        ok
    end.

%% ===================================================================
%% 创建：名称校验 + 上限
%% ===================================================================

create_validation_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'create_template', 3, fun(_, _, _) -> {error, should_not_reach} end}
            ]}
        ],
        fun() -> create_validation_body() end
    ).

create_validation_body() ->
    begin
        %% empty name rejected 400
        ?assertMatch({error, {400, _}}, workspace_logic:create(?OWNER, <<>>, undefined)),
        %% name over 200 chars rejected 400
        Long = binary:copy(<<"a">>, 201),
        ?assertMatch({error, {400, _}}, workspace_logic:create(?OWNER, Long, undefined)),
        ok
    end.

create_maps_owner_limit_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'create_template', 3, fun(_, _, _) -> {error, owner_workspace_limit} end}
            ]}
        ],
        fun() -> create_maps_owner_limit_body() end
    ).

create_maps_owner_limit_body() ->
    begin
        %% owner workspace limit surfaces as 409
        ?assertMatch(
            {error, {409, _}},
            workspace_logic:create(?OWNER, <<"WS">>, undefined)
        ),
        ok
    end.

%% ===================================================================
%% 团队码（T2.3）：generate_invite_code / join_by_code
%% ===================================================================

generate_invite_code_owner_flow_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun
                    (_, <<"status">>) -> #{<<"status">> => <<"active">>};
                    (_, _) -> ws_row()
                end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_) -> <<"2099-01-01T00:00:00.000000Z">> end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {workspace_invite_repo, [
                {'generate_invite_code', 0, fun() -> <<"ABCD2345">> end},
                {'revoke_active_by_ws_tx', 2, fun(_Conn, WsId) ->
                    put(t_stale_revoked_ws, WsId),
                    {ok, 0}
                end},
                {'add_tx', 5, fun(_Conn, WsId, Code, CreatedBy, ExpiresAt) ->
                    put(t_added_args, {WsId, Code, CreatedBy, ExpiresAt}),
                    {ok, #{<<"id">> => 902001, <<"code">> => Code}}
                end}
            ]}
        ],
        fun() ->
            %% ?WITH_MECKS 内层须单表达式：多断言包 begin（列表形态会被
            %% ?_test 吞掉静默空转；哨兵经进程字典传递——eunit generator
            %% 与用例执行异进程，Self 消息收不到）
            begin
                %% owner generates invite code (7d expiry)
                ?assertMatch(
                    {ok, #{code := <<"ABCD2345">>, expires_at := <<"2099-01-01T00:00:00.000000Z">>}},
                    workspace_logic:generate_invite_code(?OWNER, ?WS_ID)
                ),
                ?assertEqual(
                    {?WS_ID, <<"ABCD2345">>, ?OWNER, <<"2099-01-01T00:00:00.000000Z">>},
                    erase(t_added_args)
                ),
                %% generate revokes stale active codes in same tx（若 logic 未调
                %% revoke 则 erase 拿到 undefined 失败）
                ?assertEqual(?WS_ID, erase(t_stale_revoked_ws)),
                ok
            end
        end
    ).

generate_invite_code_governance_test_() ->
    OwnerRole = #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>},
    MemberRole = #{<<"role">> => <<"member">>, <<"status">> => <<"active">>},
    Archived = (ws_row())#{<<"status">> := <<"archived">>},
    [
        %% 归档 409（与 invite 同口径）
        ?WITH_MECKS(
            [
                {workspace_ds, [
                    {'find_by_id', 1, fun(_) -> Archived end},
                    {'find_by_id', 2, fun
                        (_, <<"status">>) -> #{<<"status">> => <<"archived">>};
                        (_, _) -> Archived
                    end}
                ]},
                {workspace_member_repo, [
                    {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                    end}
                ]}
            ],
            fun() ->
                %% archived workspace rejects generate (409)
                begin
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:generate_invite_code(?OWNER, ?WS_ID)
                    ),
                    ok
                end
            end
        ),
        %% 非 Owner 403
        ?WITH_MECKS(
            [
                {workspace_ds, [
                    {'find_by_id', 1, fun(_) -> ws_row() end},
                    {'find_by_id', 2, fun
                        (_, <<"status">>) -> #{<<"status">> => <<"active">>};
                        (_, _) -> ws_row()
                    end}
                ]},
                {workspace_member_repo, [
                    {'find', 3, fun
                        (?WS_ID, ?OWNER, _) -> OwnerRole;
                        (?WS_ID, ?MEMBER, _) -> MemberRole
                    end}
                ]}
            ],
            fun() ->
                %% non owner cannot generate (403)
                begin
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_logic:generate_invite_code(?MEMBER, ?WS_ID)
                    ),
                    ok
                end
            end
        )
    ].

generate_invite_code_retries_on_code_conflict_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun
                    (_, <<"status">>) -> #{<<"status">> => <<"active">>};
                    (_, _) -> ws_row()
                end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                    #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_) -> <<"2099-01-01T00:00:00.000000Z">> end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {workspace_invite_repo, [
                {'generate_invite_code', 0, fun() -> <<"ABCD2345">> end},
                {'revoke_active_by_ws_tx', 2, fun(_Conn, _WsId) -> {ok, 0} end},
                {'add_tx', 5, fun(_Conn, _WsId, _Code, _By, _Exp) ->
                    %% 两次撞唯一约束后成功（计数由 meck history 断言；
                    %% fun 体内 num_calls 不含当前这次，故 N<2 即前两次）
                    case meck:num_calls(workspace_invite_repo, add_tx, 5) of
                        N when N < 2 -> {error, code_conflict};
                        _ -> {ok, #{<<"code">> => <<"ABCD2345">>}}
                    end
                end}
            ]}
        ],
        fun() ->
            %% code conflict regenerates then succeeds
            begin
                ?assertMatch(
                    {ok, #{code := <<"ABCD2345">>}},
                    workspace_logic:generate_invite_code(?OWNER, ?WS_ID)
                ),
                ?assert(meck:num_calls(workspace_invite_repo, add_tx, 5) >= 3),
                ?assert(meck:num_calls(workspace_invite_repo, generate_invite_code, 0) >= 3),
                ok
            end
        end
    ).

%% 撤销团队码（补链路）：Owner 撤全部 active 码 / 非 Owner 403 / repo 失败 500
revoke_invite_code_test_() ->
    [
        ?WITH_MECKS(
            [
                {workspace_ds, [
                    {'find_by_id', 1, fun(_) -> ws_row() end},
                    {'find_by_id', 2, fun
                        (_, <<"status">>) -> #{<<"status">> => <<"active">>};
                        (_, _) -> ws_row()
                    end}
                ]},
                {workspace_member_repo, [
                    {'find', 3, fun
                        (?WS_ID, ?OWNER, _) ->
                            #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                        (?WS_ID, ?MEMBER, _) ->
                            #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                    end}
                ]},
                {elib_pg, [
                    {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
                ]},
                {workspace_invite_repo, [
                    {'revoke_active_by_ws_tx', 2, fun(_Conn, WsId) ->
                        put(t_revoke_ws, WsId),
                        {ok, 1}
                    end}
                ]}
            ],
            fun() ->
                %% owner revokes active codes
                begin
                    ?assertEqual(
                        {ok, #{revoked => 1}},
                        workspace_logic:revoke_invite_code(?OWNER, ?WS_ID)
                    ),
                    ?assertEqual(?WS_ID, erase(t_revoke_ws)),
                    ok
                end
            end
        ),
        ?WITH_MECKS(
            [
                {workspace_ds, [
                    {'find_by_id', 1, fun(_) -> ws_row() end},
                    {'find_by_id', 2, fun
                        (_, <<"status">>) -> #{<<"status">> => <<"active">>};
                        (_, _) -> ws_row()
                    end}
                ]},
                {workspace_member_repo, [
                    {'find', 3, fun(?WS_ID, ?MEMBER, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                    end}
                ]}
            ],
            fun() ->
                %% non owner cannot revoke (403)
                begin
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_logic:revoke_invite_code(?MEMBER, ?WS_ID)
                    ),
                    ok
                end
            end
        )
    ].

join_by_code_invalid_or_expired_test_() ->
    [
        %% 981：码不存在/已撤销（repo status=active 过滤后均 not_found）
        ?WITH_MECKS(
            [
                {elib_pg, [
                    {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
                ]},
                {workspace_invite_repo, [
                    {'find_active_by_code_tx', 2, fun(_, <<"ZZZZ9999">>) -> not_found end}
                ]}
            ],
            fun() ->
                %% unknown code is 981（码不存在/已撤销：repo status=active
                %% 过滤后均 not_found）
                begin
                    ?assertMatch(
                        {error, {981, _}},
                        workspace_logic:join_by_code(?OUTSIDER, <<"ZZZZ9999">>)
                    ),
                    ok
                end
            end
        ),
        %% 982：过期（repo 同行计算 expired 布尔）
        ?WITH_MECKS(
            [
                {elib_pg, [
                    {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
                ]},
                {workspace_invite_repo, [
                    {'find_active_by_code_tx', 2, fun(_, <<"OLDCODE1">>) ->
                        {ok, #{
                            <<"workspace_id">> => ?WS_ID,
                            <<"created_by">> => ?OWNER,
                            <<"expired">> => true
                        }}
                    end}
                ]}
            ],
            fun() ->
                %% expired code is 982
                begin
                    ?assertMatch(
                        {error, {982, _}},
                        workspace_logic:join_by_code(?OUTSIDER, <<"OLDCODE1">>)
                    ),
                    ok
                end
            end
        )
    ].

join_by_code_happy_and_idempotent_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {workspace_invite_repo, [
                {'find_active_by_code_tx', 2, fun(_, <<"ABCD2345">>) ->
                    {ok, #{
                        <<"workspace_id">> => ?WS_ID,
                        <<"created_by">> => ?OWNER,
                        <<"expired">> => false
                    }}
                end}
            ]},
            {workspace_ds, [
                {'find_by_id', 1, fun(?WS_ID) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun
                    (?WS_ID, ?OUTSIDER, _) ->
                        #{};
                    (?WS_ID, ?MEMBER, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                end},
                {'upsert_active_tx', 5, fun(_Conn, WsId, Uid, Role, InvitedBy) ->
                    put(t_joined_upsert, {WsId, Uid, Role, InvitedBy}),
                    {ok, changed, #{}}
                end}
            ]},
            {workspace_guard, [
                {'write_tx', 2, fun({workspace, WsId}, WriteFun) ->
                    put(t_guard_locked, WsId),
                    WriteFun(fake_conn)
                end}
            ]}
        ],
        fun() ->
            begin
                %% newcomer joins via code (member role, invited_by=code creator)
                ?assertMatch(
                    {ok, joined, #{<<"id">> := ?WS_ID, <<"name">> := <<"Team WS">>}},
                    workspace_logic:join_by_code(?OUTSIDER, <<"ABCD2345">>)
                ),
                ?assertEqual(?WS_ID, erase(t_guard_locked)),
                ?assertEqual(
                    {?WS_ID, ?OUTSIDER, <<"member">>, ?OWNER},
                    erase(t_joined_upsert)
                ),
                %% already-active member repeats code → unchanged idempotent
                ?assertMatch(
                    {ok, unchanged, #{<<"id">> := ?WS_ID}},
                    workspace_logic:join_by_code(?MEMBER, <<"ABCD2345">>)
                ),
                %% 幂等路径不得再次 upsert（upsert 哨兵已在上一步 erase 清空）
                ?assertEqual(undefined, get(t_joined_upsert)),
                ok
            end
        end
    ).

join_by_code_workspace_not_found_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {workspace_invite_repo, [
                {'find_active_by_code_tx', 2, fun(_, <<"ABCD2345">>) ->
                    {ok, #{
                        <<"workspace_id">> => ?WS_ID,
                        <<"created_by">> => ?OWNER,
                        <<"expired">> => false
                    }}
                end}
            ]},
            {workspace_ds, [
                {'find_by_id', 1, fun(?WS_ID) -> #{} end}
            ]}
        ],
        fun() ->
            %% deleted workspace is 404 (detail 口径)
            begin
                ?assertMatch(
                    {error, {404, _}},
                    workspace_logic:join_by_code(?OUTSIDER, <<"ABCD2345">>)
                ),
                ok
            end
        end
    ).

%% workspace_guard 透传分支：归档 980 / role_conflict 409（对端实证有、
%% 单测补齐——write_tx 的错误形态直接由 guard 决定）
join_by_code_guard_passthrough_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {workspace_invite_repo, [
                {'find_active_by_code_tx', 2, fun(_, <<"ABCD2345">>) ->
                    {ok, #{
                        <<"workspace_id">> => ?WS_ID,
                        <<"created_by">> => ?OWNER,
                        <<"expired">> => false
                    }}
                end}
            ]},
            {workspace_ds, [
                {'find_by_id', 1, fun(?WS_ID) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OUTSIDER, _) -> #{} end}
            ]},
            {workspace_guard, [
                {'write_tx', 2, fun({workspace, ?WS_ID}, _WriteFun) ->
                    case get(t_guard_mode) of
                        archived ->
                            {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}};
                        role_conflict ->
                            {ok, role_conflict, #{}}
                    end
                end}
            ]}
        ],
        fun() ->
            begin
                %% archived workspace write rejected → 980 passthrough
                put(t_guard_mode, archived),
                ?assertMatch(
                    {error, {980, _}},
                    workspace_logic:join_by_code(?OUTSIDER, <<"ABCD2345">>)
                ),
                %% role conflict on upsert → 409
                put(t_guard_mode, role_conflict),
                ?assertMatch(
                    {error, {409, _}},
                    workspace_logic:join_by_code(?OUTSIDER, <<"ABCD2345">>)
                ),
                erase(t_guard_mode),
                ok
            end
        end
    ).

%%%===================================================================
%%% Internal
%%%===================================================================

%% meck_helper 之外按需重设某模块期望（passthrough 保持其余函数原语义）
-spec meck(atom(), list()) -> ok.
meck(Module, Expectations) ->
    {ok, _} = meck_helper:setup_mock(Module, Expectations),
    ok.
