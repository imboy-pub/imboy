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
        fun() ->
            [
                {"owner reads workspace", fun() ->
                    ?assertMatch({ok, _}, workspace_logic:detail(?OWNER, ?WS_ID))
                end},
                {"member reads workspace", fun() ->
                    ?assertMatch({ok, _}, workspace_logic:detail(?MEMBER, ?WS_ID))
                end},
                {"guest reads workspace (read allowed)", fun() ->
                    ?assertMatch({ok, _}, workspace_logic:detail(?GUEST, ?WS_ID))
                end},
                {"non member gets stable 403", fun() ->
                    ?assertMatch({error, {403, _}}, workspace_logic:detail(?OUTSIDER, ?WS_ID))
                end},
                {"removed member gets 403", fun() ->
                    ?assertMatch({error, {403, _}}, workspace_logic:detail(910001, ?WS_ID))
                end},
                {"owner can create workspace resource", fun() ->
                    ?assertEqual(ok, workspace_logic:ensure_can_create_resource(?WS_ID, ?OWNER))
                end},
                {"member can create workspace resource", fun() ->
                    ?assertEqual(ok, workspace_logic:ensure_can_create_resource(?WS_ID, ?MEMBER))
                end},
                {"guest cannot create workspace resource", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_logic:ensure_can_create_resource(?WS_ID, ?GUEST)
                    )
                end},
                {"non member cannot create workspace resource", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_logic:ensure_can_create_resource(?WS_ID, ?OUTSIDER)
                    )
                end},
                {"member cannot govern (update_profile 403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_logic:update_profile(?MEMBER, ?WS_ID, <<"x">>, undefined)
                    )
                end},
                {"guest cannot govern (update_branding 403)", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_logic:update_branding(?GUEST, ?WS_ID, #{<<"name">> => <<"x">>})
                    )
                end}
            ]
        end
    ).

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
        fun() ->
            [
                {"owner role resolved", fun() ->
                    ?assertEqual({ok, <<"owner">>}, workspace_logic:my_role(?WS_ID, ?OWNER))
                end},
                {"guest role resolved", fun() ->
                    ?assertEqual({ok, <<"guest">>}, workspace_logic:my_role(?WS_ID, ?GUEST))
                end},
                {"missing membership is 403", fun() ->
                    ?assertMatch({error, {403, _}}, workspace_logic:my_role(?WS_ID, ?OUTSIDER))
                end}
            ]
        end
    ).

%% ===================================================================
%% 邀请：幂等 + 仅注册用户 + 不自动入群/订阅
%% ===================================================================

invite_idempotent_and_no_auto_join_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?OUTSIDER, _) ->
                    #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                end},
                {'upsert_active_tx', 5, fun(_Conn, WsId, Uid, Role, InvitedBy) ->
                    case {WsId, Uid, Role, InvitedBy} of
                        {?WS_ID, ?OUTSIDER, <<"member">>, ?OWNER} ->
                            Self ! upsert_called,
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
                    Self ! auto_join_forbidden,
                    {ok, 0}
                end}
            ]},
            {channel_subscription_repo, [
                {'upsert_active', 2, fun(_, _) ->
                    Self ! auto_subscribe_forbidden,
                    {ok, ok}
                end}
            ]}
        ],
        fun() ->
            {"invite changed then unchanged, no auto join/subscribe", fun() ->
                ?assertMatch(
                    {ok, changed, _},
                    workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
                ),
                receive
                    upsert_called -> ok
                after 500 -> ?assert(false)
                end,
                %% 已是 active 同角色 → unchanged（幂等，不再 upsert）
                meck(workspace_member_repo, [
                    {'find', 3, fun(?WS_ID, ?OUTSIDER, _) ->
                        #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                    end},
                    {'upsert_active_tx', 5, fun(_, _, _, _, _) ->
                        Self ! upsert_should_not_run,
                        {ok, changed, #{}}
                    end}
                ]),
                ?assertMatch(
                    {ok, unchanged, _},
                    workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
                ),
                %% active 不同角色 → role_conflict 409，不静默改角色
                meck(workspace_member_repo, [
                    {'find', 3, fun(?WS_ID, ?OUTSIDER, _) ->
                        #{<<"role">> => <<"guest">>, <<"status">> => <<"active">>}
                    end}
                ]),
                ?assertMatch(
                    {error, {409, _}},
                    workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
                ),
                receive
                    auto_join_forbidden ->
                        ?assert(false, "join_group must not be called by invite");
                    auto_subscribe_forbidden ->
                        ?assert(false, "upsert_active must not be called by invite")
                after 0 -> ok
                end
            end}
        end
    ).

invite_requires_existing_user_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {user_repo, [
                {'find_by_id', 2, fun(_, _) -> #{} end}
            ]}
        ],
        fun() ->
            {"invite unregistered user is 404", fun() ->
                ?assertMatch(
                    {error, {404, _}},
                    workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
                )
            end}
        end
    ).

invite_rejects_invalid_role_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]}
        ],
        fun() ->
            {"invalid role is 400 before any write", fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"admin">>)
                )
            end}
        end
    ).

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
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {workspace_member_repo, [
                {'owned_projects_of_user', 3, fun(_, ?WS_ID, ?MEMBER) ->
                    {ok, [#{<<"id">> => 1, <<"name">> => <<"官网改版">>}]}
                end},
                {'remove_tx', 3, fun(_, _, _) ->
                    self() ! remove_should_not_run,
                    ok
                end}
            ]}
        ],
        fun() ->
            {"owned project blocks removal with 409", fun() ->
                ?assertMatch(
                    {error, {409, Msg}} when is_binary(Msg),
                    workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
                ),
                receive
                    remove_should_not_run -> ?assert(false)
                after 0 -> ok
                end
            end}
        end
    ).

remove_member_conflict_unfinished_tasks_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]},
            {workspace_member_repo, [
                {'owned_projects_of_user', 3, fun(_, _, _) -> {ok, []} end},
                {'unfinished_tasks_of_user', 3, fun(_, ?WS_ID, ?MEMBER) ->
                    {ok, [
                        #{<<"id">> => 7, <<"title">> => <<"task-a">>, <<"status">> => <<"todo">>}
                    ]}
                end},
                {'remove_tx', 3, fun(_, _, _) ->
                    self() ! remove_should_not_run,
                    ok
                end}
            ]}
        ],
        fun() ->
            {"unfinished task blocks removal with 409", fun() ->
                ?assertMatch(
                    {error, {409, _}},
                    workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
                ),
                receive
                    remove_should_not_run -> ?assert(false)
                after 0 -> ok
                end
            end}
        end
    ).

remove_member_cascades_group_members_test_() ->
    Self = self(),
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end},
                {'execute', 3, fun(_Conn, Sql, [_, GmId]) ->
                    Self ! {disable_group_member, Sql, GmId},
                    {ok, 1}
                end}
            ]},
            {workspace_member_repo, [
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
                    Self ! {parent_removed, WsId, Uid},
                    ok
                end}
            ]}
        ],
        fun() ->
            {"no-conflict removal disables workspace group members and returns manifest", fun() ->
                ?assertMatch(
                    {ok, #{
                        status := <<"removed">>,
                        affected_groups := [#{group_id := 777001}, #{group_id := 777002}]
                    }},
                    workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
                ),
                receive
                    {disable_group_member, SqlA, 555001} ->
                        ?assert(
                            binary:match(SqlA, <<"UPDATE group_member SET status = 0">>) =/= nomatch
                        )
                after 500 -> ?assert(false, "group member 555001 not disabled")
                end,
                receive
                    {disable_group_member, _, 555002} -> ok
                after 500 -> ?assert(false, "group member 555002 not disabled")
                end,
                receive
                    {parent_removed, ?WS_ID, ?MEMBER} -> ok
                after 500 -> ?assert(false, "parent membership not removed")
                end
            end}
        end
    ).

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
        fun() ->
            [
                {"primary owner cannot be removed", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:remove_member(?OWNER, ?WS_ID, ?OWNER)
                    )
                end},
                {"owner cannot remove self (non-primary owner)", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        workspace_logic:remove_member(910003, ?WS_ID, 910003)
                    )
                end},
                {"non member cannot remove", fun() ->
                    ?assertMatch(
                        {error, {403, _}},
                        workspace_logic:remove_member(?OUTSIDER, ?WS_ID, ?MEMBER)
                    )
                end}
            ]
        end
    ).

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
                    self() ! role_should_not_change,
                    ok
                end}
            ]}
        ],
        fun() ->
            {"last owner cannot be demoted", fun() ->
                ?assertMatch(
                    {error, {409, _}},
                    workspace_logic:change_role(?OWNER, ?WS_ID, ?OWNER, <<"member">>)
                ),
                receive
                    role_should_not_change -> ?assert(false)
                after 0 -> ok
                end
            end}
        end
    ).

change_role_succeeds_with_second_owner_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, _) -> ws_row() end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun(?WS_ID, ?MEMBER, _) ->
                    #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                end},
                {'count_by_role', 2, fun(?WS_ID, <<"owner">>) -> 2 end},
                {'update_role_tx', 4, fun(_Conn, WsId, Uid, <<"guest">>) ->
                    self() ! {role_changed, WsId, Uid},
                    ok
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]}
        ],
        fun() ->
            {"role change works when another owner remains", fun() ->
                ?assertMatch(
                    {ok, #{role := <<"guest">>}},
                    workspace_logic:change_role(?OWNER, ?WS_ID, ?MEMBER, <<"guest">>)
                ),
                receive
                    {role_changed, ?WS_ID, ?MEMBER} -> ok
                after 500 -> ?assert(false)
                end
            end}
        end
    ).

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
                    self() ! {owner_id_transferred, WsId, NewOwner},
                    ok
                end}
            ]},
            {workspace_member_repo, [
                {'find', 3, fun
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
                    self() ! {role_tx, Role},
                    ok
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
            ]}
        ],
        fun() ->
            [
                {"guest target rejected", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?GUEST)
                    )
                end},
                {"removed member target rejected", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:transfer_owner(?OWNER, ?WS_ID, 910002)
                    )
                end},
                {"already owner target rejected", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:transfer_owner(?OWNER, ?WS_ID, 910003)
                    )
                end},
                {"self transfer rejected", fun() ->
                    ?assertMatch(
                        {error, {400, _}},
                        workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?OWNER)
                    )
                end},
                {"member target succeeds in one tx", fun() ->
                    ?assertMatch(
                        {ok, #{owner_id := ?MEMBER, previous_owner_id := ?OWNER}},
                        workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?MEMBER)
                    ),
                    receive
                        {owner_id_transferred, ?WS_ID, ?MEMBER} -> ok
                    after 500 -> ?assert(false, "workspace.owner_id not transferred")
                    end,
                    receive
                        {role_tx, <<"owner">>} -> ok
                    after 0 -> ok
                    end,
                    receive
                        {role_tx, <<"member">>} -> ok
                    after 0 -> ok
                    end
                end}
            ]
        end
    ).

%% ===================================================================
%% branding 白名单（仅 name/logo/primaryColor）
%% ===================================================================

branding_whitelist_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(_) -> ws_row() end},
                {'find_by_id', 2, fun(_, <<"branding">>) ->
                    #{
                        <<"branding">> =>
                            jsone:encode(#{
                                <<"name">> => <<"Team WS">>,
                                <<"_request_id">> => <<"req-abc">>,
                                <<"favicon">> => <<"should-not-leak">>
                            })
                    }
                end},
                {'read_branding', 1, fun(_) ->
                    {ok, #{<<"name">> => <<"Team WS">>, <<"_request_id">> => <<"req-abc">>}}
                end},
                {'update_branding', 3, fun(_WsId, NewFields, _Current) ->
                    self() ! {branding_update, NewFields},
                    {ok,
                        maps:with(
                            [<<"name">>, <<"logo">>, <<"primaryColor">>],
                            NewFields
                        )}
                end}
            ]}
        ],
        fun() ->
            [
                {"write filters non-whitelist keys", fun() ->
                    ?assertMatch(
                        {ok, _},
                        workspace_logic:update_branding(?OWNER, ?WS_ID, #{
                            <<"primaryColor">> => <<"#22a6b3">>,
                            <<"favicon">> => <<"evil">>,
                            <<"__admin">> => <<"x">>
                        })
                    ),
                    receive
                        {branding_update, Submitted} ->
                            ?assertEqual(#{<<"primaryColor">> => <<"#22a6b3">>}, Submitted)
                    after 500 -> ?assert(false)
                    end
                end},
                {"write only by owner", fun() ->
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
                    )
                end}
            ]
        end
    ).

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
        fun() ->
            [
                {"invite rejected on archived", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:invite(?OWNER, ?WS_ID, ?OUTSIDER, <<"member">>)
                    )
                end},
                {"remove rejected on archived", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER)
                    )
                end},
                {"change role rejected on archived", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:change_role(?OWNER, ?WS_ID, ?MEMBER, <<"guest">>)
                    )
                end},
                {"transfer owner rejected on archived", fun() ->
                    ?assertMatch(
                        {error, {409, _}},
                        workspace_logic:transfer_owner(?OWNER, ?WS_ID, ?MEMBER)
                    )
                end}
            ]
        end
    ).

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
        fun() ->
            [
                {"empty name rejected 400", fun() ->
                    ?assertMatch({error, {400, _}}, workspace_logic:create(?OWNER, <<>>, undefined))
                end},
                {"name over 200 chars rejected 400", fun() ->
                    Long = binary:copy(<<"a">>, 201),
                    ?assertMatch({error, {400, _}}, workspace_logic:create(?OWNER, Long, undefined))
                end}
            ]
        end
    ).

create_maps_owner_limit_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'create_template', 3, fun(_, _, _) -> {error, owner_workspace_limit} end}
            ]}
        ],
        fun() ->
            {"owner workspace limit surfaces as 409", fun() ->
                ?assertMatch(
                    {error, {409, _}},
                    workspace_logic:create(?OWNER, <<"WS">>, undefined)
                )
            end}
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
