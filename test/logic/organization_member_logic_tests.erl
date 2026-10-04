-module(organization_member_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(OWNER, 201).
-define(ADMIN, 202).
-define(MEMBER, 203).

%% EB-08 的通用 suspend 与依赖资源 409 映射：被测的 Core 文件。
-define(CORE_REL, "src/logic/organization_member_logic.erl").

admin_can_invite_and_remove_member_test_() ->
    ?WITH_MECKS(
        common_mocks(?ADMIN, <<"admin">>) ++
            [
                {user_repo, [
                    {'find_by_id', 2, fun(?MEMBER, <<"id">>) -> #{<<"id">> => ?MEMBER} end}
                ]},
                {user_denylist_logic, [
                    {'blocked_between', 2, fun(?ADMIN, ?MEMBER) -> false end}
                ]}
            ],
        fun() ->
            ?assertMatch(
                {ok, changed, #{<<"role">> := <<"member">>}},
                organization_member_logic:invite(?ADMIN, ?ORG_ID, ?MEMBER, <<"member">>)
            ),
            ?assertMatch(
                {ok, #{status := <<"removed">>}},
                organization_member_logic:remove(?ADMIN, ?ORG_ID, ?MEMBER)
            )
        end
    ).

admin_cannot_manage_admin_role_test_() ->
    ?WITH_MECKS(
        common_mocks(?ADMIN, <<"admin">>) ++
            [
                {user_repo, [
                    {'find_by_id', 2, fun(?MEMBER, <<"id">>) -> #{<<"id">> => ?MEMBER} end}
                ]},
                {user_denylist_logic, [
                    {'blocked_between', 2, fun(?ADMIN, ?MEMBER) -> false end}
                ]}
            ],
        fun() ->
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:invite(?ADMIN, ?ORG_ID, ?MEMBER, <<"admin">>)
            ),
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:change_role(
                    ?ADMIN, ?ORG_ID, ?MEMBER, <<"admin">>
                )
            ),
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:remove(?ADMIN, ?ORG_ID, ?ADMIN)
            )
        end
    ).

primary_owner_can_promote_and_remove_admin_test_() ->
    ?WITH_MECKS(
        common_mocks(?OWNER, <<"owner">>),
        fun() ->
            ?assertMatch(
                {ok, changed, #{role := <<"admin">>}},
                organization_member_logic:change_role(
                    ?OWNER, ?ORG_ID, ?MEMBER, <<"admin">>
                )
            ),
            ?assertMatch(
                {ok, #{status := <<"removed">>}},
                organization_member_logic:remove(?OWNER, ?ORG_ID, ?ADMIN)
            )
        end
    ).

primary_owner_is_protected_test_() ->
    ?WITH_MECKS(
        common_mocks(?OWNER, <<"owner">>),
        fun() ->
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:remove(?OWNER, ?ORG_ID, ?OWNER)
            ),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:change_role(
                    ?OWNER, ?ORG_ID, ?OWNER, <<"member">>
                )
            )
        end
    ).

ordinary_member_cannot_list_test_() ->
    ?WITH_MECKS(
        [
            {organization_member_repo, [
                {'find_active', 3, fun(?ORG_ID, ?MEMBER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"member">>}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {403, _}}, organization_member_logic:list(?MEMBER, ?ORG_ID, 1, 10)
            )
        end
    ).

%% —— §5.2 成员详情的有权 Workspace（workspaces/3）——
%% 权限面刻意宽于 list/4：任意 active 成员都可查（成员详情是全员可见能力），
%% 但目标必须是本 Org active 成员（否则 404，不泄露外部用户在本企业的授权）。
member_workspaces_allows_ordinary_member_test_() ->
    ?WITH_MECKS(
        [
            {organization_member_repo, [
                %% 同一 {Fun,Arity} 的多分支写在一个 fun 的子句里
                {'find_active', 3, fun
                    (?ORG_ID, ?MEMBER, <<"role">>) -> {ok, #{<<"role">> => <<"member">>}};
                    (?ORG_ID, ?OWNER, <<"user_id">>) -> {ok, #{<<"user_id">> => ?OWNER}}
                end},
                {'member_workspaces', 2, fun(?ORG_ID, [?OWNER]) ->
                    {ok, #{
                        ?OWNER => [
                            #{<<"id">> => 9001, <<"name">> => ~B'总部工作区'},
                            #{<<"id">> => 9002, <<"name">> => ~B'广州项目组'}
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, [#{<<"id">> := 9001}, #{<<"id">> := 9002}]},
                organization_member_logic:workspaces(?MEMBER, ?ORG_ID, ?OWNER)
            )
        end
    ).

member_workspaces_forbids_non_member_test_() ->
    ?WITH_MECKS(
        [
            {organization_member_repo, [
                {'find_active', 3, fun(?ORG_ID, 999, <<"role">>) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:workspaces(999, ?ORG_ID, ?OWNER)
            )
        end
    ).

member_workspaces_target_must_be_org_member_test_() ->
    ?WITH_MECKS(
        [
            {organization_member_repo, [
                {'find_active', 3, fun
                    (?ORG_ID, ?MEMBER, <<"role">>) -> {ok, #{<<"role">> => <<"member">>}};
                    (?ORG_ID, 999, <<"user_id">>) -> {error, not_found}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {404, _}},
                organization_member_logic:workspaces(?MEMBER, ?ORG_ID, 999)
            )
        end
    ).

member_workspaces_requires_positive_ids_test_() ->
    ?WITH_MECKS(
        [],
        fun() ->
            ?assertMatch(
                {error, {400, _}}, organization_member_logic:workspaces(0, ?ORG_ID, ?OWNER)
            ),
            ?assertMatch(
                {error, {400, _}}, organization_member_logic:workspaces(?MEMBER, ?ORG_ID, 0)
            )
        end
    ).

%% ORG-01：transfer command 下沉到 organization_owner_transfer（src/lib/organization），
%% logic 入口仅作兼容委托；以下用例的 mock 目标由旧 repo 切换到 organization_owner_store。
primary_owner_can_transfer_to_active_member_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun run_tx/1}
            ]},
            {organization_owner_store, [
                {'lock_organization_tx', 2, fun(fake_conn, ?ORG_ID) ->
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"owner_id">> => ?OWNER,
                        <<"status">> => <<"active">>
                    }}
                end},
                {'lock_member_with_account_tx', 3, fun
                    (fake_conn, ?ORG_ID, ?OWNER) ->
                        {ok, #{
                            <<"role">> => <<"owner">>,
                            <<"status">> => <<"active">>,
                            <<"account_type">> => 0
                        }};
                    (fake_conn, ?ORG_ID, ?MEMBER) ->
                        {ok, #{
                            <<"role">> => <<"member">>,
                            <<"status">> => <<"active">>,
                            <<"account_type">> => 0
                        }}
                end},
                {'demote_previous_owner_tx', 3, fun(fake_conn, ?ORG_ID, ?OWNER) ->
                    put(t_previous_owner_demoted, true),
                    ok
                end},
                {'promote_target_tx', 3, fun(fake_conn, ?ORG_ID, ?MEMBER) -> ok end},
                {'update_owner_projection_tx', 3, fun(fake_conn, ?ORG_ID, ?MEMBER) ->
                    put(t_org_owner_updated, true),
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"owner_id">> => ?MEMBER,
                        <<"status">> => <<"active">>
                    }}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {ok, #{
                    organization_id := ?ORG_ID,
                    owner_id := ?MEMBER,
                    previous_owner_id := ?OWNER,
                    previous_owner_role := <<"admin">>
                }},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(true, erase(t_org_owner_updated)),
            ?assertEqual(true, erase(t_previous_owner_demoted))
        end
    ).

non_owner_cannot_transfer_owner_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun run_tx/1}
            ]},
            {organization_owner_store, [
                {'lock_organization_tx', 2, fun(fake_conn, ?ORG_ID) ->
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"owner_id">> => ?OWNER,
                        <<"status">> => <<"active">>
                    }}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:transfer_owner(?ADMIN, ?ORG_ID, ?MEMBER)
            )
        end
    ).

transfer_owner_validation_and_sync_failures_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun run_tx/1}
            ]},
            {organization_owner_store, [
                {'lock_organization_tx', 2, fun(fake_conn, ?ORG_ID) ->
                    Status =
                        case get(t_owner_transfer_case) of
                            archived -> <<"archived">>;
                            _ -> <<"active">>
                        end,
                    {ok, #{
                        <<"id">> => ?ORG_ID,
                        <<"owner_id">> => ?OWNER,
                        <<"status">> => Status
                    }}
                end},
                {'lock_member_with_account_tx', 3, fun
                    (fake_conn, ?ORG_ID, ?OWNER) ->
                        {ok, #{
                            <<"role">> => <<"owner">>,
                            <<"status">> => <<"active">>,
                            <<"account_type">> => 0
                        }};
                    (fake_conn, ?ORG_ID, ?MEMBER) ->
                        case get(t_owner_transfer_case) of
                            inactive_target ->
                                {ok, #{
                                    <<"role">> => <<"member">>,
                                    <<"status">> => <<"removed">>,
                                    <<"account_type">> => 0
                                }};
                            _ ->
                                {ok, #{
                                    <<"role">> => <<"member">>,
                                    <<"status">> => <<"active">>,
                                    <<"account_type">> => 0
                                }}
                        end
                end},
                {'demote_previous_owner_tx', 3, fun(fake_conn, ?ORG_ID, ?OWNER) -> ok end},
                {'promote_target_tx', 3, fun(fake_conn, ?ORG_ID, ?MEMBER) -> ok end},
                {'update_owner_projection_tx', 3, fun(fake_conn, ?ORG_ID, ?MEMBER) ->
                    put(t_owner_transfer_updated, true),
                    case get(t_owner_transfer_case) of
                        %% 投影更新失败（原用例的 sync failure 语义等价映射）
                        sync_failure ->
                            {error, projection_update_failed};
                        _ ->
                            {ok, #{
                                <<"id">> => ?ORG_ID,
                                <<"owner_id">> => ?MEMBER,
                                <<"status">> => <<"active">>
                            }}
                    end
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {400, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?OWNER)
            ),
            put(t_owner_transfer_case, archived),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(undefined, erase(t_owner_transfer_updated)),
            put(t_owner_transfer_case, inactive_target),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(undefined, erase(t_owner_transfer_updated)),
            put(t_owner_transfer_case, sync_failure),
            ?assertMatch(
                {error, {500, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(true, erase(t_owner_transfer_updated)),
            erase(t_owner_transfer_case),
            ok
        end
    ).

common_mocks(ActorUid, ActorRole) ->
    [
        {elib_pg, [
            {'with_tx', 1, fun run_tx/1}
        ]},
        {workspace_member_repo, [
            {lock_organization_memberships_tx, 3, fun(_, ?ORG_ID, _) -> {ok, []} end}
        ]},
        {organization_member_repo, [
            {'find_organization_for_share_tx', 3, fun(_, ?ORG_ID, <<"id,owner_id,status">>) ->
                {ok, #{
                    <<"id">> => ?ORG_ID,
                    <<"owner_id">> => ?OWNER,
                    <<"status">> => <<"active">>
                }}
            end},
            {'find_active_for_share_tx', 4, fun(_, ?ORG_ID, ActualUid, <<"role">>) ->
                ?assertEqual(ActorUid, ActualUid),
                {ok, #{<<"role">> => ActorRole}}
            end},
            {'upsert_active_tx', 5, fun(_, ?ORG_ID, ?MEMBER, Role, InvitedBy) ->
                ?assertEqual(ActorUid, InvitedBy),
                {ok, changed, #{<<"role">> => Role}}
            end},
            {'find_active_tx', 4, fun(_, ?ORG_ID, ?MEMBER, _) ->
                {ok, #{
                    <<"organization_id">> => ?ORG_ID,
                    <<"user_id">> => ?MEMBER,
                    <<"role">> => <<"member">>,
                    <<"status">> => <<"active">>
                }}
            end},
            {'find_for_update_tx', 4, fun(_, ?ORG_ID, TargetUid, <<"role,status">>) ->
                case TargetUid of
                    ?OWNER -> {ok, #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}};
                    ?ADMIN -> {ok, #{<<"role">> => <<"admin">>, <<"status">> => <<"active">>}};
                    ?MEMBER -> {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}}
                end
            end},
            {'update_role_tx', 4, fun(_, ?ORG_ID, ?MEMBER, _Role) -> ok end},
            {'remove_tx', 3, fun(_, ?ORG_ID, TargetUid) when
                TargetUid =:= ?MEMBER; TargetUid =:= ?ADMIN
            ->
                ok
            end}
        ]}
    ].

run_tx(Tx) ->
    try Tx(fake_conn) of
        Result -> Result
    catch
        throw:{abort_tx, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% EB-08：通用 suspend（Core 能力，S1 Gate）
%% ===================================================================

%% @doc suspend 只做一件事：把 organization_member.status 从 active 置为
%% suspended（**唯一**一条写语句，org 作用域显式），不动个人账号、不动经办关系。
suspend_marks_member_suspended_with_a_single_scoped_statement_test_() ->
    ?WITH_MECKS(
        suspend_mocks(),
        fun() ->
            ?assertMatch(
                {ok, #{
                    organization_id := ?ORG_ID,
                    user_id := ?MEMBER,
                    status := <<"suspended">>
                }},
                organization_member_logic:suspend(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            Sqls = lists:reverse(get(t_sqls)),
            ?assertEqual(1, length(Sqls)),
            [Sql] = Sqls,
            ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE organization_member">>)),
            %% 唯一的目标状态来自参数（$4），来源态同样参数化（$3）——SQL 里没有
            %% 任何状态字面量可被篡改
            ?assertNotEqual(nomatch, binary:match(Sql, <<"SET status = $4">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"organization_id = $1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"user_id = $2">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"status = $3">>)),
            ?assertEqual(
                [?ORG_ID, ?MEMBER, <<"active">>, <<"suspended">>], get(t_params)
            )
        end
    ).

%% @doc 主 Owner 不能被暂停（409，且零写入）；非 Owner/Admin 不得暂停（403，零写入）。
suspend_protects_primary_owner_and_requires_governance_test_() ->
    ?WITH_MECKS(
        suspend_mocks(),
        fun() ->
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:suspend(?OWNER, ?ORG_ID, ?OWNER)
            ),
            ?assertEqual(undefined, erase(t_sqls)),
            ?assertMatch(
                {error, {403, _}},
                organization_member_logic:suspend(?MEMBER, ?ORG_ID, ?ADMIN)
            ),
            ?assertEqual(undefined, erase(t_sqls))
        end
    ).

%% @doc 已 suspended / removed 的成员再次撤权 ⇒ 409（明确拒绝，不静默成功）。
suspend_is_not_idempotent_silently_test_() ->
    ?WITH_MECKS(
        suspend_mocks(),
        fun() ->
            put(t_target_status, <<"suspended">>),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:suspend(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(undefined, erase(t_sqls)),
            erase(t_target_status)
        end
    ).

%% @doc 参数形状：非法 target 直接 400，不触库。
suspend_validates_arguments_test() ->
    ?assertMatch(
        {error, {400, _}},
        organization_member_logic:suspend(?OWNER, ?ORG_ID, 0)
    ).

%% suspend 的 meck 期望：只读查找走 repo，唯一写语句走 elib_pg:execute/3。
suspend_mocks() ->
    [
        {elib_pg, [
            {'with_tx', 1, fun run_tx/1},
            {'execute', 3, fun(_Conn, Sql, Params) ->
                put(t_sqls, [iolist_to_binary(Sql) | sqls_so_far()]),
                put(t_params, Params),
                {ok, 1}
            end}
        ]},
        {organization_member_repo, [
            {'find_organization_for_share_tx', 3, fun(_, ?ORG_ID, <<"id,owner_id,status">>) ->
                {ok, #{
                    <<"id">> => ?ORG_ID,
                    <<"owner_id">> => ?OWNER,
                    <<"status">> => <<"active">>
                }}
            end},
            {'find_active_for_share_tx', 4, fun(_, ?ORG_ID, ActorUid, <<"role">>) ->
                case ActorUid of
                    ?MEMBER -> {ok, #{<<"role">> => <<"member">>}};
                    _ -> {ok, #{<<"role">> => <<"owner">>}}
                end
            end},
            {'find_for_update_tx', 4, fun(_, ?ORG_ID, TargetUid, <<"role,status">>) ->
                Status =
                    case TargetUid of
                        ?OWNER -> <<"active">>;
                        _ -> target_status()
                    end,
                Role =
                    case TargetUid of
                        ?OWNER -> <<"owner">>;
                        ?ADMIN -> <<"admin">>;
                        _ -> <<"member">>
                    end,
                {ok, #{<<"role">> => Role, <<"status">> => Status}}
            end}
        ]}
    ].

%% 进程字典读默认值（`erlang:get/1` 无 /2 版本）。
sqls_so_far() ->
    case get(t_sqls) of
        undefined -> [];
        Sqls -> Sqls
    end.

target_status() ->
    case get(t_target_status) of
        undefined -> <<"active">>;
        Status -> Status
    end.

%% ===================================================================
%% EB-D07：通用 restore（suspended → active 复位端）
%% ===================================================================

%% @doc restore 只做一件事：把 organization_member.status 从 suspended 置回
%% active（**唯一**一条写语句，org 作用域显式），与 suspend 共用同一迁移通道。
restore_marks_suspended_member_active_with_a_single_scoped_statement_test_() ->
    ?WITH_MECKS(
        suspend_mocks(),
        fun() ->
            put(t_target_status, <<"suspended">>),
            ?assertMatch(
                {ok, #{
                    organization_id := ?ORG_ID,
                    user_id := ?MEMBER,
                    role := <<"member">>,
                    status := <<"active">>
                }},
                organization_member_logic:restore(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            Sqls = lists:reverse(get(t_sqls)),
            ?assertEqual(1, length(Sqls)),
            [Sql] = Sqls,
            ?assertNotEqual(nomatch, binary:match(Sql, <<"UPDATE organization_member">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"SET status = $4">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"organization_id = $1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"user_id = $2">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"status = $3">>)),
            ?assertEqual(
                [?ORG_ID, ?MEMBER, <<"suspended">>, <<"active">>], get(t_params)
            )
        end
    ).

%% @doc 非 suspended 来源（active / removed）恢复 ⇒ 409 明确拒绝，零写入；
%% 非成员（not_found）同样 409。removed 是终态：恢复走重新邀请，不静默复活。
restore_rejects_non_suspended_member_test_() ->
    ?WITH_MECKS(
        suspend_mocks(),
        fun() ->
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:restore(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(undefined, erase(t_sqls)),
            put(t_target_status, <<"removed">>),
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:restore(?OWNER, ?ORG_ID, ?MEMBER)
            ),
            ?assertEqual(undefined, erase(t_sqls))
        end
    ).

%% @doc 参数形状：非法 target 直接 400，不触库。
restore_validates_arguments_test() ->
    ?assertMatch(
        {error, {400, _}},
        organization_member_logic:restore(?OWNER, ?ORG_ID, 0)
    ).

%% ===================================================================
%% EB-08：dependent_resources 的 409 映射（Core 能力，S3 Gate）
%% ===================================================================

%% @doc 纯函数：只有「已知的 Core 依赖守卫 + 23514」才映射为 409；其余一律不映射。
dependent_resources_mapping_is_narrow_and_generic_test() ->
    Guard = <<"trg_organization_member_offboarding_guard">>,
    {conflict, Message} = organization_member_logic:dependent_resources_conflict(
        db_error(<<"23514">>, Guard)
    ),
    ?assert(is_binary(Message)),
    ?assertNotEqual(nomatch, binary:match(Message, <<"依赖资源"/utf8>>)),
    %% 负例：别的 23514 约束、别的错误码、非错误项都不映射（不是「一律 409」）
    ?assertEqual(
        none,
        organization_member_logic:dependent_resources_conflict(
            db_error(<<"23514">>, <<"ck_organization_member_status">>)
        )
    ),
    ?assertEqual(
        none,
        organization_member_logic:dependent_resources_conflict(
            db_error(<<"23503">>, Guard)
        )
    ),
    ?assertEqual(none, organization_member_logic:dependent_resources_conflict(unavailable)),
    ?assertEqual(none, organization_member_logic:dependent_resources_conflict(undefined)).

%% @doc 接线：DB 守卫拒绝移除时，remove/3 返回 409（可区分），而不是被压成 500。
dependent_resources_rejection_reaches_caller_as_409_test_() ->
    Guard = <<"trg_organization_member_offboarding_guard">>,
    ?WITH_MECKS(
        remove_mocks(fun(_, ?ORG_ID, ?MEMBER) -> {error, db_error(<<"23514">>, Guard)} end),
        fun() ->
            ?assertMatch(
                {error, {409, _}},
                organization_member_logic:remove(?OWNER, ?ORG_ID, ?MEMBER)
            )
        end
    ).

%% @doc 负例（有牙齿）：同一个 remove 路径上的**其它**数据库错误仍是 500，
%% 说明 409 是窄映射而不是「把任何失败都说成被依赖资源引用」。
other_db_errors_stay_internal_test_() ->
    ?WITH_MECKS(
        remove_mocks(fun(_, ?ORG_ID, ?MEMBER) ->
            {error, db_error(<<"23503">>, <<"fk_something">>)}
        end),
        fun() ->
            ?assertMatch(
                {error, {500, _}},
                organization_member_logic:remove(?OWNER, ?ORG_ID, ?MEMBER)
            )
        end
    ).

remove_mocks(RemoveTxFun) ->
    [
        {elib_pg, [
            {'with_tx', 1, fun run_tx/1}
        ]},
        {workspace_member_repo, [
            {lock_organization_memberships_tx, 3, fun(_, ?ORG_ID, _) -> {ok, []} end}
        ]},
        {organization_member_repo, [
            {'find_organization_for_share_tx', 3, fun(_, ?ORG_ID, <<"id,owner_id,status">>) ->
                {ok, #{
                    <<"id">> => ?ORG_ID,
                    <<"owner_id">> => ?OWNER,
                    <<"status">> => <<"active">>
                }}
            end},
            {'find_active_for_share_tx', 4, fun(_, ?ORG_ID, _ActorUid, <<"role">>) ->
                {ok, #{<<"role">> => <<"owner">>}}
            end},
            {'find_for_update_tx', 4, fun(_, ?ORG_ID, _TargetUid, <<"role,status">>) ->
                {ok, #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}}
            end},
            {'remove_tx', 3, RemoveTxFun}
        ]}
    ].
db_error(Code, Constraint) ->
    #error{
        severity = error,
        code = Code,
        codename = check_violation,
        message = <<"synthetic">>,
        extra = [{constraint_name, Constraint}]
    }.

%% ===================================================================
%% EB-08-A06：依赖方向（Feature -> Core），零反向引用
%% ===================================================================

%% @doc Core 的离职/暂停段只依赖通用能力：文件里不得出现任何纵切单元的模块名。
%% 判定函数本身带负例（对含反向引用的夹具必判红），故不是恒真断言。
core_has_zero_feature_module_references_test() ->
    {ok, Src} = file:read_file(?CORE_REL),
    ?assertEqual([], feature_refs(Src)),
    ?assertNotEqual(
        [],
        feature_refs(<<"f() -> enterprise_business_facade:open_offboarding(1, #{}).">>)
    ),
    ?assertNotEqual(
        [],
        feature_refs(<<"f() -> customer_service_logic:handover(1).">>)
    ),
    ?assertEqual(
        [],
        feature_refs(<<"f() -> organization_member_repo:remove_tx(C, 1, 2).">>)
    ).

feature_refs(Bin) ->
    case
        re:run(Bin, "\\b(enterprise_[a-z0-9_]+|customer_service_[a-z0-9_]+)", [
            global, {capture, first, binary}
        ])
    of
        {match, Matches} -> Matches;
        nomatch -> []
    end.
