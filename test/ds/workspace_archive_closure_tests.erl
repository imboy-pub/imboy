-module(workspace_archive_closure_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% Workspace 归档守卫全路径收口（P0）— 收口路径 980 拒绝回归
%%%
%%% 镜像 test/logic/workspace_archive_tests.erl 模式：mock workspace_resolver
%%% （archived workspace）+ elib_pg（with_tx/行锁查询），跑**真 DS 写函数**，
%%% 断言归档后写入被拒（稳定错误码 980）且底层 repo 写不执行。
%%% 覆盖清单（改前状态 → 改后同事务守卫）：
%%%   频道：评论 add/delete/like/unlike（原 R3 #9 前置检查）、反应
%%%   insert/delete_reaction（#10 前置检查）、频道管理员 add/delete/update_role
%%%   （#13 前置检查）、消息 update/delete/revoke（置顶/编辑/删除/撤回，原无守卫）、
%%%   频道设置 update/delete（原无守卫）、workspace 域建频道 create_channel
%%%   （原无守卫）、邀请 create/accept/reject（原无守卫）、webhook create/disable
%%%   （原无守卫）、浏览计数 insert_message_view（派生：skip 不落库）；
%%%   群：公告见 workspace_archive_tests（R3 #17）、群资料 update_by_id（edit/
%%%   set_e2ee_mode 共用，原无守卫）、解散 dissolve_group（原无守卫）、
%%%   入群 join_group / 角色变更 update_role（原无守卫）、退群 leave（freeze：静默
%%%   ok 不落库）、工作区建群 workspace_add_tx（原无守卫）；
%%%   附件：转正落库 verify_and_save（group/channel scope，原无守卫）。

-define(WS_ID, 800001).
-define(UID, 900001).
-define(GID, 777001).
-define(CID, 666001).
-define(MID, 555001).
-define(COMMENT_ID, 444001).
-define(NOTICE_ID, 333001).
-define(INV_ID, 222001).

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

%%% 归档 workspace + 全资源可解析的 mock 基座
archived_mocks() ->
    [
        {workspace_resolver, [
            {'resolve_workspace', 1, fun
                ({workspace, ?WS_ID}) -> {ok, ?WS_ID};
                ({group, ?GID}) -> {ok, ?WS_ID};
                ({channel, ?CID}) -> {ok, ?WS_ID};
                ({channel_message, ?MID}) -> {ok, ?WS_ID};
                ({channel_comment, ?COMMENT_ID}) -> {ok, ?WS_ID};
                ({group_notice, ?NOTICE_ID}) -> {ok, ?WS_ID};
                ({channel_invitation, ?INV_ID}) -> {ok, ?WS_ID};
                (_) -> personal
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'one', 2, fun(<<"SELECT status FROM workspace", _/binary>>, _) ->
                {ok, #{<<"status">> => <<"archived">>}}
            end},
            {'query', 3, fun
                (fake_conn, <<"SELECT status FROM workspace", _/binary>>, _) ->
                    {ok, [#{<<"status">> => <<"archived">>}]};
                (fake_conn, _Sql, _Params) ->
                    {ok, []}
            end},
            {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
        ]}
    ].

%%% personal 资源直通（回归红线：个人频道/群零行为变化）
personal_mocks() ->
    [
        {workspace_resolver, [
            {'resolve_workspace', 1, fun(_) -> personal end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'query', 3, fun(_C, <<"SELECT status FROM workspace", _/binary>>, _) ->
                {error, must_not_lock_workspace}
            end},
            {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
        ]}
    ].

%%% ===================================================================
%%% 频道域
%%% ===================================================================

channel_closure_test_() ->
    [
        {"channel_comment add/delete/like/unlike rejected 980 (R3 #9)", fun() ->
            MustNot = [
                {channel_comment_repo, [
                    {'add_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'delete_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'increment_like_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'decrement_like_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_comment_ds:add(#{<<"channel_id">> => ?CID})
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_comment_ds:delete(?COMMENT_ID)
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_comment_ds:like(?COMMENT_ID)
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_comment_ds:unlike(?COMMENT_ID)
                )
            end)
        end},
        {"channel reaction insert/delete rejected 980 (R3 #10)", fun() ->
            MustNot = [
                {channel_repo, [
                    {'insert_reaction_tx', 6, fun(_, _, _, _, _, _) ->
                        {error, must_not_write}
                    end},
                    {'delete_reaction_tx', 5, fun(_, _, _, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_ds:insert_reaction(?CID, ?MID, ?UID, <<"like">>, 1)
                ),
                ?assertMatch(
                    {error, {980, _}},
                    channel_ds:delete_reaction(?CID, ?MID, ?UID, <<"like">>)
                )
            end)
        end},
        {"channel message update/delete/revoke rejected 980", fun() ->
            MustNot = [
                {channel_message_repo, [
                    {'update_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'delete_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'revoke_tx', 4, fun(_, _, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}}, channel_message_ds:update(?MID, #{is_pinned => true})
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_message_ds:delete(?MID)
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_message_ds:revoke(?MID, ?UID, <<"now">>)
                )
            end)
        end},
        {"channel settings update/delete rejected 980", fun() ->
            MustNot = [
                {channel_repo, [
                    {'update_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'delete_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}}, channel_ds:update(?CID, #{name => <<"x">>})
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_ds:delete(?CID)
                )
            end)
        end},
        {"channel admin add/delete/update_role rejected 980 (R3 #13)", fun() ->
            MustNot = [
                {channel_admin_repo, [
                    {'add', 2, fun(_, _) -> {error, must_not_write} end},
                    {'delete_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'update_role_tx', 4, fun(_, _, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_admin_ds:add(#{channel_id => ?CID, user_id => ?UID, role => 2})
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_admin_ds:delete(?CID, ?UID)
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_admin_ds:update_role(?CID, ?UID, 2)
                )
            end)
        end},
        {"workspace-scope channel creation rejected 980", fun() ->
            MustNot = [
                {channel_repo, [
                    {'add', 2, fun(_, _) -> {error, must_not_create} end}
                ]},
                {imboy_cache, [
                    {'flush', 1, fun(_) -> ok end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_ds:create_channel(?UID, <<"ws-ch">>, #{
                        scope => workspace, workspace_id => ?WS_ID
                    })
                )
            end)
        end},
        {"channel invitation create/accept/reject rejected 980", fun() ->
            MustNot = [
                {channel_invitation_repo, [
                    {'create_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'accept_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'reject_tx', 3, fun(_, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_invitation_ds:create(#{
                        channel_id => ?CID, inviter_uid => ?UID, invitee_uid => ?UID + 1
                    })
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_invitation_ds:accept(?INV_ID, ?UID)
                ),
                ?assertMatch(
                    {error, {980, _}}, channel_invitation_ds:reject(?INV_ID, ?UID)
                )
            end)
        end},
        {"channel webhook create short-circuits before bot user", fun() ->
            MustNot = [
                {user_repo, [
                    {'create', 1, fun(_) -> {error, must_not_create_bot} end}
                ]},
                {channel_webhook_repo, [
                    {'add_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}}, channel_webhook_ds:create(?CID, <<"hook">>, ?UID)
                ),
                ?assertEqual(0, meck:num_calls(user_repo, create, 1))
            end)
        end},
        {"channel webhook disable rejected 980", fun() ->
            MustNot = [
                {channel_webhook_repo, [
                    {'set_status_tx', 4, fun(_, _, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}}, channel_webhook_ds:disable(?CID, 1)
                )
            end)
        end},
        {"message view insert skips when archived (derived, read stays open)", fun() ->
            MustNot = [
                {channel_repo, [
                    {'insert_message_view_tx', 5, fun(_, _, _, _, _) ->
                        {error, must_not_write}
                    end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertEqual(
                    {ok, 0}, channel_ds:insert_message_view(?CID, ?MID, ?UID, 1)
                )
            end)
        end},
        {"personal channel comment/reaction pass without workspace lock", fun() ->
            RepoOk = [
                {channel_comment_repo, [
                    {'add_tx', 2, fun(_, _) -> {ok, 1} end}
                ]},
                {channel_repo, [
                    {'insert_reaction_tx', 6, fun(_, _, _, _, _, _) -> {ok, 1} end}
                ]}
            ],
            ?WITH_MECKS(personal_mocks() ++ RepoOk, fun() ->
                ?assertMatch(
                    {ok, 1}, channel_comment_ds:add(#{<<"channel_id">> => ?CID})
                ),
                ?assertMatch(
                    {ok, 1}, channel_ds:insert_reaction(?CID, ?MID, ?UID, <<"like">>, 1)
                )
            end)
        end}
    ].

%%% ===================================================================
%%% 群域
%%% ===================================================================

group_closure_test_() ->
    [
        {"group profile update_by_id rejected 980 (edit/set_e2ee_mode 共用)", fun() ->
            MustNot = [
                {group_repo, [
                    {'update_by_id_tx', 3, fun(_, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}}, group_ds:update_by_id(?GID, #{title => <<"x">>})
                )
            end)
        end},
        {"group dissolve rejected 980", fun() ->
            MustNot = [
                {imboy_cache, [
                    {'get', 1, fun(_) -> undefined end}
                ]},
                {group_log_repo, [
                    {'add', 2, fun(_, _) -> {error, must_not_log} end}
                ]},
                {group_member_repo, [
                    {'list_by_gid', 3, fun(_, _, _) -> {ok, []} end}
                ]}
            ],
            G = #{<<"id">> => ?GID, <<"owner_uid">> => ?UID},
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}}, group_ds:dissolve_group(?UID, ?GID, ?UID, G)
                )
            end)
        end},
        {"group member join rejected 980 in tx", fun() ->
            MustNot = [
                {group_member_ds, [
                    {'join_group', 5, fun(_, _, _, _, _) -> {error, must_not_join} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_member_logic:join_group(<<"invite">>, ?UID, ?GID, #{})
                )
            end)
        end},
        {"group member leave freezes silently when archived", fun() ->
            MustNot = [
                {group_member_ds, [
                    {'leave', 4, fun(_, _, _, _) -> {error, must_not_leave} end}
                ]},
                {group_ds, [
                    {"leave", 2, fun(_, _) -> {error, must_not_cache} end}
                ]},
                {imboy_domain_event, [
                    {'publish', 1, fun(_) -> {error, must_not_publish} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertEqual(ok, group_member_logic:leave(?UID, ?GID, ?UID))
            end)
        end},
        {"group member role change rejected 980 in tx", fun() ->
            PermOk = [
                {group_member_ds, [
                    {'get_member_info', 3, fun(_, _, _) -> {ok, #{<<"role">> => 4}} end},
                    {'find_by_gid_and_uid', 3, fun(_, _, _) -> #{<<"id">> => 1} end},
                    {'update_role', 4, fun(_, _, _, _) -> {error, must_not_write} end},
                    {'update_role', 5, fun(_, _, _, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ PermOk, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_member_logic:update_role(?UID, ?GID, ?UID + 1, 1)
                )
            end)
        end},
        {"workspace group creation rejected 980 (workspace_add_tx)", fun() ->
            MustNot = [
                {workspace_logic, [
                    {'ensure_can_create_resource', 2, fun(_, _) -> ok end}
                ]},
                {group_ds, [
                    {'create_scoped_group', 7, fun(_, _, _, _, _, _, _) ->
                        {error, must_not_create}
                    end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertEqual(
                    {error, ?ERR_WORKSPACE_ARCHIVED},
                    group_logic:add(0, ?UID, 2, [], {?WS_ID, <<"workspace">>})
                )
            end)
        end},
        {"personal group profile update passes without workspace lock", fun() ->
            RepoOk = [
                {group_repo, [
                    {'update_by_id_tx', 3, fun(_, _, _) -> {ok, 1} end}
                ]}
            ],
            ?WITH_MECKS(personal_mocks() ++ RepoOk, fun() ->
                ?assertMatch(
                    {ok, 1}, group_ds:update_by_id(?GID, #{title => <<"x">>})
                )
            end)
        end}
    ].

%%% ===================================================================
%%% 附件转正落库（scope=group/channel）
%%% ===================================================================

attachment_save_closure_test_() ->
    OssMock =
        {elib_oss, [
            {'get_bucket', 1, fun(<<"group">>) -> <<"bucket">> end},
            {'head_object', 2, fun(_, _) ->
                {ok, #{size => 10, content_type => <<"image/png">>}}
            end},
            {'max_file_size', 0, fun() -> 1000 end},
            {'validate_file_type', 1, fun(_) -> true end}
        ]},
    AttachMock =
        {attachment_ds, [
            {'save', 4, fun(_, _, _, _) -> {error, must_not_save} end}
        ]},
    Meta = #{<<"cipher">> => null},
    [
        {"group-scope attachment confirm rejected 980", fun() ->
            ?WITH_MECKS(archived_mocks() ++ [OssMock, AttachMock], fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    attach_logic:verify_and_save(
                        ?UID, <<"k">>, <<"group">>, integer_to_binary(?GID), Meta
                    )
                )
            end)
        end},
        {"channel-scope attachment confirm rejected 980", fun() ->
            ?WITH_MECKS(archived_mocks() ++ [OssMock, AttachMock], fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    attach_logic:verify_and_save(
                        ?UID, <<"k">>, <<"channel">>, integer_to_binary(?CID), Meta
                    )
                )
            end)
        end},
        {"c2c-scope attachment confirm passes (personal domain)", fun() ->
            ConvMock =
                {conv_key_vo, [
                    {'c2c_members', 1, fun(_) -> {ok, {?UID, ?UID + 1}} end}
                ]},
            SaveOk =
                {attachment_ds, [
                    {'save', 4, fun(_, _, _, _) -> ok end},
                    {'pending_remove', 1, fun(_) -> ok end}
                ]},
            PublicMock =
                {elib_oss, [
                    {'get_bucket', 1, fun(<<"c2c">>) -> <<"bucket">> end},
                    {'head_object', 2, fun(_, _) ->
                        {ok, #{size => 10, content_type => <<"image/png">>}}
                    end},
                    {'max_file_size', 0, fun() -> 1000 end},
                    {'validate_file_type', 1, fun(_) -> true end}
                ]},
            ?WITH_MECKS(personal_mocks() ++ [PublicMock, ConvMock, SaveOk], fun() ->
                ?assertMatch(
                    {ok, _},
                    attach_logic:verify_and_save(
                        ?UID, <<"k">>, <<"c2c">>, <<"ref">>, Meta
                    )
                )
            end)
        end}
    ].

%%% ===================================================================
%%% write_tx / write_tx_or_skip 助手
%%% ===================================================================

guard_helper_test_() ->
    [
        {"write_tx executes fun after guard passes", fun() ->
            ?WITH_MECKS(personal_mocks(), fun() ->
                ?assertEqual(
                    {written, 42},
                    workspace_guard:write_tx({group, ?GID}, fun(_Conn) -> {written, 42} end)
                )
            end)
        end},
        {"write_tx_or_skip returns skipped when archived", fun() ->
            ?WITH_MECKS(archived_mocks(), fun() ->
                ?assertEqual(
                    skipped,
                    workspace_guard:write_tx_or_skip({group, ?GID}, fun(_Conn) ->
                        {written, must_not_run}
                    end)
                )
            end)
        end},
        {"write_tx_or_skip returns written when active", fun() ->
            ActiveMocks =
                {elib_pg, [
                    {'with_tx', 1, tx_fun()},
                    {'query', 3, fun(fake_conn, <<"SELECT status FROM workspace", _/binary>>, _) ->
                        {ok, [#{<<"status">> => <<"active">>}]}
                    end}
                ]},
            ResolverMock =
                {workspace_resolver, [
                    {'resolve_workspace', 1, fun({group, ?GID}) -> {ok, ?WS_ID} end}
                ]},
            ?WITH_MECKS([ResolverMock, ActiveMocks], fun() ->
                ?assertEqual(
                    {written, ok},
                    workspace_guard:write_tx_or_skip({group, ?GID}, fun(_Conn) -> ok end)
                )
            end)
        end}
    ].
