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
%%%
%%% P0 后续批（群子功能域 7 域，均原无守卫 → DS 层同事务守卫）：
%%%   vote：insert_vote/insert_options_batch/insert_record/update_record/
%%%         delete_record/update_vote_status（980）；
%%%   schedule：insert_schedule/update_schedule/update_status/insert_participant/
%%%         update_participant_status/delete_participant/insert_remind（980）、
%%%         update_remind_sent + process_reminders 提醒推送门控（freeze：归档
%%%         零推送泄漏）；
%%%   album：create_album/upload_photo（OSS 前预检）/delete_photo/like/unlike/
%%%          add_comment/update_album/update_album_cover/delete_album（980）；
%%%   file：upload_file（OSS 前预检）/delete_file/soft_delete（980）、
%%%         download_file 下载计数（freeze：下载读取永不 403）；
%%%   task：insert_task/update_task/soft_delete/restore/assignment_insert/
%%%         assignment_update（980，含 adm 治理共用入口；logic 侧不再吞错）；
%%%   tag：add（去重+插入同事务）/remove/delete（980）；
%%%   category：move_group_to_category（freeze：用户个人归类静默跳过）；
%%%   分类 CRUD 为 user_id 键控个人数据，不加守卫（personal 红线）。

-define(WS_ID, 800001).
-define(UID, 900001).
-define(GID, 777001).
-define(CID, 666001).
-define(MID, 555001).
-define(COMMENT_ID, 444001).
-define(NOTICE_ID, 333001).
-define(INV_ID, 222001).
%% P0 后续批：群子功能域资源 ID
-define(VOTE_ID, <<"vote_t1">>).
-define(VOTE_REC_ID, 111001).
-define(SCHED_SID, <<"sched_t1">>).
-define(SCHED_PK, 112001).
-define(REMIND_ID, 113001).
-define(ALBUM_PK, 114001).
-define(PHOTO_ID, <<"photo_t1">>).
-define(FILE_PK, 115001).
-define(TASK_PK, 116001).
-define(TASK_ID, <<"task_t1">>).
-define(ASSIGN_ID, 117001).

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
                %% P0 后续批：群子功能域
                ({group_vote, ?VOTE_ID}) -> {ok, ?WS_ID};
                ({group_vote_record, ?VOTE_REC_ID}) -> {ok, ?WS_ID};
                ({group_schedule, ?SCHED_SID}) -> {ok, ?WS_ID};
                ({group_schedule, ?SCHED_PK}) -> {ok, ?WS_ID};
                ({group_schedule_remind, ?REMIND_ID}) -> {ok, ?WS_ID};
                ({group_album, ?ALBUM_PK}) -> {ok, ?WS_ID};
                ({group_album_photo, ?PHOTO_ID}) -> {ok, ?WS_ID};
                ({group_file, ?FILE_PK}) -> {ok, ?WS_ID};
                ({group_task, ?TASK_PK}) -> {ok, ?WS_ID};
                ({group_task, ?TASK_ID}) -> {ok, ?WS_ID};
                ({group_task_assignment, ?ASSIGN_ID}) -> {ok, ?WS_ID};
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
%%% 群子功能域（P0 后续批）
%%% ===================================================================

subdomain_closure_test_() ->
    [
        {"vote writes rejected 980 (create/cast/close/record)", fun() ->
            MustNot = [
                {group_vote_repo, [
                    {'insert_vote_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'insert_options_batch_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'insert_record_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'update_record_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'delete_record_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'update_vote_status_tx', 3, fun(_, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_vote_ds:insert_vote(#{
                        group_id => ?GID,
                        vote_id => ?VOTE_ID,
                        title => <<"t">>,
                        creator_id => ?UID
                    })
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_vote_ds:insert_options_batch([
                        #{vote_id => ?VOTE_ID, option_id => <<"o1">>, option_text => <<"a">>}
                    ])
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_vote_ds:insert_record(#{
                        vote_id => ?VOTE_ID, user_id => ?UID, option_ids => <<"[]">>
                    })
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_vote_ds:update_record(?VOTE_REC_ID, #{option_ids => <<"[]">>})
                ),
                ?assertMatch(
                    {error, {980, _}}, group_vote_ds:delete_record(?VOTE_REC_ID)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_vote_ds:update_vote_status(?VOTE_ID, 2)
                )
            end)
        end},
        {"vote logic normalizes 980 (close_vote)", fun() ->
            RepoOk = [
                {group_vote_repo, [
                    {'find_by_vote_id', 1, fun(_) ->
                        {ok, #{
                            <<"creator_id">> => ?UID,
                            <<"group_id">> => ?GID,
                            <<"status">> => 1
                        }}
                    end},
                    {'update_vote_status_tx', 3, fun(_, _, _) -> {error, must_not_write} end}
                ]},
                {group_member_ds, [
                    {'check_admin', 2, fun(_, _) -> false end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ RepoOk, fun() ->
                ?assertEqual(
                    {error, 980}, group_vote_logic:close_vote(?VOTE_ID, ?UID)
                )
            end)
        end},
        {"schedule writes rejected 980 (create/update/cancel/participant/remind)", fun() ->
            MustNot = [
                {group_schedule_repo, [
                    {'insert_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'update_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'update_status_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'insert_participant_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'update_participant_status_tx', 4, fun(_, _, _, _) ->
                        {error, must_not_write}
                    end},
                    {'delete_participant_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'insert_remind_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_schedule_ds:insert_schedule(#{
                        group_id => ?GID, title => <<"t">>
                    })
                ),
                ?assertMatch(
                    {error, {980, _}}, group_schedule_ds:update_schedule(?SCHED_PK, #{})
                ),
                ?assertMatch(
                    {error, {980, _}}, group_schedule_ds:update_status(?SCHED_PK, 4)
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_schedule_ds:insert_participant(#{
                        schedule_id => ?SCHED_SID, user_id => ?UID
                    })
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_schedule_ds:update_participant_status(?SCHED_SID, ?UID, 1)
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_schedule_ds:delete_participant(?SCHED_SID, ?UID)
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_schedule_ds:insert_remind(#{schedule_id => ?SCHED_SID})
                )
            end)
        end},
        {"schedule remind marking skips + zero push leak when archived", fun() ->
            Reminds = [
                #{
                    <<"id">> => ?REMIND_ID,
                    <<"schedule_id">> => ?SCHED_SID,
                    <<"user_id">> => ?UID
                }
            ],
            Mocks = [
                {group_schedule_repo, [
                    {'list_pending_reminds', 0, fun() -> {ok, Reminds} end},
                    {'update_remind_sent_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]},
                {msg_s2c_ds, [
                    {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(must_not_push) end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ Mocks, fun() ->
                %% freeze：已发送标记跳过不报错
                ?assertEqual({ok, 0}, group_schedule_ds:update_remind_sent(?REMIND_ID)),
                %% 归档后提醒推送零泄漏（通知发送在守卫之后才会执行）
                ?assertEqual({ok, 0}, group_schedule_logic:process_reminders()),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end},
        {"album writes rejected 980 (album/photo/like/comment)", fun() ->
            MemberOk = [
                {group_ds, [
                    {'is_member', 2, fun(_, _) -> true end}
                ]},
                {group_album_repo, [
                    {'find_photo_by_id', 1, fun(_) ->
                        #{
                            <<"id">> => 1,
                            <<"group_id">> => ?GID,
                            <<"uploader_id">> => ?UID,
                            <<"album_id">> => <<"alb">>
                        }
                    end},
                    {'is_liked', 2, fun(_, _) -> false end},
                    {'create_album_tx', 5, fun(_, _, _, _, _) -> {error, must_not_write} end},
                    {'insert_photo_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'delete_photo_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'like_photo_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'unlike_photo_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'add_comment_tx', 4, fun(_, _, _, _) -> {error, must_not_write} end},
                    {'update_album_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'delete_album_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MemberOk, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_album_ds:create_album(?GID, ?UID, <<"album">>, undefined)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_album_ds:delete_photo(1, ?UID)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_album_ds:like_photo(?PHOTO_ID, ?UID)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_album_ds:unlike_photo(?PHOTO_ID, ?UID)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_album_ds:add_comment(?PHOTO_ID, ?UID, <<"c">>)
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_album_ds:update_album(#{<<"id">> => ?ALBUM_PK, album_name => <<"n">>})
                ),
                ?assertMatch(
                    {error, {980, _}}, group_album_ds:delete_album(?ALBUM_PK)
                )
            end)
        end},
        {"album upload_photo short-circuits before OSS when archived", fun() ->
            MemberOk = [
                {group_ds, [
                    {'is_member', 2, fun(_, _) -> true end}
                ]},
                {elib_oss, [
                    {'upload', 3, fun(_, _, _) -> erlang:error(must_not_upload) end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MemberOk, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_album_ds:upload_photo(?GID, ?UID, <<"alb">>, <<0, 0, 0>>, <<"a.png">>)
                ),
                ?assertEqual(0, meck:num_calls(elib_oss, upload, 3))
            end)
        end},
        {"file upload/delete rejected 980 (upload before OSS)", fun() ->
            MemberOk = [
                {group_ds, [
                    {'is_member', 2, fun(_, _) -> true end}
                ]},
                {group_file_repo, [
                    {'find_by_id', 1, fun(_) ->
                        #{
                            <<"id">> => ?FILE_PK,
                            <<"group_id">> => ?GID,
                            <<"uploader_id">> => ?UID,
                            <<"file_url">> => <<"u">>
                        }
                    end},
                    {'insert_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'soft_delete_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]},
                {elib_oss, [
                    {'validate_file_type', 1, fun(_) -> true end},
                    {'upload', 3, fun(_, _, _) -> erlang:error(must_not_upload) end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MemberOk, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_file_ds:upload_file(?GID, ?UID, <<"a.txt">>, <<0>>, <<"text/plain">>)
                ),
                ?assertEqual(0, meck:num_calls(elib_oss, upload, 3)),
                ?assertMatch(
                    {error, {980, _}}, group_file_ds:delete_file(?FILE_PK, ?UID)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_file_ds:soft_delete(?FILE_PK)
                )
            end)
        end},
        {"file download stays open with frozen counter when archived", fun() ->
            MemberOk = [
                {group_ds, [
                    {'is_member', 2, fun(_, _) -> true end}
                ]},
                {group_file_repo, [
                    {'find_by_id', 1, fun(_) ->
                        #{
                            <<"id">> => ?FILE_PK,
                            <<"group_id">> => ?GID,
                            <<"file_url">> => <<"u">>
                        }
                    end},
                    {'increment_download_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MemberOk, fun() ->
                %% 归档可读红线：下载读取永不 403（计数在 spawn 内 freeze 跳过）
                ?assertEqual({ok, <<"u">>}, group_file_ds:download_file(?FILE_PK, ?UID))
            end)
        end},
        {"task writes rejected 980 (task/assignment/adm)", fun() ->
            MustNot = [
                {group_task_repo, [
                    {'insert_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'update_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'soft_delete_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'restore_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]},
                {group_task_assignment_repo, [
                    {'insert_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'update_tx', 3, fun(_, _, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    group_task_ds:insert_task(#{
                        group_id => ?GID, task_id => ?TASK_ID, title => <<"t">>
                    })
                ),
                ?assertMatch(
                    {error, {980, _}}, group_task_ds:update_task(?TASK_PK, #{status => 3})
                ),
                ?assertMatch(
                    {error, {980, _}}, group_task_ds:soft_delete(?TASK_PK)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_task_ds:restore(?TASK_PK)
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_task_ds:assignment_insert(#{
                        task_id => ?TASK_ID, user_id => ?UID
                    })
                ),
                ?assertMatch(
                    {error, {980, _}},
                    group_task_ds:assignment_update(?ASSIGN_ID, #{status => 2})
                )
            end)
        end},
        {"task logic propagates 980 instead of swallowing (create/assign)", fun() ->
            MemberOk = [
                {group_ds, [
                    {'is_member', 2, fun(_, _) -> true end}
                ]},
                {group_task_repo, [
                    {'find_by_id', 1, fun(_) ->
                        {ok, #{<<"creator_id">> => ?UID, <<"task_id">> => ?TASK_ID}}
                    end},
                    {'insert_tx', 2, fun(_, _) -> {error, must_not_write} end},
                    {'update_tx', 3, fun(_, _, _) -> {error, must_not_write} end}
                ]},
                {group_task_assignment_repo, [
                    {'find_by_task_and_user', 2, fun(_, _) -> {error, not_found} end},
                    {'insert_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MemberOk, fun() ->
                ?assertMatch(
                    {error, _, 980},
                    group_task_logic:create(?GID, ?UID, <<"t">>, #{})
                ),
                ?assertMatch(
                    {error, _, 980},
                    group_task_logic:assign(?TASK_PK, [?UID + 1], ?UID)
                )
            end)
        end},
        {"tag writes rejected 980 (add/remove/delete)", fun() ->
            MustNot = [
                {group_tag_repo, [
                    {'exists_tx', 3, fun(_, _, _) -> false end},
                    {'add', 2, fun(_, _) -> {error, must_not_write} end},
                    {'delete_tx', 3, fun(_, _, _) -> {error, must_not_write} end},
                    {'delete_by_group_id_tx', 2, fun(_, _) -> {error, must_not_write} end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                ?assertMatch(
                    {error, {980, _}}, group_tag_ds:add(?GID, ?UID, <<"tag">>)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_tag_ds:remove(?GID, ?UID, <<"tag">>)
                ),
                ?assertMatch(
                    {error, {980, _}}, group_tag_ds:delete(?GID, <<"tag">>)
                )
            end)
        end},
        {"category move_group freezes silently when archived (personal org)", fun() ->
            MustNot = [
                {group_category_repo, [
                    {'update_group_category_tx', 4, fun(_, _, _, _) ->
                        {error, must_not_write}
                    end}
                ]}
            ],
            ?WITH_MECKS(archived_mocks() ++ MustNot, fun() ->
                %% freeze：用户个人群归类静默跳过（与退群 leave 同族），
                %% 分类 CRUD 本身为 user_id 键控个人数据不加守卫。
                ?assertEqual(
                    {ok, 0}, group_category_ds:move_group_to_category(?UID, ?GID, 1)
                )
            end)
        end},
        {"personal sub-domain writes pass without workspace lock", fun() ->
            RepoOk = [
                {group_ds, [
                    {'is_member', 2, fun(_, _) -> true end}
                ]},
                {group_vote_repo, [
                    {'insert_vote_tx', 2, fun(_, _) -> {ok, 1, #{}} end}
                ]},
                {group_schedule_repo, [
                    {'insert_tx', 2, fun(_, _) -> {ok, 1, #{}} end}
                ]},
                {group_task_repo, [
                    {'insert_tx', 2, fun(_, _) -> {ok, 1, #{}} end}
                ]},
                {group_tag_repo, [
                    {'exists_tx', 3, fun(_, _, _) -> false end},
                    {'add', 2, fun(_, _) -> {ok, 1} end}
                ]},
                {group_category_repo, [
                    {'update_group_category_tx', 4, fun(_, _, _, _) -> {ok, 1} end}
                ]},
                {group_album_repo, [
                    {'create_album_tx', 5, fun(_, _, _, _, _) -> {ok, 1} end}
                ]}
            ],
            ?WITH_MECKS(personal_mocks() ++ RepoOk, fun() ->
                ?assertMatch(
                    {ok, 1, _},
                    group_vote_ds:insert_vote(#{
                        group_id => ?GID,
                        vote_id => ?VOTE_ID,
                        title => <<"t">>,
                        creator_id => ?UID
                    })
                ),
                ?assertMatch(
                    {ok, 1, _},
                    group_schedule_ds:insert_schedule(#{group_id => ?GID, title => <<"t">>})
                ),
                ?assertMatch(
                    {ok, 1, _},
                    group_task_ds:insert_task(#{
                        group_id => ?GID, task_id => ?TASK_ID, title => <<"t">>
                    })
                ),
                ?assertMatch(
                    {ok, 1}, group_tag_ds:add(?GID, ?UID, <<"tag">>)
                ),
                ?assertEqual(
                    {ok, 1}, group_category_ds:move_group_to_category(?UID, ?GID, 1)
                ),
                ?assertMatch(
                    {ok, _}, group_album_ds:create_album(?GID, ?UID, <<"album">>, undefined)
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
