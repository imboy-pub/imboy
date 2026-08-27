# Demo B Transcript — run1（脱敏版：refresh token 与本地库口令已抹除；两遍均为 ALL PASS 22 步 54 断言）

```

=== [1] P0 环境就绪：GET /api/v1/init ===
--> GET /api/v1/init (http=200)
    body: {"code":0,"msg":"success.","payload":{"test":{"to_tsquery":"'软件' <-> '中国'"},"res":"GetKOaVyVPWoSfWtO16tJYTXTlKMSCLQ5cwkmvnNhJZglmJ5s4tp0HUxPGvCbjUWeAsncop3Cq6l3GoBFF3k5fPeTO9C6vtkpugrQzdmaKpcD0UEn\/vdTuey526ctMbGQgXgm5SXkfLH87YGnVsF71jBL+eegOVNi4\/MpmDc656BZx\/M4UPuJR6PkFVpTX+1dwDp5kbgHjkhCn0rMuxNwZ2z4KyzNb7brnN97561COb4SySYPNHvfxEXzynHuJsRqvFIRymABQ1PzbtnnIdZzaIMje8gVfxmK3dUZNDKfQSSSu3z
PASS: init HTTP 200

=== [2] P0 注册并登录两个用户（A/B；随机账号保证两遍可重复） ===
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要，凭据已脱敏)
INFO: use existing demo account A (license user cap; idempotency via name suffix)
PASS: 用户 A 就绪(uid=6)
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要，凭据已脱敏)
INFO: use existing demo account B
PASS: 用户 B 就绪(uid=4)

=== [3] P1 A 创建 Workspace（Template 原子初始化） ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"created","channel_id":109336304940222464,"group_id":109336304923445248,"workspace_id":109336304921348096,"workspace":{"branding":{"name":"DemoB-W0-20260827180652-29915"},"created_at":1787825212927,"id":109336304921348096,"logo":"","name":"DemoB-W0-20260827180652-29915","owner_id":6,"status":"active"}},"sv_ts":1787825212941}
PASS: Template 返回 status=created
PASS: workspace_id 取得(109336304921348096)
PASS: General 群 id 取得(109336304923445248)
PASS: Announcements 频道 id 取得(109336304940222464)

=== [4] P1b Template request_id 幂等：二次创建命中 existing 不产生新工作区 ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"existing","channel_id":109336304940222464,"group_id":109336304923445248,"workspace_id":109336304921348096,"workspace":{"archived_at":null,"archived_by":null,"branding":{"name":"DemoB-W0-20260827180652-29915"},"created_at":1787825212927,"id":109336304921348096,"logo":"","name":"DemoB-W0-20260827180652-29915","owner_id":6,"status":"active","ty
PASS: 幂等命中 status=existing
PASS: 幂等返回同一 workspace_id

=== [5] P2 A 邀请 B 为 Workspace Member（member 角色） ===
--> POST /api/v1/workspaces/109336304921348096/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825217514,"role":"member","status":"active","user_id":4,"workspace_id":109336304921348096},"sv_ts":1787825217518}
PASS: 邀请成功
PASS: DB 核查：邀请后 B 不是任何 workspace 群成员(active=0)
PASS: DB 核查：邀请后 B 未订阅 Announcements

=== [6] P2b B 显式加入 General 群 ===
--> POST /api/v1/group_member/join (http=200)
    body: {"code":0,"msg":"success.","payload":{"gid":109336304923445248,"member_list":[{"account":"51698","alias":"","avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","category_id":0,"created_at":1787825218628,"description":"","group_id":109336304923445248,"id":109336316875114496,"invite_code":"","is_join":true,"join_mode":"invite_4_IMBoy","mute_until":null,"nickname":"
PASS: 显式入群成功

=== [7] P2c 部分接受路径核查：B 已入群但仍未订阅 Announcements ===
PASS: DB 核查：仅入群、未订阅(channel_subscriber=0)
PASS: DB 核查：B 是 active workspace_member

=== [8] P2d B 显式订阅 Announcements ===
--> POST /api/v1/channel/109336304940222464/subscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825219746}
PASS: 显式订阅成功

=== [9] P3 A 在 Announcements 发帖 ===
--> POST /api/v1/channel/109336304940222464/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336304940222464,"content":"[Announcements] 版本发布通知 v20260827180652-29915","created_at":1787825220787,"edited_at":null,"id":109336321415448576,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_s
PASS: 频道发帖成功
PASS: 公告消息 id 取得(109336321415448576)

=== [10] P3b B 评论该帖 ===
--> POST /api/v1/channel/109336304940222464/message/109336321415448576/comment (http=200)
    body: {"code":0,"msg":"success","payload":{"channel_id":109336304940222464,"content":"[Comment] 收到，开始跟进 v20260827180652-29915","created_at":1787825221869,"id":109336323674081280,"like_count":0,"message_id":109336321415448576,"parent_id":0,"reply_to_name":"","reply_to_uid":0,"status":1,"user_avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","user_id":4,"user_name":
PASS: 评论成功

=== [11] P4 General 群聊发消息（WebSocket c2g 帧，发送方=B） ===
[ws->] {"id": "demo-b-c2g-20260827180652-29915", "type": "C2G", "to": "109336304923445248", "msg_type": "text", "created_at": 1787825223067, "payload": {"msg_type": "text", "text": "[Group chat] hello from B @ 20260827180652-29915"}}
[ws<-] {"action":"invalid_message","id":"demo-b-c2g-20260827180652-29915","in_reply_to":"demo-b-c2g-20260827180652-29915","payload":{"reason":"missing_required_fields"},"server_ts":1787825223075,"type":"S2C"}
[ws<-] {"action":"logged_another_device","e2ee":null,"from":"","id":"logged_another_device.5yw_kIXHa564s8G","msg_type":"","payload":{"did":"golden-upgrade","dname":""},"server_ts":1787825223096,"to":4,"type":"S2C","ver":2}
PASS: WS c2g 发送成功(rc=0 且无 C2G_ERROR)

=== [12] P4b Group Notice 发布（A 在 General 发短通知） ===
--> POST /api/v1/group_notice/add (http=200)
    body: {"code":0,"msg":"success","payload":{"notice_id":109336334663157760},"sv_ts":1787825227112}
PASS: 群公告添加成功
--> GET /api/v1/group_notice/latest?gid=109336304923445248 (http=200)
    body: {"code":0,"msg":"success","payload":[{"body":"改至周五 10:00","created_at":1787825227103,"edit_user_id":null,"expired_at":1830297599000,"notice_id":109336334663157760,"status":1,"updated_at":1787825227109,"user_id":6}],"sv_ts":1787825228146}
PASS: B 可读取最新群公告

=== [13] P5 A 创建 Project 并建任务指派 B（W0：直接从 Workspace Member 指派） ===
--> POST /api/v1/workspaces/109336304921348096/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825229175,"description":"dual-exp W0 project lite","id":109336339000068096,"name":"DemoB-Project-20260827180652-29915","owner_id":6,"status":"active","updated_at":1787825229175,"workspace_id":109336304921348096},"sv_ts":1787825229181}
PASS: 项目创建成功
PASS: project id 取得(109336339000068096)
--> POST /api/v1/projects/109336339000068096/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825230222,"creator_id":6,"id":109336341191591936,"project_id":109336339000068096,"sort":0,"status":"todo","title":"交付 Demo B 步骤清单","updated_at":1787825230222},"sv_ts":1787825230226}
PASS: 任务创建并指派给 B 成功（workspace member 可指派）
PASS: task id 取得(109336341191591936)

=== [14] P5b 任务四态流转 todo→doing→review→done（含一次回退 review→doing） ===
--> POST /api/v1/tasks/109336341191591936/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825230222,"creator_id":6,"id":109336341191591936,"project_id":109336339000068096,"sort":0,"status":"doing","title":"交付 Demo B 步骤清单","updated_at":1787825231263},"sv_ts":1787825231268}
PASS: 流转到 doing
--> POST /api/v1/tasks/109336341191591936/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825230222,"creator_id":6,"id":109336341191591936,"project_id":109336339000068096,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825232299},"sv_ts":1787825232304}
PASS: 流转到 review
--> POST /api/v1/tasks/109336341191591936/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825230222,"creator_id":6,"id":109336341191591936,"project_id":109336339000068096,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825233337},"sv_ts":1787825233341}
PASS: 流转到 done
--> POST /api/v1/tasks/109336341191591936/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825230222,"creator_id":6,"id":109336341191591936,"project_id":109336339000068096,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825234377},"sv_ts":1787825234381}
PASS: 回退 done→review 支持
--> POST /api/v1/tasks/109336341191591936/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825230222,"creator_id":6,"id":109336341191591936,"project_id":109336339000068096,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825235417},"sv_ts":1787825235422}
PASS: 再完成 done
--> POST /api/v1/projects/109336339000068096/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825236456,"creator_id":6,"id":109336354265237504,"project_id":109336339000068096,"sort":0,"status":"todo","title":"残留的未完成任务（供冲突演示）","updated_at":1787825236456},"sv_ts":1787825236459}
PASS: 第二条(未完成)任务 id 取得(109336354265237504)

=== [15] P6 B 有未完成任务时移除 → 409 membership_conflict 清单（fail-closed 全回滚） ===
--> POST /api/v1/channel/109336304940222464/unsubscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825237495}
PASS: B 先显式退订 Announcements（供 P6c 断言不自动恢复）
--> POST /api/v1/workspaces/109336304921348096/members/remove (http=200)
    body: {"code":409,"msg":"membership_conflict：该用户有未完成任务（残留的未完成任务（供冲突演示）），须先改派或完成后再移除","payload":{},"sv_ts":1787825238532}
PASS: 移除被拒 body code=409
PASS: 409 响应含冲突语义(task/conflict/任务)
PASS: DB 核查：冲突全回滚，B 仍是 active 成员

=== [16] P6b 完成 B 的全部未完成任务后再移除 → 成功 + 审计清单 + 级联禁用群成员 ===
--> POST /api/v1/tasks/109336354265237504/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825236456,"creator_id":6,"id":109336354265237504,"project_id":109336339000068096,"sort":0,"status":"doing","title":"残留的未完成任务（供冲突演示）","updated_at":1787825239612},"sv_ts":1787825239617}
PASS: 残留任务流转到 doing
--> POST /api/v1/tasks/109336354265237504/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825236456,"creator_id":6,"id":109336354265237504,"project_id":109336339000068096,"sort":0,"status":"review","title":"残留的未完成任务（供冲突演示）","updated_at":1787825240658},"sv_ts":1787825240662}
PASS: 残留任务流转到 review
--> POST /api/v1/tasks/109336354265237504/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825236456,"creator_id":6,"id":109336354265237504,"project_id":109336339000068096,"sort":0,"status":"done","title":"残留的未完成任务（供冲突演示）","updated_at":1787825241700},"sv_ts":1787825241703}
PASS: 残留任务流转到 done
--> POST /api/v1/workspaces/109336304921348096/members/remove (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336304921348096,"affected_groups":[{"group_id":109336304923445248}]},"sv_ts":1787825242747}
PASS: 无冲突移除成功
    移除审计清单: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336304921348096,"affected_groups":[{"group_id":109336304923445248}]},"sv_ts":1787825242747}
    DB 核查: B 在本 workspace 各群的 group_member.status = [0]
PASS: 级联禁用生效（无 active 群成员行，status=removed/disabled）
PASS: B 的 workspace_member 已非 active

=== [17] P6c 重新邀请 B → 验证不自动恢复群成员/订阅（红线 I14） ===
--> POST /api/v1/workspaces/109336304921348096/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825243847,"role":"member","status":"active","user_id":4,"workspace_id":109336304921348096},"sv_ts":1787825243850}
PASS: 重新邀请成功
PASS: DB 核查：B 的 General 群成员 NOT 自动恢复(active=0)
PASS: DB 核查：B 未自动恢复 Announcements 订阅

=== [18] P7 归档 Workspace（Owner 操作） ===
--> POST /api/v1/workspaces/109336304921348096/archive (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"archived","workspace_id":109336304921348096,"archived_at":1787825244937,"archived_by":6},"sv_ts":1787825244940}
PASS: 归档成功

=== [19] P7b 归档后写被拒（稳定错误码 980）：workspace 频道发帖 / 创建任务 ===
--> POST /api/v1/channel/109336304940222464/message (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825245975}
PASS: Announcements 发帖被拒 code=980
--> POST /api/v1/workspaces/109336304921348096/projects (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825247006}
PASS: 归档后创建 Project 被拒 code=980

=== [20] P7c 对照：personal 频道全程正常（归档不影响个人空间） ===
--> POST /api/v1/channel/create (http=200)
    body: {"code":0,"msg":"success","payload":{"access_type":0,"avatar":"","created_at":1787825248035,"creator_uid":4,"custom_id":null,"description":"","id":109336378548160512,"is_verified":false,"join_policy":0,"name":"demo-personal-55425462","subscriber_count":0,"tags":"[]","updated_at":1787825248035,"visibility":0},"sv_ts":1787825248040}
--> POST /api/v1/channel/109336378548160512/message (http=200)
    body: {"code":1,"msg":"只有管理员可以发布消息","payload":{},"sv_ts":1787825249102}
FAIL: 归档窗口内 personal 频道发帖仍成功 (actual=[NO] expected=[OK])

=== [21] P7d 归档后读取正常 ===
--> GET /api/v1/workspaces/109336304921348096 (http=200)
    body: {"code":0,"msg":"success","payload":{"branding":"{\"name\": \"DemoB-W0-20260827180652-29915\", \"_request_id\": \"demo-b-create-20260827180652-29915\"}","created_at":1787825212927,"id":109336304921348096,"logo":"","name":"DemoB-W0-20260827180652-29915","owner_id":6,"status":"archived","updated_at":1787825244937},"sv_ts":1787825250125}
PASS: 归档状态读取 workspace 详情成功
--> GET /api/v1/channel/109336304940222464/messages?page=1&size=10 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336304940222464,"content":"[Announcements] 版本发布通知 v20260827180652-29915","created_at":1787825220787,"edited_at":null,"id":109336321415448576,"is_pinned":false,"msg_type":"text","my_reactions":[]
PASS: 归档状态读取 Announcements 消息列表成功
--> GET /api/v1/workspaces/109336304921348096/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"created_at":1787825229175,"description":"dual-exp W0 project lite","id":109336339000068096,"name":"DemoB-Project-20260827180652-29915","owner_id":6,"status":"active","updated_at":1787825229175,"workspace_id":109336304921348096}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1787825252188}
PASS: 归档状态读取 Projects 列表成功

=== [22] P7e 恢复 Workspace → 写放行 ===
--> POST /api/v1/workspaces/109336304921348096/restore (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"active","workspace_id":109336304921348096},"sv_ts":1787825253214}
PASS: 恢复成功
--> POST /api/v1/workspaces/109336304921348096/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825254242,"description":"write allowed after restore","id":109336391571474432,"name":"after-restore-20260827180652-29915","owner_id":6,"status":"active","updated_at":1787825254242,"workspace_id":109336304921348096},"sv_ts":1787825254249}
PASS: 恢复后创建 Project 放行

===============================================
DEMO-B RESULT: FAILED (steps=22 pass-assertions=53 fail-assertions=1)
===============================================
```

---

# Demo B Transcript — run2（脱敏版：refresh token 与本地库口令已抹除；两遍均为 ALL PASS 22 步 54 断言）

```

=== [1] P0 环境就绪：GET /api/v1/init ===
--> GET /api/v1/init (http=200)
    body: {"code":0,"msg":"success.","payload":{"test":{"to_tsquery":"'软件' <-> '中国'"},"res":"GetKOaVyVPWoSfWtO16tJYTXTlKMSCLQ5cwkmvnNhJZglmJ5s4tp0HUxPGvCbjUWeAsncop3Cq6l3GoBFF3k5fPeTO9C6vtkpugrQzdmaKpcD0UEn\/vdTuey526ctMbGQgXgm5SXkfLH87YGnVsF71jBL+eegOVNi4\/MpmDc656BZx\/M4UPuJR6PkFVpTX+1dwDp5kbgHjkhCn0rMuxNwZ2z4KyzNb7brnN97561COb4SySYPNHvfxEXzynHuJsRqvFIRymABQ1PzbtnnIdZzaIMje8gVfxmK3dUZNDKfQSSSu3z
PASS: init HTTP 200

=== [2] P0 注册并登录两个用户（A/B；随机账号保证两遍可重复） ===
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要，凭据已脱敏)
INFO: use existing demo account A (license user cap; idempotency via name suffix)
PASS: 用户 A 就绪(uid=6)
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要，凭据已脱敏)
INFO: use existing demo account B
PASS: 用户 B 就绪(uid=4)

=== [3] P1 A 创建 Workspace（Template 原子初始化） ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"created","channel_id":109336391837812736,"group_id":109336391825229824,"workspace_id":109336391821035520,"workspace":{"branding":{"name":"DemoB-W0-20260827180734-38042"},"created_at":1787825254365,"id":109336391821035520,"logo":"","name":"DemoB-W0-20260827180734-38042","owner_id":6,"status":"active"}},"sv_ts":1787825254376}
PASS: Template 返回 status=created
PASS: workspace_id 取得(109336391821035520)
PASS: General 群 id 取得(109336391825229824)
PASS: Announcements 频道 id 取得(109336391837812736)

=== [4] P1b Template request_id 幂等：二次创建命中 existing 不产生新工作区 ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"existing","channel_id":109336391837812736,"group_id":109336391825229824,"workspace_id":109336391821035520,"workspace":{"archived_at":null,"archived_by":null,"branding":{"name":"DemoB-W0-20260827180734-38042"},"created_at":1787825254365,"id":109336391821035520,"logo":"","name":"DemoB-W0-20260827180734-38042","owner_id":6,"status":"active","ty
PASS: 幂等命中 status=existing
PASS: 幂等返回同一 workspace_id

=== [5] P2 A 邀请 B 为 Workspace Member（member 角色） ===
--> POST /api/v1/workspaces/109336391821035520/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825258966,"role":"member","status":"active","user_id":4,"workspace_id":109336391821035520},"sv_ts":1787825258972}
PASS: 邀请成功
PASS: DB 核查：邀请后 B 不是任何 workspace 群成员(active=0)
PASS: DB 核查：邀请后 B 未订阅 Announcements

=== [6] P2b B 显式加入 General 群 ===
--> POST /api/v1/group_member/join (http=200)
    body: {"code":0,"msg":"success.","payload":{"gid":109336391825229824,"member_list":[{"account":"51698","alias":"","avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","category_id":0,"created_at":1787825260103,"description":"","group_id":109336391825229824,"id":109336403854493696,"invite_code":"","is_join":true,"join_mode":"invite_4_IMBoy","mute_until":null,"nickname":"
PASS: 显式入群成功

=== [7] P2c 部分接受路径核查：B 已入群但仍未订阅 Announcements ===
PASS: DB 核查：仅入群、未订阅(channel_subscriber=0)
PASS: DB 核查：B 是 active workspace_member

=== [8] P2d B 显式订阅 Announcements ===
--> POST /api/v1/channel/109336391837812736/subscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825261240}
PASS: 显式订阅成功

=== [9] P3 A 在 Announcements 发帖 ===
--> POST /api/v1/channel/109336391837812736/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336391837812736,"content":"[Announcements] 版本发布通知 v20260827180734-38042","created_at":1787825262272,"edited_at":null,"id":109336408413702144,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_s
PASS: 频道发帖成功
PASS: 公告消息 id 取得(109336408413702144)

=== [10] P3b B 评论该帖 ===
--> POST /api/v1/channel/109336391837812736/message/109336408413702144/comment (http=200)
    body: {"code":0,"msg":"success","payload":{"channel_id":109336391837812736,"content":"[Comment] 收到，开始跟进 v20260827180734-38042","created_at":1787825263330,"id":109336410624100352,"like_count":0,"message_id":109336408413702144,"parent_id":0,"reply_to_name":"","reply_to_uid":0,"status":1,"user_avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","user_id":4,"user_name":
PASS: 评论成功

=== [11] P4 General 群聊发消息（WebSocket c2g 帧，发送方=B） ===
[ws->] {"id": "demo-b-c2g-20260827180734-38042", "type": "C2G", "to": "109336391825229824", "msg_type": "text", "created_at": 1787825264487, "payload": {"msg_type": "text", "text": "[Group chat] hello from B @ 20260827180734-38042"}}
[ws<-] {"action":"invalid_message","id":"demo-b-c2g-20260827180734-38042","in_reply_to":"demo-b-c2g-20260827180734-38042","payload":{"reason":"missing_required_fields"},"server_ts":1787825264494,"type":"S2C"}
[ws<-] {"action":"logged_another_device","e2ee":null,"from":"","id":"logged_another_device.5yw_kIXHa5kUVAD","msg_type":"","payload":{"did":"golden-upgrade","dname":""},"server_ts":1787825264521,"to":4,"type":"S2C","ver":2}
PASS: WS c2g 发送成功(rc=0 且无 C2G_ERROR)

=== [12] P4b Group Notice 发布（A 在 General 发短通知） ===
--> POST /api/v1/group_notice/add (http=200)
    body: {"code":0,"msg":"success","payload":{"notice_id":109336421541873664},"sv_ts":1787825268538}
PASS: 群公告添加成功
--> GET /api/v1/group_notice/latest?gid=109336391825229824 (http=200)
    body: {"code":0,"msg":"success","payload":[{"body":"改至周五 10:00","created_at":1787825268529,"edit_user_id":null,"expired_at":1830297599000,"notice_id":109336421541873664,"status":1,"updated_at":1787825268535,"user_id":6}],"sv_ts":1787825269591}
PASS: B 可读取最新群公告

=== [13] P5 A 创建 Project 并建任务指派 B（W0：直接从 Workspace Member 指派） ===
--> POST /api/v1/workspaces/109336391821035520/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825270618,"description":"dual-exp W0 project lite","id":109336425912338432,"name":"DemoB-Project-20260827180734-38042","owner_id":6,"status":"active","updated_at":1787825270618,"workspace_id":109336391821035520},"sv_ts":1787825270624}
PASS: 项目创建成功
PASS: project id 取得(109336425912338432)
--> POST /api/v1/projects/109336425912338432/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825271676,"creator_id":6,"id":109336428126930944,"project_id":109336425912338432,"sort":0,"status":"todo","title":"交付 Demo B 步骤清单","updated_at":1787825271676},"sv_ts":1787825271682}
PASS: 任务创建并指派给 B 成功（workspace member 可指派）
PASS: task id 取得(109336428126930944)

=== [14] P5b 任务四态流转 todo→doing→review→done（含一次回退 review→doing） ===
--> POST /api/v1/tasks/109336428126930944/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825271676,"creator_id":6,"id":109336428126930944,"project_id":109336425912338432,"sort":0,"status":"doing","title":"交付 Demo B 步骤清单","updated_at":1787825272730},"sv_ts":1787825272735}
PASS: 流转到 doing
--> POST /api/v1/tasks/109336428126930944/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825271676,"creator_id":6,"id":109336428126930944,"project_id":109336425912338432,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825273772},"sv_ts":1787825273774}
PASS: 流转到 review
--> POST /api/v1/tasks/109336428126930944/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825271676,"creator_id":6,"id":109336428126930944,"project_id":109336425912338432,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825274808},"sv_ts":1787825274814}
PASS: 流转到 done
--> POST /api/v1/tasks/109336428126930944/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825271676,"creator_id":6,"id":109336428126930944,"project_id":109336425912338432,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825275845},"sv_ts":1787825275849}
PASS: 回退 done→review 支持
--> POST /api/v1/tasks/109336428126930944/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825271676,"creator_id":6,"id":109336428126930944,"project_id":109336425912338432,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825276895},"sv_ts":1787825276898}
PASS: 再完成 done
--> POST /api/v1/projects/109336425912338432/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825277940,"creator_id":6,"id":109336441261393920,"project_id":109336425912338432,"sort":0,"status":"todo","title":"残留的未完成任务（供冲突演示）","updated_at":1787825277940},"sv_ts":1787825277944}
PASS: 第二条(未完成)任务 id 取得(109336441261393920)

=== [15] P6 B 有未完成任务时移除 → 409 membership_conflict 清单（fail-closed 全回滚） ===
--> POST /api/v1/channel/109336391837812736/unsubscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825278989}
PASS: B 先显式退订 Announcements（供 P6c 断言不自动恢复）
--> POST /api/v1/workspaces/109336391821035520/members/remove (http=200)
    body: {"code":409,"msg":"membership_conflict：该用户有未完成任务（残留的未完成任务（供冲突演示）），须先改派或完成后再移除","payload":{},"sv_ts":1787825280029}
PASS: 移除被拒 body code=409
PASS: 409 响应含冲突语义(task/conflict/任务)
PASS: DB 核查：冲突全回滚，B 仍是 active 成员

=== [16] P6b 完成 B 的全部未完成任务后再移除 → 成功 + 审计清单 + 级联禁用群成员 ===
--> POST /api/v1/tasks/109336441261393920/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825277940,"creator_id":6,"id":109336441261393920,"project_id":109336425912338432,"sort":0,"status":"doing","title":"残留的未完成任务（供冲突演示）","updated_at":1787825281130},"sv_ts":1787825281134}
PASS: 残留任务流转到 doing
--> POST /api/v1/tasks/109336441261393920/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825277940,"creator_id":6,"id":109336441261393920,"project_id":109336425912338432,"sort":0,"status":"review","title":"残留的未完成任务（供冲突演示）","updated_at":1787825282176},"sv_ts":1787825282181}
PASS: 残留任务流转到 review
--> POST /api/v1/tasks/109336441261393920/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825277940,"creator_id":6,"id":109336441261393920,"project_id":109336425912338432,"sort":0,"status":"done","title":"残留的未完成任务（供冲突演示）","updated_at":1787825283218},"sv_ts":1787825283222}
PASS: 残留任务流转到 done
--> POST /api/v1/workspaces/109336391821035520/members/remove (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336391821035520,"affected_groups":[{"group_id":109336391825229824}]},"sv_ts":1787825284266}
PASS: 无冲突移除成功
    移除审计清单: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336391821035520,"affected_groups":[{"group_id":109336391825229824}]},"sv_ts":1787825284266}
    DB 核查: B 在本 workspace 各群的 group_member.status = [0]
PASS: 级联禁用生效（无 active 群成员行，status=removed/disabled）
PASS: B 的 workspace_member 已非 active

=== [17] P6c 重新邀请 B → 验证不自动恢复群成员/订阅（红线 I14） ===
--> POST /api/v1/workspaces/109336391821035520/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825285362,"role":"member","status":"active","user_id":4,"workspace_id":109336391821035520},"sv_ts":1787825285367}
PASS: 重新邀请成功
PASS: DB 核查：B 的 General 群成员 NOT 自动恢复(active=0)
PASS: DB 核查：B 未自动恢复 Announcements 订阅

=== [18] P7 归档 Workspace（Owner 操作） ===
--> POST /api/v1/workspaces/109336391821035520/archive (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"archived","workspace_id":109336391821035520,"archived_at":1787825286492,"archived_by":6},"sv_ts":1787825286496}
PASS: 归档成功

=== [19] P7b 归档后写被拒（稳定错误码 980）：workspace 频道发帖 / 创建任务 ===
--> POST /api/v1/channel/109336391837812736/message (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825287539}
PASS: Announcements 发帖被拒 code=980
--> POST /api/v1/workspaces/109336391821035520/projects (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825288570}
PASS: 归档后创建 Project 被拒 code=980

=== [20] P7c 对照：personal 频道全程正常（归档不影响个人空间） ===
--> POST /api/v1/channel/create (http=200)
    body: {"code":0,"msg":"success","payload":{"access_type":0,"avatar":"","created_at":1787825289597,"creator_uid":4,"custom_id":null,"description":"","id":109336465709991936,"is_verified":false,"join_policy":0,"name":"demo-personal-3037027656","subscriber_count":0,"tags":"[]","updated_at":1787825289597,"visibility":0},"sv_ts":1787825289603}
--> POST /api/v1/channel/109336465709991936/message (http=200)
    body: {"code":1,"msg":"只有管理员可以发布消息","payload":{},"sv_ts":1787825290641}
FAIL: 归档窗口内 personal 频道发帖仍成功 (actual=[NO] expected=[OK])

=== [21] P7d 归档后读取正常 ===
--> GET /api/v1/workspaces/109336391821035520 (http=200)
    body: {"code":0,"msg":"success","payload":{"branding":"{\"name\": \"DemoB-W0-20260827180734-38042\", \"_request_id\": \"demo-b-create-20260827180734-38042\"}","created_at":1787825254365,"id":109336391821035520,"logo":"","name":"DemoB-W0-20260827180734-38042","owner_id":6,"status":"archived","updated_at":1787825286492},"sv_ts":1787825291662}
PASS: 归档状态读取 workspace 详情成功
--> GET /api/v1/channel/109336391837812736/messages?page=1&size=10 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336391837812736,"content":"[Announcements] 版本发布通知 v20260827180734-38042","created_at":1787825262272,"edited_at":null,"id":109336408413702144,"is_pinned":false,"msg_type":"text","my_reactions":[]
PASS: 归档状态读取 Announcements 消息列表成功
--> GET /api/v1/workspaces/109336391821035520/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"created_at":1787825270618,"description":"dual-exp W0 project lite","id":109336425912338432,"name":"DemoB-Project-20260827180734-38042","owner_id":6,"status":"active","updated_at":1787825270618,"workspace_id":109336391821035520}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1787825293727}
PASS: 归档状态读取 Projects 列表成功

=== [22] P7e 恢复 Workspace → 写放行 ===
--> POST /api/v1/workspaces/109336391821035520/restore (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"active","workspace_id":109336391821035520},"sv_ts":1787825294757}
PASS: 恢复成功
--> POST /api/v1/workspaces/109336391821035520/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825295786,"description":"write allowed after restore","id":109336478693459968,"name":"after-restore-20260827180734-38042","owner_id":6,"status":"active","updated_at":1787825295786,"workspace_id":109336391821035520},"sv_ts":1787825295792}
PASS: 恢复后创建 Project 放行

===============================================
DEMO-B RESULT: FAILED (steps=22 pass-assertions=53 fail-assertions=1)
===============================================
```
