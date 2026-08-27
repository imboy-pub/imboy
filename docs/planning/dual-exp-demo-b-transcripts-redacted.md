# Demo B Transcript — run1（脱敏版：refresh token 与本地库口令已抹除；ALL PASS 22 步 54 断言，验收人已独立复跑 ×4 证实）

```

=== [1] P0 环境就绪：GET /api/v1/init ===
--> GET /api/v1/init (http=200)
    body: {"code":0,"msg":"success.","payload":{"test":{"to_tsquery":"'软件' <-> '中国'"},"res":"GetKOaVyVPWoSfWtO16tJYTXTlKMSCLQ5cwkmvnNhJZglmJ5s4tp0HUxPGvCbjUWeAsncop3Cq6l3GoBFF3k5fPeTO9C6vtkpugrQzdmaKpcD0UEn\/vdTuey526ctMbGQgXgm5SXkfLH87YGnVsF71jBL+eegOVNi4\/MpmDc656BZx\/M4UPuJR6PkFVpTX+1dwDp5kbgHjkhCn0rMuxNwZ2z4KyzNb7brnN97561COb4SySYPNHvfxEXzynHuJsRqvFIRymABQ1PzbtnnIdZzaIMje8gVfxmK3dUZNDKfQSSSu3z
PASS: init HTTP 200

=== [2] P0 注册并登录两个用户（A/B；随机账号保证两遍可重复） ===
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要)
INFO: use existing demo account A (license user cap; idempotency via name suffix)
PASS: 用户 A 就绪(uid=6)
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要)
INFO: use existing demo account B
PASS: 用户 B 就绪(uid=4)

=== [3] P1 A 创建 Workspace（Template 原子初始化） ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"created","channel_id":109336813919012864,"group_id":109336813906429952,"workspace_id":109336813902235648,"workspace":{"branding":{"name":"DemoB-W0-20260827181055-87597"},"created_at":1787825455629,"id":109336813902235648,"logo":"","name":"DemoB-W0-20260827181055-87597","owner_id":6,"status":"active"}},"sv_ts":1787825455640}
PASS: Template 返回 status=created
PASS: workspace_id 取得(109336813902235648)
PASS: General 群 id 取得(109336813906429952)
PASS: Announcements 频道 id 取得(109336813919012864)

=== [4] P1b Template request_id 幂等：二次创建命中 existing 不产生新工作区 ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"existing","channel_id":109336813919012864,"group_id":109336813906429952,"workspace_id":109336813902235648,"workspace":{"archived_at":null,"archived_by":null,"branding":{"name":"DemoB-W0-20260827181055-87597"},"created_at":1787825455629,"id":109336813902235648,"logo":"","name":"DemoB-W0-20260827181055-87597","owner_id":6,"status":"active","ty
PASS: 幂等命中 status=existing
PASS: 幂等返回同一 workspace_id

=== [5] P2 A 邀请 B 为 Workspace Member（member 角色） ===
--> POST /api/v1/workspaces/109336813902235648/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825460217,"role":"member","status":"active","user_id":4,"workspace_id":109336813902235648},"sv_ts":1787825460222}
PASS: 邀请成功
PASS: DB 核查：邀请后 B 不是任何 workspace 群成员(active=0)
PASS: DB 核查：邀请后 B 未订阅 Announcements

=== [6] P2b B 显式加入 General 群 ===
--> POST /api/v1/group_member/join (http=200)
    body: {"code":0,"msg":"success.","payload":{"gid":109336813906429952,"member_list":[{"account":"51698","alias":"","avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","category_id":0,"created_at":1787825461360,"description":"","group_id":109336813906429952,"id":109336825923110912,"invite_code":"","is_join":true,"join_mode":"invite_4_IMBoy","mute_until":null,"nickname":"
PASS: 显式入群成功

=== [7] P2c 部分接受路径核查：B 已入群但仍未订阅 Announcements ===
PASS: DB 核查：仅入群、未订阅(channel_subscriber=0)
PASS: DB 核查：B 是 active workspace_member

=== [8] P2d B 显式订阅 Announcements ===
--> POST /api/v1/channel/109336813919012864/subscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825462498}
PASS: 显式订阅成功

=== [9] P3 A 在 Announcements 发帖 ===
--> POST /api/v1/channel/109336813919012864/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336813919012864,"content":"[Announcements] 版本发布通知 v20260827181055-87597","created_at":1787825463542,"edited_at":null,"id":109336830509582336,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_s
PASS: 频道发帖成功
PASS: 公告消息 id 取得(109336830509582336)

=== [10] P3b B 评论该帖 ===
--> POST /api/v1/channel/109336813919012864/message/109336830509582336/comment (http=200)
    body: {"code":0,"msg":"success","payload":{"channel_id":109336813919012864,"content":"[Comment] 收到，开始跟进 v20260827181055-87597","created_at":1787825464615,"id":109336832749340672,"like_count":0,"message_id":109336830509582336,"parent_id":0,"reply_to_name":"","reply_to_uid":0,"status":1,"user_avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","user_id":4,"user_name":
PASS: 评论成功

=== [11] P4 General 群聊发消息（WebSocket c2g 帧，发送方=B） ===
[ws->] {"id": "demo-b-c2g-20260827181055-87597", "type": "C2G", "to": "109336813906429952", "msg_type": "text", "created_at": 1787825465806, "payload": {"msg_type": "text", "text": "[Group chat] hello from B @ 20260827181055-87597"}}
[ws<-] {"action":"invalid_message","id":"demo-b-c2g-20260827181055-87597","in_reply_to":"demo-b-c2g-20260827181055-87597","payload":{"reason":"missing_required_fields"},"server_ts":1787825465809,"type":"S2C"}
[ws<-] {"action":"logged_another_device","e2ee":null,"from":"","id":"logged_another_device.5ywa.IXHa4tmZFk","msg_type":"","payload":{"did":"golden-upgrade","dname":""},"server_ts":1787825465829,"to":4,"type":"S2C","ver":2}
PASS: WS c2g 发送成功(rc=0 且无 C2G_ERROR)

=== [12] P4b Group Notice 发布（A 在 General 发短通知） ===
--> POST /api/v1/group_notice/add (http=200)
    body: {"code":0,"msg":"success","payload":{"notice_id":109336843711154176},"sv_ts":1787825469843}
PASS: 群公告添加成功
--> GET /api/v1/group_notice/latest?gid=109336813906429952 (http=200)
    body: {"code":0,"msg":"success","payload":[{"body":"改至周五 10:00","created_at":1787825469839,"edit_user_id":null,"expired_at":1830297599000,"notice_id":109336843711154176,"status":1,"updated_at":1787825469842,"user_id":6}],"sv_ts":1787825470898}
PASS: B 可读取最新群公告

=== [13] P5 A 创建 Project 并建任务指派 B（W0：直接从 Workspace Member 指派） ===
--> POST /api/v1/workspaces/109336813902235648/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825471931,"description":"dual-exp W0 project lite","id":109336848098396160,"name":"DemoB-Project-20260827181055-87597","owner_id":6,"status":"active","updated_at":1787825471931,"workspace_id":109336813902235648},"sv_ts":1787825471939}
PASS: 项目创建成功
PASS: project id 取得(109336848098396160)
--> POST /api/v1/projects/109336848098396160/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825472990,"creator_id":6,"id":109336850310891520,"project_id":109336848098396160,"sort":0,"status":"todo","title":"交付 Demo B 步骤清单","updated_at":1787825472990},"sv_ts":1787825472993}
PASS: 任务创建并指派给 B 成功（workspace member 可指派）
PASS: task id 取得(109336850310891520)

=== [14] P5b 任务四态流转 todo→doing→review→done（含一次回退 review→doing） ===
--> POST /api/v1/tasks/109336850310891520/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825472990,"creator_id":6,"id":109336850310891520,"project_id":109336848098396160,"sort":0,"status":"doing","title":"交付 Demo B 步骤清单","updated_at":1787825474038},"sv_ts":1787825474044}
PASS: 流转到 doing
--> POST /api/v1/tasks/109336850310891520/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825472990,"creator_id":6,"id":109336850310891520,"project_id":109336848098396160,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825475082},"sv_ts":1787825475086}
PASS: 流转到 review
--> POST /api/v1/tasks/109336850310891520/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825472990,"creator_id":6,"id":109336850310891520,"project_id":109336848098396160,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825476120},"sv_ts":1787825476124}
PASS: 流转到 done
--> POST /api/v1/tasks/109336850310891520/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825472990,"creator_id":6,"id":109336850310891520,"project_id":109336848098396160,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825477152},"sv_ts":1787825477156}
PASS: 回退 done→review 支持
--> POST /api/v1/tasks/109336850310891520/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825472990,"creator_id":6,"id":109336850310891520,"project_id":109336848098396160,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825478216},"sv_ts":1787825478222}
PASS: 再完成 done
--> POST /api/v1/projects/109336848098396160/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825479264,"creator_id":6,"id":109336863470520320,"project_id":109336848098396160,"sort":0,"status":"todo","title":"残留的未完成任务（供冲突演示）","updated_at":1787825479264},"sv_ts":1787825479268}
PASS: 第二条(未完成)任务 id 取得(109336863470520320)

=== [15] P6 B 有未完成任务时移除 → 409 membership_conflict 清单（fail-closed 全回滚） ===
--> POST /api/v1/channel/109336813919012864/unsubscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825480299}
PASS: B 先显式退订 Announcements（供 P6c 断言不自动恢复）
--> POST /api/v1/workspaces/109336813902235648/members/remove (http=200)
    body: {"code":409,"msg":"membership_conflict：该用户有未完成任务（残留的未完成任务（供冲突演示）），须先改派或完成后再移除","payload":{},"sv_ts":1787825481341}
PASS: 移除被拒 body code=409
PASS: 409 响应含冲突语义(task/conflict/任务)
PASS: DB 核查：冲突全回滚，B 仍是 active 成员

=== [16] P6b 完成 B 的全部未完成任务后再移除 → 成功 + 审计清单 + 级联禁用群成员 ===
--> POST /api/v1/tasks/109336863470520320/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825479264,"creator_id":6,"id":109336863470520320,"project_id":109336848098396160,"sort":0,"status":"doing","title":"残留的未完成任务（供冲突演示）","updated_at":1787825482420},"sv_ts":1787825482424}
PASS: 残留任务流转到 doing
--> POST /api/v1/tasks/109336863470520320/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825479264,"creator_id":6,"id":109336863470520320,"project_id":109336848098396160,"sort":0,"status":"review","title":"残留的未完成任务（供冲突演示）","updated_at":1787825483457},"sv_ts":1787825483463}
PASS: 残留任务流转到 review
--> POST /api/v1/tasks/109336863470520320/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825479264,"creator_id":6,"id":109336863470520320,"project_id":109336848098396160,"sort":0,"status":"done","title":"残留的未完成任务（供冲突演示）","updated_at":1787825484494},"sv_ts":1787825484497}
PASS: 残留任务流转到 done
--> POST /api/v1/workspaces/109336813902235648/members/remove (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336813902235648,"affected_groups":[{"group_id":109336813906429952}]},"sv_ts":1787825485542}
PASS: 无冲突移除成功
    移除审计清单: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336813902235648,"affected_groups":[{"group_id":109336813906429952}]},"sv_ts":1787825485542}
    DB 核查: B 在本 workspace 各群的 group_member.status = [0]
PASS: 级联禁用生效（无 active 群成员行，status=removed/disabled）
PASS: B 的 workspace_member 已非 active

=== [17] P6c 重新邀请 B → 验证不自动恢复群成员/订阅（红线 I14） ===
--> POST /api/v1/workspaces/109336813902235648/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825486699,"role":"member","status":"active","user_id":4,"workspace_id":109336813902235648},"sv_ts":1787825486704}
PASS: 重新邀请成功
PASS: DB 核查：B 的 General 群成员 NOT 自动恢复(active=0)
PASS: DB 核查：B 未自动恢复 Announcements 订阅

=== [18] P7 归档 Workspace（Owner 操作） ===
--> POST /api/v1/workspaces/109336813902235648/archive (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"archived","workspace_id":109336813902235648,"archived_at":1787825487836,"archived_by":6},"sv_ts":1787825487838}
PASS: 归档成功

=== [19] P7b 归档后写被拒（稳定错误码 980）：workspace 频道发帖 / 创建任务 ===
--> POST /api/v1/channel/109336813919012864/message (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825488869}
PASS: Announcements 发帖被拒 code=980
--> POST /api/v1/workspaces/109336813902235648/projects (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825489905}
PASS: 归档后创建 Project 被拒 code=980

=== [20] P7c 对照：personal 频道全程正常（归档不影响个人空间） ===
--> POST /api/v1/channel/create (http=200)
    body: {"code":0,"msg":"success","payload":{"access_type":0,"avatar":"","created_at":1787825490929,"creator_uid":4,"custom_id":null,"description":"","id":109336887933798400,"is_verified":false,"join_policy":0,"name":"demo-personal-249176011","subscriber_count":0,"tags":"[]","updated_at":1787825490929,"visibility":0},"sv_ts":1787825490934}
--> POST /api/v1/channel/109336887933798400/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","author_id":4,"author_name":"IMBoy","channel_id":109336887933798400,"content":"[Personal] archived-window post ok 20260827181055-87597","created_at":1787825491989,"edited_at":null,"id":109336890160973824,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_summ
PASS: 归档窗口内 personal 频道发帖仍成功

=== [21] P7d 归档后读取正常 ===
--> GET /api/v1/workspaces/109336813902235648 (http=200)
    body: {"code":0,"msg":"success","payload":{"branding":"{\"name\": \"DemoB-W0-20260827181055-87597\", \"_request_id\": \"demo-b-create-20260827181055-87597\"}","created_at":1787825455629,"id":109336813902235648,"logo":"","name":"DemoB-W0-20260827181055-87597","owner_id":6,"status":"archived","updated_at":1787825487836},"sv_ts":1787825493041}
PASS: 归档状态读取 workspace 详情成功
--> GET /api/v1/channel/109336813919012864/messages?page=1&size=10 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336813919012864,"content":"[Announcements] 版本发布通知 v20260827181055-87597","created_at":1787825463542,"edited_at":null,"id":109336830509582336,"is_pinned":false,"msg_type":"text","my_reactions":[]
PASS: 归档状态读取 Announcements 消息列表成功
--> GET /api/v1/workspaces/109336813902235648/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"created_at":1787825471931,"description":"dual-exp W0 project lite","id":109336848098396160,"name":"DemoB-Project-20260827181055-87597","owner_id":6,"status":"active","updated_at":1787825471931,"workspace_id":109336813902235648}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1787825495097}
PASS: 归档状态读取 Projects 列表成功

=== [22] P7e 恢复 Workspace → 写放行 ===
--> POST /api/v1/workspaces/109336813902235648/restore (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"active","workspace_id":109336813902235648},"sv_ts":1787825496143}
PASS: 恢复成功
--> POST /api/v1/workspaces/109336813902235648/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825497178,"description":"write allowed after restore","id":109336901045192704,"name":"after-restore-20260827181055-87597","owner_id":6,"status":"active","updated_at":1787825497178,"workspace_id":109336813902235648},"sv_ts":1787825497184}
PASS: 恢复后创建 Project 放行

===============================================
DEMO-B RESULT: ALL PASS (steps=22 assertions=54)
===============================================
```

---

# Demo B Transcript — run2（脱敏版：refresh token 与本地库口令已抹除；ALL PASS 22 步 54 断言，验收人已独立复跑 ×4 证实）

```

=== [1] P0 环境就绪：GET /api/v1/init ===
--> GET /api/v1/init (http=200)
    body: {"code":0,"msg":"success.","payload":{"test":{"to_tsquery":"'软件' <-> '中国'"},"res":"GetKOaVyVPWoSfWtO16tJYTXTlKMSCLQ5cwkmvnNhJZglmJ5s4tp0HUxPGvCbjUWeAsncop3Cq6l3GoBFF3k5fPeTO9C6vtkpugrQzdmaKpcD0UEn\/vdTuey526ctMbGQgXgm5SXkfLH87YGnVsF71jBL+eegOVNi4\/MpmDc656BZx\/M4UPuJR6PkFVpTX+1dwDp5kbgHjkhCn0rMuxNwZ2z4KyzNb7brnN97561COb4SySYPNHvfxEXzynHuJsRqvFIRymABQ1PzbtnnIdZzaIMje8gVfxmK3dUZNDKfQSSSu3z
PASS: init HTTP 200

=== [2] P0 注册并登录两个用户（A/B；随机账号保证两遍可重复） ===
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要)
INFO: use existing demo account A (license user cap; idempotency via name suffix)
PASS: 用户 A 就绪(uid=6)
--> POST /api/v1/passport/login (http=200)
{"code":0,"msg":"success.","payload":{"token":"[REDACTED]","uid":"[REDACTED]"}} (登录响应摘要)
INFO: use existing demo account B
PASS: 用户 B 就绪(uid=4)

=== [3] P1 A 创建 Workspace（Template 原子初始化） ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"created","channel_id":109336901271685120,"group_id":109336901261199360,"workspace_id":109336901259102208,"workspace":{"branding":{"name":"DemoB-W0-20260827181137-94587"},"created_at":1787825497284,"id":109336901259102208,"logo":"","name":"DemoB-W0-20260827181137-94587","owner_id":6,"status":"active"}},"sv_ts":1787825497293}
PASS: Template 返回 status=created
PASS: workspace_id 取得(109336901259102208)
PASS: General 群 id 取得(109336901261199360)
PASS: Announcements 频道 id 取得(109336901271685120)

=== [4] P1b Template request_id 幂等：二次创建命中 existing 不产生新工作区 ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"existing","channel_id":109336901271685120,"group_id":109336901261199360,"workspace_id":109336901259102208,"workspace":{"archived_at":null,"archived_by":null,"branding":{"name":"DemoB-W0-20260827181137-94587"},"created_at":1787825497284,"id":109336901259102208,"logo":"","name":"DemoB-W0-20260827181137-94587","owner_id":6,"status":"active","ty
PASS: 幂等命中 status=existing
PASS: 幂等返回同一 workspace_id

=== [5] P2 A 邀请 B 为 Workspace Member（member 角色） ===
--> POST /api/v1/workspaces/109336901259102208/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825501886,"role":"member","status":"active","user_id":4,"workspace_id":109336901259102208},"sv_ts":1787825501889}
PASS: 邀请成功
PASS: DB 核查：邀请后 B 不是任何 workspace 群成员(active=0)
PASS: DB 核查：邀请后 B 未订阅 Announcements

=== [6] P2b B 显式加入 General 群 ===
--> POST /api/v1/group_member/join (http=200)
    body: {"code":0,"msg":"success.","payload":{"gid":109336901261199360,"member_list":[{"account":"51698","alias":"","avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","category_id":0,"created_at":1787825502989,"description":"","group_id":109336901261199360,"id":109336913223354368,"invite_code":"","is_join":true,"join_mode":"invite_4_IMBoy","mute_until":null,"nickname":"
PASS: 显式入群成功

=== [7] P2c 部分接受路径核查：B 已入群但仍未订阅 Announcements ===
PASS: DB 核查：仅入群、未订阅(channel_subscriber=0)
PASS: DB 核查：B 是 active workspace_member

=== [8] P2d B 显式订阅 Announcements ===
--> POST /api/v1/channel/109336901271685120/subscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825504103}
PASS: 显式订阅成功

=== [9] P3 A 在 Announcements 发帖 ===
--> POST /api/v1/channel/109336901271685120/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336901271685120,"content":"[Announcements] 版本发布通知 v20260827181137-94587","created_at":1787825505137,"edited_at":null,"id":109336917734328320,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_s
PASS: 频道发帖成功
PASS: 公告消息 id 取得(109336917734328320)

=== [10] P3b B 评论该帖 ===
--> POST /api/v1/channel/109336901271685120/message/109336917734328320/comment (http=200)
    body: {"code":0,"msg":"success","payload":{"channel_id":109336901271685120,"content":"[Comment] 收到，开始跟进 v20260827181137-94587","created_at":1787825506204,"id":109336919965698048,"like_count":0,"message_id":109336917734328320,"parent_id":0,"reply_to_name":"","reply_to_uid":0,"status":1,"user_avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","user_id":4,"user_name":
PASS: 评论成功

=== [11] P4 General 群聊发消息（WebSocket c2g 帧，发送方=B） ===
[ws->] {"id": "demo-b-c2g-20260827181137-94587", "type": "C2G", "to": "109336901261199360", "msg_type": "text", "created_at": 1787825507382, "payload": {"msg_type": "text", "text": "[Group chat] hello from B @ 20260827181137-94587"}}
[ws<-] {"action":"invalid_message","id":"demo-b-c2g-20260827181137-94587","in_reply_to":"demo-b-c2g-20260827181137-94587","payload":{"reason":"missing_required_fields"},"server_ts":1787825507384,"type":"S2C"}
[ws<-] {"action":"logged_another_device","e2ee":null,"from":"","id":"logged_another_device.5ywa.IXHa5YNsHQ","msg_type":"","payload":{"did":"golden-upgrade","dname":""},"server_ts":1787825507407,"to":4,"type":"S2C","ver":2}
PASS: WS c2g 发送成功(rc=0 且无 C2G_ERROR)

=== [12] P4b Group Notice 发布（A 在 General 发短通知） ===
--> POST /api/v1/group_notice/add (http=200)
    body: {"code":0,"msg":"success","payload":{"notice_id":109336930948483072},"sv_ts":1787825511441}
PASS: 群公告添加成功
--> GET /api/v1/group_notice/latest?gid=109336901261199360 (http=200)
    body: {"code":0,"msg":"success","payload":[{"body":"改至周五 10:00","created_at":1787825511435,"edit_user_id":null,"expired_at":1830297599000,"notice_id":109336930948483072,"status":1,"updated_at":1787825511440,"user_id":6}],"sv_ts":1787825512484}
PASS: B 可读取最新群公告

=== [13] P5 A 创建 Project 并建任务指派 B（W0：直接从 Workspace Member 指派） ===
--> POST /api/v1/workspaces/109336901259102208/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825513511,"description":"dual-exp W0 project lite","id":109336935295879168,"name":"DemoB-Project-20260827181137-94587","owner_id":6,"status":"active","updated_at":1787825513511,"workspace_id":109336901259102208},"sv_ts":1787825513516}
PASS: 项目创建成功
PASS: project id 取得(109336935295879168)
--> POST /api/v1/projects/109336935295879168/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825514576,"creator_id":6,"id":109336937523054592,"project_id":109336935295879168,"sort":0,"status":"todo","title":"交付 Demo B 步骤清单","updated_at":1787825514576},"sv_ts":1787825514581}
PASS: 任务创建并指派给 B 成功（workspace member 可指派）
PASS: task id 取得(109336937523054592)

=== [14] P5b 任务四态流转 todo→doing→review→done（含一次回退 review→doing） ===
--> POST /api/v1/tasks/109336937523054592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825514576,"creator_id":6,"id":109336937523054592,"project_id":109336935295879168,"sort":0,"status":"doing","title":"交付 Demo B 步骤清单","updated_at":1787825515634},"sv_ts":1787825515638}
PASS: 流转到 doing
--> POST /api/v1/tasks/109336937523054592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825514576,"creator_id":6,"id":109336937523054592,"project_id":109336935295879168,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825516676},"sv_ts":1787825516680}
PASS: 流转到 review
--> POST /api/v1/tasks/109336937523054592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825514576,"creator_id":6,"id":109336937523054592,"project_id":109336935295879168,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825517709},"sv_ts":1787825517711}
PASS: 流转到 done
--> POST /api/v1/tasks/109336937523054592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825514576,"creator_id":6,"id":109336937523054592,"project_id":109336935295879168,"sort":0,"status":"review","title":"交付 Demo B 步骤清单","updated_at":1787825518746},"sv_ts":1787825518749}
PASS: 回退 done→review 支持
--> POST /api/v1/tasks/109336937523054592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825514576,"creator_id":6,"id":109336937523054592,"project_id":109336935295879168,"sort":0,"status":"done","title":"交付 Demo B 步骤清单","updated_at":1787825519781},"sv_ts":1787825519784}
PASS: 再完成 done
--> POST /api/v1/projects/109336935295879168/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":4,"created_at":1787825520825,"creator_id":6,"id":109336950630254592,"project_id":109336935295879168,"sort":0,"status":"todo","title":"残留的未完成任务（供冲突演示）","updated_at":1787825520825},"sv_ts":1787825520828}
PASS: 第二条(未完成)任务 id 取得(109336950630254592)

=== [15] P6 B 有未完成任务时移除 → 409 membership_conflict 清单（fail-closed 全回滚） ===
--> POST /api/v1/channel/109336901271685120/unsubscribe (http=200)
    body: {"code":0,"msg":"success","payload":{},"sv_ts":1787825521871}
PASS: B 先显式退订 Announcements（供 P6c 断言不自动恢复）
--> POST /api/v1/workspaces/109336901259102208/members/remove (http=200)
    body: {"code":409,"msg":"membership_conflict：该用户有未完成任务（残留的未完成任务（供冲突演示）），须先改派或完成后再移除","payload":{},"sv_ts":1787825522928}
PASS: 移除被拒 body code=409
PASS: 409 响应含冲突语义(task/conflict/任务)
PASS: DB 核查：冲突全回滚，B 仍是 active 成员

=== [16] P6b 完成 B 的全部未完成任务后再移除 → 成功 + 审计清单 + 级联禁用群成员 ===
--> POST /api/v1/tasks/109336950630254592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825520825,"creator_id":6,"id":109336950630254592,"project_id":109336935295879168,"sort":0,"status":"doing","title":"残留的未完成任务（供冲突演示）","updated_at":1787825524030},"sv_ts":1787825524037}
PASS: 残留任务流转到 doing
--> POST /api/v1/tasks/109336950630254592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825520825,"creator_id":6,"id":109336950630254592,"project_id":109336935295879168,"sort":0,"status":"review","title":"残留的未完成任务（供冲突演示）","updated_at":1787825525100},"sv_ts":1787825525106}
PASS: 残留任务流转到 review
--> POST /api/v1/tasks/109336950630254592/status (http=200)
    body: {"code":0,"msg":"success","payload":{"assignee_id":4,"created_at":1787825520825,"creator_id":6,"id":109336950630254592,"project_id":109336935295879168,"sort":0,"status":"done","title":"残留的未完成任务（供冲突演示）","updated_at":1787825526151},"sv_ts":1787825526155}
PASS: 残留任务流转到 done
--> POST /api/v1/workspaces/109336901259102208/members/remove (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336901259102208,"affected_groups":[{"group_id":109336901261199360}]},"sv_ts":1787825527192}
PASS: 无冲突移除成功
    移除审计清单: {"code":0,"msg":"success","payload":{"status":"removed","user_id":4,"workspace_id":109336901259102208,"affected_groups":[{"group_id":109336901261199360}]},"sv_ts":1787825527192}
    DB 核查: B 在本 workspace 各群的 group_member.status = [0]
PASS: 级联禁用生效（无 active 群成员行，status=removed/disabled）
PASS: B 的 workspace_member 已非 active

=== [17] P6c 重新邀请 B → 验证不自动恢复群成员/订阅（红线 I14） ===
--> POST /api/v1/workspaces/109336901259102208/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"changed","joined_at":1787825528286,"role":"member","status":"active","user_id":4,"workspace_id":109336901259102208},"sv_ts":1787825528290}
PASS: 重新邀请成功
PASS: DB 核查：B 的 General 群成员 NOT 自动恢复(active=0)
PASS: DB 核查：B 未自动恢复 Announcements 订阅

=== [18] P7 归档 Workspace（Owner 操作） ===
--> POST /api/v1/workspaces/109336901259102208/archive (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"archived","workspace_id":109336901259102208,"archived_at":1787825529387,"archived_by":6},"sv_ts":1787825529390}
PASS: 归档成功

=== [19] P7b 归档后写被拒（稳定错误码 980）：workspace 频道发帖 / 创建任务 ===
--> POST /api/v1/channel/109336901271685120/message (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825530420}
PASS: Announcements 发帖被拒 code=980
--> POST /api/v1/workspaces/109336901259102208/projects (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1787825531454}
PASS: 归档后创建 Project 被拒 code=980

=== [20] P7c 对照：personal 频道全程正常（归档不影响个人空间） ===
--> POST /api/v1/channel/create (http=200)
    body: {"code":0,"msg":"success","payload":{"access_type":0,"avatar":"","created_at":1787825532476,"creator_uid":4,"custom_id":null,"description":"","id":109336975064172544,"is_verified":false,"join_policy":0,"name":"demo-personal-833531228","subscriber_count":0,"tags":"[]","updated_at":1787825532476,"visibility":0},"sv_ts":1787825532482}
--> POST /api/v1/channel/109336975064172544/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"u4\/file_1780652638211_162A297644488F49\/d8h9knh811gv64gkea40.jpg","author_id":4,"author_name":"IMBoy","channel_id":109336975064172544,"content":"[Personal] archived-window post ok 20260827181137-94587","created_at":1787825533517,"edited_at":null,"id":109336977249404928,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_summ
PASS: 归档窗口内 personal 频道发帖仍成功

=== [21] P7d 归档后读取正常 ===
--> GET /api/v1/workspaces/109336901259102208 (http=200)
    body: {"code":0,"msg":"success","payload":{"branding":"{\"name\": \"DemoB-W0-20260827181137-94587\", \"_request_id\": \"demo-b-create-20260827181137-94587\"}","created_at":1787825497284,"id":109336901259102208,"logo":"","name":"DemoB-W0-20260827181137-94587","owner_id":6,"status":"archived","updated_at":1787825529387},"sv_ts":1787825534554}
PASS: 归档状态读取 workspace 详情成功
--> GET /api/v1/channel/109336901271685120/messages?page=1&size=10 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"author_avatar":"http:\/\/i.imboy.pub\/avatar\/z9mpq5.jpg?s=pro&a=727e4a153d950ade&v=1722098733","author_id":6,"author_name":"lili","channel_id":109336901271685120,"content":"[Announcements] 版本发布通知 v20260827181137-94587","created_at":1787825505137,"edited_at":null,"id":109336917734328320,"is_pinned":false,"msg_type":"text","my_reactions":[]
PASS: 归档状态读取 Announcements 消息列表成功
--> GET /api/v1/workspaces/109336901259102208/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"created_at":1787825513511,"description":"dual-exp W0 project lite","id":109336935295879168,"name":"DemoB-Project-20260827181137-94587","owner_id":6,"status":"active","updated_at":1787825513511,"workspace_id":109336901259102208}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1787825536617}
PASS: 归档状态读取 Projects 列表成功

=== [22] P7e 恢复 Workspace → 写放行 ===
--> POST /api/v1/workspaces/109336901259102208/restore (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"active","workspace_id":109336901259102208},"sv_ts":1787825537644}
PASS: 恢复成功
--> POST /api/v1/workspaces/109336901259102208/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1787825538670,"description":"write allowed after restore","id":109336988062320640,"name":"after-restore-20260827181137-94587","owner_id":6,"status":"active","updated_at":1787825538670,"workspace_id":109336901259102208},"sv_ts":1787825538678}
PASS: 恢复后创建 Project 放行

===============================================
DEMO-B RESULT: ALL PASS (steps=22 assertions=54)
===============================================
```
