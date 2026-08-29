# Demo B W2 Transcripts（ZC-08 验收存档：两遍 ALL PASS 66/66；无敏感信息，JWT/口令不出现在响应体）

## run1（EXIT=0，断言 66/66 通过，前缀 2026082921125999）

```

=== [1] P0 环境就绪：GET /api/v1/init ===
--> GET /api/v1/init (http=200)
    body: {"code":0,"msg":"success.","payload":{"test":{"to_tsquery":"'软件' <-> '中国'"},"res":"Rk+Ii4ZzYgsC3JReq7UHgLqKOD7yixWj2mpoSfGPFJ1ZEMNrnXg2f+auZ62mV1P8E3WluVp4O\/m9wBiqGBnHxjul9oLh529nelSNN9ZsmOh1GguVvCpyccy7Yyl3eAOXqpCJ50dtoGy9dLyq3o6uIxhgZh8L2v1QTzo3BrftJV33HLU3tY\/AcDSvvtUE3CSxz4V8AhjGssmKsj4m8S1oE4zXT7WXGl8Q2xAyAR4teefwVhDloIQRDnJRUsBoCjva0BpSytnwhrnVmajPHAurNsItdyEkvYeqbZMktaa6mOatnlrQ09Y
PASS: init HTTP 200

=== [2] P1 创建唯一前缀账号 u1/u2（imboy_ctl；license 上限下 signup 402 的仓内通道） ===
PASS: 用户 A 就绪(uid=178800918139721)
PASS: 用户 B 就绪(uid=178800918139722)
PASS: 前置校验：u1/u2 账号已落库（count=2，防 mobile 超长静默失败）

=== [3] P2 A 创建 Workspace（Template 原子初始化） ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"created","channel_id":109722113897924608,"group_id":109722113881147392,"workspace_id":109722113874855936,"workspace":{"branding":{"name":"W2Demo-W2-20260829211259-23972"},"created_at":1788009180973,"id":109722113874855936,"logo":"","name":"W2Demo-W2-20260829211259-23972","owner_id":178800918139721,"status":"active"}},"sv_ts":1788009180993}
PASS: Template 返回 status=created
PASS: workspace_id 取得(109722113874855936)
PASS: General 群 id 取得(109722113881147392)
PASS: Announcements 频道 id 取得(109722113897924608)

=== [4] P3 四关系之 workspace 成员邀请：A 邀 B（member）+ DB 核查 ===
--> POST /api/v1/workspaces/109722113874855936/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"changed","joined_at":1788009182048,"role":"member","status":"active","user_id":178800918139722,"workspace_id":109722113874855936},"sv_ts":1788009182061}
PASS: workspace 成员邀请成功
PASS: DB 核查：B 是 active workspace_member

=== [5] P4 A 创建 Project：Owner 自动入项目 + B 直访 403（W2 项目隔离） ===
--> POST /api/v1/workspaces/109722113874855936/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1788009183123,"description":"demo-b W2 project","id":109722118390024192,"name":"W2Demo-Project-20260829211259-23972","owner_id":178800918139721,"status":"active","updated_at":1788009183123,"workspace_id":109722113874855936},"sv_ts":1788009183138}
PASS: 项目创建成功
PASS: project id 取得(109722118390024192)
--> GET /api/v1/projects/109722118390024192/members (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"account":"w2d2026082921125999u1@smoke.local","avatar":"","invited_by":null,"joined_at":1788009183133,"nickname":"w2demoA","project_id":109722118390024192,"status":"active","user_id":178800918139721,"workspace_id":109722113874855936}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1788009184173}
PASS: members 列表可读
PASS: Owner 自动入项目（members 含 owner uid=178800918139721）
PASS: 初始 members 仅 Owner 一人（total=1）
--> GET /api/v1/projects/109722118390024192/members (http=200)
    body: {"code":403,"msg":"仅项目成员可访问该项目资源","payload":{},"sv_ts":1788009185216}
PASS: B 直访 members 被拒 code=403（非 Project Member）

=== [6] P5 Task：A 建任务指派 B（workspace member 可指派） ===
--> POST /api/v1/projects/109722118390024192/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":178800918139722,"created_at":1788009186256,"creator_id":178800918139721,"id":109722124956207104,"project_id":109722118390024192,"sort":0,"status":"todo","title":"W2Demo 任务 20260829211259-23972","updated_at":1788009186256},"sv_ts":1788009186267}
PASS: 任务创建并指派成功
PASS: task id 取得(109722124956207104)

=== [7] P6 Milestone：create → status 参数 400 → reach → 重复 reach 幂等 → status 过滤 ===
--> POST /api/v1/projects/109722118390024192/milestones (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1788009187302,"due_date":"{2026,9,30}","id":109722127147730944,"name":"W2Demo-M1-20260829211259-23972","project_id":109722118390024192,"reached_at":null,"status":"planned","updated_at":1788009187302,"workspace_id":109722113874855936},"sv_ts":1788009187652}
PASS: 里程碑创建成功
PASS: milestone id 取得(109722127147730944)
PASS: 初始 status=planned
--> POST /api/v1/projects/109722118390024192/milestones (http=200)
    body: {"code":400,"msg":"里程碑状态请通过 \/reach 接口流转，不接受 status 字段","payload":{},"sv_ts":1788009188691}
PASS: create/update 携带 status 字段被 400 拒绝（状态机唯一入口 /reach）
--> POST /api/v1/milestones/109722127147730944/reach (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"reached","created_at":1788009187302,"due_date":"{2026,9,30}","id":109722127147730944,"name":"W2Demo-M1-20260829211259-23972","project_id":109722118390024192,"reached_at":1788009189722,"status":"reached","updated_at":1788009189722,"workspace_id":109722113874855936},"sv_ts":1788009189727}
PASS: reach → status_flag=reached
--> POST /api/v1/milestones/109722127147730944/reach (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"already_reached","created_at":1788009187302,"due_date":"{2026,9,30}","id":109722127147730944,"name":"W2Demo-M1-20260829211259-23972","project_id":109722118390024192,"reached_at":1788009189722,"status":"reached","updated_at":1788009189722,"workspace_id":109722113874855936},"sv_ts":1788009191031}
PASS: 重复 reach 幂等 → status_flag=already_reached
--> GET /api/v1/projects/109722118390024192/milestones?status=reached (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"created_at":1788009187302,"due_date":"{2026,9,30}","id":109722127147730944,"name":"W2Demo-M1-20260829211259-23972","project_id":109722118390024192,"reached_at":1788009189722,"status":"reached","updated_at":1788009189722,"workspace_id":109722113874855936}],"size":20,"page":1},"sv_ts":1788009192064}
PASS: list?status=reached 过滤含已达成里程碑

=== [8] P7 Channel 关联：link → 幂等 → 列表 → unlink → 404 → 重连 ===
--> POST /api/v1/projects/109722118390024192/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created"},"sv_ts":1788009193107}
PASS: link 频道 → status_flag=created
--> POST /api/v1/projects/109722118390024192/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"existing"},"sv_ts":1788009194345}
PASS: 重复 link 幂等 → status_flag=existing
--> GET /api/v1/projects/109722118390024192/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"avatar":"","channel_id":109722113897924608,"channel_status":1,"linked_at":1788009193093,"name":"Announcements","workspace_id":109722113874855936}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1788009195386}
PASS: 关联频道列表含 Announcements
--> POST /api/v1/channel/109722113897924608/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"","author_id":178800918139721,"author_name":"w2demoA","channel_id":109722113897924608,"content":"[W2Demo] related-posts seed 20260829211259-23972","created_at":1788009196424,"edited_at":null,"id":109722146288437248,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_summary":"{}","request_id":"w2demo-msg-20260829211259-23972","revoked"
PASS: 关联频道发帖成功（related_posts 数据源）
--> GET /api/v1/projects/109722118390024192/aggregations/related_posts (http=200)
    body: {"code":0,"msg":"success","payload":[{"author_id":178800918139721,"channel_id":109722113897924608,"created_at":1788009196424,"id":109722146288437248,"msg_type":"text"}],"sv_ts":1788009197474}
PASS: related_posts 非空（len>0）
PASS: related_posts 为有界摘要（无正文字段）
--> POST /api/v1/projects/109722118390024192/channels/109722113897924608/unlink (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"unlinked"},"sv_ts":1788009198511}
PASS: unlink → status_flag=unlinked
--> POST /api/v1/projects/109722118390024192/channels/109722113897924608/unlink (http=200)
    body: {"code":404,"msg":"关联不存在","payload":{},"sv_ts":1788009199551}
PASS: unlink 缺失关联 → 404（定向删除语义）
--> POST /api/v1/projects/109722118390024192/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created"},"sv_ts":1788009200590}
PASS: 重新 link（为归档/聚合场景保持关联态）→ created

=== [9] P8 四聚合：pinned/resources 空态 + activity 非空 + B 入项目后成员可读 ===
--> GET /api/v1/projects/109722118390024192/aggregations/pinned?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[],"size":20,"total":0,"page":1,"total_page":0},"sv_ts":1788009201618}
PASS: pinned 可用且空态（total=0）
--> GET /api/v1/projects/109722118390024192/aggregations/resources (http=200)
    body: {"code":0,"msg":"success","payload":[],"sv_ts":1788009202895}
PASS: resources 空态（links 未配置返回 []）
--> POST /api/v1/projects/109722118390024192/links/update (http=200)
    body: {"code":0,"msg":"success","payload":{"links":[{"name":"w2demo-docs","url":"https:\/\/example.com\/w2demo\/20260829211259-23972"}]},"sv_ts":1788009204030}
PASS: update_links 全量替换成功
--> GET /api/v1/projects/109722118390024192/aggregations/resources (http=200)
    body: {"code":0,"msg":"success","payload":[{"name":"w2demo-docs","url":"https:\/\/example.com\/w2demo\/20260829211259-23972"}],"sv_ts":1788009205067}
PASS: resources 非空（links 返回，len=1）
--> GET /api/v1/projects/109722118390024192/aggregations/activity?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"actor_id":178800918139721,"created_at":1788009203929,"event_type":"links_updated","id":109722162019174400,"payload":{"count":1},"target_id":109722118390024192},{"actor_id":178800918139721,"created_at":1788009200588,"event_type":"channel_linked","id":109722155012589568,"payload":{"channel_id":109722113897924608,"channel_name":"Announcements"},"target_i
PASS: activity 可用且非空（total=6）
PASS: activity 仅元数据（payload 无正文字段）
--> POST /api/v1/projects/109722118390024192/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","invited_by":178800918139721,"joined_at":1788009207148,"project_id":109722118390024192,"status":"active","user_id":178800918139722,"workspace_id":109722113874855936},"sv_ts":1788009207152}
PASS: A 邀 B 入项目 → status_flag=created
--> GET /api/v1/projects/109722118390024192/aggregations/pinned?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[],"size":20,"total":0,"page":1,"total_page":0},"sv_ts":1788009208179}
PASS: B（active Project Member）可读 pinned 聚合

=== [10] P9 归档 Workspace：W2 写端点全被 980 拒，读可通 ===
--> POST /api/v1/workspaces/109722113874855936/archive (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"archived","workspace_id":109722113874855936,"archived_at":1788009209210,"archived_by":178800918139721},"sv_ts":1788009209217}
PASS: 归档成功
--> POST /api/v1/projects/109722118390024192/milestones (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009210244}
PASS: 归档后 milestone create 被拒 code=980
--> POST /api/v1/milestones/109722127147730944/reach (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009211291}
PASS: 归档后 milestone reach 被拒 code=980
--> POST /api/v1/projects/109722118390024192/channels (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009212345}
PASS: 归档后 channel link 被拒 code=980
--> POST /api/v1/projects/109722118390024192/links/update (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009213379}
PASS: 归档后 links/update 被拒 code=980
--> POST /api/v1/projects/109722118390024192/members/invite (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009214415}
PASS: 归档后 project member invite 被拒 code=980
--> GET /api/v1/projects/109722118390024192/aggregations/pinned?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[],"size":20,"total":0,"page":1,"total_page":0},"sv_ts":1788009215452}
PASS: 归档后 pinned 聚合读可通
--> GET /api/v1/projects/109722118390024192/members (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"account":"w2d2026082921125999u1@smoke.local","avatar":"","invited_by":null,"joined_at":1788009183133,"nickname":"w2demoA","project_id":109722118390024192,"status":"active","user_id":178800918139721,"workspace_id":109722113874855936},{"account":"w2d2026082921125999u2@smoke.local","avatar":"","invited_by":178800918139721,"joined_at":1788009207148,"nickn
PASS: 归档后 members 列表读可通
--> GET /api/v1/projects/109722118390024192/aggregations/activity?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"actor_id":178800918139721,"created_at":1788009207151,"event_type":"member_invited","id":109722168774100992,"payload":{"actor":178800918139721,"invited_by":178800918139721},"target_id":178800918139722},{"actor_id":178800918139721,"created_at":1788009203929,"event_type":"links_updated","id":109722162019174400,"payload":{"count":1},"target_id":1097221183
PASS: 归档后 activity 聚合读可通

=== [11] P10 恢复 Workspace → W2 写放行 ===
--> POST /api/v1/workspaces/109722113874855936/restore (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"active","workspace_id":109722113874855936},"sv_ts":1788009219562}
PASS: 恢复成功
--> POST /api/v1/projects/109722118390024192/links/update (http=200)
    body: {"code":0,"msg":"success","payload":{"links":[{"name":"w2demo-after-restore","url":"https:\/\/example.com\/restore\/20260829211259-23972"}]},"sv_ts":1788009220604}
PASS: 恢复后 links/update 放行

=== [12] P11 移除成员后再邀：不自动恢复历史状态（W2 重邀语义） ===
--> POST /api/v1/projects/109722118390024192/members/remove (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"removed","user_id":178800918139722,"project_id":109722118390024192,"status_flag":"removed"},"sv_ts":1788009222023}
PASS: 移除 B → status_flag=removed
--> GET /api/v1/projects/109722118390024192/members (http=200)
    body: {"code":403,"msg":"仅项目成员可访问该项目资源","payload":{},"sv_ts":1788009223059}
PASS: 移除后 B 直访 members → 403
PASS: DB 核查：B 的 project_member 行 status=removed
--> POST /api/v1/projects/109722118390024192/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","invited_by":178800918139721,"joined_at":1788009224299,"project_id":109722118390024192,"status":"active","user_id":178800918139722,"workspace_id":109722113874855936},"sv_ts":1788009224302}
PASS: 重邀 → status_flag=created（覆盖激活，非静默 existing）
PASS: DB 核查：invited_by 重置为本次邀请人 A（历史不保留）
PASS: DB 核查：重邀不改变项目 Owner（仍为 A）
--> GET /api/v1/projects/109722118390024192/members (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"account":"w2d2026082921125999u1@smoke.local","avatar":"","invited_by":null,"joined_at":1788009183133,"nickname":"w2demoA","project_id":109722118390024192,"status":"active","user_id":178800918139721,"workspace_id":109722113874855936},{"account":"w2d2026082921125999u2@smoke.local","avatar":"","invited_by":178800918139721,"joined_at":1788009224299,"nickn
PASS: 重邀后 B 直访 members 恢复 200（恢复的是成员身份本身）

=== [13] P12 teardown：按唯一前缀清理全部痕迹 + 残留=0 自验证 ===
    teardown scope: WSIDS=[109722113874855936] UIDS=[178800918139721,178800918139722] ACC_PFX=[w2d2026082921125999]
PASS: teardown DELETE 单事务执行成功（ON_ERROR_STOP）
PASS: teardown 自验证：workspace 残留=0
PASS: teardown 自验证：project 残留=0
PASS: teardown 自验证：user 残留=0
PASS: teardown 自验证：孤儿 workspace 频道=0（共享库无残留）
PASS: teardown 自验证：孤儿 project_member 行=0

===============================================
DEMO-B-W2 RESULT: ALL PASS (steps=13 断言 66/66 通过)
===============================================
```

## run2（EXIT=0，断言 66/66 通过，前缀 2026082921141333）

```

=== [1] P0 环境就绪：GET /api/v1/init ===
--> GET /api/v1/init (http=200)
    body: {"code":0,"msg":"success.","payload":{"test":{"to_tsquery":"'软件' <-> '中国'"},"res":"Rk+Ii4ZzYgsC3JReq7UHgLqKOD7yixWj2mpoSfGPFJ1ZEMNrnXg2f+auZ62mV1P8E3WluVp4O\/m9wBiqGBnHxjul9oLh529nelSNN9ZsmOh1GguVvCpyccy7Yyl3eAOXqpCJ50dtoGy9dLyq3o6uIxhgZh8L2v1QTzo3BrftJV33HLU3tY\/AcDSvvtUE3CSxz4V8AhjGssmKsj4m8S1oE4zXT7WXGl8Q2xAyAR4teefwVhDloIQRDnJRUsBoCjva0BpSytnwhrnVmajPHAurNsItdyEkvYeqbZMktaa6mOatnlrQ09Y
PASS: init HTTP 200

=== [2] P1 创建唯一前缀账号 u1/u2（imboy_ctl；license 上限下 signup 402 的仓内通道） ===
PASS: 用户 A 就绪(uid=178800925548931)
PASS: 用户 B 就绪(uid=178800925548932)
PASS: 前置校验：u1/u2 账号已落库（count=2，防 mobile 超长静默失败）

=== [3] P2 A 创建 Workspace（Template 原子初始化） ===
--> POST /api/v1/workspaces (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"created","channel_id":109722268774696960,"group_id":109722268760016896,"workspace_id":109722268755822592,"workspace":{"branding":{"name":"W2Demo-W2-20260829211413-24893"},"created_at":1788009254826,"id":109722268755822592,"logo":"","name":"W2Demo-W2-20260829211413-24893","owner_id":178800925548931,"status":"active"}},"sv_ts":1788009254838}
PASS: Template 返回 status=created
PASS: workspace_id 取得(109722268755822592)
PASS: General 群 id 取得(109722268760016896)
PASS: Announcements 频道 id 取得(109722268774696960)

=== [4] P3 四关系之 workspace 成员邀请：A 邀 B（member）+ DB 核查 ===
--> POST /api/v1/workspaces/109722268755822592/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"changed","joined_at":1788009255885,"role":"member","status":"active","user_id":178800925548932,"workspace_id":109722268755822592},"sv_ts":1788009255891}
PASS: workspace 成员邀请成功
PASS: DB 核查：B 是 active workspace_member

=== [5] P4 A 创建 Project：Owner 自动入项目 + B 直访 403（W2 项目隔离） ===
--> POST /api/v1/workspaces/109722268755822592/projects (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1788009256973,"description":"demo-b W2 project","id":109722273266796544,"name":"W2Demo-Project-20260829211413-24893","owner_id":178800925548931,"status":"active","updated_at":1788009256973,"workspace_id":109722268755822592},"sv_ts":1788009256990}
PASS: 项目创建成功
PASS: project id 取得(109722273266796544)
--> GET /api/v1/projects/109722273266796544/members (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"account":"w2d2026082921141333u1@smoke.local","avatar":"","invited_by":null,"joined_at":1788009256979,"nickname":"w2demoA","project_id":109722273266796544,"status":"active","user_id":178800925548931,"workspace_id":109722268755822592}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1788009258029}
PASS: members 列表可读
PASS: Owner 自动入项目（members 含 owner uid=178800925548931）
PASS: 初始 members 仅 Owner 一人（total=1）
--> GET /api/v1/projects/109722273266796544/members (http=200)
    body: {"code":403,"msg":"仅项目成员可访问该项目资源","payload":{},"sv_ts":1788009259062}
PASS: B 直访 members 被拒 code=403（非 Project Member）

=== [6] P5 Task：A 建任务指派 B（workspace member 可指派） ===
--> POST /api/v1/projects/109722273266796544/tasks (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","assignee_id":178800925548932,"created_at":1788009260533,"creator_id":178800925548931,"id":109722280724269056,"project_id":109722273266796544,"sort":0,"status":"todo","title":"W2Demo 任务 20260829211413-24893","updated_at":1788009260533},"sv_ts":1788009260536}
PASS: 任务创建并指派成功
PASS: task id 取得(109722280724269056)

=== [7] P6 Milestone：create → status 参数 400 → reach → 重复 reach 幂等 → status 过滤 ===
--> POST /api/v1/projects/109722273266796544/milestones (http=200)
    body: {"code":0,"msg":"success","payload":{"created_at":1788009261569,"due_date":"{2026,9,30}","id":109722282896918528,"name":"W2Demo-M1-20260829211413-24893","project_id":109722273266796544,"reached_at":null,"status":"planned","updated_at":1788009261569,"workspace_id":109722268755822592},"sv_ts":1788009261573}
PASS: 里程碑创建成功
PASS: milestone id 取得(109722282896918528)
PASS: 初始 status=planned
--> POST /api/v1/projects/109722273266796544/milestones (http=200)
    body: {"code":400,"msg":"里程碑状态请通过 \/reach 接口流转，不接受 status 字段","payload":{},"sv_ts":1788009262608}
PASS: create/update 携带 status 字段被 400 拒绝（状态机唯一入口 /reach）
--> POST /api/v1/milestones/109722282896918528/reach (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"reached","created_at":1788009261569,"due_date":"{2026,9,30}","id":109722282896918528,"name":"W2Demo-M1-20260829211413-24893","project_id":109722273266796544,"reached_at":1788009263642,"status":"reached","updated_at":1788009263642,"workspace_id":109722268755822592},"sv_ts":1788009263648}
PASS: reach → status_flag=reached
--> POST /api/v1/milestones/109722282896918528/reach (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"already_reached","created_at":1788009261569,"due_date":"{2026,9,30}","id":109722282896918528,"name":"W2Demo-M1-20260829211413-24893","project_id":109722273266796544,"reached_at":1788009263642,"status":"reached","updated_at":1788009263642,"workspace_id":109722268755822592},"sv_ts":1788009264682}
PASS: 重复 reach 幂等 → status_flag=already_reached
--> GET /api/v1/projects/109722273266796544/milestones?status=reached (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"created_at":1788009261569,"due_date":"{2026,9,30}","id":109722282896918528,"name":"W2Demo-M1-20260829211413-24893","project_id":109722273266796544,"reached_at":1788009263642,"status":"reached","updated_at":1788009263642,"workspace_id":109722268755822592}],"size":20,"page":1},"sv_ts":1788009265713}
PASS: list?status=reached 过滤含已达成里程碑

=== [8] P7 Channel 关联：link → 幂等 → 列表 → unlink → 404 → 重连 ===
--> POST /api/v1/projects/109722273266796544/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created"},"sv_ts":1788009266756}
PASS: link 频道 → status_flag=created
--> POST /api/v1/projects/109722273266796544/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"existing"},"sv_ts":1788009267811}
PASS: 重复 link 幂等 → status_flag=existing
--> GET /api/v1/projects/109722273266796544/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"avatar":"","channel_id":109722268774696960,"channel_status":1,"linked_at":1788009266740,"name":"Announcements","workspace_id":109722268755822592}],"size":20,"total":1,"page":1,"total_page":1},"sv_ts":1788009268844}
PASS: 关联频道列表含 Announcements
--> POST /api/v1/channel/109722268774696960/message (http=200)
    body: {"code":0,"msg":"success","payload":{"author_avatar":"","author_id":178800925548931,"author_name":"w2demoA","channel_id":109722268774696960,"content":"[W2Demo] related-posts seed 20260829211413-24893","created_at":1788009269871,"edited_at":null,"id":109722300315863040,"is_pinned":false,"msg_type":"text","payload":"{}","reaction_summary":"{}","request_id":"w2demo-msg-20260829211413-24893","revoked"
PASS: 关联频道发帖成功（related_posts 数据源）
--> GET /api/v1/projects/109722273266796544/aggregations/related_posts (http=200)
    body: {"code":0,"msg":"success","payload":[{"author_id":178800925548931,"channel_id":109722268774696960,"created_at":1788009269871,"id":109722300315863040,"msg_type":"text"}],"sv_ts":1788009271204}
PASS: related_posts 非空（len>0）
PASS: related_posts 为有界摘要（无正文字段）
--> POST /api/v1/projects/109722273266796544/channels/109722268774696960/unlink (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"unlinked"},"sv_ts":1788009272247}
PASS: unlink → status_flag=unlinked
--> POST /api/v1/projects/109722273266796544/channels/109722268774696960/unlink (http=200)
    body: {"code":404,"msg":"关联不存在","payload":{},"sv_ts":1788009273288}
PASS: unlink 缺失关联 → 404（定向删除语义）
--> POST /api/v1/projects/109722273266796544/channels (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created"},"sv_ts":1788009274332}
PASS: 重新 link（为归档/聚合场景保持关联态）→ created

=== [9] P8 四聚合：pinned/resources 空态 + activity 非空 + B 入项目后成员可读 ===
--> GET /api/v1/projects/109722273266796544/aggregations/pinned?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[],"size":20,"total":0,"page":1,"total_page":0},"sv_ts":1788009275360}
PASS: pinned 可用且空态（total=0）
--> GET /api/v1/projects/109722273266796544/aggregations/resources (http=200)
    body: {"code":0,"msg":"success","payload":[],"sv_ts":1788009276386}
PASS: resources 空态（links 未配置返回 []）
--> POST /api/v1/projects/109722273266796544/links/update (http=200)
    body: {"code":0,"msg":"success","payload":{"links":[{"name":"w2demo-docs","url":"https:\/\/example.com\/w2demo\/20260829211413-24893"}]},"sv_ts":1788009277425}
PASS: update_links 全量替换成功
--> GET /api/v1/projects/109722273266796544/aggregations/resources (http=200)
    body: {"code":0,"msg":"success","payload":[{"name":"w2demo-docs","url":"https:\/\/example.com\/w2demo\/20260829211413-24893"}],"sv_ts":1788009278449}
PASS: resources 非空（links 返回，len=1）
--> GET /api/v1/projects/109722273266796544/aggregations/activity?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"actor_id":178800925548931,"created_at":1788009277424,"event_type":"links_updated","id":109722316147263488,"payload":{"count":1},"target_id":109722273266796544},{"actor_id":178800925548931,"created_at":1788009274330,"event_type":"channel_linked","id":109722309660772352,"payload":{"channel_id":109722268774696960,"channel_name":"Announcements"},"target_i
PASS: activity 可用且非空（total=6）
PASS: activity 仅元数据（payload 无正文字段）
--> POST /api/v1/projects/109722273266796544/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","invited_by":178800925548931,"joined_at":1788009280509,"project_id":109722273266796544,"status":"active","user_id":178800925548932,"workspace_id":109722268755822592},"sv_ts":1788009280514}
PASS: A 邀 B 入项目 → status_flag=created
--> GET /api/v1/projects/109722273266796544/aggregations/pinned?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[],"size":20,"total":0,"page":1,"total_page":0},"sv_ts":1788009281546}
PASS: B（active Project Member）可读 pinned 聚合

=== [10] P9 归档 Workspace：W2 写端点全被 980 拒，读可通 ===
--> POST /api/v1/workspaces/109722268755822592/archive (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"archived","workspace_id":109722268755822592,"archived_at":1788009282590,"archived_by":178800925548931},"sv_ts":1788009282593}
PASS: 归档成功
--> POST /api/v1/projects/109722273266796544/milestones (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009283631}
PASS: 归档后 milestone create 被拒 code=980
--> POST /api/v1/milestones/109722282896918528/reach (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009284682}
PASS: 归档后 milestone reach 被拒 code=980
--> POST /api/v1/projects/109722273266796544/channels (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009286049}
PASS: 归档后 channel link 被拒 code=980
--> POST /api/v1/projects/109722273266796544/links/update (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009287409}
PASS: 归档后 links/update 被拒 code=980
--> POST /api/v1/projects/109722273266796544/members/invite (http=200)
    body: {"code":980,"msg":"工作区已归档，写操作被拒绝","payload":{},"sv_ts":1788009288623}
PASS: 归档后 project member invite 被拒 code=980
--> GET /api/v1/projects/109722273266796544/aggregations/pinned?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[],"size":20,"total":0,"page":1,"total_page":0},"sv_ts":1788009289681}
PASS: 归档后 pinned 聚合读可通
--> GET /api/v1/projects/109722273266796544/members (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"account":"w2d2026082921141333u1@smoke.local","avatar":"","invited_by":null,"joined_at":1788009256979,"nickname":"w2demoA","project_id":109722273266796544,"status":"active","user_id":178800925548931,"workspace_id":109722268755822592},{"account":"w2d2026082921141333u2@smoke.local","avatar":"","invited_by":178800925548931,"joined_at":1788009280509,"nickn
PASS: 归档后 members 列表读可通
--> GET /api/v1/projects/109722273266796544/aggregations/activity?page=1&size=20 (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"actor_id":178800925548931,"created_at":1788009280512,"event_type":"member_invited","id":109722322623268864,"payload":{"actor":178800925548931,"invited_by":178800925548931},"target_id":178800925548932},{"actor_id":178800925548931,"created_at":1788009277424,"event_type":"links_updated","id":109722316147263488,"payload":{"count":1},"target_id":1097222732
PASS: 归档后 activity 聚合读可通

=== [11] P10 恢复 Workspace → W2 写放行 ===
--> POST /api/v1/workspaces/109722268755822592/restore (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"active","workspace_id":109722268755822592},"sv_ts":1788009292783}
PASS: 恢复成功
--> POST /api/v1/projects/109722273266796544/links/update (http=200)
    body: {"code":0,"msg":"success","payload":{"links":[{"name":"w2demo-after-restore","url":"https:\/\/example.com\/restore\/20260829211413-24893"}]},"sv_ts":1788009293825}
PASS: 恢复后 links/update 放行

=== [12] P11 移除成员后再邀：不自动恢复历史状态（W2 重邀语义） ===
--> POST /api/v1/projects/109722273266796544/members/remove (http=200)
    body: {"code":0,"msg":"success","payload":{"status":"removed","user_id":178800925548932,"project_id":109722273266796544,"status_flag":"removed"},"sv_ts":1788009294867}
PASS: 移除 B → status_flag=removed
--> GET /api/v1/projects/109722273266796544/members (http=200)
    body: {"code":403,"msg":"仅项目成员可访问该项目资源","payload":{},"sv_ts":1788009295896}
PASS: 移除后 B 直访 members → 403
PASS: DB 核查：B 的 project_member 行 status=removed
--> POST /api/v1/projects/109722273266796544/members/invite (http=200)
    body: {"code":0,"msg":"success","payload":{"status_flag":"created","invited_by":178800925548931,"joined_at":1788009296977,"project_id":109722273266796544,"status":"active","user_id":178800925548932,"workspace_id":109722268755822592},"sv_ts":1788009296980}
PASS: 重邀 → status_flag=created（覆盖激活，非静默 existing）
PASS: DB 核查：invited_by 重置为本次邀请人 A（历史不保留）
PASS: DB 核查：重邀不改变项目 Owner（仍为 A）
--> GET /api/v1/projects/109722273266796544/members (http=200)
    body: {"code":0,"msg":"success","payload":{"list":[{"account":"w2d2026082921141333u1@smoke.local","avatar":"","invited_by":null,"joined_at":1788009256979,"nickname":"w2demoA","project_id":109722273266796544,"status":"active","user_id":178800925548931,"workspace_id":109722268755822592},{"account":"w2d2026082921141333u2@smoke.local","avatar":"","invited_by":178800925548931,"joined_at":1788009296977,"nickn
PASS: 重邀后 B 直访 members 恢复 200（恢复的是成员身份本身）

=== [13] P12 teardown：按唯一前缀清理全部痕迹 + 残留=0 自验证 ===
    teardown scope: WSIDS=[109722268755822592] UIDS=[178800925548931,178800925548932] ACC_PFX=[w2d2026082921141333]
PASS: teardown DELETE 单事务执行成功（ON_ERROR_STOP）
PASS: teardown 自验证：workspace 残留=0
PASS: teardown 自验证：project 残留=0
PASS: teardown 自验证：user 残留=0
PASS: teardown 自验证：孤儿 workspace 频道=0（共享库无残留）
PASS: teardown 自验证：孤儿 project_member 行=0

===============================================
DEMO-B-W2 RESULT: ALL PASS (steps=13 断言 66/66 通过)
===============================================
```
