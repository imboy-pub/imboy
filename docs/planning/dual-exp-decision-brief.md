# 双体验架构 v2.5.2 — WP0 Decision Brief（R1–R6 六节）

> **unsafe_experiment 声明**：本文档由无人值守自动执行产出。Gate 0（真实客户价值验证）被用户授权跳过、Gate W 由编排者按零证据默认档自动代行（见"Gate W 决策"节），均**待人工复核**。本文档及其结论不得作为 Release、工程 DoD 或客户验收的依据；生产规模类结论一律 BLOCKED。
>
> - 计划：`.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md`（v2.5.2）
> - Preflight 基线：`imboy/docs/planning/dual-exp-preflight-baseline.md`
> - 执行分支：imboy `dual-exp-v21`（基线 `e29f6fdf`）；imboyapp 于隔离 worktree `.worktrees/imboyapp`（`5c06cc7b`）；imboyadmin `dual-exp-v21`（未触碰）
> - 执行时间：2026-08-26 19:27–(进行中) +0800 | 方式：只读代码 + 真实命令 + 本文档，零业务代码改动
> - 大段 EXPLAIN 原始输出见同目录附件 `dual-exp-explain-appendix.md`

---

## R1 绿灯基线（真实命令数字）

### R1.1 后端 imboy：`make app` → `make eunit-local`（串行）

| 项 | 结果 |
|---|---|
| `make app` | **exit 0，0 warning**（`grep -ci warning` = 0） |
| `make eunit-local` | **Passed: 5804 / Failed: 59 / Skipped: 0**（make Error 2；另报 "One or more tests were cancelled"） |

**59 个失败 = 基线既有红灯**（干净基线 d4153c30 上即失败，非本计划引入）。样例：`test/stress/high_concurrency_stress_tests.erl` context setup failed——`depcache` ets 表 `badarg`（`ets:lookup('m:imboy_cache',...)`），属 stress 测试环境的 cache 应用未启动类问题，非缺 DB。T13 对账口径：失败总数 **不得高于 59**、通过数 **不得低于 5804**。

### R1.2 Flutter imboyapp（worktree 5c06cc7b）：`flutter pub get` → `flutter test`

| 项 | 结果 |
|---|---|
| `flutter pub get` | **exit 0** |
| 裸检出直接 `flutter test` | **编译失败（非测试失败）**：52+ 个测试文件报 `Compilation failed`，315 处 `env_*.g.dart` / `env_pro.dart` 缺失引用。原因：仓库约定 `env_*.g.dart` 生成物与 `env_dev/env_pro.dart` 模板不入库（`.gitignore` 第 70–75 行），干净检出必须先跑 CI 配方 `.github/scripts/ci_gen_env.sh`（恢复模板 + 写 dummy `.env.*` + build_runner，生成 136 个输出）。**这是 setup 缺口不是测试红灯**，与计划 R4 GOTCHA（envied 需 build_runner）一致 |
| 按 CI 配方（`ci_gen_env.sh`）后全量 `flutter test` | **+5545 通过 / ~237 跳过 / -44 失败**（10m32s，"Some tests failed"） |

**44 个失败 = 基线既有红灯**（干净检出 5c06cc7b 上即失败，非本计划引入；完整清单可在 T13 时以 `flutter test` 复现对照）。样例（前 4）：`a11y/icon_only_button_label_test.dart`（纯图标按钮读屏标签）、`page/group/album/group_album_photo_navigation_test.dart` ×3（批量删除/刷新）。T13 对账口径：失败总数 **不得高于 44**、通过数 **不得低于 5545**。

### R1.3 CI 真相（仓内定义 ≠ 在跑；origin=gitee，GitHub 镜像有真实运行记录）

三仓 `.github/workflows/` 均存在定义（imboy 10 个 / imboyapp 7 个 / imboyadmin 5 个）。GitHub 镜像（imboy-pub/*）**有可核实的远端运行记录**，且调度管线在 main 分支上**多为红灯**（截至 2026-08-26，`gh run list` 证据）：

**imboy-pub/imboy**（最新一次/工作流）：

| 工作流 | 最近运行 | 触发/分支 | 结论 |
|---|---|---|---|
| Backend Nightly | 2026-08-25T20:20Z | schedule/main | **failure**（16m41s；08-24 亦 failure） |
| Backend CI | 2026-08-24 | PR(dependabot) | failure |
| Release | 2026-08-25T19:28Z | schedule/main | skipped |
| SBOM Diff Report | 2026-08-24 | push/main | success |
| API Contract Gate | 2026-08-24 | PR | success |
| imboy Quality Gate / SonarCloud / codemap | 2026-08-24 | PR | failure / failure / failure |
| Dependabot auto-merge / Updates | 2026-08-24 | dynamic/PR | failure |

**imboy-pub/imboy-flutter**：

| 工作流 | 最近运行 | 触发/分支 | 结论 |
|---|---|---|---|
| ImBoy CI/CD | 2026-08-25T18:19Z | schedule/main | **failure**（27m13s；08-24 亦 failure） |
| Integration Tests | 2026-08-24 | PR | failure |
| imboyapp Contract Gate / Quality Gate | 2026-08-24 | PR | success / success |
| imboyapp SonarCloud | 2026-08-24 | PR | failure |
| Dependabot auto-merge / Updates | 2026-08-24 | PR/dynamic | failure |

**imboy-pub/imboy-admin-frontend**：最近记录全部为 2026-08-24 dependabot PR（Admin Frontend CI / Playwright E2E / Quality Gate / SonarCloud 均 failure，8–17 秒即失败，疑似 workflow 级问题）；无 main 调度运行记录。

结论：**CI 在跑但不可作为绿灯依据**——三仓镜像的近期运行大面积 failure；本计划以 R1.1/R1.2 本地实测数字为唯一基线。`dual-exp-v21` 分支无任何远端运行记录（本地分支未推送）。

---

## R2 Channel 生产数据画像 + 迁移性能验证

### R2.1 生产规模：BLOCKED

生产/类生产库访问被无人值守红线禁止（无人工授权，禁止 SSH/直连）。**生产行数量级、峰值写入（pg_stat）、锁行为：一律 BLOCKED**，T3 发布窗口的最终判定必须由人工在授权后按本节框架补齐数据。

### R2.2 本地画像（imboy_v1 @ 127.0.0.1:4323，Docker Debian aarch64 PostgreSQL 18.1，库 68 MB；本地规模≠生产规模）

| 表 | 行数 | 说明 |
|---|---:|---|
| `channel` | **26** | 现有索引：`channel_pkey(id)`、`i_channel_creator_uid`、`i_channel_status_created(status,created_at DESC)`、`i_channel_subscriber_count`、`i_channel_tags(gin)`、部分索引 `i_channel_visibility_active/idx_channel_category_id/idx_channel_featured`、`uk_channel_custom_id` |
| `"group"`（**确认为保留字表名，SQL 中必须双引号**） | **515** | 现有索引：`group_pkey`、`i_creatorid_memberidsum`、`i_status_owneruid_type(status,owner_uid,type)`、部分索引 `idx_group_category_id/idx_group_featured`；有两个触发器（`fts_group_update`、`set_updated_at`） |
| `group_member` | 2,020 | 唯一约束 `uk_gid_uid(group_id,user_id)`；active 查询走 `status`/`is_join` |
| `channel_subscription` | 20 | 唯一约束 `(channel_id,user_id)`，FK 两向 CASCADE |
| `channel_admin` | 21 | — |
| `channel_message` | 23 | 本地 pg_stat 全 0 写入（无近期活动） |
| `group_notice` | 37 | — |
| `attachment` | 17 | — |
| `msg_c2c` / `msg_c2g` | 1,997 / 363 | — |

本地 pg_stat 无峰值写入证据（channel 仅 86 次 update；多为测试残留），**不能外推生产**。

### R2.3 T3 迁移 SQL 草稿（channel/group 同款）

```sql
ALTER TABLE channel ADD COLUMN scope text NOT NULL DEFAULT 'personal';
ALTER TABLE channel ADD COLUMN workspace_id bigint;
ALTER TABLE channel ADD CONSTRAINT chk_channel_scope_xor CHECK (
  (scope='personal' AND workspace_id IS NULL) OR
  (scope='workspace' AND workspace_id IS NOT NULL));  -- 双向 XOR，I1
-- 存量行经 DEFAULT 自动成为 scope='personal', workspace_id=NULL → I2 回填即"零回填"
CREATE INDEX i_channel_scope_personal ON channel (creator_uid, created_at DESC)
  WHERE scope='personal' AND status=1;
CREATE INDEX i_channel_scope_ws ON channel (workspace_id, created_at DESC)
  WHERE scope='workspace' AND status=1;
-- "group" 同款（列/约束/索引名前缀 group_）；group_notice 不加 scope（经 group 归属解析）
```

草稿与附录 C 一致性核对：group 实际表名为保留字 `"group"`（已实测确认）；索引列均来自实际存在的列（creator_uid/owner_uid/status/created_at），未出现计划 GOTCHA 警告的"凭空 `channel_id`"。

### R2.4 EXPLAIN (ANALYZE, BUFFERS) 验证（TEMP 表沙箱，零持久化写入）

对个人列表 / Workspace 列表 / 按 ID 直访三类查询，在 ~10 万行合成数据的 TEMP 沙箱中全部命中目标索引（原始输出见附件 `dual-exp-explain-appendix.md`）：

| 查询 | channel_sim | group_sim |
|---|---|---|
| 个人列表 | Index Scan `i_channel_sim_personal`，0.31 ms | Index Scan `i_group_sim_personal`，0.18 ms |
| Workspace 列表 | Index Scan `i_channel_sim_ws`，2.31 ms（50 行） | Index Scan `i_group_sim_ws`，0.22 ms |
| 按 ID 直访 | Index Scan pkey，0.08 ms | Index Scan pkey，0.03 ms |
| XOR 双向违规写入 | CHECK 拒绝（workspace+NULL） | CHECK 拒绝（personal+workspace_id） |

### R2.5 锁/时长/在线迁移判断框架（生产数据缺失，最终判定 BLOCKED 待人工）

1. **加列**：PG 18 的 `ADD COLUMN ... NOT NULL DEFAULT 常量` 为元数据变更，不重写堆，`ACCESS EXCLUSIVE` 锁持有时间毫秒级——风险低。
2. **CHECK 约束**：新表数据全为默认值时校验代价趋近于零；仍建议 `NOT VALID` → 单独 `VALIDATE`（仅 SHARE UPDATE EXCLUSIVE）两阶段以锁定最坏情况。
3. **索引**：⚠️ **R6 发现的关键约束**——`erlang_migrate` 的 PG driver 对每份迁移恒包 `BEGIN...COMMIT`（`erlang_migrate_pg.erl:129-132` `exec_sql/2`），而 `CREATE INDEX CONCURRENTLY` 不能运行在事务块内 → **经现有迁移器无法做 CONCURRENTLY**。T3 只有两条路：(a) 普通.CREATE INDEX（`SHARE` 锁阻塞写入，26/515 行本地毫秒级，生产需行数×IO 估算）＝**需发布窗口**；(b) 为迁移器新增"非事务语句"能力/独立运维脚本执行 CONCURRENTLY（工程改造）。**建议默认 (a)+发布窗口**，行数证据到位后由人工终判。
4. **回填 I2**：默认值方案＝零回填（存量行自动 personal/NULL），无长事务风险。
5. **结论**：结构变更本身可在线（加列/CHECK 两阶段）；**索引是唯一可能要求发布窗口的步骤**；在拿到生产行数与峰值写入（pg_stat_user_tables / pg_stat_statements）前，T3 整体窗口判定为 **BLOCKED（需人工补生产数据）**。

---

## R3 Workspace 写守卫路径清单（归档写守卫必须覆盖的写路径穷举）

> 范围：对将成为 `scope=workspace` 的资源（Group/Channel 及其从属）产生**写入**的全部路径。枚举原则：不只查 channel_logic——逐 repo/ds 扫 SQL 写语句（动态表名拼接 `<<"INSERT INTO ", Tb/binary>>` 亦覆盖），再回溯 handler/WS 入口。
> 标记：✅=可与业务写同事务（已有 with_tx 或 Conn 传递形态）；⚠️=单语句自动提交（守卫需包事务或改写单语句带 EXISTS 条件）；BLOCKED=无法同事务且需结构性改造。

| # | 写路径 | 入口 | 写落点（文件:行） | target→workspace 解析 | 事务边界 |
|---|---|---|---|---|---|
| 1 | 群消息发送（c2g） | WS `c2g` → `message_router_logic.erl:67-69` → `msg_c2g_logic:c2g` | `repo/msg_c2g_repo.erl:92` `write_msg/9`（with_tx：消息表+timeline 批插同事务） | 写入自带 `Gid` → `"group".id → workspace_id` | ✅ with_tx，守卫可入同事务 |
| 2 | 群消息撤回/编辑/销毁 | WS `c2g action` → `message_router_logic.erl:109-129` | `repo/msg_c2g_repo.erl:225`（pinned UPDATE）、`:243`（payload UPDATE）、`:200`（DELETE） | 同上（msg → to_id=Gid） | ⚠️ 自动提交单语句 |
| 3 | 单聊消息（c2c） | WS `c2c` → `msg_c2c_repo.erl:99/116` write_msg 系 | `repo/msg_c2c_repo.erl:133/189/237`（INSERT） | C2C 属个人资源，**不进 workspace 守卫**（personal 直通） | —（personal 恒放行） |
| 4 | Bot 发消息（外部 API） | `POST /api/v1/bot/send_message`（`imboy_router.erl:477,1007`）→ `api/bot_handler.erl:169` | `logic/bot_logic.erl:168-173` → `msg_c2c_logic:c2c` | bot→c2c 为 personal；若未来 bot 入群需走 #1 | ⚠️（API 层 fail-open 风险点：该路由鉴权在 `bot_handler:authenticate/1`，非 JWT 中间件——守卫不可依赖 handler 前置校验） |
| 5 | AI agent 群回复 | 群内 @agent → `logic/ai_agent_group_reply.erl:260-268` | 复用 `msg_c2g_logic:c2g` | 同 #1 | ✅（复用 #1 事务） |
| 6 | Channel 发帖/带 request_id 幂等发帖 | `POST /api/v1/channel/:id/message`（`imboy_router.erl:351`）→ `channel_logic_message.erl:280/285 publish_message` | `ds/channel_ds.erl:252/257` → `repo/channel_message_repo.erl:54`（带 Conn 版）/`:75`（自动提交版） | 路由自带 ChannelId → `channel.workspace_id` | ⚠️ **发帖主链路无事务包裹**（add 后另有独立的 `increment_all_unread`，`channel_ds.erl:267`）；repo 已有 Conn 形态可改造 |
| 7 | Channel webhook 外部触发发帖 | `POST /api/v1/webhook/channel/:token`（`imboy_router.erl:464`，免 JWT） | `logic/channel_webhook_logic.erl:103,116-119` → 复用 `publish_message` | token→bot→ChannelId | ⚠️ 同 #6；**无用户上下文的机器入口**，守卫必须解析 channel.scope 而非依赖 uid |
| 8 | Channel 消息撤回/删除/置顶/编辑 | `imboy_router.erl:384-395` → `channel_logic_message.erl:477/528/591/643` | `repo/channel_message_repo.erl:172`（revoke）、`ds/channel_message_ds.erl:27,30` | ChannelId（消息→channel_id 回溯） | ⚠️ 自动提交 |
| 9 | Channel 评论/点赞 | `imboy_router.erl:400-414` → `channel_comment_logic` | `repo/channel_comment_repo.erl:93`（软删）、`:112/:120`（like ±） | comment→channel_id | ⚠️ 自动提交 |
| 10 | Channel 表态（reaction） | `imboy_router.erl:378-382` → `channel_logic.erl:214-218` → `channel_logic_stats.erl:118/150` | `ds/channel_ds.erl:377-383` → `channel_repo:insert_reaction/delete_reaction` | ChannelId | ⚠️ 自动提交 |
| 11 | Channel 订阅/退订 | `imboy_router.erl:344-347` → `channel_logic_subscription` | `repo/channel_subscription_repo.erl:49` `upsert_active(Conn,...)`（**已有 Conn 形态**）；`:160` increment_unread | ChannelId | ✅/⚠️ 混合（upsert 可入事务，计数器自动提交） |
| 12 | Channel 已读/清未读（派生写） | `POST /api/v1/channel/:id/read`（`imboy_router.erl:355`）→ `api/channel_handler.erl:316` | `repo/channel_subscription_repo.erl:175-192` clear_unread | ChannelId | ⚠️ 派生写——按 T7 约定：archived 时**跳过计数但不 403 读取** |
| 13 | Channel Admin 增删/改角色 | `imboy_router.erl:361-366,422` → `channel_handler_admin` | `repo/channel_admin_repo.erl:34`（add）、`:46`（add Conn 版）、`:84`（delete） | ChannelId | ⚠️ 自动提交（有 Conn 版 add） |
| 14 | 群成员加入/被邀 | `POST /api/v1/group_member/join`（`imboy_router.erl:262`）→ `logic/group_member_logic.erl:109` | `ds/group_member_ds.erl:117` `join_group(Conn,...)`（调用方事务）；`:57` add 包 with_tx | Gid → `"group".workspace_id` | ✅ Conn 形态；**此处即 `Group Member ⊆ Workspace Member` 子集触发器（T3-④）的落点** |
| 15 | 群成员退出/移除 | `imboy_router.erl:263` → `group_member_logic.erl:140-154` | `ds/group_member_ds.erl:161` `leave(Conn,...)`（`:174` execute(Conn)） | Gid | ✅ Conn 形态 |
| 16 | 群成员禁言/角色/别名 | `imboy_router.erl:265-269` | `ds/group_member_ds.erl:276`（mute，with_tx）、`:316`（role，with_tx）、`:359`（alias，`elib_pg:update` 自动提交） | Gid | ✅/⚠️ 混合 |
| 17 | Group Notice 增/改/删/置顶/取消置顶 | `imboy_router.erl:285-295` → `group_notice_logic` | `repo/group_notice_repo.erl:39`（insert）、`:73`（update）、`:191`（increment_read_count）；软删/置顶同文件 | notice→group_id（表内冗余 group 维度） | ⚠️ **全 repo 0 个 with_tx**——Group Notice 全链路自动提交 |
| 18 | Group Notice 已读计数（派生写） | `mark_read`（`imboy_router.erl:295`） | `repo/group_notice_repo.erl:191` | 同上 | ⚠️ 派生写，策略同 #12 |
| 19 | 消息表情（c2c/c2g reaction） | `msg_reaction_logic` | `repo/msg_reaction_repo.erl:59`（add）、`:75`（remove） | msg→msg_type+msg 表回溯（c2g 才需） | ⚠️ 自动提交；**c2g reaction 需要跨表回溯目标**——目标解析成本最高的路径 |
| 20 | 单聊已读回执（msg_read） | `messaging_logic.erl:79` 一带 | `repo/msg_read_repo.erl:46`（save_read）、`:89`（delete） | C2C personal，不进守卫 | —（personal） |
| 21 | @mention 落库/已读 | 发消息同链路派生 | `repo/mention_repo.erl:178,191,206`（UPDATE）、`:248`（DELETE） | mention→msg→c2g 回溯 | ⚠️ 派生写，与 #1 同事务才可守卫（当前独立） |
| 22 | 附件预签名/确认/授权 URL | `imboy_router.erl:573-577` → `attach_logic` | `logic/attach_logic.erl:189`（confirm 销账 with_tx）；`repo/attachment_repo.erl:108`（upsert）、`:145`（带 Conn 版 save）、`:297`（scope_ref UPDATE） | attach.scope_ref（表内已有 scope 概念！）→消息→群/频道 | ✅/⚠️ 混合；**attachment 已有 scope_ref 列，是天然守卫挂点** |
| 23 | Channel webhook CRUD | `imboy_router.erl:454-464` → `channel_webhook_handler` | `repo/channel_webhook_repo.erl`（add/disable；0 with_tx） | webhook→channel_id | ⚠️ 自动提交 |
| 24 | Bot 注册/启停/更新 | `imboy_router.erl:469-475` → `bot_logic` | `repo/bot_repo.erl:45`（INSERT）、`:144`（UPDATE） | bot 本身无 workspace 归属（owner_uid） | ⚠️ personal 资源直通；仅 #4/#7 的**使用**进入守卫 |
| 25 | 群文件/相册/投票/日程/群任务写 | `group_file/group_album/group_vote/group_schedule/group_task` handlers（`imboy_router.erl` group_* 段） | 各 `repo/group_*_repo.erl`（抽查均自动提交） | 均 group_id 直挂 | ⚠️ 自动提交（T7 按需接入；W0 Scope Contract 未含群协作增强，归档时统一拦截） |

**BLOCKED/需改造清单（同事务守卫的硬缺口）**：

1. **#6/#7 Channel 发帖主链路无事务**：`channel_ds.erl:252-270` 的 add + increment_unread 是两次独立自动提交。改造：包 `elib_pg:with_tx`（repo 已有 `channel_message_repo.erl:54` 带 Conn 版本，改造成本低）；webhook 入口（#7）无用户上下文，守卫只能按 `channel.scope+workspace.status` 行级条件实现。
2. **#17 Group Notice 全链路 0 事务**：归档期间"停写"需要逐函数接 Conn 或改为带 `EXISTS(workspace active)` 的条件 UPDATE。
3. **#19 c2g 消息表情跨表回溯**：reaction 表只有 msg_id+msg_type，目标群需要 JOIN msg_c2g——建议守卫在写入前经 msg→group→workspace 解析并缓存（同一事务内 SELECT FOR UPDATE group 行）。
4. **#4 bot send_message 免 JWT 路由**：守卫不可假设存在 current_uid；API 层 fail-open 是已知事故模式，T7 对该入口必须做独立回归。

计数：穷举 **25 条写路径**；✅ 可同事务 **7** 条（#1/5/11 部分/14/15/16 部分/22 部分），⚠️ 自动提交需改造 **16** 条，结构性 BLOCKED **2** 类（#6/#7 发帖事务化、#17 Notice 事务化）；personal 恒放行 2 条（#3/#20）。

---

## R4 Product Experience 配置链（现状核实 + T1 落线设计）

### R4.1 后端安装级链路（已核实的既有事实）

```
deploy/.env（IMBOY_* 条目，.env.example:56-66 已有 IMBOY_PRODUCT_PROFILE=community）
  → deploy/docker-compose.prod.yml:92-112（environment 段注入容器）
  → deploy/helm/templates/configmap.yaml:18（IMBOY_PRODUCT_PROFILE: {{...| default "community"}}）
  → imboy_env.erl:override_from_env/0（src/lib/imboy_env.erl:46 文档、:189-198 实现：
    os:getenv("IMBOY_PRODUCT_PROFILE") → application:set_env(imboy, product_profile, community|enterprise)，
    非法值直接 erlang:error fail-closed）
  → 启动时一次性读取，运行期只读
```

**登录前可安全下发的既有端点（免 JWT 白名单，`imboy_router.erl:973-975`）**：
- `GET /api/v1/init`（`imboy_router.erl:47` → `api/index_handler.erl:36` `api_init/1`）——已下发 WS URL/上传配置/solidified key；**这是 `{effective_product_experience, config_version}` 的最佳挂点**。
- `GET /api/v1/app/features`（`imboy_router.erl:49` → `app_feature_handler`）——特性开关下发，可作旁证。

### R4.2 命名隔离（关键风险）

现有 `product_profile=community|enterprise` 是**销售/版本档位**（`imboy_env.erl:46,189-198` + `deploy/helm/values.prod.yaml:31`），新增 `product_experience=chat|workspace` 是**体验开关**。两者并存、互不覆盖：新模块命名 `product_experience.erl`、环境变量 `IMBOY_PRODUCT_EXPERIENCE`、application env key `{imboy, product_experience}`。

> ⚠️ **计划内部矛盾记录**：计划 §4.1 术语约束明确"禁止再新增同名 `product_profile.erl`"，但 §七 T1 OWN 列又写 `imboy/src/lib/product_profile.erl`（新）。以 §4.1 为准（与附录 C"避免冲突"一致），T1 应新建 `src/lib/product_experience.erl`。此矛盾已上报记录，不静默二选一。

### R4.3 Flutter 现状（worktree 5c06cc7b 实测）

- 入口：`lib/main.dart:13` `APP_ENV` dart-define（默认 `pro`）→ `lib/main.dart:27` `AppInitializer.initialize(env:, signKeyVsn:)`（`lib/config/init.dart:203`）。
- 服务端下发已先例化：`lib/config/const.dart:82` `initConfig='/api/v1/init'`；`lib/config/env.dart:141` 注释明确"优先用服务端 `/api/v1/init` 下发值（写入 StorageService），未下发时回落本地"——**§4.1 的"服务端优先+本地缓存+离线降级"模式在 env 配置上已被验证过**，product_experience 完全镜像此模式即可。
- 缓存：`lib/service/storage.dart`（StorageService，AppInitializer 内初始化）。
- 离线/未知值降级：按 §4.1 缺失/未知 → 按 `chat` 渲染。

### R4.4 T1 落线设计（WP1 输入）

1. **唯一注入点**：`imboy_env.erl:override_from_env/0`（启动时读 `IMBOY_PRODUCT_EXPERIENCE` → `{imboy, product_experience}`；合法值 `chat|workspace`，缺失/非法 fail-safe 为 `chat`——注意与 product_profile 的 fail-closed 语义**不同**，体验开关错误值应降级而非拒启）。
2. **下发**：`/api/v1/init` 响应增加 `effective_product_experience` 与 `config_version` 两字段（白名单字段，不暴露其他 application env）。
3. **config_version 稳定摘要算法**：
   `config_version = hex(sha256("experience=" ++ Effective ++ ";app=" ++ AppVsn))[0..15]`
   ——输入只有"有效 experience"与"应用发布版本"（`imboy_app` vsn / `application:get_key(vsn)`）；相同部署恒定，任一变化必变化；16 hex 足够客户端做缓存键比对。
4. **受控重启切换步骤**（写入部署文档）：
   ① 改 `deploy/.env`（或 Helm values）`IMBOY_PRODUCT_EXPERIENCE=workspace` → ② `docker compose up -d`（或 `helm upgrade`，滚动重启后端）→ ③ 校验 `/api/v1/init` 返回新值与新 `config_version` → ④ 客户端下次启动比对 config_version 失效缓存并切壳。Admin（T11）仅只读展示该值/来源/version，不提供运行时写入。
5. **dart-define**：仅开发覆盖（`--dart-define=PRODUCT_EXPERIENCE=workspace` 且 UI 显式标识"开发覆盖"），不得覆盖服务端已下发值。

---

## R5 回归场景矩阵（12 行；❌ 项 = T13 工作范围）

> 判定口径：资产存在 ≠ 覆盖——已抽查打开测试文件确认断言对象（抽查记录见各行"证据"）。双端任一侧有实质自动化即 ✅，并注明另一侧缺口。

| # | 场景 | 定级 | 证据（已开文件核实断言） |
|---|---|---|---|
| 1 | 登录 | ✅ | 后端 `test/api/passport_handler_tests.erl`（signup/`?assertEqual(200, StatusCode)`）、`qr_login_handler_tests.erl`；Flutter `test/unit_test/api/auth_api_test.dart`（"1.1 正确凭证登录 — 返回 token 和 uid"，断言 accessToken 非空）、`integration_test/auth/register_flow_test.dart` |
| 2 | 最近聊天（会话列表） | ✅ | 后端 `test/api/conversation_handler_tests.erl`（断言 req_ok/current_uid）；Flutter `integration_test/chat/conversation_test.dart`、`test/unit_test/api/conversation_api_test.dart` |
| 3 | 单聊（C2C 收发） | ✅ | 后端 `test/api/msg_handler_tests.erl` + `test/ds/msg_c2c_ds_tests.erl`；Flutter `integration_test/e2e_chat_test.dart`（"打开已有单聊并发送文本消息"，带 `requireBusinessWriteAuthorization` 门）、`integration_test/chat/single_chat_readonly_test.dart`、`integration_test/two_client/mac_peer_c2c_ping_test.dart` |
| 4 | 群聊（C2G） | ✅ | 后端 `test/api/group_handler_tests.erl`、`group_member_handler_tests.erl`、`group_member_join_auth_tests.erl`、`test/ds/msg_c2g_ds_tests.erl`；Flutter `integration_test/chat/group_chat_test.dart`（多 expect 断言）、`group/*_readonly_test.dart`、`test/unit_test/api/group_member_api_test.dart` |
| 5 | Channel（订阅/发帖/评论） | ✅ | 后端 `test/api/channel_handler_tests.erl` + `test/logic/channel_logic_message_tests.erl`、`channel_logic_subscription_tests.erl`、`channel_publish_idempotency_tests.erl`、`channel_fanout_tests.erl`、`test/api/channel_discovery_handler_tests.erl`；Flutter `integration_test/channel/channel_e2e_test.dart`、`channel_publish_test.dart`、`channel_edit_persistence_test.dart`、`test/unit_test/api/channel_api_test.dart` |
| 6 | Message（模型/仓库/迁移通用） | ✅ | 后端 `test/ds/msg_store_ds_tests.erl`、`msg_operation_ds_tests.erl`、`msg_archive_ds_tests.erl`、`test/logic/msg_*_tests.erl`；Flutter `test/unit_test/message_model_msg_type_test.dart`、`message_repo_v2_migration_test.dart` |
| 7 | File/附件 | ✅（后端 handler 层有缺口） | 后端 `test/logic/attach_logic_tests.erl`、`test/ds/attachment_ds_tests.erl`、`test/ds/attach_pending_cleanup_tests.erl`——**无 `attach_handler` API 级测试**（presign/confirm HTTP 层）；Flutter `test/unit_test/store/attachment_api_test.dart`、`attachment_upload_presign_test.dart` 补齐了 API 侧 |
| 8 | E2EE | ✅ | 最厚：后端 e2ee 套件 10+ 文件（`e2ee_handler_tests`、`olm_*`、`e2ee_otk_*`、`e2ee_backup_*`、c2g/passthrough contract 等，Makefile:236 有 E2EE 专项 verify 目标）；Flutter `integration_test/e2ee_*.dart`（c2c/group outbound frame、跨平台互操作、megolm/olm device）+ `test/unit_test/service/e2ee/` |
| 9 | Push 推送 | ⚠️ 只有手动+单元 | 后端 `test/logic/push_notification_logic_tests.erl`（15 测试：register/unregister token）、`test/ds/push_notification_ds_tests.erl`、`test/repo/push_token_repo_tests.erl`；Flutter `test/unit_test/service/notification_service_test.dart`（本地通知去重/payload 规则）。**厂商通道（JPush/APNs）真机端到端无自动化**，只能真机手动 |
| 10 | 音视频通话 | ⚠️ 只有手动+logic/API 级 | 后端 `test/logic/rtc_room_logic_tests.erl`（断言 room_name/ws_url grant）、`test/logic/webrtc_ws_logic_tests.erl`；Flutter `test/unit_test/api/rtc_room_api_test.dart`（API 层）、`integration_test/chat/voice_render_verify_test.dart`（语音消息渲染，非通话）。**通话建立/双向媒体无自动化**；`test/demo_flow/call_flow.md`、`live_room_flow.md` 为手动脚本 |
| 11 | 搜索（FTS） | ✅ | 后端 `test/api/fts_handler_tests.erl`（user_search 分页/空结果三用例）、`test/ds/fts_user_ds_tests.erl`；Flutter `test/unit_test/store/repository/message_fts_repo_test.dart`、`test/unit_test/store/api/fts_api_test.dart` |
| 12 | Profile（用户资料） | ✅ | 后端 `test/api/user_handler_tests.erl`（search_by_email 断言 total/page/list 结构）；Flutter `test/unit_test/store/service/user_profile_service_test.dart`、`integration_test/mine/mine_subpages_smoke_test.dart` |

矩阵结论：**❌ 项为 0**——12 行中 10 行 ✅、2 行 ⚠️（#9 Push、#10 音视频，均为"单元/logic 级有、端到端真机链路只能手动"，属设备/厂商依赖，与计划 T13/T14"真机 Demo 取证"约定一致）。R1 基线一旦有红灯，优先修复顺序即按本矩阵 ✅ 行的数字缺口排。

---

## R6 迁移器回滚语义（T3 批次设计约束）

读码对象：`imboy/src/lib/imboy_migrate.erl`（**实际路径，非计划所写 `src/imboy_migrate.erl`**）+ `erlang_migrate/src/erlang_migrate.erl`（597 行）+ `erlang_migrate_pg.erl`。

| 语义 | 代码证据 | T3 影响 |
|---|---|---|
| down 文件缺失时 down 行为 | `erlang_migrate.erl:373-374`：`DownFile =:= undefined → {error, {no_down_migration, Version}}` ——**硬失败，不跳过**；且 `apply_down` 按版本从高到低逐份回滚，遇到缺口即中止 | 74/75 无 down ⇒ 当前库**无法**经迁移器整体 down 到 73 以下 |
| 74/75 无 down 是否惯例 | `priv/migrations/` 实测：**74 个 up / 72 个 down**，仅 `00000074_e2ee_backup_kdf_lower_bound`、`00000075_legacy_attachment_encryption` 两个最新迁移缺 down（8 月 25–26 日新增）；72/73 及之前全部成对 | **成对 down 是本仓既定惯例**，74/75 是近期例外（数据修正型前滚迁移），不构成新惯例 |
| strict 乱序检测 | `imboy_migrate.erl:87`（`strict => true`，需 erlang_migrate ≥0.3.0）；`erlang_migrate.erl:489-544`：仅 **up** 在 `enforce` 模式校验（版本 ≤ current 且不在 `schema_migrations_history` 即 `{error,{out_of_order,Versions}}`）；down/goto 仅 `init` 回填 | T3 新迁移编号必须取当前最大 75 之后（00000076+），且合并顺序不得出现低编号后至 |
| 单份迁移的原子性 | `erlang_migrate_pg.erl:129-132` `exec_sql/2` 恒包 `BEGIN...COMMIT`，失败 ROLLBACK——**每份迁移文件 = 单事务原子** | 一份文件内多语句要么全成要么全败；**副作用：`CREATE INDEX CONCURRENTLY` 在此机制下必然报"cannot run inside a transaction block"**（R2.5 已引用） |
| 失败中断与恢复 | `erlang_migrate.erl:334-364` `run_one_up`：先 `set_version(Version, dirty=true)`（独立已提交事务）→ exec_sql 失败 → 返回错误并**停在整个 up 序列**；dirty 位残留 → 下次 `check_dirty` 拒绝运行（`imboy_migrate.erl:93-95`）→ 只能 `force/2` 人工恢复（`erlang_migrate.erl:155-174`，会重建 history） | T3 任一份失败：imboy_app 启动即崩（`{migration_dirty,...}`），升级演练必须包含 force 恢复路径 |
| 本地库现状 | `schema_migrations`：version=75, dirty=f；history 74 行 | 本地库可作为 T3 演练起点 |

**结论（T3 每份迁移是否必须成对 down）：必须成对。** 依据：① 本仓 74 个迁移中 72 个成对，成对是惯例；② T3 VALIDATE 要求"按 R6 允许时 down→up 演练"，缺 down 直接使演练不可行；③ 迁移器对缺失 down 是硬失败，未来任何整体回滚都会被 T3 文件卡断。**唯一例外**：若某物理文件只含 `CREATE INDEX CONCURRENTLY`（需脱离迁移器执行），其配对 down 也应同时作为运维脚本交付（R2.5 方案 b）；选择方案 a（普通 CREATE INDEX + 发布窗口）则全部文件照常成对。

---

## Gate W 决策（无人值守自动代行，待人工复核）

> 依据编排者指令原样记录。Gate W 本为人工门（计划 §6.4 铁律 7），本节为**自动代行决策**，不具备人工签认效力；人工复核时可整体推翻，推翻后本文档 Scope Contract 部分作废重签。

**档位：W0（Project Lite）**

依据：Gate 0 被授权跳过 = 无真实客户证据；计划 §6.3 W0 为默认档（"只证明需要任务跟踪，未证明项目级可见性/邀请"）；"其余五项按证据最多再选 2 项"在零证据下选 0 项。

**Scope Contract（十二项）**：

| # | 能力 | 决策 | schema 落点 |
|---|---|---|---|
| 1 | Project：Tasks 四态 | **now**（必选） | `project_task`；`project_event` 表**随 Tasks 建**（T6b 状态流转事件原子性要求） |
| 2 | Project：Milestones | defer | `project_milestone` **不建** |
| 3 | Project：Pinned 聚合 | defer | 零新存储（无 schema，自然满足） |
| 4 | Project：Resources 聚合 | defer | `project.links` 列**不建** |
| 5 | Project：Activity 聚合端点/视图 | defer | （`project_event` 表因 #1 建立，但聚合端点/视图不开发） |
| 6 | Project：关联 Channel 集合 | defer | `project_channel_rel` **不建** |
| 7 | Workspace：成员三角色 | **now** | `workspace_member`（owner|member|guest） |
| 8 | Workspace：权限 | **now** | `workspace_member` + scope XOR |
| 9 | Workspace：Branding | **now** | `workspace.branding` jsonb（name/logo/primaryColor） |
| 10 | Workspace：资源清单 | **now** | Workspace/Project/scope 查询 |
| 11 | Workspace：生命周期（含归档） | **now** | `workspace.status/archived_at/archived_by`（§9.2 P0 明确列出） |
| 12 | Workspace：计费锚点（只读 owner_id） | **now** | `workspace.owner_id`（只读，不计费） |

Workspace 六项全 now 的依据：Workspace Experience 是双体验之一，六项为其成立最小集（§9.2 P0）；§9.2 明确列出 Archive/Branding。

**W0 硬约束**：所有 `project_member` 相关检查项自动 defer，**禁止建表**；WP2/T3 迁移⑤只保留 Workspace Membership 校验、跳过⑥；WP8 验收含 "schema 断言 `project_member` 表不存在"。

**Gate W 四问（供人工复核快速作答）**：
1. 迁移是否需窗口？→ **结构可在线，索引需窗口（CONCURRENTLY 不可用，R6）；生产规模 BLOCKED 待人工补数据**（R2.5）。
2. 现有回归红灯是否阻断？→ **不阻断**：后端基线 5804 通过/59 失败、Flutter 基线 5545 通过/44 失败，均为基线既有红灯（干净检出即失败），T13 对账口径为"失败数不增、通过数不减"；不因基线红灯阻断 Stage 1。
3. 哪些写路径必须同事务？→ R3 清单 25 条中 7 条现成可同事务；#6/#7 Channel 发帖与 #17 Group Notice 需事务化改造后才能承载归档守卫。
4. Scope Contract 选 W0/W1/W2？→ 自动代行 **W0**（上表），待人工签认。

---

## 附录：执行环境与异常记录

1. 本地 PG 双实例：默认 5432（homebrew postgresql@18，含 `imboy_mig_test`）与项目实际使用的 **4323（Docker Debian aarch64 PG 18.1，`imboy_v1`）**——所有画像/EXPLAIN 均在 4323。
2. 计划两处与代码矛盾（以代码为准并记录）：① T1 OWN 列 `product_profile.erl` vs §4.1 禁令（R4.2）；② `imboy_migrate.erl` 实际路径 `src/lib/imboy_migrate.erl`（R6）。
3. Flutter 干净检出必须先跑 `.github/scripts/ci_gen_env.sh`（CI 配方），否则 52+ 文件编译失败——建议 README/部署文档补"干净检出 setup"一节（T15 范围）。
4. `erlang_migrate` 每份迁移恒包事务 ⇒ `CREATE INDEX CONCURRENTLY` 不可用——T3 设计的硬输入（R2.5/R6）。
5. CI 镜像大面积红灯（R1.3）：本计划不依赖 CI 绿灯作为基线，WP8 收口时需单独判断是否先修 CI。
