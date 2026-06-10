# TSID Audit And Remediation Implementation Plan

> **For Codex:** REQUIRED SUB-SKILL: Use `executing-plans` style task-by-task execution for later batches.

**Goal:** 统一后端 PostgreSQL、Erlang API、Admin Frontend、Flutter/Dart、SQLite 和 API 文档中的 TSID 语义、传输格式与存储策略，先消除不一致，再分批完成代码修复。

**Architecture:** 先建立一份“TSID 字段真值矩阵”，把“是否由 TSID 生成”“字段是否承载 TSID 语义”“物理列类型是什么”“API 实际返回什么”“前端/Flutter/SQLite 如何存”拆开记录。然后按“后端事实 -> 文档契约 -> Admin Frontend -> Flutter/SQLite -> 测试与资产”的顺序分批收敛，避免客户端在契约未冻结前反复返工。

**Tech Stack:** Erlang/OTP, PostgreSQL/TimescaleDB migrations, Markdown/OpenAPI/AsyncAPI, TypeScript/Vite Admin Frontend, Flutter/Dart, SQLite.

---

## 当前结论（已确认）

### 1. 直接由 TSID 生成器管理的对象，不等于已经全链路一致

- `imboy/src/imboy_app.erl` 中注册了 **68** 个命名 TSID 生成器。
- `imboy/src/repo/*.erl` 中实际可见的 `elib_tsid:generate/1` 调用覆盖 **60** 个名称。
- `imboy/priv/migrations/00000080_tsid_migration.sql` 中被迁移为 `BIGINT` 的 `id` 列覆盖 **73** 张表。
- 这三份名单目前并不一致，说明仓库里同时存在：
  - 命名映射不一致
  - 迁移覆盖不一致
  - 代码路径未落到 Repo
  - 已弃用/不该使用 TSID 生成器的残留配置

### 2. 高优先级异常（已确认，不需要再猜）

- `imboy/src/repo/user_log_repo.erl` 插入时强行写入 `id`，但 `imboy/priv/migrations/00000008_user_log.sql` 的 `user_log` 表没有 `id` 列。
- `imboy/src/repo/user_setting_repo.erl` 的 `save/3` 和 `update/2` 都写入 `id`，但 `imboy/priv/migrations/00000009_user_setting.sql` 的 `user_setting` 表主键只有 `user_id`，也没有 `id` 列。
- `imboy/src/imboy_app.erl` 把 `verification_code` 和 `geo_people_nearby` 注册成 TSID 生成器，但：
  - `verification_code.id` 是业务字符串主键（`varchar(80)`）
  - `geo_people_nearby` 没有 `id` 列，主键是 `user_id`
- 命名存在系统性错位：
  - `group_info` <-> `group`
  - `friend` <-> `user_friend`
  - `friend_category` <-> `user_friend_category`
  - `group_schedule_reminder` <-> `group_schedule_remind`
  - `group_album_comment` <-> `group_album_photo_comment`

### 3. API 契约已经自相矛盾

- `imboy/doc/api/rest-api.md`
- `imboy/doc/api/tsid-field-convention.md`
- `imboy/doc/api/openapi.yaml`
- `imboy/doc/api/asyncapi.yaml`

上述文档整体仍把 JSON 中的 TSID 描述为 `integer/int64`。

但管理端实际已有多个 handler 在响应阶段把 TSID 转成字符串：

- `imboy/src/adm/adm_message_handler.erl`
- `imboy/src/adm/adm_group_handler.erl`
- `imboy/src/adm/adm_channel_handler.erl`
- `imboy/src/adm/adm_feedback_handler.erl`
- `imboy/src/adm/adm_user_handler.erl`
- `imboy/src/adm/adm_logout_apply_handler.erl`

这意味着现在至少存在三套“真相”：

- 文档说：JSON integer
- Admin API 实际行为：很多字段已经是 JSON string
- Admin Frontend 仍保留 `safeParseBigIntJson` 兜底

### 4. Admin Frontend 还没有完成 TSID 类型收敛

- `imboy-admin-frontend/src/types/common.ts` 已定义 `EntityId = string`
- 但仍有 **9** 个文件继续使用本地 `IdLike = string | number`

当前残留文件：

- `imboy-admin-frontend/src/services/api/admins.ts`
- `imboy-admin-frontend/src/modules/channels/api/public.ts`
- `imboy-admin-frontend/src/modules/groups/api/public.ts`
- `imboy-admin-frontend/src/modules/moments/api/public.ts`
- `imboy-admin-frontend/src/modules/identity/api/users.ts`
- `imboy-admin-frontend/src/modules/social_graph/api/tags.ts`
- `imboy-admin-frontend/src/modules/ops_governance/api/reports.ts`
- `imboy-admin-frontend/src/modules/social_graph/api/collects.ts`
- `imboy-admin-frontend/src/modules/groups/api/enhancements.ts`

### 5. Flutter/SQLite 侧的旧迁移文档已经过期

- `imboyapp-loop-chat-bubble/docs/tsid_migration_plan.md` 仍写着“SQLite 中所有 ID 列都是 TEXT，无需数据库迁移”。
- 实际代码和资产已经不是这个状态：
  - `imboyapp-loop-chat-bubble/lib/service/sqlite.dart` 的 `_dbVersion = 16`
  - `imboyapp-loop-chat-bubble/assets/migrations/upgrade.sql` 已把多张表的 TSID 相关列从 `TEXT` 迁到 `INTEGER`
  - `imboyapp-loop-chat-bubble/assets/example10.db` 的 `PRAGMA user_version = 16`
- 已确认这些预置 SQLite 表使用了 `INTEGER`：
  - `channel`
  - `msg_c2c`
  - `group_notice`
  - `conversation`

### 6. Flutter 端还混着“远端 TSID”和“本地缓存自增主键”两种语义

这不是错误，但必须在计划里明确，不然很容易误改。

典型例子：

- `conversation.id` 在 SQLite 里是本地自增行号，不应简单等价为后端实体 TSID
- `conversation.peer_id` / `conversation.last_msg_id` 是远端 TSID
- `channel_subscription.id` / `channel_admin.id` 是本地自增
- `channel_id` / `user_id` / `last_message_id` 是远端 TSID

### 7. PostgreSQL 里“承载 TSID 语义”的列，并不都已经是 BIGINT

已确认：

- 典型 BIGINT TSID 字段：
  - `msg_c2c.from_id`
  - `msg_c2c.to_id`
  - `msg_c2g.from_id`
  - `msg_c2g.to_id`
  - `channel.creator_uid`
  - `channel_subscription.channel_id`
  - `channel_message.author_id`
  - `group.owner_uid`
  - `group.creator_uid`

- 典型仍为字符串物理类型、但语义上承载 TSID 的字段：
  - `conversation.user_id`
  - `conversation.peer_id`
  - `conversation.last_msg_id`
  - `msg_c2c.msg_id`
  - `msg_c2g.msg_id`

结论：后续修复必须区分“TSID 语义收敛”和“物理列类型迁移”，不能默认这两件事是同一个动作。

---

## 执行进度（2026-04-11 Checkpoint B）

- [x] Batch 0 首版矩阵已建立：
  - `imboy/doc/api/tsid-field-matrix.md`
- [x] Batch 1 的安全子集已落地：
  - `user_log_repo` 不再向无 `id` 列的 `user_log` 写入 TSID
  - `user_setting_repo` 不再向无 `id` 列的 `user_setting` 写入 TSID
  - `imboy_app:tsid_generator_names/0` 已移除误注册对象：
    - `user_log`
    - `user_setting`
    - `verification_code`
    - `geo_people_nearby`
  - `elib_pg_sql` 已恢复 `insert/2` 兼容导出，避免现有 Repo 在运行时触发 `undef`
- [ ] Batch 1 剩余项：
  - 把“生成器名 <-> 实际表名”沉淀成单一映射来源
  - 决定是否需要新增 `00000083_tsid_cleanup.sql` 之类的清理迁移
- [ ] Batch 2 以后仍等待 D1-D4 决策后再扩大范围：
  - JSON TSID 统一 `string` 还是双轨契约
  - Flutter Web 是否在支持范围
  - PostgreSQL 物理类型是否在第一轮迁移

---

## 推荐先冻结的决策（需要你确认）

### 决策 D1：JSON API 中的 TSID 最终统一成什么格式

**推荐：统一为 JSON string。**

原因：

- 当前 TSID 值在 2026 年已明显超出 JavaScript `Number.MAX_SAFE_INTEGER`
- Admin API 实际已经在这么做
- TypeScript、Flutter、SQLite 都能兼容 string -> int 的边界转换
- 文档统一后，前端不再依赖脆弱的“碰到大整数再改写”的兜底逻辑

如果你不想全量切成 string，则要接受“双轨契约”：

- App REST / WebSocket 继续 JSON integer
- Admin API 固定 JSON string
- 文档必须明确区分，不再写“所有 API 都是 integer”

### 决策 D2：无独立实体主键的表是否继续挂 TSID 生成器

**推荐：不继续。**

建议直接从 TSID 生成器名单中移除或停用以下对象：

- `verification_code`
- `geo_people_nearby`
- `user_setting`
- `user_log`（除非确认要补真实 `id` 列）

### 决策 D3：Flutter Web 是否在 TSID 修复范围内

**推荐：明确支持范围。**

- 如果 Flutter Web 仍然是支持目标，则网络边界中的 TSID 不能继续默认使用 `int`
- 如果 Flutter Web 不在支持范围，则要把这个前提写进文档，避免继续写出“Web 也安全”的错误说明

### 决策 D4：这次是否做 PostgreSQL 物理列类型全量迁移

**推荐：第一轮不做全量。**

第一轮只做：

- 真值矩阵
- 契约统一
- 代码/文档/类型收敛
- 明显错误修复

对 `conversation.user_id` / `peer_id` / `last_msg_id` 一类“语义是 TSID、物理还是字符串”的列，先建专项清单，不在第一批里直接改表。

---

## 直接使用 TSID 生成器的对象（初步清单）

以下对象可以视为“高置信度由应用层生成 TSID 主键”的第一批范围，后续执行时优先核对其 `id` 字段、API 输出和客户端模型：

### 用户与社交

- `user`
- `user_device`
- `user_denylist`
- `user_tag`
- `user_tag_relation`
- `friend`
- `friend_category`

### 群组

- `group_info`
- `group_member`
- `group_notice`
- `group_log`
- `group_random_code`
- `group_category`
- `group_tag`
- `group_vote`
- `group_vote_option`
- `group_schedule`
- `group_schedule_reminder`
- `group_album`
- `group_album_photo`
- `group_album_comment`
- `group_file`
- `group_task`
- `group_task_assignment`

### 消息与会话

- `msg_c2c`
- `msg_c2g`
- `msg_c2s`
- `msg_s2c`
- `msg_store`
- `msg_read`
- `msg_mention`
- `msg_forward`
- `msg_reaction`
- `conversation_delete`
- `conversation_pin`

### 频道

- `channel`
- `channel_message`
- `channel_subscription`
- `channel_admin`
- `channel_message_view`
- `channel_order`
- `channel_invitation`

### Moment / 举报 / 钱包 / 直播 / 配置

- `moment_post`
- `moment_comment`
- `moment_like`
- `moment_timeline`
- `moment_post_acl`
- `moment_report`
- `report_ticket`
- `report_action_log`
- `wallet`
- `wallet_transaction`
- `live_room`
- `app_version`
- `app_ddl`
- `app_upgrade_log`
- `app_version_policy`

> 说明：这份清单是“应用层明确调用 `elib_tsid:generate/1` 的高置信度对象”。它不是最终字段矩阵，不能等同于“所有 TSID 语义字段”。

---

## 第一轮不直接改动、但必须纳入矩阵的 TSID 语义字段

这些字段未必在本表内生成 TSID，但它们承载 TSID 语义，必须和 API/客户端一起核对：

- `uid`
- `user_id`
- `from_id`
- `to_id`
- `peer_id`
- `group_id`
- `owner_uid`
- `creator_uid`
- `author_id`
- `channel_id`
- `conversation_id`
- `last_msg_id`
- `mentioned_uid`
- `from_uid`
- `reply_to_uid`
- `reporter_uid`
- `handled_by`
- `recipient_uid`

---

## 分批执行清单

### Batch 0: 建立 TSID 真值矩阵（必须先做）

**目标：** 给每个字段打上统一标签，后续所有改动都基于这张表。

**Files:**
- Create: `imboy/doc/api/tsid-field-matrix.md`
- Read/Verify:
  - `imboy/src/imboy_app.erl`
  - `imboy/src/repo/*.erl`
  - `imboy/priv/migrations/*.sql`
  - `imboy/doc/api/rest-api.md`
  - `imboy/doc/api/openapi.yaml`
  - `imboy/doc/api/asyncapi.yaml`
  - `imboy-admin-frontend/src/**/*`
  - `imboyapp-loop-chat-bubble/lib/**/*`
  - `imboyapp-loop-chat-bubble/assets/migrations/upgrade.sql`

**产出：**
- 表名
- 字段名
- 是否“本表生成 TSID”
- 是否“承载远端 TSID”
- PostgreSQL 物理类型
- API 实际输出类型
- Admin Frontend 类型
- Flutter 模型类型
- SQLite 物理类型
- 是否本地 surrogate key

**验收：**
- 所有后续修改文件都能回链到这张矩阵
- 所有命名映射都显式写清楚

### Batch 1: 修后端生成器/Repo/表结构明显冲突（P0）

**目标：** 先把会导致错误写入或错误认知的地方修掉。

**Files:**
- Modify:
  - `imboy/src/imboy_app.erl`
  - `imboy/src/repo/user_log_repo.erl`
  - `imboy/src/repo/user_setting_repo.erl`
  - `imboy/src/repo/group_repo.erl`
  - `imboy/src/repo/friend_repo.erl`
  - `imboy/src/repo/friend_category_repo.erl`
  - `imboy/src/repo/group_schedule_repo.erl`
  - `imboy/src/repo/group_album_repo.erl`
- Create:
  - `imboy/priv/migrations/00000083_tsid_cleanup.sql`（编号示意，实际执行前再确认）
- Verify:
  - `imboy/priv/migrations/00000008_user_log.sql`
  - `imboy/priv/migrations/00000009_user_setting.sql`
  - `imboy/priv/migrations/00000005_verification_code.sql`
  - `imboy/priv/migrations/00000023_geo_people_nearby.sql`

**动作：**
- 删除或修正不该存在的 TSID 生成器注册
- 对命名错位建立统一映射并落回代码注释/常量
- 处理 `user_log` / `user_setting` 的 `id` 插入冲突
- 明确 `verification_code` / `geo_people_nearby` 不属于“TSID 主键表”

**验收：**
- Repo 不再向无 `id` 列的表写入 `id`
- TSID 生成器名单中不再出现明显伪对象
- 命名映射有单一来源

### Batch 2: 冻结 JSON 契约并修 API 文档（P0）

**目标：** 先统一“对外怎么说”，再统一“代码怎么做”。

**Files:**
- Modify:
  - `imboy/doc/api/rest-api.md`
  - `imboy/doc/api/tsid-field-convention.md`
  - `imboy/doc/api/openapi.yaml`
  - `imboy/doc/api/asyncapi.yaml`
  - `imboy/doc/api/channel_api_contract_v1.md`
  - `imboy/doc/api/moment_api_contract_v1.md`

**动作：**
- 选定 JSON contract：`string` 或“双轨”
- 明确哪些 handler 已经 string 化，哪些仍是 integer
- 把“TSID 语义字段”与“非 TSID ID 字段”重新列全
- 把 `conversation.*` 这类“语义是 TSID、物理是字符串”的字段单独注明

**验收：**
- 文档不再同时出现互相冲突的描述
- OpenAPI/AsyncAPI 与真实 handler 行为一致

### Batch 3: Admin Frontend 全量收敛到 `EntityId`（P1）

**目标：** 消除 `IdLike = string | number` 的残留。

**Files:**
- Modify:
  - `imboy-admin-frontend/src/services/api/admins.ts`
  - `imboy-admin-frontend/src/modules/channels/api/public.ts`
  - `imboy-admin-frontend/src/modules/groups/api/public.ts`
  - `imboy-admin-frontend/src/modules/groups/api/enhancements.ts`
  - `imboy-admin-frontend/src/modules/moments/api/public.ts`
  - `imboy-admin-frontend/src/modules/identity/api/users.ts`
  - `imboy-admin-frontend/src/modules/social_graph/api/tags.ts`
  - `imboy-admin-frontend/src/modules/social_graph/api/collects.ts`
  - `imboy-admin-frontend/src/modules/ops_governance/api/reports.ts`
- Verify:
  - `imboy-admin-frontend/src/types/common.ts`
  - `imboy-admin-frontend/src/services/api/client.ts`
  - 相关页面和测试文件

**动作：**
- 删除本地 `IdLike`
- 全部改用 `EntityId`
- 修测试数据中把 TSID 写成 number 的用例
- 决定是否继续保留 `safeParseBigIntJson` 作为兜底

**验收：**
- 搜索 `type IdLike =` 在 `imboy-admin-frontend/src` 下为 0
- 编译和测试通过

### Batch 4: Flutter 端收敛“远端 TSID vs 本地 surrogate”边界（P0/P1）

**目标：** 先统一语义，再决定是否改具体类型。

**Files:**
- Modify:
  - `imboyapp-loop-chat-bubble/docs/tsid_migration_plan.md`
  - `imboyapp-loop-chat-bubble/lib/utils/tsid_helper.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/model_parse_utils.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/webrtc_signaling_model.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/channel_model.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/channel_message_model.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/group_model.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/group_member_model.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/conversation_model.dart`
  - `imboyapp-loop-chat-bubble/lib/store/model/message_model.dart`

**动作：**
- 修正文档中的错误结论
- 明确哪些模型字段是远端 TSID，哪些只是本地 SQLite 行号
- 如果支持 Flutter Web，收敛网络边界类型，避免继续默认 `int`
- 对 `parseModelInt` 使用点做专项审计，避免把应当保留字符串的远端 ID 强行数值化

**验收：**
- `docs/tsid_migration_plan.md` 与真实数据库版本一致
- Web 风险说明不再错误宣称“TSID 不超过 2^53”
- 关键 DTO/Model 的 ID 语义有注释或测试保护

### Batch 5: SQLite 资产与迁移脚本对齐（P1）

**目标：** 确认脚本、预置库、仓库代码三者一致。

**Files:**
- Modify:
  - `imboyapp-loop-chat-bubble/assets/migrations/upgrade.sql`
  - `imboyapp-loop-chat-bubble/assets/migrations/downgrade.sql`
  - `imboyapp-loop-chat-bubble/lib/service/sqlite.dart`
- Regenerate/Verify:
  - `imboyapp-loop-chat-bubble/assets/example10.db`

**动作：**
- 明确哪些列是远端 TSID INTEGER
- 明确哪些列是本地 surrogate AUTOINCREMENT
- 如果预置库与迁移脚本不一致，重建 `example10.db`

**验收：**
- `PRAGMA user_version`
- `upgrade.sql`
- `example10.db`
- 关键 Repo/Model

以上四者一致。

### Batch 6: 回归测试与验收（P1）

**目标：** 确保修的是契约，不是只改注释。

**Files:**
- Modify/Add Tests:
  - `imboy-admin-frontend/src/services/api/client.bigint.test.ts`
  - Admin Frontend 相关 API 类型测试
  - `imboyapp-loop-chat-bubble/test/utils/tsid_helper_test.dart`
  - Flutter 关键模型 `fromJson` 测试
  - Erlang 对应 handler/repo 单测或集成测

**动作：**
- 添加 TSID string/int 双格式兼容测试
- 添加“错误把 TSID 当 JS number / Flutter Web int”的回归用例
- 添加生成器名单与 DDL 名单一致性测试（至少脚本检查）

**验收：**
- 文档、代码、测试、数据库资产形成闭环

---

## 推荐执行顺序

1. Batch 0
2. 决策 D1-D4 由你确认
3. Batch 1
4. Batch 2
5. Batch 3
6. Batch 4
7. Batch 5
8. Batch 6

---

## 我建议你优先确认的 6 个点

- 是否接受“JSON API 中的 TSID 统一改成 string”
- `user_log` 是要补真实 `id` 列，还是移除 Repo 中的 TSID 注入
- `user_setting` 是否继续坚持 `user_id` 作为唯一主键
- `verification_code` / `geo_people_nearby` 是否确认从 TSID 生成器名单移除
- Flutter Web 是否仍在支持范围内
- 第一轮是否只做“契约统一 + 明显错误修复”，暂缓 PostgreSQL 全量列类型迁移

---

## 备注

- 本计划先解决“事实不一致”和“契约不一致”，不把第一次执行扩大成全量历史字段重构。
- 真正高风险的不是某个 `int`/`string` 细节，而是同一个字段在后端、文档、管理端、Flutter、SQLite 五处各说各话。
- 只要你先确认上面的 6 个点，后续就可以按批次稳定推进。
