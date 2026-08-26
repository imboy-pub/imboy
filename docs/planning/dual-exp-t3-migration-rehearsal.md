# 双体验 v2.5.2 — WP2/T3 迁移演练报告（Gate W = W0）

> 执行分支：imboy `dual-exp-v21` | 执行日期：2026-08-26 | 执行者：T3 subagent
> 计划：`.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md` §七 WP2/T3
> 输入：`docs/planning/dual-exp-decision-brief.md`（R2 迁移草稿/EXPLAIN、R6 迁移器语义、Gate W=W0 Scope Contract）
> 环境：本地 PG 18.1 @ 127.0.0.1:4323（Docker），库 `imboy_v1`（连接参数 `config/sys.local.config` 的 `super_account`）

---

## 1. 交付物：3 对物理迁移文件（10 个逻辑变更，W0 裁剪后实际 7 项）

| # | 文件（`priv/migrations/`） | 逻辑变更 | 内容 |
|---|---|---|---|
| 1 | `00000076_workspace_foundation.up.sql` / `.down.sql` | ① workspace + ② workspace_member | 2 新表 + 5 索引 + 5 FK（user FK 镜像 00000009 惯例：主引用 CASCADE、可空审计列 SET NULL）；`workspace_member` 复合主键 `(workspace_id,user_id)` 即计划要求的唯一约束 |
| 2 | `00000077_resource_scope.up.sql` / `.down.sql` | ③ channel scope + ④ "group" scope + ②④ 的可延迟触发器 | 双表加 `scope`+`workspace_id`+双向 XOR CHECK（R2.3 草稿原文）+FK+各 2 部分索引；`trg_group_member_ws_subset`（写入端子集）与 `trg_workspace_member_remove_guard`（移除端保护），均 `DEFERRABLE INITIALLY DEFERRED` |
| 3 | `00000078_project_foundation.up.sql` / `.down.sql` | ⑤ project(W0 版) + ⑧ project_task + ⑩ project_event | 3 新表 + 复合 FK（DEFERRABLE）+ `trg_project_owner_membership_active`（owner active 校验，可延迟） |

### W0 裁剪落实（Scope Contract 硬约束）

- ❌ 不建：`project_member`（W0 禁止落表）、`project_milestone`、`project_channel_rel`
- ❌ `project` 无 `links` 列
- ❌ ⑤ 无 Project Member 触发器（只保留 Workspace Membership 校验）
- ✅ `project_event.event_type` CHECK 仅列 W0 实际值：`project_created|project_status|task_created|task_status|task_assignee`（未来加值需新迁移，正常演进）

### 与拆分建议的一处偏差（按 R6 语义微调，已获任务卡授权）

任务卡细节 2 将 workspace_member 的移除保护触发器归在 ②（逻辑上属 00000076），但该触发器函数体引用 `"group".scope` 列（00000077 才引入）——若定义在 76，down 77 之后会留下引用不存在列的悬挂触发器，违反 R6"每份迁移 = 自洽原子单元"。故移入 00000077 定义（与 group_member 子集触发器同文件，二者本就是同一不变量 `Group Member ⊆ Workspace Member` 的写入端/移除端）。76 的表注释中已记录该决策。

### group_member 实际结构核实（00000001_foundation，任务卡要求的差异报告）

| 预期项 | 实际核实结果 |
|---|---|
| 唯一约束 | `uk_gid_uid(group_id,user_id)` 是**唯一索引**（非约束）✅ 与 R2.2 一致 |
| active 判定 | `status smallint CHECK(-1,0,1,2)`，active = `status=1`；`is_join boolean` 为独立维度（是否加入群），不参与子集判定——触发器仅用 status=1 |
| 外键 | **无任何 FK**（group_member 不引用 group/user）——触发器为该表第一个引用式约束 |
| 既有触发器 | `trg_group_member_updated_at`（BEFORE UPDATE，00000009）与本批 AFTER 约束触发器共存无冲突 |
| 其他 | `id bigint NOT NULL`（TSID 手动赋值，无序列默认）；`role CHECK(0..5)` |

channel 表实际位于 `00000003_channel.up.sql`（不在 foundation）；经 00000072 后已无 `type` 列，有 `visibility/access_type/join_policy`——新加列与其正交。

---

## 2. 验证证据（逐项）

### 2.1 空库演练 ✅（替代路径，如实记录）

任务卡首选"临时库从零跑 00000001-78"；实际采用任务卡预设的替代路径：**pg_dump imboy_v1 schema-only（75 版，134 表）→ 恢复到临时库 `imboy_t3_rehearsal`** → 仅演练 76/77/78。原因：74/75 无 down 文件（R6 已确认），"从零可重放全部历史"未验证前不冒险；且 foundation 为合并基线（非逐份原始迁移），从零重放等价性无契约保证。

| 步骤 | 命令/方式 | 结果 |
|---|---|---|
| 恢复 75 版 schema | `pg_dump --schema-only --no-owner --no-privileges` → `psql -f` | ✅ 134 表（与源一致），RESTORE exit=0 |
| up ×2（幂等） | 依次 `psql -v ON_ERROR_STOP=1 -f 0000007{6,7,8}.up.sql` 两轮 | ✅ 第一轮全部创建；第二轮全部 NOTICE skip（IF NOT EXISTS / OR REPLACE / DROP-IF-EXISTS-再 ADD 幂等模式成立） |
| down 78→77→76 | 逆序执行三份 `.down.sql` | ✅ 表数回到 134；workspace/project 5 表、scope/workspace_id 列、3 触发器全部清零 |
| up→down→up 循环 | down 后重新 up | ✅ 134→139（+5 新表），三份 up 再次成功 |

> 注：演练库的 up/down 由 psql 直接执行（临时库无迁移器 tracking 状态）；迁移器路径的 down 演练见 §2.4（在 imboy_v1 上经 `erlang_migrate:down/2` 实测，含事故与恢复，证据更完整）。

### 2.2 约束行为测试 ✅（临时库，psql 直插，每条含错误信息）

测试数据：`"user"(900001..900004)`、`workspace(800001, owner=900001)`、`workspace_member(800001,900001,owner,active)`。

**XOR 双向（I1）**：

| # | SQL | 结果 |
|---|---|---|
| XOR-1 | `INSERT INTO channel (..., scope, workspace_id) VALUES (..., 'personal', 800001)` | ✅ 拒绝：`ERROR: new row for relation "channel" violates check constraint "chk_channel_scope_xor"` |
| XOR-2 | `INSERT INTO channel (..., scope) VALUES (..., 'workspace')`（workspace_id 隐式 NULL） | ✅ 拒绝：同上 |
| XOR-3 | `"group"` personal+workspace_id | ✅ 拒绝：`violates check constraint "chk_group_scope_xor"` |
| XOR-4 | `"group"` workspace+NULL | ✅ 拒绝：同上 |
| OK-path | channel/group 各插 workspace（带 id）与 personal（NULL）行 | ✅ `INSERT 0 2` ×2 |

**Group Member ⊆ Workspace Member（写入端，I14）**：

| # | 场景 | 结果 |
|---|---|---|
| SUBSET-1 | u1（active wm）入 workspace 群 | ✅ 通过 |
| SUBSET-2 | u2（无 wm 行）入 workspace 群，单语句自动提交 | ✅ COMMIT 时拒绝：`ERROR: group_member 子集约束（Group Member ⊆ Workspace Member）：用户 900002 不是 workspace 800001 的 active workspace_member...`（INSERT 本身放行、COMMIT 报错——DEFERRABLE INITIALLY DEFERRED 生效） |
| SUBSET-3 | u2 有 `status='removed'` 的 wm 行再入群 | ✅ 仍拒绝（active 校验，非仅存在性） |
| SUBSET-4 | 同事务先 `UPDATE workspace_member SET status='active'` 再入群 | ✅ 通过（可延迟设计意图：同事务写序自由） |
| SUBSET-5 | u2 无 wm 入 **personal** 群 | ✅ 通过（personal 群不受 Workspace 子集约束） |

**Workspace Member 移除保护（移除端，§1.4.2 规则 8 的 DB 兜底）**：

| # | 场景 | 结果 |
|---|---|---|
| GUARD-1 | u1 是 workspace 群 active group_member，直接 `UPDATE workspace_member SET status='removed'` | ✅ COMMIT 时拒绝：`ERROR: workspace_member 移除保护：用户 900001 在 workspace 800001 仍有 active 的 workspace 群成员关系...` |
| GUARD-2 | 同事务先 `UPDATE group_member SET status=0` 再移除 wm | ✅ 通过（正确移除流程） |

**Project Owner（W0 版⑤）**：

| # | 场景 | 结果 |
|---|---|---|
| OWNER-1R | owner 完全无 wm 行，单事务 INSERT project + COMMIT | ✅ COMMIT 时拒绝：`ERROR: insert or update on table "project" violates foreign key constraint "fk_project_owner_membership"`；事务回滚（project 0 行）——可延迟 FK 允许 INSERT 即时通过、提交才校验 |
| OWNER-2 | owner 有 `removed` wm 行（FK 满足、状态不满足） | ✅ 触发器拒绝：`ERROR: project Owner 校验：用户 900001 不是 workspace 800001 的 active workspace_member...` |
| OWNER-3 | 同事务先激活 wm 再建 project | ✅ 通过 |
| OWNER-4 | **反序**：先 INSERT project 再补 INSERT owner 的 wm 行（T6a 计划顺序） | ✅ 通过（FK+触发器双 DEFERRABLE，事务内写序完全自由） |

**W0 断言**（临时库与 imboy_v1 均验证）：

```sql
SELECT tablename FROM pg_tables WHERE tablename IN ('project_member','project_milestone','project_channel_rel');  -- 0 行 ✅
SELECT count(*) FROM information_schema.columns WHERE table_name='project' AND column_name='links';               -- 0 ✅
```

### 2.3 imboy_v1 正式应用（项目机制）✅

```
erl -pa ebin deps/*/ebin -config config/sys.local -eval 'imboy_migrate:migrate().'
[imboy_migrate] running migrations from .../priv/migrations
MIGRATE RESULT: ok
[imboy_migrate] all migrations applied
```

- `schema_migrations`: `78|f` ✅；history 76/77/78 已记录 ✅
- **I2 存量回填断言（真实存量，零回填验证）**：
  - `channel`：26 行，26 行 `scope='personal' AND workspace_id IS NULL`，违规 0 ✅
  - `"group"`：518 行，518 行 personal/NULL，违规 0 ✅（518 vs R2.2 的 515：画像后新增 3 行，正常漂移）
- 新表 5 张就位；表总数 134→139 ✅

### 2.4 迁移器 down 演练：事故 + force 恢复 ⚠️→✅（本报告最重要发现）

在 imboy_v1（version=78）经迁移器执行部分回滚：

```erlang
erlang_migrate:down(Config, 3).   %% 返回 ok
```

**事故**：down 3 份（78/77/76）的 SQL 全部正确执行（每份独立事务、down 干净：表数回 134、新对象清零、业务数据零丢失——channel 26/group 518/user 16143/msg_c2c 1997/msg_c2g 363 全部与 R2.2 画像一致），**但 version 被置为 `undefined` 而非 75**。随后 `erlang_migrate:up(Config)` 从"零状态"重放 00000001，撞 foundation 的 `CREATE FUNCTION`（无 OR REPLACE）→ `duplicate_function (42723)` → `version=1, dirty=t`。

**根因（erlang_migrate 库缺陷，`erlang_migrate.erl:378-381`）**：`apply_down` 的 `PrevVersion` 取自**本次回滚 sublist 的剩余项**；部分回滚时 sublist 耗尽 → `PrevVersion=undefined` → `set_version(undefined)` = 清空 tracking（该语义本应仅用于全部回滚到零）。`down(Config, N)`（N < 已应用总数）与 `goto` 的部分回滚路径均受影响。

**恢复（R6 记录的 force/2 路径，实测有效）**：

```erlang
erlang_migrate:force(Config, 75).   %% ok（set_version 75/f + strict_rebuild 重建 history）
erlang_migrate:up(Config).          %% ok（重新应用 76/77/78）
%% FINAL: version=78 dirty=false
```

恢复后全量断言复核通过（I2 存量 26/26、518/518；W0 断言；5 新表；3 触发器在位；表数 139）。

**对 T3 交付与生产运维的结论**：
1. 三份迁移的 down SQL 本身干净可逆（临时库与本次实测双重证明）；
2. **生产部分回滚禁止直接用 `erlang_migrate:down(Config, N)` / `goto` 到中间版本**——回滚后必须立即 `force(Config, 75)` 校正 tracking，或干脆手动 psql 执行三份 down 后 `force`；
3. 建议（超出 T3 范围，记录给后续）：erlang_migrate 修复 `apply_down` 的 `PrevVersion` 语义（部分回滚时应取目标版本而非 sublist 剩余），或 imboy 侧封装 `migrate_down_to/1` 内置 force 校正。

### 2.5 EXPLAIN 抽查 ✅（imboy_v1 真实表；R2.3 三类查询）

| 查询 | 默认计划 | 说明 |
|---|---|---|
| group Workspace 列表 | `Index Scan using i_group_scope_ws`（`EXPLAIN ANALYZE`: 0.061 ms, Buffers hit=5） | ✅ 直接命中 |
| group 个人列表 | `Index Scan using i_group_scope_personal` | ✅ 直接命中 |
| channel 个人/Workspace 列表、按 ID 直访 | `Seq Scan`（26 行小表，含 pkey 直访亦 Seq Scan——PG 小表正常行为） | ✅ `SET enable_seqscan=off` 强制形态下三者分别命中 `i_channel_scope_personal` / `i_channel_scope_ws` / `channel_pkey`（Index Only Scan）——索引可用性证明；表增长后优化器自然切换 |

### 2.6 make app ✅

`make app` exit=0，`grep -ci warning` = 0（迁移为纯 SQL，不影响编译；无 .erl 改动）。

### 2.7 针对性 eunit ✅

不跑全量（约 40 分钟，R1 基线 5804/59）。仓内迁移相关测试 `test/lib/imboy_migrate_tests.erl`（纯 meck，不碰真实库）：**7/7 passed**（0.472s）。

### 2.8 收尾

- 演练库 `imboy_t3_rehearsal` 已 DROP；
- EXPLAIN 样本数据（ws 810001 及其 channel/group/user 910001）已从 imboy_v1 清除（`workspace` 0 行、无 workspace 归属行）；
- imboy_v1 最终状态：`78|f`。

---

## 3. 发布结论（可在线 / 需窗口 / 回滚策略）

延续 R2.5 框架（生产规模数据 BLOCKED 待人工补齐，本地证据不能外推生产）：

| 步骤 | 结论 | 依据 |
|---|---|---|
| 加列（scope/workspace_id）+ 新表 + CHECK/FK/触发器 | **可在线** | PG 18 常量默认值加列为元数据变更；本地 26/518 行全程毫秒级；触发器为新表/新列服务，不影响存量写路径 |
| 4 个部分索引（channel/group × personal/ws） | **需发布窗口** | erlang_migrate 恒包事务 ⇒ `CREATE INDEX CONCURRENTLY` 不可用（R2.5/R6）；普通 `CREATE INDEX` 持 SHARE 锁阻塞写入，时长 ∝ 生产行数×IO——**生产行数与峰值写入未授权获取，窗口判定 BLOCKED 待人工** |
| 回滚 | **down 可逆但须修正迁移器 tracking** | 三份 down 干净（数据无损于 75→78 增量对象）；生产回滚路径 = 手动/迁移器 down 后**必须** `force(Config, 75)` 校正 version（§2.4 事故教训）；一旦 76-78 之上叠加了后续迁移或 workspace 产生真实数据，down 将丢数据——届时只能前滚兼容 |
| 升级窗口建议 | 双保险 | 发布时设置 `statement_timeout` + `lock_timeout`（R2.5 建议）；任一份失败进 dirty → `force` 人工恢复路径已在本地实测走通 |

**生产部署判定：BLOCKED（待人工授权获取生产行数/写入画像后终判）**——与 Decision Brief R2.5 结论一致，本次本地证据不改变该判定。

---

## 4. 残留与移交

1. **erlang_migrate 部分回滚缺陷**（§2.4）：建议独立 issue 跟进（库属独立仓 gitee.com/imboy-pub/erlang_migrate）；WP3/WP4 期间无人需要 down，不阻塞。
2. scope/workspace_id 创建后不可变（§1.4.2 规则 9）由应用层保证，schema 未加 immutability 触发器（计划未要求）。
3. `project_event.actor_id` 无 user FK（镜像 app_upgrade_log 审计惯例），系统 user 为软删，已注释说明。
4. workspace.type 无值域 CHECK（预留扩展字段，计划未定义值域）。

## 5. 验证清单汇总

| 项 | 状态 |
|---|---|
| 临时库 up ×2 幂等 | ✅ |
| down 回滚干净（134 基线）/ up→down→up 循环（139） | ✅ |
| XOR 双向 ×4 + 合法路径 | ✅ |
| group_member 子集（active OK / 无 wm 拒 / removed wm 拒 / 同事务激活过 / personal 放行） | ✅ 5/5 |
| workspace_member 移除保护（有下级拒 / 同事务先清下级过） | ✅ 2/2 |
| project owner（无 wm FK 拒 / removed 触发器拒 / 正序过 / 反序过） | ✅ 4/4 |
| W0 断言（project_member/milestone/rel/links 不存在） | ✅ |
| I2 存量回填（26/26、518/518 全 personal/NULL） | ✅ |
| imboy_v1 项目机制 up → version 78/f | ✅ |
| 迁移器 down 3 + force(75) + up 恢复 → 78/f | ✅（含事故记录 §2.4） |
| EXPLAIN（group 命中 / channel 强制形态命中 / ANALYZE 0.061ms） | ✅ |
| make app 零警告 | ✅ |
| imboy_migrate_tests 7/7 | ✅ |
