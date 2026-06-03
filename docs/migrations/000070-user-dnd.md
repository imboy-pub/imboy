# 迁移 000070：用户免打扰(DND)偏好 / Migration 000070: User Do-Not-Disturb (DND) Preference

> 简体中文为权威版本，English follows. / Simplified Chinese is authoritative; English follows.

---

## 1. 目的 / Purpose

**中文**：新增用户免打扰(DND)能力的数据层基础——按用户维度存储免打扰时段规则，并在 `user` 表增加全局免打扰开关。

**English**: Add the data-layer foundation for user Do-Not-Disturb (DND): store per-user DND time-window rules and add a global DND switch to the `user` table.

---

## 2. 变更内容 / Changes

| 段 / Section | 操作 / Operation | 对象 / Object |
|---|---|---|
| 1 | `CREATE TABLE` | `public.user_dnd_rule`（含 `uk_dnd_rule_UserId`） |
| 2 | `ADD COLUMN` | `public."user".dnd_enabled boolean NOT NULL DEFAULT false` |
| 3 | `CREATE INDEX` | `i_dnd_rule_Status`（部分索引 `WHERE status = 1`） |
| 4 | `UPDATE`（回填 / backfill） | 为已禁用用户（`status=0`）默认开启免打扰 |

**中文**：`user_dnd_rule` 以 `user_id` 唯一（1:1），`id` 为 `bigserial` 自增。时段以"自 0 点起的分钟数"(0–1439) 表示，支持跨午夜区间。

**English**: `user_dnd_rule` is unique by `user_id` (1:1) with a `bigserial` `id`. Time windows are expressed as minutes-from-midnight (0–1439) and support overnight ranges.

---

## 3. 执行机制与锁表评估 / Execution & Lock Assessment

**中文**：
- 本迁移由 `erlang_migrate:up/1`（`imboy_app` 启动时自动触发）将整个 `.up.sql` 文件包裹在**单个事务**中执行，任一语句失败则整体回滚。
- 因外层已包裹事务，文件内**不写** `BEGIN/COMMIT`，且**不使用** `CREATE INDEX CONCURRENTLY`（事务块内不被支持）。
- 目标为小表/新表，DDL 取 `ACCESS EXCLUSIVE` 锁但持锁时间可忽略，**无锁表风险**。
- ⚠️ 若未来涉及大表索引，必须走 DBA 手动 `CONCURRENTLY` 脚本，不能纳入自动迁移。

**English**:
- This migration runs via `erlang_migrate:up/1` (auto-triggered at `imboy_app` startup), wrapping the entire `.up.sql` in a **single transaction**; any failure rolls back atomically.
- Because the outer transaction is enforced, the file contains **no** explicit `BEGIN/COMMIT` and does **not** use `CREATE INDEX CONCURRENTLY` (unsupported inside a transaction block).
- Targets are small/new tables; DDL takes `ACCESS EXCLUSIVE` locks but holds them negligibly — **no locking risk**.
- ⚠️ Future large-table index changes must use a manual DBA `CONCURRENTLY` script, kept out of the automatic migration.

---

## 4. 回滚 / Rollback

**中文**：`imboy_migrate` 未封装 down CLI，需经 `remote_console` 调用库 API：

**English**: `imboy_migrate` exposes no down CLI; roll back via `remote_console` using the library API:

```erlang
Conf = config_ds:env(super_account).
{ok, Conn} = epgsql:connect(Conf).
erlang_migrate:down(#{conn => Conn, dir => imboy_migrate:get_scripts_path()}).
```

**中文**：`down.sql` 与 `up.sql` 严格逆序对称。注意 `DROP COLUMN dnd_enabled` 将丢失该列数据。

**English**: `down.sql` is strictly the reverse of `up.sql`. Note `DROP COLUMN dnd_enabled` discards that column's data.

---

## 5. 关联代码 / Related Code

| 层 / Layer | 模块 / Module |
|---|---|
| Repo | `user_dnd_rule_repo`（`tablename/0`·`find_by_uid/1`·`upsert/1`·`delete_by_uid/1`） |
| DS | `user_dnd_rule_ds`（缓存键 `{user_dnd_rule, Uid}`·`is_dnd_at/2`） |
| Test | `test/repo/user_dnd_rule_repo_tests.erl`·`test/ds/user_dnd_rule_ds_tests.erl` |

**中文**：`user.dnd_enabled` 全局开关属 `user` 表，接入时用 `user_repo:find_by_uid(Uid, <<"dnd_enabled">>)` 读取，无需新建模块。Handler/Logic 层待真实功能开发时补建。

**English**: The global `user.dnd_enabled` switch belongs to the `user` table; read it via `user_repo:find_by_uid(Uid, <<"dnd_enabled">>)` when integrating — no new module needed. Handler/Logic layers are to be built when the feature is actually developed.

---

## 6. 验证 / Verification

```bash
cd imboy && make compile && make eunit
# remote_console 中验证结构 / verify schema in remote_console:
#   \d public.user_dnd_rule   ; \d public."user"
```
