# 备份恢复演练记录 2026-06 / Restore Drill 2026-06

> 简体中文为权威版本，English follows.
> 关联 / Related: [backup-restore.md](./backup-restore.md)、`scripts/backup_pg.sh`、`scripts/restore_pg.sh`
> **结论 / Result: ✅ PASS**（修复 2 个真实缺陷后达成 / after fixing 2 real defects）

---

## 演练信息 / Drill Info

| 项 / Item | 值 / Value |
|---|---|
| 日期 / Date | 2026-06-10 |
| 执行环境 / Environment | macOS (darwin), Docker 容器 `imboy_pg18`（PostgreSQL 18 + timescaledb + postgis），bash 3.2.57 |
| 源库 / Source DB | `imboy_v1`（本地开发库，111 张表）|
| 目标库 / Target DB | `imboy_restore_test`（演练后已清理 / dropped after drill）|
| 备份文件 / Backup file | `data/backups/pg/imboy_v1_20260610T034215Z.dump`（564 KB，pg_dump -Fc）|
| RTO 实测 / Measured RTO | **2 秒**（目标 < 30 min，远超达标）|
| RPO | 全量备份间隔（当前 `archive_mode=off`，无 PITR；生产建议开启 WAL 归档）|

---

## 演练步骤 / Steps

```bash
# 1) 备份（库名 imboy_v1 与脚本默认 imboy_pro 不同，需覆盖）
POSTGRES_DB=imboy_v1 bash scripts/backup_pg.sh
#   → data/backups/pg/imboy_v1_20260610T034215Z.dump，完整性校验通过

# 2) 恢复到测试库（FORCE=1 跳过交互确认，仅用于自动化演练）
FORCE=1 POSTGRES_DB=imboy_v1 bash scripts/restore_pg.sh \
  data/backups/pg/imboy_v1_20260610T034215Z.dump --target imboy_restore_test

# 3) 双边精确行数对比（源库 vs 恢复库）
```

---

## 演练暴露的真实缺陷与修复 / Defects Found & Fixed

> 脚本此前仅做过 `bash -n` 语法校验、从未真实运行，两个运行时致命缺陷一直潜伏。
> 这正是"未演练的备份等于没有备份"的实证。

### 缺陷 1（HIGH）：`pg_restore` 从 stdin 并行恢复不被支持
- **现象**: `pg_restore: error: parallel restore from standard input is not supported` → 恢复库 public schema 无任何表。
- **根因**: `restore_pg.sh` 用 `pg_restore -j 4 < "$BACKUP_FILE"`，但 `-j` 并行需要可 seek 的文件，stdin 不可 seek。
- **修复**: 先 `docker cp` 备份进容器再从文件恢复，保留并行能力。

### 缺陷 2（CRITICAL）：timescaledb hypertable 数据全部丢失
- **现象**: `ERROR: chunk _hyper_1_1_chunk has no dimension slices`；`msg_c2c` 等消息表父表不可查询，chunk 内部 0 行——**源库 2 行消息在恢复库丢失**。
- **根因**: imboy 核心消息表（`msg_c2c`/`msg_c2g`/`msg_s2c`/`msg_store` 等）是 timescaledb hypertable。普通 `pg_restore` 无法恢复 chunk 的 dimension slices（分区切片元数据），导致 hypertable 数据不可访问。对 IM 系统这是致命的——"有备份"实为虚假安全感。
- **修复**: 在 `restore_pg.sh` 中检测 timescaledb，用 `timescaledb_pre_restore()` / `timescaledb_post_restore()` 包裹恢复；该模式禁用并行 `-j`（官方要求串行）。

### 附带修复：bash 3.2 兼容
- macOS 自带 bash 3.2.57，`set -u` 下空数组 `"${arr[@]}"` 报 `unbound variable`。改用 `"${arr[@]+"${arr[@]}"}"` 安全展开，保证脚本在买家任意环境可跑。

---

## 验证结果 / Verification

| 校验项 / Check | 源库 imboy_v1 | 恢复库 imboy_restore_test | 结果 |
|---|---|---|---|
| public 表数 | 111 | 111 | ✅ |
| `user` | 53 | 53 | ✅ |
| `user_friend` | 84 | 84 | ✅ |
| `fts_user` | 53 | 53 | ✅ |
| `msg_c2c`（hypertable）| 2 | 2 | ✅ |
| `msg_s2c`（hypertable）| 18 | 18 | ✅ |
| `msg_store`（hypertable）| 2 | 2 | ✅ |
| hypertable chunk 注册 | — | msg_c2c=1 / msg_s2c=1 / msg_store=1 | ✅ |

> 恢复过程仍有约 106 条 timescaledb chunk 索引 `already exists` 告警，属 pre_restore 模式下的可忽略告警（extension 自建索引与 dump 显式 DDL 重叠），不影响数据完整性。

**结论**: 普通表与 timescaledb hypertable 数据均完整一致，RTO 远低于目标，演练 **PASS**。

---

## 后续建议 / Follow-ups

1. 生产首次启用前，按本演练在生产同构环境复跑一次（数据量更大，复核 RTO）。
2. 开启 PostgreSQL `archive_mode=on` + WAL 归档，使 RPO 从"备份间隔"降到 < 5 min（PITR）。
3. `restore_pg.sh` 的 timescaledb 处理已就绪；`backup-restore.md` 已同步说明恢复对 timescaledb 库为串行 + pre/post_restore。
4. 增补"恢复后启动后端冒烟（登录/发消息）"自动化（当前演练止于数据层校验）。

---

## English Summary

A real backup→restore drill was performed on 2026-06-10 against the local `imboy_v1` DB (111 tables) in Docker container `imboy_pg18` (PostgreSQL 18 + timescaledb). The drill **exposed two latent runtime defects** in scripts that had only ever been syntax-checked:

1. **HIGH** — `pg_restore -j 4 < stdin` is unsupported (parallel restore needs a seekable file). Fixed by `docker cp`-ing the dump into the container and restoring from the file.
2. **CRITICAL** — timescaledb hypertables (the core message tables `msg_c2c`, `msg_store`, etc.) lost all data with `chunk has no dimension slices`. Fixed by wrapping the restore in `timescaledb_pre_restore()` / `timescaledb_post_restore()` (serial only, no `-j`).

Also fixed bash 3.2 empty-array `unbound variable` for portability.

After fixes: table count (111=111), regular tables, and all hypertable row counts match exactly; **RTO = 2 s** (target < 30 min). **Result: PASS.** A backup you have never restored is not a backup — this drill turned a paper promise into a verified, repeatable recovery.
