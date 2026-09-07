# IMBoy 备份与恢复指南

## 备份策略

| 类型 | 频率 | 保留期 | 工具 |
|------|------|--------|------|
| 全量备份 | 每日 03:00 | 30 天 | `scripts/backup_pg.sh --full` |
| Schema 备份 | 每次迁移前 | 永久 | `scripts/backup_pg.sh --schema-only` |
| WAL 归档 | 实时 | 7 天 | PostgreSQL `archive_command` |

### RTO/RPO 目标

| 指标 | 目标 |
|------|------|
| RPO (恢复点目标) | < 5 分钟 (WAL 归档) |
| RTO (恢复时间目标) | < 30 分钟 (全量恢复) |

---

## 执行备份

### 全量备份

```bash
# 手动执行
bash scripts/backup_pg.sh --full

# Cron 定时（每日 03:00）
0 3 * * * cd /opt/imboy && bash scripts/backup_pg.sh --full >> /var/log/imboy-backup.log 2>&1
```

### Schema 备份

```bash
bash scripts/backup_pg.sh --schema-only
```

### 验证备份完整性

```bash
# 列出备份内容
pg_restore --list /path/to/backup.dump | head -20

# 测试恢复到临时库
createdb imboy_test_restore
pg_restore -d imboy_test_restore /path/to/backup.dump
dropdb imboy_test_restore
```

---

## 恢复流程

### 全量恢复

> ⚠️ **timescaledb 关键警告**：imboy 核心消息表（`msg_c2c`/`msg_c2g`/`msg_store` 等）是 timescaledb hypertable。
> 直接 `pg_restore -j 4` 会丢失**全部 hypertable 数据**（报 `chunk ... has no dimension slices`，消息全丢）。
> **请用 `scripts/restore_pg.sh`**——它已内置 timescaledb `pre_restore()`/`post_restore()` 包裹、危险二次确认与恢复后行数校验。详见同目录 [restore-drill-2026-06.md](./restore-drill-2026-06.md)（含真实演练与 2 个已修复缺陷）。
>
> ```bash
> # 先恢复到测试库验证（不影响生产）
> POSTGRES_DB=imboy_v1 bash scripts/restore_pg.sh /path/to/backup.dump --target imboy_restore_test
> # 行数校验无误后，恢复目标库（需输入 yes 二次确认）
> POSTGRES_DB=imboy_v1 bash scripts/restore_pg.sh /path/to/backup.dump --target imboy_v1
> ```

如需手动恢复（仅限非 timescaledb 库，或已用 `timescaledb_pre_restore()` 进入恢复模式）：

```bash
# 1. 停止应用
_rel/imboy/bin/imboy stop

# 2. 创建新数据库（或清空现有）
sudo -u postgres dropdb imboy
sudo -u postgres createdb imboy

# 3. 恢复备份（⚠️ timescaledb 库直接这样做会丢数据，见上方警告）
pg_restore -d imboy -j 4 /path/to/backup.dump

# 4. 验证
sudo -u postgres psql -d imboy -c "SELECT count(*) FROM public.user;"

# 5. 启动应用
_rel/imboy/bin/imboy start
```

### PITR 恢复 (Point-in-Time Recovery)

```bash
# 1. 停止 PostgreSQL
sudo systemctl stop postgresql

# 2. 备份当前数据目录
sudo mv /var/lib/postgresql/18/main /var/lib/postgresql/18/main.bak

# 3. 恢复基础备份
sudo -u postgres pg_basebackup -D /var/lib/postgresql/18/main

# 4. 配置恢复目标
cat >> /var/lib/postgresql/18/main/postgresql.auto.conf <<EOF
recovery_target_time = '2026-04-08 12:00:00+08'
restore_command = 'cp /path/to/wal_archive/%f %p'
EOF

# 5. 创建恢复信号文件
touch /var/lib/postgresql/18/main/recovery.signal

# 6. 启动 PostgreSQL
sudo systemctl start postgresql
```

---

## WAL 归档配置

在 `postgresql.conf` 中添加：

```conf
wal_level = replica
archive_mode = on
archive_command = 'cp %p /path/to/wal_archive/%f'
archive_timeout = 300
```

---

## 灾难恢复演练

建议每季度执行一次恢复演练：

1. 创建测试备份
2. 在隔离环境恢复
3. 验证数据完整性
4. **执行恢复后删除重放（见下节）并验证已删账号仍处于已删除状态**
5. 记录恢复时间
6. 更新 RTO/RPO 指标

---

## 恢复后删除重放（Deletion Replay）— 合规必做

> T-02 要求：恢复演练必须证明**已被删除的数据不会因备份恢复而重新引入**。
> Apple/Google 删除合规（D-04）与各隐私法口径下，账号删除不可因灾备恢复而复活。

### 背景与原理

- 备份是时间点快照（T0）。T0 之后完成的账号删除（D-01..D-03 链）只存在于生产库；
- 用 T0 备份恢复会把 T0 时仍存在的用户数据**带回来**，除非显式重放删除；
- 墓碑表 `user_deletion_job`（一人一行，UNIQUE user_id，用户主行删除后幸存）
  既是审计证据也是重放的驱动源：`status='completed' AND finished_at > 恢复时间点`
  的行即"备份点之后完成的删除"。

### 操作步骤（恢复完成后、对外开放服务前）

1. **确定恢复时间点** `:restore_point`（备份完成时刻的 timestamptz；PITR 即
   `recovery_target_time`）。

2. **对账：找出被复活的目标**（只读，先看规模）：

   ```sql
   -- 墓碑显示已删除，但 user 行因恢复而存在
   SELECT j.user_id, j.account, j.finished_at
   FROM public.user_deletion_job j
   JOIN public."user" u ON u.id = j.user_id
   WHERE j.status = 'completed'
     AND j.finished_at > :'restore_point';
   ```

3. **重置删除任务为 pending**（D-03 编排器会自动重新认领执行，幂等）：

   ```sql
   UPDATE public.user_deletion_job
   SET status = 'pending', attempts = 0, finished_at = NULL, updated_at = NOW()
   WHERE status = 'completed'
     AND finished_at > :'restore_point'
     AND EXISTS (SELECT 1 FROM public."user" u WHERE u.id = user_deletion_job.user_id);
   ```

4. **触发/等待重放**：删除 worker（`user_deletion_logic` 周期清扫）会按
   SKIP LOCKED 逐个认领重放全部删除步骤（含 Garage 附件删除入队、会话吊销、
   墓碑更新）。也可手动触发一轮：

   ```bash
   _rel/imboy/bin/imboy eval 'user_deletion_logic:cleanup_now().'
   ```

5. **验证重放完成**：第 2 步对账查询应返回 **0 行**；抽查若干 user_id 确认
   `public."user"` 无该行、`user_deletion_job.status='completed'` 且
   `finished_at` 已更新为重放时间。

### 注意事项

- 若生产库连同备份**一起丢失**（job 墓碑也丢），删除历史无法从库内重建——
  必须以应用商店/客服的删除请求外部记录为准，人工补建删除任务。
  这是备份策略保留 `user_deletion_job` 全量行（data-disposition: retain）
  的原因，勿对该表做时间清理。
- 恢复演练报告须附：对账查询结果（前后对比）+ 重放完成时间，作为
  「删除不被恢复重新引入」的证据存档（T-02 验收）。
