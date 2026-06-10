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
> **请用 `scripts/restore_pg.sh`**——它已内置 timescaledb `pre_restore()`/`post_restore()` 包裹、危险二次确认与恢复后行数校验。详见同目录 [RESTORE-DRILL-2026-06.md](./RESTORE-DRILL-2026-06.md)（含真实演练与 2 个已修复缺陷）。
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
4. 记录恢复时间
5. 更新 RTO/RPO 指标
