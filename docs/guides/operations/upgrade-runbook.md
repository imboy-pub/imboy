# IMBoy 升级手册：1.0.0-rc.1 → 1.0.0
# IMBoy Upgrade Runbook: 1.0.0-rc.1 → 1.0.0

> **版本 / Version**: 1.0.0 | **最后更新 / Last Updated**: 2026-05-28
> **适用范围 / Scope**: 从 `1.0.0-rc.1` 升级至 `1.0.0` 正式版
> **预计停机时间 / Estimated Downtime**: 冷重启 ≤ 5 分钟；热升级（relup）≤ 30 秒
> **回滚时间 / Rollback Window**: ≤ 15 分钟（PITR + 切回旧 release）

---

## 目录 / Table of Contents

1. [升级前检查 / Pre-upgrade Checks](#1-升级前检查)
2. [升级步骤：数据库 Schema 变更 / DB Schema Changes](#2-数据库-schema-变更)
3. [升级步骤：Erlang Release 部署 / Release Deployment](#3-erlang-release-部署)
4. [配置文件变更 / Config Changes](#4-配置文件变更)
5. [部署后验证 / Post-deploy Verification](#5-部署后验证)
6. [回滚步骤 / Rollback](#6-回滚步骤)
7. [常见问题 / Troubleshooting](#7-常见问题)

---

## 1. 升级前检查
## 1. Pre-upgrade Checks

在执行任何变更操作之前，按顺序完成以下检查。所有检查必须 PASS 才能继续。

Before making any changes, complete the following checks in order. All must PASS to proceed.

### 1.1 数据库备份 / Database Backup

```bash
# 确认 WAL 归档正常运行（需要 pg_archivecleanup 已配置）
# Confirm WAL archiving is running
psql -U imboy_user -d imboy_v1 -c "SELECT pg_walfile_name(pg_current_wal_lsn()), now();"

# 执行迁移前全量备份（写入带时间戳的文件）
# Perform a pre-migration full backup
BACKUP_FILE="imboy_rc1_pre_upgrade_$(date +%Y%m%d_%H%M%S).dump"
pg_dump -U imboy_user -d imboy_v1 -Fc -f "/backups/${BACKUP_FILE}"

# 验证备份可读
# Verify backup integrity
pg_restore --list "/backups/${BACKUP_FILE}" | head -20

echo "Backup saved: /backups/${BACKUP_FILE}"
```

> 备份文件保留至少 30 天。在 backup-restore.md 中记录备份位置和文件名。
> Keep the backup file for at least 30 days. Record its location in backup-restore.md.

### 1.2 节点状态确认 / Node Status Check

```bash
# 检查节点是否在线并可响应
# Check node is online and responsive
make ctl ARGS="node status"

# 确认连接池状态（无泄漏、pool 未饱和）
# Confirm connection pool health
_rel/imboy/bin/imboy remote_console
# 在 console 中执行：
> pooler:status().
# 预期输出类似：{pools, [{pgsql, [{size,80},{available,70},{in_use,10}]}]}

# 检查当前活跃 WebSocket 连接数
> length(syn:members(ws_session_group, node())).

# 退出 console
> q().
```

### 1.3 流量切换 / Traffic Drain

如需零停机热升级，先将流量从升级节点切走：

For zero-downtime hot upgrade, drain traffic from the node being upgraded first:

```bash
# 方式 A：nginx upstream 标记节点不可用（若使用 nginx 负载均衡多节点）
# Option A: Mark the node as unhealthy in nginx upstream (multi-node setup)
# 编辑 nginx 配置注释/下线目标 upstream 节点，校验并 reload:
docker exec imboy_nginx nginx -t
docker exec imboy_nginx nginx -s reload

# 方式 B：单节点部署 — 通知用户维护窗口后直接继续
# Option B: Single-node — announce maintenance window, then proceed
```

### 1.4 版本标记记录 / Version Tag Record

```bash
# 记录当前 release 路径（用于回滚）
# Record current release path for rollback
ls -la _rel/imboy/releases/
# 例如：_rel/imboy/releases/1.0.0-rc.1/

# 记录当前 git commit（若从源码构建）
# Record current git commit
git -C . rev-parse HEAD
```

---

## 2. 数据库 Schema 变更
## 2. Database Schema Changes

### 2.1 rc.1 → 1.0.0 迁移文件

> **迁移系统已重构**：自提交 `8634ddf` 起，迁移文件从 `00000XXX_name.sql` 单文件格式
> 迁移为 `XXXXXX_name.up.sql` / `XXXXXX_name.down.sql` 双文件格式，并重新编号（6位）。
>
> **Migration system refactored**: Since commit `8634ddf`, migration files use the new
> `XXXXXX_name.up.sql` / `XXXXXX_name.down.sql` format with 6-digit numbering.

rc.1 → 1.0.0 期间**新增了 2 个迁移**，将在应用启动时自动执行：

There are **2 new migrations** between rc.1 and 1.0.0, applied automatically on startup:

| 迁移文件 | 说明 | 影响范围 |
|---------|------|---------|
| `000065_schema_fixes.up.sql` | 综合 schema 修复：boolean 列类型修正、json→jsonb、auto updated_at 触发器、FTS 触发器、缺失 FK 索引、varchar→text | 多个表，45KB，变更广泛 |
| `000066_attachment_cleanup_index.up.sql` | 附件清理复合索引（`referer_time, created_at`） | `attachment` 表 |

以下是 rc.1 阶段已包含的关键历史迁移，确认它们在目标数据库已执行：

The following are key historical migrations from rc.1 — verify they have been applied:

```sql
-- 检查已执行的迁移版本（erlang_migrate 记录表）
-- Check applied migration versions
SELECT version, applied_at
FROM schema_migrations
ORDER BY version DESC
LIMIT 10;
```

| 新格式迁移文件（现用） | 对应旧文件（已删除） | 说明 | 状态检查 |
|---|---|------|---------|
| `000010_tsid_migration.up.sql` | `00000080_tsid_migration.sql` | TSID 全量迁移（BIGSERIAL → BIGINT） | `SELECT id FROM users LIMIT 1;` 类型应为 bigint |
| `000064_hypertable_timeseries.up.sql` | `00000083_hypertable_timeseries.sql` | TimescaleDB hypertable 覆盖审计日志 | `SELECT * FROM timescaledb_information.hypertables;` |
| `000065_schema_fixes.up.sql` | `00000084_conversation_varchar_to_bigint.sql`（合并） | conversation.id varchar→bigint 及综合修复 | `\d conversation` |
| `000057_e2ee.up.sql` | `00000088_uuid_v7_type.sql`（合并） | e2ee_transfer_sessions.session_id → uuid | `\d e2ee_transfer_sessions` |

> **⚠️ DBA 角色分离**：旧的 `00000085_role_separation.sql` 已从迁移目录移除（该文件原标记为"DBA 手动执行"，
> 不纳入自动迁移）。如尚未在生产数据库中创建 `imboy_app` / `imboy_admin` / `imboy_readonly` 角色，
> 需由 DBA 在升级前参照 [security.md](security.md) 手动执行角色分离脚本：
>
> **⚠️ DB Role Separation**: The old `00000085_role_separation.sql` has been removed from the
> migrations directory (it was always marked manual-DBA-only). If the roles `imboy_app` /
> `imboy_admin` / `imboy_readonly` have not been created in the target database, a DBA must
> run the role separation script manually before upgrading. See [security.md](security.md).
>
> ```sql
> \du imboy_app imboy_admin imboy_readonly
> ```

### 2.2 迁移自动执行（应用启动时）/ Auto-migration on Startup

imboy 使用 `erlang_migrate` 库（`imboy_migrate:migrate/0`）在应用启动时自动扫描并执行
`priv/migrations/` 目录中未执行的 `.up.sql` 文件。

imboy uses `erlang_migrate` (`imboy_migrate:migrate/0`) to automatically execute unapplied
`.up.sql` files from `priv/migrations/` at startup.

**迁移文件格式 / Migration file format**：
- `XXXXXX_name.up.sql` — 正向迁移 / forward migration
- `XXXXXX_name.down.sql` — 回滚脚本 / rollback script

```bash
# 确认迁移路径配置（sys.config 中）：
# {scripts_path, "$PROJECT_DIR/migrations"}

# 部署后在日志中确认迁移执行：
grep -i "\[imboy_migrate\]\|migration" _rel/imboy/log/debug.log | tail -20
```

---

## 3. Erlang Release 部署
## 3. Erlang Release Deployment

### 3.1 方式选择 / Deployment Method

| 方式 | 适用场景 | 停机时间 |
|------|---------|---------|
| **冷重启（推荐）** | 单节点、首次升级、存在代码结构变更 | ≤ 5 分钟 |
| **热升级（relup）** | 多节点滚动、无 API 变更 | ≤ 30 秒/节点 |
| **Docker 镜像替换** | 容器化部署（`docker-compose.prod.yml`） | ≤ 2 分钟 |

### 3.2 冷重启升级（推荐）/ Cold Restart (Recommended)

```bash
# ---- 步骤 1：构建新 Release ----
# ---- Step 1: Build new release ----
cd /opt/imboy
git pull origin main          # 拉取 1.0.0 代码
make compile
IMBOYENV=pro make rel         # 生成 _rel/imboy/

# 确认版本号
cat _rel/imboy/releases/*/version
# 预期输出: 1.0.0

# ---- 步骤 2：停止当前节点 ----
# ---- Step 2: Stop current node ----
_rel/imboy/bin/imboy stop
# 等待节点完全停止（最多 30 秒）
sleep 5
ps aux | grep beam | grep -v grep    # 确认进程已退出

# ---- 步骤 3：备份旧 Release（保留回滚能力）----
# ---- Step 3: Keep old release for rollback ----
# 旧的 _rel 目录已在 step 1 git pull 前备份，或者手动保留：
cp -r _rel/imboy/releases/1.0.0-rc.1 /backups/release_rc1_$(date +%Y%m%d)

# ---- 步骤 4：启动新节点 ----
# ---- Step 4: Start new node ----
IMBOYENV=pro _rel/imboy/bin/imboy start

# 等待节点就绪（DB 迁移可能需要额外时间）
sleep 10
_rel/imboy/bin/imboy ping    # 预期输出: pong
```

### 3.3 热升级（relup）/ Hot Upgrade (relup)

> 仅当 rc.1 和 1.0.0 之间没有 gen_server state 格式变更、没有 ETS 表结构变更时适用。
> Only applicable when there are no gen_server state format changes or ETS schema changes between rc.1 and 1.0.0.

```bash
# ---- 前置：确认 appup/relup 文件存在 ----
ls _rel/imboy/releases/1.0.0/relup

# ---- 步骤 1：复制新版本到 releases 目录 ----
cp imboy-1.0.0.tar.gz _rel/imboy/releases/1.0.0/

# ---- 步骤 2：执行热升级 ----
_rel/imboy/bin/imboy upgrade 1.0.0

# ---- 步骤 3：确认升级成功 ----
_rel/imboy/bin/imboy remote_console
> release_handler:which_releases().
# 预期: [{<<"imboy">>,<<"1.0.0">>,[...],current}]
> q().
```

### 3.4 Docker 容器镜像替换 / Docker Image Replacement

```bash
# ---- 步骤 1：更新 .env 中的版本号 ----
# ---- Step 1: Update version in .env ----
cd /opt/imboy/deploy
sed -i 's/IMBOY_VERSION=1.0.0-rc.1/IMBOY_VERSION=1.0.0/' .env

# ---- 步骤 2：拉取新镜像并重建容器 ----
# ---- Step 2: Pull new image and recreate container ----
docker compose -f docker-compose.prod.yml pull imboy_backend
docker compose -f docker-compose.prod.yml up -d --no-deps imboy_backend

# ---- 步骤 3：等待健康检查通过 ----
# ---- Step 3: Wait for health check ----
watch -n 2 'docker inspect imboy_backend --format "{{.State.Health.Status}}"'
# 等待输出变为 "healthy"（约 30-60 秒）
```

---

## 4. 配置文件变更
## 4. Config File Changes

### 4.1 sys.config 关键变更点 / Key Changes in sys.config

rc.1 → 1.0.0 在 `config/sys.config`（或 `config/sys.pro.config`）中需要关注以下变更：

| 配置项 | rc.1 默认值 | 1.0.0 建议值 | 说明 |
|--------|------------|-------------|------|
| `api_auth_switch` | `<<"off">>` | `<<"on">>` | 生产环境**必须**开启 API 签名验证 |
| `msg_archive_enabled` | `true` | `true` | 消息永久存储（conv_seq 方案 B），**默认已开启**，无需修改 |
| `tsid_dc_id` | `1` | 按数据中心编号 | 多节点/多DC 时每节点必须唯一 |
| `tsid_node_id` | `1` | 按节点编号 | 多节点时每节点必须唯一（0–127） |
| `dsync_enabled` | `true` | `true` | 分布式缓存同步，单节点也保持 true |
| `verification_master_code` | `<<"6666">>` | `<<>>` | 生产环境**必须**清空此万能验证码 |
| `garage.endpoint` | `<<"http://127.0.0.1:3900">>` | Garage 服务地址 | **1.0.0 新增**，附件对象存储后端 |
| `garage.bucket` | `<<"imboy">>` | 按实际 bucket 名 | **1.0.0 新增** |
| `garage.access_key` | `<<"GKxxxx">>` | 真实 Access Key | **1.0.0 新增**，生产**必须**替换占位符 |
| `garage.secret_key` | `<<"xxxx...">>` | 真实 Secret Key | **1.0.0 新增**，生产**必须**替换占位符 |

### 4.2 必须通过环境变量注入的凭据 / Secrets via Environment Variables

以下项在 `sys.config` 中默认留空，**生产环境必须通过 `IMBOY_*` 环境变量注入**（`imboy_env.erl` 在启动时统一覆盖）：

```bash
# 生成并设置（示例）
export IMBOY_JWT_KEY=$(openssl rand -base64 32)
export IMBOY_POSTGRE_AES_KEY=$(openssl rand -base64 32)
export IMBOY_SOLIDIFIED_KEY=$(openssl rand -base64 32)
export IMBOY_SOLIDIFIED_KEY_IV=$(openssl rand -base64 16)
export IMBOY_ADM_COOKIE_SECRET=$(openssl rand -hex 32)
export IMBOY_PASSWORD_SALT="<rc.1 中使用的相同盐值 — 不可更改>"
export IMBOY_LOGIN_RSA_PUB_KEY_FILE=/etc/imboy/keys/login_rsa_pub.pem
export IMBOY_LOGIN_RSA_PRIV_KEY_FILE=/etc/imboy/keys/login_rsa_priv.pem
export IMBOY_PG_HOST=127.0.0.1
export IMBOY_PG_PORT=5432
export IMBOY_PG_DATABASE=imboy_v1
export IMBOY_PG_USERNAME=imboy_app
export IMBOY_PG_PASSWORD=<你的数据库密码>
# 1.0.0 新增 — Garage S3 对象存储凭证 / 1.0.0 New — Garage S3 credentials
export IMBOY_GARAGE_ENDPOINT=https://s3.your-garage-host.com
export IMBOY_GARAGE_BUCKET=imboy-prod
export IMBOY_GARAGE_ACCESS_KEY=<Garage Access Key>
export IMBOY_GARAGE_SECRET_KEY=<Garage Secret Key>
```

> **重要 / Important**: `IMBOY_PASSWORD_SALT` 一旦投产后**不可更改**，否则已存储的旧格式密码永久失效。
> `IMBOY_PASSWORD_SALT` **must not change** after first production deployment — changing it will permanently invalidate stored legacy-format passwords.

### 4.3 Docker 部署的 .env 变更 / .env Changes for Docker Deployment

```bash
# 对比 .env.example 与当前 .env，确认新增变量
diff deploy/.env.example deploy/.env

# rc.1 → 1.0.0 新增的 .env 变量（若有）：
# IMBOY_VERSION=1.0.0              ← 版本号更新
# SENTRY_DSN=https://...@sentry.io/xxx  ← Sentry 生产 DSN（可选）
```

---

## 5. 部署后验证
## 5. Post-deploy Verification

### 5.1 节点启动验证 / Node Startup Check

```bash
# 节点存活
_rel/imboy/bin/imboy ping          # 预期: pong

# CLI 节点状态
make ctl ARGS="node status"

# 数据库连接
make ctl ARGS="db ping"            # 预期: DB OK
```

### 5.2 Smoke Test（冒烟测试）

```bash
# Tier-0 完整冒烟（C2C 消息 + WebSocket + CLI）
make smoke

# 或分步执行：
make smoke-c2c   # C2C 单聊消息流
make smoke-ws    # WebSocket 连接 + ACK
make smoke-ctl   # CLI 工具链验证

# 功能 Feature Flag 冒烟（指定部署地址）
make feature-smoke \
  FEATURE_SMOKE_BASE_URL=https://api.imboy.pub \
  FEATURE_SMOKE_EXPECTS='core=true moment=true channel=true'
```

### 5.3 关键 API 端点检查 / API Endpoint Check

```bash
BASE=https://api.imboy.pub

# 健康检查
curl -s "${BASE}/health" | jq .

# 应用初始化 — 1.0.0 字段变化：
#   - upload_key / upload_scene 现在始终返回 "" (旧 go-fastdfs 字段已废弃)
#   - 新增 attach_presign_endpoint: "/api/v1/attachment/presign"（Garage S3 直传入口）
curl -s "${BASE}/api/v1/app/init" | jq '{ws_url, attach_presign_endpoint, upload_key}'
# 预期: attach_presign_endpoint = "/api/v1/attachment/presign", upload_key = ""

# Presign 接口可用性（需有效 JWT）
# curl -s -H "Authorization: Bearer <token>" "${BASE}/api/v1/attachment/presign?filename=test.png" | jq .

# Feature flags
curl -s "${BASE}/api/v1/app/features" | jq .
```

### 5.4 观测指标验证 / Metrics Verification

```bash
# Prometheus 指标端点（内网）
curl -s http://localhost:9090/metrics | grep -E "imboy_(active_connections|msg_delivered|error_rate)"

# Grafana 仪表盘检查（手动）：
# 打开 http://<grafana_host>:3000/d/imboy-overview
# 确认：
#   - active_ws_connections > 0（有连接）
#   - error_rate_5xx < 0.01（错误率低）
#   - pg_pool_available > 10（连接池有余量）
```

### 5.5 日志错误扫描 / Log Error Scan

```bash
# 启动后 5 分钟内检查错误日志
tail -f _rel/imboy/log/error.log

# 无 CRITICAL/ERROR 级别日志为通过
# grep 关键错误模式
grep -E "CRITICAL|crash|init_failed|db_connect_failed" _rel/imboy/log/debug.log | tail -20
```

---

## 6. 回滚步骤
## 6. Rollback Steps

### 6.1 判断是否需要回滚 / When to Rollback

满足以下任一条件时立即触发回滚：
Trigger rollback if any of the following occurs:

- 节点启动后 `ping` 超过 60 秒无响应
- 数据库连接池全部耗尽（`pooler:status()` 显示 `in_use` = 上限）
- 错误日志出现 `crash` 或 `init_failed`
- Smoke test 失败超过 2 个用例
- Grafana 5xx 错误率超过 1%

### 6.2 数据库 PITR 回滚 / Database PITR Rollback

```bash
# ---- 步骤 1：确定回滚目标时间点（rc.1 备份时刻）----
# ---- Step 1: Identify PITR target timestamp ----
# 使用升级前备份的时间戳
PITR_TARGET="2026-05-27 10:30:00"  # 替换为实际备份时间

# ---- 步骤 2：停止应用 ----
_rel/imboy/bin/imboy stop
docker compose -f deploy/docker-compose.prod.yml stop imboy_backend

# ---- 步骤 3：PITR 恢复 ----
# 参考 docs/guides/operations/deployment/backup-restore.md 中的 PITR 章节
# 以下为快速参考：

# 停止 PostgreSQL
docker compose -f deploy/docker-compose.prod.yml stop imboy_pg18

# 替换数据目录为 PITR 快照（从 WAL 归档恢复）
# 注意：此操作会丢失 PITR_TARGET 之后的所有数据
pg_restore -d imboy_v1 "/backups/${BACKUP_FILE}"

# 或使用 WAL 回放到指定时间点：
# recovery.conf / postgresql.conf 追加:
# restore_command = 'cp /wal_archive/%f %p'
# recovery_target_time = '2026-05-27 10:30:00'
# recovery_target_action = 'promote'

# ---- 步骤 4：重启数据库 ----
docker compose -f deploy/docker-compose.prod.yml start imboy_pg18

# ---- 步骤 5：验证数据库 ----
make ctl ARGS="db ping"
```

### 6.3 切回旧版本 Release / Switch to Old Release

```bash
# 方式 A：冷重启切回（推荐）
# Option A: Cold restart rollback

# 确认旧版本 release 仍存在
ls _rel/imboy/releases/1.0.0-rc.1/

# 切换到旧版本 sys.config（确保 IMBOY_* 环境变量与 rc.1 一致）
IMBOYENV=pro _rel/imboy/bin/imboy start 1.0.0-rc.1

# 方式 B：热降级（仅当热升级路径干净时）
# Option B: Hot downgrade (only if hot upgrade was used and relup is clean)
_rel/imboy/bin/imboy downgrade 1.0.0-rc.1

# 方式 C：Docker 镜像回滚
# Option C: Docker rollback
cd deploy
sed -i 's/IMBOY_VERSION=1.0.0/IMBOY_VERSION=1.0.0-rc.1/' .env
docker compose -f docker-compose.prod.yml up -d --no-deps imboy_backend
```

### 6.4 回滚后验证 / Post-rollback Verification

```bash
# 确认版本
_rel/imboy/bin/imboy remote_console
> application:get_key(imboy, vsn).
# 预期: {ok, "1.0.0-rc.1"}

# 执行 smoke test
make smoke
```

---

## 7. 常见问题
## 7. Troubleshooting

### 7.1 端口冲突 / Port Conflict

**症状 / Symptom**: 节点启动失败，日志出现 `{error,{listen,{eaddrinuse,{...9800...}}}}`

```bash
# 查找占用 9800 端口的进程
lsof -i :9800
# 或
ss -tlnp | grep 9800

# 若是旧版本残留进程（BEAM），强制终止
kill -9 <PID>

# 验证端口释放
sleep 3 && lsof -i :9800   # 无输出则端口已释放

# 重新启动
_rel/imboy/bin/imboy start
```

### 7.2 连接池耗尽 / Connection Pool Exhaustion

**症状 / Symptom**: API 返回 503 或超时；日志出现 `{error, pool_exhausted}`

```bash
# 检查连接池状态
_rel/imboy/bin/imboy remote_console
> pooler:status().
# {pools, [{pgsql, [{size,80},{available,0},{in_use,80}]}]}  ← 耗尽

# 应急：临时增加连接池大小（不重启，运行时调整）
# sys.config 中默认 max_count: 80，可临时扩容：
> application:set_env(imboy, pg_conf, #{name => pgsql, max_count => 120, init_count => 5, start_mfa => ...}).
# 注意：此方式仅影响运行时，重启后恢复 sys.config 值

# 根本原因排查：
# 1. 检查是否有长事务
> psql -U imboy_user -d imboy_v1 -c "SELECT pid, now()-xact_start AS duration, query FROM pg_stat_activity WHERE state='active' ORDER BY duration DESC LIMIT 10;"

# 2. 终止长事务（超过 5 分钟）
> psql -U imboy_user -d imboy_v1 -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE state='active' AND now()-xact_start > interval '5 minutes';"
```

**永久修复**：在 `sys.config` 中增大 `max_count`，或排查上层业务是否有连接泄漏。

### 7.3 节点启动失败 / Node Startup Failure

**症状 / Symptom**: `_rel/imboy/bin/imboy start` 后节点无响应，`ping` 返回 `pong` 失败

```bash
# 查看启动日志
tail -100 _rel/imboy/log/error.log
tail -100 _rel/imboy/log/debug.log

# 常见原因 1：缺少必要环境变量（fail-fast 检查）
# imboy_app.erl: validate_runtime_config/0 会在启动时校验
grep "validate_runtime_config\|missing_required\|fail-fast" _rel/imboy/log/error.log

# 补齐环境变量后重试
export IMBOY_JWT_KEY=...
export IMBOY_POSTGRE_AES_KEY=...
_rel/imboy/bin/imboy start

# 常见原因 2：数据库不可达
make ctl ARGS="db ping"
# 若失败，检查 PG 容器是否运行
docker ps | grep imboy_pg18
docker inspect imboy_pg18 --format "{{.State.Health.Status}}"

# 常见原因 3：迁移执行失败（SQL 语法错误 / 权限不足）
grep -i "migration.*error\|pure_migrations.*error" _rel/imboy/log/error.log
# 手动执行失败的迁移文件：
psql -U imboy_user -d imboy_v1 -f priv/migrations/00000XXX_xxx.sql

# 常见原因 4：erlang cookie 不一致（集群场景）
# 确认 ~/.erlang.cookie 与节点启动参数 -setcookie 一致
cat ~/.erlang.cookie
```

### 7.4 升级后客户端无法连接 / Clients Cannot Connect After Upgrade

**症状 / Symptom**: App 显示"连接失败"，WebSocket 握手被拒绝

```bash
# 1. 检查 nginx 反向代理是否正常
docker logs imboy_nginx --tail 50

# 2. 确认 WebSocket 端点可达
curl -v --include \
  --no-buffer \
  --header "Connection: Upgrade" \
  --header "Upgrade: websocket" \
  --header "Host: api.imboy.pub" \
  --header "Origin: https://api.imboy.pub" \
  --header "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
  --header "Sec-WebSocket-Version: 13" \
  https://api.imboy.pub/ws

# 3. 检查 JWT_KEY 是否与客户端持有的 token 对应
# 如果 JWT_KEY 变更，所有客户端 token 立即失效，需要重新登录
# 日志中查找：
grep "jwt.*invalid\|token.*expired\|401" _rel/imboy/log/debug.log | tail -20
```

### 7.5 迁移执行超时 / Migration Timeout

**症状 / Symptom**: 含大表的 migration（如 `000010_tsid_migration.up.sql`、`000065_schema_fixes.up.sql`）在高并发下锁等待超时

```bash
# 检查数据库锁等待
psql -U imboy_user -d imboy_v1 -c "
  SELECT pid, wait_event_type, wait_event, query
  FROM pg_stat_activity
  WHERE wait_event_type = 'Lock';
"

# 若有锁等待，找到阻塞方并评估是否终止
psql -U imboy_user -d imboy_v1 -c "
  SELECT pid, pg_blocking_pids(pid) AS blocked_by, query
  FROM pg_stat_activity
  WHERE cardinality(pg_blocking_pids(pid)) > 0;
"

# 在非峰值时段重试迁移（先降流量再执行）
```

---

## 附录：升级检查清单 / Appendix: Upgrade Checklist

操作员在执行升级时请逐项打勾。

```
升级前 / Pre-upgrade:
[ ] 数据库全量备份已完成，文件名已记录
[ ] WAL 归档运行正常（pg_walfile_name 返回有效 LSN）
[ ] 连接池状态正常（available > 20）
[ ] 旧版本 release 路径已备份记录
[ ] 流量已切走（多节点）或维护窗口已通知（单节点）
[ ] 所有 IMBOY_* 必需环境变量已准备（含 IMBOY_GARAGE_* 四项）
[ ] Garage S3 服务可达（curl ${IMBOY_GARAGE_ENDPOINT}/health 或等效检查）

升级中 / During upgrade:
[ ] 新 release 编译通过（make compile 无错误）
[ ] 版本号确认为 1.0.0
[ ] 节点停止成功（BEAM 进程已退出）
[ ] 新节点启动成功（ping 返回 pong）
[ ] 迁移日志无错误（000065/000066 迁移成功执行）

升级后 / Post-upgrade:
[ ] make ctl ARGS="node status" 通过
[ ] make ctl ARGS="db ping" 通过
[ ] make smoke 全部通过
[ ] /api/v1/app/init 返回 attach_presign_endpoint 字段（Garage 直传入口）
[ ] 附件上传端到端测试（Flutter presign → PUT Garage → 可访问）
[ ] Grafana 5xx 错误率 < 0.1%
[ ] 连接池 available > 20
[ ] 无 CRITICAL/ERROR 级别日志（启动后 5 分钟内）
[ ] 客户端可正常登录并收发消息
```
