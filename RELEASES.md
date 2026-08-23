# RELEASES

IMBoy 版本历史与升级说明。**升级前必读**：先在下表找到目标版本，阅读对应的
「升级说明」小节，再按其中的步骤操作；跳过备份直接升级属于未受支持的操作。

- 镜像版本策略：compose 镜像引用 pin 与 git tag 强一致的 semver tag（`IMBOY_VERSION`），禁止 `latest`。
- 每个正式版本的 GitHub Release 说明会附 Release Identity 三元组（见下节），用于核验「装的正是被验证过的那一个」。
- 每版发布流程受发布工程计划（Golden Gates）的门禁约束：Golden Install（全新安装门禁）与 Golden Upgrade（升级门禁）双绿才允许打正式 tag 与发布。

## 当前状态

**IMBoy 当前尚无正式 semver 发布。** 首个正式版本将在 Golden Install + Golden Upgrade
双门禁全绿后发布；届时本文件会补齐该版本行与对应的升级说明小节。在此之前的
`1.0.0-alpha.*` / `1.0.0-rc.*` 等版本号均为开发过程中的内部版本，不构成正式发布，
不建议直接用于生产。

## Release Identity（三元组）

每个正式版本由三元组唯一标识。`deploy/install.sh` 安装完成后会原样打印：

```text
IMBOY_VERSION=v1.2.3
IMBOY_GIT_SHA=<构建该版本的 commit sha>
IMBOY_IMAGE_DIGEST=sha256:...
```

升级前或排查问题时，先比对运行环境打印的三元组与 GitHub Release 说明中的三元组，
确认自己运行的正是被发布验证过的那个镜像（而不是重新构建的另一个）。

## 版本历史

| 版本 | 发布日期 | 镜像 digest | GitHub Release | 升级说明 |
|------|----------|-------------|----------------|----------|
| — | — | — | 尚无正式发布，见[「当前状态」](#当前状态) | — |

## 升级说明模板

以下模板供每个正式版本发布时复制填写（发布时替换占位内容并删除本行）。

### 升级到 vX.Y.Z（模板）

**变更摘要**：一句话说明本版本的重点变更。

**兼容性**：与本节相关的破坏性变更、必要的前置操作（如 PostgreSQL 大版本升级、
.env 新增必填变量等）；无则明确写「无破坏性变更」。

#### 1. auto_migrate（数据库迁移行为）

- Docker Compose 部署（社区版 / 商务版）默认 `auto_migrate=true`：新镜像启动时
  自动执行数据库迁移（`imboy_migrate:migrate()`），**无需人工执行迁移命令**。
- 蓝绿部署**必须**关闭自动迁移：`scripts/deploy.sh` 以 `IMBOY_AUTO_MIGRATE=false`
  启动新节点，切流完成后才显式执行 `imboy_ctl db migrate`（迁移时序详见
  [scripts/README.md](./scripts/README.md) 的「蓝绿发布的迁移时序」）。

#### 2. Docker Compose 部署升级

```bash
cd deploy
# 1) 升级前备份（见第 4 节）
# 2) 修改 .env 的 IMBOY_VERSION 为目标版本（单一版本来源，镜像 tag 由它插值）
# 3) 拉新镜像并滚动更新（社区版示例；商务版按 deploy/README.md 叠加 sales-policy overlay）
docker compose -f docker-compose.community.yml pull
docker compose -f docker-compose.community.yml up -d
# 4) 观察迁移与启动
docker compose -f docker-compose.community.yml logs -f imboy_backend
```

日志出现 `started on port 9800` 即就绪；首次启动需等待 PG 健康检查与迁移，约 30-60 秒。

#### 3. 蓝绿升级（零停机）

```bash
# 底层脚本（HTTP 持续可用，旧 WebSocket 在迁移前短暂重连）
bash scripts/deploy.sh <SERVER_HOST> <VSN> <NODE_NAME>
# 或统一入口（all / api / admin / migrate / rollback）
bash scripts/imboy-deploy.sh all
```

用法、环境变量与迁移时序详见 [scripts/README.md](./scripts/README.md)。

#### 4. 数据备份（升级前必做）

```bash
# PostgreSQL 全量备份（pg_dump -Fc，支持并行恢复 + 压缩；输出默认 ./data/backups/pg）
bash scripts/backup_pg.sh

# Garage S3 附件备份（前置：安装 rclone 并配置 Garage 与备份目标两个 remote）
bash scripts/backup_garage.sh
```

恢复与演练：`bash scripts/restore_pg.sh <backup-file.dump>`；可恢复性验证用
`scripts/restore_smoke.sh`。完整手册见
[backup-restore.md](./docs/guides/operations/deployment/backup-restore.md)。

#### 5. 回滚

- **Docker Compose**：把 `.env` 的 `IMBOY_VERSION` 改回上一版本，重跑
  `pull` + `up -d`。注意：升级时 `auto_migrate` 已执行的新数据库迁移**不会自动回滚**，
  降级前必须先核对本次升级是否包含不兼容迁移（见本版「兼容性」小节）。
- **蓝绿**：`bash scripts/deploy.sh --rollback <SERVER_HOST> <VSN> <NODE_NAME>`
  只切回旧色节点，**不回滚数据库迁移**；迁移失败时按
  [scripts/README.md](./scripts/README.md) 的指引核对已应用 schema 后人工处置，
  不能自动切回。

## EOL 政策

IMBoy 尚未发布正式版本，EOL（End of Life）政策**暂不适用**。首个正式的版本支持与
EOL 政策将随 v1.0 发布时一并公布；在此之前，本文件每个版本的升级说明会明确该版本
是否仍被支持。

政策定稿时会参考 Rocket.Chat 的通行实践（仅当前主版本与上一主版本受支持），
以「当前主版本 + 上一主版本」为基线评估，具体范围以 v1.0 发布时公布的版本为准。
