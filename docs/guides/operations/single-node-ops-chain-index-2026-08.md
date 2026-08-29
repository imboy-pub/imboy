# 单节点运维操作链索引：安装 → 备份 → 恢复 → 升级 → 回滚

> **版本 / Version**: 2026-08 v1.0
> **定位**：一页索引，把单节点交付生命周期各环节指到**已有文档与脚本**，不重复其内容；各环节细节以被引文档为准。
> **读者**：交付/运维工程师、PoC 执行人（配套 [PoC 验收矩阵 A8/A9](../../business/poc-single-node-acceptance-matrix-2026-08.md)）。
> **边界**：仅覆盖**单节点 Docker Compose 交付形态**（2026-07-31 拍板，见 [edition-boundary.md](../../business/edition-boundary.md)）；`deploy/helm/` 多副本配置未经生产验证，不在本链内。

---

## 0. 两条路径，先选对（变量名不通用）

| 路径 | 入口 | 适用 | 警告 |
|---|---|---|---|
| **A. Docker Compose（推荐/默认）** | `deploy/install.sh` | 绝大多数部署，含 PoC 单节点 | 配置走 `deploy/.env` |
| **B. 裸机 / release 包** | [day1-quickstart.md](./deployment/day1-quickstart.md) | 无 Docker 约束的环境 | 变量带 `IMBOY_` 前缀、密钥在 `/etc/imboy/keys/`，**与 compose `.env` 字段名和挂载点不通用，照抄会配错** |

---

## 1. 安装（Install）

| 材料 | 位置 | 说明 |
|---|---|---|
| 一键安装脚本 | [deploy/install.sh](../../../deploy/install.sh) | `--edition community\|business`；`--admin-phone/--admin-password` 可无浏览器建超管；装完打印 Release Identity 三元组 |
| 前置检查 | [deploy/preflight.sh](../../../deploy/preflight.sh) | **先填 `.env` 再跑**（缺失时直接退出）；`--edition` 切换检查口径 |
| 部署栈说明 | [deploy/README.md](../../../deploy/README.md) | 交付清单、前置条件（Debian 13 基准 / 8 GB 内存 / Compose v2.23.1+ / 双域名 / 80+443） |
| 完整手册 | [deployment/deployment.md](./deployment/deployment.md) | compose 全量参考 |
| 裸机快速上手 | [deployment/day1-quickstart.md](./deployment/day1-quickstart.md) | 路径 B 入口（5 分钟最小配置） |
| 裸机部署脚本手册 | [deployment/deploy-script.md](./deployment/deploy-script.md) | `scripts/imboy-deploy.sh` 全量/增量部署 |

健康验证口径：`curl -s https://<API_DOMAIN>/api/v1/init` 返回 `{"code":0,...}`；容器状态 `docker compose ps`。

## 2. 备份（Backup）

| 材料 | 位置 | 说明 |
|---|---|---|
| 备份恢复手册 | [deployment/backup-restore.md](./deployment/backup-restore.md) | 策略（每日全量 03:00 / Schema / WAL 归档）、RPO<5min / RTO<30min 目标 |
| PG 全量备份脚本 | [scripts/backup_pg.sh](../../../scripts/backup_pg.sh) | `--full`（pg_dump -Fc）/ `--schema-only` |
| 附件备份脚本 | [scripts/backup_garage.sh](../../../scripts/backup_garage.sh) | rclone 同步 Garage bucket（需预配置 remote） |
| DB 备份（部署栈口径） | [scripts/backup_imboy_db.sh](../../../scripts/backup_imboy_db.sh) | 部署脚本族配套 |

## 3. 恢复（Restore）

| 材料 | 位置 | 说明 |
|---|---|---|
| PG 恢复脚本 | [scripts/restore_pg.sh](../../../scripts/restore_pg.sh) | 含恢复后行数抽样校验（演练验收用） |
| 恢复冒烟 | [scripts/restore_smoke.sh](../../../scripts/restore_smoke.sh) | 恢复后冒烟验证 |
| 恢复演练实录 | [deployment/restore-drill-2026-06.md](./deployment/restore-drill-2026-06.md) | 2026-06 演练记录与耗时参考 |

## 4. 升级（Upgrade）

| 材料 | 位置 | 说明 |
|---|---|---|
| **版本与升级真相源** | [RELEASES.md](../../../RELEASES.md) | 升级前必读：先查目标版本行与「升级说明」小节；镜像 pin semver tag，禁 `latest`；Release Identity 三元组核验 |
| 升级手册 | [upgrade-runbook.md](./upgrade-runbook.md) | rc.1→1.0.0 路径（**草案**，正式版发布后生效；含停机与预估窗口） |
| alpha 线升级 | RELEASES.md「当前状态」节 | 首个正式 semver 未发布前，`1.0.0-alpha.*` 为内部版本，不构成正式发布 |

## 5. 回滚（Rollback）

| 材料 | 位置 | 说明 |
|---|---|---|
| 回滚步骤 | [upgrade-runbook.md](./upgrade-runbook.md) §6 | PITR + 切回旧版本；目标窗口 ≤ 15 分钟 |
| 回滚前置 | 本链 §2 备份 | 任何变更前先有可验证备份（`pg_restore --list` 通过） |

## 6. 支撑环节（链路周边）

| 环节 | 材料 |
|---|---|
| 监控/可观测性 | [observability.md](./observability.md)、[deployment/monitoring.md](./deployment/monitoring.md)（社区版 `--profile monitoring`） |
| 安全基线 | [security.md](./security.md) |
| 依赖清单 | [dependencies.md](./dependencies.md) |
| 故障排查 | deploy/README.md 排错表、[benchmark.md](./benchmark.md)（性能口径） |

---

## 链路速查（PoC D7 运维验证日用）

```
安装   deploy/.env 三变量 → preflight.sh → install.sh → 三元组抄录 → /api/v1/init 探活
备份   backup_pg.sh --full → pg_restore --list 验证 → backup_garage.sh（附件）
恢复   restore_pg.sh（含行数抽样）→ restore_smoke.sh → 抽验消息/附件
升级   RELEASES.md 查版本说明 → 按小节执行 → 三元组复核 → 探活 + 消息冒烟
回滚   upgrade-runbook.md §6 → 恢复到旧版本与数据 → 探活 + 消息冒烟
```

> 缺口登记：截至 2026-08-28，`upgrade-runbook.md` 为 rc.1 草案（正式版未发布），正式 semver 发布后需回填本索引与 RELEASES.md 的对应关系。
