# PG18 镜像蓝绿升级 Runbook

> 适用：已核定实际配置的 `deploy/docker-compose.prod.yml` / `imboy_pg18`；该商务版 Compose 不随仓分发。
> 脚本：`scripts/pg18_bluegreen_upgrade.sh`（分阶段执行，恢复写入前失败自动回滚，恢复写入后失败停写并保留现场）。
> 停机 5–12 分钟、回滚约 10 分钟是待实测预算，不是生产承诺；零丢失仅在所有写入已冻结且尚未恢复的边界内成立。

## 升级原理（物理蓝绿）

```
零停机阶段（prepare）
  单库逻辑备份 backup_pg.sh --full + restore_smoke.sh ← 回滚锚1（异地恢复须独立验证）
  rsync 在线预拷贝  data/pg18 → data/pg18_new      ← 压小停机窗口

停机窗口（cutover）
  停 backend（其他写入方、作业、HA 自动拉起必须事先隔离）
  采集基线（行数抽检/hypertable 数）
  停旧 PG（必须确认正常关库，不能仅凭 stop 返回成功推定）
  冷快照 data/pg18_cold_<TS>.tar                   ← 回滚锚2
  rsync --checksum delta（失败立即中止；扫描成本须按实际数据量测量）
  分两步改名：pg18 → pg18.<TS> ; pg18_new → pg18    ← 回滚锚3（两步整体不具原子性）
  PG_IMAGE=<新镜像> compose up -d imboy_pg18
  立即 ALTER EXTENSION × N（timescaledb 优先！见下"关键教训"）
  硬验证门禁（扩展版本/hypertable 数/行数抽检/timescale launcher）
  原子保存 WRITES_RELEASED=1 → 起 backend → healthz
  此后失败：停 backend + PG，保留两侧数据，转人工恢复

切换后（人工观察期）
  稳定 ≥7 天 → 人工核定恢复锚保留；cleanup 目前仅列清单后拒绝删除
  发现问题 → 保留新写入并评估恢复；接受丢弃切换后写入才允许手动 rollback
```

`pg18_upgrade.state` 通过同目录临时文件再改名发布，记录 `PHASE`、`SWAPPED`、
`WRITES_RELEASED`。原子发布不等于断电耐久或两次数据目录改名的事务；异常中断后须核对
状态和目录，不能仅凭旧状态重跑。回滚任一步失败应停止并保留现场，不报告恢复成功。
`PHASE=rolled_back` 表示旧库恢复及 SQL 就绪已确认，该状态须在重新启动 backend 前保存；
完整回滚成功仍要求 backend 健康检查通过。未处置的升级记录会阻止下一次 prepare/cutover。

## 关键教训（dev 实证，脚本已内置规避）

1. **切镜像 ≠ 升级完成**：新二进制 + 旧扩展目录 = 半瘫态，`ALTER EXTENSION
   timescaledb UPDATE` 完成前**库内所有查询报错**（`could not access file
   "timescaledb-2.23.1"`）。因此 backend 必须先停、ALTER 必须是容器起来后的
   第一个动作。
2. **catalog 错位模式下普通 SELECT 也可能报错**：ALTER 前不查询新库扩展清单，
   `timescaledb` 优先；SQL 失败不能作为“扩展未安装”或“计数为零”跳过。
3. **行数抽检不证明文件完整**：基线在停 backend 后、停 PG 前采集；抽检表行数须相等，
   但不能据此推断未抽检数据完整。冻结后的同步使用 `--checksum`，参见
   [PostgreSQL 18 文件系统备份说明](https://www.postgresql.org/docs/18/backup-file.html)。
4. **backend 已启动即可能写入**：`healthz` 未通过不代表没有写入。写入标记必须先落状态文件；
   标记之后即使启动命令失败也保守转人工处理，不自动切回旧数据。

## 前置条件（T-1 天检查）

已发布的 `imboy/pg18:3.7.0-1` 中 amd64/arm64 镜像配置均为 PostgreSQL `18.7-1.pgdg13+2`；
这不是生产已升级的证据。准备切换时以实测通过的镜像摘要为准，不用浮动标签代替。
在隔离环境用拟部署的同一摘要记录 `SHOW server_version;`、已加载扩展和恢复结果。
依 [PostgreSQL 18.7 发布说明](https://www.postgresql.org/docs/release/18.7/)，确认 C 扩展已按目标版本构建；
只对受影响的索引制定重建方案。若依赖 PostgreSQL 的物理增量备份，升级后做新的全量基线并独立演练恢复；
这与本手册提到的单库逻辑备份是两条不同的路径。

```bash
# 以下是批准升级后的检查示例；本审查未在生产执行
# 事先核定 COMPOSE_FILE、绝对 DATA_DIR、PG_OLD_IMAGE、PG_NEW_IMAGE、BACKEND_SERVICE
bash scripts/pg18_bluegreen_upgrade.sh status   # 盘点现状
docker image inspect "${PG_NEW_IMAGE:?须指定经验证的不可变镜像}"
df -h "${DATA_DIR:?须指定已核定挂载的绝对目录}"
```

- 核对真实 Compose 的服务名、`PG_IMAGE` 生效方式及容器实际 bind mount / `PGDATA`；
  社区版 backend 名为 `imboy_backend`，不能据此假定商务版配置。镜像固定 digest，验证旧/新 PG18 与扩展兼容性。
- 源、目标、回滚目录处于同一文件系统，容量覆盖新副本、冷快照和余量；外部 tablespace/WAL、
  符号链接或额外挂载没有完整隔离与备份方案时禁止执行。确认干净关库及所有写入方、作业、HA 已隔离。
- `backup_pg.sh --full` 是指定库的逻辑备份（不保留 owner/ACL），不是整集群备份；
  `restore_smoke.sh` 在同一 PG 容器临时库恢复并抽检，不证明异地副本或全部数据可恢复。
  异地/加密未配置时现有备份脚本可告警后成功；准入须另有异地取回、解密恢复、
  非空代表数据核验及 globals/config/扩展材料的证据，并保留冷快照与旧目录。
- 在独立非生产环境按目标镜像、实际规模完成升级、故障回滚、恢复失败和放行写入后失败演练；
  记录 checksum 扫描、扩展升级、校验、停机和恢复耗时，确定负责人及新写入对账方案。

## 一键执行

```bash
# 阶段一（零停机，白天可跑）：备份 + 在线预拷贝
bash scripts/pg18_bluegreen_upgrade.sh prepare

# 阶段二（停机窗口，建议低峰）：交互确认后自动执行全链
bash scripts/pg18_bluegreen_upgrade.sh cutover
#   放行写入前失败 → 按停写、停库、恢复目录、旧库就绪、启动 backend 顺序回滚
#   放行写入后失败 → 停写并保留新旧数据；人工核对日志与状态，不自动回退

# 切换后复跑扩展/运行状态验证（不重做旧库行数基线对比）
bash scripts/pg18_bluegreen_upgrade.sh verify

# 只列回滚锚后非零退出；本版禁用自动删除，不能当作清理成功
bash scripts/pg18_bluegreen_upgrade.sh cleanup
```

`--yes` 只跳过普通交互，不构成放弃新写入的确认。人工评估并接受数据损失后，手动回滚
须显式设置 `ALLOW_DATA_LOSS=1`；先保存新数据和恢复计划。生产准入不得依赖
`--skip-backup`、`--skip-cold-snapshot` 或禁用 backend 健康检查来取得通过。

## 回滚矩阵

| 故障点 | 现象 | 动作 | 数据损失 |
|---|---|---|---|
| prepare 阶段失败 | 非零退出，不进入 cutover | 保留 `pg18_new` 和日志，核对故障再决定如何重试 | 未切换 |
| 放行写入前失败 | 自动回滚；恢复失败则停止并报错 | 核对旧镜像、旧目录、状态和 backend；保留失败目录 `pg18.failed_<TS>` | 仅在全部写入已冻结的前提下为 0 |
| backend 启动或 healthz 失败，已标记放行写入 | 停 backend + PG，禁止自动回退旧目录 | 保留当前新目录和 `pg18.<TS>`，人工恢复/对账；停服务失败也需立即人工接管 | 未知，不能声称为 0 |
| 切换成功后发现问题 | 人工评估 `rollback` | 显式 `ALLOW_DATA_LOSS=1` 后确认；先保留新数据，旧库就绪后才能恢复 backend | 直接物理回退会放弃切换后的新写入 |
| 旧目录已由另行批准的人工维护移除 | 从已验证备份恢复 | 按备份恢复流程执行，需完整依赖和恢复材料 | 取决于备份点与可用 WAL；须事先核定 |

## 本地预演记录（2026-11-19，原提交作者报告）

小靶场（旧镜像 3.6.1-2 种入 2.23.1/3.6.1/4.0.0/1.1 五扩展 + hypertable/普通表/postgis 数据）：
prepare ✓ → cutover ✓（ALTER 五连 2.32.0/3.7.0/3.7.0/4.0.2/1.3，delta 只传 198 文件）→
**真实失败自动回滚 ✓**（ALTER 前预查询踩坑触发）→ 恢复后二次 cutover 全绿 ✓ →
rollback 子命令物理还原 ✓（镜像/数据/扩展版本全还原）→ verify 复跑 ✓ → cleanup ✓。
预演中发现并修复 5 个缺陷：flag 默认值缺失、preflight 误拒预拷贝产物、
ALTER 前预查询必死、die 绕过 ERR 陷阱致半切换遗留、launcher 检查 SIGPIPE 竞态。

以上是原提交的历史小靶场记录，不是本补丁的生产验收，也不证明修订后的失败路径已在真实库复验。
本补丁的合成故障测试只使用隔离目录与命令替身，复跑命令：

```bash
bash scripts/test/pg18_bluegreen_upgrade_test.sh scripts/pg18_bluegreen_upgrade.sh
```

真实 PostgreSQL/镜像升级、异地恢复、生产规模停机/回滚、生产执行与数据清理：本次均为 **NOT_RUN**。

## 已知边界

- 仅支持已核定的 bind-mount 数据目录；Docker named volume 场景（dev
  `imboy_pg18_data` 卷）需走卷快照路线，本脚本不适用
- `PG_OLD_IMAGE` 自动探测依赖旧容器还在运行；容器已丢时显式传参
- 行数门默认抽检 `msg_c2c msg_c2s msg_c2g msg_read msg_store user_log`，
  可用 `SMOKE_TABLES` 覆盖；新表需同步补充
- `cleanup` 的原目录匹配及年龄保护不可靠，本版仅列清单并拒绝删除；不因满 7 天自动清理。
- SIGKILL、断电和进程外自动重启不受 Bash EXIT 陷阱保护；须单一操作者、外部停写与人工恢复流程。
