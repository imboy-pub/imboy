# PG18 镜像蓝绿升级 Runbook

> 适用：prod 环境 `deploy/docker-compose.prod.yml` 的 `imboy_pg18` 服务
> 脚本：`scripts/pg18_bluegreen_upgrade.sh`（一键分阶段，失败自动回滚）
> 设计目标：最小停机（预算 5–12 分钟）、数据零丢失、任一步失败可安全回滚

## 升级原理（物理蓝绿）

```
零停机阶段（prepare）
  全量备份 backup_pg.sh --full + restore_smoke.sh   ← 回滚锚1（异地）
  rsync 在线预拷贝  data/pg18 → data/pg18_new      ← 压小停机窗口

停机窗口（cutover）
  停 backend（停止写入）
  采集基线（行数抽检/hypertable 数）
  优雅停旧 PG（干净关库 = checkpoint）
  冷快照 data/pg18_cold_<ts>.tar                   ← 回滚锚2（秒级回滚点）
  rsync delta（只传变化文件，GB 级库也在分钟内）
  原子换目录：pg18 → pg18_old_<ts> ; pg18_new → pg18   ← 回滚锚3（切后回滚）
  PG_IMAGE=<新镜像> compose up -d imboy_pg18
  立即 ALTER EXTENSION × N（timescaledb 优先！见下"关键教训"）
  硬验证门禁（扩展版本/hypertable 数/行数抽检/timescale launcher）
  起 backend + healthz

切换后（人工观察期）
  稳定 ≥7 天 → cleanup 清理回滚锚（需确认）
  发现问题 → rollback（镜像+目录一起还原，RTO ≈10 分钟）
```

## 关键教训（dev 实证，脚本已内置规避）

1. **切镜像 ≠ 升级完成**：新二进制 + 旧扩展目录 = 半瘫态，`ALTER EXTENSION
   timescaledb UPDATE` 完成前**库内所有查询报错**（`could not access file
   "timescaledb-2.23.1"`）。因此 backend 必须先停、ALTER 必须是容器起来后的
   第一个动作。
2. **catalog 错位模式下普通 SELECT 也报错**：ALTER 前禁止预查询扩展清单，
   脚本按固定顺序容错尝试，`timescaledb` 永远第一个（跑通它解锁后续查询）。
3. **行数门是防 delta 缺文件的最后一道闸**：旧库基线在停 backend 后、停 PG
   前采集，冻结数据上的 count 新旧必须逐表相等。

## 前置条件（T-1 天检查）

```bash
# 在 prod 主机、imboy 仓根执行
bash scripts/pg18_bluegreen_upgrade.sh status   # 盘点现状
docker image inspect imboy/pg18:3.7.0-1         # 新镜像已在本地（save/load 或 pull）
df -h $(dirname data/pg18)                      # 磁盘 ≥ 数据目录 ×2.5
```

## 一键执行

```bash
# 阶段一（零停机，白天可跑）：备份 + 在线预拷贝
bash scripts/pg18_bluegreen_upgrade.sh prepare

# 阶段二（停机窗口，建议低峰）：交互确认后自动执行全链
bash scripts/pg18_bluegreen_upgrade.sh cutover
#   任一步失败 → 自动回滚旧镜像+原目录+backend，打印取证日志路径

# 切换后随时复跑深度验证
bash scripts/pg18_bluegreen_upgrade.sh verify

# 观察期后清理（交互确认；默认建议 ≥7 天）
bash scripts/pg18_bluegreen_upgrade.sh cleanup
```

全自动（无人工确认，CI/熟悉后用）：`--yes`；省磁盘跳过冷快照：`--skip-cold-snapshot`。

## 回滚矩阵

| 故障点 | 现象 | 动作 | 数据损失 |
|---|---|---|---|
| prepare 阶段任何失败 | 脚本报错退出 | 无需动作（旧栈未动），删残留 `data/pg18_new` 重跑 | 0 |
| cutover 窗口内失败 | **自动回滚**，日志打印现场 | 检查日志 `/tmp/pg18_bluegreen_*.log`，修复后重跑 | 0（冻结点之后无写入） |
| 切换后 backend 异常 | **自动回滚**（healthz 门禁在 cutover 内） | 同上 | 0 |
| 切换成功后数日内发现问题 | 手动 `rollback` 子命令 | 目录换回+旧镜像起+backend 起，RTO≈10 分钟 | 切换点后的新写入（届时评估 pg_dump 增量导回） |
| 观察期已 cleanup 后发现问题 | 用锚1：restore_pg.sh 全量恢复 | 按 BACKUP-RESTORE.md 流程 | 备份点之后写入 |

## 本地预演记录（2026-11-19，全链通过）

小靶场（旧镜像 3.6.1-2 种入 2.23.1/3.6.1/4.0.0/1.1 五扩展 + hypertable/普通表/postgis 数据）：
prepare ✓ → cutover ✓（ALTER 五连 2.32.0/3.7.0/3.7.0/4.0.2/1.3，delta 只传 198 文件）→
**真实失败自动回滚 ✓**（ALTER 前预查询踩坑触发）→ 恢复后二次 cutover 全绿 ✓ →
rollback 子命令物理还原 ✓（镜像/数据/扩展版本全还原）→ verify 复跑 ✓ → cleanup ✓。
预演中发现并修复 5 个缺陷：flag 默认值缺失、preflight 误拒预拷贝产物、
ALTER 前预查询必死、die 绕过 ERR 陷阱致半切换遗留、launcher 检查 SIGPIPE 竞态。

## 已知边界

- 仅支持 bind-mount 数据目录（prod 现状）；Docker named volume 场景（dev
  `imboy_pg18_data` 卷）需走卷快照路线，本脚本不适用
- `PG_OLD_IMAGE` 自动探测依赖旧容器还在运行；容器已丢时显式传参
- 行数门默认抽检 `msg_c2c msg_c2s msg_c2g msg_read msg_store user_log`，
  可用 `SMOKE_TABLES` 覆盖；新表需同步补充
