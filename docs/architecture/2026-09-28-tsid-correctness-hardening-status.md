# TSID 正确性加固计划 — 状态附录

> 计划文档：`2026-09-28-tsid-correctness-hardening-implementation-plan.md`
> **（不入仓**，勿按文件名在仓内检索：SHA-256 `92eef9736...` 锚定；计划原文与
> `.sha256` 由执行环境持有、保持入仓前状态，本附录为独立文件不修改计划原文）
> 附录更新：2026-09-29（TSID-11）
> 附录更新：2026-09-29（TSID-11）

## 1. 执行概况

- 执行基线：`BASE_SHA_EXECUTED = 984d40e6869fe5c774fa016a96b13d34e1c1c339`
- 分支：`task/tsid-correctness-hardening`（独立 worktree，未 push）
- 卡状态：TSID-00..TSID-10 全部 VERIFIED_PASS；TSID-11 于本附录提交时完成
- Gate（TSID-G0..G7）：candidate 冻结后统一执行
- **RELEASE=NO_GO**：无论本地结果如何，本轮始终 NO_GO
  （未执行任何外部/生产验证：无 push、无 PR、无部署、未访问生产数据库；
  EXT-04/05 目标 PVC/存储类 crash 矩阵与双实例锁仅在本机 APFS 近似环境验证）

## 2. 卡状态与提交清单

| 卡 | 状态 | 提交 |
|---|---|---|
| TSID-00 基线冻结/盘点/ADR 裁决 | VERIFIED_PASS | （无源码提交） |
| TSID-01 测试 seam 与契约冻结 | VERIFIED_PASS | 82b09683 |
| TSID-02 63-bit/时间/calendar/输入校验 | VERIFIED_PASS | 1e50a648 |
| TSID-03 单全局 cursor 与注册竞态 | VERIFIED_PASS | fef5ddd9 |
| TSID-04 batch reservation 与有界逻辑时间 | VERIFIED_PASS | a6cb0566 |
| TSID-05 双槽 durable future fence store | VERIFIED_PASS | b23a5b19 |
| TSID-06 lifetime lock/guard/健康状态 | VERIFIED_PASS | 8bc8147a |
| TSID-07 配置/持久卷/部署安全 | VERIFIED_PASS | b485757e |
| TSID-08 cutover 高水位/bootstrap/回滚 | VERIFIED_PASS | 949b5bea |
| TSID-09 完整正确性/crash/fuzz | VERIFIED_PASS | 1627cf65 |
| TSID-10 基准/soak/参数定标 | VERIFIED_PASS | 6aaf5bdf |
| TSID-11 文档/独立 review/冻结 | VERIFIED_PASS | （本卡提交，见 candidate-manifest） |

关键设计裁决与结果摘要：

- **ADR-0004**：裁决 DEFERRED_BY_TSID_HARDENING；本卡在 ADR-0004 顶部标注
  Deferred 并更新过时背景（见该文件 2026-09-29 更新块）。
- **全局唯一**：所有命名生成器共享单一全局 cursor，label 不再分配独立数值空间。
- **有界逻辑时间**：`max_logical_lead_ms` 缺省由 TSID-10 定标为 512ms
  （1M-id 突发需借支 ~392ms，余量 31%；lead 先于 fence window 1000ms 绑定，
  崩溃烧槽由 fence window 承担）。详见 `BENCH/parameter-decision.md`（证据目录）。
- **基准结果**（同机同 OTP 对比基线 9,888,263 ids/s）：
  - 单 ID：median 10,462,987 = 基线 105.8%（Gate ≥90%）
  - batch 1k/10k：11.0x / 10.8x（Gate ≥2x），CAS 每调用 1 次
  - 微批 p99 = 1μs；guarded 1M-id 突发 fence_persists = 0
  - 30min soak：73,502,000 ids、零错误、lead 不变量成立、RSS 非持续线性
- **正确性**：66 + guard/store/env 套件全绿；fuzz 2 万案例 0 未捕获；
  100 次 kill -9 crash/restart 全唯一、fence 未穿越。

## 3. 外部未验证项（EXT）

| 项 | 状态 | 说明 |
|---|---|---|
| EXT-04 目标 PVC/存储类 crash 矩阵 | BLOCKED（外部） | 本机 APFS 仅为近似环境；双槽 store 的真实 NFS/PVC 故障语义未在目标存储类上验证 |
| EXT-05 同机双实例锁（目标环境） | BLOCKED（外部） | flock/registry provider 在容器双 BEAM 已验证；目标 K8s 节点拓扑未验证 |
| push / PR / 部署 / 生产迁移 | 未授权 | 本轮授权边界明确排除；生产高水位扫描（tsid_scanner）未对生产库执行 |

## 4. 证据

全部证据位于执行环境 `$EVIDENCE_ROOT`（BASELINE / CARDS / TESTS / BENCH / CRASH / FINAL），
每卡 `CARDS/TSID-<NN>/RESULT.json` 含验收映射、verify 明细、findings 与证据 SHA-256；
最终 manifest 见 `FINAL/`（G-Gate 生成）。
