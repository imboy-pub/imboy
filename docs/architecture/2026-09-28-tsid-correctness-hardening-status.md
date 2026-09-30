# TSID 正确性加固计划 — 状态附录

> 计划文档：`2026-09-28-tsid-correctness-hardening-implementation-plan.md`
> **（不入仓**，勿按文件名在仓内检索：SHA-256 `92eef9736...` 锚定；计划原文与
> `.sha256` 由执行环境持有、保持入仓前状态，本附录为独立文件不修改计划原文）
> 附录更新：2026-09-29（TSID-11）
> 附录更新：2026-09-29（catalog v1 / 自举状态机合同 / 主键治理后续事项）

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

## 5. 后续事项（独立登记）：主键生成方式治理 + catalog 递减

- **状态**：Open（未排期）。本节仅登记目标、路径与约束，不构成本轮交付。
- **治理目标**（用户裁决原话语义）：TSID 只用于**需要跨数据中心/跨地域分布式同步**
  的实体表主键（用户 ID、群 ID、频道 ID 这一类）；不做分布式同步的实体不应使用 TSID。
- **批次化路径**（每批固定七步）：
  1. 选中一批非同步实体；
  2. 主键迁移为数据库本地生成（bigserial / uuid），含历史数据回填策略；
  3. 调用点改造；
  4. catalog 移除对应列；
  5. catalog version+1（digest 随 version 与清单联动变化）；
  6. digest 重绑：既有割接 manifest 的 `catalog_digest` 与新 digest 不符属预期
     FAIL（`catalog_changed`），按 runbook 重新割接/绑定；
  7. 全量 Gate 重跑。
- **约束**：
  - catalog 收缩只能发生在治理迁移完成之后，绝不能先于它（论证见 §6）；
  - 每次递减必须同步更新 manifest 绑定与证据。

## 6. v1 catalog（现状口径）

- **范围**：全部运行时 TSID 主键列（104 个；数字与清单
  **以 `elib_tsid_catalog:primary_keys()` 为准**——数据核对回填已完成，`digest()`
  锚定当前 104 表清单，割接 manifest 绑定该 digest）。
- **为何取现状口径**（撞号风险论证）：
  1. 第一次 cutover 前，凡可能已写入历史 TSID 的主键列必须全部纳入扫描保护；
  2. 任何漏扫列中的历史 TSID 都会在重启自举（auto_scan 取 max(id)+1）时被新 ID 撞号；
  3. 因此 v1 宁全勿漏：catalog 收缩只能发生在治理迁移（§5）完成之后。
- **数据来源**：call-sites × migrations DDL 静态扫描生成，人工逐条核对。
- **绑定机制**：`digest()`（对 {version, 排序后清单} 的 SHA-256）→ 写入割接 manifest
  的 `catalog_digest` 字段（`elib_tsid_bootstrap`，魔数 IMBTSIDB1）→ 自举校验不符即
  `{stop, catalog_changed}`（FAIL 级，见 §7）。

## 7. 首启自举状态机（pristine-only）

设计合同与运行时实现均在 `src/lib/elib_tsid_bootstrap.erl`（`decide/1` 与 manifest
读写已落地并接入 guard 启动链，见 §8）。

- **pristine 判定**：无割接 manifest **且** store 无 durable floor。状态机已接管 pristine
  判定（接入 guard 启动链，见 §8），`store_bootstrap=fresh|existing` 的「调用方自我
  声明」语义已被取代：生产装配缺省 `existing`（双槽 absent 时报 `no_valid_slot` 交
  状态机判定），`fresh` 打开仅发生在状态机授权 `proceed_floor` 之后由 guard 内部
  重开——误配 fresh 无法再绕过 manifest/floor 检查烧号；preflight 另保留
  fresh+非空目录拒绝。
- **STOP 矩阵**（全部 FAIL 级 `{stop, Reason}`，不得降 warning）：

| STOP 原因 | 触发条件 |
|---|---|
| `blocked_legacy_writer` | 无 manifest 且 store 有 floor（旧 writer 未证明停写，或首启中断于 persist 之后、写 manifest 之前）；仅 `IMBOY_TSID_BOOTSTRAP_LEGACY_ACK` 可显式接管 |
| `store_lost` | 有 manifest 且 store floor 丢失/清零 |
| `store_corrupt` | store 打开损坏（corrupt_no_valid / split_brain） |
| `store_identity_mismatch` | 身份/布局不符（store 内部 manifest 或本割接 manifest） |
| `catalog_changed` | manifest 的 `catalog_digest` ≠ 当前 digest |
| `{bootstrap_env, D}` | 环境变量非法（不静默取默认） |
| `{bootstrap_scan, R}` | auto_scan 扫描失败 |

- **环境变量**（非法值一律 `{stop, ...}`）：

| 变量 | 语义 |
|---|---|
| `IMBOY_TSID_BOOTSTRAP_MODE` | `auto_scan`（缺省）\| `manual_floor` |
| `IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS` | 整数 unix 毫秒；manual_floor 必填；换算 `floor_safe_before = ms - EPOCH_MS`（相对毫秒域，**无移位**；带 `<<11` 的是 ID 的 slot/cursor 域，floor 与 store 的 `safe_before` 同域，guard 直接与墙钟 rel-ms 比较） |
| `IMBOY_TSID_BOOTSTRAP_LEGACY_ACK` | 唯一合法值 `I-CONFIRM-OLD-WRITER-STOPPED`；操作员确认旧版停写，补写 manifest 后按 legacy_ack 接管现有 floor |

- **decide/1 返回合同**：`proceed_existing`（正常重启）/ `proceed_floor`（pristine 首启）/
  `adopt_existing`（LEGACY_ACK 接管）/ `{stop, Reason}`。

## 8. 验证状态

- **RELEASE=NO_GO 维持不变**（LOCAL_CANDIDATE_PASS / EXTERNAL_VALIDATION_PENDING 口径不变）。
- EXT-04（目标 PVC/存储类 crash 矩阵）、EXT-05（目标环境双实例锁）维持外部 NO_GO。
- §6/§7 所述 catalog/scan/bootstrap 已由**合同先行**推进为**代码落地**（以下均为
  本地代码可核实的事实，不涉及任何生产/外部验证，不影响 NO_GO 口径）：
  - `elib_tsid_scan` 与 `elib_tsid_bootstrap` 的运行时实现已在 `src/lib/` 落地
    （`scan/1` / `check_schema/2` / `bootstrap_floor/1`；`decide/1` /
    `write_manifest/2` / `read_manifest/1`）；
  - guard 接线已落地：`imboy_sup` 把 `elib_tsid_guard` 置于子进程列表首位
    （早于任何可生成 ID 的 worker），启动链 acquire lifetime lock → open store →
    `elib_tsid_bootstrap:decide/1` → 授权后 persist floor + 写割接 manifest 才
    进入可生成状态；测试可经 `bootstrap_env_fun` / `bootstrap_scan_fun` 注入假
    env/scan，生产缺省真实实现；
  - catalog v1 已回填 104 表并经逐表核对（`elib_tsid_catalog:primary_keys/0`；
    mcp_client 主键 client_id、消息表 hypertable 复合主键等特殊形态已在
    catalog/scan 注释登记）；
  - escript 退役（步骤 6）已完成：`scripts/tsid/tsid_scanner.escript` 与
    `tsid_bootstrap_floor.escript` 均改为复用 `src/lib/elib_tsid_scan.erl`
    唯一权威实现的 thin CLI shell（后者头部标注 DEPRECATED），脚本内不再自带 SQL；
  - catalog_check 已落地：`scripts/tsid/tsid_catalog_check.escript`（CI gate，复用
    `elib_tsid_scan:check_schema/2`）。
  逐项完成对照见证据树 `gate-supplement/catalog-v1-addendum.md`。
- 本轮 Gate 重跑：由主协调者冻结后执行——**待补**。
