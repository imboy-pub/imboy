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

- **范围**：全部运行时 TSID 主键列（约 104 个；最终数字与清单
  **以 `elib_tsid_catalog:primary_keys()` 为准**——数据核对回填前该函数为空列表占位，
  此时的 `digest()` 锚定空清单，不得用于割接）。
- **为何取现状口径**（撞号风险论证）：
  1. 第一次 cutover 前，凡可能已写入历史 TSID 的主键列必须全部纳入扫描保护；
  2. 任何漏扫列中的历史 TSID 都会在重启自举（auto_scan 取 max(id)+1）时被新 ID 撞号；
  3. 因此 v1 宁全勿漏：catalog 收缩只能发生在治理迁移（§5）完成之后。
- **数据来源**：call-sites × migrations DDL 静态扫描生成，人工逐条核对。
- **绑定机制**：`digest()`（对 {version, 排序后清单} 的 SHA-256）→ 写入割接 manifest
  的 `catalog_digest` 字段（`elib_tsid_bootstrap`，魔数 IMBTSIDB1）→ 自举校验不符即
  `{stop, catalog_changed}`（FAIL 级，见 §7）。

## 7. 首启自举状态机（pristine-only）

设计合同见 `src/lib/elib_tsid_bootstrap.erl` 头注释（本轮冻结；运行时实现未落地，见 §8）。

- **pristine 判定**：无割接 manifest **且** store 无 durable floor。状态机接管 pristine
  判定后，现役 `store_bootstrap=fresh|existing`（调用方自我声明）退役为被取代机制——
  防止误配 fresh 绕过 manifest/floor 检查烧号；guard 接线状态见 §8。
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
| `IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS` | 整数 unix 毫秒；manual_floor 必填；换算 `floor_safe_before = (ms-EPOCH)<<11` |
| `IMBOY_TSID_BOOTSTRAP_LEGACY_ACK` | 唯一合法值 `I-CONFIRM-OLD-WRITER-STOPPED`；操作员确认旧版停写，补写 manifest 后按 legacy_ack 接管现有 floor |

- **decide/1 返回合同**：`proceed_existing`（正常重启）/ `proceed_floor`（pristine 首启）/
  `adopt_existing`（LEGACY_ACK 接管）/ `{stop, Reason}`。

## 8. 验证状态

- **RELEASE=NO_GO 维持不变**（LOCAL_CANDIDATE_PASS / EXTERNAL_VALIDATION_PENDING 口径不变）。
- EXT-04（目标 PVC/存储类 crash 矩阵）、EXT-05（目标环境双实例锁）维持外部 NO_GO。
- §6/§7 所述 catalog/scan/bootstrap 合同先行交付**已全部落地**（提交链
  352bee53→5ccf04e6：实现、guard 接线、escript 退役、catalog_check）；
  逐项完成对照见证据树 `gate-supplement/catalog-v1-addendum.md`。
- 本轮 Gate 重跑：**已完成**（2026-09-29，24 命令落档、finalize 复算 24/24、
  TSID 套件 190 用例全绿；证据树 `20260929T113446Z-5ccf04e6`，审查轮与
  合入后增量审查结论见其 `FINAL/` 与 `REPORT.md`）。

## 9. 配置面加固（R1，2026-09-30，dd2be7ea）

0cc0d7ce（TSID guard eunit 轨道豁免）合入 main 后的 review 轮（证据树
`FINAL/post-merge-review-8c070819.md`）登记 R1：测试 seam 与 lock_provider
切换无环境门槛，生产 sys.config 误设可分别导致 ID 重用（假 scan 从
floor 0 起跳）与跨 VM 互斥失效（registry 为同 VM 语义，双实例红线绕过）。
本轮按「**默认硬编码最优配置**」原则加固落地：

- **lock_provider 配置面删除**，环境分档硬编码（`imboy_sup:lock_provider_for/1`）：
  prod（及一切未知值，含 <<"pro">>/拼错值）恒 flock；test 恒 registry；
  local/dev 探测 flock 可用则用、缺失回落 registry（仅单机开发场景）。
  未知环境一律按生产对待——与 `imboy_env:current/0` fail-safe 哲学一致
  （未设置即 prod），**默认即最保守生产配置**。任何环境显式设置
  `{imboy, tsid_lock_provider}` → `{tsid_config_forbidden,_}` 拒启。
- **seam 双键仅 test 轨道存在**（`imboy_sup:maybe_bootstrap_seams/1`）：
  非 test 轨道代码路径不读 `tsid_bootstrap_env_fun/scan_fun`，检测到设置
  即拒启；test 轨道半设/类型不符亦拒启（收硬 0cc0d7ce 的静默忽略）。
- eunit_setup 删除 `tsid_lock_provider` set_env，改为
  `os:putenv("IMBOYENV","test")` 轨道环境声明（IMBOYENV 优先于
  application env，覆盖外部启动方式；eunit VM 一次性无需还原）。
- 合同测试 +6 用例（分档矩阵/local 探测注入/禁键拒启/seam 透传与半设
  拒启/prod 端到端）；TSID 八套件 196 用例全绿（190 + 新增 6）。

**部署注意**：升级到本提交后，配置中残留 `{imboy, tsid_lock_provider}`
会使应用拒启（错误消息含移除指引）——这是设计行为，删除该键即可；
`tsid_bootstrap_env_fun/scan_fun` 同理，仅 eunit 轨道合法。
