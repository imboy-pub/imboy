# Release Evidence Manifest（机器生成）

> 由 `scripts/evidence_manifest.sh` 生成。字段集见 `../v2/20-implementation-and-acceptance-plan.md` §13。
> **⛔ 标记的字段是真的没有，不是懒得填。**

## 1. 版本与 commit 锚

| 项 | 值 |
|---|---|
| imboy HEAD | `6a1159a8` |
| imboy 工作树 | ⚠️ 18 个文件未提交（manifest 与工作树不一致） |
| imboyapp HEAD | `3074befa` |
| imboyapp 工作树 | ⚠️ 31 个文件未提交（manifest 与工作树不一致） |
| imboyapp 版本 | 1.0.0-alpha.15+6 |

## 2. 依赖锁与哈希（SHA-256 前 16 位）

| 项 | 值 |
|---|---|
| imboyapp `pubspec.lock` | `10c39c58043d9028` |
| imboy `Makefile`（DEPS 真源） | `50bb42b22f9cb130` |
| 许可证清单（生成态） | `5df0d1e0624913e9` |
| SBOM（CycloneDX/SPDX） | ⛔ **未提供** —— 未生成。`SBOM Diff Report` 工作流跑 trivy，但产物未纳入本清单 |

## 3. 测试计数

| 项 | 值 |
|---|---|
| Erlang eunit 测试函数 | 5234 |
| Erlang 测试文件 | 478 |
| Dart 测试用例（test/testWidgets） | 5303 |
| Dart 测试文件 | 511 |
| 其中 e2ee 专项测试文件 | 60 |
| 已知 skip / 暂排 | CI 暂排 2 个文件（X16/X17，理由见 known-issues-ledger IMB-2026-026） |

## 4. 互操作与向量

| 项 | 值 |
|---|---|
| 跨实现 golden vectors | KT profile v1 已核验（ADR 29 §8）；fallback key canonical 双端钉死（含长度 82） |
| 向量文件哈希 | ⛔ **未提供** —— 向量以内联形式散在 ADR/测试中，无独立向量文件可哈希 |
| 第三方实现互操作测试 | ⛔ **未提供** —— 未做。我方两端均为自研客户端，无第三方 Olm 实现对接 |

## 5. 真机与性能

| 项 | 值 |
|---|---|
| 真机型号 / OS / 性能结果 | ⛔ **未提供** —— **双端真机从未验证**（IMB-2026-021 / X2） |

## 6. 鲁棒性

| 项 | 值 |
|---|---|
| fuzz 语料 / 运行次数 / crash 数 | ⛔ **未提供** —— 未做 fuzz |
| 崩溃一致性运行次数 / 失败数 | ⛔ **未提供** —— 未做。无跨进程 harness（IMB-2026-022） |
| 故障注入计数 | ⛔ **未提供** —— 同上 |

## 7. 迁移与回滚

| 项 | 值 |
|---|---|
| 迁移文件数 | 55 |
| 最新迁移 | 00000056_red_packet_scope.up.sql |
| 回滚演练 id | ⛔ **未提供** —— 未做演练 |

## 8. 外部审计

| 项 | 值 |
|---|---|
| 上游 `vodozemac` | Least Authority 2022-03 已审计（建议复用结论，重点审我方胶水层） |
| 我方外部审计报告 id / open findings | ⛔ **未提供** —— **未采购审计**。按 2026-08-01 决策，本包为「就绪包」而非已完成审计（TT-D5 SHOULD 降级形态） |

## 9. 发布与灰度

| 项 | 值 |
|---|---|
| canary 指标窗口 / stop-trigger 计数 | ⛔ **未提供** —— 未做灰度发布流程 |

## 10. 签署

| 项 | 值 |
|---|---|
| 负责人 | leeyi（solo） |
| 时间戳 | ⛔ **未提供** —— 由发布流程在采纳本 manifest 时填写；脚本不自造时间以免与 git 历史矛盾 |

## 11. 门禁现状

| 项 | 值 |
|---|---|
| 分发阻断门 | `scripts/license_inventory.sh --check` —— **当前必然退出 1**（AGPL 未解，预期行为） |
| 许可证判别自检 | `scripts/license_inventory.sh --selftest` 10/10 |
| 模块边界门 | `scripts/check_module_boundaries.sh` |

---

本 manifest **不含**密钥、用户数据或生产 PII。
