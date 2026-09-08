# GATE-01 — LOCAL_AGENT_HUB_GATE

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 GATE-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/GATE-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Coordinator
- Dependencies：REV-01、EVID-00；AGT-02 仅在 AGT-01=Go 时要求完成
- 判定条件：
  1. 所有必选任务的 evidence.json 均经 verifier 判定 PASS，AGT-01 有 Go/No-Go 书面结论。
  2. `base-only` 与 `agent-hub-selected` 两套三端构建矩阵通过。
  3. 最终合并 Base 上 Golden Flow、correlation 重建和负例 verifier 通过，证据 sha256 完整；旧 Base 证据均 superseded。
  4. HIGH/MEDIUM 阻断清零，secret/PII 扫描清零。
  5. 运维文档能在全新 scratch 环境复现，不依赖个人机器残留状态。
- 输出：`$IMBOY_EVIDENCE_ROOT/GATE-01/evidence.json` 和人读 `gate-report.md`，结论只能是 `PASS`、`PARTIAL`、`FAIL`、`BLOCKED`。
- Acceptance IDs：`GATE-01-A01` 全部必选 task evidence 经统一 verifier 为 PASS；`GATE-01-A02` 最终 Base 的构建/E2E/审计证据 hash 完整；`GATE-01-A03` HIGH/MEDIUM 与 secret/PII 阻断清零；`GATE-01-A04` 空 scratch 环境按 runbook 可复现。
- 禁止替代：unit tests 不能替代真实协议 harness；Admin mock 不能替代后端契约；本地浏览器不能替代真机；静态 manifest 不能替代裁剪产物检查。
