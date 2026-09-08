# CUST-01 — CUSTOMER_PRODUCT_GATE

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 CUST-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/CUST-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked_external`
- Owner：用户指定的产品验收负责人；实现团队不得自评自签
- Dependencies：GATE-01=PASS、用户明确授权目标用户参与和联系方式使用
- Protocol：使用隔离演示环境和固定脚本，不给参与者口头操作提示；只记录计时、结构化答案和脱敏审计结果，不采集无关 PII。
- Metrics：
  1. Time-to-first-success：从已完成 Golden Install 的空实例、管理员首次进入开始，到完成首个 `MCP task -> HITL -> terminal`；目标 `<=30 分钟`，全程无需工程师改 DB、代码或隐藏配置。
  2. Approval comprehension：目标用户无口头提示回答“谁请求、将执行什么、影响什么、如何拒绝/撤销”；总体正确率 `>=80%`，且任何高风险动作的“执行内容/影响/拒绝方式”关键项不得答错。
  3. Audit reconstruction：独立审阅者只使用 Admin audit 与 evidence，在 `<=10 分钟` 内重建 actor、request、task、decision、execution、delivery、outcome；correlation 链完整率必须 `100%`。
- Verify：CUST-01 evidence.json 包含匿名 participant/session ID、开始/结束时间、逐题评分、审计重建实体计数和 artifact hash；EVID-00 verifier 只校验完整性与阈值，不代替人工真实性签署。
- Acceptance IDs：`CUST-01-A01` 首次成功耗时达标；`CUST-01-A02` 审批理解度达标且关键项零错；`CUST-01-A03` 审计重建时间和 100% correlation 完整率达标；`CUST-01-A04` 验收负责人签署已知限制且证据脱敏。
- Stop：未获参与者/联系方式授权、样本不是目标用户、验收由实现者代答、或需接触生产数据时保持 `blocked_external`，不得用内部演练替代。本计划不预设样本规模或目标用户招募口径，须由用户另行授权后冻结；授权前继续保持 `blocked_external`。

`CUSTOMER_AGENT_HUB_GATE` 的唯一判定公式：`EXT-01=PASS AND CUST-01=PASS`。任一项未执行、BLOCKED、PARTIAL 或 FAIL，最终门均不得 PASS。
