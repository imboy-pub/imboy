# EVID-00 — Evidence JSON Schema、Verifier 与汇总规则

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 EVID-00 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/EVID-00/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Evidence-Contract
- Dependencies：G0-01 draft evidence 完成
- Repo：`imboy`
- Files：`docs/testing/agent-hub-evidence.schema.json`、`scripts/verify_agent_hub_task_evidence.py`、`test/scripts/test_verify_agent_hub_task_evidence.py`。
- Actions：
  1. 把 §4.3 的最低语义固化为 JSON Schema；限定状态枚举、SHA/hash 格式、命令退出码、Acceptance 映射和 residual risk 结构。
  2. verifier 校验 Schema 后逐条确认所有计划 Acceptance ID 都有且仅有一个证据结果，artifact 存在且 sha256 匹配，支撑 PASS 的命令退出码均为 0。
  3. 缺字段、缺 Acceptance、重复 ID、伪造路径、hash 不符、非零命令、`BLOCKED/PARTIAL` 冒充 PASS 全部返回非零。
  4. 支持 `--task` 单任务和 `--gate` 多任务汇总，stdout 输出稳定 JSON；不得扫描或回显 secret 内容。
  5. 用 verifier 回验 G0-01 的 draft evidence；不符合时只补证据结构，不改写基线结果。回验 PASS 后由 Coordinator 同时结算 G0-01 和 EVID-00。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
python3 test/scripts/test_verify_agent_hub_task_evidence.py
python3 scripts/verify_agent_hub_task_evidence.py --task "$IMBOY_EVIDENCE_ROOT/G0-01/evidence.json"
git diff --check -- docs/testing/agent-hub-evidence.schema.json scripts/verify_agent_hub_task_evidence.py test/scripts/test_verify_agent_hub_task_evidence.py
```

- Acceptance IDs：`EVID-00-A01` Schema 正反 fixtures 全通过；`EVID-00-A02` 每种 fail-closed 条件均使 verifier 非零；`EVID-00-A03` G0-01 可生成并通过统一 evidence.json；`EVID-00-A04` gate 汇总不会把 BLOCKED/PARTIAL 聚合为 PASS。
- Stop：不得引入仅供此计划使用的第三方依赖；Python 标准库无法完整校验的 Schema 关键约束必须由显式代码和测试补齐。
