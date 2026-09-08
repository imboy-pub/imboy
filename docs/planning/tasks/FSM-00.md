# FSM-00 — Agent Task 状态机机器契约冻结

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 FSM-00 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/FSM-00/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Task-Contract
- Dependencies：G0-01 draft evidence 完成、EVID-00=PASS
- Repo：`imboy`
- Files：`docs/api-contracts/agent_hub_task_state_machine.md`、`.json`、`scripts/verify_agent_hub_task_state_machine.py`、`test/scripts/test_verify_agent_hub_task_state_machine.py`。
- Actions：
  1. 以机器可读矩阵逐行固定 `from`、`action`、`to`、允许 actor、幂等语义、side effect 和 terminal 标识；文档只能解释 JSON，不得另建第二真源。
  2. 覆盖 `submitted`、`working`、`awaiting_approval`、`approved`、`rejected`、`expired`、`completed`、`failed`、`cancelled` 的全部合法迁移；显式生成并验证状态全集的非法迁移补集。
  3. 固定 terminal 不可逆、first-writer-wins、重复 approve/reject、审批过期、服务重启恢复和执行调度语义。
  4. 执行规则采用“幂等副作用键下沉 + 可证明幂等时恢复重试；否则 at-most-once + 人工复核”，禁止使用无条件 exactly-once 声明。
  5. verifier 检查无重复边、无悬空状态、所有 terminal 无出边、所有非 terminal 有收敛路径、非法迁移默认拒绝。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
python3 test/scripts/test_verify_agent_hub_task_state_machine.py
python3 scripts/verify_agent_hub_task_state_machine.py docs/api-contracts/agent_hub_task_state_machine.json
git diff --check -- docs/api-contracts scripts/verify_agent_hub_task_state_machine.py test/scripts/test_verify_agent_hub_task_state_machine.py
```

- Acceptance IDs：`FSM-00-A01` 矩阵覆盖全部状态与合法/非法迁移；`FSM-00-A02` 并发、重复、过期、terminal 和重启规则可机验；`FSM-00-A03` 执行语义不存在无法兑现的 exactly-once 承诺。
- Stop：状态、actor 或执行语义仍需产品猜测时标记 `blocked_decision`；DATA-01 不得先行自创状态。
