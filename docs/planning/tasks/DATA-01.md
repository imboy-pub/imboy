# DATA-01 — 持久 Agent Task、事件与审批仲裁

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 DATA-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/DATA-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Backend-Task
- Dependencies：PDT-01、FSM-00、TRACE-00
- Repo：`imboy`
- Files：migration 90；`agent_task_repo/ds/logic`；现有 `agent_task_observer.erl`、`agent_task_handler.erl`；相关 tests。
- Actions：
  1. 用 migration 90 创建最小 `agent_task`、`agent_task_event`、`agent_task_decision`，带 FSM-00 状态 CHECK、`correlation_id`、幂等键和必要索引。
  2. 把 ETS pending/decision 替换为 DB 原子更新；first-writer-wins 由唯一约束或条件 UPDATE 保证。
  3. 保留现有过渡态 ephemeral、终态 durable 的投递语义；数据库故障时审批 fail-closed。
  4. 任务读写必须校验群成员、agent 归属和发起者身份；不信任事件携带的 member list 做授权。
  5. demo driver 仅在 dev/test profile 可用，生产 route 返回 404 或不编译注册；所有持久事件按 TRACE-00 继承 correlation_id。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
make app
make eunit-local t=agent_task_repo_tests
AGENT_TASK_APPROVAL_WORKERS="${AGENT_TASK_APPROVAL_WORKERS:-32}" make eunit-local t=agent_task_logic_tests
make eunit-local t=agent_task_observer_tests
make eunit-local t=agent_task_handler_tests
make eunit-local t=agent_task_demo_handler_tests
git diff --check
```

- Acceptance IDs：`DATA-01-A01` 并发 approve/reject 只有一个成功且符合 FSM-00；`DATA-01-A02` 重启后 pending/decision 与 correlation 可恢复；`DATA-01-A03` 重复 event 不重复发 durable 消息；`DATA-01-A04` 非群成员、agent 本人、E2EE/未知模式都拒绝；`DATA-01-A05` migration up/down 在 scratch PG 通过。
- Evidence 特项：并发竞争目标为 32 worker、最低可接受为 8 worker，且全部竞争同一个 task。默认必须跑 32；仅当机器资源不足且证据记录 requested/actual worker 数、资源限制、降级理由和重跑命令时，才允许降至 `8..31` 并据此判定 Acceptance。低于 8 标记 `BLOCKED`，不得报假 FAIL 或 PASS。证据还必须包含重启恢复结果。
- Stop：需要把群成员快照持久化或改变现有 group 权限模型时先回 PDT-01。
