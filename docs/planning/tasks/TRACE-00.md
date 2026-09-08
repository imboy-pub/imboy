# TRACE-00 — correlation_id 全链路契约冻结

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 TRACE-00 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/TRACE-00/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Trace-Contract
- Dependencies：G0-01=PASS、EVID-00=PASS
- Repo：`imboy`
- Files：`docs/api-contracts/agent_hub_correlation_contract.md`、`scripts/verify_agent_hub_correlation_trace.py`、对应 tests/fixtures。
- Actions：
  1. 固定可信入口：服务端在首个受信入口生成 `correlation_id`；仅允许已认证内部上下文透传，外部客户端提供的同名值不得覆盖可信值。
  2. request、task、event、approval、execution、delivery 继承同一 `correlation_id`，同时保留各自独立 ID；JSON 一律 string，格式、长度和日志字段名唯一。
  3. 固定 HTTP/MCP/SSE、异步 worker、重试、Bot reply、Webhook outbox 和服务重启后的传播规则；断链必须显式失败或形成可告警缺口，不能静默生成第二条链。
  4. 日志/审计只记录 correlation 和实体 ID，不记录 secret、消息正文、完整 URL 或 PII。
  5. verifier 接收审计导出，按 correlation 重建 `request -> task -> event -> approval -> execution -> delivery -> outcome`，检查唯一根、必需边、时间顺序和孤儿记录。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
python3 test/scripts/test_verify_agent_hub_correlation_trace.py
python3 scripts/verify_agent_hub_correlation_trace.py test/fixtures/agent_hub/correlation/valid.json
! python3 scripts/verify_agent_hub_correlation_trace.py test/fixtures/agent_hub/correlation/orphan-delivery.json
git diff --check -- docs/api-contracts/agent_hub_correlation_contract.md scripts/verify_agent_hub_correlation_trace.py test/scripts/test_verify_agent_hub_correlation_trace.py
```

- Acceptance IDs：`TRACE-00-A01` 六类实体和 outcome 可由一个 correlation 重建；`TRACE-00-A02` 客户端覆盖、异步丢失、孤儿/重复根全部拒绝；`TRACE-00-A03` verifier 不依赖消息正文或 secret。
- Stop：已有协议字段冲突时记录兼容映射，不批量重命名无关 request_id。
