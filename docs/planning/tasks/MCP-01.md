# MCP-01 — MCP Client 独立身份、凭证与默认强制治理

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 MCP-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/MCP-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Backend-MCP-Identity
- Dependencies：PDT-01、TRACE-00
- Repo：`imboy`
- Files：migration 91；`mcp_client_repo`、治理 logic/handler、MCP route auth、tests。
- Actions：
  1. 去除“一 owner 只能一个 client”的产品限制，保留 owner 归属；新增稳定 `client_key`、credential digest、prefix、expiry、last_used_at。
  2. 创建时返回一次明文 credential；数据库、日志、Admin list/detail 永不返回明文。
  3. MCP route 只在该 route 接受 MCP credential；解析后注入 `owner_uid + client_id`，tools 不接受参数自报身份。
  4. 撤销、过期、禁用、未知凭证全部 fail-closed；高熵 token 使用固定 SHA-256 digest 索引精确查找，认证成功后只把 server-side correlation_id 和 principal 注入上下文。
  5. `agent_hub`/enterprise profile 默认 enforce=true；community 兼容行为须由显式配置开启，不能静默放行。
  6. approve 只授予契约定义的默认 read tools，不再自动授予未来所有新 tool；新增 tool 默认无授权。
  7. 复用现有限流组件实现 per-client tools/call 速率和并发上限；超限记录 client/correlation，不记录参数正文。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
make app
make eunit-local t=mcp_client_repo_tests
make eunit-local t=mcp_governance_logic_tests
make eunit-local t=mcp_handler_auth_tests
make eunit-local t=mcp_authz_gate_tests
make eunit-local t=adm_mcp_handler_tests
git diff --check
```

- Acceptance IDs：`MCP-01-A01` 同 owner 可建两个独立 client；`MCP-01-A02` 凭证只显示一次且库中无明文；`MCP-01-A03` 撤销/过期立即拒绝；`MCP-01-A04` 新增 tool 未授权默认拒绝；`MCP-01-A05` 兼容 JWT 路径有显式 profile 测试且生产默认不旁路 enforce；`MCP-01-A06` 单 client 洪泛被限流且不拖累其他 client。
- Stop：不得为方便测试全局放开 MCP route，或把 credential 写入 repo fixture。
