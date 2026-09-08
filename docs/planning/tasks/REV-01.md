# REV-01 — 安全、类型与静默失败复审

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 REV-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/REV-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Review-Only
- Dependencies：E2E-01、EVID-00
- Scope：只读审查三仓 diff 和证据；不直接修复，问题退回原 owner。
- Review matrix：
  - Security：MCP credential、越权、SSRF、redirect、DNS rebinding、Webhook 重放、secret 泄漏、E2EE fail-closed。
  - Correctness：FSM 矩阵、并发审批、幂等/at-most-once 边界、outbox 重试/死信、服务重启恢复。
  - Silent failure：catch-all、best-effort audit、HTTP error、delivery lost、UI 空回退。
  - Type/contract：TSID string、Admin types、Flutter dynamic map、MCP JSON Schema、错误码。
  - Scope：无动态市场、无支付 tool、无生产配置/PII、无 unrelated diff。
- Verify：

```bash
: "${IMBOY_BASE_SHA:?set IMBOY_BASE_SHA from G0-01 evidence}"
: "${ADMIN_BASE_SHA:?set ADMIN_BASE_SHA from G0-01 evidence}"
: "${APP_BASE_SHA:?set APP_BASE_SHA from G0-01 evidence}"
git -C "$IMBOY_WORKSPACE_ROOT/imboy" diff --check "$IMBOY_BASE_SHA..HEAD"
git -C "$IMBOY_WORKSPACE_ROOT/imboyadmin" diff --check "$ADMIN_BASE_SHA..HEAD"
git -C "$IMBOY_WORKSPACE_ROOT/imboyapp" diff --check "$APP_BASE_SHA..HEAD"
rg -n -i "bearer[[:space:]]+[A-Za-z0-9._-]{16,}|api[_-]?key['\"[:space:]:=]+[A-Za-z0-9._-]{12,}|(verify|api|access|refresh)[_-]?token['\"[:space:]:=]+[A-Za-z0-9._-]{12,}" "$IMBOY_EVIDENCE_ROOT/E2E-01"
```

- 扫描规则：上面的具体 secret 格式扫描必须零真实泄漏；另用 `rg -n -i 'secret|token|credential|mcp_'` 做宽模式初筛并人工分诊，合法 schema 键名不得伪装成“扫描清零”。分诊结果进入 evidence.json。
- Acceptance IDs：`REV-01-A01` HIGH=0、MEDIUM 阻断=0；`REV-01-A02` 每个 finding 有文件/行、复现和 owner；`REV-01-A03` LOW 仅进入 residual risk 且不影响 Golden Flow 数据完整性或权限；`REV-01-A04` secret 精确扫描零泄漏且宽扫描逐项分诊。
- Stop：发现 secret/PII 时先隔离证据目录并报告，不把敏感值复制进文档。
