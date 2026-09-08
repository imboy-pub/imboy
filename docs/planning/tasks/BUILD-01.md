# BUILD-01 — Agent Hub 三端构建期产品切片

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 BUILD-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/BUILD-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Build-Composition
- Dependencies：BUILD-00=Go、MCP-02、BOT-01、WH-02、ADM-01、APP-01；AGT-02 为可选
- Repos：`imboy`、`imboyadmin`、`imboyapp`
- Actions：
  1. 不创建 `feature.enterprise`；新增产品 manifest/profile `agent_hub`，选择细粒度 features。
  2. 推荐 feature keys：`ai_agent`、`mcp`、`developer_bot`、`channel_webhook`、`agent_task`；最终依赖图由 PDT-01 和现有 plugin registry 校验。
  3. 生成器输出 Backend 编译常量、Flutter routes/imports、Admin feature contract；禁用功能不仅隐藏菜单，还不得编译/注册专属 route/worker。
  4. 保留 `core` 必选和 E2EE capability 约束；Agent Hub profile 若允许服务端 AI，必须明确 `e2ee_mode` 兼容矩阵。
  5. 复用 BUILD-00 的 forbidden-asset 机制建两套矩阵：`base-only` 断言 Agent Hub 资产/route/module/worker 不存在，`agent-hub-selected` 断言全部所需资产存在。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
python3 scripts/generate_product_features.py --manifest config/product-feature-manifests/agent_hub.json --check
FEATURE_EVIDENCE_DIR="$IMBOY_EVIDENCE_ROOT/BUILD-01" \
  bash scripts/run_product_feature_matrix.sh base-only
FEATURE_EVIDENCE_DIR="$IMBOY_EVIDENCE_ROOT/BUILD-01" \
  bash scripts/run_product_feature_matrix.sh agent_hub
make eunit-local t=imboy_feature_compiled_tests
```

并验证：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboyadmin" && bun run build && bun run typecheck
cd "$IMBOY_WORKSPACE_ROOT/imboyapp" && flutter analyze lib/app_core/feature_flags lib/config/router
```

- Acceptance IDs：`BUILD-01-A01` 一个 manifest 决定三端产物；`BUILD-01-A02` base-only 中无 Agent/MCP/Bot/Webhook UI、route、module、worker 或编译 symbol；`BUILD-01-A03` selected 中 Golden Flow 所需能力齐；`BUILD-01-A04` manifest hash 三端一致且 artifact verifier PASS。
- Stop：如果生成器只能隐藏菜单但仍编译完整模块，本任务不得标 PASS。
