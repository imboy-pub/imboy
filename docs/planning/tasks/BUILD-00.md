# BUILD-00 — 三端真实编译裁剪可行性 Spike

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 BUILD-00 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/BUILD-00/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Build-Spike
- Dependencies：G0-01=PASS、EVID-00=PASS
- Repos：`imboy`、`imboyadmin`、`imboyapp`
- Scope：优先只读验证；允许给现有 artifact verifier 增加最小 forbidden-marker/forbidden-asset 断言和对应测试，不新增 Agent Hub feature、不重构构建系统。
- Actions：
  1. 复用当前可选 feature（优先 `moment`，必要时 `bot_webhook`）作为探针，使用当前已存在的 `base-only` 与 `full-selected` preset。
  2. 在 Backend release/BEAM 中验证专属 route/worker/module 不存在；在 Admin dist 中验证专属动态 chunk/import/module marker 不存在；在 Flutter release APK/AOT payload 中验证专属 route/import/compiled symbol 不存在。
  3. 对 `full-selected` 做同一组正向检查，证明断言能发现资产，而不是 marker 永远匹配不到。
  4. 明确区分 `compiled_features` 契约 marker 与物理资产；只隐藏菜单、route guard 或运行时开关一律 No-Go。
  5. 输出 `Go|No-Go`、三端观测表、命令、artifact hash、已知误报边界；No-Go 时先另行修复构建架构，本计划 PDT-01 及后续任务保持 blocked。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
python3 test/scripts/test_verify_product_feature_artifacts.py
FEATURE_EVIDENCE_DIR="$IMBOY_EVIDENCE_ROOT/BUILD-00/base-only" bash scripts/run_product_feature_matrix.sh base-only
FEATURE_EVIDENCE_DIR="$IMBOY_EVIDENCE_ROOT/BUILD-00/full-selected" bash scripts/run_product_feature_matrix.sh full-selected
python3 scripts/verify_agent_hub_task_evidence.py --task "$IMBOY_EVIDENCE_ROOT/BUILD-00/evidence.json"
```

- Acceptance IDs：`BUILD-00-A01` base-only 三端均证明 probe 物理资产不存在；`BUILD-00-A02` full-selected 三端均证明相同资产存在；`BUILD-00-A03` 结果不是仅检查 compiled_features marker；`BUILD-00-A04` 决策、hash 和残余风险可复核。
- Go：A01-A04 全 PASS，且无需为 Agent Hub 另建第二套构建框架。
- No-Go：任一端只能运行时隐藏或负向检查不可证明；状态记 FAIL/No-Go，禁止 PDT-01 和大规模实现。
