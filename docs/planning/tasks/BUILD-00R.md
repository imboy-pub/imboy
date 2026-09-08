# BUILD-00R — 后端编译期物理裁剪机制 + BUILD-00 Spike 重跑

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 BUILD-00R 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。
> 诞生背景：BUILD-00 spike 实证后端只能运行时隐藏（No-Go），本卡为其唯一列明的修复路径
> （"No-Go 时先另行修复构建架构"），由 Coordinator 以用户"继续"授权自主实施。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- `erlang.mk`（vendored）与 `scripts/run_product_feature_matrix.sh`（有并行会话 staged 修改）禁改；
  机制只能通过项目 `Makefile` 覆写与生成器实现。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/BUILD-00R/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`done`（见证据）
- Owner：Coordinator
- Dependencies：BUILD-00=FAIL/No-Go（本卡为其修复）、EVID-00=PASS
- Repos：`imboy`
- Scope：
  1. 生成器为编译期选中的 feature 输出 per-feature `-define(IMBOY_FEATURE_<UPPER>, true)`，
     为未选中 feature 输出后端模块排除清单 `include/generated/imboy_product_features_erlc.mk`。
  2. `Makefile` 接线：include `.mk` 并以 `ERLC_EXCLUDE ?=` 注入 erlang.mk 原生排除；
     覆写 `ERLC_EXCLUDE_PATHS` 为按本仓 `src/<子目录>/` 布局的递归解析；
     `relx-rel` 前置 prune 钩子清 ebin 陈旧 beam 并删除同版本号 release 目录。
  3. `imboy_router`/`attach_logic`/`report_logic` 三处对 moment 模块的交叉引用改为
     form 级 `-ifdef(IMBOY_FEATURE_MOMENT)` 门控 helper（列表 splice + fail-closed 兜底子句），
     未选中时编译产物不含对 moment 模块的任何引用。
  4. 双 preset 矩阵重跑 + 后端物理探针正反验证；机制不得破坏 full-selected 全量口径。
- Actions：
  1. `scripts/generate_product_features.py` 增补 FEATURE_BACKEND_MODULES、per-feature defines、
     `.mk` 输出；同步 `test/scripts/test_generate_product_features.py` 契约断言。
  2. Makefile 接线（见 Scope 2）；`imboy_router.erl` API/admin 两张路由表 splice + 门控 helpers；
     `attach_logic.erl`、`report_logic.erl` form 级门控 helpers。
  3. `make app` + generator 测试回归；双 preset 矩阵（full-selected 先、base-only 后）。
  4. 后端物理探针：release lib 目录 moment beam 枚举、`imboy_router.beam` 路由字符串 grep、
     `imboy_feature.beam` manifest hash 校验；正反双向留档。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboy"
python3 test/scripts/test_generate_product_features.py
FEATURE_EVIDENCE_DIR="$IMBOY_EVIDENCE_ROOT/BUILD-00R/full-selected" bash scripts/run_product_feature_matrix.sh full-selected
FEATURE_EVIDENCE_DIR="$IMBOY_EVIDENCE_ROOT/BUILD-00R/base-only" bash scripts/run_product_feature_matrix.sh base-only
python3 scripts/verify_agent_hub_task_evidence.py --task "$IMBOY_EVIDENCE_ROOT/BUILD-00R/evidence.json"
```

- Acceptance IDs：`BUILD-00R-A01` base-only 后端 release 中 moment 物理资产不存在
  （0 个 moment beam、imboy_router.beam 0 条 moment 路由字符串）；`BUILD-00R-A02`
  full-selected 后端同一组探针全部命中（机制不破坏全量口径）；`BUILD-00R-A03`
  双 preset 矩阵机器判定 PASS（含 eunit/Flutter/Admin 与 artifacts verifier）；
  `BUILD-00R-A04` 决策、hash、残余风险可复核（含对 BUILD-00 证据的 supersession 记录）。
- Go：A01-A04 全 PASS ⇒ BUILD-00 的 No-Go 条款解除，PDT-01 解锁。
- Fail：任一端仍只能运行时隐藏 ⇒ 保持 BUILD-00 No-Go 语义，本卡记 FAIL。
