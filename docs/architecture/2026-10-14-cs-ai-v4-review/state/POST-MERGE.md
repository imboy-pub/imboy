# POST-MERGE：main 同步进分支后的全量复验

**时间**：2026-10-15 06:48 CST ｜ **分支**：`cs/ai-agent-v1-20261014` ｜ **状态**：复验通过，等待用户决策

## 合并操作

- 合并提交 `0d467919`（chore(merge): sync main into cs/ai-agent-v1-20261014，
  baseline drift 56 commits），把 main（当时至 `990556de`）合入分支。
- 带入：迁移 173–179、hird_runtime v0.4.0（rust-tree 布局、无 Makefile）、
  moya/adm 侧若干修复。
- hird_runtime 首次构建失败（无 Makefile）后按正规路径重做：
  `rm -rf deps/hird_runtime .erlang.mk/dep_built/hird_runtime .erlang.mk/deps.log && make deps`
  （dep_fetch 阶段 autopatch 完整重跑，生成 src 布局与 Makefile）。

## 复验结果（全绿）

| 检查项 | 结果 |
|---|---|
| 10 个 AI 套件 + closure（consume 8 / delivery 7 / grounding 6 / budget 8 / purge 7 / handoff 5 / retrieval-ws 4 / permission 159 / intent 22 / decision 19 / retrieval-pure 13 / closure 8） | 全绿 |
| pytest（产品特性登记，28 例） | 全过 |
| dialyze | 0 违规 |
| arch 模块边界 | 0 违规（25→0 保持） |
| migrations 检查 | ✓ head=180 不变 |
| feature-cross-repo-check（11 份产物三仓逐字节一致） | PASS（2026-10-15 06:44 复跑，修正上轮 ADMIN_DIR 路径笔误） |

## 两点定性

1. **consume 套件首跑 A-5 失败（seen=0）为 xmin-horizon 并发抖动**：复跑 8/8
   绿、并发 beam 消失。xmin-horizon 闸门在并发 xid 分配下把新事件延迟到下一
   轮，是"宁可延迟不可越过"冻结语义的正确行为，非回归。
2. **contract-check 失败为 main 自身遗留**：adm_channel 路由注册表与生成产物
   漂移，合并前后均在、与本分支无关（详见 state/P0-8.md、P0-11.md 记录）。

## 当前状态与待决策

- 分支 **未合并回 main、未 push**（计划约束：合并由用户另行决定）。
- 待用户决策：① 分支合回 main 的时机；② main 侧 contract-check 注册表漂移
  收口（属 main 遗留）；③ 合并后 CI 全量复跑。
- 任务期 PG 库（cs_ai_v4_probe / probe2 / accept）按计划"用后清理"于本记录
  落档后 DROP；验收库重建路径 = createdb + 全量迁移（head=180，见
  state/P0-11.md），`scripts/test/cs_ai_v4_acceptance.sh` 自带前置校验。
