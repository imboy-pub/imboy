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
2. **contract-check 失败 = 本分支遗漏 + main 累积漂移（2026-10-15 复查修正）**：
   初判"纯 main 遗留"不准确——实跑 `make contract-export` 后 diff 含本分支
   P0-10 的 9 个 `/api/adm/cs/ai/*` 端点（此前未按"改真源同 PR 内 export 并
   提交产物"规矩登记落仓产物），另含 main 带入的累积漂移（micro_community
   discussion/candidates、essence/posts/tags、attachment gateway、workspaces
   attachments、invite 等 15 端点及枚举排序）。已在本分支 export 收口，
   完整 contract-check（--admin/--flutter 指向主工作区两仓）PASS；
   合并回 main 后该门即绿。

## 收口与清理（2026-10-15）

- 契约产物再生成 + 本文件与 state/P0-11.md 定性修正：单提交收口。
- 任务期 PG 库（cs_ai_v4_probe / probe2 / accept）已按计划"用后清理"DROP，
  残留 0；验收库重建路径 = createdb + 全量迁移（head=180，见
  state/P0-11.md），`scripts/test/cs_ai_v4_acceptance.sh` 自带前置校验。
  集群角色 cs_ai_purge 保留（迁移 180 的 schema 预期组件，IF NOT EXISTS
  防护，不在"库清理"授权范围）。

## 范围外 /utf8 扫尾调查（2026-10-15，全仓清零）

P0-12 遗留的"范围外 /utf8 扫尾（group_schedule/group_vote 等）"已闭环：

- **erlc 拼接语义探针实证**（/tmp utf8_probe，真红留档）：单段无 /utf8 截断
  （"中文test" → 6 字节低 8 位）；相邻字符串字面量**拼接后整体作为一个
  segment**，任意段带 /utf8 即整体安全（10 字节完整 UTF-8）；**逗号分段各段
  独立**（`"中文", "test"/utf8` 仍截断）；`"x"/utf8` 后继续拼接直接语法错误。
- **窄口径全仓扫描**（binary 字面量内非 ASCII 且无 /utf8）：src/ 命中 12 处
  —— 11 处为多行拼接 + 末段 /utf8（按探针语义安全，含 audit_policy 6 处、
  workspace_logic 2 处、moya_ai_draft_logic、user_export_logic 各 1 处），
  1 处为注释示例误报；test/+scripts/ 命中 1 处真暴露
  （test/lib/elib_multipart_tests.erl:106 单段文件名，套件断言只校验
  payload 与 filename 无关 → 无假绿，仅字面意图被截断）→ **e9c7cb21 修复，
  套件 15/15 绿**。
- P0-5 点名的 group_schedule/group_vote/friend_ds/group_file_logic 四文件
  已由 b9b900ab 全部覆盖（现态：error_msg 集中映射或已带 /utf8）。
- 结论：**全仓 erlc /utf8 截断暴露清零**。宽口径（含 char list/moduledoc）
  的 802 行命中不受此陷阱影响——截断只发生在 binary 字面量。
- 运维注记：复跑验证时发现 worktree 的 `config/sys.local.config`
  （gitignored 本机文件，eunit-local 的 PG relay 以它为 sed 源）曾丢失，
  已从主工作区拷回；另清理了一次失败跑残留的 15432 端口 relay 进程。

## project_milestone 归因闭环（2026-10-15）

P0-11 全量 eunit 946 例中最后一例失败
`project_milestone_handler_tests:-list_success_test_` **单跑不复现**：
`IMBOYENV=local make eunit t=project_milestone_handler` → **10/10 全绿**
（2026-10-15 07:13，boot 期 migrate econnrefused 噪声不影响该套件——
纯 handler 单测走 mock 路径）。归因确认 P0-11 初判：全量轨道当时的
并行会话共享容器资源竞争，非代码问题。至此 P0-11 的 2 例非客服域
失败全部闭环（moya 例代码问题已修 e9c02322，本例环境类不复现）。

## 当前状态与待决策

- 分支 **未合并回 main、未 push**（计划约束：合并由用户另行决定）。
- 待用户决策（最终收敛）：① 分支合回 main 的时机；② 合并后 CI 全量复跑。
  （contract 收口、/utf8 扫尾、eunit 非客服域失败归因三项均已闭环，
  无遗留技术待办。）
