# 限流中断与恢复日志

> 事件时间：2026-09-09 18:5x–19:0x
> 记录人：Coordinator

## 事件

Wave 2 四个 Agent（A/B/C/D）恢复/派发后全部因模型请求失败（限流）中断，运行时长 67–152 秒不等。无 Agent 产出完整 handoff；部分成果已落盘（见下）。

## 现场取证（只读命令）

取证时间约 19:0x，命令与结果：

| 仓库 | 检查 | 结果 |
|---|---|---|
| imboy | `git rev-parse HEAD` | `5b7e2055f71087930eb4e896d1ce0f895140d614`（不变，无 commit） |
| imboy | `git status --short` / `git diff --cached --name-only \| wc -l` | **60 个文件 staged（A/M）** |
| imboy | `git status --short \| grep -v '^A '` | 仅 `M  include/error_code.hrl`（Agent B 教学错误码工作产物） |
| moya | `git rev-parse HEAD` | `9644a2e8...`（不变，无 commit） |
| moya | `git status --short` | 无 staged；README 为未暂存 M，其余 ??（**未污染**） |
| moya | `ls src/core/` | env/errors/platform/request/types.ts 五件（D 的 Step 13 中间态） |

### Git index contamination（imboy）

60 个 staged 文件构成：
1. 任务开始前已存在的用户 dirty 文件 8 个：`docs/planning/imboy-next-long-running-functional-*` ×2、`docs/plans/2026-09-08-iot-smart-home.md`、`docs/plans/2026-09-09-imboy-zcode-idle-functional-closure.md`、`docs/plans/2026-09-09-moke-calligraphy-homework-pilot-design.md`、`docs/plans/2026-09-09-moya-calligraphy-ai-review-{execution-plan,orchestrate}.md`、`test/logic/temp_probe2_tests.erl`
2. 本次 Wave 1 全部产物（STEP-01..12 证据、合规/UX 文档、迁移 95/96/97、moya_teaching_migration_tests.erl）
3. `include/error_code.hrl`（M，Agent B 的教学错误码合法修改）

违例性质：违反"禁止 git add / 不得混入既有 dirty 文件"边界。执行主体无法确证（Wave 2 中断前 B 在 imboy 工作）。处置：**不 unstage、不 commit、不 reset/restore/clean/stash**，保留现场待用户决定，最终报告如实披露。

### 契约缺口（Step 4 契约 vs 迁移 00000097）

- `homework_submission` 缺 `idempotency_key`、`request_digest`、`withdrawn_at`、`withdrawn_by`
- 缺 `(submitted_by, assignment_id, idempotency_key)` 有效唯一约束
- `attempt_no` MAX+1 分配的并发串行化（assignment 行锁或等价）
- 撤回与发布并发互斥的表达

待 Agent B 审计坐实（STEP-08/schema-gap.md）→ Agent C 出 00000098 修复迁移。

## 恢复决策（按用户指令执行）

- 并发上限 4→2；首次恢复单请求；冷却已满足（取证+登记耗时 >180s）
- R1：19:07 恢复 D（仅完成 Step 13）；19:09 观察 120s 无失败后恢复 B（Step 8→独立 handoff→schema audit）
- R2：schema gap 坐实后恢复 C（00000098 修复迁移，含 up/down/up 与并发测试）
- R3：Coordinator 接受 Step 8+DB 修复后，B 完成 Step 9（无 AI 人工闭环优先）
- A 的 Wave 2（页面规格/错误文案）不恢复，非阻塞项
- 后续收缩：不新建 Parent/Teacher Agent，D 串行承担；Step 11 受 vision=false 限制预计 PARTIAL/BLOCKED_EXTERNAL；Step 16-18 视剩余时间
- 再限流：降单 Agent 串行 + 等 ≥5 分钟；第三次连续限流 → 停止派发，输出 PARTIAL 报告

## Agent 恢复请求记录

| 时间 | Agent | 批次 | 任务 | 状态 |
|---|---|---|---|---|
| 19:07 | D | R1 | 续作完成 Step 13（修 Biome→最小公共壳→check/build→handoff 即停） | 后台运行中 |
| 19:09 | B | R1 | Step 8 完成并独立 handoff → schema audit（不改迁移，缺口写 STEP-08/schema-gap.md） | 后台运行中 |
