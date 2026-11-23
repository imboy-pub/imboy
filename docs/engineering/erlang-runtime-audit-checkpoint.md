# 审计检查点 / CHECKPOINT

> 最后更新：2026-11-23 · 会话续接三件套之一（RESUME.md → STATUS.json → 本文件）

> 🔁 **R0 已接手（2026-11-23）**：当前检查点改为 `r0-capacity-checkpoint.md`；本文件为第一轮审查的历史快照，原样保留。

## 当前阶段

**第一轮（只审查）已完成，等待属主授权是否进入修复阶段。**

## 已完成

- Phase A 基线：main@`a5c74219f`，dirty 仅既有 `docs/research/`（未触碰）；OTP 29 本地 / CI 29+28；Cowboy 2.19.0 / Ranch 2.3.0。
- 资料：15/15 章精读（clone 至 /tmp/lerc-20261123，仅本会话参考用）。
- Phase B 监督树/进程 → `erlang-process-health-audit.md`（PB-1/PB-2 + spawn/receive 全量定性）。
- Phase C Binary/ETS → `erlang-memory-ets-audit.md`（红旗零热路径命中 + 20 表归属定性 + 17 处 persistent_term 定性）。
- Phase D 测试 → `erlang-testing-gap-analysis.md`（两缺口：投递状态机 PBT、断连混沌编排）。
- Phase E 可观测性 → `erlang-observability-audit.md`（OBS-1/2/3）。
- Phase F 性能 → `erlang-performance-audit.md`（P1-1 容量悬崖全要素 + hibernate 待实证）。
- 交付 10/10 文件落 `docs/engineering/`。
- OTP 官方文档核对 3 项（+P 默认值、terminate/trap_exit、cowboy terminate-on-crash）——第 3 项**推翻教程类推**，以 vendored 2.19 源码为准。

## 已发现问题（计数）

P0×0 · P1×1（容量悬崖）· P2×6 · P3×7 · 强项确认×8 · 教程式误报排除×3。明细见 STATUS.json `findings`。

## 未完成任务（均需属主授权）

1. P1-1 修复（vm.args +P/+Q + max_connections 设界 + bench 阶梯）——**必须与 +S 同批评审**。
2. OBS 批次（system_monitor + metrics 维度 + 重启计数 + 告警）。
3. P2-4 生产默认 auto_migrate=false 固化。
4. bench_ws_throughput.sh 补齐（先测后改，P2-5/6 的前提）。
5. P3 微修三处（catch×2、worker_loop 兜底）。
6. 二轮加查：15 个嵌套 call 候选静态调用图；binary:part 抽样 referenced_byte_size。

## 测试结果

本轮**未运行**任何构建/测试（review-only；不触碰生产配置与既有基线）。发现的验证手段（复现命令）已写入各专项报告。

## 当前 Git 状态

- 分支 main @ `a5c74219f`（领先 origin/main 56，未推送——属既有状态，非本审计造成）。
- 本轮新增：`docs/engineering/` 10 个文件（未提交，未暂存）。属主可见性：`git status` 即现；回滚 = 删除该目录。
- 未跟踪 `docs/research/`：审计开始前即存在，保持原样。
- worktree：6 个任务车道 worktree 存在（.task-state/、_task_worktrees/），与本审计无关，未触碰。

## 下一步动作（默认）

等待属主对 `erlang-runtime-review.md` §5 修复优先级的授权。若授权 P1-1：先改 vm.args + websocket_options + install.sh 注入通道 → 本地 `make compile && make eunit`（聚焦 websocket_options 测试）→ CONNS 阶梯 bench → 独立 commit（可单独回滚）。
