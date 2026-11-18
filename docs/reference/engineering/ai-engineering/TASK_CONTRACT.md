# 统一任务契约（Task Contract）

> 版本 1.0 · 2026-11-18 · 隶属 [ai-engineering](./README.md)
> 定位：三仓所有 AI Agent 任务的**最小统一契约**。不是新状态系统——它收敛既有的 AUTOTEST v2.1 STATE schema v3（`_task_packages/IMBOY_AUTOTEST_V2.1/runtime/STATE.json`）与 .task-state 实践，补齐历史包缺失的 `execution-log.md`。

---

## 1. 任务包目录结构（标准形）

新任务一律在 `_task_packages/<TASK_ID>/`（跨仓任务）或仓内专用车道建立，结构如下：

```
<TASK_ID>/
├── README.md            # 任务一句话定位 + 契约字段摘要（人读入口）
├── runtime/
│   ├── state.json       # 唯一活跃状态（单写权，见 §3）
│   └── .state-writer.lock  # 运行时生成（executor 加锁时创建），不属必备交付物
├── execution-log.md     # 追加式执行流水（每原子动作一行，禁改写历史行）
├── evidence/            # 验证证据（命令、退出码、日志路径、判定），按 VERIFICATION_POLICY.md 归档
└── RESUME_PROMPT.md     # 接力提示词（可引用本仓 ai-engineering/RESUME_PROMPT.md 通用部分）
```

历史包不回溯改造；新包从本模板起。`_task_packages/AI_ENGINEERING_V1/` 是首个示范。

## 2. 契约必填字段（README.md 或 state.json）

| 字段 | 说明 |
|---|---|
| Task ID | 全局唯一，前缀区分仓/域（如 `AI_ENGINEERING_V1`） |
| 目标与业务背景 | 为什么做、解决什么真实问题 |
| 所属仓库 | imboy / imboyapp / imboyadmin / 跨仓 |
| 输入上下文 | 相关代码路径、文档、前置任务 |
| 允许修改范围 | 目录/文件白名单（越界=违约） |
| 禁止修改范围 | 保留区（vendored、ios/*、plugin/r_upgrade 等）+ 在途他人车道 |
| 前置依赖 | 其他任务 ID / 迁移号 / 环境条件 |
| 验收标准 | 可判定的行为断言，不是"完成了" |
| 测试命令 | 精确到可复制执行（含 env） |
| 风险等级 | LOW / MEDIUM / HIGH（HIGH 必须属主审批实施） |
| 回滚策略 | 分支删除 / revert SHA / 迁移降级路径 |
| 交付证据 | evidence/ 内路径清单 |
| 当前状态 | §4 状态机之一 |

## 3. 单写权规则（继承 AUTOTEST 实践）

- `runtime/state.json` 是**唯一**活跃状态；旧态只读留存，不并存两份活跃状态。
- 换 executor：先确认旧 owner 停止 → 记录一次 adoption → 原子切换。
- 中断续原操作：先读 state.json + execution-log.md 末行 + evidence 清单，对齐后再动手。

## 4. 状态机

```
DISCOVERED → READY → ASSIGNED → IMPLEMENTING → PR_CREATED → VERIFYING → VERIFIED → MERGED
                                                  │
异常：BLOCKED / FAILED / NEEDS_MANUAL_REVIEW / CANCELLED（任一状态可进，需记原因）
```

**判定纪律**：
- 状态只能由证据驱动推进；`VERIFIED` 只能由独立验证（VERIFICATION_POLICY.md）写入，Worker 不得自证。
- `BLOCKED` 必须附：阻塞点、依赖方、已保存的现场路径。
- 断点恢复判读：`PASS`→SKIP_ALREADY_PASS；代码与预期不符→DRIFT（先分析）；依赖未满足→BLOCKED；进程中断→核对 worktree+evidence 后恢复。禁止仅因旧状态记录失败就全量重跑；重试上限 3 次，连续失败记根因。

## 5. state.json 最小 schema

```json
{
  "task_id": "…", "schema": "ai-eng-task-state/1",
  "phase": "IMPLEMENTING",
  "repos": {"<repo>": {"base_sha": "…", "branch": "…", "worktree": "…", "touched": true}},
  "protected_user_work": ["…"],
  "deliverables": {"git_tracked": "…", "task_package": "…", "fixes": ["…"]},
  "verification": {"level1_static": "PASS|FAIL|PENDING", "level2_dynamic": "…", "level3_acceptance": "…", "verdict": "PASS|FAIL|NEEDS_MANUAL_REVIEW|PENDING", "evidence_paths": ["…"]},
  "constraints": ["…"], "history": [{"ts": "…", "event": "…"}]
}
```

大型长周期任务（如 AUTOTEST 类）可扩展 cards/receipts 字段，但不得删减上述最小集。

## 6. 并发与资源登记

- 每任务开工前在 state.json 登记：占用 worktree、分支、设备、端口、PG fixture、BEAM 节点。
- 冲突资源（同仓写入、迁移号、真机）用 `.Codex/coordination/` 既有锁机制登记，不另建。
- 同仓同一时间只允许一个代码写入者；跨仓并行需依赖明确且文件范围隔离。
