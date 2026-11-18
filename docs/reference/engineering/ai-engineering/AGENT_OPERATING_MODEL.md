# Agent 运营模型（Agent Operating Model）

> 版本 1.0 · 2026-11-18 · 隶属 [ai-engineering](./)
> 精简组织：不追求 50 个 Bot。一个 Agent 会话可在不同任务阶段承担多角色，唯 Worker/Verification 在验证阶段必须独立（见 [VERIFICATION_POLICY.md](./VERIFICATION_POLICY.md) §3）。

---

## 1. 六角色

| 角色 | 职责 | 关键约束 |
|---|---|---|
| **Coordinator** | 任务拆解、依赖与资源管理、worktree/锁分配 | 唯一有权调度并行度；维护任务队列 |
| **PM** | 需求澄清、任务契约（[TASK_CONTRACT.md](./TASK_CONTRACT.md) §2）、验收标准 | 契约缺验收标准不得 READY |
| **Worker** | 原子任务实现 | 单仓单写者；只改白名单范围；只能推进到 PR_CREATED |
| **Verification** | 独立执行三级验证 | 独立工作目录；不改被验证代码；写 VERIFIED/FAIL 判定 |
| **Gardener（园丁）** | 重复错误→工程规则闭环 | 优先落地为 lint/类型/schema/CI 门禁，不堆文档；新增规则须附回归验证 |
| **Human Owner（属主）** | 产品/架构/风险决策、不可逆与外部操作授权 | §5 决策队列唯一出口 |

简单任务不必全角色流转：单文件 docs 修复可 Worker 直接做完由 CI 充当 Verification；风险等级越高，角色分离越严格。

## 2. 并发与资源规则

1. 默认最多 **2~3 个活动编码 Agent**；同仓同时刻**只有一个代码写入者**。
2. 跨仓可并行（依赖满足时）；同仓多任务并行须：文件范围完全隔离 + 独立 worktree + 依赖显式登记。
3. 共享契约（API/协议/DB 迁移）变更必须**先冻结契约**（contract-gate / contract.yml / widget-pairing 是既成冻结机制）。
4. 禁止两 Agent 改同一文件；冲突资源用 `.Codex/coordination/` 既有锁（heavy-backend.lock、migration-reservations、状态 writer lock）。
5. **资源独占清单**（登记进任务 state.json）：PG fixture（如 web-mvp beam 96275@52392 存活时严禁重启其 PG）、真机设备、端口、迁移号。
6. 事故教训：GitHub installation API 配额会被多循环并发烧干——同一时刻只留一个 triage/循环类任务。

## 3. 权限边界（默认，无需逐次审批）

**允许**：读代码/文档/历史；建专用分支+隔离 worktree；改 worktree 内任务文件；本地测试；新建工程文档与脚本；授权范围内本地 commit；本地审计报告与证据。

**禁止（待属主显式授权）**：覆盖用户未提交修改；清理未知 worktree；reset 用户分支或 main；强制推送；删未知分支；自动合并 main；push 远程；改生产环境/生产 DB 迁移；改 GitHub 安全权限/分支保护；自动外发消息；泄露凭证。

**必须属主审批的代码域**（即使低风险）：DB Migration、消息协议、E2EE、用户权限/RBAC、支付退款、数据删除、生产配置、基础设施、生产部署。

## 4. 闲时执行协议（ZCode/Codex idle 任务）

每会话执行序：**Recon → 恢复检查 → Gap → 优先级 → 实施 → 测试 → 独立复审 → 证据 → 续跑检查**。

1. 先查：已有 RUN_ID / 任务锁 / STATE / worktree / checkpoint——**优先恢复未完成任务，不重复创建**。
2. 核三仓 git 状态与未提交修改；未知修改一律保护。
3. 实施于独立 worktree + 专用分支。
4. 每完成一个原子任务立即落 state.json + execution-log.md + evidence/。
5. 完成后扫描下一个可安全推进的独立任务；阻塞则存现场（BLOCKED+原因+路径）换任务。
6. 不为保持运行而制造工作；真实边际耗尽时收敛并给属主决策清单。

断点判读：PASS→SKIP_ALREADY_PASS；不一致→DRIFT 先分析；依赖未满足→BLOCKED；中断→核对现场后恢复。重试上限 3。

## 5. 属主决策队列（NEEDS_MANUAL_REVIEW 汇集处）

当前挂起项（2026-11-18）：
1. imboyapp PR#76 closed 悬空：worktree `_task_worktrees/imboyapp-pr76-merge`（7 commits 领先 origin/main）——重建 PR 或放弃，二选一。
2. geo_regions PR#4/#5 绿待合。
3. OTP 优化收尾：车道载体待属主核实（2026-11-18 晚本轮 ref 下本地无 OTP 分支，"领先 10 签收 commit"不可核实；证据在 `.task-state/otp-opt-20261112-evidence/`），push 需授权。
4. AUTOTEST v2.1 CALL-08 复测排期（依赖前任证据义务）。

新决策项由 Verification/Coordinator 追加到此表（附任务 ID 与证据路径）。

## 6. 与三仓 AGENTS.md 的关系

本模型是跨仓统一层；各仓 AGENTS/CLAUDE.md 的技术规范（分层架构、Token 化、bun --isolate、真机优先等）与"工程交付边界"段落继续有效且优先级更高。冲突时以更严格者为准。
