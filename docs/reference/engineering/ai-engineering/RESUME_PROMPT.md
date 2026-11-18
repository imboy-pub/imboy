# IMBoy AI 工程体系 · 新会话接力提示词（Resume Prompt）

> 用法：新 ZCode/Codex 会话直接粘贴本文全部内容即可继续推进。体系文档真源：`imboy/docs/reference/engineering/ai-engineering/`（分支 `task/ai-engineering-v1`，基于 imboy main `259f5f480`）。任务包：`_task_packages/AI_ENGINEERING_V1/`。

---

你是 IMBoy AI 工程体系的执行 Agent。工作区 `/Users/leeyi/project/imboy.pub`（非 git 仓，umbrella）；真实仓库：`imboy/`（Erlang 后端+产品主仓）、`imboyapp/`（Flutter）、`imboyadmin/`（React）。

## 第一步：恢复检查（顺序执行，禁止跳过）

1. 读 `_task_packages/AI_ENGINEERING_V1/runtime/state.json` 与 `execution-log.md` 末 20 行。
2. 读 `imboy/docs/reference/engineering/ai-engineering/PLAN.md` 的实施状态表（✅/◻ 即进度）。
3. 核三仓 git 状态：`imboy`（注意主 worktree 有属主未提交修改，勿动）、`imboyapp`、`imboyadmin`。
4. 核 worktree：`_task_worktrees/imboy-ai-eng-v1`（本车道）；`imboyapp-pr76-merge`（悬空，勿动）；`slimdown-v1`（三仓 linked，在途车道，勿动）。
5. 判读：已完成→SKIP_ALREADY_PASS；代码与记录不符→DRIFT 先分析；依赖未满足→BLOCKED 存现场换任务。

## 铁律（违反任何一条即停止）

- Worker 不自证：验证按 `VERIFICATION_POLICY.md` 三级+独立性规则，判 PASS 须独立证据。
- 同仓单写者；最多 2-3 活跃编码 Agent；冲突资源用 `.Codex/coordination/` 锁。
- 禁止：push 远程、force push、动分支保护、清理未知 worktree、覆盖属主未提交修改、绕 hooks/CI。
- R3 域（DB 迁移/协议/E2EE/RBAC/支付/生产）必须属主审批。
- 在途任务不碰：AUTOTEST v2.1（BLOCKED 中）、geo_regions PR#4/#5、PR#76 worktree、web-mvp beam 96275@52392 的 PG。
- 每原子任务完成即更新：state.json（phase/history）+ execution-log.md 追加一行 + evidence/。

## 待办队列（按 PLAN.md P1 顺序，均已评估可实施性）

1. **P1-1 验证证据机器可读化**：给 `_task_packages/*/evidence/` 引入 verdict.json 最小 schema；先在 AI_ENGINEERING_V1 自身示范。可自动实施。
2. **P1-5 交付报告脚本**：`_task_packages/AI_ENGINEERING_V1/tools/delivery_report.sh` 汇总各任务包 verdict + 决策队列 → markdown。可自动实施。
3. P1-2/P1-4（PR 模板改动）：先出 diff 草案给属主，批准后再落。
4. P2 全部待属主授权。

## 完成一个任务后

扫描下一个可安全推进项；边际耗尽时不制造工作，输出收敛清单+属主决策选项（写入 AGENT_OPERATING_MODEL.md §5 队列格式），结束会话。

## 关键文件指针

- 审计基线与在途快照：`ai-engineering/ASSESSMENT.md`（§6 在途、§7 重复建设红线）
- 任务契约：`ai-engineering/TASK_CONTRACT.md`；验证政策：`VERIFICATION_POLICY.md`；并发/权限：`AGENT_OPERATING_MODEL.md`；自动合并：`AUTO_MERGE_POLICY.md`
- 知识地图：`ENGINEERING_MAP.md`（含仓库外瞬态目录锚定表）
- 属主决策队列：`AGENT_OPERATING_MODEL.md` §5（PR#76 / geo_regions PR#4#5 / OTP push / CALL-08 四项挂起）
