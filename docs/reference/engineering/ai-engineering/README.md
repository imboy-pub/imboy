# AI 工程体系（ai-engineering）

> IMBoy 三仓（imboy / imboyapp / imboyadmin）AI 工程交付体系的统一文档层。
> 建立于 2026-11-18 · 基线 imboy `259f5f480` / imboyapp `5ae7b5f2b` / imboyadmin `5a8925a` · 首个任务包示范：`_task_packages/AI_ENGINEERING_V1/`

## 文档清单

| 文档 | 职责 | 读者 |
|---|---|---|
| [ASSESSMENT.md](./ASSESSMENT.md) | 三仓工程体系现状与差距审计（真实运行 vs 规划漂移） | 所有人首读 |
| [TASK_CONTRACT.md](./TASK_CONTRACT.md) | 统一任务契约：字段、状态机、state.json 最小 schema、任务包结构 | PM / Worker |
| [VERIFICATION_POLICY.md](./VERIFICATION_POLICY.md) | 三级验证、证据标准、独立性规则、反假通过清单 | Verification / Worker |
| [AGENT_OPERATING_MODEL.md](./AGENT_OPERATING_MODEL.md) | 六角色、并发与资源规则、权限边界、闲时执行协议、属主决策队列 | 所有人 |
| [AUTO_MERGE_POLICY.md](./AUTO_MERGE_POLICY.md) | 风险分级、自动合并必要条件、扩展路径（当前仅 dependabot 白名单运行） | 属主 |
| [ENGINEERING_MAP.md](./ENGINEERING_MAP.md) | 九张知识地图（指针式）+ 仓库外瞬态结构锚定 | 所有人 |
| [四仓 ECC Prompt Library](../../../../prompts/README.md) | 72 个可复制 ECC 使用案例的统一索引与跨仓分配 | 开发者 / Agent |

## 与现有文档的关系

- 本目录**不取代** `docs/reference/engineering/` 既有 10 个工程专项 md（ci-notes 等）——ENGINEERING_MAP 以指针引用它们。
- 三仓各自 AGENTS.md/CLAUDE.md 的技术规范与"工程交付边界"优先级更高；本层是跨仓统一层。
- 任务包瞬态目录（`_task_packages/` 等）不入 git；其结构约定见 TASK_CONTRACT §1。
- 实施进度、待办队列和会话接力提示词保存在任务包中，不作为产品文档入库。通用恢复流程见 [TASK_CONTRACT §7](./TASK_CONTRACT.md)。
- 首批任务的历史材料仍可从固定提交查看：[实施计划](https://github.com/imboy-pub/imboy/blob/b9324af3b03f6e3ccb477d831f2191e95aaa0d34/docs/reference/engineering/ai-engineering/PLAN.md)、[接力提示词](https://github.com/imboy-pub/imboy/blob/b9324af3b03f6e3ccb477d831f2191e95aaa0d34/docs/reference/engineering/ai-engineering/RESUME_PROMPT.md)。其中的分支、工作树和任务状态仅代表当时现场，恢复时须重新核验。P1-3 的长期设计约定保留在 [VERIFICATION_POLICY §6](./VERIFICATION_POLICY.md)。

## 维护规则

- 任务实施状态随每批任务更新到任务包的 `runtime/state.json` 与 `execution-log.md`；ASSESSMENT §6 在途快照随大事件更新（标注日期）。
- 修改本目录文档 = R0 变更：走标准 PR，注释级/事实修正可直接实施。
- 新决策项一律追加进 AGENT_OPERATING_MODEL §5 属主决策队列，不散落。
