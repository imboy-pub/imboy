# AI 工程体系增量实施计划（AI Engineering Plan）

> 版本 1.0 · 2026-11-18 · 隶属 [ai-engineering](./) · 输入：[ASSESSMENT.md](./ASSESSMENT.md)
> 与已有计划的关系：本计划只覆盖 **AI 工程交付体系** 维度；业务功能、质量工程 LP、AUTOTEST、geo_regions 等既有车道不在此重复（见 ASSESSMENT §6 在途快照）。

---

## P0（保护与止损，本批已实施 ✅ / 待属主 ◻）

| # | 项 | 现状→问题 | 位置 | 实施状态 |
|---|---|---|---|---|
| P0-1 | 统一任务契约模板 | 15 任务包 4 种模式、execution-log.md 全缺 → 断点恢复成本高 | `docs/reference/engineering/ai-engineering/TASK_CONTRACT.md` + 示范包 `_task_packages/AI_ENGINEERING_V1/` | ✅ 本批落地 |
| P0-2 | 独立验证政策 | Worker 自证风险、证据格式三种并存 | `…/VERIFICATION_POLICY.md` | ✅ 本批落地 |
| P0-3 | 失效指针修复 | IMBOY_ERLANG_QUALITY/RUNTIME_LOCATION.json 指向已删目录 → 未来会话按错指针找状态 | `_task_packages/IMBOY_ERLANG_QUALITY/RUNTIME_LOCATION.json` | ✅ 本批修复 |
| P0-4 | quality-loop 引用漂移 | 4 处注释引用已收编删除的计划文件 | `quality.yml:3`、`pull_request_template.md:3`、`codemap.yml:3,109`（sonar.yml 已禁用不动） | ✅ 本批修复（YAML 语义零变化；codemap 周报 issue 文案按预期更新） |
| P0-5 | 保护在途任务与用户工作 | PR#76 悬空 worktree、AUTOTEST BLOCKED 态、staged docker-compose.yml | 登记于本计划 + AGENT_OPERATING_MODEL §5 决策队列 | ✅ 登记完成；处置 ◻ 待属主 |

**P0 验收标准**：新任务有模板可抄；三个历史失效点消除；在途风险全部有登记与属主出口。

## P1（能力建设，待授权排期）

| # | 项 | 现状→问题 | 位置 | 依赖 | 可自动实施 |
|---|---|---|---|---|---|
| P1-1 | 验证证据机器可读化 | receipts/ledger/log 三格式无法汇总 Verified Delivery Rate | 任务包 evidence/ 增加 `verdict.json`（最小 schema：task/verdict/sha/cmd/exit_code/counts） | P0-1 | ✅ 低风险 |
| P1-2 | 园丁机制成文+首批规则 | 缺陷→规则闭环靠人脑；重复错误（bun --isolate、批次连坐、陈旧 beam）已有解法但未固化入 CI | `…/AGENT_OPERATING_MODEL.md` §1 已定义角色；首批候选：验证反假通过清单进 PR 模板自查 | 属主批准 PR 模板改动 | ◻ 需 review |
| P1-3 | 跨端契约扩展 | error_code 逐字门已冻结；API 行为/WS 契约无自动门 | imboy contract-gate.yml 扩展（参考 AUTOTEST 车道用例） | AUTOTEST v2.1 解 BLOCKED | ◻ 依赖在途 |
| P1-4 | 工程地图活文档化 | 本批 ENGINEERING_MAP 是快照；需随大版本刷新 | 修订责任 = 每次三仓版本 bump 的 PR 自查项（加进 PR 模板一行） | P1-2 | ◻ 需 review |
| P1-5 | 每日/每批交付报告 | 属主需要高频盘面（已证偏好） | 任务包 evidence/ + 决策队列生成 markdown 摘要 | P1-1 | ✅ 脚本级 |

## P2（自动化扩展，全部待属主授权后另立任务）

- CI 失败自动归因（triage bot，注意 installation API 配额单循环约束）。
- 低风险自动合并扩展（R0 label 触发，imboyadmin 试点）→ 前置 [AUTO_MERGE_POLICY.md](./AUTO_MERGE_POLICY.md) §4。
- 任务自动调度（Coordinator cron 化）。
- 成本/效果指标看板（Verified Delivery Rate、首次验证通过率、返工率、人工介入时间、token 成本）。

## 实施纪律（每项通用）

1. 均走 TASK_CONTRACT 流程：专用分支 worktree → 实施 → 独立验证 → 证据落包 → 属主 review。
2. R3 域（DB/协议/E2EE/RBAC/支付/生产）永不自动；docs/注释级可直接实施（本批 P0-3/4 即此类）。
3. 每项完成即更新本表"实施状态"列 + execution-log；半成品必须留 BLOCKED 现场。

## 风险与回滚

- 本批全部变更为 docs + 任务包瞬态文件 + 注释级修正：回滚 = revert 单 commit / 删任务包目录，无运行时影响。
- 未来 P1-3（契约门扩展）触及 CI：回滚 = 关 workflow 入口，独立 PR。
