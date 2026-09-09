# STEP-01 基线冻结报告

> 采集时间：2026-09-09 18:17:23 +0800
> 采集人：Coordinator（ZCode 多 Agent 会话）
> 采集方式：只读命令，未执行任何写操作

## 1. 聚合工作区

- `/Users/leeyi/project/imboy.pub` → `git rev-parse --show-toplevel` 返回 `fatal: not a git repository`（退出码非 0）
- 结论：聚合工作区**不是** Git 仓库，符合伞形工作区设定。

## 2. imboy（后端与产品主仓）

| 项 | 值 |
|---|---|
| Git root | `/Users/leeyi/project/imboy.pub/imboy` |
| HEAD | `5b7e2055f71087930eb4e896d1ce0f895140d614` |
| branch | `main` |
| remote | `gitcode` (git@gitcode.com:imboy/imboy.git)、`gitee` (git@gitee.com:leeyi/imboy.git)，均只读记录，本次不 push |
| dirty state | 8 个未跟踪文件，0 个已跟踪文件被修改 |

未跟踪文件清单（默认归属用户/其他会话，本次不清理、不覆盖、不暂存）：

```
?? docs/planning/imboy-next-long-running-functional-orchestrate-2026-09-08.md
?? docs/planning/imboy-next-long-running-functional-plan-2026-09-08.md
?? docs/plans/2026-09-08-iot-smart-home.md
?? docs/plans/2026-09-09-imboy-zcode-idle-functional-closure.md
?? docs/plans/2026-09-09-moke-calligraphy-homework-pilot-design.md   ← 旧讨论草案，仅历史来源
?? docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md ← 本次权威计划（真源）
?? docs/plans/2026-09-09-moya-calligraphy-ai-review-orchestrate.md   ← 编排参考
?? test/logic/temp_probe2_tests.erl
```

## 3. moya（墨芽微信小程序独立仓）

| 项 | 值 |
|---|---|
| Git root | `/Users/leeyi/project/imboy.pub/moya` |
| HEAD | `9644a2e804ecabc232fd313d87f7825339b4aceb`（非 UNBORN，已有 1 个提交，内容为 README.md） |
| branch | `main` |
| remote | 无 |
| dirty state | 2 个未跟踪文件：`moyalogo_144X144.png`（21972B）、`moyalogo_256X256.png`（68291B） |
| 已有文件 | `README.md`（742B）+ 两个未跟踪 logo PNG |

## 4. 数据库迁移基线

- 最新迁移：`00000094_moderation_appeal`（.up.sql/.down.sql 齐全）
- **下一组迁移编号从 `00000095` 开始**，由 DATABASE Agent 独占分配，执行前必须重新 `ls priv/migrations/ | sort | tail` 复核。
- 历史迁移 `00000076_workspace_foundation.up.sql:8` 确实写有"I11：不引入 Organization 层"——与计划 §3.2 一致；该文件为历史事实，本次不修改，通过新增迁移演进。

## 5. Acceptance 判定

| ID | 判定 | 证据 |
|---|---|---|
| BASE-01 | **PASS** | 两个独立 Git root（§2/§3），聚合根 `git rev-parse` 失败（§1），命令与退出码见 commands.md |
| BASE-02 | **PASS** | 权威计划 §6.1/§6.2 明确 `Organization └─ Workspace └─ Group/Class` 与 `learner.organization_id`；旧草案 `2026-09-09-moke-calligraphy-homework-pilot-design.md` 仅标记为讨论来源（计划头部第 13 行） |
| BASE-03 | **PASS** | 本 Step 仅执行只读命令（git rev-parse/status/branch/remote、ls、grep、date），无提交、推送、远端创建、author 修改、数据库操作、生产操作、外部平台写操作 |

## 6. 决策台账（引用真源，不复制维护）

D-01 至 D-14 冻结决策以权威计划 §2 为唯一真源，本报告不另行维护副本以防漂移。
废弃工作名"墨课"仅存于旧草案（历史来源，不用于新界面/新文档标题）。

## 7. 本次允许修改的路径（全局授权边界）

| 仓库 | 允许新增/修改 | 绝对禁止 |
|---|---|---|
| imboy | `priv/migrations/00000095+`（仅 DATABASE Agent）；`src/api|logic|ds|repo/*teaching*` 新文件（仅 API-ACL Agent）；`src/imboy_router.erl` 教学路由段（仅 API-ACL Agent）；教学错误码（仅 API-ACL Agent）；对应新测试文件；`docs/plans/evidence/moya-calligraphy-ai-review/**`（各 Step 独立目录）；`docs/plans/` 下本计划新增交付文档 | 历史迁移、`erlang.mk`、§2 列出的既有未跟踪文件、生产配置 |
| moya | 根配置、工程文件、页面、组件、测试（按 MOYA/PARENT/TEACHER 分工） | `.git/`、真实 AppID/secret、提交推送 |

## 8. 证据命名约定

`docs/plans/evidence/moya-calligraphy-ai-review/STEP-XX/{commands,tests,notes,...}.md`，真实 PII 不得入库。
