# IMBoy Channel-first-class + W2 Project Workspace Alpha 发布执行计划

> 版本：1.0  
> 日期：2026-08-29  
> 目标版本：后端 `1.0.0-alpha.70`、App/Admin `1.0.0-alpha.16`  
> 执行器：ZCode 多 Agent 会话  
> 当前基线：三个仓库 `main` 已具备 W0 双体验纵切；本计划只补 W2 与正式 Alpha 发布门  
> 状态口径：完成代码不等于 Release；所有人工门和发布门通过前统一标记 `release-candidate / BLOCKED`

---

## 1. 目标与不可变边界

### 1.1 最终目标

1. Channel 成为 Workspace 内一等内容资源：支持发布、订阅、评论、搜索、置顶，并可与多个同 Workspace Project 关联。
2. Project Workspace 达到 W2：Project Member 隔离、Tasks、Milestones、Pinned、Resources、Activity、Related Posts、Channel 关联全部闭环。
3. Chat 与 Workspace 共用现有 Message、Channel、Group、WebSocket、E2EE、附件内核，不产生第二套实现。
4. 三仓代码、契约、迁移、文档、测试、真机、干净部署和人工验收全部通过后，才允许准备 Alpha 标签。

### 1.2 禁止范围

- 不实现付费订阅、跨 Workspace Channel、Organization、完整 Jira/CRM、SSO/SCIM、独立文档实体。
- 不修改 `imboy/erlang.mk`、`imboyapp/ios/*`、`imboyapp/macos/*`、`imboyapp/plugin/r_upgrade`。
- 不重写 Message/Group/Channel/WebSocket/E2EE 核心。
- 不引入 Redis、Project Role、通用 RBAC、多态参与者表。
- 不自行提交、推送、打 tag、发布制品、发消息、邀请测试者或访问生产环境。

### 1.3 人工门

| Gate | 决策人 | 通过条件 | 未通过时允许动作 |
|---|---|---|---|
| H0 工作树 | 用户 | 确认三仓 Base SHA 与脏文件归属 | 只读分析、独占新文件工作 |
| H1 W2 | 用户 | 明确签认 W2 全能力与 Scope Contract | 不得落 W2 schema |
| H2 外部测试 | 用户 | 确认真机、测试账号、测试人员及联系方式 | 只能本地自动化，标 BLOCKED |
| H3 生产等价演练 | 用户 | 确认环境、数据来源和访问授权 | 只能空库/脱敏快照演练 |
| H4 Release | 用户 | 确认 git 身份、目标远端、版本、tag 与发布渠道 | 只能交付 release-ready diff |

> 本计划记录用户已选择 W2 和完整发布门，但执行会话仍须在 H0/H1 生成书面签认记录。Gate 0 真实用户价值证据尚未存在，不得伪造或用本地 Demo 代替。

---

## 2. 执行规则

### 2.1 仓库与共享工作树

- 聚合根 `/Users/leeyi/project/imboy.pub` 不是 Git 仓库。
- 后端：`/Users/leeyi/project/imboy.pub/imboy`
- App：`/Users/leeyi/project/imboy.pub/imboyapp`
- Admin：`/Users/leeyi/project/imboy.pub/imboyadmin`
- 每个任务开始必须执行 `git rev-parse --show-toplevel`、`git status --short`、`git rev-parse HEAD`。
- 不允许 `reset --hard`、`clean`、覆盖脏文件或恢复他人修改。
- 共享文件必须只有一个 Owner；其他 Agent 只提交变更请求清单。

### 2.2 Agent 输出合同

每张任务卡结束必须输出：

1. Base SHA、最终 HEAD、工作树状态。
2. 实际修改文件清单和 `git diff --stat`。
3. 执行命令、退出码、通过/失败数字。
4. 每条验收标准对应证据路径。
5. 未完成项、风险和是否触发停止条件。
6. 不得自行 commit；只给建议 commit message 和 pathspec。

### 2.3 失败与停止

- 同一失败连续 3 次：停止，报告根因与最小复现，不继续试错。
- 发现需要修改禁区、扩大范围或改变公开契约：停止，等待用户决定。
- 发现 CRITICAL/HIGH 安全缺陷：阻断后续 Release 卡，先修复并复审。
- 缺少真机、账号、生产授权或第三方确认：标 `BLOCKED`，不得用 mock/local 冒充通过。

---

## 3. 阶段、依赖与并行模型

```text
S0 基线与人工门
  └─ S1 后端 W2 schema
       ├─ S2A Project Member + 权限
       ├─ S2B Milestone
       └─ S2C Channel Rel + 聚合
            └─ S3 后端安全/归档/契约整合
                 ├─ S4A Flutter W2
                 ├─ S4B Admin W2
                 └─ S4C Demo/迁移工具
                      └─ S5 三仓总验收与文档
                           └─ S6 人工设备/部署/Release Gate
```

- 最大并行 Agent：3。
- S2A/S2B/S2C 可并行，但不得同时编辑路由、公共错误码或 Project 主 Handler；由 S3 唯一整合。
- S4A/S4B/S4C 可并行，均只消费 S3 冻结的 API Contract。
- 合并顺序固定：DB → Backend Domain/API → Contract → Flutter/Admin → Tests/Demo → Docs/Version。

---

## 4. 可派发任务卡

## ZC-00 — Preflight、Base SHA 与 Gate 记录

- **依赖**：无。
- **Owner**：总控 Agent。
- **独占文件**：新建 `docs/planning/channel-firstclass-w2-execution-ledger.md`。
- **动作**：记录三仓 Base SHA、分支、远端、脏文件；核对 W0 schema/API/UI/测试；把 W2 十二项全部标为 `now`；记录 H0/H1 状态。
- **命令**：三仓 `git status --short`、`git log -10 --oneline`；搜索 `project_member|project_milestone|project_channel_rel|project_event`。
- **验收**：不存在未知脏文件；每项 W2 能力都有当前状态和目标证据；用户原有脏文件列为保护清单。
- **停止条件**：脏文件与本计划目标重叠且无法安全隔离。

## ZC-01 — W2 数据迁移与 DB 约束

- **依赖**：ZC-00、H1。
- **Owner**：Backend DB Agent。
- **独占文件**：新的成对 migration、对应 schema tests；不编辑现有 76–78 migration。
- **TDD**：先写空库、W0 upgrade、down→up、子集约束、并发唯一性失败测试。
- **实现**：新增 `project_member`、`project_milestone`、`project_channel_rel`、`project.links`；复合 FK/唯一约束/索引；Project Member 必须来自同 Workspace active Member。
- **验收**：W0 数据无损升级；重复成员/关联只一行；跨 Workspace/removed Member 被 DB 拒绝；全部新 migration 有 down。
- **证据**：SQL、测试日志、schema 快照、EXPLAIN、锁时长。
- **停止条件**：必须重写历史 migration、无法安全回滚或生产锁预算未知。

## ZC-02 — Project Member 与 W2 授权

- **依赖**：ZC-01。
- **Owner**：Backend Membership Agent。
- **独占文件**：`project_member_*` 四层新模块及其测试；对公共 Project 文件只提交 patch 清单给 ZC-05。
- **TDD**：Owner 自动入项目、邀请/移除幂等、非 Workspace Member 失败、Owner/未完成 Task 冲突、Guest 只读、直接 ID 403、并发移除竞争。
- **实现**：成员列表/邀请/移除/Owner 转移；Project 非 Owner 访问叠加 active Project Member；Workspace Owner 保留治理权。
- **验收**：Project Member ⊆ Workspace Member 在 DB、应用事务、并发测试三层成立；失败不产生部分写入。
- **停止条件**：需要新增 Project Role 或通用 RBAC。

## ZC-03 — Milestone 完整纵切

- **依赖**：ZC-01。
- **Owner**：Backend Milestone Agent。
- **独占文件**：`project_milestone_*` 四层新模块及测试。
- **TDD**：CRUD、`planned→reached`、重复 reached 幂等、非法回退、Guest 403、无权 403、事件同事务。
- **实现**：字段只允许 name、due_date、status；写入同步产生 `project_event`。
- **验收**：状态与事件无孤儿；归档 Workspace 下拒绝写、允许读。
- **停止条件**：需求扩张到依赖、甘特图或复杂状态机。

## ZC-04 — Channel 关联与四类聚合

- **依赖**：ZC-01。
- **Owner**：Backend Aggregation Agent。
- **独占文件**：关联/聚合 Repo、DS、Logic 新模块及测试。
- **TDD**：同 Workspace 关联、跨 Workspace 400、重复幂等、并发唯一；Pinned 不含 Group Notice；Resources 不直出原始附件 URL；Activity 不含正文；稳定分页、无 N+1。
- **实现**：关联/解除 Channel；Pinned、Resources、Activity、Related Posts 四个有界查询；`project.links` 校验 name/url 数组。
- **验收**：每个聚合有空态、权限过滤、稳定游标或 page/size；SQL 查询数有上限。
- **停止条件**：需要跨 Workspace Channel 或新增 Docs 实体。

## ZC-05 — 后端 API、归档守卫与契约整合

- **依赖**：ZC-02/03/04。
- **Owner**：Backend Integration Agent。
- **唯一共享文件 Owner**：`imboy_router.erl`、Project 主 Handler/Logic、`include/error_code.hrl`、`.contract/api_contract.json`。
- **实现**：统一 REST 路由和错误语义；所有 W2 写路径接入事务归档守卫；导出 API Contract；补 Admin API。
- **安全验收**：成员、Milestone、关联、聚合、附件、WebSocket/Notice 直接入口不可绕过；personal 行为不变。
- **命令**：`make app`、针对性 EUnit、`make contract-export`、`git diff --check`。
- **停止条件**：任何 CRITICAL/HIGH、公共契约存在未决字段。

## ZC-06 — Flutter API/Model 与 W2 页面

- **依赖**：ZC-05 冻结契约。
- **Owner**：Flutter Agent。
- **独占范围**：Workspace Project 页面、相关 API/Model/Provider、路由、i18n、测试；生成错误码由 ZC-11 统一处理。
- **TDD**：成员权限、Milestone 状态、四聚合空/错/加载态、Channel 关联、403 明确错误、重复点击、Guest 只读。
- **实现**：Members、Milestones、Pinned、Resources、Activity、Related Posts、Channels；保留本机 Experience 选择但不缓存授权数据作为成功结果。
- **UI 约束**：Token 化颜色/间距/字号、44pt 点击区、暗色模式、无障碍标签、无硬编码文案。
- **验收**：targeted widget tests 全绿；`dart analyze lib` 零新增；ChatShell 回归。
- **停止条件**：需要修改 iOS/macOS 或保留插件区。

## ZC-07 — Admin W2 治理面

- **依赖**：ZC-05 冻结契约。
- **Owner**：Admin Agent。
- **独占范围**：Workspace API/types、Workspace/Project pages、对应测试和必要导航。
- **TDD**：EntityId 不丢精度、分页复位、权限错误、空态、成员/里程碑/关联治理、聚合只读。
- **实现**：Project Members、Milestones、Channel 关联管理；Pinned/Resources/Activity/Related Posts 只读；Product Experience 保持只读。
- **验收**：`bun test`、`bun run build`、`bun run lint`；关键 Playwright 旅程通过。
- **停止条件**：出现运行时配置写接口或 TSID 转 number。

## ZC-08 — Demo B W2 与迁移演练工具

- **依赖**：ZC-05。
- **Owner**：Backend Test Agent。
- **独占范围**：`scripts/demo/` 新脚本、测试辅助代码、新演练报告；不得访问生产。
- **实现**：注册→Template→四关系→Project→Task→Milestone→Channel Rel→四聚合→归档/恢复→重邀不自动恢复；支持 teardown/唯一前缀，禁止污染共享断言。
- **验收**：连续两遍 ALL PASS；断言数固定；失败退出码非零；transcript 与实际结果一致。
- **停止条件**：需要真实联系方式、生产账号或外部通知。

## ZC-09 — 后端全量与安全审查

- **依赖**：ZC-05/08。
- **Owner**：未参与后端实现的 Review Agent。
- **动作**：代码审查、安全审查、SQL/事务/授权/静默失败专项；执行全量后端测试与迁移演练。
- **验收**：CRITICAL/HIGH=0；MEDIUM 有修复或用户书面豁免；全量数字与基线对账。
- **输出**：只写新 W2 review 报告，不修改用户现有 `dual-exp-v3-security-review.md`。

## ZC-10 — 三端自动化集成验收

- **依赖**：ZC-06/07/09。
- **Owner**：Integration Agent。
- **动作**：三仓构建/测试/静态检查、Contract Gate、Admin E2E、Flutter W2 widget/integration 子集、Demo B 双遍。
- **验收**：三仓新增范围零红灯；既有红灯必须用 Base SHA 对照证明非新增，不得口头归因。
- **输出**：命令、退出码、测试数、耗时和日志路径表。

## ZC-11 — 契约、版本与发布文档

- **依赖**：ZC-10 全绿。
- **Owner**：Release Docs Agent。
- **唯一共享文件 Owner**：版本文件、CHANGELOG、API Catalog、部署/升级/回滚、最终 acceptance。
- **实现**：再生 App 错误码；更新后端 `1.0.0-alpha.70`、App/Admin `1.0.0-alpha.16`；清除 W0 完成声明，历史报告保留历史标记；产出当前 HEAD W2 验收报告。
- **验收**：文档没有伪造真机/生产/客户证据；版本只在全部自动化通过后修改。
- **停止条件**：ZC-10 未全绿或版本号与用户选择不一致。

## ZC-12 — 真机、真人、干净部署与 Release Gate

- **依赖**：ZC-11、H2、H3。
- **Owner**：独立 Acceptance Agent + 人工操作者。
- **动作**：两台真机 Demo A/B；Push/音视频/附件/升级提示；3 人 30 秒理解测试；未参与实现者干净部署；生产等价数据量迁移与回滚演练。
- **验收**：阻断缺陷 0；可告知缺陷全部入册；所有证据含设备、网络、版本、HEAD、时间、结果。
- **Release 判定**：全部通过后输出 `READY_FOR_ALPHA_RELEASE`；否则输出 `BLOCKED` 和精确缺口。
- **外向操作**：tag、push、制品上传、公告必须停在 H4，等待用户确认 git 身份和目标远端。

---

## 5. API 与数据契约冻结表

| 资源 | 最小接口 | 关键约束 |
|---|---|---|
| Project Member | list/invite/remove/transfer-owner | active Workspace Member；无 Project Role |
| Milestone | list/create/update/reach | planned→reached；重复 reach 幂等 |
| Project Channel | list/link/unlink | 同 Workspace；重复 link 幂等 |
| Pinned | list | 只聚合关联 Channel 置顶帖；不含 Group Notice |
| Resources | list/update-links | 授权附件 URL；links=name+url |
| Activity | list | 只含系统事件；稳定分页；不含正文 |
| Related Posts | list | 与 Activity 分离；有界摘要 |

所有 TSID 后端仍以 JSON integer 输出；Flutter 使用既有安全解析模型；Admin 必须使用 `EntityId`，禁止 `Number/parseInt`。

---

## 6. 最终证据矩阵

| 能力 | Schema | Backend | Flutter | Admin | Tests | Demo | 人工证据 |
|---|---:|---:|---:|---:|---:|---:|---:|
| Project Member W2 | 必须 | 必须 | 必须 | 必须 | 必须 | 必须 | 权限走查 |
| Milestone | 必须 | 必须 | 必须 | 必须 | 必须 | 必须 | 真机 |
| Channel Rel | 必须 | 必须 | 必须 | 必须 | 必须 | 必须 | 真机 |
| Pinned | 零新增表 | 必须 | 必须 | 只读 | 必须 | 必须 | 真机 |
| Resources | links 列 | 必须 | 必须 | 只读 | 必须 | 必须 | 附件真机 |
| Activity | 复用 event | 必须 | 必须 | 只读 | 必须 | 必须 | 真机 |
| Related Posts | 零新增表 | 必须 | 必须 | 只读 | 必须 | 必须 | 真机 |
| 正式 Alpha Release | — | 全绿 | 全绿 | 全绿 | 全绿 | 双遍 | H0–H4 |

缺少任一“必须”证据时，该行不得标完成。

---

## 7. 建议提交切片与合并顺序

> Agent 只建议，不自行 commit。每次提交前由用户确认 git author/committer 身份。

1. `feat(db): add W2 project membership and channel-firstclass schema`
2. `feat(project): enforce W2 membership and ownership boundaries`
3. `feat(project): add milestones and project event flow`
4. `feat(channel): add project relations and bounded aggregates`
5. `test(project): close W2 authorization archive and migration matrix`
6. `feat(app): add complete W2 project workspace experience`
7. `feat(admin): add W2 project workspace governance`
8. `chore(contract): publish W2 API and error-code contracts`
9. `docs(release): prepare alpha.70 alpha.16 acceptance evidence`

---

## 8. ZCode 总控提示词

```text
你是 IMBoy W2 发布总控 Agent。请严格执行：
/Users/leeyi/project/imboy.pub/imboy/docs/planning/imboy-channel-firstclass-w2-alpha-release-execution-plan.md

目标：完成 Channel-first-class、W2 Project Workspace，并收敛到可人工批准的 Alpha Release。

强制规则：
1. /Users/leeyi/project/imboy.pub 是聚合目录，不是 Git 仓库。每次进入 imboy、imboyapp、imboyadmin 后先执行 git rev-parse --show-toplevel、git status --short、git rev-parse HEAD。
2. 保护所有既有脏文件，禁止 reset/clean/checkout 覆盖、禁止修改不属于任务卡 OWN 的文件。
3. 按 ZC-00→ZC-12 执行；最多 3 个并行 Agent；严格遵守依赖、独占文件和固定合并顺序。
4. 每张卡测试先行：先产生可证明新行为缺失的失败测试，再实现到绿，再重构。
5. 公共路由、错误码、契约和版本文件只能由计划指定的唯一 Owner 编辑。
6. 不得自行 commit、push、tag、发布、联系/通知第三方、访问生产或选择远端。提交前停在 Gate 2，给用户 diff、测试证据、建议 commit message 和 pathspec。
7. Gate 0/真机/真人/生产等价演练缺证据时必须标 BLOCKED；禁止用 mock、API、本机或文档替代。
8. 同一失败连续 3 次即停止；CRITICAL/HIGH 未清零禁止进入 Release 卡。
9. 每张卡输出 Base SHA、最终状态、修改文件、命令与退出码、测试数字、验收证据、残余风险、下一张可启动卡。
10. 最终只有 H0-H4、三仓全绿、迁移、双机、真人和干净部署全部通过，才输出 READY_FOR_ALPHA_RELEASE；否则输出 BLOCKED，不得声称正式 Release。

现在只执行 ZC-00。完成后停止，向用户展示 execution ledger、保护文件清单、W2 Scope Contract 和 H0/H1 待确认项；得到人工确认后再启动 ZC-01。
```

