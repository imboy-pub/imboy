# 墨芽习字：AI 视频回课产品执行与验收计划

> 状态：本地实现已收口，外部验收待完成（2026-09-10 复核：14 PASS / 4 PARTIAL）
>
> 日期：2026-09-09
>
> 当前基线复核：2026-09-10
>
> 产品名称：墨芽习字，简称“墨芽”
>
> 产品主仓：`/Users/leeyi/project/imboy.pub/imboy`
>
> 微信小程序独立仓：`/Users/leeyi/project/imboy.pub/moya`
>
> 讨论来源：`docs/plans/2026-09-09-moke-calligraphy-homework-pilot-design.md`

## 1. 目标与完成定义

面向书法培训机构，把当前“家长在微信群发约一分钟练字视频，老师逐条观看并拍视频回复”的流程改造成结构化、默认私密、可追踪的回课闭环：

1. 家长在墨芽小程序中替低龄学员查看作业并提交练字视频和可选作品照片。
2. IMBoy 后端保存作业、提交、附件、权限和状态，并异步生成 AI 点评草稿。
3. 老师在同一个墨芽小程序中查看待点评队列、核对 AI 观察点、录制或上传真人回评。
4. 只有老师主动审核发布后，监护人才能看到回评；AI 不得自动给家长发结论。
5. 首先完成“你姐姐所在机构、一个真实班级”的试点，但数据边界支持一个 Organization 下多个 Workspace。

本计划的工程完成不等于正式上线。完整完成分为四级：

| 级别 | 含义 | 可否称为完成 |
|---|---|---|
| `LOCAL PASS` | 静态检查、单元测试、迁移演练和模拟 E2E 通过 | 只能称本地工程通过 |
| `DEVICE PASS` | 微信开发者工具和至少两台真实手机完成关键流程 | 可称真机流程通过 |
| `PILOT PASS` | 经人工确认后，一个真实班级按协议完成试点并达到指标 | 可称试点通过 |
| `RELEASE PASS` | 主体、类目、备案、隐私文本、监护人同意和发布审核全部通过 | 才可称正式上线 |

## 2. 已冻结的产品与架构决策

| ID | 决策 | 约束 |
|---|---|---|
| D-01 | 产品名为“墨芽习字”，简称“墨芽” | “墨课”只保留为废弃工作名，不再用于新界面和新文档标题 |
| D-02 | 老师端和家长端使用同一个微信小程序、同一个 AppID | 后端按真实权限返回身份，不依赖前端隐藏按钮做鉴权 |
| D-03 | 一人可同时是老师和家长 | 小程序提供明确身份切换，并保留当前 Organization、Workspace、班级上下文 |
| D-04 | IMBoy 是唯一业务后端 | 不引入 DeepFlux，不创建墨芽后端分叉，不增加第二套鉴权、队列和运维 |
| D-05 | `moya` 是独立微信小程序 Git 仓库 | 不放进 `imboy` 子目录，不把小程序代码混入 IMBoy 服务端提交 |
| D-06 | Organization 是培训机构/SaaS 租户边界 | 一个 Organization 可包含多个 Workspace；Workspace 是机构内协作和内容空间 |
| D-07 | Group 是班级沟通容器，不等于完整班级模型 | 教学属性进入 `class_profile`；聊天管理角色不等于教学角色 |
| D-08 | 低龄学员首版是受监护的教学档案 | 家长是可登录用户；`learner.user_id` 首版允许为空，不创建假账号 |
| D-09 | 学员档案属于 Organization | 同一机构跨 Workspace 保持历史连续；跨机构数据仍隔离 |
| D-10 | AI 只生成老师草稿 | AI 失败不阻断人工回评；家长永远不能读取未审核草稿 |
| D-11 | 提交和回评默认私密 | 同班群成员关系本身不授予儿童视频查看权 |
| D-12 | 品牌在新客户端，服务端继续使用 IMBoy 代码与品牌 | `pro.imboy.pub` 或新品牌域名只能是同一组 API 的入口，不能作为租户或权限边界 |
| D-13 | 微信小程序是首个正式客户端，不是唯一未来客户端 | 首版不同时实现飞书/钉钉；通过平台中立 API 与身份 provider 字段保留扩展性 |
| D-14 | 首版以单班试点验证价值，不提前建设完整 SaaS | 暂不做计费、套餐、组织多管理员、代理商、跨机构报表和自助开通 |

## 3. 仓库边界与当前基线

### 3.1 仓库责任

| 仓库 | 负责 | 不负责 |
|---|---|---|
| `imboy` | PostgreSQL 迁移、Handler→Logic→DS→Repo、微信身份映射、教学 ACL、作业与回评 API、附件授权、AI Worker、审计和产品文档 | 墨芽小程序页面和视觉资源 |
| `moya` | 微信小程序工程、登录态、身份切换、家长/老师页面、上传录制、回评播放和本地端测试 | 业务真源、模型密钥、对象存储凭据、越权决策 |
| `imboyadmin` | 仅在试点确有低频后台操作缺口时追加机构管理页 | 首版家长/老师日常工作流 |

`imboyapp`、DeepFlux 和生产部署不在本计划首版实现范围内。

### 3.2 当前事实

- `imboy` 复核时 HEAD 为 `f0a3f4af`（`main`），墨芽迁移已演进到 `00000100_teaching_sentinel_unify`；后续执行仍必须重新取最新编号，不能预占固定编号。
- 历史迁移 `00000076_workspace_foundation` 明确写过“不引入 Organization 层”，不得修改历史文件；新设计通过新增迁移演进。
- 当前已有 `organization`、`class_profile`、`class_staff`、`learner`、`class_enrollment`、`guardian_learner`、多次提交、AI 草稿、老师回评、教学 ACL、私密附件和账号绑定实现；实现提交与证据见 `342d0e49`、`53bfc2ed`、`b14a36ec` 及后续 R6-R20 收口提交。
- `moya` 为 `main` 分支的独立 Git 仓库，无 remote；复核时 HEAD 为 `099e9c7`，已有原生 TypeScript 小程序、公共壳、家长/老师页面、真实附件请求链和 108 个单元测试。两个 logo PNG 仍为用户未跟踪资产，不得擅自处理。
- 2026-09-10 当前 HEAD 复验：`make compile`、11 个墨芽专项 EUnit 套件、OpenAPI/Schema 37 项检查、`npm run check` 和 `npm run build` 均退出 0；`moya` lint 有 2 条 warning，未构成失败。
- 两个仓库均不得由 Agent 擅自设置 Git author、创建远端、提交、推送或发布。

### 3.3 `.worktrees` 归属与隔离

`/Users/leeyi/project/imboy.pub/.worktrees` 下共有 20 个 Git 工作树（`imboy` 9 个、`imboyapp` 7 个、`imboyadmin` 4 个），但没有任何一个属于 `moya`：

| 容器 | 子仓数量 | Git 归属 | 当前用途/状态 |
|---|---:|---|---|
| `A1` | 2 | `imboyapp`、`imboyadmin` | 旧 detached、clean |
| `A2` | 1 | `imboy` | Agent Hub 证据/脚本 dirty |
| `A3` | 2 | `imboy`、`imboyapp` | E2EE 文档 dirty；App clean |
| `A4` | 2 | `imboy`、`imboyapp` | C2C/E2EE 测试与 AI 明文门禁 dirty |
| `A6` | 1 | `imboy` | 认证会话撤销 dirty，拟用迁移 `00000101` |
| `a5` | 1 | `imboy` | 群历史边界 dirty，拟用迁移 `00000101` |
| `integration` | 3 | `imboy`、`imboyapp`、`imboyadmin` | 非墨芽集成候选；后端拟用迁移 `00000101/102` |
| `lt01` | 3 | `imboy`、`imboyapp`、`imboyadmin` | 旧长程任务快照；后端 Agent Hub golden-flow dirty，App/Admin clean |
| `lt01-refresh` | 3 | `imboy`、`imboyapp`、`imboyadmin` | 刷新后的长程任务快照；后端 Agent Hub golden-flow dirty，App/Admin clean |
| `lt02` | 2 | `imboy`、`imboyapp` | E2EE 审计文档 dirty；App clean |

这些工作树是其他 IMBoy 长程任务资产，不是本计划输入、实现或验收证据。墨芽续跑不得进入、修改、合并、提交、清理或删除这些目录，也不得占用其迁移号；主仓需要新迁移时，先由 Coordinator 重新盘点全部 worktree 后分配。当前 Git worktree 注册路径已指向 `.worktrees`，但路径迁移成功不代表内容已合并、可删除或可作为当前基线；相关集成/清理须另立任务并取得用户授权。

### 3.4 2026-09-10 完成度判定

| Step | 当前状态 | 判定 |
|---|---|---|
| 1-11 | `PASS` | 本地产物、契约、迁移、ACL、私密附件、AI 人工降级与专项测试已存在并复验 |
| 12 | `PARTIAL` | 命令行质量门和构建通过；微信开发者工具导入尚无证据 |
| 13 | `PASS` | 登录/请求/上下文/身份切换与 TSID 本地测试通过 |
| 14 | `PARTIAL` | 家长端和附件链本地测试通过；`PARENT-03` 真机未验 |
| 15 | `PARTIAL` | 老师端和无 AI 人工回评本地测试通过；`TEACHER-03` 真机未验 |
| 16 | `PASS` | 历史、绑定/解绑和审计本地测试通过 |
| 17 | `PARTIAL` | 当前本地编译、专项 EUnit、契约和 Moya 四门通过；微信登录、活 Garage 真传、跨仓真实联调和 `DEVICE-01` 未完成 |
| 18 | `PASS` | 试点材料已准备，但当前 Go/No-Go 结论仍为 `NO-GO`，不得自行启动试点 |

当前最高证据等级仍是 `LOCAL PASS`。`DEVICE PASS`、`PILOT PASS` 和 `RELEASE PASS` 均未达到。

## 4. MVP 范围

### 4.1 必须完成

- 一个 Organization 创建一个默认 Workspace，并可在数据模型上继续添加 Workspace。
- 一个 Workspace 下使用 Group 建立班级，并配置 manager、teacher、assistant 教学角色。
- 家长微信登录/绑定 IMBoy 用户，绑定一个或多个低龄学员档案。
- 老师发布 `group_task`，按学员创建或关联 assignment。
- 家长提交一段练字视频和可选作品照片；上传失败可重试，不能产生重复提交记录。
- 老师看到待点评队列、视频、照片、AI 状态和 AI 草稿。
- 老师可忽略或修改 AI 草稿，上传真人点评视频并发布。
- 家长只能看到自己所监护学员的已发布回评和历史记录。
- 老师可要求重练；每次提交保留独立记录，不能覆盖前一次证据。
- AI 超时、失败或结果不合规时，老师仍可纯人工完成回评。
- 提供数据导出、删除请求和附件清理所需的最小接口或后台操作路径。

### 4.2 明确暂缓

- 学员独立登录 UI、绑定码和申诉流程；首版只预留稳定 `learner.user_id` 与绑定审计能力。
- 飞书小程序、钉钉小程序、Web 和 Flutter 墨芽客户端。
- 微信支付、课程销售、套餐计费、Organization 自助注册和代理商体系。
- AI 自动发布、逐字精确打分、排行榜、公开作品广场和公开儿童视频。
- 直播、群内自动转发、完整 LMS、课表、排课、考勤和财务管理。
- Organization 多管理员表；首版先使用 `organization.owner_id`，出现第二位跨 Workspace 管理员时再增加。
- DeepFlux 集成、独立 AI 微服务和品牌专属后端。

## 5. 角色、权限与可见性

### 5.1 身份模型

| 身份 | 登录主体 | 权限来源 |
|---|---|---|
| Organization Owner | IMBoy 用户 | `organization.owner_id` |
| 班级 manager/teacher/assistant | IMBoy 用户 | `class_staff`，不能从 Group 管理员自动推断 |
| 家长/监护人 | IMBoy 用户 | `guardian_learner`，可按孩子细分提交和查看权限 |
| 低龄学员 | `learner` 教学档案 | 不要求账号；未来可绑定正常 IMBoy 用户 |

### 5.2 资源访问矩阵

| 资源 | Organization Owner | 授权老师 | 绑定监护人 | 其他同班成员 | 学员未来账号 |
|---|---:|---:|---:|---:|---:|
| 班级作业说明 | 是 | 是 | 是 | 按 Group 规则 | 是 |
| 学员提交视频/照片 | 不因 Owner 身份自动获得 | 是，仅负责班级 | 是，仅绑定学员 | 否 | 是，仅本人 |
| AI 草稿 | 否，除非同时是授权老师 | 是 | 否 | 否 | 否 |
| 老师已发布回评 | 不因 Owner 身份自动获得 | 是 | 是，仅绑定学员 | 否 | 是，仅本人 |
| 班级匿名汇总 | 按机构策略 | 是 | 可选 | 可选 | 可选 |

所有读写 API 必须从 JWT 用户、资源关系和数据库记录解析权限。`organization_id`、`workspace_id`、`learner_id` 和 `reviewer_uid` 等字段不得接受客户端声明后直接信任。

## 6. 最小数据设计

### 6.1 机构与 Workspace

```text
organization
- id
- name
- owner_id
- status                 active | archived
- branding               jsonb，首版只存必要字段
- settings               jsonb，首版只存必要字段
- created_at
- updated_at

workspace
- organization_id        新增可空外键，expand 阶段兼容历史 Workspace
```

迁移策略：

1. 新增 `organization` 和 `workspace.organization_id NULL`，不修改旧迁移。
2. 只给明确加入墨芽试点的 Workspace 建 Organization 并回填；不得按相同 `owner_id` 猜测机构归属。
3. 新建 Organization 时事务内创建默认 Workspace。
4. 经过数据审计后再决定是否把 `organization_id` 收紧为非空；首版不强制迁移所有历史 Workspace。
5. `workspace.owner_id` 保留为 Workspace 本地治理 Owner；未来计费锚点是 Organization，不再是 Workspace。

### 6.2 教学身份

```text
class_profile
- group_id               PK/FK -> group.id
- course_type            hard_pen | brush | mixed
- term                    可空
- status                  active | archived
- created_at
- updated_at

class_staff
- group_id
- user_id
- role                    manager | teacher | assistant
- status                  active | removed
- created_at
- updated_at
- UNIQUE(group_id, user_id)

learner
- id
- organization_id
- display_name
- birth_year              可空；不收完整生日
- user_id                 可空；未来账号绑定
- account_bound_at        可空
- account_bound_by        可空
- status                  active | archived
- created_at
- updated_at
- UNIQUE(organization_id, user_id) WHERE user_id IS NOT NULL

class_enrollment
- group_id
- learner_id
- status                  active | removed
- joined_at
- UNIQUE(group_id, learner_id)

guardian_learner
- guardian_uid
- learner_id
- relation                guardian | other
- can_submit
- can_view_review
- status                  active | removed
- created_at
- updated_at
- UNIQUE(guardian_uid, learner_id)
```

必须保证 `learner.organization_id == group.workspace.organization_id`。实现时优先使用数据库约束；如果现有表结构无法直接建立复合外键，使用可延迟约束触发器并留下数据库集成测试，不能只靠 Handler 校验。

### 6.3 作业、提交和回评

保留 `group_task` 和 `group_task_assignment`，最小扩展 assignment：

```text
group_task_assignment
- learner_id             教学作业非空，普通 IMBoy 群作业为空
- submitted_by           最近一次提交人，仅作快捷字段；真源在 submission
```

现有唯一约束 `(task_id, user_id)` 必须兼容“一位家长替两个孩子接收同一份作业”：

- 删除原全量唯一约束后，为普通群作业建立 `(task_id, user_id) WHERE learner_id IS NULL` 部分唯一索引，并尽量保留原约束名对应的错误契约。
- 为教学作业建立 `(task_id, learner_id) WHERE learner_id IS NOT NULL` 部分唯一索引。
- 教学 assignment 的 `user_id` 继续作为兼容字段和默认通知账号，但提交与查看权限的真源是 `guardian_learner`，不能只认该 user_id。

为支持重练而不覆盖历史，增加：

```text
homework_submission
- id
- assignment_id
- learner_id
- submitted_by
- attempt_no
- status                  submitted | withdrawn
- submitted_at
- created_at
- UNIQUE(assignment_id, attempt_no)

submission_asset
- id
- submission_id
- attachment_id
- kind                    practice_video | final_photo
- sort_order
- created_by
- created_at
- UNIQUE(submission_id, attachment_id)

calligraphy_review_draft
- id
- submission_id
- ai_task_id              可空
- status                  queued | running | succeeded | failed
- model_profile
- prompt_version
- rubric_version
- input_digest
- result_json             只存结构化结果，不存模型思维过程
- error_code
- created_at
- completed_at

teacher_review
- id
- submission_id
- reviewer_uid
- positive_point
- focus_problem
- practice_action
- comment
- video_attachment_id     可空
- rework_required
- status                  draft | published | discarded
- published_at
- created_at
- updated_at
```

核心不变量：

- 一个 submission 只能属于一个 assignment 和 learner。
- 同一家长可为多个 learner 接收同一个 task；每个 learner 对同一 task 仍只能有一个教学 assignment。
- assignment、learner、Group、Workspace 必须最终解析到同一 Organization。
- 家长只能为 `can_submit=true` 的绑定学员提交。
- 每个 submission 同时最多有一个有效 AI 草稿和一个已发布老师回评。
- `teacher_review.status=published` 时必须有 `reviewer_uid`、`published_at` 和至少一种有效反馈内容。
- 重练创建新的 submission/attempt，不修改旧 submission、AI 草稿或已发布回评。
- 附件只保存 `attachment_id`，业务表不得持久化 presigned URL。

### 6.4 账号绑定预留

首版只允许 `learner.user_id` 为空或绑定一个正常 IMBoy 用户，并记录 `account_bound_at/account_bound_by`。真正开放学员自助绑定前，另行实现短期绑定码、监护人确认、机构复核、解绑和申诉；不得仅凭姓名或出生年份认领。

## 7. API 与异步处理边界

### 7.1 API 组

以 IMBoy 现有版本路由为准，对外 REST 统一前缀 `/api/v1`（项目路由铁律，见根级 CLAUDE.md），在实现前先冻结 OpenAPI/契约：

```text
POST /api/v1/auth/wechat-mini/login
GET  /api/v1/moya/contexts
POST /api/v1/moya/context/switch

GET  /api/v1/moya/assignments
GET  /api/v1/moya/assignments/:id
POST /api/v1/moya/assignments/:id/submissions
GET  /api/v1/moya/submissions/:id

GET  /api/v1/moya/review-queue
GET  /api/v1/moya/submissions/:id/review-workbench
PUT  /api/v1/moya/submissions/:id/review-draft
POST /api/v1/moya/submissions/:id/reviews/publish

GET  /api/v1/moya/learners/:id/history
```

附件继续复用 IMBoy presign/confirm/view URL 能力，但要增加教学资源 scope 或可信的教学资源授权解析。微信 `openid`、`unionid` 只进入身份映射层，不进入 learner、作业或回评核心表。

### 7.2 幂等与状态机

- 创建提交使用客户端生成的 idempotency key；重试返回同一 submission，不重复增加 attempt。
- 附件 confirm 与 submission 关联必须在事务或可补偿流程内完成。
- AI 状态独立于 assignment 状态；`failed` 仍进入老师人工队列。
- 发布回评必须做条件更新，只允许 `draft -> published` 一次，重复请求返回已发布结果。
- `submitted -> withdrawn` 仅允许 `can_submit=true` 的监护人在该 submission 尚无已发布回评时发起；撤回后老师队列即时移除该条，附件证据保留不删除；老师和 Owner 不能替家长撤回。
- 客户端不得直接提交 `published_at`、`reviewer_uid`、AI status 或跨租户 ID。

### 7.3 AI Worker

```text
submission created
  -> enqueue business IDs and version only
  -> load authorized attachment from Garage
  -> validate duration/type/size
  -> transcode and sample frames
  -> invoke registered IMBoy LLM provider
  -> validate JSON Schema
  -> persist review draft
  -> notify authorized teacher queue
```

任务载荷只含 Organization/Workspace/assignment/submission/attachment ID 和 rubric/prompt 版本，不保存短时 URL。派生帧使用独立生命周期并随原视频删除；日志不得记录儿童姓名、视频 URL、帧内容、模型密钥或完整模型输入。

AI 草稿最小字段为：正向观察、一个主要问题、证据时间点、一个练习动作、三点以内老师口播提纲和“需要人工特别核对”标记。置信度只供老师参考，不向家长宣传为准确率。

## 8. 墨芽小程序 UI/UX

### 8.1 信息架构

同一个 AppID，登录后先从后端取得可用身份和上下文。只有一个身份时直接进入；多身份时保留上次选择，并在“我的”顶部提供身份切换。

家长模式底部导航：

- `作业`：待完成、分析中、待回评、已回评。
- `成长`：按学员查看历次作业、老师建议和重练结果。
- `我的`：孩子切换、身份切换、隐私、授权和问题反馈。

老师模式底部导航：

- `待点评`：按提交时间显示队列和 AI 状态。
- `作业`：发布、查看提交进度和筛选未交/待评。
- `班级`：班级、学员和授权老师。
- `我的`：身份切换、录制权限、隐私和缓存清理。

### 8.2 家长关键流程

1. 选择孩子和待完成作业。
2. 查看短作业说明和拍摄要求。
3. 录制或选择约一分钟视频，可补一张作品照片。
4. 本地预览、删除重选、显示上传进度和失败重试。
5. 提交成功后显示“已提交/AI 整理中/等待老师回评”，不显示虚假倒计时。
6. 收到老师发布通知后查看真人视频、一个重点问题和一个练习动作。
7. 老师要求重练时从原回评进入新 attempt，旧记录仍可回看。

### 8.3 老师关键流程

1. 进入待点评队列，按班级、作业、提交时间和 AI 状态筛选。
2. 查看学生视频、作品照片、关键时间点和 AI 草稿。
3. 采用、修改或标记 AI 判断不准确；无 AI 时直接人工填写。
4. 录制/选择真人点评视频，预览后保存草稿。
5. 发布前二次确认接收学员与可见范围。
6. 发布成功后自动进入下一份，不自动转发到微信群。

### 8.4 视觉与品牌

- 正式名称统一使用“墨芽习字”，导航内可简称“墨芽”。
- 头像方向：纸白背景、墨色笔画长出两片绿色嫩芽、隐约田字格、少量印章红点缀、无文字、适配圆形裁切。
- 最终头像交付：`PNG 144x144`、小于 `2 MB`，另保留高清源文件；不得含政治、色情、暴力、宗教或易误认官方标识的元素。
- 小程序介绍候选：`墨芽习字面向书法老师、家长和学员，支持练字作业发布、视频与作品提交、老师回评、练习建议和成长记录。AI仅辅助整理点评，所有反馈均由老师审核后发布。`
- UI 使用墨黑、纸白、芽绿和少量印章红；控件保持高对比、触控区不小于微信平台建议值，长名称必须换行或截断。
- 不在页面堆叠功能说明；用明确状态、进度、空态、错误态和确认动作表达流程。

## 9. 法务、主体和隐私门禁

以下是产品与工程门禁，不构成正式法律意见；上线前需按当时微信官方规则和适用法律重新核验。

### 9.1 主体策略

- 无新注册企业不妨碍完成需求、原型、开发版和体验版验证。
- 涉及儿童练字视频、老师视频回评和教育服务，个人主体的类目与能力存在较高审核风险，不能把正式上线建立在“个人主体一定可过审”的假设上。
- 正式试点优先评估由你姐姐所在培训机构作为小程序主体，前提是其营业执照、文化艺术/非学科培训资质和微信类目适用；实际选择必须由用户人工确认。
- 不得为过审伪装成工具、办公或图片处理类目。
- 正式发布前完成小程序备案及微信当时要求的认证或材料流程。

### 9.2 数据处理关系

品牌不是法律主体，协议和隐私文本不能笼统写“数据属于墨芽”或“数据属于 IMBoy”。建议合同结构为：机构决定教学目的、教师权限和保存期限时，机构作为个人信息处理者；IMBoy 技术提供方按书面约定作为受托处理方。最终措辞需结合真实经营主体由专业人士复核。

机构主体合作协议至少明确：

- 小程序 AppID、管理员和代码知识产权归属。
- IMBoy 与墨芽品牌的许可和展示边界。
- 双方的数据处理角色、目的、范围、保存期限、安全责任和事件响应。
- 服务终止后的数据导出、迁移、删除和备份清理。
- 不得将真实儿童数据用于通用模型训练，除非另有合法、明确、单独授权。

### 9.3 儿童隐私硬门槛

- 不满十四周岁未成年人信息按敏感个人信息高标准处理。
- 真实试点前取得监护人明确同意，说明视频用途、AI 参与、接收者、保存期和撤回方式。
- 默认最小可见范围，短时授权 URL，不产生公开桶 URL。
- 提供撤回、删除、导出、更正和解绑路径，并记录操作审计。
- 开发、测试、日志、截图和 Git 只使用合成或去标识数据。
- 未完成监护人同意、访问控制和删除验证时，真实儿童试点为 `NO-GO`。

## 10. 并行推进规则

### 10.1 工作流与依赖图

```text
Step 1 基线冻结
  ├─ Step 2 合规门禁
  ├─ Step 3 UX/品牌规格
  ├─ Step 4 API/威胁模型
  ├─ Step 5 -> Step 6 -> Step 7  数据迁移串行泳道
  └─ Step 12 moya 工程基线

Step 4 + Step 7 -> Step 8 -> Step 9 -> Step 10 -> Step 11  后端业务泳道
Step 3 + Step 4 + Step 12 -> Step 13 -> {Step 14, Step 15}  小程序泳道
Step 8 + Step 9 -> Step 16  学员历史与账号绑定预留
Step 9..16 -> Step 17  联调、安全与 E2E
Step 2 + Step 17 -> Step 18  试点包与 Go/No-Go
```

### 10.2 文件所有权

| 泳道 | 独占范围 | 可并行范围 |
|---|---|---|
| DB-MIGRATION | `imboy/priv/migrations/*`、迁移编号登记 | Step 5-7 必须由同一 owner 串行完成 |
| BACKEND-TEACHING | `imboy/src/api/*teaching*`、`src/logic/*teaching*`、`src/ds/*teaching*`、`src/repo/*teaching*`、对应测试 | 可与 `moya` 页面并行，不与迁移 owner 同时改同一 Repo |
| BACKEND-AI | 新的书法 AI Worker/Schema/测试；只通过稳定接口调用教学 Logic | 可在 Step 9 契约冻结后与部分 UI 并行 |
| MOYA-SHELL | `moya` 根配置、登录态、请求层、公共组件 | Step 13 完成前，其他小程序 Agent 不改公共壳 |
| MOYA-PARENT | `moya` 家长页面与其测试 | Step 14 与 Step 15 可并行 |
| MOYA-TEACHER | `moya` 老师页面与其测试 | Step 15 与 Step 14 可并行 |
| DOC-EVIDENCE | `imboy/docs/plans/` 下本计划的证据目录或后续验收报告 | 不改业务代码 |

每个 Agent 开工前必须记录两个仓库的 HEAD 和 dirty 状态，只改任务声明的 owned paths；发现其他 Agent 新改动时与其兼容，不 reset、clean、stash 或覆盖。

`.worktrees/**` 不属于本计划所有权；即使其中代码与教学模块发生文本重叠，也必须视为外部并行工作，不得由墨芽 Agent 合并或清理。

## Step 1 — 冻结基线、决策台账与证据目录

**意图**：把两个独立仓库的真实状态、旧设计冲突、范围和证据格式固定下来，避免后续 Agent 从过期“Workspace=机构”假设开工。

**依赖**：无。

**所有权**：仅文档和本地证据清单；不改业务代码。

**任务清单**：

- [ ] 记录 `imboy` 与 `moya` 的 Git root、HEAD、branch、remote 和 dirty state。
- [ ] 建立决策台账 D-01 至 D-14，并把旧草案标记为讨论来源而非当前真源。
- [ ] 建立证据命名：`STEP-XX/{commands,tests,screenshots,notes}`，真实 PII 不得入库。

**验收**：

- `BASE-01`：脚本或报告能证明 `imboy` 与 `moya` 是两个 Git root，且未混入工作区根。
- `BASE-02`：当前真源明确写出 Organization→Workspace→Group 和 `learner.organization_id`。
- `BASE-03`：没有提交、推送、远端创建、Git author 修改或生产操作。

**Out of scope**：任何数据库、API、小程序业务代码和外部平台修改。

## Step 2 — 核验主体、类目、备案与儿童隐私门禁

**意图**：使用执行时的微信官方规则和法律原文更新上线矩阵，形成开发版、真实试点和正式发布三道门禁；只做核验和材料草拟，不替用户选择主体或对外提交。

**依赖**：Step 1。

**所有权**：合规核验报告、隐私清单、协议条款清单。

**任务清单**：

- [ ] 核验个人/机构主体、教育类目、视频能力、认证、备案和微信支付的现行要求。
- [ ] 草拟监护人同意、隐私规则、儿童信息处理、删除导出和 AI 参与说明。
- [ ] 输出“机构作为处理者、IMBoy 技术方作为受托方”的待专业复核条款清单。

**验收**：

- `LEGAL-01`：每项规则有官方来源、核验日期和适用条件，不用旧截图代替现行规则。
- `LEGAL-02`：开发版、真实试点、正式发布各自有明确 `GO/BLOCKED/NO-GO` 条件。
- `LEGAL-03`：报告没有替用户决定主体、发布、联系方式或对外责任事项。

**Out of scope**：注册公司、提交微信审核/备案、签署协议、联系机构或录入任何真实儿童信息。

## Step 3 — 完成角色化 UX 原型与品牌资产规格

**意图**：设计同一小程序内家长和老师的关键任务流、所有状态和视觉规范，并生成可供真机走查的原型及头像候选。

**依赖**：Step 1。

**所有权**：UX 流程、线框稿、视觉 token、头像源图和文案候选；不碰小程序业务实现。

**任务清单**：

- [ ] 覆盖登录、身份切换、孩子切换、提交、AI 等待、老师审核、发布、重练和历史回看。
- [ ] 覆盖无权限、空列表、上传失败、AI 失败、视频不可播、重复发布和账号解绑状态。
- [ ] 盘点 `moya` 仓已有 logo 资产（提交 `9644a2e` 的 README 与未跟踪的 `moyalogo_144X144.png`/`moyalogo_256X256.png`），先确认去留与是否达标，再决定补几个候选。
- [ ] 生成至少三个头像候选并验证 144x144 圆形裁切和小尺寸辨识度。

**验收**：

- `UX-01`：家长与老师核心流程都能在不阅读功能说明的情况下完成纸面/点击原型走查。
- `UX-02`：每个异步步骤有 loading、success、empty、error、retry 状态且不遮挡、不溢出。
- `BRAND-01`：头像 PNG 为 144x144、小于 2 MB、无文字和敏感元素；介绍不超过微信限制。

**Out of scope**：公开发布品牌、注册商标、申请同名账号或把头像上传到微信平台。

## Step 4 — 冻结 API 契约、状态机和威胁模型

**意图**：在前后端并行编码前，设计平台中立的 REST 契约、错误码、幂等键、角色上下文和儿童视频访问威胁模型。

**依赖**：Step 1；吸收 Step 2、Step 3 已完成的结论，不阻塞只读草案。

**所有权**：OpenAPI/契约文档、JSON Schema、状态机和安全用例。

**任务清单**：

- [ ] 定义登录、上下文、作业、提交、待评队列、草稿、发布和历史 API。
- [ ] 定义 TSID JSON 表达、分页、错误码、幂等、乐观并发和附件引用规则。
- [ ] 定义横向越权、角色混淆、伪造 learner、重放提交、短时 URL 泄漏和 AI prompt 注入用例。

**验收**：

- `API-01`：契约示例可通过 OpenAPI/JSON Schema 校验，ID 不发生 JavaScript 精度丢失。
- `API-02`：家长、老师、多身份用户和 AI 失败流程均有确定响应与状态转换。
- `SEC-01`：威胁模型至少包含跨 Organization/Workspace/learner 的 deny-by-default 测试矩阵。

**Out of scope**：实现 API、引入跨平台前端框架或修改生产域名。

## Step 5 — 实现 Organization 与 Workspace 兼容迁移

**意图**：新增 Organization 租户层和 `workspace.organization_id`，通过 expand-first 迁移保留所有历史 Workspace 行为。

**依赖**：Step 1；数据库迁移泳道起点。

**所有权**：下一组未占用迁移 up/down 文件及其迁移测试；不得修改 `00000076` 等历史迁移。

**任务清单**：

- [ ] 执行前重新分配最新连续迁移编号。
- [ ] 创建 organization、索引、FK 和约束，给 workspace 增加可空 organization_id。
- [ ] 实现显式创建/关联策略，不按 owner_id 自动合并历史 Workspace。

**验收**：

- `DB-ORG-01`：空库 up、down、再 up 通过，历史迁移文件校验和不变。
- `DB-ORG-02`：一个 Organization 可关联多个 Workspace，一个 Workspace 最多属于一个 Organization。
- `DB-ORG-03`：旧 Workspace 在未回填时仍可按原 IMBoy 功能读取，新增墨芽路径拒绝无 Organization 上下文。

**Out of scope**：Organization 多管理员、套餐、计费、自动归并历史 Workspace 和生产迁移。

## Step 6 — 实现班级、学员与监护关系迁移

**意图**：建立 class_profile、class_staff、learner、class_enrollment 和 guardian_learner，数据库层阻止跨机构绑定。

**依赖**：Step 5；与 Step 7 在同一迁移泳道串行。

**所有权**：下一组未占用迁移 up/down 文件、约束函数和迁移测试。

**任务清单**：

- [ ] 创建五张教学身份表、必要索引、状态 CHECK、唯一约束和外键。
- [ ] 实现 learner 与 Group 所属 Organization 一致性约束。
- [ ] 保留 learner.user_id 空值和同机构部分唯一约束。

**验收**：

- `DB-LEARNER-01`：合法的同机构跨 Workspace 入班通过，跨机构 enrollment 在提交时失败。
- `DB-GUARDIAN-01`：一名家长可绑定多个学员，一名学员可绑定多个监护人，重复有效关系被拒绝。
- `DB-BIND-01`：未登录学员档案可长期存在；同一 IMBoy 用户可绑定不同 Organization 的独立 learner，但同机构不能重复绑定。

**Out of scope**：学员自助注册、绑定码、申诉、批量导入和 Organization 管理员 UI。

## Step 7 — 实现多次提交、AI 草稿与老师回评迁移

**意图**：扩展 assignment 并新增 homework_submission、submission_asset、calligraphy_review_draft 和 teacher_review，保证重练历史不可覆盖。

**依赖**：Step 6；数据库迁移泳道终点。

**所有权**：下一组未占用迁移 up/down 文件、状态约束和迁移测试。

**任务清单**：

- [ ] 给 assignment 增加兼容旧群作业的可空 learner_id/submitted_by。
- [ ] 把 `(task_id,user_id)` 改成仅约束普通群作业的部分唯一索引，并增加教学作业 `(task_id,learner_id)` 部分唯一索引。
- [ ] 创建四张回课闭环表和热路径索引。
- [ ] 用约束保证 attempt、发布状态、附件关系和 Organization 一致性。

**验收**：

- `DB-SUBMIT-01`：同 assignment 可保存多个有序 attempt，旧 attempt 不被新提交更新或删除。
- `DB-ASSIGN-01`：同一家长可替两个孩子接收同一 task；同一 learner 不会收到重复教学 assignment。
- `DB-REVIEW-01`：未审核 AI 草稿与老师已发布回评分表保存，发布约束不允许缺 reviewer/published_at。
- `DB-COMPAT-01`：普通 IMBoy 群作业继续允许 learner_id 为空，现有 group_task 测试通过。

**Out of scope**：删除旧 attachment/content 字段、迁移历史群作业附件和生产数据回填。

## Step 8 — 实现微信身份、教学上下文与服务端 ACL

**意图**：复用 sso_identity 模式实现微信小程序身份映射，并在 Logic 层集中解析 Organization、Workspace、班级、角色和监护关系。

**依赖**：Step 4、Step 7。

**所有权**：IMBoy 微信登录 Handler/Logic/DS/Repo、教学 context/ACL 模块及测试。

**任务清单**：

- [ ] 服务端用一次性 code 换取微信身份，客户端不持有 app secret。
- [ ] 返回用户可用家长/老师身份和 Organization/Workspace/班级上下文。
- [ ] 对每个教学资源实现 deny-by-default 的读取、提交、草稿和发布守卫。

**验收**：

- `AUTH-01`：code 重放、伪造 openid、无效 provider 和未绑定用户均按契约失败且不泄漏内部信息。
- `ACL-01`：跨 Organization、跨 learner、仅 Group 管理员和仅 Organization Owner 的儿童视频访问均被拒绝。
- `ACL-02`：同一用户同时为家长和老师时可显式切换，服务端不混用上一次资源上下文。

**Out of scope**：微信开放平台账号操作、App Secret 配置到真实环境、手机号快速验证和其他平台登录实现。

## Step 9 — 实现作业、提交、队列与回评 API

**意图**：按现有 Handler→Logic→DS→Repo 分层实现墨芽核心 API 和状态机，并复用 group_task、attachment 与通知能力。

**依赖**：Step 4、Step 7、Step 8。

**所有权**：教学业务 API/Logic/DS/Repo、路由、错误码和对应测试。

**任务清单**：

- [ ] 实现家长作业列表/详情、幂等提交和历史读取。
- [ ] 实现老师待评队列、工作台草稿、保存、发布和要求重练。
- [ ] 实现条件更新和事务边界，失败时不留下半发布或重复 attempt。

**验收**：

- `FLOW-01`：发布作业→家长提交→老师发布→家长查看的 API 集成测试通过。
- `IDEMP-01`：相同 idempotency key 重试不新增 submission/attempt/attachment relation。
- `STATE-01`：非法状态跳转、重复发布和非授权 reviewer 全部失败，AI failed 不阻断人工发布。

**Out of scope**：AI 模型调用、前端页面、微信群机器人和公开作品分享。

## Step 10 — 实现私密视频上传、播放与生命周期

**意图**：在现有 Garage presign/confirm/view URL 基础上增加教学附件授权、视频约束、失败补偿和删除传播。

**依赖**：Step 8、Step 9。

**所有权**：教学附件 scope/解析、上传确认、短时播放 URL、清理任务和测试。

**任务清单**：

- [ ] 校验 MIME、扩展名、大小、时长和上传者权限，拒绝客户端伪造。
- [ ] 只签发短时读 URL，并在每次签发时重新检查 submission 权限。
- [ ] 覆盖未 confirm、submission 创建失败、撤回、删除和派生帧清理。

**验收**：

- `MEDIA-01`：未授权家长、同班其他成员和非任课老师无法取得有效播放 URL。
- `MEDIA-02`：上传重试可恢复，孤儿对象和派生帧有可验证清理路径，不误删已绑定附件。
- `MEDIA-03`：业务表和日志中不存在持久化 presigned URL、对象存储密钥或真实视频内容。

**Out of scope**：CDN 公开分发、直播、跨区域复制和生产桶策略修改。

## Step 11 — 实现 IMBoy 内部 AI 视频回课 Worker

**意图**：在 IMBoy 内部异步处理视频和作品照片，复用 imboy_llm provider 注册模式生成结构化草稿，任何失败均回落到人工流程。

**依赖**：Step 4、Step 9、Step 10。

**所有权**：书法 AI job/worker、抽帧适配、JSON Schema、provider 能力校验、提示词版本和测试。

**任务清单**：

- [ ] 入队只保存业务 ID 和版本；Worker 运行时获取受权附件。
- [ ] 队列与任务状态载体使用 PostgreSQL 表（原子取任务用 `DELETE ... RETURNING` 或等价事务内锁），进程内缓存用 depcache；禁止引入 Redis 或任何外部 broker（项目全栈禁 Redis 铁律）。
- [ ] 转码/抽帧设置资源和超时上限，Schema 校验失败写 failed，不写半成品。
- [ ] 老师反馈可记录“采用/修改/判断错误”，但不把儿童数据默认用于模型训练。

**验收**：

- `AI-01`：成功、超时、provider 错误、非法 JSON 和附件删除五种路径均有确定状态与重试上限。
- `AI-02`：结构化草稿包含版本和 input_digest，不包含思维链，家长 API 永远不返回该表。
- `AI-03`：无模型密钥或模型不可用时，系统明确降级到人工队列且核心回课闭环仍通过。

**Out of scope**：DeepFlux、独立 AI 服务、AI 自动发布、真实儿童数据训练和逐字权威评分。

## Step 12 — 初始化 moya 微信小程序工程基线

**意图**：在独立空仓 `moya` 建立最小可运行的微信小程序 TypeScript 工程、质量门和环境配置约束，不提前引入多端框架。

**依赖**：Step 1；可与 Step 2-7 并行。

**所有权**：`moya` 根配置、README、构建检查、测试基线和示例环境文件。

**任务清单**：

- [ ] 先用原生微信小程序 + TypeScript；API 请求层使用平台中立 DTO，不在业务页面散落 openid。
- [ ] 增加 lint、typecheck、unit test 和敏感配置检查的可运行命令。
- [ ] AppID、API base URL 和密钥不入 Git；开发占位值与真实环境分离。

**验收**：

- `MOYA-BASE-01`：微信开发者工具可导入并显示非空首屏，命令行 typecheck/test 通过。
- `MOYA-BASE-02`：仓库无 secret、真实 AppID、真实儿童数据和 IMBoy 服务端凭据。
- `MOYA-BASE-03`：未引入飞书/钉钉适配器、跨端框架、状态库或 UI 库，除非有当前页面的可验证必要性。

**Out of scope**：创建远端、提交/推送、微信平台配置、飞书/钉钉客户端和生产域名。

## Step 13 — 实现登录、请求层、身份切换与公共壳

**意图**：实现微信登录、IMBoy token、错误处理、Organization/Workspace/班级上下文和家长/老师模式切换，为两条 UI 泳道提供稳定公共能力。

**依赖**：Step 4、Step 8、Step 12。

**所有权**：`moya` 登录、请求、会话、上下文、导航和公共组件；完成后冻结公共接口。

**任务清单**：

- [ ] 登录失败、token 过期、无任何身份、多身份和上下文失效均有恢复路径。
- [ ] TSID 全程按 string 处理，序列化和日志不转 number。
- [ ] 家长/老师导航由服务端权限驱动，切换后清理不属于新上下文的缓存。

**验收**：

- `MOYA-AUTH-01`：模拟登录和 token 刷新测试通过，敏感 token 不写普通日志。
- `MOYA-ROLE-01`：单身份直达、多身份显式切换、无权限入口不可见且直接访问仍被后端拒绝。
- `MOYA-ID-01`：最大 64-bit TSID 在请求、缓存、路由和渲染中保持原值。

**Out of scope**：家长作业页面、老师点评页面和真实微信账号验收。

## Step 14 — 实现家长端作业提交与成长记录

**意图**：完成家长模式的作业列表、详情、视频/照片选择、上传进度、幂等提交、回评查看、重练和历史记录。

**依赖**：Step 3、Step 9、Step 10、Step 13；可与 Step 15 并行。

**所有权**：`moya` 家长页面、家长专用组件和测试；不修改公共请求契约。

**任务清单**：

- [ ] 覆盖孩子切换及所有提交/回评状态。
- [ ] 上传前预览、取消、失败重试和重复点击保护。
- [ ] 回评只展示老师已发布内容，重练保留旧 attempt 时间线。

**验收**：

- `PARENT-01`：组件/页面测试覆盖成功、空态、上传失败、AI 失败和重练流程。
- `PARENT-02`：切换孩子后旧孩子数据不闪现、不串缓存；直接篡改 learner_id 得到拒绝。
- `PARENT-03`：真机可录制/选择约一分钟视频、后台切回后恢复状态并完成上传。

**Out of scope**：课程购买、公开分享、家长群聊天和学员独立账号 UI。

## Step 15 — 实现老师端待评队列与视频回评

**意图**：完成老师模式的待评队列、筛选、视频工作台、AI 草稿核对、真人视频录制、发布确认和连续处理。

**依赖**：Step 3、Step 9、Step 10、Step 11、Step 13；可与 Step 14 并行。

**所有权**：`moya` 老师页面、老师专用组件和测试；不修改家长页面。

**任务清单**：

- [ ] AI queued/running/succeeded/failed 均不阻塞老师进入工作台。
- [ ] 老师可采用、修改或标错 AI 内容，并能无 AI 直接回评。
- [ ] 发布前确认学员和可见范围，成功后进入下一条，失败保留草稿。

**验收**：

- `TEACHER-01`：队列筛选、草稿保存、重复发布保护和无 AI 人工回评测试通过。
- `TEACHER-02`：非任课老师、assistant 无发布权配置和已移除 staff 无法播放或发布。
- `TEACHER-03`：真机录制/选择点评视频、预览、取消、重试和发布流程通过。

**Out of scope**：自动发微信群、班级直播、AI 代老师发布和复杂排课管理。

## Step 16 — 实现学员历史读取与未来账号绑定预留

**意图**：确保历史数据始终归 learner_id，并验证未来绑定正常 IMBoy 账号后可读取本人历史而不打通机构后台权限。

**依赖**：Step 6、Step 8、Step 9。

**所有权**：learner 历史查询、最小绑定管理接口或内部 Logic、审计测试；不开放自助绑定 UI。

**任务清单**：

- [ ] 历史按 learner 聚合多个 Workspace/班级的已发布回评，范围仅限同 Organization。
- [ ] 管理侧最小绑定动作要求授权确认并记录 account_bound_by/at。
- [ ] 解绑只移除登录关系，不删除 learner、submission 或 review。

**验收**：

- `HISTORY-01`：同 Organization 跨 Workspace 历史连续，跨 Organization 历史分区且不可互读。
- `BIND-01`：绑定前后 submission/review 主键和 learner_id 不变，无复制或迁移历史。
- `BIND-02`：解绑后历史仍由监护人按原权限访问，账号本人立即失去入口并留下审计记录。

**Out of scope**：公开学员注册入口、短期绑定码、姓名搜索认领、申诉和自动合并跨机构档案。

## Step 17 — 完成契约、集成、安全与真机 E2E 验收

**意图**：把后端、小程序和对象存储串成可复现测试，重点证明不串租户、不泄漏儿童视频、失败可恢复和 AI 可降级。

**依赖**：Step 9-16。

**所有权**：测试、测试数据工厂、E2E 旅程、截图/日志脱敏和验收报告；只修复阻断性缺陷。

**任务清单**：

- [ ] 后端 EUnit/数据库集成、OpenAPI 契约、小程序 unit/typecheck 全绿。
- [ ] 使用合成视频完成家长和老师关键 E2E，覆盖两个 Organization 的越权攻击用例。
- [ ] 在至少两台真实手机验证拍摄、上传、播放、身份切换、网络中断和恢复。

**验收**：

- `TEST-01`：`make compile`、相关 EUnit、迁移 up/down/up、moya typecheck/unit test 和契约检查均有命令与退出码证据。
- `SEC-E2E-01`：跨租户、跨 learner、URL 重放、assistant 越权和客户端伪造 reviewer 全部 fail closed。
- `DEVICE-01`：真实微信环境关键旅程通过；开发者工具或 mock 结果不能冒充真机证据。

**Out of scope**：真实儿童数据、生产环境、应用商店审核、压力容量承诺和未授权第三方测试。

## Step 18 — 准备单班试点包并执行 Go/No-Go 评审

**意图**：形成可由用户和机构人工批准的单班试点包、基线指标、退出机制和发布前清单；Agent 不联系家长、不发消息、不擅自启动真实试点。

**依赖**：Step 2、Step 17。

**所有权**：试点说明、监护人材料、老师操作卡、数据删除演练、指标模板和 Go/No-Go 报告。

**任务清单**：

- [ ] 先记录微信群现状基线：每份作业老师耗时、漏评率、平均回评等待和家长查找成本。
- [ ] 设计 2-4 周单班试点，限定人数、数据保存期、支持窗口、退出和删除方式。
- [ ] 对主体/类目/备案、协议、监护人同意、真机结果和删除演练逐项判定。

**验收**：

- `PILOT-PACK-01`：试点包不含虚构主体、联系方式、签名或已取得授权的声明。
- `GO-NOGO-01`：任何硬门槛未满足时结论明确为 `NO-GO` 或 `BLOCKED`，不以 LOCAL PASS 替代。
- `METRIC-01`：指标至少包含老师单份总耗时、回评完成率、AI 草稿采用/修改/错误率、家长完成率和隐私事件数。

**Out of scope**：通知家长、签约、上传真实儿童视频、提交微信审核、上线、部署、付费或任何对外发布。

## 11. 统一测试矩阵

| 层级 | 最小检查 | 失败含义 |
|---|---|---|
| SQL | 迁移编号唯一；up/down/up；FK/CHECK/partial unique；跨租户拒绝 | 数据模型不可进入 API 开发 |
| Erlang | `make compile`；相关 Repo/Logic/Handler EUnit；边界检查 | 后端 `LOCAL PASS` 不成立 |
| API | OpenAPI/JSON Schema；TSID；幂等；状态机；权限矩阵 | 前后端不可声明契约稳定 |
| Moya | typecheck、lint、unit/component test、secret scan | 小程序工程不可交付联调 |
| 媒体 | 上传重试、短时 URL、撤回删除、孤儿清理、错误 MIME | 儿童视频路径 `NO-GO` |
| AI | success/timeout/error/bad JSON/no key；人工降级 | 不能启用 AI，但人工闭环可继续 |
| E2E | 家长提交→老师回评→家长查看；多角色切换；跨租户攻击 | 不得进入真实试点 |
| 真机 | iOS/Android 微信拍摄、上传、播放、弱网恢复 | 只能保持 `LOCAL PASS` |
| 合规 | 主体、类目、备案、协议、监护人同意、删除演练 | 正式试点或发布 `NO-GO` |

## 12. 试点指标与退出条件

试点前先采集一周现有微信群流程基线。建议目标不是 AI 分数，而是：

- 老师单份作业从查找视频到发布回评的中位耗时下降至少 30%。
- 已提交作业的回评完成率不低于 95%，且无因系统丢失造成的漏评。
- AI 草稿被老师直接采用或小改后采用的比例达到 60% 以上；“判断错误”必须单独统计。
- 家长提交成功率不低于 95%，失败后能自行重试恢复。
- 任何跨学员视频泄漏、未授权访问或 AI 自动发布均为零容忍事件，发生即暂停试点。
- 如果老师实际总耗时没有下降、AI 错误持续增加复核负担，或多数家长仍回到微信群提交，则暂停 AI 扩展，保留无 AI 的结构化作业闭环。

## 13. 人工确认点

以下动作不因本计划或 `/orchestrate` 命令而获得授权，必须由用户另行明确确认：

1. 选择个人、姐姐所在机构或其他主体注册小程序。
2. 使用任何邮箱、电话、微信号、IM 账号、身份证明、营业执照或联系人信息。
3. 创建/修改微信 AppID、类目、备案、域名、DNS、证书、隐私协议或平台管理员。
4. 设置 Git author/committer、创建 remote、提交、推送、开 PR 或公开发布代码。
5. 联系机构、老师、家长、学员或其他第三方，发送通知、招募或协议。
6. 使用真实儿童姓名、视频、作品、账号和其他个人信息。
7. 部署生产服务、执行生产迁移、创建对象存储桶、调用付费模型或产生费用。

## 14. 最终交付清单

- [x] 当前决策台账与已废弃决策均已标记。
- [x] 微信规则核验、主体方案和儿童隐私门禁材料已完成；正式采用仍需人工/专业复核。
- [x] 家长/老师同小程序 UX、状态和品牌资产候选已完成。
- [x] Organization→Workspace→Group 与 learner 机构归属已实现并有迁移证据。
- [x] 多次 submission、私密附件、AI 草稿和老师 published review 已实现。
- [x] IMBoy 微信登录映射、教学 ACL、API 和 AI Worker 已通过本地测试。
- [ ] 独立 `moya` 仓已完成家长/老师本地产物；微信开发者工具和真机验证未完成。
- [x] 学员未来账号绑定不迁移、不复制历史数据。
- [x] 跨租户、跨 learner、URL 泄漏和角色混淆已有本地 fail-closed 证据；真实环境仍归 Step 17。
- [x] 单班试点包、基线指标、删除演练和 Go/No-Go 报告已准备，当前结论为 `NO-GO`。
- [x] 未经人工确认，没有执行任何外部、生产、发布、联系或真实儿童数据动作。

## 15. 当前推荐启动顺序

不得再从 Step 1-18 整批重跑。续跑先在当前 HEAD 上复核 Acceptance 台账，然后只处理可复现的本地缺口：Step 17 的跨仓契约/HTTP/合成附件闭环、状态口径和回归证据。Step 12、14、15、17 的真机或微信平台子项在没有用户明确授权和真实环境时保持 `PARTIAL/BLOCKED_EXTERNAL`。

续跑并发上限为 6 个活跃 Agent（含 Coordinator）；`.worktrees/**` 全部排除。数据库 Agent 不得创建新迁移，除非当前主仓存在可复现的数据库缺陷、Coordinator 完成全 worktree 迁移号盘点并授予唯一编号。

不建议现在同时做飞书/钉钉，不建议先建完整 SaaS 计费系统，也不建议把老师工作流放回 IMBoy App。当前最小而不堵死未来的组合是：一个墨芽小程序、一个 IMBoy 后端、Organization 多 Workspace 数据边界、一个真实班级试点。
