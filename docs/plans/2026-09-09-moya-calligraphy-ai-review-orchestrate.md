# Plan-Orchestrate Result

**Plan**: `docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md`
**Lang**: `unknown`
**ECC mode**: `plugin`
**Steps**: 18
**Scope**: `all`

## Steps overview

| # | Title | Tags | Chain |
|---|---|---|---|
| 1 | 冻结基线、决策台账与证据目录 | docs, review | `ecc:doc-updater,ecc:code-reviewer` |
| 2 | 核验主体、类目、备案与儿童隐私门禁 | security, docs, lookup, review | `ecc:security-reviewer,ecc:code-reviewer,ecc:doc-updater,ecc:docs-lookup` |
| 3 | 完成角色化 UX 原型与品牌资产规格 | design, impl, review | `ecc:planner,ecc:architect,ecc:tdd-guide,ecc:code-reviewer` |
| 4 | 冻结 API 契约、状态机和威胁模型 | design, security, docs, review | `ecc:planner,ecc:architect,ecc:security-reviewer,ecc:code-reviewer` |
| 5 | 实现 Organization 与 Workspace 兼容迁移 | impl, migration, db, test | `ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer` |
| 6 | 实现班级、学员与监护关系迁移 | impl, migration, db, test | `ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer` |
| 7 | 实现多次提交、AI 草稿与老师回评迁移 | impl, migration, db, test | `ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer` |
| 8 | 实现微信身份、教学上下文与服务端 ACL | impl, test, security | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer` |
| 9 | 实现作业、提交、队列与回评 API | impl, test | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer` |
| 10 | 实现私密视频上传、播放与生命周期 | impl, test, security | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer` |
| 11 | 实现 IMBoy 内部 AI 视频回课 Worker | impl, test, security | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer` |
| 12 | 初始化 moya 微信小程序工程基线 | impl, test, build, docs | `ecc:tdd-guide,ecc:build-error-resolver,ecc:e2e-runner,ecc:code-reviewer` |
| 13 | 实现登录、请求层、身份切换与公共壳 | impl, test, security | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer` |
| 14 | 实现家长端作业提交与成长记录 | impl, test, security | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer` |
| 15 | 实现老师端待评队列与视频回评 | impl, test, security | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer` |
| 16 | 实现学员历史读取与未来账号绑定预留 | impl, test, security | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer` |
| 17 | 完成契约、集成、安全与真机 E2E 验收 | test, security, review | `ecc:tdd-guide,ecc:e2e-runner,ecc:security-reviewer,ecc:code-reviewer` |
| 18 | 准备单班试点包并执行 Go/No-Go 评审 | plan, security, docs, review | `ecc:planner,ecc:security-reviewer,ecc:code-reviewer,ecc:doc-updater` |

---

## Step 1 — 冻结基线、决策台账与证据目录

**Intent**: 记录两个独立仓库的当前状态，把本次会话决策设为实施真源，并建立不含个人信息的证据结构。
**Tags**: docs, review
**Chain rationale**: `doc-updater` 整理决策与证据文档，`code-reviewer` 以当前仓库事实复核；自动语言为 unknown，因此使用通用 reviewer。

```bash
/ecc:orchestrate custom "ecc:doc-updater,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-1] 在 /Users/leeyi/project/imboy.pub/imboy 与独立仓 /Users/leeyi/project/imboy.pub/moya 做只读基线审计，记录 Git root、HEAD、branch、remote、dirty state、最新迁移和空仓状态；把 D-01 至 D-14、Organization→Workspace→Group、learner.organization_id 及旧草案冲突写入当前决策台账，建立 STEP-XX 证据命名并确保无 PII；Acceptance: BASE-01 两个 Git root 有可复核证据；BASE-02 当前真源层级正确；BASE-03 无提交、推送、远端、author 或生产修改；Out of scope: 任何数据库、API、小程序业务代码和外部平台修改。"
```

## Step 2 — 核验主体、类目、备案与儿童隐私门禁

**Intent**: 用现行官方材料核验正式上线约束，形成开发版、真实试点和正式发布的分级门禁与待确认材料。
**Tags**: security, docs, lookup, review
**Chain rationale**: `security-reviewer` 负责儿童敏感信息硬门槛，`docs-lookup` 核验官方规则，`doc-updater` 固化材料，`code-reviewer` 检查边界和证据。

```bash
/ecc:orchestrate custom "ecc:security-reviewer,ecc:code-reviewer,ecc:doc-updater,ecc:docs-lookup" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-2] 仅使用执行时有效的微信官方文档和法律原文，核验个人/机构主体、教育类目、视频能力、认证、备案、支付与未成年人信息要求；草拟监护人同意、隐私、AI 参与、删除导出及机构处理者/IMBoy 受托方条款清单，每项标注来源、日期、条件和待专业复核点；Acceptance: LEGAL-01 规则均有官方来源；LEGAL-02 三阶段有 GO/BLOCKED/NO-GO 条件；LEGAL-03 不替用户选择主体或承担外部责任；Out of scope: 注册公司、提交微信审核/备案、签署协议、联系机构或录入任何真实儿童信息。"
```

## Step 3 — 完成角色化 UX 原型与品牌资产规格

**Intent**: 把家长和老师在同一个小程序里的关键流程做成可走查原型，同时形成可验收的视觉、文案和头像资产。
**Tags**: design, impl, review
**Chain rationale**: `planner,architect` 先收敛角色流程和信息架构，`tdd-guide` 用状态清单驱动原型验收，`code-reviewer` 检查完整性并收尾。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:architect,ecc:tdd-guide,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-3] 设计同一 AppID 下的家长/老师模式、身份与孩子切换、作业提交、AI 等待、老师审核、视频发布、重练和历史回看原型；补齐空态、无权限、上传失败、AI 失败、播放失败和重复发布状态，生成至少三款纸白+墨色笔画+绿色嫩芽的头像候选及 144x144 圆形裁切版本；Acceptance: UX-01 两条流程无需说明即可走通；UX-02 异步状态完整且无溢出；BRAND-01 PNG 尺寸体积和介绍长度合规；Out of scope: 公开发布品牌、注册商标、申请同名账号或把头像上传到微信平台。"
```

## Step 4 — 冻结 API 契约、状态机和威胁模型

**Intent**: 为 Erlang 后端和空白小程序仓建立共同契约，使后续 Agent 能按稳定边界并行实现。
**Tags**: design, security, docs, review
**Chain rationale**: `planner,architect` 负责契约和状态机设计，`security-reviewer` 覆盖租户及儿童视频威胁，`code-reviewer` 复核现有 IMBoy 兼容性。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:architect,ecc:security-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-4] 基于 IMBoy 现有路由、group_task、attachment、sso_identity 和错误码模式，冻结微信登录、教学上下文、作业、submission、待评队列、草稿、发布、重练与历史 OpenAPI/JSON Schema；明确 TSID 字符串传输、分页、幂等键、条件更新及附件 ID 规则，并建立跨 Organization/Workspace/learner、角色混淆、重放和 URL 泄漏威胁用例；Acceptance: API-01 契约通过 Schema 校验且 ID 无精度损失；API-02 所有角色和失败状态有确定响应；SEC-01 deny-by-default 矩阵完整；Out of scope: 实现 API、引入跨平台前端框架或修改生产域名。"
```

## Step 5 — 实现 Organization 与 Workspace 兼容迁移

**Intent**: 用新迁移增加机构租户层，在不破坏历史 Workspace 的前提下支持一个机构多个 Workspace。
**Tags**: impl, migration, db, test
**Chain rationale**: `tdd-guide` 先建立迁移断言，`architect` 检查 expand-first 兼容策略，`database-reviewer` 审核 PostgreSQL 约束，通用 `code-reviewer` 收尾。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-5] 在 imboy 执行前重新读取最新迁移号，新增 organization 表及索引/FK/CHECK，并以 expand-first 方式给 workspace 增加可空 organization_id；不得修改 00000076 等历史迁移，不按 owner_id 猜测或合并历史机构，新建机构与默认 Workspace 使用明确事务边界；Acceptance: DB-ORG-01 空库 up/down/up 且历史校验和不变；DB-ORG-02 一机构多 Workspace 且 Workspace 单归属；DB-ORG-03 旧功能兼容而墨芽无机构上下文 fail closed；Out of scope: Organization 多管理员、套餐、计费、自动归并历史 Workspace 和生产迁移。"
```

## Step 6 — 实现班级、学员与监护关系迁移

**Intent**: 建立独立教学身份与监护关系，并在数据库层表达同机构跨 Workspace、跨机构隔离的不变量。
**Tags**: impl, migration, db, test
**Chain rationale**: `tdd-guide` 先写跨机构失败测试，`architect` 处理通用 Group 与教学模型分离，`database-reviewer` 审查约束，通用 reviewer 最终验收。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-6] 在 Step 5 后由同一迁移 owner 串行新增 class_profile、class_staff、learner、class_enrollment、guardian_learner，包含状态 CHECK、索引、唯一约束、FK 和 learner.user_id 部分唯一约束；用数据库约束或可延迟触发器保证 learner.organization_id 等于 Group→Workspace→Organization，聊天管理员不能替代教学角色；Acceptance: DB-LEARNER-01 同机构跨 Workspace 入班通过而跨机构失败；DB-GUARDIAN-01 多监护关系正确且去重；DB-BIND-01 空账号档案及跨机构独立绑定符合约束；Out of scope: 学员自助注册、绑定码、申诉、批量导入和 Organization 管理员 UI。"
```

## Step 7 — 实现多次提交、AI 草稿与老师回评迁移

**Intent**: 为视频回课建立不会覆盖重练历史的持久化模型，同时兼容普通 IMBoy 群作业。
**Tags**: impl, migration, db, test
**Chain rationale**: `tdd-guide` 固化 attempt 与发布不变量，`architect` 审核与旧 assignment 的兼容，`database-reviewer` 验证索引和约束，通用 reviewer 收尾。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-7] 在同一迁移泳道扩展 group_task_assignment 的可空 learner_id/submitted_by，把旧(task_id,user_id)唯一约束改为仅约束 learner_id IS NULL 的普通群作业部分唯一索引，并为教学作业增加(task_id,learner_id)部分唯一索引，使一个家长可替两个孩子接收同一 task；再新增 homework_submission、submission_asset、calligraphy_review_draft、teacher_review 及 attempt/发布/附件/跨机构约束；Acceptance: DB-SUBMIT-01 多 attempt 且旧证据不可覆盖；DB-ASSIGN-01 多子女可分配而同学员不重复；DB-COMPAT-01 普通群作业测试继续通过；Out of scope: 删除旧 attachment/content 字段、迁移历史群作业附件和生产数据回填。"
```

## Step 8 — 实现微信身份、教学上下文与服务端 ACL

**Intent**: 复用 IMBoy 身份映射并建立统一教学授权入口，避免各 Handler 重复且不一致地判断儿童资源权限。
**Tags**: impl, test, security
**Chain rationale**: `tdd-guide` 驱动认证与 ACL 测试，`e2e-runner` 验证角色旅程，通用 reviewer 检查分层，`security-reviewer` 对 fail-closed 权限做最终关卡。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-8] 复用 sso_identity provider 模式实现微信小程序一次性 code 身份映射和教学 contexts，app secret 只在服务端；在 Logic 层集中解析 JWT 用户、Organization、Workspace、Group、class_staff 与 guardian_learner，对作业、submission、附件、AI 草稿和 review 默认拒绝，忽略客户端自报权限字段；Acceptance: AUTH-01 code 重放和伪造身份安全失败；ACL-01 跨机构/学员及仅群管理员或机构 Owner 读儿童视频均拒绝；ACL-02 多身份切换不串上下文；Out of scope: 微信开放平台账号操作、App Secret 配置到真实环境、手机号快速验证和其他平台登录实现。"
```

## Step 9 — 实现作业、提交、队列与回评 API

**Intent**: 按 IMBoy 分层实现最小视频回课闭环，状态、幂等和事务失败都有可验证行为。
**Tags**: impl, test
**Chain rationale**: `tdd-guide` 从核心状态机测试开始，`e2e-runner` 串接完整 API 旅程，通用 `code-reviewer` 检查 Erlang 分层与最小实现。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-9] 按 Handler→Logic→DS→Repo 实现家长作业列表/详情、幂等 submission、历史读取，以及老师待评队列、工作台草稿、保存、发布和要求重练；复用 group_task、attachment 与通知能力，用事务和条件更新防止半发布、重复 attempt 与重复回评，AI failed 仍进入人工队列；Acceptance: FLOW-01 发布→提交→回评→查看集成测试通过；IDEMP-01 同幂等键不新增记录；STATE-01 非法跳转、重复发布和未授权 reviewer 全部失败；Out of scope: AI 模型调用、前端页面、微信群机器人和公开作品分享。"
```

## Step 10 — 实现私密视频上传、播放与生命周期

**Intent**: 在现有 Garage 附件链路上补齐教学资源授权和清理，确保儿童视频不因 URL 或对象生命周期泄漏。
**Tags**: impl, test, security
**Chain rationale**: `tdd-guide,e2e-runner` 覆盖上传与恢复路径，通用 reviewer 审核复用现有附件能力，`security-reviewer` 最终检查访问和密钥边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-10] 复用 attach presign/confirm/view_url，增加教学附件 scope 或可信资源解析；服务端校验 MIME、扩展名、大小、时长和上传者，每次播放重新检查 submission 权限并只发短时 URL；实现未 confirm、submission 失败、撤回、删除和派生帧的补偿清理，保护已绑定对象；Acceptance: MEDIA-01 非授权家长/同班成员/非任课老师无播放 URL；MEDIA-02 重试与孤儿清理可验证且不误删；MEDIA-03 表和日志无持久 URL、密钥或视频内容；Out of scope: CDN 公开分发、直播、跨区域复制和生产桶策略修改。"
```

## Step 11 — 实现 IMBoy 内部 AI 视频回课 Worker

**Intent**: 用 IMBoy 内部异步任务生成结构化老师草稿，并证明任何模型故障都不会阻断人工回评。
**Tags**: impl, test, security
**Chain rationale**: `tdd-guide,e2e-runner` 验证异步成功和失败分支，通用 reviewer 审核 provider 复用，`security-reviewer` 关闭儿童数据、密钥和提示注入风险。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-11] 在 IMBoy 内部实现书法 AI job/worker：任务仅存业务 ID 与 prompt/rubric 版本，运行时授权读取附件，限制转码抽帧资源与超时，复用 imboy_llm provider 并校验 JSON Schema 后写草稿；错误写 failed 并进入人工队列，记录老师采用/修改/标错但不默认用于训练；Acceptance: AI-01 五类成功失败路径和重试上限确定；AI-02 草稿含版本/digest、无思维链且家长不可读；AI-03 无 key/provider 时人工闭环仍通过；Out of scope: DeepFlux、独立 AI 服务、AI 自动发布、真实儿童数据训练和逐字权威评分。"
```

## Step 12 — 初始化 moya 微信小程序工程基线

**Intent**: 在独立空仓建立原生微信小程序 TypeScript 最小工程和质量门，不为尚未实现的飞书/钉钉增加抽象。
**Tags**: impl, test, build, docs
**Chain rationale**: `tdd-guide` 建立最小检查，`build-error-resolver` 解决空仓构建问题，`e2e-runner` 验证可导入首屏，通用 reviewer 收尾；unknown 语言按规则使用通用 build/review。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:build-error-resolver,ecc:e2e-runner,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-12] 在独立 Git root /Users/leeyi/project/imboy.pub/moya 初始化原生微信小程序+TypeScript 最小工程，提供非空首屏、README、示例环境、typecheck、lint、unit test 和 secret 检查；API DTO 保持平台中立，openid 不散落业务页，真实 AppID/API 地址/密钥不入 Git，不添加无当前必要性的跨端框架、状态库或 UI 库；Acceptance: MOYA-BASE-01 开发者工具可导入且命令行检查通过；MOYA-BASE-02 无 secret/PII/服务端凭据；MOYA-BASE-03 无投机多端依赖；Out of scope: 创建远端、提交/推送、微信平台配置、飞书/钉钉客户端和生产域名。"
```

## Step 13 — 实现登录、请求层、身份切换与公共壳

**Intent**: 建立家长和老师页面共同依赖的登录、会话、ID、上下文与导航能力，并在完成后冻结公共接口。
**Tags**: impl, test, security
**Chain rationale**: `tdd-guide,e2e-runner` 覆盖多身份和会话恢复，通用 reviewer 检查 TypeScript/微信实现，`security-reviewer` 审核 token、缓存和上下文隔离。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-13] 在 moya 实现 wx.login→IMBoy token、统一请求/错误处理、Organization/Workspace/班级 contexts、家长/老师导航和身份切换；处理登录失败、token 过期、无身份、多身份和上下文撤销，所有 TSID 按 string，切换时清理不属于新上下文的缓存，token 不写普通日志；Acceptance: MOYA-AUTH-01 模拟登录和刷新测试通过；MOYA-ROLE-01 单/多身份、直接越权入口行为正确；MOYA-ID-01 最大 64-bit ID 往返不变；Out of scope: 家长作业页面、老师点评页面和真实微信账号验收。"
```

## Step 14 — 实现家长端作业提交与成长记录

**Intent**: 完成家长替孩子提交练字视频、等待和查看老师回评的完整移动端体验，并隔离多个孩子的数据缓存。
**Tags**: impl, test, security
**Chain rationale**: `tdd-guide,e2e-runner` 驱动页面状态与真机旅程，通用 reviewer 检查实现，`security-reviewer` 最终验证 learner 隔离和回评可见性。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-14] 在 moya 家长独占目录实现孩子切换、作业列表/详情、视频和照片选择预览、上传进度、取消重试、幂等提交、AI 等待、已发布回评、重练入口和 attempt 历史；不得改公共请求契约，切换孩子先清理旧缓存，页面绝不显示 AI 草稿或未发布 review；Acceptance: PARENT-01 成功/空态/上传失败/AI失败/重练测试完整；PARENT-02 不串孩子且篡改 learner_id 被拒；PARENT-03 真机录制、后台恢复和上传通过；Out of scope: 课程购买、公开分享、家长群聊天和学员独立账号 UI。"
```

## Step 15 — 实现老师端待评队列与视频回评

**Intent**: 让老师在小程序内连续处理提交、核对 AI 并发布真人视频，同时保持无 AI 的完整人工路径。
**Tags**: impl, test, security
**Chain rationale**: `tdd-guide,e2e-runner` 覆盖队列到发布旅程，通用 reviewer 检查页面和状态，`security-reviewer` 最终验证 staff 角色与视频权限。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-15] 在 moya 老师独占目录实现待评队列筛选、学生视频/照片工作台、AI queued/running/succeeded/failed 展示、采用/修改/标错、无 AI 人工填写、真人视频录制预览、保存草稿、发布确认和成功后下一条；发布失败保留草稿且不得改家长页；Acceptance: TEACHER-01 队列/草稿/重复发布/人工路径测试通过；TEACHER-02 非任课老师、受限 assistant 和 removed staff 被拒；TEACHER-03 真机录制预览重试发布通过；Out of scope: 自动发微信群、班级直播、AI 代老师发布和复杂排课管理。"
```

## Step 16 — 实现学员历史读取与未来账号绑定预留

**Intent**: 证明教学历史稳定归属于 learner，而不是家长账号或 Workspace，并为未来学员账号提供受控绑定点。
**Tags**: impl, test, security
**Chain rationale**: `tdd-guide,e2e-runner` 验证绑定前后和跨 Workspace 历史，通用 reviewer 检查最小实现，`security-reviewer` 关闭跨机构合并与解绑残权风险。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-16] 实现 learner 历史查询和最小受控账号绑定/解绑 Logic：同 Organization 聚合多个 Workspace/班级的已发布回评，跨 Organization 始终隔离；绑定记录 account_bound_by/at，不能复制或迁移 submission/review，解绑仅移除账号入口且保留档案、监护权限和审计，本期不开放自助 UI；Acceptance: HISTORY-01 同机构历史连续且跨机构不可读；BIND-01 绑定前后主键和 learner_id 不变；BIND-02 解绑立即撤销账号入口且历史不删除；Out of scope: 公开学员注册入口、短期绑定码、姓名搜索认领、申诉和自动合并跨机构档案。"
```

## Step 17 — 完成契约、集成、安全与真机 E2E 验收

**Intent**: 汇总后端、小程序、对象存储和 AI 降级证据，严格区分本地自动化和真实设备结果。
**Tags**: test, security, review
**Chain rationale**: `tdd-guide,e2e-runner` 组织自动化和真实旅程，`security-reviewer` 执行越权矩阵，通用 `code-reviewer` 汇总跨仓缺陷与证据。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:security-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-17] 使用合成视频执行 imboy compile、相关 EUnit、迁移 up/down/up、OpenAPI/Schema、moya typecheck/lint/unit 和端到端旅程；创建两个 Organization 覆盖跨租户、跨 learner、短时 URL 重放、assistant 越权、伪造 reviewer、AI 无 key 降级，并在至少两台真实手机核验拍摄上传播放、身份切换和弱网恢复；Acceptance: TEST-01 命令/退出码齐全；SEC-E2E-01 攻击均 fail closed；DEVICE-01 真机证据独立于 mock/开发者工具；Out of scope: 真实儿童数据、生产环境、应用商店审核、压力容量承诺和未授权第三方测试。"
```

## Step 18 — 准备单班试点包并执行 Go/No-Go 评审

**Intent**: 把技术结果转成需要用户与机构人工批准的真实试点材料，任何硬门槛缺失时明确停止。
**Tags**: plan, security, docs, review
**Chain rationale**: `planner` 组织试点和指标，`security-reviewer` 检查儿童数据硬门槛，`code-reviewer` 核对证据等级，`doc-updater` 形成可交付材料。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:security-reviewer,ecc:code-reviewer,ecc:doc-updater" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-18] 准备 2-4 周单班试点包：微信群现状基线采集表、限定人数与保存期、老师操作卡、监护人材料、支持/退出/删除流程、指标模板和 Go/No-Go 报告；逐项核对主体类目备案、协议同意、真机和删除演练，Agent 只准备材料，不联系或启动真实试点；Acceptance: PILOT-PACK-01 无虚构主体/联系方式/授权声明；GO-NOGO-01 硬门槛缺失即 NO-GO/BLOCKED；METRIC-01 覆盖耗时、完成率、AI采用修改错误率、家长成功率和隐私事件；Out of scope: 通知家长、签约、上传真实儿童视频、提交微信审核、上线、部署、付费或任何对外发布。"
```

## Batch execution

```bash
/ecc:orchestrate custom "ecc:doc-updater,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-1] 在 /Users/leeyi/project/imboy.pub/imboy 与独立仓 /Users/leeyi/project/imboy.pub/moya 做只读基线审计，记录 Git root、HEAD、branch、remote、dirty state、最新迁移和空仓状态；把 D-01 至 D-14、Organization→Workspace→Group、learner.organization_id 及旧草案冲突写入当前决策台账，建立 STEP-XX 证据命名并确保无 PII；Acceptance: BASE-01 两个 Git root 有可复核证据；BASE-02 当前真源层级正确；BASE-03 无提交、推送、远端、author 或生产修改；Out of scope: 任何数据库、API、小程序业务代码和外部平台修改。"
/ecc:orchestrate custom "ecc:security-reviewer,ecc:code-reviewer,ecc:doc-updater,ecc:docs-lookup" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-2] 仅使用执行时有效的微信官方文档和法律原文，核验个人/机构主体、教育类目、视频能力、认证、备案、支付与未成年人信息要求；草拟监护人同意、隐私、AI 参与、删除导出及机构处理者/IMBoy 受托方条款清单，每项标注来源、日期、条件和待专业复核点；Acceptance: LEGAL-01 规则均有官方来源；LEGAL-02 三阶段有 GO/BLOCKED/NO-GO 条件；LEGAL-03 不替用户选择主体或承担外部责任；Out of scope: 注册公司、提交微信审核/备案、签署协议、联系机构或录入任何真实儿童信息。"
/ecc:orchestrate custom "ecc:planner,ecc:architect,ecc:tdd-guide,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-3] 设计同一 AppID 下的家长/老师模式、身份与孩子切换、作业提交、AI 等待、老师审核、视频发布、重练和历史回看原型；补齐空态、无权限、上传失败、AI 失败、播放失败和重复发布状态，生成至少三款纸白+墨色笔画+绿色嫩芽的头像候选及 144x144 圆形裁切版本；Acceptance: UX-01 两条流程无需说明即可走通；UX-02 异步状态完整且无溢出；BRAND-01 PNG 尺寸体积和介绍长度合规；Out of scope: 公开发布品牌、注册商标、申请同名账号或把头像上传到微信平台。"
/ecc:orchestrate custom "ecc:planner,ecc:architect,ecc:security-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-4] 基于 IMBoy 现有路由、group_task、attachment、sso_identity 和错误码模式，冻结微信登录、教学上下文、作业、submission、待评队列、草稿、发布、重练与历史 OpenAPI/JSON Schema；明确 TSID 字符串传输、分页、幂等键、条件更新及附件 ID 规则，并建立跨 Organization/Workspace/learner、角色混淆、重放和 URL 泄漏威胁用例；Acceptance: API-01 契约通过 Schema 校验且 ID 无精度损失；API-02 所有角色和失败状态有确定响应；SEC-01 deny-by-default 矩阵完整；Out of scope: 实现 API、引入跨平台前端框架或修改生产域名。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-5] 在 imboy 执行前重新读取最新迁移号，新增 organization 表及索引/FK/CHECK，并以 expand-first 方式给 workspace 增加可空 organization_id；不得修改 00000076 等历史迁移，不按 owner_id 猜测或合并历史机构，新建机构与默认 Workspace 使用明确事务边界；Acceptance: DB-ORG-01 空库 up/down/up 且历史校验和不变；DB-ORG-02 一机构多 Workspace 且 Workspace 单归属；DB-ORG-03 旧功能兼容而墨芽无机构上下文 fail closed；Out of scope: Organization 多管理员、套餐、计费、自动归并历史 Workspace 和生产迁移。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-6] 在 Step 5 后由同一迁移 owner 串行新增 class_profile、class_staff、learner、class_enrollment、guardian_learner，包含状态 CHECK、索引、唯一约束、FK 和 learner.user_id 部分唯一约束；用数据库约束或可延迟触发器保证 learner.organization_id 等于 Group→Workspace→Organization，聊天管理员不能替代教学角色；Acceptance: DB-LEARNER-01 同机构跨 Workspace 入班通过而跨机构失败；DB-GUARDIAN-01 多监护关系正确且去重；DB-BIND-01 空账号档案及跨机构独立绑定符合约束；Out of scope: 学员自助注册、绑定码、申诉、批量导入和 Organization 管理员 UI。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:architect,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-7] 在同一迁移泳道扩展 group_task_assignment 的可空 learner_id/submitted_by，把旧(task_id,user_id)唯一约束改为仅约束 learner_id IS NULL 的普通群作业部分唯一索引，并为教学作业增加(task_id,learner_id)部分唯一索引，使一个家长可替两个孩子接收同一 task；再新增 homework_submission、submission_asset、calligraphy_review_draft、teacher_review 及 attempt/发布/附件/跨机构约束；Acceptance: DB-SUBMIT-01 多 attempt 且旧证据不可覆盖；DB-ASSIGN-01 多子女可分配而同学员不重复；DB-COMPAT-01 普通群作业测试继续通过；Out of scope: 删除旧 attachment/content 字段、迁移历史群作业附件和生产数据回填。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-8] 复用 sso_identity provider 模式实现微信小程序一次性 code 身份映射和教学 contexts，app secret 只在服务端；在 Logic 层集中解析 JWT 用户、Organization、Workspace、Group、class_staff 与 guardian_learner，对作业、submission、附件、AI 草稿和 review 默认拒绝，忽略客户端自报权限字段；Acceptance: AUTH-01 code 重放和伪造身份安全失败；ACL-01 跨机构/学员及仅群管理员或机构 Owner 读儿童视频均拒绝；ACL-02 多身份切换不串上下文；Out of scope: 微信开放平台账号操作、App Secret 配置到真实环境、手机号快速验证和其他平台登录实现。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-9] 按 Handler→Logic→DS→Repo 实现家长作业列表/详情、幂等 submission、历史读取，以及老师待评队列、工作台草稿、保存、发布和要求重练；复用 group_task、attachment 与通知能力，用事务和条件更新防止半发布、重复 attempt 与重复回评，AI failed 仍进入人工队列；Acceptance: FLOW-01 发布→提交→回评→查看集成测试通过；IDEMP-01 同幂等键不新增记录；STATE-01 非法跳转、重复发布和未授权 reviewer 全部失败；Out of scope: AI 模型调用、前端页面、微信群机器人和公开作品分享。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-10] 复用 attach presign/confirm/view_url，增加教学附件 scope 或可信资源解析；服务端校验 MIME、扩展名、大小、时长和上传者，每次播放重新检查 submission 权限并只发短时 URL；实现未 confirm、submission 失败、撤回、删除和派生帧的补偿清理，保护已绑定对象；Acceptance: MEDIA-01 非授权家长/同班成员/非任课老师无播放 URL；MEDIA-02 重试与孤儿清理可验证且不误删；MEDIA-03 表和日志无持久 URL、密钥或视频内容；Out of scope: CDN 公开分发、直播、跨区域复制和生产桶策略修改。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-11] 在 IMBoy 内部实现书法 AI job/worker：任务仅存业务 ID 与 prompt/rubric 版本，运行时授权读取附件，限制转码抽帧资源与超时，复用 imboy_llm provider 并校验 JSON Schema 后写草稿；错误写 failed 并进入人工队列，记录老师采用/修改/标错但不默认用于训练；Acceptance: AI-01 五类成功失败路径和重试上限确定；AI-02 草稿含版本/digest、无思维链且家长不可读；AI-03 无 key/provider 时人工闭环仍通过；Out of scope: DeepFlux、独立 AI 服务、AI 自动发布、真实儿童数据训练和逐字权威评分。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:build-error-resolver,ecc:e2e-runner,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-12] 在独立 Git root /Users/leeyi/project/imboy.pub/moya 初始化原生微信小程序+TypeScript 最小工程，提供非空首屏、README、示例环境、typecheck、lint、unit test 和 secret 检查；API DTO 保持平台中立，openid 不散落业务页，真实 AppID/API 地址/密钥不入 Git，不添加无当前必要性的跨端框架、状态库或 UI 库；Acceptance: MOYA-BASE-01 开发者工具可导入且命令行检查通过；MOYA-BASE-02 无 secret/PII/服务端凭据；MOYA-BASE-03 无投机多端依赖；Out of scope: 创建远端、提交/推送、微信平台配置、飞书/钉钉客户端和生产域名。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-13] 在 moya 实现 wx.login→IMBoy token、统一请求/错误处理、Organization/Workspace/班级 contexts、家长/老师导航和身份切换；处理登录失败、token 过期、无身份、多身份和上下文撤销，所有 TSID 按 string，切换时清理不属于新上下文的缓存，token 不写普通日志；Acceptance: MOYA-AUTH-01 模拟登录和刷新测试通过；MOYA-ROLE-01 单/多身份、直接越权入口行为正确；MOYA-ID-01 最大 64-bit ID 往返不变；Out of scope: 家长作业页面、老师点评页面和真实微信账号验收。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-14] 在 moya 家长独占目录实现孩子切换、作业列表/详情、视频和照片选择预览、上传进度、取消重试、幂等提交、AI 等待、已发布回评、重练入口和 attempt 历史；不得改公共请求契约，切换孩子先清理旧缓存，页面绝不显示 AI 草稿或未发布 review；Acceptance: PARENT-01 成功/空态/上传失败/AI失败/重练测试完整；PARENT-02 不串孩子且篡改 learner_id 被拒；PARENT-03 真机录制、后台恢复和上传通过；Out of scope: 课程购买、公开分享、家长群聊天和学员独立账号 UI。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-15] 在 moya 老师独占目录实现待评队列筛选、学生视频/照片工作台、AI queued/running/succeeded/failed 展示、采用/修改/标错、无 AI 人工填写、真人视频录制预览、保存草稿、发布确认和成功后下一条；发布失败保留草稿且不得改家长页；Acceptance: TEACHER-01 队列/草稿/重复发布/人工路径测试通过；TEACHER-02 非任课老师、受限 assistant 和 removed staff 被拒；TEACHER-03 真机录制预览重试发布通过；Out of scope: 自动发微信群、班级直播、AI 代老师发布和复杂排课管理。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-16] 实现 learner 历史查询和最小受控账号绑定/解绑 Logic：同 Organization 聚合多个 Workspace/班级的已发布回评，跨 Organization 始终隔离；绑定记录 account_bound_by/at，不能复制或迁移 submission/review，解绑仅移除账号入口且保留档案、监护权限和审计，本期不开放自助 UI；Acceptance: HISTORY-01 同机构历史连续且跨机构不可读；BIND-01 绑定前后主键和 learner_id 不变；BIND-02 解绑立即撤销账号入口且历史不删除；Out of scope: 公开学员注册入口、短期绑定码、姓名搜索认领、申诉和自动合并跨机构档案。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:security-reviewer,ecc:code-reviewer" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-17] 使用合成视频执行 imboy compile、相关 EUnit、迁移 up/down/up、OpenAPI/Schema、moya typecheck/lint/unit 和端到端旅程；创建两个 Organization 覆盖跨租户、跨 learner、短时 URL 重放、assistant 越权、伪造 reviewer、AI 无 key 降级，并在至少两台真实手机核验拍摄上传播放、身份切换和弱网恢复；Acceptance: TEST-01 命令/退出码齐全；SEC-E2E-01 攻击均 fail closed；DEVICE-01 真机证据独立于 mock/开发者工具；Out of scope: 真实儿童数据、生产环境、应用商店审核、压力容量承诺和未授权第三方测试。"
/ecc:orchestrate custom "ecc:planner,ecc:security-reviewer,ecc:code-reviewer,ecc:doc-updater" "[Plan: docs/plans/2026-09-09-moya-calligraphy-ai-review-execution-plan.md#step-18] 准备 2-4 周单班试点包：微信群现状基线采集表、限定人数与保存期、老师操作卡、监护人材料、支持/退出/删除流程、指标模板和 Go/No-Go 报告；逐项核对主体类目备案、协议同意、真机和删除演练，Agent 只准备材料，不联系或启动真实试点；Acceptance: PILOT-PACK-01 无虚构主体/联系方式/授权声明；GO-NOGO-01 硬门槛缺失即 NO-GO/BLOCKED；METRIC-01 覆盖耗时、完成率、AI采用修改错误率、家长成功率和隐私事件；Out of scope: 通知家长、签约、上传真实儿童视频、提交微信审核、上线、部署、付费或任何对外发布。"
```
