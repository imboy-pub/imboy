# AI Agent 角色与能力管理实施计划

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 将 imboy 的 AI 助手/Agent 与 AI 角色模板收敛为清晰的两层模型，让角色统一管理提示词、能力和知识库成本策略，助手通过角色继承行为，并在管理后台支持分页、草稿、测试、发布和自动化验证。

**Architecture:** `AI 助手/Agent` 继续表示可聊天的用户账号和运行实例；新增持久化的 `AI 角色模板` 作为行为来源，保存 system prompt、固定能力策略和知识库检索策略。平台安全规则与成本上限位于最外层，角色只能在平台边界内生效；助手只管理身份、模型、可见性和角色绑定，不再维护独立的能力副本。现有 `ai_roles` KV、`ai_agent.system_prompt` 和 `ai_agent.capabilities` 保留为兼容回退，待新链路稳定后再移除。

**Tech Stack:** Erlang/OTP 28、Cowboy、PostgreSQL migration、EUnit/meck、React 19、TypeScript、TanStack Query/Table、Bun test、Testing Library、Playwright。

---

## 0. 范围、术语与不做事项

### 产品术语

| 产品术语 | 技术含义 | 本计划处理方式 |
|---|---|---|
| AI 助手 | 用户可搜索、添加、聊天的 AI 账号 | 对应现有 `ai_agent`，继续分页管理 |
| AI Agent | AI 助手的后端/运行时名称 | 不新增第三种实体 |
| AI 角色模板 | 可被多个助手绑定的行为配置 | 新增独立持久化模型和管理页 |
| 知识库 | 被角色按策略检索的外部上下文 | 独立于角色 Prompt，第一版复用现有 FAQ/群规来源 |

### 第一版能力目录

只实现有后端执行路径的能力，不允许后台自由输入任意 capability key：

1. 基础对话：隐式开启，不显示开关。
2. 知识库：`off`、`on_demand`、`required`，默认 `on_demand`。
3. 群聊回复：关闭或 `mention_only`，默认关闭。
4. 主动消息：关闭或 `welcome_only`，默认关闭，并继续受现有 onboarding 与限流控制。

以下内容明确排除在第一版之外：多 Agent 协作、AI Group、任意工具市场、外部搜索、代码执行、文件分析、图片理解、复杂可视化工作流、按 token 对客户计费。

### 既有现场保护

开始任何实现前必须在两个仓库分别记录：

```bash
cd /Users/leeyi/project/imboy.pub/imboy
git status --short --branch

cd /Users/leeyi/project/imboy.pub/imboyadmin
git status --short --branch
```

当前已有 AI 相关未提交修改，实施时只允许提交本计划新增或明确修改的文件，禁止使用 `git reset --hard`、`git checkout --` 或覆盖既有工作。

---

## 1. 固化 API、数据和有效配置契约

**目标:** 在写业务代码前把“角色继承、能力策略、知识库成本策略、兼容回退”写成可测试契约。

**Files:**

- Create: `/Users/leeyi/project/imboy.pub/imboy/docs/api-contracts/ai_agent_role_capability_api_contract.md`
- Create: `/Users/leeyi/project/imboy.pub/imboy/docs/adr/2026-08-08-ai-agent-role-inheritance.md`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/docs/roadmap/ai-agent-platform-roadmap.md`（只补充已决定边界，不重写历史记录）

### Step 1: 写契约中的失败示例

先写出以下必须被拒绝或降级的场景：

- 角色缺少 `code`、`name` 或空 `system_prompt`。
- 能力包含未知 key。
- `knowledge.mode` 不是 `off|on_demand|required`。
- `group_reply` 不是关闭或 `mention_only`。
- 角色发布时存在未绑定的知识库来源。
- Agent 绑定不存在或已停用角色。
- 角色缺失时，旧 Agent 能够回退到旧字段并产生日志，而不是静默改变行为。

### Step 2: 固化有效配置优先级

契约必须明确：

```text
平台安全边界 > 角色发布版本 > Agent 身份/模型配置 > legacy fallback
```

其中：

- `system_prompt` 和 capabilities 由角色发布版本提供。
- `provider`、`model`、头像、昵称、可见性、状态属于 Agent。
- E2EE 场景始终禁止服务端 AI，不受角色开关覆盖。
- Agent 不能通过请求体覆盖角色能力。

### Step 3: 写 JSON 示例和分页响应示例

角色发布版本的规范化配置示例：

```json
{
  "code": "official_welcome",
  "name": "官方新手助手",
  "description": "负责新用户欢迎和 imboy 功能介绍",
  "system_prompt": "你是 imboy 官方 AI 新手助手……",
  "capabilities": {
    "knowledge": {
      "mode": "on_demand",
      "source": "faq",
      "max_context_bytes": 2400
    },
    "group_reply": {
      "mode": "off"
    },
    "proactive": {
      "mode": "welcome_only",
      "daily_limit": 1
    }
  }
}
```

列表接口必须统一返回 `page/size/total/items`，角色行至少包含 `code/name/status/active_version/bound_agent_count/updated_at`。

### Step 4: 文档校验

Run:

```bash
cd /Users/leeyi/project/imboy.pub/imboy
git diff --check -- docs/api-contracts/ai_agent_role_capability_api_contract.md docs/adr/2026-08-08-ai-agent-role-inheritance.md
```

Expected: 无空白错误；契约能够被后续 EUnit、前端单测和 E2E 直接引用。

### Step 5: Commit

```bash
git add docs/api-contracts/ai_agent_role_capability_api_contract.md docs/adr/2026-08-08-ai-agent-role-inheritance.md docs/roadmap/ai-agent-platform-roadmap.md
git commit -m "docs: define ai agent role inheritance contract"
```

---

## 2. 新增角色模板和版本持久化

**目标:** 用可分页、可审计、可发布的数据库模型替代 `config.ai_roles` 作为主存储。

**Files:**

- Create: `/Users/leeyi/project/imboy.pub/imboy/priv/migrations/00000058_ai_agent_role.up.sql`
- Create: `/Users/leeyi/project/imboy.pub/imboy/priv/migrations/00000058_ai_agent_role.down.sql`
- Create: `/Users/leeyi/project/imboy.pub/imboy/src/repo/ai_agent_role_repo.erl`
- Create: `/Users/leeyi/project/imboy.pub/imboy/src/ds/ai_agent_role_ds.erl`
- Create: `/Users/leeyi/project/imboy.pub/imboy/test/repo/ai_agent_role_repo_tests.erl`
- Create: `/Users/leeyi/project/imboy.pub/imboy/test/ds/ai_agent_role_ds_tests.erl`

### Step 1: 写迁移 RED 测试/校验

先增加迁移约束验证，覆盖：

- 角色 code 唯一且长度受限。
- 角色版本 `(role_code, version)` 唯一。
- 一个角色最多一个 published 版本。
- 版本状态只能是 `draft|published|archived`。
- capabilities 和 knowledge policy 默认是合法空策略。
- 删除角色前不能存在有效 Agent 引用，或后端必须返回明确冲突错误。

### Step 2: 创建最小表结构

建议新增两张表：

```text
ai_agent_role
  code varchar(40) primary key
  name varchar(80) not null
  description text not null default ''
  status smallint not null default 1
  active_version integer not null default 0
  created_by bigint not null default 0
  created_at timestamptz not null
  updated_at timestamptz not null

ai_agent_role_version
  id bigint primary key
  role_code varchar(40) references ai_agent_role(code)
  version integer not null
  state varchar(16) check (state in ('draft','published','archived'))
  system_prompt text not null
  capabilities jsonb not null default '{}'
  knowledge_policy jsonb not null default '{}'
  created_by bigint not null default 0
  published_by bigint
  created_at timestamptz not null
  published_at timestamptz
  unique(role_code, version)
```

`ai_agent.role_id` 第一阶段继续保存角色 `code`，避免立刻改变现有 varchar 字段和历史数据；它是业务 code，不是 TSID。

### Step 3: 实现 Repo 查询和写入

`ai_agent_role_repo.erl` 至少实现：

- `page(Page, Size, Filters)`
- `find(Code)`
- `create(Role)`
- `update_metadata(Code, Patch)`
- `create_draft(Code, Config)`
- `publish(Code, Version, AdminUid)`
- `set_status(Code, Status)`
- `count_bound_agents(Code)`
- `count_published_version(Code)`

所有动态条件参数化；Page、Size 经过正整数校验后才能用于 LIMIT/OFFSET。

### Step 4: 实现 DS 校验和 legacy 迁移读取

`ai_agent_role_ds.erl` 负责：

- 规范化 binary key。
- 校验能力目录和 knowledge policy。
- 保存草稿。
- 发布前校验角色完整性。
- 读取角色失败时回退旧 `config_ds:get(<<"ai_roles">>)`，并打出明确 warning。

不要在 Handler 中直接调用 Repo；遵循现有 Handler → Logic/DS → Repo 边界。

### Step 5: 运行 RED、实现 GREEN

Run:

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make eunit t=ai_agent_role_repo_tests
make eunit t=ai_agent_role_ds_tests
```

Expected first run: 新测试因模块/函数不存在而失败，且失败原因不能是无关编译错误；实现后相同命令全部通过。

### Step 6: Commit RED 和 GREEN

```bash
git add priv/migrations/00000058_ai_agent_role.* test/repo/ai_agent_role_repo_tests.erl test/ds/ai_agent_role_ds_tests.erl
git commit -m "test: add ai agent role persistence contract"

git add src/repo/ai_agent_role_repo.erl src/ds/ai_agent_role_ds.erl priv/migrations/00000058_ai_agent_role.*
git commit -m "feat: persist versioned ai agent roles"
```

若当前已有未提交修改导致无法安全分离提交，必须先记录文件清单并只提交本任务新增文件，不能把其他现场一起提交。

---

## 3. 实现有效角色配置和固定能力策略

**目标:** 让运行时真正使用角色发布版本，并让 capability 从“数据库字段”变成“执行门控”。

**Files:**

- Create: `/Users/leeyi/project/imboy.pub/imboy/src/logic/ai_agent_policy.erl`
- Create: `/Users/leeyi/project/imboy.pub/imboy/test/logic/ai_agent_policy_tests.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/ds/ai_agent_ds.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/logic/ai_agent_reply.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/logic/ai_agent_group_reply.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/logic/ai_agent_proactive.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/test/logic/ai_agent_reply_tests.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/test/logic/ai_agent_group_reply_tests.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/test/logic/ai_agent_proactive_tests.erl`

### Step 1: 写策略函数 RED 测试

测试 `ai_agent_policy.erl` 的纯函数：

- 角色发布版本优先于 Agent legacy 字段。
- 缺失角色时回退 legacy 字段。
- 未知 capability 被拒绝，不被透传。
- 空策略得到安全默认值：knowledge off、group_reply off、proactive off。
- 平台硬规则覆盖角色配置：E2EE 永不执行 AI；群聊不能绕过 mention-only；主动消息不能绕过 onboarding 和限流。
- Agent 请求体中的 capabilities 字段不能覆盖 effective policy。

### Step 2: 添加群聊和主动消息门控测试

新增失败用例：

- `group_reply=off` 时，@Agent 不调用 LLM。
- `group_reply=mention_only` 时，普通群消息不调用 LLM，明确 @ 才调用。
- `proactive=off` 时，欢迎流程不调用 Agent LLM。
- `proactive=welcome_only` 时，只允许 onboarding 欢迎路径，其他主动触发仍拒绝。
- 所有 deny 路径不产生模型调用、不产生消息投递。

### Step 3: 实现 effective policy

`ai_agent_policy:effective/1` 返回：

```erlang
#{
  role_code => <<"official_welcome">>,
  system_prompt => Prompt,
  capabilities => NormalizedCapabilities,
  knowledge_policy => KnowledgePolicy,
  source => role | legacy_fallback
}
```

`ai_agent_ds:is_agent/1` 和 `get/1` 需要把 effective policy 提供给下游；如果角色不存在，保留旧行为并记录可检索日志。

### Step 4: 接入执行门控

- `ai_agent_reply.erl`：基础 C2C 对话继续允许，但 Prompt 使用 effective role。
- `ai_agent_group_reply.erl`：进入 LLM 前检查 `group_reply`。
- `ai_agent_proactive.erl`：进入个性化欢迎 LLM 前检查 `proactive`。
- 保留现有 `agent_rate_limiter` 和 E2EE fail-closed 逻辑。

### Step 5: 运行 RED/GREEN

Run:

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make eunit t=ai_agent_policy_tests
make eunit t=ai_agent_reply_tests
make eunit t=ai_agent_group_reply_tests
make eunit t=ai_agent_proactive_tests
```

Expected: 新门控测试先因 capability 未参与运行时而失败；实现后全部通过，且旧测试无回归。

### Step 6: Commit

```bash
git add src/logic/ai_agent_policy.erl src/ds/ai_agent_ds.erl src/logic/ai_agent_reply.erl src/logic/ai_agent_group_reply.erl src/logic/ai_agent_proactive.erl test/logic/ai_agent_policy_tests.erl test/logic/ai_agent_reply_tests.erl test/logic/ai_agent_group_reply_tests.erl test/logic/ai_agent_proactive_tests.erl
git commit -m "feat: enforce ai agent role capabilities"
```

---

## 4. 实现低成本、按需知识库策略

**目标:** 取消“每次请求追加完整知识库”的高成本路径，先以关键词/规则过滤和长度预算实现低成本按需注入。

**Files:**

- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/logic/ai_agent_kb_logic.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/logic/ai_agent_prompt.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/test/logic/ai_agent_kb_logic_tests.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/test/logic/ai_agent_prompt_tests.erl`
- Create: `/Users/leeyi/project/imboy.pub/imboy/test/logic/ai_agent_knowledge_policy_tests.erl`

### Step 1: 写检索策略 RED 测试

覆盖以下行为：

- `off`：不读取知识库，不生成 system context。
- `on_demand` + 无关键词命中：不读取或不注入知识库。
- `on_demand` + 命中 FAQ：只注入 FAQ 相关片段。
- `on_demand` + 命中群规：只注入群规片段。
- `required`：即使没有关键词，也检索指定来源。
- 空知识库、超过长度上限、异常读取：安全返回空上下文，不拖垮主回复。
- 注入内容不超过 `max_context_bytes`。

### Step 2: 实现 cheap-first retrieval

第一版只使用现有 FAQ/群规配置：

```text
消息文本
  ↓
角色 knowledge.mode
  ├── off：直接 LLM
  ├── on_demand：关键词/规则命中后截取上下文
  └── required：按角色 source 读取上下文
```

不要在第一版新增 embedding 或外部 rerank。保留未来替换点，例如：

```erlang
ai_agent_kb_logic:retrieve(Query, KnowledgePolicy) ->
    {ok, #{text := Context, source := Source, chunks := Count}} | {skip, Reason}.
```

### Step 3: 改造 Prompt 组装

`ai_agent_prompt:build_messages/2` 和 `build_messages_with_user/2` 必须从 Agent effective policy 获取知识库策略，而不是无条件调用旧的 `kb_text/0`。

消息正文为空、E2EE 或 capability 禁止时，不能触发知识库读取。

### Step 4: 验证成本护栏

至少记录结构化元数据，不记录用户原文：

- `knowledge_attempted`
- `knowledge_hit`
- `source`
- `context_bytes`
- `chunks`
- `skip_reason`

### Step 5: 运行 RED/GREEN

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make eunit t=ai_agent_knowledge_policy_tests
make eunit t=ai_agent_kb_logic_tests
make eunit t=ai_agent_prompt_tests
```

Expected: 先验证旧的“无条件注入”测试被新策略明确改变，再更新测试为角色策略契约并全部通过。

### Step 6: Commit

```bash
git add src/logic/ai_agent_kb_logic.erl src/logic/ai_agent_prompt.erl test/logic/ai_agent_kb_logic_tests.erl test/logic/ai_agent_prompt_tests.erl test/logic/ai_agent_knowledge_policy_tests.erl
git commit -m "feat: add cost-aware on-demand agent knowledge retrieval"
```

---

## 5. 增加管理后台角色 API

**目标:** 将现有全量 KV 角色接口升级为分页、草稿、发布、影响范围可见的管理 API，同时保留旧接口兼容期。

**Files:**

- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/adm/adm_ai_agent_handler.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/src/imboy_router.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboy/test/adm/adm_ai_agent_handler_tests.erl`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/api/public.ts`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/api/public.test.ts`

### Step 1: 写 Handler/API RED 测试

后端测试覆盖：

- GET 角色分页返回 `items/page/size/total`。
- keyword/status 过滤参数生效。
- 新建角色生成 draft version。
- 更新草稿不会影响 active version。
- 发布前返回 bound agent count。
- 发布后 active version 更新。
- 无权限读取返回拒绝；无权限写入返回拒绝。
- 删除/停用被有效 Agent 引用的角色返回明确错误。

前端 API 测试覆盖：

- `getAiRolePage` 参数正确。
- `getAiRoleDetail` 正确解析 draft、published、bound count。
- `saveAiRoleDraft` 发送规范化 JSON。
- `publishAiRole` 发送版本和确认参数。
- legacy `/ai_agent/roles` 兼容读取仍能解析。

### Step 2: 增加路由和权限

建议新增：

```text
GET  /api/adm/ai_agent/role/list
GET  /api/adm/ai_agent/role/detail?code=...
POST /api/adm/ai_agent/role/create
POST /api/adm/ai_agent/role/draft
POST /api/adm/ai_agent/role/publish
POST /api/adm/ai_agent/role/set_status
```

读操作使用 `users:read`，写操作使用 `users:update`；前端路由只负责页面访问，后端 Handler 必须再次校验权限。

### Step 3: 实现最小 Handler

Handler 只做：method、参数解析、权限门控、统一响应；校验和业务编排放到 DS/Logic，禁止 Handler 直接操作 Repo。

### Step 4: 运行 RED/GREEN

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make eunit t=adm_ai_agent_handler_tests

cd /Users/leeyi/project/imboy.pub/imboyadmin
bun test src/modules/ai_agent/api/public.test.ts
```

### Step 5: Commit

```bash
cd /Users/leeyi/project/imboy.pub/imboy
git add src/adm/adm_ai_agent_handler.erl src/imboy_router.erl test/adm/adm_ai_agent_handler_tests.erl
git commit -m "feat: expose versioned ai agent role admin api"

cd /Users/leeyi/project/imboy.pub/imboyadmin
git add src/modules/ai_agent/api/public.ts src/modules/ai_agent/api/public.test.ts
git commit -m "feat: add ai agent role admin api client"
```

---

## 6. 改造 AI 助手分页管理页

**目标:** 管理大量 AI 助手时，以角色为行为来源；助手编辑页只能绑定角色并查看有效能力。

**Files:**

- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/pages/AiAgentListPage.tsx`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/pages/AiAgentListPage.test.tsx`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/api/public.ts`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/App.tsx`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/components/layout/sidebarSchema.ts`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/tests/e2e/ai-agent-manage.spec.ts`

### Step 1: 写页面 RED 测试

新增测试：

- 列表显示当前角色 code/name。
- 编辑助手时加载角色列表并回显当前角色。
- 选择角色后显示继承能力摘要。
- 助手保存请求只发送 `role_id`，不发送自定义 capabilities。
- 页面不再提供原始 `system_prompt` 和逗号分隔 capabilities 输入框。
- 角色停用后不能被新助手绑定。
- 分页、分类、关键词变化时 page 重置为 1。
- 接口返回角色缺失时显示兼容回退提示。

### Step 2: 实现角色绑定

`AgentForm` 保留身份和模型字段，增加：

```ts
role_id: string
role_name?: string
effective_capabilities?: AiAgentCapabilities
```

编辑助手时调用详情接口和角色摘要接口；角色摘要只读显示，例如：

```text
知识库：按需检索
群聊：仅 @ 回复
主动消息：关闭
```

### Step 3: 清理错误的自由输入

删除/替换当前的：

- `capabilitiesCsv`
- 助手级 `system_prompt` 编辑
- 任意 capability key 输入

保留后端 legacy 字段读取，但不再从新页面发送它们。

### Step 4: 调整权限和菜单文案

将菜单名称从“AI 角色管理”调整为“AI 角色模板”；AI 助手管理保持主入口。角色写操作的前端可见性与 `users:update` 对齐，后端权限继续作为最终安全边界。

### Step 5: 运行 RED/GREEN

```bash
cd /Users/leeyi/project/imboy.pub/imboyadmin
bun test src/modules/ai_agent/pages/AiAgentListPage.test.tsx
```

Expected: 新增测试先因页面仍显示自由输入而失败；改造后通过，现有 AI 页面测试不回归。

### Step 6: Commit

```bash
git add src/modules/ai_agent/pages/AiAgentListPage.tsx src/modules/ai_agent/pages/AiAgentListPage.test.tsx src/modules/ai_agent/api/public.ts src/App.tsx src/components/layout/sidebarSchema.ts tests/e2e/ai-agent-manage.spec.ts
git commit -m "feat: bind ai assistants to role templates"
```

---

## 7. 改造角色模板分页、草稿和发布页面

**目标:** 将现有全量 `AiRolesPage` 改造成可管理大量角色的角色模板页面。

**Files:**

- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/pages/AiRolesPage.tsx`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/pages/AiRolesPage.test.tsx`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/tests/e2e/ai-roles.spec.ts`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/src/modules/ai_agent/public.ts`

### Step 1: 写页面 RED 测试

覆盖：

- 角色分页加载、关键词筛选、状态筛选。
- 列表显示绑定助手数量和 active version。
- 新建角色保存 draft，不立即影响运行时。
- 编辑角色显示结构化能力控件，不出现 JSON 输入。
- knowledge 模式切换时显示成本说明。
- 发布前弹窗显示绑定助手数量和影响范围。
- 发布成功后刷新 active version。
- 发布失败时草稿和当前生产版本都不丢失。
- 停用角色后列表状态和绑定规则正确。

### Step 2: 实现页面分区

角色编辑页分为：

1. 基础信息：名称、code、描述、状态。
2. 行为提示词：system prompt、输出风格说明。
3. 能力与成本：知识库模式、最大上下文、群聊触发、主动消息策略。
4. 发布检查：绑定助手数量、有效策略摘要、发布按钮。

### Step 3: 实现安全默认值

新角色默认：

- knowledge `off` 或 `on_demand`，不得默认 `required`。
- group reply `off`。
- proactive `off`。
- 最大知识库上下文有明确上限。

### Step 4: 运行 RED/GREEN

```bash
cd /Users/leeyi/project/imboy.pub/imboyadmin
bun test src/modules/ai_agent/pages/AiRolesPage.test.tsx
```

### Step 5: Commit

```bash
git add src/modules/ai_agent/pages/AiRolesPage.tsx src/modules/ai_agent/pages/AiRolesPage.test.tsx src/modules/ai_agent/public.ts tests/e2e/ai-roles.spec.ts
git commit -m "feat: add paginated ai role template editor"
```

---

## 8. Playwright 关键用户流程

**目标:** 用浏览器自动化验证真实页面路由、权限门控、分页、角色继承和发布影响提示。

**Files:**

- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/tests/e2e/ai-agent-manage.spec.ts`
- Modify: `/Users/leeyi/project/imboy.pub/imboyadmin/tests/e2e/ai-roles.spec.ts`
- Create: `/Users/leeyi/project/imboy.pub/imboyadmin/tests/e2e/ai-agent-role-inheritance.spec.ts`

### E2E 场景

1. 管理员登录后打开助手分页页，分页请求包含 `page/size`。
2. 创建角色草稿，列表显示 draft 状态。
3. 编辑知识库模式为 `on_demand`，页面显示成本说明。
4. 发布角色，确认弹窗显示绑定助手数量。
5. 打开助手编辑页，角色选择器回显已发布角色。
6. 保存助手只发送 `role_id`，不发送自由 capabilities。
7. 角色发布后助手列表的角色版本摘要刷新。
8. 非授权管理员看不到写入按钮，直接调用写接口被后端拒绝。
9. 角色停用后，创建/编辑助手不能选择它。

### 运行命令

```bash
cd /Users/leeyi/project/imboy.pub/imboyadmin
bun run test:e2e -- --project=chromium ai-agent-role-inheritance
bun run test:e2e -- --project=chromium ai-agent-manage ai-roles
```

业务 API 可以沿用现有 `page.route()` mock；登录继续使用现有 `loginAsAdmin`，避免 E2E 依赖生产 AI provider。

---

## 9. 全量验证、覆盖率和安全门禁

### 前端验证

```bash
cd /Users/leeyi/project/imboy.pub/imboyadmin
bun test --coverage
bun run typecheck
bun run lint
bun run build
```

验收要求：

- AI 模块新增/修改代码行、函数、分支覆盖率目标 80%+。
- 不存在 skipped、todo 或临时 disabled 测试。
- TypeScript、lint、生产构建全部通过。

### 后端验证

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make eunit
make eunit-local t=ai_agent_role_ds_tests
make eunit-local t=ai_agent_policy_tests
make eunit-local t=ai_agent_kb_logic_tests
make security-gate
make format-check
```

本地数据库必须已经应用到最新 migration；若 `eunit-local` 因数据库版本或环境失败，必须记录失败原因，不能把失败误判为业务通过。

### 关键回归要求

- 普通 C2C Agent 回复不回归。
- 群聊 @Agent 门控不回归。
- E2EE 不触发 AI。
- onboarding 欢迎流程仍受总开关和限流控制。
- legacy `ai_roles` 在角色表无数据时仍可用。
- Agent 列表和客户端发现列表字段不混淆。
- 角色发布不会改动 Agent 账号身份、可见性和模型 provider。

---

## 10. 发布与回滚策略

### 发布顺序

1. 先发布数据库 migration，表为空时不改变旧 Agent 行为。
2. 发布后端兼容读取：新角色优先、legacy fallback。
3. 发布后端能力门控和按需知识库策略。
4. 发布管理后台 API client 和角色页。
5. 最后切换助手编辑页为角色绑定模式。

### 回滚

- 前端回滚不影响数据库。
- 后端回滚前确保旧字段仍完整存在。
- migration down 只允许在新表没有有效绑定和版本数据时执行。
- 已发布角色版本不能物理删除，只能 archived，保证 Agent 回滚可追溯。

### 可观测性

至少记录以下无用户原文的事件：

- role draft created/updated
- role published
- role publish failed
- role bound agent count
- knowledge attempted/hit/skipped
- capability denied reason
- effective policy source: role/legacy fallback

---

## 11. 完成定义

本任务只有同时满足以下条件才算完成：

1. 后端有独立角色模板表和版本表，角色可分页查询。
2. Agent 通过 `role_id` 继承已发布 Prompt、能力和知识库策略。
3. Agent 不再从后台提交自由 capabilities 或 system prompt。
4. 群聊、主动消息和知识库策略真实参与运行时门控。
5. 知识库默认不全量注入，每次请求有明确成本策略和上下文上限。
6. 角色支持草稿、测试检查、发布、停用和绑定数量提示。
7. 后端 EUnit、前端单测、Playwright E2E、typecheck、lint、build 和安全门禁通过。
8. 关键新增/修改代码测试覆盖率达到 80%+。
9. 既有未提交 AI 改动没有被覆盖或混入无关提交。

执行时遵循 `@tdd-workflow`：每个业务阶段先写并运行 RED 测试，再实现 GREEN，最后才重构和提交；每个阶段结束必须保留可验证的 checkpoint commit。
