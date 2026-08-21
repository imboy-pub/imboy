# IMBoy Discovery & Agent/Bot 架构实施计划

> **Goal:** 落地 Group 公开搜索、Channel 发现页、Agent/Bot 架构分离（Agent=平台 AI，Bot=开发者服务）
> **Architecture:** 4 层架构（Handler→Logic→DS→Repo），Agent/Bot 共享消息通道和身份体系，各自有独立的表、API、代码路径。
> **Tech Stack:** Erlang/OTP 28+, Cowboy 2.10, PostgreSQL 18+ (pg_jieba), EUnit + meck

---

## 修订记录

- **v2（2026-08-21）**：依据代码库审查修订——
  - C1: 开发者 Bot 改用 `account_type=3`（`2=system_bot` 已被频道 incoming webhook bot 占用，见 `channel_webhook_ds.erl:18` / 迁移 00000027）
  - C2: 迁移执行改走 `imboy_migrate` 迁移系统，禁止手动 `psql -f`（绕过 `schema_migrations` 版本记录会导致下次启动重跑崩溃或 out_of_order）
  - C3: Phase 4 前置新增「Agent C2C 消息入口」任务（`bot_*` 前缀是私聊触发 LLM 的唯一入口，废弃前必须先建替代通路并迁移限流闸门）
  - H1: 明确 Bot 消息推送仅覆盖 C2C，群内 @Bot（C2G）不在本期
  - H2: Webhook 推送改为 `elib_async` 异步 + 超时 + 失败策略
  - H3: 新增「产品边界与范围约束」章节（trending 定位、Bot 注册表 vs GitOps 插件市场）
  - M1-M6: 修正 app.src 无效指引、既有现场描述、路由提交状态、OAuth 重授权约束、Webhook 头精简
- **v1（2026-08-21）**：初稿

---

## 产品边界与范围约束

1. **范围**：本计划纯后端。客户端发现页 UI（imboyapp）与 admin 管理界面不在本期，须另立计划。
2. **发现页定位（已拍板 2026-08-21：保留 hot/trending）**：`search`（全文检索）+ `categories`（分类目录）+ `featured`（运营人工精选标记）+ `hot`/`trending`（**全局统计榜单**，基于 group/channel 公开计数排序，所有用户同一份榜）。全部为非个性化、无用户画像、无定向分发的目录能力，不触碰「不做基于算法的内容发现」红线。
3. **Bot 注册表 vs GitOps 插件市场**：`bot` 表是**运行时注册元数据**（实例内 Bot 身份、凭证、OAuth 授权、订阅事件）；`imboy-plugin-marketplace` 是**分发渠道**（发现、安装包元数据）。两者互补不替代：`bot.is_public` + search 仅做本实例注册表检索，不承担跨实例分发职责。
4. **账号类型语义（权威）**：`0=human`，`1=agent`（平台 AI，绑定 `ai_agent` 表），`2=system_bot`（频道 incoming webhook bot，`channel_webhook_ds` 创建，**非开发者 Bot**），`3=bot`（开发者服务 Bot，本计划 Phase 2 引入）。

---

## 既有现场保护

开始任何实现前必须记录：

```bash
cd /Users/leeyi/project/imboy.pub/imboy
git status --short --branch
```

当前实际状态（v2 核实，2026-08-21）：

- `main...origin/main [ahead 51]`（本地多 51 个未推送提交）
- 已修改未提交：`CLAUDE.md`
- 未跟踪：Phase 1 的 4 个迁移文件 + 6 个源文件（见下表）
- **11 条路由已随本地提交入库**（`imboy_router.erl` 工作树干净，路由位于 275-280、353-357 行），不在未提交清单中

实施时只允许提交本计划新增或明确修改的文件，提交必须带 pathspec。

**提交策略（已拍板 2026-08-21）**：Phase 1 的 10 个 untracked 文件（6 源文件 + 4 迁移）在 Task 1.1 编译 + Task 1.2 迁移 + Task 1.3/1.4 测试全部通过后，作为一个 commit 提交（带 pathspec），不提交未验证代码。

---

## Phase 1: 验证已完成的 Discovery 代码（1 天）

已创建的文件：

| 文件 | 层 | 说明 | 状态 |
|------|-----|------|------|
| `priv/migrations/00000068_fts_group.up.sql` | 迁移 | fts_group 表 + 触发器 + group_category 分类表 + is_featured 字段 + 存量回填 | 未跟踪 |
| `priv/migrations/00000068_fts_group.down.sql` | 迁移 | 回滚 | 未跟踪 |
| `priv/migrations/00000069_channel_discovery.up.sql` | 迁移 | channel_category 表 + fts_channel 表 + is_featured 字段 + 存量回填 | 未跟踪 |
| `priv/migrations/00000069_channel_discovery.down.sql` | 迁移 | 回滚 | 未跟踪 |
| `src/repo/fts_group_repo.erl` | Repo | 全文搜索 + 发现页查询（支持分类筛选） | 未跟踪 |
| `src/ds/fts_group_ds.erl` | DS | 搜索/发现/精选/热门数据服务 | 未跟踪 |
| `src/logic/group_discovery_logic.erl` | Logic | 搜索/发现/精选/热门/分类/预览业务逻辑 | 未跟踪 |
| `src/api/group_discovery_handler.erl` | Handler | 6 个 REST 端点 | 未跟踪 |
| `src/logic/channel_discovery_logic.erl` | Logic | 搜索/发现/精选/热门/分类 + 热门算法 | 未跟踪 |
| `src/api/channel_discovery_handler.erl` | Handler | 5 个 REST 端点 | 未跟踪 |
| `src/imboy_router.erl` | 路由 | 11 条路由（Group 6 条 + Channel 5 条，替换旧 search/discover） | **已提交** |

范式依据：`fts_user`（迁移 00000001，jiebacfg + setweight A/B + 触发器）是既有 FTS 范式，fts_group/fts_channel 与其一致；`channel_stats_daily` 已存在（迁移 00000003），trending 直接依赖。

新增 API 端点（11 个）：

```
# Group 发现
GET /api/v1/group/search?q=xxx&category_id=1&page=1&size=20
GET /api/v1/group/discover?category_id=1&sort=popular&page=1&size=20
GET /api/v1/group/featured?limit=10
GET /api/v1/group/hot?limit=20
GET /api/v1/group/categories
GET /api/v1/group/preview?group_id=xxx

# Channel 发现
GET /api/v1/channels/search?q=xxx&category_id=1&page=1&size=20
GET /api/v1/channels/discover?category_id=1&sort=popular&page=1&size=20
GET /api/v1/channels/featured?limit=10
GET /api/v1/channels/trending?period=7d&limit=20
GET /api/v1/channels/categories
```

### Task 1.1: 编译验证

**Step 1: 编译**

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make compile 2>&1 | grep -i "error\|warning" | head -30
```

Expected: 无 error。

**Step 2: 修复编译错误**

常见问题：
- `elib_param:int_val/3` 不存在 → 已在代码中使用 `elib_param:int/3` 和 `safe_int_qs/2`
- `group_ds:detail/1` 不存在 → 已修复为 `group_ds:find_by_id/2`

模块注册无需手工干预：`src/imboy.app.src` 不存在，erlang.mk 自动从 `src/**/*.erl` 生成 modules 列表。

### Task 1.2: 迁移执行（走迁移系统，禁止手动 psql）

⚠️ **禁止 `psql -f priv/migrations/*.sql` 手动执行**：本项目用 `imboy_migrate`（PG 实现 + advisory lock，版本表 `schema_migrations`）。手动执行绕过版本记录，下次节点启动时迁移系统会重跑 68/69——`CREATE TABLE public.fts_group` 无 `IF NOT EXISTS`，重跑直接崩溃、app 启动失败；版本乱序还会触发 `{out_of_order, ...}` 使启动失败（Makefile:211 注释）。本地库是 docker `imboy_pg18`（端口 4323），裸 `psql -U postgres -d imboy` 也连不上。

```bash
cd /Users/leeyi/project/imboy.pub/imboy
IMBOYENV=local make run    # 启动时 imboy_migrate 自动执行未记录的 68/69
```

验证（版本记录 + 表结构）：

```bash
docker exec imboy_pg18 psql -U postgres -d imboy -c "SELECT * FROM schema_migrations ORDER BY 1 DESC LIMIT 5;"
docker exec imboy_pg18 psql -U postgres -d imboy -c "\d fts_group"
docker exec imboy_pg18 psql -U postgres -d imboy -c "\d group_category"
docker exec imboy_pg18 psql -U postgres -d imboy -c "\d channel_category"
docker exec imboy_pg18 psql -U postgres -d imboy -c "\d fts_channel"
```

Expected: 版本表含 68/69 记录；四张表存在；`fts_group`/`fts_channel` 存量回填行数 ≥ 公开群/频道数。

### Task 1.3: 编写 Group Discovery 测试

**Files:**
- Create: `test/repo/fts_group_repo_tests.erl`
- Create: `test/logic/group_discovery_logic_tests.erl`
- Create: `test/api/group_discovery_handler_tests.erl`

注意：Repo 层测试连 DB，需按项目惯例注入测试配置（EUNIT_ERL_OPTS 携带 `-config` 与 `-pa`，参见既有 `test/repo/` 集成测试写法）。

### Task 1.4: 编写 Channel Discovery 测试

**Files:**
- Create: `test/logic/channel_discovery_logic_tests.erl`
- Create: `test/api/channel_discovery_handler_tests.erl`

---

## Phase 2: Bot 表与基础设施（2 天）

### Task 2.1: Bot 表迁移

**Files:**
- Create: `priv/migrations/00000070_bot.up.sql`
- Create: `priv/migrations/00000070_bot.down.sql`

```sql
-- 00000070_bot.up.sql
-- Bot 基表 + Bot OAuth 授权表（开发者服务，account_type=3）

BEGIN;

-- 1. Bot 表（开发者服务）
CREATE TABLE public.bot (
    user_id        BIGINT PRIMARY KEY,              -- 关联 user.id
    name           VARCHAR(80) NOT NULL,             -- Bot 名称
    username       VARCHAR(80) UNIQUE,               -- @调用名（唯一）
    description    VARCHAR(500) DEFAULT '',          -- 简介
    avatar         VARCHAR(320) DEFAULT '',          -- 头像
    owner_uid      BIGINT NOT NULL,                  -- 开发者/所有者
    webhook_url    TEXT DEFAULT '',                  -- 消息推送地址
    api_token      VARCHAR(128) UNIQUE,              -- API 认证 token
    verify_token   VARCHAR(128) DEFAULT '',          -- webhook 验签 token
    commands       JSONB NOT NULL DEFAULT '[]',       -- 注册的命令
    permissions    JSONB NOT NULL DEFAULT '[]',       -- 权限列表
    events         JSONB NOT NULL DEFAULT '[]',       -- 订阅的事件类型
    is_public      BOOLEAN DEFAULT false,             -- 是否公开（本实例注册表检索）
    status         SMALLINT DEFAULT 1,               -- -1=deleted, 0=disabled, 1=active
    created_at     TIMESTAMPTZ DEFAULT now(),
    updated_at     TIMESTAMPTZ DEFAULT now(),
    CONSTRAINT fk_bot_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE
);

CREATE INDEX idx_bot_owner ON public.bot(owner_uid);
CREATE INDEX idx_bot_username ON public.bot(username);
CREATE INDEX idx_bot_is_public ON public.bot(is_public) WHERE is_public = true;
CREATE INDEX idx_bot_status ON public.bot(status);

COMMENT ON TABLE public.bot IS '开发者 Bot（Webhook 驱动的第三方服务，account_type=3）';
COMMENT ON COLUMN public.bot.user_id IS '关联 user.id，Bot 作为一等 user 账号存在（account_type=3）';
COMMENT ON COLUMN public.bot.username IS 'Bot 唯一调用名，用于 @botname 提及';
COMMENT ON COLUMN public.bot.api_token IS 'Bot 调用 IMBoy API 的凭证';
COMMENT ON COLUMN public.bot.verify_token IS 'Webhook 推送时的验签 token（与 api_token 职责分离）';

-- 2. Bot OAuth 授权表
-- 注意 UNIQUE(bot_id, user_id)：重新授权必须复用同一行（UPDATE access_token/scopes/
-- expires_at 并清空 revoked_at），不得 INSERT 新行，否则违反唯一约束。
CREATE TABLE public.bot_oauth_grant (
    id           BIGINT PRIMARY KEY,
    bot_id       BIGINT NOT NULL,                    -- Bot 的 user_id
    user_id      BIGINT NOT NULL,                    -- 授权用户
    scopes       JSONB NOT NULL DEFAULT '[]',         -- 授权范围
    access_token VARCHAR(128) UNIQUE,                 -- 访问令牌
    expires_at   TIMESTAMPTZ,                         -- 过期时间
    revoked_at   TIMESTAMPTZ,                         -- 撤销时间
    status       SMALLINT DEFAULT 1,                 -- 0=revoked, 1=active
    created_at   TIMESTAMPTZ DEFAULT now(),
    updated_at   TIMESTAMPTZ DEFAULT now(),
    UNIQUE(bot_id, user_id),
    CONSTRAINT fk_bot_grant_bot FOREIGN KEY (bot_id) REFERENCES public.bot(user_id) ON DELETE CASCADE,
    CONSTRAINT fk_bot_grant_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE
);

CREATE INDEX idx_bot_oauth_grant_bot ON public.bot_oauth_grant(bot_id);
CREATE INDEX idx_bot_oauth_grant_user ON public.bot_oauth_grant(user_id);
CREATE INDEX idx_bot_oauth_grant_token ON public.bot_oauth_grant(access_token);

COMMENT ON TABLE public.bot_oauth_grant IS 'Bot OAuth 授权：用户授权 Bot 代表自己操作';

-- 3. account_type 注释更新（扩展枚举，2 的既有语义不变）
COMMENT ON COLUMN public."user".account_type IS '账号类型 0=human 1=agent(平台AI) 2=system_bot(频道webhook bot) 3=bot(开发者服务)';

COMMIT;
```

```sql
-- 00000070_bot.down.sql
BEGIN;
DROP TABLE IF EXISTS public.bot_oauth_grant;
DROP TABLE IF EXISTS public.bot;
COMMENT ON COLUMN public."user".account_type IS '账号类型 0=human 1=ai_agent 2=system_bot';
COMMIT;
```

### Task 2.2: Bot Repo 层

**Files:**
- Create: `src/repo/bot_repo.erl`

核心函数：`tablename/0`, `create/1`, `find/1`, `find_by_username/1`, `find_by_token/1`, `update/2`, `set_status/2`, `page/2`, `page_by_owner/3`, `search/3`

### Task 2.3: Bot DS 层

**Files:**
- Create: `src/ds/bot_ds.erl`

核心逻辑：
- `create/1` — 建 user 行 + 标 **account_type=3** + 绑 bot 元数据。**镜像既有范式 `channel_webhook_ds:create_bot_user/0`**（其注释明说镜像 `ai_agent_ds:create_agent_user`；注意它建的是 account_type=2 的频道 webhook bot，勿混淆枚举值）
- `is_bot/1` — 检查 **account_type=3**（不是 2；2 是频道 webhook bot）
- `find_by_token/1` — 通过 api_token 查找 Bot（Bot 调用 API 时认证）

### Task 2.4: Bot Logic 层

**Files:**
- Create: `src/logic/bot_logic.erl`

核心函数：`register/1`, `get/1`, `update/2`, `set_status/2`, `list_mine/2`, `search/3`, `send_message/3`

### Task 2.5: Bot Handler 层 + 路由

**Files:**
- Create: `src/api/bot_handler.erl`
- Modify: `src/imboy_router.erl`

新增 API 端点（7 个）：

```
POST /api/v1/bot/register
GET  /api/v1/bot/get?bot_id=xxx
POST /api/v1/bot/update
POST /api/v1/bot/disable
POST /api/v1/bot/enable
GET  /api/v1/bot/list_mine?page=1
GET  /api/v1/bot/search?q=xxx&page=1&size=20
```

### Task 2.6: Bot 测试

**Files:**
- Create: `test/repo/bot_repo_tests.erl`
- Create: `test/logic/bot_logic_tests.erl`

---

## Phase 3: Bot Webhook 推送（1 天）

> **范围边界**：本期仅覆盖 **C2C 私聊**（用户 ↔ Bot 一对一）。群内 @Bot（C2G 触发）不在本期，须另立计划。

### Task 3.1: Bot Webhook 推送模块

**Files:**
- Create: `src/logic/bot_webhook_logic.erl`

核心功能：
- `push/2` — 推送消息到 Bot 的 webhook URL。**必须经 `elib_async` 异步执行**，HTTP 超时 5s；禁止在消息主路径内同步调用外部 URL（外部服务慢/挂会拖垮 C2S 处理）
- `push_message/3` — 封装消息格式后推送
- `sign_payload/2` — HMAC-SHA256 签名（密钥用 `verify_token`；`api_token` 是 Bot 调 IMBoy API 的凭证，不用于推送验签）

失败策略（本期）：推送失败记 error 日志 + 失败计数，**不重投**（离线 Bot 消息不暂存）。如后续需要重投，再引入队列化方案。

Webhook 推送格式（只发签名，不发 token——接收方即 verify_token 持有者，Slack 同款范式）：

```
POST {webhook_url}
Content-Type: application/json
X-IMBoy-Signature: sha256=xxx

{
    "event": "message",
    "from": {"user_id": "xxx", "nickname": "张三"},
    "chat": {"type": "c2c", "chat_id": "xxx"},
    "message": {"msg_id": "xxx", "msg_type": "text", "text": "..."}
}
```

### Task 3.2: 消息路由集成

**Files:**
- Modify: `src/logic/msg_c2s_logic.erl`

在 `c2s/3` 中，`bot_*` 分支之前，添加对 Bot 的 Webhook 推送。

判定条件必须**双条件**：收件人 `account_type=3` **且** `bot` 表存在该 user_id 行（有 webhook_url 才推）。单看 account_type 会误伤——频道 incoming webhook bot（account_type=2）与开发者 Bot 是两个群体，前者不推 webhook。

---

## Phase 4: Agent C2C 入口 + 废弃 bot_* 前缀（2 天）

> ⚠️ **执行顺序强约束**：Task 4.1（Agent C2C 消息入口）必须先完成并验收，才允许执行 Task 4.3/4.4（废弃 bot_*）。`bot_*` 前缀是当前私聊触发 LLM 的**唯一入口**（`msg_c2s_logic.erl` C2S 分派仅有 sync / bot_* / c2s_unsupported 三分支；Agent 用户账号在消息路径中无任何触发逻辑），先废弃等于私聊 AI 功能直接断链。

### Task 4.1: Agent C2C 消息入口（先行，阻断项）

**Files:**
- Modify: `src/logic/msg_c2s_logic.erl`（或 C2C 消息落库路径的对应模块）
- Modify: `src/logic/ai_agent_logic.erl`

实现目标：用户向 `account_type=1` 的 Agent 用户账号发送 C2C 消息 → 落库后触发 `ai_agent` 分派（按 `ai_agent.provider` 调 LLM）→ 应答经既有消息通道返回。

- **参照实现**：`src/logic/ai_agent_group_reply.erl`（群内 Agent 触发是现成范式，C2C 入口镜像其分派与应答方式；实现前先通读该模块）
- **闸门迁移**：现有 `bot_*` 通路带金钱 DoS 限流闸门（`msg_c2s_logic.erl` c2s_to_llm 内，scope=归一化 provider 名）。Agent C2C 入口必须接入同一限流器，不得裸奔
- **验收**：真机/测试客户端给 Agent 账号发消息能收到 LLM 应答；限流器对 Agent 入口生效（超限静默丢弃行为与 bot_* 一致）

### Task 4.2: 为现有 LLM Provider 创建默认 Agent

**Files:**
- Create: `priv/migrations/00000071_bot_prefix_to_agent.up.sql`
- Create: `priv/migrations/00000071_bot_prefix_to_agent.down.sql`

为每个已注册的 LLM provider 创建一个默认 Agent（幂等迁移）。`ai_agent.provider` 字段已存在（迁移 00000027），数据模型成立。

### Task 4.3: 废弃 bot_* 前缀（依赖 Task 4.1 验收通过）

**Files:**
- Modify: `src/logic/msg_c2s_logic.erl`

废弃 `<<"bot_", _/binary>>` 分支，改为返回 `bot_prefix_deprecated` 提示，引导用户改用 Agent（直接与 Agent 账号对话）。

### Task 4.4: 废弃 bot_role_chat

**Files:**
- Modify: `src/logic/msg_c2s_logic.erl`

移除 `c2s_to_role_chat/3` 函数和相关调用，改为通过 Agent 系统处理。同样依赖 Task 4.1 验收通过。

---

## Phase 5: Agent 公开发现（1 天）

### Task 5.1: Agent 公开发现 API

**Files:**
- Modify: `src/api/ai_agent_handler.erl`
- Modify: `src/imboy_router.erl`

新增 API 端点（3 个）：

```
GET /api/v1/agent/discover?page=1&size=20
GET /api/v1/agent/search?q=xxx&page=1&size=20
GET /api/v1/agent/categories
```

---

## Phase 6: 文档（1 天）

> 新文档遵循仓库双语文档规则（中英对照）。

### Task 6.1: 更新 CLAUDE.md

**Files:**
- Modify: `imboy/CLAUDE.md`

确认产品定位章节包含 Agent/Bot 分离的说明（含 account_type 四值语义）。

### Task 6.2: 写 Agent 用户指南

**Files:**
- Create: `docs/guides/agent-user-guide.md`

内容大纲：
- 什么是 Agent
- 如何发现 Agent
- 如何把 Agent 添加到群/私聊
- 如何配置 Agent 的触发策略
- Agent 的权限和安全

### Task 6.3: 写 Bot 开发者指南

**Files:**
- Create: `docs/guides/bot-developer-guide.md`

内容大纲：
- 什么是 Bot
- 如何注册 Bot
- Webhook 消息推送格式（含签名校验方法）
- Bot API 参考
- Bot 权限和 OAuth
- Bot 注册表检索与 GitOps 插件市场分发的关系

---

## Phase 7: 集成测试（1 天）

### Task 7.1: 端到端测试

**Files:**
- Create: `test/api/group_discovery_e2e_tests.erl`
- Create: `test/api/channel_discovery_e2e_tests.erl`
- Create: `test/api/bot_e2e_tests.erl`

### Task 7.2: 全量回归

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make eunit 2>&1 | tail -20
```

Expected: 所有测试通过。

---

## 验收清单

### Group Discovery
- [ ] `GET /api/v1/group/search?q=xxx` 返回匹配的公开群
- [ ] `GET /api/v1/group/discover` 返回公开群列表
- [ ] `GET /api/v1/group/featured` 返回精选群
- [ ] `GET /api/v1/group/hot` 返回热门群
- [ ] `GET /api/v1/group/categories` 返回分类列表
- [ ] `GET /api/v1/group/preview?group_id=xxx` 返回群预览
- [ ] 全文搜索支持中文（pg_jieba）
- [ ] 搜索结果按相关性排序
- [ ] 只返回 status=1 且 type=1 的公开群

### Channel Discovery
- [ ] `GET /api/v1/channels/search?q=xxx` 返回匹配的频道
- [ ] `GET /api/v1/channels/discover` 返回频道列表
- [ ] `GET /api/v1/channels/featured` 返回精选频道
- [ ] `GET /api/v1/channels/trending` 返回热门频道
- [ ] `GET /api/v1/channels/categories` 返回分类列表
- [ ] 热门频道基于 channel_stats_daily 统计排序
- [ ] 全文搜索支持中文

### Bot 基础设施
- [ ] `bot` 表创建成功（迁移系统执行，`schema_migrations` 有记录）
- [ ] `bot_oauth_grant` 表创建成功
- [ ] `POST /api/v1/bot/register` 注册 Bot（user 行 account_type=3）
- [ ] `GET /api/v1/bot/search?q=xxx` 搜索 Bot
- [ ] Bot Webhook 异步推送正常（主路径无阻塞）
- [ ] Bot 消息到达时触发 Webhook（C2C 场景）
- [ ] **频道 webhook bot（account_type=2）不触发开发者 Bot 推送**（双条件判定回归）

### Agent/Bot 分离
- [ ] **发给 Agent 账号（account_type=1）的 C2C 消息获得 LLM 应答（Task 4.1 验收）**
- [ ] **Agent C2C 入口限流闸门生效**
- [ ] `bot_*` 前缀返回 deprecated 提示（Task 4.1 验收通过后才允许上线）
- [ ] 每个 LLM provider 有对应的 Agent
- [ ] Agent 可以被公开发现
- [ ] Bot 和 Agent 的数据模型完全分离
- [ ] `account_type` 语义清晰：0=human, 1=agent, 2=system_bot, 3=bot，两类 bot 判定互不误伤

---

## 时间线估算

| Phase | 内容 | 估时 |
|-------|------|------|
| 1 | 验证 Discovery 代码 + 迁移 + 测试 | 1 天 |
| 2 | Bot 表 + Repo + DS + Logic + Handler | 2 天 |
| 3 | Bot Webhook 推送（C2C）+ 消息路由集成 | 1 天 |
| 4 | Agent C2C 入口 + 废弃 bot_* + 并入 Agent | 2 天 |
| 5 | Agent 公开发现 API | 1 天 |
| 6 | 文档 | 1 天 |
| 7 | 集成测试 + 回归 | 1 天 |
| **合计** | | **9 天** |
