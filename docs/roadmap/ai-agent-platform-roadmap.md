# imboy AI Agent 载体落地路线图 / AI Agent Platform Roadmap

> 目标：把 imboy 从"IM 平台"演进为"AI Agent 的私有化载体"。
> 定位来源：深度研究「方向二：IM 作为 AI Agent 载体（最大机会）」。
> ⚠️ 战略校正（三层协议都已名花有主，别再找"协议空白"——imboy 的护城河是产品形态）：
> - **MCP**（agent↔tool）已被 Anthropic/AAIF 占，公有云更被 Slack 占 → 空白只在**私有化 / E2EE MCP**。
> - **A2A**（agent↔agent 协作）已被 Google/Linux Foundation 占（v1.0）→ 空白只在**「IM 作为其协作载体」**。
> - **AGUI**（agent↔user 交互，CopilotKit）已占**单人**「agent↔前端」交互协议（流式/工具调用/状态/HITL 审批）→ 空白只在**「多人群协作的 IM 载体」**（AGUI 不做多人共同观察/介入）。
> - **结论**：三层协议都有主了，不存在"协议空白"。imboy 的定位是**「在已有协议之上做唯一的产品形态」——私有化 + 多人 + 合规的 IM**，把 MCP/A2A/AGUI 缝成一个「agent 协作的人类载体」。护城河本就是产品形态（私有化 IM）而非协议发明——这个论证更诚实、更难被推翻。
> **版本**：1.3.0 | **创建**：2026-07-09 | **最后更新**：2026-07-10 | **状态**：Phase 0/1/2/3 ✅ 已交付（Phase 0 `0f230ee1`+`ae3baa72`、Phase 1 `00b56a56`+`77138253`、Phase 2 `09974f8f`+`8b8d5b56`+`965c51f8`、Phase 3 T3.0–T3.6，**全部未 push**）；Phase 4 未开工（前瞻大阶段，待立项）。⚠️ Phase 2 流式真机 E2E 未验（生产 `.env.pro` 无 Phase 2 代码/provider/agent 账号，需本地环境验证）

---

## 0. 一句话战略 / TL;DR

不去卷"有没有 AI 功能"，而是打两张别人打不了的牌：
1. **把 IM 做成 AI Agent 的私有化载体**——MCP server + 一等 agent 账号 + 插件市场升级。
2. **卖 AI 主权**——数据不出域 + 推理不出域 + 可审计，木兰宽松许可无传染，切受监管行业合规红利。

关键事实：**这两张牌几乎全部复用 imboy 现有地基**，不是从零造。

---

## 1. 现状盘点：哪些电路已通电，哪些待通 / Current Wiring

> 这是全部规划的事实基线。已通过代码核查（2026-07-09）。

### 1.1 已通电（可直接复用）✅

| 能力 | 现状证据 | 复用方式 |
|------|---------|---------|
| **AI/Bot 通道雏形** | `src/logic/msg_c2s_logic.erl:83` `c2s_to_external/5`：完整 stage→ACK→enqueue→异步调 LLM→投递响应链路，状态机 10/11/12/20 | Agent 回复链路的现成骨架 |
| **一个 LLM provider** | `src/lib/qianfan_api.erl` 百度文心千帆 `create_chat/3`，被 `msg_c2s_logic:37` 以 `bot_qian_fan` 分派 | 抽象为 provider 之一 |
| **角色/system_prompt** | `msg_c2s_logic:182` `c2s_to_role_chat/3`，从 `config_ds:env(ai_roles)` 读角色提示词 | Agent 人格的现成机制 |
| **provider 扩展点（已注释预留）** | `msg_c2s_logic:46-51` `bot_openai`/`bot_claude` 注释 | 明示的扩展意图 |
| **长期 token** | refresh token ~1 年（`include/common.hrl:5` `REFRESHTOKEN_VALID=30758400`），普通 user 账号即可长期在线 | Agent 无需特殊认证体系 |
| **WS 长连接收发** | `websocket_handler` 握手 → `user_logic:online/4`（syn 注册）→ `msg_c2c_logic:c2c/3` | Agent 当"永不掉线的群成员" |
| **WS action 动态注册** | `src/lib/imboy_ws_action_registry.erl:76` `register/3`（ETS，O(1) 查表） | 新增 AI action 无需改核心路由 |
| **HTTP 路由动态注册** | `src/lib/imboy_router_registry.erl:67` `register/2`（热更 cowboy dispatch，须 `/v{n}/<plugin>/` 命名空间） | MCP server 端点挂载点 |
| **插件 behaviour** | `src/lib/imboy_plugin.erl` 7 callback（manifest/start/stop/migrate/routes/capabilities/health），进程内 Erlang 模块 | Agent/MCP 作为插件 |
| **REST API 面** | `docs/reference/rest-api-v1-catalog.md` 575 行，40+ 分组端点 | MCP tools 的现成来源 |
| **License 授权层** | `imboy_license`（RSA 验签、锁规模不锁功能、配额 gate） | AI 模块按规模授权 |
| **支付子系统** | `erlang_pay` + `billing_logic` | Agent 支付（AP2）的结合点 |

### 1.2 待通电（缺失需新建）❌

| 缺口 | 现状证据 | 影响的阶段 |
|------|---------|-----------|
| **LLM provider 硬编码** | `msg_c2s_logic:43` 直接 `fun qianfan_api:create_chat/3`，非配置驱动 | Phase 0 |
| **无 Agent 账号身份** | user 表（`priv/migrations/00000001_foundation.up.sql:2459`）无 `account_type`/机器人标识；bot 目前只是特殊 `to` 字符串，不能在群里被 @、不能真正 C2C/C2G | Phase 1 |
| **无流式回复** | `imboy_codec.erl:285` 仅 8 种 content_type（text/image/video/audio/file/location/custom/e2ee），无 stream/draft/typing；`c2s_to_external` 是一次性 result | Phase 2 |
| **未对外暴露 MCP** | 无 MCP server；WS action 注册表就绪但**零插件使用**（grep 仅注释） | Phase 3 |
| **插件 sup 空骨架** | `imboy_plugin_generic_sup:init` Children=[]，无 worker | Phase 1/4 |
| **群防刷屏门控** | 无 agent 触发策略；群权限判定散在 `msg_c2g_logic:46` 内联 | Phase 1 |
| **无会话记忆/RAG** | `c2s_to_role_chat` 的 History 仅角色开场，无多轮持久化上下文注入 | Phase 2（可选） |

---

## 2. 目标架构 / Target Architecture

```
                          ┌─────────────────────────────────────────┐
   外部 AI (Claude/       │              imboy 后端 (Erlang/OTP)      │
   Cursor/ChatGPT)        │                                           │
        │  MCP/SSE         │  ┌──────────────┐   ┌─────────────────┐  │
        └─────────────────┼─▶│ MCP Server    │──▶│ 现有 REST/Logic │  │
                          │  │ (插件, /v1/mcp)│   │ (发消息/搜索/群) │  │
                          │  └──────────────┘   └─────────────────┘  │
                          │         ▲ 审批+scope+审计(imboyadmin)     │
   人类用户 ──C2C/C2G──▶  │  ┌──────────────────────────────────┐    │
        ▲                 │  │ 消息路由 message_router_logic      │    │
        │  流式回复        │  │   ├─ to=agent账号 ─▶ Agent Runtime │    │
        └─────────────────┼──┤   └─ 群@agent ─▶ 防刷屏门控         │    │
                          │  └──────────────┬───────────────────┘    │
                          │                 ▼                         │
                          │  ┌──────────────────────────────────┐    │
                          │  │ imboy_llm 适配层 (behaviour)        │    │
                          │  │  ├─ imboy_llm_qianfan (现有)        │    │
                          │  │  ├─ imboy_llm_openai (OpenAI兼容)   │    │
                          │  │  │   └─▶ DeepSeek/Qwen/vLLM/Ollama  │    │
                          │  │  └─ 配置驱动注册表 (BYO-LLM)         │    │
                          │  └──────────────────────────────────┘    │
                          └─────────────────────────────────────────┘
```

**设计原则**（KISS/YAGNI）：
- IM 内核不动，AI 一律作为「可插拔外挂」接入（对齐 Mattermost/Rocket.Chat/野火共识）。
- 统一走 **OpenAI 兼容接口**做抽象，部署方自选云端 API / 自托管模型。
- 复用 `msg_c2s_logic:c2s_to_external/5` 已验证的投递骨架，不重造消息可靠性。

---

## 3. 分阶段任务规划 / Phased Task Plan

> 工作量档：S（局部改动）/ M（跨 2-3 层）/ L（跨模块+迁移+前端）。
> 每个任务标注 **[复用]** 或 **[新建]**。

---

### Phase 0 — LLM 适配层：把千帆硬编码解耦 🔌 [地基] ✅ 已交付（`0f230ee1`+`ae3baa72`，未 push）

**目标**：一条消息可在 qianfan / OpenAI 兼容 provider 间配置切换，为后续所有 AI 能力提供统一底座。

| ID | 任务 | 复用/新建 | 涉及文件 | 验收 | 档 |
|----|------|----------|---------|------|----|
| T0.1 | ✅ 定义 `imboy_llm` behaviour（provider 抽象，OpenAI 兼容语义） | [新建] | `src/lib/imboy_llm.erl` | callback `chat/3`、`chat_stream/4`、`capabilities/0` 定义清晰，dialyze 通过 | S |
| T0.2 | ✅ 把 `qianfan_api` 包成 `imboy_llm_qianfan` 实现 behaviour | [复用] | `src/lib/imboy_llm_qianfan.erl` 包裹 `qianfan_api` | 现有 `bot_qian_fan` 路径行为不变 | S |
| T0.3 | ✅ 新增 `imboy_llm_openai`（OpenAI 兼容，覆盖 DeepSeek/Qwen/vLLM/Ollama） | [新建] | `src/lib/imboy_llm_openai.erl` | 接一个本地 Ollama/vLLM 能正常对话 | M |
| T0.4 | ✅ provider 配置驱动注册表（对齐 `payment_gateway` 配置驱动模式） | [新建] | `src/ds/llm_provider_ds.erl` + `config/sys.*.config` `{llm_providers, [...]}` | `msg_c2s_logic:43` 的 fun 分派改为查注册表 | M |
| T0.5 | ✅ 改造 `c2s_to_external` 分派点为注册表查表 | [复用] | `src/logic/msg_c2s_logic.erl:37-51` | 新增 provider 无需改 `msg_c2s_logic` | S |

**behaviour 草案**：
```erlang
-module(imboy_llm).
%% 统一 OpenAI 兼容消息格式：[#{<<"role">>=>..., <<"content">>=>...}]
-callback chat(Uid::integer(), Messages::[map()], Opts::map()) ->
    {ok, #{<<"result">> := binary()}} | {error, term()}.
-callback chat_stream(Uid::integer(), Messages::[map()], Opts::map(),
                      StreamFun::fun((binary()) -> ok)) ->
    {ok, #{<<"result">> := binary()}} | {error, term()}.
-callback capabilities() -> #{stream := boolean(), vision := boolean(),
                              tools := boolean()}.
```

**测试**：EUnit + meck mock provider，验证配置驱动分派与降级（provider 不可用→友好错误）。

---

### Phase 1 — Agent 账号 + 一等参与者 👤 [核心] ✅ 已交付（`00b56a56`+`77138253`，未 push）

**目标**：AI 以真实账号身份进入 IM，能被 C2C 私聊、能被拉进群、群里 @它才回。

| ID | 任务 | 复用/新建 | 涉及文件 | 验收 | 档 |
|----|------|----------|---------|------|----|
| T1.1 | ✅ user 表加 `account_type` + 独立 `ai_agent` 元数据表 | [新建] | `priv/migrations/00000012_ai_agent.up.sql` | 迁移 up/down 真 PG 往返通过 | M |
| T1.2 | ✅ Agent 账号创建/管理（管理后台） | [复用] | `src/adm/adm_ai_agent_handler.erl` + imboyadmin 页面 | 后台可建 agent、绑 provider/role/owner | M |
| T1.3 | ✅ Agent 长期登录成一等 WS 用户（常驻 runtime 进程持 token 连 WS） | [复用] | 复用 `user_logic:online/4` + 新 `ai_agent_runtime`（gen_server，持 refresh token） | agent 显示为在线，能收 C2C | M |
| T1.4 | ✅ C2C 消息路由到 agent（to=agent 账号触发 LLM） | [复用] | `src/logic/msg_c2c_logic.erl:38` 判定 to.account_type=agent → 走 `c2s_to_external` 骨架 | 给 agent 发私聊，收到 LLM 回复 | M |
| T1.5 | ✅ 群防刷屏门控（agent 仅被@/`?`结尾/关键词/白名单触发） | [新建] | `src/lib/agent_trigger_policy.erl`，在 `msg_c2g_logic:46` 前置调用 | 群里 @agent 才回，普通消息不回 | S |

> **✅ 遗留已补（金钱 DoS 闸门，2026-07-10，未 push，经 3 轮多 agent review 硬化）**：新增 `src/lib/agent_rate_limiter.erl`——**双维固定窗口限流**：per-(scope,requester)（默认 30/60s，挡单账号 flood）+ per-scope 总量（默认 600/60s，属主账单硬顶，挡多账号分布式刷）。deny 静默丢弃（不回复/不花钱/不给攻击者信号）。
> - **覆盖两条 LLM 触发路径**（初版仅 C2C，review R1 抓到 bot_* 旁路）：① C2C agent 账号 `ai_agent_reply:dispatch/4`；② `bot_*` C2S `msg_c2s_logic:c2s_to_llm/4`。
> - **scope = 归一化计费身份**（review R3 抓到别名绕过）：bot_* 用 `provider_name(To)` 而非原始 `To`——`bot_qian_fan`/`bot_qianfan` 等别名映射同一 provider 必须共享计数，否则可交替别名双倍突破配额。C2C 用 agent uid(integer)。
> - **原子 + 真固定窗口**（初版 depcache read-then-write 有竞态、TTL 刷新非固定窗口，均被 review 抓到）：镜像 `msg_rate_logic` 的 `ets:update_counter/4` 单操作原子（无竞态）+ key 内嵌时间桶（跨桶自然归零，正确性不依赖定时器）+ `ensure_ready` 惰性自愈建表（免 gen_server/sup 接线）。
> - **22 EUnit 全绿零回归**：agent_rate_limiter 6 + ai_agent_reply 9（含 deny 路径）+ bot 路径 7（含别名共享计数回归测试）。
> - ⚠️ **已知出范围**（ponytail 标注）：单节点计数（各节点独立，够金钱 DoS 兜底）；仅分钟级速率闸门，无小时/日级预算硬顶（长期蚕食留给 billing/license 配额位）；`msg_c2s_logic:c2s_to_role_chat/3` 当前无调用者（死代码），接线时须一并走本闸门或直接删除。C2G 群 @agent 接线时走同一闸门。

**DDL 草案**：
```sql
-- user 表加账号类型（最小侵入，先一个字段）
ALTER TABLE public."user" ADD COLUMN account_type smallint NOT NULL DEFAULT 0;
COMMENT ON COLUMN public."user".account_type IS '0=human 1=ai_agent 2=system_bot';

-- agent 元数据独立表，避免 user 表膨胀
CREATE TABLE public.ai_agent (
    user_id       bigint PRIMARY KEY REFERENCES public."user"(id),
    provider      varchar(40)  NOT NULL,      -- qianfan/openai/...（对应 llm_providers key）
    model         varchar(80),
    role_id       varchar(40),                -- 对应 ai_roles
    system_prompt text,
    owner_uid     bigint,                     -- 归属/创建者（计费与权限用）
    trigger_policy jsonb DEFAULT '{}',        -- 群触发规则：{mention, suffix_q, keywords[], group_allowlist[]}
    status        smallint DEFAULT 1,         -- 1=启用 0=停用
    created_at    timestamptz DEFAULT now()
);
```

**关键复用点**：T1.4 直接复用 `c2s_to_external/5` 已验证的 stage/ACK/enqueue/异步/投递骨架——只是把入口从"特殊 to 字符串"换成"真实 agent 账号的 C2C/C2G"。**不重造消息可靠性**。

**里程碑 M1 = Phase 0 + Phase 1**：agent 能以真实身份收发消息、provider 可切换、群里守规矩。这是"最小可用的 AI Agent 载体"。

---

### Phase 2 — 流式回复 ⌨️ [体验] ✅ 已交付（后端 `09974f8f` + 前端 `8b8d5b56`+`965c51f8`，未 push）

**目标**：AI 回复逐字出现（真正"在打字"），中间态不落库，只定稿落归档。

| ID | 任务 | 复用/新建 | 涉及文件 | 验收 | 档 |
|----|------|----------|---------|------|----|
| T2.1 | ✅ 新增流式 content_type（走 JSON 通道，不改 proto） | [新建] | `imboy_codec.erl:285` 加 `stream`/`stream_delta`；注册 WS action | JSON 通道可发流式帧 | S |
| T2.2 | ✅ provider `chat_stream/4` + 节流（内存态增量，仅定稿落库） | [复用] | `imboy_llm_openai` 实现 SSE 流式（httpc）；`llm_stream` 共享节流模块；`c2s_to_external` 分支流式 | 2000 token 不产生 2000 次写库 | M |
| T2.3 | ✅ Flutter 流式气泡渲染（逐字气泡接线 + review 修复：竞态/乱序/泄漏/bot 归属） | [新建] | imboyapp 聊天组件（`message.dart` processMessage 最入口拦截共享 `stream_id`） | AI 回复逐字显示、定稿刷新 | L |

> **⚠️ 真机 E2E 未验**：Android 真机回归（`.env.pro` 生产 + Phase 2 debug 包）通过——app 启动/登录/聊天页/C2C 双勾送达全正常，前端 3 处改动无回归；iOS 启动+E2EE 公钥正常。但**流式新功能本身未真机验证**：生产后端未 push Phase 2 代码/provider/agent 账号，不下发 `stream_delta`，看不到逐字气泡。要验流式需 `.env.local` + 起 Phase 2 后端节点 + 配 OpenAI 兼容 provider（本地 Ollama/vLLM 或 mock SSE）+ 建 agent 账号 + `set_env llm_stream_enabled=true`。设计+2 轮 review 修复+踩坑详见项目记忆 `project_phase2_streaming_reply`。

> **后端 62 EUnit 全绿；前端 `dart analyze` 净。**

**关键约束**（研究结论）：中间 token **走内存态/ephemeral 更新，仅定稿 partial-update 落库**。否则打爆 timescaledb 写入。Erlang per-connection 进程模型天然适合"每个 run 一个进程 + 可取消（run.cancel）"。

**为何走 JSON 不改 proto**（YAGNI）：`imboy_codec` 里 JSON 路径 `msg_type` 是自由 binary（不强枚举），protobuf 路径才受 enum 约束。流式先走 JSON 通道成本最低；proto 支持留到真有性能需求再做。

---

### Phase 3 — 对外暴露 MCP Server 🔗 [差异化最大杠杆]

**目标**：Claude/Cursor/ChatGPT 用户零代码把 imboy 接入他们的 AI，读写消息/联系人/群。护城河=私有数据+连接权。

#### 技术选型（已调研定稿）：策略 B —— 借协议引擎，传输/鉴权用 imboy 自己的 cowboy

调研了 3 个纯 Erlang MCP server 库（`serge2/emcp`、`barrel-platform/barrel_mcp`、`erlsci/erlmcp`）+ 1 个 agent 网关（`peterdmv/beamclaw`）。结论：

- **不整库依赖，只取协议引擎**。MCP 有两层：① 协议逻辑（JSON-RPC 编解码 + 方法派发 + tool 注册 + schema 校验，传输无关）；② 传输特性（SSE/会话/重放/主动推送，和各库的 HTTP 栈耦合）。imboy 已有 cowboy 2.10（原生 HTTP/1.1+HTTP/2+SSE via `stream_events`）+ 静态路由 `imboy_router.erl`（全站 `/api/v1/*`）+ JWT 中间件，**传输②用自己的，只借协议①**。
- **路由前缀 = `/api/v1/mcp/`**（与全站 REST 一致）。MCP 端点直接进 `imboy_router.erl` 的静态 `ApiV1Routes`，**不走 `imboy_router_registry`**：该注册表服务的是冻结的动态插件平台（roadmap-only，现有插件也未实际使用它），而 MCP server 是核心固定端点、无需热插拔。（注册表路由前缀已由 ADR 0003 对齐为 `/api/v{n}/`。）
- **基座 = `barrel_mcp` 的协议引擎**（`barrel_mcp_protocol.erl`：`decode → handle/2 → drive_async_plan/3(AuthInfo) → encode`，完全传输无关）。选它因为：协议最全最新（2025-11-25，含 tasks/sampling/completion）、**用 OTP27 内建 `json` 零 JSON 依赖冲突**（imboy OTP28 ✓）、`AuthInfo` 通道接 JWT 最干净、维护者 Chesneau（hackney 作者）信誉最高。⚠️ 风险：barrel 跟的 `2025-11-25` 是 **draft**，MCP spec 仍在 RC 演进，vendor 后需持续跟踪定稿并回归兼容性。
- **h1/h2 传输层不引**（cowboy 全替代，零能力损失）；hackney 更无需——那是 barrel 的 MCP **client** 角色用的库，我们只取 server 协议引擎。fork barrel 时裁掉 app.src 的 h1/h2 依赖，只留协议/注册/schema 模块。
- **`beamclaw` 不作库用**（方向反：它是 MCP client/host），仅作 Phase 1/4「内建 agent」的 loop/approval/session 设计参考。
- 备选 `emcp`（自带 cowboy+SSE 现成，但协议旧一版、cowboy 2.17 版本冲突、单人 PoC）；`erlmcp` 出局（入站传输未落地、半成品重构）。

| ID | 任务 | 复用/新建 | 涉及文件 | 验收 | 档 |
|----|------|----------|---------|------|----|
| T3.0 | ✅ **前置 spike（先做，go/no-go）**：用 cowboy `stream_events` 验证 MCP Streamable HTTP 的 SSE + `Mcp-Session-Id` 会话 + `Last-Event-ID` 重放能自写实现并评估工作量 | [新建] | 一次性 spike，不进生产 | ✅ 结论=策略 B 传输自写**可控**（并入 T3.6 落地，真机验证 SSE 连接/重放/resync 全绿） | S |
| T3.1 | ✅ vendor/fork `barrel_mcp` 协议引擎（裁掉 h1/h2 传输层，只留协议+注册+schema） | [复用] | vendor 进 `src/mcp/`（`barrel_mcp_protocol`/`barrel_mcp_registry`/`barrel_mcp_schema`）；改 app.src 依赖 | ✅ 编译+单测通过 `fb8ad16c` | M |
| T3.2 | ✅ cowboy handler 桥接协议引擎，挂 `/api/v1/mcp` | [新建] | `src/api/mcp_handler.erl`，路由进 `imboy_router.erl` 静态 `ApiV1Routes`（不走 `imboy_router_registry`，理由见上节） | ✅ 真机 POST 响应 initialize/tools/list（7 tool） `97b39276` | M |
| T3.3 | ✅ JWT 注入：handler 复用 `auth_middleware` 取 uid → 作 `AuthInfo` 线程进 tool `Ctx.auth_info` | [复用] | `mcp_handler` 调现有 `auth_middleware`；`drive_async_plan/3` 第三参 | ✅ 真机验证 uid 传递（Bearer→condition→handler_opts→Ctx.auth_info→闸门） `3c1ed095` | M |
| T3.4 | ✅ tool wrapper：`reg_tool` 注册薄封装，函数体调 imboy 现有 logic | [复用] | `src/mcp/imboy_mcp_tools.erl`（映射见下表） | ✅ 7 个 tool 可被调用 `0660d50f`/`18353838`/`bed09ffc` | M |
| T3.5 | ✅ 治理：管理员审批 + 按 tool 授权 + 审计日志 | [复用] | imboyadmin UI + 复用 JWT/权限/审计；后端 `adm_mcp_handler`/`mcp_governance_logic`/`mcp_authz_gate`/迁移 00000028 | ✅ 后端 `e379fb4d` + 前端骨架 `9fd3ac1` + 前端联调真接口 `7ebd02c`(imboyadmin)；enforce 状态机 pending→deny→approve→allow 真机全验 | M |
| T3.6 | ✅ SSE 流式 + `Mcp-Session-Id` 会话 + `Last-Event-ID` 重放 | [新建] | cowboy `cowboy_loop`（barrel 的 SSE 绑 h1/h2 搬不动，自写）；反代 `proxy_buffering off` | ✅ 真机验证真实重放(`id:`帧)+resync 降级(`last_event_id_expired`) `87afcc04` | M |

> **MVP 边界**：T3.0 先做 spike 探 SSE/session 自写可行性（go/no-go）。T3.1–T3.5 的 `tools/list`+`tools/call` 是同步 request/response，**不依赖 SSE 即可跑通**——即使 T3.0 判定 SSE 难自写，同步 MVP 仍可先上，或回退 emcp（自带 SSE）。T3.6 的 SSE/会话/长任务是第二步，成败取决于 T3.0。

> **✅ Phase 3 交付状态（2026-07-10，均未 push）**：T3.0–T3.6 全部完成并真机 E2E 验证（独立节点 9810 + curl，不扰主节点）。
> - **真机 E2E 全绿**：initialize / tools/list(7 tool) / SSE 连接+会话头 / 真实重放(`Last-Event-ID`→`id:`帧) / resync 降级 / enforce 授权状态机（pending→`deny(客户端待审批)`→approve→`allow`，grants 7 tool 全 enabled，审计流水落库）。
> - **⚠️ E2E 暴露真 bug 已修 `81c2870e`**：`imboy_app:tsid_generator_names()` 遗漏 `mcp_client`/`mcp_client_grant`/`mcp_audit_log` 三个 TSID 标签，生产 enforce=true 首个 `tools/call` 与 adm approve 端点会崩 `elib_tsid_generator_not_registered`；干净重启验证三标签开机自动注册、全链路无崩溃。
> - **生产启用**：治理默认 `mcp_governance_enforce=false`（只登记+审计放行）；enforce=true 需显式配置。排障方法学见项目记忆 `project_mcp_phase3_e2e_realdevice`。

**MCP tools 首批映射**（全部复用现有 logic，零新业务）：

| MCP tool | 映射到现有 | catalog 分组 |
|----------|-----------|-------------|
| `send_message` | `msg_c2c_logic:c2c/4` | 消息 |
| `search_messages` | `fts_logic`（全文搜索，Phase 未来加语义） | 全文搜索 |
| `list_conversations` | 会话 API | 会话 |
| `get_contacts` | 好友 API | 好友 |
| `create_group` / `list_group_members` | `group_logic` / 群成员 API | 群组核心/群成员 |
| `get_user_profile` | 用户 API | 用户 |

**治理对齐 Slack MCP server**：workspace 管理员审批并管理所有 MCP 客户端接入，OAuth 按 tool 授权 + 审计日志 + IP allowlist。imboyadmin 承接审批与审计 UI。

**里程碑 M2 = Phase 2 + Phase 3**：流式体验 + 对外 MCP。此时 imboy 已是「可被外部 AI 调用 + 内建 AI agent」的双向载体。

---

### Phase 4 — 插件市场→Agent/MCP 市场 + 前瞻 🌐 [卡位]

> **📋 立项规划已产出（2026-07-10，多 agent 工作流）**：[phase4-agent-mcp-marketplace-charter.md](../planning/phase4-agent-mcp-marketplace-charter.md)——3 路研究现状 + architect 综合，含前置依赖核对、AGUI A/B 决策(倾向 A + 6 项 spike go/no-go)、T4.1/T4.2/T4.3 分子任务立项(复用点文件级/新建点/MVP 边界/工作量/风险)、落地顺序、非目标、开放问题。⚠️ 关键发现：Phase 3 MCP 与插件体系**完全脱钩**（T4.1 正是要接起来）；`ai_agent.owner_uid` 已建表但**无任何计费代码消费**（T4.3 记账到属主是纯占位）；群内 agent 触发链路未落地（T4.3 隐藏前置）。**建议先稳固 M1/M2 交付基线（push + Phase 2 真机验证）再启 M3。**

**目标**：把插件市场升级为「可被外部 AI 发现和调用的能力目录」，并卡位 A2A/MCP/AGUI 都不覆盖的位置——**IM 作为多人群 agent 协作的人类实时载体**（不与任何协议抢，只做它们之上「多人 + 私有化」的产品形态）。

#### AGUI 对比（T4.2 的前置决策）

AGUI（Agent-User Interaction Protocol，CopilotKit）已标准化**单个用户 ↔ 单个 agent** 的前端事件流（run / text 流 / tool_call / state_delta / HITL 审批）。它**不覆盖** imboy 的场景——多人在一个群里共同观察、介入多个 agent 协作。两条路，需 spike 定夺（未深读 AGUI 原文前不拍死）：

- **A（用 AGUI 事件模型做 IM 渲染器输入，倾向此路）**：把 AGUI/A2A 事件流作为 agent→IM 的标准输入，imboy 将事件渲染成群消息 + 补 AGUI 不做的「多人群」形态。复用标准、只做 IM 独有的多人载体，与 Phase 3「借 barrel 协议引擎」策略一致。
- **B（绕开 AGUI 自建事件契约）**：仅当 AGUI 单人事件模型与 IM 多人群语义严重不匹配时才走。

| ID | 任务 | 复用/新建 | 涉及文件 | 验收 | 档 |
|----|------|----------|---------|------|----|
| T4.1 | plugin manifest 扩展 `mcp_tools`/`a2a_agent_card` 声明 | [复用] | `imboy_plugin_registry` manifest v2；`imboy_plugin_generic_sup` 空骨架挂 agent worker | 插件可声明暴露的 MCP tools | M |

> **🔨 T4.1 首切片已实施（2026-07-10，未 push，spike 确认为 Phase 4 正确起点）**：`imboy_plugin_registry` 的 `normalize_manifest/1` 加可选 `mcp_tools` 字段（默认 `[]`，向后兼容）+ `mcp_tool_declarations/0`/`mcp_tools_from/1` 采集器；`imboy_mcp_tools:reg_all/0` 加 `reg_plugin_tools/0` 桥接——把生产插件声明的 mcp_tools 注册进 MCP 表。**安全**：仅从硬编码生产 manifest 采集（在仓可信代码，规避第三方任意 mfa 越权）；结构不完整声明跳过并告警。**6 EUnit 全绿 + 39 既有测试无回归**。
> **🔨 T4.1 第二切片：能力发现端点（同日，未 push）**：新增 `src/api/agent_card_handler.erl` + 路由 `GET /api/v1/agent-card`（进静态 ApiV1Routes，遵路由惯例）——广告身份 + MCP 端点(`/api/v1/mcp`) + 插件声明的 tools（仅 name/description，不泄露 module/function/schema）；复用 `app_manifest_handler` build→ETag→200 模式。**3 EUnit 全绿**。至此 T4.1 核心达成（插件可声明 MCP tools + 可被外部 AI 发现）。
> **剩余 T4.1（低价值，按 spike B 路降级）**：marketplace index.json 升版（纯生态快照、不接后端运行时）、`a2a_agent_card` 字段（B 路不采完整 A2A，仅借 task 词汇 → 暂缓）。
> **📋 Phase 4 spike go/no-go 决策已产出**：[phase4-spike-gonogo-decision.md](../planning/phase4-spike-gonogo-decision.md)——AGUI **裁决 B 路 + 轻混合**（自建契约 + 仅借 A2A task 词汇；6 探针 5 判 No-Go）；fan-out **S–M 可行非阻断**（复用 `imboy_syn:publish`+`group_ds:member_uids` N×单播，勿碰 `?ROOM_SCOPE` 空壳）；**T4.2 转 ready**（余 2 非阻断边界：观察流 ephemeral/可靠档位、多授权人审批去重）；推荐首任务 = T4.1。⚠️ D1：**Phase 4 启动仍 user-gated**（先稳 M1/M2：push + Phase 2 真机 E2E）。
> **🔨 T4.2 fan-out 原语切片已实施（2026-07-10，未 push）**：`llm_stream:publish/3` 加可选 `observer_uids` 支持——有则扇出给群内多观察者，缺省回退单 `target_uid`（Phase 2 向后兼容，注意 `maps:get/3` 默认急切求值故用 case 惰取）。**3 EUnit + Phase 2 流式 16 回归全绿**。这是 T4.2「群 agent 协作可观测窗」的多播原语。
> **🔨 群触发地基已实施（T4.2 核心 + T4.3 共同前置，2026-07-10，未 push）**：新增 `src/logic/ai_agent_group_reply.erl` + 挂钩 `msg_c2g_logic:do_send_c2g`（fire-and-forget）——群里 @agent → 门控（复用 `agent_trigger_policy` + `agent_rate_limiter`）→ 异步 LLM → 流式 fan-out 给群成员（用 llm_stream observer_uids）→ 定稿以 agent 一等成员身份经 `msg_c2g_logic:c2g` 回群（复用 QoS/归档/扇出）。**防线**：E2EE 群跳过（红线）、agent 发送者不触发（防 agent↔agent 环）、agent 回复不 @agent 故不自触发。**5 EUnit 全绿 + 39 会话全测无回归**。这解开 spike 指出的「msg_c2g_logic 未接 agent 触发」前置。剩余 T4.2（A2A task 事件桥接、群渲染卡片、审批控制台）为 L 级后续。
> **✅ 群触发经多 agent review 硬化（2026-07-10）**：security review 抓 2 HIGH + 2 MEDIUM 已修——① HIGH mentions 未去重（`[42,42,...]` 单帧烧光限流）→ `lists:ukeysort/2` 按 uid 去重；② HIGH 挂钩无视 stage 结果（QoS 正常重发 `{ok,duplicate}` 误触发）→ 移进 `do_stage_and_send_c2g` 的 `{ok,new}` 分支（含引用不存在 error 场景一并解决）；③ MEDIUM fan-out 未过滤在线 → `run_stream` 前置 `user_logic:is_online` 过滤。E2EE 红线/防环/T4.1 越权经审查达标。含去重回归测试，**47 会话全测绿**。
> **🔨 T4.3 支付 mandate 地基已实施（多 agent，未 push）**：新增 `priv/migrations/00000029_agent_payment_mandate` + `agent_payment_mandate_repo/ds` + `src/logic/agent_payment_logic.erl:pay_with_mandate/4`——**三道闸门**：① mandate 有效(SQL 侧 `status=1 AND expires_at>NOW()`)；② 单笔≤max_amount_fen + 周期累计原子 `try_reserve`（固定窗口 check-and-increment，同 agent_rate_limiter 思路）；③ 从 **owner_uid**（非 agent 自己）幂等扣款（`wallet_ds:atomic_balance_change`+`find_transaction_by_ref` RefNo）。扣款失败释放预留。**7 EUnit + 真 PG 往返验证**。⚠️ **贷记结算腿延后**（当前仅原子借记付款人 + ToUid 记 ledger remark，无实际到账；避免两次非原子 balance_change 的资金丢失窗口，留待 transfer/escrow 集成）。未复用 `transfer_logic:send`（托管需 accept、无 RefNo 幂等）。
| T4.2 | **IM 作为 A2A 协作的可观测前端**（不承载 A2A 协议本身，只做人类观察窗 + 介入控制台） | [新建] | PoC：订阅/桥接 A2A task 事件 → 渲染成群消息流（一条消息=一个 task 状态更新/一次 agent 发言），复用 WS+ACK+群成员权限；人类可 @追问、在 `awaiting_approval` 点审批 | agent 之间跑**标准 A2A**，人在群里实时观察并介入（PoC） | L |
| T4.3 | Agent 支付探索（AP2/x402 + erlang_pay） | [复用] | `erlang_pay` + AP2 mandate | 群里 agent 可发起受控支付（PoC） | L |

**里程碑 M3**：前瞻性 PoC，验证「IM 作为 agent 实时协作总线」方向。不追求生产化。

---

## 4. E2EE 红线（贯穿全程，不可逾越）/ E2EE Guardrails

> imboy 已有「服务端不接触明文私钥」红线（见项目记忆 `e2ee_backend_audit`）。AI 接入**绝不能破坏它**。

| 场景 | 允许的 AI 处理 |
|------|--------------|
| **非 E2EE 群/频道/公开内容** | ✅ 服务端 LLM 可处理（摘要/翻译/审核/客服） |
| **E2EE C2C/私群** | ❌ 服务端**绝不解密**做 AI；只能：① 端侧小模型（Flutter `llama_cpp_dart`，数据不出设备）；② 用户显式把消息发给 agent（agent 是显式一端） |
| **任何 AI 功能** | 默认关闭、opt-in、AI 作为对话显式一端提示用户（对齐 WhatsApp/Signal 立场） |

**产品原则**（差异化叙事）：agent 能力默认关闭、可 opt-out、边界清晰——把 Signal 的批评当成产品准则，反而是主打隐私/私有化客户的卖点。

---

## 5. 非目标 / Non-Goals（YAGNI，明确不做）

- ❌ **不自建 TEE 机密计算**（AMD SEV-SNP + H100 远程证明）——Meta 级投入 + 两轮第三方审计才勉强达标，中小团队不宜自建。等云厂商机密计算 GPU 托管成熟再评估。
- ❌ **不在 E2EE 会话做服务端 AI**——红线。
- ❌ **不自训模型**——BYO-LLM 适配层，部署方自选。
- ❌ **不按 token 对私有化客户计费**——模型跑客户机上收不到；走 License 授权位（见变现）。
- ❌ **Phase 2 先不改 proto**——走 JSON 加 stream 类型；proto 支持等真有性能需求。
- ❌ **不建复杂 agent 权限体系**——`account_type` 一个字段 + `ai_agent` 表起步。

---

## 6. 变现挂钩 / Monetization Hook

复用已有 `imboy_license`，扩一个 `ai_module` 授权位（**按部署规模/席位授权，不按 token**）：

| 档位 | AI 能力 | 闸门 |
|------|--------|------|
| 标准档（引流） | 单聊摘要/翻译、基础 agent | — |
| 企业档（付费） | 气隙部署、AI 用量审计、分级权限护栏、多租户 LLM 管理、MCP server 治理 | License gate |

卖点叙事：**BYO-LLM + E2EE + 私有部署 = 可过合规审计**（数据主权≠AI 主权，切受监管行业审计后半段）。木兰无 AGPL 传染，比 OpenIM（客户端 AGPL）、Rocket.Chat（EE 闭源门槛）对怕许可传染的政企更友好。

---

## 7. 落地顺序与里程碑 / Milestones

| 里程碑 | 内容 | 交付价值 | 状态 |
|--------|------|---------|------|
| **M1** | Phase 0 + Phase 1 | 最小可用 AI Agent 载体：agent 真实身份收发、provider 可切、群守规矩 | ✅ 已交付（未 push） |
| **M2** | Phase 2 + Phase 3 | 流式体验 + 对外 MCP：双向 AI 载体 | ✅ 已交付（未 push；Phase 2 流式真机 E2E 待验） |
| **M3** | Phase 4 | IM-as-agent-bus 前瞻 PoC | ❌ 未开工（待立项） |

**建议起步**：T0.1→T0.5（LLM 适配层）是一切的地基，且改动局部、风险低、能立即让现有千帆通道支持 DeepSeek/Qwen/本地模型——**投入产出比最高的第一铲**。

---

## 附录：关键文件索引 / Key Files

| 用途 | 文件 |
|------|------|
| AI/Bot 通道骨架 | `src/logic/msg_c2s_logic.erl`（`c2s_to_external/5`、`c2s_to_role_chat/3`） |
| 现有 LLM 调用 | `src/lib/qianfan_api.erl` |
| WS action 注册 | `src/lib/imboy_ws_action_registry.erl:76` |
| HTTP 路由注册 | `src/lib/imboy_router_registry.erl:67` |
| 插件 behaviour | `src/lib/imboy_plugin.erl` |
| 消息路由 | `src/logic/message_router_logic.erl:34` |
| C2C/C2G 发送 | `src/logic/msg_c2c_logic.erl:38` / `src/logic/msg_c2g_logic.erl:46` |
| 消息编解码/类型 | `src/lib/imboy_codec.erl:285` |
| user 表 DDL | `priv/migrations/00000001_foundation.up.sql:2459` |
| REST API 全目录 | `docs/reference/rest-api-v1-catalog.md` |
| License 授权 | `imboy_license` |

---

*研究来源见同批次深度研究报告（4 路 web 研究 + AGUI 原文深读，约 60 篇引用）。MCP / A2A / AGUI 均为 2025-2026 已成型的协议标准（三层各有主）；imboy 的定位不是发明协议，而是「在已有协议之上做私有化 + 多人 + 合规的 IM 产品形态」——这一产品形态尚无标准占据。Phase 4（A2A/AGUI 之上的多人载体）为前瞻探索。*
