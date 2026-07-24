# Phase 2 流式 + Phase 4 全链真机 E2E 验证清单

> 用途：设备就绪后照此逐条执行，验证 Phase 2 流式回复 + Phase 4 群@触发/任务观察/审批/支付指令全链。
> 性质：**纯执行手册**（只读整理，未改任何生产代码）。所有接线锚点均已对磁盘核实（`文件:行号`）。
> 创建：2026-07-10 | 状态：待设备就绪执行 | 依据：`ai-agent-platform-roadmap.md` §Phase2/§Phase4 + `phase4-*` 两份决策文档 + 代码实测。

---

## 0. 硬阻塞与前置（不满足则不能开跑）

### 运行环境实测（2026-07-11，对磁盘/对库核实）

- ✅ **代码/节点就绪**：运行中 release 于 **07-10 23:16 构建**（新），`llm_stream`/`ai_agent_reply`/`ai_agent_group_reply`/`agent_task_demo`/`agent_task_observer`/`agent_payment_logic`/`agent_payment_mandate_ds`/`imboy_llm_openai`/`imboy_mcp_tools` 全部 beam 在 release ebin 内。节点 `imboy_dev@127.0.0.1`，9800 LISTEN。
- ✅ **DB 同库确认**：`.mcp.json`(`local_imboy`) 与 `sys.local.config:94-114` 均指 `127.0.0.1:4323 / imboy_v1 / imboy_user`。**待确认-1 已解**。
- ✅ **Phase 1/3 schema 已在库**：`ai_agent` / `mcp_client` / `mcp_client_grant` / `mcp_audit_log` 表存在；`user.account_type` 列存在（205 用户全 type=0 human）。
- ❌ **B3 当前不通过**：`schema_migrations` 当前版本 **28**（干净、非 dirty）；**迁移 29（`agent_payment_mandate` 表）与 30（`chk_wallet_tx_type` 放行 20/21）未应用**。实测约束仍为 `ARRAY[1..11]`，`agent_payment_mandate` 表不存在。
- ⏳ **尚无 agent 账号**：account_type 分布仅 0（human）——需按 §1(3) 建一个 agent。

### 分区就绪度（重要）

| 场景 | 依赖 29/30？ | 现状 |
|------|:---:|------|
| §2 Phase 2 C2C 流式 | 否 | **可跑**（补 provider+开关+agent 账号+真机） |
| §3 群@触发+fan-out | 否 | **可跑**（同上，另需群+2 在线成员） |
| §4/§5 任务观察+审批 | 否 | **可跑**（同上） |
| §6 支付指令 | **是** | **阻塞**：须先应用迁移 29+30 |

| # | 前置 | 检查命令 | 通过判据 |
|---|------|---------|---------|
| B0 | **真机在线**（禁模拟器） | `list_devices` / `adb devices` | 至少一台物理 iPhone/Android，非 simulator |
| B1 | 后端节点运行 | `pgrep -fl "_rel/imboy/bin/imboy"` | ✅ 已满足 |
| B2 | **DB 对账**（最易翻车） | 对比 `.mcp.json` 与 `sys.local.config` pg_conf | ✅ 已满足（同 `imboy_v1@127.0.0.1:4323`） |
| B3 | migration 29+30 已应用 | `SELECT version FROM schema_migrations;`（当前 28）+ 查 `chk_wallet_tx_type` 含 20,21 | ❌ **未满足**——仅支付验证(§6)需要；顺序应用 29→30（DB 干净停 28，无缺口） |
| B4 | 真机登录账号可用 | 见 §待确认-2 | 待现场（库有 205 human 账号，密码真值待定） |

> `remote_console` 进入：`_rel/imboy/bin/imboy remote_console`（cookie `imboycookie`）。

### ⚠️ 待现场确认（执行前逐条落定）

1. **MCP 库 == 后端库？** 用户声明 DB 走「pgsql MCP 配置的库」，但后端 `config/sys.local.config:94-114` `pg_conf` 指向 `127.0.0.1:4323 / imboy_v1 / imboy_user`。**若 MCP 指向的不是同一库，你预置的 agent/mandate/wallet 行后端看不到，全链失败**。先 `db ping` + `current_database()` 与 MCP 侧对账。
2. **真机登录账号真值**：项目记忆的 `uid5/uid7 + Test1234` 在库文档中 grep 不到；`docs/reference/ws-repl-cheatsheet.md:22-42` 记的是 `alice@test.com`/`bob@test.com` 密码 `test123456`。以现场查 `user` 表 / 实际登录成功为准。
3. **imboyapp @ 是否把目标 uid 写进 `payload.mentions`**：群触发的**唯一前端依赖**。发 @agent 消息时抓 WS 帧确认 `payload.mentions` 含 AgentUid，否则群触发不成立。
4. **前端是否监听 `a2a_task_update`**：codec 白名单有此 msg_type（`imboy_codec.erl:314`）但当前后端只 emit `agent_task`，无生产者。若前端卡片只认 `a2a_task_update` 则看不到任务卡片。
5. **provider 可达**：`llm_providers` 里配的 OpenAI 兼容端点（DeepSeek/Qwen/本地 Ollama/vLLM）网络可达且 key 有效，否则流式看不到逐字。

---

## 1. 一次性环境注入（remote_console）

> `config_ds` **无 `set_env`**；运行时打开用裸 `application:set_env/3`。持久化则写进 `sys.local.config` 后 `config_ds:local_reload()`。

```erlang
%% (1) 配 OpenAI 兼容 provider —— name 必须与 agent 的 provider 字段一致
application:set_env(imboy, llm_providers,
  [#{name => <<"openai">>, module => imboy_llm_openai,
     base_url => <<"https://api.deepseek.com/v1">>,
     api_key  => <<"sk-...">>,
     model    => <<"deepseek-chat">>,
     max_tokens => 512}]).

%% (2) 打开流式（三条件之一：另两条 openai 适配器已满足 chat_stream/4 + capabilities.stream=true）
application:set_env(imboy, llm_stream_enabled, true).
config_ds:env(llm_stream_enabled).   %% 校验 = true

%% (3) 建 agent 账号（OwnerUid = 你真机登录的 uid）
{ok, #{<<"user_id">> := AgentUid}} =
  ai_agent_ds:create(#{
    <<"nickname">>      => <<"小助手"/utf8>>,
    <<"provider">>      => <<"openai">>,
    <<"system_prompt">> => <<"你是群助手">>,
    <<"owner_uid">>     => OwnerUid,
    <<"trigger_policy">>=> #{<<"mention">> => true}
  }).
%% 记下 AgentUid —— C2C 私聊目标 + 群里 @ 目标

%% (4) 仅支付验证需要：预置一条 status=1 未过期 mandate（owner = 群里发指令的人）
{ok, _MandateId} = agent_payment_mandate_ds:create(#{
    owner_uid       => OwnerUid,
    agent_uid       => AgentUid,
    max_amount_fen  => 10000,     %% 单笔上限 100 元
    max_total_fen   => 50000,     %% 周期累计 500 元
    expires_in_secs => 86400
  }).
%% 同一 agent 同时只能一条 status=1（唯一索引 uniq_agent_payment_mandate_active）
%% 另需：owner 与收款人各有 wallet，owner 余额 > 付款额（否则 insufficient_balance）
```

关键锚点：provider 读 `imboy_llm_registry.erl:21`；开关读 `llm_stream.erl:76`；建号 `ai_agent_ds.erl:35`（account_type=1，表 `00000027`）；mandate `agent_payment_mandate_ds:create`（表 `00000029`）。

---

## 2. Phase 2 — C2C 流式回复

| 步骤 | 操作 | 预期 | 通过判据 |
|------|------|------|---------|
| 2.1 | 真机账号给 `AgentUid` 发一条**普通文本**私聊 | 触发 `msg_c2c_logic.erl:69 → ai_agent_reply:maybe_dispatch/3` | 后端日志见 dispatch，无 rate-limit deny |
| 2.2 | 观察气泡 | 逐字出现（打字机效果） | 收到多帧 `msg_type:"stream_delta"`，`payload.delta` 累加、`index` 递增 |
| 2.3 | 回复结束 | 收尾帧 `is_end=true` | 流式气泡被**同 `stream_id`** 的定稿 `text` 消息替换（主键去重），不重影 |
| 2.4 | 反例：非文本/E2EE 私聊 | 不触发 LLM | `ai_agent_reply.erl:34/39` 跳过 |

`stream_delta` 帧结构（`llm_stream.erl:96-109`）：
```json
{"id":"<stream_id>","type":"C2C","from":<AgentUid>,"to":<HumanUid>,
 "msg_type":"stream_delta",
 "payload":{"stream_id":"<sid>","index":<seq>,"delta":"文字","is_end":false}}
```
> 节流：满 12 字节才推一帧（`llm_stream.erl:28`）；直推走 `imboy_syn:publish`，不落库/不 ACK；定稿走 `message_ds:send_next`（落库）。

---

## 3. Phase 4 — 群@触发 + fan-out

**前置**：AgentUid 已 `status=1`、已被拉进测试群、`trigger_policy.mention=true`。至少 2 个在线成员以验 fan-out。

| 步骤 | 操作 | 预期 | 通过判据 |
|------|------|------|---------|
| 3.1 | 群里发文本 **@AgentUid**（确认 `payload.mentions` 含 AgentUid，见 待确认-3） | `msg_c2g_logic.erl:357` 在 `{ok,new}` 分支触发 `ai_agent_group_reply:maybe_dispatch/4` | 后端见 dispatch |
| 3.2 | 观察**多个在线成员**端 | 每个在线成员都看到逐字流 | fan-out：`observer_uids` = 在线成员（`ai_agent_group_reply.erl:124`），各自收到 `stream_delta`（`type:"C2G"`） |
| 3.3 | 回复定稿 | agent 以一等成员身份回群一条正式消息 | 走 `msg_c2g_logic:c2g`（`:263`），落库+QoS+全员可见 |
| 3.4 | 反例：agent 自己发言、E2EE 群、普通不@消息 | 均不触发 | 防 agent↔agent 环（`:41`）、E2EE 红线跳过、非 mention 不触发 |

---

## 4. Phase 4 — 任务观察 + 审批卡片（agent_task_demo）

> ⚠️ **已知缺陷（属实，真实越权面）**：`agent_task_demo_handler.erl:30-45` **只校验 `group_id>0`，不校验 current_uid 是否该群成员**。任意登录用户可对任意 group_id 触发 demo。真机联调无害，**真实 A2A bridge 落地前必须补群成员校验**（对齐 `agent_task_handler` 审批端点的 `authorized/2`）。

| 步骤 | 操作 | 预期 | 通过判据 |
|------|------|------|---------|
| 4.1 | 群成员账号 `POST /api/v1/agent_task/demo`，体 `{"group_id":<GID>}`（JWT 会话充当 agent） | `agent_task_demo:run_demo/3` 发 `working` + `awaiting_approval` 两事件（同 task_id，恒 e2ee=false） | 200 返回 `{agent_uid, group_id, member_count}` |
| 4.2 | 在线成员端观察 | 先收进度（ephemeral），后收审批卡片 | `working`→`msg_type:"agent_task"` ephemeral（`agent_task_observer.erl:106`）；`awaiting_approval`→durable 卡片（`:148`，payload `status:"awaiting_approval"`, `actions:["approve","reject"]`） |
| 4.3 | **取 task_id** | 从审批卡片群消息 `payload.agent_task.task_id` 读 | ⚠️ demo 端点响应体**不含** task_id（`:41-44`），必须从卡片帧抓 |

> `emit` E2EE 闸门 fail-closed：仅 `e2ee=:=false` 才投（`agent_task_observer.erl:240-241`）。

---

## 5. Phase 4 — 审批仲裁（first-wins）

| 步骤 | 操作 | 预期 | 通过判据 |
|------|------|------|---------|
| 5.1 | **群成员**账号 `POST /api/v1/agent_task/approve`，体 `{"task_id":"<tid>"}` | `agent_task_observer:do_decide/3` 授权 `approver≠agent 且 approver∈群成员`（`:182,212`）→ `ets:insert_new` 抢占（`:194`）→ durable 群消息 | 200，群里落一条 approved 消息 |
| 5.2 | 第二个成员再 approve/reject 同 task_id | first-wins：后到者失败 | 返回 `already_decided`（`:207`），不重复落消息 |
| 5.3 | 反例：非群成员 / agent 自己审批 | 拒绝 | `authorized/2` = `lists:member(uid, group_ds:member_uids(gid))` 挡下 |

> ApproverUid 恒取 JWT `current_uid`，**绝不从请求体读**（`agent_task_handler.erl:41`）。

---

## 6. Phase 4 — 群内确定性支付指令

> 红线：**LLM 不进资金路径**，指令由 `agent_payment_command.erl` 确定性解析。前置见 §1(4) mandate + wallet 余额 + migration 00000030。

| 步骤 | 操作 | 预期 | 通过判据 |
|------|------|------|---------|
| 6.1 | **owner 账号**在群里发「@AgentUid @PayeeUid 转账 50」（mentions=[AgentUid, PayeeUid]，恰 1 agent + 1 非 agent 收款人） | `ai_agent_group_reply.erl:151 try_pay_command` 命中，不走 LLM | — |
| 6.2 | 授权校验 | 仅 `FromUid == mandate.owner_uid` 放行（`agent_payment_command.erl:49`） | 非 owner 发同指令→**静默无反应**（防 oracle，`:209-216`） |
| 6.3 | 三道闸门 + 结算 | ①mandate 有效 ②单笔≤max + 周期累计 try_reserve ③`wallet_ds:atomic_transfer` 单事务两腿（借 owner tx_type=20 / 贷 payee tx_type=21） | 群回执「已向 X 付款 XX 元」（`:224-231`）；owner 余额减、payee 增 |
| 6.4 | 幂等 | RefNo=`AGP_<MsgId>`；重发同消息不重复扣 | `find_transaction_by_ref` 命中不二次扣 |
| 6.5 | 边界：超单笔上限 / 超周期累计 / mandate 过期 / 余额不足 | 拒付、无扣款 | 闸门拦截，`try_reserve` 失败释放预留（`agent_payment_logic.erl:137`） |

金额解析：`?CMD_RE=(?:付款|转账|支付|pay|transfer)(?=[\s0-9@:：]|$)` + 元（≤2 位小数）→整分（`agent_payment_command.erl:25,68-86`）。

---

## 7. 路由自检（可选，起跑前 curl 冒烟）

全部 `/api/v1/*`（静态 ApiV1Routes，走 JWT 中间件）：
- `GET /api/v1/agent-card`（`imboy_router.erl:47`）+ `GET /.well-known/agent.json`（`:49`）
- `POST /api/v1/agent_task/demo`（`:441`）
- `POST /api/v1/agent_task/approve`（`:438`）/ `reject`（`:439`）

> local 环境 `api_auth_switch=off`（`sys.local.config:9`）关的是**设备签名**校验，**不是 JWT**——仍需登录拿 token。

---

## 8. 收尾

- 本轮为**验证**，不含 push / commit（外向操作 user-gated）。
- 若 E2E 暴露真 bug（如 Phase 3 曾暴露 tsid 标签遗漏 `81c2870e`），先记录 → 隔离 verbose 复现 → 修 → 重跑，勿直接改测试掩盖。
- 验证结论回填本文件 §状态，并按需更新项目记忆 `project_phase2_streaming_reply` / `project_ai_agent_phase4_commits`。
