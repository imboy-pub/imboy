# Phase 4 立项规划文档 / Phase 4 Project Charter

> imboy AI Agent 载体路线图 · Phase 4「插件市场→Agent/MCP 市场 + 前瞻」立项深化
> **依据**：`docs/planning/ai-agent-platform-roadmap.md` v1.3.0 §3 Phase 4 / §4 E2EE 红线 / §5 非目标
> **状态**：立项待决策（Phase 0/1/2/3 ✅ 已交付未 push；Phase 4 ❌ 未开工）
> **性质**：前瞻 PoC 立项，**不追求生产化**（对齐 M3 里程碑定性）

---

## 1. 一句话定位 + 立项背景 / Positioning & Context

**一句话**：Phase 4 = 卡位「A2A / MCP / AGUI 三层协议都不覆盖的位置——多人群 agent 协作的**人类实时载体**」，把已交付的 Phase 0–3 地基（provider 适配 / 一等 agent 账号 / 流式 / MCP server）缝成一个"agent 协作的人类观察+介入总线"，全程复用不新造协议。

**立项背景**（诚实版，延续 roadmap §0 战略校正）：
- 三层协议**都已名花有主**：MCP=Anthropic/AAIF、A2A=Google/Linux Foundation v1.0、AGUI=CopilotKit（单人 agent↔user）。**不存在协议空白**。
- imboy 的护城河**是产品形态**：私有化 + 多人 + 合规的 IM。AGUI 明确只做**单人**「一个 user 观察一个 agent」，**不做多人共同观察/介入**——这正是 imboy 群聊天生擅长、且无标准占据的形态位。
- Phase 4 不发明协议，只做「在 A2A/AGUI 之上的多人 IM 载体」，把 imboy 已验证的消息可靠性（QoS/ACK）、群权限、@触发、审批治理范式**当地基复用**。

---

## 2. 前置依赖核对 / Prerequisite Wiring

Phase 0–3 均已交付（未 push）。Phase 4 三子任务对既有地基的依赖：

| 子任务 | 依赖 Phase 0 (LLM适配) | 依赖 Phase 1 (agent账号) | 依赖 Phase 2 (流式) | 依赖 Phase 3 (MCP) |
|--------|:--:|:--:|:--:|:--:|
| **T4.1** 能力目录 | — | agent 账号/`owner_uid` 归属 | — | `barrel_mcp_registry:reg/4`、`imboy_mcp_tools` 鉴权范式、`mcp_governance_logic` 审批 |
| **T4.2** A2A 观察窗 | agent 回复经 provider | ✅ **强依赖**：一等 agent 参与者身份、`ai_agent_runtime`、`agent_trigger_policy`（@触发）、`agent_rate_limiter` | ✅ **强依赖**：`llm_stream` ephemeral 节流、`imboy_codec` JSON 自由 msg_type | `barrel_mcp_tasks` 状态机范式、`mcp_governance_logic` 审批范式 |
| **T4.3** Agent 支付 | — | ✅ **强依赖**：`ai_agent.owner_uid`；**+ 隐藏前置：群内 agent 触发尚未落地**（见 §4） | — | `mcp_client_grant` 授权范式（可参考） |

**关键交叉事实（研究实证）**：
- Phase 3 MCP 子系统与"插件"体系**完全脱钩**——`barrel_mcp_registry`/`imboy_mcp_tools` 直接挂顶层 `imboy_sup`（`imboy_sup.erl:196-223`），7 个 tool 硬编码于 `reg_all/0`，**不读任何 plugin manifest**。T4.1 要做的正是把这两套接起来。
- `ai_agent_runtime`（Phase 1）是 **presence-only 单例**（挂顶层 `imboy_sup`，非插件树），**不是**「每插件一个 agent worker」模型——T4.1/T4.2 不得直接复用它挂进 `imboy_plugin_generic_sup`。
- `ai_agent.owner_uid` 字段已建表，但**全仓无任何计费/权限代码消费它**——T4.3 的"记账到属主"是纯意图占位，需新写读取逻辑。

---

## 3. AGUI A/B 决策 / AGUI Path Decision

**倾向：A 路**（复用 AGUI/A2A 事件模型做 IM 渲染器输入）。理由（技术上站得住，研究已核实）：
1. 与 Phase 3「借 barrel 协议引擎、自写传输」策略**同一套打法**——复用标准事件模型，只做 IM 独有的多人渲染层。
2. Phase 2 已验证 `imboy_codec` **JSON 通道自由 `msg_type`**（不改 proto 即可接外部事件 schema→内部 msg_type），A 路天然适配"外部事件→群消息映射"。

**但必须 spike 验证再拍板**（AGUI 是**单人** agent↔user 协议，与多人群语义存在结构性不匹配，roadmap §3 已点破，未深读原文前不拍死）。

**Go/No-Go 清单（spike 前置，PoC 级不追生产化）**：

| # | 验证问题 | Go 判据 | No-Go → 走 B 路信号 |
|---|---------|---------|---------|
| 1 | AGUI `state_delta`/`run` 是否有天然多方广播语义？ | 事件可 fan-out 给群内多观察者，适配层薄 | 严格假设单订阅者，fan-out 适配层过重 |
| 2 | HITL 审批能否表达"群内任一有权成员皆可审批"？ | 可映射多人审批 | 硬编码"唯一发起者审批" |
| 3 | A2A task 事件与 AGUI 事件是否共享同一 JSON schema？ | 共享/薄转换 | 需厚转换层，复用收益被吃掉 |
| 4 | 原始事件哪些字段须原样透传（审计）、哪些可摘要渲染？ | 薄映射即可 | 须厚转换=A 路优势消失 |
| 5 | **YAGNI 检验**：若只需 task 4 终态+progress+发言文本，AGUI 全量 schema 是否过度设计？ | A 路多出的复杂度被"跨生态互操作"真实收益覆盖 | 几十行 map schema（对齐 `barrel_mcp_tasks` 61 行 `#task{}` 量级）足够 → 选 B |
| 6 | 是否有可 fork 的开源 AGUI SDK/renderer（GitHub 搜索优先）？ | 有现成 TS/Erlang 解析库 → A 路成本骤降 | 无 → 手写解析器成本≈B 路 → 选 B |

**决策规则**：spike 输出须包含"A 路多出的复杂度是否被真实互操作收益覆盖"的 YAGNI 结论。默认倾向 A，但 #3/#4/#6 任一强 No-Go 则回退 B（自建几十行事件契约）。

---

## 4. 分子任务立项 / Sub-Task Charters

> 工作量档：S（局部改动）/ M（跨 2–3 层）/ L（跨模块+迁移+前端）。标注 **[复用]/[新建]**。

### T4.1 — plugin manifest → Agent/MCP 能力目录

| 项 | 内容 |
|----|------|
| **目标** | 让插件 manifest 可声明 `mcp_tools`/`a2a_agent_card`，被外部 AI 发现和调用；打通"插件"与 Phase 3 MCP 注册（当前完全脱钩）。 |
| **复用点(文件级)** | `imboy_plugin_registry:normalize_manifest/1`（Defaults 合并模式，`src/lib/imboy_plugin_registry.erl:258-273`）；`barrel_mcp_registry:reg(tool,...)`（`imboy_mcp_tools.erl:114-117`）；`imboy_mcp_tools` 的 `with_caller/2` 鉴权红线；`app_manifest_handler` 的 build→ETag→200 模式（做发现端点）；`mcp_governance_logic` 审批三表（插件级 tool 治理）。 |
| **新建点** | manifest 加**可选字段** `mcp_tools=>[]`/`a2a_agent_card=>undefined`（`=>` 不进 `Required`）；插件 manifest→MCP tool 注册桥接（遍历 `manifests()` 逐条 `reg`，挂载时机对齐 `imboy_mcp_tools` wait_for_ready）；`/.well-known/agent.json` 发现端点 handler；`imboy-plugin-marketplace/index.json` schema 升版 + `validate_index.py` 校验。 |
| **spike (go/no-go)** | 无硬 spike。**决策项**：`mcp_tools` 加在**生产路线**（`imboy_plugin_registry` 硬编码 map）还是 **FROZEN 路线**（`imboy_plugin.erl` behaviour + `imboy_plugin_toml` 校验器）？→ **裁决：加生产路线**（FROZEN 路线的字段对 `app_manifest_handler`/MCP 注册不生效）。 |
| **MVP 边界** | 仅**官方 4 插件**可声明 `mcp_tools`；不开放第三方自由声明 `mfa`（越权/供应链风险，见风险#2）。marketplace index.json 升版为纯生态元数据快照，**不接入后端运行时**。 |
| **工作量** | Manifest 可选字段 **S** · MCP 桥接 **M** · `/.well-known/agent.json` **S–M** · marketplace schema **S** · generic_sup child spec 参数化（路径 A）**S–M**。综合 **M**。 |
| **风险** | ①**两套 manifest 概念混淆**（behaviour 契约 21 字段 vs 生产硬编码字段集不是同一 schema，照抄 `docs/plugin/contract.md` 会加到 FROZEN 路线不生效）；②第三方 `mcp_tools` 自由声明 `mfa`=越权面；③marketplace 升字段但后端零消费=两层皮死数据（须同批做发现端点）；④误挂 `ai_agent_runtime` 进 generic_sup 语义混乱。 |

### T4.2 — IM 作为 A2A 协作可观测前端

| 项 | 内容 |
|----|------|
| **目标** | agent 之间跑**标准 A2A**，人在群里实时观察 + 介入（@追问、`awaiting_approval` 点审批）。IM 只做"人类观察窗+介入控制台"，**不承载 A2A 协议本身**。 |
| **复用点(文件级)** | 一次 agent 发言→`message_ds:send_next/4`（QoS/ACK 全套，`ai_agent_reply.erl:159` 已这么用）；高频中间态→`llm_stream` ephemeral publish 节流（`src/lib/llm_stream.erl`，只终态落库）；群内 @追问→`msg_c2g_logic` mention + `agent_trigger_policy`（Phase 1 已交付真机验证）；JSON 自由 msg_type→`imboy_codec.erl:285`（加 `a2a_task_update`，不碰 proto）；审批状态机范式→`mcp_governance_logic` pending/approve/reject/审计双写；task 生命周期设计参考→`barrel_mcp_tasks.erl`（61 行 `#task{}`）。 |
| **新建点** | A2A task 事件订阅/桥接器（外部 A2A 流→imboy 内部事件，**零代码**）；task 状态→群消息映射/渲染层；群内**可点击审批卡片消息** UI（前后端都要）；**外部事件源写群消息的门控**（现有 `msg_c2g_logic` 入口全是"人类客户端发送"语义，无"系统/桥接进程代表 agent 写群"入口）；AGUI/A2A schema 解析层（A 路专属，取决于 §3 spike）。 |
| **spike (go/no-go)** | ✅ **须先做**（§3 全部 6 问）。另一 spike：cowboy/`imboy_syn:publish` 能否把"一份事件流 fan-out 给群内多观察者"——验适配层工作量。 |
| **MVP 边界** | 映射：`submitted/working/progress`→ephemeral publish；`completed/failed`→`send_next` 落一条正式群消息；一次 agent 发言→`send_next`（from=agent 账号）。**仅非 E2EE 群**（见 §7）。不追生产化。 |
| **工作量** | **L**（外部协议桥接 + 多人群渲染层 + 前端可视化控制台，即使复用覆盖了投递/权限/审批范式，新建仍是大头）。 |
| **风险** | ①无任何模块可不改代码直接做 A2A 桥接（Phase 3 提供"证明过的范式"非"可插拔组件"）；②审批场域不同（MCP 治理在管理后台页 vs T4.2 设想群内卡片）=新 UI/交互；③A/B 路选错导致返工（故 spike 先行）。 |

### T4.3 — Agent 支付探索（AP2/x402 + erlang_pay）

| 项 | 内容 |
|----|------|
| **目标** | 群里 agent 可发起**受控**支付（owner 预授权 + 单笔/周期上限）。AP2/x402 作**命名/概念参照**，不落地真实协议兼容（无真实交易对手方）。 |
| **复用点(文件级)** | 扣款原语→`wallet_ds:atomic_balance_change/4` + `transfer_logic:send/4`（imboy 唯一"用户对用户资金转移"范式，比 `payment_gateway` 更贴题）；幂等范式→`payment_transaction_ds`(gateway,no) / `wallet_ds:find_transaction_by_ref`(reference_no)；消费额度闸门骨架→`agent_rate_limiter` ETS 固定窗口双维（模块头已声明"只判频率不判金额，金额留给 billing/license 位"）；额度判断→`billing_logic:check_quota_with_delta/4` 范式；`ai_agent.owner_uid` 字段（表已建，**读取逻辑需新写**）。 |
| **新建点** | **群内 agent 触发链路**（`msg_c2g_logic` 零接入 `ai_agent`/`trigger_policy`，**T4.3 隐藏前置**）；Agent 工具调用/function-calling 框架（`ai_agent_reply` 纯文本进出，无"LLM 决定调用 action"调度层——当前最大架构缺口）；`agent_payment_mandate` 表（owner_uid/agent_uid/max_amount_fen/max_total_fen/expires_at/status）；`agent_payment_logic:pay_with_mandate/4`（前置闸门，内部调 `transfer_logic`，付款人恒为 `owner_uid`）。 |
| **spike (go/no-go)** | **前置 go/no-go**：群内 agent 触发是否本立项做？若不做，T4.3 退回**单聊场景**（复用已有 C2C 触发链路，不碰群聊缺口）。AP2 签名/x402 协议**明确不落地**（无对手方无法验证）。 |
| **MVP 边界** | 收窄到：单聊 + mandate 表（owner 显式预授权 + 单笔/周期上限）+ 复用 `transfer_logic` 扣款 + 复用 `agent_rate_limiter` 模式做额度闸门。**完全不碰 AP2 签名/x402 协议本身**。 |
| **工作量** | 群内触发接线 **M** · agent 工具调用框架 **L**（最大缺口）· mandate 表+校验 **M** · AP2/x402 协议本身 **L 且不做**。综合 **L**（且依赖链最长）。 |
| **风险** | ①依赖链空中楼阁——"群内 agent 能否被触发"这个更基础能力都未做，直跳"自主发起支付"不现实；②**强制安全审查**（认证/支付/金融代码变更前必走 security-reviewer，PoC 也不跳输入校验：单笔/周期上限、mandate 过期、owner_uid 归属）；③`owner_uid` 计费归属当前不成立（无消费者，需新写）。 |

**三道支付闸门串联（不可合并）**：消息到达→`agent_rate_limiter:allow/2`（挡频率，已存在）→ `agent_payment_mandate` 额度校验（挡超额，待建）→ `transfer_logic:send/4` 真扣款。

---

## 5. 落地顺序与里程碑 / Sequencing & Milestones

**依赖关系**：
- T4.1 相对独立（生产路线内扩展，不碰 FROZEN），**风险最低、可最先落**。
- T4.2 依赖 §3 AGUI spike 结论，且是 M3 方向验证的**核心**。
- T4.3 依赖链最长（隐藏前置：群内 agent 触发 + function-calling 框架），**最前瞻、建议最后**。

**建议顺序**：

| 阶段 | 动作 | 产出 |
|------|------|------|
| **S0（先做 spike）** | §3 AGUI A/B go/no-go spike（含 GitHub 搜索 fork 候选）；T4.3 群内触发 go/no-go 决策 | A/B 路裁决 + T4.3 范围收窄结论 |
| **S1** | T4.1（能力目录，M 档，风险最低，先积累"插件→MCP 注册"通路） | manifest 可选字段 + 发现端点 + marketplace schema |
| **S2** | T4.2（依 S0 结论选 A/B，L 档，M3 核心 PoC） | A2A 观察窗群消息渲染 + 群内审批卡片 PoC |
| **S3（可选/最后）** | T4.3（单聊收窄版 PoC，L 档，最前瞻） | mandate 表 + 受控支付 PoC |

**先做哪个 spike**：**AGUI A/B spike 最优先**（S0）——它决定 T4.2 整个技术路线，且 T4.2 是 M3 方向验证的核心；不先定 A/B 会导致 T4.2 大面积返工。

---

## 6. 非目标 / Non-Goals（YAGNI，延续 §5）

Phase 4 **明确不做**：
- ❌ **不生产化**——M3 定性为前瞻 PoC，验证"IM-as-agent-bus"方向即止。
- ❌ **不解冻 v2 动态插件平台**（`imboy_plugin_toml/loader/manager/lifecycle/signature/dependency` 6 个 FROZEN 模块）——解冻是架构级 ADR 动作，远超单个 T4.1 范围。T4.1 只在生产路线内扩展。
- ❌ **不开放第三方插件自由声明 `mcp_tools` 的 `mfa`**——越权/供应链风险，仅官方 4 插件。
- ❌ **不落地 AP2 签名协议 / x402 稳定币结算**——无真实商户/交易对手方，接了也无法验证；作命名参照。
- ❌ **不自建 TEE 机密计算 / 不自训模型**（延续 §5）。
- ❌ **不在 E2EE 会话做服务端 AI**（红线，见 §7）。
- ❌ **不建复杂 agent 权限体系**——mandate 表几个字段起步。
- ❌ **不改 proto**——A2A/task 事件走 JSON 自由 msg_type。

---

## 7. E2EE 红线约束 / E2EE Guardrails（§4 对 Phase 4 逐任务）

| 任务 | E2EE 边界 |
|------|-----------|
| **T4.1** | 能力目录/发现端点面向"外部 AI 发现能力"，不触碰任何会话明文，天然合规。 |
| **T4.2** | ⚠️**核心约束**：A2A 协作观察群**必须是非 E2EE 群**。"服务端渲染 task 事件成群消息"本质需服务端读懂内容，与"服务端不接触明文"直接冲突。桥接器**必须复用 `ai_agent_reply.erl:34` 的 `is_e2ee/1` 判定**，在**设计阶段**就把"E2EE 群禁止接入 A2A 桥接"写进门控，而非事后补丁。 |
| **T4.3** | agent 支付走 `wallet_ds`/`transfer_logic`（资金流水本就服务端记账，非 E2EE 内容），红线不受影响；但触发 agent 支付的**消息若在 E2EE 群则不触发**（同 `is_e2ee` 守卫）。 |

**通用**：AI 能力默认关闭、opt-in、agent 作为对话显式一端提示用户（对齐 WhatsApp/Signal 立场，把 Signal 的批评当产品准则=私有化卖点）。

---

## 8. 风险与开放问题 / Open Questions（需决策）

**需用户/后续拍板**：
1. **AGUI A vs B 路**——须先跑 §3 spike（尤其 #3/#4/#6），spike 前不下最终结论。**建议 S0 优先执行**。
2. **T4.3 是否含"群内 agent 触发"**——若含则背上 function-calling 框架（L 档最大缺口）；若不含则 T4.3 收窄到单聊 PoC。**倾向收窄单聊**（YAGNI）。
3. **T4.1 谁能声明 `mcp_tools`**——官方 4 插件 vs 真第三方市场插件。**倾向仅官方**（越权/供应链）。
4. **marketplace 仓是否重启"拉取"路线**——当前 `imboy_plugin_loader` 只读本地 `priv/plugins/`，无任何 HTTP 拉取 marketplace 代码。升 index.json 字段本身是纯生态工作，不重启拉取则后端零感知。**倾向不重启**（保持 index.json 为人类浏览快照，真实发现端点落后端 `/.well-known/agent.json`）。
5. **Phase 4 是否在本轮立项即启动**——Phase 0–3 全部**未 push**、Phase 2 流式真机 E2E 未验。是否先补齐 push + Phase 2 真机验证再进 Phase 4？**建议先稳固 M1/M2 交付基线再启 M3**。

**技术未决（需 spike，诚实标注）**：
- `imboy_plugin_loader` 的 gen_server 是否真被 `imboy_sup` 挂载——研究未在本次范围核实其 child spec 接线状态（T4.1 若走路径 B 才相关，路径 A 无关）。
- AGUI 是否有可 fork 的开源 renderer（GitHub 搜索待做，直接影响 A 路成本）。
- fan-out 适配层（一份事件流→群内多观察者）的真实工作量——须 T4.2 spike 量化。

---

**关键文件索引**（均在 worktree `/Users/leeyi/project/imboy.pub/imboy/.claude/worktrees/ai-agent-phase1/` 下，权威计划在主仓 `imboy/docs/planning/ai-agent-platform-roadmap.md`）：

- 权威计划：`/Users/leeyi/project/imboy.pub/imboy/docs/planning/ai-agent-platform-roadmap.md`（§3/§4/§5）
- T4.1：`src/lib/imboy_plugin_registry.erl`、`src/lib/imboy_plugin.erl`、`include/imboy_plugin.hrl`、`src/imboy_plugin_generic_sup.erl`、`src/imboy_plugin_sup.erl`、`src/api/app_manifest_handler.erl`、`src/mcp/imboy_mcp_tools.erl`、`src/mcp/barrel_mcp_registry.erl`、`src/logic/mcp_governance_logic.erl`；独立仓 `imboy-plugin-marketplace/index.json` + `scripts/validate_index.py`
- T4.2：`src/mcp/barrel_mcp_tasks.erl`、`src/mcp/barrel_mcp_session.erl`、`src/ds/message_ds.erl`、`src/lib/llm_stream.erl`、`src/logic/msg_c2g_logic.erl`、`src/logic/mention_logic.erl`、`src/lib/agent_trigger_policy.erl`、`src/logic/ai_agent_reply.erl`(`is_e2ee/1`)、`src/lib/imboy_codec.erl:285`、`src/adm/adm_mcp_handler.erl`
- T4.3：`src/ds/wallet_ds.erl`、`src/logic/transfer_logic.erl`、`src/logic/red_packet_logic.erl`、`src/logic/billing_logic.erl`、`src/logic/payment_gateway.erl`、`src/ds/payment_transaction_ds.erl`、`src/lib/agent_rate_limiter.erl`、`src/ds/ai_agent_ds.erl`、`src/repo/ai_agent_repo.erl`；独立仓 `/Users/leeyi/project/imboy.pub/erlang_pay`
