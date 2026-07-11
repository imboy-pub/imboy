# Phase 4 Spike Go/No-Go 决策文档 — AGUI A/B + fan-out 可行性

> **文档类型**：架构决策文档（ADR 集合）｜**日期**：2026-07-10｜**作者**：架构评审
> **状态**：待用户拍板（见 §5）
> **决策依据**：SPIKE 1（AGUI A/B 路线）+ SPIKE 2（fan-out 可行性），均基于本 worktree `ai-agent-phase1` 代码实测。

---

## 0. 前置事实与诚实标注

**关键事实：charter 原文在本 worktree 不存在。**
- 目标路径 `docs/planning/phase4-agent-mcp-marketplace-charter.md` 及 `docs/planning/` 目录均不存在；`docs/plans/` 仅有 `2026-04-11-tsid-audit` 与 `2026-07-06-priority-batches` 两份，无 charter。
- 两个 spike 独立复核，均确认 charter 缺失。故 **charter §3 的 6 条 go/no-go 判据的原文阈值、T4.2 的原始验收标准文本，本决策文档无法逐字引用，属"待验"**。
- 本文的"6 个 go/no-go"以 **SPIKE 1 的 Q1–Q6 六问**为代理判据（它们即是 A/B 路线的六个决策探针），逐条给结论。若后续找回 charter 原文，须用其 §3 原始阈值回填校准本裁决。

**web 研究"待验"清单（诚实标注，未在代码/一手文档二次核实的部分）**：
- AGUI 无正式"2026 版本号"，为滚动 17 事件规范；A2A v1.0 为真实稳定版（2026-03 发布，Linux Foundation 治理）——**version 事实可信，但"17 事件类型"具体枚举未在本轮逐一核对**。
- BEAM 侧 SDK 星数/活跃度（`a2a-elixir` 18★、`a2a_ex` 8★、`ag_ui_ex` v0.1.0）来自 web 检索快照，**star 数与"最近 push"为检索时点值，未在 GitHub 二次核实**。
- "AGUI 官方立场：Session management is your problem"、"HITL is just conversation"等为社区/文档引述，**语义方向可信，逐字措辞待验**。

**已在代码侧证实的锚点（非待验）**：
- `src/mcp/barrel_mcp_tasks.erl` 已实现一套 **MCP 任务状态机**（`working|completed|failed|cancelled`，MCP 2025-11-25 词汇），含 `notifications/tasks/status` 经 SSE 推送、ETS + TTL 清理。这是**已落地资产**，非规划。
- `src/logic/ai_agent_reply.erl` + `src/lib/llm_stream.erl` 已实现 Phase 1/2 的 **C2C agent 流式回复**（系统旁路 `deliver_reply` 直调 `message_ds:send_next`）。
- fan-out 所需原语（`imboy_syn:publish/2,3`、`group_ds:member_uids/1`、`user_logic:is_online/1`、`msg_c2g_logic` 的 N×单播循环）均已生产验证。

---

## 1. AGUI A/B 最终裁决

### 裁决：**B 路（自建契约）+ 轻混合**
> 自建几十行事件契约作为机制主体；**仅借用 A2A v1.0 的 task-state 词汇表作为 `state` 枚举命名约定**，不引入 A2A/AGUI 的传输层、SDK、Agent Card、JSON-Patch state 同步。
> 落地时进一步优先对齐**本仓已存在**的 `barrel_mcp_tasks` MCP 词汇（`working/completed/failed/cancelled`），避免再引入第三套状态名。

### 逐条 Go/No-Go（对应 SPIKE 1 Q1–Q6 六探针）

| # | 探针（go/no-go 判据） | 结论 | 依据 |
|---|---|---|---|
| G1 | AGUI 事件是否有天然多方广播语义，可省去 imboy 自建 fan-out？ | **No-Go（走 A 无收益）** | AGUI 是单 SSE 流 / 单 `threadId`/`runId` 模型，`StateSnapshot/StateDelta`（RFC6902）假设**一个** UI 持权威 state。多方广播完全在协议外，A/B 都得 imboy 自建 → A 路省不下这块。 |
| G2 | AGUI HITL 能否表达"群内任一有权成员皆可审批"？ | **No-Go（走 A 无收益）** | AGUI 无审批一等原语，HITL = 单 client 回传单 `TOOL_CALL_RESULT`，结构假设**唯一决策者**。"多授权人任一可批 + 去重仲裁"是 imboy 核心差异化，协议层零覆盖，必自建。 |
| G3 | A2A task 事件与 AGUI 事件是否共享同一 schema（A 路可低成本统一解析）？ | **No-Go（走 A 需厚转换）** | 两者异层异 wire format（A2A=JSON-RPC 2.0；AGUI=SSE 自描述信封）。官方定位"互补非同构"。同时吃两协议需两套解析 + 一层归一化 = 厚转换。 |
| G4 | 是否存在可 fork 的对口成熟 renderer/SDK（显著降低 A 路成本）？ | **No-Go（无对口资产）** | TS 生态成熟（CopilotKit 35.9k★）但与 imboy（Erlang 后端 + Flutter 端）技术栈错位、单用户向；BEAM 侧仅 Elixir 且 8–18★、pre-1.0，不满足私有化 IM 稳定性门槛。无 Erlang 原生成熟 SDK。（星数待验） |
| G5 | A2A v1.0 task 状态机映射"一条群消息=一次状态更新"是否自然？ | **Go（词汇值得借）** | `TaskStatusUpdateEvent` 本身即一次状态跃迁 + 可携中间 message；`working→进度气泡 / input-required→审批气泡 / 终态→定稿气泡`语义干净。**唯一有实质价值的 A 路要素**——但只借词汇表，不借传输。 |
| G6 | 采纳全量 AGUI/A2A schema 是否通过 YAGNI 检验？ | **No-Go（过度设计）** | imboy 实需仅"4 终态 + progress + 发言文本"。AGUI 17 事件 / A2A 9 状态 + Agent Card + Artifact + push 绝大部分无关。自建对标 `barrel_mcp_tasks` 记录仅十余行即够。全量 schema = 为 3 个需求扛整套协议表面积。 |

**综合**：6 条中 5 条判 No-Go（走 A 无收益或高成本），仅 G5 判 Go 且**仅限借词汇**。→ **B 路 + 轻混合**成立。

**反向重估 A 的触发信号（当前均未出现，出现任一须重启 A 评估）**：
1. imboy 需与**外部第三方/跨厂 agent** 互操作（此时 A2A 的 Agent Card/JSON-RPC 发现才真正值钱）；
2. 出现 **Erlang 原生（非 Elixir）、真实采纳量**的 AGUI/A2A SDK；
3. 渲染需求膨胀到 generative UI / 流式 tool-call 可视化，超出"文本 + 进度 + 状态"。

**退回纯 B（连词汇也不借）的信号**：A2A 状态枚举（如 `auth-required`/`unknown`）在群场景造成映射噪音；或"借词汇"让团队误判"已兼容 A2A"产生错误互操作预期。

---

## 2. fan-out 可行性结论

### 结论：**适配层工作量档 = S–M（约 60–100 行新代码），不构成 T4.2 阻断项。**

### 复用点（零改动，已生产验证）
| 复用资产 | 位置 | 作用 |
|---|---|---|
| `imboy_syn:publish/2,3` | `src/lib/imboy_syn.erl:141` | 单 uid 多设备扇出原语，group fan-out 只需外层套循环 |
| `group_ds:member_uids/1` + `user_logic:is_online/1` | — | 取在线成员列表（`msg_c2g_logic.erl:322-329` 已验证 pattern） |
| `message_ds:send_next/4,6` | `src/ds/message_ds.erl:74` | 观察流若需可靠 QoS 投递而非纯 ephemeral |
| `msg_c2g_logic` 的"MemberUids 循环" | `msg_c2g_logic.erl:322-329` | "群广播 = N×单播"pattern 可原样照抄 |
| C2C 系统旁路模式 | `ai_agent_reply.erl:135-160` `deliver_reply` | 系统/agent 写消息跳过 Logic 权限门、直落 DS 的既定模式 |

### 新建点（小范围、模式已知）
| 新建项 | 规模 | 说明 |
|---|---|---|
| `llm_stream.erl` 的 `Ctx`/`publish/3` 改造 | ~20–30 行 | `target_uid :: integer()` → `ObserverUids :: [integer()]` + 循环 publish；帧 `to` 字段单 uid → 群 Gid 语义。**节流/seq/reset 状态逻辑不动**（按 `stream_id` 隔离，与目标数无关） |
| 新"agent 群消息 fan-out"模块 | ~30–50 行 | 仿 `ai_agent_reply` C2C 旁路：`group_ds:member_uids/1` 取成员 → 逐个 `imboy_syn:publish`（ephemeral）或 `message_ds:send_next`（可靠）。因 `msg_c2g_logic:c2g/3` 是人类客户端语义、`do_stage_and_send_c2g/13` 未导出，无法直接复用 |

### 明确不建议碰（成本/风险不对称）
- **不要激活 `?ROOM_SCOPE` 做 syn 原生组播**。它是"注册但零使用"的空壳 scope（全仓仅 `imboy_syn.erl` 一处命中）。启用需新增 WS connect/disconnect 的 `syn:join/leave` 生命周期管理（新状态机、新故障模式：断线未 leave、多设备重复 join）。现有"N×单播"循环对群观察者规模（几人到几十人）已够。真到上千观察者高频广播出现瓶颈时，再作独立优化项。

### 是否阻断 T4.2
**否。** fan-out 的两个命脉需求（多方广播、多授权人审批）虽 AGUI/A2A 协议层不提供，但 imboy 侧全部有已验证原语可组装；适配层 S–M 档、模式已知、无需碰 syn 底层或 WS 连接生命周期。**不构成 T4.2 阻断项。**

---

## 3. T4.2 是否 ready 实施

### 结论：**T4.2 从"需 spike"转为 ready 实施（附 2 项非阻断的已知边界）。**

spike 已消解 T4.2 的两大未知：
1. **路线选型**（原未知：A 复用 vs B 自建）→ 已定 **B 路 + 轻混合借 A2A 词汇**，且优先对齐本仓已存在的 `barrel_mcp_tasks` MCP 词汇。
2. **fan-out 技术可行性**（原未知：syn 能否支撑多观察者广播）→ 已定 **S–M 档、复用现有原语**，非阻断。

### 仍存在的已知边界（非阻断，落地时决策即可，不需再开 spike）
1. **可靠性档位未定**：观察流走 **ephemeral（`imboy_syn:publish`，不落库、断线丢失）** 还是 **可靠 QoS（`message_ds:send_next`，落库可补拉）**。二者原语都现成，属产品语义选择而非技术未知。建议默认 ephemeral（观察流本质是实时旁观），审批类关键事件走可靠通路。
2. **多授权人审批的去重仲裁语义**：AGUI/A2A 都不给，须 imboy 自建（"群内任一有权成员抢先批准 → 其余请求作废/幂等"）。这是**产品 + 权限设计**任务，非 spike 未知；可复用 `barrel_mcp_tasks` 的 `transition` 幂等终态模式（已终态则忽略后续，天然抗重复批准）。

> **charter 回填提醒**：若找回 charter 原文，须用其 T4.2 原始验收标准复核上述"ready"判定——本判定基于 spike 探针，非 charter 原文验收项。

---

## 4. Phase 4 推荐首个可实施任务

### 推荐起点：**T4.1（无硬 spike、reuse 最重的任务），而非 T4.2 的 fan-out。**

> 注：T4.1 具体内容因 charter 缺失无法逐字引用，此处依 charter 背景（"T4.1 无硬 spike、reuse 最重"）+ 本仓已落地资产给出落地建议。

**理由（KISS / YAGNI）**：
1. **最短路径到"能跑"**：本仓已有 `barrel_mcp_tasks`（MCP 任务状态机 + SSE 通知）、`ai_agent_reply`/`llm_stream`（C2C agent 流式回复）两块落地资产。T4.1 若是"在既有 MCP/agent 基础设施上做单人/单群的确定性能力接线"，其复用比最高、无新协议、无 fan-out 多主体仲裁风险——**先摘熟果**。
2. **fan-out 先不做多主体**（YAGNI）：T4.2 的多观察者广播 + 多授权人审批是**多主体**复杂度。在没有真实"多人围观多 agent"用户需求验证前，先用 T4.1 把**单主体链路**跑通、真机验收，再按需扩到 fan-out。避免为设想中的多人协作预建仲裁层。
3. **fan-out 已证明是 S–M、随时可接**：正因 §2 证明 fan-out 不阻断、改动局限在 `llm_stream` + 一个新模块，**推迟它零风险**——需要时再从 T4.1 的单播链路直接扩循环即可，不必现在就扛多主体复杂度。

**落地建议动作（首个任务的最小切片）**：
- 复用 `ai_agent_reply` + `llm_stream` 的 C2C 单播链路作为 agent 能力接入骨架；
- 若 T4.1 涉及任务态展示，直接复用 `barrel_mcp_tasks` 的 `working/completed/failed/cancelled` 词汇 + `notifications/tasks/status`，**不新造状态机**；
- fan-out（`Ctx.target_uid → ObserverUids`）留作 T4.2 的独立后续切片，代码位置已锁定（`llm_stream.erl` publish 循环 + 新 fan-out 模块）。

---

## 5. 仍需用户拍板的点

| # | 决策点 | 为何须用户拍板 |
|---|---|---|
| D1 | **Phase 4 是否现在启动**（charter 已列"先稳 M1/M2 再启 Phase 4"）：**push + Phase 2 真机 E2E 是 user-gated** | 据项目记忆，Phase 2 流式回复的后端 2 + 前端 2 commit **未 push、真机待验**。Phase 4 建立在 Phase 1/2 之上，未 push/未真机验收前启动 Phase 4 有返工风险。是否解除该 gate 须用户明确。 |
| D2 | **charter 缺失如何处理** | 本决策基于 spike 探针，非 charter 原文 §3/T4.2 验收项。是否需先补齐/找回 charter 再据其原始阈值复核本裁决，须用户定。 |
| D3 | **观察流可靠性档位**（ephemeral vs 可靠 QoS，§3 边界 1） | 产品语义选择（实时旁观 vs 可补拉审计），影响是否落库、是否占存储，须产品拍板。 |
| D4 | **"轻混合借 A2A 词汇" vs "纯 B 用本仓 MCP 词汇"** | 本仓 `barrel_mcp_tasks` 已用 MCP 词汇（`cancelled`），A2A 用 `canceled`+更多态（`input-required`/`rejected`/`auth-required`）。借哪套作为对外互操作命名基准，涉及未来对外集成方向，须用户/产品定。默认建议：对内统一用已落地的 MCP 词汇，A2A 词汇仅在确有跨厂互操作需求时再引入。 |
| D5 | **Phase 4 首个任务确认为 T4.1** | 本文推荐 T4.1 优先、T4.2 fan-out 推迟；因 charter 缺失无法逐字核对 T4.1 范围，最终任务排序须用户确认。 |

---

## 附：本文引用的代码锚点（均为绝对路径）
- `/Users/leeyi/project/imboy.pub/imboy/.claude/worktrees/ai-agent-phase1/src/mcp/barrel_mcp_tasks.erl`（MCP 任务状态机，已落地）
- `/Users/leeyi/project/imboy.pub/imboy/.claude/worktrees/ai-agent-phase1/src/logic/ai_agent_reply.erl`（C2C 系统旁路 `deliver_reply`）
- `/Users/leeyi/project/imboy.pub/imboy/.claude/worktrees/ai-agent-phase1/src/lib/llm_stream.erl`（流式 publish，fan-out 改造点）
- `/Users/leeyi/project/imboy.pub/imboy/.claude/worktrees/ai-agent-phase1/src/lib/imboy_syn.erl`（`publish/2,3` 单 uid 扇出；`?ROOM_SCOPE` 空壳）
- `/Users/leeyi/project/imboy.pub/imboy/.claude/worktrees/ai-agent-phase1/src/logic/msg_c2g_logic.erl`（N×单播群循环 pattern；`c2g/3` 人类客户端语义）
- `/Users/leeyi/project/imboy.pub/imboy/.claude/worktrees/ai-agent-phase1/src/ds/message_ds.erl`（`send_next` 可靠投递原语）

> **charter 未找到**：`docs/planning/phase4-agent-mcp-marketplace-charter.md` 在本 worktree 不存在，本文 §1 六判据、§3 ready 判定均以 spike 探针为代理，须以 charter 原文回填校准。
