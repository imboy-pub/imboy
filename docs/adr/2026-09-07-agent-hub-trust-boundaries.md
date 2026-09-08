# ADR 2026-09-07 — Agent Hub 信任边界

- 状态：Accepted（PDT-01 冻结）
- 关联契约：task API / webhook / MCP client 三契约（docs/api-contracts/agent_hub_*.md）、
  FSM-00 状态机、TRACE-00 correlation 契约
- 实现归属：DATA-01、MCP-01/02、WH-01/02、BOT-01

## 背景与决策

Agent Hub 在 IMBoy 内引入五类平台 actor（human、ai_agent、mcp_client、
developer_bot、system_bot，冻结于 task API 契约 §2）。agent 能力本质是"以受限
身份调用工具并产生外部副作用"，必须先于实现固定信任边界，防止实现期以便利为
名逐个击破。本 ADR 冻结五条边界：E2EE、SSRF/IP pin、身份代理、MCP 滥用、审计
红线。

## 边界一：E2EE（fail-closed）

1. Agent Hub 全链路**不处理 E2EE 消息正文**：不解密、不存密文副本、不把正文传
   给任何工具/LLM/出站 webhook。agent 可见的只有元数据（发送者 ID、时间、
   message_id 引用）。
2. HITL 审批卡片经**平台消息通道**投递到群（服务端生成的卡片消息，非用户端到
   端密文），卡片内容只含任务元数据与结果摘要（digest），不含工具参数明文。
3. 工具执行结果回传链路按"最小披露"：审计链只允许 result_digest（TRACE-00 §7）。
4. fail-closed：无法判定某数据是否属正文级敏感（含密文/明文/URL 全文）时，按
   敏感处理（不落链、不外发）。

## 边界二：SSRF / IP pin（出站 webhook）

冻结于 webhook 契约 §2.5，此处重申决策理由与不可妥协项：

- 出站只允许 HTTPS；目标 IP 在投递创建时解析一次并 **pin 到 outbox**，重试只
  连 pinned IP（防 DNS rebinding 把可信域名重解析到内网）。
- 私网/loopback/link-local/保留段在创建时拒绝（fail-fast，不是投递期失败）。
- V1 **不跟随 redirect**；未来启用必须逐跳重新校验（同域后缀、重新 pin、重签、
  ≤1 跳）。
- 元数据服务（如 169.254.169.254）类目标属于保留段，天然被拒。

## 边界三：身份代理（agent 不得冒充人）

1. agent（ai_agent/mcp_client/developer_bot）执行时的审计身份永远是 agent 自身，
   **永不映射到用户身份**；MCP credential 不持 JWT、不复用 JWT 中间件
   （mcp_client 契约 §3）。
2. 审批人身份只能由服务端已认证会话（JWT current_uid）派生；请求体透传的
   actor 字段一律忽略。防自我审批：审批人不得是任务所属 agent（task API §6）。
3. `resume`/`expire` 属平台 system 动作，不暴露给任何外部调用方；`system_bot`
   只投递平台通知，不具备状态机 system 动作权限（task API §2 映射表）。
4. agent 对用户数据的可见性走既有授权模型（群成员/频道权限服务端权威判定），
   不因 agent 身份放宽；agent 请求按最小授权：grant 白名单（MCP）/工具注册
   （内建）。

## 边界四：MCP 滥用（per-client 隔离）

冻结于 mcp_client 契约 §4：速率/并发/会话/payload 四限按 credential 独立计数，
超限 `429` 可观测（服务端度量含 client_id），且**不影响其他 client 与用户流量**
（禁止全局熔断、禁止连带限流）。grant 白名单外的工具在 `tools/list` 中不可见
（不泄露存在性），调用一律拒绝。

## 边界五：审计红线

TRACE-00 §7 为唯一真源，重申不可妥协项：

- 链数据只允许 7 字段白名单（correlation_id/entity_*/parent/timestamp/status
  [+client_override 违规标记]）。
- 禁止进入链与审计日志：secret/token/API key（含 MCP 摘要全文）、消息正文（密
  文与明文）、完整 URL、PII（账号、手机号、邮箱、明文 IP）。
- 工具参数/结果只允许 SHA-256 digest 落审计。
- `client_override: true` 出现即机器违规（verifier exit 2）。

## 后果

- 实现任务（DATA-01/MCP-01/02/WH-01/02/BOT-01）的验收包含边界负例；违反本
  ADR 的实现按缺陷退回。
- 已知取舍：V1 不跟随 redirect、financial 工具不启用、sampling/elicit 不启用
  ——均为收窄攻击面的刻意保守，启用需契约升版 + Coordinator 合并。
- E2EE 边界意味着 agent 无法"读懂"加密会话内容；产品侧不得宣传 agent 具备
  E2EE 会话理解能力。

## 违规处置

任何实现发现与本 ADR 冲突：REV-01 复审按 HIGH 阻断清零处理；不可就地放宽。
