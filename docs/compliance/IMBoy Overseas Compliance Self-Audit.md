# IMBoy Overseas Compliance Self-Audit

> 审计日期：2026-09-03（CST）
> 审计类型：只读代码级自查，不是法律意见，也不构成“合规认证”
> 快照：backend `df01eaea9b4ef61c4f5106735943b8cfb654da1e`，Flutter `0f7368a9f2e3630b87bd5b9dba9b952d9c779de2`，Admin `8eb6fcb773b80862ae390663b3f31d6d2c42452f`
> 工作树：三仓均有既有未提交改动。本报告按当前工作树取证；结论必须在发布候选 HEAD 重验。

## 1. Executive Summary

结论：**NO-GO**。IMBoy 的 Workspace / Project / Group / Channel / 好友驱动 C2C 产品定位本身适合海外普通协作型 IM；代码也已有认证、设备管理、服务端 C2C 屏蔽、举报工单、管理端 RBAC、E2EE、限流、账号注销入口和有限数据导出。但当前仍缺少能通过商店审核并可运营的最小闭环。

本轮确认的发布阻断主要不是“完全没有功能”，而是已有能力之间未闭合：

1. `P0` 账号自动删除查询使用不存在的 `user.updated_at`；清理默认关闭，且删除集合未覆盖消息、附件、频道/工作区、支付、举报等数据。
2. `P1` 消息举报被降级成“举报发送者 + description 中塞 message_id”，缺少消息对象、证据快照和稳定定位。
3. `P1` 举报处理只改变工单状态；没有 warning/removal/mute/ban 等处罚动作，也没有申诉闭环。
4. `P1` `review_queue` 和敏感词管理存在，但消息写入链未调用它们，审核队列不会由生产消息生成。
5. `P1` Block 只在部分服务端路径生效；Group、Channel、mention、invite、profile/search 缺统一决策和测试矩阵。
6. `P1` 管理员可在 `audit_mode=full` 下读取/导出消息 payload，但读、详情和导出本身未见逐次管理员操作审计；`messages:read` 同时授权导出。
7. `P1` 隐私声明、第三方披露、数据流/处理者清单和商店 Data Safety / Privacy Nutrition Label 尚无可核验的一致性证据。
8. `P1` birthday 是可自由修改的字符串资料字段，不能支持可信年龄分层；澳大利亚服务分类和 2026 年生效规则需要法律确认。
9. `P1` 没有统一 retention policy；消息归档可永久开启，Loki 实际配置 180 天且注释写 30 天，删除与备份到期未形成可证明链路。
10. `P1` 现有第三方 SDK/服务的数据类别、目的、地域、可选性、DPA 和删除传播均未登记。

已验证代码能力不等于生产能力；本轮没有连接生产数据库、对象存储、推送商户、支付商户、Sentry 项目、应用商店后台或真实审核队列。

## 2. Current Product Boundary

确认的核心模型是 User -> C2C/Friend -> Group -> Channel -> Workspace/Project。存在朋友圈、附近的人、群音视频、频道发现/付费频道、钱包/支付、AI Agent/Bot，但没有证据表明产品是 Dating、Random Chat、Anonymous Chat、Adult Social 或 Gambling。

产品分类风险：公开频道发现、朋友圈、附近的人、AI Agent 和直播目录会扩大 UGC/社交审核面。它们若不在海外首发范围，最小方案是通过现有 product profile/feature gate 明确禁用并用构建/运行证据验证，而不是重写核心模型。

## 3. Compliance Capability Inventory

| Domain | Current | Code evidence | Closure assessment |
|---|---|---|---|
| Account identity | Partial | `priv/migrations/00000001_foundation.up.sql:2459`; `src/logic/passport_logic.erl` | TSID、账号、手机/邮箱、注册 IP、设备、状态存在；非实名，身份可追溯到账号/设备/IP，不等于现实身份 |
| Sessions/devices | Implemented, unverified | `src/api/user_device_handler.erl`; `src/logic/user_device_logic.erl` | 列表、踢设备、刷新 token 设备绑定存在；未做真实多设备验证 |
| Age/minor | Missing | `src/domain/user_agg.erl:22`; `imboyapp/lib/page/personal_info/profile/profile_page.dart:549` | birthday 原样写入且可修改；无 age band、可信度、区域年龄策略 |
| Report | Partial | `src/api/report_handler.erl:33`; `src/logic/report_logic.erl:13` | user/group/channel/moment 工单可建；消息/文件/头像/Workspace 不是真正对象 |
| Block | Partial | `src/logic/msg_c2c_logic.erl:53`; `src/ds/moment_ds.erl:435` | C2C/好友/通话/朋友圈有服务端门；其他互动路径无统一覆盖 |
| Moderation | Scaffold only | `priv/migrations/00000022_content_moderation.up.sql:4`; `src/logic/adm_moderation_logic.erl` | 词表和队列 CRUD 存在；消息链没有 enqueue，reject 不执行内容处置 |
| Enforcement | Partial | `src/logic/msg_rate_logic.erl`; group mute logic; admin user status | 限流/群禁言/账号状态有散点能力；未与 report/case 连接 |
| Appeal | Missing | 全仓业务代码搜索仅测试文案命中 | 无 action -> appeal -> review -> final decision |
| Account delete | Broken/partial | `src/logic/user_deletion_logic.erl:173`; `src/ds/user_ds.erl:245` | UI/API 有申请；后台清理查询不可执行且清理范围不完整 |
| Data export | Partial | `src/logic/user_export_logic.erl:26`; `src/ds/user_ds.erl:433` | 仅资料/好友/群/设置；消息、附件、频道、工作区、支付等缺失 |
| Retention | Fragmented | `config/sys.config.example:133`; `deploy/loki/loki.yml:50`; backup scripts | 有个别 TTL/清理，但无统一数据类别政策及删除证明 |
| Admin RBAC | Partial | `src/adm/adm_index_handler.erl:192`; `src/adm/adm_moderation_handler.erl:149` | 权限守卫存在；角色语义未覆盖 Security/Support/Moderator 职责分离 |
| E2EE | Implemented, conflict documented | `src/lib/imboy_policy.erl:151`; `src/logic/msg_c2g_logic.erl:252` | 服务端密文门和 audit mode 存在；举报证据与密文治理冲突未产品化 |
| Payments | Present | migrations 10-12; payment/billing/channel-order handlers | 支付宝/微信/Stripe、钱包、付费频道存在；海外商店计费路径需单独审核 |
| AI | Present | `src/logic/ai_agent_reply.erl`; AI Agent/Admin modules | Agent/Bot 账号类型已区分；用户可见披露、生成内容举报与供应商数据流未闭环 |
| Tests | Partial | report/block/deletion/export/E2EE/rate-limit tests | 多为单元/静态；缺跨路径、真实 DB、双账号和商店验收证据 |

## 4. Account & Identity

### Facts

- `user` 存储 TSID、account、mobile、email、password hash、region、birthday、reg_ip、OS/version、source 和状态（`priv/migrations/00000001_foundation.up.sql:2459-2482`）。
- 登录包含密码、验证码、quick login、支付宝和 OIDC 代码路径；token 支持 DID 绑定，设备可查看/踢出。
- `status=-1/0/1/2` 表达删除、禁用、启用、申请注销。
- 未发现匿名访客账号；Workspace 的 `guest` 是授权角色，不是匿名账户。

### Assessment

“身份可追溯”达到账号 ID + 登录标识 + 注册 IP + 设备记录 + 部分登录日志层级。没有现实身份/KYC，当前产品边界也不需要默认增加 KYC。多账号、设备关联强度、risk account 标记和恢复欺诈规则没有统一策略。

## 5. Age & Minor Safety

当前收集可选 birthday，但其类型是 `varchar(20)`，domain 明确资料字段原样落库，Flutter 日期选择器允许再次修改。无年龄校验来源、修改冷却/审计、age band、minor state、guardian consent、成人资源、NSFW 标签或区域策略。

因此：

- 无法可靠区分未成年人/成年人。
- 当前未发现明确成人功能，故不应凭空建设成人审核或 KYC。
- 若首发声明 13+/16+/18+，现架构只能展示自报生日，不能执行可信资源门。
- 应先确定产品年龄定位、目标商店 age rating 和澳大利亚服务分类，再实现最小 `age_band + assurance_level + policy_profile`。阈值不能散落成 `age < 13`。

## 6. UGC & Moderation

UGC 表面包括 profile/nickname/avatar、C2C、群消息、频道消息/评论、朋友圈、图片/音频/视频/文件、群/频道/Workspace 名称和 Bot/Agent 输出。

内容审核 migration 创建 `sensitive_word` 与 `review_queue`，Admin 有列表、导入和 approve/reject API/UI。但全后端只有 moderation Admin CRUD 引用 `review_queue`; C2C/C2G/channel/moment/profile/file 写入路径没有 `review_queue_repo` 调用。这是“管理面存在、采集面缺失”，不是可工作的审核系统。未发现 malware/URL/NSFW scanning。

E2EE 内容不能由服务端预扫描；这不是移除 E2EE 的理由。最小模型应区分：公开/服务端可见内容的服务端治理，与 E2EE 内容的客户端主动举报证据包。

## 7. Report / Block / Enforcement

### Report

`report_logic:create/5` 仅接受 moment/group/channel/user，要求 reason，description 最长 500；唯一键阻止同一举报人重复举报同一对象。工单记录 target/reporter/reason/description/status/handler，处理日志另表追加。

Flutter 有用户、群和消息入口；消息入口实际用 `target_type=user`，把 `message_id` 写入自由文本 description。这会导致消息被删除/编辑、ID 格式变化或审核检索时证据不稳定。未确认 Channel、Workspace、profile/avatar/file/image/video 的清晰入口。

### Enforcement

举报 resolve 只将 status 设为 1/2，并写 action log，不删除内容、不禁言/封禁、不通知用户。`review_queue` reject 同样只改队列状态。散点能力包括全局发送限流自动禁言、群成员禁言、移除成员、用户 disable，但没有统一 action record、期限、reason、scope 和解除流程。

### Block matrix

| Path after A blocks B | Current evidence | Result |
|---|---|---|
| B -> DM A | `msg_c2c_logic:c2c_send/5` 检查 A 对 B denylist | Enforced |
| B -> friend request A | `friend_logic` / `friend_ds:pending_status` | Enforced |
| B -> call A | `webrtc_ws_logic` | Enforced |
| B -> moment visibility | `moment_ds:is_blocked_pair/2` | Enforced |
| B -> Group containing A | C2G 仅检查成员/群禁言 | Not enforced by block |
| B -> mention A | 只检查 `@all` 管理权限 | Gap |
| B -> Channel A | 未见 denylist check | Gap |
| B -> invite A | Group/Channel/Workspace 邀请未统一检查 | Gap |
| B -> profile/search A | 未见统一 denylist filter | Gap |

Group 内是否彻底隐藏双方内容是产品决策；但 mention、直接邀请和一对一触达应默认拒绝，且规则必须写入 policy 并测试。

## 8. Privacy & Data Governance

| Data | Collected / purpose (code evidence) | Storage | Retention | Access | Delete/export | Cross-border |
|---|---|---|---|---|---|---|
| Account/profile/contact | Yes, identity/social | PostgreSQL + local SQLite | Unknown | user/admin APIs | Partial/partial | Unknown |
| Mobile/email/password | Yes, auth/recovery | PostgreSQL | Unknown | auth/admin paths | Delete list partial; excluded from export | SMS/email provider unknown |
| Device/session/push token | Yes, auth/push | PostgreSQL + device | Token validity/config specific | user/admin/provider | Partial/not exported | FCM/APNs conditional |
| IP/security/login logs | Yes | PostgreSQL + app/Loki logs | Mixed/unknown | admin/ops | Intentionally incomplete | Deployment dependent |
| Location | Nearby/map feature | PostgreSQL + device; AMap SDK | Unknown | feature APIs/provider | Deletion includes geo row; export missing | AMap destination unknown |
| Messages | Yes, routing/archive | PostgreSQL + SQLite | archive flag; no category TTL | participants; admin per audit mode | delete/export incomplete | Hosting/push dependent |
| Attachments | Yes | PostgreSQL + Garage/object store | orphan/pending cleanup only | authorized URL paths/admin storage | account cascade/export missing | Hosting/CDN dependent |
| Group/channel/workspace membership | Yes | PostgreSQL + SQLite | Unknown | members/admin | account cascade/export incomplete | Hosting dependent |
| Audit/security/moderation | Yes, fragmented | PostgreSQL + Loki | OTK 7d option; Loki 180d; others unknown | admins/ops | policy exception unresolved | Hosting dependent |
| Payment | Yes when enabled | PostgreSQL + payment providers | Unknown | finance/admin/provider | not in user export/delete map | Provider dependent |
| Analytics/crash | Sentry optional; Admin UX telemetry | Sentry/backend | Unknown | vendor/admin | propagation unknown | Vendor project region unknown |
| AI/Bot | Conditional | backend + configured LLM/webhook | Unknown | provider/bot owner/admin | not mapped | Provider dependent |

“用途、访问人、期限、跨境”中无法由代码确认的项目必须在部署级 RoPA/data map 中补齐，不能从 self-hosted 宣称推导。

## 9. Data Retention

- `msg_archive_enabled=true` 示例配置支持永久消息归档，但未发现按 tenant/conversation/category 的期限和清理 job。
- Loki `retention_period: 4320h` 是 180 天，而紧邻注释写“30 天”；这会造成声明/配置不一致。
- PG 备份默认 7 天，本地 Garage 备份默认 30 天；远端 Garage 使用 `rclone sync`，实际版本/生命周期取决于目标端，仓库无法确认。
- 附件仅有 pending/orphan cleanup，不等于用户删除或内容删除后的对象级删除传播。
- moderation/report/admin/user logs 未见明确 TTL；OTK cleanup 默认关闭。

当前无法回答“账号删除后多久从主库、对象存储、日志、备份和供应商完成删除”。

## 10. Data Export / Delete

### Export

`POST /api/v1/user/export_data` 的 Uid 来自鉴权上下文，避免 IDOR，并执行敏感键过滤和导出审计。这是有效基础。但 `user_ds:export_data/1` 只取 user_info、friends、groups、settings，缺消息、附件、频道、Workspace/Project、朋友圈、设备/session、支付、举报和 AI/Bot 数据；其注释也明确承认不是全量。

### Delete

Flutter 设置页提供导出、确认和注销申请，Web 有 `/account-deletion` 指引。服务端将 status 改为 2 并写 user_log，定时 GenServer 默认禁用。

`user_deletion_logic` 以 `user.updated_at` 计算 60 天，但 `user` schema 只有 `created_at`，`apply_logout` 也没有独立申请时间字段。因此 cleanup 查询失败。当前事务仅列出部分关系表，并明确保留 `trust_audit`；它不处理消息、附件对象、channel/workspace/payment/report 等。管理员 approve 只把状态设为 -1，也不执行物理清理。

这是 Apple/Google 发布 Blocker；修复前不能把申请成功页当成删除成功。

## 11. Cross-border Data

```text
Flutter/Web/Admin
  -> IMBoy API / WebSocket
     -> PostgreSQL (account, relationships, messages, governance, payment)
     -> Garage/S3 (attachments)
     -> Loki/Prometheus/Grafana (logs/metrics)
     -> SMTP/SMS/JPush (conditional)
     -> FCM/APNs (push, conditional)
     -> AMap (location/map, conditional)
     -> Sentry (crash, build-time optional)
     -> LiveKit/TURN (media/signaling, conditional)
     -> Alipay/WeChat/Stripe (payment, conditional)
     -> LLM/Bot webhook providers (AI/Bot, conditional)
```

代码只能确认可能的数据流，不能确认数据中心国家、实际启用项、控制者/处理者角色、SCC/adequacy/DPA 或 AU APP 8 安排。数据所在地与跨境传输机制必须分别登记。

## 12. Third-party SDK

| Provider | Purpose / likely data | Optional | Destination/DPA/privacy docs | Assessment |
|---|---|---|---|---|
| Firebase/FCM | push token, device/app metadata, notification data | Config-dependent | Unknown | Inventory required |
| Apple APNs | push | Config-dependent | Unknown | Inventory required |
| Sentry | exception/stack/device context | Build-time optional | Unknown | Scrubbing/region/retention unknown |
| AMap | location/map/device data | Feature-dependent | Unknown | Consent/disclosure and overseas availability review |
| JPush/SMS/SMTP | push/phone/email delivery | Config-dependent | Unknown | Processor records missing |
| LiveKit/TURN | IP/media metadata; media path | Feature-dependent | Self/third-party unknown | Topology and retention unknown |
| Alipay/WeChat/Stripe | identity/payment/transaction | Feature-dependent | Unknown | Store billing + DPA/legal review |
| LLM/Bot webhook | prompts/messages/identity metadata | Feature-dependent | Unknown | E2EE exclusion and disclosure must be proven |

Dependencies alone do not prove runtime collection. Release inventory must bind provider to build flavor, config, data fields and deletion path; unknowns remain `UNKNOWN / NEEDS LEGAL VERIFICATION`.

## 13. Security & Abuse Prevention

Positive evidence: password hashing, parameterized DB helpers, login-attempt tracking, OTP/email throttles, refresh/WS/API/message/E2EE rate definitions, device kick/session revocation, group membership checks, E2EE fail-closed checks, payment callback verification tests.

Gaps/risks:

- rate limits are fragmented and several depend on process-local state; multi-node/global effectiveness needs proof.
- no verified attachment malware/URL scanning or quarantine.
- registration bot/fake-account/risk-state model is absent.
- report creation lacks explicit per-target abuse rate and malicious-report workflow beyond unique constraint.
- logging calls include actor IDs and operational payload/error objects; no centralized redaction contract was found for all backend/Flutter/Sentry paths.
- full release security testing (IDOR, role escalation, enumeration, account takeover, delete/export authorization) has not been run in this audit.

## 14. Admin / Moderator RBAC

Admin middleware authenticates a signed cookie, supports production disabling of legacy unsigned cookies and optional IP allowlist. Handlers commonly call `adm_acl:ensure_permission`; reports distinguish read/handle.

Risks:

- built-in role catalog is broad and does not establish explicit Moderator/Security Admin/Support separation.
- message list/detail/export all rely on `messages:read`; no distinct `messages:export` permission.
- `audit_mode=full` returns raw payload. This may expose non-E2EE private chat or E2EE envelope data.
- message read/detail/export endpoints do not themselves append `admin_operation_logs`; the Admin “Audit Log” UI partly reconstructs events from message and logout lists, which is not immutable access auditing.
- moderation decisions write report action log, but sensitive-word/review-queue actions do not visibly share a comprehensive actor/action/target audit contract.

High-risk design statement: **当前配置允许时，具有 `messages:read` 的管理员可以查看并导出所有查询到的消息 payload，且未见逐次访问审计。** 需要最小权限拆分和强制审计；不是要求移除企业合规审计模式。

## 15. E2EE

当前策略支持 `none/metadata/full` audit mode 和 `disabled/required/compliance` E2EE 模式；C2C/C2G 对要求加密的内容执行服务端 fail-closed，服务端路由/存储密文信封。Admin full mode只能看到数据库 payload；对于正确 E2EE 消息不应得到明文。

Architecture Conflict：

- 服务端敏感词/NSFW 扫描与纯 E2EE 明文不可见模型冲突。
- 精确消息举报需要可验证证据，但服务端不能主动解密历史消息。
- 推送 preview、客户端日志、引用 snippet、附件加密状态仍需跨端验证。

计划必须保留 E2EE：公开/服务端可见内容走服务端 moderation；E2EE 举报由举报者客户端显式提交最小证据包（消息 ID、发送者、会话 scope、客户端解密后的被举报片段/附件引用、完整性元数据和同意声明），并严格限制审核访问、期限与审计。

## 16. Commercialization

代码真实存在 wallet、recharge、payment transaction、billing plan/subscription/invoice、paid channel/order/refund、Alipay/WeChat/Stripe。故不能写“无支付”。但未发现 Apple IAP / Google Play Billing 依赖或按数字内容渠道选择的完整证据。

海外移动端若售卖 app 内数字频道/订阅，商店计费规则与地区例外必须由发布团队和法律/财务确认。第一阶段若不发布付费频道，使用现有 feature gate 明确关闭，并验证 UI/API/深链均不可达；不要为合规新增支付系统。

## 17. AI

`account_type` 区分 human/agent/system_bot/bot，AI Agent 可绕过好友关系接受 C2C，但仍尊重 denylist；E2EE 消息按代码注释跳过 LLM。存在 Admin Agent/Bot 管理。

缺口：用户侧身份标识是否在所有头像、会话、通知、转发和群回复中清晰一致；AI 输出举报/处置；模型供应商、prompt/data retention；生成内容安全反馈。AI disclosure 应分别归类为 Google Play policy applicability、消费者透明度与最佳实践，不声称全球统一法律义务。

## 18. Apple App Store

**Confirmed platform requirements（以提交时最新官方文本复核）：**

- Apple Guideline 1.2 对 UGC 要求防滥用过滤、举报及及时响应、屏蔽 abusive users、公开联系方式。IMBoy 的举报/审核/block 尚未完整覆盖，当前为 FAIL。
- 支持账号创建的 App 必须允许在 App 内发起完整账号删除；仅停用不够。当前删除执行链损坏且数据覆盖不足，FAIL。
- 数字内容/订阅支付需按实际商品形态映射 Guideline 3.1.1；当前 paid channel 海外开关和 IAP 路径未知，LEGAL REVIEW。

定位风险：核心是好友/组织驱动 IM，不是随机匿名聊天；但“附近的人”、公开发现和 live-room 若开放，会增加高风险社交分类概率。首发应按证据关闭非核心发现功能。

官方来源：[App Review Guidelines](https://developer.apple.com/app-store/review/guidelines/)，[Offering account deletion](https://developer.apple.com/support/offering-account-deletion-in-your-app/)。

## 19. Google Play

**Confirmed platform requirements（以提交时最新官方文本复核）：**

- UGC policy 要求持续 moderation，用户接受条款，并按互动类型提供 app 内 report/block；目前仅部分满足。
- App 内可创建账号时，需要 app 内删除入口及可在 app 外访问的 web 删除资源；冻结账号不够，必要留存要准确披露。当前 FAIL。
- Data Safety 声明必须覆盖 app 与第三方 SDK 的收集/分享；当前 provider inventory 未闭合。
- 若 AI chatbot 构成 Google 定义的生成式 AI 核心功能，需适用 AI-generated content policy；当前功能与首发开关需确认。

官方来源：[UGC moderation guidance](https://support.google.com/googleplay/android-developer/answer/12923286)，[Account deletion](https://support.google.com/googleplay/android-developer/answer/13327111)，[User Data policy](https://support.google.com/googleplay/android-developer/answer/10144311)，[AI-generated content](https://support.google.com/googleplay/android-developer/answer/14094294)。

## 20. Regional Compliance

| Region | Known | Likely / interpretation | Unknown / legal review |
|---|---|---|---|
| EU/EEA | GDPR requires lawful basis, transparency, minimization, storage limitation and applicable access/erasure/portability rights; third-country transfers need a Chapter V mechanism | IMBoy operator may be controller, self-host customer roles may vary; DSA hosting/online-platform duties may apply to hosted public UGC | Controller/processor allocation, establishment/targeting, DSA classification/exemptions, lawful bases, SCC/TIA, retention exceptions |
| US | FTC Act requires honoring privacy/security representations; COPPA applies based on child-directed/actual-knowledge tests, not every general audience app | State privacy/deletion rules may apply by thresholds and states; TAKE IT DOWN Act applicability may matter for covered platforms | Launch states, thresholds, child audience, biometric/location/payment rules, designated agent/process |
| Australia | IM/chat can be a “relevant electronic service”; online safety standards/codes and APPs may apply; APP 8 addresses overseas disclosure | IMBoy is more likely messaging/RES than an age-restricted social-media service, but actual features/use control classification | Entity coverage, RES vs social-media classification, 2026 age-restricted code duties, data residency/recipient arrangements |
| UK/CA/BR/JP/IN/SG | Not assessed beyond issue spotting | Separate privacy/online-safety regimes likely relevant | Entire mapping NEEDS LEGAL REVIEW before those markets |

Official sources: [GDPR text](https://eur-lex.europa.eu/eli/reg/2016/679/oj), [DSA](https://eur-lex.europa.eu/eli/reg/2022/2065), [EU international transfers](https://commission.europa.eu/law/law-topic/data-protection/rules-business-and-organisations/obligations/what-rules-apply-if-my-organisation-transfers-data-outside-eu_en), [FTC children's privacy](https://www.ftc.gov/business-guidance/privacy-security/childrens-privacy), [eSafety code assessment](https://www.esafety.gov.au/industry/codes/assess-which-online-safety-codes-and-standards-apply), [OAIC APP 8](https://www.oaic.gov.au/privacy/australian-privacy-principles/australian-privacy-principles-guidelines/chapter-8-app-8-cross-border-disclosure-of-personal-information).

## 21. Code-level Findings

| ID | Severity | Finding / evidence | Current behavior and risk | Recommendation | Test coverage |
|---|---|---|---|---|---|
| F-01 | P0 | `user_deletion_logic:delete_expired_users/2` -> `user_ds:find_expired_logout_users/2`; SQL uses `user.updated_at` at `user_ds.erl:486`, absent from schema | request succeeds but automated deletion cannot select candidates | add explicit deletion-request timestamp/state source; transactional, idempotent deletion orchestrator | unit exists; add real PG integration + clock cases |
| F-02 | P0 | `user_ds:delete_all_related_data/2:245-289` enumerates only part of data | messages/attachments/channel/workspace/payment/report/backups/vendor copies survive; ownership deletion may break relations | make policy-owned deletion manifest with delete/anonymize/retain outcome; object/vendor jobs | existing tests are partial; add seeded cross-domain DB acceptance |
| F-03 | P1 | `message_action_menu.dart:431-470`; report target types exclude message | message report is free-text pointer to a user; evidence not immutable/searchable | add message target/evidence schema and endpoint while retaining general report model | add C2C/C2G/channel/E2EE report tests |
| F-04 | P1 | `report_logic:admin_resolve/5` and `review_queue_repo:moderate` only update status | confirmed violation has no enforcement or notification | explicit small action set: remove, warn, scoped mute, account restrict; append-only action log | missing end-to-end case -> action tests |
| F-05 | P1 | only Admin moderation modules call `review_queue`; no content ingest caller | queue remains empty unless externally/manual inserted | wire only non-E2EE/public supported writes through one policy decision; E2EE uses report evidence | missing ingest tests |
| F-06 | P1 | denylist callers cover C2C/friend/call/moment, not C2G/channel/invites | blocked user can still mention/invite/contact through sibling paths | one shared `user_safety` decision called at direct-contact boundaries | no full block matrix tests |
| F-07 | P1 | `adm_message_handler:418-471`; `messages:read` grants list/detail/export; raw payload in full mode | broad private-content access without per-access audit or export separation | separate permissions; default metadata; reason/ticket requirement; immutable access audit | sanitizer tests only; add RBAC/audit integration |
| F-08 | P1 | birthday string and `user_agg` passthrough | no enforceable age policy or trusted age | decide launch age profile first; store derived band/assurance, protect modification | missing |
| F-09 | P1 | `loki.yml:50-52` says 30d but config 180d; message archive has no TTL | privacy notice and runtime can diverge; indefinite storage | data-class retention register + jobs + deployment assertion | missing expiry/hold/backup tests |
| F-10 | P1 | dependency/config evidence for FCM/Sentry/AMap/SMS/payment/LLM, no machine-readable data inventory | Data Safety/privacy disclosures and deletion propagation cannot be substantiated | checked-in provider inventory bound to build/runtime flags | missing release check |
| F-11 | P1 | Admin resolve/review lacks appeal model; report reasons/actions sparse | operations cannot consistently notify, reverse, measure or defend decisions | minimal case/action/appeal records; applicability gated by policy/legal decision | missing |
| F-12 | P1 | no attachment malware/URL safety pipeline found | shared files/links can distribute abuse/malware | size/type validation now; quarantine/scanner only when file sharing is in launch profile | missing malicious attachment cases |
| F-13 | P2 | export sanitizer uses substring blacklist after `SELECT *` settings | false positives and future sensitive fields rely on naming convention | explicit export allowlist per data category | sanitizer unit tests exist; add schema drift test |
| F-14 | P2 | `apply_logout` ignores transaction result and handler always returns success | DB failure can be shown as successful request | propagate transaction error and expose request status | missing failure-path API test |
| F-15 | P2 | Flutter/Sentry error objects lack a proven global scrubber | token/message/PII may reach logs through exception context | central redaction at sinks plus forbidden-key tests | missing |

## 22. Test Coverage

Existing focused tests cover report handler/logic/Admin resolve, denylist repository, message send relationship checks, login security, rate limiting, deletion/export functions, RBAC handlers and E2EE policy. They are useful code evidence but not a release acceptance suite.

Missing minimum suites:

- PostgreSQL-backed deletion/export fixture covering every data category and object-storage job.
- dual-account Block matrix: DM, call, friend, mention, invite, group/channel/profile/search.
- report lifecycle with immutable message evidence, duplicate/malicious report, moderator decision, enforcement, notification and appeal.
- permission matrix for User/Group Owner/Admin/Workspace Owner/Admin/Moderator/Platform Admin/Security/Support.
- E2EE report evidence without server plaintext access; push preview and logs contain no plaintext.
- retention expiry with backup/object-store tombstone evidence.
- dependency/build flavor inventory vs Apple privacy label and Google Data Safety declaration.
- real-device Apple/Android account deletion route and public web deletion route.

No tests were executed in this read-only audit; only source/config/schema analysis and official policy research were performed.

## 23. Risk Matrix

| Risk | Likelihood | Impact | Severity | Owner |
|---|---|---|---|---|
| account deletion advertised but not performed | High | store rejection/privacy harm | P0 | Backend + Privacy |
| UGC report/block/moderation incomplete | High | store rejection/user harm | P1 | Safety + Backend/App |
| privileged private-message access lacks audit | Medium | severe confidentiality/insider risk | P1 | Security + Admin |
| undefined retention/deletion propagation | High | privacy/legal mismatch | P1 | Privacy + Ops |
| unreliable minor classification | Medium | wrong age controls/ratings | P1 | Product + Legal |
| third-party disclosure mismatch | High | store/privacy enforcement | P1 | Mobile + Privacy |
| E2EE governance design conflict | Medium | weaken security or fail safety response | P1 | Security + Safety |
| digital-content payment mismatch | Medium | store rejection/commercial loss | P1, depends on launch scope | Product + Legal/Finance |
| no malware/link abuse controls | Medium | user/device harm | P1/P2, depends on file launch | Security |
| region classification incorrect | Medium | regulatory exposure | NEEDS LEGAL REVIEW | Legal |

## 24. Unknown / Need Legal Review

- Legal entity, controller/processor roles for hosted vs self-hosted deployments.
- Exact launch countries/states, user targeting, age rating, children-directed status and contractual customer model.
- GDPR lawful bases, Article 8 member-state age threshold handling, DPO/representative need, retention exceptions and transfer mechanism.
- DSA intermediary/hosting/online-platform classification, micro/small exemptions and appeal/transparency duties.
- US state privacy thresholds; COPPA actual-knowledge/directed-to-children analysis; TAKE IT DOWN Act coverage/process.
- Australian Privacy Act entity coverage, RES/social-media classification and applicable 2026 codes/standards.
- Whether paid channel/subscriptions are mobile digital content requiring Apple/Google billing in each storefront.
- Third-party provider contracts, subprocessors, destination regions, DPA/SCC, deletion APIs and retention.
- Law-enforcement preservation/legal hold requirements; code explicitly says legal hold unsupported.

### Ten required judgments

1. **海外普通 IM / Community / Workspace 是否适合发布？** 产品定位适合；当前实现状态不适合正式发布，结论 NO-GO。
2. **最大 10 个缺口？** F-01 至 F-10。
3. **代码问题？** 删除查询/清理、消息举报对象、block 路径、审核 enqueue/处罚、权限拆分/访问审计、export、日志 redaction、retention jobs。
4. **产品设计问题？** 首发功能 profile、年龄边界、群内 block 语义、E2EE 举报证据、AI 标识、付费频道范围。
5. **运营问题？** 审核 SLA、moderator staffing、处罚手册、申诉、紧急升级、透明度与供应商台账。
6. **法律顾问确认？** 第 24 节全部地区适用性、合法依据、期限例外、跨境、支付和年龄分类。
7. **可暂不做？** KYC/身份证、通用 AI moderation、视频预审、creator economy、多租户重构、全球规则引擎；若首发关闭则附近的人/live/付费频道/AI 可延后。
8. **阻碍 Apple/Google？** 可用完整账号删除、清晰 UGC report/block/moderation、公开联系方式/条款、第三方数据声明；数字内容支付若启用也可能阻碍。
9. **最小 20%？** 修复删除；闭合消息举报+处罚；统一直接触达 block；默认 metadata 管理访问并审计；建立 retention/provider inventory；首发关闭非核心高风险功能。
10. **何时具备基础能力？** Implementation Plan 的 Phase A/B 全部通过、P0/P1=0、双平台声明与真实设备/真实部署验收完成，并取得区域法律分类意见；仍只能称“海外发布基础合规能力”。

### Compliance Release Gate (current)

```text
P0 = 2                         FAIL
P1 = 11                        FAIL
Critical security findings    UNVERIFIED

Account:       PARTIAL
Age:           FAIL / LEGAL REVIEW
UGC:           FAIL
Report:        FAIL
Block:         FAIL
Moderation:    FAIL
Privacy:       FAIL
Delete:        FAIL
Export:        FAIL
Retention:     FAIL
E2EE:          PARTIAL / ARCHITECTURE CONFLICT
Apple:         FAIL / LEGAL REVIEW
Google Play:   FAIL / LEGAL REVIEW
EU:            LEGAL REVIEW
US:            LEGAL REVIEW
Australia:     LEGAL REVIEW

FINAL: NO-GO
```
