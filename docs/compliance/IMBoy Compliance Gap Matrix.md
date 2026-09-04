# IMBoy Compliance Gap Matrix

> 基线与证据口径见 [IMBoy Overseas Compliance Self-Audit.md](./IMBoy%20Overseas%20Compliance%20Self-Audit.md)。`Required Before Launch` 是工程/商店 Gate 判断；法律适用性仍以法律顾问意见为准。

| Domain | Current | Evidence | Gap | Severity | Required Before Launch | Proposed Solution |
|---|---|---|---|---|---|---|
| Account | Partial | `user` status; password/code/OIDC/Alipay; device APIs | 无统一 risk state；恢复/多账号政策未定义 | P2 | Depends | 保持现有身份模型，只补 risk/account policy 与审计 |
| Age | Missing | birthday 字符串可修改 | 无可信年龄带/区域策略 | P1 | Depends on rating/region | 先做产品/法律决定；仅实现 age band + assurance + policy profile |
| Report | Partial | `report_handler`, `report_ticket` | 对象仅 user/group/channel/moment；消息证据是自由文本 | P1 | Yes | 增加 message/attachment/profile 等实际首发对象与不可变 evidence ref |
| Block | Partial | C2C/friend/call/moment denylist checks | mention/invite/channel/profile/search 旁路 | P1 | Yes | 复用 denylist，增加共享 server-side direct-contact decision |
| Moderation | Scaffold | sensitive words/review queue Admin CRUD | 生产内容不入队，reject 不处置 | P1 | Yes for UGC | 仅非 E2EE/公开内容接入 policy；决定触发与 action |
| Enforcement | Fragmented | rate mute, group mute/kick, user status | report 与处罚未连接；无期限/scope/reversal | P1 | Yes | 最小 action 表和 executor，复用现有 mute/remove/status |
| Appeal | Missing | 无业务链 | 用户不可申诉或收到理由 | P1/P2 | Legal classification dependent; operational baseline recommended | 最小 appeal request/review/final decision；先法律确认 DSA 适用性 |
| Delete | Broken | apply/cancel/API/UI/job | nonexistent timestamp column；默认 off；范围不全 | P0 | Yes | explicit request record + idempotent orchestrator + object/vendor propagation |
| Export | Partial | self-only endpoint + sanitizer/audit | 仅资料/好友/群/设置 | P1/P2 | Yes for declared rights; scope by legal review | 异步 manifest 导出，逐类 allowlist |
| Retention | Fragmented | archive flag, Loki 180d, backup 7/30d | 无统一 policy，注释/配置冲突，账号删除无备份证明 | P1 | Yes | data-class register + jobs + deploy gate + deletion tombstone |
| Privacy notice | Partial/template | static privacy page, app markdown | 与真实 SDK/data flows/retention 不可核验一致 | P1 | Yes | 基于 inventory 生成/审核 notice 与 store declarations |
| Consent | Unknown/partial | OS push/location permissions | necessary vs optional processing 未登记 | P2 | Yes where applicable | purpose registry；只对确需 consent 的 optional processing 建 gate |
| Third-party | Unknown | FCM/Sentry/AMap/SMS/payment/LLM configs | purpose/fields/region/DPA/delete unknown | P1 | Yes | machine-readable build/provider inventory + owner/legal verification |
| Cross-border | Unknown | configurable self-host topology | location/mechanism均未知 | P1 / Legal | Yes for operated service | deployment data map + transfer assessment; self-host roles separately |
| Admin RBAC | Partial | signed cookie, `adm_acl`, permissions | message export shares read permission；无职责分离 | P1 | Yes | separate sensitive permissions and least-privilege seeded roles |
| Admin access audit | Missing for message reads | message endpoints return payload by mode | list/detail/export access未逐次不可变记录 | P1 | Yes if admin content access enabled | ticket/reason + access audit; metadata default |
| E2EE | Partial/strong base | required mode gates, per-device envelopes | server moderation vs plaintext conflict；evidence path缺失 | P1 architecture conflict | Yes | preserve E2EE; client-submitted evidence only on explicit report |
| Attachment safety | Missing | Garage upload/view/cleanup | 无 malware/URL quarantine | P1/P2 | Yes if file sharing enabled | size/type guard + quarantine/scanner adapter; no AI moderation required |
| Abuse prevention | Partial | login/message/API/E2EE throttles | report/invite/channel/registration abuse不统一 | P1/P2 | Yes | targeted limits and risk counters, not a generic fraud platform |
| AI identity | Partial | distinct account types | all surfaces disclosure and output reporting unverified | P1/P2 | If AI enabled | consistent badge/metadata + report target + provider disclosure |
| Payment | Present | wallet/billing/paid channel/providers | mobile digital content store billing mapping unknown | P1 / Legal | If enabled | disable for phase 1 or implement approved store-specific purchase path |
| Apple UGC | Fail | partial report/block/moderation | Guideline 1.2 closure absent | P1 | Yes | pass report/block/response/contact-info acceptance matrix |
| Apple deletion | Fail | in-app request exists, deletion broken | full account/data removal not achieved | P0 | Yes | D-01..D-04 and real-device review evidence |
| Google UGC | Fail | partial tools | required in-app moderation/report/block incomplete | P1 | Yes | same safety baseline + clear ToS acceptance |
| Google deletion | Fail | web + app entry present | associated-data deletion incomplete | P0 | Yes | full request/status/deletion + Play Console URL evidence |
| EU/EEA | Unknown | privacy/export/delete fragments | controller role, rights scope, DSA and transfer mechanism unknown | Legal | Legal review | counsel memo + RoPA/DPA/SCC/TIA as applicable |
| US | Unknown | general-audience code signals only | state scope/COPPA/TIDA applicability unknown | Legal | Legal review | market/audience/threshold memo; no universal age-13 hardcode |
| Australia | Unknown/high attention | IM fits likely RES definition | 2026 codes, privacy coverage, age duties/classification unknown | Legal | Legal review | eSafety classification assessment + APP/cross-border review |
| Tests | Partial | focused EUnit/Flutter/Admin tests | no cross-domain DB, dual-user, device, store acceptance | P1 | Yes | release suite and evidence ledger from plan |
| Feature composition / build slicing | Partial | Backend policy/profile, Flutter registry/route guard, Admin feature route/sidebar | 现有机制主要是运行时隐藏/拒绝；关闭模块仍可能进入 binary/bundle，三仓配置可能漂移 | P1 delivery architecture / P2 compliance | Yes for the requested distributable editions; not a legal requirement by itself | 单一 canonical manifest + dependency validation + generated registries + three-artifact hash；运行时只能在编译集合内继续关闭 |

## Launch Scope Shortcut

The smallest defensible phase-1 profile is:

```text
ON: account, friend-driven C2C, group, workspace/project, basic channel,
    report, block, moderation operations, E2EE, delete/export, retention

OFF until separately accepted: nearby people, public discovery/trending,
    live room, paid channel/wallet, AI Agent marketplace, third-party bots
```

This is a proposed launch build preset, not a product deletion and not a legal mandate. A canonical product-feature manifest should define the compiled feature ceiling for Backend, Flutter and Admin. Existing feature/profile mechanisms remain runtime enforcement and may only further disable compiled features. Every OFF item needs artifact/import or chunk evidence plus UI, API, WebSocket and deep-link verification. Shared infrastructure and a compatible database-schema superset may remain when physical removal would require rewriting core domains.
