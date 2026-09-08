# IMBoy Compliance Gap Matrix

> 基线与证据口径见 [IMBoy Overseas Compliance Self-Audit.md](./IMBoy%20Overseas%20Self-Audit.md)。`Required Before Launch` 是工程/商店 Gate 判断；法律适用性仍以法律顾问意见为准。
> **2026-09-08 终态回写**：实施计划（IMBoy Overseas Compliance Implementation Plan）F/L/D/R/B/A/T/P/V/E 十条线工程侧已全部执行完毕，本表已按当前代码逐行回写。全部提交在各自仓 main、未 push。剩余 Gap 均为外部依赖：E-02 真机验收（需设备）、R-04/B-02（LEGAL 门/矩阵拍板）、各 pending-owner 项（DPA/SCC 签署、生产开关、保留值翻转等）。各任务执行记录见同目录 Checklist 文档。

| Domain | Current | Evidence | Gap | Severity | Required Before Launch | Proposed Solution |
|---|---|---|---|---|---|---|
| Account | Partial | `user` status; password/code/OIDC/Alipay; device APIs | 无统一 risk state；恢复/多账号政策未定义 | P2 | Depends | 保持现有身份模型，只补 risk/account policy 与审计 |
| Age | Missing | birthday 字符串可修改 | 无可信年龄带/区域策略 | P1 | Depends on rating/region | 先做产品/法律决定；仅实现 age band + assurance + policy profile |
| Report | Mostly done | `report_handler`, `report_ticket`; R-01（迁移 00000087）增加 message 对象 + target_author_id/scope_id + 不可变 evidence jsonb + admin 工单证据端点；E2EE 举报走 e2ee_consent 显式披露门 | attachment/profile 对象仍未覆盖 | P1 | Yes | attachment/profile 对象待后续补齐 |
| Block | Mostly done | C2C/friend/call/moment denylist checks；B-01（0e979c70）WS/HTTP 同链门 + 频道/工作区邀请双向撮合门（blocked_between，DB 异常 fail-closed） | mention/群拉人/profile/search 语义待 B-01 矩阵拍板（三选项待 owner） | P1 | Yes | 拍板后按选项落 B-02 UX 对齐 |
| Moderation | Mostly done | sensitive words/review queue Admin CRUD；R-03（ff9774a5）moderation_policy 单入口（决定性关键词/fail-open）接入 channel_message/moment_post 两面（high 发布前拦截、medium/low 先发后审、reject 联动撤下、overdue SLA 24h）；R-03.1（63f7f14f）补 profile_field 文本面（nickname/sign/profession/school/interests，reject 清字段，字段名编码 msg_type） | 改名频率风控未立项；队列 Admin 页 overdue 列 UI 打磨 | P1 → P2 | Yes for UGC（工程闭环已达成） | E2EE 私信绝不入审核（surface 白名单契约，见 e2ee-visibility-matrix.md） |
| Enforcement | Mostly done | R-02（迁移 00000088）moderation_action 审计行 + executor + /api/adm/report_action + admin 动作面板；动作集 warning/mute/kick/restrict/reject/content_removal，含期限 end_at、reversal、prev_status 恢复、到期 sweep | content_removal 的 channel 已支持、用户消息撤回通知联动待打磨；action 与 rate-limit 的联动未做 | P1 | Yes | 通知策略打磨与 rate-limit 联动 |
| Appeal | Missing | 无业务链 | 用户不可申诉或收到理由 | P1/P2 | Legal classification dependent; operational baseline recommended | 最小 appeal request/review/final decision；先法律确认 DSA 适用性 |
| Delete | Mostly done | D-01（00000085+幂等 apply/cancel+状态端点）D-02（data-disposition 145 表）D-03（00000086 job/orchestrator/到期 sweep）D-04（macOS e2e 全绿 + 真机走查截图） | 生产启用需 `user_deletion_enabled=true`；第三方传播（vendor propagation）未接 | P0 | Yes | vendor propagation 与生产开关由 owner 决策 |
| Export | Mostly done | P-01（516a7846）export_data_bounded（friends 5000/groups 1000 LIMIT+truncated 诚实标记；settings 列 allowlist）、冷却门（user_log type=130 真源，默认 24h，429）、scope 范围声明（excluded 逐项点名+disclaimer） | 异步大表归档（加密 zip+过期链接）待 legal review 圈范围；data-disposition.yml 加 export 维度待 owner（受限 YAML schema 变更） | P2 | Yes for declared rights; scope by legal review | scope 声明已可对外；如需扩 messages 类再立项异步导出 |
| Retention | Mostly done | T-01（a67565dc）retention-policy.yml 12 数据类（4 类 evidence-backed：Loki/Prometheus 180d、backup 7d/10 份、token event-driven）+ validate 四道门（红线：禁 country、legal_hold 必须 unsupported）；T-02（2c4d5bc6）credential_retention_worker（默认禁用/dry-run/bounded）+ BACKUP-RESTORE 删除重放节（墓碑对账→SKIP LOCKED 重放）；F-08（29860bc1）契约化"保留义务不随 feature 禁用停" | 8 类 pending-owner/legal-review 保留值待翻转；login_attempt 清理口径待 owner | P1 → P2 | Yes | owner 逐类翻转 pending 值后即成完整可证链 |
| Privacy notice | Partial/template | static privacy page, app markdown；V-01 inventory（070ebde9）与 V-02 脱敏（imboy 75bb6585/app 35fea795）提供了可核验的 SDK/数据流底册 | notice 与 store declarations 的最终措辞/一致性审核未做（V-01 遗留：Sentry 后台 Data Scrubbing 截图归档待 owner） | P1 | Yes | 基于 inventory 生成/审核 notice 与 store declarations |
| Consent | Unknown/partial | OS push/location permissions | necessary vs optional processing 未登记 | P2 | Yes where applicable | purpose registry；只对确需 consent 的 optional processing 建 gate |
| Third-party | Registered, DPA pending | V-01（070ebde9）third-party-data-inventory.yml 22 行（12 真实出站/3 self-hosted/7 显式 no-data，逐行 purpose/fields/region/retention/evidence）+ validate 四道门（schema/覆盖/实证双向/--overseas-gate）+ quality.yml CI job | 12 provider DPA/SCC 未签署（--overseas-gate 现红=设计意图，阻断海外运营）；sms.platform=aliyun 有配置无实现、高德 3dmap 自采范围待 owner 审计 | P1 | Yes | DPA/SCC 签署是海外发布唯一解阻路径；配置修正随签署批处理 |
| Cross-border | Partially mapped | V-01 region 字段已登记 12 出站 provider 地域；self-host（Garage/LiveKit/eturnal）dpa=not-applicable | transfer mechanism assessment 仍待 legal | P1 / Legal | Yes for operated service | deployment data map + transfer assessment; self-host roles separately |
| Admin RBAC | Mostly done | signed cookie, `adm_acl`, permissions；A-02（11921777/5eb6b57）内置角色扩至 1..6（moderator/security_admin/support 最小权限种子）+ 防自我提权三道门（assign/权限保存/disable），修真缺陷 disable 绕超管守卫 | 消息 export 权限已随 A-01 分离为 messages:export；自定义角色策略按需细化 | P1 → P2 | Yes | 职责分离种子已就绪，剩余为运营期权限微调 |
| Admin access audit | Done | A-01（2c1c69b8 后端 + 3ad1ba0 admin）权限三分（metadata:read/content:read/export）+ 工单制内容门（audit_mode=full + 权限 + 有效工单 + 原因，缺一硬错误）+ list/detail/export 逐次审计落 admin_operation_logs（fail-closed） | 生产 audit_mode 取值由 profile 决定（默认 metadata） | P1 → 已关闭 | Yes if admin content access enabled | 已实现 ticket/reason + access audit; metadata default |
| E2EE | Strong base, contract closed | required mode gates, per-device envelopes；E-01（b1ea97f2）e2ee-visibility-matrix.md 双矩阵 + 契约套件 6/6（required 下明文不达 logs/admin/push：日志哨兵测试/admin audit 三态/push 静态占位）+ PFv3 信封真实判定防误伤 | E-02 两账号多设备真机验收待执行（需 Android+iOS 设备） | P1 architecture conflict → 已关闭，剩验收 | Yes | 可见性矩阵即对外声明底稿；合规私钥托管披露见 e2ee-policy.md §3 |
| Attachment safety | Missing | Garage upload/view/cleanup | 无 malware/URL quarantine | P1/P2 | Yes if file sharing enabled | size/type guard + quarantine/scanner adapter; no AI moderation required |
| Abuse prevention | Partial | login/message/API/E2EE throttles | report/invite/channel/registration abuse不统一 | P1/P2 | Yes | targeted limits and risk counters, not a generic fraud platform |
| AI identity | Partial | distinct account types | all surfaces disclosure and output reporting unverified | P1/P2 | If AI enabled | consistent badge/metadata + report target + provider disclosure |
| Payment | Present | wallet/billing/paid channel/providers | mobile digital content store billing mapping unknown | P1 / Legal | If enabled | disable for phase 1 or implement approved store-specific purchase path |
| Apple UGC | Partial (tooling closed) | report（R-01 message 一等对象+consent 门）/block（B-01 服务端门）/moderation（R-03/R-03.1 三面接入+reject 处置）/audit（A-01）工具链闭环 | Guideline 1.2 验收矩阵（report/block/response/contact-info）真机走查待执行（并入 E-02 设备批） | P1 | Yes | 双账号真机走查出证据后翻 PASS |
| Apple deletion | Partial | in-app request + D-01..D-04 全链（幂等 apply/cancel、145 表处置清单、到期 sweep、macOS e2e 全绿+真机走查截图） | 生产启用需 `user_deletion_enabled=true`（owner）；关联数据删除的商店证据（Play Console URL 等）待提交 | P0 | Yes | owner 翻开关+商店后台取证 |
| Google UGC | Partial (tooling closed) | 同 Apple UGC 工具链（report/block/moderation/audit） | required in-app moderation 验收证据待真机批 | P1 | Yes | same safety baseline + clear ToS acceptance |
| Google deletion | Partial | web + app entry + D 链全链 | associated-data deletion 商店证据待提交（owner） | P0 | Yes | full request/status/deletion + Play Console URL evidence |
| EU/EEA | Unknown | privacy/export/delete fragments | controller role, rights scope, DSA and transfer mechanism unknown | Legal | Legal review | counsel memo + RoPA/DPA/SCC/TIA as applicable |
| US | Unknown | general-audience code signals only | state scope/COPPA/TIDA applicability unknown | Legal | Legal review | market/audience/threshold memo; no universal age-13 hardcode |
| Australia | Unknown/high attention | IM fits likely RES definition | 2026 codes, privacy coverage, age duties/classification unknown | Legal | Legal review | eSafety classification assessment + APP/cross-border review |
| Tests | Mostly done | focused EUnit（后端 6600+ 基线，本系列新增 e2ee_safety_contract/feature_composition_compat/moderation/export/log_redact 等契约套件）/Flutter/Admin tests；F-07 三仓产物矩阵（base-only/full-selected/overseas_baseline/agent_hub）证据归档 feature-composition-evidence/ | 跨域真库、双账号、双设备、商店验收证据仍待 E-02 真机批 | P1 | Yes | release suite 与真机证据台账（E-02） |
| Feature composition / build slicing | Done | F-00~F-07 执行记录+三仓 worktree 矩阵全 PASS；L-01 overseas_baseline preset（45018e03）+三端点 preset 感知+路由守卫映射修复（c45f1eb9/cf78cc7）；BUILD-00R 编译期物理裁剪（ERLC_EXCLUDE+prune+beam-cache 双槽位+ifdef 门控）；F-08/F-09（29860bc1）packaging-contract.md + 兼容契约测试 6/6 | 三仓矩阵证据的定期重跑频率待 release owner 定 | 已关闭 | Yes for the requested distributable editions | canonical manifest + 物理裁剪 + 三层物理断言已落地；运行时只能在编译集合内继续关闭 |

## Launch Scope Shortcut

The smallest defensible phase-1 profile is:

```text
ON: account, friend-driven C2C, group, workspace/project, basic channel,
    report, block, moderation operations, E2EE, delete/export, retention

OFF until separately accepted: nearby people, public discovery/trending,
    live room, paid channel/wallet, AI Agent marketplace, third-party bots
```

This is a proposed launch build preset, not a product deletion and not a legal mandate. A canonical product-feature manifest should define the compiled feature ceiling for Backend, Flutter and Admin. Existing feature/profile mechanisms remain runtime enforcement and may only further disable compiled features. Every OFF item needs artifact/import or chunk evidence plus UI, API, WebSocket and deep-link verification. Shared infrastructure and a compatible database-schema superset may remain when physical removal would require rewriting core domains.

**当前实现状态（2026-09-08）**：上述 preset 已以 `config/product-feature-manifests/overseas_baseline.json`（L-01）+ 三端生成物 + BUILD-00R 编译期物理裁剪 + `verify_product_feature_artifacts.py` 三层物理断言落地；OFF 清单逐项有产物/路由/深链证据（feature-composition-evidence/）。操作口径见 [docs/product/packaging-contract.md](../product/packaging-contract.md)。

## 剩余外部依赖汇总（无工程阻塞项）

| 项 | 依赖 |
|---|---|
| E-02 双账号多设备真机验收（含 Apple/Google UGC 验收矩阵走查） | Android+iOS 设备 |
| R-04 申诉链 | LEGAL 门（DSA 适用性结论） |
| B-02 拉黑 UX 对齐 | B-01 mention/群拉人/profile/search 三选项拍板 |
| D 链生产开关 `user_deletion_enabled` | owner |
| 12 provider DPA/SCC 签署（--overseas-gate 解阻） | owner/legal |
| retention-policy 8 类 pending-owner/legal-review 保留值翻转 | owner/legal |
| 海外发布门（--overseas-gate）是否自动挂 release.yml | owner |
