# 术语表（Terminology / Glossary）

> **Status**: CURRENT · **生效范围**: IMBoy 三端（imboy 后端 / imboyapp Flutter 客户端 / imboyadmin 管理后台）全部文档
> **维护规则**: 本文是全项目概念的唯一权威定义处。其他文档引用术语，不重复定义。术语与代码冲突时，先改代码事实对应的行，再同步本文。
> **UI 显示名**: 面向用户的文案（界面、i18n）以 `imboyapp/assets/i18n/` 与后端 `priv/terminology/` 为准；本文管的是文档与代码交流层的正式名称。

## 使用规则

1. 正文第一次出现的重要概念写「中文名称（English Name）」，之后可单用任一正式名。
2. 缩写（Org、CS、EB、E2EE 等）只用于代码标识符、表格和已标注缩写的场合；正式段落用全称。
3. 一个概念只有一个正式名称。别名栏里的词只用于引述历史文档或代码时。

## 1. 账号与身份

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 用户 | User | 表 `"user"`、`user_id` | 平台账号主体，登录与消息的最终归属。登录名存于 `account` 列（唯一）。 | — |
| 账号类型 | Account Type | `user.account_type` | 账号主体的分类枚举，见下表。 | — |
| 真人 | Human | `account_type = 0` | 自然人用户。组织所有者不变量、Agent 委托人等关键角色仅允许真人。 | — |
| 智能体 | Agent | `account_type = 1`，表 `ai_agent` | 平台 AI 账号：一等 user，拥有 provider/model/角色模板/触发策略，可入群对话。 | UI 显示名「AI 助手」；译法「代理」不再使用 |
| 系统机器人 | System Bot | `account_type = 2` | 平台内置服务账号，当前唯一形态是频道 Webhook Bot。 | — |
| Bot | Bot | `account_type = 3`，表 `bot`、`bot_delivery` | 开发者第三方服务账号：`@调用名`、API Token、Webhook 交付（outbox 重试模型）。**Bot 与智能体是两套体系，不混称。** | — |
| 设备 | Device | 表 `user_device` | 用户登录与 E2EE 密钥的设备级主体，持有 Olm 身份密钥与能力声明（capabilities）。 | — |
| 管理员 | Platform Admin | 表 `adm_user`/`adm_role`，路由 `/api/adm` | 管理后台运营者，独立于 `user` 体系的账号域（cookie 会话 + RBAC）。 | 「后台管理员」 |
| 管理后台 | Admin Console | 仓库 imboyadmin | 面向平台管理员的 Web 控制台。 | 「admin」泛称 |

## 2. 协作层级（组织 → 工作区 → 项目 / 群组 / 频道）

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 组织 | Organization | 表 `organization`，路由 `/api/v1/organizations` | SaaS 租户边界：owner、成员、部门、邀请、默认工作区的归属根。 | 缩写 Org 仅限代码标识符（`OrgId`、`:org_id`） |
| 组织成员 | Organization Member | 表 `organization_member`，role `owner/admin/member` | 组织内成员关系（Membership）的载体表。 | 「membership」不是表名 |
| 组织所有者 | Owner | `organization.owner_id` + 成员行 | 每组织恰好一名 active 真人 owner（数据库不变量，迁移 126/127）。 | — |
| 部门 | Department | 表 `organization_department(_member)` | 组织内目录树结构；部门管理员只是目录角色，不产生资源权限。 | — |
| 工作区 | Workspace | 表 `workspace(_member)`，路由 `/api/v1/workspaces` | 协作边界与资源容器：群组/频道/项目的归属层；角色 `owner/member/guest`（明确不扩展为通用 RBAC）。 | 缩写 ws 仅限代码 |
| 邀请码 | Workspace Invite Code | 表 `workspace_invite` | 8 位团队码加入工作区，一工作区至多一个有效码。 | — |
| 项目 | Project | 表 `project(_task/_member/_milestone/_channel_rel)` | 工作区内的结构化协作单元：任务（四态）、里程碑、成员、频道关联。**文档中「子项目/子仓」指代码仓库，与此业务实体无关。** | — |
| 群组 | Group | 表 `"group"`/`group_member`，路由 `/api/v1/group*` | 对等多人会话主体（C2G 消息的收件方）。群角色 0-5：成员/嘉宾/管理员/群主/副群主。 | 「群聊」指会话行为；「社群」弃用 |
| 频道 | Channel | 表 `channel_*`（10 张），路由 `/api/v1/channel*` | 订阅制非对称广播主体：订阅（subscription）而非成员制，支持付费订单与 webhook。**与「支持渠道（Support Channel，用户求助途径）」无关。** | — |
| 默认工作区 | Default Workspace | 表 `organization_default_workspace` | 每组织至多一个默认工作区引用（迁移 130）。 | — |
| 资源范围 | Resource Scope | `"group"/"channel".scope` = `personal/workspace` | 群组/频道两态归属：个人级或工作区级，创建后不可变（迁移 77）。 | — |

## 3. 消息

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 单聊消息 | C2C Message | 表 `msg_c2c`，WS type `c2c` | 客户端到客户端（一对一）消息。 | — |
| 群聊消息 | C2G Message | 表 `msg_c2g`，WS type `c2g` | 客户端到群组消息，扇出给群成员。 | — |
| 智能体消息 | C2S Message | 表 `msg_c2s`，WS type `c2s` | 客户端到智能体（AI）会话消息。 | — |
| 服务端推送 | S2C Message | 表 `msg_s2c`，WS type `s2c` | 服务端到客户端的系统通知。 | — |
| 会话 | Conversation | 表 `conversation` | 客户端会话列表聚合实体（单聊/群聊统一视图）。**与企业会话、客服会话是三个不同实体。** | — |
| 会话序号 | Conversation Sequence | `conv_seq`（`msg_store`/`msg_c2g_timeline`） | 服务端持久化的会话内权威递增序，离线补投与游标分页的基准。 | — |
| 时间线 | Timeline | 表 `msg_c2g_timeline` | 群消息离线投递时间线。 | — |
| 交付确认 | Delivery Ack | 表 `msg_delivery` | 按设备 ACK 的投递账本：行存在即该设备已确认；全设备确认后主行删除。 | — |
| 世代 | Generation | 表 `group_member_generation` | 群成员 E2EE 历史边界：同一成员每段连续在群期为一个 append-only 世代，重入群拿不到旧世代 Megolm 房间密钥。 | — |
| 阅后即焚 | Burn Message | `msg_burn_logic` | 定时焚毁消息。 | — |

## 4. 智能体域（Agent）

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 授权 | Grant | 表 `agent_grant(+workspace/capability/event)`（迁移 133） | 真人委托人授予智能体的最大可执行边界，默认拒绝（DEFAULT=DENY），能力约束只能收窄。 | 与「商业授权（License）」无关 |
| 委派 | Delegation | 概念 = Grant + 事件谱系（ADR-AG31-003）；字段 `delegator_user_id` | 真人向智能体让渡执行权这一行为的概念名；**不是独立代码实体**，代码定式是 Grant+event。 | delegator 译「委托人」 |
| 代理运行 | Agent Run | 表 `agent_run(+event)`、`agent_effect`（迁移 134） | 一次受 Grant 约束的智能体执行：八态状态机、租约（lease）、幂等键；事件与效果全 append-only。 | — |
| Hirð 运行时 | Hirð Runtime | 模块 `imboy_hird`/`imboy_hird_replay`，`agent_run.runtime_type` | Agent 执行的 ACL 桥与确定性重放器（fail-closed）。代码 ASCII 形态写 `hird`，文档可用 Hirð。 | `hirdir` 拼写不存在 |
| 效果 | Effect | 表 `agent_effect` | Run 内工具调用的 dispatch/reconcile 账本：dispatching 必须先于 adapter 调用持久化。 | — |
| 工具权限 | Tool Permission | `mcp_client_grant`（按 tool 粒度）、`agent_tool_authorizer` | MCP 客户端按工具名授权开关；智能体工具调用经 authorizer 决策。 | — |
| 支付授权 | Payment Mandate | 表 `agent_payment_mandate` | 智能体代付边界：单笔/周期总额上限，付款人恒为 owner。 | mandate 在 `/agent/mandate/*` 路由兼作授权 API 命名 |
| 人工介入 | Human-in-the-Loop, HITL | `agent_hitl_policy`、`agent_task._decision` | 工具调用中需真人审批的决策点，first-writer-wins 仲裁。 | — |
| 智能体任务 | Agent Task | 表 `agent_task(+event/_decision)` | 群内 @智能体 触发的任务流（九态），与 Agent Run、MCP task 三个账本绝不共表。 | — |

## 5. 客服域（Customer Service）

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 客服 | Customer Service | 模块前缀 `cs_`，表前缀 `customer_service_`，路由 `/api/v1/cs` | 客服子系统统称。缩写 CS 可用。 | — |
| 业务身份 | Business Identity | 表 `organization_business_identity`（迁移 114） | 组织的稳定经办主体：不可登录、不发 JWT、handover 不换 ID；`function_key` = `sales`/`customer_service`。 | — |
| 坐席 | Seat | 表 `customer_service_seat`，主键 = `business_identity_id` | 客服接待能力的运营属性（并发上限、启停），挂靠业务身份而非具体用户，换人后原地可用。 | — |
| 客服会话 | Customer Service Session | 表 `customer_service_session` | 访客与坐席的会话状态机（queued/active/closed）：不存消息副本，消息经企业会话真源；claim/transfer/close 全 CAS。 | — |
| 访客 | Visitor | 表 `customer_service_visit_token`、event `actor_kind='visitor'` | 外部咨询者：凭门店密钥签发的访问令牌接入，**不是 user 表账号、不占 License 配额**。 | — |
| 门店密钥 | Shop Key | 表 `customer_service_shop_key` | 商户侧接入密钥，只存 sha256 摘要，明文仅返回一次。 | — |
| 客服挂件 | Customer Service Widget | 表 `customer_service_widget_installation(_identity_key/_nonce)`（迁移 132） | 可嵌入第三方站点的聊天挂件：签名钥摘要化 + jti 防重放，路由 `/api/v1/cs/widget/*`。 | — |

## 6. 企业业务域（Enterprise Business）

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 企业业务 | Enterprise Business | 模块前缀 `eb_`，表前缀 `enterprise_`（12 张），路由 `/api/v1/enterprise` | 组织对外客户经营子系统：联系人/会话/消息/资产/留持/离岗交接。**与「商务版（Business Edition，分发版次）」是完全不同的概念。** | 缩写 EB 可用；勿与商务版混淆 |
| 企业联系人 | Enterprise Contact | 表 `enterprise_contact(_identity/_assignment)` | 组织拥有的客户关系（非个人好友链）：渠道标识只存 HMAC+掩码，支持幂等去重。 | — |
| 企业会话 | Enterprise Conversation | 表 `enterprise_conversation` | 组织与联系人的会话容器，消息落 `enterprise_message`，**不入个人 `msg_c2c`/ACK 清理链**。 | — |
| 企业消息 | Enterprise Message | 表 `enterprise_message(_delivery)` | 企业会话消息：托管密文（`body_cipher`+密钥版本）、保留期快照、送达账本。 | — |
| 企业资产 | Enterprise Asset | 表 `enterprise_asset` | 企业域附件：presign/confirm 直传 + 密文元数据。 | — |
| 留持 | Retention Hold | 表 `enterprise_retention_policy/_hold` | 合规保留策略与法定留持：hold 阻断定期清除；`retain_until` 只能后移。 | — |
| 离岗交接 | Offboarding | 表 `enterprise_offboarding_case(_item)` | 成员离职时的资产/会话移交状态机；与组织成员移除守卫并存。 | — |
| 审计事件 | Audit Event | 表 `enterprise_audit_event` | 企业域 append-only 审计真源（该模式始祖，客服/Grant 事件复用）。 | — |

## 7. 安全与加密（E2EE）

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 端到端加密 | End-to-End Encryption | `e2ee_*` 模块/表/路由 | 服务器不可读的消息加密体系总称。缩写 E2EE 正式可用。 | — |
| Olm | Olm (X3DH + Double Ratchet) | 表 `olm_identity/_one_time_key/_fallback_key` | 单聊 E2EE 协议：服务端只存公钥侧；OTK claim 即删。 | — |
| Megolm | Megolm | 表 `e2ee_group_session_attestation(_member)`；`msg_store` 会话 scope 值 | 群聊 E2EE 协议：服务端只存会话溯源（标识+成员世代），永不存房间密钥明文。 | — |
| vodozemac | vodozemac | imboyapp `flutter_vodozemac ^0.8.1` | 客户端 Olm/Megolm 的 Rust 密码库实现（Matrix vodozemac）。后端无此依赖。 | — |
| 房间密钥 | Room Key | WS action `e2ee_room_key` | Megolm 群会话密钥帧：经服务器**不透明中转**（迁移与 WS 路由注册，c2c+c2g 两路）。 | — |
| 设备信任 | Device Trust | 表 `trust_audit`，`user_device.trust_state` | 设备身份核验状态（unverified/verified/revoked）与带签名的信任决策审计。 | — |
| 密钥备份 | Key Backup (4S) | 表 `e2ee_key_backups`，路由 `/api/v1/e2ee/backup/*` | 零信任云端密文备份：服务端只存密文 + KDF 参数（PBKDF2 迭代 ≥100000，客户端建议 310000）。 | 「4S」别名可用 |
| 安全码 | Safety Number | imboyapp `safety_number*.dart` | 设备身份指纹的可读比对物。 | — |
| 合规密钥 | Compliance Key | 表 `compliance_key`，三层加密 | 受监管场景的合规密钥分发（RSA-OAEP-256；私钥列已移除）。 | — |

## 8. 平台与运维

| 中文 | English | 代码形态 | 定义 | 别名 / 弃用 |
|---|---|---|---|---|
| 商业授权 | License | `src/lib/imboy_license`（无独立数据表） | 规模/能力授权（无 License 即社区版形态）。**与 Grant（技术授权）严格区分，文档中「授权」默认指 Grant，商业语境写「License」。** | — |
| 计费 | Billing | 表 `billing_plan/_subscription/_invoice/_usage` | 订阅计划/配额/账单/用量。 | — |
| 错误码 | Error Code | `include/error_code.hrl`（整数：0 成功、1 通用、4xx/5xx/9xx 区间） | 统一响应信封 `{code: integer, msg, payload}` 的 code。**整数是唯一现行形态。** | 字符串码（如 `"USER_NOT_FOUND"`）为弃用设计，从未在现行代码落地 |
| 管理面 | Admin API | 路由 `/api/adm/*`，`adm_*_handler` | 平台管理员专用 API 面。 | — |
| 特性开关 | Feature Flag / Product Feature | `product-feature-manifest.json`、`make feature-smoke` | 编译期/运行期产品特性裁剪与开关体系。 | — |
| 插件 | Plugin | `imboy_plugin_*`、`priv/plugins/` | 签名插件动态加载体系（可注册路由与 WS action）。 | — |
| MCP | Model Context Protocol | `src/mcp/`（vendored barrel_mcp）、`mcp_*` 表 | 智能体工具调用协议引擎与客户端治理。 | — |
| 墨芽 | Moya (Calligraphy) | `moya_*_handler`，路由 `/api/v1/moya/*` | 微信小程序书法教学子系统（班级/作业/AI 回课），独立业务线。 | — |

## 9. 代码命名前缀约定（供文档引用代码时对照）

| 前缀 | 含义 | 例 |
|---|---|---|
| `imboy_` | 平台基础设施/核心设施模块 | `imboy_syn`、`imboy_license` |
| `elib_` | 通用工具库 | `elib_tsid`、`elib_response` |
| `adm_` | 管理后台 API 面 | `adm_acl` |
| `cs_` | 客服域模块（表名用 `customer_service_` 前缀） | `cs_seat_app` |
| `eb_` | 企业业务域模块（表名用 `enterprise_` 前缀） | `eb_pg_session` |
| `agent_` | 智能体域（features/agent 切片内） | `agent_run_fsm` |
| `*_logic / *_ds / *_repo` | 分层：业务逻辑 / 数据服务 / SQL 执行 | `group_logic` |
| `*_handler` | Cowboy HTTP/WS 入口 | `workspace_handler` |

## 10. 易混清单（写作时必读）

| 冲突对 | 裁定 |
|---|---|
| 智能体 / AI 助手 / 代理（Agent） | 文档正文「智能体（Agent）」；「AI 助手」仅指客户端 UI 显示名；「代理」弃用（与 HTTP proxy 语义冲突）。 |
| 企业业务（EB） vs 商务版（Business Edition） | 前者是 Enterprise Business 功能域；后者是分发/许可版次。英文都含 business，凡译英文必须写全称。 |
| 授权（Grant） vs 商业授权（License） | 技术语境「授权」= Grant；商业语境写「License」。 |
| 频道（Channel） vs 支持渠道（Support Channel） | Channel 特指订阅制广播实体；用户求助途径统一写「支持渠道」。 |
| 群组 / 群聊 / 社群（Group） | 实体名「群组」；「群聊」仅描述会话行为；「社群」弃用。 |
| 委派 / 委托（Delegation） | 概念名统一「委派」；delegator 译「委托人」时指代码字段语义主体。 |
| Hirð / hird | 文档可写 Hirð；代码标识符恒为 ASCII `hird`（`imboy_hird`）。 |
| 会话（Conversation）三义 | 个人「会话」、企业会话（Enterprise Conversation）、客服会话（CS Session）是三个实体，不得混写。 |
| 项目（Project） vs 子项目/子仓 | 业务实体 Project ≠ 代码仓库划分。涉及仓库时写「仓库」。 |

## 11. 与机器术语体系的关系

- 客户端展示名（如 group → 群组）的机器可读映射在 `imboy/priv/terminology/`（`product_terminology.erl` 启动期校验，`make terminology-check` 门禁）。
- i18n 文案术语见 `imboyapp/I18N_TERMINOLOGY.md`。
- 本文是三端文档层的唯一权威；上述两处是展示层，与本文冲突时以本文 + 代码为准并双向同步。
