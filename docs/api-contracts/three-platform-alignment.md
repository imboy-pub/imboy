# 三端 API 对齐（Three-Platform API Alignment）

> Purpose：一张表回答「后端 API 面、Flutter 客户端（imboyapp）、管理后台（imboyadmin）三者谁消费什么、约定差异在哪」，并登记**契约漂移（Contract Drift）**。
> 机器真源：后端路由 `src/imboy_router.erl`；契约产物 `.contract/api_contract.json`（当前 758 endpoints：adm 282 / api_v1 465 / main 8 / test_dev_only 3）。本文做人工导航与漂移登记，不复制端点全清单。

## 1. 三条 API 面与认证模型

| API 面 | 路径前缀 | 消费方 | 认证 | 凭证形态 |
|---|---|---|---|---|
| 客户端面 | `/api/v1/*` | imboyapp、客服挂件（仅 `/cs/widget/*`）、墨芽小程序（`/moya/*` 子集免 Bearer） | JWT Bearer（`auth_ds`/`token_ds` 签发校验） | `Authorization: Bearer`；WS 握手同源校验 |
| 管理面 | `/api/adm/*` | imboyadmin | cookie 会话 + CSRF（三层：csrf_token 参数 + SameSite=Lax + `X-Requested-With`） | RSA-OAEP 加密密码登录 |
| 挂件面 | `/api/v1/cs/widget/*` | 嵌入第三方站点的挂件 | 访问令牌 | `x-cs-visit-token` 头（禁止 URL 携带）+ 每请求申报 `organization_id` |

**纪律**（imboyadmin 模块头注释，CS-03-A01）：admin 模块严禁接入 `/api/v1`，唯一例外是挂件应用。

## 2. 域 × 端 消费矩阵

图例：●=主要消费端；○=局部/只读消费；—=不消费。

| 业务域 | 后端路由组 | imboyapp | imboyadmin | 备注 |
|---|---|---|---|---|
| 认证/护照 | `/passport` `/auth` `/refreshtoken` | ● | —（admin 独立登录） | App 支持 QR 扫码 + OIDC SSO |
| 用户/设备 | `/user` `/user_device` | ● | ○（治理） | |
| 好友/联系人 | `/friend` | ● | — | |
| 会话/消息 | `/conversation` `/msg` `/mention` `/fts` | ● | ○（消息检索） | |
| 群组 | `/group*`（57 条） | ● | ●（治理 38 条 adm） | |
| 频道 | `/channel*`（46 条） | ● | ●（20 条 adm） | |
| 朋友圈 | `/moment*` | ● | ○ | |
| 组织 | `/organizations`（26 条） | ●（租户自助） | ●（平台治理 16+2 条） | App 动作名 `offboard` ↔ Admin 动作名 `remove`，同义 |
| 工作区 | `/workspaces`（19 条） | ● | ○（adm 5 条） | |
| 项目 | `/projects`（16 条） | ● | ○ | |
| 钱包/支付 | `/wallet` + 支付回调 | ● | ○（财务面） | |
| 计费 | `/billing`（10 条） | ● | ●（`finance/billing/*`） | |
| E2EE | `/e2ee` `/e2ee/olm`（21 条） | ● | —（合规密钥在 settings） | 密码学只在客户端 |
| 附件 | `/attachment` | ● | ○（存储治理 adm 8 条） | Garage S3 presign/confirm |
| AI 助手发现 | `/agent/list` `/agent-card` | ● | — | |
| 智能体任务 | `/agent_task/*` `/agent/mandate/*` | ●（审批） | ○（mandate 管理） | |
| 智能体执行（Grant/Run） | features/agent handler | — | — | **后端已实现、两端暂无消费 UI**（见漂移 #3） |
| 客服（租户面） | `/cs/*`（24 条） | ●（坐席工作台） | — | 坐席读写复用 enterprise 端点 |
| 客服（平台面） | adm `customer-service/*` | — | ● | 每条必带 `workspace_id` |
| 客服挂件 | `/cs/widget/*` | — | ●（构建产物 dist-widget） | 独立 iframe 应用 |
| 企业业务（租户面） | `/enterprise/*`（19 条） | ● | — | |
| 企业业务（平台面） | adm `enterprise-business/*` | — | ●（只读+交接执行） | |
| MCP 治理 | adm `mcp/*` | — | ● | |
| Bot 管理 | `/bot*` + adm | — | ● | |
| 墨芽教学 | `/moya/*`（18 条） | — | — | 消费方是独立微信小程序（外部仓），App/Admin 均不接 |
| 直播/RTC | `/live_room` `/rtc/room/join` | ● | — | LiveKit |

## 3. 跨端数据约定

| 约定 | 内容 | 三端落点 |
|---|---|---|
| ID 传输 | TSID（64-bit）JSON 中一律字符串 | 后端 `binary_to_integer` 解码；App `EntityId=String`；Admin `safeParseBigIntJson` 自动加引号 + `EntityId` |
| 响应信封 | `{code: integer, msg, payload}`（admin 侧另有 `sv_ts`）；`code=0` 成功 | 后端 `elib_response:reply_json/4`；App `HttpResponse`；Admin `ApiResponse`（`code!==0` reject） |
| 错误码 | 整数区间 0/1/4xx/5xx/9xx，真源 `include/error_code.hrl` | 客户端不做 message 文本分支 |
| 分页 | 常规列表 page/size 偏移分页；高基数/会话流键集分页（after_id） | Admin `DataTablePagination`（默认 size=10）+ `CursorPaginationBar`；App 列表页 + CS 队列键集 |
| 时间 | UTC 毫秒 ISO-8601 | 见 [CONVENTIONS](../CONVENTIONS.md) |
| WS action | `snake_case`，注册于 `imboy_ws_action_registry` | App 侧常量真源 `lib/service/message_type_constants.dart` |
| CAS 乐观锁 | 写操作带 `expected_version` | CS 会话/坐席、org 部门 |

## 4. 契约漂移登记簿（Contract Drift Registry）

> 只登记，不改代码。每条注明影响与建议处理方。

| # | 漂移 | 证据 | 影响 | 建议 |
|---|---|---|---|---|
| 1 | Admin 组织证据文档仍写 `/api/v1/.../offboard`，代码已收敛为 adm 面 `members/:uid/remove` | imboyadmin `docs/plans/evidence/enterprise-organization-v1/ORG-ADMIN-WIRING/`（历史证据，模块头注释已声明以代码为准） | 低（证据文档定位即历史快照） | 保留，不再新写 offboard 旧路径 |
| 2 | Admin 前端内置角色 '4','5','6' 出现在路由门但无名称映射 | `src/components/shared/AdminProfilePanel.tsx` 仅映射 1/2/3 | 中（权限语义只存在于后端 adm_role 种子/文档，UNKNOWN：前端语义缺失） | 后端补角色名下发或前端补映射（代码任务） |
|   | ↑ 2026-09-19 复核实证：**内置角色语义不在代码层**——`adm_role` 建表迁移（00000001）零种子数据，`adm_acl` 动态解析 role_id→权限集，无 1-6 号角色的代码级定义；前端 1=超级/2=运营/3=审计 的名称映射是硬编码约定（对应运行库数据），4/5/6 连约定都没有。定性从「文档缺失」改为「角色语义属各部署实例的运行数据」；若需三端统一须后端角色名下发（代码任务，长期项） | `priv/migrations/00000001_foundation.up.sql:3643`、`src/adm/adm_acl.erl:83-118` | | |
| 3 | 后端 Grant/Run API（迁移 133/134）无任何客户端消费 | 消费矩阵上行；App 仅 `/agent/list`+`agent_task` | 无（分阶段交付设计） | 客户端接入前保持矩阵状态 |
| 4 | 客户端 App 本地 `contact.account_type` 只建模 0/1/2 三值，类型 3 Bot 无独立模型 | imboyapp SQLite v23 迁移注释；`BotBadge` 仅徽章 | 低（Bot 对 App 用户不可见为现状设计） | 若开放 Bot 生态需三端同步 |
| 5 | 权限字符串域前缀 snake（`enterprise_business:read`）与路由/菜单 kebab（`/enterprise-business`）风格不一致 | imboyadmin 模块代码 | 低（约定问题） | 在 CONVENTIONS 增加一条映射规则（本文已记录） |
| 6 | imboyadmin `docs/legal/` 残留 `MulanPSL-2.0.txt` 与 LICENSE（BUSL-1.1）不一致 | imboyadmin 仓 | 中（法务材料口径） | 删除或替换法务文件（非 markdown，本审计不動，转交维护者） |
| 7 | WS S2C action 清单的服务端注册表与 App 常量表为两处人工同步 | `imboy_ws_action_registry` vs imboyapp `message_type_constants.dart` | 中（新增 action 靠纪律） | 建议纳入契约导出（代码任务） |
|   | ↑ 2026-09-19 复核实证（修正原表述——实际是**三方且下行无注册表**）：①上行 action 有单一真源 `imboy_ws_action_registry`（内置 9 个：message_{revoke,edit,read}(_ack)+input 七个 + `e2ee_room_key` c2c/c2g 两路，registry:69-70）；②下行应答类 action 以 `message_ds:assemble_s2c` 字面量分散在 43 处调用（16 个错误/控制 action 如 peer_offline/rate_limited/permission_denied）；③业务事件推送（group_*/channel_*/moment_* 等）分散在各业务 logic、形态不一；App 侧 41 个 action 常量单侧维护。**三方无机器对账是结构性缺口**，建议随 contract-export 一并解决（代码任务，长期项） | `src/lib/imboy_ws_action_registry.erl`、`grep -r assemble_s2c src/`（43 处）、imboyapp `message_type_constants.dart` | | |
| 8 | **错误码生成物滞后 3 项（2026-09-19 实证）**：后端 `include/error_code.hrl` 188 个错误码中，Flutter 生成物 `lib/config/error_code.dart` 缺 3 个——`ERR_TOKEN_EXPIRED_REFRESHABLE(705)`、`ERR_TOKEN_MALFORMED(706)`（CHANGELOG alpha.77 新增对，生成器未重跑）与 `ERR_TEACHING_ORG_OWNER_REQUIRED`；其余 185 项两侧名称/值一致（含 ERR_ 前缀剥离规则）。影响：客户端只能硬编码 705/706 或走默认分支。与历史 C-20 事故（生成器漂移 7 个月）同款模式；生成器自带 `--check` 校验模式但未见门禁强制 | `include/error_code.hrl` vs imboyapp `lib/config/error_code.dart`（文件头注明生成命令） | 中（客户端 token 错误处理分支缺失） | 代码任务：imboyapp 重跑 `dart scripts/generate_error_code.dart` 并考虑把 `--check` 接入 CI |

## 5. 三端职责一句话

- **imboy（后端）**：全部业务真源——路由、迁移、协议、计费、部署。
- **imboyapp（Flutter）**：真人用户体验全量域 + 坐席工作台 + 企业会话移动端 + 全部端侧密码学。
- **imboyadmin（React）**：平台运营治理面（用户/内容/财务/AI 助手/Bot/MCP/组织/客服/企业业务）+ 客服挂件构建产物。
