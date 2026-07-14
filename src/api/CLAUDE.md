# API 层文档 - HTTP REST API 处理器

[根目录](../CLAUDE.md) > **src/api** | 54 个模块 | 职责：处理 HTTP REST API 请求、参数验证、调用 Logic 层

> **最后更新**: 2026-06-10 | **计数**: 以 `find src/api -maxdepth 1 -name '*.erl' | wc -l` 为准（截至 2026-06）

---

## 模块职责

API 层负责：HTTP REST 请求入口、请求参数验证与权限控制、调用 Logic 层、WebSocket 连接管理、返回标准 JSON 响应。

---

## 路由定义（src/imboy_router.erl）

```erlang
get_routes() ->
    Host = config_ds:env(host, '_'),
    MainRoutes = [
        {"/", index_handler, #{action => help}},
        {"/api/ws", websocket_handler, #{}},
        {"/api/passport/login", passport_handler, #{action => login}},
        % ... 更多路由（2026-07-07 43224c1f/4cc20e81 硬切换后全部路由统一
        % /api 前缀，仅根路径 "/" 及网站白名单如 /help、/brand 等保留裸路径）
    ],
    [{Host, MainRoutes}].
```

- 认证中间件：`auth_middleware.erl`、`auth_middleware_api_v1.erl`
- 开放路由（无需认证）定义在 `imboy_router:open/0`

---

## API 接口清单

### 用户与认证

> 2026-07-08：v0 裸 `/api/*` 业务路由（不带 v1 段）已下架，全部收口到
> `/api/v1/*`；`/api/adm/*` 保持原样（无 v1 版本，设计如此）。除根路径
> `/` 与网站白名单（`/help`、`/brand`、`/privacy-policy`、
> `/account-deletion`、`/metrics`）外，其余路由均为 `/api/v1/*` 或 `/api/adm/*`。

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `passport_handler` | `/api/passport/*` | 登录、注册、口令认证 |
| `qr_login_handler` | `/api/v1/passport/qr_login/*` | QR 码扫码登录 |
| `qr_login_sse_handler` | `/api/v1/passport/qr_login/subscribe` | QR 登录 SSE 长连接 |
| `user_handler` | `/api/user/*` | 用户信息查询与修改 |
| `user_device_handler` | `/api/user_device/*` | 设备登录与管理 |
| `user_collect_handler` | `/api/user_collect/*` | 用户收藏功能 |
| `user_denylist_handler` | `/api/friend/denylist/*` | 黑名单管理 |
| `user_tag_handler` | `/api/user_tag/*` | 用户标签管理 |
| `user_tag_relation_handler` | `/api/user_tag_relation/*` | 用户标签关系 |
| `auth_handler` | `/api/auth/*` | 认证授权处理 |

### 好友 & 社交

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `friend_handler` | `/api/friend/*` | 好友关系与列表管理 |
| `friend_category_handler` | `/api/friend/category/*` | 好友分组管理 |
| `mention_handler` | `/api/mention/*` | 消息@提及用户 |
| `moment_handler` | `/api/moment/*` | 动态/朋友圈 |

### 群组管理

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `group_handler` | `/api/group/*` | 群组核心操作 |
| `group_member_handler` | `/api/group_member/*` | 群成员管理 |
| `group_notice_handler` | `/api/group_notice/*` | 群公告管理 |
| `group_file_handler` | `/api/group_file/*` | 群文件管理 |
| `group_album_handler` | `/api/group_album/*` | 群相册管理 |
| `group_category_handler` | `/api/group_category/*` | 群分类管理 |
| `group_tag_handler` | `/api/group_tag/*` | 群内标签管理 |
| `group_schedule_handler` | `/api/group_schedule/*` | 群日程管理 |
| `group_task_handler` | `/api/group_task/*` | 群任务管理 |
| `group_vote_handler` | `/api/group_vote/*` | 群投票与民主决策 |

### 消息与实时

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `msg_handler` | `/api/msg/*` | 消息发送、撤回、转发等 |
| `conversation_handler` | `/api/conversation/*` | 会话管理 |
| `websocket_handler` | `/api/ws`、`/api/v1/ws` | WebSocket 长连接与实时投递 |
| `location_handler` | `/api/location/*` | 位置分享与地理位置服务 |

### 隐私与加密

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `e2ee_handler` | `/api/v1/e2ee/*` | 端到端加密(E2EE)密钥管理 |

### 应用与内容

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `app_version_handler` | `/api/app_version/*` | 应用版本检查与更新 |
| `app_feature_handler` | `/api/app_feature/*` | 应用功能特性与配置 |
| `app_manifest_handler` | `/api/app_manifest/*` | 应用清单与元数据 |
| `app_upgrade_log_handler` | `/api/app_upgrade_log/*` | 应用升级日志 |
| `fts_handler` | `/api/fts/*` | 全文搜索 |
| `feedback_handler` | `/api/feedback/*` | 用户反馈与问题报告 |
| `report_handler` | `/api/report/*` | 用户举报与内容审核 |
| `wallet_handler` | `/api/wallet/*` | 钱包与余额管理 |
| `billing_handler` | `/api/v1/billing/*` | SaaS 计费：套餐管理 + 租户订阅/用量/配额/账单 |

### 频道（Channel）

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `channel_handler` | `/api/channel/*` | 频道内容 HTTP 适配器 |
| `channel_handler_message` | `/api/channel/*/msg/*` | 频道消息与反应 |
| `channel_handler_admin` | `/api/channel/*/admin/*` | 频道管理员操作 |
| `channel_handler_order` | `/api/channel/*/order/*` | 频道付费订单处理 |

### 直播与其他

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `live_room_handler` | `/api/live_room/*` | 直播间管理 |
| `attach_handler` | `/api/v1/attachment/*` | 文件上传 Presigned URL |

### 基础设施 & 中间件

| 中间件 | 职责 |
|---------|------|
| `auth_middleware` | 通用认证中间件 |
| `auth_middleware_api_v1` | `/api/*` 路由认证中间件（含设备签名 verify_sign 校验） |
| `cors_middleware` | CORS 跨域资源共享 |
| `security_headers_middleware` | 安全响应头（XSS、点击劫持防护） |
| `throttle_middleware` | 基于 UID/IP 的限流 |

### 运维与测试

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `index_handler` | `/`、`/api/init`、`/api/v1/init` | 首页/根路由信息展示 + 客户端初始化探针 |
| `metrics_handler` | `/metrics`、`/api/metrics`、`/api/v1/metrics` | Prometheus 可观测性指标（裸路径为网站白名单） |
| `test_handler` | `/api/test/*` | 测试与调试端点（仅非生产环境注册） |

---

## 依赖关系

> 下表为各 handler 的**直接依赖模块**（Logic 层 + 跨层基础设施 DS：`auth_ds`/`config_ds`/`token_ds`），由 `scripts/check_module_boundaries.sh` 实测校验，截至 2026-06。Handler 不得直接依赖 `*_repo`（分层违规）。

| API Handler | 直接依赖模块 |
|-------------|-----------|
| `passport_handler` | `passport_logic`, `user_logic`, `config_ds`, `token_ds` |
| `user_handler` | `user_logic`, `friend_logic`, `auth_ds`, `config_ds` |
| `friend_handler` | `friend_logic`, `user_logic`, `auth_ds` |
| `group_handler` | `group_logic`, `group_member_logic`, `auth_ds`, `config_ds` |
| `msg_handler` | `messaging_logic`, `msg_forward_logic`, `msg_pinned_logic` |
| `websocket_handler` | `websocket_logic`, `message_router_logic`, `msg_c2c_logic`, `msg_c2g_logic`, `msg_s2c_logic`, `app_version_logic`, `user_logic`, `websocket_ds`, `auth_ds` |

基础库依赖：`elib_req`（参数解析）、`elib_response`（响应格式化）、`elib_cnv`（ID 转换）

---

## 请求/响应模式

```erlang
% 解析请求体
{ok, Body} = elib_req:body(Req, []),
Uid = proplists:get_value(current_uid, State),

% 成功响应
elib_response:success(Req, #{<<"nickname">> => Nickname})

% 错误响应
elib_response:error(Req, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)

% 分页参数
{Page, PageSize} = elib_param:page(Body, #{page => 1, page_size => 20})
```

---

## 文件清单（53 个）

| # | Handler 模块 | 说明 |
|---|---|---|
| 1 | `app_feature_handler` | 应用功能特性配置（ICE、功能开关等） |
| 2 | `app_manifest_handler` | 应用清单和元数据管理 |
| 3 | `app_upgrade_log_handler` | 应用升级日志记录 |
| 4 | `app_version_handler` | 应用版本检查与更新通知 |
| 5 | `attach_handler` | 附件/文件上传 Presigned URL 生成 |
| 6 | `auth_handler` | 认证授权处理（与 auth_logic 配合） |
| 7 | `auth_middleware` | 通用认证中间件 |
| 8 | `auth_middleware_api_v1` | /v1 路由认证中间件 |
| 9 | `channel_handler` | 频道内容 HTTP 适配器 |
| 10 | `channel_handler_admin` | 频道管理员操作（邀请、同步等） |
| 11 | `channel_handler_message` | 频道消息、置顶、反应处理 |
| 12 | `channel_handler_order` | 频道付费订单与交易处理 |
| 13 | `conversation_handler` | 会话管理接口 |
| 14 | `cors_middleware` | CORS 跨域资源共享中间件 |
| 15 | `e2ee_handler` | 端到端加密(E2EE)用户密钥管理 |
| 18 | `feedback_handler` | 用户反馈与问题报告 |
| 19 | `friend_category_handler` | 好友分组管理 |
| 20 | `friend_handler` | 好友关系与好友列表管理 |
| 21 | `fts_handler` | 全文搜索接口 |
| 22 | `group_album_handler` | 群相册管理 |
| 23 | `group_category_handler` | 群分类管理 |
| 24 | `group_file_handler` | 群文件管理 |
| 25 | `group_handler` | 群组核心操作（创建、查询、退群等） |
| 26 | `group_member_handler` | 群成员管理与身份操作 |
| 28 | `group_notice_handler` | 群公告发布与查看 |
| 29 | `group_schedule_handler` | 群日程与时间管理 |
| 30 | `group_tag_handler` | 群内标签管理 |
| 31 | `group_task_handler` | 群任务与待办事项管理 |
| 32 | `group_vote_handler` | 群投票与民主决策（插件控制） |
| 33 | `index_handler` | 首页/根路由信息展示 |
| 34 | `live_room_handler` | 直播间创建、开始/停止、查询 |
| 35 | `location_handler` | 位置分享与地理位置服务 |
| 36 | `mention_handler` | 消息@提及用户处理 |
| 37 | `metrics_handler` | Prometheus 可观测性指标导出 |
| 38 | `moment_handler` | 动态/朋友圈相关接口 |
| 39 | `msg_handler` | 消息发送、撤回、转发等核心消息处理 |
| 40 | `passport_handler` | 登录、注册、口令认证 |
| 41 | `qr_login_handler` | QR 码扫码登录处理器（WhatsApp Web 风格） |
| 42 | `qr_login_sse_handler` | QR 登录 SSE 长连接（Server-Sent Events） |
| 43 | `report_handler` | 用户举报与内容审核 |
| 44 | `security_headers_middleware` | 安全响应头中间件（XSS、点击劫持防护） |
| 45 | `test_handler` | 测试与调试端点 |
| 46 | `throttle_middleware` | 基于 UID/IP 的限流中间件 |
| 47 | `user_collect_handler` | 用户收藏与收集功能 |
| 48 | `user_denylist_handler` | 黑名单管理 |
| 49 | `user_device_handler` | 设备登录与管理 |
| 50 | `user_handler` | 用户信息查询、修改、资料卡等 |
| 51 | `user_tag_handler` | 用户标签管理 |
| 52 | `user_tag_relation_handler` | 用户标签关系处理 |
| 53 | `wallet_handler` | 钱包与余额管理 |
| 55 | `billing_handler` | SaaS 计费：套餐 CRUD + 订阅/续费/取消 + 用量上报/配额查询 + 账单生成/支付 |
| 54 | `websocket_handler` | WebSocket 长连接与实时消息投递 |

---

## 测试文件（50+ 个）

`test/api/` 目录包含所有 handler 对应的 `_tests.erl` 文件，覆盖：
`passport`, `user`, `friend`, `group`, `msg`, `conversation`, `websocket_logic`, `e2ee`, `fts`, `location`, `feedback` 等。

---

## 测试配置

- 框架：EUnit；超时：30s；环境：`application:set_env(imboy, env, test)`

## 操作指南

- **添加新端点**：`src/api/` 建 handler → `src/imboy_router.erl` 加路由 → `src/logic/` 建 logic → 写测试
- **文件上传**：使用 `cowboy_req:read_part/2`
- **WebSocket 调试**：`http://coolaf.com/tool/chattest`
