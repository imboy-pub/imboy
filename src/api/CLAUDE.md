# API 层文档 - HTTP REST API 处理器

[根目录](../CLAUDE.md) > **src/api**

> **最后更新**: 2026-02-01 | **模块数量**: 29 个
> **职责**: 处理 HTTP REST API 请求，参数验证，调用 Logic 层，返回标准 JSON 响应

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
        {"/ws", websocket_handler, #{}},
        {"/passport/login", passport_handler, #{action => login}},
        % ... 更多路由
    ],
    [{Host, MainRoutes}].
```

- 认证中间件：`auth_middleware.erl`、`auth_middleware_api_v1.erl`
- 开放路由（无需认证）定义在 `imboy_router:open/0`

---

## API 接口清单

### 用户相关

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `user_handler` | `/user/*` | 用户信息管理 |
| `passport_handler` | `/passport/*` | 登录注册 |
| `user_device_handler` | `/user_device/*` | 设备管理 |
| `user_collect_handler` | `/user_collect/*` | 用户收藏 |
| `user_denylist_handler` | `/friend/denylist/*` | 黑名单管理 |
| `user_tag_handler` | `/user_tag/*` | 用户标签 |
| `user_tag_relation_handler` | `/user_tag_relation/*` | 标签关系 |

### 好友 / 群组

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `friend_handler` | `/friend/*` | 好友管理 |
| `friend_category_handler` | `/friend/category/*` | 好友分组 |
| `group_handler` | `/group/*` | 群组管理 |
| `group_member_handler` | `/group_member/*` | 群成员管理 |
| `group_notice_handler` | `/group_notice/*` | 群公告 |

### 消息 / 其他

| Handler | 路由前缀 | 说明 |
|---------|---------|------|
| `msg_handler` | `/msg/*` | 消息处理 |
| `conversation_handler` | `/conversation/*` | 会话管理 |
| `websocket_handler` | `/ws` | WebSocket 连接 |
| `e2ee_handler` | `/v1/e2ee/*` | 端到端加密 |
| `e2ee_transfer_handler` | `/v1/e2ee/transfer/*` | E2EE 设备间传输 |
| `e2ee_social_handler` | `/v1/e2ee/social/*` | E2EE 社交恢复 |
| `location_handler` | `/location/*` | 位置服务 |
| `fts_handler` | `/fts/*` | 全文搜索 |
| `feedback_handler` | `/feedback/*` | 用户反馈 |
| `app_version_handler` | `/app_version/*` | 版本检查 |

---

## 依赖关系

| API Handler | 依赖 Logic |
|-------------|-----------|
| `passport_handler` | `passport_logic`, `auth_logic` |
| `user_handler` | `user_logic` |
| `friend_handler` | `friend_logic` |
| `group_handler` | `group_logic` |
| `msg_handler` | `msg_c2c_logic`, `msg_c2g_logic` |
| `websocket_handler` | `websocket_logic`, `msg_xxx_logic` |

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

## 文件清单（29 个）

```
src/api/
├── app_version_handler.erl      ├── auth_handler.erl
├── auth_middleware.erl           ├── auth_middleware_api_v1.erl
├── conversation_handler.erl     ├── e2ee_handler.erl
├── e2ee_social_handler.erl      ├── e2ee_transfer_handler.erl
├── feedback_handler.erl          ├── friend_category_handler.erl
├── friend_handler.erl            ├── fts_handler.erl
├── group_handler.erl             ├── group_member_handler.erl
├── group_member_transfer.erl    ├── group_notice_handler.erl
├── index_handler.erl             ├── live_room_stream_handler.erl
├── location_handler.erl          ├── msg_handler.erl
├── passport_handler.erl          ├── test_handler.erl
├── user_collect_handler.erl     ├── user_denylist_handler.erl
├── user_device_handler.erl      ├── user_handler.erl
├── user_tag_handler.erl          ├── user_tag_relation_handler.erl
└── websocket_handler.erl
```

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
