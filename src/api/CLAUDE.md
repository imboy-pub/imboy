# API 层文档 - HTTP REST API 处理器

[根目录](../CLAUDE.md) > **src/api**

> **最后更新**: 2026-01-20 08:48:18 CST
> **模块数量**: 27 个
> **职责**: 处理 HTTP REST API 请求，参数验证，调用 Logic 层处理业务逻辑

---

## 模块职责

API 层是 Imboy 系统的入口层，负责：
- 处理 HTTP REST API 请求
- 验证请求参数和权限
- 调用 Logic 层处理业务逻辑
- 返回标准格式的 JSON 响应
- WebSocket 连接管理

---

## 入口与启动

### 路由定义

所有 API 路由在 `src/imboy_router.erl` 中定义：

```erlang
% 主路由
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

### 认证中间件

- **主中间件**: `src/api/auth_middleware.erl`
- **API v1 中间件**: `src/api/auth_middleware_api_v1.erl`

开放路由（无需认证）定义在 `imboy_router:open/0`。

---

## 对外接口

### 用户相关 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `user_handler.erl` | `/user/*` | 用户信息管理 |
| `passport_handler.erl` | `/passport/*` | 登录注册 |
| `user_device_handler.erl` | `/user_device/*` | 设备管理 |
| `user_collect_handler.erl` | `/user_collect/*` | 用户收藏 |
| `user_denylist_handler.erl` | `/friend/denylist/*` | 黑名单管理 |
| `user_tag_handler.erl` | `/user_tag/*` | 用户标签 |
| `user_tag_relation_handler.erl` | `/user_tag_relation/*` | 标签关系 |

### 好友相关 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `friend_handler.erl` | `/friend/*` | 好友管理 |
| `friend_category_handler.erl` | `/friend/category/*` | 好友分组 |

### 群组相关 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `group_handler.erl` | `/group/*` | 群组管理 |
| `group_member_handler.erl` | `/group_member/*` | 群成员管理 |
| `group_notice_handler.erl` | `/group_notice/*` | 群公告 |

### 消息相关 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `msg_handler.erl` | `/msg/*` | 消息处理 |
| `conversation_handler.erl` | `/conversation/*` | 会话管理 |

### 其他 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `websocket_handler.erl` | `/ws` | WebSocket 连接 |
| `location_handler.erl` | `/location/*` | 位置服务 |
| `fts_handler.erl` | `/fts/*` | 全文搜索 |
| `feedback_handler.erl` | `/feedback/*` | 用户反馈 |
| `app_version_handler.erl` | `/app_version/*` | 版本检查 |
| `e2ee_handler.erl` | `/v1/e2ee/*` | 端到端加密 |
| `test_handler.erl` | `/test/*` | 测试接口 |

---

## 关键依赖与配置

### 依赖的 Logic 模块

| API Handler | 依赖的 Logic |
|-------------|-------------|
| `passport_handler` | `passport_logic`, `auth_logic` |
| `user_handler` | `user_logic` |
| `friend_handler` | `friend_logic` |
| `group_handler` | `group_logic` |
| `msg_handler` | `msg_c2c_logic`, `msg_c2g_logic` |
| `websocket_handler` | `websocket_logic`, `msg_xxx_logic` |

### 依赖的基础库

- `elib_req.erl`: 请求参数解析
- `elib_response.erl`: 响应格式化
- `elib_hashids.erl`: ID 编码/解码
- `auth_middleware.erl`: 认证中间件

---

## 数据模型

### 请求参数解析

使用 `elib_req:body/2` 解析请求体：

```erlang
% 解析 JSON 请求体
{ok, Body} = elib_req:body(Req, []),
Uid = proplists:get_value(current_uid, State),
Nickname = maps:get(<<"nickname">>, Body, <<>>).
```

### 响应格式

成功响应：
```erlang
elib_response:success(Req, #{<<"nickname">> => Nickname})
```

错误响应：
```erlang
elib_response:error(Req, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)
```

---

## 测试与质量

### 测试文件位置

```
test/api/
├── passport_handler_tests.erl
├── user_handler_tests.erl
├── friend_handler_tests.erl
├── group_handler_tests.erl
├── msg_handler_tests.erl
└── ...
```

### 测试配置

- **超时**: 30 秒
- **环境标记**: `application:set_env(imboy, env, test)`
- **测试框架**: EUnit

---

## 常见问题 (FAQ)

### Q: 如何添加新的 API 端点?

1. 在 `src/api/` 创建新的 handler 文件
2. 在 `src/imboy_router.erl` 添加路由
3. 在 `src/logic/` 创建对应的 logic 文件
4. 编写测试

### Q: 如何处理文件上传?

使用 Cowboy 的 `cowboy_req:read_part/2` API。

### Q: 如何实现分页?

使用 `elib_param:page/2` 解析分页参数：

```erlang
{Page, PageSize} = elib_param:page(Body, #{page => 1, page_size => 20})
```

---

## 相关文件清单

### Handler 文件 (27 个)

```
src/api/
├── app_version_handler.erl
├── auth_handler.erl
├── auth_middleware.erl
├── auth_middleware_api_v1.erl
├── conversation_handler.erl
├── e2ee_handler.erl
├── feedback_handler.erl
├── friend_category_handler.erl
├── friend_handler.erl
├── fts_handler.erl
├── group_handler.erl
├── group_member_handler.erl
├── group_member_transfer.erl
├── group_notice_handler.erl
├── index_handler.erl
├── live_room_stream_handler.erl
├── location_handler.erl
├── msg_handler.erl
├── passport_handler.erl
├── test_handler.erl
├── user_collect_handler.erl
├── user_denylist_handler.erl
├── user_device_handler.erl
├── user_handler.erl
├── user_tag_handler.erl
├── user_tag_relation_handler.erl
└── websocket_handler.erl
```

### 测试文件 (50+ 个)

```
test/api/
├── app_version_handler_tests.erl
├── auth_logic_tests.erl
├── conversation_handler_tests.erl
├── feedback_handler_tests.erl
├── friend_category_handler_tests.erl
├── friend_handler_tests.erl
├── friend_logic_tests.erl
├── fts_handler_tests.erl
├── fts_logic_tests.erl
├── group_handler_tests.erl
├── group_logic_tests.erl
├── group_member_handler_tests.erl
├── group_member_logic_tests.erl
├── group_member_transfer_tests.erl
├── group_notice_handler_tests.erl
├── group_notice_logic_tests.erl
├── index_handler_tests.erl
├── location_handler_tests.erl
├── msg_c2c_logic_tests.erl
├── msg_c2g_logic_tests.erl
├── msg_handler_tests.erl
├── msg_s2c_logic_tests.erl
├── passport_handler_tests.erl
├── passport_logic_tests.erl
├── stress_testing_ws_handler_tests.erl
├── user_collect_handler_tests.erl
├── user_denylist_handler_tests.erl
├── user_device_handler_tests.erl
├── user_device_logic_tests.erl
├── user_handler_tests.erl
├── user_tag_handler_tests.erl
├── user_tag_logic_tests.erl
├── user_tag_relation_handler_tests.erl
├── user_tag_relation_logic_tests.erl
└── websocket_logic_tests.erl
```

---

## 变更记录 (Changelog)

### 2026-01-20
- 新增 `e2ee_handler.erl` 端到端加密 API
- 新增 `message_router_logic.erl` 消息路由器
- 完善认证中间件文档

### 2026-01-07
- 完善 API 层文档
- 新增 API v1 路由
- 优化认证中间件

---

**文档维护**: 请在添加新的 API 端点时同步更新此文档。
