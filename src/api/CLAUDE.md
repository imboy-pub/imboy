[根目录](../CLAUDE.md) > **src/api**

---

# API 层 (src/api/)

> **最后更新**: 2026-01-07 10:05:54 CST
> **模块数量**: 27 个 | **覆盖率**: 60%

## 模块职责

API 层是 Imboy 系统的 **用户接口层 (User Interface Layer)**，负责：

1. **HTTP 请求处理**: 解析 HTTP 请求参数
2. **协议适配**: 支持 RESTful API 和 WebSocket
3. **认证鉴权**: JWT Token 验证
4. **响应格式化**: 统一 JSON 响应格式
5. **参数校验**: 请求参数基础验证

## 模块列表

### 认证与用户

| 模块 | 说明 | 路由 |
|------|------|------|
| `passport_handler.erl` | 用户注册、登录、快速登录 | `POST /passport/*` |
| `auth_handler.erl` | Token 刷新 | `POST /auth/refresh` |
| `auth_middleware.erl` | 认证中间件 | - |
| `user_handler.erl` | 用户资料、设置、状态 | `GET/POST /user/*` |
| `user_device_handler.erl` | 设备管理 | `POST /user_device/*` |

### 好友与群组

| 模块 | 说明 | 路由 |
|------|------|------|
| `friend_handler.erl` | 好友添加、删除、列表 | `POST /friend/*` |
| `friend_category_handler.erl` | 好友分组管理 | `POST /friend_category/*` |
| `group_handler.erl` | 群组创建、编辑、解散 | `POST /group/*` |
| `group_member_handler.erl` | 群成员管理 | `POST /group_member/*` |
| `group_member_transfer.erl` | 群主转让 | `POST /group_member/transfer` |
| `group_notice_handler.erl` | 群公告管理 | `POST /group_notice/*` |

### 消息与会话

| 模块 | 说明 | 路由 |
|------|------|------|
| `msg_handler.erl` | 离线消息、消息确认 | `POST /msg/*` |
| `conversation_handler.erl` | 会话列表 | `GET /conversation/list` |
| `websocket_handler.erl` | WebSocket 连接 | `WS /ws` |

### 功能扩展

| 模块 | 说明 | 路由 |
|------|------|------|
| `location_handler.erl` | 位置服务 | `POST /location/*` |
| `fts_handler.erl` | 全文搜索 | `GET /fts/*` |
| `user_collect_handler.erl` | 收藏管理 | `POST /user_collect/*` |
| `user_denylist_handler.erl` | 黑名单管理 | `POST /user_denylist/*` |
| `user_tag_handler.erl` | 用户标签管理 | `POST /user_tag/*` |
| `user_tag_relation_handler.erl` | 标签关联管理 | `POST /user_tag_relation/*` |

### 系统与测试

| 模块 | 说明 | 路由 |
|------|------|------|
| `index_handler.erl` | 首页、健康检查 | `GET /` |
| `test_handler.erl` | 测试接口 | `GET /test/*` |
| `app_version_handler.erl` | 版本检查 | `GET /app_version` |
| `feedback_handler.erl` | 用户反馈 | `POST /feedback` |
| `live_room_stream_handler.erl` | 直播流 | `POST /live_room/*` |

## 对外接口

### 用户管理 (`user_handler.erl`)

```
GET  /api/user/show           # 查看用户资料
POST /api/user/update         # 更新用户资料
POST /api/user/change_state   # 修改在线状态
POST /api/user/setting        # 用户设置
POST /api/user/change_password # 修改密码
POST /api/user/apply_logout   # 申请注销
POST /api/user/cancel_logout  # 取消注销
POST /api/user/search         # 搜索用户
GET  /api/user/qrcode         # 用户二维码
```

### 认证授权 (`passport_handler.erl`)

```
POST /api/passport/signup        # 注册
POST /api/passport/login         # 登录
POST /api/passport/quick_login   # 快速登录
POST /api/passport/getcode       # 获取验证码
POST /api/passport/findpassword  # 找回密码
POST /api/passport/bind_mail     # 绑定邮箱
```

### 好友管理 (`friend_handler.erl`)

```
POST /api/friend/add         # 添加好友
POST /api/friend/confirm     # 确认好友
POST /api/friend/delete      # 删除好友
GET  /api/friend/list        # 好友列表
GET  /api/friend/information # 好友信息
POST /api/friend/change_remark # 修改备注
POST /api/friend/move        # 移动到分组
```

### 群组管理 (`group_handler.erl`)

```
POST /api/group/add           # 创建群组
POST /api/group/edit          # 编辑群组
POST /api/group/dissolve      # 解散群组
GET  /api/group/detail        # 群组详情
GET  /api/group/page          # 群组分页
POST /api/group/remark        # 群备注
GET  /api/group/qrcode        # 群二维码
POST /api/group/face2face     # 面对面建群
```

### 消息处理 (`msg_handler.erl`)

```
POST /api/msg/offline      # 获取离线消息
POST /api/msg/offline_ack  # 确认离线消息
```

### WebSocket (`websocket_handler.erl`)

```
WS /ws  # WebSocket 连接端点
```

## 认证中间件 (`auth_middleware.erl`)

**路由分类**:
- `open()`: 无需认证的路由（如登录、注册）
- `option()`: 需要 authorization 头但不需要登录的路由

```erlang
% 开放路由
open() ->
    [
        {"/api/passport/login", passport_handler, []},
        {"/api/passport/signup", passport_handler, []},
        {"/api/passport/getcode", passport_handler, []},
        ...
    ].

% 需要认证的路由
other() ->
    [
        {"/api/user/show", user_handler, []},
        {"/api/friend/list", friend_handler, []},
        ...
    ].
```

## 关键依赖

### 上游依赖
- Cowboy 2.10: HTTP/WS 服务器
- jwerl: JWT Token 验证

### 下游调用
- `src/logic/`: 业务逻辑层
- `src/ds/`: 数据服务层

## 数据模型

### 请求参数格式

```erlang
% 从 Cowboy Req 中解析参数
Params = imboy_req:body_params(Req),
Uid = imboy_req:uid(Req),
Token = imboy_req:token(Req).
```

### 响应格式

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {}
}
```

## 测试覆盖

### 测试文件位置

```
test/api/
├── user_handler_tests.erl
├── passport_handler_tests.erl
├── friend_handler_tests.erl
├── group_handler_tests.erl
├── msg_handler_tests.erl
├── websocket_logic_tests.erl
└── ...
```

### 测试覆盖情况

- **覆盖率**: 约 60%
- **已测试**: 核心接口
- **待补充**: 边缘情况、错误处理

### 缺失测试

- `group_member_transfer.erl` 缺少测试文件
- 部分新建 Handler 的测试不完整

## 常见问题

### Q: 如何添加新的 API 端点?

A:
1. 在 `src/api/` 创建 `{模块}_handler.erl`
2. 实现 `init/2`, `allowed_methods/2`, `content_types_provided/2` 等
3. 在 `src/imboy_router.erl` 添加路由
4. 编写测试文件

### Q: 如何处理文件上传?

A: 使用 `cowboy_req:read_part/2` 读取 multipart 数据。

### Q: 如何获取当前用户信息?

A: 从 Token 中解析：
```erlang
{ok, Token} = imboy_req:token(Req),
{ok, UID} = token_ds:decrypt_token(Token).
```

## 相关文件

- `src/imboy_router.erl`: 路由定义
- `src/api/auth_middleware.erl`: 认证中间件
- `src/api/auth_handler.erl`: Token 刷新
- `test/api/`: 测试文件

## 变更记录

### 2026-01-07
- 更新模块列表，补充完整路由信息
- 更新覆盖率统计

### 2026-01-03
- 初始化 API 层文档
- 整理核心 API 端点
