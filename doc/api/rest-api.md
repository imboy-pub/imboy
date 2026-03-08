# Imboy REST API（通用入口与基础契约）

> Last Updated: 2026-03-08  
> Source of truth: `src/imboy_router.erl` + 本文涉及接口对应的 handler / logic 代码  
> Related docs: `doc/api/channel_api_contract_v1.md`, `doc/api/moment_api_contract_v1.md`, `doc/api/e2ee_server_persisted_shard_contract_v1.md`, `doc/api/websocket-api-2.md`

## 1. 文档定位

本文档只保留通用 REST 入口、兼容性接口和基础契约。

以下模块使用独立契约文档维护，不再在本文件重复展开：

- 频道：`doc/api/channel_api_contract_v1.md`
- 朋友圈：`doc/api/moment_api_contract_v1.md`
- E2EE 社交恢复分片：`doc/api/e2ee_server_persisted_shard_contract_v1.md`
- WebSocket：`doc/api/websocket-api-2.md`
- 通用响应包：`doc/api/envelope.schema.json`

## 2. 通用响应约定

所有 REST 接口默认返回统一 envelope：

```json
{
  "code": 0,
  "msg": "success.",
  "sv_ts": 1771481621000,
  "payload": {}
}
```

约定：

- `code = 0` 表示成功；
- `code != 0` 表示业务失败；
- `payload` 的具体结构由对应接口定义。

## 3. 初始化接口

### GET `/v1/init`

用途：客户端启动时拉取初始化配置。

当前返回内容以加密后的 `payload.res` 形式下发，解密后通常包含：

- `ws_url`
- `upload_url`
- `upload_key`
- `upload_scene`
- `login_pwd_rsa_encrypt`
- `login_rsa_pub_key`

响应示例：

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "res": "encrypt by aes_256_cbc"
  }
}
```

说明：

- `res` 为服务端包装后的初始化数据；
- 客户端应按约定密钥派生逻辑解密；
- 口令 RSA 包装是附加保护，不可替代 HTTPS。

## 4. 会话列表契约

### GET `/v1/conversation/mine`

用途：拉取服务端权威会话列表。

Query：

- `last_server_ts`（可选）：按服务端时间戳增量拉取

`payload` 为会话项列表，典型字段：

```json
[
  {
    "conversation_id": "hashid",
    "conversation_type": "c2c|c2g",
    "server_ts": 1700000000123,
    "last_msg_id": "msg-uuid-or-id",
    "last_msg": {},
    "is_pinned": false
  }
]
```

权威规则：

- 服务端统一聚合 `c2c` 与 `c2g` 会话；
- 相同会话按最新 `server_ts` 去重；
- 删除态由服务端删除表决定，不以客户端本地状态为准；
- 置顶、删除语义以后端返回为准。

## 5. `@` 提及兼容接口

### POST `/v1/mention/list`

用途：获取 `@我` 列表。

参数可从 Query 或 Body 读取：

- `page`、`size`
- `is_read`：支持 `true/false` 或 `1/0`
- `gid` / `group_id`：按群过滤（可选）

返回字段：

- `total`、`page`、`size`
- `list`（标准）
- `items`（兼容，等价于 `list`）

列表项兼容字段：

- `id`
- `msg_id`
- `group_id`
- `from_uid`
- `mentioned_uid`
- `is_read`
- `is_read_bool`
- `created_at`

### POST `/v1/mention/unread`

用途：获取未读计数。

参数可从 Query 或 Body 读取：

- `gid` / `group_id`：按群统计（可选）

返回字段：

- `count`

### POST `/v1/mention/mark_read`

用途：标记已读。

参数（Body）：

- `msg_id`：按消息标记（标准）
- `mention_id`：按 mention 记录标记（兼容）
- `all=true`：全部标记已读
- `group_id` / `gid`：`all=true` 时可按群全部已读

### POST `/v1/mention/suggest`

用途：输入 `@` 时获取成员建议。

参数可从 Query 或 Body 读取：

- `gid` / `group_id`（必填）
- `keyword`（可选）

返回字段：

- `members`（标准）
- `items`（兼容，等价于 `members`）

## 6. 群列表兼容接口

### GET `/v1/group/page`

用途：分页拉取群列表（按视图）。

请求参数（Query）：

- `page`：页码，从 `1` 开始
- `size`：每页条数
- `attr`：群视图，支持：
  - `owner`：我创建的群
  - `join`：我加入的群
  - `manager`：我管理的群

默认行为：

- 未传 `attr` 或传入未知值时，服务端回退到 `owner` 视图。

返回字段：

- `total`、`page`、`size`
- `list`

## 7. 功能开关接口

### GET `/v1/app/features`

用途：返回 App 侧可消费的功能开关矩阵，用于启动阶段、入口显隐、路由守卫和关闭态兜底。

权限：公开接口，无需登录。

成功响应示例：

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "core": true,
    "e2ee": false,
    "channel": true,
    "location": false,
    "moment": false,
    "channel_discover": true,
    "channel_invitation": true,
    "channel_order": false,
    "group_vote": false,
    "group_schedule": false,
    "group_task": false
  }
}
```

缺省兼容策略：

- 整个 `features` 配置块缺失时，已登记功能默认按开启处理；
- 单个功能项缺失，或未声明 `enabled` 时，该功能默认按开启处理；
- `channel_discover`、`channel_invitation`、`channel_order` 依赖 `channel`，父开关关闭时，对外返回一律为 `false`。

### GET `/adm/admin/config/features`

用途：返回管理后台当前生效的功能开关矩阵，用于菜单、页面、按钮与 RBAC 的功能层联动。

权限：需要后台登录态，并具备 `settings:view` 权限。

成功响应的 `payload` 与 `/v1/app/features` 完全一致。

无权限响应示例：

```json
{
  "code": 403,
  "msg": "无权限操作",
  "payload": {}
}
```

说明：

- 该接口用于后台读取只读功能矩阵，不返回原始配置结构；
- 后台应先经过登录鉴权，再由 `settings:view` 权限控制是否可读；
- App 与后台都应直接以接口返回的布尔矩阵为准，而不是本地硬编码默认值。

## 8. 模块契约索引

以下接口已迁移到独立文档维护：

- 频道主契约与邀请 / 订单 / 未读同步：`doc/api/channel_api_contract_v1.md`
- 朋友圈主契约：`doc/api/moment_api_contract_v1.md`
- E2EE 社交恢复分片：`doc/api/e2ee_server_persisted_shard_contract_v1.md`
- WebSocket 消息协议：`doc/api/websocket-api-2.md`

## 9. Related Docs

- `doc/api/channel_api_contract_v1.md`
- `doc/api/moment_api_contract_v1.md`
- `doc/api/e2ee_server_persisted_shard_contract_v1.md`
- `doc/api/websocket-api-2.md`
- `doc/api/envelope.schema.json`
- `doc/standards/error-codes.md`
