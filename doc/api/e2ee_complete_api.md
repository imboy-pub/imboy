# E2EE API 文档

> **版本**: 0.7.3+
> **最后更新**: 2026-02-01
> **用途**: E2EE+ 密钥恢复功能 API 接口文档

---

## 基础信息

### API 基础 URL

```
开发环境: http://localhost:9800/v1
生产环境: https://api.imboy.com/v1
```

### 认证方式

所有 API 请求需要在 Header 中携带 JWT Token：

```
Authorization: Bearer {jwt_token}
```

### 响应格式

成功响应：
```json
{
  "code": 0,
  "msg": "success.",
  "payload": { ... },
  "sv_ts": 1738375200000
}
```

错误响应：
```json
{
  "code": 5000,
  "msg": "错误描述",
  "payload": {},
  "sv_ts": 1738375200000
}
```

**说明**：
- `code`: 响应码，0 表示成功，非 0 表示错误
- `msg`: 响应消息，成功时为 `"success."`
- `payload`: 实际响应数据
- `sv_ts`: 服务器时间戳（毫秒）

---

## 错误码定义

### 设备传输错误码 (5000-5008)

| 错误码 | 常量名 | 描述 |
|-------|--------|------|
| 5000 | `ERR_E2EE_TRANSFER_INVALID_SESSION` | 无效的传输会话 |
| 5001 | `ERR_E2EE_TRANSFER_SESSION_EXPIRED` | 传输会话已过期 |
| 5002 | `ERR_E2EE_TRANSFER_SESSION_CANCELLED` | 传输会话已取消 |
| 5003 | `ERR_E2EE_TRANSFER_ALREADY_ACCEPTED` | 传输会话已被接受 |
| 5004 | `ERR_E2EE_TRANSFER_INVALID_STATUS` | 无效的会话状态 |
| 5005 | `ERR_E2EE_TRANSFER_NOT_OWNER` | 不是会话所有者 |
| 5006 | `ERR_E2EE_TRANSFER_KEY_BUNDLE_INVALID` | 密钥包格式无效 |
| 5007 | `ERR_EEE_TRANSFER_CONFIRM_FAILED` | 确认传输失败 |

### 社交恢复错误码 (5010-5026)

| 错误码 | 常量名 | 描述 |
|-------|--------|------|
| 5010 | `ERR_E2EE_SOCIAL_CONTACT_IS_SELF` | 不能添加自己为可信联系人 |
| 5011 | `ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND` | 可信联系人不存在 |
| 5012 | `ERR_E2EE_SOCIAL_CONTACT_ALREADY_EXISTS` | 可信联系人已存在 |
| 5013 | `ERR_EEE_SOCIAL_INSUFFICIENT_PROXIES` | 代理数量不足 |
| 5014 | `ERR_EEE_SOCIAL_INVALID_THRESHOLD` | 无效的恢复阈值 |
| 5015 | `ERR_EEE_SOCIAL_SHARE_NOT_FOUND` | 密钥分片不存在 |
| 5016 | `ERR_EEE_SOCIAL_SHARE_ALREADY_USED` | 密钥分片已使用 |
| 5017 | `ERR_EEE_SOCIAL_INSUFFICIENT_SHARES` | 分片数量不足 |
| 5018 | `ERR_EEE_SOCIAL_SHARE_DECRYPT_FAILED` | 分片解密失败 |
| 5019 | `ERR_EEE_SOCIAL_RECOVER_FAILED` | 密钥恢复失败 |
| 5020 | `ERR_EEE_SOCIAL_INVALID_PROXY` | 无效的代理 |
| 5021 | `ERR_EEE_SOCIAL_PROXY_NOT_FRIEND` | 代理不是好友 |
| 5022 | `ERR_EEE_SOCIAL_NOT_RECOVERABLE` | 无法恢复密钥 |
| 5023 | `ERR_EEE_SOCIAL_SHARE_CREATE_FAILED` | 创建分片失败 |
| 5024 | `ERR_EEE_SOCIAL_SHARDS_INCONSISTENT` | 分片数据不一致 |
| 5025 | `ERR_EEE_SOCIAL_INVALID_VERSION` | 无效的密钥版本 |
| 5026 | `ERR_EEE_SOCIAL_VERSION_MISMATCH` | 密钥版本不匹配 |

### 本地备份错误码 (5030-5037)

| 错误码 | 常量名 | 描述 |
|-------|--------|------|
| 5030 | `ERR_E2EE_BACKUP_NOT_FOUND` | 备份不存在 |
| 5031 | `ERR_EEE_BACKUP_INVALID_FORMAT` | 备份格式无效 |
| 5032 | `ERR_EEE_BACKUP_DECRYPT_FAILED` | 备份解密失败 |
| 5033 | `ERR_EEE_BACKUP_CHECKSUM_MISMATCH` | 校验和不匹配 |
| 5034 | `ERR_EEE_BACKUP_PASSWORD_WRONG` | 备份密码错误 |
| 5035 | `ERR_EEE_BACKUP_VERSION_MISMATCH` | 备份版本不匹配 |
| 5036 | `ERR_EEE_BACKUP_DEVICE_MISMATCH` | 设备不匹配 |
| 5037 | `ERR_EEE_BACKUP_RESTORE_FAILED` | 备份恢复失败 |

---

## API 端点

### 1. 设备间传输 API

#### 1.1 创建传输会话

创建从旧设备到新设备的密钥传输会话。

**请求**
```http
POST /v1/e2ee/transfer/create
Content-Type: application/json
Authorization: Bearer {token}

{
  "to_uid": "encoded_target_user_id",
  "encrypted_key_bundle": "base64_encoded_encrypted_key_bundle"
}
```

**参数说明**

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `to_uid` | string | 是 | 目标用户 ID（HashID 编码） |
| `encrypted_key_bundle` | string | 是 | 使用目标用户公钥 RSA-OAEP-256 加密的密钥包 |

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "session_id": "uuid-v4",
    "expires_at": "2026-02-01T12:05:00Z"
  },
  "sv_ts": 1738375200000
}
```

#### 1.2 接受传输

新设备接受传输会话。

**请求**
```http
POST /v1/e2ee/transfer/accept
Content-Type: application/json
Authorization: Bearer {token}

{
  "session_id": "session-uuid",
  "device_id": "new_device_id"
}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "session_id": "session-uuid",
    "from_uid": 123,
    "from_device_id": "old_device_id",
    "encrypted_key_bundle": "base64_encoded_bundle",
    "status": "accepted",
    "expires_at": "2026-02-01T12:05:00Z"
  }
}
```

#### 1.3 确认传输完成

确认密钥传输完成。

**请求**
```http
POST /v1/e2ee/transfer/confirm
Content-Type: application/json
Authorization: Bearer {token}

{
  "session_id": "session-uuid"
}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "message": "传输成功"
  }
}
```

#### 1.4 查询传输会话信息

**请求**
```http
GET /v1/e2ee/transfer/info?session_id=session-uuid
Authorization: Bearer {token}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "session_id": "session-uuid",
    "from_uid": 123,
    "from_device_id": "old_device_id",
    "status": "pending",
    "expires_at": "2026-02-01T12:05:00Z"
  }
}
```

#### 1.5 获取待处理传输列表

**请求**
```http
GET /v1/e2ee/transfer/pending
Authorization: Bearer {token}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "transfers": [
      {
        "session_id": "session-uuid",
        "from_uid": 123,
        "from_device_id": "old_device_id",
        "expires_at": "2026-02-01T12:05:00Z",
        "created_at": "2026-02-01T12:00:00Z"
      }
    ]
  }
}
```

---

### 2. 社交恢复 API

#### 2.1 列出可信联系人

**请求**
```http
GET /v1/e2ee/social/contacts
Authorization: Bearer {token}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "contacts": [
      {
        "id": 1,
        "contact_uid": 456,
        "contact_nickname": "张三",
        "created_at": "2026-02-01T12:00:00Z"
      }
    ]
  }
}
```

#### 2.2 添加可信联系人

**请求**
```http
POST /v1/e2ee/social/contacts/add
Content-Type: application/json
Authorization: Bearer {token}

{
  "contact_uid": "encoded_contact_uid",
  "nickname": "可选昵称"
}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "message": "添加可信联系人成功"
  }
}
```

#### 2.3 移除可信联系人

**请求**
```http
POST /v1/e2ee/social/contacts/remove
Content-Type: application/json
Authorization: Bearer {token}

{
  "contact_uid": "encoded_contact_uid"
}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "message": "移除可信联系人成功"
  }
}
```

#### 2.4 创建密钥分片

使用 Shamir Secret Sharing 算法创建密钥分片。

**请求**
```http
POST /v1/e2ee/social/create_shards
Content-Type: application/json
Authorization: Bearer {token}

{
  "total_shards": 3,
  "threshold": 2,
  "proxies": [
    {
      "proxy_uid": 789,
      "encrypted_public_key": "base64_encoded_public_key"
    }
  ]
}
```

**参数说明**

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `total_shards` | integer | 是 | 总分片数 (2-5) |
| `threshold` | integer | 是 | 恢复阈值 (必须 ≤ total_shards) |
| `proxies` | array | 是 | 代理列表 |

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "key_version": "1234567890",
    "total_shards": 3,
    "threshold": 2,
    "shards": [
      {
        "id": 1,
        "shard_index": 0,
        "proxy_uid": 789,
        "shard_id": "shard-uuid"
      }
    ]
  }
}
```

#### 2.5 获取用户分片

**请求**
```http
GET /v1/e2ee/social/shards?key_version=latest
Authorization: Bearer {token}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "shards": [
      {
        "id": 1,
        "shard_index": 0,
        "total_shards": 3,
        "threshold": 2,
        "proxy_uid": 789,
        "status": "active",
        "created_at": "2026-02-01T12:00:00Z"
      }
    ]
  }
}
```

#### 2.6 恢复密钥（零信任架构）

**请求**
```http
POST /v1/e2ee/social/recover
Content-Type: application/json
Authorization: Bearer {token}

{
  "decrypted_shards": ["base64_decrypted_shard1", "base64_decrypted_shard2"]
}
```

**参数说明**

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `decrypted_shards` | array | 是 | 已解密的分片列表（从代理获取） |

**零信任架构流程**：

1. **客户端联系代理**：通过 WebSocket 向代理请求解密分片
2. **代理解密分片**：代理使用自己的私钥解密分片，返回给用户
3. **收集解密分片**：用户收集至少 `threshold` 个解密分片
4. **重组密钥**：调用服务端 API 恢复密钥

**WebSocket 消息格式（请求解密）**：
```json
{
  "type": "S2C",
  "to": "proxy_hashid",
  "payload": {
    "msg_type": "e2ee_social_shard",
    "action": "decrypt_request",
    "shard_id": "uuid-1",
    "requester_uid": 123,
    "requester_nickname": "张三"
  }
}
```

**代理解密响应（C2S）**：
```json
{
  "type": "C2S",
  "payload": {
    "msg_type": "e2ee_social_shard",
    "action": "decrypted_shard",
    "shard_id": "uuid-1",
    "decrypted_shard": "base64_decrypted_shard_data"
  }
}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "message": "密钥恢复成功"
  },
  "sv_ts": 1738375200000
}
```

#### 2.7 获取代理分片

获取当前用户作为代理存储的分片。

**请求**
```http
GET /v1/e2ee/social/proxy_shards
Authorization: Bearer {token}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "shards": [
      {
        "id": 1,
        "uid": 123,
        "key_version": "latest",
        "shard_index": 0,
        "created_at": "2026-02-01T12:00:00Z"
      }
    ]
  }
}
```

#### 2.8 解密分片

代理用户解密存储的分片。

**请求**
```http
POST /v1/e2ee/social/decrypt_shard
Content-Type: application/json
Authorization: Bearer {token}

{
  "shard_id": "shard-uuid"
}
```

**响应**
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "decrypted_shard": "decrypted_shard_data"
  }
}
```

---

## 使用示例

### 完整流程示例：社交恢复密钥

```bash
# 1. 设置认证 Token
export TOKEN="your_jwt_token"
export API_URL="http://localhost:9800/v1"

# 2. 添加可信联系人
curl -X POST "$API_URL/e2ee/social/contacts/add" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "contact_uid": "g6h7j8k9",
    "nickname": "张三"
  }'

# 3. 创建密钥分片
curl -X POST "$API_URL/e2ee/social/create_shards" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "total_shards": 3,
    "threshold": 2,
    "proxies": [
      {"proxy_uid": 123, "encrypted_public_key": "base64..."},
      {"proxy_uid": 456, "encrypted_public_key": "base64..."},
      {"proxy_uid": 789, "encrypted_public_key": "base64..."}
    ]
  }'

# 4. 查看分片状态
curl -X GET "$API_URL/e2ee/social/shards?key_version=latest" \
  -H "Authorization: Bearer $TOKEN"

# 5. 恢复密钥
curl -X POST "$API_URL/e2ee/social/recover" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "key_version": "latest",
    "shard_ids": ["shard-id-1", "shard-id-2"]
  }'
```

### 完整流程示例：设备间传输

```bash
# 1. 旧设备创建传输会话
curl -X POST "$API_URL/e2ee/transfer/create" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "to_uid": "g6h7j8k9",
    "encrypted_key_bundle": "base64_encrypted_key_bundle"
  }'

# 2. 新设备接受传输
curl -X POST "$API_URL/e2ee/transfer/accept" \
  -H "Authorization: Bearer $NEW_DEVICE_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "session_id": "session-uuid",
    "device_id": "new_device_id"
  }'

# 3. 新设备确认传输完成
curl -X POST "$API_URL/e2ee/transfer/confirm" \
  -H "Authorization: Bearer $NEW_DEVICE_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "session_id": "session-uuid"
  }'
```

---

## 数据模型

### 传输会话状态

| 状态 | 说明 |
|------|------|
| `pending` | 等待接受 |
| `accepted` | 已接受 |
| `confirmed` | 已确认 |
| `expired` | 已过期 |
| `cancelled` | 已取消 |

### 分片状态

| 状态 | 说明 |
|------|------|
| `active` | 活跃，可用于恢复 |
| `used` | 已使用，无法再次使用 |

---

## 安全注意事项

1. **零信任架构**:
   - 服务器永远无法访问用户的私钥
   - **社交恢复分片不存储在服务端**，分片通过 WebSocket 直接发送给代理
   - 代理将分片存储在本地设备，服务端只作为传输通道

2. **加密传输**: 所有敏感数据使用 RSA-OAEP-256 加密

3. **会话过期**: 传输会话 5 分钟后自动过期

4. **权限验证**: 所有操作都验证用户身份和权限

5. **HashID 编码**: 所有用户 ID 使用 HashID 编码，避免暴露真实 ID

### 社交恢复分片存储架构

```
用户 A 生成密钥分片（总分片数: 3, 阈值: 2）
    │
    ├─ 分片 0 → 通过 WebSocket 发送 → 代理 B（存储在本地）
    ├─ 分片 1 → 通过 WebSocket 发送 → 代理 C（存储在本地）
    └─ 分片 2 → 通过 WebSocket 发送 → 代理 D（存储在本地）

服务端: 仅作为 WebSocket 传输通道，不存储任何分片数据
```

**关键点**：
- 服务端**不存储**社交恢复的分片
- 分片通过 **WebSocket S2C 消息**直接发送给代理
- 代理将分片存储在**本地设备**（加密存储）
- 恢复时，用户通过 WebSocket 请求代理解密分片

---

## 附录

### A. 错误处理示例

```json
{
  "code": 5010,
  "msg": "不能添加自己为可信联系人",
  "payload": {}
}
```

### B. 分片加密格式

分片使用代理用户的公钥 RSA-OAEP-256 加密：

```
encrypted_shard = base64(
    RSA-OAEP-256(
        proxy_public_key,
        JSON({
            index: 0,
            x: 12345,
            y: 67890
        })
    )
)
```

### C. 密钥包加密格式

密钥包使用目标用户的公钥 RSA-OAEP-256 加密：

```
encrypted_key_bundle = base64(
    RSA-OAEP-256(
        target_public_key,
        private_key_pem
    )
)
```

---

**文档版本**: 1.0
**最后更新**: 2026-02-01
