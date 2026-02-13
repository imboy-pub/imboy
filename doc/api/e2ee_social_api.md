# E2EE 社交恢复 API 文档

> **API 版本**: v1
> **基础路径**: `/v1/e2ee/social`
> **认证方式**: JWT Token
> **最后更新**: 2026-01-31

---

## 📋 API 概览

| 方法 | 路径 | 说明 |
|------|------|------|
| POST | `/create_shards` | 创建恢复分片 |
| GET | `/shards` | 获取用户的恢复分片 |
| POST | `/recover` | 恢复密钥 |
| GET | `/proxy_shards` | 获取代理的分片列表 |
| POST | `/decrypt_shard` | 解密分片（代理调用） |

---

## 🔧 API 详细说明

### 1. 创建恢复分片

**接口**: `POST /v1/e2ee/social/create_shards`

**认证**: 需要 JWT Token

**请求体**:
```json
{
  "total_shards": 3,
  "threshold": 2,
  "proxies": [
    {
      "proxy_uid": 123,
      "encrypted_public_key": "BASE64_ENCODED_PUBLIC_KEY"
    }
  ]
}
```

**参数说明**:
| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `total_shards` | int | 是 | 总分片数（3-5） |
| `threshold` | int | 是 | 恢复阈值（2-3） |
| `proxies` | array | 是 | 代理列表 |

**proxies 数组项**:
| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `proxy_uid` | int | 是 | 代理用户 ID |
| `encrypted_public_key` | string | 是 | 使用代理公钥加密的公钥（BASE64） |

**响应**:
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
        "shard_id": "uuid-1",
        "shard_index": 0,
        "total_shards": 3,
        "threshold": 2,
        "proxy_uid": 123,
        "status": "active",
        "created_at": "2026-01-31T10:00:00Z"
      }
    ]
  }
}
```

**错误响应**:
```json
{
  "code": 5001,
  "msg": "参数错误：分片数必须大于阈值",
  "payload": {},
  "sv_ts": 1738375200000
}
```

---

### 2. 获取用户的恢复分片

**接口**: `GET /v1/e2ee/social/shards`

**认证**: 需要 JWT Token

**查询参数**:
| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `key_version` | string | 否 | 密钥版本号（默认 "latest"） |

**响应**:
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "shards": [
      {
        "id": 1,
        "uid": 456,
        "key_version": "1234567890",
        "shard_index": 0,
        "total_shards": 3,
        "threshold": 2,
        "proxy_uid": 123,
        "shard_id": "uuid-1",
        "status": "active",
        "created_at": "2026-01-31T10:00:00Z",
        "used_at": null
      }
    ]
  }
}
```

---

### 3. 恢复密钥

**接口**: `POST /v1/e2ee/social/recover`

**认证**: 需要 JWT Token

**请求体**:
```json
{
  "key_version": "latest",
  "shard_ids": ["uuid-1", "uuid-2"]
}
```

**参数说明**:
| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `key_version` | string | 否 | 密钥版本号（默认 "latest"） |
| `shard_ids` | array | 是 | 要使用的分片 ID 列表 |

**响应**:
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "message": "密钥恢复成功"
  }
}
```

**错误响应**:
```json
{
  "code": 5002,
  "msg": "分片数量不足，至少需要 2 个分片才能恢复密钥",
  "payload": {},
  "sv_ts": 1738375200000
}
```

---

### 4. 获取代理的分片列表

**接口**: `GET /v1/e2ee/social/proxy_shards`

**认证**: 需要 JWT Token

**响应**:
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "shards": [
      {
        "id": 1,
        "uid": 123,
        "key_version": "1234567890",
        "shard_index": 0,
        "total_shards": 3,
        "threshold": 2,
        "shard_id": "uuid-1",
        "status": "active",
        "created_at": "2026-01-31T10:00:00Z"
      }
    ]
  }
}
```

---

### 5. 解密分片（代理调用）

**接口**: `POST /v1/e2ee/social/decrypt_shard`

**认证**: 需要 JWT Token（代理的 Token）

**请求体**:
```json
{
  "shard_id": "uuid-1"
}
```

**参数说明**:
| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `shard_id` | string | 是 | 分片 ID |

**响应**:
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "decrypted_shard": "DECRYPTED_SHARD_DATA"
  }
}
```

---

## 📊 数据模型

### e2ee_social_shards 表结构

| 字段 | 类型 | 说明 |
|------|------|------|
| `id` | bigint | 主键 |
| `uid` | bigint | 用户 ID（密钥所有者） |
| `key_version` | varchar(32) | 密钥版本号 |
| `shard_index` | integer | 分片索引（0-based） |
| `total_shards` | integer | 总分片数 |
| `threshold` | integer | 恢复阈值 |
| `encrypted_shard` | text | 加密的分片数据 |
| `proxy_uid` | bigint | 代理用户 ID |
| `shard_id` | varchar(64) | 分片唯一标识符 |
| `status` | varchar(20) | 状态：active / used |
| `created_at` | timestamptz | 创建时间 |
| `used_at` | timestamptz | 使用时间 |

### 状态说明

| 状态 | 说明 |
|------|------|
| `active` | 分片有效，可用于恢复 |
| `used` | 分片已用于恢复密钥，不可再用 |

---

## 🔐 安全说明

### 加密方式

1. **分片加密**: 使用代理的 RSA-2048 公钥加密（RSA-OAEP-256）
2. **密钥分割**: 使用 Shamir Secret Sharing (k/n 门限方案)
3. **传输安全**: 所有 API 使用 HTTPS + JWT 认证

### 权限控制

- 用户只能访问自己的分片
- 代理只能访问作为代理的分片
- 恢复操作需要验证用户身份

### 最佳实践

1. **参数验证**
   - `total_shards` 必须 > `threshold`
   - `threshold` 必须 >= 2
   - `proxies` 数量必须 >= `total_shards`

2. **错误处理**
   - 参数错误返回 400
   - 未授权返回 401
   - 服务器错误返回 500

3. **安全检查**
   - 验证代理是否是好友
   - 验证代理公钥有效性
   - 防止重复创建分片

---

## 📈 错误码

| 错误码 | 说明 |
|--------|------|
| 5000 | 内部服务器错误 |
| 5001 | 参数错误 |
| 5002 | 分片数量不足 |
| 5003 | 代理数量不足 |
| 5004 | 分片不存在 |
| 5005 | 无效的分片状态 |
| 5006 | 恢复失败 |

---

## 🔄 流程图

### 创建分片流程

```
用户 A
  │
  ├─ 1. 设置分片参数 (n=3, k=2)
  │
  ├─ 2. 选择代理 [B, C, D]
  │
  ├─ 3. 获取代理公钥
  │
  ├─ 4. 生成私钥分片 (Shamir Secret Sharing)
  │
  ├─ 5. 使用代理公钥加密分片
  │
  └─ 6. 存储加密分片到数据库
        │
        ▼
      代理 B  ← 分片 1 (加密)
      代理 C  ← 分片 2 (加密)
      代理 D  ← 分片 3 (加密)
```

### 恢复密钥流程

```
用户 A (新设备/密钥丢失)
  │
  ├─ 1. 查询可用分片
  │
  ├─ 2. 选择至少 k 个分片 (例如: [分片1, 分片2])
  │
  ├─ 3. 请求代理解密分片
  │    │
  │    ├─► 代理 B 解密分片 1
  │    ├─► 代理 C 解密分片 2
  │
  ├─ 4. 使用 Shamir Secret Sharing 重组密钥
  │
  └─ 5. 保存密钥到本地存储
```

---

## 🧪 测试命令

### 使用 curl 测试

```bash
# 设置环境变量
export API_URL="https://your-api.com"
export TOKEN="your_jwt_token"

# 1. 创建分片
curl -X POST "$API_URL/v1/e2ee/social/create_shards" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "total_shards": 3,
    "threshold": 2,
    "proxies": [
      {"proxy_uid": 123, "encrypted_public_key": "..."}
    ]
  }'

# 2. 获取分片
curl -X GET "$API_URL/v1/e2ee/social/shards?key_version=latest" \
  -H "Authorization: Bearer $TOKEN"

# 3. 恢复密钥
curl -X POST "$API_URL/v1/e2ee/social/recover" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "shard_ids": ["shard-id-1", "shard-id-2"]
  }'

# 4. 获取代理分片
curl -X GET "$API_URL/v1/e2ee/social/proxy_shards" \
  -H "Authorization: Bearer $TOKEN"
```

---

**文档版本**: 1.0
**最后更新**: 2026-01-31
