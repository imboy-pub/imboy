# E2EE 社交恢复零信任架构实现指南

> **版本**: 0.7.3+
> **最后更新**: 2026-02-02
> **架构原则**: 服务端不存储任何分片数据，仅作为 WebSocket 传输通道

---

## 一、核心设计原则

### 1.1 零信任架构

```
┌─────────────────────────────────────────────────────────────┐
│                      传统架构（❌ 不安全）                     │
├─────────────────────────────────────────────────────────────┤
│  用户 A → 生成分片 → 存储到服务端数据库 → 代理从服务端获取  │
│                                                               │
│  问题：服务端存储了所有分片，存在单点泄露风险                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                   零信任架构（✅ 安全）                        │
├─────────────────────────────────────────────────────────────┤
│  用户 A → 生成分片 → 通过 WebSocket 直接发送给代理           │
│                                                               │
│  代理 B ← 分片 0（存储在本地设备）                           │
│  代理 C ← 分片 1（存储在本地设备）                           │
│  代理 D ← 分片 2（存储在本地设备）                           │
│                                                               │
│  服务端：仅作为 WebSocket 传输通道，不存储分片              │
└─────────────────────────────────────────────────────────────┘
```

### 1.2 关键特性

| 特性 | 说明 |
|------|------|
| **服务端零存储** | 分片不存储在服务端数据库 |
| **端到端加密** | 分片使用代理公钥加密，只有代理能解密 |
| **本地存储** | 代理将分片存储在本地设备（Secure Storage） |
| **WebSocket 传输** | 通过 WebSocket S2C 消息直接发送 |
| **无需信任服务端** | 即使服务端被攻破，分片也是安全的 |

---

## 二、分片创建流程

### 2.1 API 端点

**请求**：
```http
POST /v1/e2ee/social/create_shards
Content-Type: application/json
Authorization: Bearer {token}

{
  "total_shards": 3,
  "threshold": 2,
  "proxies": [
    {"proxy_uid": 123, "encrypted_public_key": "base64..."},
    {"proxy_uid": 456, "encrypted_public_key": "base64..."},
    {"proxy_uid": 789, "encrypted_public_key": "base64..."}
  ]
}
```

**响应**：
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
        "proxy_uid": 123,
        "encrypted_shard": "base64_encoded_shard",
        "status": "pending"
      },
      {
        "shard_id": "uuid-2",
        "shard_index": 1,
        "proxy_uid": 456,
        "encrypted_shard": "base64_encoded_shard",
        "status": "pending"
      },
      {
        "shard_id": "uuid-3",
        "shard_index": 2,
        "proxy_uid": 789,
        "encrypted_shard": "base64_encoded_shard",
        "status": "pending"
      }
    ]
  },
  "sv_ts": 1738375200000
}
```

### 2.2 客户端处理流程

```dart
// 1. 调用 API 创建分片
final result = await api.createKeyShards(
  totalShards: 3,
  threshold: 2,
  proxies: [
    {'proxy_uid': 123, 'encrypted_public_key': '...'},
    {'proxy_uid': 456, 'encrypted_public_key': '...'},
    {'proxy_uid': 789, 'encrypted_public_key': '...'},
  ],
);

// 2. 获取分片列表
final shards = result['shards'] as List<Map<String, dynamic>>;

// 3. 通过 WebSocket 发送分片给代理
for (var shard in shards) {
  final proxyUid = shard['proxy_uid'] as int;
  final encryptedShard = shard['encrypted_shard'] as String;
  final shardId = shard['shard_id'] as String;

  // 发送 WebSocket 消息给代理
  await sendShardToProxy(
    toUid: proxyUid,
    shardId: shardId,
    encryptedShard: encryptedShard,
  );
}

// 4. 等待代理确认接收
// 代理会通过 WebSocket 返回确认消息
```

---

## 三、WebSocket 消息格式

### 3.1 发送分片给代理 (S2C)

**用户 → 代理**：

```json
{
  "id": "msg_id",
  "type": "S2C",
  "from": "user_hashid",
  "to": "proxy_hashid",
  "payload": {
    "msg_type": "e2ee_social_shard",
    "action": "store_shard",
    "shard_id": "uuid-1",
    "shard_index": 0,
    "total_shards": 3,
    "threshold": 2,
    "key_version": "1234567890",
    "encrypted_shard": "base64_encoded_shard",
    "from_uid": 123
  },
  "created_at": 1738375200000
}
```

### 3.2 代理确认接收 (C2S)

**代理 → 服务端**：

```json
{
  "id": "msg_id",
  "type": "C2S",
  "from": "proxy_hashid",
  "payload": {
    "msg_type": "e2ee_social_shard",
    "action": "shard_stored",
    "shard_id": "uuid-1",
    "status": "stored"
  },
  "created_at": 1738375200000
}
```

### 3.3 代理通知用户 (S2C)

**服务端 → 用户**：

```json
{
  "id": "msg_id",
  "type": "S2C",
  "payload": {
    "msg_type": "e2ee_social_shard",
    "action": "proxy_confirmed",
    "shard_id": "uuid-1",
    "proxy_uid": 123,
    "proxy_nickname": "张三",
    "status": "stored"
  },
  "created_at": 1738375200000
}
```

---

## 四、代理本地存储

### 4.1 存储格式

代理将接收到的分片存储在本地 Secure Storage：

```dart
// 代理端存储格式
final storedShard = {
  'shard_id': 'uuid-1',
  'uid': 123,  // 密钥所有者
  'key_version': '1234567890',
  'shard_index': 0,
  'total_shards': 3,
  'threshold': 2,
  'encrypted_shard': 'base64_encoded_shard',
  'created_at': '2026-02-02T10:00:00Z',
  'status': 'active',
};

// 存储到 Secure Storage
await storageSecure.saveE2EEShard(shardId, jsonEncode(storedShard));
```

### 4.2 获取代理的分片列表

**API 端点**：
```http
GET /v1/e2ee/social/proxy_shards
Authorization: Bearer {token}
```

**响应**：
```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "shards": [
      {
        "shard_id": "uuid-1",
        "uid": 123,
        "key_version": "1234567890",
        "shard_index": 0,
        "total_shards": 3,
        "threshold": 2,
        "status": "active",
        "created_at": "2026-02-02T10:00:00Z"
      }
    ]
  },
  "sv_ts": 1738375200000
}
```

---

## 五、密钥恢复流程

### 5.1 用户请求分片解密

**API 端点**：
```http
POST /v1/e2ee/social/request_decrypt
Content-Type: application/json

{
  "shard_ids": ["uuid-1", "uuid-2"]
}
```

### 5.2 服务端发送解密请求 (S2C)

**服务端 → 代理**：

```json
{
  "id": "msg_id",
  "type": "S2C",
  "payload": {
    "msg_type": "e2ee_social_shard",
    "action": "decrypt_request",
    "shard_id": "uuid-1",
    "requester_uid": 123,
    "requester_nickname": "张三"
  },
  "created_at": 1738375200000
}
```

### 5.3 代理解密并发送 (C2S)

**代理 → 服务端**：

```json
{
  "id": "msg_id",
  "type": "C2S",
  "payload": {
    "msg_type": "e2ee_social_shard",
    "action": "decrypted_shard",
    "shard_id": "uuid-1",
    "decrypted_shard": "decrypted_shard_data"
  },
  "created_at": 1738375200000
}
```

---

## 六、后端实现变更

### 6.1 Logic 层变更

**变更前**（存储到数据库）：
```erlang
create_shards(Uid, KeyVersion, TotalShards, Threshold, PrivateKeyPem, Proxies) ->
    Shards = shamir_secret_sharing:split_secret(PrivateKeyPem, TotalShards, Threshold),
    {ok, ShardRecords} = create_encrypted_shards(...),  % 存储到数据库
    {ok, ShardRecords}.
```

**变更后**（不存储，返回给客户端）：
```erlang
create_shards(Uid, KeyVersion, TotalShards, Threshold, PrivateKeyPem, Proxies) ->
    Shards = shamir_secret_sharing:split_secret(PrivateKeyPem, TotalShards, Threshold),
    {ok, ShardRecords} = encrypt_shards_for_proxies(...),  % 仅加密，不存储
    {ok, ShardRecords}.  % 返回给客户端通过 WebSocket 发送
```

### 6.2 数据库表变更

**保留表结构但不再存储分片**：
```sql
-- e2ee_social_shards 表保留用于元数据（可选）
-- 但 encrypted_shard 字段不再使用

CREATE TABLE e2ee_social_shards (
    id BIGSERIAL PRIMARY KEY,
    uid BIGINT NOT NULL,
    key_version VARCHAR(32) NOT NULL,
    shard_index INTEGER NOT NULL,
    total_shards INTEGER NOT NULL,
    threshold INTEGER NOT NULL,
    proxy_uid BIGINT NOT NULL,
    shard_id VARCHAR(64) NOT NULL,
    status VARCHAR(20) DEFAULT 'pending',
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- 注意：encrypted_shard 字段已移除，分片由代理本地存储
```

---

## 七、前端实现指南

### 7.1 创建分片并发送

```dart
class E2EESocialService {
  /// 创建并发送分片给代理
  static Future<void> createAndSendShards({
    required int totalShards,
    required int threshold,
    required List<Map<String, dynamic>> proxies,
  }) async {
    // 1. 生成私钥分片
    final privateKey = await StorageSecure().getPrivateKey();
    final shards = _splitPrivateKey(privateKey, totalShards, threshold);

    // 2. 为每个代理加密分片
    final encryptedShards = await _encryptForProxies(shards, proxies);

    // 3. 通过 WebSocket 发送给代理
    for (var shardData in encryptedShards) {
      await _sendShardToProxy(shardData);
    }

    // 4. 等待代理确认
    await _waitForProxyConfirmation(encryptedShards.length);
  }

  /// 通过 WebSocket 发送分片
  static Future<void> _sendShardToProxy(Map<String, dynamic> shardData) async {
    final proxyUid = shardData['proxy_uid'] as int;
    final proxyHashId = elib_hashids.encode(proxyUid);

    final message = {
      'type': 'S2C',
      'to': proxyHashId,
      'payload': {
        'msg_type': 'e2ee_social_shard',
        'action': 'store_shard',
        ...shardData,
      },
    };

    await WebSocketService.send(message);
  }
}
```

### 7.2 代理接收并存储

```dart
class E2EEProxyService {
  /// 处理接收到的分片
  static Future<void> handleIncomingShard(Map<String, dynamic> payload) async {
    final shardId = payload['shard_id'] as String;
    final encryptedShard = payload['encrypted_shard'] as String;

    // 存储到本地 Secure Storage
    final storedShard = {
      'shard_id': shardId,
      'uid': payload['from_uid'],
      'key_version': payload['key_version'],
      'shard_index': payload['shard_index'],
      'total_shards': payload['total_shards'],
      'threshold': payload['threshold'],
      'encrypted_shard': encryptedShard,
      'created_at': DateTime.now().toIso8601String(),
      'status': 'active',
    };

    await StorageSecure().saveE2EEShard(shardId, jsonEncode(storedShard));

    // 发送确认消息
    await _sendShardStoredConfirmation(shardId);
  }

  /// 解密分片（当用户请求恢复时）
  static Future<String> decryptShard(String shardId) async {
    // 从本地存储获取分片
    final shardJson = await StorageSecure().getE2EEShard(shardId);
    final shard = jsonDecode(shardJson) as Map<String, dynamic>;

    // 使用自己的私钥解密
    final encryptedShard = shard['encrypted_shard'] as String;
    final decryptedShard = await _decryptWithPrivateKey(encryptedShard);

    return decryptedShard;
  }
}
```

---

## 八、测试场景

### 8.1 创建分片测试

```bash
# 1. 用户创建分片
curl -X POST "$API_URL/v1/e2ee/social/create_shards" \
  -H "Authorization: Bearer $USER_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "total_shards": 3,
    "threshold": 2,
    "proxies": [
      {"proxy_uid": 123, "encrypted_public_key": "..."},
      {"proxy_uid": 456, "encrypted_public_key": "..."},
      {"proxy_uid": 789, "encrypted_public_key": "..."}
    ]
  }'

# 2. 验证响应包含 3 个分片
# 3. 验证分片通过 WebSocket 发送给代理
# 4. 验证代理本地存储了分片
# 5. 验证服务端数据库中没有 encrypted_shard 数据
```

### 8.2 恢复密钥测试

```bash
# 1. 用户请求恢复密钥
# 2. 服务端发送解密请求给代理
# 3. 代理解密分片并发送
# 4. 用户重组密钥
# 5. 验证恢复成功
```

---

## 九、安全考虑

### 9.1 分片加密

- 使用代理的 RSA 公钥加密分片
- 只有代理能解密（使用其私钥）
- 服务端无法查看分片内容

### 9.2 本地存储

- 代理将分片存储在 Secure Storage
- 分片始终以加密形式存储
- 即使设备被盗，分片也是安全的

### 9.3 传输安全

- 分片通过 WebSocket 传输
- 使用 TLS 加密传输层
- 分片本身已使用代理公钥加密

---

## 十、总结

### 10.1 架构优势

| 优势 | 说明 |
|------|------|
| **零信任** | 服务端不存储分片，无单点泄露风险 |
| **端到端加密** | 分片加密传输，只有代理能解密 |
| **分布式存储** | 分片存储在多个代理设备 |
| **用户控制** | 用户选择可信代理，完全自主 |

### 10.2 实现清单

- [x] 后端 Logic 层：不存储分片
- [x] 后端 Handler：返回分片列表
- [x] API 文档：更新零信任架构说明
- [ ] 前端：实现分片发送逻辑
- [ ] 前端：实现代理接收和存储逻辑
- [ ] WebSocket：添加分片传输消息类型

---

**文档版本**: 1.0
**最后更新**: 2026-02-02
