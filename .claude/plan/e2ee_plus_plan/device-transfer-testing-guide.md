# E2EE 设备间传输 - 测试指南

> **方法**: A - 设备间传输
> **状态**: ✅ 后端完成 | ✅ 前端完成 | ⏳ 待测试

---

## 📋 测试前准备

### 1. 数据库迁移

在测试前，需要先执行数据库迁移：

```bash
# 连接到数据库
psql -U imboy -d imboy

# 执行迁移
\i priv/migrations/00000046_e2ee_device_transfer.sql

# 验证表已创建
\d e2ee_transfer_sessions
```

### 2. 启动后端服务

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make run
```

### 3. 启动前端应用

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp
flutter run
```

---

## 🧪 测试场景

### 场景 1: 完整传输流程

**前置条件**:
- 两个设备（或模拟器）已登录
- 设备 A 已有 E2EE 密钥
- 设备 B 没有密钥

**测试步骤**:

1. **设备 A - 生成密钥**
   ```
   进入"设置" → "E2EE 密钥管理"
   确认密钥已存在（显示设备 ID、密钥 ID）
   ```

2. **设备 A - 创建传输会话**
   ```
   进入"设置" → "E2EE 密钥管理"
   点击"设备间传输"
   点击"发送密钥到新设备"
   输入设备 B 的用户 ID（从数据库或后端获取）
   点击"创建传输会话"
   显示二维码
   ```

3. **设备 B - 扫描二维码**
   ```
   进入"设置" → "E2EE 密钥管理"
   点击"设备间传输"
   点击"从旧设备接收密钥"
   扫描设备 A 的二维码
   等待传输完成
   ```

4. **设备 B - 验证密钥**
   ```
   返回"设置" → "E2EE 密钥管理"
   确认密钥已成功导入（显示设备 ID、密钥 ID）
   密钥 ID 应与设备 A 一致
   ```

**预期结果**:
- 设备 A 成功创建传输会话
- 二维码正确显示 session_id
- 设备 B 成功扫描并解密密钥
- 密钥成功保存到设备 B
- 传输状态从 pending → accepted → confirmed

### 场景 2: 会话过期

**测试步骤**:

1. **创建传输会话**
   ```
   设备 A: 创建传输会话
   记录 session_id 和 expires_at
   ```

2. **等待会话过期**
   ```
   等待 5 分钟后
   或手动修改数据库：
   UPDATE e2ee_transfer_sessions
   SET expires_at = NOW() - INTERVAL '1 minute'
   WHERE session_id = 'xxx';
   ```

3. **尝试接受传输**
   ```
   设备 B: 扫描二维码
   应显示"会话不存在或已过期"
   ```

**预期结果**:
- 后端返回 404 错误
- 前端显示错误提示

### 场景 3: 重复传输

**测试步骤**:

1. **设备 A 创建传输会话**
   ```
   创建会话，获取 session_id
   ```

2. **设备 B 第一次接受**
   ```
   扫描二维码，接受传输
   验证密钥已保存
   ```

3. **设备 B 第二次接受**
   ```
   再次扫描同一个二维码
   应显示错误或提示会话已处理
   ```

**预期结果**:
- 第二次接受时返回错误（会话状态已为 accepted）

### 场景 4: 错误的二维码

**测试步骤**:

1. **使用无效的二维码**
   ```
   生成一个非 E2EE 传输的二维码
   设备 B: 尝试扫描
   ```

2. **使用格式错误的二维码**
   ```
   使用 JSON 格式错误的二维码
   设备 B: 尝试扫描
   ```

**预期结果**:
- 前端验证二维码格式
- 显示"无效的二维码"提示

---

## 🔍 调试命令

### 查看传输会话

```sql
-- 查看所有会话
SELECT
    session_id,
    from_uid,
    from_device_id,
    to_uid,
    to_device_id,
    status,
    expires_at,
    created_at
FROM e2ee_transfer_sessions
ORDER BY created_at DESC;

-- 查看待处理会话
SELECT
    session_id,
    from_uid,
    from_device_id,
    to_uid,
    status,
    expires_at
FROM e2ee_transfer_sessions
WHERE status = 'pending'
  AND expires_at > NOW();

-- 查看特定用户会话
SELECT * FROM e2ee_transfer_sessions
WHERE to_uid = 123
ORDER BY created_at DESC;
```

### 手动清理过期会话

```sql
-- 执行清理函数
SELECT cleanup_expired_transfer_sessions();

-- 手动删除过期会话
DELETE FROM e2ee_transfer_sessions
WHERE expires_at < NOW();
```

### 测试 API

```bash
# 创建传输会话
curl -X POST http://localhost:9800/v1/e2ee/transfer/create \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"to_uid": 123}'

# 获取待处理列表
curl -X GET http://localhost:9800/v1/e2ee/transfer/pending \
  -H "Authorization: Bearer YOUR_TOKEN"

# 获取会话信息
curl -X GET "http://localhost:9800/v1/e2ee/transfer/info?session_id=xxx" \
  -H "Authorization: Bearer YOUR_TOKEN"

# 接受传输
curl -X POST http://localhost:9800/v1/e2ee/transfer/accept \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"session_id": "xxx", "device_id": "new_device_id"}'

# 确认传输
curl -X POST http://localhost:9800/v1/e2ee/transfer/confirm \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"session_id": "xxx"}'
```

---

## 🐛 常见问题

### 问题 1: 扫描二维码后无响应

**可能原因**:
- 二维码数据格式错误
- session_id 无效

**解决方案**:
1. 检查二维码内容：`{"type":"e2ee_transfer","session_id":"uuid"}`
2. 检查后端日志确认会话存在

### 问题 2: 解密密钥失败

**可能原因**:
- 新设备没有生成密钥对
- 加密算法不匹配

**解决方案**:
1. 确认新设备已生成密钥对
2. 检查 `e2ee_transfer_logic.erl` 中的加密实现
3. 检查前端解密实现

### 问题 3: 传输会话创建失败

**可能原因**:
- 接收方用户不存在
- 发送方没有私钥

**解决方案**:
1. 确认接收方用户已注册
2. 确认发送方已生成 E2EE 密钥

### 问题 4: 数据库表不存在

**错误信息**: `relation "e2ee_transfer_sessions" does not exist`

**解决方案**:
```bash
# 执行数据库迁移
psql -U imboy -d imboy -f priv/migrations/00000046_e2ee_device_transfer.sql
```

---

## ✅ 测试检查清单

### 功能验证

- [ ] 创建传输会话成功
- [ ] 二维码正确显示
- [ ] 扫描二维码成功解析
- [ ] RSA 解密成功
- [ ] 密钥保存成功
- [ ] 传输状态正确更新
- [ ] 会话过期正确处理
- [ ] 错误二维码正确处理

### 数据验证

- [ ] session_id 为有效 UUID
- [ ] encrypted_key_bundle 为有效 Base64
- [ ] 密钥解密后与原始私钥一致
- [ ] 设备 ID 和密钥 ID 正确传输

### 安全验证

- [ ] 加密使用 RSA-OAEP-256
- [ ] 私钥不在服务端存储
- [ ] 会话在 5 分钟后过期
- [ ] 只有接收方能解密密钥

---

**文档版本**: 1.0
**最后更新**: 2026-01-31
