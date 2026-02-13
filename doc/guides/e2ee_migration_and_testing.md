# E2EE 密钥恢复功能 - 数据库迁移与测试指南

> **功能版本**: 0.7.3
> **最后更新**: 2026-01-31
> **用途**: 执行数据库迁移并验证功能

---

## 📋 迁移前准备

### 环境检查

- [ ] 确认 PostgreSQL 18+ 已安装并运行
- [ ] 确认数据库连接配置正确
- [ ] 备份当前数据库（可选但推荐）

### 备份数据库

```bash
# 备份当前数据库
pg_dump -h localhost -U imboy -d imboy_local > backup_before_e2ee_$(date +%Y%m%d_%H%M%S).sql

# 或使用 pg_dump 更安全的选项
pg_dump -h localhost -U imboy -d imboy_local -F c -f backup_before_e2ee_$(date +%Y%m%d_%H%M%S).dump
```

---

## 🗄️ 数据库迁移执行

### 方法 1: 使用 psql 命令行

```bash
# 连接到数据库
psql -h localhost -U imboy -d imboy_local

# 执行迁移文件
\i priv/migrations/00000044_e2ee.sql

# 验证表结构
\d e2ee_local_backups
\d e2ee_transfer_sessions
\d e2ee_social_shards

# 退出
\q
```

### 方法 2: 使用单个 SQL 文件

```bash
# 合并所有迁移文件
cat priv/migrations/00000044_e2ee.sql \
    > priv/migrations/e2ee_all_in_one.sql

# 执行
psql -h localhost -U imboy -d imboy_local -f priv/migrations/e2ee_all_in_one.sql
```

### 方法 3: 使用 Erlang 节点

```erlang
% 在节点 shell 中执行
% 启动节点
IMBOYENV=local make run

% 在另一个终端连接
_rel/imboy/bin/imboy remote_console

% 执行迁移
{ok, _} = elib_pg:query(<<"
    -- 迁移 1: 本地备份表
    CREATE TABLE IF NOT EXISTS e2ee_local_backups (
        id BIGSERIAL PRIMARY KEY,
        uid BIGINT NOT NULL,
        device_id VARCHAR(64) NOT NULL,
        key_version VARCHAR(32) NOT NULL DEFAULT 'latest',
        backup_name VARCHAR(255) NOT NULL,
        encrypted_data TEXT NOT NULL,
        salt TEXT NOT NULL,
        nonce TEXT NOT NULL,
        tag TEXT NOT NULL,
        file_size BIGINT NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
    );
    CREATE INDEX IF NOT EXISTS idx_e2ee_local_backups_uid ON e2ee_local_backups(uid);
    CREATE INDEX IF NOT EXISTS idx_e2ee_local_backups_key_version ON e2ee_local_backups(key_version);
">>),

{ok, _} = elib_pg:query(<<"
    -- 迁移 2: 设备传输表
    CREATE TABLE IF NOT EXISTS e2ee_transfer_sessions (
        id BIGSERIAL PRIMARY KEY,
        session_id VARCHAR(36) UNIQUE NOT NULL,
        from_uid BIGINT NOT NULL,
        from_device_id VARCHAR(64) NOT NULL,
        to_uid BIGINT NOT NULL,
        to_device_id VARCHAR(64),
        status VARCHAR(20) NOT NULL DEFAULT 'pending',
        encrypted_key_bundle TEXT NOT NULL,
        expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
    );
    CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_session_id ON e2ee_transfer_sessions(session_id);
    CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_from_uid ON e2ee_transfer_sessions(from_uid);
    CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_to_uid ON e2ee_transfer_sessions(to_uid);
    CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_status ON e2ee_transfer_sessions(status);
    CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_expires_at ON e2ee_transfer_sessions(expires_at);
">>),

{ok, _} = elib_pg:query(<<"
    -- 迁移 3: 社交分片表
    CREATE TABLE IF NOT EXISTS e2ee_social_shards (
        id BIGSERIAL PRIMARY KEY,
        uid BIGINT NOT NULL,
        key_version VARCHAR(32) NOT NULL DEFAULT 'latest',
        shard_index INTEGER NOT NULL,
        total_shards INTEGER NOT NULL,
        threshold INTEGER NOT NULL,
        encrypted_shard TEXT NOT NULL,
        proxy_uid BIGINT NOT NULL,
        shard_id VARCHAR(64) UNIQUE NOT NULL,
        status VARCHAR(20) NOT NULL DEFAULT 'active',
        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
        used_at TIMESTAMP WITH TIME ZONE
    );
    CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_uid ON e2ee_social_shards(uid);
    CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_key_version ON e2ee_social_shards(key_version);
    CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_proxy_uid ON e2ee_social_shards(proxy_uid);
    CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_shard_id ON e2ee_social_shards(shard_id);
    CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_status ON e2ee_social_shards(status);
">>).
```

---

## ✅ 迁移验证

### 检查表是否创建成功

```sql
-- 查看所有 E2EE 相关表
SELECT table_name, table_type
FROM information_schema.tables
WHERE table_schema = 'public'
  AND table_name LIKE 'e2ee%';

-- 预期输出
--      table_name      | table_type
-- ---------------------+------------
--  e2ee_local_backups  | BASE TABLE
--  e2ee_transfer_sessions | BASE TABLE
--  e2ee_social_shards  | BASE TABLE
```

### 检查索引是否创建

```sql
-- 查看索引
SELECT
    tablename,
    indexname,
    indexdef
FROM pg_indexes
WHERE tablename LIKE 'e2ee%'
ORDER BY tablename, indexname;
```

### 测试插入数据

```sql
-- 测试 e2ee_local_backups
INSERT INTO e2ee_local_backups (
    uid, device_id, key_version, backup_name,
    encrypted_data, salt, nonce, tag, file_size
) VALUES (
    1, 'test-device', 'v1', 'Test Backup',
    'encrypted_data_here', 'salt_here', 'nonce_here', 'tag_here', 1024
);

-- 测试 e2ee_transfer_sessions
INSERT INTO e2ee_transfer_sessions (
    session_id, from_uid, from_device_id, to_uid,
    status, encrypted_key_bundle, expires_at
) VALUES (
    'test-session-123', 1, 'device-1', 2,
    'pending', 'encrypted_key_bundle', NOW() + INTERVAL '5 minutes'
);

-- 测试 e2ee_social_shards
INSERT INTO e2ee_social_shards (
    uid, key_version, shard_index, total_shards, threshold,
    encrypted_shard, proxy_uid, shard_id
) VALUES (
    1, 'v1', 0, 3, 2,
    'encrypted_shard_here', 2, 'shard-id-123'
);

-- 验证数据
SELECT * FROM e2ee_local_backups;
SELECT * FROM e2ee_transfer_sessions;
SELECT * FROM e2ee_social_shards;

-- 清理测试数据
DELETE FROM e2ee_local_backups WHERE uid = 1;
DELETE FROM e2ee_transfer_sessions WHERE session_id = 'test-session-123';
DELETE FROM e2ee_social_shards WHERE uid = 1;
```

---

## 🧪 单元测试

### Erlang 后端测试

```bash
# 进入项目目录
cd /Users/leeyi/project/imboy.pub/imboy

# 运行所有测试
make eunit

# 运行特定测试
erl -noshell -eval "eunit:test(shamir_secret_sharing, [verbose])" -s init stop

# 运行 E2EE 相关测试
erl -noshell \
    -eval "eunit:test([shamir_secret_sharing], [verbose])" \
    -s init stop
```

### 预期测试结果

```erlang
% shamir_secret_sharing_tests.erl
test_split_and_combine:     ok
test_threshold_recovery:    ok
test_insufficient_shares:   ok
test_parameter_validation:  ok
test_different_combinations:ok
test_consistency:           ok

All 6 tests passed.
```

### Flutter 前端测试

```bash
# 进入 Flutter 项目目录
cd /Users/leeyi/project/imboy.pub/imboyapp

# 运行所有测试
flutter test

# 运行特定测试文件
flutter test test/service/shamir_secret_sharing_test.dart
flutter test test/service/e2ee_social_service_test.dart

# 运行 E2EE 相关测试
flutter test --name "E2EE|Shamir"

# 查看覆盖率
flutter test --coverage
genhtml coverage/lcov.info -o coverage/html
open coverage/html/index.html
```

### 预期测试结果

```
00:00 +0: shamir_secret_sharing split/combine basic
00:00 +1: shamir_secret_sharing threshold recovery
00:00 +2: shamir_secret_sharing insufficient shares
00:00 +3: shamir_secret_sharing parameter validation
00:00 +4: shamir_secret_sharing different combinations
00:00 +5: shamir_secret_sharing consistency
00:00 +6: e2ee_social_service splitSecret
00:00 +7: e2ee_social_service canRecover
00:00 +8: e2ee_social_service error handling

All tests passed!
```

---

## 🔗 集成测试

### 测试环境设置

```bash
# 1. 启动后端服务
IMBOYENV=local make run

# 2. 启动前端应用（另一终端）
cd /Users/leeyi/project/imboy.pub/imboyapp
flutter run

# 3. 准备测试账号
# - 账号 A: 主要测试账号
# - 账号 B、C、D: 好友账号（作为代理）
```

### 测试场景清单

参考: `doc/guides/e2ee_key_recovery_test_scenarios.md`

| 场景 | 测试内容 | 预期结果 |
|------|---------|---------|
| 场景 1.1 | 导出本地备份 | 生成 .imboy_backup 文件 |
| 场景 1.2 | 导入本地备份 | 密钥恢复成功 |
| 场景 1.3 | 密码错误测试 | 显示错误提示 |
| 场景 2.1 | 创建传输会话 | 二维码正确显示 |
| 场景 2.2 | 接收传输 | 传输成功 |
| 场景 2.3 | 会话过期测试 | 拒绝过期会话 |
| 场景 3.1 | 创建恢复分片 | 分片创建成功 |
| 场景 3.2 | 查看已创建分片 | 代理信息正确显示 |
| 场景 4.1 | 恢复密钥 | 密钥恢复成功 |
| 场景 4.2 | 分片不足测试 | 显示错误提示 |
| 场景 4.3 | 查看分片状态 | 状态正确更新 |

### API 测试命令

```bash
# 设置环境变量
export API_URL="http://localhost:9800"
export TOKEN="your_jwt_token"

# 1. 创建分片（方法 B）
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
    "key_version": "latest",
    "shard_ids": ["shard-id-1", "shard-id-2"]
  }'

# 4. 创建传输会话（方法 A）
curl -X POST "$API_URL/v1/e2ee/transfer/create" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "to_uid": "encoded_uid_here",
    "encrypted_key_bundle": "..."
  }'
```

---

## 🐛 故障排除

### 常见问题

#### 1. 表已存在错误

```sql
-- 如果表已存在，检查是否可以安全删除
-- 警告：删除表会丢失所有数据！

DROP TABLE IF EXISTS e2ee_social_shards CASCADE;
DROP TABLE IF EXISTS e2ee_transfer_sessions CASCADE;
DROP TABLE IF EXISTS e2ee_local_backups CASCADE;

-- 然后重新执行迁移
```

#### 2. 权限错误

```sql
-- 授予用户权限
GRANT ALL PRIVILEGES ON TABLE e2ee_local_backups TO imboy;
GRANT ALL PRIVILEGES ON TABLE e2ee_transfer_sessions TO imboy;
GRANT ALL PRIVILEGES ON TABLE e2ee_social_shards TO imboy;

GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO imboy;
```

#### 3. 测试失败

```bash
# 检查依赖是否正确安装
# Flutter
flutter pub get
flutter doctor

# Erlang
erl -version
which psql
```

### 回滚迁移

```sql
-- 如果需要回滚，执行以下命令
DROP TABLE IF EXISTS e2ee_social_shards CASCADE;
DROP TABLE IF EXISTS e2ee_transfer_sessions CASCADE;
DROP TABLE IF EXISTS e2ee_local_backups CASCADE;

-- 验证删除
SELECT table_name
FROM information_schema.tables
WHERE table_schema = 'public'
  AND table_name LIKE 'e2ee%';
```

---

## 📊 测试报告模板

```markdown
### E2EE 密钥恢复功能 - 测试报告

**测试日期**: 2026-01-31
**测试环境**: local
**测试人员**: [姓名]

#### 数据库迁移
- [ ] 迁移 00000045_e2ee_key_recovery.sql - 通过/失败
- [ ] 迁移 00000046_e2ee_device_transfer.sql - 通过/失败
- [ ] 迁移 00000047_e2ee_social_shards.sql - 通过/失败

#### 单元测试
- [ ] shamir_secret_sharing_tests.erl - 6/6 通过
- [ ] shamir_secret_sharing_test.dart - 6/6 通过
- [ ] e2ee_social_service_test.dart - 3/3 通过

#### 集成测试
| 场景 | 结果 | 备注 |
|------|------|------|
| 场景 1.1 | ☐ 通过 | |
| 场景 1.2 | ☐ 通过 | |
| 场景 2.1 | ☐ 通过 | |
| 场景 3.1 | ☐ 通过 | |
| 场景 4.1 | ☐ 通过 | |

#### 发现的问题
1. [问题描述]
   - 重现步骤:
   - 期望行为:
   - 实际行为:
   - 严重程度: 低/中/高

#### 建议
- [建议1]
- [建议2]

#### 总结
- 测试通过率: X%
- 阻塞性问题: 0
- 建议发布: 是/否
```

---

## ✅ 完成检查清单

### 迁移完成检查
- [ ] 所有三个表创建成功
- [ ] 所有索引创建成功
- [ ] 测试数据插入成功
- [ ] 测试数据清理成功

### 测试完成检查
- [ ] Erlang 单元测试通过
- [ ] Flutter 单元测试通过
- [ ] 方法 C（本地备份）集成测试通过
- [ ] 方法 A（设备传输）集成测试通过
- [ ] 方法 B（社交恢复）集成测试通过

### 文档检查
- [ ] API 文档完整
- [ ] 用户指南完整
- [ ] 测试场景文档完整
- [ ] 测试报告填写完整

---

**文档版本**: 1.0
**最后更新**: 2026-01-31
