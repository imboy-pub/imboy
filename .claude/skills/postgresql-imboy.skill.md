# PostgreSQL Imboy Skill

## Description
imboy 项目的 PostgreSQL 数据库设计、查询优化和维护技能集。

## Environment
- **版本**: PostgreSQL 18+
- **文档**: `./doc/postgresql/`
- **Schema 版本**: vsn0.1（开发中，以首个发布版为准）

## Schema Design Principles

### 命名约定
- **表名**: 小写，下划线分隔，使用复数形式 (e.g., `users`, `group_members`, `msg_c2c`)
- **字段名**: 小写，下划线分隔 (e.g., `created_at`, `is_deleted`)
- **索引**: `idx_表名_字段名` (e.g., `idx_users_username`)
- **唯一约束**: `uk_表名_字段名` (e.g., `uk_staging_msg_id`)
- **外键**: `fk_表名_引用表_字段` (e.g., `fk_messages_users_user_id`)

### 数据类型

```sql
-- 主键
BIGSERIAL PRIMARY KEY

-- 用户 ID（引用用户表）
BIGINT NOT NULL

-- 时间戳（使用 TIMESTAMPTZ 支持时区）
TIMESTAMPTZ NOT NULL DEFAULT NOW()

-- 状态
SMALLINT NOT NULL DEFAULT 0

-- 文本
VARCHAR(255)  -- 短文本
TEXT          -- 长文本

-- JSONB（推荐用于灵活数据）
JSONB         -- 灵活的 JSON 数据

-- 数组
BIGINT[]      -- 用户 ID 列表（群聊场景）
VARCHAR[]     -- 字符串数组
```

### 通用字段

```sql
id              BIGSERIAL PRIMARY KEY,
created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
is_deleted      SMALLINT NOT NULL DEFAULT 0
```

## 消息相关表

### 正式消息表

#### msg_c2c (单聊消息表)

```sql
CREATE TABLE IF NOT EXISTS msg_c2c (
    id BIGSERIAL PRIMARY KEY,
    msg_id VARCHAR(64) NOT NULL,
    from_id BIGINT NOT NULL,
    to_id BIGINT NOT NULL,
    payload JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL,
    server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- 索引
CREATE INDEX idx_msg_c2c_msg_id ON msg_c2c(msg_id);
CREATE INDEX idx_msg_c2c_from_id ON msg_c2c(from_id);
CREATE INDEX idx_msg_c2c_to_id ON msg_c2c(to_id);
CREATE INDEX idx_msg_c2c_created_at ON msg_c2c(created_at DESC);
```

#### msg_c2g (群聊消息表)

```sql
CREATE TABLE IF NOT EXISTS msg_c2g (
    id BIGSERIAL PRIMARY KEY,
    msg_id VARCHAR(64) NOT NULL,
    from_id BIGINT NOT NULL,
    group_id BIGINT NOT NULL,
    payload JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL,
    server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

#### msg_s2c (系统消息表)

```sql
CREATE TABLE IF NOT EXISTS msg_s2c (
    id BIGSERIAL PRIMARY KEY,
    msg_id VARCHAR(64) NOT NULL,
    from_id BIGINT NOT NULL,
    to_id BIGINT NOT NULL,
    payload JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL,
    server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

### 备份表 (消息队列)

#### msg_store_staging (消息写入队列备份表)

**用途**: 消息队列的持久化备份，确保零丢失

```sql
CREATE TABLE IF NOT EXISTS msg_store_staging (
    id BIGSERIAL PRIMARY KEY,
    msg_type VARCHAR(10) NOT NULL CHECK (msg_type IN ('c2c', 'c2g', 's2c', 'c2s')),
    msg_id VARCHAR(64) NOT NULL,
    payload JSONB NOT NULL,
    from_id BIGINT NOT NULL,
    to_id BIGINT,
    to_id_list BIGINT[],
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    server_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    retry_count INTEGER DEFAULT 0,
    processed_at TIMESTAMPTZ,
    error_msg TEXT,
    CONSTRAINT uk_staging_msg_id UNIQUE (msg_type, msg_id)
);

-- 索引
CREATE INDEX idx_staging_created_at ON msg_store_staging(created_at);
CREATE INDEX idx_staging_msg_type ON msg_store_staging(msg_type);
CREATE INDEX idx_staging_to_id ON msg_store_staging(to_id);
CREATE INDEX idx_staging_processed_at ON msg_store_staging(processed_at);
```

**设计说明**:

| 字段 | 说明 |
|------|------|
| `msg_type` | 消息类型：c2c(单聊)、c2g(群聊)、s2c(系统)、c2s(机器人) |
| `msg_id` | 消息唯一 ID，与正式表关联 |
| `payload` | JSONB 格式的消息内容 |
| `to_id` | 单聊接收者 ID |
| `to_id_list` | 群聊接收者 ID 列表 |
| `retry_count` | 重试次数，用于监控 |
| `processed_at` | 处理完成时间，NULL 表示未处理 |
| `error_msg` | 错误信息，用于故障排查 |

**工作流程**:

```
1. Logic 层发送消息时先写入备份表 (stage)
2. 消息入队到 shq (内存队列)
3. Worker 批量从队列取出消息
4. 写入正式表 (msg_c2c/msg_c2g/msg_s2c)
5. 删除备份表记录 (unstage)
6. 启动时从备份表恢复未处理消息
```

**监控查询**:

```sql
-- 查看待处理消息
SELECT msg_type, COUNT(*)
FROM msg_store_staging
WHERE processed_at IS NULL
GROUP BY msg_type;

-- 查看处理失败的消息
SELECT msg_type, msg_id, error_msg, retry_count
FROM msg_store_staging
WHERE processed_at IS NOT NULL AND error_msg IS NOT NULL
ORDER BY created_at DESC
LIMIT 10;

-- 查看备份表统计
SELECT
    COUNT(*) FILTER (WHERE processed_at IS NULL) as pending,
    COUNT(*) FILTER (WHERE processed_at IS NOT NULL) as processed,
    COUNT(*) as total
FROM msg_store_staging;
```

**清理已处理记录**:

```sql
-- 清理超过 1 小时的已处理记录
DELETE FROM msg_store_staging
WHERE processed_at IS NOT NULL
  AND processed_at < NOW() - INTERVAL '1 hour';
```

## Query Patterns (Erlang)

### 基础查询

```erlang
% 单行查询
Sql = "SELECT id, username FROM users WHERE id = $1 AND is_deleted = 0",
{ok, _, [{UserId, Username}]} = imboy_pg:query(Sql, [UserId]).

% 多行查询
Sql = "SELECT id, username FROM users WHERE is_deleted = $1 ORDER BY id LIMIT $2",
{ok, _, Rows} = imboy_pg:query(Sql, [0, 10]),
Users = [{Id, Username} || {Id, Username} <- Rows].
```

### 参数化查询（防 SQL 注射）

```erlang
% ✅ 正确 - 使用参数
Sql = "SELECT * FROM users WHERE username = $1",
imboy_pg:query(Sql, [Username]).

% ❌ 错误 - 字符串拼接（SQL 注射风险）
Sql = "SELECT * FROM users WHERE username = '" ++ Username ++ "'",
imboy_pg:query(Sql, []).
```

### IN 子句（数组参数）

```erlang
% 使用 PostgreSQL 的 ANY 语法
Ids = [1, 2, 3],
Sql = "SELECT * FROM users WHERE id = ANY($1)",
imboy_pg:query(Sql, [Ids]).

% 或者使用数组展开
Sql = "SELECT * FROM group_members WHERE group_id = $1 AND user_id = ANY($2)",
imboy_pg:query(Sql, [GroupId, MemberIds]).
```

### JSONB 操作

```erlang
% 查询 JSONB 字段
Sql = "SELECT msg_id, payload FROM msg_c2c WHERE payload->>'msg_type' = $1",
imboy_pg:query(Sql, [<<"text">>]).

% 更新 JSONB 字段
Sql = "UPDATE msg_c2c SET payload = jsonb_set(payload, '{read}', 'true') WHERE id = $1",
imboy_pg:execute(Sql, [MsgId]).
```

### 事务处理

```erlang
% 使用 imboy_pg:transaction
Fun = fun() ->
    % 插入消息
    {ok, _} = imboy_pg:query("INSERT INTO msg_c2c ...", [Params1]),
    % 更新会话
    {ok, _} = imboy_pg:query("UPDATE conversations ...", [Params2]),
    {ok, success}
end,
{ok, Result} = imboy_pg:transaction(Fun).
```

## Performance Optimization

### 索引策略

```sql
-- 单列索引
CREATE INDEX idx_users_username ON users(username);

-- 复合索引（查询顺序重要）
CREATE INDEX idx_messages_user_id_created_at ON messages(user_id, created_at);

-- 部分索引（只索引满足条件的行）
CREATE INDEX idx_users_active ON users(username) WHERE is_deleted = 0;

-- 唯一索引（含软删除）
CREATE UNIQUE INDEX idx_users_username
ON users(username)
WHERE is_deleted = 0;

-- JSONB 字段索引
CREATE INDEX idx_msg_c2c_payload_type ON msg_c2c USING GIN (payload);
```

### 查询优化

```erlang
% 使用 LIMIT 限制结果
Sql = "SELECT * FROM messages WHERE user_id = $1 ORDER BY created_at DESC LIMIT $2",
imboy_pg:query(Sql, [UserId, 20]).

% 避免 SELECT *
Sql = "SELECT id, content FROM messages WHERE id = $1",
imboy_pg:query(Sql, [MsgId]).

% 使用游标分页（性能优于 OFFSET）
Sql = "SELECT * FROM messages WHERE user_id = $1 AND id > $2 ORDER BY id LIMIT $3",
imboy_pg:query(Sql, [UserId, LastId, 20]).
```

### 批量插入

```erlang
% 使用 COPY 批量导入（最快）
% 或者使用 VALUES 列表
Sql = "INSERT INTO msg_c2c (msg_id, from_id, to_id, payload) VALUES ",
Values = lists:join(",", [
    "($1, $2, $3, $4)",
    "($5, $6, $7, $8)",
    ...
]),
imboy_pg:query(Sql ++ Values, Params).
```

### 连接池配置

确保 PostgreSQL 连接池正确配置（参考 `config/sys.config`）：

```erlang
{pooler, [
    {pools, [
        [{name, "imboy_pg"},
         {max_count, 20},      % 最大连接数
         {init_count, 5},      % 初始连接数
         {start_mfa, {imboy_pg, start_link, ["imboy_pg"]}}]
    ]}
]}.
```

## Monitoring & Maintenance

### 慢查询日志

检查 PostgreSQL 慢查询日志，优化耗时操作：

```bash
# 查看慢查询配置
SHOW log_min_duration_statement;

# 查看当前运行的查询
SELECT pid, now() - query_start as duration, query
FROM pg_stat_activity
WHERE state = 'active'
ORDER BY duration DESC;
```

### 连接监控

```erlang
% 在 imboy shell 中检查连接池状态
pooler:status().

% 或查看 PostgreSQL 连接数
% SELECT count(*) FROM pg_stat_activity;
```

### 表大小监控

```sql
SELECT
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
WHERE schemaname = 'public'
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;
```

## Migration Strategy

### Schema 变更流程

1. 在 `./doc/postgresql/migrations/` 创建迁移文件
2. 编写迁移 SQL（使用版本号命名，如 `00000043_msg_store_staging.sql`）
3. 在测试环境验证
4. 生产环境执行（使用 relup 平滑升级）

### 迁移文件命名

```
00000001_initial_schema.sql
00000002_add_user_table.sql
00000043_msg_store_staging.sql
...
```

### 迁移最佳实践

1. **向前兼容**: 新增字段使用 DEFAULT 值
2. **索引优先**: 先创建索引，再添加约束
3. **分步执行**: 大表变更分多次执行
4. **回滚方案**: 每个迁移都要有对应的回滚脚本

## Best Practices

1. **始终使用参数化查询** - 防止 SQL 注射
2. **避免 N+1 查询** - 使用 JOIN 或批量查询
3. **合理使用索引** - 不过度索引，但确保常用查询有索引
4. **使用 JSONB** - 灵活的非结构化数据
5. **软删除优先** - 保留数据用于审计和恢复
6. **事务要小** - 避免长事务
7. **监控连接池** - 避免连接泄漏
8. **使用 TIMESTAMPTZ** - 支持时区的时间戳
9. **批量操作优化** - 使用 COPY 或 VALUES 列表
10. **定期清理** - 清理备份表和过期数据

## Debugging

### 查看查询计划

```sql
EXPLAIN ANALYZE SELECT * FROM users WHERE username = 'test';
```

### 检查死锁

```sql
SELECT * FROM pg_stat_activity WHERE datname = 'imboy_v1' AND state = 'idle in transaction';
```

### 表分析（更新统计信息）

```sql
ANALYZE msg_c2c;
VACUUM ANALYZE msg_c2c;
```

## Common Operations

### 插入并返回 ID

```sql
INSERT INTO users (username, password, created_at, updated_at)
VALUES ($1, $2, NOW(), NOW())
RETURNING id;
```

### 软删除

```erlang
Sql = "UPDATE users SET is_deleted = 1, updated_at = NOW() WHERE id = $1",
imboy_pg:execute(Sql, [UserId]).
```

### 分页查询

```erlang
% 使用游标分页（推荐）
Sql = "SELECT * FROM messages
       WHERE user_id = $1 AND is_deleted = 0
       AND id > $2
       ORDER BY id ASC
       LIMIT $3",
imboy_pg:query(Sql, [UserId, LastId, Limit]).

% 或使用 OFFSET（小数据量）
Sql = "SELECT * FROM messages
       WHERE user_id = $1 AND is_deleted = 0
       ORDER BY created_at DESC
       LIMIT $2 OFFSET $3",
imboy_pg:query(Sql, [UserId, Limit, Offset]).
```

### 统计查询

```sql
-- 消息统计
SELECT
    DATE(created_at) as date,
    COUNT(*) as count
FROM msg_c2c
WHERE created_at >= NOW() - INTERVAL '7 days'
GROUP BY DATE(created_at)
ORDER BY date DESC;
```

## When to Use This Skill

当用户需要以下操作时使用：
- 设计新的数据表结构
- 编写数据库查询
- 优化 SQL 性能
- 处理数据库事务
- 数据库迁移
- 调试数据库问题
- 创建备份表
- 消息队列持久化设计
