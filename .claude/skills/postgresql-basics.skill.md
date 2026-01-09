# PostgreSQL 18 基础学习技能

## Description
面向 PostgreSQL 初学者的基础学习指南，涵盖 SQL 基础、表设计、索引优化和 PG 18 新特性。

---

## 🎯 学习目标

完成本指南后，您将能够：
- 理解 PostgreSQL 基本概念和 SQL 语法
- 设计合理的数据库表结构
- 使用索引优化查询性能
- 了解 PG 18 的新特性
- 在 Erlang 中安全地操作数据库

---

## 📖 第一部分：PostgreSQL 基础

### 1. 什么是 PostgreSQL？

PostgreSQL 是**最先进的开源关系型数据库**。

**核心特点：**
- ✅ **ACID 事务** - 数据一致性保证
- ✅ **MVCC** - 多版本并发控制，读写不阻塞
- ✅ **丰富的数据类型** - JSON/JSONB、数组、地理位置等
- ✅ **强大的扩展** - 全文搜索、时序数据、PostGIS 等

### 2. 连接数据库

```bash
# 命令行连接
psql -h localhost -U username -d database_name

# Imboy 项目连接示例
psql -h localhost -U imboy_user -d imboy_v1

# 常用命令
\l          # 列出所有数据库
\dt         # 列出当前表
\d table    # 查看表结构
\q          # 退出
```

---

## 🗄️ 第二部分：SQL 基础

### 1. 创建表 (CREATE TABLE)

```sql
-- 基本表结构
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,           -- 自增主键
    username VARCHAR(50) NOT NULL,      -- 用户名，不能为空
    email VARCHAR(100) UNIQUE,          -- 邮箱，唯一
    password_hash TEXT NOT NULL,        -- 密码哈希
    created_at TIMESTAMPTZ DEFAULT NOW(),  -- 创建时间（带时区）
    updated_at TIMESTAMPTZ DEFAULT NOW()   -- 更新时间
);

-- 添加注释
COMMENT ON TABLE users IS '用户表';
COMMENT ON COLUMN users.email IS '用户邮箱';
```

**常用数据类型：**
| 类型 | 说明 | 示例 |
|------|------|------|
| `BIGINT` / `BIGSERIAL` | 大整数 / 自增大整数 | `id BIGSERIAL PRIMARY KEY` |
| `VARCHAR(n)` | 变长字符串 | `username VARCHAR(50)` |
| `TEXT` | 不限长度字符串 | `content TEXT` |
| `TIMESTAMPTZ` | 时间戳（带时区） | `created_at TIMESTAMPTZ` |
| `JSONB` | 二进制 JSON（推荐） | `payload JSONB` |
| `BOOLEAN` | 布尔值 | `is_active BOOLEAN` |

### 2. 插入数据 (INSERT)

```sql
-- 基本插入
INSERT INTO users (username, email, password_hash)
VALUES ('alice', 'alice@example.com', 'hash_value');

-- 插入多行
INSERT INTO users (username, email, password_hash)
VALUES
    ('alice', 'alice@example.com', 'hash1'),
    ('bob', 'bob@example.com', 'hash2');

-- 使用 DEFAULT
INSERT INTO users (username, email)
VALUES ('charlie', 'charlie@example.com');
-- password_hash 会使用默认值（如果设置了）
```

### 3. 查询数据 (SELECT)

```sql
-- 基本查询
SELECT * FROM users;
SELECT username, email FROM users;

-- WHERE 条件
SELECT * FROM users WHERE id = 1;
SELECT * FROM users WHERE email LIKE '%@example.com';

-- 排序
SELECT * FROM users ORDER BY created_at DESC;
SELECT * FROM users ORDER BY username ASC, created_at DESC;

-- 限制结果
SELECT * FROM users LIMIT 10;
SELECT * FROM users LIMIT 10 OFFSET 20;  -- 分页

-- 聚合
SELECT COUNT(*) FROM users;
SELECT COUNT(*) FROM users WHERE is_active = true;
```

### 4. 更新数据 (UPDATE)

```sql
-- 基本更新
UPDATE users SET email = 'newemail@example.com' WHERE id = 1;

-- 更新多个字段
UPDATE users
SET email = 'new@example.com',
    updated_at = NOW()
WHERE id = 1;
```

### 5. 删除数据 (DELETE)

```sql
-- 基本删除
DELETE FROM users WHERE id = 1;

-- 删除符合条件的所有行
DELETE FROM users WHERE created_at < '2024-01-01';
```

---

## 🎨 第三部分：表设计最佳实践

### 1. Imboy 项目命名规范

```sql
-- 表名：小写，下划线分隔，复数形式
users
group_members
msg_c2c

-- 字段名：小写，下划线分隔
created_at
is_deleted

-- 索引命名
idx_users_username          -- 普通索引
uk_users_email              -- 唯一索引 (unique key)
fk_groups_owner_id          -- 外键 (foreign key)
```

### 2. 约束 (Constraints)

```sql
-- 主键约束
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY
);

-- 唯一约束
CREATE TABLE users (
    email VARCHAR(100) UNIQUE
);
-- 或
ALTER TABLE users ADD CONSTRAINT uk_users_email UNIQUE (email);

-- 检查约束
CREATE TABLE users (
    age INTEGER CHECK (age >= 0 AND age < 150)
);

-- 非空约束
CREATE TABLE users (
    username VARCHAR(50) NOT NULL
);
```

### 3. 外键关系

```sql
-- 用户表
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL
);

-- 消息表
CREATE TABLE messages (
    id BIGSERIAL PRIMARY KEY,
    from_id BIGINT NOT NULL,
    to_id BIGINT NOT NULL,
    payload TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    FOREIGN KEY (from_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (to_id) REFERENCES users(id) ON DELETE CASCADE
);
```

**外键选项：**
| 选项 | 说明 |
|------|------|
| `ON DELETE CASCADE` | 删除用户时，级联删除其消息 |
| `ON DELETE SET NULL` | 删除用户时，消息的 from_id 设为 NULL |
| `ON DELETE RESTRICT` | 删除用户时，如果有消息则禁止删除 |

---

## 🚀 第四部分：索引优化

### 1. 创建索引

```sql
-- B-tree 索引（默认）
CREATE INDEX idx_users_username ON users(username);

-- 唯一索引
CREATE UNIQUE INDEX uk_users_email ON users(email);

-- 复合索引
CREATE INDEX idx_messages_from_to ON messages(from_id, to_id);

-- 表达式索引
CREATE INDEX idx_users_lower_username ON users(LOWER(username));

-- 部分索引（只索引符合条件的行）
CREATE INDEX idx_active_users ON users(id) WHERE is_active = true;
```

### 2. JSONB 索引

```sql
-- GIN 索引（用于 JSONB）
CREATE TABLE events (
    id BIGSERIAL PRIMARY KEY,
    payload JSONB NOT NULL
);

-- 创建 GIN 索引
CREATE INDEX idx_events_payload ON events USING GIN (payload);

-- 查询示例
SELECT * FROM events WHERE payload @> '{"type": "login"}';
```

### 3. 何时使用索引

| 场景 | 是否使用索引 | 原因 |
|------|-------------|------|
| `WHERE id = 1` | ✅ 是 | 精确匹配 |
| `WHERE username LIKE 'alice%'` | ✅ 是 | 前缀匹配 |
| `WHERE username LIKE '%alice%'` | ❌ 否 | 包含匹配，无法使用索引 |
| `WHERE created_at > NOW() - INTERVAL '1 day'` | ✅ 是 | 范围查询 |
| `ORDER BY created_at` | ✅ 是 | 排序 |

---

## 🆕 第五部分：PostgreSQL 18 新特性

### 1. 异步 I/O (AIO)

PG 18 最重大的架构变革，大幅提升大表扫描性能。

```sql
-- 启用 AIO（默认启用）
-- 无需额外配置，自动生效

-- 适用场景：大表扫描
SELECT * FROM large_table WHERE condition;

-- 性能提升：可提升 30-50% 的 I/O 性能
```

### 2. UUIDv7 原生支持

UUIDv7 更适合时间排序和索引。

```sql
-- 使用 UUIDv7 作为主键
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),  -- PG 18 支持 UUIDv7
    username VARCHAR(50) NOT NULL
);

-- UUIDv7 优势：
-- 1. 时间排序友好（按时间大致递增）
-- 2. 索引性能更好（减少页分裂）
-- 3. 全局唯一性
```

### 3. 虚拟生成列

不占用存储空间的计算列。

```sql
CREATE TABLE products (
    id BIGSERIAL PRIMARY KEY,
    price NUMERIC(10, 2) NOT NULL,
    tax_rate NUMERIC(5, 2) NOT NULL,
    total_price NUMERIC(10, 2) GENERATED ALWAYS AS (price * (1 + tax_rate / 100)) STORED
);

-- 插入时自动计算
INSERT INTO products (price, tax_rate) VALUES (100, 10);
-- total_price 自动为 110.00
```

### 4. 增强的 MERGE 命令

```sql
-- MERGE 用于 upsert 操作
MERGE INTO users u
USING (VALUES ('alice', 'alice@example.com')) AS v(username, email)
ON u.username = v.username
WHEN MATCHED THEN
    UPDATE SET email = v.email
WHEN NOT MATCHED THEN
    INSERT (username, email) VALUES (v.username, v.email);
```

---

## 🔒 第六部分：安全与最佳实践

### 1. 参数化查询（防止 SQL 注入）

```erlang
%% ❌ 错误：直接拼接 SQL（易受 SQL 注入攻击）
Sql = "SELECT * FROM users WHERE id = " ++ integer_to_list(UserId),
imboy_pg:query(Sql, []).

%% ✅ 正确：使用参数化查询
Sql = "SELECT * FROM users WHERE id = $1",
imboy_pg:query(Sql, [UserId]).
```

**Erlang 中的参数占位符：**
| 占位符 | 说明 |
|--------|------|
| `$1`, `$2`, `$3` | 位置参数（推荐） |
| 参数列表 | `imboy_pg:query(Sql, [Arg1, Arg2, Arg3])` |

### 2. 事务 (Transactions)

```sql
-- 开始事务
BEGIN;

-- 执行多个操作
INSERT INTO accounts (user_id, balance) VALUES (1, 100);
INSERT INTO transactions (from_id, to_id, amount) VALUES (1, 2, 50);

-- 提交事务
COMMIT;

-- 或回滚
ROLLBACK;
```

```erlang
%% Erlang 中使用事务
imboy_pg:transaction(fun(Conn) ->
    % 插入账户
    Sql1 = "INSERT INTO accounts (user_id, balance) VALUES ($1, $2)",
    {ok, _, _} = imboy_pg:query(Sql1, [1, 100], Conn),

    % 插入交易记录
    Sql2 = "INSERT INTO transactions (from_id, to_id, amount) VALUES ($1, $2, $3)",
    {ok, _, _} = imboy_pg:query(Sql2, [1, 2, 50], Conn),

    ok
end).
```

### 3. 数据加密

```erlang
%% Imboy 项目中使用加密存储
%% 例如：消息体加密

%% 加密 Payload
PayloadJson = jsone:encode(PayloadMap),
Encrypted = imboy_hasher:encoded_val(PayloadJson),

%% 存储加密数据
Sql = "INSERT INTO messages (from_id, to_id, payload) VALUES ($1, $2, $3)",
imboy_pg:query(Sql, [FromId, ToId, {raw, Encrypted}]).
```

---

## 📊 第七部分：常用查询示例

### 1. Imboy 项目典型查询

```sql
-- 查找用户
SELECT id, username, email
FROM users
WHERE id = $1;

-- 查询好友列表
SELECT u.id, u.username
FROM friends f
JOIN users u ON f.friend_id = u.id
WHERE f.user_id = $1
  AND f.is_deleted = false;

-- 查询离线消息
SELECT id, from_id, to_id, payload, created_at
FROM msg_c2c
WHERE to_id = $1
  AND created_at > $2
ORDER BY created_at ASC
LIMIT 100;

-- 统计未读消息
SELECT COUNT(*)
FROM msg_c2c
WHERE to_id = $1
  AND is_read = false;
```

### 2. JOIN 查询

```sql
-- INNER JOIN：只匹配两表都存在的行
SELECT u.username, m.content
FROM messages m
INNER JOIN users u ON m.from_id = u.id
WHERE m.to_id = 1;

-- LEFT JOIN：左表所有行，右表没有匹配则为 NULL
SELECT u.username, COUNT(m.id) as msg_count
FROM users u
LEFT JOIN messages m ON u.id = m.from_id
GROUP BY u.id, u.username;
```

---

## 🔍 第八部分：调试与监控

### 1. 查看查询计划

```sql
-- 分析查询性能
EXPLAIN SELECT * FROM users WHERE username = 'alice';

-- 详细执行计划（实际执行）
EXPLAIN ANALYZE SELECT * FROM users WHERE username = 'alice';

-- 输出示例：
-- Index Scan using idx_users_username on users  (cost=0.42..8.44 rows=1 width=64) (actual time=0.021..0.022 rows=1 loops=1)
--   Index Cond: (username = 'alice'::text)
-- Planning Time: 0.089 ms
-- Execution Time: 0.043 ms
```

### 2. 常用监控查询

```sql
-- 查看表大小
SELECT
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
WHERE tablename = 'users';

-- 查看索引使用情况
SELECT
    schemaname,
    tablename,
    indexname,
    idx_scan AS index_scans,
    idx_tup_read AS tuples_read,
    idx_tup_fetch AS tuples_fetched
FROM pg_stat_user_indexes
WHERE tablename = 'users';

-- 查看慢查询
SELECT
    query,
    calls,
    total_exec_time / 1000 as total_time_sec,
    mean_exec_time / 1000 as avg_time_sec
FROM pg_stat_statements
ORDER BY mean_exec_time DESC
LIMIT 10;
```

---

## ✅ 最佳实践清单

### 表设计
- [ ] 使用 `BIGSERIAL` 作为主键
- [ ] 时间字段使用 `TIMESTAMPTZ`
- [ ] 添加 `created_at` 和 `updated_at`
- [ ] 为常用查询字段添加索引
- [ ] 使用约束保证数据完整性

### 查询优化
- [ ] 使用参数化查询
- [ ] 避免 `SELECT *`，只查询需要的字段
- [ ] 使用 `EXPLAIN ANALYZE` 分析慢查询
- [ ] 为 `WHERE`、`JOIN`、`ORDER BY` 字段建索引
- [ ] 使用 `LIMIT` 限制结果集大小

### 安全
- [ ] 永远使用参数化查询
- [ ] 敏感数据加密存储
- [ ] 使用事务保证数据一致性
- [ ] 设置合理的用户权限

---

## 📚 学习资源

### 官方文档
- [PostgreSQL 官方文档](https://www.postgresql.org/docs/)
- [PG 18 发布说明](https://www.postgresql.org/about/news/

)
- [Neon - PG 18 新特性详解](https://neon.com/postgresql/postgresql-18-new-features)
- [Bytebase - PG 18 开发者视角](https://www.bytebase.com/blog/what-is-new-in-postgres-18-for-developer/)

### 在线教程
- [PostgreSQL Tutorial](https://www.postgresqltutorial.com/)
- [SQL Fiddle](https://sqlfiddle.com/) - 在线练习 SQL

### Imboy 项目资源
- `./doc/postgresql/` - 项目数据库文档
- `.claude/skills/postgresql-imboy.skill.md` - Imboy PG 实践

---

## 🔍 常见问题

### Q: 什么时候使用 TEXT，什么时候使用 VARCHAR(n)？
**A:**
- `TEXT`：不限长度，性能相同，推荐使用
- `VARCHAR(n)`：需要限制长度时使用（如用户名）

### Q: JSON 和 JSONB 有什么区别？
**A:**
| 类型 | 存储 | 查询 | 性能 |
|------|------|------|------|
| `JSON` | 原始文本 | 慢 | 插入快 |
| `JSONB` | 二进制格式 | 快 | 查询快（推荐） |

### Q: 如何优化慢查询？
**A:**
1. 使用 `EXPLAIN ANALYZE` 查看执行计划
2. 检查是否使用了索引
3. 考虑添加或调整索引
4. 使用 `LIMIT` 限制结果集

---

## 🎯 适用场景

当您需要以下操作时，使用此技能：
- 设计新的数据库表
- 编写 SQL 查询
- 优化数据库性能
- 理解 Imboy 项目的数据库结构
- 在 Erlang 中操作 PostgreSQL
