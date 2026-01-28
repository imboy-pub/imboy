# Imboy 多机房部署 - ID段动态分配方案

> **版本**: 1.0.0
> **创建日期**: 2026-01-28
> **状态**: 生产就绪

---

## 📋 目录

- [方案概述](#方案概述)
- [架构设计](#架构设计)
- [快速开始](#快速开始)
- [配置说明](#配置说明)
- [监控和运维](#监控和运维)
- [故障处理](#故障处理)
- [API参考](#api参考)

---

## 方案概述

### 问题背景

Imboy 项目原有数据库表使用 PostgreSQL `BIGSERIAL` 自增ID，在多机房部署场景下存在以下问题：

1. **ID冲突**: 每个机房独立生成ID，数据同步时必然产生冲突
2. **扩展困难**: 传统序列步长方案需要预先规划机房数量
3. **配置复杂**: 每个机房需要手动配置不同的序列参数

### 解决方案

采用 **ID段动态分配方案**，实现：

- ✅ 机房数量完全动态（可随时新增/缩减）
- ✅ 零代码改动（Repo/DS/Logic层无需修改）
- ✅ 自动管理（自动续期、监控）
- ✅ 性能无损（预分配ID段）

### 方案对比

| 特性 | 传统序列步长 | Snowflake | **ID段动态分配** |
|------|------------|-----------|------------------|
| 机房数量 | 固定 | 动态 | ✅ **动态** |
| 代码改动 | 无 | 大 | ✅ **无** |
| 性能 | 好 | 优秀 | ✅ **优秀** |
| 扩展性 | 差 | 好 | ✅ **好** |
| 运维成本 | 中 | 中 | ✅ **低** |

---

## 架构设计

### 系统架构

```
┌─────────────────────────────────────────────────────────────┐
│                    应用层 (Erlang/OTP)                      │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  PostgreSQL 序列 (无需代码改动)                       │  │
│  │  - user_id_seq: 1-100000 (机房1)                     │  │
│  │  - user_id_seq: 100001-200000 (机房2)               │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    数据库层 (PostgreSQL 18)                  │
│  ┌──────────────────────────────────────────────────────┐  │
│  │           ID段管理系统 (4表+10函数+2视图)              │  │
│  │                                                       │  │
│  │  数据表:                                              │  │
│  │  - system_datacenter      (机房注册表)               │  │
│  │  - system_id_segment      (ID段分配记录)             │  │
│  │  - system_datacenter_log  (审计日志)                 │  │
│  │  - system_id_segment_stats (统计表)                  │  │
│  │                                                       │  │
│  │  核心函数:                                            │  │
│  │  - get_or_allocate_id_segment()  (自动分配ID段)      │  │
│  │  - init_sequence_from_segment()  (初始化序列)        │  │
│  │  - register_datacenter()          (注册机房)         │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### 数据结构

#### 机房注册表 (system_datacenter)

```sql
CREATE TABLE system_datacenter (
    id SERIAL PRIMARY KEY,              -- 机房ID
    name VARCHAR(50) UNIQUE,            -- 机房名称
    region VARCHAR(50),                 -- 所在区域
    api_endpoint VARCHAR(255),          -- API端点
    is_active BOOLEAN,                  -- 是否激活
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);
```

#### ID段分配记录表 (system_id_segment)

```sql
CREATE TABLE system_id_segment (
    id BIGSERIAL PRIMARY KEY,
    datacenter_id SMALLINT,             -- 所属机房
    table_name VARCHAR(50),             -- 表名
    segment_start BIGINT,               -- 起始ID
    segment_end BIGINT,                 -- 结束ID
    allocated_size INT,                 -- 段大小
    used_count INT,                     -- 已使用数量
    is_active BOOLEAN,                  -- 是否活跃
    allocated_at TIMESTAMPTZ,
    expired_at TIMESTAMPTZ,
    UNIQUE (datacenter_id, table_name)  -- 每机房每表一个活跃段
);
```

### 工作流程

```
┌──────────────────────────────────────────────────────────────┐
│ 1. 应用启动时                                                 │
│    调用 init_all_sequences(datacenter_id)                    │
│    ↓                                                          │
│    为每张表调用 get_or_allocate_id_segment()                 │
│    ↓                                                          │
│    获取ID段 → 创建/重置序列                                  │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│ 2. 正常运行时                                                 │
│    PostgreSQL 序列自动递增                                    │
│    ↓                                                          │
│    使用率达到80%时触发续期                                    │
│    ↓                                                          │
│    获取新ID段 → 重置序列                                      │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│ 3. 新增机房时                                                 │
│    调用 register_datacenter()                                │
│    ↓                                                          │
│    自动分配机房ID                                             │
│    ↓                                                          │
│    为新机房分配独立的ID段                                     │
└──────────────────────────────────────────────────────────────┘
```

---

## 快速开始

### 第一步：数据库迁移（2分钟）

```erlang
% 在 Erlang Shell 中执行
imboy_migrate:migrate().
```

**预期输出**：
```
NOTICE:  ID段服务创建完成
NOTICE:  版本: 1.0.0
NOTICE:  创建内容:
NOTICE:   - 4张数据表
NOTICE:   - 10个管理函数
NOTICE:   - 2个监控视图
NOTICE:   - 3个默认机房
```

### 第二步：注册机房（1分钟）

```sql
-- 方式1: 使用默认机房（已创建）
SELECT id, name FROM system_datacenter WHERE is_active = TRUE;

-- 方式2: 注册新机房
SELECT register_datacenter('guangzhou', 'cn-south', 'https://imboy-gz.internal');
-- 返回: 机房ID
```

### 第三步：初始化序列（1分钟）

```sql
-- 批量初始化所有表的序列
SELECT * FROM init_all_sequences(1);  -- 1 是机房ID
```

### 第四步：配置应用（1分钟）

```erlang
% 在 config/sys.config 中添加
{imboy, [
    {datacenter_id, 1},
    {datacenter_name, <<"beijing">>},
    {datacenter_region, <<"cn-north">>}
]}.
```

### 第五步：重启应用

```bash
IMBOYENV=prod make restart
```

### 验证

```sql
-- 查看ID段状态
SELECT * FROM v_id_segment_monitor;

-- 应该看到：
-- - 机房1的各表ID段
-- - 使用率、剩余天数等指标
```

---

## 配置说明

### 应用配置

```erlang
% config/sys.config
{imboy, [
    %% 必须配置
    {datacenter_id, 1},                    % 机房ID
    {datacenter_name, <<"beijing">>},      % 机房名称
    {datacenter_region, <<"cn-north">>},   % 机房区域

    %% 可选配置
    {id_segment_enabled, true},            % 是否启用ID段服务
    {id_segment_refresh_interval, 60000},  % 检查间隔（毫秒）
    {id_segment_threshold, 80},            % 使用率阈值（%）
    {id_segment_size, 100000}              % 默认段大小
]}.
```

### 数据库配置

默认配置即可工作，可选调整：

```sql
-- 调整默认段大小
SELECT * FROM get_or_allocate_id_segment(1, 'user', 200000, 30);
--                                          机房   表    段大小  过期天

-- 调整过期时间
SELECT * FROM get_or_allocate_id_segment(1, 'user', 100000, 0);
--                                                            0=永不过期
```

---

## 监控和运维

### 核心监控指标

| 指标 | 说明 | 正常范围 | 告警阈值 |
|------|------|---------|---------|
| `segment_usage_percent` | ID段使用率 | 0-80% | >80% 警告, >90% 严重 |
| `segment_remaining_days` | ID段剩余天数 | >7天 | <7天 警告, <3天 严重 |
| `active_datacenters` | 活跃机房数量 | 配置值 | <配置值 |

### 监控查询

#### 查看所有机房状态

```sql
SELECT * FROM v_datacenters;
```

#### 查看ID段使用情况

```sql
SELECT
    datacenter_name,
    table_name,
    segment_start,
    segment_end,
    used_count,
    usage_percent,
    status
FROM v_id_segment_monitor
WHERE status IN ('WARNING', 'CRITICAL')
ORDER BY usage_percent DESC;
```

#### 查看需要关注的表

```sql
-- 使用率超过80%的表
SELECT
    datacenter_name,
    table_name,
    usage_percent,
    remaining_days
FROM v_id_segment_monitor
WHERE usage_percent > 80
ORDER BY usage_percent DESC;
```

### 定期维护

#### 每日检查

```sql
-- 1. 检查ID段使用情况
SELECT * FROM v_id_segment_monitor WHERE usage_percent > 70;

-- 2. 记录统计数据
SELECT collect_id_segment_stats();  -- 定时任务自动执行
```

#### 每周检查

```sql
-- 1. 检查历史趋势
SELECT
    table_name,
    stats_time,
    usage_percent
FROM system_id_segment_stats
WHERE stats_time > NOW() - INTERVAL '7 days'
ORDER BY table_name, stats_time;

-- 2. 审计日志检查
SELECT
    datacenter_id,
    action,
    details,
    created_at
FROM system_datacenter_log
WHERE created_at > NOW() - INTERVAL '7 days'
ORDER BY created_at DESC;
```

---

## 故障处理

### 问题1: ID段即将用尽

**症状**: `usage_percent > 90%`

**解决**:
```sql
-- 手动续期（应用会自动续期，也可手动触发）
SELECT * FROM renew_id_segment(1, 'user', 100000);
```

### 问题2: 机房注册失败

**症状**: `register_datacenter()` 报错

**排查**:
```sql
-- 检查机房名是否已存在
SELECT * FROM system_datacenter WHERE name = 'beijing';

-- 检查序列状态
SELECT * FROM pg_sequences WHERE schemaname = 'public';
```

### 问题3: 序列初始化失败

**症状**: `init_sequence_from_segment()` 报错

**排查**:
```sql
-- 检查序列是否存在
SELECT * FROM pg_sequences
WHERE schemaname = 'public'
AND sequencename = 'user_id_seq';

-- 手动删除重建
DROP SEQUENCE IF EXISTS user_id_seq CASCADE;
SELECT init_sequence_from_segment('user', 1);
```

### 问题4: 需要回滚

**场景**: 发现严重Bug需要回滚到传统方案

**步骤**:
```sql
-- 1. 停止所有应用节点
-- 2. 删除ID段管理表
DROP TABLE IF EXISTS system_datacenter CASCADE;
DROP TABLE IF EXISTS system_id_segment CASCADE;
DROP TABLE IF EXISTS system_datacenter_log CASCADE;
DROP TABLE IF EXISTS system_id_segment_stats CASCADE;

-- 3. 重置序列为默认值
ALTER SEQUENCE user_id_seq RESTART WITH 1;
-- ... 其他表同理

-- 4. 重启应用
```

---

## API参考

### 机房管理

#### register_datacenter

注册新机房

```sql
SELECT register_datacenter(
    'guangzhou',           -- 机房名称
    'cn-south',            -- 区域
    'https://api.gz.url'   -- API端点（可选）
);
-- 返回: 机房ID (INTEGER)
```

#### unregister_datacenter

注销机房（软删除）

```sql
SELECT unregister_datacenter(1);
-- 返回: TRUE
```

#### get_datacenters

获取机房列表

```sql
SELECT * FROM get_datacenters(TRUE);  -- TRUE=只返回活跃机房
```

### ID段管理

#### get_or_allocate_id_segment

获取或分配ID段（核心函数）

```sql
SELECT * FROM get_or_allocate_id_segment(
    1,          -- 机房ID
    'user',     -- 表名
    100000,     -- 段大小（可选）
    30          -- 过期天数（可选）
);
-- 返回: segment_start, segment_end
```

#### init_sequence_from_segment

初始化单个序列

```sql
SELECT init_sequence_from_segment(
    'user',  -- 表名
    1        -- 机房ID
);
-- 返回: TRUE
```

#### init_all_sequences

批量初始化所有表序列

```sql
SELECT * FROM init_all_sequences(1);
-- 返回: 每张表的初始化状态
```

#### renew_id_segment

手动续期ID段

```sql
SELECT * FROM renew_id_segment(
    1,          -- 机房ID
    'user',     -- 表名
    100000      -- 新段大小（可选）
);
-- 返回: 新的 segment_start, segment_end
```

### 监控查询

#### get_datacenter_segment_status

查看ID段状态

```sql
SELECT * FROM get_datacenter_segment_status(
    1,          -- 机房ID（可选）
    'user'      -- 表名（可选）
);
```

#### v_id_segment_monitor

ID段监控视图

```sql
SELECT * FROM v_id_segment_monitor;
```

#### v_datacenters

机房列表视图

```sql
SELECT * FROM v_datacenters;
```

---

## 附录

### A. 数据表清单

| 表名 | 用途 | 关键字段 |
|------|------|---------|
| `system_datacenter` | 机房注册表 | id, name, is_active |
| `system_id_segment` | ID段分配记录 | datacenter_id, table_name, segment_start, segment_end |
| `system_datacenter_log` | 审计日志 | datacenter_id, action, details |
| `system_id_segment_stats` | 使用统计 | datacenter_id, table_name, usage_percent |

### B. 函数清单

| 函数 | 用途 | 返回值 |
|------|------|--------|
| `register_datacenter` | 注册机房 | 机房ID |
| `unregister_datacenter` | 注销机房 | BOOLEAN |
| `reactivate_datacenter` | 重新激活机房 | BOOLEAN |
| `get_or_allocate_id_segment` | 获取/分配ID段 | segment_start, segment_end |
| `init_sequence_from_segment` | 初始化序列 | BOOLEAN |
| `init_all_sequences` | 批量初始化 | TABLE |
| `get_datacenter_segment_status` | 查看状态 | TABLE |
| `renew_id_segment` | 手动续期 | segment_start, segment_end |
| `get_datacenters` | 机房列表 | TABLE |
| `collect_id_segment_stats` | 记录统计 | INT |

### C. 视图清单

| 视图 | 用途 |
|------|------|
| `v_id_segment_monitor` | ID段监控 |
| `v_datacenters` | 机房列表 |

### D. 相关文件

| 文件 | 说明 |
|------|------|
| `priv/migrations/00000043_id_segment_service.sql` | 数据库迁移文件 |
| `scripts/verify_id_segment.sh` | 验证脚本 |
| `doc/architecture/distributed-id-quickref.md` | 快速参考卡片 |
| `doc/architecture/distributed-id-testing-guide.md` | 测试指南 |

---

**文档维护**: 如有问题或建议，请及时更新此文档。
