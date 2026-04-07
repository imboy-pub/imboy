# Lib 层文档 - 基础库函数

[根目录](../CLAUDE.md) > **src/lib**

> **最后更新**: 2026-02-01 04:35:00 CST
> **模块数量**: 31 个
> **职责**: 提供基础工具函数，封装数据库连接、缓存、加密等通用功能

---

## 模块职责

Lib 层是 Imboy 系统的基础库层，负责：
- 数据库连接与操作
- 缓存管理
- 加密与解密
- ID 编码与解码
- 异步执行与重试
- 日志记录
- 类型转换
- 字符串处理
- URL 解析

---

## 入口与启动

Lib 模块由各层调用：

```erlang
% 使用数据库连接
{ok, _, [{Res}]} = elib_pg:query(Sql, Params).

% 使用缓存
{ok, Value} = imboy_cache:get(Key).

% 使用异步执行
Pid = elib_async:async(fun() -> ok end).
```

---

## 对外接口

### 数据库相关

| Lib | 说明 |
|-----|------|
| `elib_pg.erl` | PostgreSQL 连接与操作（核心） |
| `elib_pg_sql.erl` | SQL 构建工具 |

### 缓存相关

| Lib | 说明 |
|-----|------|
| `imboy_cache.erl` | 缓存封装 |
| `imboy_cache_sync.erl` | 跨节点缓存同步 |

### 加密相关

| Lib | 说明 |
|-----|------|
| `elib_cipher.erl` | 加密与解密 |
| `elib_hasher.erl` | 哈希计算 |
| `elib_password.erl` | 密码哈希 |
| `shamir_secret_sharing.erl` | Shamir 密钥分割算法 |

### ID 编码

| Lib | 说明 |
|-----|------|
| `elib_tsid.erl` | TSID 时间有序分布式唯一 ID 生成器（已替代 hashids + bigserial） |
| `elib_cnv.erl` | 类型转换工具（`id_to_binary/1` 已不再使用，ID 直接以 integer 返回） |

### 异步与重试

| Lib | 说明 |
|-----|------|
| `elib_async.erl` | 异步任务执行 |
| `elib_retry.erl` | 同步重试逻辑 |
| `elib_retry_config.erl` | 重试配置 |

### 分布式相关

| Lib | 说明 |
|-----|------|
| `imboy_syn.erl` | 分布式进程注册 |
| `imboy_cluster.erl` | 集群管理 |

### 工具函数

| Lib | 说明 |
|-----|------|
| `elib_str.erl` | 字符串处理 |
| `elib_uri.erl` | URL 解析 |
| `imboy_kv.erl` | 键值对处理 |
| `elib_dt.erl` | 日期时间 |
| `elib_cnv.erl` | 类型转换 |
| `elib_type.erl` | 类型检查 |
| `imboy_func.erl` | 函数工具 |

### HTTP 相关

| Lib | 说明 |
|-----|------|
| `elib_req.erl` | 请求解析 |
| `elib_response.erl` | 响应格式化 |

### 其他工具

| Lib | 说明 |
|-----|------|
| `elib_log.erl` | 日志记录 |
| `imboy_error.erl` | 错误处理 |
| `elib_param.erl` | 参数处理 |
| `imboy_sms.erl` | 短信发送 |
| `imboy_message_helper.erl` | 消息辅助 |
| `imboy_migrate.erl` | 数据库迁移 |
| `imboy_dtl.erl` | 模板引擎 |

### 测试工具

| Lib | 说明 |
|-----|------|
| `eunit_runner.erl` | EUnit 运行器 |
| `epgsql_codec_rfc3339_bin.erl` | RFC3339 编解码 |
| `elib_tsid_tests.erl` | TSID 生成器测试 (15 个测试) |

### 其他

| Lib | 说明 |
|-----|------|
| `qianfan_api.erl` | 千帆 AI API |

---

## elib_tsid — TSID 时间有序分布式唯一 ID 生成器

### 位布局 (64-bit 有符号整数，适配 PostgreSQL BIGINT)

```
[0] [timestamp: 42 bits] [node: 10 bits] [sequence: 11 bits]
sign  毫秒时间戳           DC+节点标识      毫秒内序列号
```

### 容量

| 指标 | 值 |
|------|-----|
| 时间跨度 | 2^42 ms ≈ 139.5 年 (纪元 2025-01-01 → 2164 年) |
| 节点总数 | 2^10 = 1024 (DC×Node 任意分配) |
| 每节点每毫秒 | 2^11 = 2048 个 ID |
| 每节点每秒 | 2,048,000 个 ID |
| 数字位数 | 永远 ≤ 19 位 (BIGINT 最大值 9.2 × 10^18) |

### DC/Node 分配 (dc_bits 参数控制)

| dc_bits | DC 数量 | 每 DC 节点数 | 推荐场景 |
|---------|---------|-------------|---------|
| 0 | 1 | 1024 | 单机房 |
| 2 | 4 | 256 | 多区域 |
| **3 (默认)** | **8** | **128** | **推荐：多 DC + 充足节点** |
| 4 | 16 | 64 | DC 多节点少 |
| 5 | 32 | 32 | 极多 DC |

### 唯一性保证

1. **不同 NodeId** → 跨节点唯一 (10-bit node 嵌入 ID)
2. **同节点同毫秒** → Sequence 单调递增 (CAS 原子操作)
3. **时钟回拨** → `EffTs = max(NowRel, OldTs)` 沿用上次时间戳 + 递增序列，绝不倒退
4. **序列溢出** → 借用下一毫秒时间戳 `{OldTs + 1, 0}`，绝不阻塞
5. **并发安全** → `atomics:compare_exchange` lock-free CAS 循环

### 排序语义与限制

- `elib_tsid` 保证 **跨节点唯一**，不保证 **跨数据中心 / 跨节点严格单调生成顺序**
- 当前位布局是 `[timestamp][node][sequence]`，同一毫秒内 ID 排序实际是 `(timestamp, node_id, local_sequence)`，不是全集群真实生成先后顺序
- `generate_n/1` 和单节点内连续 `generate/0` 可视为本地单调递增；跨节点并发时只能认为是“近似时间有序”
- 不能把 TSID 直接当作 IM 消息的全局排序键、全局游标或跨节点严格顺序号
- 如果业务要求严格顺序，必须使用单独的顺序字段或中心化线性化机制，例如：
  - 单会话 / 单群严格顺序：`conv_seq` / `room_seq`
  - 全局严格顺序：数据库 sequence、Redis `INCR`、独立 sequencer

### 使用

```erlang
%% 初始化 (应用启动时调用一次)
elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}).

%% 生成 ID
Id = elib_tsid:generate().           %% 单个
Ids = elib_tsid:generate_n(100).     %% 批量

%% 解析
#{timestamp := Ts, dc_id := Dc, node_id := Node, sequence := Seq} = elib_tsid:parse(Id).

%% 提取
Ts = elib_tsid:timestamp(Id).        %% Unix 毫秒时间戳
NodeId = elib_tsid:node_id(Id).      %% 10-bit 节点标识

%% Base62 编码 (URL/日志场景，最长 11 字符)
Encoded = elib_tsid:to_base62(Id).   %% <<"1a2B3c4D5">>
Id = elib_tsid:from_base62(Encoded).
```

### 迁移状态 (elib_hashids → elib_tsid) ✅ 已完成

迁移已于 2026-04-07 完成，共涉及：
- **60 个 repo 文件**：所有 insert 改用 `elib_tsid:generate(table_name)` 预生成 ID
- **95 个 .erl 文件**：清理所有 `elib_hashids` encode/decode 调用
- **41 个文件**：移除所有 `elib_cnv:id_to_binary/1` 调用，ID 直接以 integer 返回
- **DDL 迁移**：`priv/migrations/00000080_tsid_migration.sql`
- **elib_hashids.erl**：已删除，`hashids_erlang` 依赖已移除

> 例外：消息投递队列、消息时间线、历史同步游标依赖严格顺序语义，保留 `conv_seq` 等专用顺序字段。

### 关键约束

- `dc_bits` 全集群必须统一，否则 `parse/1` 会错误拆分 DC/Node
- `persistent_term` 键: `elib_tsid_state`, `elib_tsid_node_id`, `elib_tsid_dc_bits`
- 纪元: 2025-01-01 00:00:00 UTC (`?EPOCH_MS = 1735689600000`)
- 适用场景：业务主键、幂等键、外部引用 ID、近似按时间排序
- 不适用场景：跨 DC / 跨节点严格排序、消息时间线权威顺序、全局单调游标

---

## 关键依赖与配置

### 数据库配置

```erlang
{pg_conf, #{name => pgsql,
    max_count => 80,
    init_count => 5,
    start_mfa => {epgsql, connect, [...]}}}
```

### 缓存配置

```erlang
{depcache, [
    {depcache_memory_max, 100}  % MB
]}
```

---

## 数据模型

### 数据库操作

```erlang
% 使用 elib_pg 查询
{ok, _, [{Res}]} = elib_pg:query(
    <<"SELECT * FROM user WHERE uid = $1">>,
    [Uid]
).

% 使用 elib_pg_sql 构建 SQL
{Sql, Params} = elib_pg_sql:select(
    <<"user">>,
    [<<"uid">>, <<"nickname">>],
    [{<<"status">>, 1}],
    #{limit => 20}
).
```

### 缓存操作

```erlang
% 获取缓存
{ok, Value} = imboy_cache:get(Key).

% 设置缓存
ok = imboy_cache:set(Key, Value, 3600).

% 删除缓存
ok = imboy_cache:delete(Key).
```

### 异步执行

```erlang
% 简单异步
Pid = elib_async:async(fun() -> ok end).

% 异步带重试
Pid = elib_async:async_retry(Fun, 3, 1000).

% 同步重试
{ok, Result} = elib_retry:with_retry(Fun, 3, 1000).
```

---

## 测试与质量

### 测试文件位置

```
test/lib/
├── epgsql_codec_rfc3339_bin_tests.erl
├── imboy_cache_sync_tests.erl
├── imboy_cache_sync_tests_simple.erl
├── imboy_cache_tests.erl
├── elib_cipher_tests.erl
├── elib_hasher_tests.erl
├── imboy_kv_tests.erl
├── elib_log_tests_simple.erl
├── elib_password_tests.erl
├── elib_pg_tests.erl
├── imboy_sms_tests.erl
├── imboy_sms_tests_simple.erl
├── elib_str_tests.erl
├── imboy_syn_tests_simple.erl
├── elib_type_tests.erl
└── elib_uri_tests.erl
```

---

## 常见问题 (FAQ)

### Q: 如何使用数据库连接?

必须使用 `elib_pg:query/2` 进行所有数据库操作。

### Q: 如何实现异步任务?

使用 `elib_async:async/1,2,4,6` 或 `elib_async:async_retry/1,2,3`。

### Q: 如何实现重试?

使用 `elib_retry:with_retry/1,2,3,4`。

### Q: 如何编码/解码 ID?

已完成 TSID 迁移。ID 生成：`elib_tsid:generate(table_name)`。客户端输入转整数：`ec_cnv:to_integer(Bin)`。客户端输出：直接返回 integer（不做字符串转换）。`elib_hashids` 已删除。

---

## 相关文件清单

### Lib 文件 (30 个)

```
src/lib/
├── epgsql_codec_rfc3339_bin.erl
├── eunit_runner.erl
├── elib_async.erl
├── imboy_cache.erl
├── imboy_cache_sync.erl
├── elib_cipher.erl
├── imboy_cluster.erl
├── elib_cnv.erl
├── elib_dt.erl
├── imboy_dtl.erl
├── imboy_error.erl
├── imboy_func.erl
├── elib_hasher.erl
├── imboy_kv.erl
├── elib_log.erl
├── imboy_message_helper.erl
├── imboy_migrate.erl
├── elib_param.erl
├── elib_password.erl
├── elib_pg.erl
├── elib_pg_sql.erl
├── elib_req.erl
├── elib_response.erl
├── elib_retry.erl
├── elib_retry_config.erl
├── shamir_secret_sharing.erl
├── imboy_sms.erl
├── elib_str.erl
├── imboy_syn.erl
├── elib_tsid.erl
├── elib_type.erl
├── elib_uri.erl
└── qianfan_api.erl
```

---

## 变更记录 (Changelog)

### 2026-04-07
- **TSID 全量迁移完成**：60 repo + 95 清理 + 41 id_to_binary 移除
- ID 直接以 integer 返回客户端，不做字符串转换
- `elib_cnv:id_to_binary/1` 不再使用
- `elib_hashids.erl` 已删除，`hashids_erlang` 依赖已移除
- 79 个测试文件中的 elib_hashids mock 已清理
- 性能基准: 805 万 ids/sec (顺序), 471 万 ids/sec (并发10进程)

### 2026-04-04
- 新增 `elib_tsid.erl` TSID 时间有序分布式唯一 ID 生成器
- 位布局: [sign:1][timestamp:42][node:10][sequence:11]，适配 PostgreSQL BIGINT
- 更新模块数量：30 → 31

### 2026-02-01
- 新增 `shamir_secret_sharing.erl` Shamir 密钥分割算法
- 更新模块数量：29 → 30

### 2026-01-20
- 新增 `elib_async.erl` 异步执行库
- 新增 `elib_retry.erl` 重试机制库
- 完善 Lib 层文档

---

**文档维护**: 请在添加新的基础库时同步更新此文档。
