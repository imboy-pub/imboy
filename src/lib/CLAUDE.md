# Lib 层文档 - 基础库函数

[根目录](../CLAUDE.md) > **src/lib**

> **最后更新**: 2026-01-20 08:48:18 CST
> **模块数量**: 29 个
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

### ID 编码

| Lib | 说明 |
|-----|------|
| `elib_hashids.erl` | HashID 编码/解码 |

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

### 其他

| Lib | 说明 |
|-----|------|
| `qianfan_api.erl` | 千帆 AI API |

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
├── elib_hashids_tests.erl
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

使用 `elib_hashids:encode/1` 和 `elib_hashids:decode/1`。

---

## 相关文件清单

### Lib 文件 (29 个)

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
├── elib_hashids.erl
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
├── imboy_sms.erl
├── elib_str.erl
├── imboy_syn.erl
├── elib_type.erl
├── elib_uri.erl
└── qianfan_api.erl
```

---

## 变更记录 (Changelog)

### 2026-01-20
- 新增 `elib_async.erl` 异步执行库
- 新增 `elib_retry.erl` 重试机制库
- 完善 Lib 层文档

---

**文档维护**: 请在添加新的基础库时同步更新此文档。
