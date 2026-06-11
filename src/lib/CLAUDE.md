# Lib 层 - 基础库函数

[根目录](../CLAUDE.md) > **src/lib** | 61 个模块（截至 2026-06，以 `find src/lib -name '*.erl'\|wc -l` 为准） | 职责：数据库、缓存、加密、ID、异步、重试等通用功能

---

## 模块清单

### 数据库

| 模块 | 说明 |
|------|------|
| `elib_pg` | PostgreSQL 连接与操作（核心，所有 DB 操作必须经此） |
| `elib_pg_sql` | SQL 构建工具 |

### 缓存

| 模块 | 说明 |
|------|------|
| `imboy_cache` | 缓存封装（depcache） |
| `imboy_cache_sync` | 跨节点缓存同步 |

### 加密

| 模块 | 说明 |
|------|------|
| `elib_cipher` | 加密与解密 |
| `elib_hasher` | 哈希计算 |
| `elib_password` | 密码哈希 |
| `shamir_secret_sharing` | Shamir 密钥分割 |

### ID

| 模块 | 说明 |
|------|------|
| `elib_tsid` | TSID 时间有序分布式唯一 ID（已全量替代 elib_hashids） |
| `elib_cnv` | 类型转换（`id_to_binary/1` 已废弃，ID 直接以 integer 返回） |

### 异步 & 重试

| 模块 | 说明 |
|------|------|
| `elib_async` | 异步任务执行 |
| `elib_retry` | 同步重试逻辑 |
| `elib_retry_config` | 重试配置 |

### 分布式

| 模块 | 说明 |
|------|------|
| `imboy_syn` | 分布式进程注册 |
| `imboy_cluster` | 集群管理 |

### HTTP

| 模块 | 说明 |
|------|------|
| `elib_req` | 请求解析 |
| `elib_response` | 响应格式化 |

### 工具函数

| 模块 | 说明 |
|------|------|
| `elib_str` | 字符串处理 |
| `elib_uri` | URL 解析 |
| `elib_dt` | 日期时间 |
| `elib_type` | 类型检查 |
| `elib_email` | SMTP 邮件发送（gen_smtp）+ 邮箱格式校验 |
| `elib_id` | ID 处理工具 |
| `elib_metric` | 指标统计 |
| `elib_oss` | 对象存储 |
| `elib_param` | 参数处理 |

### 配置 & 功能管理

| 模块 | 说明 |
|------|------|
| `imboy_env` | 环境配置（运行时 IMBOY_* 环境变量覆盖，优先级最高） |
| `imboy_feature` | 功能开关 |
| `imboy_policy` | 策略管理 |
| `imboy_profile_preset` | 配置预设 |
| `imboy_plugin_registry` | 插件注册表 |

### 编解码 & 协议

| 模块 | 说明 |
|------|------|
| `imboy_codec` | 消息编解码 |
| `imboy_frame` | 帧处理 |

### 其他

| 模块 | 说明 |
|------|------|
| `elib_log` | 日志记录 |
| `imboy_error` | 错误处理 |
| `imboy_sms` | 短信发送 |
| `imboy_message_helper` | 消息辅助 |
| `imboy_migrate` | 数据库迁移（fail-fast 模式） |
| `imboy_dtl` | 模板引擎 |
| `epgsql_codec_rfc3339_bin` | RFC3339 编解码 |
| `eunit_runner` | EUnit 运行器 |
| `qianfan_api` | 千帆 AI API |

---

## elib_tsid — TSID 分布式 ID

**位布局**：`[sign:1][timestamp:42][node:10][sequence:11]` — 适配 PostgreSQL BIGINT

| 指标 | 值 |
|------|-----|
| 时间跨度 | 139.5 年（纪元 2025-01-01） |
| 节点数 | 1024（DC×Node 任意分配） |
| 每节点每毫秒 | 2048 个 ID |
| 默认 dc_bits | 3（8 DC × 128 节点） |

**关键 API**：

```erlang
elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}).  % 启动时调用一次
Id  = elib_tsid:generate().           % 生成单个
Ids = elib_tsid:generate_n(100).      % 批量生成
#{timestamp := Ts} = elib_tsid:parse(Id).
Enc = elib_tsid:to_base62(Id).        % URL/日志场景
```

**约束**：
- 跨节点唯一，不保证跨节点严格单调顺序
- 严格排序业务必须用 `conv_seq` / `room_seq` 等专用字段
- `dc_bits` 全集群必须统一
- ID 直接以 integer 返回客户端，不做字符串转换

---

## 关键 API 速查

```erlang
%% 数据库
{ok, _, Rows} = elib_pg:query(Sql, Params).

%% 缓存
{ok, V} = imboy_cache:get(Key).
ok = imboy_cache:set(Key, Value, TTL).   % TTL 单位：秒
ok = imboy_cache:delete(Key).

%% 异步
Pid = elib_async:async(Fun).
Pid = elib_async:async_retry(Fun, RetryCount, DelayMs).

%% 重试
{ok, R} = elib_retry:with_retry(Fun, RetryCount, DelayMs).
```

---

## 迁移状态

- **elib_hashids → elib_tsid**（2026-04-07 完成）：60 repo + 95 erl 清理，`elib_hashids.erl` 已删除
- **elib_cnv:id_to_binary/1** 已废弃，41 个文件已清理

---

**文档维护**: 添加新 Lib 模块时同步更新此文档。
