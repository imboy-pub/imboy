# Lib 层 - 基础库函数

[根目录](../../CLAUDE.md) > **src/lib** | 116 个模块（截至 2026-09-19，以 `find src/lib -name '*.erl'\|wc -l` 为准） | 职责：数据库、缓存、加密、ID、异步、重试等通用功能

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

### 附件上传

| 模块 | 说明 |
|------|------|
| `attachment_upload_spool` | 私有、已取消目录链接的有界上传快照；同一描述符用于哈希与写入 |
| `attachment_upload_body` | 分块 HTTP 收体与大小限制；调用方负责关闭成功返回的快照 |

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
| `group_member_transfer` | 群成员列表 ID 字段转换工具（从 src/api/ 迁入，保留原名） |
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
| 每节点每毫秒每 Namespace | 2048 个 ID（硬上限，无借支） |
| 默认 dc_bits | 3（8 DC × 128 节点） |
| Namespace | 152 canonical + 3 别名（`namespaces/0` 为准） |

**关键 API**（V10：多 Namespace 独立游标、无持久化）：

```erlang
%% imboy_sup 启动时由 elib_tsid:init/1 发布 runtime（非持久化，纯内存）
Id  = elib_tsid:generate(user).        % 生产：显式 namespace（必填）
Ids = elib_tsid:generate_n(user, 100). % 批量（生产零调用，保留）
#{timestamp := Ts} = elib_tsid:parse(Id).
elib_tsid:namespaces().                % canonical 清单（顺序即游标槽位）
elib_tsid:aliases().                   % 别名 → canonical（group/audit 等）
elib_tsid:readiness().                 % ready | clock_unsafe | not_initialized
```

> 仅保留显式实体入口 `generate/1`、`generate_n/2`；无 default 槽位、
> `generate/0`、`generate_n/1` 或 register/registered 兼容接口。未知名一律 typed error。

**约束**：
- 同 Namespace 内、单次 BEAM 生命周期内严格唯一（含任意时钟回拨/前跳）
- **跨 Namespace 数值允许重复**：跨域引用必须 `Domain + ID`，禁止裸数值等值比较/全局去重
- **无持久化**：跨重启要求新时间戳严格大于旧发号水位；同毫秒重启、NTP 回拨/快照恢复/同 NodeId 双实例均可能重叠，不能仅凭“没有回拨”保证唯一
- **禁止借未来毫秒**：Sequence 耗尽时有界等待真实时间推进（`capacity_wait_timeout_ms`，默认 100ms），超时 typed error
- 固定 Offset 时钟（monotonic + init 采集 Offset）：与系统钟偏差 > `max_clock_drift_ms` 时 `readiness/0` 翻闸阻断发号；无内置周期探针，暴露窗口 = 外部就绪探测轮询间隔
- `(dc_id, node_id)` 全集群唯一是**部署层责任**（V10 无互斥锁，代码层不可证）
- 严格排序业务必须用 `conv_seq` / `room_seq` 等专用字段
- `dc_bits` 全集群必须统一
- ID 直接以 integer 返回客户端，不做字符串转换（客户端以 decimal string 承载 64-bit 精度）

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
