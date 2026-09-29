# TSID 字段约定

> Last Updated: 2026-09-29（TSID correctness hardening 后同步）  
> Status: 长期协议契约文档  
> Scope: 所有 REST API 与 WebSocket API 中 TSID 字段的传输格式约定

---

## 1. 什么是 TSID

TSID（Time-Sorted ID）是 Imboy 使用的分布式 ID 生成方案，取代原先的 BIGSERIAL 自增 ID。

**位布局**（64 位，无符号，但存储为有符号 BIGINT）：

```
[sign:1=0][timestamp_ms:42][node_id:10][sequence:11]
```

| 字段 | 位数 | 说明 |
|------|------|------|
| 符号位 | 1 | 始终为 0（正整数） |
| 时间戳 | 42 | 相对于纪元（2025-01-01 00:00:00 UTC）的毫秒数 |
| 节点 ID | 10 | 按 `dc_bits`（1..10，缺省 3；应用层 API 允许 0=纯 node 模式，部署链 preflight 限 1..10）动态切分为 dc_id + node_id |
| 序列号 | 11 | 每毫秒每节点最多 2048 个 ID（= 节点持续容量 2048 ids/ms） |

**特性**（2026-09 TSID correctness hardening 后）：

- **纪元**：2025-01-01 00:00:00 UTC；时间跨度 139.5 年（2025 → 2164）
- **全局唯一语义**：同一节点上所有命名生成器（label，含 default）共享同一全局
  cursor——任意两个生成器产生的 ID 数值永不相同；label 仅是治理/兼容标签，
  跨表/跨域引用可直接按数值关联
- **时间戳精度**：毫秒；高吞吐下 ID 内 ts 允许有界领先墙钟
  （`max_logical_lead_ms` 缺省 512ms，borrow 未来毫秒，崩溃恢复由 durable
  fence 保证不重不越）——因此 ID 提取的时间戳是生成序的近似，排序请用 ID 本身
- **生成吞吐量**：持续容量 2048 ids/ms/节点（≈205 万 ID/s）；短突发实测
  10M+ ID/s（基准证据见 hardening status 附录）；多节点水平扩展
- **有界逻辑时间**：突发超过容量时 fail-closed（typed `capacity_exhausted`），
  绝不无限借未来
- **数据库类型**：PostgreSQL `BIGINT`（signed 63-bit 上界有校验）
- **错误族**：全部 typed error（非静默回退）——`elib_tsid_not_initialized` /
  `elib_tsid_capacity_exhausted`（clock_wait | cas_contention）/
  `elib_tsid_fenced` / `elib_tsid_invalid_config` / `elib_tsid_invalid_input` /
  `elib_tsid_clock_before_epoch` / `elib_tsid_timestamp_exhausted` /
  `elib_tsid_invalid_input`（越界输入）等
- **部署与 cutover**：多实例唯一性依赖 `IMBOY_TSID_DC_ID`/`IMBOY_TSID_NODE_ID`
  配置 + preflight 位布局校验 + 双槽 durable 状态 + lifetime lock；
  存量 BIGSERIAL→TSID 迁移与回滚见
  `docs/architecture/tsid-cutover-runbook.md` 与
  `docs/architecture/tsid-rollback-decision-table.md`

---

## 2. API 传输格式

**后端以 JSON integer（number）返回 TSID 字段**。

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "id": 84442613760000001,
    "from_id": 84442613760000002,
    "to_id": 84442613760000003
  }
}
```

### 客户端精度处理

JavaScript `Number` 最大安全整数为 `2^53 - 1 = 9,007,199,254,740,991`（约 9×10^15），  
而当前 TSID 值（从 2025-01-01 起约 400+ 天后）已超过此上限（约 8×10^16）。

**前端（imboy-admin-frontend）解决方案**：`axios` 客户端通过 `transformResponse` 中的  
`safeParseBigIntJson` 函数，在 JSON 解析阶段将 16 位及以上整数自动转为字符串：

```typescript
// client.ts — safeParseBigIntJson
const safeText = trimmed.replace(
  /(?<=[:,[\s])(-?\d{16,})(?=[,\]}\s])/g,
  '"$1"'
)
return JSON.parse(safeText)
```

因此，**前端 TypeScript 中所有 TSID 字段类型为 `EntityId = string`**。

**Flutter（imboyapp）解决方案**：Dart `int` 为 64 位，可直接承载 TSID，无精度丢失。  
从 API 响应解析时使用 `int.parse(json['id'].toString())` 兼容 int/string 两种格式。

---

## 3. 哪些字段是 TSID

以下字段在 API 响应中为 TSID（JSON integer，前端接收为 string）：

| 字段名 | 含义 | 所在接口 |
|--------|------|---------|
| `id` | 资源主键 | 所有资源接口 |
| `uid` | 用户 ID | 用户、消息、群组相关接口 |
| `from_id` | 消息发送方 ID | 消息接口 |
| `to_id` | 消息接收方 ID | 消息接口 |
| `peer_id` | 会话对端 ID | 会话接口 |
| `group_id` | 群组 ID | 群组、消息接口 |
| `user_id` | 用户 ID（别名） | 群成员、设备接口 |
| `owner_uid` | 群主 ID | 群组接口 |
| `creator_uid` | 创建者 ID | 群组接口 |
| `mentioned_uid` | 被 @ 用户 ID | 提及接口 |
| `from_uid` | 消息发送方 ID（别名） | 提及接口 |
| `key_id` | 合规密钥 ID | 合规密钥接口 |
| `conversation_id` | 会话 ID | 会话接口 |
| `last_msg_id` | 最后消息 ID | 会话接口 |
| `channel_id` | 频道 ID | 频道接口 |
| `author_id` | 作者 ID | 频道消息接口 |

**不是 TSID 的 ID 字段**：

| 字段名 | 含义 | 类型 |
|--------|------|------|
| `device_id` | 设备标识符 | string（UUID 或自定义） |
| `did` | 设备 ID（别名） | string |
| `role_id` | 角色 ID | number（小整数） |

---

## 4. 前端规范

### TypeScript（imboy-admin-frontend）

```typescript
// src/types/common.ts
export type EntityId = string  // TSID 经 safeParseBigIntJson 转换后的 string

// 使用示例
interface User {
  id: EntityId       // ✅ 使用 EntityId，不直接写 string
  account: string    // ✅ 非 TSID 字段直接写 string
}
```

**规则**：
- 所有 TSID 字段必须使用 `EntityId` 类型，不直接写 `string`
- 不要用 `Number(id)` 或 `parseInt(id)` 处理 TSID（会丢失精度）
- 在 UI 展示时直接渲染字符串，不做数值运算

### Dart/Flutter（imboyapp）

```dart
// TSID 解析：兼容 int 和 string 两种 JSON 格式
int parseId(dynamic value) {
  if (value is int) return value;
  return int.parse(value.toString());
}

// 存储：SQLite INTEGER 列，Dart int 类型
final int id;
final int fromId;
final int toId;
```

---

## 5. 后端规范（Erlang）

```erlang
%% 生成 TSID（Repo 层 insert 时）
Id = elib_tsid:generate(table_name).

%% 客户端输入：binary → integer
UidInt = ec_cnv:to_integer(UidBin).

%% 客户端输出：直接返回 integer（JSON number）
%% 不做 integer_to_binary 转换
Payload = #{<<"id">> => Id, <<"from_id">> => FromId}.
```

---

## 6. 相关文档

- `docs/reference/rest-api.md` — REST API 通用约定
- `docs/reference/websocket-api-2.md` — WebSocket 消息协议
- `src/lib/elib_tsid.erl` — TSID 生成器实现
- `imboy-admin-frontend/src/services/api/client.ts` — safeParseBigIntJson 实现
- `imboy-admin-frontend/src/types/common.ts` — EntityId 类型定义
