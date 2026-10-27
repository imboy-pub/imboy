# TSID 字段约定

> Last Updated: 2026-10-19（TSID V10 与业务消息身份边界同步）
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
| 序列号 | 11 | 每毫秒每节点每 Namespace 最多 2048 个 ID |

**特性**（2026-10 V10 后）：

- **纪元**：2025-01-01 00:00:00 UTC；时间跨度 139.5 年（2025 → 2164）
- **Namespace 唯一语义（V10）**：一个业务实体一个独立 cursor，同一 namespace
  内严格唯一；**跨 namespace 数值允许重复**。旧「任意两 label 数值永不相同，
  跨表按数值关联」的合同已被 V10 显式废除——跨业务引用必须 Domain + ID。
- **时间戳精度**：毫秒；V10 禁止借用未来毫秒（无借支铁律），序列耗尽时等待
  生成时钟推进（默认上限 100ms，超时 typed 报错）。生成时钟为 monotonic +
  启动时固定 Offset，可能与当前系统墙钟存在偏差；同域排序可用 ID，跨域不保证全局顺序
- **跨重启边界**：无持久化状态。单次 BEAM 生命周期内严格唯一；跨重启仅当
  新生成时间戳严格大于旧发号水位时不重复。同毫秒重启、跨重启回拨或快照恢复
  均可能与历史 ID 重叠，禁止表述为"绝不重复"
- **理论容量**：每 Namespace 每节点 2048 ids/ms（≈205 万 ID/s）；
  实际吞吐受调度、竞争和有界等待影响，不能将理论容量作为生产承诺
- **无借支**：序列耗尽不借未来毫秒，有界等待真实时间推进（typed
  `capacity_exhausted`，clock_wait | cas_contention 两相）
- **时钟安全**：固定 Offset 时钟与系统钟偏差超限（`tsid_max_clock_drift_ms`）
  时阻断发号（typed `clock_unsafe`）
- **数据库类型**：PostgreSQL `BIGINT`（signed 63-bit 上界有校验）
- **错误族**：全部 typed error（非静默回退）——`elib_tsid_not_initialized` /
  `elib_tsid_generator_not_registered` / `elib_tsid_capacity_exhausted` /
  `elib_tsid_clock_before_epoch` / `elib_tsid_clock_unsafe` /
  `elib_tsid_timestamp_exhausted` / `elib_tsid_already_initialized` /
  `elib_tsid_invalid_config` / `elib_tsid_invalid_input`
- **部署**：多实例唯一性依赖 `(IMBOY_TSID_DC_ID, IMBOY_TSID_NODE_ID)` 全集群
  唯一 + preflight 位布局校验；V10 无持久化状态目录、无跨实例互斥锁

> **⚠️ TSID 内嵌时间戳 ≠ 业务时间字段**（2026-10-26 R22-B 登记）：TSID 高 42 位
> 的时间成分仅用于发号排序与本地化生成，**不是**可消费的业务时间来源——禁止从
> ID 反解时间用于展示或比较。业务时间字段的 wire 单位/语义（REST 毫秒 int、
> EB 域秒、内部管线 RFC3339 微秒、conv_seq 序数等）见
> [时间字段合同](./time-field-contract.md)。

---

## 2. API 传输格式

**常规业务API保持JSON integer（number）返回TSID字段**。部分Admin/专用接口显式返回十进制字符串，例如管理端群资料归一化，以端点合同为准。客户端EntityId=string是解析后的类型，不等于所有服务器响应均为JSON string；V10不统一改写既有API。

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

以下是常见实体TSID字段；常规响应JSON integer，Admin解析后string，显式string端点按自身合同。本地行号和业务字符串键除外：

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
| `channel_id` | 频道 ID | 频道接口 |
| `author_id` | 作者 ID | 频道消息接口 |

**不是 TSID 的 ID 字段**：

| 字段名 | 含义 | 类型 |
|--------|------|------|
| `device_id` | 设备标识符 | string（UUID 或自定义） |
| `did` | 设备 ID（别名） | string |
| `msg_id` | 普通 IM 业务消息标识 | opaque string（客户端 XID；新服务端普通 IM 使用 UUIDv7；历史/传入值原样保留，varchar(40)） |
| `last_msg_id` | 最后消息标识 | opaque string（原样取自 msg_c2c/msg_c2g.msg_id；不按 TSID 数字解析，见迁移 00000198） |
| `role_id` | 角色 ID | number（小整数） |

普通 IM 的 `msg_id` 是跨消息类型的业务身份，不是消息表行主键；新的服务端消息
使用现有 `elib_uuid:gen_v7/0` 生成36字符UUIDv7。客户端XID、历史uid/base64或
TSID十进制字符串等既有身份不重编号；重试、流式定稿、ACK、引用和E2EE绑定
必须继续使用首次确定的原值。处理或完成更新仍用 `Type + MsgId` 精确定位，
防御历史或异常同值误伤，不把不同逻辑消息同ID视为合法并存，也不放松归档唯一约束。
客服canonical `message_id` 是独立实体主键，仍使用该实体的TSID；不能仅凭字段名
将它机械替换成普通IM业务msgId。消息严格顺序由既有 `conv_seq` 等业务游标承担，
不按XID/UUIDv7/历史TSID的混合字符串排序。

审核队列 `review_queue.msg_id` 自迁移 00000230 起使用 text，保留 opaque 原值；
频道、动态和资料审核的既有实体整数引用仅转换为十进制字符串。回滚为 bigint
会拒绝非规范十进制或越界值，避免改变身份或丢失数据。

Ordinary IM message IDs are opaque business identities, distinct from entity row
keys. Clients retain XID; newly created server messages use UUIDv7. Preserve existing
IDs across retries, finalization, acknowledgements, references and E2EE bindings.
Use Type + MsgId for defensive completion targeting without relaxing archive
uniqueness. Canonical customer-service message entity keys remain domain TSIDs.
Migration 00000230 stores review queue message references as text, preserving opaque
IDs and decimal entity references. Its rollback rejects noncanonical or out-of-range IDs.

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

- [TSID V10架构与取舍](../explanation/tsid-v10.md)
- [静态Namespace完整清单](./tsid-namespaces.md)
- [环境变量](./env-vars.md) — 五个有效TSID覆盖变量

- `docs/reference/rest-api.md` — REST API 通用约定
- `docs/reference/websocket-api-2.md` — WebSocket 消息协议
- `src/lib/elib_tsid.erl` — TSID 生成器实现
- `imboy-admin-frontend/src/services/api/client.ts` — safeParseBigIntJson 实现
- `imboy-admin-frontend/src/types/common.ts` — EntityId 类型定义

## 7. 已知风险与运行约束 / Known Risks and Operating Constraints

| 场景 | 已验证行为与限制 |
|---|---|
| 同毫秒重启 | 两次独立 BEAM 发号实测 100 个 ID 重叠；仅时钟没有回拨不足以保证跨重启唯一 |
| 跨重启回拨 10 秒 | 回拨期间无重叠，但时间返回旧毫秒后实测 100 个 ID 重叠 |
| 两个实例复用 NodeId | 同一 `(dc_id, node_id)` 的两个独立 BEAM 实测 50 个 ID 重叠；无跨实例锁 |
| 固定 Offset 与系统钟偏离 | 超过 `tsid_max_clock_drift_ms` 时阻断发号；调用方必须处理 typed error，不得绕过保护 |
| 跨 Namespace 同值 | 数字相等不代表同一实体；引用、缓存键与关联条件须包含业务域 |
| 序列耗尽或 CAS 竞争 | 有界等待可能超时；调用方须处理失败，不能假定发号永远成功 |

投产前须核对全体实例的 NodeId 分配、位布局及停机重启流程；preflight 检查不能证明未知独立实例没有复用 NodeId。V10 不保存 Cursor/Fence，不执行旧 catalog 高水位扫描或 manifest 重绑，也不保证任意跨重启场景唯一。保留数据库主键约束，遇到冲突应停止并排查原因。

2026-10-11 后端验收：全量 EUnit 11085 项、TSID 专项 72 项、Agent 真实 PG 28 项、客服真实 PG 25 项通过；一小时 121 个漂移样本偏差 0～1ms，仅代表该观测区间。
同宿主三轮对照早期 V10，公平单 ID 场景吞吐变化 −3.28%～+1.82%；批量场景约下降 93.39%，但批尾时间约束不同，不能作为等约束性能比较，也不能宣称零回归。未验证生产容量、长期漂移及三端运行验收。

V10 is nonpersistent: same-millisecond restarts, clock rollback across restarts,
and independent instances sharing a NodeId can produce duplicate IDs. Operators
must assign unique NodeIds; callers must preserve domain-qualified references and
handle clock/capacity errors. Local backend tests do not establish production capacity.
