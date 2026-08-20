# 消息链路 API 契约（REST 历史 + WebSocket 信封）

> 状态：存量契约固化（2026-08-20 以 backend 代码为权威源整理）。REST 前缀 `/api/v1`，需要用户登录态；WS 信封为 v2.0 格式（`ver` 字段见 `src/ds/message_ds.erl` 的 `?CUR_MSG_VER`）。

## 1. 端点表

| 端点 | 方法 | 说明 | 权威源 |
|---|---|---|---|
| `/api/v1/msg/history` | GET | 会话历史消息（conv_seq 游标分页，升序增量） | `src/api/msg_handler.erl:152-178` |
| WS 出站信封（实时） | — | 在线投递帧，字段 `from`/`to` | `src/ds/message_ds.erl:192-217` |
| WS 出站信封（离线） | — | 离线补投帧，内部行字段 `from_id`/`to_id`，出站前归一为 `from`/`to` | `src/ds/message_ds.erl:479-520` |
| `/api/v1/msg/offline` | GET | 离线消息 HTTP 拉取（pull 模式），行形状与离线信封一致 | `src/imboy_router.erl:104`、`src/ds/message_ds.erl:468-520` |

## 2. REST `GET /api/v1/msg/history`

请求参数（`src/api/msg_handler.erl:165-171`）：

| 参数 | 类型 | 必填 | 语义 |
|---|---|---|---|
| `chat_type` | string | 是 | `c2c` 或 `c2g`（小写）；其他值报错 `不支持的 chat_type`（`src/logic/messaging_logic.erl:142-143`） |
| `peer_id` | string | 是 | TSID 编码态：C2C 为对方 uid，C2G 为 group_id |
| `after_seq` | int | 否 | 游标：上次最后一条 `conv_seq`，首次传 0；默认 0 |
| `limit` | int | 否 | 每页条数，默认 50，**服务端夹紧上限 100**（`msg_handler.erl:171`） |

响应 payload（`src/logic/messaging_logic.erl:119-124`）：

```json
{
  "messages": [ ... ],
  "next_seq": 42,
  "has_more": true,
  "conv_key": "c2c:100:200"
}
```

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `messages` | array | 否（可为空数组） | 消息行数组，按 `conv_seq` 升序 | `messaging_logic.erl:117` |
| `next_seq` | int | 否 | 本页最大 `conv_seq`（空页时回传 `after_seq`），作下次游标 | `messaging_logic.erl:171-175` |
| `has_more` | bool | 否 | `本页行数 >= limit`（注意：正好等于 limit 时可能虚报 true） | `messaging_logic.erl:122` |
| `conv_key` | string | 否 | 会话键，C2C `c2c:<min_uid>:<max_uid>`、C2G `c2g:<gid>` | `messaging_logic.erl:123`、`msg_archive_ds` conv_key |

`messages[]` 行字段（SQL 列源 `src/repo/msg_archive_repo.erl:160-164`；`from_id`/`to_id` → `from`/`to` 重命名源 `src/logic/messaging_logic.erl:146-168`）：

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `msg_id` | string | 否 | 消息唯一 ID（业务 msg_id） | `msg_archive_repo.erl:160` |
| `chat_type` | string | 否 | `c2c` / `c2g`（小写，归档表冗余列） | `msg_archive_repo.erl:160` |
| `conv_seq` | int | 否 | 会话内单调递增序号（游标字段） | `msg_archive_repo.erl:160`、`msg_archive_repo.erl:111-114` |
| `msg_type` | string | 否 | 内容类型 text/image/voice/video/file/location/custom/e2ee 等 | `msg_archive_repo.erl:160` |
| `from` | string/int | 否 | 发送者 ID（TSID，由 `from_id` 重命名；DB bigint，epgsql 出来是 int，未显式 string 化） | `messaging_logic.erl:146-156` |
| `to` | string/int | 是 | 接收者 ID；C2G 行为 null（群消息无单点接收者） | `messaging_logic.erl:158-163` |
| `group_id` | int | 是 | 群 ID，仅 C2G 行有值；C2C 为 null | `messaging_logic.erl:164-167` |
| `e2ee` | object/null | 是 | 端到端加密元数据（PFv3 时密文在 `e2ee.devices`，外层 payload 为空串） | `msg_archive_repo.erl:160` |
| `sender_did` | string | 是 | 发送方设备 ID（迁移 48 前旧行为 null） | `msg_archive_repo.erl:160` |
| `payload` | object/string | 否 | 消息载荷（E2EE 时为密文字符串） | `msg_archive_repo.erl:160` |
| `created_at` | int | 否 | **毫秒 int**（REST 信封统一转换，见 §5） | `msg_archive_repo.erl:161` |
| `server_ts` | int | 否 | **毫秒 int**（bigint 列直通） | `msg_archive_repo.erl:161` |

## 3. WebSocket 信封（实时路径）

权威源 `src/ds/message_ds.erl:202-217`（assemble_msg/8）与 `:267-306`（encode_websocket_message/1，白名单）：

```json
{
  "ver": 2,
  "id": "msg_xxx",
  "type": "C2C",
  "from": "100",
  "to": "200",
  "msg_type": "text",
  "action": "",
  "e2ee": null,
  "payload": { "text": "hello" },
  "server_ts": 1755690000123
}
```

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `ver` | int | 否 | 信封版本（出站=当前版本；旧客户端入站缺省视为当前） | `message_ds.erl:206、326` |
| `id` | string | 否 | 消息 ID（msg_id，binary 字符串） | `message_ds.erl:208` |
| `type` | string | 否 | `C2C`/`C2G`/`C2S`/`S2C`（大写） | `message_ds.erl:209` |
| `from` | string | 否 | 发送者 ID（TSID binary；DB 行的 `from_id` 在编码时归一为 `from`） | `message_ds.erl:271-285、299` |
| `to` | string | 否 | 接收者 ID（同上，`to_id` → `to`） | `message_ds.erl:279-285、300` |
| `msg_type` | string | 否 | 内容类型，顶层字段（v2.0），默认 `""` | `message_ds.erl:288、301` |
| `action` | string | 否 | S2C 指令；C2C/C2G 为 `""` | `message_ds.erl:289、302` |
| `e2ee` | object/null | 是 | E2EE 元数据，默认 null | `message_ds.erl:290、303` |
| `payload` | object | 否 | 载荷，默认 `{}` | `message_ds.erl:304` |
| `server_ts` | int | 否 | **毫秒 int**（`elib_dt:millisecond()`） | `message_ds.erl:216、305` |
| `sender_did` | string | 是 | 服务端认证态注入的发送方设备 ID；**没有就不补**（不出现 ≠ 空串），仅 C2C 实时帧带 | `message_ds.erl:513-520`、`msg_c2c_logic.erl:389-394` |

实时信封**没有 `created_at`**；客户端侧时间基准用 `server_ts`。

## 4. WebSocket 信封（离线路径）与 from_id 双轨

离线补投把 DB 行（`msg_c2c`/`msg_c2g`/`msg_s2c`，列名 `from_id`/`to_id`）组装成出站信封，权威源 `src/ds/message_ds.erl:479-520`（offline_envelope/2）：

- 入参 Row 为 binary key map，字段 `from_id`/`to_id`/`msg_id`/`payload`/`msg_type`/`e2ee`/`created_at`/`server_ts`（+可选 `sender_did`）。
- 出站仍经 `encode_websocket_message/1` 归一：**最终线上帧字段是 `from`/`to`**（`from_id` 只存在于 DB 行/内部 map 这一轨）。
- `action`：旧表无独立列，从 `e2ee.relay_action` 恢复（E2EE 编辑旁路），默认 `""`（`message_ds.erl:489-491、522-526`）。
- `created_at`/`server_ts` 组帧前经 `elib_cnv:convert_at_timestamps/1` 统一转**毫秒 int**（`message_ds.erl:495-497`）。
- `sender_did`：旧行（迁移 48 前）与非 C2C 类型为 null → 信封**不带该键**，绝不补空串（fail-closed，`message_ds.erl:513-520`）。

**from_id 双轨总结**：DB/内部行命名 `from_id`/`to_id`；所有出站（WS 实时、WS 离线、REST history）统一 `from`/`to`。消费方不得在出站帧里找 `from_id`。

## 5. 时间戳双轨（已知行为，务必注意）

- **REST**：`elib_response:success` 对 payload 全量做 `convert_at_timestamps`（`src/lib/elib_response.erl:30-32`）：键名以 `_at`/`_ts` 结尾的值统一经 `elib_dt:rfc3339_to/1`——timestamptz 元组 → 毫秒 int（`src/lib/elib_dt.erl:226-235`）；已是 int（如 bigint 毫秒列）直通（`elib_dt.erl:270-271`）。因此 REST 一切 `_at`/`_ts` 字段均为**毫秒 int**。
- **服务端内部管线**：消息写入路径的 `created_at` 用 `elib_dt:now()` 生成 **RFC3339 微秒字符串**（`src/logic/msg_c2c_logic.erl:125、334`；`src/lib/elib_dt.erl:118-128`）。该形态可能出现在兼容路径回投帧与 DB 落库值中。
- **Flutter 双格式容错**（消费侧事实）：`lib/service/message.dart:528-532` 与 `lib/store/repository/message_repo_sqlite.dart:1065-1073` 均按「int 直用 / RFC3339 字符串解析为毫秒」双轨归一。新消费方必须实现同样的容错。

## 6. 前端消费方

| 端 | 文件 | 说明 |
|---|---|---|
| Flutter | `imboyapp/lib/store/api/msg_api.dart`（history 请求，limit 客户端亦 clamp 1-100） | 请求构造 |
| Flutter | `imboyapp/lib/page/chat/chat/services/chat_archive_service.dart:242-282、336-355` | 消费 `messages/next_seq/has_more`；`chat_type`→`type`，C2G 行 `to` 为 null 时用 `group_id` 兜底 |
| Flutter | `imboyapp/lib/store/repository/message_repo_sqlite.dart:923-985` | WS/离线行落库：读 `from`/`to`；`sender_did` 仅 C2C 持久化 |
| Flutter | `imboyapp/lib/store/model/message_model.dart`、`message_columns.dart` | 本地列 `from_id`/`to_id`（本地 SQLite 命名，与服务端出站 `from`/`to` 是两套名字） |
| Flutter | `imboyapp/lib/service/message.dart:526-536` | 实时帧 `created_at` 字符串→毫秒归一 |
| admin | `imboyadmin/src/types/message.ts`（`ManagedMessage`） | 管理端消息行：`server_ts` 注释明确「13 位毫秒 number」 |

## 7. 已知漂移与注意事项

1. **`has_more` 语义**：`length(Rows) >= Limit` 判定，末页正好等于 limit 时返回 true，客户端需以「空页」终止（Flutter 已按 fetched==0 处理）。
2. **REST history 行的 `from`/`to` 类型**：DB bigint 直出 JSON number，未做 TSID→string（`messaging_logic.erl:146-168` 只重命名不转类型）。Dart int64 安全；JS 消费方有 53bit 精度风险（admin 侧 `safeParseBigIntJson` 仅兜底 16 位以上数字）。
3. **`conv_seq` 稀疏性**：按 `conv_seq > after_seq` 严格递增游标，不假设连续。
4. **WS 离线帧 `action` 旁路**：E2EE 编辑消息的 `action` 从 `e2ee.relay_action` 恢复，不读密文正文（`message_ds.erl:489-491`）。
5. **`sender_did` fail-closed**：缺失即不带键，消费方不得把「键不存在」当成「空串设备」。
