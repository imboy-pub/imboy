# IMBoy 协议一致性审计报告 / Protocol Consistency Audit Report

> **审计日期 / Audit Date**：2026-05-27  
> **版本 / Version**：后端 Erlang/OTP 28+ | 客户端 Flutter/Dart 3.8+  
> **审计范围 / Scope**：5 大协议点，覆盖 WebSocket ACK、消息结构、好友流程、E2EE 密钥、离线消息拉取

---

## 总览 / Summary

| 协议点 | 状态 | 关键发现 |
|--------|------|---------|
| 1. WebSocket ACK 协议 | ✅ 基本一致（含已知差异） | 超时/重试间隔双端有差异，但不阻断功能 |
| 2. 消息结构（C2C/C2G） | ✅ 一致 | 字段映射完全匹配 |
| 3. 好友申请/同意流程 | ✅ 一致 | 路径、参数均匹配 |
| 4. E2EE 密钥交换 | ⚠️ 存在路径差异 | 客户端有 1 个路由未在后端注册 |
| 5. 离线消息拉取 | ⚠️ 参数缺失 | 客户端未发送分页时间戳参数 |

---

## 1. WebSocket ACK 协议 / WebSocket ACK Protocol

### 1.1 文本格式 ACK（向后兼容路径）/ Text-format ACK (Backward-compatible path)

**后端格式定义（`websocket_handler.erl:129`）**  
**Backend format (`websocket_handler.erl:129`)**

```
CLIENT_ACK,<type>,<msgid>,<did>
# 示例 / Example: CLIENT_ACK,C2C,msg001,device_abc
```

**客户端格式定义（`ack_manager.dart:322`）**  
**Client format (`ack_manager.dart:322`)**

```dart
return 'CLIENT_ACK,$type,$msgId,$effectiveDeviceId';
```

✅ **格式完全一致 / Format is identical**

---

### 1.2 二进制 v2 帧 ACK

**后端（`websocket_handler.erl:218–233`）**

```erlang
dispatch_v2_frame(?FRAME_TYPE_ACK, <<MsgIdInt:64/big-unsigned>>, State)
% payload: 64-bit big-endian unsigned integer
```

**客户端（`websocket.dart:520–529`、`ack_manager.dart:357–363`）**

```dart
// 接收路径：
final msgId = ByteData.sublistView(frame.payload).getUint64(0, Endian.big);
// 发送路径（ImboyFrame.ack）：
final bytes = ImboyFrame.ack(numericId); // numericId = int.tryParse(msgId)
```

✅ **二进制帧结构一致 / Binary frame structure is consistent**

---

### 1.3 ACK 确认消息（服务端响应）/ ACK confirmation message (server response)

**后端发送（`websocket_handler.erl:321–326`）**

```erlang
AckConfirmMsg = #{
    <<"id">>        => MsgId,
    <<"type">>      => <<"CLIENT_ACK_CONFIRM">>,
    <<"action">>    => <<"CLIENT_ACK_CONFIRM">>,
    <<"server_ts">> => elib_dt:millisecond()
}
```

**客户端处理（`websocket.dart:679–743`）**

```dart
if (action == 'CLIENT_ACK_CONFIRM' && messageId.isNotEmpty) {
    AckManager.to.ackConfirmed(messageId);
}
```

✅ **字段映射一致：`id`→`messageId`，`action`→`CLIENT_ACK_CONFIRM`**  
✅ **Field mapping consistent: `id`→`messageId`, `action`→`CLIENT_ACK_CONFIRM`**

---

### 1.4 超时与重试间隔差异 / Timeout & Retry Interval Difference

| 参数 | 后端（Erlang） | 前端（Flutter） | 状态 |
|------|--------------|-----------------|------|
| 服务端重试序列 | `elib_retry_config:intervals(<<"s2c">>)` → 2000 / 5000 / 7000 / 11000 ms（CLAUDE.md） | 不控制服务端重试 | — |
| 客户端 ACK 重试间隔 | 不适用 | 3000 / 5000 / 10000 ms（`ack_manager.dart:71`） | ⚠️ 差异 |
| 客户端最大重试次数 | 服务端 4 次 → 转离线 | `_maxRetries = 3`（`ack_manager.dart:67`） | ⚠️ 差异 |
| WebSocket idle_timeout | 180000 ms (3min) | `_pingInterval = 60s`（`websocket.dart:73`） | ✅ 兼容 |

⚠️ **说明 / Note**：客户端 ACK 最大重试 3 次（18s 内），服务端重试 4 次（25s 内）。参数差异不会导致协议故障，但在弱网场景下服务端可能在客户端放弃重试后仍持续发送。建议双端对齐到 4 次重试。

⚠️ **Note**: Client ACK max retries is 3 (within 18s); server retries 4 times (within 25s). The difference won't break the protocol but may cause redundant delivery in poor network conditions. Recommend aligning both sides to 4 retries.

---

## 2. 消息结构（C2C/C2G）/ Message Structure (C2C/C2G)

### 2.1 客户端发送字段 / Client send fields

**客户端（`service/CLAUDE.md` + `message_model.dart`）**

| 字段 | 类型 | 说明 |
|------|------|------|
| `id` | String | 消息 ID（Xid base32hex） |
| `type` | String | `C2C` / `C2G` |
| `from` | String | 发送者 TSID |
| `to` | String | 接收者 TSID（v2.0，不再是 `to_id`） |
| `msg_type` | String | `text` / `image` / `file` / `voice` |
| `payload` | String\|Map | 消息内容 |
| `e2ee` | Map | E2EE 元数据（必须是 Map） |
| `created_at` | int | 毫秒时间戳 |

**后端（`src/logic/CLAUDE.md` + `message_ds.erl`）**

| 字段 | 类型 | 说明 |
|------|------|------|
| `<<"id">>` | binary | 消息 ID |
| `<<"type">>` | binary | `<<"C2C">>` / `<<"C2G">>` |
| `<<"from">>` | binary | 发送者 TSID（兼容 `from_id`） |
| `<<"to">>` | binary | 接收者 TSID（兼容 `to_id`） |
| `<<"msg_type">>` | binary | 消息类型 |
| `<<"payload">>` | binary | 消息内容 |
| `<<"e2ee">>` | map | E2EE 元数据（Map，非 JSON 字符串） |
| `<<"created_at">>` | integer | 毫秒时间戳 |

✅ **字段名称完全一致 / Field names are fully consistent**  
✅ **`to` 字段：双端均使用 `to`，v2.0 向后兼容 `to_id`（后端 `message_ds:decode_websocket_message/1`）**

---

### 2.2 服务端消息封装结构 / Server message envelope structure

**后端（`message_ds:assemble_msg`，在多处 logic 模块引用）**

```erlang
#{
    <<"id">>        => MsgId,
    <<"type">>      => Type,      % "C2C" / "C2G" / "S2C"
    <<"from">>      => From,
    <<"to">>        => To,
    <<"payload">>   => Payload,
    <<"action">>    => Action,    % v2.0 顶层 action
    <<"server_ts">> => elib_dt:millisecond()
}
```

**客户端解析（`websocket.dart:643–649`）**

```dart
final action = msg['action']?.toString() ?? '';
final messageType = msg['type']?.toString() ?? '';
final messageId = msg['id']?.toString() ?? '';
```

✅ **消息解包字段完全匹配 / Message unpacking fields fully match**

---

## 3. 好友申请/同意流程 / Friend Request/Confirm Flow

### 3.1 好友申请 / Friend Request

**后端路由 (`imboy_router.erl:299`)**
```
POST /v1/friend/add
```

**后端参数 (`friend_handler.erl:51–53`)**
```erlang
To = maps:get(<<"to">>, PostVals, undefined),
Payload = maps:get(<<"payload">>, PostVals, undefined),
CreatedAt = maps:get(<<"created_at">>, PostVals, undefined)
```

**客户端调用 (`apply_friend_provider.dart:98–110`)**
```dart
Map<String, dynamic> msg = {
    "to": to,
    "payload": json.encode(payload),
    "created_at": createdAt,
};
// POST ${Env().apiBaseUrl}${API.addFriend}  → /v1/friend/add
```

✅ **路径一致：`/v1/friend/add`**  
✅ **参数一致：`to`、`payload`（JSON 字符串）、`created_at`**

---

### 3.2 好友申请确认 / Friend Request Confirm

**后端路由 (`imboy_router.erl:300`)**
```
POST /v1/friend/confirm
```

**后端参数 (`friend_handler.erl:70–75`)**
```erlang
From = maps:get(<<"from">>, PostVals, undefined),
To = maps:get(<<"to">>, PostVals, undefined),
Payload = maps:get(<<"payload">>, PostVals, undefined)
```

**客户端调用 (`confirm_new_friend_provider.dart:97–101`)**
```dart
Map<String, dynamic> msg = {
    "from": from,
    "to": to,
    "payload": json.encode(payload),
};
// POST ${Env().apiBaseUrl}${API.confirmFriend} → /v1/friend/confirm
```

✅ **路径一致：`/v1/friend/confirm`**  
✅ **参数一致：`from`、`to`、`payload`（JSON 字符串）**

---

### 3.3 好友申请通知（S2C）/ Friend request notification (S2C)

**后端 S2C action（`msg_s2c_logic.erl` / `message_ds`）**：`apply_friend`  
**客户端处理（`message_s2c.dart:141–145`）**

```dart
case 'apply_friend':
    await _providerContainer
        .read(newFriendProvider.notifier)
        .receivedAddFriend(data);
    break;
```

**好友确认通知**：`apply_friend_confirm`  
客户端处理（`message_s2c.dart:147`）：`_handleApplyFriendConfirm`

✅ **S2C action 字段匹配 / S2C action fields match**

---

## 4. E2EE 密钥交换 / E2EE Key Exchange

### 4.1 API 路径对比 / API Path Comparison

| 功能 | 后端路由（`imboy_router.erl`） | 客户端常量（`const.dart`） | 状态 |
|------|-------------------------------|--------------------------|------|
| 上报设备公钥 | `POST /v1/e2ee/report_device_key` | `API.e2eeReportDeviceKey = '/v1/e2ee/report_device_key'` | ✅ |
| 获取用户密钥 | `GET /v1/e2ee/user_keys` | `API.e2eeUserKeys = '/v1/e2ee/user_keys'` | ✅ |
| 获取群成员密钥 | `GET /v1/e2ee/group_member_keys` | `API.e2eeGroupMemberKeys = '/v1/e2ee/group_member_keys'` | ✅ |
| 合规公钥 | `GET /v1/e2ee/compliance_key` | `API.e2eeComplianceKey = '/v1/e2ee/compliance_key'` | ✅ |
| 密钥状态 | `GET /v1/e2ee/key/status` | 客户端未定义常量 | ⚠️ |
| 密钥变更通知拉取 | `GET /v1/e2ee/notifications/pull` | 客户端未定义常量 | ⚠️ |
| 备份列表 | `GET /v1/e2ee/backup/list` | `API.e2eeBackupList = '/v1/e2ee/backup/list'` | ✅ |
| 删除备份 | `DELETE /v1/e2ee/backup/delete` | `API.e2eeBackupDelete = '/v1/e2ee/backup/delete'` | ✅ |

⚠️ **差异说明 / Difference note**：  
后端注册了 `/v1/e2ee/key/status` 和 `/v1/e2ee/notifications/pull` 两个路由，但客户端 `const.dart` 中没有对应的常量定义，说明这两个接口尚未在客户端侧集成（可能是功能尚未对接，或通过其他路径调用）。

⚠️ **Note**: The backend registers `/v1/e2ee/key/status` and `/v1/e2ee/notifications/pull`, but client `const.dart` has no matching constants, indicating these two endpoints are not yet integrated on the client side.

---

### 4.2 公钥字段格式 / Public Key Field Format

**后端接收（`e2ee_handler.erl:155–157`）**
```erlang
PublicKey = maps:get(<<"public_key">>, PostVals, <<>>),
KeyId = maps:get(<<"key_id">>, PostVals, <<>>),
```

**客户端发送（`e2ee_api.dart:34–39`）**
```dart
data: {
    'device_id': deviceId,
    'device_type': deviceType,
    'device_name': deviceName,
    'public_key': publicKey,     // PEM 格式
    'key_id': keyId,
}
```

✅ **字段名一致：`public_key`、`key_id`、`device_id`、`device_type`**

---

### 4.3 响应字段格式 / Response field format

**后端 user_keys 响应（`e2ee_logic:user_keys`，经 `elib_response:success` 包装）**  
期望结构：`{devices: [...]}`  

**客户端解析（`e2ee_api.dart:49–53`）**
```dart
final devices = payload['devices'];
```

✅ **响应结构匹配**

**后端 group_member_keys 响应（`e2ee_logic:group_member_keys`）**  
期望结构：`{members: [...]}`  

**客户端解析（`e2ee_api.dart:65–69`）**
```dart
final members = payload['members'];
```

✅ **响应结构匹配**

---

## 5. 离线消息拉取 / Offline Message Pull

### 5.1 接口路径 / Endpoint Path

| 接口 | 后端路由 | 客户端常量 | 状态 |
|------|---------|-----------|------|
| 拉取离线消息 | `GET /v1/msg/offline` | `API.msgOffline = '/v1/msg/offline'` | ✅ |
| 确认离线消息 | `POST /v1/msg/offline_ack` | `API.msgOfflineAck = '/v1/msg/offline_ack'` | ✅ |

---

### 5.2 分页参数差异（关键差异）/ Pagination Parameter Difference (Critical)

**后端支持的 GET 查询参数（`messaging_logic.erl:43–51`）**

```erlang
{ok, Limit} = elib_param:int(limit, Req0, 1000),
{ok, C2CLastMsgAtInt} = elib_param:int(c2c_last_msg_at, Req0, 0),
{ok, C2GLastMsgAtInt} = elib_param:int(c2g_last_msg_at, Req0, 0),
{ok, S2CLastMsgAtInt} = elib_param:int(s2c_last_msg_at, Req0, 0),
```

**客户端调用（`message_offline.dart:230`）**

```dart
final resp = await HttpClient.client.get(API.msgOffline);
// 不带任何查询参数！
```

⚠️ **差异 / Difference**：

| 参数 | 后端默认值 | 客户端是否传递 |
|------|-----------|--------------|
| `limit` | 1000 | ❌ 未传递（使用服务端默认 1000） |
| `c2c_last_msg_at` | 0（全量） | ❌ 未传递（每次全量拉取） |
| `c2g_last_msg_at` | 0（全量） | ❌ 未传递 |
| `s2c_last_msg_at` | 0（全量） | ❌ 未传递 |

⚠️ **影响 / Impact**：客户端每次调用 `/v1/msg/offline` 都是全量拉取（无增量游标），后端返回全部未读离线消息（默认 limit=1000）。在消息量大时，可能产生较大的响应体，影响性能。客户端通过 `has_more` 字段检测是否有更多，并循环最多 20 次（`_maxPullCount = 20`），实际行为上接近"分批全量"而非"增量拉取"。

⚠️ **Impact**: Each call to `/v1/msg/offline` is a full pull (no incremental cursor). The backend returns all unread offline messages (default limit=1000). For large message volumes, this may produce large response bodies. The client uses `has_more` for pagination (up to 20 rounds), resulting in "batched full pull" rather than true incremental pull.

---

### 5.3 响应结构 / Response Structure

**后端响应（`messaging_logic.erl:69–87`）**

```erlang
#{
    <<"c2c">> => #{
        <<"has_more">>          => boolean(),
        <<"next_last_msg_at">>  => binary(),   % RFC3339
        <<"total">>             => integer(),
        <<"list">>              => [...]
    },
    <<"c2g">> => #{ ... },
    <<"s2c">> => #{ ... }
}
```

**客户端解析（`message_offline.dart:253–282`）**

```dart
final section = payload[typeKey];           // 'c2c' / 'c2g' / 's2c'
final rawList = section['list'];            // ✅
bool hasMore = section['has_more'] == true; // ✅
```

✅ **响应结构字段匹配：`list`、`has_more` 均正确访问**  
⚠️ **客户端未使用 `next_last_msg_at` 和 `total` 字段**（丢弃了游标信息，导致无法做增量拉取）

---

### 5.4 离线消息确认参数 / Offline message ACK parameters

**后端（`messaging_logic.erl:217–219`）**

```erlang
Type = string:lowercase(maps:get(<<"type">>, PostVals, <<>>)),  % "c2c"/"c2g"/"s2c"
MsgIds = maps:get(<<"msg_ids">>, PostVals, []),
```

**客户端（`message_offline.dart:429–435`）**

```dart
await HttpClient.client.post(
    API.msgOfflineAck,
    data: {'type': type, 'msg_ids': msgIds},  // type 为 "C2C"/"C2G"/"S2C"
);
```

⚠️ **大小写差异 / Case Difference**：后端用 `string:lowercase` 转小写处理，最终比较 `<<"c2c">>`；客户端发送的是大写 `"C2C"`/`"C2G"`/`"S2C"`。  
✅ **后端做了 `string:lowercase` 处理，因此大小写差异不会引起故障**，但建议客户端统一使用小写以减少依赖。

⚠️ **Case difference**: Backend uses `string:lowercase` to normalize to lowercase; client sends uppercase `"C2C"`. Backend handles this gracefully, but client should send lowercase to reduce coupling.

---

## 汇总与建议 / Summary & Recommendations

### 已确认一致 / Confirmed Consistent

1. ✅ WebSocket 文本 ACK 格式 `CLIENT_ACK,type,msgid,did`
2. ✅ WebSocket v2 二进制 ACK 帧（64-bit big-endian msgId）
3. ✅ `CLIENT_ACK_CONFIRM` / `CLIENT_ACK_ERROR` 响应字段
4. ✅ C2C/C2G 消息字段名（`id`、`type`、`from`、`to`、`msg_type`、`payload`、`e2ee`、`created_at`）
5. ✅ 好友申请：`POST /v1/friend/add`，参数 `to`/`payload`/`created_at`
6. ✅ 好友确认：`POST /v1/friend/confirm`，参数 `from`/`to`/`payload`
7. ✅ E2EE 核心接口路径（`report_device_key`、`user_keys`、`group_member_keys`、`compliance_key`）
8. ✅ E2EE 字段名（`public_key`、`key_id`、`device_id`、`device_type`）
9. ✅ 离线消息接口路径（`/v1/msg/offline`、`/v1/msg/offline_ack`）
10. ✅ 离线消息响应结构（`list`、`has_more`）

### 存在差异 / Identified Differences

| 编号 | 严重程度 | 位置 | 差异描述 | 建议 |
|------|---------|------|---------|------|
| D1 | 低 | ACK 超时配置 | 客户端 ACK 重试 3 次（18s），服务端重试 4 次（25s） | 双端对齐到 4 次 |
| D2 | 低 | ACK 重试间隔 | 客户端 3s/5s/10s，服务端 2s/5s/7s/11s | 对齐到服务端配置 |
| D3 | 中 | E2EE 接口 | `/v1/e2ee/key/status` 和 `/v1/e2ee/notifications/pull` 后端有路由，客户端无常量 | 评估是否需集成，若需要则在 `const.dart` 补充 |
| D4 | 中 | 离线消息拉取 | 客户端未传分页时间戳参数，每次全量拉取 | 传递 `c2c_last_msg_at` 等参数，利用 `next_last_msg_at` 实现增量拉取 |
| D5 | 低 | 离线消息确认类型 | 客户端发大写 `"C2C"`，后端期望小写 `"c2c"`（后端有兼容处理） | 客户端改为小写，减少对后端兼容逻辑的依赖 |

---

## 附录：关键文件路径 / Appendix: Key File Paths

| 文件 | 路径 |
|------|------|
| 后端 WS Handler | `imboy/src/api/websocket_handler.erl` |
| 后端 WS Logic | `imboy/src/logic/websocket_logic.erl` |
| 后端消息逻辑 | `imboy/src/logic/messaging_logic.erl` |
| 后端好友 Handler | `imboy/src/api/friend_handler.erl` |
| 后端 E2EE Handler | `imboy/src/api/e2ee_handler.erl` |
| 后端路由 | `imboy/src/imboy_router.erl` |
| 客户端 WS 服务 | `imboyapp/lib/service/websocket.dart` |
| 客户端 ACK 管理 | `imboyapp/lib/service/ack_manager.dart` |
| 客户端 S2C 处理 | `imboyapp/lib/service/message_s2c.dart` |
| 客户端离线消息 | `imboyapp/lib/service/message_offline.dart` |
| 客户端 E2EE API | `imboyapp/lib/store/api/e2ee_api.dart` |
| 客户端 E2EE 密钥服务 | `imboyapp/lib/service/e2ee_key_service.dart` |
| 客户端好友申请 | `imboyapp/lib/page/contact/apply_friend/apply_friend_provider.dart` |
| 客户端好友确认 | `imboyapp/lib/page/contact/confirm_new_friend/confirm_new_friend_provider.dart` |
| 客户端常量配置 | `imboyapp/lib/config/const.dart` |
