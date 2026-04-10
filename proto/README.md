# IMBoy Protocol Buffers Schema

## 概述

本目录定义 IMBoy WebSocket 二进制协议的 Protocol Buffers Schema，用于替代当前的 JSON 文本协议。

## 文件结构

```
proto/
└── imboy.proto          # 完整的消息定义（单文件，便于双端同步）
```

## 设计决策

### 1. 单文件 vs 多文件

选择 **单文件** 方案，原因：
- IMBoy 消息类型总量有限（~20种），不需要拆分
- 单文件更易于 Erlang (gpb) 和 Dart (protoc) 双端同步
- 避免跨文件 import 在不同工具链中的兼容性问题

### 2. ID 类型选择

| 字段 | proto 类型 | 说明 |
|------|-----------|------|
| 用户/群组 ID | `sint64` | TSID 是 64-bit 有符号整数，匹配 PostgreSQL BIGINT |
| 消息 ID | `string` | 保持与现有 TSID 字符串格式兼容 |

### 3. Payload 设计

采用 `bytes` 而非 `oneof`：
- `IMBoyMessage.payload` 是 `bytes` 类型
- 根据 `msg_type` 字段决定用哪个 `PayloadXxx` 消息解码
- 这样做的好处：
  - 避免 `oneof` 的字段号冲突限制
  - 服务端可以"透传"不需要解析的 payload（如 E2EE 加密内容）
  - 新增消息类型不影响 envelope 结构

### 4. 与现有 JSON 协议的映射

```
JSON                          Protobuf
────────────────────────────  ────────────────────
{"id": "abc123"}              id = "abc123"
{"type": "C2C"}               type = C2C (enum 1)
{"from": "123456"}            from = 123456 (sint64)
{"to": "789012"}              to = 789012 (sint64)
{"msg_type": "text"}          msg_type = TEXT (enum 1)
{"action": "message_revoke"}  action = "message_revoke"
{"e2ee": {...}}               e2ee = E2EEMeta{...}
{"payload": {...}}            payload = PayloadText{...}.encode()
{"created_at": "2026-..."}    created_at = 1710000000000 (ms)
{"server_ts": 1710000000000}  server_ts = 1710000000000
```

### 5. 重要变化

| 变化点 | JSON 协议 | Protobuf 协议 |
|--------|----------|---------------|
| UID 类型 | string `"123456"` | sint64 `123456` |
| 时间戳 | RFC3339 string 或 int | 统一 int64 毫秒 |
| E2EE nonce/ek | base64 string | raw bytes |
| CLIENT_ACK | 文本 `"CLIENT_ACK,C2C,id,did"` | PayloadClientAck 消息 |
| 传输帧 | WebSocket text frame | WebSocket binary frame |

## 代码生成

### Erlang (gpb)

```bash
# 安装 gpb
# rebar.config: {deps, [{gpb, "4.21.1"}]}

# 生成 Erlang 代码
gpb_compile:file("proto/imboy.proto", [
    {maps, true},
    {type_specs, true},
    {strings_as_binaries, true},
    {module_name, imboy_pb},
    {o_erl, "src/lib"},
    {o_hrl, "include"}
]).
```

生成的文件：
- `src/lib/imboy_pb.erl` — 编解码模块
- `include/imboy_pb.hrl` — 类型定义

### Flutter/Dart (protoc)

```bash
# 安装 protoc 和 dart 插件
dart pub global activate protoc_plugin

# 生成 Dart 代码
protoc --dart_out=lib/generated proto/imboy.proto
```

生成的文件：
- `lib/generated/imboy.pb.dart` — 消息类
- `lib/generated/imboy.pbenum.dart` — 枚举
- `lib/generated/imboy.pbjson.dart` — JSON 映射

## 使用示例

### Erlang 编码/解码

```erlang
%% 编码文本消息
Payload = imboy_pb:encode_msg(#{body => <<"Hello">>, mentions => []}, 'PayloadText'),
Msg = imboy_pb:encode_msg(#{
    id => <<"msg-123">>,
    type => 'C2C',
    from => 123456,
    to => 789012,
    msg_type => 'TEXT',
    action => <<>>,
    payload => Payload,
    created_at => erlang:system_time(millisecond)
}, 'IMBoyMessage'),

%% 发送 WebSocket binary frame
{[{binary, Msg}], State}.

%% 解码
#{type := Type, msg_type := MsgType, payload := PayloadBin} =
    imboy_pb:decode_msg(Bin, 'IMBoyMessage'),

case MsgType of
    'TEXT' ->
        #{body := Body} = imboy_pb:decode_msg(PayloadBin, 'PayloadText');
    'IMAGE' ->
        #{url := Url} = imboy_pb:decode_msg(PayloadBin, 'PayloadImage')
end.
```

### Dart 编码/解码

```dart
import 'package:imboy/generated/imboy.pb.dart';

// 编码文本消息
final payload = PayloadText()..body = 'Hello';
final msg = IMBoyMessage()
  ..id = 'msg-123'
  ..type = MsgDirection.C2C
  ..from = Int64(123456)
  ..to = Int64(789012)
  ..msgType = ContentType.TEXT
  ..payload = payload.writeToBuffer()
  ..createdAt = Int64(DateTime.now().millisecondsSinceEpoch);

// 发送 WebSocket binary frame
websocket.add(msg.writeToBuffer());

// 解码
final decoded = IMBoyMessage.fromBuffer(bytes);
if (decoded.msgType == ContentType.TEXT) {
  final text = PayloadText.fromBuffer(decoded.payload);
  print(text.body);
}
```

## Schema 演进规则

1. **永远不要**删除或重用已分配的字段号
2. 新增字段使用新的字段号（proto3 默认值机制保证向后兼容）
3. 新增枚举值追加在末尾
4. 使用 `reserved` 标记已废弃的字段号
5. Payload 消息可以独立演进，不影响 envelope
