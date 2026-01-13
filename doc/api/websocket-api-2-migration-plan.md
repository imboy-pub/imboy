# WebSocket API v2.0 代码迁移计划

> **版本**: 2.0.0
> **创建时间**: 2025-01-19
> **目标**: 直接在原代码基础上调整为 v2.0 API 规范

---

## 目录

- [迁移概述](#迁移概述)
- [核心变更](#核心变更)
- [前端迁移步骤](#前端迁移步骤)
- [后端迁移步骤](#后端迁移步骤)
- [验证测试](#验证测试)
- [时间计划](#时间计划)

---

## 迁移概述

### 迁移目标

从 WebSocket API v1.0 迁移到 v2.0，主要变更包括：

1. **消息结构扁平化**：`msg_type`、`action`、`e2ee` 从 payload 提升到顶层
2. **字段职责分离**：S2C 消息使用 `action`，非 S2C 消息使用 `msg_type`
3. **E2EE 优化**：`payload` 直接存储密文字符串，`e2ee` 仅含元数据
4. **数据库表分离**：按消息类型分为 `msg_c2c`、`msg_c2g`、`msg_c2s`、`msg_s2c`（表结构已调整完成）

### 迁移策略

**直接修改原代码**，不创建新文件，确保最小化变更范围。

### 数据库处理

- **前端**：新用户使用 `assets/example10.db`（已包含新表结构）；老用户删除旧消息表，应用更新后自动创建新表
- **后端**：表结构已调整完成，无需额外迁移操作

---

## 核心变更

### 1. 消息结构变更

#### v1.0 结构（旧）

```json
{
  "id": "msg_id",
  "type": "C2C",
  "from": "user_id",
  "to": "user_id",
  "payload": {
    "msg_type": "text",
    "content": "Hello"
  }
}
```

#### v2.0 结构（新）

```json
{
  "id": "msg_id",
  "type": "C2C",
  "from": "user_id",
  "to": "user_id",
  "msg_type": "text",
  "action": "",
  "e2ee": "",
  "payload": {
    "content": "Hello"
  }
}
```

### 2. 字段使用规则

| 消息类型 | `action` | `msg_type` | `e2ee` |
|---------|----------|------------|--------|
| **S2C** | ✅ 必须有 | ✅ 预留 | ❌ 不支持 |
| **C2C** | ❌ 不需要 | ✅ 必须有 | 🔸 加密时需要 |
| **C2G** | ❌ 不需要 | ✅ 必须有 | 🔸 加密时需要 |
| **C2S** | ❌ 不需要 | ✅ 必须有 | 🔸 加密时需要 |

### 3. E2EE 消息结构

#### v2.0 E2EE 格式

```json
{
  "e2ee": {
    "e2ee": true,
    "e2ee_ver": 1,
    "e2ee_suite": "RSA-OAEP-256+AES-256-GCM",
    "nonce": "base64_nonce",
    "keys": [...]
  },
  "payload": "base64_ciphertext"
}
```

---

## 前端迁移步骤

### 阶段 1：更新消息模型（2-3 天）

#### 1.1 修改 MessageModel

**文件**：`lib/store/model/message_model.dart`

**原代码**：
```dart
class MessageModel {
  String? id;
  String? type;
  Map<String, dynamic>? payload;  // 旧：所有内容在 payload 里

  factory MessageModel.fromJson(Map<String, dynamic> data) {
    return MessageModel(
      id: data['id'],
      type: data['type'],
      payload: data['payload'],  // 旧：直接读取
    );
  }
}
```

**修改为**：
```dart
class MessageModel {
  String? id;
  String? type;

  // v2.0 新增字段
  String? msgType;   // C2C/C2G/C2S: 必须有, S2C: 可选
  String? action;    // S2C: 必须有, 其他: 不需要
  Map<String, dynamic>? e2ee;  // C2C/C2G: 加密时有值
  dynamic payload;   // 非 E2EE: Map, E2EE: String

  factory MessageModel.fromJson(Map<String, dynamic> data) {
    final type = data['type'] ?? '';

    return MessageModel(
      id: data['id']?.toString(),
      type: type,
      // v2.0: 从顶层读取字段
      msgType: data['msg_type']?.toString(),
      action: data['action']?.toString(),
      e2ee: _parseE2EE(data['e2ee']),
      payload: _parsePayload(data, type),
    );
  }

  Map<String, dynamic> toJson() {
    final type = this.type ?? '';

    return {
      'id': id,
      'type': type,
      // v2.0: 字段提升到顶层
      if (msgType != null) 'msg_type': msgType,
      if (action != null) 'action': action,
      if (e2ee != null && e2ee!.isNotEmpty) 'e2ee': e2ee,
      'payload': payload,
      // ... 其他字段
    };
  }

  static Map<String, dynamic>? _parseE2EE(dynamic data) {
    if (data == null || data == '') return null;
    if (data is String) return jsonDecode(data);
    return data as Map<String, dynamic>?;
  }

  static dynamic _parsePayload(Map<String, dynamic> data, String type) {
    final payload = data['payload'];
    // E2EE 消息的 payload 是字符串
    if (data['e2ee'] != null && data['e2ee'] != '') {
      return payload ?? '';
    }
    return payload;
  }
}
```

#### 1.2 修改消息 Repository

**文件**：`lib/store/repository/message_repo_sqlite.dart`

**原代码**：
```dart
class MessageRepoSqlite {
  static const messageTable = 'message';
  static const groupMessageTable = 'group_message';

  Future<void> insert(MessageModel msg) async {
    await db.insert(messageTable, msg.toJson());
  }
}
```

**修改为**：
```dart
class MessageRepoSqlite {
  // v2.0: 新表名
  static const c2cTable = 'msg_c2c';
  static const c2gTable = 'msg_c2g';
  static const c2sTable = 'msg_c2s';
  static const s2cTable = 'msg_s2c';

  // v2.0: 字段常量
  static const msgType = 'msg_type';
  static const action = 'action';
  static const e2ee = 'e2ee';

  /// 根据消息类型获取表名
  static String getTableName(String? type) {
    switch (type?.toUpperCase()) {
      case 'C2C': return c2cTable;
      case 'C2G': return c2gTable;
      case 'C2S': return c2sTable;
      case 'S2C': return s2cTable;
      default: return c2cTable;
    }
  }

  Future<void> insert(MessageModel msg) async {
    final table = getTableName(msg.type);
    final db = await SqliteService.to.db;

    await db.insert(
      table,
      msg.toJson(),
      conflictAlgorithm: ConflictAlgorithm.replace,
    );
  }

  /// 查询消息（多表联合）
  Future<List<MessageModel>> getMessages(String conversationId) async {
    final db = await SqliteService.to.db;
    final results = <MessageModel>[];

    // 从多个表查询
    for (final table in [c2cTable, c2gTable]) {
      final data = await db.query(
        table,
        where: 'conversation_uk3 = ?',
        whereArgs: [conversationId],
        orderBy: 'created_at DESC',
      );
      results.addAll(data.map((e) => MessageModel.fromJson(e)));
    }

    return results..sort((a, b) => b.createdAt!.compareTo(a.createdAt!));
  }
}
```

### 阶段 2：更新消息发送逻辑（2-3 天）

#### 2.1 修改消息发送

**文件**：`lib/service/message.dart`

**原代码**：
```dart
Future<void> sendTextMessage(String to, String content) async {
  final message = {
    'id': generateId(),
    'type': 'C2C',
    'from': userId,
    'to': to,
    'payload': {  // 旧：msg_type 在 payload 里
      'msg_type': 'text',
      'content': content,
    },
  };

  websocket.send(jsonEncode(message));
}
```

**修改为**：
```dart
Future<void> sendTextMessage(String to, String content) async {
  final now = DateTime.now().toUtc();

  final message = {
    'id': generateId(),
    'type': 'C2C',
    'from': userId,
    'to': to,
    // v2.0: 字段提升到顶层
    'msg_type': 'text',
    'action': '',
    'e2ee': '',
    'payload': {
      'content': content,
    },
    'created_at': now.toIso8601String(),
  };

  websocket.send(jsonEncode(message));

  // 保存到本地数据库
  await MessageRepoSqlite.insert(MessageModel.fromJson(message));
}
```

#### 2.2 修改 E2EE 消息发送

**原代码**：
```dart
Future<void> sendEncryptedMessage(String to, String content) async {
  final encrypted = await E2EEService.encrypt(content);
  final ciphertext = encrypted['ciphertext'];

  final message = {
    'type': 'C2C',
    'payload': {
      'msg_type': 'text',
      'content': ciphertext,  // 旧：密文在 content 里
    },
    'e2ee': encrypted['metadata'],
  };

  websocket.send(jsonEncode(message));
}
```

**修改为**：
```dart
Future<void> sendEncryptedMessage(String to, String content) async {
  final e2eeResult = await E2EEService.buildE2EEData(
    plaintext: content,
    recipients: await getRecipientDevices(to),
  );

  final now = DateTime.now().toUtc();
  final message = {
    'id': generateId(),
    'type': 'C2C',
    'from': userId,
    'to': to,
    'msg_type': 'text',
    'action': '',
    // v2.0: e2ee 只含元数据
    'e2ee': e2eeResult['e2ee'],
    // v2.0: payload 直接是密文字符串
    'payload': e2eeResult['ciphertext'],
    'created_at': now.toIso8601String(),
  };

  websocket.send(jsonEncode(message));
}
```

### 阶段 3：更新消息接收逻辑（2-3 天）

#### 3.1 修改 WebSocket 消息处理

**文件**：`lib/service/message_s2c.dart`

**原代码**：
```dart
void handleS2CMessage(Map<String, dynamic> data) {
  final payload = data['payload'];
  final msgType = payload['msg_type'];  // 旧：从 payload 读取
  final action = payload['action'];

  if (action == 'pull_offline_msg') {
    // 处理
  }
}
```

**修改为**：
```dart
void handleS2CMessage(Map<String, dynamic> data) {
  final type = data['type'] ?? '';

  if (type != 'S2C') return;

  // v2.0: 从顶层读取 action
  final action = data['action'] ?? '';
  final payload = data['payload'] ?? {};

  switch (action) {
    case 'pull_offline_msg':
      _handlePullOfflineMsg(payload);
      break;
    case 'please_refresh_token':
      _handleRefreshToken(payload);
      break;
    case 'logged_another_device':
      _handleLoggedAnotherDevice(payload);
      break;
    // ... 其他 action
  }
}

void _handlePullOfflineMsg(Map<String, dynamic> payload) {
  final count = payload['count'] ?? 0;
  // 拉取离线消息
}
```

#### 3.2 修改 C2C/C2G 消息接收

**文件**：`lib/service/message_s2c.dart`

**原代码**：
```dart
void handleChatMessage(Map<String, dynamic> data) {
  final payload = data['payload'];
  final msgType = payload['msg_type'];  // 旧：从 payload 读取

  switch (msgType) {
    case 'text':
      _handleTextMessage(payload);
      break;
    case 'image':
      _handleImageMessage(payload);
      break;
  }
}
```

**修改为**：
```dart
void handleChatMessage(Map<String, dynamic> data) {
  final type = data['type'] ?? '';

  // S2C 消息单独处理
  if (type == 'S2C') {
    handleS2CMessage(data);
    return;
  }

  // v2.0: 从顶层读取 msg_type
  final msgType = data['msg_type'] ?? '';
  final payload = data['payload'];
  final e2ee = data['e2ee'];

  // 处理 E2EE 消息
  if (e2ee != null && e2ee.isNotEmpty) {
    _handleE2EEMessage(data);
    return;
  }

  // 处理普通消息
  switch (msgType) {
    case 'text':
      _handleTextMessage(data);
      break;
    case 'image':
      _handleImageMessage(data);
      break;
    case 'voice':
      _handleVoiceMessage(data);
      break;
    case 'video':
      _handleVideoMessage(data);
      break;
    case 'file':
      _handleFileMessage(data);
      break;
    default:
      _handleUnknownMessage(data);
  }
}

Future<void> _handleE2EEMessage(Map<String, dynamic> data) async {
  final ciphertext = data['payload'] as String;
  final e2ee = data['e2ee'] as Map<String, dynamic>;

  // 解密消息
  final decrypted = await E2EEService.decryptE2EEMessage(
    ciphertext: ciphertext,
    e2ee: e2ee,
  );

  // 解析解密后的内容
  final content = jsonDecode(decrypted);
  // 显示消息
}
```

### 阶段 4：更新 UI 组件（2-3 天）

#### 4.1 修改聊天页面消息渲染

**文件**：`lib/page/chat/chat/chat_page.dart`

**原代码**：
```dart
Widget buildMessageItem(MessageModel msg) {
  final payload = msg.payload!;
  final msgType = payload['msg_type'];  // 旧：从 payload 读取

  switch (msgType) {
    case 'text':
      return TextWidget(content: payload['content']);
    // ...
  }
}
```

**修改为**：
```dart
Widget buildMessageItem(MessageModel msg) {
  // v2.0: 直接从模型读取 msg_type
  final msgType = msg.msgType ?? 'unknown';

  switch (msgType) {
    case 'text':
      return TextWidget(message: msg);
    case 'image':
      return ImageWidget(message: msg);
    case 'voice':
      return VoiceWidget(message: msg);
    case 'video':
      return VideoWidget(message: msg);
    case 'file':
      return FileWidget(message: msg);
    default:
      return UnknownWidget(message: msg);
  }
}
```

#### 4.2 修改文本消息组件

**文件**：`lib/page/chat/widget/text_message_widget.dart`

**原代码**：
```dart
class TextMessageWidget extends StatelessWidget {
  final Map<String, dynamic> payload;

  @override
  Widget build(BuildContext context) {
    final content = payload['content'];  // 旧：从 payload 读取
    return Text(content);
  }
}
```

**修改为**：
```dart
class TextMessageWidget extends StatelessWidget {
  final MessageModel message;

  @override
  Widget build(BuildContext context) {
    final payload = message.payload;
    final content = payload is Map ? payload['content'] : payload;

    // 处理 E2EE 消息
    if (message.e2ee != null && message.e2ee!.isNotEmpty) {
      return _buildEncryptedContent();
    }

    return Text(content ?? '');
  }

  Widget _buildEncryptedContent() {
    // 显示加密提示或解密后的内容
    return Consumer(
      builder: (context, ref, child) {
        final decrypted = ref.watch(encryptedMessageProvider(message.id));
        return Text(decrypted ?? '🔒 加密消息');
      },
    );
  }
}
```

---

## 后端迁移步骤

### 阶段 1：更新消息编码（2-3 天）

#### 1.1 修改 WebSocket 消息编码

**文件**：`imboy_ws.erl`

**原代码**：
```erlang
encode_message(Msg) ->
    Payload = maps:get(<<"payload">>, Msg, #{}),
    MsgType = maps:get(<<"msg_type">>, Payload, <<>>),
    Action = maps:get(<<"action">>, Payload, <<>>),

    #{
        <<"id">> => maps:get(<<"id">>, Msg),
        <<"type">> => maps:get(<<"type">>, Msg),
        <<"payload">> => Payload
    }.
```

**修改为**：
```erlang
%% 编码 v2.0 WebSocket 消息
encode_message(Msg) ->
    Type = maps:get(<<"type">>, Msg),

    %% v2.0: 根据 Type 决定字段
    {MsgType, Action, E2EE} = case Type of
        <<"S2C">> ->
            %% S2C: action 必须有，msg_type 预留
            {
                maps:get(<<"msg_type">>, Msg, <<>>),
                maps:get(<<"action">>, Msg),
                <<>>
            };
        _ ->
            %% 非 S2C: msg_type 必须有，action 为空
            {
                maps:get(<<"msg_type">>, Msg),
                <<>>,
                maps:get(<<"e2ee">>, Msg, <<>>)
            }
    end,

    #{
        <<"id">> => maps:get(<<"id">>, Msg),
        <<"type">> => Type,
        <<"from">> => maps:get(<<"from_id">>, Msg),
        <<"to">> => maps:get(<<"to_id">>, Msg),
        <<"msg_type">> => MsgType,
        <<"action">> => Action,
        <<"e2ee">> => E2EE,
        <<"payload">> => maps:get(<<"payload_content">>, Msg, #{}),
        <<"server_ts">> => elib_dt:milliseconds()
    }.
```

#### 1.2 修改消息解码

**文件**：`imboy_ws.erl`

**原代码**：
```erlang
decode_message(Data) ->
    Msg = jsx:decode(Data, [return_maps]),
    Payload = maps:get(<<"payload">>, Msg, #{}),
    MsgType = maps:get(<<"msg_type">>, Payload, <<>>),

    %% 转换为内部格式
    #{
        <<"type">> => maps:get(<<"type">>, Msg),
        <<"msg_type">> => MsgType,
        <<"payload_content">> => Payload
    }.
```

**修改为**：
```erlang
%% 解码 v2.0 WebSocket 消息
decode_message(Data) ->
    Msg = jsx:decode(Data, [return_maps]),
    Type = maps:get(<<"type">>, Msg, <<>>),

    %% v2.0: 根据 Type 读取字段
    {MsgType, Action, E2EE} = case Type of
        <<"S2C">> ->
            {
                maps:get(<<"msg_type">>, Msg, <<>>),
                maps:get(<<"action">>, Msg),
                <<>>
            };
        _ ->
            {
                maps:get(<<"msg_type">>, Msg),
                <<>>,
                maps:get(<<"e2ee">>, Msg, <<>>)
            }
    end,

    %% 转换为内部格式
    #{
        <<"id">> => maps:get(<<"id">>, Msg),
        <<"type">> => Type,
        <<"from_id">> => maps:get(<<"from">>, Msg),
        <<"to_id">> => maps:get(<<"to">>, Msg),
        <<"msg_type">> => MsgType,
        <<"action">> => Action,
        <<"e2ee">> => E2EE,
        <<"payload_content">> => maps:get(<<"payload">>, Msg, #{}),
        <<"created_at">> => maps:get(<<"created_at">>, Msg, elib_dt:milliseconds())
    }.
```

### 阶段 2：更新数据库存储（3-4 天）

#### 2.1 修改消息插入逻辑

**文件**：`imboy_message_repo.erl`

**原代码**：
```erlang
insert_message(Msg) ->
    Sql = "INSERT INTO message (id, type, payload, created_at) VALUES ($1, $2, $3, $4)",
    elib_pg:query(Sql, [
        maps:get(<<"id">>, Msg),
        maps:get(<<"type">>, Msg),
        jsx:encode(maps:get(<<"payload_content">>, Msg)),
        elib_dt:milliseconds()
    ]).
```

**修改为**：
```erlang
%% 插入 v2.0 消息到数据库
insert_message(Msg) ->
    Type = maps:get(<<"type">>, Msg),
    TableName = get_table_name(Type),

    %% v2.0: 根据 Type 插入对应字段
    case Type of
        <<"S2C">> ->
            %% S2C: action 必须有
            Sql = io_lib:format(
                "INSERT INTO ~s (id, action, msg_type, from_id, to_id, payload, created_at) "
                "VALUES ($1, $2, $3, $4, $5, $6, $7)",
                [TableName]
            ),
            elib_pg:query(Sql, [
                maps:get(<<"id">>, Msg),
                maps:get(<<"action">>, Msg),
                maps:get(<<"msg_type">>, Msg, <<>>),
                maps:get(<<"from_id">>, Msg, <<>>),
                maps:get(<<"to_id">>, Msg),
                jsx:encode(maps:get(<<"payload_content">>, Msg, #{})),
                maps:get(<<"created_at">>, Msg, elib_dt:milliseconds())
            ]);
        _ ->
            %% 非 S2C: msg_type 必须有
            Sql = io_lib:format(
                "INSERT INTO ~s (id, msg_type, e2ee, from_id, to_id, payload, created_at) "
                "VALUES ($1, $2, $3, $4, $5, $6, $7)",
                [TableName]
            ),
            E2EE = case maps:get(<<"e2ee">>, Msg, #{}) of
                #{<<"e2ee">> := true} -> jsx:encode(maps:get(<<"e2ee">>, Msg));
                _ -> <<>>
            end,
            elib_pg:query(Sql, [
                maps:get(<<"id">>, Msg),
                maps:get(<<"msg_type">>, Msg),
                E2EE,
                maps:get(<<"from_id">>, Msg),
                maps:get(<<"to_id">>, Msg),
                jsx:encode(maps:get(<<"payload_content">>, Msg, #{})),
                maps:get(<<"created_at">>, Msg, elib_dt:milliseconds())
            ])
    end.

get_table_name(<<"C2C">>) -> <<"msg_c2c">>;
get_table_name(<<"C2G">>) -> <<"msg_c2g">>;
get_table_name(<<"C2S">>) -> <<"msg_c2s">>;
get_table_name(<<"S2C">>) -> <<"msg_s2c">>.
```

#### 2.2 修改消息查询逻辑

**文件**：`imboy_message_repo.erl`

**原代码**：
```erlang
get_messages(ConversationId, Limit, Offset) ->
    Sql = "SELECT * FROM message WHERE conversation_id = $1 ORDER BY created_at DESC LIMIT $2 OFFSET $3",
    elib_pg:query(Sql, [ConversationId, Limit, Offset]).
```

**修改为**：
```erlang
%% v2.0: 从多个表查询消息
get_messages(ConversationId, Limit, Offset) ->
    %% 从 C2C 和 C2G 表查询
    C2CSql = "
        SELECT id, msg_type, e2ee, from_id, to_id, payload, created_at
        FROM msg_c2c
        WHERE conversation_uk3 = $1
        ORDER BY created_at DESC
        LIMIT $2 OFFSET $3
    ",
    C2GSql = "
        SELECT id, msg_type, e2ee, from_id, to_id, payload, created_at
        FROM msg_c2g
        WHERE conversation_uk3 = $1
        ORDER BY created_at DESC
        LIMIT $2 OFFSET $3
    ",

    C2CResults = elib_pg:query(C2CSql, [ConversationId, Limit, Offset]),
    C2GResults = elib_pg:query(C2GSql, [ConversationId, Limit, Offset]),

    %% 合并结果并转换为 v2.0 格式
    lists:map(fun(Row) ->
        #{
            <<"id">> => maps:get(<<"id">>, Row),
            <<"type">> => get_type_from_table(Row),
            <<"msg_type">> => maps:get(<<"msg_type">>, Row),
            <<"action">> => <<>>,
            <<"e2ee">> => maps:get(<<"e2ee">>, Row, <<>>),
            <<"payload">> => maps:get(<<"payload">>, Row),
            <<"server_ts">> => maps:get(<<"created_at">>, Row)
        }
    end, C2CResults ++ C2GResults).

get_type_from_table(#{table_name := <<"msg_c2c">>}) -> <<"C2C">>;
get_type_from_table(#{table_name := <<"msg_c2g">>}) -> <<"C2G">>;
get_type_from_table(#{table_name := <<"msg_c2s">>}) -> <<"C2S">>;
get_type_from_table(#{table_name := <<"msg_s2c">>}) -> <<"S2C">>.
```

---

## 验证测试

### 前端测试

#### 1. 单元测试

**文件**：`test/model/message_model_test.dart`

```dart
void main() {
  group('MessageModel v2.0 Tests', () {
    test('parse C2C message with msg_type', () {
      final json = {
        'id': 'msg_123',
        'type': 'C2C',
        'msg_type': 'text',
        'action': '',
        'e2ee': '',
        'payload': {'content': 'Hello'},
      };

      final msg = MessageModel.fromJson(json);

      expect(msg.type, 'C2C');
      expect(msg.msgType, 'text');
      expect(msg.action, '');
      expect(msg.payload, isA<Map<String, dynamic>>());
    });

    test('parse S2C message with action', () {
      final json = {
        'id': 's2c_123',
        'type': 'S2C',
        'msg_type': '',
        'action': 'pull_offline_msg',
        'e2ee': '',
        'payload': {'count': 5},
      };

      final msg = MessageModel.fromJson(json);

      expect(msg.type, 'S2C');
      expect(msg.action, 'pull_offline_msg');
    });

    test('parse E2EE message with string payload', () {
      final json = {
        'id': 'msg_456',
        'type': 'C2C',
        'msg_type': 'text',
        'e2ee': {'e2ee': true, 'nonce': 'abc123'},
        'payload': 'base64_encoded_ciphertext',
      };

      final msg = MessageModel.fromJson(json);

      expect(msg.e2ee, isNotNull);
      expect(msg.payload, isA<String>());
    });
  });
}
```

#### 2. 集成测试

```dart
void main() {
  group('WebSocket v2.0 Integration Tests', () {
    testWidgets('send and receive C2C message', (tester) async {
      // 发送消息
      await MessageService.to.sendTextMessage(
        to: 'test_user',
        content: 'Test message',
      );

      // 等待响应
      await tester.pump(Duration(seconds: 2));

      // 验证消息
      final messages = await MessageRepoSqlite.getMessages('conversation_id');
      expect(messages.isNotEmpty, true);
      expect(messages.first.msgType, 'text');
      expect(messages.first.action, '');
    });

    testWidgets('receive and process S2C message', (tester) async {
      // 模拟接收 S2C 消息
      final s2cMsg = {
        'id': 's2c_test',
        'type': 'S2C',
        'action': 'pull_offline_msg',
        'msg_type': '',
        'payload': {'count': 3},
      };

      // 处理消息
      MessageS2CService.switchS2C(s2cMsg);

      // 验证处理逻辑
      // ...
    });
  });
}
```

### 后端测试

```erlang
%% 消息编码测试
encode_message_v2_test_() ->
    [
        ?_assertEqual(
            <<"text">>,
            maps:get(<<"msg_type">>, imboy_ws:encode_message(#{
                <<"type">> => <<"C2C">>,
                <<"msg_type">> => <<"text">>,
                <<"payload_content">> => #{<<"content">> => <<"Hello">>}
            }))
        ),
        ?_assertEqual(
            <<>>,
            maps:get(<<"msg_type">>, imboy_ws:encode_message(#{
                <<"type">> => <<"S2C">>,
                <<"action">> => <<"pull_offline_msg">>,
                <<"payload_content">> => #{}
            }))
        )
    ].

%% 消息解码测试
decode_message_v2_test_() ->
    [
        ?_assertEqual(
            <<"text">>,
            maps:get(<<"msg_type">>, imboy_ws:decode_message(<<
                "{\"type\":\"C2C\",\"msg_type\":\"text\",\"payload\":{}}"
            >>))
        ),
        ?_assertEqual(
            <<"pull_offline_msg">>,
            maps:get(<<"action">>, imboy_ws:decode_message(<<
                "{\"type\":\"S2C\",\"action\":\"pull_offline_msg\",\"payload\":{}}"
            >>))
        )
    ].
```

---

## 时间计划

| 阶段 | 任务 | 预计时间 | 负责人 |
|------|------|----------|--------|
| **前端** | | | |
| 1 | 更新消息模型 | 2-3 天 | 前端开发 |
| 2 | 更新消息发送逻辑 | 2-3 天 | 前端开发 |
| 3 | 更新消息接收逻辑 | 2-3 天 | 前端开发 |
| 4 | 更新 UI 组件 | 2-3 天 | 前端开发 |
| 5 | 测试和调试 | 3-4 天 | 前端开发 + QA |
| **后端** | | | |
| 1 | 更新消息编码/解码 | 2-3 天 | 后端开发 |
| 2 | 更新数据库存储 | 3-4 天 | 后端开发 |
| 3 | 测试和调试 | 3-4 天 | 后端开发 + QA |
| **联调** | | | |
| 1 | 前后端联调 | 3-5 天 | 全体 |
| 2 | 性能测试 | 2-3 天 | QA |
| 3 | 上线准备 | 1-2 天 | 运维 |

**总计**：约 30-35 个工作日

---

## 检查清单

### 前端上线前检查

- [ ] MessageModel 正确处理 msg_type/action/e2ee 字段
- [ ] E2EE 消息 payload 为字符串格式
- [ ] WebSocket 消息发送使用 v2.0 格式
- [ ] WebSocket 消息接收使用 v2.0 格式
- [ ] 数据库表名使用 msg_c2c/msg_c2g/msg_c2s/msg_s2c
- [ ] 所有单元测试通过
- [ ] 集成测试通过

### 后端上线前检查

- [ ] 消息编码/解码使用 v2.0 格式
- [ ] 数据库存储使用新表结构
- [ ] 所有单元测试通过
- [ ] 集成测试通过
- [ ] 性能测试通过

### 联调检查

- [ ] 前后端消息格式一致
- [ ] E2EE 加解密正常
- [ ] 所有消息类型可正常发送和接收
- [ ] S2C 消息正确处理
- [ ] 离线消息正常
- [ ] 实时消息正常

---

**相关文档**：
- [WebSocket API v2.0 规范](./websocket-api-2.md)
- [数据库表结构优化](./websocket-api-2-table-structure-optimization.md)
