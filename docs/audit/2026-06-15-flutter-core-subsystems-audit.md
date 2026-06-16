# imboy Flutter 核心子系统深度审计报告

**审计日期**：2026-06-15  
**审计范围**：4 个核心子系统 + NotificationService 与 WebSocket S2C 收发链路  
**审计方法**：静态代码分析、执行路径追踪、并发场景推演  
**严重度标记**：🔴 CRITICAL / 🟠 HIGH / 🟡 MEDIUM / 🟢 LOW / ✅ SAFE

---

## 目录

1. [子系统一：会话管理](#subsystem-1)
2. [子系统二：单聊与 E2EE](#subsystem-2)
3. [子系统三：群聊（频道语音 + 日程）](#subsystem-3)
4. [子系统四：群管理与 ACL](#subsystem-4)
5. [周边链路：NotificationService](#notification)
6. [周边链路：WebSocket S2C 收发链路](#s2c)
7. [技术债汇总矩阵](#debt-matrix)
8. [修复补丁](#patches)
9. [优先级矩阵与修复顺序](#priority)

---

<a id="subsystem-1"></a>
## 一、子系统一：会话管理

### BUG-01 🔴 CRITICAL — `save()` 无事务 TOCTOU 竞态，`unreadNum` / `mentionUnread` 静默丢失

**文件**：`imboyapp/lib/store/repository/conversation_repo_sqlite.dart:126-146`

**问题代码**：

```dart
Future<ConversationModel> save(ConversationModel obj) async {
  ConversationModel? oldObj = await findByPeerId(obj.type, obj.peerId.toString()); // 1. 读
  int unreadNumOld = oldObj == null ? 0 : oldObj.unreadNum;
  int mentionUnreadOld = oldObj == null ? 0 : oldObj.mentionUnread;
  obj.unreadNum = obj.unreadNum + unreadNumOld;       // 2. 在 Dart 内存中计算
  obj.mentionUnread = obj.mentionUnread + mentionUnreadOld;
  if (oldObj == null) {
    obj.id = await insert(obj);                        // 3. 写
  } else {
    await updateById(oldObj.id, data);                 // 3. 写
  }
  return obj;
}
```

**并发场景推演**：
- WebSocket Isolate-A 处理消息 M1：读到 `unreadNum=0`
- WebSocket Isolate-B 处理消息 M2：也读到 `unreadNum=0`（A 尚未写入）
- 两者各自写入 `1`，最终 `unreadNum=1`，实际应为 `2`

**根因**：`_db.transaction()` 才能在 sqflite 内提供行级串行化。不用事务则整个 READ-MODIFY-WRITE 序列对其他 Isolate 可见，属于经典检查时间/使用时间（TOCTOU）竞态。

**影响**：会话角标永久性少计，且无任何错误抛出，完全静默。每次 `save()` 并发调用都会丢失计数。

---

### BUG-02 🔴 CRITICAL — `list()` / `search()` 漏读列，DND 与 @ 未读静默归零

**文件**：`imboyapp/lib/store/repository/conversation_repo_sqlite.dart:153-185, 205-230`

**问题代码**（两处均缺少 `mention_unread` 和 `is_muted`）：

```dart
// list()
List<Map<String, dynamic>> maps = await _db.query(
  tableName,
  columns: ['id', 'peer_id', 'avatar', 'title', 'subtitle', 'type',
             'msg_type', 'last_msg_id', 'last_time', 'unread_num',
             'payload', 'is_show', 'is_top', 'created_at', 'updated_at'],
  // 缺少 mention_unread 和 is_muted
);
```

**后果**：

| 丢失字段 | 影响 |
|---------|------|
| `is_muted` | `ConversationModel.fromJson()` 默认 `0`，用户设置的 DND 被重置；通知抑制逻辑误判 |
| `mention_unread` | 群 @ 角标丢失，用户无法感知被点名 |

**触发时机**：每次打开会话列表页（`list()`）或搜索会话（`search()`）时。

---

### BUG-03 🟡 MEDIUM — `ConversationModel.empty()` 类型字面量错误

**文件**：`imboyapp/lib/store/model/conversation_model.dart`

`ConversationModel.empty()` 中 `subtitle`、`lastTime` 字段应为 `String`，但工厂构造中使用了 `int` 字面量 `0`。Dart 运行时会将其以 `dynamic` 存储，但在 `parseModelString()` 强类型路径下会抛出 `TypeError`。

---

<a id="subsystem-2"></a>
## 二、子系统二：单聊与 E2EE

### BUG-04 🔴 CRITICAL — E2EE 重试键名不匹配，重试机制 100% 静默失效

**文件**：`imboyapp/lib/service/e2ee_health_check_service.dart:~606`  
**对照**：`imboyapp/lib/service/e2ee_service.dart`（`_decryptFailedPayload` 构造）

`decryptIncomingPayload()` 失败时将原始密文存入嵌套 Map 键 `_e2ee_raw`：

```dart
// e2ee_service.dart 中 _decryptFailedPayload 返回结构
{
  '_e2ee_failed': true,
  '_e2ee_raw': {          // 嵌套 Map
    'payload': originalCiphertext,
    'e2ee': originalE2EEMeta,
  },
}
```

但 `_retryDecryptMessage()` 查找顶层平铺键 `_e2ee_raw_ciphertext`（旧格式）：

```dart
// e2ee_health_check_service.dart:~606
final rawCiphertext = payload['_e2ee_raw_ciphertext']; // 旧格式键
if (rawCiphertext == null) return;  // 新格式消息 100% 在此早退
```

**后果**：`retryFailedMessages()` 每次扫描数据库中所有 `_e2ee_failed=true` 的消息，每条都在 `rawCiphertext == null` 处提前返回，等同于重试机制自格式升级后完全失效。用户看到的 `[解密失败]` 消息永远不会自动恢复。

---

### BUG-05 🔴 CRITICAL — `_refreshE2EEKeys()` 是空操作，"重建密钥"按钮无效

**文件**：`imboyapp/lib/page/chat/chat/chat_page.dart:2100-2127`

当用户在 E2EE 密钥不匹配对话框中点击"重新创建密钥"，实际执行：

```dart
Future<void> _refreshE2EEKeys() async {
  E2EEService.clearCache();                          // 1. 清内存缓存（仅本进程）
  await StorageService.to.remove('e2ee_key_refresh_time');
  await E2EEService.getUserDevicePublicKeys(currentUid); // 2. 拉取本人已有公钥
  // 注意：拉取本人已有公钥，不是生成新密钥对
  EasyLoading.showSuccess(t.chat.e2eeKeyRecreated);  // 3. 欺骗性成功提示
}
```

代码注释写 `"这里我们只需清理缓存，下次使用时会自动生成新的密钥对"` — 这是错误预设：
- RSA 密钥对不会自动重生成，必须显式调用 `RSAService.generateKeyPair()` 并上传公钥 API
- 用户收到成功 Toast，实际密钥未变，下次 E2EE 解密仍然失败
- 用户被欺骗性提示误导为"已修复"

---

### BUG-06 🟠 HIGH — E2EE 通知游标在同步失败时跳过，失效密钥永不重试

**文件**：`imboyapp/lib/service/e2ee_health_check_service.dart:174-180`

`pullKeyNotifications()` 内的游标 `maxUpdatedAt` 在循环末尾更新。若 `syncFriendPublicKey(peerUid)` 中途抛出异常（catch 后 continue），该通知条目的 `updated_at` 仍被并入游标——下次轮询时该通知已被跳过，失效密钥的重新同步被永久忽略。

**修复方向**：仅在成功同步后推进游标，失败条目记入单独的重试队列。

---

### BUG-07 🟠 HIGH — E2EE 缓存遮蔽密钥吊销（最长 30 分钟）

**文件**：`imboyapp/lib/service/e2ee_service.dart:541-548`

```dart
if (forceRefresh) {
  keys = await _fetchUserDevicePublicKeys(uid);
  if (keys.isEmpty) {
    keys = _userKeyCacheByDevice[uid] ?? []; // 空列表 fallback 到旧缓存
  }
}
```

**场景**：用户登出全部设备后，后端删除所有公钥。其他设备因 `forceRefresh` 拿到空列表后回落旧缓存，最长 30 分钟内仍可加密发消息给"已注销"设备，违反最小权限原则。

---

### BUG-08 🟠 HIGH — `save()` 不传递 `isMuted`，DND 通知抑制可能失效

**文件**：`imboyapp/lib/store/repository/conversation_repo_sqlite.dart:126-146`  
**关联**：`imboyapp/lib/service/message.dart:909-916`

`message.dart:914` 用 `savedConv.isMuted` 决定是否抑制通知：

```dart
// message.dart:909-916
if (!isFromCurrentUser &&
    !isUserInChat &&
    !shouldSuppressNotification(
      isMuted: savedConv.isMuted,  // 来自 save() 返回值
      isMentioned: mentionIncrement > 0,
    )) {
  _showMessageNotification(...);
}
```

`save()` 从 `oldObj` 读取 `unreadNum`/`mentionUnread` 进行累加，但从未将 `oldObj.isMuted` 写回返回的 `obj`。若 S2C 消息构造的 `ConversationModel` 未预填 `isMuted`（用户的 DND 偏好不在消息体内），则 `savedConv.isMuted=0`，DND 通知抑制永远不生效。

---

<a id="subsystem-3"></a>
## 三、子系统三：群聊

### BUG-09 🟡 MEDIUM — GroupSchedule 本地注入 + 服务端回显产生重复消息

**文件**：`imboyapp/lib/page/group/schedule/group_schedule_page.dart`

`_createSchedule()` 成功后：
1. 本地注入 `CustomMessage`（ID 为 `Xid().toString()`，由客户端生成）
2. 服务端 S2C 推送真实消息（ID 为后端分配的 TSID，不同）

两条消息 ID 不同，`msgIds` 去重 Set 无法过滤，UI 出现两条"日程"卡片。**无无限循环风险**，但重复卡片破坏用户体验。

---

### SAFE-01 — `_ChannelAudioPlayer` 缓存路径安全

**文件**：`imboyapp/lib/page/channel/channel_message_item.dart:757-761`

```dart
final file = await IMBoyCacheManager().getSingleFile(
  widget.uri,
  validateImageData: false,
);
```

`IMBoyCacheManager.getSingleFile()` 内部调用 `AssetsService.viewUrl()` 重授权；URL 已通过 `Uri.encodeComponent` 编码，无路径注入风险。**此处无问题。**

---

### SAFE-02 — 输入法打字指示符正确节流

**文件**：`imboyapp/lib/page/chat/chat/chat_page.dart:1195-1224`

`_handleInputChanged()` 使用 `decideTypingIndicator()` 配合 5 秒 idle Timer 正确节流，不存在 WebSocket 泛洪。

---

<a id="subsystem-4"></a>
## 四、子系统四：群管理与 ACL

### BUG-10 🟠 HIGH — 被禁言用户可通过图片/语音/贴纸绕过禁言

**文件**：`imboyapp/lib/page/chat/chat/chat_page.dart:963-1085`

`_handleSendPressed`（文字发送）正确检查 `SendDenyMuted`，但以下媒体入口均无 `_isMuted` 检查：

| 方法 | 行范围 | 媒体类型 |
|------|--------|---------|
| `_handleImageSelection` | ~971 | 图片/视频 |
| `_handleVoiceSelection` | ~963 | 语音消息 |
| `_handleStickerSelection` | ~1070 | 贴纸/Emoji |
| 位置/文件选择器 | ~1085 | 位置/文件附件 |

被管理员禁言的用户只要绕过文字输入框，均可正常发送多媒体消息。

---

### SAFE-03 — `GroupMemberPage` 订阅无内存泄漏

**文件**：`imboyapp/lib/page/group/group_member/group_member_page.dart:103-110`

`_ssMemberMute` / `_ssMemberUnmute` 均在 `dispose()` 中显式 `.cancel()`，无泄漏。

---

### BUG-11 🟢 LOW — `GroupMemberPage` 原地修改 Model 对象

**文件**：`imboyapp/lib/page/group/group_member/group_member_page.dart:75`

```dart
_memberList[idx].muteUntilMs = event.muteUntilMs; // 原地修改
```

违反项目不可变性约定。若多处持有相同 `GroupMemberModel` 引用，会产生幽灵状态更新。功能上目前可用，属于架构债。

---

<a id="notification"></a>
## 五、周边链路：NotificationService

### 观察 N-01 — DND 路由逻辑设计正确，但依赖链断裂

**文件**：`imboyapp/lib/service/message.dart:909-916`  
**关联**：BUG-02、BUG-08

NotificationService 本身设计正确：通知 payload 解析使用 sealed class（`NotificationParseResult`），路由判断为纯函数（`parseNotificationPayload`），零外部依赖，易于测试。

DND 抑制逻辑（`shouldSuppressNotification(isMuted: savedConv.isMuted)`）的设计意图也正确，支持"@ 穿透抑制"（即使 DND 开启，被 @ 时仍发通知）。

**但依赖链在两处断裂**：
1. `list()` 漏读 `is_muted`（BUG-02）→ UI 层看到的 `isMuted` 永远为 0
2. `save()` 不传递 `isMuted` 到返回值（BUG-08）→ 通知抑制判断用的 `savedConv.isMuted` 可能为 0

设计正确但执行链条有漏洞，等同于 DND 功能无效。

---

### 观察 N-02 🟡 MEDIUM — 通知点击路由无 `peerId` 合法性校验

**文件**：`imboyapp/lib/service/notification.dart:107-118`

```dart
case NotificationMessageRoute(:final peerId, :final chatType):
  _navigateToChat(context, peerId, chatType);  // 未校验 peerId 格式
```

`peerId` 来自推送 payload，若推送服务被篡改（MITM、恶意第三方推送），任意字符串都会被传入路由。应在进入路由前校验 `peerId` 为合法 TSID 格式（纯数字字符串）。

---

<a id="s2c"></a>
## 六、周边链路：WebSocket S2C 收发链路

### 观察 S-01 — S2C dispatch 表结构合理，DND 判断在正确层级

**文件**：`imboyapp/lib/service/message_s2c.dart:87-280`

`switchS2C()` 采用 `switch(action.toLowerCase())` 分发，共 30+ 个 action，结构清晰。DND 逻辑未污染 S2C 层，正确下沉到 `MessageService`，符合单一职责原则。

---

### 观察 S-02 🟡 MEDIUM — ACK 在处理完成后发送，处理失败时 ACK 仍会送出

**文件**：`imboyapp/lib/service/message_s2c.dart:272-276`

```dart
} on Object catch (e, s) {
  iPrint("switchS2C error: $e, $s");
  // autoAck 仍为 true，catch 后继续执行下方 ACK
}
if (autoAck) {
  AckManager.to.sendAckDirect('S2C', msgId as String);
}
```

即使某条 S2C 消息处理失败（如写 SQLite 失败），ACK 依然发送，服务端认为消息已成功投递，不会重推。若写 DB 失败属于偶发（磁盘空间不足、加密错误），消息无法恢复，用户侧静默丢消息。

---

### 观察 S-03 🟡 MEDIUM — `e2ee_device_key_changed` S2C 缓存清理待验证

**文件**：`imboyapp/lib/service/message_s2c.dart:190-193`

`_handleE2EEDeviceKeyChanged()` 应清除对应 `uid` 的 E2EE 公钥缓存（`_userKeyCacheByDevice[uid]`）并触发重拉。若实现不完整，密钥变更通知到达后仍使用旧公钥加密，导致对端无法解密。需审查 `_handleE2EEDeviceKeyChanged()` 的具体实现。

---

<a id="debt-matrix"></a>
## 七、技术债汇总矩阵

| ID | 文件:行 | 类型 | 严重度 | 一句话描述 |
|----|---------|------|--------|-----------|
| BUG-01 | `conversation_repo_sqlite.dart:126` | 并发/事务 | 🔴 CRITICAL | save() 无事务，unreadNum 竞态丢失 |
| BUG-02 | `conversation_repo_sqlite.dart:205` | 接口不一致 | 🔴 CRITICAL | list/search 漏读 is_muted/mention_unread |
| BUG-04 | `e2ee_health_check_service.dart:606` | 键名不匹配 | 🔴 CRITICAL | E2EE 重试键名错误，重试永久失效 |
| BUG-05 | `chat_page.dart:2100` | 功能缺失 | 🔴 CRITICAL | _refreshE2EEKeys() 是空操作，欺骗性 Toast |
| BUG-06 | `e2ee_health_check_service.dart:174` | 游标设计 | 🟠 HIGH | 失败密钥同步被游标永久跳过 |
| BUG-07 | `e2ee_service.dart:541` | 安全设计 | 🟠 HIGH | forceRefresh 空列表 fallback，密钥吊销延迟 30 分钟 |
| BUG-08 | `conversation_repo_sqlite.dart:126` | 字段传递 | 🟠 HIGH | save() 不传递 isMuted，DND 通知抑制失效 |
| BUG-10 | `chat_page.dart:963` | ACL 绕过 | 🟠 HIGH | 媒体发送路径不检查 _isMuted，禁言形同虚设 |
| S-02 | `message_s2c.dart:272` | 可靠性 | 🟡 MEDIUM | 处理异常后仍 ACK，失败消息静默丢失 |
| N-02 | `notification.dart:107` | 安全 | 🟡 MEDIUM | 通知路由 peerId 无格式校验 |
| S-03 | `message_s2c.dart:190` | E2EE | 🟡 MEDIUM | e2ee_device_key_changed 缓存清理实现待验证 |
| BUG-09 | `group_schedule_page.dart` | UX | 🟡 MEDIUM | 本地注入 + S2C 回显产生重复消息卡片 |
| BUG-03 | `conversation_model.dart` | 类型 | 🟡 MEDIUM | empty() 工厂使用错误类型字面量 |
| BUG-11 | `group_member_page.dart:75` | 可变性 | 🟢 LOW | 原地修改 Model，违反不可变性约定 |

---

<a id="patches"></a>
## 八、修复补丁

### Patch-01：`save()` 加事务，同时修复 BUG-01 与 BUG-08

```dart
// conversation_repo_sqlite.dart — 替换整个 save() 方法
Future<ConversationModel> save(ConversationModel obj) async {
  return await _db.transaction((txn) async {
    final List<Map<String, dynamic>> rows = await txn.query(
      tableName,
      columns: ['id', 'unread_num', 'mention_unread', 'is_muted'],
      where: 'type = ? AND peer_id = ?',
      whereArgs: [obj.type, obj.peerId.toString()],
      limit: 1,
    );

    if (rows.isEmpty) {
      obj.id = await txn.insert(
        tableName,
        obj.toJson(),
        conflictAlgorithm: ConflictAlgorithm.replace,
      );
    } else {
      final row = rows.first;
      final int oldUnread = (row['unread_num'] as int?) ?? 0;
      final int oldMention = (row['mention_unread'] as int?) ?? 0;
      final int storedIsMuted = (row['is_muted'] as int?) ?? 0;

      final Map<String, dynamic> data = obj.toJson();
      data['unread_num'] = (data['unread_num'] as int) + oldUnread;
      data['mention_unread'] = ((data['mention_unread'] as int?) ?? 0) + oldMention;
      // 保留用户设置的 DND 状态，不被 S2C 消息覆盖
      if (obj.isMuted == 0) {
        data['is_muted'] = storedIsMuted;
      }
      data['updated_at'] = DateTime.now().millisecondsSinceEpoch;

      obj.id = (row['id'] as int?) ?? 0;
      obj.unreadNum = data['unread_num'] as int;
      obj.mentionUnread = data['mention_unread'] as int;
      obj.isMuted = data['is_muted'] as int;

      await txn.update(
        tableName,
        data,
        where: 'id = ?',
        whereArgs: [obj.id],
      );
    }
    return obj;
  });
}
```

---

### Patch-02：`list()` / `search()` 补全缺失列（BUG-02）

```dart
// conversation_repo_sqlite.dart — list() 和 search() 的 columns 列表（两处均需修改）
// 修改前：
columns: ['id', 'peer_id', 'avatar', 'title', 'subtitle', 'type',
           'msg_type', 'last_msg_id', 'last_time', 'unread_num',
           'payload', 'is_show', 'is_top', 'created_at', 'updated_at'],

// 修改后：
columns: [
  'id', 'peer_id', 'avatar', 'title', 'subtitle', 'type',
  'msg_type', 'last_msg_id', 'last_time', 'unread_num',
  'payload', 'is_show', 'is_top', 'created_at', 'updated_at',
  'mention_unread',  // 补充
  'is_muted',        // 补充
],
```

---

### Patch-03：修复 E2EE 重试键名不匹配（BUG-04）

```dart
// e2ee_health_check_service.dart — _retryDecryptMessage() 内，替换第~606行
// 修改前：
final rawCiphertext = payload['_e2ee_raw_ciphertext'];
if (rawCiphertext == null) return;

// 修改后：兼容新旧两种格式
final dynamic rawE2EE = payload['_e2ee_raw'];
final rawCiphertext = payload['_e2ee_raw_ciphertext']       // 旧格式（兼容保留）
    ?? (rawE2EE is Map ? rawE2EE['payload'] : null);        // 新格式
if (rawCiphertext == null) return;

// e2ee 元数据读取路径同步升级
final rawE2EEMeta = rawE2EE is Map
    ? rawE2EE['e2ee']
    : payload['_e2ee_raw_meta'];
```

---

### Patch-04：实现真正的密钥重生成（BUG-05）

```dart
// chat_page.dart — 替换 _refreshE2EEKeys()
Future<void> _refreshE2EEKeys() async {
  try {
    EasyLoading.show(status: t.chat.e2eeRecreatingKey);

    // 1. 生成新 RSA 密钥对并写入安全存储
    await RSAService.generateKeyPair();

    // 2. 将新公钥上传到后端注册
    final publicKey = await RSAService.getPublicKeyPem();
    final deviceId = DeviceExt.did;
    final ok = await E2EEApi().uploadDevicePublicKey(
      deviceId: deviceId,
      publicKey: publicKey,
    );
    if (!ok) throw Exception('上传公钥失败');

    // 3. 清空本地 E2EE 缓存，强制下次重拉对端公钥
    E2EEService.clearCache();
    await StorageService.to.remove('e2ee_key_refresh_time');

    EasyLoading.dismiss();
    EasyLoading.showSuccess(t.chat.e2eeKeyRecreated);
  } on Exception catch (e) {
    EasyLoading.dismiss();
    EasyLoading.showError(
      t.common.e2eeKeyRecreationFailed(error: e.toString()),
    );
  }
}
```

---

### Patch-05：forceRefresh 时区分"空=已吊销"（BUG-07）

```dart
// e2ee_service.dart — 替换 forceRefresh 分支（~541-548行）
if (forceRefresh) {
  final fetched = await _fetchUserDevicePublicKeys(uid);
  if (fetched.isNotEmpty) {
    _userKeyCacheByDevice[uid] = fetched;
    _userKeyCacheExpiry[uid] = DateTime.now().add(_cacheTtl);
    keys = fetched;
  } else {
    // 空列表 = 服务端已吊销所有密钥，不回落旧缓存
    _userKeyCacheByDevice.remove(uid);
    _userKeyCacheExpiry.remove(uid);
    keys = [];
    iPrint('E2EE[$uid]: 服务端返回空密钥列表，已清除本地缓存');
  }
}
```

---

### Patch-06：禁言 ACL 统一收口（BUG-10）

```dart
// chat_page.dart — 新增辅助方法
/// 发送前统一鉴权：被禁言时弹提示并返回 false
bool _checkSendAllowed() {
  if (_isMuted) {
    final msg = _muteExpiryTimer != null
        ? t.chat.mutedUntil(time: _muteExpiryFormatted)
        : t.chat.mutedIndefinitely;
    EasyLoading.showToast(msg);
    return false;
  }
  return true;
}

// 在以下方法头部均加入检查（示例）：
Future<void> _handleImageSelection() async {
  if (!_checkSendAllowed()) return;
  // ... 原有逻辑不变
}

Future<void> _handleVoiceSelection() async {
  if (!_checkSendAllowed()) return;
  // ... 原有逻辑不变
}

Future<void> _handleStickerSelection() async {
  if (!_checkSendAllowed()) return;
  // ... 原有逻辑不变
}
```

---

### Patch-07：修复 GroupSchedule 重复消息（BUG-09）

```dart
// group_schedule_page.dart — _createSchedule() 成功后
// 删除本地注入，等待 S2C 回显作为唯一来源

// 修改前：
await ref.read(chatProvider.notifier).addMessage(localCustomMessage);

// 修改后：删除上面一行，改为即时反馈 Toast
EasyLoading.showSuccess(t.group.scheduleCreated);
if (mounted) context.pop(true); // 返回并通知父页面刷新
```

---

### Patch-08：S2C 处理失败时不 ACK（S-02）

```dart
// message_s2c.dart:278 — 在 catch 块内设置 autoAck = false
} on Object catch (e, s) {
  iPrint("switchS2C error: $e, $s");
  autoAck = false; // 处理失败，允许服务端超时重推
}
```

---

<a id="priority"></a>
## 九、优先级矩阵与修复顺序

| 优先级 | Bug ID | Patch | 原因 |
|--------|--------|-------|------|
| 🔴 P0 立即 | BUG-01 | Patch-01 | 数据静默丢失，会话角标不可信，生产必现 |
| 🔴 P0 立即 | BUG-04 | Patch-03 | 重试 100% 失效，所有 [解密失败] 消息永久无法恢复 |
| 🔴 P0 立即 | BUG-05 | Patch-04 | 欺骗性 Toast，用户误信密钥已重建，功能完全无效 |
| 🔴 P0 本周 | BUG-02 | Patch-02 | DND 状态每次刷新会话列表后重置，影响通知体验 |
| 🟠 P1 下迭代 | BUG-10 | Patch-06 | 安全绕过，禁言管控形同虚设 |
| 🟠 P1 下迭代 | BUG-08 | Patch-01 含 | DND 通知抑制全面失效（Patch-01 已合并修复） |
| 🟠 P1 下迭代 | BUG-07 | Patch-05 | 密钥吊销延迟 30 分钟，违反最小权限原则 |
| 🟡 P2 技术债 | BUG-06 | 待设计 | 游标跳过逻辑需重构，引入重试队列 |
| 🟡 P2 技术债 | S-02 | Patch-08 | 消息处理失败时不应 ACK |
| 🟡 P2 技术债 | BUG-09 | Patch-07 | GroupSchedule 重复卡片 |
| 🟡 P2 技术债 | S-03 | 待验证 | e2ee_device_key_changed 缓存清理实现需审查 |
| 🟡 P2 技术债 | N-02 | — | 通知路由 peerId 格式校验 |
| 🟡 P2 技术债 | BUG-03 | — | ConversationModel.empty() 类型字面量 |
| 🟢 P3 清理 | BUG-11 | — | GroupMemberModel 原地修改改为 copyWith |

---

## 十、核心结论

**E2EE 子系统存在系统性失效**：BUG-04（重试键名不匹配）+ BUG-05（重建是空操作）+ BUG-06（游标跳过失败项）三个 Bug 组合，使整个"E2EE 密钥恢复"能力完全失效。用户看到的 `[解密失败]` 消息既无法自动重试，也无法通过手动"重建密钥"修复，且用户会收到欺骗性成功提示。这是架构设计层面的系统性缺口，需要将 E2EE 健康检查服务整体回归测试。

**会话管理存在基础性数据一致性问题**：BUG-01 的 TOCTOU 竞态是 SQLite 并发写的经典错误；BUG-02 的列漏读导致 DND 等关键状态在每次列表读取后被归零。两者合并修复效率最高（Patch-01 已包含 BUG-08 的 `isMuted` 传递修复）。

**禁言 ACL 存在全路径绕过**：BUG-10 说明禁言检查仅覆盖文字发送入口，媒体路径完全敞开，是功能性安全漏洞，优先级不应低于 P1。
