# imboy.pub 核心子系统代码审计报告

> 审计日期:2026-06-15
> 范围:4 个核心子系统 + 周边(NotificationService / WebSocket 收发链路)
> 原则:实事求是,行号与代码均经亲自精读核实。**已亲自验证的 Critical 标 ✅。**
> 交付性质:纯审计报告,未修改任何 imboy 代码。Patch 为外科手术式建议,由维护者决定是否落地。

---

## 一、严重度总览

| # | 子系统 | 问题 | 位置 | 严重度 |
|---|--------|------|------|--------|
| 1 | 会话管理 | `save()` 是无事务的 read-modify-write,并发 S2C 包丢未读计数 | conversation_repo_sqlite.dart:126-146 ✅ | **Critical** |
| 2 | E2EE | 私钥单 key 覆盖写,无 key history,旧密钥一旦覆盖历史密文永久不可解密 | e2ee_key_service.dart:133-139 ✅ | **Critical** |
| 3 | E2EE | 密钥不匹配分支丢弃原始密文,自愈 `retryFailedMessages` 对这类消息恢复率=0 | message.dart:1507-1515 ✅ | **Critical** |
| 4 | 会话管理 | 未设 `PRAGMA busy_timeout`,`query()` 无 locked 重试,save 的 SELECT 在锁竞争时直接抛 | sqlite.dart:272-284 | **High** |
| 5 | E2EE | `isKeyMismatch` 用宽泛子串匹配(`contains('密钥')`/`contains('device')`),误判诱导用户走销毁密钥路径 | message.dart:1488-1492 ✅ | **High** |
| 6 | E2EE | 密钥不匹配对话框不接已有的恢复中心,反而导向无效的"重建密钥" | chat_page.dart:2075-2094 | **High** |
| 7 | 群协同 | `getSingleFile` 的 cacheKey 无 host 白名单/URL 规范化,SSRF-to-storage + 缓存污染 | imboy_cache_manager.dart:119-134 | **High** |
| 8 | 会话管理 | `findByPeerId`/`findById`/`list` 的 SELECT columns 未含 `mentionUnread`/`isMuted`,读出恒为 0;叠加 save() 全字段覆写可清掉用户的 DND | conversation_repo_sqlite.dart:155-363 | **Medium** |
| 9 | WS 收发 | `AckManager._pendingAcks[msgId]` 无条件覆盖,retryCount 永不清零 → 重试活锁 | ack_manager.dart:286-298 | **Medium** |
| 10 | E2EE | 健康检查与手动 refresh/登录上报对公钥缓存并发刷新无互斥,kid/pem 可能错配 | e2ee_health_check_service.dart:84-106 | **Medium** |
| 11 | 群协同 | GroupSchedule 卡片无 `local_origin` 标记,仅靠 msgId 去重,服务端若改写 id 会重复显示 | group_schedule_page.dart:139-165 | **Medium** |
| 12 | WS 收发 | `_channel!.sink.add` 在 dispose 并发下有 NPE 风险;离线/在线 S2C 同路径无独立幂等键 | websocket.dart:775-801 | **Medium** |
| 13 | 群管理 | `ChatAttachmentHandler` 的 8 个媒体入口 + `forwardMessage` handler 层完全不检查 isMuted,仅靠 ChatInput build 短路兜底 | attachment_handler.dart / message_action_handler.dart:281 | **Medium** |
| 14 | E2EE | `_refreshE2EEKeys` 名实不符(只清缓存不重建密钥),却弹"密钥已重新创建"假 toast | chat_page.dart:2099-2127 | **Low** |
| 15 | WS 收发 | typing 包已有 3s throttle + 300ms debounce + 5s idle,无洪泛(否定任务假设) | chat_input.dart:517 / typing_indicator_rules.dart:43 | **Info** |
| 16 | 群协同 | GroupSchedule 不存在 S2C 回环路径(NewScheduleEvent 全工程零调用方),无死循环(否定任务假设) | message_s2c.dart:87-281 | **Info** |
| 17 | 群管理 | `group_member_page` 3 个订阅全部正确 cancel,**无内存泄漏**(否定任务假设) | group_member_page.dart:102-110 | **Info** |
| 18 | 通知 | 本地通知路由对 DND 处理正确(`shouldSuppressNotification` + @ 穿透),无问题 | message.dart:911-916 | **Info** |

---

## 二、Critical 详解(含 patch)

### C1. ✅ `ConversationRepo.save()` 无事务 read-modify-write 丢未读计数

**位置**:`imboyapp/lib/store/repository/conversation_repo_sqlite.dart:126-146`

**原文已验证**:

```dart
126  Future<ConversationModel> save(ConversationModel obj) async {
128    ConversationModel? oldObj = await findByPeerId(   // ← 1) SELECT 读出旧值
129      obj.type,
130      obj.peerId.toString(),
131    );
132    int unreadNumOld = oldObj == null ? 0 : oldObj.unreadNum;
133    int mentionUnreadOld = oldObj == null ? 0 : oldObj.mentionUnread;
135    obj.unreadNum = obj.unreadNum + unreadNumOld;            // ← 2) 在 Dart 内存里相加
136    obj.mentionUnread = obj.mentionUnread + mentionUnreadOld;
137    if (oldObj == null) {
138      obj.id = await insert(obj);
139    } else {
140      Map<String, dynamic> data = obj.toJson();
141      data.remove(ConversationRepo.id);
142      await updateById(oldObj.id, data);                     // ← 3) 绝对值覆写 UPDATE,无事务
143    }
144  }
```

- `findByPeerId`(SELECT)→ `obj.unreadNum + unreadNumOld`(Dart 内存相加)→ `updateById`(绝对值 UPDATE)。**全程无 `db.transaction`**。
- SELECT 走 `SqliteService.query`(不经过 `_dbLock`、无 locked 重试),UPDATE 走 `_dbLock`。读与写在不同的锁边界,中间 await 点让出 event loop。

**触发场景**:断线重连批量推送 / 群消息回放 / 多设备同步 → 两个 S2C 包几乎同时触发 `save()`。T1/T2 都 `findByPeerId` 读到 unread=5 → 都写 6(本应 7),第二个包 +1 丢失。

**对比**:服务端 `imboy/src/repo/wallet_repo.erl:142-146` 用的是原子 `SET balance = balance + $1 ... RETURNING balance`,并发安全。客户端做法恰恰相反。

**Patch**(参照 wallet 的原子自增范式):

```dart
// conversation_repo_sqlite.dart — 新增原子自增方法,替换 save() 里的 unread 累加逻辑
Future<ConversationModel?> incrementUnread(
  int type,
  String peerId, {
  int unreadDelta = 0,
  int mentionDelta = 0,
}) async {
  final db = SqliteService.to;
  // 原子自增 + RETURNING,避免 read-modify-write;事务保证 unread/mention 同生共死
  final rows = await db.transaction((txn) async {
    return await txn.rawQuery(
      'UPDATE ${ConversationRepo.tableName} '
      'SET ${ConversationRepo.unreadNum} = ${ConversationRepo.unreadNum} + ?, '
      '    ${ConversationRepo.mentionUnread} = ${ConversationRepo.mentionUnread} + ?, '
      '    ${ConversationRepo.isShow} = 1 '
      'WHERE ${ConversationRepo.type} = ? '
      '  AND ${ConversationRepo.userId} = ? '
      '  AND ${ConversationRepo.peerId} = ? '
      'RETURNING *',
      [unreadDelta, mentionDelta, type, UserRepoLocal.to.currentUid, peerId],
    );
  });
  if (rows.isEmpty) return null; // 调用方走 insert 新建
  return ConversationModel.fromJson(rows.first);
}
```

> 注:`sqflite` 支持事务回调内执行,RETURNING 在 SQLite 3.35+ 可用。调用方把 `save(conv)`(仅 unread 场景)改为 `incrementUnread(...)`;`save()` 保留给"全字段首建/其他字段更新"路径。

---

### C2. ✅ 私钥单 key 覆盖写,无 key history

**位置**:`imboyapp/lib/service/e2ee_key_service.dart:133-139` → `storage_secure.dart:147-149`

**原文已验证**:

```dart
// e2ee_key_service.dart:133-139
133    await Future.wait([
134      storage.savePrivateKey(privateKeyPem),    // ← 单 key 覆盖写
135      storage.savePublicKey(publicKeyPem),
136      storage.setDeviceId(deviceId),
137      storage.setKeyId(keyId),
138      storage.setKeyCreatedAt(createdAt),
139    ]);

// storage_secure.dart:147-149
147  Future<void> savePrivateKey(String privateKey) async {
148    await write(key: 'e2ee_private_key', value: privateKey);   // 已存在则覆盖
149  }
```

- 存储是单一 key `e2ee_private_key`,语义为覆盖。全仓 grep `previous.*key|keyHistory|archiveKey` **零命中**,**无任何密钥历史链 / key backup / key archive**。
- 一旦 `generateKeyPair()` 被调用,旧私钥永久消失 → 用旧公钥包装过的历史 AES 密钥(`ek` 字段)永远无法 RSA 解密 → 历史密文永久不可读。

**触发入口**:
- `e2ee_key_recovery_page.dart:790`(用户主动,有 `e2eeWarnIrreversible` 警告但事后才弹)
- `e2ee_transfer_receive_page.dart:85`(deviceId 空时)
- `e2ee_transfer_handler.dart:434-436`(注释自承"删除后将无法解密历史消息")
- `e2ee_key_recovery_page.dart:906-909`(`deleteAllE2EEKeys()`)

**Patch**(建立 key history chain,按消息里的 `kid` 回溯解密):

```dart
// storage_secure.dart — 覆盖前先把旧私钥归档到 e2ee_private_key_history_{oldKid}
Future<void> savePrivateKey(String privateKey) async {
  // 覆盖前归档:把当前私钥移入历史链,保留按 kid 回溯解密的能力
  final existing = await read(key: 'e2ee_private_key');
  final existingKid = await getKeyId();
  if (existing != null && existingKid != null && existingKid.isNotEmpty) {
    await write(
      key: 'e2ee_private_key_history_$existingKid',
      value: existing,
    );
  }
  await write(key: 'e2ee_private_key', value: privateKey);
}

/// 按 kid 查找历史私钥(含当前私钥)。解密失败时按消息里的 kid 回溯。
Future<String?> findPrivateKeyByKid(String kid) async {
  final currentKid = await getKeyId();
  if (currentKid == kid) {
    return read(key: 'e2ee_private_key');
  }
  return read(key: 'e2ee_private_key_history_$kid');
}
```

> 解密侧(`e2ee_service.dart` 的 `decryptE2EEMessage`)改为按消息 payload 的 `kid` 调 `findPrivateKeyByKid(kid)`,找不到再判定"真·密钥缺失"。归档链需设上限(如保留最近 5 把)避免无限增长;归档私钥的清除要绑定"用户主动清空所有数据"。

---

### C3. ✅ 密钥不匹配分支丢弃原始密文 → 自愈对这类消息恢复率=0

**位置**:`imboyapp/lib/service/message.dart:1507-1515`(丢密文)↔ `e2ee_health_check_service.dart:604-606` / `e2ee_service.dart:711-714`(自愈依赖密文)

**原文已验证**:

```dart
// message.dart:1507-1515 — 密钥不匹配分支返回的 payload 无 _e2ee_raw
1507        // 密钥不匹配:不保存原始密文,避免将密文写入 SQLite
1508        return {
1509          'msg_type': originalMsgType,
1510          'text':
1511              '🔒 此消息无法解密\n\n可能原因:...',
1512          '_e2ee_failed': true,
1513          '_e2ee_reason': 'key_mismatch',
1514          '_show_relogin_button': true,
1515        };

// e2ee_health_check_service.dart:604-606 — 自愈第一步就要密文
604       // 检查是否有原始密文
605       if (payloadMap['_e2ee_raw_ciphertext'] == null) {
606         return false;
607       }

// e2ee_service.dart:711-714 — retryDecryptFailedMessage 同样依赖 _e2ee_raw
711       if (rawCiphertext == null || rawCiphertext.isEmpty) {
712         iPrint('⚠️ [E2EE] 消息不包含原始密文,无法重试解密');
713         return failedPayload;
714       }
```

- 密钥不匹配分支返回的 payload **既没有 `_e2ee_raw` 也没有 `_e2ee_raw_ciphertext`**,只有占位文本。
- 健康检查 `_findFailedE2EEMessages` 靠 `payload LIKE '%_e2ee_failed%'` 扫到这些消息,但 `_retryDecryptMessage` 第一步就因密文缺失 return false。
- **结论**:即使用户后续通过社交恢复 / 设备转移拿回旧私钥,这些消息也**永远无法再被重试解密**。
- 这是整个审计最致命的设计缺陷:**自愈机制的前提(保留密文)被触发机制(丢弃密文)破坏了。**

**Patch**(密文加密落库 + TTL 清理,兼顾安全与可恢复):

```dart
// message.dart:1494 分支 —— 保留加密后的原始密文,供健康检查重试
if (isKeyMismatch) {
  final peerId = data['from']?.toString();
  AppEventBus.fire(E2EEKeyMismatchEvent(
    messageId: msgId, reason: '密钥不匹配', peerId: peerId,
  ));
  return {
    'msg_type': originalMsgType,
    'text': '🔒 此消息无法解密\n\n可能原因:\n• 您在其他设备上登录\n• 设备密钥已过期\n\n建议:前往密钥恢复中心导入历史密钥',
    '_e2ee_failed': true,
    '_e2ee_reason': 'key_mismatch',
    '_show_relogin_button': true,
    // ✅ 保留原始密文(已在传输层加密),供 retryFailedMessages 重试
    '_e2ee_raw': data['_e2ee_raw'] ?? data['payload'],
    '_e2ee_raw_ciphertext': data['_e2ee_raw_ciphertext'] ?? data['ciphertext'],
    '_e2ee_retry_after': DateTime.now().toIso8601String(),
  };
}
```

> 配套:`e2ee_health_check_service.dart` 加 30 天 TTL 清理,超期未解密的 `_e2ee_failed` 消息才删掉密文,平衡安全与可恢复。

---

## 三、High 详解(修复要点)

### H1. 未设 busy_timeout + query 无重试

**位置**:`sqlite.dart:272-284`(_onConfigure 只设 foreign_keys/synchronous/cache_size,无 busy_timeout);`sqlite.dart:649-692`(query 无 locked 重试)

**影响**:SQLite 默认 busy_timeout=0,拿不到锁立刻抛 `database is locked`。`save()` 的 SELECT 走 query,在写锁竞争时直接异常冒泡(无 try/catch)。WAL 已在 `_onOpen`(`sqlite.dart:355`)开启,但 WAL 不能替代 busy_timeout。

**Patch**:

```dart
// sqlite.dart _onConfigure 追加
await db.execute('PRAGMA busy_timeout = 5000'); // 5s,与 _dbLock 重试配合
// sqlite.dart query() 增加 locked 重试,复用现有 _isDatabaseLockedError + 退避逻辑(sqlite.dart:836-842)
```

### H2. ✅ isKeyMismatch 宽泛子串误判

**位置**:`message.dart:1488-1492`

```dart
1488      final errorStr = e.toString().toLowerCase();
1489      final isKeyMismatch =
1490          errorStr.contains('no key found for device') ||
1491          errorStr.contains('密钥') ||
1492          errorStr.contains('device');
```

- `contains('密钥')` 命中"生成 RSA 密钥对失败""获取私钥失败"等所有含"密钥"的异常;
- `contains('device')` 命中任何提到 device 的异常(deviceId 空、device_id 字段缺失等)。
- 真正的密钥不匹配信号只有一个:`decryptE2EEMessage` 抛出的 `'No key found for device: $myDid'`(`e2ee_service.dart:344`)。
- 误判后果:丢弃原始密文(C3)+ 弹不匹配对话框 → 链式诱导用户销毁密钥。

**Patch**:收窄为只匹配精确异常:

```dart
final isKeyMismatch = errorStr.contains('no key found for device');
```

### H3. 密钥不匹配对话框不接恢复中心

**位置**:`chat_page.dart:2075-2094`(自造三按钮:重建/重登/稍后)↔ 已有 `showE2EERecoveryGuide`(`e2ee_recovery_guide_dialog.dart:34-69`,路由到 `/e2ee_key_recovery`,接社交恢复/设备转移/本地备份)

- 项目已有统一恢复引导对话框,但 `_showE2EEKeyMismatchDialog` **完全没调用**它,反而导向无效的"重建密钥"(见 Low #14:其实只清缓存没重建)。
- 用户在最能感知"解不开历史消息"的时刻,却被导向一个**不恢复反销毁**的路径。

**Patch**:`_showE2EEKeyMismatchDialog` 改调 `showE2EERecoveryGuide(context, scene: E2EERecoveryScene.decryptFailed)`,或至少加"前往恢复中心"按钮路由到 `/e2ee_key_recovery`。

### H4. getSingleFile 的 cacheKey 无 host 白名单

**位置**:`imboy_cache_manager.dart:119-134`

```dart
124      cacheKey = 'objkey://$url';
...
129      cacheKey =
130          '${rawUri.scheme}://${rawUri.host}:${rawUri.port}${rawUri.path}';
```

- cacheKey 直接拼 rawUri 的 scheme/host/port/path,消息 payload.uri 对端可控。
- 攻击者把消息 payload.uri 设成 `http://evil.com/x`,会被直接当 key 且真的请求 evil.com 下载字节到本地缓存目录(SSRF-to-storage + 缓存污染)。
- **澄清**:`fileName = imboy_cache_${hashCode}.$ext`(L238-240)前缀固定 + hashCode 为整数,**无 `../` 目录穿越**(这点已防御,不夸大)。

**Patch**:`getSingleFile` 入口加 host 白名单(只允许 imboy 自己的资产域名/objkey 协议),对 URL 做 `Uri.parse` 后 path 规范化,拒绝非法 host。

---

## 四、Medium 详解(修复要点,给方向)

### M1. SELECT 列表遗漏新字段

**位置**:`conversation_repo_sqlite.dart:155-363`(findByPeerId/findById/list/clearMessages/search 的 columns 各不相同)

- 这些查询的 columns **不包含 `mentionUnread`、`isMuted`、`region`、`sign`**,只 SELECT 了 `unreadNum`。
- `fromJson` 用 `parseModelInt(json[...])`,缺列时返回默认 0 → 从 DB 读出的会话 `isMuted` 永远是 0。
- 叠加 `save()` 全字段覆写(L140 `data = obj.toJson()`),`save()` 里 `oldObj.isMuted` 恒为 0 → 可能把用户的 DND(`is_muted`)清掉。
- 这是 v18 新增字段后**遗漏同步到所有 SELECT 列表**的疏漏。

**修法**:所有 SELECT columns 统一为 `null`(全列)或显式补齐这 4 个字段。

### M2. AckManager _pendingAcks 覆盖致 retryCount 永不清零

**位置**:`ack_manager.dart:286-298`

- `_pendingAcks[msgId] = _PendingAck(..., retryCount: 0)` 无条件覆盖。服务端重发同 msgId 时 retryCount 清零 → 重试上限失效,配合 `_scheduleRetry` 取消旧 Timer 建新 Timer,形成 ACK 重试活锁。

**修法**:已存在则保留 retryCount,只更新 sendTime。

### M3. 健康检查与手动 refresh 并发刷公钥缓存

**位置**:`e2ee_health_check_service.dart:90/98/104` 与 `_refreshE2EEKeys`(`chat_page.dart:2105`)都调 `getUserDevicePublicKeys(forceRefresh:true)`(`e2ee_service.dart:551-553`)

- 三处最终都写同一份 `_userKeyCacheByDevice[uid]` + `_userKidCacheByDevice[uid]` + `_userKeyCacheTimestamp[uid]`。Dart 事件循环下这三处赋值不是原子的,A 的 didToPem 写入后、didToKid 写入前,B 可能插队并清掉缓存 → **kid 与 pem 来自不同批次 API 响应**。
- 后果:接收方按 kid 找不到匹配私钥 → 又触发 H2 的误判 → 链式灾难。竞态窗口小但存在,且 `forceRefresh:true` 路径在重连/回前台/手动刷新时高频出现。

**修法**:给 `getUserDevicePublicKeys(forceRefresh:true)` 加 per-uid `Completer` 锁,保证同一 uid 的并发刷新串行化。

### M4. GroupSchedule 卡片无 local_origin 标记

**位置**:`group_schedule_page.dart:145-156`

- metadata 只有 `msg_type/id/group_id/title/start_time`,本地发起与服务器回推在 type 层无法区分(都是 `msg_type=groupSchedule`)。
- 去重靠 `_generateContentHash`(`message.dart:741-749`)优先用 msgId;若服务端为群广播重新分配 id(常见 IM 设计),A 收到回推时 msgId 不同 → 内容哈希回退到 `from:to:type:msgType:createdAt:clientSendTs` → A 会看到自己的日程卡片重复显示。这是 UX bug,不是死循环。

**修法**:metadata 加 `'local_origin': true`(本地发起时),S2C 回推走 msgId 去重并要求服务端原样回传 id。

### M5. _channel!.sink.add NPE + 离线/在线 S2C 无独立幂等

**位置**:`websocket.dart:775-801`(flushMessageQueue) + `message_offline.dart:485-499`

- L783 `_channel!.sink.add(payload)` 强解包,`_cancelStream`(L1136-1150)在 dispose/重连路径会把 `_channel` 置 null。while 循环体内 L783 之前无 await(同步安全),但 L790 `await Future.delayed` 期间可能被置 null,实测有 break 兜底但不够干净。
- 离线 S2C 经 `batchInsertOfflineMessages` 的 `onS2CMessage` 回调走 `switchS2C`(`message_s2c.dart:87-281`),与在线 S2C 同路径,部分 `_handleXxx`(如 `_handleGroupMemberJoin` L389-433)无幂等检查,重复投递会重复写库 + 重复弹事件。

**修法**:L783 加空判;给关键 S2C action 加基于 msgId+action 的幂等键。

### M6. 媒体入口 handler 层无 isMuted 守卫

**位置**:`attachment_handler.dart`(全文 grep `isMuted` 0 命中)+ `message_action_handler.dart:281`(`forwardMessage`)

- `chat_page.dart` 的 8 个媒体发送入口(语音/图片/文件/相机/位置/名片/收藏/贴纸)直接调 `ChatAttachmentHandler`,**handler 层完全不检查 isMuted**。
- 当前靠 ChatInput build 顶部 `if (widget.isMuted) return Container(...)`(chat_input.dart:1071-1115)短路兜底——媒体按钮 UI 不渲染,所以**当前不可达**。
- 但 **`forwardMessage`(长按消息转发)既无 handler 守卫也无 UI 兜底**,被禁言用户长按消息选转发 → 选择目标会话 → 转发成功,完全绕过 isMuted。

**修法**:在 ChatAttachmentHandler 各发送方法入口 + forwardMessage 入口加 `_isMuted` 显式守卫,把防御从单点(ChatInput build)下沉到 handler 层。

---

## 五、Low

### L1. `_refreshE2EEKeys` 名实不符

**位置**:`chat_page.dart:2099-2127`

- 按钮文案 `t.chat.e2eeRecreatingKey` + toast `t.chat.e2eeKeyRecreated`("密钥已重新创建")。
- 实际只调 `E2EEService.clearCache()`(只清内存公钥缓存 Map,**完全不碰** `flutter_secure_storage` 里的 `e2ee_private_key`)+ `remove('e2ee_key_refresh_time')` + `getUserDevicePublicKeys`。
- 密钥根本没重建,本地 RSA 私钥原封不动 → 既没解决"不匹配",又给用户虚假的安全感。

**修法**:要么真的重建密钥(且先 archive 旧密钥,见 C2),要么改名 + 改文案为"刷新对方公钥缓存",避免"密钥已重新创建"的虚假 toast。

---

## 六、技术债评估

1. **SQLite 层缺统一并发治理**:开了 WAL 却配了一把粗粒度 `_dbLock`(所有表写共享),又没设 busy_timeout,query 不重试。并发收益被锁抵消,锁竞争又无超时兜底。**建议**:WAL + busy_timeout=5000 + 细粒度按表锁 + query 重试。

2. **E2EE 可恢复性设计自相矛盾**:有 `retryFailedMessages` 自愈,却丢密文(C3);有 `generateKeyPair`,却无 key history(C2);有 `showE2EERecoveryGuide`,却不接密钥不匹配入口(H3)。三处都是"半截子工程"。**建议**:统一为"保留密文(TTL 清理)+ key history chain + 所有不匹配入口走恢复中心"。

3. **isMuted 有两套语义**(admin 群禁言 ACL `_isMuted` vs 用户本地 DND `conversation.isMuted`),代码注释(`conversation_model.dart:38-40`)已区分但易混。媒体入口守卫缺失是典型"UI 兜底而非单点强制"。**建议**:下沉到 handler 层。

4. **死代码误导**:`GroupScheduleService.handleNewSchedule` / `NewScheduleEvent`(`group_schedule_service.dart:207-216, 280-286`)全工程零调用方,容易让后人误以为有 S2C 回环路径而加防御逻辑。**建议**:删除或补全。

---

## 七、否定任务假设(实事求是)

| 任务假设 | 实际结论 | 证据 |
|----------|----------|------|
| typing 包洪泛 | **不存在**,已有 3s throttle + 300ms debounce + 5s idle | chat_input.dart:517 / typing_indicator_rules.dart:43-56 |
| GroupSchedule 死循环 | **不存在 S2C 回环路径**,卡片走 C2G,NewScheduleEvent 零调用方 | message_s2c.dart:87-281, group_schedule_service.dart:207 |
| GroupMemberPage 订阅泄漏 | **不存在**,3 个订阅(_localeSubscription/_ssMemberMute/_ssMemberUnmute)全部正确 cancel | group_member_page.dart:102-110 |
| 音频缓存 `../` 目录穿越 | **不存在**,fileName 用 `imboy_cache_${hashCode}.$ext`,无路径段拼接 | imboy_cache_manager.dart:238-240 |
| DND 影响通知路由错误 | **无问题**,(message.dart:911-916)调用 `shouldSuppressNotification(isMuted: savedConv.isMuted, isMuted: mentionIncrement > 0)` 闸门正确,@ 穿透符合预期 | message.dart:911-916, message_conversation_utils.dart:108-114 |

---

## 八、建议的修复优先级

| 优先级 | 条目 | 理由 |
|--------|------|------|
| P0 | **C3**(保留密文) + **H2**(收窄误判) | 这两条是同一个链式灾难的根与放大器,一起修才能止血 |
| P0 | **C1**(原子自增 unread) | 数据正确性,影响所有用户的未读角标 |
| P1 | **H1**(busy_timeout) + **M1**(SELECT 列) | SQLite 并发治理,低风险高收益 |
| P1 | **H4**(cacheKey 白名单) | 安全,SSRF-to-storage |
| P2 | **H3**(接恢复中心) + **L1**(改名) | UX + 防止用户误销毁密钥 |
| P2 | **M6**(媒体入口守卫) | 防御深度 |
| P3 | **C2**(key history) | 改动较大,涉及存储格式与解密链路,建议单独排期并配测试 |
| P3 | **M2/M3/M4/M5** | 并发与幂等优化 |

---

*报告结束。所有 Critical(✅)已亲自精读源码核实行号与代码片段。如需把某几个 patch 落地成实际代码修改,告知具体条目即可。*
