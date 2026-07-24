# imboy Flutter 核心子系统审计 — 复核更正版（VERIFIED）

**复核日期**：2026-06-15
**复核方法**：对前版审计（`2026-06-15-flutter-core-subsystems-audit.md`）逐条拿真实代码比对，行号与代码摘录均为直读核实
**结论**：前版报告 **大量误报**——其针对的多为历史版本代码。本文为更正记录，**前版作废**。

> 严重度：🔴 CRITICAL / 🟠 HIGH / 🟡 MEDIUM / 🟢 LOW / ✅ SAFE

---

## 零、复核总览（实事求是）

| 旧编号 | 旧定级 | 复核裁决 | 当前真相一句话 |
|---|---|---|---|
| BUG-01 save() TOCTOU 竞态 | 🔴CRITICAL | **❌推翻** | 实为 SQL 原子自增 `unread=unread+?` + `exclusive` 事务，无内存读改写 |
| BUG-02 list/search 漏列 | 🔴CRITICAL | **❌推翻** | `mention_unread`/`is_muted` 两列均已在 columns 中 |
| BUG-03 empty() 类型字面量 | 🟡MEDIUM | **❌推翻** | `subtitle=""`(String)，`lastTime` 字段本就是 int，类型自洽 |
| BUG-04 E2EE 重试键名不匹配 | 🔴CRITICAL | **❌推翻** | 读写均围绕嵌套 `_e2ee_raw`，全仓无 `_e2ee_raw_ciphertext` 键 |
| BUG-08 save() 不传 isMuted | 🟠HIGH | **❌推翻** | save() 返回 DB 重读快照，isMuted 为真值 |
| BUG-09 群日程双卡片 | 🟡MEDIUM | **❌推翻** | 后端 C2G 排除发送者本人 + msgId 去重，不产生重复，无循环 |
| BUG-10 禁言 ACL 全路径绕过 | 🟠HIGH | **❌推翻** | 11 个媒体发送方法全部经 `_sendMessage()` 统一 `isMutedCheck` 门控(C13) |
| **BUG-05 `_refreshE2EEKeys` 空操作** | 🔴CRITICAL | **✅确认（降级 HIGH）** | 真未重建密钥却弹成功 Toast；但**不删私钥，无历史丢失** |
| **BUG-06 游标跳过失败项** | 🟠HIGH | **✅确认（降级 LOW）** | 游标确实越过失败条目，但有 `failedUids` 重试队列(C6-γ)兜底 |
| **BUG-07 forceRefresh 空回退** | 🟠HIGH | **🟡部分（降级 MEDIUM）** | 回退受 30min TTL 门槛约束，属"抗抖动 vs 即时吊销"权衡 |
| **BUG-11 原地改 Model** | 🟢LOW | **✅确认** | `group_member_page.dart:75/93` 原地写 `muteUntilMs` |
| N-02 通知路由 peerId 未校验 | 🟡MEDIUM | **🟡部分** | 确无格式校验，但输入源是受控本地 payload |
| S-02 异常仍 ACK | 🟡MEDIUM | **🟡部分（机制相反）** | 异常其实**跳过** ACK；真隐患是 catch 静默吞异常 + 非抛错型逻辑失败仍 ACK |
| S-03 device_key_changed 清缓存 | 🟡MEDIUM | **🟡部分** | 清缓存完整，但仅惰性重取，未主动重拉 |
| SAFE-01 频道语音路径安全 | ✅ | **✅确认** | scheme 白名单 + `viewUrlAsync` 重授权 + 结构化 cacheKey，无注入 |
| SAFE-03 订阅无泄漏 | ✅ | **✅确认** | `_ssMemberMute/_ssMemberUnmute` 均在 dispose cancel |
| 打字状态节流 | ✅ | **✅确认** | 300ms debounce + 3s 节流 + 5s idle stop，无 WS 泛洪 |

**结论**：14 项 BUG 级结论中 **7 项纯误报**（含 3/4 的 CRITICAL 与"安全漏洞"头条），仅 **BUG-05 / BUG-06 / BUG-11** 为真实缺陷，其中只有 BUG-05 值得近期处理；其余为权衡或可观测性问题。

---

## 一、真实缺陷（需处理）

### BUG-05 🟠 HIGH（原 CRITICAL，降级）— "重新创建密钥"按钮是空操作 + 欺骗性成功提示

**文件**：`imboyapp/lib/page/chat/chat/chat_page.dart:2102-2129`（按钮入口 2078-2084）

直读真实代码：

```dart
Future<void> _refreshE2EEKeys() async {
  try {
    EasyLoading.showToast(t.chat.e2eeRecreatingKey);
    E2EEService.clearCache();                       // 仅清内存公钥缓存
    // 2. 重新生成密钥对（RSA服务会自动处理）       ← 注释占位，无实际生成
    final currentUid = UserRepoLocal.to.currentUid;
    if (currentUid.isNotEmpty) {
      await StorageService.to.remove('e2ee_key_refresh_time');
      await E2EEService.getUserDevicePublicKeys(currentUid); // 仅拉本人现有公钥
    }
    EasyLoading.showSuccess(t.chat.e2eeKeyRecreated); // 无条件弹"已重建"
```

**核实**：
1. `clearCache()`（`e2ee_service.dart:93-101`）只清 6 个内存公钥缓存 Map，**不触碰** secure storage 中按 kid 存的私钥 → **历史密文不会因点此按钮而永久不可解**（审计指令核心安全问题，答案为"否"）。
2. 但本方法**未**调用真实存在的 `E2EEKeyService.generateKeyPair()`（`e2ee_key_service.dart:103`）也未上传新公钥，密钥不匹配问题点完按钮后**依旧存在**，却得到虚假成功提示。

**影响**：能力名实不副，误导用户"已修复"。非数据安全/丢失问题，故降为 HIGH。

---

### BUG-06 🟢 LOW（原 HIGH，降级）— 通知游标越过失败条目（已有重试队列兜底）

**文件**：`imboyapp/lib/service/e2ee_health_check_service.dart:172-218`

```dart
int maxUpdatedAt = since;
for (final notify in notifications) {
  final bool success = await syncFriendPublicKey(peerUid);
  if (success) { ... } else { failedUids.add(peerUid); }   // 失败入队
  // 游标推进与 success 无关 ↓
  if (updatedAtVal != null && updatedAtVal > maxUpdatedAt) maxUpdatedAt = updatedAtVal;
}
...
if (maxUpdatedAt > since) StorageService.to.setInt('e2ee_notification_last_since', maxUpdatedAt);
```

**核实**：游标确实越过失败条目（该通知不再重放）；**但** `failedUids` 持久化（`_saveFailedSyncUids`）后，下轮 `pullKeyNotifications` 开头（137-147 行，标记 C6-γ）会重试。残余风险仅在"failedUids 持久化也失败"的极端场景，实际很低。建议保留但降级 LOW。

---

### BUG-11 🟢 LOW（确认）— `GroupMemberModel` 原地修改

**文件**：`imboyapp/lib/page/group/group_member/group_member_page.dart:75, 93`

```dart
setState(() { _memberList[idx].muteUntilMs = event.muteUntilMs; }); // :75 禁言
setState(() { _memberList[idx].muteUntilMs = null; });               // :93 解禁
```

违反项目不可变性约定（应 `copyWith` + 替换列表元素）。功能可用，属架构债。

---

## 二、权衡 / 可观测性（可选优化，非缺陷）

### BUG-07 🟡 MEDIUM（原 HIGH，降级）— forceRefresh 空响应回退旧缓存

**文件**：`imboyapp/lib/service/e2ee_service.dart:546-555`，TTL `_cacheTtlMs = 30min`（34-35 行）

```dart
if (didToPem.isEmpty && forceRefresh) {
  final cached = _userKeyCacheByDevice[uid];
  if (cached != null && cached.isNotEmpty && !_isCacheExpired(...)) { // 受 TTL 门槛
    return _userKeyResult(uid, cached);                              // 回退旧缓存
  }
}
```

回退**带未过期门槛**，并非无条件。这是有意修复（API 瞬时空响应 vs 真吊销无法区分时优先抗抖动）。若要"吊销即时生效"，需后端用显式 `revoked` 标志区分"空=吊销"与"空=异常"，而非靠空列表推断。优先级低。

### N-02 🟡 — 通知点击路由未校验 peerId 格式
`imboyapp/lib/service/notification.dart:127` `final path = '/chat/$peerId?type=$chatType';` 直接拼接。输入源为受控本地 payload（经 sealed 解析层 + FormatException 守卫），威胁面窄。建议进路由前加 TSID（纯数字）正则校验做纵深防御。

### S-02 🟡 — switchS2C 异常处理（机制需更正）
`imboyapp/lib/service/message_s2c.dart:343-346`：`_sendS2CAck` 在 try 内、switch 之后；handler `throw` 会**跳过** ACK（与前版"异常仍 ACK"描述相反）。真实可改进点：catch 仅 `iPrint` 静默吞异常，且"不抛异常的逻辑失败"仍会 ACK。建议对关键 action 失败时显式不 ACK + 上报。

### S-03 🟡 — `_handleE2EEDeviceKeyChanged` 仅清缓存不主动重拉
`imboyapp/lib/service/message_s2c.dart:810-827`：`clearUserKeyCache(uid)` 清除完整（三份），靠下次发送惰性重取。单聊场景够用；如需即时一致可追加 `syncFriendPublicKey(uid)` 预热。

---

## 三、已确认安全（无需动）

- **SAFE-01 频道语音播放无路径注入**：`channel_message_item.dart:758-761` → `IMBoyCacheManager.getSingleFile`，内部 scheme 白名单（非 http/https 抛 `Security Block`）+ `AssetsService.viewUrlAsync` HMAC 重授权 + 结构化 cacheKey，`widget.uri` 来源为服务端 payload。安全。
- **SAFE-03 订阅无内存泄漏**：`group_member_page.dart:49-50` 声明，`107-108` dispose cancel。
- **打字状态节流正确**：`chat_input.dart` 300ms debounce → `chat_page.dart:_handleInputChanged` 3s 节流（`typing_indicator_rules.dart`）+ 5s idle 自动 stop，最多每 3s 一个 `typing.start`，无泛洪。
- **会话计数并发安全**：`conversation_repo_sqlite.dart` 计数走 SQL 原子自增，写入经 `exclusive` 事务串行化；Dart 单线程事件循环 + sqflite 事务队列，不存在前版所述 TOCTOU。

---

## 四、修复补丁（仅针对真实缺陷）

### Patch BUG-05：实现真正的密钥重建（替换 `chat_page.dart:2102-2129`）

```dart
/// 重新创建 E2EE 密钥对并上传公钥
Future<void> _refreshE2EEKeys() async {
  try {
    EasyLoading.show(status: t.chat.e2eeRecreatingKey);
    // 1. 真正生成新 RSA 密钥对（私钥落 secure storage，返回含公钥/kid）
    final keyInfo = await E2EEKeyService.generateKeyPair();
    // 2. 上传新公钥注册到后端（用项目既有 E2EE 上传 API）
    final ok = await E2EEKeyService.uploadDevicePublicKey(keyInfo);
    if (!ok) throw const E2EEKeyUploadException();
    // 3. 清本机内存公钥缓存，强制下次重拉对端公钥
    E2EEService.clearCache();
    await StorageService.to.remove('e2ee_key_refresh_time');
    EasyLoading.dismiss();
    EasyLoading.showSuccess(t.chat.e2eeKeyRecreated);
  } on E2EEKeyUploadException {
    EasyLoading.dismiss();
    EasyLoading.showError(t.common.e2eeKeyRecreationFailed(error: 'upload'));
  } on Exception catch (e) {
    EasyLoading.dismiss();
    EasyLoading.showError(t.common.e2eeKeyRecreationFailed(error: e.toString()));
  }
}
```
> 注：`uploadDevicePublicKey` 需对齐 `e2ee_key_service.dart` 既有上传方法签名（落地前先确认实际方法名/返回类型）。**重建密钥前应提示用户：换新密钥对后，仅用旧密钥加密给本设备的历史密文将无法再解（这是 E2EE 语义，非缺陷）**——可在对话框补一句风险说明。

### Patch BUG-11：改为不可变更新（替换 `group_member_page.dart:74-76 / 92-94`）

```dart
// 禁言事件
setState(() {
  _memberList = [
    for (final m in _memberList)
      m.userId.toString() == event.userId
          ? m.copyWith(muteUntilMs: event.muteUntilMs)
          : m,
  ];
});
// 解禁事件：copyWith(muteUntilMs: null)（需 copyWith 支持显式置空，否则加 clearMuteUntil 标志）
```

### Patch BUG-06（可选）：游标仅在全部成功时整体推进，或对失败条目回退游标
最简方案是维持现状（failedUids 已兜底）。如需更严谨：`maxUpdatedAt` 只在 `success` 时参与 max，失败条目不推进游标——但会与 failedUids 形成双重重试，需评估重复成本。**建议保留现状，不改**。

---

## 五、对前版审计的处置建议

1. 前版 `2026-06-15-flutter-core-subsystems-audit.md` 的 BUG-01/02/03/04/08/09/10 七条为误报，**不可据其打补丁**——尤其 Patch-01（给 save() 加事务，实际会与现有 `exclusive` 事务嵌套）、Patch-06（给媒体入口加 `_checkSendAllowed`，与现有 C13 门控重复）若执行将引入回归。
2. 建议将前版标注作废或删除，以本 VERIFIED 版为准。
3. 教训：审计结论必须**直读当前代码**核验行号与方法体；委托型推断（含子代理）易对已重构的代码产生"按印象/旧版本"的臆测。

---

## 六、修复落地记录（2026-06-15）

| 项 | 状态 | 改动文件 | 说明 |
|---|---|---|---|
| **BUG-05** | ✅已修 | `chat_page.dart` `_refreshE2EEKeys` + `e2ee_key_service.dart` `regenerateAndReportDeviceKey()`(新增) | 改为真正调用 `generateKeyPair()`+`reportDeviceKey()`，失败弹错误、成功才弹成功 |
| **潜在竞态** | ✅已修 | `e2ee_key_service.dart` `generateKeyPair()` | `Future.wait` 并发存私钥/写kid → 改有序串行（savePrivateKey 在 setKeyId 前），避免重建时旧私钥被错误归档 |
| **BUG-11** | ✅已修 | `group_member_model.dart`(加 copyWith) + `group_member_mute_util.dart`(新增纯函数) + `group_member_page.dart` | 原地改 Model → 不可变 `applyMemberMuteUpdate` |
| **N-02** | ✅已修 | `notification_payload_rules.dart` | 加 `isValidNotificationPeerId` TSID 校验，非法 peerId 降级 `invalid_peer_id` Skip |
| BUG-06 | ⏸不改 | — | 已有 `failedUids` 重试队列(C6-γ)兜底，非真实缺陷 |
| BUG-07 | ⏸不改 | — | 受 30min TTL 约束的抗抖动权衡；彻底解需后端 `revoked` 显式标志（超出客户端范围） |
| S-02 | ⏸不改 | — | 复核证伪：异常实际**跳过** ACK，机制已正确 |
| S-03 | ⏸不改 | — | 惰性重取在发送路径有效，主动重拉属可选优化非缺陷 |

### 测试
- `test/service/notification_payload_rules_test.dart`：+4 用例（N-02 校验器 + 解析层降级 + 合法仍放行）
- `test/page/group/group_member_mute_util_test.dart`：+9 用例（copyWith 语义 + 不可变更新 + 无匹配/空列表边界）
- 结果：**33/33 通过**；改动文件 `dart analyze` **零问题**。

### 待真机验证（无法在静态环境完成）
- **BUG-05**：换密钥后需真机回归 —— ①新公钥成功上报后端；②对端能用新公钥加密、本端能解；③历史密文经 `getPrivateKeyByKid` 仍可解（验证旧私钥归档链 C2）。

### 重建密钥对话框提示文案（已更正落地，2026-06-16）
> ⚠️ 更正前一版措辞："历史可能丢失"是**错误**的。经 research-ops（E2EE 密钥轮换最佳实践，多源对比 Signal/Matrix/Ember）核实并结合 C2 归档机制确认：
> - imboy 采用"静态密钥 + 手动轮换 + 历史保留(C2)"模型（同 Matrix 取舍：牺牲前向保密换历史可读）。
> - 因旧私钥按 kid 归档，**重建密钥不会丢历史**；"重建"的真实价值是**密钥不匹配故障恢复**，而非安全增强。
> - 已在对话框新增提示 key `common.e2eeDecryptRecreateHint`（zh-CN，其余语言 slang fallback_strategy=base_locale 自动回退）：
>   *"提示：重建后，对方需重新获取你的新密钥；重建期间未送达的加密消息可能不会自动重发。历史消息不受影响。"*（对齐 Signal "安全码变更后未送达消息不自动重发"行为）

### 后续可选（非本次范围）
- TOFU 密钥变更提示：`_handleE2EEDeviceKeyChanged` 当前静默清缓存，可补会话内"对方安全码已变更"非阻塞提示（防中间人，产品决策）。
- 战略：静态密钥模型对设备被物理查获无前向保密防护；若 ToB 威胁模型含设备取证，评估迁移 Double Ratchet（libsignal），单独立项。
