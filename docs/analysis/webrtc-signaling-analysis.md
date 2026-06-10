# WebRTC 信令稳定性分析 / WebRTC Signaling Stability Analysis

> 生成时间 / Generated: 2026-05-27  
> 分析范围 / Scope: 后端 Erlang + Flutter 客户端  

---

## 1. 信令架构总览 / Signaling Architecture Overview

```
Flutter A                后端信令服务器              Flutter B
─────────               ─────────────────            ─────────
    │── WEBRTC_OFFER ──►│ webrtc_ws_logic.erl        │
    │                   │ message_router_logic.erl   │
    │                   │ message_ds:send_next/4     │
    │                   ├──────────────────────────►│
    │◄─────────── WEBRTC_ANSWER ─────────────────────│
    │── WEBRTC_CANDIDATE ──────────────────────────► │
    │◄──────────── WEBRTC_CANDIDATE ─────────────────│
    │                   [P2P 媒体流直连]              │
    │── WEBRTC_BYE ────────────────────────────────► │
```

**核心结论**：后端是**纯转发**模式，不持久化 WebRTC 信令内容，不参与 SDP 协商，仅检查好友关系和黑名单后转发。

---

## 2. ICE 候选收集和交换流程 / ICE Candidate Collection & Exchange

### ✅ 消息格式（Flutter → 后端 → Flutter）

**顶层消息格式**（WebSocket JSON）：
```json
{
  "type": "webrtc_offer",   // 或 webrtc_answer / webrtc_candidate / webrtc_bye / webrtc_busy
  "id": "<msgId>",
  "from": "<uid>",
  "to": "<uid>",
  "payload": { ... }
}
```

**Offer payload**：
```json
{
  "sd": { "sdp": "v=0\r\n...", "type": "offer" },
  "media": "video"  // 或 "audio"
}
```

**Answer payload**：
```json
{
  "sd": { "sdp": "v=0\r\n...", "type": "answer" },
  "media": "video"
}
```

**Candidate payload**：
```json
{
  "candidate": {
    "sdpMLineIndex": 0,
    "sdpMid": "0",
    "candidate": "candidate:4234997325 1 udp 2043278322 ..."
  }
}
```

### ✅ ICE 服务器配置

后端通过 `/api/user/webrtc_credential` 下发动态 TURN 凭证：

```erlang
% user_ds.erl:webrtc_credential/1
Username = <<TmBin/binary, ":", UidBin/binary>>,  % 格式: "过期时间:uid"
Credential = base64:encode(crypto:mac(hmac, sha, Secret, Username)),
% 有效期 86400 秒（24 小时）
```

Flutter 客户端 ICE 服务器配置（`p2p_call_screen_provider.dart:_getIceConf`）：
```dart
'iceServers': [
  {'urls': stun_urls},                          // 项目 STUN
  {'urls': 'stun:stun.l.google.com:19302'},     // Google STUN 备用
  {'urls': turn_urls_udp, ...credentials},      // TURN UDP
  {'urls': turn_urls_tcp, ...credentials},      // TURN TCP（UDP 封锁时降级）
],
"iceCandidatePoolSize": 10,
"iceTransportPolicy": "all",
"bundlePolicy": "balanced",
"sdpSemantics": "unified-plan",
```

### ✅ Candidate 乱序缓冲

```dart
// _receiveCandidate: 如果 RemoteDescription 尚未设置，先缓存
if (description != null) {
  await s.pc?.addCandidate(candidate);
} else {
  s.remoteCandidates.add(candidate);  // 缓存，等 offer 设置后批量添加
}

// createSession 中处理缓存的候选
if (s2.remoteCandidates.isNotEmpty) {
  for (var candidate in s2.remoteCandidates) {
    await s2.pc?.addCandidate(candidate);
  }
  s2.remoteCandidates.clear();
}
```

---

## 3. ICE 重连触发条件和重连逻辑 / ICE Reconnection Logic

### ✅ ICE 连接状态监听

```dart
pc.onIceConnectionState = (RTCIceConnectionState state) {
  switch (state) {
    case RTCIceConnectionStateConnected:
    case RTCIceConnectionStateCompleted:
      _iceRestartCount = 0;         // 连接成功，重置重试计数
      _iceDisconnectTimer?.cancel();
      updateConnected(true);
      break;

    case RTCIceConnectionStateDisconnected:
      // 等待 5 秒后尝试重连（网络抖动容忍期）
      _iceDisconnectTimer = Timer(Duration(seconds: 5), () {
        if (pc.iceConnectionState == Disconnected) {
          _attemptIceRestart();
        }
      });
      break;

    case RTCIceConnectionStateFailed:
      _attemptIceRestart();         // 立即尝试重启
      break;

    case RTCIceConnectionStateClosed:
      _iceDisconnectTimer?.cancel();
      break;
  }
};
```

### ✅ ICE 重启（带重试次数限制）

```dart
static const int _maxIceRestarts = 3;  // 最多重试 3 次

void _attemptIceRestart() {
  if (_iceRestartCount < _maxIceRestarts) {
    _iceRestartCount++;
    currentSession!.pc!.restartIce();  // 触发 ICE 重启
  } else {
    // 超过重试次数，通知通话结束
    onCallStateChange?.call(session!, WebRTCCallState.callStateBye);
    updateStateTips(t.common.errorNetwork);
  }
}
```

### ⚠️ ICE 重启后信令协商缺失

`pc.restartIce()` 会触发 `onRenegotiationNeeded` 回调，调用方（caller）会重新发起 Offer：

```dart
pc.onRenegotiationNeeded = () async {
  if (caller) {
    _createOffer(msgId, media);  // ✅ caller 侧会重新发 Offer
  }
};
```

⚠️ **问题**：当 ICE 重启发生在 **被叫方（callee）** 侧时，`caller == false`，`onRenegotiationNeeded` 不会触发重新 Offer，被叫方依赖主叫方主动发起，若主叫方未感知到被叫方 ICE 失败，重连无法完成。

---

## 4. 通话断开/超时处理 / Call Disconnect & Timeout

### ✅ 主叫方超时（无人接听）

```dart
// CallTimeoutConfig.answerTimeout = 60 秒
void startAnswerTimer(VoidCallback onTimeout) {
  _answerTimer = Timer(
    Duration(seconds: CallTimeoutConfig.answerTimeout),
    onTimeout,  // 60 秒无回应，触发超时回调（挂断并发 BYE）
  );
}
```

### ✅ 通话计时

```dart
void startCallTimer(void Function() onUpdate) {
  _callTimer = Timer.periodic(Duration(seconds: 1), (timer) {
    _callSeconds++;
    state = state.copyWith(callDuration: '${mm}:${ss}');
    onUpdate();
  });
}
```

### ✅ BYE 信令

主动挂断：
```dart
void sendBye(String msgId) {
  sendWebRTCMsg('bye', {'sid': currentSession.sid}, msgId: msgId, to: peerId);
  _closeSession(webRTCSessions[currentSession.sid]!);
}
```

被动收到 BYE（`onMessageP2P`）：
```dart
case 'bye':
  final s2 = webRTCSessions.remove(sid);
  onCallStateChange?.call(s2, WebRTCCallState.callStateBye);
  _closeSession(s2);
```

### ✅ BUSY 信令

来电时已在通话中：
```dart
// message_webrtc.dart:handleWebRTC
if (['WEBRTC_BUSY', 'WEBRTC_BYE'].contains(type)) {
  for (var id in webrtcMsgIds) {
    changeLocalMsgState(id, 4);  // 状态4=结束/忙碌
  }
  webrtcMsgIds.clear();
  navigatorKey.currentState?.pop();
  gTimer?.cancel();
}
```

### ⚠️ 无网络超时无信令 BYE

当网络完全断开时（非 ICE Disconnected，而是 WebSocket 断开），对方无法收到 BYE 信令，通话会话将悬空直到 `_iceDisconnectTimer`（5秒）+ ICE 重试（3次）耗尽后才通知 `callStateBye`，对端可能长时间处于等待状态。

---

## 5. 后端信令转发逻辑 / Backend Signaling Relay Logic

### ✅ 纯转发（不存储）

```erlang
% message_router_logic.erl
<<"webrtc_", _Event/binary>> ->
    webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, OriginalMsg);
```

```erlang
% webrtc_ws_logic.erl:event/4
event(CurrentUid, ToUid, MsgId, Msg) ->
    IsFriend = friend_ds:is_friend(ToUid, CurrentUid),
    InDenylist = user_denylist_logic:in_denylist(ToUid, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            MsLi = [0],  % 不重试，立即投递一次
            message_ds:send_next(ToUid, MsgId, Msg, MsLi),
            ok;
        {_, InDenylist2} when InDenylist2 > 0 ->
            % 返回 in_denylist 错误
        {false, _} ->
            % 返回 not_a_friend 错误
    end.
```

### ✅ 安全校验

- 好友关系验证（`friend_ds:is_friend`）
- 黑名单检查（`user_denylist_logic:in_denylist`）

### ❌ WebRTC 信令不持久化

`MsLi = [0]`：只投递一次（Delay=0），不重试。若目标用户**离线**，信令消息**丢失**：

- 离线 OFFER → 对方上线后收不到来电，呼叫无效
- 离线 CANDIDATE → ICE 候选丢失，连接可能失败

### ❌ 无信令离线暂存

普通 C2C 消息使用 `MsLi = [0, 5000, 10000, ...]` 多次重试并有离线存储，但 WebRTC 信令的 `MsLi = [0]`，**没有离线暂存和重传机制**。

### ⚠️ Protobuf 编解码覆盖不完整

```erlang
% imboy_codec.erl
msg_direction_to_enum(<<"webrtc_offer">>) -> 'WEBRTC_OFFER';
msg_direction_to_enum(<<"webrtc_answer">>) -> 'WEBRTC_ANSWER';
msg_direction_to_enum(<<"webrtc_candidate">>) -> 'WEBRTC_CANDIDATE';
% ⚠️ 缺少: webrtc_bye, webrtc_busy, webrtc_ringing
```

Proto 定义中有 `WEBRTC_BYE = 13`，但 codec 映射中无对应 `msg_direction_to_enum(<<"webrtc_bye">>)` 条目。若启用 Protobuf 编解码传输，BYE/BUSY/RINGING 信令可能回退到 JSON 或失败。

---

## 6. TURN 凭证安全 / TURN Credential Security

### ✅ 短时效凭证（HMAC-SHA1）

- 凭证格式：`Username = "timestamp:uid"`，HMAC-SHA1 签名
- 有效期：24 小时
- secret 为空时拒绝生成凭证并返回错误

### ⚠️ TURN secret 未配置时的降级

```erlang
{[_ | _], <<>>} ->
    #{
      <<"error">> => <<"eturnal_secret_not_configured">>,
      <<"stun_urls">> => StunUrls  % 仅返回 STUN，无 TURN
    };
```

Flutter 客户端处理：
```dart
if (turnCredential.isEmpty && from == 'openCallScreen') {
  EasyLoading.showError(t.common.failedRequestPleaseCheckNetwork);
  return null;
}
```

⚠️ **问题**：当后端返回 `{"error": "...", "stun_urls": [...]}` 时，Flutter 侧 `turnCredential.isEmpty` 判断为 `false`（Map 有内容），但 `turn_urls` 为空，ICE 配置中无 TURN 服务器，仅依赖 STUN，在严格 NAT 环境下通话会失败。

---

## 7. 问题清单 / Issue List

| # | 状态 | 描述 |
|---|------|------|
| 1 | ✅ 已实现 | offer/answer/candidate 消息格式完整（sdp + type + media） |
| 2 | ✅ 已实现 | ICE candidate 乱序缓冲（`remoteCandidates` 列表） |
| 3 | ✅ 已实现 | ICE Disconnected 5 秒延迟重试（网络抖动容忍） |
| 4 | ✅ 已实现 | ICE Failed 立即重试，最多 3 次后挂断 |
| 5 | ✅ 已实现 | TURN UDP + TCP 双通道（UDP 封锁降级） |
| 6 | ✅ 已实现 | TURN 凭证 HMAC-SHA1，24 小时有效期 |
| 7 | ✅ 已实现 | 好友/黑名单鉴权后转发信令 |
| 8 | ✅ 已实现 | 主叫方 60 秒无人接听超时挂断 |
| 9 | ✅ 已实现 | 来电时已在通话中发送 BUSY 信令 |
| 10 | ✅ 已实现 | 发送方 peer 安全校验（防止信令伪造） |
| 11 | ⚠️ 待改进 | ICE 重启时被叫方（callee）不主动重新协商，依赖主叫方感知 |
| 12 | ⚠️ 待改进 | TURN 未配置时返回 error Map，Flutter 侧 `isEmpty` 判断失效，可能静默降级为仅 STUN |
| 13 | ⚠️ 待改进 | WebSocket 断开时对端无法收到 BYE，悬空等待约 15 秒（5s + 3次ICE）后才挂断 |
| 14 | ❌ 缺失 | WebRTC 信令不持久化，目标用户离线时 OFFER/CANDIDATE 丢失，无离线来电通知（APNs/FCM 推送） |
| 15 | ❌ 缺失 | Protobuf codec 缺少 `webrtc_bye`/`webrtc_busy`/`webrtc_ringing` 枚举映射 |
| 16 | ❌ 缺失 | 无通话超时保护：接通后若双方均不主动挂断，通话可无限持续，无最大时长限制 |

---

## 8. 改进建议 / Recommendations

**高优先级**：
1. **离线来电推送**：在 `webrtc_ws_logic` 中检测目标用户是否在线，离线时通过 APNs/FCM 推送来电通知（含 `msgId`），用户上线后 App 可从推送恢复来电 UI。
2. **Protobuf codec 补全**：在 `imboy_codec.erl` 中添加 `webrtc_bye`、`webrtc_busy`、`webrtc_ringing` 的 `msg_direction_to_enum` 映射。

**中优先级**：
3. **TURN 凭证降级修复**：Flutter 侧 `_getIceConf` 在检测到 `turnCredential['error']` 非空时，应显式提示用户并返回 `null`，不要静默使用无 TURN 的配置。
4. **最大通话时长**：在 `P2pCallScreenNotifier` 中增加最大通话时长（如 3 小时）的 Timer，防止意外无限通话。

**低优先级**：
5. **被叫方 ICE 重启**：当 `callee` 侧发生 ICE Failed 时，发送 `webrtc_candidate`（ICE restart offer）给主叫方，主动触发重新协商。
