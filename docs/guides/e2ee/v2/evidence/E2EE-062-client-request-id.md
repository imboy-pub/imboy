# E2EE-062 第四刀：客户端发送 `request_id`

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第四刀（残留 ①）
  - 第一刀 `E2EE-062-otk-claim-idempotent-lease.md`（单设备幂等租约）
  - 第二刀 `E2EE-062-per-target-throttle.md`（目标级限流）
  - 第三刀 `E2EE-062-batch-claim-idempotency.md`（batch 幂等）
- **会话**：`20260728-2200-claude-code`
- **仓库**：`imboyapp`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）

---

## 1. 做了什么

前三刀把幂等租约铺满了服务端的两条 claim 路径。但客户端**从不发送
`request_id`** —— `OlmApi.claimKey` 的请求体里只有 `target_uid` 与 `device_id`。
服务端 `normalize_request_id/1` 拿到缺省值 `<<>>`，走的是旧的逐次消费分支。
**服务端三刀的全部工作在生产流量上等于零。**

实际的重试向量：`MessageRetry` 的 3/5/10/20s 退避重发 → `encryptC2CMessage`
→ `_loadSession` 返回 null → `_establishOutboundSession` → 再 claim 一次。
每一次退避都真实消费一条对端 one-time prekey。

| 接缝 | 改动 |
|---|---|
| `lib/service/e2ee/olm_claim_request_id.dart`（新） | 幂等键的铸造与生命周期 |
| `lib/store/api/olm_api.dart` | `claimKey` 新增可选 `requestId`；抽出 `buildClaimBody` 作为可验收接缝 |
| `lib/service/olm_session_service.dart` | `_establishOutboundSession` 首尾 `issue` / `complete` |

无新依赖（`dart:math` 的 `Random.secure()`）、无协议变更（`request_id` 是可选字段）、
无 schema 变更。

### 1.1 关键取舍：幂等键的作用域是**一次建会话尝试**，不是「一对设备」

直觉方案是用 `peerUid:peerDeviceId` 派生一个恒定 id，重试天然命中。**没有采用**：

服务端租约会**恒返回同一条已消费的 OTK**。于是该对端此后所有会话——包括会话被
销毁、ratchet 重置后重建的——都会复用同一条 one-time prekey。
one-time prekey 的一次性被破坏，比「重试多消费一条」严重得多。

采用的方案：进程内挂起一个随机 id，**成功建出会话后立即丢弃**。

- 重投命中挂起的 id → 服务端命中租约 → 池不再减少（幂等生效）；
- 成功后换新 id → 新会话消费新 OTK（一次性不被破坏）；
- 进程重启后挂起表丢失 → 重投消费一条新 OTK，**与今天的行为一致，无回归**，
  且方向在安全那一侧（宁可少去重，绝不多去重）。

`complete` 的调用点选在 `createOutboundSession` 成功之后、**而非持久化成功之后**，
同样是这个方向：持久化失败时重建会多消费一条 OTK（即今天的行为），但绝不会让
两条不同的会话共用同一条 prekey。

### 1.2 id 不从 uid / device_id 派生

`claim_request_id` 会在服务端落库并保留到审计保留期。派生等于把对端标识多写进
一列；128 bit 随机 hex（32 字符，服务端 `varchar(64)` 内）同样唯一标识一次尝试。

---

## 2. RED 记录

新增 `test/service/e2ee/olm_claim_request_id_test.dart`（8 例）。
先只落**空实现载体**（`issue` 返回 `''`、`buildClaimBody` 忽略 `requestId`），
使 RED 是行为失败而非「文件不存在」的编译错误。

```
00:00 +3 -5: Some tests failed.
```

**5 红均为行为失败**：

| 用例 | 失败形态 |
|---|---|
| `首次 issue 必须给出一个非空 id` | 得 `''` |
| `正向可用性：成功建会话后必须换新 id` | `''` == `''` |
| `不同对端设备的 id 互不相同` | 3 个 id 去重后只剩 1 个 |
| `id 必须落在服务端白名单内` | `id=""` 不合规 → 会被服务端静默降级为空 |
| `request_id 非空时必须进入请求体` | `body['request_id']` 为 `null` |

### 2.1 harness 自评：一条「假绿」及其对策

`同一次尝试的重投拿到同一个 id` 在空实现下**恒真**（`'' == ''`），
RED 阶段是绿的。**它单独不构成守护**。
`首次 issue 必须给出一个非空 id` 正是为此而设——两条合起来才排除「根本没有 id」。
写进本节是为了避免后来者误读那条绿。

**对照组**：`白名单谓词本身：越界字符与超长必须被判不合规` 打的是已实现的纯谓词
`isServerAcceptable`，改前改后都绿 → harness 本身没坏。

### 2.2 「只验拒收」反模式的规避

`正向可用性：成功建会话后必须换新 id` 是本刀的核心正向用例：一个「恒定 id」的
实现在幂等指标上恒得满分，被这条否掉（见 §1.1）。
`request_id 为空时不得写入该键` 守护旧语义零破坏。

---

## 3. 生产调用方核实

```
lib/service/olm_session_service.dart:517  OlmApi().claimKey(...)   ← 唯一 claim 调用方
lib/config/const.dart:266                 olmClaimKey = '/api/v1/e2ee/olm/claim'
```

`grep -rn "olm/claim\|batch_claim\|claimOneTime\|claimKey" lib/` 全仓只有这一处
调用点，链路为
`MessageRetry 退避重发 → encryptC2CMessage → _loadSession(null) →
 _establishOutboundSession → OlmApi.claimKey → POST /api/v1/e2ee/olm/claim`。
`buildClaimBody` 是 `claimKey` 真正用来构造请求体的函数，不是平行实现，
也不是读源码的结构断言。

⚠️ 客户端**没有** batch_claim 调用方（服务端第三刀的 batch 幂等目前无客户端流量）。
已实证（上述 grep 全仓零命中）。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/olm_claim_request_id_test.dart
  All 8 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (345 passed；上一轮基线 337，本刀 +8)

$ flutter test test/service/
  All tests passed!   (1225 passed)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）
```

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **端到端未实证** —— 本刀证明了 `request_id` 进入请求体、且生命周期正确；
   **未**在真实网络上跑通「重投 → 服务端命中租约 → 池不减少」的完整闭环。
   服务端半边已在真 PG 实证（第一/三刀），客户端半边已在单测实证，
   但两半拼接**只有文件级论证**。
   **认识论状态：分段已实证，端到端未实证。**
2. **进程重启后重投仍消费新 OTK** —— 挂起表只在进程内。这是**有意识的取舍**
   （见 §1.1），不是遗漏；持久化它需要权衡「跨重启复用同一条 prekey」的风险。
3. **低水位补传与耗尽告警缺失** —— 「限流只拖慢、靠补传恢复」的前提，目前不成立。
   注：`OlmApi.countPrekeys` 目前是**桩实现**（恒返回 0，注释自承「需后端补 count
   端点」），补传链路实际不完整。**已实证**（读该函数体）。
4. **客户端无 batch_claim 调用方** —— 服务端第三刀的 batch 幂等暂无生产流量
   （见 §3）。不是缺陷，是范围事实。
5. 租约无独立 TTL；fallback prekey 未在服务端验签；
   「耗尽/限流绝不触发 RSA/Megolm/明文」无守护用例；
   单租户/全局两层限流未做；`olm_claim` 门仍朴素写法；60/min 未压测校准。
6. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| `request_id` 进入 claim 请求体 | **已实证**（`buildClaimBody` 是生产构造函数） |
| 同一次尝试的重投拿同一个 id | **已实证** |
| 成功后换新 id（不恒定去重） | **已实证** |
| id 合乎服务端白名单（50 次抽样） | **已实证** |
| 空 id 时旧语义零破坏 | **已实证** |
| `claimKey` 是全仓唯一 claim 调用方 | **已实证**（全仓 grep） |
| 客户端无 batch_claim 调用方 | **已实证**（全仓 grep 零命中） |
| 「重投不消费新 OTK」端到端成立 | **分段已实证，端到端未实证**（见 §5.1） |
| 「OTK 抗耗尽」整体达成 | **不成立** —— 见 §5.3 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增依赖。
