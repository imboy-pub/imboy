# E2EE-025 —— 选项 C 落地：`session_ref` 修复与 counter 语义收敛

- **会话**：`20260728-1141-claude-code`
- **仓库**：`imboyapp`（HEAD `955e27a6`，未提交）
- **依据**：提案 25 §3 选项 C（2026-07-28 人工签字）→ 本次新增 `26-supersedes-15-counter-semantics.md`
- **前置诊断**：[`E2EE-025-production-wiring-finding.md`](./E2EE-025-production-wiring-finding.md)
- **状态**：实现完成、验收通过；`22` 中 E2EE-025 的状态标记**未擅改**（见 §7）

---

## 1. 修了什么

### 1.1 P0 —— 生产 C2C Olm v3 消息整条不可读

`chat_network_service.dart` 传 `sessionRef: ''`，注释称「OlmProtocol 内部填充
session_id」——**该填充不存在**。真实 `OlmProtocol.encrypt` 只把 `session_id`
写进 `protocol_metadata`；而 `_validateContextBinding` §7 硬比对
`protocol_metadata.session_id == protected_header.session_ref`，
于是每条消息都被判 `context_mismatch_session_id`。

### 1.2 P2 —— 修 P0 会立刻引爆的次生故障

`session_ref` 一旦非空，接收侧原本被整段跳过的序列检查就会**激活**；
而 `epoch_or_counter` 恒 0、检查是严格单调（`sequence <= lastSeq → 拒绝`），
`0 <= 0` 成立 → **首条合法消息即被判 replay**。

因此选项 C 的两半（填 `session_ref` + 取消 Olm/Megolm 序列检查）
**必须同批落地**，单独做任何一半都会让 C2C 全线不可读。提案 25 §4 已明确
「不接受『只修 session_ref 不动 counter』」，本次实现遵此执行。

---

## 2. 循环依赖与方案 A

`ProtectedFrameV3.buildProtectedHeader` 需要 `sessionRef` 作**入参**，
其产物 header 要先进 inner_frame 才能被 `protocol.encrypt` 加密；
而 Olm session id 只在 `OlmSessionService.encryptC2CMessage` **内部**
`_loadSession ?? _establishOutboundSession` 之后才产生。
`E2eeSessionProtocol` 接口被 ADR 02 §10 冻结，不能加方法取该值。

人工拍板采用**方案 A**：在 `OlmSessionService` 单独暴露 `ensureSessionId/2`，
与 `encryptC2CMessage` 共用同一把 per-device 锁与同一套 load-or-establish 逻辑。

### 2.1 实现期发现的陷阱（差点引入更严重的 bug）

`_establishOutboundSession` **只返回 session，既不写 `_sessions` 缓存也不落库**
（持久化由其调用方 `encryptC2CMessage` 负责）。若 `ensureSessionId` 照抄
`_loadSession ?? _establishOutboundSession` 而不持久化，会造成：

1. 新建的 session 被丢弃；
2. 随后的 `encryptC2CMessage` 再次 `_establishOutboundSession` →
   **再 claim 一个对端 OTK**（无谓消耗，助长 OTK 耗尽，正是 E2EE-062 要防的）；
3. 拿到**另一个** session id → 与已写进 header 的 `session_ref` 必然不符 →
   每条首发消息仍被拒。

修正：`ensureSessionId` 在 establish 分支后**立即 `_persistSession`**。
该不变量由 §4 的真实 vodozemac 测试守护。

### 2.2 已知取舍（人工已知情并接受）

- **竞态窗口**：`ensureSessionId` 与随后 `encrypt` 之间若会话被替换，
  header 的 `session_ref` 与实际加密所用 session 不一致 →
  接收侧 fail-closed 拒收该条消息（可用性抖动，非绑定弱化）。
- **网络时序**：对某设备**首次**发消息时，claim prekey 发生在
  `ensureSessionId` 内，比改动前提前一步。

---

## 3. 根因层守卫（不只修一个调用点）

只把 `chat_network_service` 改对，无法阻止下一个调用点重犯。
因此在 `ProtectedFrameV3.buildProtectedHeader`——**所有** PFv3 header 的唯一
构造处——对空 `sessionRef` fail-closed 抛 `ArgumentError`。
ADR 15 §3.1 本就把该字段定义为 `text, 1..256 字节`（非空），
这条守卫只是把冻结约束变成可执行的。

---

## 4. 变更清单

| 文件 | 变更 |
|---|---|
| `lib/service/e2ee/protected_frame_v3.dart` | `buildProtectedHeader` 对空 `sessionRef` fail-closed |
| `lib/service/olm_session_service.dart` | 新增 `ensureSessionId/2`（锁内 load-or-establish + **立即持久化**） |
| `lib/page/chat/chat/services/chat_network_service.dart` | 传 `ensureSessionId` 返回值，删除 `sessionRef: ''` 及其错误注释 |
| `lib/service/e2ee_service.dart` | 接收侧移除 Olm/Megolm 序列检查（选项 C）；MLS 分支显式 `mls_not_implemented`；新增 `CryptoStoreUnavailableException → crypto_store_unavailable` 分类 |
| `lib/service/e2ee/crypto_store.dart` | 新增 `CryptoStoreUnavailableException`；`checkAndUpdateSequence` 存储故障改为抛该异常，不再伪装成重放（提案 §5.2）；补「MLS 不得直接复用本严格单调实现」的警示 |

测试：

| 文件 | 变更 |
|---|---|
| `test/service/e2ee/production_session_ref_wiring_test.dart` | **新增** 5 项：RC-01a/01b/02/04/04b |
| `test/service/e2ee/olm_pfs_production_path_test.dart` | **新增** 3 项：真实 vodozemac 下 `ensureSessionId` 与 encrypt 的会话一致性、幂等性、不破坏 ratchet |
| `test/service/e2ee/replay_counter_epoch_test.dart` | **重写** 2 项断言到选项 C 语义（未删除、未 skip，见 §6） |
| `test/service/e2ee/crypto_store_test.dart` | **新增** 1 项：存储不可用时抛 `CryptoStoreUnavailableException` |

---

## 5. 验收

### 5.1 RED → GREEN

诊断阶段（改生产代码前）：

```
RC-01: Expected: non-empty / Actual: ''
RC-02: _e2ee_reason: context_mismatch_session_id
```

修复后：

```
$ flutter test test/service/e2ee/production_session_ref_wiring_test.dart
  All tests passed!   (5)

$ flutter test test/service/e2ee/olm_pfs_production_path_test.dart
  All tests passed!   (8，含真实 vodozemac + 真实 SQLite)

$ flutter test test/service/e2ee/crypto_store_test.dart
  All tests passed!   (24)

$ flutter test test/service/e2ee/replay_counter_epoch_test.dart
  All tests passed!   (4)
```

### 5.2 提案 §6 验收表

| ID | 用例 | 结论 | 证据 |
|---|---|---|---|
| RC-01 | 生产 header 的 `session_ref` 非空且等于实际协议会话标识 | ✅ | RC-01a（空值 fail-closed）+ RC-01b（相等）+ 真实 Olm 一致性 3 项 |
| RC-02 | 首条合法消息必须被接受 | ✅ | 可用性回归门，绿 |
| RC-03 | 同一密文重放 100 次业务只提交一次 | ⚠️ 未新增 | 由既有 `message_id` dedupe 守护（`crypto_inbox_dedupe`，E2EE-027 已有测试）；本次未补 100 次压力用例 |
| RC-04 | 离线批量 + 乱序投递全部可读，0 误判 | ✅ | 50 条乱序全读 + 递减 counter 不拒 |
| RC-05 | DB 故障期间分类为 `crypto_store_unavailable` | ✅ | crypto_store 层新增用例 + 既有 `decrypt_error_taxonomy_test` |
| RC-06 | 选 B 时的窗口边界 | N/A | 选项 C 不适用 |

### 5.3 全量回归

```
$ flutter test test/service/e2ee/
  All tests passed!   313        （基线 304 → 313，新增 9，无一被打破）

$ dart analyze lib
  1 issue found.   仅 component/ui/ios_settings_ui.dart 既有 info，与本任务无关

$ dart format --set-exit-if-changed <9 个改动文件>
  Formatted 9 files (2 changed)   → 已格式化后复跑通过
```

更大范围（`test/service/` + `test/integration/`）：`1594 passed / 33 failed`。
33 项失败全部落在 5 个 UI 流程文件（collect / moment feed / moment publish /
contact tag / group tag），失败原因为 widget finder 失配
（`Found 0 widgets with type "RefreshIndicator"`、`Found 0 widgets with icon ...`），
与 E2EE、Olm、chat_network_service、crypto_store、protected_frame_v3 均无关，
属预存 UI 测试漂移。

---

## 6. 关于「重写而非删除」既有测试

`replay_counter_epoch_test.dart` 原有两项断言
「重复/较小的 `epoch_or_counter` → `replay_detected`」**已被选项 C 明确废止**。

这不是「为了变绿改测试」：

1. 旧断言之所以能过，是测试**手工递增了 counter**——生产恒 0，
   按旧语义生产环境**每一条**消息都会被判重复；
2. 选项 C 的核心论证就是该层与 `message_id` dedupe 功能重叠，
   而其可用性风险（离线批量 + 乱序误杀）高于安全收益；
3. 两项用例**未删除、未 skip**，而是改为守护新语义（不得误判 replay），
   并在文件头写明废止理由与出处。

---

## 7. 残留风险与未决项

1. **`22` 中 E2EE-025 的状态标记未改**。提案 25 §7 第 4 项（`PASS` 是否回退）
   仍未签字，且 `22` §3 状态机不含 `PASS -> PARTIAL` 转换。需人工裁定。
2. **RC-03 未补 100 次重放压力用例**（见 §5.2）。
3. **竞态窗口与 claim 时序变化**（§2.2）为方案 A 的固有代价，已知情接受；
   真机验证未做（本会话无真机与凭证）。
4. **`CryptoStore.checkAndUpdateSequence` 目前无生产调用方**。保留供 MLS，
   但 ADR 26 §2.4 已明确 **MLS 不得直接复用**（严格单调 ≠ 滑动窗口）。
   若 MLS 迟迟不落地，应考虑删除以免腐化。
5. **连带建议复核 E2EE-012 / E2EE-024 的 `PASS` 判定**：它们的验收对象正是
   `_validateContextBinding` 与 mutation matrix，可能同样建立在
   「测试内手工对齐 sessionRef」而非生产 wiring 之上。本会话未复核。
6. **未 commit、未 push、未部署、未访问生产、未通知第三方。**
