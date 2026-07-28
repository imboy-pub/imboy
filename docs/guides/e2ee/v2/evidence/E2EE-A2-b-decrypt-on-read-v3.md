# A2-b：客户端 decrypt-on-read v3 接线

- **Slice**：`22-claude-code-execution-state.md` §1.1 自动推进队列第 2 项
- **会话**：`20260728-1810-claude-code`
- **仓库**：`imboyapp`（单仓；本 Slice 不改后端）
- **状态**：`PASS`
- **依赖**：A2-a ✅（`evidence/E2EE-A2-a-offline-sender-did.md`）
- **上游依据**：`evidence/E2EE-012-024-025-029-reacceptance.md` §6.1.2

---

## 1. 做了什么

IMBoy 有**两条**解密路径：

| 路径 | 存储状态 | 解密入口 | v3 接线 |
|---|---|---|---|
| 实时 WS | 存明文（收到即解密再落库） | `message.dart::_handleE2EEMessage` → `decryptInboundV3` | 上一轮已接 |
| 离线拉取 | 存**密文**（decrypt-on-read） | `message_model_mapper.dart::toTypeMessage()` | ❌ **本轮接** |

离线路径此前把**外层 payload** 当密文传给 `decryptE2EEMessage`，而 v3 的外层
payload **恒为空串**（真密文在 `e2ee.devices[<did>].ciphertext`）——传错了输入，
任何协议都解不出明文。A2-a 已在后端补上 `sender_did` 的持久化；本轮补客户端的
承载点与分流。

| # | 接缝 | 改动 |
|---|---|---|
| 1 | 本地 schema | SQLite v24 → **v25**：`msg_c2c` 加 `sender_did TEXT`。`upgrade.sql`（存量升级）与 `baseline_schema.sql`（全新安装）**两处同步**，加 `_dbVersion = 25` |
| 2 | 落库 | `MessageRepo.batchInsertOfflineMessages` 把服务端信封顶层的 `sender_did` 写进新列 |
| 3 | 模型 | `MessageColumns.senderDid`；`MessageModel.senderDid` + `fromJson` / `toJson` 对称 |
| 4 | 分流 | `toTypeMessage()` 先 `_toInboundFrame()` → `E2EEService.decryptInboundV3`；返回 null 才回落 v1/v2（`_decryptLegacyPayload()`） |

### 1.1 三处设计取舍（按「选安全那个」裁决，未询问）

**取舍一：专用列，而不是把 `sender_did` 塞进本地持久化的 `e2ee` JSON blob。**
后者零 schema 改动。**否决**：`e2ee` 承载的是密码学信封，`sender_did` 是
**服务端声明**——context binding 第 6 项的全部意义就是拿「服务端声明」去比对
「受认证的 header」。把二者混进同一个 blob 会让这条防线在语义上自证，
且与后端 A2-a 的同款取舍（不复用 `e2ee` jsonb，因 E2EE-060 要求逐字节透传）
保持一致。

**取舍二：不把离线路径改成「落库前解密」。**
若在 `batchInsertOfflineMessages` 就解密（此时帧里还有 `sender_did`），
可以完全不加列、还能把两条路径收敛成一个解密入口——更简单，也更能根治
「双路径分叉」这一 bug 类别。**否决**：那会把离线消息的 at-rest 状态从
**密文**改成**明文**，是安全姿态的实质弱化；且属存储语义的架构级变更，
与停放区的「候选任务 B：`_receiveMessage` 副作用链解耦」同源，需人工点头。
按「选安全那个（fail-closed、不改协议、不加依赖）」裁决，本轮走加列方案。
**此替代方案记录在案，供后续架构决策时取用。**

**取舍三：`sender_did` 缺失时不伪造空串，让 context binding 如实失配。**
`_toInboundFrame()` 用 `if (senderDid != null)` 条件写入；`toJson()` 同理；
落库时空串不写列。迁移 v25 之前的旧行因此得到精确的
`context_mismatch_sender_did` 分类，而不是与真篡改混成同一种失败。
与后端 A2-a 的 `put_sender_did/2` / `null_if_empty/1` 同一原则。

---

## 2. RED 记录

### 2.1 第一次尝试：编译错误（不算 RED）

直接在测试里写 `senderDid:` 命名参数 → Dart 静态类型报
`No named parameter with the name 'senderDid'`。**这是编译错误，不是行为失败**，
不满足「RED 必须是行为失败」。因此先只加**承载字段**（`MessageModel.senderDid`
+ 构造参数，**不接任何线**），把 RED 降格成纯行为问题。

### 2.2 真正的 RED（行为失败）

```
00:00 +2 -3: Some tests failed.
```

| 用例 | 失败形态 |
|---|---|
| `v3 离线行经 toTypeMessage 能读出明文` | 行为失败：`decrypt_failed`（`FormatException: Unexpected end of input` —— 正是「把空串当密文」的症状） |
| `缺 sender_did 的旧行必须 fail-closed 且不得暴露密文` | 行为失败：分类是 `decrypt_failed` 而非 `context_mismatch_sender_did` |
| `toTypeMessage 必须先经 decryptInboundV3 分流` | 行为失败：结构守护（反转后）为 false |

### 2.3 对照组（harness 有效性）

RED 阶段**同时绿**的 2 例，证明 harness 没坏：

- `toTypeMessage 所用的 decryptE2EEMessage 无法解出 v3 明文`（原缺口实证，仍成立）
- **`对照组：同一行数据经 decryptInboundV3 必须可读`** —— 同一 harness、同一行数据，
  改走生产 v3 入口即可读出明文。**这一项若红就说明是 harness 缺陷而非路径缺口**，
  按纪律须立刻停下重估。它没红。

### 2.4 断言语义变更声明（不得删用例）

`decrypt_on_read_v3_gap_test.dart` 的「结构级」一组原本钉的是
「mapper **没有** v3 分流」。该事实已被本轮接线废止，废止依据是
`22-claude-code-execution-state.md` §1.1 队列第 2 项的明文要求
（"接线完成后同步反转结构守护断言并补正向可用性用例"）。
断言按此**反转重写**，用例保留，废止理由已写在测试文件头。

### 2.5 「只验拒收」反模式的规避

本轮既有负向（缺 `sender_did` 必须拒收 + 密文不得回灌 UI）**也有正向可用性**：
`v3 离线行经 toTypeMessage 能读出明文`，逐字断言 `metadata['body'] == body`。
一个「拒绝所有消息」的实现在正向用例上拿零分。

---

## 3. 生产调用方核实

| 被测函数 | 生产调用链 |
|---|---|
| `MessageModelMapper.toTypeMessage()` | `MessageOfflineService._processOfflineMessages` → `MessageRepo.batchInsertOfflineMessages`（密文落库）→ UI 读取时 `toTypeMessage()`。另有 `chat_archive_service` 归档回放同链 |
| `MessageRepo.batchInsertOfflineMessages` | `message_offline.dart:494`、`chat_archive_service.dart:254` |
| `MessageModel.fromJson` / `toJson` | 全仓消息读写的唯一序列化边界 |

`_toInboundFrame()` / `_decryptLegacyPayload()` 是本轮从 `toTypeMessage()` 内抽出的
私有 helper，**唯一调用方就是 `toTypeMessage()`**——不是为测试新造的旁路，
测试打的入口始终是 `toTypeMessage()` 本身。

---

## 4. 验收命令与结果

| 命令 | 基线 | 结果 |
|---|---|---|
| `flutter test test/service/e2ee/` | 335 | **337 passed**（+2 净增） |
| `flutter test test/service/` | 1212 | **1217 passed** |
| `dart analyze lib` | 1 条既有 info | **1 issue**（`ios_settings_ui.dart`，与 E2EE 无关） |
| `flutter test test/integration/db_v25_msg_c2c_sender_did_test.dart` | 新增 | **3 passed** |

> `test/service/` 的 1217 高于任务书记的 1212 基线 5 例；本轮 e2ee 目录净增 2，
> 差额 3 来自基线记录之后其他会话的提交（基线漂移）。全绿，未深究。
> **认识论状态：未逐条比对，属推断。**

### 4.1 额外跑的两个目录（不在门禁清单内）

| 目录 | 结果 |
|---|---|
| `test/store/` | 396 passed / **1 failed** |
| `test/integration/` | 与 store 合跑 801 passed / 10 skipped / **34 failed** |

**均为预存失败，非本轮引入**，判定依据（已实证，非推断）：

- `test/store/attachment_upload_presign_test.dart` —— 失败是 presign 编排返回的
  `object_key` / `md5` 负载不符；`grep` 证实该文件**不 import**
  `message_model` / `message_repo` / `SqliteService` 中的任何一个。
- `test/integration/moment/*` / `collect/*` —— 失败形态是
  `Found 0 widgets with icon "IconData(U+0EE39)"`（UI 图标断言漂移）；
  同样 `grep` 证实不 import 上述任何模块。

两者**未删除、未 skip、未放宽**，原样留红并在此备案。

---

## 5. 残留风险（未闭合）

1. **真机双端始终未验证。** 本轮全部结论只在单测层成立。整条 v3 离线链
   （后端 A2-a + 客户端 A2-b）在真机上从未跑通过，与
   `session_ref` / `message_id` / `message_type` / ADR 26 counter 语义 /
   E2EE-030 PFS 同属停放区的那条真机腿。

2. **迁移 v25 之前落库的旧离线行永久不可读。** 它们的 `sender_did` 为 NULL，
   context binding 第 6 项必然失配。这是 fail-closed 的**设计选择**而非缺陷，
   但用户可见后果是「升级前积压的离线加密消息显示为 `[加密消息]`」。
   无回填路径——服务端也没有那些历史行的设备标识（A2-a 迁移 48 同理）。

3. **C2G 未覆盖。** `msg_c2g` 表未加 `sender_did` 列。当前 C2G 走 Megolm v2
   （非 PFv3），不受影响；C2G 若上 PFv3，schema 与落库路径需同步扩。

4. **`downgrade.sql` 未同步。** 该文件是到 v17 的整体回退脚本，不按版本分块；
   `sender_did` 列在回退路径上不被 DROP。多一个可空列不破坏 v17 结构，
   但严格意义上 v25→v24 无单步回退。**认识论状态：文件级阅读结论，未实证。**

5. **`toTypeMessage()` 的可测性债仍在。** 正向用例是把 `currentUid` 设成发送方
   uid 来绕开 `ContactRepo` 取数（测试库无 `contact` 表）。该富化耦合与停放区的
   「候选任务 B：`_receiveMessage` 副作用链解耦」同源，未解。
   本轮用例因此**未覆盖**「非本人发送 + 富化取数」这条组合路径。

6. **落库侧只在单测层验证。** `batchInsertOfflineMessages` 写 `sender_did` 的
   那几行**没有**端到端用例覆盖（新增的 v25 测试打的是裸 SQLite + `MessageModel`
   往返，不是 `batchInsertOfflineMessages` 本身）。
   **认识论状态：该函数的改动为文件级阅读 + 编译通过，未实证。**

7. **归档回放路径（`chat_archive_service`）未核实是否透传 `sender_did`。**
   它复用 `batchInsertOfflineMessages`，但其形状适配层是否携带该字段未查。
   **认识论状态：未实证。**

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| v3 离线行经 `toTypeMessage()` 能读出明文 | **已实证**（正向可用性用例，真 vodozemac + 真 SQLite） |
| 缺 `sender_did` 时 fail-closed，且密文不回灌 UI | **已实证** |
| v1/v2 回落路径未被破坏 | **已实证**（`test/service/e2ee/` 337 全绿 + 结构守护） |
| v25 迁移新增列、旧行保持 NULL、模型往返对称 | **已实证**（裸 SQLite + `MessageModel` 往返） |
| `batchInsertOfflineMessages` 真的写进了该列 | **未实证**（编译通过 + 阅读；见残留风险 6） |
| 归档回放路径透传 `sender_did` | **未实证**（见残留风险 7） |
| `downgrade.sql` 不受影响 | **文件级阅读结论，未实证** |
| 离线 v3 消息「在真机上可读」 | **未实证**——真机腿在停放区 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改任何 ADR、未改协议规范、未动 E2EE-012/023/024/025/029 的状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未开 fallback 变绿。
- 未触碰停放区任何一项。
