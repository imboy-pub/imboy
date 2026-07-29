# E2EE-061 —— AAD 绑定改用方案甲 + MIME 拍板（解除 Slice 4 阻塞）

> **会话**：20260730-0400-claude-code ｜ **仓库**：imboyapp（代码）、imboy（文档）
> **状态**：Slice 4 的**阻塞已解除**；接线本身仍未做，E2EE-061 整体仍 `PENDING`

---

## 1. 两项人工拍板（2026-07-30）

| 项 | 决定 | 已知代价（拍板时接受） |
|---|---|---|
| 块 AAD 绑定 | **方案甲**：`SHA-256(canonical_cbor{ctx, message_id, conversation_id, sender_uid})` | 绑定强度**弱于**原设计——不含 `message_type`/`action`/`created_at_ms`/`session_ref`/`epoch_or_counter` |
| MIME | **不隐藏**，保留真实 MIME | §3.2 点名的泄漏旁路**持续存在**：服务端仍知道「这是一张 jpg / 一个 pdf」。**不得对外宣称附件元数据完全不可见** |

选甲的依据是上一刀已实证的阻塞：`header_hash` 逐收件设备不同、附件对象只有一份
（`evidence/E2EE-061-slice4-blocked-header-hash-binding.md`）。
甲的三个值**上传前即可确定且全设备一致**，ATT-01 由 `message_id` 成立。

不隐藏 MIME 保住了服务端类型白名单（`elib_oss:validate_file_type/1`）与现有预览行为。

---

## 2. 改了什么

| 文件 | 改动 |
|---|---|
| `lib/service/e2ee/attachment_binding.dart`（新） | 方案甲的纯函数 + 域分隔串 + 三项非空 fail-closed |
| `attachment_chunk_codec.dart` | `headerHash` → `bindingHash`，`headerHashLength` → `bindingLength`，AAD key `header_hash` → `binding` |
| `attachment_encryptor.dart` | 同上重命名 |
| `attachment_descriptor.dart` | 新增本地 `sha256Length = 32`，**不再借用** codec 的绑定长度常量 |
| 三个测试文件 | 同步重命名 + 新增 `attachment_binding_test.dart`（12 例） |

### 2.1 为什么必须改名而不是留着 `headerHash`

该值**已被实证不是** PFv3 的 header hash。留着原名会让下一个读代码的人
按错误的心智模型接线——这正是本项目反复付出代价的那类错误。

### 2.2 为什么把 `sha256Length` 从 codec 里拆出来

descriptor 原本用 `AttachmentChunkCodec.headerHashLength` 校验 `plain_sha256`。
两者恰好都是 32 字节，但**语义无关**（一个是明文哈希，一个是 AAD 绑定值）；
借用会让将来任一方改长度时另一方被无声带偏。

---

## 3. 空验证（四条，全部精确变红）

| 空验证 | 结果 |
|---|---|
| A 绑定里去掉 `message_id` | **2 红** —— 「换 message_id 绑定值变」+ 端到端「ATT-01」 |
| B 去掉 `conversation_id` | 1 红 |
| C 允许空 `message_id` | 1 红 —— fail-closed 用例 |
| D 用字节拼接替代 canonical CBOR | 1 红 —— 「字段边界不可平移」 |

恢复后 12/12。A 同时红两条，说明**端到端那条确实穿过了绑定值**，
不是只在纯函数层自说自话。

---

## 4. 验收

```
flutter test test/service/e2ee/attachment_binding_test.dart → All 12 passed
flutter test test/service/e2ee/                              → All 502 passed（上轮 490）
flutter test test/service/                                   → All 1382 passed（上轮 1370）
dart analyze lib                                             → 1 issue（既有 info）
```

---

## 5. 残留风险

1. ⚠️ **接线仍未做** —— `AttachmentBinding` 目前只被测试引用，
   **生产附件路径依旧明文直传**；
2. ⚠️ **`message_id` 必须在上传前生成并贯穿到发送** —— 当前链路是先上传后建消息，
   `message_id` 在哪生成、如何传到上传处，**尚未落实**，是 Slice 4 的首要工作；
3. **MIME 泄漏旁路按拍板保留** —— ATT 系列在 MIME 这一项上不成立，属已知缺口；
4. 绑定强度弱于原设计（见 §1），已接受；
5. `epoch_or_counter` 重发前进的问题**因改用甲案而自然消失**（甲不含该字段）——
   这是**推理**，未构造重发实测。

---

## 6. 认识论状态

| 结论 | 状态 |
|---|---|
| 甲案三项各自生效、边界不可平移、空值 fail-closed | **已实证**（空验证 A–D 精确变红） |
| 端到端封装/开封确实经过绑定值 | **已实证**（空验证 A 同时红端到端用例） |
| 甲案绑定强度足以支撑 ATT-01 | **按拍板接受**；`message_id` 唯一性本身未在本刀验证 |
| 改用甲案消除了 `epoch_or_counter` 失配 | **推理，未实测** |
| 本刀对生产的影响 | **零** —— 无生产调用方 |

## 7. 未做
- 不接线、不改协议版本、不改 ADR、不改任务状态标记。不 push、不部署。
