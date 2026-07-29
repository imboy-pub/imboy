# E2EE-061 Slice 4 —— 封装判定闸门（防「密钥明文出网」）

> **会话**：20260730-0600-claude-code ｜ **仓库**：imboyapp
> **状态**：闸门已落地并实证；**尚未接线到上传路径**，E2EE-061 整体仍 `PENDING`

---

## 1. 这道闸门防的是什么

`attachment_descriptor` 里带着 **content key**（能解开整个附件），
只有随 PFv3 **加密 payload** 发送才安全。

若在一个 payload 不加密的会话里封装了附件，descriptor 会**以明文出网**——
**这比今天更糟**：既上传了密文（用户以为受保护），又把解密钥匙贴在旁边。

因此判定 **fail-closed**：任何一项拿不准就**不封装**，退回今天已知的明文行为。

⚠️ 刻意做成**唯一入口**：新增上传路径调用它，不在调用点各写一份 `if`。
E2EE-062 第七刀的教训——判定散落多处时，新增那处漏判**不会有任何测试变红**。

---

## 2. 设计取舍

`payloadWillBeEncrypted` 由调用方传入（聊天侧即
`E2EEService.shouldEncryptOutgoingPayload(chatType)`），**不在本模块内去查**：
保持纯函数可直接验收，且让「谁决定加密」留在它本来的位置。

「跳过」返回 `SealSkipped` 而**不是抛异常**：本闸门只决定「要不要加密」，
不决定「能不能传」。若改成抛异常，附件功能会在非 E2EE 会话里**整体失效**。
该语义已由一条 `returnsNormally` 用例钉死。

`payloadNotEncrypted` 的判定**优先于**绑定缺失，避免在最危险的场景里
报出一个误导性的原因。

---

## 3. 空验证（三条，全部精确变红）

| 空验证 | 结果 |
|---|---|
| A 忽略 `payloadWillBeEncrypted` | **3 红** —— 第 2 组全部（核心闸门） |
| B 忽略绑定输入为空 | **3 红** —— 第 3 组三条 |
| C 恒返回 skip | **1 红** —— 唯独正向可用性锚点 |

C 的存在正是因为：**恒 skip 的实现在全部负向用例上满分**。

---

## 4. 验收

```
flutter test .../attachment_seal_policy_test.dart → All 8 passed
flutter test test/service/e2ee/                   → All 521 passed（上轮 513）
flutter test test/service/                        → All 1401 passed（上轮 1393）
dart analyze lib                                  → 1 issue（既有 info）
```

---

## 5. 残留风险

1. ⚠️ **未接线** —— 上传路径尚未调用本闸门，`seal` 仍无人传，
   **生产附件路径依旧明文直传**；
2. ⚠️ **`message_id` 仍在上传之后生成**（`attachment_handler.dart:308` / `:337`
   的 `id: Xid().toString()` 写在消息构造里）。接线前必须先提前；
3. ⚠️ **明文哈希进消息体已实证**：`attachment_handler.dart:317` / `:349`
   的 `metadata: {'file_hash256': …}`。即便附件字节加密，
   **非 E2EE 会话下明文哈希仍随消息明文上行**，已知文件识别旁路照旧成立。
   本闸门**不解决**这一条；
4. **未真机验证** —— 用户已明确「代码先写完，真机验证留作单独一轮」，
   故本刀及其后续**均不得标 PASS**。

---

## 6. 认识论状态

| 结论 | 状态 |
|---|---|
| 三条判定分支各自生效、恒 skip 会被抓 | **已实证**（空验证 A/B/C） |
| 跳过不阻断上传 | **已实证**（`returnsNormally` 用例） |
| `message_id` 在上传后生成 | **已实证**（行号） |
| 明文哈希进消息 metadata | **已实证**（行号） |
| 接线后的真实行为 | **未验证**（未接线、未真机） |

## 7. 未做
- 未接线、未改协议、未改 ADR、未改任务状态标记。不 push、不部署。
