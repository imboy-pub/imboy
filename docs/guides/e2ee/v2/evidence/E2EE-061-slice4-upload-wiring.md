# E2EE-061 Slice 4（第一半）—— `uploadViaPresign` 加密接线

> **会话**：20260730-0500-claude-code ｜ **仓库**：imboyapp
> **状态**：上传漏斗已能发密文；**尚无调用方开启**，E2EE-061 整体仍 `PENDING`

---

## 1. 交付范围与刻意留下的部分

**已做**：`AttachmentApi.uploadViaPresign` 新增可选 `seal` 参数。非 null 时：
presign → **封装** → PUT **密文** → confirm 上报**密文**哈希/大小 + `cipher` 判别位，
并把 descriptor 回填给调用方。

**未做（下一刀）**：5 个上传入口都还**没传** `seal`，
`message_id` 也还没提前到上传之前生成。因此**生产行为零变化**。

### 1.1 为什么先做漏斗而不是先改 5 个入口

5 个入口（`uploadBytesViaPresignMeta` / `uploadImageEntityViaPresign` /
`uploadVideoViaPresign` / `uploadVideoFileViaPresign` / `uploadFileViaPresignCompat`）
**全部经由 `uploadViaPresign` 这一个漏斗**。在漏斗里做一次，
5 个入口只需各传一个 `seal`——而不是在 5 处各写一遍加密。
上一刀记录的「5 处明文 `sha256`」里，**漏斗这一处是唯一决定上送内容的**，
其余 4 处是构造返回给调用方的 meta（下一刀随消息体一起处理）。

### 1.2 为什么用可变承载而不是改返回类型

`uploadViaPresign` 返回 `String`。改成返回 descriptor 会让 5 个入口全部跟着改，
而它们在本刀内**还不需要**加密。`AttachmentSealRequest` 把「要不要加密」与
「谁需要 descriptor」解耦，未接线的入口**一行不用动**。

### 1.3 MIME 拍板带来的意外收益

因为拍板「不隐藏 MIME」，PUT 的 Content-Type 与 presign 的 `mime_type`
**都保持真实值**，故 **presign / confirm 契约无需任何改动**。
Slice 1 曾判定「改 MIME 必须重新 presign」，该约束因这次拍板而**不触发**。

---

## 2. 关键实现点

- **封装必须在 presign 之后**：descriptor 要带 `object_key`，而 object_key 由 presign 返回；
- `validateUpload` 仍按**明文**长度做上限校验（100MB 是对用户文件的限制，不是对密文的）；
- content key / base nonce 由 `Random.secure()` 每次新生成，**不复用**；
- descriptor 含 content key，注释里明确标注：**必须随 PFv3 加密 payload 发送，
  绝不可写日志、绝不可进未加密字段**。

---

## 3. 空验证（四条，全部精确变红）

| 空验证 | 结果 |
|---|---|
| A 照旧 PUT 明文 | **4 红** —— 密文长度 / 明文片段不出现 / 密文哈希 / 端到端还原 |
| B confirm 改回上报明文哈希 | 1 红 —— 拍板 ① 那条 |
| C 去掉 `cipher` 判别位 | 1 红 |
| D `size` 改回明文大小 | 1 红 |

恢复后 11/11。**A 红 4 条说明这些断言确实穿过了真实上传字节**，不是各自独立的桩。

---

## 4. 覆盖（11 例）

**第 1 组（`seal` 为 null）是本刀零风险的前提**，只验加密分支等于没验「没打破什么」：

| 组 | 用例 |
|---|---|
| 1 旧行为零破坏 | PUT 的是明文本身；confirm 上报明文哈希/明文大小且**不带** `cipher` |
| 2 加密分支 | 密文长度 = 明文 + 每块 16 字节 tag；**明文任意 16 字节连续片段都不出现在上传字节里**（逐 8 字节滑窗）；上报密文哈希且**绝不等于**明文哈希；上报密文大小 + `cipher`；MIME 保持真实值；descriptor 回填且 `object_key` 一致；**正向可用性**：用回填的 descriptor 能把上传字节还原成原明文；**content key 不出现在 confirm body 或上传字节中** |
| 3 fail-closed | 空 `attachmentId` 构造即抛 |

测试走 `attachment_api.dart` 既有的三个注入 seam，**不碰网络**。

---

## 5. ⚠️ 顺带修复一个已红约三周的既有测试

`test/store/attachment_upload_presign_test.dart` 的「快乐路径」断言 `confirmBody`
含 `'md5'`，但该字段已于 **2026-07-09 commit `71d20283`**
（`refactor(attach)!: 文件完整性哈希 md5→file_hash256`）重命名并改算法，
**该提交未同步本测试**（测试文件上次改动是 2026-06-20）。

已核实**与本刀无关**：`git show HEAD:lib/store/api/attachment_api.dart` 第 169 行
本就发 `file_hash256`。断言已按现行契约更新（**收紧**到 SHA-256，非放宽），
废止理由与出处写进了该用例上方注释。

> 这说明 `test/store/` 此前**不在任何绿灯门内**——红了三周无人发现。

---

## 6. 验收

```
flutter test .../attachment_upload_sealed_test.dart → All 11 passed
flutter test test/service/e2ee/                     → All 513 passed（上轮 502）
flutter test test/service/                          → All 1393 passed（上轮 1382）
flutter test test/store                             → All 397 passed（修复前 396 passed 1 failed）
dart analyze lib                                    → 1 issue（既有 info）
```

---

## 7. 残留风险

1. ⚠️ **没有任何调用方传 `seal`** —— 生产附件路径**依旧明文直传**，ATT-01..05 仍不成立；
2. ⚠️ **`message_id` 仍未提前** —— 当前链路先上传后建消息，绑定值算不出来。
   这是下一刀的首要工作，**没有它就无法开启加密**；
3. **其余 4 处明文 `sha256` 未动** —— 它们构造返回给调用方的 meta，
   随消息体一起处理属下一刀；⚠️ 这些 meta 里的 `file_hash256` 会进消息 payload，
   **非 E2EE 会话下是明文**，需一并评估；
4. **缩略图/视频缩略图未加密**（Slice 7）—— 设计 §3.3 明确「缩略图不加密 = 预览即泄漏」；
5. **整文件入内存** —— `seal` 收整个明文 `Uint8List`，100MB 上限下低端机未实测；
6. **未真机验证**；未与后端做一次真实的密文 confirm 往返。

---

## 8. 认识论状态

| 结论 | 状态 |
|---|---|
| 加密分支确实 PUT 密文、上报密文哈希/大小/`cipher` | **已实证**（11 例 + 空验证 A–D） |
| `seal` 为 null 时旧行为逐字节不变 | **已实证**（第 1 组） |
| content key 不外泄到 confirm body 或上传字节 | **已实证**（第 2 组末条） |
| 5 个入口全部经由该漏斗 | **已实证**（grep 调用点） |
| 既有测试的红与本刀无关 | **已实证**（`git show HEAD:` + 两个 commit 日期） |
| 端到端与后端的真实往返 | **未验证** |

## 9. 未做
- 未开启任何调用方；未改协议版本、ADR、任务状态标记。不 push、不部署。
