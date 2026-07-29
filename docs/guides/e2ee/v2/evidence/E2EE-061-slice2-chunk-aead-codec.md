# E2EE-061 Slice 2 —— 附件分块 AEAD 编解码器

> **会话**：20260729-2300-claude-code ｜ **仓库**：imboyapp ｜ **状态**：Slice 2 完成，E2EE-061 整体仍 `PENDING`
> **授权依据**：用户于本轮明确放行「纯函数实施刀」（`30-e2ee-decision-brief.md` §2 决策四）。
> 本刀**不接线、不碰协议版本、不碰 presign/confirm 契约、不加新依赖**。

---

## 1. 做了什么

按 `27-e2ee-061-attachment-encryption-design.md` §2.1 与 §5 的 Slice 2 定义，
新增纯函数编解码器 `lib/service/e2ee/attachment_chunk_codec.dart`：

| 函数 | 职责 |
|---|---|
| `deriveNonce(baseNonce, chunkIndex)` | 每块 nonce 派生 |
| `buildAad({headerHash, attachmentId, chunkIndex, chunkCount})` | 每块 AAD 构造 |
| `encryptChunk(...)` | AES-256-GCM 封块，返回 `ciphertext \|\| tag` |
| `decryptChunk(...)` | 解封，失败一律抛 `AttachmentChunkException` |

**未做**：不接上传/下载路径（Slice 4 / 6）、不定义 `attachment_descriptor`（Slice 3）、
不选 `chunk_size`（属 §6 三项人工拍板之一，本刀刻意让它作为**调用方传入的参数**，
编解码器内不设默认值，因此不预支该决定）。

---

## 2. 两处「两种合理实现二选一」的裁决

按会话规则「二选一取安全那个」，两处均取更安全项，理由如下：

### 2.1 nonce 派生：XOR 而非截断拼接

| 方案 | 取舍 |
|---|---|
| `base_nonce[0..7] \|\| uint32_be(index)`（Tink streaming 风格） | 丢弃 32 bit 随机性 |
| **`base_nonce XOR (0^8 \|\| uint32_be(index))`**（RFC 8446 §5.3 构造）✅ | 对固定 `base_nonce` 在 `index` 上单射，且保留全部 96 bit 熵 |

`maxChunkCount = 0xFFFFFFFF` 是**硬边界而非性能建议**：超过 uint32 会让不同块撞同一
nonce，**同 key 下 nonce 复用直接摧毁 GCM 的认证性**。已由用例钉死。

### 2.2 AAD 编码：复用已在产的 canonical CBOR 而非字节拼接

字节拼接 `header_hash || attachment_id || ...` 存在**切分歧义**——不同字段划分可拼出
同一串。CBOR 每项自带长度前缀，天然无歧义；且 `CanonicalCbor` 已被 PFv3 用于生产、
已有测试，**不引入第三套编码**（与 065 复用 `trust_event_canonical` 的判断同源）。
歧义性已由 §5 的「字段边界不可平移」用例直接验证（`("ab")` 与 `("a")` 产出不同 AAD）。

额外加了域分隔串 `imboy/e2ee/attachment-chunk/v1`，防本 AAD 结构被复用到别的上下文
仍然验得过。

---

## 3. 空验证（negative control）—— 本刀的核心证据

**只验「篡改能否拒收」无效**：一个「一律拒绝」的实现在篡改矩阵上恒得满分。
因此每组都配了正向可用性锚点，并逐条摘掉防线，确认**唯独**对应用例变红。

### 3.1 第一轮：五个空验证，其中**两个全绿**——两条真发现

| 空验证 | 手法 | 结果 | 结论 |
|---|---|---|---|
| A | AAD 里删掉 `chunk_index` | **全绿 33/33** | ⚠️ **该项对重排是冗余防线**——nonce 派生已单射，先挡住了 |
| B | AAD 里删掉 `chunk_count` | `+18 -1`，唯独「块截断」红 | ✅ 载荷防线，且唯一 |
| C | AAD 里删掉 `header_hash` | `+15 -1`，唯独「ATT-01」红 | ✅ 载荷防线，且唯一 |
| D | AAD 里删掉 `attachment_id` | `-2`（互换 + 歧义两条） | ✅ 载荷防线 |
| E | nonce 派生注释掉 `nonce[8] ^= (index >> 24)` | **全绿 33/33** | ⚠️ **我自己的测试有盲区** |

### 3.2 空验证 E 暴露的测试盲区（已修）

原「不同 index 派生出互不相同的 nonce（抽 1000 个）」只覆盖到 index < 1000，
**只碰得到 nonce 的末两个字节**。把 `index` 高 8 位从派生里删掉，该用例依然全绿——
即「一次重构丢掉高位字节，不会有任何测试变红」，而那正是 nonce 复用的成因。

补 `index 的四个字节位置全部进入 nonce（高位不得被丢弃）`：
取值只点亮单个字节位置（`0x01 / 0x100 / 0x10000 / 0x1000000`）外加 `0` 与 `0xFFFFFFFF`。

**修后复跑**：

| 空验证 | 结果 |
|---|---|
| E（删 `>> 24` 那行） | `+6 -1`，**唯独新用例红** |
| E2（删 `>> 16` 那行） | `+6 -1`，**唯独新用例红** |
| A2（再删 AAD 的 `chunk_index`） | 仍 34/34 全绿 —— 冗余性结论**复核成立** |
| 恢复后 | 34/34 全绿，无漂移 |

### 3.3 关于发现 A 的处置（保留冗余，不删）

`chunk_index` 进 AAD 对**重排**确实是冗余的（nonce 已单射）。**仍保留**，两条理由：

1. 设计文档 §2.1 明文要求「每块 AAD **至少**绑定 …… `chunk_index`」——
   删它是改协议形状，不属本刀可自行裁决的范围；
2. 纵深防御：nonce 派生若在未来被改成常量（正是空验证 E 那类改动），
   AAD 仍能兜住。**该冗余性已实证并记录在此**，不是未经检验的假设。

### 3.4 对照组

第 1 组三条**不经过被测代码**，直接用 pointycastle 验证底层假设
（`AEADParameters` 确实把 AAD 纳入认证、tag 改一字节即拒）。
**它红 = harness 缺陷**，后面所有「AAD 失配即拒收」的结论都不成立。对照组全程绿。

---

## 4. 覆盖矩阵

| 组 | 条数 | 覆盖 |
|---|---:|---|
| 1 对照组 | 3 | 底层 AEAD 原语的假设 |
| 2 nonce 派生 | 6 | 单射、确定性、不就地改入参、长度/上限 fail-closed、**四字节位置全覆盖** |
| 3 正向可用性 | 5 | 单块 / 空块 / 1·15·16·17 字节分组边界 / 确实加密了 / 3 块流拼接还原 |
| 4 篡改矩阵 | 10 | ATT-01 `header_hash`、`attachment_id`、`chunk_index`（重排）、`chunk_count`（截断）、key、nonce、密文正文、tag、两块整体对调，**外加参数全对的正向锚点** |
| 5 AAD 编码 | 3 | 切分歧义、确定性、域分隔串 |
| 6 参数边界 | 6 | key 长度（加解密两侧）、header_hash 长度、空 id、count<1、index≥count、密文短于 tag |

---

## 5. 验收

```
flutter test test/service/e2ee/attachment_chunk_codec_test.dart   → All 34 passed
flutter test test/service/e2ee/                                   → All 428 passed（上轮 394，+34）
flutter test test/service/                                        → All 1308 passed（上轮 1274，+34）
dart analyze lib                                                  → 1 issue（既有 info，ios_settings_ui.dart，与 E2EE 无关）
```

imboy 侧未改动，`make e2ee-verify` 不适用。

---

## 6. 残留风险

1. **未接线** —— 本刀是纯函数，生产上传/下载路径**至今仍是明文直传**。
   `27-...` §1 的实证结论不变：附件面今天完全没有 E2EE，ATT-01..05 全部不成立。
   **不得据本刀认为附件加密"已经有了"**；
2. **`chunk_size` 未定** —— 属 §6 三项人工拍板之一。编解码器不设默认值，
   由调用方传入，Slice 4 接线前必须先拍板；
3. **content key 的生成与保管不在本刀** —— 编解码器只收 key，不产生 key、不存 key。
   CSPRNG 来源与 CryptoStore 落点属 Slice 3/4；
4. **未做恒定时间比较** —— GCM 的 tag 校验由 pointycastle 承担，本刀未独立审计
   其实现是否恒定时间。**这是文件级信任，未实证**；
5. **单块内存模型** —— `encryptChunk` 收整块 `Uint8List`，大文件的流式/背压
   属 Slice 4 的接线问题，本刀不涉及；
6. **未验证与后端的互操作** —— 后端不接触 content key（服务端零密码学门禁不变），
   但 `attachment` 表的 hash/size 语义变更是 Slice 5。

---

## 7. 认识论状态

| 结论 | 状态 |
|---|---|
| 篡改矩阵四项各自生效（B/C/D） | **已实证**（空验证逐条精确变红） |
| `chunk_index` 进 AAD 对重排是冗余防线 | **已实证**（空验证 A 与 A2 两次全绿） |
| nonce 派生对 index 四个字节位置全覆盖 | **已实证**（空验证 E/E2 精确变红） |
| 底层 `AEADParameters` 把 AAD 纳入认证 | **已实证**（对照组，不经被测代码） |
| pointycastle 的 tag 比较是否恒定时间 | **未实证**（见残留 4） |
| 本刀对生产附件路径的影响 | **零** —— 未接线，属文件级事实（无任何 import 指向本模块） |

---

## 8. 未做

- 不接线、不改协议版本、不改 ADR、不改任何既有任务的状态标记。
- 不 push、不部署、不访问生产、不通知第三方。
