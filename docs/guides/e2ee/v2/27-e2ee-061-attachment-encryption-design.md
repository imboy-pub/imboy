# 27 — E2EE-061 附件独立 content key 与分块 AEAD：设计与切片计划

> **状态**：设计草案（**不实施**）。队列第 5 项明确规定「先只产出设计与切片计划，
> 实施需人工确认」。本文件不改动任何生产代码。
> **上位约束**：ADR 15 §9（附件加密）、ADR 14 G5「附件」行、验收用例 ATT-01..05
> **执笔**：Claude Code loop，会话 `20260729-0400-claude-code`，2026-07-29

---

## 1. 现状（已实证）

以下四条均为**读源码 + 逐行核实**得出，不是推测：

| 事实 | 出处 | 状态 |
|---|---|---|
| 附件字节**完全未加密**，`bytes` 原样 PUT 到 Garage | `imboyapp/lib/store/api/attachment_api.dart` `uploadViaPresign` 第 3 步 | **已实证** |
| `file_hash256` 是**明文** SHA-256，且随 confirm **上报服务端** | 同上第 4 步（`sha256.convert(bytes)`） | **已实证** |
| 缩略图 / 压缩视频是**各自独立的对象**，分别 presign 直传 | `uploadVideoFileViaPresign` 文档注释与实现 | **已实证** |
| PUT 时 `Options(contentType: mime)`，声明**真实 MIME** | `_rawDioPut` | **已实证** |

E2EE 消息里被 PFv3 保护的只有 **object_key**（消息 payload 的一个字段）。
**对象本身是明文。**

### 1.1 一条被实证推翻的假设

设计初期假设 `uploadViaPresign(process: true)` 会触发**服务端**对对象做处理
（图片压缩等），因而「加密附件会破坏服务端处理」是硬约束。

**核实结果：不成立。** `process` 只控制 `AppLoading.showProgress` 的上传进度 UI
（`_rawDioPut` 的 `onSendProgress` 回调），与服务端无关。
该假设若不核实就写进设计，会凭空造出一条不存在的阻塞项。

### 1.2 ATT-01..05 今天的成立情况

| ID | 用例 | 今天 |
|---|---|---|
| ATT-01 | 附件对象从消息 A 换到消息 B | **不成立**——对象无 AAD 绑定，换了照样能打开 |
| ATT-02 | 交换/删除/重复/截断 chunk | **不适用**——今天不分块，也无每块 tag |
| ATT-03 | 篡改 MIME/name/size/hash/chunk_count | **不成立**——这些字段在明文 confirm body 与消息 payload 里 |
| ATT-04 | 未授权方拿到 Garage 原始对象 | **直接失败**——拿到就是原文件 |
| ATT-05 | 下载/解密中途 kill 或磁盘满 | **不适用**——今天没有解密临时文件这一步 |

**结论：附件面目前完全没有 E2EE。** ADR 14 G5 的「附件」行是 GA-C2C 硬门禁，
在本项完成前 GA-C2C 不成立——这一点 `22-...state.md` §5.3 已记载。

---

## 2. 目标设计（对齐 ADR 15 §9）

### 2.1 密钥与信封

- 每个附件（**含缩略图，各自独立**）生成 256-bit content key，CSPRNG，
  不复用消息 key / 其他附件 key / 上传凭证。
- `attachment_descriptor` 放进**加密的** `payload`，随 PFv3 一起被认证：

  ```
  attachment_descriptor = {
    attachment_id,            // 客户端生成，参与 AAD
    object_key,               // Garage 对象标识
    content_key,              // 256-bit，base64url
    cipher,                   // 例如 "AES-256-GCM"
    chunk_size, chunk_count,
    base_nonce,               // 随机
    plain_size, plain_sha256, // 明文大小与哈希（**只在此处**）
    mime, name,
    thumb: { ...同结构，独立 content_key... }
  }
  ```

- 每块 nonce 由 `base_nonce` 与 `chunk_index` 按固定唯一映射派生。
- 每块 AAD **至少**绑定 `header_hash + attachment_id + chunk_index + chunk_count`
  —— `header_hash` 来自 PFv3 protected header，这是 ATT-01 的直接依据：
  换一条消息 → header_hash 变 → AAD 失配 → 拒绝打开。

> ⛔⛔ **本条已被实证证明在当前发送链路上不可实现（2026-07-30，Slice 4 开工核实）。**
> 两条独立原因：
> ① 附件上传发生在消息组装**之前**，上传时 header 尚未存在
>    （`attachment_handler.dart:271`）；
> ② **决定性**：同一条消息对每个收件设备各建一份 protected_header
>    （`chat_network_service.dart:636` 逐设备循环 + `ensureSessionId(toId, peerDid)`
>    ⇒ `session_ref` 逐设备不同 ⇒ `header_hash` 逐设备不同），
>    而**附件对象只有一份**。3 台设备 = 3 个 header_hash 对 1 个对象，
>    密文块只能绑其中一个，另两台**必然打不开**。
> 此外 `epoch_or_counter` 在加密时才定、重发会前进，即便单设备也会失配。
>
> **Slice 4 据此 BLOCKED，等待人工修订本条的 AAD 构成。**
> 三个候选（甲：改绑 `message_id+conversation_id+sender_uid`；
> 乙：每设备一份密文附件对象；丙：上传移到加密之后且禁止重加密）与各自代价，
> 见 `evidence/E2EE-061-slice4-blocked-header-hash-binding.md` §4。
> ⚠️ **Slice 2/3/5 均不受影响、无需返工**（codec 只要求「32 字节绑定值」，
> 换成别的摘要一行不用改）。

### 2.2 接收端

按 ADR 15 §9：验证块顺序、数量、每块 tag、完整明文 hash 与声明大小**全部通过后**
才交给预览器；失败时删除临时明文。

---

## 3. 三个必须点名的陷阱

这三条都是「加密了内容但仍然泄漏」的旁路，且都不是新增代码，而是**现有行为需要改**。

### 3.1 `file_hash256` 明文哈希上报服务端 = 已知文件识别

confirm body 今天上报明文 SHA-256。即使内容加密，只要还上报明文哈希，
服务端或拿到该字段的攻击者就能与已知文件库比对，确认「这个人发的是哪个文件」。
**这等于把内容加密的收益抵消掉一大半。**

处置：confirm 只能上报**密文**哈希（用于对象完整性），明文哈希只进
`attachment_descriptor`。这需要后端 `attachment` 表字段语义同步变更。

### 3.2 Content-Type 泄漏 —— **原表述已被实证推翻，结论比原来更强**

PUT 声明真实 MIME。加密后仍声明 `image/jpeg` 等于告诉服务端这是什么类型的文件。

> ⚠️ **本节原写着**「presigned URL 的签名**通常覆盖** Content-Type：只改 PUT 不改
> presign 请求会导致签名失配、直传直接失败」，并标为未实证、要求先验 Garage。
> **该表述错误**，已由 Slice 1 推翻（`evidence/E2EE-061-slice1-presign-mime-binding.md`）。

Slice 1 实证结论（探针 `test/lib/e2ee_presign_mime_binding_tests.erl`，5/5）：

| 性质 | 结果 |
|---|---|
| `X-Amz-SignedHeaders` 内容 | **只有 `host`** —— PUT 请求的 Content-Type **请求头不被签名覆盖** |
| MIME 在何处进签名 | 以 **query 参数** `Content-Type=<mime>` 进入 canonical query string，**因而被绑进签名** |
| 改 MIME 的后果 | 必然换 URL（不同 MIME → 不同签名），**必须重新 presign** |
| 空 MIME | 不产出 `Content-Type=` 参数（`presign_get`/`presign_delete` 走此路） |

**修正后的结论（更强）**：

1. 只改客户端 PUT 请求头**不会**导致签名失配——但**也毫无用处**：
   MIME 已经写在 presigned URL 的 query 参数里；
2. 真正的问题不是「PUT 与 presign 同刀改」，而是**整个 presign/confirm 契约**：
   服务端在 `presign` 请求里收 `mime_type`、在 `confirm` 里又存一次
   （`attachment_api.dart` 的 `confirmBody`）——**服务端本来就知道真实 MIME**。
   隐藏 MIME 必须改这两处契约，而不只是改客户端请求头；
3. 权威来源是**我方的 `elib_s3_sign:presign_url/6`**，不是 Garage 的行为——
   签名覆盖什么由我们自己的实现决定。原设计要求「先验 Garage」**找错了地方**。

**残余未实证**：Garage 是否**校验** PUT 请求头的 Content-Type 与 URL 里的 query
参数一致。该问题只影响「改了 presign 后客户端要不要同步改请求头」这一细节，
**不影响上面三条结论**；需本地 Garage 就绪后确认。

### 3.3 缩略图不加密 = 预览即泄漏

缩略图是独立对象。若只加密主体不加密缩略图，ATT-04 在缩略图上直接失败：
拿到缩略图对象就能看到画面内容。缩略图必须同样加密、同样有独立 content key。

同理，ADR 15 §9 点名的**波形 / OCR / EXIF / 推送摘要**若含敏感内容，
必须一并进 E2EE payload，或在产品上明确声明不受保护。

---

## 4. 兼容性约束

| 约束 | 说明 |
|---|---|
| 历史明文对象必须仍可读 | 与 A2-a 的 `sender_did` 情形**方向相反**：旧对象是明文，天然可读。需要一个判别标志（`attachment_descriptor` 存在与否）来选择走解密还是直读，**不得**让旧附件不可读 |
| `confirm` 的 `size` | 密文大小 ≠ 明文大小。上报密文大小；明文大小只在 descriptor 内 |
| 服务端零密码学 | 后端只存/转密文与对象元数据，不得接触 content key。现有 `check_server_zero_crypto.sh` 门禁必须继续通过 |
| 不改协议版本 | descriptor 是 `payload` 内的新字段，PFv3 信封结构不变，`meta_version` 保持 3 |

---

## 5. 切片计划

每刀独立可验收、可提交，尺寸对齐本 loop「一轮一件」的节奏。
**任何一刀开工前都需要人工确认**（队列规定）。

| # | Slice | 仓库 | 内容 | 验收对象 |
|---|---|---|---|---|
| 1 | ~~**presign / PUT 的 Content-Type 实证**~~ | imboy | ✅ **DONE**（2026-07-29）。权威来源是我方 `elib_s3_sign`，非 Garage；原 §3.2 表述已推翻 | 单测 5/5，已入 e2ee-verify 门禁 |
| 2 | ~~**分块 AEAD 编解码器（纯函数）**~~ | imboyapp | ✅ **DONE**（2026-07-29）。`attachment_chunk_codec.dart`，34 例。nonce 用 XOR 派生（保留全部 96 bit 熵）、AAD 复用已在产的 canonical CBOR（切分歧义已验）。⚠️ 空验证暴露两条：`chunk_index` 进 AAD 对重排**是冗余防线**（nonce 已单射，已实证并保留作纵深防御）；原 nonce 单射用例**只覆盖末两字节**，高位丢弃不会有任何测试变红，已补四字节位置全覆盖用例 | 往返 + 篡改矩阵 10 例 + 空验证逐条精确变红；见 `evidence/E2EE-061-slice2-chunk-aead-codec.md` |
| 3 | ~~**`attachment_descriptor` codec**~~ | imboyapp | ✅ **DONE**（2026-07-29）。`attachment_descriptor.dart`，43 例。⚠️ 关键增补：**`chunk_count` 自洽闸门**——`chunk_count` 虽进每块 AAD，但 AAD 只保证「发送方当初声明的那个数」，**不保证它与文件自洽**，故 parser 强制 `chunk_count == ceil(plain_size/chunk_size)`（空文件取 1 而非 0）。另：未知字段拒收、cipher 不做协商、thumb 必须独立 key/nonce 且走同一套校验、`toString` 抹掉 content_key | 往返 + 严格性矩阵；七个空验证逐条精确变红；见 `evidence/E2EE-061-slice3-attachment-descriptor.md` |
| 4 | **上传侧接线** | imboyapp | `uploadViaPresign` 前置加密；confirm 改上报密文哈希与密文大小 | 上传产物是密文（ATT-04 的一半） |
| 5 | **后端字段语义** | imboy | `attachment` 表 hash/size 语义变更 + 迁移；OpenAPI 同步 | 真 PG 集成测试 |
| 6 | **下载侧接线 + 完整性门** | imboyapp | 解密、块顺序/数量/tag/明文 hash 校验后才交预览器 | ATT-01/02/03 端到端 |
| 7 | **缩略图 / 视频缩略图** | imboyapp | 独立 content key，同样加密 | ATT-04 在缩略图上成立 |
| 8 | **临时明文生命周期** | imboyapp | 解密临时文件的权限、崩溃恢复清理 | ATT-05 |
| 9 | **兼容性回归** | 两仓 | 历史明文对象仍可读 | **正向可用性**：旧附件不得因本改动不可读 |

~~**建议起点**：Slice 1~~ —— **已完成**。其结论已并入 §3.2：
Slice 4（上传侧接线）必须连带改 **presign 与 confirm 的 MIME 契约**，
而不只是改客户端 PUT 请求头。~~下一个可推进的是 **Slice 2 / 3**~~ ——
**Slice 2 与 Slice 3 均已完成**（2026-07-29，用户放行「纯函数实施刀」后）。
**放行范围内的纯函数刀至此在 061 内用尽**：剩余 Slice 4..9 全部是接线或后端字段语义。
⚠️ **Slice 4 开工前必须先拍板 §6 的三项**——尤其 `chunk_size`：
Slice 2 刻意让它作为调用方传入的参数、编解码器内不设默认值，因此**未预支**该决定。

---

## 6. 需要人工拍板的取舍

> ✅ **三项均已于 2026-07-30 由用户拍板**（见下表）。本节自此是**约束输入**，
> 不再是待决项；Slice 4..9 必须按此执行，改动须重新拍板。
>
> | # | 决定 | 直接后果 |
> |---|---|---|
> | ① 明文哈希 | **confirm 只上报密文哈希**，明文 SHA-256 只进加密的 descriptor | 服务端**失去**跨用户秒传/去重/已知违规文件识别。对外表述须与 ADR 18 合规边界一致 |
> | ② 历史回迁 | **暂不回迁，但预留判别位** | 旧对象保持明文且**必须仍可读**（Slice 9）；attachment 侧须能区分「明文旧对象/密文新对象」，以便日后盘点与分批回迁。**只多一个字段，不多一条分支** |
> | ③ `chunk_size` | **1 MiB** | 100MB 上限 → ≤100 块；descriptor 元数据小、单块峰值内存可控 |
>
> 以下为拍板前的原文，保留以说明取舍理由：

以下三项**不属于**「两种合理实现选安全那个」可以自行裁决的范围，必须人工确认：

1. **是否接受服务端失去附件元数据能力**。明文哈希去除后，服务端无法做跨用户
   秒传 / 去重 / 已知违规文件识别。这是**产品与合规层面的决定**，不是技术取舍。
   —— 与 ADR 18（合规边界）直接相关。
2. **历史明文附件是否需要回迁**。不回迁则老附件永远是明文（服务端可读）；
   回迁需要客户端下载—加密—重传，成本与风险都很高。
3. **chunk_size 取值**。影响内存占用、断点续传粒度与元数据体积；
   与 `AttachmentApi.maxUploadBytes = 100MB` 的上限一起定。

---

## 7. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 附件字节今天完全未加密 | **已实证**（`uploadViaPresign` 逐行） |
| `file_hash256` 是明文哈希且上报服务端 | **已实证** |
| 缩略图 / 压缩视频是独立对象 | **已实证** |
| PUT 声明真实 MIME | **已实证** |
| `process` 与服务端处理无关（推翻初始假设） | **已实证**（`_rawDioPut` 的 `onSendProgress`） |
| ATT-01..05 今天均不成立 | **已实证**（由上述事实直接推出） |
| `SignedHeaders` 只含 `host`，请求头 Content-Type 不被签名覆盖 | **已实证**（Slice 1，5/5） |
| MIME 以 query 参数被绑进签名，改 MIME 必须重新 presign | **已实证**（同上） |
| 服务端在 presign 与 confirm 两处都拿到真实 MIME | **已实证**（`elib_s3_sign` + `attachment_api.dart`） |
| 原「签名覆盖 Content-Type 请求头」表述 | **已被推翻**（见 §3.2） |
| Garage 是否校验请求头与 query 参数一致 | **未实证**（本地 Garage 未运行；不影响上述结论） |
| 本设计能让 ATT-01..05 全部成立 | **设计推理，未实证** —— 需按切片计划逐刀验收 |

---

## 8. 未做

- **未实施任何生产代码**（队列规定：实施需人工确认）。
- 未改 ADR / 协议规范；未新增迁移；未改动任何既有测试。
- 不 push、不部署、不访问生产。
