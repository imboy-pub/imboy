# 密码学清单（Cryptographic Inventory）

> 审计就绪包**第 2 件工件**（P5-2）。审计方判断"用了什么原语、参数是否合规、实现来自哪里"的单一入口。
> 最后核对：2026-08-02

本清单分两半：**原语与参数**（本文）+ **库/版本/许可证**（[`../../../legal/third-party-licenses.md`](../../../legal/third-party-licenses.md)，含 AGPL 阻断项醒目标注）。
密钥的生成/存储/轮换/销毁见 [`../key-lifecycle.md`](../key-lifecycle.md)。

## 0. 证据等级说明

每行标注**出处**，审计方据此判断该参数是我方选定还是上游规范：

| 标记 | 含义 |
|---|---|
| 📄 **本仓实证** | 参数写在本仓代码里，已逐条对照源码核实（附文件） |
| 📕 **上游规范** | 我方不实现该原语，参数由上游库/规范决定，本仓不覆盖 |
| ⚙️ **上游默认** | 依赖库的默认值，本仓**未显式覆盖**（已 grep 确认无 override） |

## 1. 消息通道（Olm / Megolm）

我方**不实现** Olm/Megolm 原语，全部由 `vodozemac` 提供。以下参数以 Olm/Megolm 规范为准。

| 用途 | 原语 | 出处 |
|---|---|---|
| 一对一密钥协商 | X3DH 变体，**Curve25519** | 📕 Olm spec |
| 一对一棘轮 | Double Ratchet，**HKDF-SHA-256** | 📕 Olm spec |
| 一对一消息加密 | **AES-256-CBC** + **HMAC-SHA-256**（tag 截断 8 字节） | 📕 Olm spec |
| 设备身份签名 | **Ed25519** | 📕 Olm spec |
| 群组棘轮 | Megolm，**SHA-256** 链推进 | 📕 Megolm spec |
| 群组消息加密 | **AES-256-CBC** + **HMAC-SHA-256** | 📕 Megolm spec |
| 群组消息签名 | **Ed25519**（发送者签名） | 📕 Megolm spec |

> ⚠️ **审计口径**：消息通道用的是 **CBC + HMAC（Encrypt-then-MAC）**，不是 GCM。
> 本仓其他位置（附件、备份）确实用 GCM，两者不要混谈。上游 `vodozemac`
> 已由 Least Authority 于 2022-03 审计，建议复用其结论，重点审我方胶水层与协议集成。

## 2. 信封层（Protected Frame v3）

| 项 | 值 | 出处 |
|---|---|---|
| 编码 | **RFC 8949 deterministic（canonical）CBOR** | 📄 `protected_frame_v3.dart`（ADR 15 §3） |
| 信封上限 | 10 MiB | 📄 同上（ADR 15 §7） |
| protected header 上限 | 8 KiB | 📄 同上 |
| 嵌套深度上限 | 16 | 📄 同上 |
| map 条目上限 | 128 | 📄 同上 |

> PFv3 **本身不做加密**：它是 canonical CBOR 信封，承载来自协议层（Olm/Megolm）的
> `ciphertext` 字段并对 header 提供可认证的规范字节。资源上限是抗解析炸弹的硬边界。

## 3. 附件（E2EE-061）

| 项 | 值 | 出处 |
|---|---|---|
| content key | **AES-256**（每附件独立；缩略图另持独立 key） | 📄 `attachment_chunk_codec.dart` |
| 分块 AEAD | **AES-256-GCM**（PointyCastle `GCMBlockCipher(AESEngine())`） | 📄 同上 |
| base nonce | **96 bit**（GCM 标准） | 📄 同上 |
| auth tag | **128 bit**，附于每块密文尾部 16 字节 | 📄 同上 |
| AAD 绑定 | `message_id`（**非** PFv3 `header_hash`——后者已实证不可实现） | 📄 `attachment_binding.dart` |

> ⚠️ **运行时状态**：以上实现全部在仓内且有测试，但**加密开关未翻开**（gap-matrix X12）。
> 今天附件以**明文**存于对象存储。判据是运行时开关，不是代码存在性。详见威胁模型 **T10**。

## 4. 备份容器

| 项 | 值 | 出处 |
|---|---|---|
| KDF | **PBKDF2-HMAC-SHA256** | 📄 `e2ee_crypto_service.dart`（`HMac(SHA256Digest(), 64)`） |
| 迭代次数 | **310,000** | 📄 同上 `pbkdf2Iterations` |
| 派生密钥长度 | **256 bit** | 📄 同上 |
| 内容加密 | **AES-256-GCM** | 📄 同上 `GCMBlockCipher(AESEngine())` |
| 完整性摘要 | SHA-256 | 📄 同上 |
| KDF 可迁移 | 版本字段预留（PBKDF2 → Argon2id），守护测试 `backup_kdf_version_migration_test` | 📄 威胁模型 §4（T6） |

> 310,000 轮对齐 OWASP 对 PBKDF2-HMAC-SHA256 的建议下限。
> **无 HSM / 无硬件绑定**是已知并接受的弱点（见 §9）。

## 5. 本地存储

| 项 | 值 | 出处 |
|---|---|---|
| 数据库加密 | SQLCipher 4（经 `sqflite_sqlcipher ^3.4.0`） | 📄 `pubspec.yaml:62` |
| 分组密码 / HMAC / KDF | SQLCipher 4 **默认参数** | ⚙️ 本仓未覆盖：`cipher_page_size` / `kdf_iter` / `cipher_hmac_algorithm` 三个 PRAGMA 在 `lib/` 全部零命中 |
| DB key 存放 | `db_cipher_key_<uid>` @ 平台 secure storage | 📄 `e2ee_secret_inventory.dart` |

> ⚙️ 标记的含义：这些参数是依赖库默认值，**我方未做安全评估也未显式选定**。
> 若审计方要求明确的分组密码/KDF 参数，需显式下 PRAGMA 并纳入守护测试。

## 6. 密钥透明（KT，未部署）

| 项 | 值 | 出处 |
|---|---|---|
| 树结构 | **RFC 6962 Merkle Tree Hash** | 📄 ADR 29 §2 |
| 哈希 | SHA-256 | 📄 ADR 29 |
| STH 签名 | Ed25519 | 📄 ADR 29 §5 |
| 跨实现 golden vectors | 已核验 | 📄 ADR 29 §8 |

> ⚠️ **未部署**（P3-8）。规范已冻结，无运行时。详见威胁模型 **T11**。

## 7. 遗留（RSA v1，仅解密）

| 项 | 值 | 出处 |
|---|---|---|
| 非对称 | RSA-2048，OAEP-SHA256，e=65537 | 📄 历史实现 |
| 对称 | AES-256-GCM | 📄 同上 |
| 现状 | **仅解密**，不再用于加密新消息 | 📄 ADR 24（Olm-only cutover） |

## 8. 服务端

服务端**不执行任何 E2EE 密码学运算**——不加密、不解密、不签名消息内容。
仅有的密码学用途是与 E2EE 无关的基础设施（JWT 签名、密码哈希、支付网关签名、SSO 配置加密）。

验证：olm 三张表 schema 无任何 `private`/`secret` 列；`grep -rn "private_key" src/` 的命中全部是支付宝网关 / APNS / SSO 配置。

## 9. 已知的密码学弱点（诚实清单）

| 项 | 状态 |
|---|---|
| 附件加密开关未翻开 → 附件今天明文存储 | ❌ X12，**非设计意图** |
| KT 未部署 → 服务端分叉视图不可检测 | ❌ P3-8 |
| Safety Number 生产零调用 → 名义防线不存在 | ❌ B1 |
| 备份 KDF 无 HSM / 无硬件绑定 | 🟡 已接受 |
| 无后量子（PQ）保护 | 🟡 路线图，明确不在当前范围 |
| `vodozemac` 为 **AGPL-3.0** | ⛔ 分发阻断，处置见 X15 |
| SQLCipher 参数用库默认，未显式选定 | 🟡 **本次清单新发现** |
