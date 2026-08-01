# 规范索引（Normative References）

> **层**：Research ｜ **复核周期**：季度 ｜ **本次日期**：2026-08-01
> **用途**：标准层每条 TT 需求的规范锚；审计时审计方核对的"我们声称符合什么"。
> **状态列**：NORM=当前实现的规范依据；INFO=参考；ROADMAP=路线图项启用时才生效。

## 1. IETF RFC / NIST

| 规范 | 标题 | 我方用途 | TT 需求 | 状态 |
|---|---|---|---|---|
| RFC 9420 | Messaging Layer Security (MLS) | 群聊路线图候选协议；大群安全属性参照系 | TT-A3（属性对照）、TT-R2 | ROADMAP |
| RFC 6962 | Certificate Transparency（Merkle Tree） | KT profile v1 的树结构/proof 基础（`e2ee_kt_merkle.erl`、v2/29 号文） | TT-B3、TT-E5、TT-C7 | NORM |
| RFC 9381 | Verifiable Random Functions (VRF) | KT 盲查询隐私（Signal KT 同法，我方 KT 启用时跟随） | TT-B3 | ROADMAP（随 KT） |
| RFC 8032 | Edwards-Curve Digital Signature (Ed25519) | 设备身份签名/trust 事件签名/fallback key 签名/device manifest | TT-A2、TT-B1、TT-E5 | NORM |
| RFC 7748 | Elliptic Curves for Security (X25519) | Olm 双棘轮 DH（vodozemac 内部） | TT-A1 | NORM |
| RFC 5869 | HKDF | Olm/Megolm KDF 链（vodozemac 内部）；备份 KDF 链 | TT-A1、TT-D4 | NORM |
| RFC 8018 | PKCS #5 v2.1（PBKDF2） | 4S 备份口令派生（PBKDF2-HMAC-SHA256 310k 迭代） | TT-E2 | NORM |
| NIST SP 800-38D | AES-GCM | 消息/备份/附件 AEAD | TT-A7、TT-D3 | NORM |
| FIPS 203 | ML-KEM（Kyber 标准化） | PQ 混合握手路线图 | TT-R1 | ROADMAP |

## 2. 协议规范（非 RFC，行业事实标准）

| 规范 | 出处 | 我方用途 | TT 需求 | 状态 |
|---|---|---|---|---|
| X3DH | signal.org/docs/specifications/x3dh/ | Olm 会话建立（vodozemac 实现） | TT-A1、TT-A2 | NORM |
| Double Ratchet | signal.org/docs/specifications/doubleratchet/ | 1:1 逐消息棘进（vodozemac 实现） | TT-A1、TT-A4 | NORM |
| PQXDH | signal.org/docs/specifications/pqxdh/（rev3 2024-01-23） | PQ 握手路线图的直接模板 | TT-R1 | ROADMAP |
| Megolm | matrix.org spec + vodozemac 仓库文档 | 群聊发送者密钥棘轮；room key 轮换规则 | TT-A3 | NORM |
| Matrix 4S（SSSS） | spec.matrix.org 客户端 secrets 章节 | 云备份/恢复密钥模型（P3-1 依据） | TT-B4 | NORM |
| Sesame | signal.org/blog/sesame/（仅博客） | 多设备会话管理参照 | TT-B5、TT-C2 | INFO |
| DMLS | draft-kohbrok-mls-dmls（IETF 个体草案） | 去中心化 MLS 跟踪 | TT-R2 | ROADMAP |
| IETF keytrans WG | datatracker.ietf.org/wg/keytrans/about/ | KT 标准化跟踪（全局一致视图/零用户操作） | TT-B3 | INFO |

## 3. 行业白皮书/威胁模型格式

| 文档 | 出处 | 用途 | 状态 |
|---|---|---|---|
| WhatsApp Encryption Overview（含 Noise Pipes） | whatsapp.com | 传输层设计参照（E4 差距声明依据） | INFO |
| WhatsApp E2EE 备份白皮书 | engineering.fb.com/2021/09/10/security/whatsapp-e2ee-backups | 备份抗爆破托管（HSM/OPAQUE）差距声明依据（TT-E2） | NORM（台账） |
| Apple PQ3 威胁分级 Level 0-3 | security.apple.com/blog/imessage-pq3/ | 威胁模型分级格式（TT-D2） | NORM |
| ETH Threema 三威胁模型 | breakingthe3ma.app | 威胁模型格式（TT-D2）+对抗目录（TT-C3） | NORM |
| DAVE protocol.md | github.com/discord/dave-protocol | 密码套件声明格式（TT-D3：精确到 ciphersuite） | NORM |

## 4. 版本锚定（审计时锁定）

| 库 | 版本 | 锚 | 许可证 |
|---|---|---|---|
| vodozemac / flutter_vodozemac | ^0.5.0（pubspec.lock 锁定 sha256） | Least Authority 2022-03 审计（注意：审计版本与我方版本差异需逐 release note 核对） | **AGPL-3.0（发布门未解）** |
| pointycastle | pubspec.lock | Dart RSA/AES 原语 | — |
| SQLCipher | 随 sqflite_sqlcipher 依赖 | 本地加密库 | — |
| Erlang/OTP crypto | OTP 28+ | 服务端验签（trust 事件） | Apache-2.0 |

> 完整密码学清单（含参数/用途/锚 commit）= 审计包 D3 工件 `../audit-ready-package.md` 组装时生成；本表只列规范级锚。
