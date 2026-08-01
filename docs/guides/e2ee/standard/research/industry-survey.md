# 行业调查：顶级 IM E2EE 玩家现状（2026-08 快照）

> **层**：Research ｜ **复核周期**：季度 ｜ **本次日期**：2026-08-01
> **可信度**：✅一手来源 / ⚠️二手或未抓取原文 / ❌未找到
> **用途**：`../top-tier-standard-2026.md` 的需求依据；`benchmark.md` 的数据源。

## 0. 总结论

2026 年"顶级 E2EE"事实标杆已从"双棘轮+安全码"演进为五件套：**PQ 混合握手 + PQ 持续棘轮 + Key Transparency + 强制设备验证 + 公开第三方审计/形式化验证**。Signal 与 Apple 全线落地；WhatsApp 缺 PQ；Matrix 缺 PQ 与 MLS；Threema 刚补前向保密；Wire 已迁 MLS。

## 1. Signal

- **协议**：PQXDH（2023-09，X3DH+ML-KEM-1024 混合握手，规范 rev1→rev3，rev2 修形式化发现问题）✅ https://signal.org/docs/specifications/pqxdh/ ；SPQR/Triple Ratchet（2025-10 宣布，Double Ratchet+ML-KEM-768 稀疏 PQ 棘轮，ProVerif+hax 持续验证）✅ https://pqshield.com/diving-into-signals-new-pq-protocol/ ；Sesame 多设备（2017，仅博客无正式规范）⚠️
- **验证**：PQXDH ProVerif+CryptoVerif（ePrint 2023/1738）✅；原始协议 Cohn-Gordon 2017（ePrint 2016/1013）✅；**无公开 NCC/ToB/Cure53 代码审计**（证伪，见 public-audit-cases.md）
- **可验证性**：60 位安全码/QR；**KT 进行中**——libsignal 已实现（Merkle 前缀树+日志树、RFC 9381 VRF 盲查询），官方审计器参考实现 https://github.com/signalapp/key-transparency-auditor ，Android beta 7.70（2026-01）含 KT ✅
- **对 IMBoy 的启示**：KT 审计器"独立可运行、不一致即抛异常"是 TT-B3/C7 的直接对标。

## 2. WhatsApp

- **协议**：Signal Protocol（消息层）+ Noise Pipes（传输层 Curve25519/AES-GCM/SHA256，服务器不存认证凭据）✅
- **KT（生产级）**：AKD 追加式目录+epoch 批量提交（2023-04）✅ https://engineering.fb.com/2023/04/13/security/whatsapp-key-transparency/ ；开源 https://github.com/facebook/akd ；Cloudflare Plexi 第三方审计器 ✅ https://blog.cloudflare.com/key-transparency/ ；NCC 实现评审 20 人日（2023-08）✅
- **E2EE 备份（生产级）**：设备随机密钥+OPAQUE+HSM Vault 限次防爆破（2021-10）；NCC 35 人日评估+UC 框架学术证明 ✅ https://engineering.fb.com/2021/09/10/security/whatsapp-e2ee-backups/
- **PQ**：❌未部署（postquantumsecurity.org 盘点）
- **对 IMBoy 的启示**：备份抗爆破托管（HSM/OPAQUE）是我们 PBKDF2 方案的差距声明依据（台账 E2 项）。

## 3. Apple iMessage

- **PQ3**（2024-02，iOS 17.4 默认）：握手 Curve25519+ML-KEM（Kyber-1024 级）+周期 PQ rekey（Kyber-768 级，约 50 条/7 天）；Level 3（握手+持续棘轮均 PQ）；每条消息带签名→放弃 deniability ✅ https://pqshield.com/post-quantum-messaging-examining-apples-new-pq3-protocol/
- **形式化验证**：ETH Zürich Tamarin 机器检查（约 2.5 人月，覆盖无界棘轮循环，未发现攻击）ePrint 2024/1395 ✅；Stebila 归约式证明 ✅
- **CKV**（2023-10，iOS 17.2）：KT 可验证映射+自动异常告警+高风险用户手动比对 ✅（Apple HT213465）
- **对 IMBoy 的启示**：威胁模型 Level 0-3 分级格式（TT-D2 对标）；2.5 人月是形式化验证的成本锚（路线图 TT-R3）。

## 4. Matrix / Element

- **协议**：Olm（1:1 双棘轮）+Megolm（群发送者密钥棘轮）；**vodozemac**（纯 Rust，matrix-rust-sdk/Element X 加密核心）✅ https://github.com/matrix-org/vodozemac
- **审计**：NCC 2016（Olm 1.3.0，报告公开本次 403 未取明细）⚠️；**Least Authority 2022-03**（vodozemac，10 issue A-J+8 建议：zeroization 不全/key ID 错/整数溢出/旧棘轮状态清除/MAC 截断 8B 协议遗留）✅ https://leastauthority.com/static/publications/LeastAuthority-Matrix_vodozemac_Final_Audit_Report.pdf ；2022 实战漏洞（IEEE S&P 2023，设备冒充/跨签名绕过）✅ https://nebuchadnezzar-megolm.github.io/static/paper.pdf ；2024 Tamarin "Matrix Reloaded" ✅
- **可验证性**：SAS emoji/QR+交叉签名+4S（SSSS）备份；**强制设备验证**：2025-11 宣布未验证设备将禁止收发 E2EE 消息（2026-10 生效）✅ https://element.io/blog/verifying-your-devices-is-becoming-mandatory-2/
- **MLS**：不在生产；DMLS 草案（draft-kohbrok-mls-dmls，FRCGKA/FREEK 抗分叉）✅ arewemlsyet.com
- **对 IMBoy 的启示**：同族协议栈，其审计发现（zeroization/advance_to/MAC 截断）直接进我方台账核对清单；4S 备份模型=P3-1 依据；强制验证=B5 先例。

## 5. Threema

- **协议**：NaCl 自研→**Ibex**（5.0，ECDH 临时密钥+KDF 棘轮补 FS，形式化分析）✅ https://threema.com/en/blog/ibex
- **审计**：Cure53 移动（2020，16 天×3 人，8 发现均非实际漏洞，评"代码质量异常扎实"）✅；Cure53 桌面（2024，7 发现无 Critical/High）✅；**ETH USENIX Sec 2023**（三威胁模型 7 攻击：认证伪造/压缩侧信道/重放反射）https://breakingthe3ma.app/ ✅
- **PQ**：路线图（2026-02 与 IBM Research 合作）⚠️
- **对 IMBoy 的启示**：七攻击目录=对抗测试用例源（TT-C3）；三威胁模型=TT-D2 格式对标。

## 6. Wire

- **协议**：生产已从 Proteus 迁 **MLS**（core-crypto Rust，维护 OpenMLS 分叉，FFI 全平台）+E2EI（OIDC 证书身份）✅ https://github.com/wireapp/core-crypto
- **对 IMBoy 的启示**：MLS 生产可行性的现存证明，但属"大群/互通"路线——见 ADR 31 的不迁论证。

## 7. MLS 产业状态（横切）

- RFC 9420（2023-07）✅；**GSMA RCS UP 3.0**（2025-03）以 MLS 定义 E2EE，首个跨厂商互通大规模部署；iPhone↔Android 加密 RCS 2026-05 beta ✅ https://www.gsma.com/newsroom/article/rcs-encryption-a-leap-towards-secure-and-interoperable-messaging/
- OpenMLS：功能完备+SRLabs 审计（2025，8 发现 1 High 已修）但 pre-1.0、libcrux 未全审 ✅ https://blog.phnx.im/openmls-independent-security-audit/ ；AWS mls-rs 自称一致但**声明未审计** ✅
- TreeKEM 成员变更 O(log n) vs Megolm O(n)；MLS 每 commit 连续 FS/PCS ✅ https://eprint.iacr.org/2025/554.pdf

## 8. 后量子横切表

| 玩家 | PQ 状态 |
|---|---|
| Signal | ✅ PQXDH(2023)+SPQR(2025) |
| Apple | ✅ PQ3(2024，Level 3) |
| WhatsApp | ❌ 未部署 |
| Matrix | 🔬 研究阶段 |
| Wire | ❌（MLS 标准套件非 PQ） |
| Threema | 🔬 IBM 合作(2026-02) |

**代价数据**：ML-KEM-1024 公钥/密文 ~1568B vs X25519 32B（约 50 倍）；Kyber-768 交换 ~1100B vs ECDH 64B。建议路径=先混合握手（X25519+ML-KEM）后评 PQ 棘轮，不上纯 PQ。
