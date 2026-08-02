# ADR 08 — Threat Model

> **状态**：Architecture Freeze
> **关联**：所有其他 ADR 的防御决策必须可追溯到本文的某个威胁

---

## 0. Protected Assets（受保护资产）

威胁最终作用的对象是**资产**。先明确资产清单与重要性，任何新增数据（如未来的 AI Embedding）必须先归入此表，否则无法判断其安全级别。

| Asset | 重要性 | 存储位置 | 泄漏后果 |
|---|---|---|---|
| **Identity Key**（Olm ed25519 / curve25519 私钥） | Critical | 仅客户端本地（Keychain/Keystore） | 设备身份可被冒充 |
| **Olm Session State**（Double Ratchet 链） | Critical | 仅客户端本地 pickle | 该对端历史与未来消息可解（直到 ratchet 推进） |
| **Megolm Session Key**（outbound/inbound） | Critical | 仅客户端本地 | 群消息可解（直到 rotate） |
| **Message Plaintext** | Critical | 仅客户端内存 + 本地加密 DB | 通信内容泄漏 |
| **RSA Device Private Key**（v1 兼容） | Critical | 仅客户端本地 | v1 套件消息可解 |
| **Backup Passphrase** | Critical | 仅用户记忆 | 备份可解（叠 PBKDF2 成本） |
| **Device Trust State**（verified/revoked） | High | 客户端 + 服务端审计日志 | 可被篡改诱导误信伪造设备 |
| **Safety Number**（身份键指纹） | High | derived（不单独存储） | 可被篡改诱导跳过验证 |
| **Attachment Content Key**（每附件独立，随 descriptor 走加密 payload） | Critical | 客户端本地 + PFv3 加密 payload 内 | 该附件密文可解；缩略图另持独立 key |
| **Compliance Public Key** | Medium | 服务端 | 审计能力丧失（不影响机密性） |
| **Metadata**（发送方/接收方/时间戳/长度/在线状态/关系图谱/流量模式） | Low（不保护） | 服务端可见 | 与 Signal/WhatsApp 一致，IM 固有特性 |

**资产分级原则**：
- Critical：泄漏直接破坏机密性或身份不可伪造性；
- High：泄漏可被用于社工/降级攻击，但不直接破坏加密；
- Medium：丧失影响功能可用性，不破坏安全；
- Low：显式不保护（诚实声明，非疏漏）。

---

## 1. 攻击者分类

按**能力**与**位置**两个维度建模。每个攻击者明确：是否防御、由哪个 ADR 防御。

| ID | 攻击者 | 能力 | 是否防御 | 防御 ADR |
|---|---|---|---|---|
| T1 | **Curious Operator** | 运营方查 DB / 日志，不修改服务端代码 | ✅ 防御 | 02, 07 |
| T2 | **Compromised Server** | 攻陷服务端，可修改代码、注入恶意逻辑、伪造身份/降级协商 | ⚠️ 部分防御 | 02, 04, 06 |
| T3 | **Compromised DBA** | 直接读写 DB，无应用层权限 | ✅ 防御 | 07 |
| T4 | **Network MITM** | 拦截/篡改传输层（TLS 之上） | ✅ 防御 | 02, 05 |
| T5 | **Compromised Device** | 攻陷用户某一设备，读取本地私钥/ratchet state | ⚠️ 部分防御 | 02(PFS+PCS), 06 |
| T6 | **Stolen Backup** | 获取加密的密钥备份 + 暴力破解口令 | ✅ 防御 | 07(KDF 可迁移) |
| T7 | **Malicious Client** | 伪造客户端，越权操作 / 重放 / 乱序 / 复制消息 | ✅ 防御 | 04, 服务端鉴权, 05 |
| T8 | **Social Engineer** | 诱导用户手动验证伪造设备 | ⚠️ 部分防御 | 06(Safety Number) |
| T9 | **Rollback / State Rewind** | 攻击者（含被攻陷服务端）重放旧 device identity / 旧 OTK / 旧 session state | ⚠️ 部分防御 | 03, 05 |
| T10 | **Object-Store Adversary**（附件） | 读取/替换/挪用 Garage 对象存储里的附件对象，或据其元数据做流量分析 | ❌ **今天不防御**（实现在、开关未翻开） | 27（E2EE-061） |
| T11 | **Key-Server Equivocation**（分叉视图） | 服务端对不同用户提供**不同的**设备身份视图（split-view），或干脆略去某个设备不返回 | ❌ **今天不防御**（KT 未部署） | 29（Transparency Profile v1，冻结未上线） |

---

## 2. 逐威胁分析与防御

### T1 — Curious Operator（好奇的运营方）

**场景**：SaaS 部署方/自托管方有 DB 访问权，想查看用户消息内容，但不修改服务端代码。

**防御**：
- 所有 E2EE 消息 payload 在 DB 中是**密文**（`msg_c2c.payload` 存裸 base64 密文，`e2ee` 列存元数据）；
- 服务端代码零密码学（02 约束：服务端不调用任何 `decrypt` on E2EE payload）；
- 用户私钥永不上传服务端（07）。

**残留风险**：metadata（发送方/接收方/时间戳/消息长度）仍对 Operator 可见。这是 IM 的固有特性，不防御（与 Signal/WhatsApp 一致）。

**守护测试**：`grep -rn "elib_cipher.*decrypt.*e2ee_payload\|jsone:decode.*e2ee.*payload" imboy/src` 应零命中。

---

### T2 — Compromised Server（服务端被攻陷）

**场景**：攻击者获得服务端代码执行权，可注入恶意逻辑。两条具体攻击路径：
- **Fake Identity**：向客户端推送伪造的 device identity（fake curve25519 key），实施 MITM；
- **Downgrade Attack**：伪造对端的 capability 声明（"我不支持 Olm"），诱导双方降级到更弱的协议（如 RSA-OAEP，无 PFS）。

**不防御的部分**（诚实声明）：
- 这是 **E2EE 的根本限制**：除非用户主动通过带外信道（扫码/安全号码）验证对端设备，否则无法完全防御服务端 MITM。

**防御的部分**：
- **TOFU + Safety Number**（06）：用户可主动验证对端身份键指纹，发现服务端篡改；
- **Signed Capabilities**（04）：capabilities 由对端 Ed25519 身份键签名，服务端无法伪造降级声明；
- **本地降级告警**（04）：客户端记录对端历史 capability，异常降级触发 TOFU 告警；
- **Cross-signing**（06 接口位）：未来支持「已验证设备签名新设备」，服务端无法伪造签名链。

**残留风险**：未做 Safety Number 验证的用户，在服务端被攻陷时仍可被 MITM 或被诱导降级。这是**显式接受的权衡**——完整防御要求用户参与验证，UX 成本高。

---

### T3 — Compromised DBA（数据库管理员）

**场景**：DBA 直接读写 PostgreSQL，绕过应用层。

**防御**：
- 私钥（用户 RSA 私钥 / Olm pickle / Megolm session key）**永不落 DB**（07）；
- DB 中所有加密相关列都是公钥侧（`user_device.public_key`、`olm_identity.curve25519_key` 等）；
- 合规私钥改造后（线 A）也已 DROP（`compliance_key.private_key_encrypted` 不复存在）。

**残留风险**：DBA 可篡改公钥（替换为攻击者公钥），但这是 T2 的子集，由 Safety Number 兜底。

---

### T4 — Network MITM（传输层中间人）

**场景**：在 TLS 之上（或 TLS 被绕过）拦截/篡改 E2EE 消息。

**防御**：
- E2EE 消息本身是端到端加密，MITM 只能看到密文；
- **AEAD 认证**（AES-256-GCM 的 tag）：任何篡改导致解密失败；
- **Olm Ed25519 签名**：身份键签名防服务端/网络篡改 device identity；
- **Megolm 消息完整性**：session key + ciphertext 绑定，篡改不可解。

**不防御**：metadata（见 T1）。

---

### T5 — Compromised Device（设备被攻陷）

**场景**：攻击者获取用户某一设备的访问权，读取本地存储的私钥/pickle（含 ratchet state）。

**防御**：
- **Per-message PFS（Olm Double Ratchet 前向保密）**：即使 ratchet state 泄漏，**已被推进过的历史链**仍安全；
- **Post-Compromise Security（PCS）**：泄漏后，只要对端发送下一条消息触发 ratchet step，新 DH 协商会刷新链，泄漏状态失效。**PCS 恢复依赖后续 ratchet 推进，非即时**——若对端长期不发消息，泄漏窗口持续；
- **Megolm rotate**：群成员变化触发 rotate，被踢成员无法解密新消息；
- **私钥落盘加密**（07）：iOS Keychain / Android Keystore / Web IndexedDB（非 extractable）。

**残留风险**：
- 攻陷时**正在进行的会话**在 PCS 恢复前可被实时解密；
- 若攻击者复制**整个 ratchet state** 并静默监听，可解密直到下一次 ratchet 推进覆盖；
- Megolm 的 PCS 弱于 Olm（sender 共享同一 chain，需等 sender rotate）。

---

### T6 — Stolen Backup（备份被盗）

**场景**：攻击者获取用户的加密密钥备份文件（服务端备份 / 本地导出），尝试暴力破解口令。

**防御**：
- **KDF 可迁移**：当前默认 PBKDF2-HMAC-SHA256（310,000 次迭代）；备份格式含 magic number + KDF 版本字段，未来可平滑升级到 Argon2id 而不破坏存量；
- **随机 salt（16 字节）+ AES-256-GCM**；
- **不锚定具体推荐来源**（如「OWASP 2021」）：ADR 生命周期 5+ 年，推荐会变，参数应以当前密码学共识为准并支持迁移。

**残留风险**：弱口令仍可被离线暴力破解（这是用户口令的固有问题，非架构缺陷）。

---

### T7 — Malicious Client（恶意客户端）

**场景**：攻击者构造伪造客户端，尝试越权操作（如向无权限群注入 session、claim 他人 OTK）或消息层重放/乱序/复制。

**防御**：
- **服务端鉴权**：所有 E2EE 端点校验 `current_uid` + 资源所有权（如 `group_member_keys` 校验 `is_member`）；
- **room key 域一致性校验**（Megolm 已实现）：`envelopeType=C2G && gid != envelopeTo` 拒收；
- **OTK claim 原子消费**（03）：`SELECT FOR UPDATE SKIP LOCKED + DELETE`，防并发重复领取；
- **消息重放/乱序/复制**：
  - Olm/Megolm 协议层内置 ratchet counter，重复密文解密失败；
  - 应用层按 `msg_id` 去重（`ON CONFLICT DO NOTHING` 已实现）；
  - Megolm `message_index` 可用于检测乱序（ADR 05 会定）。

---

### T8 — Social Engineer（社工攻击）

**场景**：攻击者诱导用户主动验证一个伪造设备（"这是我新换的手机，请验证"）。

**防御（部分）**：
- Safety Number 显示双方身份键的稳定指纹，用户可通过带外信道比对；
- Trust State 明确展示（06）：`verified` / `unverified` / `revoked` 三态可视化。

**不防御**：用户主动误操作。这是 UX 与安全的永恒权衡。

---

### T9 — Rollback / State Rewind（状态回滚攻击）

**场景**：被攻陷的服务端（或具备 DB 写权限的攻击者）向客户端返回**旧的** device identity / 旧的 OTK / 旧的 session 元数据，诱导客户端回退到已被弃用或已泄漏的历史密钥状态。

典型路径：
- 返回用户「上一次」的 curve25519 公钥（已被替换的旧身份），让对端用泄漏过的旧身份加密；
- 返回已被 claim 但未清理的旧 OTK，复活一次性密钥；
- 返回旧的 Megolm session_id，让客户端用已 rotate 的旧 session 解密（信息泄漏面扩大）。

**防御（部分）**：
- **单调递增的 device identity 版本号**（ADR 03）：客户端本地记录对每设备见过的 `highest_seen_key_version`，服务端返回更小版本时拒收并告警；
- **OTK 一次性消费 + 审计字段**（ADR 03）：OTK 一旦 consumed 不可再被 claim，cleanup worker 物理清理过期记录；
- **Megolm session_id 全局唯一 + rotate 单调**：客户端拒绝比已知更早的 session；
- **device message counter**（ADR 05）：消息级单调计数，重放/乱序可检测。

**残留风险**：
- 首次见到某设备的客户端无历史版本可比对（TOFU 窗口）；
- 多设备场景下「新增设备」与「旧设备复活」难以仅凭版本号区分，需结合用户行为告警。

**与 MLS 的关系**：MLS 协议内置 epoch 单调度，天然抗 rollback；Olm/Megolm 需在应用层补这一层。本 ADR 的版本号约束是 Olm/Megolm 阶段的过渡防御，MLS 落地后由协议层接管。

---

### T10 — Object-Store Adversary（附件）

**场景**：附件走 Garage S3 直传，密文/明文以独立对象存放，其访问控制与消息通道是两套机制。攻击者取得对象（越权、凭证泄漏、或本就是运营方）后可读取、替换或把 A 消息的附件对象挪到 B 消息。

ADR 27 已把该面拆成 **ATT-01..05** 五条验收用例：

| 用例 | 攻击 |
|---|---|
| ATT-01 | 附件对象从消息 A 换到消息 B |
| ATT-02 | 交换 / 删除 / 重复 / 截断 chunk |
| ATT-03 | 篡改 MIME / name / size / hash / chunk_count |
| ATT-04 | 未授权方拿到原始对象 |
| ATT-05 | 下载解密中途被 kill 或磁盘满，残留明文 |

**今天的判定：❌ 不防御。** 分块 AEAD、AAD 绑定（`attachment_binding.dart`，绑 `message_id` 而非 `header_hash`——后者已实证不可实现）、封装编排、加密闸门、临时明文清理均**已实现**，但**开关未翻开**（gap-matrix **X12**，Slice 9 真机 BLOCKED）。开关未翻开意味着附件今天**以明文存放于对象存储**，ATT-04 直接失败，ATT-01/02/03 因无 AAD 绑定同样不成立。

**审计口径**：不要因为仓里有 `attachment_encryptor.dart` 就认为附件已加密。判据是运行时开关，不是代码存在性。

**残留风险（即使开关翻开后）**：对象大小与上传时序仍是元数据，属 §3 显式不保护项；缩略图需独立 content key 同样加密，否则 ATT-04 在缩略图上照样失败。

---

### T11 — Key-Server Equivocation（分叉视图）

**场景**：被攻陷或恶意的服务端对不同用户返回**不同的**设备身份视图——给 Alice 看真实的 Bob 设备列表，给 Charlie 看一份多插了一台攻击者设备的列表；或反过来，对某些查询干脆略去一台设备（non-inclusion）。因为每个客户端只能看到自己那一份，单凭本地视角**无法察觉不一致**。

这是 T2（Compromised Server）中**单靠端到端加密无法覆盖**的那一块：加密保证了信道，但设备列表本身来自服务端。

**今天的判定：❌ 不防御。** IMBoy Transparency Profile v1（ADR 29）已冻结树结构、canonical event bytes、STH、proof wire 格式与跨实现 golden vectors，但**未部署**（P3-8）。没有 Merkle 一致性证明与 gossip/witness，分叉视图在今天**不可检测**。

**理论上的当前防线及其实际状态**：
- **Safety Number 人工比对** —— §4 矩阵把它列为 T2/T8 的防御点。⚠️ **但 `safety_number.dart` 在生产代码里零调用**（`grep -rl "SafetyNumber" lib/` 只命中它自己），算法有守护测试、产品用不到。**这条防御在今天的产品里事实上不存在**（gap-matrix B1，P3-4/P3-5）。
- **TOFU + 降级告警** —— 已接线，但只能发现"变了"，无法发现"从一开始就是分叉的"。

**残留风险**：即便 KT 部署，split-view 的检测仍依赖客户端间的 gossip 或第三方 witness；只有 STH 而无 gossip 时，服务端仍可对**长期隔离**的用户维持一致的假视图。

---

## 3. 不防御的明确声明（诚实清单）

| 不防御项 | 原因 |
|---|---|
| Metadata 暴露（发送方/接收方/时间/长度） | IM 固有特性，与 Signal 一致 |
| 服务端被攻陷时的实时 MITM（未验证设备间） | E2EE 根本限制，需用户主动验证 |
| 设备攻陷后的实时解密 | E2EE 根本限制，PFS 仅保护历史 |
| 用户主动误操作（验证伪造设备） | UX 限制 |
| 量子计算攻击 | 不在本架构范围（后量子签名为未来研究方向） |
| **附件机密性（T10）** | 加密实现已在仓内但**运行时开关未翻开**（X12）。今天附件以明文存于对象存储，ATT-01..05 全部不成立。**这是当前状态，不是设计意图** |
| **服务端分叉视图（T11）** | KT profile v1 已冻结但未部署（P3-8）。名义防线 Safety Number **生产零调用**（B1），故今天无任何可用的分叉检测手段 |
| Megolm room key 分发列表的规模元数据 | 列表长度暴露群设备数量级；上限 4096 条仅防 DoS，不隐藏规模 |

---

## 4. 威胁与防御点的可追溯矩阵

每个防御点必须能追溯到至少一个威胁 ID，否则属于过度设计。**每项防御绑定一个守护测试**，PR 删除测试时 review 立即可见。

| 防御点 | 防御的威胁 | 实现 ADR | 守护测试 |
|---|---|---|---|
| 服务端零密码学 | T1, T3 | 02, 07 | `grep "elib_cipher.*decrypt.*e2ee_payload" imboy/src` 零命中（CI 守护） |
| 私钥永不落 DB | T3 | 07 | `private_key_encrypted 列已 DROP` 守护测试（已落地） |
| Per-message PFS (Olm DR) | T5 | 02 | `olm_pfs_*` 系列：旧 session key 无法解密新消息 |
| Post-Compromise Security | T5 | 02 | `olm_pcs_recovery_test`：状态泄漏后 ratchet 推进恢复 |
| AEAD (AES-256-GCM) | T4 | 02 | `aes_gcm_tamper_fails_test` |
| Ed25519 身份键签名 | T2, T4 | 03 | `device_identity_signature_verify_test` |
| Safety Number | T2, T8 | 06 | `e2ee_safety_number_test`（指纹稳定 + 篡改检测）⚠️ **算法有测试，生产零调用**（B1） |
| Signed Capabilities | T2 | 04 | `capability_signature_forgery_fails_test` |
| 本地降级告警 | T2 | 04 | `capability_shrink_triggers_tofu_alert_test` |
| OTK 原子 claim | T7 | 03 | `otk_concurrent_claim_uniqueness_test` |
| room key 域一致性校验 | T7 | 02 (Megolm) | `c2g_e2ee_room_key_relayed_opaque_and_skips_gate`（已落地） |
| 消息重放/乱序/复制 | T7 | 05 | `message_replay_rejected_test` / `out_of_order_decrypt_test` |
| KDF 可迁移（PBKDF2→Argon2id） | T6 | 07 | `backup_kdf_version_migration_test` |
| Trust State 审计 | T2, T8 | 06 | `device_trust_state_change_audit_log_test` |
| Device identity 版本单调 | T9 | 03 | `device_identity_rollback_rejected_test` |
| Megolm session rotate 单调 | T9 | 02 | `megolm_old_session_rejected_test` |
| PFv3 canonical CBOR + 资源上限 | T4, T7 | 15, 26 | `protected_frame_v3_test` / `protected_frame_v3_roundtrip_test`（10 MiB 信封 / 8 KiB header / 深度 16 / 128 项） |
| 附件分块 AEAD + AAD 绑定 `message_id` | T10(ATT-01/02) | 27 | `attachment_binding_test` / `attachment_chunk_codec_test` / `attachment_open_e2e_test` ⚠️ **开关未翻开，运行时未生效** |
| 附件解密临时明文清理 | T10(ATT-05) | 27 | `attachment_temp_hygiene_test` ⚠️ 同上 |
| 附件加密闸门（descriptor 只随加密 payload 走） | T10(ATT-04) | 27 | `attachment_seal_policy_test` / `attachment_seal_wiring_test` ⚠️ 同上 |
| 缩略图独立 content key 同样加密 | T10(ATT-04) | 27 | `attachment_thumb_seal_test` ⚠️ 同上 |
| Merkle 一致性证明 / STH | T11 | 29 | ❌ **未部署**，仅有跨实现 golden vectors |

> ⚠️ **矩阵读法警告（2026-08-02 加）**：本矩阵证明的是「**该防御的算法被测试覆盖**」，
> 不等于「**该防御在产品运行时生效**」。上表已逐条核实 16 条原有守护测试**全部真实存在**，
> 但其中 Safety Number 在生产代码零调用、附件三项的运行时开关未翻开。
> 审计时请把「有守护测试」与「已接线并启用」当作两个独立问题分别求证。

**测试命名约定**：`<主题>_<场景>_test`，放在对应模块的 `test/` 子目录；守护测试必须在 CI 关键路径（非 nightly）。

---

## 5. Threat Model 的演进

Threat Model **允许演进**（不像接口/表结构冻结）。新增威胁时：
1. 在本文 §1 表格追加威胁 ID + 攻击者描述；
2. §2 分析该威胁，明确「防御 / 部分防御 / 不防御」及理由；
3. §4 矩阵关联防御点；
4. 若需新代码守护，在对应 ADR 追加测试要求。
