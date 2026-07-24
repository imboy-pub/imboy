# ADR 07 — Storage

> **状态**：Architecture Freeze
> **冻结日期**：2026-07-18
> **关联**：ADR 02(protocol) / 03(device-identity) / 06(device-trust) / 08(threat-model T3, T5, T6)
> **不可单方面变更**：服务端零密码学契约、客户端存储 key 命名规范、`StorageProvider` 接口签名、备份 KDF 版本字段语义

---

## 1. 决策

存储层是零信任的物理下限：T3（DBA）要求私钥永不落 DB，T5（设备攻陷）要求私钥落盘必须加密，T6（备份被盗）要求 KDF 可迁移。本 ADR 冻结以下契约：

1. **服务端零密码学**（线 A 已落地）：服务端永不接收、永不在 DB 中存储任何用户私钥；DB 中所有「加密相关列」都是公钥侧；E2EE 消息 payload 以密文裸存（`msg_c2c.payload`）。
2. **客户端敏感数据分级落盘**：按 Critical / High / Rebuildable 三级决定存储机制与销毁严格度。
3. **跨平台抽象**：移动端 / 桌面端复用 `flutter_secure_storage`（iOS Keychain / Android Keystore / macOS Keychain / Windows DPAPI / Linux libsecret）；Web 端定义 `StorageProvider` 接口，多后端实现，私钥用 WebCrypto `non-extractable` CryptoKey。
4. **备份走 4S 模式**：服务端只存密文包，客户端用 PBKDF2-HMAC-SHA256 (310k 迭代) + AES-256-GCM 加密；备份格式含 `algo`/`kdf_iterations` 字段，KDF 可平滑迁移。
5. **不锚定具体 OWASP 版本号**：ADR 生命周期 5+ 年，外部推荐周期远短于此，参数以当前密码学共识为准且支持迁移。

**为什么是 ADR 而非 README**：存储 key 命名、销毁时机、KDF 参数都是跨端契约；任一客户端单方面改 key 命名会破坏服务端审计 grep 与多端互操作。冻结为 ADR 让任一平台的改动必须走 supersedes 流程。

---

## 2. 客户端敏感数据分级

承接 ADR 08 §0 的资产清单，按「泄漏后果 + 可否重建」分三级。级别决定存储机制与销毁严格度。

| 级别 | 数据 | 为什么 | 存储要求 |
|---|---|---|---|
| **Critical** | v1 RSA 设备私钥（PEM） | 泄漏即破 v1 套件所有消息 | OS Keychain/Keystore；落盘需系统级加密 |
| **Critical** | v2 Olm Account pickle（含 ed25519 + curve25519 私钥） | 泄漏即冒充设备身份 | pickle 必须用 pickle key 加密后存 |
| **Critical** | Olm pickle key（设备级 secret 派生） | 解锁所有 pickle 的主钥 | Keychain/Keystore；不可与 pickle 同介质明文并存 |
| **Critical** | Megolm outbound session key（本设备发出的群消息） | 泄漏即解本端发出的群历史 | Keychain/Keystore |
| **Critical** | E2EE 备份口令（用户记忆） | 解备份即得全部私钥 | **客户端不落盘**；仅用户记忆 |
| **High** | v2 Olm Session pickle（每对端 DR 状态） | 泄漏即解该对端历史/未来消息直到 ratchet 推进 | 加密 pickle 落盘；PCS 后失效 |
| **High** | Megolm inbound session key | 泄漏即解收到的群历史 | 加密落盘；rotate 后旧 key 失效 |
| **Rebuildable** | 群级 E2EE 旗标（`group_e2ee_mode_<gid>`） | 服务端是权威来源，丢失可重拉 | 可普通 secure storage，丢失不影响安全 |
| **Rebuildable** | Shard 元数据列表 | 重新分发可重建 | 普通 secure storage |

**分级原则**：Critical 不可重建且泄漏破坏身份/全部历史；High 可被 PFS/rotate 限制窗口；Rebuildable 有权威源（服务端或协议层）兜底。**Rebuildable 仍走 secure storage**（不降级到明文），只是丢失不致命。

---

## 3. 移动端存储（iOS / Android）

### 3.1 复用 FlutterSecureStorage

现有 `imboyapp/lib/service/storage_secure.dart` 已封装 `flutter_secure_storage`，底层是 iOS Keychain（`kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly`）与 Android Keystore（EncryptedSharedPreferences + AndroidKeyStore RSA master key）。本 ADR 冻结其作为移动端唯一敏感数据存储入口的地位，禁止业务代码直接用 `SharedPreferences` / `NSUserDefaults` / 文件系统存任何 Critical/High 数据。

**为什么不另起炉灶**：Keychain/Keystore 是各自 OS 上唯一提供硬件级密钥保护的成熟 API；自研加密必然退化为软件白盒，违反 T5 防御。复用既有封装同时避免迁移成本。

### 3.2 存储 key 命名规范（冻结）

下表是当前线上在用的 key 名，本 ADR 冻结其为契约。服务端 CI grep、客户端 wipe、销毁测试都按此表对齐。

| 数据类型 | 存储 key | 平台 | 生命周期 | 来源 |
|---|---|---|---|---|
| v1 RSA 私钥（当前） | `e2ee_private_key` | iOS/Android/Desktop | 登录创建，退出销毁 | `storage_secure.dart:160` |
| v1 RSA 历史私钥 | `e2ee_private_key_history_<kid>` | iOS/Android/Desktop | 轮换归档，退出销毁 | `storage_secure.dart:155` |
| v1 RSA 公钥 | `e2ee_public_key` | iOS/Android/Desktop | 同上 | `storage_secure.dart:186` |
| 设备 ID | `e2ee_device_id` | iOS/Android/Desktop | 登录创建，退出销毁 | `storage_secure.dart:198` |
| 密钥 ID | `e2ee_key_id` | iOS/Android/Desktop | 同上 | `storage_secure.dart:210` |
| 密钥创建时间 | `e2ee_key_created_at` | iOS/Android/Desktop | 同上 | `storage_secure.dart:222` |
| Olm Account pickle | `olm_account_pickle` | iOS/Android/Desktop | 身份键生成时创建，退出销毁 | `olm_session_service.dart:30` |
| Olm pickle key | `olm_pickle_key` | iOS/Android/Desktop | 首次pickle前派生，退出销毁 | `olm_session_service.dart:27` |
| Olm Session pickle（per 对端 DR） | `olm_session_<peerUid>:<peerDeviceId>` | iOS/Android/Desktop | 建会话时创建，退出销毁 | `olm_session_service.dart:33` |
| Megolm inbound session key | `megolm_inbound_<gid>:<sessionId>` | iOS/Android/Desktop | 收到 room key 创建，rotate 后旧值可清 | `group_session_service.dart:41` |
| 群 E2EE 旗标 | `group_e2ee_mode_<gid>` | iOS/Android/Desktop | S2C 广播写，退出可清 | `group_session_service.dart:40` |
| DB 加密 key | per-uid（见 `db_encryption_key_service.dart`） | iOS/Android/Desktop | 首启生成，退出销毁 | `db_encryption_key_service.dart` |
| 社交恢复分片（代理端） | `e2ee_shard_<shardId>` + `e2ee_shard_id_list` | iOS/Android/Desktop | 接收时写，所有者撤销时清 | `storage_secure.dart:257` |
| 分片元数据（所有者端） | `e2ee_shard_metadata_list` | iOS/Android/Desktop | 分发时写，恢复后可清 | `storage_secure.dart:329` |

**命名约定**：前缀按数据域（`e2ee_` / `olm_` / `megolm_` / `group_e2ee_`），后接稳定 ID（`<kid>` / `<peerUid>:<peerDeviceId>` / `<gid>:<sessionId>`）。**禁止**把动态 ID 拼到固定前缀以外的位置（避免 wipe 时漏清）。

### 3.3 生命周期

| 事件 | 客户端动作 |
|---|---|
| 首次登录 | 生成身份键（v1 RSA 或 v2 Olm Account），写 `e2ee_private_key` / `olm_account_pickle` |
| 身份键轮换 | 旧私钥归档到 `e2ee_private_key_history_<oldKid>`（解决历史密文死锁，C2），写新值 |
| 收到 room key | 写 `megolm_inbound_<gid>:<sessionId>` |
| 退出登录 | 调用 `deleteAllE2EEKeys` + 显式清所有 `olm_*` / `megolm_*` / `group_e2ee_*` / `e2ee_shard_*` / DB 加密 key |
| 换设备（迁移完成） | 同退出登录 |
| 远程 revoke（T5 防御） | 同退出登录，由服务端推送触发本地 wipe |

**为什么退出登录必须 wipe 而不是软删**：登录态与身份键是一对一绑定的；保留身份键却退出登录意味着「下一个登录的用户可解上一个用户的密文」，直接违反 T5。

---

## 4. Web 端 StorageProvider 抽象

### 4.1 为什么 Web 需要单独的抽象

浏览器无 Keychain/Keystore 等价物：`localStorage`/`sessionStorage` 对 JS 全明文可见（XSS 即获全量私钥）；IndexedDB 可存 `CryptoKey`（WebCrypto 标记 `extractable: false`），受同源策略保护但仍不及 Keychain 硬件隔离；不同 Web 运行时（纯浏览器/Electron/Capacitor/扩展）后端差异大。因此 Web 端不复用 `flutter_secure_storage` 的 web 实现（其 web 后端是 localStorage 加密，安全性弱），而是定义 `StorageProvider` 接口由各运行时注入最强可用后端。

**诚实声明（承接 ADR 08 §T5）**：Web 端安全性**严格弱于**移动端 Keychain/Keystore；XSS 或恶意扩展可读 IndexedDB。这是 Web 平台固有限制，非架构缺陷。Critical 数据（如 Olm Account pickle）在 Web 端存 IndexedDB 是**显式接受的权衡**，优于明文 localStorage，但不及原生。

### 4.2 StorageProvider 接口（TypeScript）

```typescript
/**
 * Web 端敏感数据存储抽象。移动/桌面端不实现此接口（直接用 flutter_secure_storage）。
 * 实现约束：read/write/delete/clearAll 全异步；私钥类数据以 WebCrypto CryptoKey
 * （non-extractable）形式存；clearAll 必须能枚举本 provider 写过的全部 key（不能漏清）。
 */
export interface StorageProvider {
  read(key: string): Promise<string | null>;            // 不存在返回 null
  write(key: string, value: string): Promise<void>;     // null/undefined 视为 delete
  delete(key: string): Promise<void>;                   // 不存在时静默成功
  clearAll(): Promise<void>;                            // 清本 provider 写入的全部 E2EE 数据

  /** 存 WebCrypto CryptoKey（non-extractable）。IndexedDB 后端直接存对象，
   *  Memory 后端存引用。普通 read/write 不可读取此 key。 */
  putCryptoKey(key: string, ck: CryptoKey): Promise<void>;
  /** 读取此前 putCryptoKey 存入的 CryptoKey；不存在返回 null。 */
  getCryptoKey(key: string): Promise<CryptoKey | null>;
}
```

### 4.3 多后端实现

| 实现 | 适用场景 | 私钥存储 | 安全性 |
|---|---|---|---|
| `IndexedDBProvider`（默认 Web） | 纯浏览器 SPA | WebCrypto `non-extractable` CryptoKey 存 IndexedDB（同源策略保护） | 弱于 Keychain，强于 localStorage |
| `MemoryProvider` | 单测 / 临时会话 | 内存 Map，进程退出即丢 | 无持久化，仅测试 |
| `ElectronProvider` | Electron 桌面（imboy-desktop） | `safeStorage`（macOS Keychain / Windows DPAPI / Linux libsecret via Electron API） | 等同原生 |
| `CapacitorProvider` | Capacitor 混合 App | `@capacitor-community/secure-storage`（落到原生 Keystore/Keychain） | 等同原生 |
| `BrowserExtensionProvider` | 浏览器扩展（MV3） | `storage.session`（MV3 service worker 内存）+ IndexedDB 兜底 | MV3 service worker 重启后丢失，需重新登录 |

**默认选择规则**：构造 SDK 时由调用方注入；不注入时按运行时探测（`window.crypto.subtle` 存在 + `indexedDB` 存在 → IndexedDB；`require('electron')` 成功 → Electron；否则 Memory + 警告）。

### 4.4 私钥的 non-extractable 语义

WebCrypto 的 `subtle.generateKey({ ..., extractable: false })` 产出的 `CryptoKey` 即便被 XSS 拿到，也无法导出原始字节（`exportKey` 抛错）。IndexedDB 可直接存 `CryptoKey`（结构化克隆），下次取出仍可用于 `sign/decrypt` 但不可 export。

**约束**：
- Olm Account 私钥、Megolm outbound session key 必须以 `non-extractable` CryptoKey 形式存，**不得**以 base64 字符串存。
- v1 RSA 私钥若须在 Web 端用（兼容），同样 `non-extractable`。
- Olm pickle 整体（含 ratchet state）是 libolm 内部字节流，Web 端用 `vodozemac-js` 时由库内部管理 CryptoKey；上层只持有 handle。

**残留风险**：XSS 仍可在页面存活期间**调用** CryptoKey 完成签名/解密（不需 export）。这是 Web 平台根本限制；缓解措施是 CSP 严格策略 + 短会话 + 关键操作用户确认。

---

## 5. 桌面端（macOS / Windows / Linux）

Flutter 桌面端复用 `flutter_secure_storage`：
- macOS → Keychain（与 iOS 同 API，`kSecClassGenericPassword`）
- Windows → DPAPI（`CryptProtectData`，绑定用户 SID）
- Linux → libsecret（GNOME Keyring / KDE KWallet，fallback 到 `~/.local/share` 加密文件）

**存储 key 命名**：与 §3.2 移动端**完全一致**（同一份 `storage_secure.dart` 跨平台编译）。销毁策略、生命周期均沿用 §3.3。

**Linux 无 secret 服务时的降级**：`flutter_secure_storage` Linux 后端在无 keyring 时退化为明文文件 + ACL。本 ADR 要求：Linux 端启动时探测，若 keyring 不可用则**拒绝写入 Critical 数据并提示用户安装 keyring**，而不是静默退化到明文。Rebuildable 数据（旗标、元数据）可放普通存储。

---

## 6. 服务端零密码学约束（冻结）

线 A 已落地，本节重申并冻结为 ADR 级不变量，任何「服务端帮客户端解一下」「服务端缓存一下解密结果」的提案均直接违反本节。

### 6.1 永不接收 / 永不存储私钥

| 禁止项 | 实现位置 |
|---|---|
| 服务端 DB 任何表不得有私钥列 | `compliance_key.private_key_encrypted` 已 DROP；`user_device.public_key` 仅公钥 |
| 服务端 API 不得有「上传私钥」字段 | 身份键上报接口只接收 `ed25519_key` / `curve25519_key`（公钥侧） |
| 服务端不得在内存中持私钥 | Erlang 代码零 `elib_cipher:*decrypt` on E2EE payload |

### 6.2 DB 中加密相关列都是公钥侧

| 表.列 | 类型 | 语义 |
|---|---|---|
| `user_device.public_key` | varchar(2048) | v1 RSA **公钥** PEM |
| `user_device.identity_signature` | text | Ed25519 **签名**（非密钥） |
| `olm_identity.ed25519_key` | text | Ed25519 **公钥** |
| `olm_identity.curve25519_key` | text | Curve25519 **公钥** |
| `olm_identity.signature` | text | libolm **签名** |
| `olm_one_time_key.key_blob` | text | X3HH **公钥** OTK |
| `msg_c2c.payload` | text | E2EE **密文**裸 base64（服务端不解） |
| `e2ee_key_backups.encrypted_payload` | bytea | 备份**密文包**（服务端不解） |

**没有任何**列存储私钥、pickle、session key、明文。

### 6.3 守护：CI grep

CI 关键路径执行以下 grep，**零命中**才能合并：

```bash
# 服务端代码不得对 E2EE payload 调用任何解密
grep -rn "elib_cipher.*decrypt.*e2ee" imboy/src --include="*.erl"
# 服务端代码不得 import 客户端密码学库（libolm 等）
grep -rn "olm\|libolm\|megolm.*decrypt" imboy/src --include="*.erl" | grep -v "atom\|comment"
```

`e2ee_backup_logic.erl:128` 的明文拦截（`<<"-----BEGIN", ...>>` 拒收）是**机器可查的下限**，挡住客户端 bug 误传明文私钥，**不是解密**——只做字节前缀匹配。

---

## 7. 备份存储（4S 模式）

### 7.1 角色

- **服务端**（`e2ee_key_backups` 表 + `e2ee_backup_logic.erl`）：只存密文包 + KDF 参数 + payload_hash；版本单调（`backup_version`），明文拦截（PEM 头拒收），不可解。
- **客户端**（`e2ee_crypto_service.dart` + `e2ee_server_backup_service.dart`）：用用户口令 PBKDF2 派生密钥，AES-256-GCM 加密私钥 bundle 后上传。

### 7.2 密文格式

```
salt(16) || iv(12) || ciphertext || tag(16)    → 整体 base64
```

参数（来自 `e2ee_crypto_service.dart:31-43`）：
- KDF：PBKDF2-HMAC-SHA256，**310,000 迭代**
- Salt：16 字节随机
- 派生密钥：32 字节（AES-256）
- IV：12 字节随机（AES-GCM 推荐）
- Auth tag：16 字节（128 bits）
- 服务端 `e2ee_backup_logic.erl:23` 的 `MIN_KDF_ITERATIONS = 100000` 是防降级下限；客户端保证 ≥ 310k。

### 7.3 KDF 可迁移（不锚定 OWASP 版本）

备份密文包与服务端 DB 行均含 `algo` 字段（当前默认 `pbkdf2-sha256/aes-256-gcm`）+ `kdf_iterations`。未来升级 Argon2id 时：新备份用新 algo + 新参数；旧备份读取时按其 `algo` 字段选择 KDF，**不破坏存量**；用户重新输入口令后客户端可选 re-encrypt 升级。

**为什么不在 ADR 写死「OWASP 2023 推荐 PBKDF2 ≥ 600k」之类的数字**：ADR 生命周期 5+ 年，推荐周期 1-2 年，写死会让 ADR 反复 supersedes。`kdf_iterations` 作为运行时参数即可，由客户端按当前共识升级。

### 7.4 服务端版本单调与并发

`e2ee_key_backups` 表 `UNIQUE(uid, backup_version)`，`put_backup` 要求 `backup_version = 当前最新 + 1`，并发上传撞版本返回 409。这是 T6 / T9 的子防御：防止旧备份覆盖新备份。

---

## 8. 密钥销毁策略

### 8.1 客户端清哪些

退出登录 / 换设备 / 远程 revoke 时，按 §3.2 表**全量清**：

| 类别 | key 模式 |
|---|---|
| v1 RSA | `e2ee_private_key` / `e2ee_private_key_history_*` / `e2ee_public_key` / `e2ee_device_id` / `e2ee_key_id` / `e2ee_key_created_at` |
| v2 Olm | `olm_account_pickle` / `olm_pickle_key` / `olm_session_*` |
| Megolm | `megolm_inbound_*` / `group_e2ee_mode_*` |
| 分片 | `e2ee_shard_*` / `e2ee_shard_id_list` / `e2ee_shard_metadata_list` |
| DB 加密 key | per-uid DB key（清后本地 SQLite 不可读） |

**实现**：`deleteAllE2EEKeys` 当前只清 v1 类（`storage_secure.dart:233`），本 ADR 要求补全 `olm_*` / `megolm_*` / `group_e2ee_*` 的全量枚举清理（部分已实现于 `olm_session_service.dart:372`，需整合到统一 wipe 入口）。

### 8.2 服务端清哪些

| 场景 | `user_device` | `olm_identity` | `olm_one_time_key` / `fallback` | `e2ee_key_backups` |
|---|---|---|---|---|
| 单设备退出 | `status=-1, trust_state='revoked'` | DELETE | DELETE | **保留**（其他设备恢复用） |
| 用户最后一个设备退出 | 同上 | 同上 | 同上 | **保留**（用户可能重新登录恢复） |
| 用户主动「销户」 | 全部 DELETE | 全部 DELETE | 全部 DELETE | 全部 DELETE |

**为什么单设备退出不清备份**：备份是跨设备恢复的命脉；用户在手机上退出，iPad 上仍可能用同一备份恢复。销户时才全清。

### 8.3 销毁完整性守护

销毁是安全关键操作，必须可验证。`deleteAllE2EEKeys` 后必须读回所有 §3.2 的 key 模式确认全部返回 null；任一非 null 即告警并阻止退出完成。

---

## 9. 多设备同步

**私钥不跨设备同步**。每台设备生成独立身份键（独立 ed25519 / curve25519），服务端按 `(user_id, device_id)` 存多份公钥。设备间恢复密文靠：

1. **4S 备份恢复**（§7）：新设备输入口令，从服务端拉密文包，本地解出身份私钥 + session keys；
2. **Transfer 协议**（ADR 06 接口位）：旧设备主动把密钥 transfer 给新设备（待实现）；
3. **社交恢复分片**：分片代理协助恢复（已落地，见 `storage_secure.dart:257` 的 shard 存储）。

**为什么不直接同步**：同步意味着「私钥在传输中存在过」，攻击面扩大；独立身份 + 显式恢复是 Signal/WhatsApp 的成熟模型，PFS/PCS 的前提就是每设备独立 ratchet。

---

## 10. 守护测试要求

| 测试 | 防御的威胁 | 通过条件 |
|---|---|---|
| **私钥零落库** | T3 | DB schema grep 无私钥列；客户端 API 网络抓包无私钥字段 |
| **服务端零密码学 grep** | T1, T3 | `grep "elib_cipher.*decrypt.*e2ee" imboy/src` 零命中（CI 关键路径） |
| **明文拦截** | T3（纵深） | 上传 PEM 头 payload 返回 `plaintext_payload_rejected`（`e2ee_backup_logic.erl:128`） |
| **私钥不出本地** | T3 | 客户端 e2e 测试：登录 + 收发消息全流程，mitmproxy 抓包无私钥/pickle 字节 |
| **备份 KDF 迁移** | T6 | 旧 algo（pbkdf2）备份能被新客户端按 `algo` 字段解；re-encrypt 升级到 argon2id 后旧备份仍可解 |
| **销毁完整性** | T5 | 退出登录后枚举 §3.2 全部 key 模式，读回全 null；DB 加密 key 清后本地 SQLite 不可读 |
| **Web non-extractable** | T5（Web） | `StorageProvider.getCryptoKey` 返回的 CryptoKey `extractable === false`；`exportKey` 抛错 |
| **命名规范一致性** | T5（运维） | grep 客户端代码全部 secure storage key 命中 §3.2 表；无遗漏命名 |
| **备份版本单调** | T9 | 并发 `put_backup` 撞版本返回 409；旧 version 覆盖新 version 失败 |

---

## 11. 与其他 ADR 的关系

| ADR | 本 ADR 的依赖点 | 对方对本 ADR 的约束 |
|---|---|---|
| **02 protocol** | §6 服务端零密码学是 02 §6 的存储侧投影；备份密文格式由 02 协议层使用 | 02 注册新协议套件时若需新存储 key，须回到本 ADR §3.2 追加 |
| **03 device-identity** | §3.2 的 `e2ee_*` / `olm_*` 存储 key 与 03 的 `user_device` / `olm_identity` 一一对应 | 03 冻结的表结构决定客户端存什么私钥；本 ADR 冻结怎么存 |
| **06 device-trust** | 销毁时 `trust_state='revoked'` 与本地 wipe 联动（§8） | 06 的 revoke 状态机触发本 ADR 的客户端 wipe |
| **08 threat-model** | §1 的每个决策可追溯到 T3 / T5 / T6；§10 守护测试映射 08 §4 矩阵 | 08 演进新增存储相关威胁时，本 ADR 须追加守护测试 |

**冲突处理**：本 ADR 冻结服务端零密码学契约、§3.2 存储 key 命名、§4.2 `StorageProvider` 接口签名、§7 备份 KDF 版本字段语义四项为**不可单方面变更**。任何改动须新建 `NN-supersedes-07.md`，并同步更新 02 / 03 / 06 / 08 的反向引用。
