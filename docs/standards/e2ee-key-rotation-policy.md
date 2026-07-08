# E2EE 密钥旋转策略
# E2EE Key Rotation Policy

> 文档版本 / Doc Version: 1.0.0
> 最后更新 / Last Updated: 2026-05-27
> 基于代码版本 / Based on Code: `e2ee_key_service.dart`, `e2ee_service.dart`, `e2ee_settings.dart`, `e2ee_transfer_service.dart`, `e2ee_local_backup_service.dart`, `shamir_secret_sharing.dart`, `imboy/src/api/e2ee_handler.erl`

---

## 1. 当前密钥生命周期 / Current Key Lifecycle

### 1.1 密钥算法规格 / Key Algorithm Specification

| 参数 | 值 | 来源 |
|------|-----|------|
| 非对称算法 | RSA-2048 | `E2EEKeyService.rsaKeySize = 2048` |
| 公钥指数 | 65537 (F4) | `E2EEKeyService.publicExponent = 65537` |
| 对称算法 | AES-256-GCM | `e2ee_suite: 'RSA-OAEP-256+AES-256-GCM'` |
| AES 密钥长度 | 256 bit (32 字节) | `_secureRandomBytes(32)` |
| GCM Nonce 长度 | 12 字节 | `_secureRandomBytes(12)` |
| 密钥 ID 格式 | `kid_{8位十六进制}` | `_generateKeyId()` |
| 设备 ID 格式 | `{8hex}-{8hex}-{8hex}` | `_generateDeviceId()` |

### 1.2 密钥存储位置 / Key Storage Location

```
客户端（Client）
├── 私钥 (RSA private key PEM)  → flutter_secure_storage（Keychain on macOS/iOS）
├── 公钥 (RSA public key PEM)   → flutter_secure_storage
├── device_id                   → flutter_secure_storage
├── key_id                      → flutter_secure_storage
└── key_created_at (ISO 8601)   → flutter_secure_storage

服务端（Server）
└── user_device 表              → PostgreSQL（device_id, public_key, key_id, uid）
```

### 1.3 当前密钥过期策略 / Current Expiry Policy

**代码层面目前无强制过期时间配置。** 从代码中提取的关键常量：

| 常量 | 值 | 说明 |
|------|-----|------|
| 公钥缓存 TTL | **30 分钟** | `E2EEService._cacheTtlMs = 30 * 60 * 1000` |
| 备份密钥派生迭代次数 | **310,000 次** | `E2EECryptoService.pbkdf2Iterations = 310000` |
| 备份文件格式版本 | 1 | `E2EECryptoService.formatVersion = 1` |
| 传输会话过期 | 由服务端控制 | `expires_at` 字段由 `e2ee_transfer_handler` 返回 |

> **注意 / Note**: `E2EESettings.isEnabled()` 当前强制返回 `false`，E2EE 由后端 policy（`EncryptionModeService`）统一控制。密钥本身在 Keychain 中持久存储，不自动过期。

---

## 2. 旋转触发条件 / Rotation Trigger Conditions

### 2.1 主动触发（用户/系统发起）

| 触发条件 | 描述 | 处理入口 |
|----------|------|---------|
| **设备更换** | 新设备首次安装，本地无密钥 | `E2EEKeyService.hasKey()` 返回 false → 自动生成 |
| **重新安装应用** | Keychain 数据未迁移，密钥丢失 | 同上；旧设备公钥仍在服务端，需执行旋转清理 |
| **手动触发** | 用户在"安全设置"中主动更新密钥 | 调用 `E2EEKeyService.generateKeyPair()` |
| **设备迁移完成** | 通过 QR 码传输到新设备后确认 | `E2EETransferService.confirmTransfer()` |
| **备份恢复** | 从 `.enc` 文件导入密钥 | `E2EELocalBackupService.importBackup()` |
| **社交恢复完成** | 收集足够 Shamir 分片后重建 | `ShamirSecretSharing.combineShares()` |

### 2.2 被动触发（安全事件）

| 安全事件 | 处理策略 |
|----------|---------|
| **密钥泄露怀疑** | 立即调用 `E2EEKeyService.deleteKey()` 删除本地密钥，重新生成，通过 `report_device_key` 上报新公钥 |
| **服务端通知密钥已失效** | 监听 `pull_notifications` 返回的密钥变更通知，清除对应用户缓存：`E2EEService.clearUserKeyCache(uid)` |
| **对端公钥缓存过期（30分钟）** | `_isCacheExpired()` 自动触发重新获取，无需手动干预 |
| **ACK 超时 / 解密持续失败** | `_e2ee_failed: true` + `_e2ee_reason` 标记；可触发 `retryDecryptFailedMessage()` 重试 |

### 2.3 后端策略触发

客户端通过 `EncryptionModeService.refresh()` 从 `/v1/app/policy` 拉取 `e2ee_mode` capability，镜像为本地 `EncryptionMode`：

| `e2ee_mode`（后端 capability） | 客户端 `EncryptionMode` | 行为 |
|------|------|------|
| `disabled` | plaintext | E2EE API 拒绝（守卫返回 `ERR_FEATURE_DISABLED`），无加密 |
| `optional` | plaintext | E2EE API 可用，但不强制加密；本地开关决定（当前 `isEnabled()` 强制 false → 实际明文） |
| `required` | strictE2ee | 强制端到端加密，密钥不存在则拒绝发送 |
| `compliance` | complianceE2ee | 双密钥加密：设备密钥 + 合规审计密钥（`compliance-audit` did） |

> 合法值集合以后端 `imboy_policy_normalize.erl:185` `normalize_e2ee_mode/2` 为权威。详见 `docs/compliance/e2ee-policy.md` §1.2。

---

## 3. 旋转流程 / Rotation Flow

### 3.1 标准旋转流程

```
用户触发旋转
     │
     ▼
1. 生成新 RSA-2048 密钥对
   E2EEKeyService.generateKeyPair()
   ├── Web 平台: Web Crypto API
   └── 移动/桌面: pointycastle (Isolate 中运行，不阻塞 UI)
     │
     ▼
2. 生成新 key_id（kid_{8hex}）和 created_at 时间戳
     │
     ▼
3. 原子写入 Keychain（flutter_secure_storage）
   Future.wait([
     savePrivateKey(pem),
     savePublicKey(pem),
     setDeviceId(did),
     setKeyId(kid),
     setKeyCreatedAt(ts),
   ])
     │
     ▼
4. 上报新公钥到服务端
   POST /v1/e2ee/report_device_key
   { device_id, device_type, device_name, public_key, key_id }
     │
     ▼
5. 服务端更新 user_device 表
   e2ee_logic:report_device_key(...)
   → 服务端通知该用户所有好友清除公钥缓存
     │
     ▼
6. 清除本地对端公钥缓存
   E2EEService.clearAllKeyCache()
     │
     ▼
7. 旧密钥处理
   ├── 旧私钥：Keychain 覆盖写入（步骤 3 已完成）
   ├── 旧公钥：服务端覆盖更新（步骤 5 已完成）
   └── 旋转前发送的历史消息：无法用新密钥解密（RSA 非对称，历史消息密文绑定旧公钥）
         → 建议：在旋转前导出本地备份（步骤可选）
     │
     ▼
8. 旋转完成，新密钥生效
```

### 3.2 旧消息处理策略 / Historical Message Handling

由于每条消息使用独立的 AES-256 密钥（由发送方用接收方公钥包装），密钥旋转后：

- **旋转后收到的消息**：使用新公钥加密，可正常解密。
- **旋转前收到并已解密的消息**：明文已存入 SQLite，不受影响。
- **旋转前收到但未解密的消息**：AES 密钥用旧公钥包装，新私钥无法解开，显示 `🔒 [加密消息无法解密]`（`_e2ee_failed: true`, `_e2ee_reason: 'no_device_key'` 或 `'decrypt_error'`）。

> **建议**：在触发旋转前，通过 `retryDecryptFailedMessage()` 批量处理所有待解密消息，或提示用户旋转后历史加密消息将不可读。

---

## 4. 设备迁移流程 / Device Migration Flow

基于 `E2EETransferService` 的 QR 码传输方案：

```
旧设备（Source Device）              新设备（Target Device）
        │                                      │
        │ 1. 用户在旧设备发起迁移              │
        │    createTransfer(toUid, encBundle)  │
        │    → POST /v1/e2ee/transfer/create   │
        │    → 获得 session_id + expires_at    │
        │                                      │
        │ 2. 生成 QR 码                        │
        │    generateQRCodeData(session_id)    │
        │    {"type":"e2ee_transfer",          │
        │     "session_id":"..."}              │
        │                                      │
        │                     3. 新设备扫描 QR  │
        │                        parseQRCodeData│
        │                                      │
        │                     4. 接受传输       │
        │                        acceptTransfer(│
        │                          session_id, │
        │                          new_device_id│
        │                        )             │
        │                        → 解密密钥包   │
        │                        → 保存到 Keychain│
        │                                      │
        │                     5. 确认完成       │
        │                        confirmTransfer│
        │                        (session_id)  │
        │                                      │
        │ 6. 新设备上报新公钥                  │
        │    report_device_key(new_did, ...)    │
```

**安全说明**:
- `encryptedKeyBundle` 使用目标用户当前在服务端注册的公钥加密（RSA-OAEP）。
- 传输会话有 `expires_at` 时效限制（服务端控制）。
- 传输完成后旧设备密钥仍有效，用户需手动在旧设备上删除（`E2EEKeyService.deleteKey()`）或在服务端撤销旧设备注册。

---

## 5. 密钥备份与恢复 / Key Backup and Recovery

### 5.1 本地文件备份（E2EELocalBackupService）

**文件格式** (`imboy_e2ee_backup_<timestamp>.enc`):

```
[文件头 32 bytes]
  Magic: "IMBOYBKP" (8 bytes)
  Version: 1          (2 bytes)
  AlgorithmId: 0x0001 (2 bytes)
  PBKDF2 Iterations: 310,000 (4 bytes)
  Salt Length: 16    (2 bytes)
  IV Length: 12      (2 bytes)
  Tag Length: 16     (2 bytes)
  Reserved: 6 bytes
[Salt 16 bytes]
[IV 12 bytes]
[GCM Auth Tag 16 bytes]
[AES-256-GCM 密文]
[用户备注长度 4 bytes]? + [备注内容]?
```

**密钥派生**: PBKDF2-HMAC-SHA256，310,000 次迭代，16 字节随机 Salt。

**密码要求**: 最低 8 位（`_validatePassword`）。建议使用强密码（12 位以上，含大小写+数字）。

**导出流程**:
```dart
final path = await E2EELocalBackupService.exportBackup(
  password: userPassword,
  privateKey: privateKeyPem,
  publicKey: publicKeyPem,
  deviceId: deviceId,
  keyId: keyId,
  userNotes: '主手机 2026-05-27',
);
await E2EELocalBackupService.shareBackup(path);
```

**恢复流程**:
```dart
final data = await E2EELocalBackupService.importBackup(
  filePath: selectedFilePath,
  password: userPassword,
);
// data 包含: private_key, public_key, device_id, key_id, created_at
// 恢复后需重新上报公钥
```

**完整性验证**: SHA-256 校验和嵌入密文中，解密后验证（`calculateChecksum`）。

### 5.2 社交恢复（Shamir Secret Sharing）

使用 `ShamirSecretSharing` 实现 (k, n) 门限方案：

| 参数 | 值 |
|------|-----|
| 素数域 | RFC 3526 MODP Group 14（2048 位安全素数） |
| 最大秘密长度 | 256 字节（适配 RSA-2048 私钥） |
| 推荐配置 | (3, 5)：5 个受信任联系人，3 个即可恢复 |
| 随机源 | Fortuna CSPRNG + `Random.secure()` 32 字节种子 |

**分片流程**:
```dart
// 将私钥 PEM 转为字节后分片
final secretBytes = utf8.encode(privateKeyPem) as Uint8List;
final shares = ShamirSecretSharing.splitSecret(secretBytes, n: 5, k: 3);
// shares[i] = {'index': i+1, 'x': int, 'y': BigInt}
// 将每个分片加密后发送给对应受信联系人保管
```

**恢复流程**:
```dart
// 收集至少 3 个分片
final collected = [share1, share3, share5]; // 任意 3 个
final secretBytes = ShamirSecretSharing.combineShares(collected);
final privateKeyPem = utf8.decode(secretBytes);
```

**安全验证**（内置）:
- 分片格式验证（x > 0, 0 < y < prime）
- 分片索引唯一性验证（防重复分片攻击）
- 恢复长度合理性验证（≤ 256 字节）

**与后端社交恢复 API 联动**:
```
POST /v1/e2ee/recovery/start
{ "device_id": "...", "method": "social_recovery" }
→ e2ee_recovery_logic:start_auto_recovery(...)
```

---

## 6. 安全建议 / Security Recommendations

### 6.1 推荐旋转周期 / Recommended Rotation Period

| 场景 | 建议周期 |
|------|---------|
| 常规使用 | **每 90 天**主动旋转一次 |
| 高安全要求用户 | **每 30 天**旋转 |
| 检测到异常登录后 | **立即旋转** |
| 设备修复/系统重装后 | **立即旋转** |

> 当前代码无自动周期旋转逻辑，建议在 `app_upgrade_orchestrator.dart` 或专用定时任务中实现：检查 `key_created_at` 距今是否超过阈值，超过则提示用户旋转。

### 6.2 强制旋转场景 / Mandatory Rotation Scenarios

以下场景必须立即执行密钥旋转：

1. **私钥文件/Keychain 数据疑似泄露**
2. **设备被盗或丢失**（需同时在服务端撤销该 device_id 对应的公钥注册）
3. **应用被卸载后重新安装**（Keychain 可能残留旧密钥，新安装时应强制重新生成）
4. **检测到 `_e2ee_reason: 'no_device_key'` 的解密失败持续超过阈值**（对端可能已旋转，本端缓存未清除）
5. **后端 `compliance_e2ee` 模式激活**（需确认本端密钥已上报且合规公钥已获取）

### 6.3 运营安全注意事项 / Operational Security Notes

| 项目 | 建议 |
|------|------|
| 备份密码强度 | 推荐 12 位以上，含大小写字母+数字（当前代码仅验证 8 位最低要求） |
| 备份文件存储 | 不应存储在与密钥相同的设备上；推荐邮件或独立云盘 |
| Shamir 分片分发 | 每个分片应由不同联系人保管，不应集中存储 |
| 传输会话 QR 码 | 生成后应在 5 分钟内使用（具体时效由服务端 `expires_at` 决定） |
| 旋转后通知联系人 | 服务端 `report_device_key` 会自动下发通知，客户端收到后清除缓存：`E2EEService.clearUserKeyCache(uid)` |
| AES 会话密钥重用 | 每条消息使用独立的 AES 密钥（`_secureRandomBytes(32)`）和 Nonce（`_secureRandomBytes(12)`），无需关注 |

### 6.4 待完善事项 / Known Gaps

| 缺口 | 建议 |
|------|------|
| 无自动旋转提醒 | 在 `app_upgrade_orchestrator` 中增加 `key_created_at` 过期检查（建议 90 天） |
| 备份密码强度校验宽松 | `_validatePassword` 仅检查 8 位长度，建议增加复杂度规则 |
| 旧设备密钥未自动撤销 | 迁移完成后需手动撤销旧设备注册，建议在 `confirmTransfer` 后自动触发 |
| `E2EESettings.isEnabled()` 强制 false | 当前 E2EE 完全由后端 policy 控制，客户端开关形同虚设；建议在 policy = `optional` 时恢复客户端开关语义 |
