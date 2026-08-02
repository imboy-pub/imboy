# E2EE 密钥全生命周期

> 对应 gap-matrix **D4**（生命周期各环节散落各 ADR）。审计场景下的单一入口：
> 每一类密钥材料，从生成到销毁，各环节由哪段代码执行、在哪里能验证。
> 最后核对：2026-08-02

**取代** [`e2ee-key-rotation-policy.md`](./e2ee-key-rotation-policy.md) —— 该文档写于 RSA 时代，
其协议规格、存储清单与恢复方案均已失真，详见本文末尾"被取代文档"一节。

## 0. 一句话结论

**服务端从不接触任何明文私钥。** 服务端只存放公开材料（公钥、一次性公钥、fallback 公钥）
与**用户口令派生密钥加密后**的备份密文。所有私钥生成、封装与解封装都在客户端完成。

## 1. 密钥清单 × 生命周期矩阵

### 1.1 客户端持有（私密材料）

| 密钥 | 生成 | 存储 | 轮换 | 可备份 | 销毁 |
|------|------|------|------|--------|------|
| Olm identity（Curve25519 + Ed25519） | 首次启用 E2EE 时 vodozemac 生成 | `olm_*` account pickle @ secure storage | 不轮换（= 设备身份） | ✅ 随备份 | 登出清理 / 设备吊销 / 注销 |
| Olm one-time keys（OTK） | 批量生成后上传公钥 | account pickle 内 | 低水位 **5** → 补到 **50** | ✅ 随备份 | 同上；被 claim 即服务端删 |
| Olm fallback key | 同上 | account pickle 内 | 每 **7 天** | ✅ 随备份 | 同上 |
| Olm session（双棘轮） | X3DH 首次握手 | `olm_*` session pickle | 棘轮逐条前进 | ❌ **有意不备份** | 同上 |
| Megolm outbound session | 首次群发时 | 内存 + pickle | **三触发器**（见 §2.2） | ❌ | 轮换即弃 |
| Megolm inbound session | 收到 room key 时 | `megolm_inbound_*` | 随 outbound 轮换新增 | ✅ **随备份** | 登出清理 / 注销 |
| 备份口令派生密钥 | PBKDF2 **310,000** 轮 | 不落盘（用完即弃） | 随用户改口令 | — | 内存生命周期内 |
| SQLCipher DB key | 首次建库 | `db_cipher_key_<uid>` | 不轮换 | ❌ | 登出清理（删后旧库永久打不开） |
| RSA legacy | 历史遗留 | `e2ee_*` | 不再生成 | ✅ | 登出清理 |
| `mls_*` | — | 预留前缀，尚无写入方 | — | — | 登出清理已覆盖 |

前缀权威清单：`imboyapp/lib/service/e2ee/e2ee_secret_inventory.dart` `secretKeyPrefixes`。
**新增任何 E2EE 存储键必须落在这些前缀之下**，否则登出清理与其测试都覆盖不到。

> **为什么 Olm session 有意不备份**：Olm session 状态含**发送侧棘轮位置**。跨设备恢复
> 会造成密钥重用 + 棘轮分叉，前向保密与后向保密同时失效。Megolm **inbound** session
> 是只读的（无发送棘轮），备份安全。这也是单聊历史换设备后不可恢复的根因，
> 详见 [`history-recoverability.md`](./history-recoverability.md)。Signal 与 Matrix 同此取舍。

### 1.2 服务端持有（公开材料 + 密文）

| 表 | 内容 | 敏感度 |
|----|------|--------|
| `olm_identity` | 设备身份**公**钥 | 公开 |
| `olm_one_time_key` | 一次性**公**钥池 | 公开，claim 即删 |
| `olm_fallback_key` | fallback **公**钥 + Ed25519 签名 | 公开 |
| `olm_otk_claim_request` | claim 幂等/租约记录（迁移 49） | 元数据 |
| `e2ee_key_backups` | 口令派生密钥加密后的**密文**（迁移 36） | 密文，服务端无法解 |
| `e2ee_local_backups` | 客户端本地备份上传的**密文** | 同上 |
| `user_device` | device_id / public_key / key_id | 公开 |
| `trust_audit` | 设备信任决策流水（actor_uid / target_uid） | **PII 元数据** |

## 2. 各环节实现位置

### 2.1 生成与分发

- **身份与 OTK 发布**：客户端生成 → `POST /api/v1/olm/*` 上传公钥。
- **OTK 领取**：`claim_one_time_key` 取走即删（一次性语义），幂等由 `olm_otk_claim_request` 保证。
- **room key 分发**：Megolm room key 经 **Olm 一对一信道**逐设备加密下发（ADR 13
  `v2/13-room-key-over-olm.md`），服务端只见密文。分发列表上限 4096 条，防超大列表 DoS。
- **fallback key 验签**：`report_fallback_key/5` 做 canonical 单射守卫 → 查已注册 ed25519 → 验签。
  ⚠️ **签名当前仍非必填**（见 §4 残留）。

### 2.2 轮换

| 对象 | 触发 | 位置 |
|------|------|------|
| Megolm outbound | ①成员/设备集合变化 ②消息数 ≥ **100** ③存活 ≥ **7 天** | `group_session_service.dart` `_maxMessagesPerSession` / `_maxSessionAgeMs` |
| OTK 池 | 余量 < **5** → 补到 **50** | `olm_session_service.dart:30,33` + `e2ee/otk_refill_policy.dart` |
| Fallback key | 每 **7 天** | `e2ee/fallback_rotation_policy.dart:11` |
| Olm session | 每条消息棘轮前进（协议内建） | vodozemac |

任何成员/设备集合变化都**整体轮换 + 全量重分发**，不做"仅新增设备定向补发"。

### 2.3 备份与恢复

口令 → PBKDF2(310,000) → 加密 → 上传密文。服务端全程只见密文，**口令丢失不可恢复，这是设计**。
备份纳入 Megolm inbound session，故换设备后**群聊历史可恢复、单聊历史不可**（§1.1 注）。
备份内容的前缀白名单及其反向断言测试见 `megolm_backup_section.dart`。

### 2.4 销毁（三条路径，互不重叠）

| 场景 | 入口 | 行为 |
|------|------|------|
| 登出 / 换号 | `E2eeSecretInventory.purgeAll` | 内存缓存 → 临时产物 → 按前缀删持久化键 → **复核零残留**；任一失败抛 `E2eeSecretPurgeException`，调用方**必须阻止建立新会话**（fail-closed） |
| 设备吊销 | `user_device_ds:delete/2` | 先删设备行（= token 吊销），再 `olm_identity_repo:delete_by_device/2` 级联清 Olm 三表。**顺序不可颠倒**：反过来若删行失败，会变成"密钥没了但 token 还有效"的最坏组合 |
| 账号注销 | `user_ds:delete_all_related_data/2` | 事务内显式删 olm 三表 + `e2ee_key_backups` + `e2ee_local_backups` |

> ⚠️ **注销路径不触发设备吊销级联**：该函数删 `user_device` 走的是本函数内的直接 SQL，
> **不经** `user_device_ds:delete/2`。所以 olm 三表必须在注销路径里**再显式删一次**。
> 少了它，账号注销后别人仍能 claim 到它的 OTK、与一个不存在的账号建立 Olm 会话。
> 这一点由 `test/ds/user_ds_tests.erl` 的删除维度断言钉住（每张 olm 表只能用本人 uid 作条件——
> 写错维度会清掉别人的密钥，光断言"调用发生过"抓不到）。

## 3. 可验证性（审计员怎么自己查）

| 主张 | 验证方式 |
|------|----------|
| 服务端不碰明文私钥 | olm 三表 schema **无任何 private/secret 列**（`grep -i "private\|secret" priv/migrations/*olm*.up.sql` 零命中）；`grep -rn "private_key" src/` 的命中全部是支付宝网关 / APNS 推送 / SSO 配置，**与 E2EE 无关** |
| 登出确实清干净 | `E2eeSecretInventory.purgeAll` 删完 `readAll()` 复核残留；对应测试断言残留为空 |
| 吊销确实级联 | `test/ds/device_revocation_tests.erl` 路径 4（4 例），含"先删设备行再清 Olm"的顺序断言 |
| 注销确实级联 | `test/ds/user_ds_tests.erl` `delete_all_related_data_cascades_olm_tables_test_` |
| 单聊历史确实不在备份里 | `megolm_backup_section.dart` 前缀白名单 + 其反向断言测试 |

## 4. 残留缺口（如实列出）

| 缺口 | 状态 |
|------|------|
| `trust_audit` 在账号注销时**未清** | 🔒 审计留存 vs 被遗忘权是政策判断，需显式拍板，不在代码里默默删 |
| fallback key 签名**非必填** | 今天若强制必填，无客户端发签名 → 所有设备发布不了 fallback key → 每次耗尽变 `no_prekey_available`（可用性事故）。用 `olm_fallback_unsigned_total` 指标判断第二阶段启动时机（E2EE-062） |
| 换设备**前**的备份提醒 UI 未加 | P3-1 残留 |
| 交叉签名 / SAS 验证 | 算法在、UI 零接线（gap-matrix B1，P3-4/P3-5） |

## 5. 被取代文档

[`e2ee-key-rotation-policy.md`](./e2ee-key-rotation-policy.md)（333 行，写于 RSA 时代）以下内容**已不成立**，
保留原文仅为追溯历史决策，不可作为当前实现的依据：

- §1.1 把 **RSA-2048 / `RSA-OAEP-256+AES-256-GCM`** 描述为主协议 —— v2 已 Olm-only cutover，
  RSA 降为**仅解密**（ADR 24 `v2/24-unified-olm-only-and-rsa-decrypt-only.md`）。
- §1.2 存储清单只列 RSA 公私钥 + device_id/key_id，**漏掉了几乎全部真实密钥材料**：
  Olm account pickle、Olm session pickle、Megolm inbound session、OTK、fallback key、SQLCipher DB key。
- §5.2 **社交恢复（Shamir Secret Sharing）** —— 已从代码删除（`grep -rn "splitSecret\|Shamir" lib/` 零命中）。
- §4 设备迁移流程引用的 `e2ee_transfer_handler` —— 已删除（迁移 38 `drop_e2ee_social_transfer`）。
