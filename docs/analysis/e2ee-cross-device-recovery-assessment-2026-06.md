# imboy E2EE 历史消息跨设备恢复能力评估报告

> 生成日期：2026-06-13
> 方法：客户端代码审计（imboyapp）+ 后端代码审计（imboy）+ 行业对标（WhatsApp / Signal / iMessage / Telegram / Matrix，20+ 一手来源）
> 置信度：高（关键事实经客户端与后端交叉验证，并校正了文档错误）

---

## 执行摘要

imboy 的 E2EE 工程完成度远超一般自研 IM：已实现服务端永久密文归档、三套密钥恢复路径（设备直传 / Shamir 社交恢复 / 密码加密本地备份）、多设备公钥分发、E2EE 健康检查与恢复引导 UI。单看组件清单，恢复选项甚至比 Signal 更丰富。

但针对核心目标——"换设备 / 重装 / 升级后历史消息平滑保留、无违和感"——结论分裂：

| 场景 | 能否恢复历史 | 平滑度 |
|------|------------|--------|
| APP 升级 | 能 | 完全平滑（密钥 + 本地库都保留） |
| 删除重装（同机） | 部分能 | 不平滑（iOS 可能残留密钥，Android 基本丢） |
| 换新手机 | 技术上能，体验上不平滑 | 强依赖用户"事前"操作 |

**一句话判断**：imboy 已具备"能恢复"的全部技术零件，但还做不到"平滑无违和"——缺少行业公认的那块拼图：服务器托管的、零知识的、自动化的加密备份（WhatsApp HSM Vault / Matrix 4S / Signal Secure Backups 那一套）。

---

## 一、核心矛盾：为什么天生困难

E2EE 的数学定义就是"服务器无法解密，密钥只在客户端"。换设备 = 新密钥 = 旧密文解不开。这是所有 E2EE 产品的根本约束。

学术上这是一个**三选二**不可能三角（综合 SVR3, OSDI 2024）：
1. 服务器零知识（不能解密）
2. 用户无需保管任何高熵秘密
3. 换设备后历史可恢复

imboy 当前位置：满足 ①，部分满足 ③，牺牲了 ②（用户必须事前主动备份/记密码，否则历史永久丢）。

---

## 二、imboy 当前实现画像

### 技术选型
- **非 Signal Protocol**，自研 **RSA-2048(RSA-OAEP-256) + AES-256-GCM 信封加密**：每条消息生成一次性 AES 密钥，对每个接收设备的 RSA 公钥单独包装一份（`imboyapp/lib/service/e2ee_service.dart:192-205`）。
- 代价：无前向保密（设备密钥对长期不变、无 Double Ratchet）、无 one-time prekey。安全强度弱于 Signal，但满足基本 E2EE 定义。

### 服务端做对的关键点
后端有**两套独立存储**：
- 离线队列：ACK 后立即删除（`imboy/src/ds/msg_operation_ds.erl:76-86`）
- 永久归档表 `msg_store`：独立写入，不随 ACK 删除，`conv_seq` 游标支持增量拉取，TTL = 1 年（`imboy/src/ds/msg_store_worker.erl:190-202`）

**重要校正**：`msg_archive_enabled` 在所有配置文件（sys.config / sys.local.config / sys.runtime.config）实际默认为 `true`，只有代码兜底值和 CLAUDE.md 文档写的是 false（文档与配置不一致，需更正）。即生产环境默认保留历史密文。

### 致命断点
即便服务器保留了全部历史密文，**新设备拉回也解不开**：
- 每条归档密文的 `e2ee.keys[]` 只包含发消息当时已注册设备的 deviceId 所对应的包装密钥。
- 新设备登录生成全新 deviceId + 新 RSA 密钥对（`imboyapp/lib/page/passport/passport_notifier.dart:1025-1067`），旧密文里没有给新 did 包装的 AES 密钥。
- 结果：拉回历史 → `e2ee_service.dart:296-309` 找不到自己的 ek → 抛 `No key found for device` → 落为 `_e2ee_failed`「[加密消息]」占位。

**唯一解法**：把旧设备私钥连同旧 deviceId/keyId 原样迁移到新设备（`imboyapp/lib/service/e2ee_transfer_service.dart:121-138` 会还原旧 did）。即历史恢复的前提是密钥迁移成功，而密钥迁移强依赖用户事前操作。

### 已实现的三套密钥恢复路径
1. **设备间直传**（`e2ee_transfer_service.dart` + `e2ee_transfer_sessions` 表）：旧机在场，扫码 RSA 加密传整个密钥包。
2. **Shamir 社交恢复**（`e2ee_social_service.dart` + `e2ee_social_shards` 表）：私钥分片给可信联系人，阈值恢复，零信任。
3. **密码加密本地备份**（`e2ee_local_backup_service.dart`）：PBKDF2-HMAC-SHA256（31 万次迭代）+ AES-256-GCM 导出 `.enc` 文件，用户自行保管，服务器不存文件本身。

> 三条路径全部需要用户事前主动操作。若用户从未导出备份、旧机损坏、也没配置社交恢复代理 → 私钥永久丢失 → 历史永久不可解。

---

## 三、与行业最佳实践的差距

| 能力 | imboy | WhatsApp | Signal | iMessage(ADP) | Matrix/Element |
|------|-------|----------|--------|---------------|----------------|
| 服务端密文归档 | 有 | 有 | 有(45天) | 有 | 有 |
| 零操作自动恢复（装好就有历史） | 无 | 有 | 部分 | 有 | 部分 |
| 服务器托管加密备份 | **无（缺）** | 有 HSM Vault | 有 Secure Backups | 有 | 有 4S/SSSS |
| 用户持有 Recovery Key | 仅本地备份密码 | 有 64位 | 有 64字符 | 有 28字符 | 有 |
| 防暴力破解（HSM/限速） | 无 | 有 HSM | 有 SVR3飞地 | 有 | 靠高熵Key |
| 设备直传迁移 | 有 | — | 有 | — | 有 |
| 社交恢复 | 有 Shamir | — | — | 有 恢复联系人 | — |
| 多设备同时在线 | 有 扇出 | 有 | 有 linked | 有 | 有 |

**imboy 缺的拼图**：一个"服务器托管的、用 Recovery Key 端到端加密的、自动化的备份-恢复闭环"。它有"密钥恢复"（迁移私钥），但没有"装好 App 输入一个 Recovery Key 就自动还原全部历史"的零操作路径——而这正是 WhatsApp/iMessage 让用户无违和感的关键。

EFF（2025）权威判断："当提供云存储时，最好的默认是把 E2EE 备份做成唯一选项。"

---

## 四、最终结论

- **APP 升级**：已经能，完全平滑。
- **换手机 / 删除重装**：目前做不到"平滑无违和"。技术上可恢复，但要求用户换机前完成密钥备份/迁移，否则历史永久丢失。对普通用户有违和感、甚至会丢数据。

**根因不是"缺能力"，而是"缺自动化闭环"**：imboy 把恢复责任压在"用户事前主动操作"，WhatsApp/iMessage 做成"装好 App → 输 Recovery Key/密码 → 历史自动回来"的被动式零操作体验。

---

## 五、补齐到"平滑"的最小可行架构（Matrix 4S 范本）

imboy 已有 80% 零件（密文归档、Garage S3 自有存储、PBKDF2、设备公钥体系、恢复引导 UI）。要补的是自动备份闭环，最现实范本是 Matrix 4S 模型（不需要 HSM）：

1. **客户端生成 256 位随机备份主密钥 K** —— 用 K（AES-256-GCM）加密历史消息，密文备份存自有 Garage S3。K 永不明文离开客户端。
2. **双因子密钥派生** —— `master_key = KDF(Argon2id(用户口令) ⊕ 服务端随机种子)`。单靠 Argon2 挡不住弱口令，必须叠加服务端因子。
3. **高熵 Recovery Key（28-64 字符）作为首选恢复凭据** —— 强制用户开启前记录确认，明示"丢失不可恢复"。Element X 教训：放弃 passphrase，只用高熵 Key。
4. **服务端只存被 Recovery Key 加密的 blob** —— 把"防暴力破解"责任转移到"Recovery Key 本身高熵"，无需 HSM。HSM/SGX 留给企业版。
5. **关键修复**：备份恢复还原旧 deviceId/keyId（transfer 流程已会做），这样拉回的归档密文才能解开。

ToB 私有化卖点：自有 Garage S3 + 零知识加密备份，企业数据不出私有集群，又给员工平滑换机体验，对标野火 IM 的差异化优势。

---

## 六、需立即行动的风险项（不依赖大改造）

1. **文档/配置不一致**：CLAUDE.md 写 `msg_archive_enabled` 默认 false，实际配置全是 true。更正文档，并纳入 `deploy/preflight.sh` 强校验。
2. **secure storage 未配置卸载行为**：`FlutterSecureStorage()` 用默认 options，未显式设置 iOS Keychain accessibility / Android 清除策略（`imboyapp/lib/service/storage_secure.dart:37`），重装场景跨平台行为不可控。
3. **`/v1/msg/history` 鉴权较弱**：`validate_history_params` 未校验 C2C 双方好友关系（`imboy/src/logic/messaging_logic.erl:172-183`），建议补充关系校验防 conv_key 枚举。
4. **1 年 TTL**：超过 1 年的消息会被 TimescaleDB 清理，需向用户明示。

---

## 关键来源

- WhatsApp E2EE 备份白皮书 / NCC Group 审计：engineering.fb.com/2021/09/10/security/whatsapp-e2ee-backups/
- Meta 2026 强化公告：engineering.fb.com/2026/05/01/security/meta-strengthening-end-to-end-encrypted-backups/
- Signal SVR / link-and-sync：signal.org/blog/secure-value-recovery/ ; signal.org/blog/a-synchronized-start-for-linked-devices/
- Signal Secure Backups：support.signal.org/hc/en-us/articles/9708267671322
- Apple iCloud / ADP：support.apple.com/en-us/102651 ; support.apple.com/guide/security/sec973254c5f/web
- Telegram（Durov）：telegra.ph/Why-Isnt-Telegram-End-to-End-Encrypted-by-Default-02-23
- Matrix MSC1946(4S) / MSC1756(cross-signing)：github.com/matrix-org/matrix-doc
- Messenger E2EE 白皮书：engineering.fb.com/wp-content/uploads/2023/12/MessengerEnd-to-EndEncryptionOverview_12-6-2023.pdf
- SVR3 (OSDI 2024)：eprint.iacr.org/2024/887
- Sender Keys (ASIACRYPT 2023)：eprint.iacr.org/2023/1385
- EFF 加密备份对比：eff.org/deeplinks/2025/05/back-it-back-it-let-us-begin-explain-encrypted-chat-backups
