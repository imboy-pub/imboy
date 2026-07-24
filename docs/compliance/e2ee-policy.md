# imboy E2EE（端到端加密）策略与合规密钥托管披露

> 版本：v2.x | 最后更新：2026-07-18（零信任改造线 A：compliance 私钥下线服务端落盘）
> 关联文档：`docs/planning/project-audit-roadmap-2026-07.md` [FEAT-03] / [SEC-04]、`imboyapp/.claude/PRPs/plans/completed/feat-03-e2ee-dead-toggle.plan.md`
> 适用：imboy 后端 + imboyapp 客户端

---

## 1. E2EE 加密决策链路

E2EE 是否启用**由后端 capability（`e2ee_mode`）全权控制**，客户端无独立用户级开关。

### 1.1 后端：policy capability 派生

| 层 | 位置 | 行为 |
|---|---|---|
| capability 源 | `src/lib/imboy_policy.erl:121-157` | `effective_capabilities/0` 按 profile（community/enterprise）+ 配置派生，产出 `e2ee_mode` 字段 |
| 判定函数 | `src/lib/imboy_policy.erl:155-157` | `e2ee_enabled/0` = `e2ee_mode =/= disabled` |
| handler 守卫 | `src/api/e2ee_handler.erl:47-57`、`src/api/e2ee_backup_handler.erl:37-47`、`src/api/olm_handler.erl:50-60`、`src/api/e2ee_trust_handler.erl:34-43` | 各公开 E2EE handler 入口检查 capability；`e2ee_mode=disabled` 时返回 `ERR_FEATURE_DISABLED` |

### 1.2 `e2ee_mode` 四态

> 合法值集合以**后端代码** `imboy_policy_normalize.erl:185` `normalize_e2ee_mode/2` 为权威：`disabled | optional | compliance | required`。

| 值 | 语义 | 后端 `message_encryption_required()` | 客户端 `EncryptionModeService` |
|---|---|---|---|
| `disabled` | E2EE 关闭，所有 E2EE API 拒绝（守卫返回 `ERR_FEATURE_DISABLED`） | false | plaintext（API 不可达，无实际加密） |
| `optional` | E2EE 能力开放（API 可用），但**不强制加密**——由本地开关决定 | false | plaintext（本地开关 `isEnabled()` 永远 false，见 §2） |
| `required` | 强制端到端加密（仅接收方公钥） | true | strictE2ee |
| `compliance` | 强制加密 + **合规公钥双 wrap**（见 §3） | true | complianceE2ee |

> ⚠️ **注意区分两个枚举**：本表的 `e2ee_mode` 是**后端 capability 字段**；客户端 `EncryptionMode` 枚举（`plaintext`/`complianceE2ee`/`strictE2ee`）是 policy 的镜像结果，二者非一一对应。`plaintext`/`secure_e2ee`/`compliance_e2ee` 是**会话级** `encryption_mode` 字段（`chat_setting_page.dart:327` 经 `fromApiString` 解析），与全局 `e2ee_mode` 是不同数据源。

> 📌 **`optional` 的已知设计债务**（非 bug，已有文档记录于 `docs/guides/e2ee/e2ee-key-rotation-policy.md:82,329`）：FEAT-03 把 `E2EESettings.isEnabled()` 硬编码为 false 后，`optional` 模式实际等价 plaintext。若未来恢复用户级开关（密钥漂移方案 a 解阻），`optional` 语义会重新生效。

### 1.3 客户端镜像：`EncryptionModeService`

客户端 `imboyapp/lib/service/encryption_mode.dart` 镜像后端 policy，出站加密决策在 `e2ee_service.dart:124-147` `shouldEncryptOutgoingPayload`：

```dart
// 1. policy 优先：策略要求加密则强制加密（仅 C2C/C2G）
final policyMode = EncryptionModeService.current;
if (policyMode.requiresEncryption) {
  if (chatType != 'C2C' && chatType != 'C2G') return false;
  return true;
}
// 2. policy=plaintext 时，本地开关兜底
if (!E2EESettings.isEnabled()) return false;  // 永远 false，见 §2
```

**决策顺序固定：policy 优先 → 本地兜底。** policy=required 时即使本地 `isEnabled()=false` 仍强制加密。

---

## 2. 本地 E2EE 开关已废弃说明

### 2.1 历史

`imboyapp/lib/service/e2ee_settings.dart` 曾提供用户级 E2EE on/off 开关（`setEnabled`/`isEnabled` 读写 storage）。开发期发现：本地 build/重装导致 Keychain 的 RSA 私钥与服务端 `user_device` 公钥/对端缓存公钥**漂移**，加密消息对端无法解密、只显示 `🔒 [加密消息无法解密]`。

### 2.2 现状（短期止血方案 b，已落地）

- `E2EESettings.isEnabled()` **硬编码 `return false`**——本地开关永久关闭，加密统一由后端 policy 控制。
- `setEnabled()` / `resetToDefaults()` / `_keyEnabled` 常量**已删除**（死代码清理，零调用方）。
- 设置页**本就无 E2EE enable/disable toggle UI**，"隐藏开关"目标自动达成。
- 保留 `shouldNotifyOnFailed()` / `setNotifyOnFailed()`：对应"E2EE 失败通知"真实功能，待后续 wiring 到 UI。

### 2.3 根因未修（BLOCKED）

密钥漂移根因修复（方案 a：重装/换设备后自动拉取对端最新公钥 + 重新协商）**仍 BLOCKED**，需真机调试 + 产品评审是否恢复用户级开关。解阻后方可重新引入 E2EE toggle UI。跟踪见 `project-audit-roadmap-2026-07.md` [FEAT-03] 与 BLK-05（E2EE 换设备恢复协议方向）。

---

## 3. 合规密钥托管披露（SEC-04）

> ⚠️ **分层状态**：
> - ① **技术事实已确认**——下方客户端 wrap 行为已在 `e2ee_service.dart:244-266` 核实属实（2026-07-07）。
> - ② **法律/对外措辞待法务与产品复核**——本节陈述技术事实，**不构成法律措辞定稿**。隐私政策、白标合规文档的最终对外措辞须交人工（法务/产品）审定。

### 3.1 技术事实

当后端 `e2ee_mode=compliance` 时，客户端在 `imboyapp/lib/service/e2ee_service.dart:244-266` 的加密流程中，**除接收方各设备公钥外，额外用 compliance 公钥 wrap 同一份 AES key**：

```dart
// e2ee_service.dart:244-266（节选）
if (policyMode == EncryptionMode.complianceE2ee) {
  final complianceKey = await ComplianceKeyService.instance.getComplianceKey();
  if (complianceKey != null) {
    final wrappedCompliance = await _wrapAESKey(
      aesKey: aesKey, publicKeyPem: complianceKey.publicKey);
    keys.add({
      'did': 'compliance-audit',
      'kid': complianceKey.keyId,
      'wrap_alg': 'RSA-OAEP-256',
      'ek': base64.encode(wrappedCompliance),
    });
  }
}
```

### 3.2 含义

- 持有 **compliance 私钥**的管理员/审计方，可解密 `e2ee_mode=compliance` 模式下的全部密文。
- 这是**依法留存 / 合规审计访问通道**，与 imboy"零接触明文用户私钥"的宣称**不矛盾**（compliance 私钥 ≠ 用户私钥；用户私钥从未离开设备）。
- 但该机制**破坏纯端到端语义**——第三方（持 compliance 私钥者）能读取密文。须在隐私政策与白标合规文档中明示。

### 3.3 后端配套（零信任改造线 A，2026-07）

> **改造记录**：原方案 compliance 私钥由服务端落盘持有（`compliance_key.private_key_encrypted` 列），
> 这使运营方/管理员具备解密所有 compliance 模式密文的能力。2026-07 零信任改造（线 A）
> 彻底下线该字段：服务端**永不接收、永不落盘** compliance 私钥。

- **合规私钥仅由审计方在本地持有**：审计方在 HSM / 离线介质（USB / 纸质等）生成 RSA 密钥对，
  私钥永不离开本地，仅将公钥经 admin 上传服务端（`POST /api/adm/admin/compliance_key/create`，
  入参仅 `public_key`）。
- **服务端零接触私钥**：`compliance_key_repo:create/3` 只存公钥；
  migration `00000046` 已 `DROP COLUMN private_key_encrypted`；
  死代码 `find_by_key_id/1`（原唯一读取私钥的入口）已删除。
- **合规公钥下发**：客户端经 `GET /api/v1/e2ee/compliance_key` 取活跃合规公钥（仅 `key_id` + `public_key`），
  用于在 compliance 模式下额外 wrap 一份 AES key（见 §3.1）。
- **审计解密路径**：审计员在自己设备上导入本地保管的合规私钥 → 读取消息 `e2ee.keys[]` 中
  `did:compliance-audit` 条目的 `ek` → RSA-OAEP-256 解出 AES key → 解密消息。**整条链路服务端零参与**。
- `e2ee_mode` 只有显式配置为 `compliance` 时才启用合规双 wrap；`required`（strict）模式**不**进行合规 wrap，是纯端到端。

### 3.4 部署方须做

1. 在隐私政策中披露"依法留存的合规密钥托管"机制（`compliance_e2ee` 模式下）。
2. **合规私钥由审计方在本地（HSM / 离线介质）生成与妥善保管**；明确保管责任人、离线介质存放位置与访问审计流程。**服务端不再保存任何私钥，私钥一旦丢失，所有用此公钥加密的历史合规密文将永久无法解密**。
3. 若部署场景不需要合规托管，将 `e2ee_mode` 配置为 `required`（纯端到端）或 `optional`/`disabled` 即可关闭合规双 wrap。
4. 对外措辞由方法务/产品最终审定——本文件仅提供技术事实陈述。

---

## 参考

- 审计原始记录：`docs/planning/project-audit-roadmap-2026-07.md` [FEAT-03]、[SEC-04]、BLK-05、BLK-06
- 实施计划：`imboyapp/.claude/PRPs/plans/completed/feat-03-e2ee-dead-toggle.plan.md`
- 相关代码：
  - 后端 policy：`imboy/src/lib/imboy_policy.erl:121-157`
  - 后端守卫：`imboy/src/api/e2ee_handler.erl:47-57`、`imboy/src/api/e2ee_backup_handler.erl:37-47`、`imboy/src/api/olm_handler.erl:50-60`、`imboy/src/api/e2ee_trust_handler.erl:34-43`
  - 客户端镜像：`imboyapp/lib/service/encryption_mode.dart`
  - 客户端决策：`imboyapp/lib/service/e2ee_service.dart:124-147`
  - 客户端合规 wrap：`imboyapp/lib/service/e2ee_service.dart:244-266`
  - 客户端设置：`imboyapp/lib/service/e2ee_settings.dart`
