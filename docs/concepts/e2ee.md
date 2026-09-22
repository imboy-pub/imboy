# 端到端加密（End-to-End Encryption, E2EE）

> Purpose：概念入口——定义 E2EE 的协议矩阵、信任模型与三端职责分工。
> 协议细节权威：[E2EE 协议规范](../reference/e2ee-protocol-specification.md)；驱动体系与证据见 `docs/guides/e2ee/`。

## Concept：服务器不可读的消息加密

| 场景 | 协议 | 服务端可见 | 客户端实现 |
|---|---|---|---|
| 单聊（C2C） | Olm（X3DH 建立会话 + Double Ratchet 前向保密） | 只存公钥侧（身份密钥/一次性预共享密钥/回退密钥） | vodozemac（Rust）经 `flutter_vodozemac` |
| 群聊（C2G） | Megolm（发送侧加密 + 房间密钥分发） | 只存会话溯源（attestation：标识+成员世代），永不存房间密钥明文 | 同上 |
| 附件 | 分块加密（E2EE 附件策略） | 密文对象 | `attachment_*` 系列策略模块 |
| 历史兼容 | RSA-OAEP（旧版） | — | `rsa_legacy_protocol`，仅兼容存量 |

## Current

- **设备即密钥主体**：`user_device` 持有 Olm 身份密钥与能力声明（capabilities: olm/megolm/rsa-oaep/mls）；预共享密钥 OTK claim 即删（原子消费），回退密钥每设备覆盖式一条。
- **信任模型**：`trust_audit` 记录带 Ed25519 签名快照的信任决策（QR 扫码/安全码比对/吊销/设备销毁）；客户端有安全码（Safety Number）与身份核验流程。
- **群历史边界**：成员世代（`group_member_generation`）+ 会话溯源（`e2ee_group_session_attestation`，迁移 112）+ 收件人快照（迁移 111）三者共同保证「重入群拿不到旧世代房间密钥」。
- **房间密钥中转**：WS action `e2ee_room_key` 服务器不透明中转（c2c/c2g 两路）。
- **密钥备份（4S）**：`e2ee_key_backups` 零信任密文备份（服务端只见密文+KDF 参数；PBKDF2 迭代 ≥100000，客户端建议 310000 防降级）。
- **社交恢复**：门限分片体系（`e2ee_key_shares` 1-5 门限 / `e2ee_social_shards`）。
- **合规密钥**：三层加密 `compliance_key`（私钥列已移除，迁移 46）。
- **AI 明文豁免**：与透明 AI 助手对话存在明文闸（`ai_plaintext_gate`）——豁免是显式策略，不是漏洞。

**设计行为（非缺陷，写作时不得当 bug 描述）：**

- 单聊密钥有意不做云端备份 → 换设备后历史单聊不可解密（客户端有解密失败占位与引导文案）。
- FTS 搜索排除 E2EE 密文。

## Contract

1. 服务端**永不**持有：Olm/Megolm 私钥、房间密钥明文、备份明文密钥、社交恢复分片明文。
2. 三端能力协商以 `user_device.capabilities` 为准；新增能力（如 MLS）先扩契约。
3. 群 E2EE 模式开关：`group.e2ee_mode`（迁移 37），开关变更广播 WS action `group_e2ee_mode`。

## 加密档位（运营视角）

部署的加密强度由两个底层策略字段共同表达：`e2ee_mode`（给**客户端**的加密规矩，
随 `/api/v1/app/policy` 下发）与 `storage_mode`（**服务端**存储/审计姿态，联动搜索/
导出/审计）。两者是同一个运营决定的两面；管理后台「能力配置 → 加密档位」单选
一次套用两个字段。标准组合共四档：

| 档位 | e2ee_mode | storage_mode | 客户端行为 | 服务端 |
|---|---|---|---|---|
| 关闭（明文交付） | `disabled` | `disabled` | 明文收发，E2EE 入口隐藏 | **硬闸**：密钥端点关闭、明文校验放行、群级加密被忽略 |
| 可选（明文归档） | `optional` | `archived` | 明文收发 | 明文归档存储，可搜索/导出 |
| 合规（可审计） | `compliance` | `compliance_e2ee` | 双密钥加密（接收方 + 合规公钥） | 审计方可凭合规密钥解密存量消息 |
| 强制（纯端到端） | `required` | `secure_e2ee` | 强制端到端加密 | 服务器不可读；消息搜索/导出自动关闭 |

- **判定优先级（客户端）**：`storage_mode=disabled`（硬闸）> `e2ee_mode` > `storage_mode`；
  非标准/未知取值一律回落明文（`imboyapp lib/service/encryption_mode.dart`）。
- **判定优先级（服务端生效值）**：环境变量覆盖 > DB 持久化（后台保存值）> profile 预设；
  硬闸（storage_mode=disabled）压倒一切加密档判据（`imboy_policy:e2ee_disabled/0`）。
  ⚠️ 部署期注入 `IMBOY_E2EE_MODE` 会覆盖后台保存值——策略页"改不动"先查这一层。
- **非标准组合**（如企业预设 `disabled + archived`：E2EE 关但无硬闸）仍合法且判定有
  兜底，但不再对应任何档位；后台会提示，选择任一档位即标准化。
- **映射真源三处同步**：imboyadmin `policy.ts ENCRYPTION_TIERS`、
  `imboy_policy_catalog.erl` 注释、本文档（admin 页面测试逐项锁定）。

## Constraints

- E2EE 群的世代边界由数据库 append-only 表守卫，任何「补发旧房间密钥」的需求都违反安全模型。
- 客户端加密库版本升级必须过跨平台互操作测试（imboyapp `integration_test/e2ee_*`）。

## References

- 协议规范（权威）：`docs/reference/e2ee-protocol-specification.md`
- 驱动体系：`docs/guides/e2ee/standard/`（现行）；`docs/guides/e2ee/v2/`（编号 ADR 00-31，注意其中 03-09/13/15 已被后续编号取代，以各篇头部 supersedes 链为准）
- 政策：`docs/compliance/e2ee-policy.md`；审计：`docs/security/audits/e2ee-2026-09-07/`
- 迁移：36-38、42-46、49、101、111、112
