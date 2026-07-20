# ADR 17 — Recovery Vault v2：恢复历史，不克隆活跃设备

> **状态**：Proposed
> **拟替代**：ADR 07 的 4S/备份格式与 PBKDF2 默认方案
> **依赖**：ADR 14、ADR 16
> **核心原则**：恢复后必须是新设备；历史可读与未来可发送是两套不同密钥权限。

---

## 1. 问题

当前本地备份主要覆盖 legacy RSA 材料，不能完整恢复 Olm/Megolm/信任状态；恢复旧 `device_id` 又可能与运行时物理 DID 分叉。若直接备份和复制活跃 Olm/MLS ratchet，则两台设备可能并发使用同一状态，导致身份冲突、密钥/nonce 重用、撤销语义不清。

此外，旧二进制 notes 长度布局和解析存在不一致风险，KDF 参数与文件尺寸必须在执行 PBKDF/KDF 和分配内存前设硬上限。

---

## 2. 决策

建立 `Recovery Vault v2`，并明确两个不能混淆的产品档位：

| 档位 | 默认 | 内容 | 安全含义 |
|---|---|---|---|
| `identity_only` | Strict 默认 | Account signing material、trust roots/high watermarks；不含消息历史 key | 最大化历史 FS；新设备不能仅靠 vault 解密旧消息 |
| `history_recoverable` | 用户明确 opt-in | 上述内容 + 只读历史解密档案 | 可跨设备读历史，但 vault 泄漏会暴露被备份的历史范围 |

UI、备份清单和安全文档必须展示档位；不得把 `history_recoverable` 宣传成与 `identity_only` 相同的前向保密边界。

Vault 中材料分为三类：

| 类别 | 是否备份 | 恢复后的权限 |
|---|---|---|
| Account Master / device-signing 恢复种子 | 是，加密封装 | 可授权“新”设备，不直接充当 device identity |
| 历史解密档案 | 是，按会话/epoch 分段 | 只读解密历史，永不用于新发送或活跃接收 ratchet |
| 活跃 Olm outbound/inbound ratchet、活跃 MLS leaf/state、物理 DID | **否** | 新设备重新建会话/加入群 |

对于确需解密历史 Olm 消息的 session snapshot，导入后必须放入 `ArchivedCryptoStore` 命名空间，固定原始设备身份并标 `decrypt_only=true`；任何 encrypt、session update 或 prekey claim API 对该命名空间永久不可用。

---

## 3. 恢复包格式

使用 deterministic CBOR，并把完整 header 作为 AEAD AAD：

```text
VaultEnvelopeV2 {
  magic: "IMBOYRV2",
  format_version: 2,
  vault_id: random 128-bit,
  created_at_ms,
  account_id,
  kdf: {name, salt, params},
  aead: {name, nonce},
  ciphertext_length,
  ciphertext,
  header_hash
}
```

解密内容：

```text
{
  account_signing_material,
  trust_roots_and_high_watermarks,
  optional_archived_rsa_keys,
  optional_archived_olm_sessions,
  optional_archived_megolm_sessions,
  optional_archived_mls_epoch_secrets,
  optional_attachment_history_keys,
  manifest
}
```

`manifest` 为每个条目的类型、原设备、会话、时间范围、hash 和权限声明；恢复时逐条验证，不允许“未知类型当作普通 secret”导入。

notes/用户备注作为加密内容中的普通有界字段，不再使用“从文件末尾反推长度”的尾部结构。

---

## 4. KDF 与密钥模式

### 4.1 默认模式

默认生成 256-bit 随机 Recovery Key，以可打印编码展示，并要求用户离线保存。高熵 key 通过 HKDF-SHA-256 派生 vault AEAD key，不执行昂贵口令 KDF。

### 4.2 口令兼容模式

允许用户选择口令时使用 Argon2id。参数按设备基准选择，但必须同时满足：

- 内存成本 `64 MiB .. 256 MiB`；默认从 64 MiB 起；
- 迭代 `2 .. 6`；并行度 `1 .. 4`；
- salt 至少 16 随机字节；
- 目标最低设备解封装时间 0.5–2 秒；
- reader 在运行 KDF 前验证范围，超界立即拒绝；
- 参数只能在新备份提升，旧备份读取后提示重封装。

具体默认值必须以最低受支持 Android/iOS 真机基准确定并记录，不能把桌面参数硬编码到移动端。

### 4.3 AEAD

使用经依赖库稳定支持且经过审计的 AEAD（AES-256-GCM 或 XChaCha20-Poly1305）。每个 vault 使用随机 nonce；header canonical bytes 作为 AAD；认证失败不得尝试其他算法或 KDF。

---

## 5. 硬资源上限

| 项目 | 默认硬上限 |
|---|---|
| Vault 文件 | 64 MiB（企业策略可降低，不可远程无界提高） |
| Header | 16 KiB |
| 加密条目 | 100,000 |
| 单条历史 secret | 1 MiB |
| notes | 16 KiB UTF-8 |
| CBOR 嵌套 | 16 层 |
| 解压 | v2 初始不支持压缩，避免 zip bomb |

reader 先读固定小 header、校验 magic/version/length/KDF 参数，再分配 ciphertext。禁止 `readAsBytes()` 无界读取不可信文件。

---

## 6. 恢复流程

1. 在新安装中生成新的物理 `device_id`、Olm account、MLS credential 和 Device Manifest。
2. 用户导入 Recovery Vault；客户端只在内存中解封装并验证全包/逐条 hash。
3. Account signing material 进入安全存储；若档位为 `history_recoverable`，历史材料进入独立、加密、只读 ArchivedCryptoStore。
4. 使用恢复的账号根 cross-sign 第 1 步的新 manifest，并通过 ADR 16 transparency 发布。
5. 新设备重新建立 Olm 会话；对现存 MLS 群以新 leaf 加入并产生 Commit。
6. 历史消息按 `original_device_id + session/epoch` 查询档案；未来消息只查询活跃 CryptoStore。
7. 恢复完成后擦除内存缓冲、临时文件和剪贴板内容；记录不含 secret 的审计事件。

禁止把 vault 中的旧 DID 写回全局设备 ID，禁止自动上传历史私钥，禁止恢复后复用旧 OTK/fallback/KeyPackage。

---

## 7. 生命周期与撤销

- Vault 是快照，不是永久实时同步状态；生成新快照前先轮换 vault key/nonce。
- 用户可用账号根发布 recovery generation 撤销；客户端保存最高 generation，拒绝旧包回滚。
- 删除本地 vault 必须清除文件与引用；普通文件系统无法保证物理擦除时要诚实说明。
- 服务端托管 vault 时只存密文、大小、版本、generation 和 hash；下载需 device-bound auth，服务端不可修改 KDF 参数而不触发 AAD 验证。

---

## 8. 验收测试

| ID | 用例 | 通过条件 |
|---|---|---|
| RV2-01 | 两台新设备分别导入同一 vault | 两者生成不同 DID/活跃 identity；无 ratchet clone |
| RV2-02 | `history_recoverable` 恢复 RSA/Olm/Megolm/MLS 历史 fixtures | 指定历史消息可解；所有 archived encrypt API 拒绝 |
| RV2-02A | `identity_only` 导入 | 账号根/信任恢复，vault 中无消息/附件历史 key，旧消息不被错误标记为可恢复 |
| RV2-03 | 修改 header/KDF 参数/length/ciphertext/manifest | AEAD 或结构验证失败，无部分导入 |
| RV2-04 | notes 长度 0、上限、截断、尾部垃圾 | round-trip 正确或严格拒绝，无越界 |
| RV2-05 | 64 MiB+1、极端 Argon2 参数、10^6 条目 | KDF/大分配前拒绝 |
| RV2-06 | 10,000 个 fuzz 样本 | 无 crash、hang、OOM、部分 secret 落盘 |
| RV2-07 | 导入后撤销/回滚旧 generation | 旧 vault 不覆盖较新 trust/manifest high watermark |
| RV2-08 | kill 在解封装/导入/授权/清理各阶段 | 要么完整完成，要么无可用的半导入状态 |
| RV2-09 | logout/account switch | 活跃及 archived store key 均不可读取，旧 SQLCipher DB 无 key 不能打开 |
| RV2-10 | 服务端/日志/崩溃报告扫描 | 无 recovery key、口令、私钥、明文 manifest 内容 |

---

## 9. 迁移

- v1 备份 reader 保留一次性导入；导入成功后立即生成 v2，不再生成 v1。
- v1 中只有 RSA 四元组时诚实提示“仅恢复 legacy 历史密钥”，不得显示“完整 E2EE 已恢复”。
- 检测到旧备份携带 DID 时仅作为 archived namespace 标识，不写入当前 device identity。
- PBKDF2 只为历史读取保留；所有新口令备份写 Argon2id。

---

## 10. 取舍与生效条件

恢复后需要重新建立会话/重新加入 MLS 群，短期 UX 不如设备克隆，但它避免复制活跃 ratchet，是安全上可接受的唯一默认方案。`history_recoverable` 是以加密 vault 换取历史可用性的显式选择，不改变新消息必须使用 fresh session 的规则。

- [ ] 产品接受 `identity_only` 默认以及 `history_recoverable` 的风险提示
- [ ] iOS/Android 的 Argon2id/AEAD 依赖通过维护性、许可证和供应链评审
- [ ] RV2-01..10 均有自动化/真机入口
- [ ] 数据保留与账号根重置流程通过安全评审

接受后在 ADR 07 顶部标注：v1 storage 抽象保留，备份格式/KDF/恢复语义由 ADR 17 替代。

---

## 11. 参考

- [OWASP Password Storage Cheat Sheet — Argon2id](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)
- [RFC 8949 — deterministic CBOR](https://www.rfc-editor.org/rfc/rfc8949.html)
