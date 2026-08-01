# ADR 15 — Protected Frame v3 与禁止未认证降级

> **状态**：**Accepted**（2026-08-02 leeyi solo 一人决策全签；豁免解除，PFv3 消息 header 绑定与 Olm→RSA 降级禁止全文生效）
> **拟替代**：ADR 05 的新写入 metadata 格式；ADR 13 的 Olm→RSA 接收 fallback 与单包退出路径
> **保留**：ADR 05 的 legacy 解析；ADR 13 的历史 RSA/Megolm 解密能力
> **依赖**：ADR 14 安全基线、ADR 16 设备身份

---

## 1. 问题

当前业务消息的 `id/from/to/type/msg_type/gid/sender_did` 等路由字段位于 E2EE 密文外。现有 AES-GCM AAD 测试证明工具函数能验证 AAD，但实际 Olm/Megolm/RSA 路径没有统一绑定这些字段。ADR 13 的接收逻辑还允许 `olm` 缺失或失败后使用静态 RSA `ek`；恶意服务端可以删除 `olm` 子对象，强迫新客户端走更弱且不能认证发送设备的路径。

因此“密文未被修改”不等价于“客户端显示的发送者、会话、消息类型未被修改”。

---

## 2. 决策

所有新 E2EE 写入使用 `ProtectedFrame v3`：

1. 把路由所需的可见字段规范化为 `protected_header`；
2. 使用 RFC 8949 deterministic CBOR 编码；
3. 计算 `header_hash = SHA-256(canonical_cbor(protected_header))`；
4. 把同一份 `protected_header` 和业务 payload 放入加密的 `inner_frame`；
5. 接收端解密后逐字段比较外层与内层 header，任何不一致均拒绝；
6. 支持原生 AAD 的协议还必须把 `header_hash` 放入 AAD/authenticated_data；
7. Strict 的新消息只接受协商出的强套件，不能因字段缺失或解密失败尝试另一个套件。

Olm API 不暴露应用级 AAD 时，`inner_frame` 的加密认证仍将 header 与密文绑定；MLS 必须同时使用 authenticated_data，形成协议层与应用层双重绑定。

---

## 3. 规范数据模型

### 3.1 `protected_header`

字段名和类型一旦进入生产即冻结：

| 键 | CBOR 类型 | 约束 | 语义 |
|---|---|---|---|
| `v` | uint | 固定 `3` | Protected Frame 版本 |
| `message_id` | text | 1..128 字节，UTF-8 | 全局消息标识 |
| `scope` | text enum | `c2c` / `group` / `control` | 密码学域 |
| `conversation_id` | text | 1..128 字节 | C2C 为排序后的 uid 对摘要；群为 gid |
| `sender_uid` | text | 1..32 字节、十进制 TSID | 发送账号 |
| `sender_did` | text | 1..128 字节 | 发送物理设备 |
| `destination` | text | 1..128 字节 | 对端 uid、gid 或控制消息目标 |
| `message_type` | text enum | 注册表内值 | 业务消息类型 |
| `action` | text | 0..64 字节 | `message`、`room_key`、`mls_commit` 等 |
| `created_at_ms` | uint | 合理时间窗 | 客户端创建时间，不作为唯一授权依据 |
| `protocol` | text enum | `olm` / `megolm` / `mls` | 实际密码学协议 |
| `protocol_version` | uint | 当前实现支持范围 | 协议版本 |
| `session_ref` | text | 1..256 字节 | Olm session、Megolm session 或 MLS group ref |
| `epoch_or_counter` | uint | 单调且有界 | MLS epoch 或应用去重计数 |
| `content_encoding` | text enum | 初始仅 `cbor` | 内层 payload 编码 |

TSID 统一以十进制文本进入 CBOR，避免 Dart/JS/JSON 64-bit 精度差异。不得把本地 map 的迭代顺序当作规范编码。

### 3.2 `inner_frame`

```text
{
  "v": 3,
  "protected_header": <与外层语义完全相同的 map>,
  "payload": <业务 payload map>,
  "padding": <可选 bytes>
}
```

`payload` 只允许注册字段；解密后仍需执行大小、类型与业务授权校验。不要因已通过 AEAD 就信任业务数据。

### 3.3 外层信封

```text
{
  "meta_version": 3,
  "protected_header": <canonical CBOR bytes 的 base64url>,
  "header_hash": <base64url SHA-256>,
  "ciphertext": <协议密文>,
  "protocol_metadata": <有界、协议特定字段>
}
```

传输层现有 `from/to/type/gid/msg_type` 可以为路由继续保留，但接收端只在其与解码后的 `protected_header` 一致时用于展示和持久化。v3 wire 不允许用 JSON map 替代 `protected_header` 的 canonical CBOR bytes，避免不同平台产生多种等价编码。

---

## 4. 各协议绑定规则

| 协议 | 新写入 | 绑定方式 | Strict 失败行为 |
|---|---|---|---|
| Olm | C2C 默认 | 加密完整 `inner_frame`；解密后比对 header；会话定位绑定已验证 `(uid,did,identity_version)` | 拒绝，不尝试 Megolm/RSA |
| Megolm | MLS 上线前的群 Preview | 加密 `inner_frame`；本地将 `(gid,session_id)` 固定绑定发送 uid/did/identity key | 拒绝，不尝试 RSA room key |
| MLS | 群 GA | `header_hash` 放入 authenticated_data，同时应用 payload 含 inner header | 拒绝并按 MLS 状态机处理 |
| RSA legacy | 仅历史解密 | 只解析 `meta_version < 3` 的历史消息 | 禁止新加密；不得作为 v3 fallback |

`protocol_metadata` 不能覆盖或补齐 `protected_header`；未知关键字段、重复 CBOR map key、非最短整数、非规范编码均拒绝。

---

## 5. 接收状态机

```text
parse outer bounds
  -> require supported meta_version
  -> canonicalize protected_header
  -> verify transmitted header_hash
  -> resolve exactly one protocol
  -> authenticate device/session binding
  -> decrypt exactly once
  -> parse inner_frame bounds
  -> constant-semantics compare inner/outer headers
  -> enforce replay/epoch/counter policy
  -> persist plaintext + crypto state atomically
  -> render
```

任何步骤失败都返回稳定、无秘密的错误分类；不得把密钥是否存在、签名哪一位错误等 oracle 细节上送服务端。

---

## 6. 降级与兼容规则

### 6.1 新写入

- `strict`：无共同强套件、策略未初始化、目标设备未验证、身份回滚或 key transparency 不一致时拒发。
- `compliance`：在 Strict 的全部要求上，额外要求有效且被组织根签名的审计接收方；缺失即拒发。
- `optional`：可以明文发送，但必须由用户/租户显式选择，UI 显著显示，wire 上标 `unencrypted=true`；不得伪装为 E2EE。
- RSA 只允许 `decryptLegacy()`；`encrypt()` 在所有 E2EE 模式永久抛错。

### 6.2 历史读取

- `meta_version` 缺失或 `<3` 时按 00–13 的 legacy parser 解密。
- legacy 消息必须明确标记“历史加密格式”，不得提升信任徽章。
- 历史解密失败不尝试猜测其他 suite；只按消息声明的唯一 suite 执行一次。
- 旧 RSA 私钥和历史 Megolm/Olm session 可在 ADR 17 的只读档案区保留；禁止用于新发送。

### 6.3 ADR 13 迁移

- 双包 `ek + olm` 只视为 legacy transitional format。
- 支持 v3 的发送端不再生成 RSA `ek`。
- 支持 v3 的接收端收到 `olm` 存在却无效、`sid` 不匹配或被删除的 v3 room-key 时直接拒绝，不回退 `ek`。
- 只有明确的 legacy 消息版本才可以使用已有 RSA `ek` 解密历史 room key。

---

## 7. Replay、乱序与资源边界

1. `message_id` 在会话域内唯一；重复密文幂等返回，不重复推进 ratchet。
2. `epoch_or_counter` 维护滑动窗口；窗口大小由协议配置并设置硬上限，超限执行 resync，不无限缓存。
3. `created_at_ms` 只用于异常检测：默认接受 ±24 小时；信任事件和设备操作使用更窄窗口并有 nonce。
4. 外层信封最大 10 MiB；header 最大 8 KiB；单字段最大值见 §3；超限在 base64/CBOR 全量分配前拒绝。
5. CBOR 最大嵌套深度 16、map entry 128、array entry 4096；禁止 indefinite-length 编码。
6. 解密失败、未知版本、过期 counter 不得触发无限网络重试。

---

## 8. 验收测试

| ID | 测试 | 通过条件 |
|---|---|---|
| PF3-01 | 分别篡改 `message_id/from/to/type/msg_type/gid/sender_did/protocol/session_ref/created_at` | 每一项都在展示/落库前拒绝 |
| PF3-02 | 删除或替换 `keys[].olm`，保留 RSA `ek` | v3 Strict 拒绝且 RSA 解包调用次数为 0 |
| PF3-03 | 把 A→B 密文复制到 A→C 或群 X→Y | header/context mismatch，拒绝 |
| PF3-04 | 同一密文重放 100 次 | 业务只提交一次，ratchet 不重复推进 |
| PF3-05 | 重复 CBOR key、非最短整数、乱序 map | parser 拒绝；跨平台结果一致 |
| PF3-06 | v3 未知 protocol/version | 稳定失败，不走 legacy fallback |
| PF3-07 | legacy v1/v2 固定 fixtures | 仍可 decrypt-only，绝不用于新发送 |
| PF3-08 | 10 MiB+1、深度 17、超长 base64 | 在密码学调用前有界拒绝，无 OOM |
| PF3-09 | Flutter↔独立实现 deterministic CBOR | header bytes 与 SHA-256 完全一致 |
| PF3-10 | kill 在“加密后/状态提交前/网络发送前/ACK 后” | 不发生 key reuse、重复业务提交或 ratchet rollback |

Critical 测试 PF3-01/02/03/10 不得 skip、mock 掉密码学库或只验证 helper。

---

## 9. 附件加密

- 每个附件生成独立 256-bit content key，不复用消息 key、其他附件 key 或上传凭证。
- 文件名、MIME、明文大小、明文 hash、对象 ID、cipher suite、chunk 大小/数量、base nonce 和 content key 全部放在加密的 `payload.attachment_descriptor`，并受 Protected Frame 认证。
- 大文件分块 AEAD；每块 nonce 从随机 base nonce 与 chunk index 按审计通过的唯一映射派生，AAD 至少绑定 `header_hash + attachment_id + chunk_index + chunk_count`。
- 接收端验证块顺序、数量、每块 tag、完整明文 hash 和声明大小后才交给预览器；失败时删除临时明文。
- Garage/S3 授权 URL 只负责访问控制，不承担内容机密性；拿到原始对象 URL 的攻击者最多获得附件密文。
- 缩略图、波形、OCR、EXIF 和推送摘要若含敏感内容，必须同样进入 E2EE payload 或在产品上明确不受保护。

附件验收：

| ID | 用例 | 通过条件 |
|---|---|---|
| ATT-01 | 把附件对象从消息 A 替换到消息 B | context/AAD 不匹配，拒绝打开 |
| ATT-02 | 交换、删除、重复、截断任一 chunk | 在产生可用文件前拒绝 |
| ATT-03 | 篡改 MIME/name/size/hash/chunk_count | inner descriptor 或完整性验证失败 |
| ATT-04 | 未授权方获得 Garage 原始对象 | 只能看到密文，不能恢复文件/缩略图 |
| ATT-05 | 下载/解密中途 kill 或磁盘满 | 临时明文不可被其他账号读取，恢复后安全清理 |

---

## 10. 实现落点

### Flutter (`imboyapp`)

- 新增 `ProtectedFrameV3`、canonical CBOR codec、严格 parser。
- `e2ee_protocol.dart` 的 encrypt/decrypt 输入输出增加受保护上下文，不允许业务层绕过。
- `chat_network_service.dart` 构造唯一 header；`message.dart` 只使用验证后的 inner header。
- `group_session_service.dart` 删除 v3 的 RSA fallback，并将 Megolm session 固定绑定发送身份。
- CryptoStore 按 ADR 14/20 原子提交 ratchet、dedupe 和消息状态。

### Erlang (`imboy`)

- 仍不解密、不解析业务 payload。
- 只做外层尺寸、必填字段类型、版本、速率限制和原样透传；不得重建/裁剪 `protected_header`。
- WS/HTTP 两条路径必须 byte/semantic preserving，并有契约测试。

---

## 11. 取舍

- 外层重复一份 header 增加几十到数百字节，但换来路由和展示前可验证，且不要求服务端解密。
- deterministic CBOR 增加跨语言实现成本，但消除 JSON 数字、键顺序和 Unicode 表示差异。
- 严格拒绝会暴露更多“无法安全发送”，这是安全承诺的真实成本；产品可提供显式 optional 模式，但不能隐藏降级。

---

## 12. 生效条件

- [ ] ADR 14 已接受
- [ ] Flutter、Web/独立实现对 canonical CBOR fixture 达成一致
- [ ] legacy decrypt-only 迁移方案通过产品与数据保留评审
- [ ] PF3-01..10 全部能在 CI/真机门禁落地

接受后，在 ADR 05 和 ADR 13 顶部标注“新写入被 ADR 15 替代；legacy 读取条款保留”。

---

## 13. 参考

- [RFC 8949 — CBOR，含确定性编码要求](https://www.rfc-editor.org/rfc/rfc8949.html)
- [RFC 9420 §6 — MLS Message Framing / Content Authentication](https://www.rfc-editor.org/rfc/rfc9420.html#section-6)
