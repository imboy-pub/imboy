# ADR 16 — Device-bound Auth、Cross-signing 与 Key Transparency

> **状态**：**Accepted**（2026-08-02 leeyi solo 一人决策全签；豁免解除，§3.1 device-bound session 完整体、§5 cross-signing、transparency log 全文生效）
> **拟替代**：ADR 03 的设备身份写入授权、ADR 04 的 capability 信任来源、ADR 06 的“仅预留 CrossSigningService”决定
> **保留**：现有 Safety Number 作为人工核验入口；现有 trust state 数据迁移后继续使用
> **依赖**：ADR 14、ADR 15；恢复根密钥见 ADR 17

---

## 1. 问题

当前 JWT/会话主要证明账号 `uid`，而 Olm identity/prekey 上传动作从请求体读取 `device_id`。因此同一账号下的恶意设备可能尝试覆盖或消耗另一设备的 E2EE 材料。仅由设备自身签名 capabilities 也不能解决首次密钥替换：被攻陷服务端可以同时替换身份键和签名。

要抵御 T2/T4/T8，设备身份必须同时具备：

1. 登录会话绑定具体物理设备；
2. 账号级信任根为合法设备授权；
3. append-only 透明度日志让服务端难以对不同用户展示不一致历史；
4. 所有状态变更有 freshness、nonce、幂等键和审计证据。

---

## 2. 信任模型

```text
Account Recovery Root (离线/加密恢复)
  └── Account Master Signing Key
        ├── Device Manifest: phone A / identity_version 7
        ├── Device Manifest: desktop B / identity_version 3
        └── Revocation: lost phone C

Device Manifest
  ├── Ed25519 signing key
  ├── Curve25519/Olm identity key
  ├── MLS credential/signature key
  └── signed capabilities

Transparency Log
  └── manifest/revocation events -> Merkle tree -> signed tree heads
```

- Account Master Signing Key 的公钥是账号级稳定信任锚；私钥不得上传服务端。
- 新账号在首个设备本地生成 Master Key；公钥首次注册后进入透明度日志。
- 新设备由已验证设备 cross-sign，或通过 ADR 17 的恢复根重新授权。
- 账号所有设备丢失且无恢复材料时，只能新建信任根；联系人必须收到强安全告警，旧 verified 状态不得继承。
- 单个设备私钥只能签本设备操作，不能直接授权另一个设备成为 verified；cross-sign 必须通过账号 Master Key 或被明确委派的 device-signing key。

---

## 3. Device-bound Session

### 3.1 会话声明

认证成功后的服务端会话必须至少绑定：

```text
session_id       128-bit+ 随机标识
uid              账号 TSID
device_id        物理设备 ID
device_generation 设备重新注册单调代数
issued_at
expires_at
auth_time
status           active/revoked
```

JWT 可以携带相同声明，但服务端必须检查可撤销会话记录；仅验证自包含 token 不足以支持设备丢失后的即时撤销。

### 3.2 授权规则

- 请求体中的 `uid/device_id` 不是授权来源，只能与会话声明完全一致。
- 上传 identity、OTK、fallback key、MLS KeyPackage、capabilities：只能操作当前 session 的设备。
- 撤销其他设备：要求账号 Master/device-signing 授权或近期强认证；普通 device session 不足够。
- 读取对端设备包：允许当前活跃设备，但返回内容必须带 manifest、log position 和 proof。
- 同一 `session_id + operation_id` 幂等；重放返回原结果，不重复消费 OTK/KeyPackage。

### 3.3 请求签名

每个写操作签名以下 canonical CBOR：

```text
{
  operation,
  operation_id,
  uid,
  device_id,
  device_generation,
  identity_version,
  issued_at_ms,
  expires_at_ms,
  body_hash,
  previous_event_hash
}
```

服务端验证 session 绑定、Ed25519 签名、时间窗、版本单调和 `previous_event_hash`。默认有效期 5 分钟；时钟偏差默认 ±2 分钟；`operation_id` 至少保留 30 天用于幂等/重放审计。

### 3.3.1 E2EE-014 Trust Event canonical 定稿（编码收敛，scoped waiver）

> 本小节为 E2EE-014 落地的**定稿**，范围收敛豁免见头部状态行。仅覆盖「A 信任 B」的 trust-event 签名，不含 §3.3 通用 operation 的 `body_hash/previous_event_hash` 链（那属完整 device-bound session/透明度日志，仍 Proposed）。

**编码**：沿用现网 `key=value\n`（ASCII 字典序键、UTF-8、整数十进制、末字段无尾随换行），**不采用 §3.3 的 CBOR**。理由：`e2ee_trust_logic:canonical_payload/6` 已用此格式，确定性、零依赖；trust-event 子集不引入 CBOR 编码器与 canonical-CBOR 键序风险。CBOR 留给未来 MLS/PFv3 通用 operation。

**签名字段（字典序，客户端 Ed25519 私钥签名，服务端逐字节复算验签）**：

| 字段 | 类型 | 说明 |
|---|---|---|
| `actor_device_generation` | int | actor 设备重注册代数，防旧设备重放 |
| `actor_uid` | int | 决策发起方（原来自 session 未签，现纳入签名防跨 actor 重放） |
| `event_id` | string `[0-9a-f-]{1,64}` | 客户端生成、全局唯一幂等键 |
| `expires_at` | int(ms) | 事件有效期上界 |
| `from_state` | string | 原信任态 |
| `issued_at` | int(ms) | 签发时刻（取代旧 `ts`），freshness 下界 |
| `target_device_id` | string | 对端设备 |
| `target_ed25519` | base64 | 决策时对端身份键快照 |
| `target_identity_version` | int | 对端身份键版本，防回退 |
| `target_uid` | int | 对端用户 |
| `to_state` | string | 目标信任态 |

**校验规则（服务端，失败一律 `invalid_*` 语义错误，不泄漏签名 oracle 细节）**：
- freshness：`now-300000 ≤ issued_at ≤ now+120000`（TTL 5min、skew ±2min，对齐 §3.3）；`issued_at < expires_at ≤ issued_at+300000`。
- 幂等：`event_id` DB 唯一；同 `event_id` 重放返回原语义结果，不新增审计、不重复广播；审计保留 ≥30 天。
- 单调：`target_identity_version` 不得 `<` 该 target device 已记录版本（回退拒绝）。
- 撤销：actor 设备须 `user_device.status=active`（撤销/禁用 actor 拒绝）。
- 合法状态转换白名单不变（§3.2）；幂等不得绕过状态机（不同 `event_id` 的非法转换仍拒绝）。

**schema 落点（open items 定稿默认，migration 00000047）**：
- `user_device` 加 `device_generation int NOT NULL DEFAULT 1`（重注册 +1，不回退）、`identity_version int NOT NULL DEFAULT 1`（identity 轮换 +1，不回退）。
- `trust_audit` 加 `event_id / issued_at / expires_at / actor_device_generation / target_identity_version`；`event_id` 加 UNIQUE 约束。
- 撤销判定复用 `user_device.status`，**不新建 session 表**（完整 device-bound session=§3.1，仍 Proposed）。

**跨仓**：客户端 `e2ee_trust` 签名逻辑须同步扩展上述字段，否则验签全拒（wire 双端契约）。是否本会话连带改 imboyapp 由 leeyi 决定。

---

## 4. Device Manifest

### 4.1 规范字段

| 字段 | 约束 |
|---|---|
| `manifest_version` | 格式版本，初始 1 |
| `uid`, `device_id` | 与 device-bound session 一致 |
| `device_generation` | 物理设备重新注册递增，不回退 |
| `identity_version` | 密码学身份更新递增，不回退 |
| `ed25519`, `curve25519` | 解码后固定长度并验证低阶/无效点 |
| `mls_credential` | MLS 启用时必填，绑定同一 uid/did |
| `capabilities` | 有序唯一集合，不接受未知 critical capability |
| `created_at_ms`, `expires_at_ms` | 有界有效期；到期必须续签 |
| `previous_manifest_hash` | 除首次外必填 |
| `device_signature` | 本设备签名全部字段 |
| `account_signature` | Account Master/device-signing key 的授权签名 |

服务器只存储和验证公开签名材料，不生成客户端私钥。

### 4.2 轮换和撤销

- identity key 更新必须增加 `identity_version`，并引用旧 manifest hash；若旧设备仍可用，还需旧身份签名。
- 丢失设备由账号签名 `DeviceRevocation`；透明度日志追加事件，不删除历史。
- 客户端观察到回滚、并行冲突版本或未授权 key change 时停止新会话，直到用户核验。
- 撤销事件立即使对应 server session 失效，并删除/封存未领取 OTK、fallback key 与 MLS KeyPackage。

---

## 5. Cross-signing

### 5.1 密钥职责

| 密钥 | 用途 | 存储/恢复 |
|---|---|---|
| Account Master | 稳定账号信任根，签 device-signing/recovery 变更 | 私钥由 ADR 17 加密恢复；日常不直接使用 |
| Device-signing | 授权本账号设备 manifest/revocation | 本地安全存储；由 Master 签名；可轮换 |
| User-signing（可选） | 签联系人账号根，支持已验证联系人体验 | 本地 + 加密恢复；不影响本账号设备授权 |
| Device identity | Olm/消息签名和当前设备操作 | 仅当前设备；不得通过恢复克隆为活跃身份 |

实现可借鉴 Matrix 的职责分离，但 wire 格式以本 ADR 的 canonical CBOR 为准，不复制未审查的产品语义。

### 5.2 新设备流程

1. 新设备完成账号登录，生成新的物理 `device_id`、Olm/MLS 身份和待授权 manifest。
2. 已验证设备通过 QR/近场或经过认证的设备列表获取 manifest hash。
3. 用户在旧设备确认后，由 device-signing key 签署 manifest。
4. 服务端校验并追加透明度日志；新旧设备都验证 inclusion/consistency proof。
5. 只有步骤 4 成功，新设备才进入 `verified` 并接收新会话秘密。

用恢复根授权时，流程相同，只是第 3 步在新设备解封恢复的 account signing material；仍生成新的 device identity。

---

## 6. Key Transparency

### 6.1 日志语义

- append-only Merkle tree，leaf 为 canonical device/account event 的 hash。
- 服务返回 `SignedTreeHead{tree_size, root_hash, timestamp, key_id, signature}`。
- 读取设备包同时返回 leaf inclusion proof；客户端保存账号最高 `tree_size/root_hash`。
- 新 tree head 必须提供 consistency proof；回退或无证明分叉视为安全错误。
- 服务端签名键通过发布渠道与应用内 pin 双路径分发并支持受控轮换。

### 6.2 Gossip 与监控

- 联系人通信时可在 Protected Frame 内携带最近 tree head 摘要，检测 split view。
- 官方 monitor 从独立网络持续拉取和比对 tree head；只监控公开哈希，不需要消息内容。
- 客户端发现两个同 size 不同 root 的有效签名头时，保存证据、停止信任新设备并显示高优先级告警。

透明度不能证明设备端未被攻陷，但能显著提高服务端静默替换/选择性展示密钥的可检测性。

---

## 7. OTK / KeyPackage 抗耗尽

1. claim 使用 `(requester_uid, requester_did, target_uid, target_did, identity_version, request_id)` 幂等键。
2. 单目标、单请求者、单租户和全局四层 token bucket；具体阈值通过压测配置，但硬上限不可被租户关闭。
3. 单次最多为目标的每个活跃设备领取一个预密钥；禁止任意批量抓取全库。
4. OTK/KeyPackage 领取后进入短租约；相同 request id 返回同一结果；超时后按协议安全规则处理，不能简单回 available 造成重复领取。
5. fallback key 只能作为协议定义的预密钥耗尽机制，不能绕过 device manifest/identity version 校验。
6. 异常耗尽触发设备端补充与服务端告警，但不能触发 RSA/明文降级。

---

## 8. Trust Event 防重放

Trust event 在现有签名字段上增加：

- `event_id`：随机 128-bit，数据库唯一；
- `actor_session_id`、`actor_device_generation`；
- `issued_at_ms`、`expires_at_ms`；
- `previous_event_hash`；
- `target_identity_version`。

同 event_id 重放为幂等；不同 event_id 的相同状态变更仍写审计，但必须符合状态机。过期、未来时间、target version 回滚、actor session 已撤销均拒绝。

---

## 9. 数据与 API 迁移

### 后端

- `auth_ds.erl` 及认证中间件：新增/验证 device-bound session。
- `olm_handler.erl`、identity/OTK logic/repo：从会话取 DID，拒绝 body 越权；所有写入接受 operation signature。
- `e2ee_trust_logic.erl`、`trust_audit`：增加 event_id、时间、session、版本、哈希链和唯一约束。
- 新增 account signing public keys、device manifest event、transparency tree/head/proof 存储与只读 API。
- 迁移必须可在线回填：现有设备先标 `legacy_unverified`，不得自动变 verified。

### Flutter

- device key bundle 解析先做 base64/长度/版本/签名/透明度 proof 验证。
- `CapabilityNegotiator` 只接受已验证 manifest 内 capabilities；缓存带 identity/log version。
- Trust UI 区分 `legacy_unverified / verified / revoked / transparency_error`。
- 收到撤销或 key change 时清除对应新发送会话，不删除历史只读解密材料。

---

## 10. 验收测试

| ID | 用例 | 通过条件 |
|---|---|---|
| DT-01 | 设备 A 的 token 上传 body.did=B 的 identity/OTK/capability | HTTP 403；DB 无变化 |
| DT-02 | 篡改 JWT did/session 或使用已撤销 session | 拒绝；无 body 值兜底 |
| DT-03 | 同 operation_id 重放 100 次 | 只有一条状态变化/一次 OTK 消费，响应一致 |
| DT-04 | identity_version 回退或并行冲突 | 拒绝并产生安全审计 |
| DT-05 | 已验证设备 cross-sign 新设备 | 双端验证 signature + inclusion proof 后才变 verified |
| DT-06 | 服务端替换 identity key 并重签自身响应 | 因 account signature/透明度 proof 无效而阻断 |
| DT-07 | 服务端向两个客户端给出 split-view tree heads | gossip/monitor 检出，同 size 异 root 永不自动接受 |
| DT-08 | trust event 过期、未来、重复、target version 回滚 | 分别被拒或幂等，不产生非法转换 |
| DT-09 | 1000 并发 claim 50 个 OTK | 无重复 key；限流生效；不触发弱协议 fallback |
| DT-10 | 撤销设备后继续 claim/上传/收群更新 | server session 失效，所有新秘密拒绝 |
| DT-11 | 公钥非法长度/无效点/超长编码 | 密码学调用前拒绝，无崩溃/OOM |
| DT-12 | 全设备丢失后重建账号根 | 联系人看到 root reset 告警，旧 verified 不继承 |

---

## 11. 运维和隐私取舍

- 透明度日志增加公开设备事件和长期存储成本；leaf 只含必要标识与公钥哈希，避免设备名称、型号等额外隐私数据。
- device-bound session 增加会话状态存储与撤销查询；这是即时设备撤销的必要成本。
- Cross-signing 增加恢复复杂度；ADR 17 把恢复变成独立安全域，避免为了 UX 克隆活跃设备私钥。
- 透明度服务属于认证系统，不得与消息内容服务共享“可修改数据库即全权伪造”的单一权限；生产部署至少分离签名键和 DB 写权限。

---

## 12. 生效条件

- [ ] 账号根丢失/重置的产品流程和客服说明获批
- [ ] transparency 数据保留与隐私评审通过
- [ ] device-bound session 的移动端升级/旧 token 过渡方案通过
- [ ] DT-01..12 自动化方案明确

接受后，在 ADR 03/04/06 顶部标注被替代的章节；Safety Number 和历史 trust audit 继续保留。
