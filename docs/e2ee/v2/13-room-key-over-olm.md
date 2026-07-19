# ADR 13 — Room Key Distribution over Olm（C2G 群密钥分发棘轮化）

> **状态**：已冻结（Frozen，用户 2026-07-19 签字）——填补 ADR 02/05 未覆盖的「Megolm room key 分发密码学」空白。§3 线格式与 §5 灰度退出条件为 Normative，变更走 01-overview §5 supersedes 流程。
> **关联**：02-protocol（C2C Olm message 方向不变）、04-capability-negotiation（Olm 能力）、05-metadata-version（§6 新增字段 checklist、双写范式）、07-storage（Olm pickle）、08-threat-model（T3/T5）
> **不取代任何冻结决定**：ADR 02 §5.1 的「C2C 用 Olm 加密消息（B-大）」方向**保留不动**；本 ADR 只处理 **Megolm room key（sender key）如何安全分发到接收设备**，这是 v1 静默继承、两份 ADR 从未规定的部分。

---

## 1. 背景：被 ADR 漏掉的弱环

现状（`group_session_service.dart:387-419` `buildRoomKeyPayload`）：Megolm outbound session 导出的 room key（`exportAt(0)`）分发时，**逐接收设备用其静态 RSA-OAEP-256 公钥包裹**：

```
keys: [{ did, kid, wrap_alg:'RSA-OAEP-256', ek: RSA_wrap(exportedKey, devicePubPem) }]
```

**弱点**：设备 RSA 密钥是**长期静态**的。攻击者只要
1. 录到 `e2ee_room_key` 消息（服务端可见、可存档），且
2. 任意时刻拿到该设备 RSA 私钥一次（设备攻陷 / 备份泄露 / 弱存储），

即可解开发给该设备的**每一把** room key，`exportAt(0)` 是棘轮起点 → 可推演整条 Megolm 链 → **解密该会话历史 + 未来全部消息**。这就是**无前向保密（FS）、无攻陷后恢复（PCS）**。静态 RSA 设备密钥几乎从不轮换，弱点长期存在。

**为什么 ADR 没覆盖**：ADR 02 定义消息层套件选择（Olm vs Megolm vs RSA），ADR 05 定义信封版本化；`e2ee_room_key` 的密钥包装算法从 v1 继承，两份 ADR 只把它当「服务端不透明透传的具名 action」（02 §6 / 05 §8），**从未审视其密码学强度**。

**为什么群聊是最大暴露面**：C2C 若按 ADR 02 §5.1 切 Olm（B-大）可消除 1:1 的此弱环，但**群聊（C2G）永远走 Megolm + room key 分发**——即使冻结 ADR 全部实现，所有群聊的密钥通道仍停在静态 RSA。群消息量/成员数更大，暴露面更广。

---

## 2. 决策

**用 Olm（X3DH + Double Ratchet）逐设备包裹 Megolm room key，替代静态 RSA-OAEP-256。** 即 Matrix 的 `m.room_key over Olm` 范式。

- **复用既有 Olm 会话基础设施**：`OlmSessionService.encryptC2CMessage/decryptC2CMessage`（per-device、X3DH claim、DR 棘轮）已实现（B.1）。room-key-over-Olm = 把 `buildRoomKeyPayload` 里的 `RSA_wrap` 换成对该设备的 Olm 会话 `encrypt`，接收侧 `RSA_unwrap` 换成 Olm `decrypt`。**不改 Megolm 消息收发、不改服务端、不改信封版本。**
- **安全增量**：room key 分发通道拿到 **per-message FS + PCS**。设备 identity/OTK 泄露一次，录到的历史 room key 密文**不可**解（Olm 用一次性 ephemeral DH），且会话下一往返自愈。
- **范围**：本 ADR 只管 **C2G room key 分发**（并顺带覆盖当前「C2C-via-Megolm」的 room key，因两者共用 `buildRoomKeyPayload`）。**C2C 是否最终切 ADR 02 §5.1 的 Olm-message（B-大）另议**，本 ADR 不决定、不冲突。

---

## 3. 线格式（wire format）

### 3.1 双包（dual-wrap）——向后兼容硬约束

过渡期每个接收设备的 `keys[]` 条目**同时携带 RSA 与 Olm 两种包裹**，Olm 作为**可选附加子对象**（遵循 ADR 05 §1 决策 4「未知字段忽略」+ §6 新增字段 checklist）：

```jsonc
{
  "did": "<peerDeviceId>",
  "kid": "<keyId>",
  "wrap_alg": "RSA-OAEP-256",          // 冻结字段不变（ADR 05 §5）
  "ek": "<RSA_wrap(exportedKey)>",     // 老客户端唯一读取路径
  "olm": {                             // 新增可选子对象；老客户端忽略
    "v": "OLM.V1",
    "type": 0,                          // Olm message_type：0=prekey, 1=normal
    "sid": "<senderDeviceId>",          // 发送方 deviceId（Olm 入站会话定位所需）
    "body": "<olmSession.encrypt(exportedKey).ciphertext>"
  }
}
```

- **老客户端**（不懂 `olm`）：只读 `wrap_alg`+`ek`，RSA 解包，行为**完全不变**。
- **新客户端**（Olm-capable）：`olm` 存在且本端对该 did 有/可建 Olm 会话 → 走 Olm 解包；否则回退 `ek`（RSA）。**优先 Olm。**
- 一个 did 一个条目（非两条），避免 `pickMyKeyEntry`（返回首个 did 命中）拿错条目。
- **`sid` 字段（发送方 deviceId）为何必需**：RSA 路径用接收方自己的私钥解包，不需发送方身份；Olm `decryptC2CMessage(peerUid, peerDeviceId, messageType, ciphertext)` 的 `createInboundSession` 需 `_lookupPeerIdentityKey(peerUid, peerDeviceId)`，即必须知道发送方 (uid, did)。发送方 **uid 复用消息传输层 `from` 字段**（后端已鉴权），发送方 **did 由 `olm.sid` 携带**。`sid` 是 payload 层附加字段（ADR 05 §6），老端忽略、缺省回退 RSA，不破坏兼容。

### 3.2 单包期（transition 完成后）

当活跃设备 Olm-capable 占比达标（§5），停止写 `ek`，条目退化为纯 Olm：

```jsonc
{ "did":"...", "kid":"...", "wrap_alg":"OLM.V1", "type":0, "sid":"<senderDeviceId>", "body":"..." }
```

`wrap_alg='OLM.V1'` 是**新增合法取值**（ADR 05 §5 `wrap_alg` 语义为「包裹算法标识」，本就为多算法预留）。RSA 解包能力**永久保留**用于历史 room key。

### 3.3 合规审计条目不变

`complianceEntryFor`（`group_session_service.dart:422`）的 `compliance-audit` 条目**保持 RSA-OAEP-256**：合规公钥是 RSA，审计侧无 Olm 会话。room-key-over-Olm 不影响合规包裹（两者是 `keys[]` 中并列的独立条目）。

---

## 4. Fan-out 成本与会话建立

- Olm 包裹需对每个接收设备有一条 Olm 出站会话。首次给某设备分发 room key 时，`_establishOutboundSession` 自动 claim 对端 OTK + X3DH 建会话（B.1/B.3 已实现，OTK claim 审计语义 B.3 已落地）。
- 成本 = **每个新设备一次 OTK claim**（room key 仅在成员/设备集变化时轮换，频率低）。远小于 B-大的每消息 fan-out。
- 对端设备无 Olm 身份（未升级）→ 该 did 的 `olm` 子对象**缺省不写**，仅 RSA（§3.1 回退），不阻断分发。

---

## 5. 灰度与退出（对齐 ADR 05 §4.3 量化范式）

| 阶段 | 动作 |
|---|---|
| 进入双包期 | 新客户端发布日起，room key 条目同写 RSA `ek` + `olm` 子对象 |
| 退出双包期（停写 `ek`）需同时满足 | ①近 30 天活跃设备 Olm-capable 占比 ≥95%；②近 30 天 room key 入站走 Olm 解包占比 ≥99%；③距最后「仅 RSA」客户端发版 ≥90 天 |
| 单包期 | 新 room key 仅 Olm 包裹；RSA 解包能力永久保留（历史 room key） |

---

## 6. 威胁模型映射（ADR 08）

| 威胁 | 现状（静态 RSA） | 本 ADR 后 |
|---|---|---|
| **T5 前向保密**（设备攻陷） | ❌ 群 room key 无 FS，一次泄露解全史 | ✅ Olm ephemeral DH，录到的历史 room key 密文不可解 |
| **T3 服务端攻陷**（存档密文 + 诱取静态私钥） | ❌ 静态私钥一次泄露 → 存档 room key 全开 | ✅ PCS 自愈，一次泄露不等于长期沦陷 |
| **T7 伪造/域注入** | 现有 gid/scope 域一致性校验（`handleRoomKeyMessage:255-277`） | **不变**，继续生效 |

**诚实声明**（ADR 08 §3 风格）：本 ADR 只加固**分发通道**；Megolm 消息体本身的安全属性不变。C2C 消息层是否达到 Signal 级 per-message PFS，取决于是否另做 B-大（ADR 02 §5.1），本 ADR 不覆盖。

---

## 7. 守护测试要求（实现 slice 落地时逐项）

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-13-01 | `buildRoomKeyPayload`（Olm 模式）产出的条目**同含** `ek` 与 `olm{type,body}` | §3.1 双包 |
| T-13-02 | 老客户端解析器（只读 `ek`）对双包条目 RSA 解包成功，得到正确 exportedKey | §3.1 向后兼容 |
| T-13-03 | 新客户端对双包条目**优先** Olm 解包，得到与 RSA 路径一致的 exportedKey | §3.1 优先 Olm |
| T-13-04 | Olm 包裹的 room key round-trip：`olm.encrypt(exportedKey)` → 对端 `olm.decrypt` → `InboundGroupSession.import` sessionId 一致 | §2 正确性 |
| T-13-05 | 对端无 Olm 身份时，条目仅 RSA、无 `olm` 子对象，分发不阻断 | §4 回退 |
| T-13-06 | 域一致性校验（gid/scope vs type/to）在 Olm 路径下仍生效 | §6 T7 不回退 |
| T-13-07 | 合规模式下 `compliance-audit` 条目保持 RSA-OAEP-256 | §3.3 |
| T-13-08 | `olm` 子对象缺 `sid`（或 sid 为空）→ 无法定位发送方，视为不可 Olm 解包，回退 `ek`（RSA），不崩溃 | §3.1 健壮性 |

真机验证（实现后，需你操作 Android 真机）：两账号各一设备入群，A 轮换 room key，B 走 Olm 解包成功收群消息；杀会话重启后历史可解。

---

## 8. 与其他 ADR 的关系

| ADR | 关系 |
|---|---|
| 02-protocol | **不冲突**。02 §5.1 管 C2C 消息层 Olm；本 ADR 管 C2G room key 分发层。Olm 会话复用 02 的 `E2eeSessionProtocol`/OlmSessionService 基础设施 |
| 04-capability | 「对端是否 Olm-capable」决定该 did 写不写 `olm` 子对象；复用 04 能力枚举 |
| 05-metadata | `keys[].olm` 是 payload 层附加字段，走 05 §6 新增字段 checklist（归 payload、老端忽略、缺省回退 RSA、不升 `meta_version`、`wrap_alg` 新取值 `OLM.V1`）；`ek`/`keys` 冻结字段语义不变 |
| 07-storage | Olm 会话 pickle 按 07 §3 存储（pickle key CSPRNG，B.2.1 已修） |
| 08-threat | T5/T3 群 room key 通道防御由本 ADR 落地 |

**冲突仲裁**：本 ADR 经签字冻结后，§3 线格式与 §5 灰度退出条件为不可单方面变更项；变更走 01-overview §5 supersedes 流程。

---

## 9. 决策摘要

| 决策点 | 选择 | 理由 |
|---|---|---|
| 弱环 | Megolm room key 走静态 RSA-OAEP-256（ADR 空白） | 无 FS/PCS，群聊最大暴露面 |
| 方案 | room-key-over-Olm（Matrix m.room_key 范式） | 小 diff、复用既有 Olm 会话、覆盖 C2G（+顺带 C2C-Megolm） |
| 兼容 | 双包（RSA `ek` + 可选 `olm` 子对象），老端忽略 `olm` | 零破坏灰度，ADR 05 §6 附加字段范式 |
| 范围 | 仅 room key 分发层；C2C 消息层 B-大另议 | 不动 ADR 02 冻结方向 |
| 服务端 | 零改动（仍不透明透传 `e2ee_room_key`） | ADR 02 §6 / 05 §8 契约不变 |
| 退出 | 95%/99%/90 天达标停写 RSA；RSA 解包永久保留 | 对齐 ADR 05 §4.3 |
