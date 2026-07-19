# ADR 04 — Capability Negotiation

> **状态**：Architecture Freeze
> **关联**：02-protocol / 03-device-identity / 06-device-trust / 08-threat-model
> **不可单方面变更**：协商算法、fallback 顺序、签名验证流程（详见 §10）

---

## 1. 决策（Decision）

**Capability Negotiation 完全在客户端进行**：发送方拉取对端每个设备的 capabilities（per-device，签名过的），按固定 fallback 顺序选择「双方都支持的最高安全级别套件」，逐设备加密 fan-out。服务端**仅存储与转发** capabilities，**不参与算法决策**。

核心不变量：

1. capabilities 必须由对端设备的 **Ed25519 signing key** 签名（与 ADR 03 衔接），服务端无法伪造；
2. 协商结果**不可降级到明文**（除非全局 `e2ee_mode = optional` 且双方均无任何 E2EE 套件）；
3. 同一对端的不同设备可能用**不同套件**（per-device 协商，非 per-user）；
4. 协商算法是**纯函数**：给定 `(my_protocols, peer_device_caps)` 集合，输出唯一确定的套件。

---

## 2. 问题陈述（Problem Statement）

### 为什么需要 per-device capability

现有 `imboy_policy` 模块（`imboy_policy_normalize.erl:185`）定义的 `e2ee_mode` 是**全局/租户级**策略，取值 `disabled / optional / compliance / required`，通过 `/api/v1/app/policy` 下发，客户端 `EncryptionModeService` 镜像。它回答的是「这个租户是否启用 E2EE」。

但它**无法回答**：

- 对端用户有 3 个设备（iOS v2 / Web v2 / 一台只支持 v1 RSA-OAEP 的老 Android），我该用什么套件发？
- 对端升级后多支持了 `megolm`，我如何感知？
- 对端的某个设备被撤销了 `olm` 能力（旧版本），我如何避免发到不可解密的套件？

如果不做 per-device capability：

- **退化为「按用户最低公分母」**：为了让对端所有设备都能解密，被迫全程使用最弱的 v1 RSA-OAEP，浪费 v2 PFS；
- **无法支持混合套件 fan-out**：老设备只能拿 RSA 密文，新设备却必须忍受弱协议；
- **服务端成为唯一真相源**：服务端可任意篡改「对端支持什么」，T2（Compromised Server）可轻易诱导降级。

### 为什么必须双向匹配

E2EE 不是「发送方决定」，而是「发送方与接收方能力的交集」。例如发送方只支持 `{rsa-oaep}`，即使对端支持 `{olm, megolm}`，也只能用 `rsa-oaep`。这决定了算法必须取**交集**而非并集。

---

## 3. Capability 数据模型

每个 device（见 ADR 03 的 `user_device` 表）携带：

```json
{
  "device_id": "01J...",
  "identity_key": "<base64 curve25519>",
  "signing_key": "<base64 ed25519>",
  "protocols": ["olm", "megolm"],          // 该设备支持的套件列表
  "protocols_sig": "<ed25519 signature over protocols>",  // 由 signing_key 签名
  "capabilities_ts": 1721300000000         // 客户端声明时间戳
}
```

字段语义：

| 字段 | 含义 | 谁写入 |
|---|---|---|
| `protocols` | 该设备支持的协议套件 ID 列表 | 客户端登录/激活时上报 |
| `protocols_sig` | 对 `protocols`（含 ts、device_id）的 Ed25519 签名 | 客户端用本地 signing key 签 |
| `signing_key` | 用于校验 `protocols_sig` 的公钥（即 ADR 03 的 device identity signing key） | 客户端上报，服务端存储 |
| `capabilities_ts` | 声明时间戳，用于客户端检测异常降级 | 客户端 |

**协议套件 ID**（与 ADR 02 的 Protocol Registry 一致）：

| ID | 全称 | 安全级别 | 来源 |
|---|---|---|---|
| `olm` | Olm Double Ratchet（1:1 PFS） | 高（per-message PFS） | v2 |
| `megolm` | Megolm 群聊 ratchet | 中（前向保密，无 PFS） | v2 |
| `rsa-oaep` | RSA-OAEP-256 + AES-256-GCM（v1 单聊） | 低（无 PFS） | v1 |
| `mls` | MLS（未来） | 高（预留） | 未实现 |

`protocols` 列表**无序**，安全级别由本表的固定排名决定，不由列表顺序决定（防服务端重排）。

---

## 4. 协商算法（Negotiation Algorithm）

### 4.1 全局 fallback 顺序（冻结）

```
SECURITY_RANK = [olm, megolm, rsa-oaep]   // 由高到低
// mls 在实现后插入 olm 之前；当前占位不参与协商
```

**为什么是这个顺序**：

- `olm` 提供 per-message PFS（ADR 02 / 08-T5），最高优先；
- `megolm` 仅前向保密、无 PFS（群聊必需的 fan-out 效率权衡），次之；
- `rsa-oaep` 是 v1 兼容路径，无前向保密，仅当对端无更好选项时使用；
- **明文不在 fallback 链中**（除非全局策略显式允许，见 §6）。

### 4.2 单设备协商（伪代码）

```
function negotiateOneDevice(myProtocols, peerDevice):
    # 1. 校验 peerDevice.protocols_sig 由 peerDevice.signing_key 签发
    if not verifySig(peerDevice.signing_key,
                     peerDevice.protocols || peerDevice.device_id || peerDevice.capabilities_ts,
                     peerDevice.protocols_sig):
        raise CapVerificationFailed   # 拒绝使用，触发 TOFU 告警

    # 2. 取交集
    common = intersect(myProtocols, peerDevice.protocols)

    # 3. 按 SECURITY_RANK 选最高的共同套件
    for suite in SECURITY_RANK:
        if suite in common:
            return suite

    # 4. 无交集
    return NO_COMMON_SUITE
```

### 4.3 多设备 fan-out（伪代码）

```
function negotiatePeer(myProtocols, peerDevices):
    plan = {}
    for device in peerDevices:
        suite = negotiateOneDevice(myProtocols, device)
        if suite == NO_COMMON_SUITE:
            plan[device.id] = UNSUPPORTED   # 该设备不投递
        else:
            plan[device.id] = suite
    return plan
```

发送时，按 `plan` 逐设备用对应套件的 `E2eeSessionProtocol.encrypt()`（ADR 02 接口）加密同一明文，再 fan-out。

### 4.4 示例：双端不匹配时的 fallback

| 我方支持 | 对端设备 A 支持 | 对端设备 B 支持 | 对端设备 C 支持 | 协商结果 |
|---|---|---|---|---|
| `{olm, megolm, rsa-oaep}` | `{olm, megolm}` | `{megolm}` | `{rsa-oaep}` | A→`olm`, B→`megolm`, C→`rsa-oaep` |
| `{olm, megolm}` | `{rsa-oaep}` | — | — | A→`NO_COMMON_SUITE`（**不降级明文**） |
| `{rsa-oaep}` | `{olm, megolm}` | — | — | A→`rsa-oaep`（取交集，发送方受限） |
| `{olm, megolm}` | `{}` 或签名失败 | — | — | A→`UNSUPPORTED` + TOFU 告警 |

---

## 5. 多设备 fan-out 语义

对端用户 U 有 3 个设备 D1 / D2 / D3：

- D1（iOS v2）：`protocols: ["olm", "megolm"]`
- D2（Web v2）：`protocols: ["megolm"]`
- D3（老 Android v1）：`protocols: ["rsa-oaep"]`

发送方 S（支持 `{olm, megolm, rsa-oaep}`）发一条消息：

```
plaintext = "hi"
for device in [D1, D2, D3]:
    suite   = negotiateOneDevice(S.protocols, device)
    payload = ProtocolRegistry.get(suite).encrypt(plaintext, device)
    send(device.id, payload)
```

结果：**同一条消息被加密 3 次**，分别用 `olm` / `megolm` / `rsa-oaep`，每设备只拿到自己能解密的版本。这是 per-device 协商的本质——**不同设备用不同套件**，避免「按最弱设备一刀切」。

代价：发送方 CPU 与上行带宽 ×N（N=对端设备数）。可优化项（非本 ADR 范围）：相同套件的多个设备可共享 Megolm room key 投递，减少重复加密。

---

## 6. 与全局 `e2ee_mode` 的关系

全局 `e2ee_mode`（`imboy_policy_normalize.erl:185`，取值 `disabled/optional/compliance/required`）是**租户级策略**，决定「协商失败怎么办」；per-device capability 决定「能不能协商出套件」。两者优先级：

| `e2ee_mode` | 协商成功 | 协商失败（NO_COMMON_SUITE） |
|---|---|---|
| `required` | 用协商出的套件加密 | **拒发**，UI 提示「对端设备不支持 E2EE，无法发送」 |
| `compliance` | 用协商套件 + compliance wrap（合规密钥双加密） | **拒发**（同 required） |
| `optional` | 用协商出的套件加密 | **可降级明文**，UI 标记「未加密发送」 |
| `disabled` | — | 全程明文（per-device capability 不查询，直接发送） |

**优先级规则**（一句话）：**全局策略是「策略层」决策，per-device capability 是「技术层」可行性；策略层不可被技术层绕过，但技术层失败时按策略层规定处理**。

- 全局 `required` 时，即使对端某设备只支持 `rsa-oaep`，仍可发（RSA-OAEP 满足「E2EE」语义）；
- 全局 `required` 时，对端某设备完全无 E2EE 套件，则该设备不投递（不能因为「想发出去」就降明文）；
- 全局 `optional` 是唯一允许降级明文的口子，且必须在 UI 显式标记。

---

## 7. 服务端职责

服务端在 capability 协商中**只做三件事**：

1. **存储**：客户端登录/激活时上报 `protocols` + `protocols_sig` + `signing_key`，写入 `user_device` 表（ADR 03）；
2. **下发**：发送方拉取对端 device 列表时，原样返回上述字段（含签名）；
3. **透传**：不解析、不排序、不裁剪 `protocols` 列表。

**服务端禁止做的事**（T2 防御的关键）：

- ❌ 根据「租户配置」篡改对端的 `protocols`（如偷偷删掉 `olm` 诱导降级）；
- ❌ 替换对端的 `signing_key`（会被签名校验拦下）；
- ❌ 参与协商算法（算法纯客户端，结果可复现验证）。

由于服务端不持有任何 device 的 Ed25519 signing private key，**伪造 capabilities 必然导致签名校验失败**。这是 ADR 03 device identity 与本 ADR 协商的衔接点。

---

## 8. 降级攻击防御（T2）

### 8.1 防御层 1：签名校验

每个 device 的 `protocols` 必须由该 device 的 Ed25519 signing key 签名。客户端在 `negotiateOneDevice` 第 1 步强制校验：

- 签名失败 → 拒用该 device，标记 `UNSUPPORTED`；
- 签名通过但 `signing_key` 与本地 TOFU 缓存不一致 → 触发 §8.2 告警。

### 8.2 防御层 2：TOFU + 异常降级告警

客户端本地持久化每个对端 device 的「上次见到的 capabilities 快照」。当检测到以下情形，触发 TOFU 告警（UI 显示「对端设备 X 的加密能力发生变化，请确认」）：

- 首次见到该 device（TOFU：trust on first use）；
- `protocols` 集合**收缩**（如从 `{olm, megolm}` 变为 `{rsa-oaep}`）——异常降级强信号；
- `signing_key` 变化——可能设备重置或中间人，必须人工确认。

**收缩**告警的合理性：正常情况下 capability 只会随软件升级**扩张**；收缩几乎只发生在攻击者试图降级时，或用户真的换了一台老设备（后者由用户主动确认）。

### 8.3 防御层 3：策略层硬约束

`e2ee_mode = required / compliance` 时，协商失败的设备直接拒发（§6），无任何「自动降级到明文」路径，服务端无法通过伪造 capabilities 让客户端发明文。

### 8.4 不防御的明确项（诚实声明）

- **用户主动接受降级**：TOFU 告警弹出后，用户点「仍然发送」，架构不阻止（UX 与安全的权衡，ADR 08-T8 同源）；
- **服务端拒绝下发某 device**：服务端可以「装作该 device 不存在」（如声称对端只有 2 个设备而非 3 个），导致消息不投递到被隐藏设备——这是**可用性攻击**，不是机密性攻击，不在本 ADR 防御范围（属 ADR 06 device-trust 的 device visibility 问题）。

---

## 9. 守护测试要求

以下测试必须存在且 CI 必跑（ADR 08 防御点追溯矩阵第 7 行）：

| 测试 ID | 场景 | 期望结果 |
|---|---|---|
| `cap_negotiate_highest_common` | 双方都支持 `{olm, megolm}` | 选 `olm` |
| `cap_negotiate_fallback_chain` | 我方 `{olm, megolm}`，对端 `{megolm, rsa-oaep}` | 选 `megolm`（不跨级跳 rsa-oaep） |
| `cap_negotiate_sender_limited` | 我方 `{rsa-oaep}`，对端 `{olm, megolm}` | 选 `rsa-oaep`（取交集） |
| `cap_negotiate_no_common_required` | 全局 `required`，无交集 | 拒发，返回 `SEND_BLOCKED_BY_POLICY` |
| `cap_negotiate_no_common_optional` | 全局 `optional`，无交集 | 降级明文，payload 标记 `unencrypted: true` |
| `cap_sig_tampered_rejected` | 服务端篡改 `protocols` 但未更新 `protocols_sig` | 签名校验失败，device 标记 `UNSUPPORTED` |
| `cap_sig_key_swap_rejected` | 服务端替换 `signing_key` 为攻击者公钥 | TOFU 缓存不匹配，触发告警 |
| `cap_downgrade_shrink_alert` | 对端 capabilities 从 `{olm,megolm}` 变 `{rsa-oaep}` | 触发 TOFU 降级告警 |
| `cap_multi_device_mixed_fanout` | 对端 3 设备分别支持 olm/megolm/rsa-oaep | 生成 3 份不同套件密文，各投递各的 |
| `cap_disabled_skips_query` | 全局 `disabled` | 不查询对端 capabilities，直接明文发送 |

测试形式：客户端 Dart 单测（协商算法）+ 服务端 Erlang eunit（存储/下发不篡改契约）。

---

## 10. 与其他 ADR 的关系

| 关联 ADR | 关系 |
|---|---|
| **02-protocol** | 协商结果调用 ADR 02 的 `E2eeSessionProtocol` 接口加密；协议套件 ID 与 Protocol Registry 一致 |
| **03-device-identity** | `protocols` / `signing_key` 字段定义在 ADR 03 的 `user_device` 表；签名验证复用 ADR 03 的 Ed25519 identity |
| **06-device-trust** | TOFU 告警的 UI 与信任状态机由 ADR 06 定义；Safety Number 验证可消除降级告警 |
| **08-threat-model** | 本 ADR 防御 T2（Compromised Server 强制降级）；T8（用户主动接受降级）显式不防御 |

**冲突原则**：本 ADR 的协商算法与 fallback 顺序属于「不可单方面变更」冻结项（见 01-overview §5）。任何变更需新建 `NN-supersedes-04.md` 并人工 review 签字。
