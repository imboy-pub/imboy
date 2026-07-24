# ADR 11 — Backward Compatibility Matrix

> **状态**：Architecture Freeze
> **用途**：集中定义新旧客户端、新旧协议套件之间的兼容矩阵。散落在 02/05 的兼容性约束在此汇总为单一查询表。
> **关联**：02-protocol（legacy 解析）、05-metadata（双写期）、08-threat-model（T9 rollback）

---

## 1. 客户端版本矩阵

| 发送方 → \ 接收方 ↓ | v0 旧客户端（仅 RSA） | v1 客户端（RSA + Megolm） | v2 客户端（RSA + Megolm + Olm） | 未来 v3（+ MLS） |
|---|---|---|---|---|
| **v0 旧客户端（仅 RSA）** | RSA 双向 ✅ | RSA（接收方降级） | RSA（接收方降级） | RSA 或不兼容 ⚠️ |
| **v1 客户端（RSA + Megolm）** | RSA（发送方降级） | Megolm 双向 ✅ | Megolm（协商交集） | Megolm 或不兼容 ⚠️ |
| **v2 客户端（RSA + Megolm + Olm）** | RSA（发送方降级） | Megolm（协商交集） | **Olm 双向 ✅**（单聊）/ Megolm（群聊） | Olm 或 Megolm（协商） |
| **未来 v3（+ MLS）** | RSA 或不兼容 | Megolm 或不兼容 | Olm/Megolm（协商） | MLS 双向 ✅（未来） |

**矩阵语义**：
- 「协商交集」= 取双方 capabilities 的交集，按 SECURITY_RANK（ADR 04）选最高；
- 「降级」= 接收方不支持发送方首选套件时，回退到双方都支持的较低套件；
- 「不兼容」= 双方无共同套件，按 `e2ee_mode` 决定（required→拒发，optional→明文）。

---

## 2. 协议套件兼容性（细粒度）

| 套件 | 加密能力 | 解密能力 | 状态 |
|---|---|---|---|
| `rsa-oaep` (RSA-OAEP-256+AES-256-GCM) | v2 客户端**不再产生**新密文 | **所有版本永久可解**（历史消息） | Legacy decrypt-only |
| `megolm` (MEGOLM.V1) | v1+ 客户端（群聊默认） | v1+ 客户端 | Active（群聊） |
| `olm` (OLM.V1) | v2 客户端（单聊，灰度开启后） | v2 客户端 | Active（单聊，灰度） |
| `mls` (MLS.V1) | 未来 v3 | 未来 v3 | Reserved（本轮不实现） |

**关键约束**（ADR 02 §5）：
- `rsa-oaep` 的 `encrypt` 在 v2 客户端中**抛 UnsupportedError**（防止降级攻击，T2）；
- `rsa-oaep` 的 `decrypt` **永久保留**（历史消息必须可解）；
- 任何套件的 `decrypt` 不可移除，否则破坏历史消息可读性。

---

## 3. Metadata 版本兼容性（ADR 05 详述）

| 发送方 metadata 版本 | v0/v1 接收方 | v2 接收方 | 未来 v3 |
|---|---|---|---|
| **v1**（`e2ee_ver=1/2` + `e2ee_suite` 字符串） | ✅ 原生支持 | ✅ `ProtocolSuite.fromMetadata` legacy 解析 | ✅ legacy 解析 |
| **v2**（`meta_version=2` + ProtocolSuite 三元组） | ⚠️ 忽略未知字段，读 `e2ee_suite`（双写保证） | ✅ 原生 | ✅ |
| **未来 v3** | ⚠️ 双写期 v1 字段兜底 | ✅ | ✅ |

**双写期约束**（ADR 05 §4）：
- v2 客户端发送时**同时写 v1 字段（`e2ee_ver`/`e2ee_suite`）和 v2 字段（`meta_version`/ProtocolSuite）**；
- 双写期退出条件：遥测确认 ≥99% 客户端可解析 v2；
- 退出后才允许只写 v2。

---

## 4. 服务端兼容性（零参与原则）

| 维度 | 服务端行为 | 兼容性 |
|---|---|---|
| e2ee map 透传 | 不解析字段，原样存 jsonb | 任意版本元数据均可透传 ✅ |
| `content_bearing_action` 白名单 | action 非空/非 message_edit 即放行 | 任意协议的 key 分发消息可透传 ✅ |
| `encrypted_message_body` 判定 | e2ee 非空 map + payload 非空 → 已加密 | 任意套件满足此条件 ✅ |
| 服务端零密码学 | 不调用任何 E2EE payload 的 decrypt | 线 A 已落地并 CI 守护 ✅ |

**结论**：服务端是协议无关的，客户端任意版本组合均不要求服务端改动。

---

## 5. 升级路径

### 5.1 v0/v1 → v2 客户端升级

- 旧客户端**无需强制升级**（v2 客户端会降级到 RSA/Megolm 与之通信）；
- 升级后旧消息**仍可解密**（legacy 套件 decrypt 保留）；
- 新消息按 capability 协商，可能用 Olm（单聊）或继续 Megolm（群聊）。

### 5.2 v2 → 未来 v3（MLS）升级

- Protocol Registry 已占位 `mls`，新增 `MlsProtocol` 实现后业务层零改动；
- 双端都升级到 v3 且 capability 含 `mls` 时，新会话用 MLS；
- 旧会话（Olm/Megolm）保持原套件直到会话结束。

### 5.3 服务端零升级要求

- 服务端代码无需因协议演进改动（透传 + 白名单机制足够）；
- 仅当新增**服务端参与的状态**（如 MLS 的 group state 同步）时才需后端改动。

---

## 6. 不兼容场景（诚实声明）

| 场景 | 行为 | 用户感知 |
|---|---|---|
| 双方无共同套件 + `e2ee_mode=required` | 拒发 | 「无法发送加密消息，请升级」 |
| 双方无共同套件 + `e2ee_mode=optional` | 降级明文 | 消息正常发送（明文） |
| 接收方收到无法识别的 meta_version | 忽略未知字段，尝试 legacy 解析 | 解密失败时显示「[加密消息]」 |
| 接收方收到无法识别的 e2ee_suite | `ProtocolSuite.parse` 返回 unknown → 尝试 legacy RSA | 解密失败时显示「[加密消息]」 |

---

## 7. 与其他 ADR 的关系

- **02-protocol**：§3 ProtocolSuite legacy 解析矩阵、§5 协商顺序；
- **04-capability**：协商算法实现本矩阵的「交集」逻辑；
- **05-metadata**：双写期策略的详细规则；
- **08-threat-model**：T2（降级攻击）、T9（rollback）的防御体现在本矩阵。

---

## 8. 维护

本矩阵随客户端版本演进更新。新增协议套件时：
1. §2 套件表追加新行；
2. §1 客户端矩阵追加新列/行；
3. §5 升级路径补充迁移说明；
4. 若破坏现有兼容性，必须走 01 §5 superseding ADR 流程。
