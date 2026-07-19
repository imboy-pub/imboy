# ADR 06 — Device Trust

> **状态**：Architecture Freeze（Trust State / Verification / Safety Number 冻结；Cross-signing 仅接口位）
> **冻结日期**：2026-07-18
> **关联**：02-protocol / 03-device-identity / 04-capability-negotiation / 08-threat-model (T2, T8)
> **不可单方面变更**：Trust State 三态语义与状态机、Safety Number 算法、`CrossSigningService` 接口签名

---

## 1. 决策

**Device Trust 由三件事构成，缺一不可**：

| 维度 | 回答的问题 | 本 ADR 的产出 |
|---|---|---|
| **Trust State（数据）** | 我对这个对端设备信任到什么程度？ | 三态模型 `unverified / verified / revoked` + 状态机 + 持久化 |
| **Verification（流程）** | 用户如何主动确认一个设备是真的？ | 扫码验证 + 手动比对安全号码两条带外路径 |
| **Safety Number（指纹）** | 我用什么和对方口头核对？ | 对称、稳定、可读的指纹串算法（HMAC-SHA256 截断分组） |

与 ADR 03 §4.3 的诚实声明一致：**E2EE 无法在协议层消除 T2（Compromised Server MITM）**，只能让用户通过带外信道（Safety Number 比对）发现篡改。本 ADR 把这条「最后一公里」的形式、算法、UI 落点全部冻结——Trust State 不提供密码学保证，只把「用户是否做过带外验证」显式化、可视化、可审计。

**本轮不实现 Cross-signing**（仅 `CrossSigningService` 接口位，见 §6）。三件事足以独立闭环：用户能验证、能撤销、能在 UI 看见信任状态。

---

## 2. 范围声明

### 2.1 本轮做（in scope）

1. Trust State 三态模型 + 状态机（§3）；
2. Safety Number 算法（§4）；
3. Verification 流程：扫码 + 手动比对（§5）；
4. Trust State 与加密决策的关系（§7）——发送给 revoked 设备拒发、unverified 标记、verified 正常；
5. 服务端职责：`trust_state` 持久化、`trust_audit` 审计表、广播变更（§8）；
6. `CrossSigningService` 接口签名（§6），仅定义不实现。

### 2.2 本轮不做（out of scope）

| 不做项 | 留给谁 | 不做的原因 |
|---|---|---|
| **Cross-signing 实现**（Master / Self-signing / User-signing Key 三层体系） | 未来 ADR | 三层密钥生命周期复杂（生成/备份/轮换/吊销/恢复），本轮做会膨胀 |
| **已 verified 设备自动 cross-sign 新设备** | 同上 | 依赖 Cross-signing 落地 |
| **Safety Number 变更触发自动 revoke 旧 session** | 本轮仅 UI 告警 | 复用现有 TOFU 广播（`e2ee_device_key_changed`），不引入新的强制断联 |
| **群聊维度的 trust 状态汇总** | 后续 | 本轮只做 per-device trust，群信任展示是 UI 层派生 |
| **Safety Number 的 emoji 形态** | 可选迭代 | 本轮冻结数字段格式，emoji 作为可选渲染层（§4.4） |

---

## 3. Trust State 模型

### 3.1 三态定义

| 状态 | 语义 | 进入条件 | 加密决策影响（详见 §7） |
|---|---|---|---|
| `unverified` | 默认态。TOFU：服务端给的 identity 第一次见到，用户尚未验证 | 设备首次出现 / Safety Number 变更后回落 | 允许发送，UI 标记「未验证」 |
| `verified` | 用户已通过带外信道（扫码 / 比对安全号码）确认过当前 identity | Verification 成功（§5） | 正常发送，UI 显示 verified badge |
| `revoked` | 用户主动撤销信任（发现可疑 / 对方换设备后未验证）或设备被销毁 | 用户手动撤销 / `user_device.status ∈ {0,-1}` | **拒发**（或强制警告后发送，见 §7） |

### 3.2 状态转换

```
                    verify 成功 (§5)
        ┌──────────────────────────────────┐
        │                                  ▼
   ┌────────────┐  Safety Number 变更  ┌─────────┐
   │ unverified │ ───────────────────→ │ verified │
   └────────────┘                      └─────────┘
        │   ▲                               │
        │   │   撤销验证                     │
        │   └─────────────────────────────┐ │
        │                                 │ │
        │   用户撤销 / 设备销毁              │ │
        ▼       (任意态)                    ▼ ▼
                  ┌──────────┐
                  │ revoked  │
                  └──────────┘
                  不可直接回 unverified/verified
                  （须重新走 Verification 流程）
```

**合法转换清单**（状态机实现必须按此白名单校验，非法转换抛 `StateError`，对应守护测试 T-06-06）：

| from | to | 触发者 | 备注 |
|---|---|---|---|
| `unverified` | `verified` | 本机用户 | Verification 成功 |
| `verified` | `unverified` | 本机用户 | 用户主动取消验证（少见但合法） |
| `unverified` / `verified` | `revoked` | 本机用户 / 服务端（设备销毁） | 用户撤销或 status=-1 |
| `revoked` | `unverified` | **仅当 identity 键变更**（新一轮 TOFU） | revoked 非终态：对方换设备产生新 identity，回落为 unverified 重新开始 |

**非法转换**：`revoked → verified`（必须先经 `unverified` 再 Verification）；任何由 `CrossSigningService` 触发的状态变更本轮抛 `UnimplementedError`（§6）。

### 3.3 谁能改 trust state

| 角色 | 能改吗 | 说明 |
|---|---|---|
| 本机用户（设备 owner 操作对端设备信任） | ✅ | 唯一的本地写入源 |
| 对端用户本人 | ❌（直接） | trust state 是「我对你的信任」，由本机用户决定 |
| 已 verified 的本端设备 cross-sign 新设备 | ⚠️ 接口位 | 本轮不实现（§6），未来 `CrossSigningService.verifyNewDevice` |
| 服务端 | ❌ 改值，✅ 写审计 | 服务端**不能凭空写 trust_state**，只接收客户端的带签请求并广播（§8） |

### 3.4 持久化

trust state 同时存两份，互不信任。**本地为权威源**：加密决策（§7）只读本地值，服务端值仅用于跨设备同步与审计。

**客户端本地**（`trust_store`，参考结构）：

```json
{
  "peer_uid": 67890, "peer_device_id": "phone-abc", "peer_ed25519": "<base64>",
  "trust_state": "verified", "verified_at": 1721300000000,
  "verified_by": "qr_scan",                    // 'qr_scan' / 'manual_number' / 'cross_sign'(reserved)
  "first_seen_at": 1721000000000,
  "last_safety_number": "12345 67890 ..."      // 缓存，便于变更检测
}
```

**服务端侧的关键澄清**：`user_device.trust_state`（ADR 03 §3.1）反映**该设备 owner 自己声明的设备自身状态**（如销毁 → revoked），与「A 对 B 的信任」是两件事。「A 信任 B」这种关系矩阵**服务端不存储**，只存事件流（§8.2 `trust_audit`）。混在一起是常见误解，本 ADR 显式区分。

---

## 4. Safety Number（安全号码）算法

### 4.1 设计目标

| 属性 | 含义 | 如何保证 |
|---|---|---|
| **对称（symmetric）** | Alice 和 Bob 独立计算得到**完全相同**的串 | 输入按字典序排序，HMAC 输出与角色无关 |
| **稳定（stable）** | 同一对 `(uid, device_id, ed25519)` 永远得到同一串 | 纯函数，无随机量、无时间因子 |
| **可带外比对（comparable）** | 用户能在电话里念出来或扫码比对 | 截断到 60 位数字、分 12 段、可选 emoji |
| **篡改可见（tamper-evident）** | 任一输入字段变化导致结果**完全不同** | HMAC-SHA256 雪崩效应 |

### 4.2 输入与输出

```
输入：
  local  = (local_uid  : uint64,
            local_device_id  : string,
            local_ed25519    : bytes)   // 32 字节 Ed25519 公钥（裸字节，非 base64）
  remote = (remote_uid : uint64,
            remote_device_id : string,
            remote_ed25519   : bytes)

输出：
  "12345 67890 11111 22222 33333 44444 55555 66666 77777 88888 99999 00000"
   └────────────────── 60 位十进制，12 段 × 5 位，空格分隔 ──────────────────┘
```

### 4.3 算法（伪代码）

```text
function safetyNumber(local, remote):
    # 1. 规范化：version(1B) || uid(8B BE) || device_id_len(1B) || device_id(UTF-8) || ed25519(32B)
    a = encode(local);  b = encode(remote)
    # 2. 对称：按字节序排序，保证双方输入一致
    (first, second) = (min(a, b), max(a, b))
    # 3. HMAC-SHA256；0x00 作为 domain separator（Signal 兼容风格）
    mac = HMAC-SHA256(key=first, message=0x00 || second)
    # 4. 前 30 字节 → 大整数 mod 10^60 → 零填充 60 位
    acc = int.from_bytes(mac[0:30], 'big')  mod  10^60
    digits = zero_pad(acc, width=60)
    # 5. 分 12 段 × 5 位
    return " ".join(digits[i:i+5] for i in range(0, 60, 5))
```

**算法选型理由**：

- **HMAC 而非裸 SHA-256**：长度扩展攻击免疫；`key/message` 角色分离便于未来用 domain separator 混入版本号。
- **30 字节 → mod 10^60**：240 bit 远大于 log2(10^60) ≈ 199.3 bit，模运算偏差 < 2^-40 可忽略；对齐 Signal 60 位事实标准便于用户跨产品认知。
- **uid 纳入输入**：若只哈希 ed25519，攻陷服务端可把 Bob 的 ed25519 挂到 Charlie 的 uid 下，Alice 的 Safety Number 不变却以为在跟 Bob 说话。uid 纳入后挂载到不同 uid → SN 全变（防御 T2 子类，对应守护测试 T-06-03）。

### 4.4 Emoji 形态（可选，非冻结）

数字串可经查表映射到 12 个 emoji（参考 Matrix `Sas.generateEmoji`）。vodozemac 0.5.0 的 `Sas` 类已提供此能力，但**本轮不冻结 emoji 表**，UI 可选渲染。数字段格式是冻结项。

---

## 5. Verification 流程

两种带外路径，殊途同归：成功后双方 `trust_state` 置 `verified`。

### 5.1 扫码验证（QR）

**QR 内容**（本端展示供对端扫）：

```json
{
  "v": 1, "uid": 12345, "device_id": "phone-abc",
  "ed25519": "<base64>", "curve25519": "<base64>",
  "identity_blob": { /* ADR 03 §4.1 */ },
  "identity_signature": "<base64>",
  "sig": "<ed25519 over (v||uid||device_id||ed25519||curve25519)>"
}
```

**验证步骤**（扫描方执行）：

1. 解析 QR，取出对端 identity；
2. 用对端 `ed25519` 校验 `identity_signature` over `identity_blob`（与 ADR 03 §4.2 一致）+ 校验顶层 `sig`；
3. 任一失败 → 拒绝，UI 显示「二维码无效或被篡改」（防御 T8 / T4）；
4. 通过 → 调用 `TrustService.markVerified(peer_uid, peer_device_id, peer_ed25519, method='qr_scan')`；
5. 客户端发送一条协议消息给对端：「我已验证你」（携带本端 identity 供对端反向校验），对端收到后同样置 verified。

**为什么不让服务端中介**：verification 是纯客户端密码学（§8 与 ADR 02 §6 一致），服务端只透传「我验证了你」这条消息。

### 5.2 手动比对安全号码

适用于两人面对面或电话念数字：

1. 双方各自打开会话设置页，UI 显示**本地计算出的 Safety Number**（§4）；
2. 用户逐位核对两端显示的串是否一致；
3. 点击「我已确认匹配」→ 调用 `markVerified(..., method='manual_number')`；
4. 客户端发送「我已验证你」消息给对端，对端 UI 提示是否也确认（可单边验证：仅一方 verified 也合法）。

**单边 vs 双边**：本 ADR 允许**单边 verified**——Alice 可在 Bob 未操作时单方面把 Bob 标为 verified。这反映「信任是观察者的主观判断」，且双边强制会让 UX 复杂化。`trust_audit` 记录方向（who_verified_whom）。

### 5.3 验证成功后写入

成功后 `markVerified` 同时写：

| 字段 | 值 |
|---|---|
| `trust_state` | `verified` |
| `verified_at` | 当前时间戳 |
| `verified_by` | `'qr_scan'` / `'manual_number'`（未来：`'cross_sign'`） |
| `verified_method_ref` | null（本轮）；cross_sign 时指向签名链 id |

并向服务端发一条带签的 trust event（§8.2 审计 + 广播给本账号其他设备）。

---

## 6. CrossSigningService 接口位（本轮不实现）

### 6.1 为什么只留接口

Reviewer 明确要求降级：Cross-signing 需要 Master Key + Self-signing Key + User-signing Key 三层密钥（参考 Matrix），生命周期含生成、4S 备份、轮换、丢失恢复、撤销、签名链传播，本轮做会显著膨胀且依赖 ADR 07（Storage / 4S backup）。

本轮冻结接口形状，未来实现时**业务层零改动**——所有 trust 决策已通过 `TrustService` 抽象，Cross-signing 只是 `TrustService` 的一个**额外信任源**，不是新通道。

### 6.2 接口签名（冻结项，未来实现不得改签名）

```dart
/// Cross-signing 服务接口。本轮所有方法抛 UnimplementedError。
/// 未来落地后，业务层（TrustService / 加密发送门）调用方式不变。
abstract interface class CrossSigningService {
  /// 设备 owner 用已 verified 的本端设备对新设备签名。
  /// 调用后所有信任本账号的用户将自动信任新设备（无需逐人验证）。
  /// 本轮：throw UnimplementedError
  Future<CrossSignResult> verifyNewDevice({
    required String signerDeviceId,
    required String newDeviceId,
    required String newDeviceEd25519,
  });

  /// 撤销对本账号某设备的 cross-sign（设备失窃 / 转卖），
  /// 触发全网 trust_state 回落为 unverified 或 revoked。
  /// 本轮：throw UnimplementedError
  Future<void> revokeDevice({required String deviceId});

  /// 查询某设备是否被本账号 Master Key 链 cross-sign。
  /// 用于 TrustService 计算「复合信任」：direct_verified OR cross_signed。
  /// 本轮：return false（保守值，不自动信任）
  bool isCrossSigned({required String userId, required String deviceId});
}

/// 未来实现时承载的数据结构（本轮仅声明）。
class MasterKey       { final String publicKey; final String? privateKeyBackupRef; }
class SelfSigningKey  { final String publicKey; }
class UserSigningKey  { final String publicKey; }
class CrossSignResult { final bool ok; final String? signatureRef; }
```

### 6.3 「未来实现时业务层零改动」的验证标准

| 验证点 | 怎么测 |
|---|---|
| TrustService 不直接判断 cross-sign | `grep -rn 'isCrossSigned' imboyapp/lib/` 在本轮应**零命中**（仅接口定义处出现） |
| 加密发送门不感知 cross-sign | 发送逻辑（§7）只读 `TrustService.getTrustState()`，不读 `CrossSigningService` |
| Cross-signing 落地 PR 影响面 | 仅在 `TrustService` 内部追加 OR 分支（direct_verified `\|\|` cross_signed），不改发送链 |

---

## 7. Trust State 与加密决策的关系

发送消息时，对每个 `RecipientDevice`（ADR 02 §2.1）查 `TrustService.getTrustState(peer_uid, peer_device_id)`：

| trust_state | 发送行为 | UI 反馈 |
|---|---|---|
| `verified` | 正常发送 | 会话页显示 verified badge（参考 iMessage） |
| `unverified` | **允许发送**，但 UI 标记 | 会话顶部黄条「该联系人尚未验证，点击验证」；首次发送可弹一次确认（可配置） |
| `revoked` | **默认拒发**；用户在警告页二次确认后可强制发送（明文或仍加密） | 红色警告「该设备已被撤销，发送可能有风险」 |

**为什么 unverified 仍允许发送**：默认拒发会让 E2EE 体验对首次通信双方断裂——绝大多数用户不会先验证再聊天，会让 E2EE 沦为「安全人员玩具」。正确策略是：默认 TOFU + 显式 UI 标记 + 提供便捷验证入口（§5）。

**为什么 revoked 默认拒发而非静默拒**：revoked 是用户主动操作（撤销某设备）的结果，意图明确。静默拒会让用户以为消息发出去了；拒发 + 红色 UI 是对用户意图的最小误读。

### 7.1 trust state 变化时的 session 处理

| 变化 | Olm Session（per-device） | Megolm Session（per-room） |
|---|---|---|
| `unverified → verified` | 不动 | 不动 |
| `verified → unverified` | 不动（保留可解历史） | 不动 |
| `任意 → revoked` | **本轮不强制销毁**；可选 `OlmSessionStore.clear(peer_device_id)` | 不动（Megolm 是群域，撤销某设备不应影响群） |
| `revoked → unverified`（identity 键变更触发） | 旧 session 自然失效（identity 变了，旧 ratchet 解不开新消息） | 不动 |

**为什么 revoked 不强制销毁 Olm session**：销毁是单向的，用户「误撤销」后将永久丢消息。本轮保守策略：revoked 只影响发送决策（§7 表），不动已建立的 session；显式销毁留给未来的「彻底清除」入口（可能放在「设备管理」页）。

**与 Safety Number 变更 TOFU 告警的衔接**：现有 `e2ee_device_key_changed` 广播（`e2ee_logic.erl:107`）已让客户端弹「安全码已变更」（i18n `e2eePeerKeyChanged`）。本轮**不新增**强制断联行为，只把该告警从「被动提示」升级为「附带『立即验证 / 撤销该设备』两个动作」。

---

## 8. 服务端职责

### 8.1 不参与 verification 算法

服务端**完全不计算 Safety Number、不验证 QR 签名、不判断 verification 是否成功**。理由：与 ADR 02 §6 服务端零密码学原则一致；服务端参与的 verification 可被攻陷服务端（T2）伪造，失去带外意义；客户端是 verification 的唯一真相源。

### 8.2 存储与审计

服务端负责两件事：

**8.2.1 存储 `trust_state`**：`user_device.trust_state`（ADR 03 §3.1 已声明列）反映设备 owner 对该设备自身的状态声明（如销毁 → revoked），CHECK 约束 `chk_user_device_trust_state IN ('unverified','verified','revoked')`（ADR 03 §7.1）。

**8.2.2 审计表 `trust_audit`（本轮新增 migration）**：记录「谁信任谁、何时、何方法」的事件流，append-only。

```sql
-- 00000044_device_trust.up.sql
CREATE TABLE IF NOT EXISTS public.trust_audit (
    id              bigserial PRIMARY KEY,
    actor_uid       bigint      NOT NULL,        -- 谁做出的信任决策
    target_uid      bigint      NOT NULL,        -- 被信任的对端用户
    target_device_id varchar(128) NOT NULL,
    target_ed25519  text        NOT NULL,        -- 决策时的对端身份键快照
    from_state      varchar(20) NOT NULL,
    to_state        varchar(20) NOT NULL,
    method          varchar(40) NOT NULL,        -- 'qr_scan'/'manual_number'/'revoke'/'device_destroyed'
    actor_signature text        NOT NULL,        -- actor 的 ed25519 对 (target_*, from_state, to_state, ts) 的签名
    created_at      timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_trust_audit_target ON public.trust_audit (target_uid, target_device_id);
CREATE INDEX IF NOT EXISTS idx_trust_audit_actor  ON public.trust_audit (actor_uid);
COMMENT ON TABLE public.trust_audit IS '设备信任决策事件流（append-only）。算法见 ADR 06。';
```

**设计理由**：

- **事件流而非关系矩阵**：trust 双向独立、时变（Alice verified Bob ≠ Bob verified Alice），矩阵丢时间维度；事件流支持「该设备历史信任变更回放」，对 T2 / T8 事后追查至关重要。
- **要求 `actor_signature`**：防 T7（Malicious Client）伪造他人 trust 事件。服务端验签通过后才写库并广播。注意：签名验证是身份认证级密码学，**不涉及 E2EE payload 解密**，不违反 ADR 02 §6 零密码学原则（签名验证 ≠ 解密）。

### 8.3 广播 trust_state 变更

复用现有 TOFU 广播模式（`e2ee_logic.erl:107 notify_friends_key_changed`）：

| 事件 | 广播给谁 | Action 名 | 客户端响应 |
|---|---|---|---|
| `unverified → verified`（我标记了对端） | 本账号其他设备（多设备同步） | `e2ee_trust_changed`（新） | 本端其他设备同步 trust_state |
| `任意 → revoked` | 本账号其他设备 + 对端用户 | `e2ee_trust_changed`（新） | 对端 UI 提示「对方撤销了对你的信任」 |
| 对端设备 identity 变更（现有） | 所有好友 | `e2ee_device_key_changed`（复用） | 弹 TOFU 告警（现有 `e2eePeerKeyChanged` 文案），新增「立即验证 / 撤销」按钮 |

**广播 payload**（透传，服务端不解释）：`{actor_uid, target_uid, target_device_id, to_state, method, ts}`。

---

## 9. 守护测试要求

下列测试必须存在，CI 强制运行。缺失任一项视为本 ADR 未落地。测试命名遵循 ADR 08 §4 约定。

### 9.1 Safety Number（对应 ADR 08 矩阵 `e2ee_safety_number_test`）

| 测试 ID | 用例 | 通过条件 |
|---|---|---|
| T-06-01 | Alice 算 SN(local=Alice, remote=Bob)，Bob 算 SN(local=Bob, remote=Alice)，两者**字节相等** | §4.1 对称性 |
| T-06-02 | 同一对 `(uid, device_id, ed25519)` 在不同时间/设备上计算，结果**完全一致** | §4.1 稳定性 |
| T-06-03 | 把 Bob 的 ed25519 挂到 Charlie 的 uid 下，Alice 算出的 SN **必须与原 SN 不同** | §4.3 防御 T2 子类（uid 绑定） |
| T-06-04 | 任一输入字段（uid / device_id / ed25519）改 1 bit，输出串**完全不同**（雪崩） | HMAC-SHA256 性质 |

### 9.2 Trust State 状态机

| 测试 ID | 用例 | 通过条件 |
|---|---|---|
| T-06-05 | 合法转换全部成功：`unverified→verified`、`verified→unverified`、`任意→revoked`、`revoked→unverified`（identity 变更） | §3.2 白名单 |
| T-06-06 | 非法转换全部抛 `StateError`：`revoked→verified`（绕过 unverified）、无触发条件的状态跳变 | §3.2 非法清单 |
| T-06-07 | `CrossSigningService.verifyNewDevice` 本轮抛 `UnimplementedError` | §6 本轮不实现 |

### 9.3 加密决策门（§7）

| 测试 ID | 用例 | 通过条件 |
|---|---|---|
| T-06-08 | 发送给 `revoked` 设备：默认拒发抛 `E2eeSendRefusedException`；用户二次确认后可强制发送 | §7 revoked 行 |
| T-06-09 | 发送给 `unverified` 设备：允许发送，metadata 含 `trust_warning='unverified'` 标记 | §7 unverified 行 |
| T-06-10 | 发送给 `verified` 设备：正常发送，无警告标记 | §7 verified 行 |

### 9.4 服务端审计与广播

| 测试 ID | 用例 | 通过条件 |
|---|---|---|
| T-06-11 | `trust_audit` 表 append-only：`UPDATE / DELETE` 被权限拒（或触发器拦） | §8.2.2 不可变审计 |
| T-06-12 | 写入 trust event 时 `actor_signature` 验签失败 → 拒写 + 拒广播 | §8.2.2 防 T7 伪造 |
| T-06-13 | trust event 写入后，S2C `e2ee_trust_changed` 在 `target_uid` 与 `actor_uid` 其他设备上均收到 | §8.3 广播 |
| T-06-14 | `grep -rn "safetyNumber\|safety_number" imboy/src --include="*.erl"` **零命中** | §8.1 服务端零算法 |

---

## 10. 与其他 ADR 的关系

| ADR | 本 ADR 的依赖点 | 对方对本 ADR 的约束 |
|---|---|---|
| **02-protocol** | Trust 决策门（§7）作用在 `E2eeSessionProtocol.encrypt` 调用之前；发送链通过 `RecipientDevice` 间接消费 trust_state | 02 §6 服务端零密码学 → 本 ADR §8.1 服务端不参与 verification 算法 |
| **03-device-identity** | `user_device.trust_state` 列、CHECK 约束、`identity_signature` 校验（§5.1 扫码验证依赖） | 03 §7.1 已冻结 trust_state 列定义；本 ADR 不得改列定义，只填值语义 |
| **04-capability-negotiation** | unverified 设备仍参与 capability 协商（trust ≠ capability）；§7 决策门在协商之后、加密之前 | 04 协商结果不被 trust 改写（unverified 设备仍可发 olm） |
| **08-threat-model** | T2（Compromised Server MITM）→ Safety Number 带外验证（§4）；T8（Social Engineer）→ trust state 可视化（§7 UI） | 08 §4 矩阵的 `e2ee_safety_number_test` / `device_trust_state_change_audit_log_test` 由本 ADR §9 落地 |

**冲突仲裁**：本 ADR 冻结 Trust State 三态语义与状态机、Safety Number 算法（§4.3 伪代码）、`CrossSigningService` 接口签名（§6.2）三项为**不可单方面变更**冻结项。任何改动须新建 `NN-supersedes-06.md` 并走 ADR 01 §5 流程。

---

## 11. 决策摘要（一页速览）

| 决策点 | 选择 | 一句话理由 |
|---|---|---|
| Trust 三态 | `unverified / verified / revoked` | 覆盖 TOFU / 已验证 / 主动撤销，简洁可表达 |
| Safety Number 算法 | HMAC-SHA256(sorted(encode(local), encode(remote))) 截断 60 位 12 段 | 对称、稳定、可带外比对、对齐 Signal |
| Verification 路径 | 扫码（QR）+ 手动比对，**纯客户端** | 服务端参与即失去带外意义 |
| Cross-signing | 仅 `CrossSigningService` 接口位，本轮抛 `UnimplementedError` | 三层密钥生命周期复杂，本轮不膨胀 |
| 客户端本地 vs 服务端 | 本地权威，服务端审计 + 广播 | trust 是观察者主观判断，本地为源 |
| revoked 发送策略 | 默认拒发 + 用户二次确认可强发 | 尊重用户主动撤销意图，不静默 |
| unverified 发送策略 | 允许 + UI 标记 | 避免 E2EE 沦为「先验证才能聊」的玩具 |
| 服务端算法参与 | 零（不计算 SN、不验 QR、不判 verification） | 服务端零密码学（ADR 02 §6）+ 防 T2 伪造 |
| 审计存储 | `trust_audit` append-only 事件流 + actor 签名 | 支持 T2/T8 事后追查 + 防 T7 伪造 |
