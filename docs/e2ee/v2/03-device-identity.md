# ADR 03 — Device Identity

> **状态**：Architecture Freeze
> **冻结日期**：2026-07-18
> **关联**：ADR 02(protocol) / 04(capability-negotiation) / 06(device-trust) / 08(threat-model)
> **不可单方面变更**：`user_device` 的 identity 相关列、`olm_identity` 表结构、签名覆盖范围

---

## 1. 决策

**Device Identity 是一等公民（first-class citizen）的数据模型，不是 `user_device` 上加一个 `trust_state` 字段就完事。**

一个 Device 实体由三类字段组成：
1. **设备核心属性**（`user_device` 表）：谁、什么设备、何时登录、是否活跃；
2. **协议身份键**（按协议分表，v2 起为 `olm_identity`）：Ed25519 签名键 + Curve25519 身份键 + 自签名；
3. **派生属性**（derived）：Safety Number、capability 交集、trust_state 计算结果——不落库或仅缓存。

本 ADR 冻结上述三类字段的存储位置、签名语义、生命周期。任何「在 `user_device` 上加密码学列」或「把 Olm 身份键塞回 `user_device`」的改动均需走 §5 的 supersedes 流程。

---

## 2. 身份键架构

### 2.1 三代身份键共存

| 代 | 协议 | 签名/加密键 | 存储位置 | 状态 |
|---|---|---|---|---|
| **v1** | RSA-OAEP + AES-256-GCM | RSA 公钥（PEM） | `user_device.public_key` / `key_id` | 兼容保留，新设备不再上报 |
| **v2** | Olm (X3DH + Double Ratchet) | Ed25519 签名键 + Curve25519 身份键 | `olm_identity` 表 | **本轮冻结** |
| **未来** | MLS | MLS credential（signature key + credential） | `mls_identity` 表（待建） | 仅占位，ADR 02 Registry 注册 |

### 2.2 关键决策：Olm 身份键独立成表，不合并进 `user_device`

**决定**：保留 `olm_identity` 为独立表（migration 00000042 已建），`user_device` 只扩展非密码学的派生列（capabilities / trust_state / 签名 blob）。理由：

| # | 理由 |
|---|---|
| 1 协议可插拔 | 身份键 *per-protocol*；合并意味着每加协议（MLS）就给 `user_device` 加列，列爆炸 |
| 2 生命周期解耦 | 身份键轮换与 `login_count`/`last_active_at` 不同频次，混一行导致无谓行锁与死写 |
| 3 零回归风险 | v1 RSA 列已有线上数据，合并要 `ALTER TABLE` 高频更新表；独立表是新增，存量零影响 |
| 4 表已落地 | 00000042 已建表带注释，合并是反向操作，收益不抵成本 |

**为什么不另一种方案（合并进 `user_device`）**：单表 JOIN 省一次查询，但上述四点全部丧失，且与 ADR 02「协议可插拔」直接冲突。

---

## 3. Device 数据模型

完整字段清单，按存储位置分组。`existing` = 已存在，`new` = migration 00000043 新增，`derived` = 不落库。

### 3.1 `user_device`（设备核心实体，1 行 / 物理设备登录）

| 字段 | 类型 | 来源 | 语义 |
|---|---|---|---|
| `id` | bigint PK | existing | 主键 |
| `user_id` | bigint | existing | 所属用户 |
| `device_type` | varchar(40) | existing | 扩展取值见 §7 |
| `device_id` | varchar(128) | existing（宽度对齐见 §7） | 设备唯一标识，与 `olm_identity.device_id` 同值 |
| `device_vsn` | text | existing | 客户端版本元数据 |
| `device_name` | varchar(80) | existing | 用户可修改的显示名 |
| `login_count` / `last_login_ip` / `last_login_at` / `last_active_at` | — | existing | 登录/活跃审计 |
| `status` | smallint | existing | 1=活跃 / 0=禁用 / -1=删除 |
| `created_at` | timestamptz | existing | 首次注册时间 |
| `public_key` / `key_id` | varchar(2048) / varchar(255) | existing | **v1 RSA 设备公钥**，兼容保留 |
| `capabilities` | text[] | **new** | 支持的协议套件，见 §5 |
| `trust_state` | varchar(20) | **new** | `unverified` / `verified` / `revoked`，状态机见 ADR 06 |
| `identity_blob` | jsonb | **new** | 规范化的待签名设备身份负载，见 §4 |
| `identity_signature` | text | **new** | 对 `identity_blob` 的 Ed25519 签名（base64） |
| `identity_signed_at` | timestamptz | **new** | 签名时间戳，用于轮换审计 |

### 3.2 `olm_identity`（Olm 协议身份键，1:1 与 user_device）

| 字段 | 类型 | 语义 |
|---|---|---|
| `id` | bigint PK | 主键 |
| `user_id` + `device_id` | bigint + varchar(128) | UNIQUE，逻辑外键指向 `user_device` |
| `ed25519_key` | text | Ed25519 签名公钥（base64），**验证 `identity_signature` 的钥匙** |
| `curve25519_key` | text | Curve25519 身份公钥（base64），X3DH 用 |
| `signature` | text | libolm 内置签名：ed25519 对 curve25519_key 的签名 |
| `created_at` / `updated_at` | timestamptz | 键的生命周期 |

### 3.3 Derived（不落库，运行时计算）

| 属性 | 计算方式 | 引用 |
|---|---|---|
| Safety Number | `HMAC-SHA256(sort([my_ed25519, peer_ed25519]))` 截断 | ADR 06 |
| Capability 交集 | 两端 `capabilities` 数组的 `∩` | ADR 04 |
| 设备可用性 | `status=1 AND trust_state≠'revoked' AND has olm_identity` | 运行时 |

---

## 4. Signing Key 语义

### 4.1 签名覆盖范围（两道签名，职责分离）

| 签名 | 签名键 | 覆盖内容 | 来源 |
|---|---|---|---|
| **libolm 内置签名**（`olm_identity.signature`） | 设备 Ed25519 私钥 | 仅 `curve25519_key` | libolm 标准，X3DH 校验用 |
| **IMBoy 扩展签名**（`user_device.identity_signature`） | 设备 Ed25519 私钥（同上） | `identity_blob`（见下） | **本 ADR 新增**，绑定键与设备元数据 |

`identity_blob` 规范化 JSON（键名字典序，序列化后 UTF-8 字节流签名）：

```json
{
  "capabilities": ["olm", "megolm"],
  "curve25519_key": "<base64>",
  "device_id": "abc-123",
  "device_type": "phone",
  "timestamp": 1721300000,
  "user_id": 12345
}
```

### 4.2 为什么需要扩展签名（libolm 内置签名不够）

libolm 内置签名仅证明「Ed25519 私钥持有者签了 Curve25519 公钥」，**不绑定 user_id / device_id**，因此：
- 攻陷服务端（T2）可把 A 用户的 curve25519_key 挂到 B 用户的 device_id 下，客户端无法发现；
- 网络 MITM（T4）可重放同一身份键到不同设备记录。

扩展签名把 `user_id` / `device_id` / `capabilities` / `timestamp` 一并纳入签名负载；客户端拉取对端设备列表时**必须先用 ed25519_key 校验 identity_signature**，任一字段被篡改即验签失败。

### 4.3 防御映射

| 威胁（ADR 08） | 防御点 |
|---|---|
| T2 Compromised Server 篡改 curve25519_key | 扩展签名校验失败 |
| T2 把 key 挪到别的 device_id | 签名负载含 device_id，篡改即败 |
| T4 Network MITM 重放 | 签名负载含 timestamp + user_id |
| T7 Malicious Client 伪造身份 | 私钥不上传，无 ed25519 私钥无法产出合法签名 |
| T9 Rollback（旧身份键回滚） | 客户端记录对每设备的 `highest_seen_identity_signed_at`；服务端返回更早签名时间的身份即拒收并 TOFU 告警（与 ADR 05 message counter、ADR 02 ProtocolSuite.version 共同构成单调度） |

**诚实声明（承接 ADR 08 §T2）**：扩展签名不能防御「服务端从零伪造一整套 ed25519/curve25519 + 签名」——这是 E2EE 的根本限制，最终由 Safety Number 带外验证（ADR 06）兜底。

**T9 残留风险**：首次见到某设备的客户端无历史签名时间可比对（TOFU 窗口），此窗口内的 rollback 无法检测。

---

## 5. Capabilities 字段

### 5.1 取值与上报

`user_device.capabilities` 是 `text[]`，取值来自 ADR 02 注册的套件短名：

| 值 | 含义 |
|---|---|
| `"olm"` | 支持 Olm 双棘轮单聊（含 X3DH） |
| `"megolm"` | 支持 Megolm 群聊 |
| `"rsa-oaep"` | 仅 v1 RSA 套件（老客户端） |
| `"mls"` | MLS（未来，Registry 占位） |

**上报时机**：客户端在身份键上报接口（首登或轮换）随 `identity_blob` 一并提交，服务端写入 `capabilities` 并纳入 `identity_signature` 签名负载。

### 5.2 查询与衔接 ADR 04

拉取对端设备列表时，每条设备记录返回 `capabilities` 数组；客户端按 ADR 04 的协商算法求交集，决定每台对端设备走哪个套件（如对端某设备 `capabilities=["megolm"]`，则单聊消息不必给该设备建 Olm 会话）。

**约束**：服务端**不解释** capabilities 语义，仅作存储与透传——协商纯客户端逻辑（ADR 02 服务端零密码学原则）。

---

## 6. Device 生命周期

状态机（节点为 `user_device.status` × 是否有 `olm_identity` 行）：

```
[未注册]
    │ 首次登录（login）
    ▼
[registered·active]  status=1，无 olm_identity，public_key 可能为 v1 RSA
    │ 客户端生成 Olm 身份键并上报
    ▼
[provisioned·active]  status=1，olm_identity 1 行，capabilities 已签
    │ （正常活跃，last_active_at 周期更新）
    ▼
[provisioned·active]  ← 稳态
    │ 退出登录 / 换设备 / 远程踢出
    ▼
[revoked]  status ∈ {0, -1}，olm_identity 被删，trust_state='revoked'
```

### 6.1 各转换的身份键处理

| 转换 | `user_device` | `olm_identity` | `olm_one_time_key` / `olm_fallback_key` | 审计 |
|---|---|---|---|---|
| 首次登录 | INSERT，status=1，capabilities 默认 `{}` | 不动 | 不动 | 注册事件 |
| 身份键上报 | UPDATE capabilities/identity_blob/identity_signature/identity_signed_at | UPSERT 1 行 | 客户端同步批量上传 OTK | provision 事件 |
| 活跃心跳 | UPDATE last_active_at | 不动 | 不动 | — |
| 退出登录 / 换设备 | UPDATE status=-1, trust_state='revoked' | **DELETE** | **DELETE** | revoke 事件（保留 ed25519 指纹快照入审计日志，供 Safety Number 历史） |
| 远程踢出（T5 防御） | 同上 | 同上 | 同上 | 同上 |

**为什么销毁时硬删 `olm_identity` 而非软删**：身份键若无对应 OTK 已无法建新会话，保留只会让对端客户端误以为设备仍可用、徒增无效 claim；ed25519 指纹另行写入不可变审计表即可保留 Safety Number 历史。

---

## 7. 表结构决策（migration 00000043）

### 7.1 DDL

```sql
-- 00000043_device_identity.up.sql
-- ADR 03：Device Identity 一等公民化（列语义见 §3）

-- 1. 新增列
ALTER TABLE public.user_device
    ADD COLUMN IF NOT EXISTS capabilities     text[]     NOT NULL DEFAULT '{}',
    ADD COLUMN IF NOT EXISTS trust_state      varchar(20) NOT NULL DEFAULT 'unverified',
    ADD COLUMN IF NOT EXISTS identity_blob    jsonb,
    ADD COLUMN IF NOT EXISTS identity_signature text,
    ADD COLUMN IF NOT EXISTS identity_signed_at timestamptz;

-- 2. trust_state 状态约束（状态机见 ADR 06）
ALTER TABLE public.user_device
    DROP CONSTRAINT IF EXISTS chk_user_device_trust_state,
    ADD  CONSTRAINT chk_user_device_trust_state
         CHECK (trust_state IN ('unverified','verified','revoked'));

-- 3. device_type 扩展取值（phone/ipad/desktop/watch 等 5+ 设备场景）
ALTER TABLE public.user_device
    DROP CONSTRAINT IF EXISTS chk_user_device_device_type,
    ADD  CONSTRAINT chk_user_device_device_type CHECK (
        device_type IN ('','web','phone','tablet','ipad','desktop','watch',
                        'ios','android','macos','windows','linux')
    );

-- 4. 对齐 device_id 宽度：40 → 128（与 olm_identity.device_id 一致，varchar 增宽无重写）
ALTER TABLE public.user_device
    ALTER COLUMN device_id TYPE varchar(128);

-- 5. 索引
CREATE INDEX IF NOT EXISTS idx_user_device_uid_active
    ON public.user_device (user_id) WHERE status = 1;
CREATE INDEX IF NOT EXISTS idx_user_device_capabilities
    ON public.user_device USING gin (capabilities);

COMMENT ON COLUMN public.user_device.capabilities       IS '设备支持的协议套件短名数组（olm/megolm/rsa-oaep/mls）';
COMMENT ON COLUMN public.user_device.trust_state        IS '设备信任态，状态机见 ADR 06';
COMMENT ON COLUMN public.user_device.identity_blob      IS '规范化的待签名设备身份负载（jsonb）';
COMMENT ON COLUMN public.user_device.identity_signature IS '对 identity_blob 的 Ed25519 签名（base64）';
COMMENT ON COLUMN public.user_device.identity_signed_at IS '签名时间戳，用于轮换审计';
```

### 7.2 索引设计理由

| 索引 | 服务的查询 | 为什么不用另一种 |
|---|---|---|
| `idx_user_device_uid_active`（user_id where status=1） | 拉取对端全部活跃设备 | 部分索引比全列索引小；不用 `idx_user_device_uid`（已存在但含已删设备，过滤在应用层） |
| `idx_user_device_capabilities`（GIN） | 「找支持 megolm 的设备」 | GIN 支持 `capabilities @> ARRAY['megolm']`；btree 不支持数组包含 |
| `olm_identity` 现有 `idx_olm_identity_user_id` + UNIQUE(user_id, device_id) | 按 user_id 拉身份键、按 (user_id,device_id) UPSERT | 已足够，不再加 |

### 7.3 为什么不加 FK `olm_identity → user_device`

`user_device` 销毁走软删（status=-1）而非 DELETE，硬 FK 会阻止应用层对 `olm_identity` 的清理顺序，且 FK 在分区/分片演进时是阻碍。**一致性由应用层在事务内保证**：revoke 操作在同一事务里 `UPDATE user_device ... ; DELETE FROM olm_identity ... ; DELETE FROM olm_one_time_key ... ;`。

### 7.4 为什么不另建 `device_identity` 视图表

有人或主张建一个 `device_identity` 视图 `JOIN user_device + olm_identity` 统一对外。拒绝：视图把密码学字段（olm 签名）与审计字段（login_count）耦合给调用方，破坏 §2.2 的协议可插拔边界；客户端 API 由专门 endpoint 组装（见 §8），不靠 DB 视图。

---

## 8. 多设备场景

一个用户 N 个设备（phone / ipad / desktop / watch / web 五设备场景），identity 组织为：1 行 `user_device` × N，每行挂 0..1 行 `olm_identity`。

### 8.1 对端设备列表 API

`GET /api/e2ee/devices/{peer_user_id}` 返回（仅 `status=1` 且有 `olm_identity` 的设备）：

```json
{
  "user_id": 67890,
  "devices": [
    {
      "device_id": "phone-abc",
      "device_type": "phone",
      "capabilities": ["olm","megolm"],
      "ed25519_key": "<base64>",
      "curve25519_key": "<base64>",
      "identity_blob": { /* §4.1 */ },
      "identity_signature": "<base64>",
      "trust_state": "unverified"
    }
  ]
}
```

### 8.2 客户端消费流程

1. 对每条设备，用 `ed25519_key` 校验 `identity_signature` over `identity_blob`；失败即丢弃并上报篡改告警（对应 §9 守护测试）；
2. 按 ADR 04 求本地 capabilities 与对端 `capabilities` 的交集，决定该设备是否建 Olm 会话；
3. 对需要的设备，按 ADR 08 §T7 原子 claim OTK（原子语义见 §8.3：`FOR UPDATE SKIP LOCKED + UPDATE status='claimed'`，migration 00000042 建表 + 00000045 补审计列）。

### 8.3 OTK claim 语义：DELETE → UPDATE（审计）

**背景**：migration 00000042 的原始 claim 语义是「即删」——`SELECT ... FOR UPDATE SKIP LOCKED + DELETE` 原子消费。B.3 后端落地时改为「UPDATE 审计」：migration 00000045 给 `olm_one_time_key` 补 `status`（`available`/`claimed`）、`consumed_at`、`claimed_by` 三列，claim 改为把选中行 `UPDATE SET status='claimed', consumed_at=now(), claimed_by=<领取方 uid>`，不删除。

**为什么从 DELETE 改为 UPDATE**：

1. **可审计**：保留消费痕迹（谁、何时领走了哪个 OTK），供 Safety Number 争议追查与滥用检测；DELETE 后无据可查。
2. **低水位统计口径统一**：`count_one_time_keys` 只数 `status='available'`，与 claim 的 `available` 过滤同源，剩余量语义一致。
3. **原子性与并发安全不变**：仍是单条 CTE 语句（`picked FOR UPDATE SKIP LOCKED` → `claimed UPDATE` → `JOIN` 返回），PostgreSQL 单语句即隐式原子事务；100 并发 claim 走 SKIP LOCKED 各取不同行，不重复领取（§9 守护测试 T7 不变）。
4. **代价**：已消费行需定期清理避免表膨胀，由 `cleanup_consumed_one_time_keys/1` 按 `consumed_at + 保留期` 删除（消费侧无 worker 时可手动/运维调度调用；不阻塞 claim 主链）。

**销毁联动**：revoke 设备时 OTK 行仍整表 DELETE（见 §5 表 / §9 T5），审计列只作用于「正常 claim」路径，不改变销毁语义。

**已知限制 — key_id 唯一约束 vs. 保留审计行**（已接受）：

`olm_one_time_key` 有 `UNIQUE(user_id, device_id, key_id)`。补传 OTK（`upsert_one_time_keys`）走 `DELETE status='available' + INSERT`，只删可用行、保留 `claimed` 审计行。若**设备重置后 olm 账号重新生成并复用了旧 key_id**，INSERT 会撞上仍保留的 `claimed` 行 → `batch_insert_failed`（对客户端表现为 `internal_error`），该设备在保留期内无法补传这些 key_id。

- **触发面极窄**：libolm 的 one-time key_id 在单个 olm 账号生命周期内单调递增、从不复用；仅「设备重置 + 同 key_id + 仍在保留期内」三条件同时满足才命中。
- **失败可见非静默**：是响亮报错，不是静默丢数据；`cleanup` 到期清走 `claimed` 行后自愈。
- **不采用的替代方案**：`INSERT ... ON CONFLICT DO NOTHING`（静默跳过 → 客户端误以为已补传，低水位计数失真）、`DO UPDATE`（复活 claimed 行 → 覆盖审计痕迹）、DELETE 连 claimed 一起删（销毁审计目的）——三者弊均大于该窄场景的收益，故**接受此限制**，不加补偿逻辑。

---

## 9. 守护测试要求

| 测试 | 防御的威胁 | 通过条件 |
|---|---|---|
| **身份键篡改检测** | T2, T4 | DB 中改 `curve25519_key` 或 `identity_blob.device_id` 后，客户端校验 `identity_signature` 必失败 |
| **签名验证单测** | T2, T7 | 规范化 blob → 签名 → 验签往返；篡改任一字段、重放旧 timestamp 均败 |
| **capabilities 查询** | T7 | `/e2ee/devices/{uid}` 返回的 `capabilities` 与入库一致；`@> 'megolm'` 过滤正确 |
| **OTK 原子 claim 并发** | T7 | 100 并发 claim，SKIP LOCKED 各取不同可用行，无重复领取；耗尽后拿 fallback 或失败（§8.3：SKIP LOCKED + UPDATE 语义） |
| **销毁联动** | T5 | revoke 后 `olm_identity` / OTK / fallback 同事务清空，`user_device.status=-1`，trust_state='revoked' |
| **私钥零落库** | T3 | `grep` DB schema 中无私钥列（仅 `public_key` / `ed25519_key` / `curve25519_key` / `signature` 公钥侧） |

---

## 10. 与其他 ADR 的关系

| ADR | 本 ADR 的依赖点 |
|---|---|
| **02 protocol** | capabilities 取值来自 Protocol Registry 注册的套件短名；服务端不解释 capabilities |
| **04 capability-negotiation** | `capabilities` 字段的消费方；本 ADR 只定义存储与上报 |
| **06 device-trust** | `trust_state` 列存本表，状态机 / Safety Number / 验证流程由 06 定义 |
| **08 threat-model** | §4.3 / §9 的每个防御点可追溯到 T2 / T4 / T7 / T3 |

**冲突处理**：本 ADR 冻结 `user_device` identity 相关列与 `olm_identity` 结构（见 ADR 01 §5 不可单方面变更清单）。任何改动须新建 `NN-supersedes-03.md`。
