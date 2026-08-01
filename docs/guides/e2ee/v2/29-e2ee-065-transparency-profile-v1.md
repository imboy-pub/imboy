# 29 — IMBoy Transparency Profile v1（冻结草案）

> **状态**：**Accepted**（2026-08-02 leeyi solo 一人决策接受；原 2026-07-29 冻结草案）。playbook E2EE-033 第 1 步要求
> 「先冻结 transparency profile … **由安全 reviewer 接受**」——
> loop 只能产出草案，**接受动作必须人工**。
> **本文件不改动任何生产代码。**
> **上位约束**：playbook E2EE-033、ADR 16 §5/§6（**仍为 Proposed**）
> **对应切片**：`28-e2ee-065-066-key-transparency-research.md` §4 **Slice 2**
> **执笔**：Claude Code loop，会话 `20260729-0800-claude-code`，2026-07-29

playbook 硬性要求：**profile 内不得有 `TBD`**。本文件所有条目均给出确定值，
并附**跨实现 golden vector**（Erlang + Python 独立复算逐字节一致，见 §8）。

---

## 1. 冻结项一览

| # | 冻结项 | 取值 |
|---|---|---|
| 1 | Hash | **SHA-256** |
| 2 | Leaf domain separation | 前缀字节 **`0x00`** |
| 3 | Node domain separation | 前缀字节 **`0x01`** |
| 4 | Tree-head signing domain separation | 前缀字节 **`0x02`** |
| 5 | 空树根 | `SHA-256(<空字节串>)` |
| 6 | Canonical event bytes | `key=value\n`，ASCII 字典序，**末字段无尾随换行**（复用既有方案，见 §3） |
| 7 | Tree-head 签名输入 | `SHA-256(0x02 ‖ canonical_head_bytes)`，Ed25519 签之（见 §5） |
| 8 | Proof wire | JSON，hash 一律 **小写 hex**（见 §6） |
| 9 | Signing key 轮换 | 双签过渡窗口，见 §7 |

选择 SHA-256 而非 SHA-3/BLAKE3 的理由：**项目内已到处是 SHA-256**
（`file_hash256`、Dart `sha256.convert`、Erlang `crypto:hash(sha256, _)`），
引入第二种摘要算法等于多一处跨实现漂移面，收益为零。

---

## 2. 树结构（RFC 6962 Merkle Tree Hash）

```
MTH({})      = SHA-256(<<>>)
MTH({d0})    = SHA-256(0x00 ‖ d0)
MTH(D[0:n])  = SHA-256(0x01 ‖ MTH(D[0:k]) ‖ MTH(D[k:n]))
               k = 小于 n 的最大 2 的幂
```

**为什么必须有 domain separation**：若 leaf 与 node 共用同一 hash 前缀，
攻击者可把一个内部节点的两个子哈希拼成一条「事件」，使
`leaf(x) == node(a,b)`，从而对同一 root 构造出两棵不同的树
（second-preimage）。`0x00` / `0x01` 前缀消除该歧义。

`0x02` 用于 tree head，防止一条 tree-head 签名被当作 leaf 数据复用。

---

## 3. Canonical event bytes（**复用既有方案，不发明第三套**）

格式与现有 trust event 完全一致：

- 每字段一行 `key=value`，行分隔符 **`\n`**；
- key 按 **ASCII 字典序**排列；
- **末字段无尾随换行**；
- 整数以十进制渲染（对齐 `e2ee_trust_logic:i2b/1`）；
- **fail-closed**：任一 value 含 `\n` 或 `\r` 即拒绝编码。

最后一条不是可选的：`key=value\n` 的分隔符唯一，value 内含换行会让编码
**非单射**——同一串字节可对应多组字段拆分，等价于签名伪造。
现有实现两侧都已带该守卫（Dart `trust_event_canonical.dart` 的 `_rejectNewline`、
Erlang 侧同样拒收），KT 必须照搬。

### 3.1 identity_log 事件字段集（冻结）

| key | 说明 |
|---|---|
| `curve25519_key` | 发布的 curve25519 公钥（base64）；`revoke` 事件为空串 |
| `device_id` | 设备标识 |
| `ed25519_key` | 发布的 ed25519 公钥（base64）；`revoke` 事件为空串 |
| `event_type` | `publish` \| `rotate` \| `revoke` |
| `user_id` | 账号 uid（十进制） |

上表**已是 ASCII 字典序**。字段集变更须走 supersedes 流程。

> ⚠️ 与 `trust_audit` 的字段集**刻意不同**：那条流记录「谁信任谁」（关系），
> 这条流记录「账号发布了哪些键」（目录）。混用会让从未被信任过的设备
> 不进日志——正是攻击者最想插入的那种。见调研文档 §1.3。

---

## 4. Leaf 与 Node

```
leaf_hash(event) = SHA-256(0x00 ‖ canonical_event_bytes(event))
node_hash(l, r)  = SHA-256(0x01 ‖ l ‖ r)
```

`l`、`r` 是 **32 字节原始摘要**，不是 hex 字符串。

---

## 5. Signed Tree Head

`canonical_head_bytes` 同样用 §3 的 `key=value\n` 方案，字段集（已按字典序）：

| key | 说明 |
|---|---|
| `domain` | 固定字面量 `imboy.kt.v1.tree_head` |
| `log_id` | 日志标识，固定 `imboy-identity-log` |
| `root_hash` | root 的**小写 hex**（64 字符） |
| `timestamp_ms` | 签发时刻（epoch ms，十进制） |
| `tree_size` | 叶子数（十进制） |

签名输入与签名：

```
signing_input = SHA-256(0x02 ‖ canonical_head_bytes)
signature     = Ed25519(signing_input)
```

`domain` 是**显式**冻结字段，不依赖「d 恰好排在最前」这种字母序巧合。

---

## 6. Proof wire 格式（冻结）

所有 hash 字段一律 **小写 hex**，不使用 base64（与 `root_hash` 一致，避免同一
文档里两种编码）。

**Inclusion proof**

```json
{
  "leaf_index": 2,
  "tree_size": 3,
  "audit_path": ["<hex>", "..."],
  "root_hash": "<hex>"
}
```

**Consistency proof**

```json
{
  "first_size": 2,
  "second_size": 3,
  "consistency_path": ["<hex>", "..."],
  "first_root": "<hex>",
  "second_root": "<hex>"
}
```

**Signed tree head**

```json
{
  "log_id": "imboy-identity-log",
  "tree_size": 3,
  "timestamp_ms": 1753747200000,
  "root_hash": "<hex>",
  "signature": "<base64>",
  "key_id": "<签名公钥指纹，小写 hex>"
}
```

`key_id` 是轮换的必需品（§7）：验证方要能在过渡窗口内选对公钥。

---

## 7. Signing key 轮换（冻结）

| 规则 | 内容 |
|---|---|
| 私钥存放 | **不在 DB、repo、日志、API**。环境变量或外部 KMS，与 log DB 写权限**分离** |
| 过渡形态 | **双签**：过渡窗口内同一 tree head 同时以旧、新 key 各签一份，`key_id` 区分 |
| 客户端行为 | 接受**任一**在有效期内的 key_id 的签名；两把都过期 → **fail-closed 拒绝**，不得放行 |
| 回滚边界 | 过渡窗口结束前可回退到只用旧 key；窗口结束、旧 key 撤销后**不可回滚** |
| 撤销 | 撤销后旧 key 签发的 tree head 一律不接受，即便时间戳在有效期内 |

⚠️ 「两把都过期 → fail-closed」是刻意的：签名 key 过期是运维失误，
但**放行未验证的 tree head 等于让整个透明度机制静默失效**——
与 E2EE-062 第七刀 `rate_not_set` 的处置方向相反，因为那里放行只是「限流暂时失效」，
这里放行是「透明度完全失效」。

---

## 8. Golden vectors（跨实现已核验）

以下值由 **Erlang（`crypto:hash/2`）与 Python（`hashlib`）两套独立实现分别计算，
逐字节一致**。复算命令见 §8.3。

### 8.1 基础

| 项 | 值 |
|---|---|
| 空树根 `MTH({})` | `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855` |

> 该值等于公认的 `SHA-256("")`，可作为**独立自校验**：若某实现算出别的值，
> 它连标准 SHA-256 都不对，后续所有向量都不必看。

### 8.2 事件与树

三条测试事件（canonical bytes，`↵` 表示 `\n`，末行无换行）：

```
E1: curve25519_key=Y3VydmUx↵device_id=dev-A↵ed25519_key=ZWQyNTUxOTE=↵event_type=publish↵user_id=1001
E2: curve25519_key=Y3VydmUy↵device_id=dev-B↵ed25519_key=ZWQyNTUxOTI=↵event_type=publish↵user_id=1001
E3: curve25519_key=↵device_id=dev-A↵ed25519_key=↵event_type=revoke↵user_id=1001
```

`E1` 的 canonical bytes 长度 = **96 字节**（长度也是向量的一部分：
长度对不上说明编码规则理解错了，不必再比 hash）。

| 项 | 值 |
|---|---|
| `leaf_hash(E1)` | `de22f9f514db9c0faa1c57e53668678cfbf93f3b166eafd1212e254368e602e1` |
| `leaf_hash(E2)` | `d067ead2437f484a7413470f3a1b68c2cc4be8b5f2ec42ae7e7943a0ae33d012` |
| `leaf_hash(E3)` | `f571dce4078d4163a60dfeb441c23b09e1fefcdb862efecf4c8ebc7ccc2c6cab` |
| `MTH([E1])` | `de22f9f514db9c0faa1c57e53668678cfbf93f3b166eafd1212e254368e602e1` |
| `MTH([E1,E2])` | `bbd5b8a61334085b836b15c8aa421104b42d906b6bc8bd91da2b320a13a77ae0` |
| `MTH([E1,E2,E3])` | `6beeef5d57749b14c1f9d4b090ebcc0eaa35422a7b19bdde36863dc8e3acb962` |

`MTH([E1]) == leaf_hash(E1)` 是 RFC 6962 的定义要求，此处成立——
第二重自校验。

**n=3 是刻意选的**：它是最小的非平衡情形（k=2，左子树 2 叶、右子树 1 叶）。
只用 n=1/2/4 的向量无法区分「实现了 RFC 6962 的分裂规则」与
「实现了朴素的两两配对」。

### 8.3 Tree head

```
canonical_head_bytes（168 字节）：
domain=imboy.kt.v1.tree_head↵
log_id=imboy-identity-log↵
root_hash=6beeef5d57749b14c1f9d4b090ebcc0eaa35422a7b19bdde36863dc8e3acb962↵
timestamp_ms=1753747200000↵
tree_size=3
```

| 项 | 值 |
|---|---|
| `SHA-256(0x02 ‖ canonical_head_bytes)` | `34760542818964fc8f23ad1a09dca6c5a9d4388561cfa87ae8110e9c04cd1f3d` |

（此处只到签名输入为止。Ed25519 签名值取决于 signing key，不属 profile 冻结项。）

### 8.4 复算

```bash
# Python 独立复算（无需项目依赖）
python3 - <<'EOF'
import hashlib
def h(b): return hashlib.sha256(b).digest()
def leaf(d): return h(b'\x00'+d)
def node(l,r): return h(b'\x01'+l+r)
def lp2(n):
    k=1
    while k*2<n: k*=2
    return k
def mth(ds):
    if not ds: return h(b'')
    if len(ds)==1: return leaf(ds[0])
    k=lp2(len(ds)); return node(mth(ds[:k]), mth(ds[k:]))
E1=b"curve25519_key=Y3VydmUx\ndevice_id=dev-A\ned25519_key=ZWQyNTUxOTE=\nevent_type=publish\nuser_id=1001"
E2=b"curve25519_key=Y3VydmUy\ndevice_id=dev-B\ned25519_key=ZWQyNTUxOTI=\nevent_type=publish\nuser_id=1001"
E3=b"curve25519_key=\ndevice_id=dev-A\ned25519_key=\nevent_type=revoke\nuser_id=1001"
print(mth([E1,E2,E3]).hex())
EOF
```

~~⚠️ **这些向量目前只存在于本文档**。playbook 要求它们由测试钉死；
该测试属 **Slice 4（Merkle 纯函数实现）**，尚未开工——见 §10 残留 1。~~

✅ **已由测试钉死**（2026-07-30，Slice 4）：`test/lib/e2ee_kt_merkle_tests.erl`
覆盖上述全部向量（含 `E1` 的 96 字节与 canonical head 的 168 字节两处**长度**），
已进 `make e2ee-verify` 硬门禁。见 `evidence/E2EE-065-slice4-merkle-and-proofs.md`。

⚠️ **钉死向量不等于接受 profile**：本文件仍是未签字的冻结草案，
接受动作必须由安全 reviewer 人工完成，loop 不得自我接受。

---

## 9. 与其他冻结项的关系

| 关系 | 说明 |
|---|---|
| `trust_audit`（ADR 06） | **不复用**。字段集与主语都不同，且该表标注「结构变更须走 supersedes」。见调研 §1.3 |
| `CanonicalCbor`（PFv3） | **不复用**为 KT canonical bytes。PFv3 用 CBOR 是因为 header 是嵌套结构；KT 事件是平坦键值，用 §3 方案更省一套 golden vector |
| leaf index | **不得**用 `bigserial`。已由 Slice 1 实证否决（分配序 ≠ 提交可见序，空洞会追溯填上），见调研 §2.1 |

---

## 10. 残留 / 未冻结

1. **golden vector 未被测试钉死** —— 目前只在本文档里。属 Slice 4。
   **认识论状态：向量值已跨实现核验（已实证）；「实现会持续符合它们」未实证。**
2. **leaf index 分配机制未定** —— 只定了「不得用 bigserial」这条约束，
   两阶段 sequencer 的具体形态属 Slice 3。
3. **本 profile 未经安全 reviewer 接受** —— playbook 第 1 步要求人工接受。
   **loop 不得自我接受。**
4. **Consistency proof 的算法细节未在本文件展开** —— wire 格式已冻结，
   路径构造沿用 RFC 6962，实现与向量属 Slice 4。
5. ADR 16 的 transparency log 部分**仍为 Proposed**，本 profile 的实施受其阻塞。

---

## 11. 未做

- **未实施任何生产代码**；未新增迁移、依赖、配置项。
- 未改 ADR / 协议规范；未代签任何 ADR。
- 未删除或 skip 任何测试。
- 不 push、不部署、不访问生产、不通知第三方。
