# C09 — 账号信任根、交叉签名与 Key Transparency Profile

> **run**: run-20261003-094804 | **card**: C09 | **AC**: AC-19, AC-20
> **状态**: **草案（D-02 待批）**——本文档由 C09 安全 worker 执笔，**不构成批准**。
> profile 批准是人工动作（对齐 29 号草案「loop 不得自我接受」原则）；
> **D-02 批准前 C10 不得动任何生产代码**（只允许本地纯函数实验，见 §10）。
> **本文件不改动任何生产源码。** 只产出文档与 fixtures。
> **规范基准**: canonical 编码与 proof 数学以
> `imboy/src/lib/e2ee_kt_merkle.erl`（HEAD 33277f7f 基线，**模块 MD5**
> `124d88abed4ffd78ad2662b82e485127`，取自 `erlang:get_module_info(e2ee_kt_merkle, md5)`；
> **不是 beam 文件 MD5**——文件 MD5 随重编译变化，模块 MD5 与代码行为一一对应，
> 作基线标识不受重编译干扰）
> 的**实际行为为准**，29 号冻结草案 `docs/guides/e2ee/v2/29-e2ee-065-transparency-profile-v1.md`
> 为上位合同。本 profile 是该合同的 **v2 扩展**（supersedes 流程见 §3.5）。
> 修订: 2026-10-03 fix-round1（W1 评审 M1/M2/minor 修复：交叉签名域分离 §3.6 +
> S5 交叉签名 golden vectors + quorum 边界披露 + 术语/引用修正）。

---

## 1. 研究综述与模型选型（≤2 页）

### 1.1 成熟方案对比

| 方案 | 核心机制 | 身份模型 | 优点 | 对本项目不可移植点 |
|---|---|---|---|---|
| **CONIKS**（Melara et al. 2015） | 单调 Merkle 目录 + VRF 用户名→索引（防枚举）+ 定期 signed tree head | 每用户一个目录条目（多键聚合在条目内） | 学术奠基；隐私（VRF 索引）与 split-view（gossip）首次系统化 | 服务端为多 provider 联邦设计；VRF 需额外原语（RFC 9381，项目内无实现）；无交叉签名/恢复层 |
| **Keybase KBFS**（2017 论文 + 产品） | 服务端签名链 + 客户端交叉核验（服务端对同一 root 向不同客户端出示不同视图即被抓） | 账号 = 多社交身份证明（Twitter/Reddit…）+ PGP 设备 | 「客户端即 witness」零运营成本；对服务端无信任 | 依赖高频活跃客户端 population；社交身份证明与 IM 账号模型不匹配；Keybase 产品已被 Acq 后收缩，作唯一范本风险高 |
| **Signal Key Transparency**（工程草案 + PARAKEET 论文线） | CT 风格 append-only log + 账号条目内多设备键聚合 + auditors/witness 分层 | 账号（电话号码）→ 设备键集合 | 与 IM 多设备账号**同构**；证明管线（inclusion + consistency + auditor cosign）成熟 | 目标规模是十亿级公共服务，auditor 层重；条目聚合的具体 VRF/POK 细节至今迭代未完全冻结，逐字跟随有追跑成本 |
| **Sigsum**（2022–，瑞典互联网基金会运营） | 极简 CT：Merkle log + witness cosign（现代签名 key 透明度线） | 日志条目=任意签名材料（不作目录） | **极小可运营面**：witness 只 cosign tree head；fail-closed 哲学与本项目一致；已实际运营 | 不是身份目录（无查询/隐私层）——只提供「防 split-view 的 head 见证」这一层 |
| **Matrix MLS identity claims**（Matrix spec v1.11+ / MLS 1.0 线） | MLS Credential + cross-signing（master/self-signing/user-signing 三钥）+ 透明目录（Spec: Identity Servers 透明化提案） | 账号 = 交叉签名根 + 设备 leaf | **交叉签名信任根与多设备账号同构**；设备增删=签名关系变化，社区活跃 | 目录透明层仍是提案；绑定 Matrix homeserver federation 假设；MLS credential 全套（C15 域）不能提前拉进 C09 |

### 1.2 选型结论

**分层拼装，而非整体跟随单一方案**：

| 层 | 取自 | 理由 |
|---|---|---|
| 目录结构：append-only Merkle log，RFC 6962 MTH | CT/Sigsum（且 29 号 v1 + `e2ee_kt_merkle.erl` 已冻结实现） | 已有代码与 golden vectors，零新数学 |
| 账号信任根：交叉签名（root 签 device、recovery quorum 签 root） | Matrix cross-signing | 与「多设备账号 + 恢复」模型同构；C01 双签名换根已迈出第一步 |
| 设备条目聚合 | Signal KT 的账号→设备集合 | IM 多设备是本项目主场景；recipient enumeration 决策点要求按账号取全设备集 |
| split-view 检测 | Sigsum witness cosign（W-A）+ Keybase 客户端交叉核验（W-B）双模型 | 私有化部署运营得起 W-A 的最小版；W-B 零货币成本兜底；D-05 两个模型都定义（§4.4） |
| 目录隐私 | CONIKS 思想但 v1 用授权查询（§4.5 P2），VRF 列为 P1 扩展 | 私有化部署的对手主要不是「公共爬虫」；VRF 对部署方自身无效，收益<成本 |

**为什么不整体跟随 Signal KT / CONIKS**：两者都以公共服务（十亿用户、公开可爬）为威胁模型起点；
IMBoy 是**私有化部署、部署方=数据控制者**的产品——部署方本来就知道全部用户与设备。
因此 v1 的隐私目标是「**对未授权查询者**（非好友/非同群的其他账号、被攻陷的普通会话方）
不暴露目录」，而非「对服务端隐藏」；后者在单部署架构里是类型错误（见 §4.5）。

**为什么不发明**：所有密码学构件（SHA-256、RFC 6962 MTH、Ed25519、key=value canonical）
全部复用已冻结实现，本 profile 只做**字段集与信任关系的版本化定义**。

---

## 2. 账号信任根与签名关系（account / device / root / recovery）

### 2.1 密钥层级

```
deployment signing key（部署签名密钥，ed25519）        —— C06 锚的 serverEd25519 同一材料
 └─ KT log signing key（树头签名钥，ed25519，29号§7）  —— 可与上者分离存放；key_id 标识
account root key（账号根密钥，ed25519）                —— 离线/恢复材料，日常不在线使用
 ├─ cross-sign：签名每个 device leaf（§2.2 R1）
 ├─ cross-sign：签名每个 agent leaf（§2.2 R2，与部署签名双背书）
 └─ 被以下密钥轮换/恢复：
     recovery keys（恢复密钥集，M-of-N，ed25519）      —— 高熵恢复码派生或用户保管
device identity key（设备签名钥 = olm identity ed25519，C01 现有）
agent identity key（AI agent 自有身份钥，ed25519，C06）
```

**不变量**：
- **INV-1** root key 私钥永不进 KT 目录、永不作为会话密钥；只出现在签名输出。
- **INV-2** root key 丢失不影响已签名 leaf 的可验证性（公钥在 root_publish leaf 里，树证明其历史）。
- **INV-3** 任意 leaf 的伴随签名集合不进 leaf 的 canonical bytes（防自引用；与 C06
  `serverSignature` 不参与 `canonicalBytes` 同一模式，见 §3.2）。

### 2.2 交叉签名规则

| # | 规则 | 防的攻击 |
|---|---|---|
| R1 | **device leaf 由 account root 签**（对 §3.6 域分离输入 `0x03 ‖ imboy.kt.v2.crosssign.device.v1 ‖ 0x00 ‖ leaf canonical bytes` 的 ed25519 签名，wire 伴随字段 `cross_signature`） | 服务端伪造「用户发布了新设备键」——C01 双签名挡住了*换根*（旧钥 PoP），挡不住服务端对**全新 device_id** 插入伪造 leaf；root 签名补上这一环 |
| R2 | **agent leaf 双背书**：账号 root（owner 用户授权 AI 的动作）+ 部署签名密钥（C06 `serverSignature` 语义）各签一份 | 单边伪造：恶意服务端自签锚（C06 已知残留）需要同时骗过 owner root——AI-ID=B 残留风险的收口路径 |
| R3 | **root 轮换/撤销由 recovery quorum 签**：M-of-N recovery keys 对新 root_publish / 旧 root_revoke 的 §3.6 recovery 域分离输入签名（quorum 判定按验签计数，见 §3.2/边界披露） | root 私钥被盗后攻击者自换 root；quorum 签名把「换 root」锚定到离线恢复材料 |
| R4 | **root 首次 publish 允许 TOFU**（同 C03 恢复合同），但必须进树且被对端 witness（客户端缓存 root_publish leaf hash；变化即触发安全码重确认） | 与 Signal 安全码同款 UX：首录不可防，变化必可见 |

**与 C01 双签名换根的衔接**：C01 的 `TransitionSignature`（旧设备钥签 rotation canonical）
继续作为 **device 层**自证；本 profile 的 R1 root 签名是**账号层**背书。两者并存：
- 首次设备注册：C01 PoP（新钥自签）→ KT leaf 携带 root cross-signature（R1）。
- 换根：C01 双签名（新钥 PoP + 旧钥 transition）→ KT leaf 同样携带 root cross-signature。
  服务端缺任一签名即拒收 leaf（AC-21 的服务端侧前置校验），客户端验证时缺任一即 fail-closed。
- 恶意服务端绕过服务端校验直接插树：客户端验 R1 签名失败 → 拒绝该 leaf 作为信任输入。

**R3 quorum 边界披露**（W1 评审 m4 补，D-02 批准前须知情）：
- **M=1 是单点**：`quorum_threshold ≥ 1` 允许部署选 M=1，此时任一恢复钥持有者可独自
  轮换 root——相比「root 私钥被盗自换根」只多了「恢复材料与 root 私钥分离存放」这一层。
  **部署默认建议 M ≥ 2**；M=1 仅适用于单用户自管全部恢复材料的场景。
- **N 规模与 M ≤ N 校验**：N 为该账号登记的 recovery keys 总数，部署侧必须校验
  `1 ≤ M ≤ N`；`M > N` 的 root_publish 应整体拒收（quorum 永不可满足 = root 层自杀式冻结）。
- **恢复材料保管三模式**（安全性递减）：
  ① 用户离线自保管（纸/硬件，与 root 私钥物理分离）——设计意图；
  ② 高熵恢复码派生（对齐 C03 备份恢复合同的口令 KDF 边界：低熵口令可被离线穷举，
     必须高熵）；
  ③ **服务端托管 = R3 退化**：若部署方（或任一能凑齐 M 份的服务端组件）托管全部
  恢复材料，则服务端可自行凑齐 quorum 换 root——R3 对该部署方形同虚设（对**外部**
  攻击者仍有效）。托管模式的部署必须在部署文档自我声明为「R3 对部署方不设防」。
- **全部恢复材料丢失的后果**：root 层**不可再轮换、不可撤销**（无任何一方能凑齐
  quorum 签 root_revoke/新 root_publish）。既有 leaf 历史仍可验证（INV-2，公钥在树上），
  但该账号的信任根进入「冻结」状态；唯一出路是**部署管理员带外冻结该账号的 KT 信任
  并触发全对端安全码重确认**（灾难恢复路径，对齐 C03「账号不可恢复」边界的产品处理，
  不是协议内自动动作）。恢复材料与 root 私钥**同时**丢失时同理。
- quorum 判定的可执行语义见 §3.2（按验签通过计数，错钥签名不凑数）；正反例被
  §5 S5-V3/V5 golden vectors 钉死。

### 2.3 版本化

| 计数器 | 范围 | 维护者 | 语义 |
|---|---|---|---|
| `identity_version` | 每 (user, device) / 每 (user, agent) | C01 `rotate_identity`（单调 +1） | 设备/agent 身份材料代数；防回退/前跳重放 |
| `anchor_version` | 每 agent 锚 | C06 `kAiIdentityAnchorStructVersion` | 锚结构自身版本 |
| leaf schema 版本 | 每部署 KT log | 本 profile（`log_version` in head） | 字段集代数（§3.5） |
| root key 代数 | 每 (user, root) | root_publish 次数 | root 轮换历史即树内 root_* leaf 序列 |

**撤销语义**（进 canonical bytes 的部分；§3.2）：
- `device_revoke`：`curve25519_key`/`ed25519_key` 置空串（继承 29 号 v1 revoke 语义——
  撤销后的 leaf 不再携带可用公钥材料）。
- `agent_revoke`：`anchor_digest` **保留**（撤销必须指明撤销哪个锚；digest 是哈希，
  不暴露公钥本体）。
- `root_revoke`：`root_ed25519` 置空、`key_id` 保留、`quorum_threshold=0`。
- 撤销后同 (user, device) 再 `device_publish`：`identity_version` 必须大于撤销时值
  （C01 撤销复活门在 KT 层的镜像约束）。

---

## 3. Canonical leaf / head 合同（与 `e2ee_kt_merkle` 逐字节对齐）

### 3.1 编码规则（代码为准，逐字重申）

以 `e2ee_kt_merkle.erl` 的 `canonical_event_bytes/1` / `canonical_head_bytes/1`
（同一函数 `encode_kv/1`）**实际行为为准**：

1. 输入字段 map；每字段渲染为 `key=value`，行间 `\n`，**末字段无尾随换行**。
2. key 按 **UTF-8 字节序**（`lists:keysort` 对 binary 即字节序；ASCII 域内=字典序）升序。
3. value 渲染（`to_bin/1` 全分支枚举，代码 L79-82）：binary 原样（UTF-8）；
   integer 十进制；atom 转 UTF-8（**本 profile 禁用 atom，见下**）；
   **string list**（unicode chardata）转 UTF-8 binary（同样禁用）。
4. 输入类型边界（实测记录）：非 map 输入返回 `{error, not_a_map}`；
   **float / tuple / map 等其余类型的 value 在 Erlang 侧 `function_clause` 崩溃**
   （非 `{error,_}` 返回值）；含非法 unicode 码点（如孤代理 U+D800）的 list 在
   `unsafe/1` 检查处 `badarg` 崩溃——调用方须按 crash=fail-closed 处理；
   第二实现侧（Python）以 `not_a_map`/`unsupported_value_type` 异常拒绝，语义等价。
   v2 字段集（§3.2）全为 binary/integer，正常路径不触达这些分支。
5. **fail-closed**：任一 key 或 value 含 `\n`/`\r`、或 key 含 `=`、或字段集为空 →
   返回 `{error, _}`，**不产生任何字节**。
6. 域分离（hash 层，非编码层）：
   `leaf_hash = SHA-256(0x00 ‖ bytes)`，`node_hash = SHA-256(0x01 ‖ L ‖ R)`，
   `tree_head_signing_input = SHA-256(0x02 ‖ head_bytes)`（`e2ee_kt_merkle.erl` L39-41 前缀）；
   交叉签名输入域前缀 `0x03` 见 §3.6。

**v2 约束（本 profile 新增，比代码更严）**：进 canonical 的 key/value 一律
**binary + integer**，禁 atom（atom 渲染依赖代码处的原子拼写，跨实现漂移面大；
Erlang 侧接受 atom 是历史兼容，第二实现按文档规范只需支持 binary/integer）。

### 3.2 identity-log leaf 字段集 v2（冻结候选）

三类 subject，字段均按字节序给出（下表顺序即 canonical 顺序）：

**device leaf**（`subject_type=device`）

| key | 类型 | 说明 |
|---|---|---|
| `curve25519_key` | string(base64) | curve25519 公钥；`device_revoke` 时空串 |
| `deployment_id` | string | 部署标识（多部署防跨部署重放 leaf；与 C06 anchor 同源） |
| `device_id` | string | 设备标识（不透明 ID，非友好名） |
| `ed25519_key` | string(base64) | ed25519 公钥；`device_revoke` 时空串 |
| `event_type` | enum | `device_publish` \| `device_rotate` \| `device_revoke` |
| `identity_version` | integer(十进制) | C01 同名计数器 |
| `subject_type` | enum | `device`（冗余显式，见 §3.4） |
| `user_id` | integer(十进制) | 账号 uid（TSID 数值） |

**agent leaf**（`subject_type=agent`，对齐 C06 `AiIdentityAnchor`）

| key | 类型 | 说明 |
|---|---|---|
| `anchor_digest` | string(hex,64) | = SHA-256(C06 anchor canonical bytes)（即 Dart `anchorFingerprint`）——leaf 与锚**逐字节绑定**而不复制锚字段 |
| `deployment_id` | string | 同 device leaf |
| `event_type` | enum | `agent_publish` \| `agent_revoke` |
| `identity_version` | integer | agent 身份材料版本（五元组第 5 维） |
| `subject_type` | enum | `agent` |
| `user_id` | integer | agent 账号 uid（与 `agent_uid` 同一命名空间） |

**root leaf**（`subject_type=root`）

| key | 类型 | 说明 |
|---|---|---|
| `deployment_id` | string | 同上 |
| `event_type` | enum | `root_publish` \| `root_revoke` |
| `key_id` | string(hex) | root 公钥指纹（轮换追踪） |
| `quorum_threshold` | integer | 恢复 quorum 门槛 M（publish 时 ≥1；revoke 时 0） |
| `root_ed25519` | string(base64) | root 公钥；`root_revoke` 时空串 |
| `subject_type` | enum | `root` |
| `user_id` | integer | 账号 uid |

**伴随证明字段（wire 层，不进 canonical bytes——INV-3）**：
`cross_signature`（R1/R2 root 签名）、`transition_signature`（C01）、
`recovery_signatures`（R3，M 份）、`deployment_signature`（R2，C06 `serverSignature`）。

**交叉签名输入（显式域分离，见 §3.6 规范）**：
R1–R3 的签名输入**不是** leaf canonical bytes 原文，而是带域前缀的拼接
`cross_sign_signing_input(domain, leaf_bytes) = 0x03 ‖ domain ‖ 0x00 ‖ leaf_bytes`
（Ed25519 对该输入原文直接签，不再二次摘要；与 C06 Dart `verifyServerSignature`
的 message=canonicalString「原文含域」语义一致）。域常量分配：
R1 用 `imboy.kt.v2.crosssign.device.v1`；R2 root 侧用 `imboy.kt.v2.crosssign.agent.v1`
（R2 的 `deployment_signature` 沿用 C06 锚内 `domain=imboy.ai-identity-anchor.v1`，不改）；
R3 用 `imboy.kt.v2.crosssign.recovery.v1`。
**R3 quorum 判定语义**：满足 quorum = 该 recovery 域输入下验签**通过**的签名份数
≥ leaf 内 `quorum_threshold`（M）——按验签计数而非按呈交计数，错钥伪造签名不凑数。

### 3.3 tree head 字段集 v2（冻结候选）

| key | 类型 | 说明 |
|---|---|---|
| `deployment_id` | string | **v2 新增**：head 与部署绑定，防跨部署 head 重放 |
| `domain` | string | 固定 `imboy.kt.v2.tree_head`（v2 域常量；v1 为 `imboy.kt.v1.tree_head`，并存期内两域字符串不同即两代 head） |
| `log_id` | string | `imboy-identity-log`（继承 v1） |
| `log_version` | integer | **v2 新增**：leaf/head schema 代数，当前 `2` |
| `root_hash` | string(hex,64) | 小写 hex（继承 v1 §6） |
| `timestamp_ms` | integer | epoch ms |
| `tree_size` | integer | 叶子数 |

签名：`signing_input = SHA-256(0x02 ‖ canonical_head_bytes)`（`tree_head_signing_input/1`），
Ed25519 签之。wire 伴随 `key_id`（签名公钥指纹，v1 §6/§7 轮换合同原样继承）。

### 3.4 设计说明

- **`subject_type` 冗余显式**：`event_type` 已含前缀（`device_*`），`subject_type` 是防御性
  冗余——校验器对两者做一致性断言（`device_*` ⇒ `subject_type=device`），把「新增 event_type
  忘了改校验」从静默漏过变成显式拒绝。
- **agent leaf 绑 digest 而非复制锚**：锚有 8 个字段且含 `issued_at`（每次签发都变），
  复制进 leaf 会让 leaf 与锚互相拖动版本；digest 单向绑定，锚重签（新 issued_at）= 新 digest
  = 新 agent leaf，撤销语义清晰。
- **`deployment_id` 进 leaf 与 head**：私有化世界里有多个独立部署，同一 `user_id` 数值在
  两部署是两个主体。无部署绑定时，部署 A 的 leaf/proof 可被重放进部署 B 的验证上下文。

### 3.5 与 29 号 v1 的关系（supersedes 流程）

- v1 字段集（5 字段 device 事件）是本 v2 device leaf 的**真子集**（v2 = v1 ∪
  {deployment_id, identity_version, subject_type}，且 v1 `event_type` 值
  `publish/rotate/revoke` 映射为 `device_*`）。
- v1 golden vectors（29 号 §8）**继续有效且不被修改**；v2 vectors 是新增集（§5）。
- 29 号规定「字段集变更须走 supersedes 流程」——本文档即是该流程的产物；
  **批准动作 = D-02 签字（§9）**，届时 v1/v2 并存策略按 §9.4 过渡节执行。

### 3.6 交叉签名域分离（cross-sign domain separation）

R1–R3 交叉签名的输入构造（canonical 规则，双侧实现必须逐字节一致）：

```
cross_sign_signing_input(domain, leaf_bytes) = 0x03 ‖ domain ‖ 0x00 ‖ leaf_bytes
```

| 分量 | 值 | 约束 |
|---|---|---|
| `0x03` | 单字节域前缀 | 接续 hash 层前缀体系（`0x00`=leaf_hash、`0x01`=node_hash、`0x02`=tree_head_signing_input），是**第四个密码学输入域**的标识字节 |
| `domain` | 固定 ASCII 常量（见下表） | 纯可打印 ASCII，互不相同，均以 `imboy.kt.v2.crosssign.` 起头，**不含 `0x00`** |
| `0x00` | domain 与 leaf 的定界符 | 单射性关键（见下） |
| `leaf_bytes` | 被签 leaf 的 canonical bytes（§3.2 字段集） | v2 三类 leaf 字段类型全部为 string(base64)/string(hex)/enum(固定 ASCII 词表)/integer(十进制)，**全部可打印 ASCII，不含 `0x00`** |

域常量分配：

| 域常量 | 用于 | 签名者 |
|---|---|---|
| `imboy.kt.v2.crosssign.device.v1` | R1：root 签 device leaf | account root key |
| `imboy.kt.v2.crosssign.agent.v1` | R2 root 侧：root 签 agent leaf | account root key |
| `imboy.kt.v2.crosssign.recovery.v1` | R3：recovery keys 签 root_publish / root_revoke leaf | recovery keys（M 份同域） |

（R2 的部署侧 `deployment_signature` 沿用 C06 锚内既有域
`imboy.ai-identity-anchor.v1`——锚 canonical bytes 自带 domain 字段，无需本节前缀。）

**单射性与不冲突论证**：
1. **域内定界单射**：domain 与 leaf bytes 都不含 `0x00`，输入串中第一个 `0x00`
   即 domain/leaf 边界；给定输入串，domain 与 leaf bytes 可唯一分解。
2. **跨域前缀分离**：不同 domain 常量互不相同且位于固定偏移（跳过首个 `0x03`），
   任一域的输入不可能等于另一域的输入。
3. **与 hash 层域分离**：cross-sign 输入首字节 `0x03`，与 leaf_hash（`0x00`）、
   node_hash（`0x01`）、tree_head_signing_input（`0x02`）输入在**首字节即互异**——
   交叉签名不可能被重放为叶哈希输入/head 签名输入，反之亦然。
4. **与体系外签名域分离**：C06 锚签名与 C01 rotation/fallback canonical 的输入均为
   可打印 ASCII `key=value` 文本（首字节为字母），cross-sign 输入首字节 `0x03`——
   跨体系无碰撞。KT 域内三类 leaf 字段集形状互异不再是唯一防线（W1 评审 M2 指出
   「形状巧合」不可依赖），前缀体系是显式保证。

**签名语义**：Ed25519 对 `cross_sign_signing_input` **原文直接签**（RFC 8032 内部
SHA-512，不预摘要）——与 §3.3 head 签名（对 SHA-256 后 32 字节摘要签）不同但输入域
已互斥（第 3 条），两者互不可重放。golden vectors 见 §5 S5（合法 R1/R2/R3、伪造错钥
拒绝、quorum 不足拒绝、域混淆拒绝、换叶重放拒绝）。

**客户端/C10 实现要求**：验签方必须**从规则（R1/R2/R3）反查 domain 常量重组输入**
后验签，不得接受 wire 上自声明 domain 的签名对象（domain 是规则的内生属性，
不是可选字段）——防「攻击者自带域字符串」的混淆面。

---

## 4. Checkpoint、证明与决策点

### 4.1 Signed Tree Head（STH）

- sequencer 按批（Slice 1 定案：leaf index 与 bigserial 解耦）追加叶子后签发 STH；
- STH wire 结构（v1 §6 扩展）：`{deployment_id, log_id, log_version, tree_size,
  timestamp_ms, root_hash, key_id, signature}`；
- 签名 key 轮换按 v1 §7 双签过渡窗口合同**原样执行**（旧/新 key 同 head 各签一份、
  过渡窗口可回退、撤销后旧 key 签发一律拒收、两把都过期 fail-closed）。

### 4.2 决策点矩阵（proof 使用场景）

| 决策点 | 所需证明 | 验证失败动作 |
|---|---|---|
| **upload**（`report_identity` 上报身份键，C01） | 服务端回执：本次 leaf 的 inclusion proof + 当前 STH | 客户端按 `e2ee_kt_merkle:verify_inclusion` 验证；失败 → 上报不视为成功（UI 明示「未入透明日志」），不得静默重试后遗忘 |
| **claim / recipient enumeration**（发起会话前取对端身份键） | 每个 (user, device) 键的 inclusion proof 绑定同一 STH；STH 验签 | **fail-closed 拒绝发起会话**（AC-21：每次生产 key trust 决策有证明链）；陈旧 STH（超 freshness 窗口）→ 刷新重取 |
| **safety number / trust 决策**（C07 安全码） | 对端**全设备集合** inclusion proofs（AC-15 完整设备集验证）+ 本地高水位 head 与当前 head 的 consistency proof | 失败 → 信任状态置「不可验证」，安全码 UI 强提示；不静默用部分设备集计算聚合码 |
| **agent 会话**（人↔AI） | agent leaf inclusion + R2 双背书验签（root cross-sig + C06 server sig）+ anchor_digest 与运行中锚指纹比对 | 任一失败 → 五元组不升级（对齐 C06：锚变化=指纹变化=旧确认失效） |

### 4.3 防回滚与防分叉

**防回滚**（客户端高水位，对齐 C02 高水位持久化模式）：
- 客户端持久化 `(deployment_id, log_version, max_tree_size, last_root)`；
- 任何接受的 STH 必须满足 `tree_size ≥ max_tree_size`；`tree_size` 相同则 `root_hash` 必须相同；
- `tree_size` 增大时索取 consistency proof（`verify_consistency`），从 `last_root` 链到新 root；
- 验证失败或倒退 → 该 head 不更新高水位，决策点矩阵全部降级为 fail-closed。

**防分叉**（split-view 检测，AC-20 核心）：
- 分叉定义：同 `(deployment_id, tree_size)` 出现两个不同 `root_hash` 的有效签名 STH；
- 检测通道：§4.4 W-A witness cosign 只认其中一个 root（先到先签，Sigsum 语义），
  客户端见到「同 size 两 root 且 witness 只 cosign 了一个」即分叉实锤；
  W-B 客户端 gossip 把彼此的 `(size, root)` 对照，不一致即分叉嫌疑；
- 分叉后动作：记录冲突对、冻结高水位推进、UI 警告；**不自动仲裁**（仲裁是人工/部署方动作）。

### 4.4 Witness gossip 模型（D-05 未决——两个可选模型）

**模型 W-A：独立 witness cosign（Sigsum 风格）**

| 项 | 内容 |
|---|---|
| witness 定义 | **独立于 imboy 应用服务器的签名服务**：不同进程、不同机器/可用区、不由应用服务器配置源控制；持有独立 ed25519 key；只做「收 STH → 检查 size 单调且能出示到上次 cosign 的 consistency proof → cosign」 |
| 见证粒度 | STH（<200B/次）；cosign 频率：每 STH 或按 N 分钟窗口（Sigsum 默认按分钟级） |
| 存储 | 只需上次 cosign 的 `(size, root)`（<100B）；可选审计归档 ~0.5GB/年（1 STH/min 量级） |
| 信任锚 | witness 公钥经部署配置**带外**分发（部署文档/客户端内置部署清单），不来自应用服务器 |
| 故障策略 | witness 不可达：客户端**不因此 fail-closed 断连**（witness 是检测器不是门禁），但 UI 显示「外部见证缺失 N 周期」；consistency proof 失败才是硬阻断；witness 连续失联超阈值（建议 24h）→ 升级为安全码重确认提示 |
| 成本区间 | 单实例 1C/512M 级 VPS $5–15/月；或复用部署方**另一信任域**已有机器（成本≈0，代价是治理承诺）；真实成本在「独立性运营」（不得由应用服务器同一凭据/同一人力全自动管理），非算力 |

**模型 W-B：客户端交叉核验 gossip（Keybase/CONIKS 风格）**

| 项 | 内容 |
|---|---|
| 机制 | 客户端把最近验证过的 STH digest 附带在常规消息/presence 中交换（对齐 28 号 §3.1：PFv3 携带 tree-head digest 需改协议——**此为 W-B 的前置依赖，属 C10/C15 决策**）；收到对方 STH 与本地高水位冲突 → 按 §4.3 分叉流程 |
| 信任锚 | 无（对等互证）；冷启动首录仍 TOFU |
| 故障策略 | 低活跃部署（少量在线客户端）检测延迟高甚至无覆盖——W-B 定位是**补充**而非独立充分；gossip 消息本身防篡改依赖 E2EE 通道（在 E2EE 会话内携带即免费获得） |
| 成本 | 零货币成本；工程成本在客户端协议位与冲突 UI |

**推荐（写入 D-05 决策）**：v1 双轨最小版——W-B（STH digest 进消息通道）为默认、
W-A（单 witness cosign）为强烈推荐配置；两者都不与「server 自签 tree」互替
（C00 §5：只有 server-signed tree 不满足 AC-20）。

### 4.5 目录隐私（leaf 不向任意查询者暴露全设备清单）

| 方案 | 机制 | 评估 | 结论 |
|---|---|---|---|
| **P2 授权查询**（v1 采纳） | KT 查询（含 proof 请求）复用 `claim_keys` 授权模型：仅好友/同群成员可枚举对方 (user, device) 目录 | 与现有 IM 授权一致、零新原语；对「未授权账号 + 恶意客户端」封闭目录；**对恶意服务端无效**（split-view 由 §4.3/4.4 层解决，查询层不背这个锅） | ✅ v1 |
| **P1 VRF 索引**（CONIKS 式，列为 v2 扩展） | leaf index = VRF(deployment_seed, user_id, device_id) 派生，查询者无法扫邻居推断条目归属 | 防的是「能拉全树的旁观者」；**私有化部署方持有 seed，P1 对部署方无效**——收益集中于公共/多租户部署或第三方镜像日志场景；需 RFC 9381 ed25519-VRF（项目内无实现，jose 无现成） | ⏸ 挂起，公共部署需求出现再启动 |
| **P3 blinded 查询 | 查询 token 盲化 | 与 inclusion proof 结构冲突（验证需确切 index）；要配 PIR/VRF 才自洽，复杂度高、收益与 P1/P2 重叠 | ❌ 不采纳 |

**leaf 内容本身的最小化**（已冻结于 §3.2）：无昵称/手机号/友好设备名；`device_id` 不透明、
`user_id` 为数值 TSID；`anchor_digest` 是哈希。**全量树 dump 仅部署管理员可及**
（私有化部署方本来就知道自己用户，见 §1.2 威胁模型定位）。

---

## 5. Golden vectors（fixtures/）

- 位置：`fixtures/vectors.json`（生成产物，`meta.version=2`）+
  `fixtures/generate.escript`（Erlang 一键再生成）
  + `fixtures/verify_python.py`（第二实现独立复算，见 §6）。
- **生成方式**：escript 加载**主仓真实 beam**（`imboy/ebin/e2ee_kt_merkle.beam`，只读，
  不改任何主仓文件），canonical/hash/proof 全部来自生产实现；Ed25519 用 `deps/jose`
  纯 Erlang 实现（`jose_jwa_ed25519`，绕开本机 OTP29/OpenSSL3.6 eddsa 互操作缺陷，
  该缺陷已记录在证据卡；jose 是主仓既有依赖，非新增）。
- **场景×检查类矩阵**：5 场景（S1 human 多设备、S2 AI agent、S3 账号信任根、
  S4 全局不变量、**S5 交叉签名**）——S1–S3 各 5 类（合法 inclusion / 合法 consistency /
  错误叶子 / 篡改 head / 分叉 head 拒绝）+ S4 不变量 5 类 + **S5 交叉签名 7 类** =
  **27 vectors**、**245 项交叉断言**（canonical bytes / leaf hash / root / audit path /
  consistency path / head canonical / signing input / Ed25519 公钥与签名 /
  cross-sign 域输入与签名 / quorum 计数 / 各类拒绝），全绿。
  逐 vector 内容见 `vectors.json`。
- **S5 交叉签名 vectors**（fix-round1，§3.6 的可执行钉死）：合法 R1 device cross-sign
  （root 签 device leaf）、合法 R2 agent 双背书（root + deploy 各签一份）、合法 R3
  recovery quorum（M=2-of-N=3 达标）、伪造 root 签名（错钥）拒绝、quorum 不足拒绝
  （1 份有效 / 1 有效 + 1 伪造凑数 / 无签 三档）、**域混淆拒绝**（device 域签名当
  recovery/agent 域用必败，原域验签通过证明签名完好）、**换叶重放拒绝**（对 leaf A
  的签名重放到 leaf B 同域必败）。「攻击者自签自证成立」如实记录为分叉面
  （与 C06 锚负例同款诚实语义）。
- 每个 vector 含：输入（事件字段 map、index/size、篡改方式、签名者/域）、期望
  canonical bytes(hex)、期望 leaf_hash / root / audit path(hex)、期望 head canonical
  bytes 与 signing_input(hex)、期望 Ed25519 签名（确定性 seed key）、期望结果
  （接受/拒绝）。

## 6. 交叉验证方法与结果（AC-19）

- **实现 A**：Erlang `e2ee_kt_merkle`（生产 beam）+ `jose_jwa_ed25519`（generate.escript）。
- **实现 B**：Python 3 标准库（`hashlib`/`struct` only）按**本文档 §3 规范**独立重写：
  canonical 编码（含全部 fail-closed 分支）、RFC 6962 MTH/PATH/SUBPROOF 生成与迭代验证、
  RFC 8032 Ed25519 纯实现、**§3.6 cross-sign 域输入构造与 quorum 计数**——与 Erlang
  无任何代码共享。B 的 Ed25519 以 **RFC 8032 §7.1 官方测试向量（TEST 1/TEST 3）自检**，
  该自检已内置进 `verify_python.py`（每轮验证先跑，官方向量不过则整体报失败）。
- **判定**：对每个 vector，A 产出 == B 重算 == vector 记录值（三向一致）；
  错误 vector 两侧都断言拒绝。S5 交叉签名的关键正负例另经 **OpenSSL 3.6.4 CLI
  第三实现**独立抽验（合法 V1 通过 / 伪造 V4 拒 / 域混淆 V6 拒 / 攻击者自证成立）。
- 结果记录：`../../../.Codex/evidence/e2ee-excellence/run-20261003-094804/cards/C09/cross-validation.md`
  （对照表 + 复现命令 + 模块 MD5）。

## 7. AC-20 论证摘要

只有 server-signed tree **不算完成**（C00 §5）。本 profile 的 split-view 检测 =
§4.3 防回滚高水位（客户端侧，无需新信任锚）+ §4.4 W-A/W-B 双模型（人工二选一或双启，
D-05）+ §4.2 决策点 fail-closed。分叉 head 的拒绝被 S1–S3 的 V5 vectors 与
S4-V5（consistency 同大小不同根）以可执行断言钉死——「能检测」不是声明而是向量。

## 8. 对 C10 的实施建议（模块边界）

1. **proof 数学零新实现**：C10 一律调 `e2ee_kt_merkle`（生产 beam 即验证过的实现）；
   Dart 侧 verifier 按 §3 规范实现并以 `fixtures/vectors.json` 为跨语言测试数据源
   （与 Erlang/Python 三方一致即达 AC-19 精神）；`cross_signing.dart`（R1–R3 验签）
   以 **S5 vectors**（§3.6 域输入 + quorum 计数 + 全部负例）为测试数据源——
   验签方按规则反查 domain 常量重组输入（§3.6 末段），不接受 wire 自声明 domain。
2. 新增模块建议（全部薄层，不含密码学）：
   - `e2ee_kt_log_logic.erl`：事件入树、STH 签发调度（key 轮换按 v1 §7）；
   - `e2ee_kt_proof_handler.erl`：upload 回执 / claim / 目录查询的 proof API（§4.2 矩阵）；
   - `e2ee_kt_witness.erl`：cosign 外发与 witness 公钥配置（W-A）；
   - App：`key_transparency_client.dart`（verify + 高水位 + gossip 收发）、
     `cross_signing.dart`（R1–R3 验签）。
3. sequencer：leaf index 分配沿用 Slice 1 定案（与 bigserial 解耦），不重开。
4. 迁移（C12 lease）：identity-log 表、root/recovery 关系表；KT 只追加不改旧列。
5. D-02 批准前 C10 只允许本地纯函数实验与 fixture 测试，不接线、不部署（§10 重申）。
6. eddsa 环境重验：本机 OTP29/OpenSSL3.6 的 `crypto:sign(eddsa, …)` 损坏（§10），
   C10 在**目标服务器环境**重验 eddsa 可用性后再定签名调用路径（crypto 或 jose 纯实现）。

## 9. 决策卡草案 D-02（批准动作 = 人工签字）

| 字段 | 内容 |
|---|---|
| 决策 | 是否批准本 profile（identity-log v2 字段集 + 交叉签名规则 R1–R4 + STH v2 + witness 双模型推荐 + 隐私 P2）作为 C10 生产实施合同 |
| 提案人 | C09 worker（run-20261003-094804） |
| 批准人 | 安全 reviewer（人工；loop 不得自我接受） |
| 批准前置 | ① AC-19 三向交叉验证全绿（§6/证据卡）；② 独立 reviewer 评审记录；③ D-05 witness 模型选择（可同为本次决策） |
| 批准后果 | C10 解锁生产接线（迁移走 C12 lease）；C07 安全码/设备集验证获得 consistency proof 合同 |
| 不批准后果 | C10 维持 BLOCKED（只允许本地纯函数实验）；C06 锚维持派生指纹（AI-ID=B 残留风险不收口）；C00 T-MS 的 KT 项维持 CONFIRMED gap |
| 回滚 | profile 版本化（log_version）；v1 vectors 永不修改，v2 可整体作废重发新 log_version，不影响 v1 数据 |

**D-05 witness 运营选项与成本区间**（随 D-02 一并裁决，详见 §4.4）：

| 选项 | 货币成本 | 治理成本 | 检测能力 |
|---|---|---|---|
| 仅 W-B（客户端 gossip） | 0 | 低（协议位设计） | 依赖活跃客户端分布，低活跃部署弱 |
| W-A 单 witness（复用另一信任域机器） | ≈0 | 中（独立性承诺书面化） | 独立检测，单点 witness 自身可用性影响检测连续性 |
| W-A 外购 VPS witness | $5–15/月 | 中（账号/续费/密钥管理） | 同上 |
| W-A 双 witness + W-B | $10–30/月 | 高 | 最强（witness 间也可互证） |

### 9.4 v1/v2 并存与过渡（§3.5 引用的落地节）

- **并存机制是结构性的，不靠协议协商**：v1 与 v2 的 tree head `domain` 常量不同
  （`imboy.kt.v1.tree_head` vs `imboy.kt.v2.tree_head`）且 v2 head 多出
  `deployment_id`/`log_version` 字段——同一部署内两代 head 字节形状互斥，
  按头即可分派验证器，无歧义分支。
- **v1 数据永不迁移、v1 vectors 永不修改**（29 号 §8 冻结；§3.5）。v2 是新 log
  代数（`log_version=2`），从创世叶起独立成树；已按 v1 验证过的历史结论保持有效。
- **过渡期读写策略**：客户端按 head 的 domain/log_version 分派——遇 v1 head 走 v1
  字段集验证（只读兼容），遇 v2 head 走本 profile；不提供 v1→v2 的叶级转换
  （两代高水位列表各自独立，consistency proof 不跨代，v1 树停更即终态）。
- **v2 整体作废流程**（若 D-02 批准后发现 v2 缺陷）：重发 `log_version=3` 新 log，
  v2 已签 head 全部作废；v1 数据与 v1 vectors 仍不受影响（§9 决策卡回滚行）。

## 10. 未做与边界重申

- **未改任何生产源码**；未新增迁移、依赖、配置；未部署任何服务；未联系任何第三方。
- **D-02 未批准**：C10 在批准前不动生产（边界重申）；本 profile 及 fixtures 均为
  本地纯函数层产物。
- witness/目录隐私的实施（P1、W-A 部署）均在 D-02/D-05 裁决之后。
- Erlang/OTP 29 + OpenSSL 3.6.4 本机 `crypto:sign(eddsa, …)` 路径损坏（实验记录见证据卡），
  vectors 采用 jose 纯 Erlang ed25519 规避；**这不是对生产签名栈的改动**（生产用
  `imboy_plugin_signature` 的 crypto 路径，服务器 OpenSSL 版本不同，且 C10 接线时
  需在目标环境重验 eddsa 可用性——已写入 C10 建议 6）。
