# C15 — MLS 群组泄露后恢复 Profile（研究阶段 / PROTOTYPE）

> **run**: run-20261003-094804 | **card**: C15 | **AC**: AC-30, AC-31
> **状态**: **研究/PROTOTYPE 上限**——本文档为 C15 worker 执笔的 profile 草案，
> **不构成批准**（批准动作 = D-03 人工签字，见 §8）。
> **D-03 批准前禁止生产接线**：不写生产 Dart/Erlang 代码、不动 `pubspec.yaml`、
> 不新增迁移、不部署任何服务（plan C15 卡 + C00 待决项表）。
> **规范基准**: RFC 9420（The Messaging Layer Security Protocol，2023-07，
> IETF Standards Track）。行号引用 `L…` 为本轮下载的 rfc-editor txt 副本行号。
>
> 配套文件（本目录 `mls-research/`）：
> - `selection-matrix.md` — 成熟实现选型对比（六维，逐项标注确证/推断/UNVERIFIED）
> - `vectors-acquisition.md` — 标准 vectors 获取方案 + SHA256 清单
> - `migration-design-draft.md` — Megolm↔MLS 状态映射表 + 双协议并存边界（AC-31）
> - `vectors/` — 已下载的 10 个 mlswg 官方 test vectors

---

## 1. 问题定义（为什么要 MLS）

当前 C2G 群加密为 Megolm（vodozemac 0.8.1）：

- **审计边界（文档确证）**：`docs/security/audits/e2ee-2026-09-07/E2EE_AUDIT_REPORT.md`
  §3 ——「Megolm rotation 不等于 Double Ratchet PCS。离群后保密依赖
  『成员撤销传播→新设备快照→下一条消息前轮换』完整成立」；即**同一 session
  （≤100 条/≤7 天）内 room key 泄露可还原该 epoch 全部消息**，且被移除成员持旧
  room key 可解移除前全部密文（FS 粒度 = session，非 per-event PCS）。
- **威胁模型（T-KD）**：C00 threat-model §2 ——「密钥泄露后恢复：Megolm
  rotation≠PCS（如实声明）；MLS 泄露后恢复不存在 → C15」。
- **MLS 提供的增量（文档确证，RFC 9420）**：
  - per-epoch 密钥调度（§8 L2761）+ Secret Tree per-sender/世代密钥（§9 L3270）
    + 删除计划（§9.2 L3387）——**epoch N 密钥材料泄露不影响 epoch ≥ N+1**；
  - TreeKEM 成员变更（commit）后旧成员 state 无法派生新 epoch 密钥；
  - commit 链 transcript hash 提供**群内分叉可检测性**（Megolm 无对应物）；
  - `epoch_authenticator`（§8.7 L3245）提供视图一致性指纹（安全码语义的群级版）。

**定位**：MLS 不是替换 Olm/Megolm 的全量方案，而是**群（C2G）域的下一世代**，
用于补 PCS/泄露恢复；C2C 单聊维持现役（PQ 化归 C16）。见 migration 草案 §3.3。

## 2. 成熟实现选型（摘要；详表见 `mls-research/selection-matrix.md`）

| 候选 | 许可证 | 审计 | FFI(Dart/FRB) | 平台 | 活跃度 | RFC 9420 |
|---|---|---|---|---|---|---|
| **OpenMLS**（Rust，Phoenix R&D + CE Labs） | MIT | **SRLabs 2026 独立审计：8 issues，high→informational，多为低危**¹ | 纯 Rust 嵌入式；openmls_dart（FRB 2.13 + openmls 0.9.0）实证可桥接 | CI 含 Android/iOS 构建² | crates 0.9.0（2026-08）；push 2026-10-02 | 是（interop 生态） |
| mls-rs（AWS Labs，Rust） | Apache-2.0/MIT | **无第三方审计**（README 自述） | 官方 FFI/UniFFI（Swift/Kotlin）；Dart 仍需自建 | 移动端明确支持 | 0.56.0（2026-08）；push 2026-09-17 | 声明 100% 符合（vectors 验证） |
| mlspp（Cisco，C++17） | BSD-2-Clause | 无公开审计 | dart:ffi 手写 + OpenSSL 构建链，成本高 | 无官方移动支持 | push 2026-09-21 | README 引用 draft（非 RFC 版本表述） |
| openmls_dart（pub `openmls` 3.2.1） | MIT | 无（个人项目，9 stars） | 现成 FRB 绑定 | Android/iOS/macOS 六平台 | 2026-09-29；CI 每日跟上游 | 随 openmls 0.9.0 |
| matrix-rust-sdk crypto | Apache-2.0 类 | vodozemac 有（2022）；MLS 线实验性无 | 绑定整个 SDK，不适用 | — | 持续 | MLS 为 SSSCS 实验线³ |
| libMLS（Inria 历史） | — | — | — | — | 停滞⁴ | draft 期 |

¹ 逐条 issue 清单与修复状态 **UNVERIFIED**（未获取审计报告原文；两篇官方博客
blog.openmls.tech 2026-03-11 / blog.phnx.im 2026-05-27 检索摘要为据）。
² CI 对移动 target「仅构建不测试」；真机验证属后续卡。
³ Matrix 基金会维护自己的 OpenMLS fork；SSSCS 实验性状态为社区信息【推断】。
⁴ 搜索结果显示被 mlspp 取代、不再显著维护【推断】。

**推荐：OpenMLS + 自建 flutter_rust_bridge 绑定层**（理由与备选切换条件见
selection-matrix.md §3）：

1. 唯一持有独立第三方审计的候选（安全先决条件最稳）；
2. MIT + 零传输/身份假设，与「自有路由层 + C09 KT 身份层 + 只换群密码层」边界最干净；
3. FRB 桥接已被 openmls_dart 实证，且本仓 flutter_vodozemac 同栈运维经验现成
   （FRB 进程级单次 init 的 BUG#72 教训直接复用）；
4. 维护最活跃。

**备选：AWS mls-rs**（仅当 PoC 中 OpenMLS 不满足冻结群规模/性能预算时切换；
无审计须在 D-03 显式接受）。**PoC 加速**：允许本地实验直接用 openmls_dart
（pub 包 `openmls`），生产接线换自建绑定。**排除**：mlspp / matrix-rust-sdk / libMLS。

**密码套件**：仅启用 MTI `0x0001`（X25519+AES-128-GCM+Ed25519），按需加
`0x0003`（ChaCha20 变体，老设备友好）【推断：对齐现役 vodozemac 套件面，最小化
审计面】。PQ 套件（draft 状态）**默认关闭**——归 C16 域，不越界。

## 3. Credential / Auth 绑定设计草案（衔接 C09 identity-transparency-profile）

### 3.1 RFC 9420 的责任边界（文档确证）

RFC 9420 §5.3.1（L1341 起）：「*The application using MLS is responsible for
specifying which identifiers it finds acceptable for each member in a group*」——
MLS 协议认证的是 **credential（leaf 签名密钥持有权）**；「这个 identity 真的属于
该账号/设备」由应用层负责。**这个应用层验证正是 C09 KT 树的职责**，两者拼合才闭环。

### 3.2 Basic vs X.509 选择：**选 Basic credential**

RFC 9420 §5.4（L1313–1338）CredentialType 仅两类：`basic`（opaque identity，
格式应用自定义）与 `x509`（DER 证书链）。

| 维度 | Basic（选） | X.509（不选） |
|---|---|---|
| 信任根 | 应用层（→ C09 KT 树 + 交叉签名 R1–R4） | CA/PKI（需部署证书签发、链验证、撤销面） |
| 身份模型匹配 | identity 为应用自定义 bytes → 直接编码 (uid, did, identity_version, deployment_id) | SAN 命名需映射 TSID/设备 ID，证书生命周期 vs 设备增删节奏不匹配 |
| 私有化部署成本 | 零新增 | 每部署一套 CA 或共享 CA 信任分发（C12 运维面扩大） |
| 现有生态 | Matrix/多数 MLS 部署用 Basic【推断：社区实践】 | MIMI/企业域线在推进（未定型） |

**理由总结**：X.509 引入的 CA 信任面与 IMBoy「部署方=数据控制者 + KT 自证」
模型冲突——KT 树本身就是比 CA 更细粒度（per-device per-version）的透明目录。
Basic credential + KT 验证 = 把「证书链验证」替换为「inclusion proof + 交叉签名验证」。

### 3.3 Basic credential 的 identity 编码（v1 冻结候选）

```
identity = "imboy-mls-basic/v1"
           ";uid="  <user_id 十进制 TSID>
           ";did="  <device_id>
           ";iv="   <identity_version 十进制>        // C01 同名计数器
           ";dep="  <deployment_id>                  // C09 leaf 同源，防跨部署重放
```

- 约束：仅 ASCII、无 `;`/`=` 冲突字符（did/deployment_id 为不透明安全字符集——
  与 C09 canonical `key=value` 同款 fail-closed 检查，实现时复用其校验器逻辑）。
- `iv`（identity_version）进 identity 的作用：**credential 变更 = identity bytes
  变更**，群成员可直接从 LeafNode 看到密钥代数，与 KT leaf 的 identity_version
  双向核对（协议面 vs 目录面）。

### 3.4 密钥与信任关系（衔接 C09 §2 R1–R4）

MLS LeafNode 含两类密钥：`encryption_key`（HPKE，TreeKEM 用）与
`signature_key`（credential 的签名密钥）。**新增设备级 MLS 签名密钥**（ed25519，
套件 0x0001 下与 leaf 绑定），与现有 olm identity key 分离存储（键前缀
`mls_id_*`，对齐 C05 policy-matrix #9 `mls_*` 预留行）。

| C09 规则 | 在 MLS 面的落点 | 验证时机 |
|---|---|---|
| **R1** device leaf 由 account root 签 | (a) KT device leaf v2 的 canonical bytes **新增字段** `mls_signature_key`（base64；`device_revoke` 置空串——继承撤销置空语义），使 MLS leaf 签名公钥进透明目录；(b) MLS Add 场景：处理含新成员 leaf 的 commit 前，取该 (uid,did) 的 KT inclusion proof + R1 cross_signature 验签——**任一失败拒绝整个 commit**（fail-closed，对齐 C09 §4.2 决策点矩阵风格） | commit 处理（§12.4.2）前置于密钥派生 |
| **R2** agent leaf 双背书 | AI agent 进 MLS 群（未来产品面）时，agent credential 的 `iv` 对应 KT agent leaf（anchor_digest 绑定）；root cross-sig + 部署签名双验 | 同上；本轮仅定义关系，不实现 |
| **R3** root 轮换由 recovery quorum 签 | root 换代 → R1 验签公钥来源变化（新 root_publish leaf）→ 客户端按 C09 §2.3 root 代数推进缓存；MLS 侧无直接动作（credential 验证走的还是「当前有效 root」） | KT 高水位推进时 |
| **R4** root 首 publish TOFU + 对端 witness | MLS credential 的信任也继承 TOFU 首录语义；**变化必可见**：credential identity bytes 或 mls_signature_key 变化 → 该成员安全码/`epoch_authenticator` 视图变化 → UI 重确认提示 | 安全码计算时 |

**MLS Update（§12.1.2）与 C01 双签名的衔接**：成员自换 leaf 密钥（设备密钥轮换/
泄露恢复动作）时，新 KeyPackage 的 credential `iv` 必须 +1；旧 leaf → 新 leaf 的
过渡在账号层仍走 C01 双签名（新钥 PoP + 旧钥 transition）上报 KT 后才被对端
接受。即：**KT leaf 先行，MLS commit 后行**（顺序不变量，违反即 fail-closed）。

**`epoch_authenticator` 的安全码联动**：群级安全码（C07 域）在 MLS 群下的输入
升级为 `epoch_authenticator`（§8.7，exporter label "authentication"）——两客户端
同视图（同 group_id、同 epoch、同 transcript）则值相同，天然防群内 split-view。
这与 C09 §4.4 W-B「STH digest 进消息通道」互补：一个盯**目录**（身份键），一个盯
**群状态**（epoch 链）。实现归 C07/C10 接线卡，本卡只定关系。

### 3.5 明确不启用面（本轮边界）

- **External Commit / external senders**（§12.4.3.2 / §12.1.8.1）：关闭。外部
  加入通道扩大攻击面（外部签名者可信度无 C09 目录背书），私有化部署无此需求。
- **ReInit / Subgroup Branching**：仅预留（灾难恢复/子群分叉的未来产品面），
  不进 v1。
- **PSK 注入**：external PSK 关闭（INV-M1，migration 草案 §3.1）；resumption PSK
  （§8.6）开启（协议内自派生，用于 epoch 链续证）。

## 4. Epoch/Commit/Welcome/Fork ↔ 现有 Megolm 世代映射（摘要）

完整映射总表 + 五个生命周期事件状态对应 + 双协议并存边界见
`mls-research/migration-design-draft.md`。要点：

| 事件 | Megolm 现状 | MLS 后 | 安全增量 |
|---|---|---|---|
| join | 新成员获 room key（当前 session 起点可解）+ 历史 grant | Add+Commit+Welcome；仅新 epoch 起可解 | 历史不可读为协议默认（修复审计 §3 历史边界缺口） |
| leave/kick | rotate 重分发；被移除者持旧 key 解旧密文 | Remove+Commit；旧 state 无法派生新 epoch 密钥 | per-event PCS（AC-31 协议基础） |
| rejoin | 无 epoch 边界（审计缺口） | Remove→Add 两段隔离 | 中间 epoch 不可解 |
| 设备换钥/泄露恢复 | rotate（旧 epoch 全暴露） | Update commit 换 leaf 密钥 | 恢复后新消息不可解（FS 上限如实披露） |
| 长离线 | grant 延展（同 generation 限制） | commit 队列按序追赶 + resumption PSK | 断链 fail-closed |
| fork | 无检测 | transcript hash 分叉可检测 | 群内 split-view 防护 |

**generation_no / conv_seq / attestation 的去向**：MLS 群内 epoch 取代
generation_no 的密码学边界职责；conv_seq 保留为服务端展示/存储序（staging 权威
顺序继续有效）；migration 112 两表对 MLS 群只读不写（历史 Megolm 会话数据不动）。

## 5. 负例测试设计清单（AC-30）

统一断言框架：每个负例 = 「预期协议行为」（RFC 9420 语义）+「测试断言点」
（OpenMLS API 返回 / 第二实现输出 / vectors 重算）。执行层次：
(a) vectors 互操作（已下载 10 文件）；(b) Rust 层集成测试（openmls test API）；
(c) Dart 绑定层重复 (b) 的关键子集（PoC 阶段用 openmls_dart 或自建绑定）。

| # | 场景 | 预期协议行为（RFC 依据） | 测试断言点 |
|---|---|---|---|
| N1 | **remove 后解密** | Remove commit 产生新 epoch；被移除者持旧 epoch state 收新消息 → 无法派生 Secret Tree 密钥（§7.7/§9） | 被移除者 `decrypt` 返回错误；移除前 epoch 消息仍可解（FS 不追溯——如实断言双向）；剩余成员正常解密 |
| N2 | **rejoin 后历史/中间不可读** | 重入 = 新 Add → 新 epoch；离开期间 epoch 无该成员路径秘密 | 重入者对离开期间密文解密失败；对离开前历史（若产品给历史 re-share，本轮不给）失败 |
| N3 | **epoch rollback** | 收到 epoch < 本地当前的 commit/消息：stale，拒绝处理（epoch 单调，GroupContext 比对） | API 返回 stale/wrong epoch 错误；本地 epoch 高水位不变；**回退后同 epoch 不同 transcript 也拒绝**（防回滚+分叉双断言） |
| N4 | **非法 commit** | confirmation tag 不匹配 / proposal 引用不完整 / 树哈希校验失败（§12.4.2 L4412）→ commit 整体拒绝，本地停留原 epoch | 篡改 confirmation_key 输出、删除 proposal、换 leaf 密钥各造一例：三例均拒绝且状态零推进（无半更新树） |
| N5 | **replay** | (a) 同一 commit 重放 → 已是该 epoch → 幂等拒绝；(b) MLSCiphertext 重放（同 generation）→ §9.2 删除计划下旧 ratchet key 已删 → 解密失败 | (a) 不产生新 epoch；(b) 解密错误；另断言应用层 message-id dedupe（现有 crypto_inbox 防线）与 MLS 层防线叠加 |
| N6 | **长离线** | 离线跨 N epoch：按序处理积压 commit 至当前；链中任一环失败（如 N4 注入）→ 从失败点起 fail-closed，不跳过 | 顺序处理后可解最新消息；注入断链后停留在断链前 epoch 并报错；**不静默跳过缺口** |
| N7 | **welcome 异常** | Welcome 引用的 KeyPackage 不匹配/过期、加密 GroupInfo 篡改 → 加入失败 | 新成员 joiner secret 派生失败；不产生半初始化组状态 |
| N8 | **credential 绑定** | Add/Update 中 credential identity bytes 解析失败、或 (uid,did,iv) 的 KT inclusion proof / R1 验签失败 → 拒绝 commit（§3.4 fail-closed） | 三种伪造（错 identity、无 proof、假 root 签名）均拒绝 |
| N9 | **vectors 互操作** | 全部 10 类官方 vectors 通过 | 选定实现（OpenMLS）原生通过；第二实现（Dart 层）对同批文件重算一致（对齐 AC-19 三向一致模式：vector 记录值 = Rust = Dart） |
| N10 | **套件/capability 一致** | `MLS.1` 元数据与实际密文套件不符 → 拒绝（INV-M3） | 伪造元数据错配例拒绝；Megolm 密文不进 MLS 解密路径、反之亦然 |

群规模预算：AC-30 要求「支持群规模满足冻结预算」——现有 Megolm 分发面上限
`_maxRoomKeyEntries=4096`（成员×设备）；MLS commit 体积/处理时延在 PoC 中以
50/200/1000/5000 成员树测得基线数据，回填冻结预算比对（C00/C12 的 SLO 域，
本轮只出数据不出 SLO 结论）。

## 6. 泄露后恢复攻击实验设计（AC-31）

目标：**攻击者持泄露旧 state，诚实成员完成安全更新后不能解密后续消息**；
**旧 Megolm 历史与新 MLS 世代隔离**。全部本地可执行（无网络、无第三方）。

### E1 — MLS PCS（泄露后恢复）实验

```
步骤（Rust 集成测试形态；Dart 层重复）：
1. 建 3 成员组 A/B/C（套件 0x0001），推进至 epoch N，期间发消息 m1..mk（可解）。
2. 攻击者建模：完整导出 B 的 MLS 本地状态副本（加密存储快照 + 内存树状态序列化——
   openmls 提供 state 序列化/反序列化接口；导出面=被盗设备等价物，对齐 T-ST 威胁）。
3. 恢复动作（诚实方）：
   a. B 检测泄露（带外）→ 走 C01 双签名换钥 + KT rotate（iv+1）；
   b. B 提交 Update proposal → 任一成员 Commit → epoch N+1；
   c. （可选变体 e1b：Remove B 旧 leaf + Add B 新 credential —— 换设备场景）。
4. A 在 epoch N+1 起发消息 m(k+1)..m(k+j)。
断言：
A1. 攻击者用步骤 2 副本解密 epoch ≥ N+1 密文：全部失败（TreeKEM path 秘密未达）。
A2. 攻击者用副本解密 epoch ≤ N 密文：成功（FS 上限——如实记录，不宣称向前追溯）。
A3. 诚实三方解密全部成功；epoch_authenticator(N+1) ≠ epoch_authenticator(N)。
A4. （变体 e1b 同断言。）
```

### E2 — 主动攻击者变体（泄露旧签名密钥的伪造 commit）

```
1. 承 E1 步骤 2：攻击者还持有 B 的旧 MLS signature key。
2. 攻击者用旧钥伪造 Update/Remove commit 并投递给 A。
断言：
A5. A 验证 commit 中 credential：identity 的 iv 落后于 KT 高水位（KT rotate 已 +1）
    → N8 拒绝；即使跳过 KT 检查，commit 链 transcript 与 A 本地视图无法衔接 → N3/N4 拒绝。
（此实验证明「静默用旧钥接管群」不可行；检测依赖 C09 目录 + commit 链双重面。）
```

### E3 — 旧 Megolm 与新 MLS 世代隔离（迁移不变量）

```
1. 同一群：Megolm 会话运行中，room key 全量泄露给攻击者（等价物：导出 inbound pickle）。
2. 群执行迁移（migration 草案 §3.2）：停发 Megolm → 建 MLS group epoch 0 →
   过渡 commit 携带 imboy.migration.v1 截止元数据。
3. MLS 域发消息至 epoch 5。
断言：
A6. 攻击者持 Megolm room key + 全部 MLS 密文：解密失败（零换算，INV-M1）。
A7. MLS 成员持 epoch 5 密钥 + 全部 Megolm 密文：解密失败（反向隔离）。
A8. 元数据面：过渡窗口内消息的 e2ee_suite 逐条与实际密文类型一致（INV-M3）；
    旧 Megolm 密文在 MLS 群视图下只以「历史（不可解）」呈现。
A9. migration 112 attestation 数据对 MLS 群零写入（只读边界）。
```

### 实施载体（PROTOTYPE）

- 主载体：OpenMLS Rust 集成测试（`openmls` crate test API：create_group /
  propose_/commit/merge、serialize state），放 mls-research/ 下的独立 cargo
  工程（不进 imboy 主构建）。
- Dart 面重复：openmls_dart（PoC 许可的第三方包）或自建最小 FRB 绑定跑 E1–E3
  断言子集；**不触碰 imboyapp 生产代码**。
- vectors 交叉：N9 用已下载 10 文件（见 `mls-research/vectors-acquisition.md`）。

## 7. 标准 Vectors 获取（摘要）

已完成：mlswg 官方 `test-vectors/` 10 个核心 JSON（~686KB）下载至
`mls-research/vectors/`，SHA256 与复现命令记录于 `vectors-acquisition.md`；
6 个大文件（>0.7MB，共 ~10MB）记录 URL 按需拉取。许可证注意：仓库无显式
LICENSE 文件——本地测试用途为社区普遍实践，**再发布前需法务确认（UNVERIFIED）**。

## 8. 决策卡草案 D-03（MLS 默认启用策略）

| 字段 | 内容 |
|---|---|
| 决策 | MLS 群加密的默认启用策略与生产接线放行 |
| 提案人 | C15 worker（run-20261003-094804） |
| 批准人 | 安全 reviewer + 用户（人工；loop 不得自我接受，对齐 D-02 模式） |
| 批准前置 | ① AC-30/31 本地实验（§5/§6）全绿并有证据卡；② C09 D-02 已批（credential 绑定依赖 KT/R1–R4 实施完成）；③ C12 版本门合同覆盖 MLS 群；④ 绑定层（自建 FRB）代码经独立 review；⑤ SRLabs 审计 8 issues 在选定 openmls 版本中的修复状态逐条核实（当前 UNVERIFIED） |
| 批准后果 | C15 解锁生产接线（App `e2ee/mls_*.dart` + 后端 relay + C12 迁移，均按 lease 串行）；capability/UI 面同步 |
| 不批准后果 | MLS 维持 PROTOTYPE；群加密安全上限维持「Megolm rotation 粒度 FS」如实披露；T-KD 的 MLS 半区维持「不存在」 |

**策略选项**：

| 选项 | 描述 | 迁移成本 | 依赖新增面 | 风险 | 建议 |
|---|---|---|---|---|---|
| **A. per deployment（部署级 opt-in，默认关）** | 部署方配置开启后，**新建群**用 MLS；存量群不迁 | 低（无存量迁移） | 见 §2/selection-matrix §4：openmls crate + FRB 绑定（Rust 2 项/Dart 0 项） | 最小暴露面；灰度可控 | **首启推荐** |
| **B. per group（群级选择）** | 建群时发起者选套件（MLS/Megolm），全员版本门通过才可选 MLS | 中（UI/协商/版本门逐群判断） | 同 A + 群级协商协议位 | 套件碎片化（同部署两代并存长期化） | 第二阶段 |
| **C. 全局默认启用** | 所有新群默认 MLS，存量群按计划迁移 | 高（存量群迁移旅程 + 旧端升级 rollout） | 同 A | 与 AC-25 旧端门强耦合；回滚面大 | 远期目标（视 A/B 运行数据） |

**推荐路径**：A（首个 MLS 生产里程碑）→ 观察（攻击实验重验 + 冻结预算数据）→
B → C。**任何阶段：存量 Megolm 群不强制迁移（INV-M4 单向性只约束已切群）**；
C2C 单聊不切（§1 定位，PQ 归 C16）。

**批准前红线（重申）**：生产 Dart/Erlang 零改动；pubspec 零改动；不部署；
不发起任何对外联系。

## 9. 未做与边界

- **未改任何生产源码/依赖/迁移/配置**；产出全部为本计划目录文档 + 公开 vectors。
- 未实现 Rust/Dart 绑定代码（E1–E3 为设计，执行待批准或明确 PoC 指令）。
- SRLabs 审计逐条清单、mlswg vectors 许可证、matrix SSSCS 状态：UNVERIFIED 已标注。
- 真机验证、群规模 SLO 结论、C12 迁移 DDL 设计：不在本轮（依赖批准与后续卡）。
- D-03 未批前，本文档一切「生产」措辞均为设计意向，非实施授权。
