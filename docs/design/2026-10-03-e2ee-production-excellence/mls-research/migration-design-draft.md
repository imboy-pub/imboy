# C15 — Megolm → MLS 迁移设计草案（epoch/commit/welcome/fork ↔ generation/conv_seq/attestation 映射）

> run: run-20261003-094804 | card: C15 | 状态：**草案（D-03 未批）**，不接生产、不改生产源码。
> 现状基线：imboyapp `group_session_service.dart`（vodozemac Megolm，发送端每会话域
> outbound、100 条/7 天/重启 rotate、room key 经 Olm 逐设备包裹分发）+
> migration 112（`e2ee_group_session_attestation` / `e2ee_group_session_member`：
> server-authoritative session 起止 conv_seq、recipient 快照、成员 generation_no）。
> RFC 9420 引用以 rfc-editor txt（L 行号 = 本轮下载副本行号）。

## 1. 状态模型映射总表

| 概念 | Megolm 现状（文档确证） | RFC 9420 对应 | 映射与差异说明 |
|---|---|---|---|
| 会话标识 | `session_id`（Megolm session 唯一） | `group_id`（GroupContext，§8.1 L2898） | 迁移时新 group_id ≠ 旧 session_id 命名空间；**禁止复用**（隔离要求，见 §3） |
| 世代计数 | `generation_no`（member 维度，migration 112） | `epoch`（uint64，GroupContext 内单调） | MLS epoch 是**群级全局**单调；Megolm generation 是**成员维度快照**粒度。语义近似但粒度不同：一次 commit（含 N 个 proposal）只 +1 epoch |
| 消息序 | `conv_seq`（服务端 staging 事务内权威固化；start/end_seq 进 attestation） | MLSCiphertext 内 `content` + Secret Tree 派生的 per-sender ratchet key（§9.1 L3311）；无全群连续 seq | MLS 没有全群连续明文序号——**展示层排序仍依赖服务端 conv_seq/staging**；MLS 层用 (epoch, sender leaf, generation) 定位。服务端 attestation 的 start/end_seq 在 MLS 模式下保留为**传输/排序元数据**，不再承担密码学授权（授权由 epoch 链承担） |
| 密钥变更 | rotate：新建 session + 全量重分发 room key（成员变化/100 条/7 天/重启触发） | Commit（§12.4 L4128）：proposal 批（Add/Update/Remove/PSK…）→ 新 epoch secret → confirmation tag 认证 | MLS 每次成员变化天然走 commit（无需全量重分发——TreeKEM 增量路径加密）；rotate 的「阈值触发」（100 条/7 天）在 MLS 中对应**无成员变化的 Update commit**（self-update path 刷新树密钥） |
| 新成员加入 | 新成员获当前 session room key（从加入点可解）+ 历史按有限 grant（D3） | Welcome（§12.4.3.1 L4572）：新 epoch 起 KeyPackage 持有者可解；**协议层无历史可读语义** | MLS 天然满足「新成员不可读历史」（除非应用显式 re-share 历史密钥——本 profile 不做） |
| 离开/踢出 | 成员集变化 → rotate 新 session；**旧 room key 仍可解 rotate 前密文**（审计 §3：rotation≠PCS） | Remove proposal + Commit → 新 epoch；被移除者持旧 epoch state **无法派生新 epoch 密钥**（TreeKEM 前向安全 + §9.2 Deletion Schedule L3387） | 这是 MLS 相对 Megolm 的核心安全增益：per-epoch PCS（AC-31 的协议基础） |
| 重入 | upsert 保留旧 created_at，无 join epoch 边界（审计 §3 记录的设计缺口） | 重新 Add → 新 leaf（新 KeyPackage）→ 新 epoch 起 | 离开期间 epoch 不可解、离开前历史不可解（两段隔离）——**顺带修复 Megolm 的历史边界缺口** |
| 设备密钥轮换 | 换设备 = 成员集变化 → rotate | Update proposal（§12.1.2 L3814）替换自己 leaf 的 HPKE/签名密钥 | Update commit 后旧密钥副本即失去派生能力（同 PCS 语义） |
| 分叉检测 | 无（session_id 无链式认证） | confirmed/interim transcript hash（§8.2 L2956）+ confirmation tag；分叉 = 同 (group_id, epoch) 不同 transcript | 客户端可本地检测分叉视图（对齐 C09 §4.3 KT 防分叉的群内版） |
| 视图一致性证明 | 无对应物 | `epoch_authenticator`（§8.7 L3245，Table 4）：同视图客户端计算值相同 | 可作为群级安全码输入（见 mls-profile §3.4） |
| 离线恢复 | 有限 grant 区间 + 同 generation 在线延展（D3 恢复合同） | 长离线：按序补处理 commit 队列至当前 epoch；resumption PSK（§8.6 L3236）跨 epoch 续证 | MLS 离线 = commit 追赶；KeyPackage 过期后需重新发布 KeyPackage（服务端可缓存） |
| 群重建 | 无对应（重建=新 session） | ReInit（§11.2 L3655）/ Subgroup Branching（§11.3 L3692）/ External Commit（§12.4.3.2 L4759） | fork 语义：ReInit=全体重建（可用于「灾难恢复一键换钥」）；Branching=子群分叉；External Commit=无分发通道的外部自救加入（本项目暂不启用 external senders，见 profile §3.5） |

## 2. 五个生命周期事件在两套协议下的状态对应

### 2.1 join（加入）

| 阶段 | Megolm | MLS |
|---|---|---|
| 发起 | 群成员表变更 → 发送端标记 `_staleGids` | Add proposal（携带被加者 KeyPackage 引用）进入下个 Commit |
| 密钥面 | 下一次发送前 rotate → 全量重分发（新成员也在列表） | Commit 产生新 epoch；组内现有成员得 update path；新成员单独收 Welcome（加密 GroupInfo + 必要路径秘密） |
| 历史 | 新成员可解当前 session 起点；更早历史仅当有 grant | 新成员只能从新 epoch 起解；历史 epoch 密文协议层不可解 |
| 服务端 | attestation 记录 recipient 快照 + start_seq | 服务端仅 relay proposal/commit/welcome（顺序敏感，见 §4）；不做密码学授权 |

### 2.2 leave / kick（离开/踢出）

| 阶段 | Megolm | MLS |
|---|---|---|
| 发起 | S2C join/leave 标记 → stale → rotate | Remove proposal（指明被移除 leaf）→ Commit |
| 密钥面 | 新 session 分发给剩余成员；**被移除者持旧 key 仍可解旧 session 全部** | 新 epoch 密钥经不含被移除者路径秘密的树派生；被移除者旧 state 无法解新 epoch |
| 边界 | 审计 §3：撤销保密依赖「传播→快照→下一条消息前轮换」完整成立 | 协议保证（无需依赖轮换时序）；服务端延迟投递 commit 只延迟保护生效，不破坏一旦生效后的隔离 |

### 2.3 rejoin（重入）

Megolm：upsert，历史边界缺口（审计确认）。MLS：Remove（离开时）→ Add（重入时）两次
commit；中间 epoch 对重入者不存在可解材料。**若 Megolm 侧在迁移前仍需处理重入，
维持 D3 有限 grant 合同；迁移后群以 MLS epoch 为唯一边界。**

### 2.4 撤销 + 泄露恢复（AC-31 核心场景）

| 动作 | Megolm | MLS |
|---|---|---|
| 检测泄露 | 人工/安全码变化发现 | 同左 + epoch_authenticator 比对 |
| 恢复 | rotate 新 session（**攻击者持泄露 room key 仍可解泄露 epoch 内全部消息**——审计边界） | 受害者 Update（换 leaf 密钥）或 Remove+Add（换 credential）；此后攻击者旧 state 无法解任何新 epoch（PCS）；旧 epoch 消息仍暴露（FS 上限，如实披露） |
| 全群自愈 | 无（只轮换发送者 session） | 任一成员 commit 后全群前进；可选全员 Update commit 模式（policy 决定） |

### 2.5 长离线（离线数个 epoch 后回归）

Megolm：grant 区间外需在线延展（同 generation 才接受）。MLS：拉取积压 commit 按
epoch 顺序处理到当前；处理链每步验证 confirmation tag/transcript，断链即 fail-closed。
若离线时长超 KeyPackage 有效期，需重新发布 KeyPackage 才能被后续 Add（不影响已在线
成员）。

## 3. 迁移期双协议并存边界（AC-31 条款）

### 3.1 不变量

- **INV-M1 历史隔离**：旧 Megolm 密文只能用旧 Megolm room key 解；新 MLS 密文只能用
  MLS epoch 密钥解。**两套密钥材料零换算**（不把 Megolm room key 注入 MLS PSK、
  不把 MLS secret 导出给 Megolm 栈）。MLS resumption PSK 只从前序 MLS epoch 派生
  （§8.6），首 epoch 的 PSK 注入面**关闭**（external PSK 预留 id `imboy.mls.migration`
  仅当显式产品决策「历史连续性优先」时才评估，默认不用）。
- **INV-M2 新建群无继承**：MLS group 从 epoch 0 全新建立，group_id 新生成
  （建议 `imboy-mls:<gid>` 域前缀 + 随机），不携带任何 Megolm 派生状态。
- **INV-M3 capability 一致**：`e2ee_suite` 元数据扩 `MLS.1`（对齐现役 `MEGOLM.V1`
  常量模式）；**消息的实际套件必须与元数据一致**（Megolm 密文标 MLS 或反之 =
  验证失败拒绝，AC-31「UI/API capability 与实际协议一致」）。
- **INV-M4 灰度单向**：群一旦切 MLS，不回切 Megolm（回切=密钥面倒退，违反
  AC-25 回滚不降级精神）；客户端版本门（C12）保证群内全员支持 MLS 才切换。

### 3.2 群状态机（迁移期）

```
纯 Megolm（现状）──迁移提议──▶ 双栈过渡（Megolm 只读收尾 + MLS epoch 0 建立）──▶ 纯 MLS
     ▲                                   │
     └──（禁止回切，INV-M4）              └─ 过渡窗口：发送侧立即停发 Megolm 新消息；
                                           接收侧旧 Megolm inbound 保留只用于解历史
```

- **过渡原子性**：切换 commit（携带 GroupContextExtensions 声明 `imboy.migration.v1`
  元数据：旧 session_id 终点 conv_seq）由发起端在停发 Megolm 之后发出；接收端以
  该元数据为「历史查找截止线」（展示层拼接两条时间线，密码学上互不可解）。
- **旧 room key 处置**：保留（历史解密能力是产品需求，C08 边界）；不进入 MLS 存储
  域（键前缀 `mls_*` 与 `megolm_inbound_*` 分离，对齐 C05 policy-matrix #9 预留行）。
- **服务端面**：MLS 分发复用 per-group 保序通道（现有 staging 事务的 conv_seq 顺序
  语义）；新增 relay 动作（`mls_proposal/commit/welcome` 或统一 `mls_message`）
  承载 MLS wire bytes，服务端不解析内容、只保序投递——具体 API 归 C12/C08 lease，
  本卡不写生产代码。

### 3.3 明确不迁移的域

- C2C 单聊（`c2c:` scope）当前也走 Megolm 2 人房（group_session_service 注释）。
  MLS 单聊切换是**可选后续**（MLS 2 人组即退化树）；默认 C2C 维持 Olm/Megolm，
  避免一次改动面过大。PQ 单聊归 C16。此边界写入 D-03。

## 4. 服务端角色变化（设计声明，不实施）

| 职责 | Megolm 现状 | MLS 迁移后 |
|---|---|---|
| 排序/线性化 | staging 事务权威 conv_seq + attestation | **保序 relay**（commit 链顺序不可乱）；conv_seq 继续作为展示/存储序 |
| 授权 | attestation recipient 快照 + generation 边界 | **降级为元数据**；密码学授权由 MLS epoch 链承担（服务端无法伪造它不持有的密钥） |
| 密钥分发 | 透传 Olm 包裹 room key 帧 | 透传 Welcome/commit（同样不透明字节） |
| 历史授权 | D3 有限 grant | 展示层查询继续用现有 membership 检查；**MLS 群不再有「历史 grant」概念**（不可解就是不可解） |

migration 112 的两张表在 MLS 群上**只读不写**（历史 Megolm 会话的 attestation 数据
保持有效）；MLS 群的等价审计面（epoch 摘要记录）若需要，属新迁移 + C12 lease，
本轮仅记录需求不设计 DDL。
