# C01 — 最小协议与资源预算冻结（CACHE_V221_BACKEND_CLOSEOUT_V1）

> 冻结时间：2026-10-11T13:30Z（Asia/Shanghai 21:30）
> 状态：FROZEN_BEFORE_FAULT_TESTS（收口计划 C01 oracle：故障测试前冻结，失败后不得调高预算获取 PASS）
> 适用：imboy 后端 V2.2.1 缓存同步实现、配置与测试三处必须引用同一摘要。

## 1. 冻结的协议增量（在既有 V2 envelope 之上，向后兼容）

既有 V2 事件字段（W3 已实现）：`protocol=2, operation, namespace, key, value,
expires_at_ms, revision, origin_seq, origin_node, origin_boot_id`。

本轮冻结新增字段（C02 实现）：

| 字段 | 类型 | 语义 |
|---|---|---|
| `epoch` | non_neg_integer() | 同步纪元。初始 0；冷恢复/超窗清理时 +1 |
| `config_hash` | binary() (16B) | 节点缓存同步配置摘要（max_ttl/max_value/batch/recovery_window/tombstone/全部 Nmax..Dmax 预算的规范化序列化 SHA-256 前 16 字节） |
| `registry_hash` | binary() (16B) | Key Registry 静态表（含 overrides 生效后视图）规范化序列化摘要 |

双端校验规则（C02）：
- 发送端：每事件携带本节点 `epoch/config_hash/registry_hash`。
- 接收端：`config_hash`/`registry_hash` 不一致 → 事件整体拒绝
  （`{reject, hash_mismatch}`，计数 `reject_total`），不部分应用、不崩进程；
  该 peer 标记 `hash_divergent`（供 19.8 可观测与 C10 混合矩阵断言）。
- `epoch` 小于本节点 epoch 的复制事件（set/delete）→ 拒绝（`{reject, stale_epoch}`）；
  invalidate 不参与 epoch 比较（与既有"不参与版本比较"语义一致，宁多失效）。
- 缺任一新增字段的结构性校验失败 → `{reject, missing_protocol_field}`，
  与既有 missing_key 等同级别处理。

## 2. incarnation 注册与旧化身拒绝（C02）

- 每节点 sync 进程 init 时生成 `BootId = crypto:strong_rand_bytes(16)`（既有），
  并写入元数据表 `{incarnation, NodeId} = BootId`（本节点当前化身）。
- 元数据表维护 `{peer_incarnation, NodeId} = BootId`：本节点观察到的每个 peer
  的最新合法化身。来源：V2 事件与恢复握手（version_report/snapshot 协议帧）。
- 化身规则（收口修订 r1，2026-10-11 实施）：单条消息无法区分"peer
  重启后的新化身首现"与"超窗旧化身重放"，拒绝新化身会使重启 peer
  的全部事件被永久拒绝。化身层降级为检测/注册/补偿：
  已知最新化身 → ok；宽限集合内旧化身（W）→ ok；全新化身 → 接受 +
  注册为最新（旧化身转入宽限）并返回 changed 信号触发 C05 补偿。
  旧值防复活由 Lamport revision + epoch 承担（原计划 5.3，不依赖化身）。
  自环过滤（W3-10）保持：`origin_node==node() andalso origin_boot_id==本化身` 跳过。
- 节点重启后新化身出现：旧化身进入宽限集合（时间戳登记），宽限 = W = 720s
  （与墓碑保留一致）；超窗清理时淘汰。
- 快照回放（既有设计）：回放事件保留原 origin 与原 boot_id；新增规则——
  若原 origin 是请求恢复节点自身，使用 `<<0:128>>` 伪装 boot_id 绕过自环过滤
  （既有行为保留，登记为合法特例）。

## 3. sync epoch 与冷恢复（对齐 19.6）

- epoch 单调整调：仅两种途径推进——(a) 本节点冷恢复完成时取
  `max(本 epoch, 全部 authority peer 报告的 max epoch)`；(b) 有界资源达到上限
  触发的受控清理（见第 8 节）。
- 超窗成员（分区 > W 或 journal/buffer 溢出）：丢弃受管副本（rindex/rev 行），
  epoch+1，走新快照回合；不能无限增量。
- 旧 epoch 重放（含快照回放）一律拒绝；epoch 不匹配的 peer 不作为 ready
  authority。

## 4. 快照回合：round / token / watermark / page digest（C04）

- 恢复节点发起快照请求时生成 `Round = crypto:strong_rand_bytes(12)`，
  请求帧 `{cache_snapshot_request, RequestorPid, Round}`。
- 应答 peer 发送：分页快照事件
  `{cache_snapshot_page, Round, PageNo, Entries, PageDigest, HasMore}`，
  每页 ≤ Pmax 项且 ≤ Pmax_bytes；页摘要
  `PageDigest = erlang:phash2({Round, PageNo, Keys, Revs})`（32 位）。
  终帧 `{cache_snapshot_done, FromNode, Round, WatermarkCounter}`。
- 接收端校验：Round 匹配（未知 round 的页/done 丢弃并计数）、PageNo 从 1
  连续、每页 digest 复算一致；缺页或 digest 不符 → 该 peer 回合失败，有界重试。
- 闭合条件（READY 判定，C04 核心）：
  1. 全部已知 peer（多节点视图）均回 done(Round)；
  2. 各 peer WatermarkCounter 已并入本节点 counter（版本上界，既有 merge）；
  3. 无失败页。
  三者同时成立才 READY。**不允许仅凭第一个空 done 或超时推进 READY。**
- 快照期间 delta：peer 正常写入照常 publish，与快照页事件按 Lamport 版本
  比较天然融合（既有设计；无 journal 的等价有界追赶论证：
  快照枚举时刻 T 前的 peer 写入要么已入页、要么作为延迟事件按版本比较合入；
  T 后写入按正常事件流到达。无漏事件由"全部 peer done + 版本水位 + 页连续
  性"三重闭合保证）。
- 恢复中不参与可用读/confirmed：`replicate_state/1` 非 ready 返回
  `unverified`（既有），C04 增测试覆盖"全部 peer recovering""多 peer 未应答"
  "孤岛空 syn 视图"三态。

## 5. 错误返回与冷恢复语义（状态转换）

```text
STARTING → DISCOVERING → RECOVERING → READY
                 ↘ (重试>3×单轮) ↘ DEGRADED
DEGRADED: 读=unverified（miss 回权威），replicate 写={error,{not_ready,degraded}}
          退出手段=节点级重启 sync（运维手册既有结论，保留）
```

- 快照回合失败（缺页/超时/digest 不符）→ 重试新 Round（≤ Rmax_rounds=3 轮）；
  仍失败 → DEGRADED。
- 无合法 authority（多节点配置但全部 peer 无应答/全 recovering/epoch 不匹配）：
  保持 RECOVERING 重试至 Dmax，然后 DEGRADED。**禁止从空 done 推导 READY。**
- 单节点显式配置（syn 视图稳定为空）：允许空缓存 READY（5.3 既有）；
  必须与"多节点暂时失联"区分（既有 3 次空重试判定保留）。

## 6. 资源预算（本地工程预算，非生产 SLO）

| 符号 | 含义 | 冻结值 | 单位 |
|---|---|---:|---|
| Nmax | rindex 复制条目上限 | 50,000 | 项 |
| Nmax_bytes | rindex+rev 元数据内存上限 | 16 | MiB（估算） |
| Tmax | 墓碑条目上限 | 20,000 | 项 |
| Tmax_bytes | 墓碑内存上限 | 8 | MiB |
| Jmax | （无 journal；等价追赶窗口=W） | — | 见第 4 节论证 |
| Bmax | 恢复接收缓冲（页事件积压） | 2,048 | 项 |
| Bmax_bytes | 恢复接收缓冲字节 | 32 | MiB |
| Pmax | 快照每页条目 | 256 | 项 |
| Pmax_bytes | 快照每页字节 | 4 | MiB |
| Rmax | 同时恢复节点数（三节点拓扑） | 2 | 个 |
| Rmax_rounds | 单节点快照回合重试 | 3 | 轮 |
| Dmax | 恢复总 deadline | 300 | 秒（=recovery_window） |
| MaxValue | 单值上限 | 32,768 | bytes（既有） |
| MaxTTL | replicate TTL 上限 | 600 | 秒（既有） |
| HighWater | sync 队列高水位 | 10,000 | 条（既有） |
| W | 墓碑/化身宽限保留窗口 | 720 | 秒（既有公式推导） |

预算摘要（配置/实现/测试三方必须一致的规范化摘要，C03 落地为
`imboy_cache_policy:budget_digest/0`）：

```text
budget_v1: Nmax=50000 Nmax_bytes=16MiB Tmax=20000 Tmax_bytes=8MiB
Bmax=2048 Bmax_bytes=32MiB Pmax=256 Pmax_bytes=4MiB Rmax=2
Rmax_rounds=3 Dmax=300s MaxValue=32768B MaxTTL=600s HighWater=10000 W=720s
```

## 7. 达到上限的行为（冻结，不得删除 tombstone 后接受旧事件）

| 上限 | 行为 |
|---|---|
| Nmax（rindex 满） | 新 replicate 写拒绝 `{error, index_full}`（fail-closed，不驱逐既有条目）；invalidate/local 不受影响 |
| Tmax（墓碑满） | 先按时间淘汰已过期墓碑；仍满 → 推进 epoch、清全部受管副本、走新快照回合（19.6 合同），不无限积压 |
| Bmax（恢复缓冲满） | 中止当前回合 → 新 Round 重试；连续失败按 Rmax_rounds → DEGRADED |
| Pmax（单页超） | 发送端拆页；单条目 > Pmax_bytes（>4MiB，不可能：MaxValue=32KiB）拒绝 |
| Dmax（deadline） | 中止恢复 → DEGRADED；不制造成功 |
| HighWater | 拒绝新 replicate 写 `{error, overloaded}`（既有） |

## 8. 权威来源（replicate 值的 ready authority）

- authority 候选 = epoch 一致 ∧ hash 一致 ∧ status=ready 的 peer。
- 失效孤岛（epoch/hash 不匹配或超窗）不是 authority。
- 全节点丢失（无任何 authority）：DEGRADED，读 miss 回权威数据源；
  不从空 peer done 或本节点残留旧值推导 READY。
- `cluster_node_info` 等 local 类不受影响（本地重建）。

## 9. 与既有行为的兼容边界

- legacy 消息语义不变；`cache_sync_protocol` 默认 legacy 不变；
  生产白名单空、confirmed=false、fenced_namespaces=[] 不变（W0 决策）。
- `put/4`、`invalidate/1`、`put_delete/1` 公开接口签名不变。
- 新增字段只在 V2 envelope 内；legacy 信封零变化（C10 混合矩阵用）。
- 894 行 sync 模块按职责最小拆分（C03）：`imboy_cache_sync.erl`（gen_server
  主体/恢复状态机）+ `imboy_cache_sync_proto.erl`（envelope 校验/epoch/
  incarnation/hash）+ `imboy_cache_sync_snapshot.erl`（分页快照/回合）；
  全部 < 800 行，exports 先冻结后消费。

## 10. oracle

- 配置/实现/测试引用同一 `budget_digest/0` 摘要（C03 落地，C12/C14 校验）。
- 本文件冻结后，任何预算调整 = 重新冻结 + 受影响证据全部重跑（19.7）。
- 这些值是本地工程预算；生产 SLO 输入仍缺失（AC-26/27 保持 BLOCKED_EXTERNAL）。
