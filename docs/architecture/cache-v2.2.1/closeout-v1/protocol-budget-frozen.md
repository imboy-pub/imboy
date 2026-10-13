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
  `{cache_snapshot_page, FromNode, Round, PageNo, Entries, PageDigest, HasMore}`，
  每页 ≤ Pmax 项且 ≤ Pmax_bytes；页摘要
  `PageDigest = crypto:hash(sha256, term_to_binary({Round, PageNo, Entries}))`（32 字节）。
  live 条目 `{Key, Value, ExpiresAtMs, Rev}`，墓碑
  `{deleted, Key, undefined, ExpiresAtMs, Rev}`；KindKeyRevs 按页顺序记录
  `{live|deleted|expired|unavailable, Key, Rev}`。墓碑按原绝对期限恢复，较旧版本跳过，相同版本不续期。
  过期版本下限为 `{expired, Key, undefined, OriginalExpiresAtMs, Rev}`，只在原期限加 W
  的保护窗口内传递，不携值、不合成 DELETE、不修改原期限；内部保留原 live 版本行。
  剩余期限不足以安全复制的 live 页条目也保留同一下限；过保护窗口则整页拒绝。
  接收校验包含来源的 5 秒时钟余量、秒截断和接收端最多 5 秒落后，未来期限容差
  约 11 秒且有界；该容差不延长值或下限的原期限。
  页与 journal 识别此类型；发送前检查窗口期限。同版 live/下限的期限冲突整页拒绝，
  同版 deleted 不被下限改成 live，旧下限不清更新值，更高版本仍可正常覆盖。
  值被淘汰而版本仍保留时发送 `{unavailable, Key, undefined, OriginalExpiresAtMs, Rev}`。
  该下限沿原期限加 W 的窗口、MaxTTL 和 N/bytes 预算传递；较新下限清旧值，
  同版可用值保留或按原期限补回，两种来源顺序一致。缺值不等于 DELETE。
  整页同时预检 Nmax/Nbytes/Tmax/Tbytes，重复行逐预算维度保守取峰值，不借用缩减。
  终帧 `{cache_snapshot_done, FromNode, Round, WatermarkCounter, TotalPages}`。
  WatermarkCounter 不得低于该来源所有已接受页的最高 live/墓碑 counter。
  合法页立即并入本地时钟；随后 abort 保留该上界，不能只在 done 时推进。
- 接收端校验：Round 匹配（未知 round 的页/done 丢弃并计数）、PageNo 从 1
  连续、每页 digest 复算一致；缺页或 digest 不符 → 该 peer 回合失败，有界重试。
- 闭合条件（READY 判定，C04 核心）：
  1. 全部已发现 peer 完成应答分类，至少一个有效 READY 来源，全部已授权来源都有合法 source-cut 和 done；
  2. 各授权来源 WatermarkCounter 已并入本节点 counter；
  3. 无失败页。
  三者同时成立才 READY。**不允许仅凭第一个空 done 或超时推进 READY。**
- 快照期间已到达 delta 与页条目按 Lamport 版本比较合并。页连续、done 与 counter
  上界不能证明 delta 未丢失；缺口已触发有界快照恢复，快照 cut/增量完整性及超过墓碑保留窗
  的跨来源删除证明仍需实现并取证，不能把来源自身确认或上述回合闭合作为完整 C04 PASS。
- 恢复中不参与可用读/confirmed：`replicate_state/1` 非 ready 返回
  `unverified`（既有），C04 增测试覆盖"全部 peer recovering""多 peer 未应答"
  "孤岛空 syn 视图"三态。

握手帧修订（收口修订 r2，2026-10-11 实施）：版本上界握手帧由"应答方
syn 视图反查请求节点成员"改为 pid 直达——恢复节点刚 restart_sync 换 pid
时，syn 视图传播滞后会使反查漏发或把应答发向陈旧死 pid，恢复轮被系统性
饿死（C09 F5 三节点实证：`sync_counter=0` 卡死至超时）。具体：
`version_request` 帧扩为 `{cache_version_request, FromPid, FromNode}`
（旧 2 元组仍被应答，走视图反查，兼容 C10 混合矩阵旧节点）；
`version_report` 帧扩为 9 元组（尾增发送者 sync pid）；
hash 一致分支的快照请求直达 report 携带的 pid（与请求帧同源竞态一并消除）。
预算与闭合条件不变；本修订只改帧的寻址方式，不改任何判定逻辑。

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
- 单节点配置（默认 `cluster_nodes=[]`，syn 视图稳定为空）：允许空缓存 READY；
  `cluster_nodes` 非空时空视图仍按多节点恢复，不能以 3 次空重试替代权威来源。
- 发现应答屏障与快照屏障分开：全部已发现对端先完成分类，非 READY 对端不提供
  快照；至少一个有效 READY 来源、全部已选来源完整 done、期限内才签 READY。
  done 的总页数必须等于实际页数，且无 `expected_more`；矛盾重复终帧中止。
- 已选来源的 Boot/能力/hash 变化中止回合；清已应用 live 值，保留 revision/墓碑
  拒绝旧事件。新回合合法同版本快照可补回缺失 live 值，不覆盖同版本墓碑。
  恢复定时器按引用识别并取消旧轮；abort 重新安排发现，Dmax 到期进入 DEGRADED。

冷清理的运行时入口先关闭 READY 屏障，再推进 epoch、清理副本与 metadata，
重设 Dmax 和恢复定时器。冷恢复必须获得与新 epoch 相同、hash 相同的 READY
authority，并完成全部已发现来源的快照回合；旧 epoch 的 READY 不能回填。
即使显式单节点配置，也不能把冷清理后的空缓存按首次启动规则直接签 READY。
无合格来源时到期进入 DEGRADED；恢复完成才重新开放复制读写。
READY 的 epoch 只采纳本轮实际授权来源，精确取最大值；已排除来源不能参与。
该规则未实现跨节点自动 epoch 提升协议；旧 epoch 来源不会被自动升级为 authority。

授权来源绑定其报告 epoch；后续报告即使 Boot 与 hash 不变，只要 epoch 改变也中止回合。
已 done 的来源同样受该绑定约束，不能将旧 epoch 页随新报告提升后参与 READY。
合法 V2 事件先于报告到达时，若其 Boot 是当前授权 Boot 且 epoch 更高，也中止旧回合；
宽限旧 Boot 和旧 epoch 不触发该新增条件，化身变化/epoch 变化与同帧缺口只计一次中止。
不同初始 epoch 的混合曾在受控回调 fixture 复现：epoch 0 页与 epoch 1 空终帧共同
签 READY 并保留旧代值可读。现通过每条数据代次绑定和同代来源选择阻止此路径；
纯回调验证不替代真实节点、完整切点或应用验收证据。
已实现的基础合同：版本记录绑定数据 EntryEpoch，本地写取本地 epoch、事件取原事件 epoch、页取
授权来源绑定 epoch；所有 live/墓碑写入与 equal 补回按 `{EntryEpoch, Rev}` 比较，读须匹配
已完成恢复的本地 epoch，预算投影计入该字段。高 epoch 事件在 READY 节点先关闭读写屏障。
低于全局数据 epoch 下限的写入统一拒绝，包括尚无版本记录的键。
全部冻结来源完成报告分类后才选择最高合法 READY epoch 的同代来源；目标不得低于已接受
数据 epoch。切换在屏障内只清低 epoch 行和值，保留目标及更高 epoch 的增量、墓碑、版本、
Lamport 时钟和来源序列；不能清掉已确认数据却保留其序列确认。新行布局要求新 Boot 升级，
不支持旧布局直接热加载。选定后来源报告或合法数据出现更高代次会中止旧回合；
来源缺失或全部低于数据下限时不能开放读写，保持原期限并到期降级。
数据绑定、冷清理及同代选择已有针对性验证；完整恢复切点与增量闭环仍未验收。

合法远端 SET/DELETE 因元数据或墓碑预算不足而不能应用时，不能确认成功并保持 READY。
该路径复用冷恢复，目标 epoch 严格高于被拒事件 epoch，避免来源离线后由旧副本恢复旧值。
恢复中再次拒绝保留原 Dmax；只有目标代次的合格 authority 才能回填，没有来源则到期
DEGRADED。该隔离不等于自动重建新代 authority 或恢复可用性完成。

### 快照尾部序列屏障

原计划 §19.6 要求 bounded journal 窗口。当前已实现发送会话期间的版本变更捕获，
以单写者维护递增位置、覆盖下限、项数和外部序列化字节；无发送会话时清空记录。
默认 Jmax=2048 项、Jmax_bytes=32 MiB，另受墓碑保留时间限制。窗口丢失、来源 Boot
或状态变化会中止发送；捕获失败中止会话，不将已成功写入报为失败。诊断排除日志全值。
日志内部记录现为 `{CapturedAt, Epoch, Entry, Origin}`；正常本地 SET/DELETE 的
Origin 来自实际分配的 `{Node, Boot, Seq}`，远端 SET/DELETE 来自已校验事件；
过期但仍在保护窗内的 SET 保留版本下限时同样捕获该来源。旧写入口及无来源的快照
条目仍为 `unknown`，禁止从 Lamport counter
猜造来源序列。整行字节计费包含来源证据；新布局需统一重启，不兼容旧日志热加载。
已知来源的日志重放条目现以 `{with_origin, {Node, Boot, Seq}, Entry}` 携带证据，
未知来源仍用原条目。页摘要绑定来源，完整页字节预算包含包装；接收端要求格式
合法；带历史来源注记的页还要求当前快照来源的 Boot/epoch 与 requested authority
绑定一致。历史 Origin 不是页授权主体，不凭页登记未知化身。无授权上下文的
来源条目接口仍要求已确认、有效宽限或自身 Boot。预算预检保留来源并复用实际
版本行构造器；接收端实际版本写入将来源传入逐键槽位及日志捕获，二次
重放可保留原始 Boot/seq，低版本条目不改写已胜出版本的证据。同版缺值补回
仍保留原期限。基础快照也保留逐键来源槽中的 Boot/seq，不依赖日志会话；
未知槽位不猜造证据。来源注记未推进 foreign floor 或连续覆盖，不构成完整
枚举、缺席、删除或 READY 证明。
接收 unavailable 下限的实际新版本写入现直接捕获 unavailable 日志，保留无值
类型、期限及来源；逐键仍为 live 下限并按 N/rindex 计费，不变成墓碑。它不为
普通 live 值意外丢失提供替代内容，后者仍中止捕获；同版或旧 unavailable 仍跳过。
捕获已知来源时现独立校验条目授权/结构与来源格式、4 KiB 预算，并要求来源节点
等于条目 revision 的节点；Boot 必须为 16 字节，来源序列为正整数，不要求等于
Lamport counter。不可信证据中止发送会话，不回滚已写版本或报假写失败；该校验
不登记来源化身，也不替代成员握手、完整枚举或接收端覆盖提交。
真实 depcache/sync 组件专项已校验本地 SET/DELETE 的实际 Boot/分配序列，以及
远端 live SET、expired SET、DELETE 经同步 actor 消息队列后的来源记录和过期值
不可读性。来源登记及日志会话为受控 fixture，该专项不证明 syn 准入或三节点资格。
基础页之后使用既有分页协议重放连续窗口中的版本更新、删除与新增键，保留版本和绝对期限；
预读及剩余条目受发送游标预算约束。重放期间的新版本继续捕获，过期或非法日志条目中止回合。
预读页在实际发送前再次检查绝对期限；等待拉取期间过期的条目中止原回合，
不发送来源确认或 done，不因曾经预读成功就沿用过期 payload。
最后页与空日志检查在同一来源单写者回调内完成；这仅证明该来源已捕获的版本数据发送完成。
失效事件覆盖和跨来源完整切点确认尚未实现，C04/AC-19 仍未闭合。
恢复完成在写入 READY 前再次推进统一 fenced 栅栏，恢复期间启动的 loader 也必须
验证新代次并重试；此前只在恢复开始推进栅栏不能保护此类回填。此保护仅适用于
已启用 fenced 的命名空间，不等于全部业务失效钩子或跨来源失效水位已验收。
W、TTL 和页缓冲均不能替代丢失增量的覆盖证明，旧“等价追赶窗口=W”结论撤回。
来源分配事件时同时保存 State 与 ETS 的 `sync_origin_seq`，同 Boot 内冷清理不重置序列。
该屏障只覆盖来源自身序列，不能证明其接收的第三方增量形成完整切点。后续跨来源
确认须绑定来源身份、Boot/epoch 与回合，并验证有界水位集合的完整性；未知化身或
未验证必需项不能忽略后签 READY。已观察最大序列与连续应用/完整快照覆盖须区分。
当前新增 `{covered_seq, {Origin, Boot}}` 连续应用下限，值为 `{Epoch, Prefix}`，
与最大已见 `{seq, ...}` 分开。高代切换重置前缀，低代迟到不推进；来源序列
不会随 epoch 重置，故新代前缀可能长期为零，须由后续快照证明补足，不能填成 maxSeen。
旧整数行不具代次证明，指标输出 -1；新行格式要求清理/重启，不宣称旧元数据热加载兼容。
成功应用或已有更高版本支配的拒绝仅推进相邻序列；其它失败、跳号和宽限旧 Boot
不推进。换 Boot 删除旧下限，清副本或采纳 epoch 重置下限；每个已登记来源至多一行。
乱序后续事件不会自动拼接到新下限，当前是保守欠确认。快照覆盖尚未并入，
READY 尚未使用该记录，不能将它或 maxSeen 直接赋值为完整跨来源证明。
来源侧 `application_coverage_candidate/0` 现可读取有界应用证据候选：最多
peer_max+1 项探测，输出受既有 4 KiB 元数据预算限制；每个当前登记来源须有
当前 Boot、同本地 epoch 的连续前缀。缺项、旧格式、错代及预算超限整份拒绝，
不删项凑完整。空登记集只返回空候选，不证明来源自身或未知第三方已覆盖。
来源单写者现将候选结果捕获到现有快照游标，与 Round、来源 Boot/epoch、摘要和
journal_start 绑定；游标整行发送缓冲预算包含候选。后续前缀推进不改写已捕获
结果，失败结果原样保留，不能将它替换成空覆盖证明。候选现随尾部切点发送并暂存
在接收回合内；尚未接入 READY 或 foreign floor，也未证明原始 Boot/seq 与完整枚举对应；不能将候选、独立
并发读取或仅应用证据解释为枚举/缺席/删除证明。
已捕获成功候选的游标在来源身份集合新增、化身改变、应用证据代次改变或覆盖
记录撤销时，通过既有 abort 通知撤销并释放日志会话；正常同代前缀推进不撤销。
预算拒绝登记不改变身份集合；失败候选从未声明覆盖，不被这条候选撤销路径提升。
受控复现进一步确认边界：登记 A、B 化身后，仅完成 B 的空快照回合并安装
B 自身序列下限；A 无对应下限，其合法同代 seq=1 SET 经真实应用引擎仍被
接受并写入真实 depcache。这不是网络或历史删除复现，不能据此宣称生产已
发生旧值复活；但证明 B 的来源自身确认没有给第三方 A 建立防重放覆盖。
完整闭合仍须来源身份绑定的有界覆盖证明，不能把 B 的空快照解释为 A 的删除证明。
最后一页之后、done 之前，由同一来源同步进程发送当前事件序列水位。接收端沿用缺口处理：
水位超过已接收序列则中止本轮、清理 live 值并重试；随后的旧 done 不能签 READY，Dmax 不延长。
随后发送来源自身序列确认及覆盖候选帧
`{cache_snapshot_cut, FromNode, Round, Boot, Epoch, Seq, ConfigHash, RegistryHash, Candidate}`。
Candidate 为原捕获的 `{ok, [{Origin, OriginBoot, Epoch, Prefix}]}` 或明确的 error：
`coverage_peer_budget`、`coverage_byte_budget`、`incomplete_application_coverage`。
向量按 Origin 唯一、规范排序，Boot 为 16 字节、Prefix 非负、代次与切点一致，
数量受 peer_max 限制；整体帧仍限 4 KiB，不能只限制向量。发送端整帧超限明确
中止，不发送成功切点或 done。接收保留完整帧，相同头部但候选不同仍属冲突。
旧八元组仅保留无候选的来源自身切点语义，解析为 missing_application_coverage，
不能补造空候选；实际新发送路径均携带候选。空候选也不证明全部必需来源已覆盖。
事件的 revision 节点必须等于 origin_node；不一致以 revision_origin_mismatch
在版本、来源序列及覆盖记录写入前拒绝，避免应用前缀与被保存版本来源脱节。
帧结构与 4 KiB 元数据预算先校验；只有本回合已请求的 authority、绑定 Boot/epoch、
当前登记化身、当前来源代次和两项摘要全部一致且未超期限，才记录确认。未知必需化身、序列缺口或
低于已见序列的确认均中止；合法相同确认幂等但仍检查最新下限，冲突确认拒绝。
旧回合、未请求或无关来源不推进确认。done 和回合闭合均强制要求来源确认；
不能因无 Round 水位被拒绝就跳过必需确认。新增能力通过配置摘要与旧候选隔离。
最终开放 READY 前重新检查全部 authority 已接受确认的身份与摘要，避免前一来源
确认后重启仍沿用旧确认；失效时中止原回合且不延长 Dmax。最终身份复核与首次确认
复用同一校验；已正确应用的后续增量不要求其序列退回到 cut。
这保护该来源未送达的最后一个 delta，不证明跨来源完整覆盖或超过墓碑窗口的删除闭合。

完整回合在开放 READY 前安装 `{snapshot_seq_floor, {Origin, Boot}}`，取各 authority
自身已确认 cut 的最大序列。后续该化身 SET/DELETE 的序列不高于此下限时，在登记
化身之前拒绝，即使逐键版本已回收或值已淘汰也不能由这些旧帧回填。
安装复核回合、来源 epoch、当前摘要与登记 Boot；来源 epoch 的采纳由恢复流程完成。
旧 Boot 下限仅在有效宽限期内保留，随宽限清扫；不证明第三方来源覆盖、未知新 Boot
或宽限期外旧化身的重放安全。需要 authority 或已有未完成回合时，成员视图暂时变空
也继续等待有效来源，不走新建单节点 READY 分支，总 deadline 不延长。
回合完成及单节点回退入口共用完整回合和数据代次下限判定；完整低代快照不能
安装完成下限或开放 READY。首次无回合单节点启动仍由发现入口核对数据代次下限。

此前化身登记已复现缺口：将旧 Boot 宽限记录置于 W 边界并执行真实
`prune_grace/1` 后，旧 Boot 的合法同代 SET 经 `check_consistency/2` 返回
`{ok, changed}`，同时把当前 `peer_incarnation` 从新 Boot 改回旧 Boot。
当前候选已让事件及序列收据停止自行登记未知化身，拒绝 payload 并进入有界身份
恢复；化身由成员握手登记。身份恢复允许通过合法握手选择最高代次，预算冷恢复
仍要求目标同代来源；事件自身不能提供代次授权。旧 Boot 回退定向回归通过，
已确认来源的组件与指标定向回归已通过；合成身份由真实报告处理逻辑建立，不能
算作 syn 成员握手验收。新增三节点受控专项已验证高代未知事件先到后的完整恢复：
真实节点重启换 Boot，接收 actor 明确拒绝目标 payload、保留旧身份并进入身份恢复；
放行后由真实成员报告及分页/cut/done 完成 READY，原版本/目标代次/完成下限一致。
该场景是受控准入注入加真实网络恢复，不证明自然乱序概率或三机资格；跨来源
完整覆盖及 19.6 全项门禁仍未签发。

健康三节点流的驱动前置现包含真实 syn 视图及六项相互 Boot 确认，使用实际
reconcile/report，不注入身份或重启。单节点 READY 不等于互相身份已确认；
握手前收到未知事件可能触发恢复，该生产启动窗口可用性仍须独立验证。
当前候选在安装完成下限并恢复 READY 后，复用版本报告向远端 syn 成员通知当前
身份；成员数不超过 peer_max 才发送，超限转交既有校对入口，不新增进程或定时器。
三节点重启后无数据写入、无显式对端校对的场景已通过；周期校对仍可能参与，
该结果不证明通知必达，也不替代并发启动及 syn 视图滞后的可用性验证。
握手入口已先加固：报告携带的 FromPid 必须是当前 syn 组中的确切 PID；恢复中
还要求其节点属于冻结回合。仅节点名称相同或旧 PID 曾属于该节点不能登记 Boot。
该检查依据当前本地 syn 视图；视图滞后可能暂留旧 PID 或暂缺新 PID，不能单独
证明全部旧进程报告重放已阻止。暂缺新 PID 的可达性依赖既有有界重试。
成员验证不提供消息签名或挑战 nonce，不能宣称防范受信任 Erlang 节点伪造内容；
该成员校验与事件准入共同构成当前修订，仍待最终集成验证。

### 快照拉取窗口

接收端一次只请求一个 authority，页应用成功后发送
`{cache_snapshot_pull, RequestorPid, Round, NextPageNo}`，该来源 done 后再启动下一来源。
页与 done 只有在本轮实际请求过该来源时才能推进；sender 只接受游标记录的下一页号。
Bmax/Bmax_bytes 限制在途页，不再限制整轮已应用总量；N/T 预算继续限制实际副本存储。
每个来源保留至多 8 个发送游标，全部游标序列化字节合计不超过 Bmax_bytes；
每个接收 PID 只保留一个回合，替换旧回合或 Dmax 到期清理；来源 epoch、配置与 Registry
摘要必须仍与建立游标时一致，否则中止。游标含后续 Key 与一页预读，
不预构造全量值；计数页序替代逐页集合。这里没有签发邮箱或 ETS 实际内存硬上限。
切点/增量完整性和超过墓碑保留窗的删除证明仍须单独验证，不能由流控通过推定。

## 6. 资源预算（本地工程预算，非生产 SLO）

live 值过期后，逐 Key 版本与 rindex 继续保留至 `ExpiresAt+tombstone_ttl_sec`。
较旧 SET 可能有更长 TTL，节点总 counter 不能替代逐 Key 下限；立即清扫会接受该旧值。
留存不延长值的有效期、不合成墓碑，仍计入 Nmax/Nbytes，过窗后统一回收并扣减账目。
因此短 TTL 高写入负载也须按保留窗口测量元数据容量；本地留存不等于冷恢复或
缺失来源后的版本覆盖已经闭合。
来源已有的过期下限现可通过显式 expired 条目传递给冷接收端；该传递仍不证明
缺失来源、快照期间所有自然过期变化或超过保护窗口的历史覆盖。

### generation 索引增量合同

generation 与依赖 generation 合计最多 `n_max` 项；序列化条目字节最多
`gen_max_bytes`，默认 6,400,000 bytes（`n_max=50,000` 时）。超限先推进统一栅栏，
再回收 generation 索引；普通缓存回源，旧物理代次不复用。正常更新 O(1)，
回收扫描至多 `n_max` 项。该字节预算不等同于实际 ETS 内存硬上限。

预算摘要包含实际 `p_max()`、`gen_max_bytes()`、`n_max_bytes()` 与 `t_max_bytes()`。默认配置的机械摘要为：
`budget_digest=b83368fbaeed214157a33f9db8832e2c`；默认白名单、fenced 集均为空时
`config_hash=d34ccf06b4be4fdc0fd058e922d38b1b`（当前源码默认配置复算；旧摘要仅对应历史候选）。摘要包含 `snapshot_cohort_v1`、
`public_api_boundary_v1`、`remote_capacity_barrier_v1`、`journal_capture_v1` 和
`journal_revision_replay_v1`、`recovery_completion_fence_v1`、`snapshot_source_cut_v1` 和
`snapshot_final_identity_v1`、`snapshot_prepared_expiry_v1`、`expired_live_revision_window_v1`
和 `snapshot_expired_floor_v1`、`snapshot_transit_floor_v1`、`delta_expired_floor_v1`、
`snapshot_invalidation_abort_v1`、`bounded_peer_discovery_v1`、`contiguous_sequence_evidence_v1`、
`snapshot_unavailable_floor_v1`、`completed_snapshot_replay_floor_v1`、`peer_report_pid_v1`、
`confirmed_event_incarnation_v1`、`ready_identity_announcement_v1`、`snapshot_origin_envelope_v1`、
`snapshot_origin_forwarding_v1`、`snapshot_unavailable_journal_v1`、
`revision_origin_metadata_v1`、`snapshot_base_origin_v1`、`retained_snapshot_tombstone_v1`、
`snapshot_receive_retention_barrier_v1`、`snapshot_partial_clock_v1`、`snapshot_coverage_candidate_cut_v1`、`event_revision_origin_binding_v1`，
不能与缺少这些保证的旧候选混用。
测试覆盖配置须按生效值重新计算。配置摘要额外绑定源码中的完整协议能力标记集，
新旧快照与缺口算法摘要不同，禁止混用来源。
候选与混合版本门禁须绑定新摘要；这些修订没有签发生产容量或 SLO。

逐键版本元数据保留五元组 `{Rev, OriginSlot, Exp, Kind, Epoch}`。经校验的来源以
`OriginSlot={Boot,Seq}` 保存，节点取自 `Rev`；未知或无效来源保留旧 counter 槽位，
不将 counter 推断为来源序列。`revision_origin_metadata_v1` 绑定此布局，事件与包装
快照的预算预检复用实际写入构造器。该内部字段变更须统一编译并重启，不宣称热升级兼容。
基础快照对 live、deleted、expired、unavailable 均保留可靠来源包装；历史 Boot
仅作为逐键注记，由当前 requested authority 的 Boot/epoch 授权页数据，不注册旧身份。
无来源上下文的条目校验及直接事件仍使用严格身份门禁；注记不推进 covered_seq 或
foreign floor。`snapshot_base_origin_v1` 绑定此接收边界；跨来源完整覆盖证明仍待完成。
删除条目在原 Exp + 墓碑保护窗内继续作为版本下限传递，发送、应用和字节预算
均保留原 Exp，不续期。直接 DELETE 事件仍须未过期；过窗快照条目拒绝。接收端
逐条应用前及整页完成后复核保护窗，失效则中止回合，不确认页号或计数。已完整
校验并进入应用的页保留版本上限，部分应用后中止仍合并 Lamport 时钟并清理 live
值；未通过摘要/条目/预算门禁的页不贡献版本上限。这些受控时间边界回归不等同
于长分区、三机容量或跨来源完整证书验收。

代次由共同单调时钟分配，读取 `max(局部代次, floor)`。升级采用 sync 重启，
生成新 Boot 隔离 depcache 中的旧物理值；旧 `Local+Floor` 元数据不支持直接热加载。

| 符号 | 含义 | 冻结值 | 单位 |
|---|---|---:|---|
| Nmax | rindex 复制条目上限 | 50,000 | 项 |
| Nmax_bytes | rindex+rev 外部序列化字节上限 | 16 | MiB |
| Tmax | 墓碑条目上限 | 20,000 | 项 |
| Tmax_bytes | 墓碑外部序列化字节上限 | 8 | MiB |
| Jmax / Jmax_bytes | 发送期间捕获及版本重放预算；完整确认未实现 | 2048 / 32 MiB（本地候选） | 项及 bytes |
| Bmax | 恢复接收缓冲（页事件积压） | 2,048 | 项 |
| Bmax_bytes | 恢复接收缓冲字节 | 32 | MiB |
| Pmax | 快照每页条目 | 256 | 项 |
| Pmax_bytes | 快照每页字节 | 4 | MiB |
| Rmax | 同时恢复节点数（三节点拓扑） | 2 | 个 |
| Rmax_rounds | 单节点快照回合重试 | 3 | 轮 |
| Dmax | 恢复总 deadline | 300 | 秒（=recovery_window） |
| MaxValue | 单值上限 | 32,768 | bytes（既有） |
| MetaMax | V2 固定字段、控制帧和快照元数据上限 | 4,096 | bytes（固定，进入预算摘要） |
| EventMax | V2 完整事件信封上限 | `2 × MaxValue + 4096`（默认 69,632） | bytes，派生值，无独立配置 |
| MaxTTL | replicate TTL 上限 | 600 | 秒（既有） |
| HighWater | sync 队列高水位 | 10,000 | 条（既有） |
| W | 墓碑/化身宽限保留窗口 | 720 | 秒（既有公式推导） |
| grace_max | 未到期宽限化身记录；满额拒绝新化身并冷恢复 | 256（本地候选） | 项，进入预算摘要 |
| peer_max | 当前登记来源；满额拒绝首次登记并冷恢复 | 64（本地候选） | 个，进入预算摘要 |

V2 接收端仅接受协议版本 2 和既定 13 个字段。完整信封先按外部序列化
字节数检查，再检查 Key/Value 与协议内容，避免额外字段或巨大计数绕过预算。
SET/DELETE 发布入口先预览并检查信封，拒绝时不分配版本或写入缓存。
DELETE 携带来源的绝对墓碑期限；接收端直接保存该期限，相同版本不续期。
期限必须为正整数、扣除时钟偏差后未过期，且不超过接收时间加 W+5 秒；
发布预检也使用实际 Now+W 期限，Value 类型和大小检查仅适用于 SET。
信封除 Key/Value 的固定字段还独立限制 4 KiB，不能借用 payload 的剩余空间。
版本报告、水位帧、快照页头、条目版本/期限和终帧元数据受同一上限约束。
快照请求必须包含 PID 与 12 字节 round 令牌；非法请求不进入发送路径。
EventMax 由 MaxValue 推导，现有摘要中的 MaxValue 已绑定该派生上限。
此检查约束应用后的事件大小，不限制 Erlang 分布式传输解码前的内存。

元数据预算以每行 `erlang:external_size/1` 累计，不等同于 ETS 实际分配内存。
同一串行写入者更新索引/版本/墓碑计数；覆盖、墓碑转 live、过期清理和冷恢复
同步更新账目，定期校准。普通 SET 的预算检查不扫描整表。快照整页预检
不借用旧版本行可能被跳过的字节缩减，避免页面中间或最终写入超限。
默认上限保持原冻结值；本次补齐执行与摘要绑定，此前证据不作为新摘要验收。

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

### 回合绑定的来源目标（当前实现）

`round_bound_origin_target_v1` 绑定以下协议：恢复请求为
`{cache_version_request, FromPid, FromNode, Round}`，应答在原九字段之后
附加 `OriginSeq, Round`（11 字段）；周期请求仍为三字段，应答附加
`OriginSeq`（10 字段）。完整应答受 4 KiB 预算约束。

当前回合在成员 PID 与化身预算校验成功后，冻结每个来源首个合法
`{Node, Boot, Epoch, OriginSeq}`；counter 不是来源序列。后续同化身的
序列升降均不移动该目标，非 READY 来源变更 Boot/Epoch 也中止旧回合。
旧回合应答在任何身份、版本写入前拒绝。九字段诊断应答和不带回合的
周期应答没有目标资格，不能将未知目标推断为零；已冻结目标的身份
变化仍可由周期应答撤销。

来源自身最终切点不得低于同 Origin/Boot/Epoch 的冻结目标或当前已见序列；
切点还不得低于成功接收页中同来源、当前 Boot 的最大原始序列；
`snapshot_manifest_origin_cut_v1` 绑定此门禁，历史 Boot、其他来源及 unknown
注记不参与该下界，revision counter 不能代替原始序列。回退在序列协调和
元数据写入前拒绝。该目标尚未用于跨来源内容、删除与缺席证明，也不能
单独签发 READY 或提升其他来源的已覆盖水位。最终来源切点、日志和
后续水位仍须闭合；C04/AC-19 保持未完成。

跨来源内容证明的实施约束：每个来源的完整页集和日志尾部必须绑定同一
Round/Boot/Epoch/cut，并记录有界逐键清单（键、版本、类型、期限和来源）。
项数、字节与 Dmax 超限中止，失败来源不能签发完整清单。最终统一验证
冻结的来源目标及清单，不得在逐来源版本合并时提前签发联合覆盖。
缺席只可裁决来源已知、处于证书覆盖范围的行；未知来源不得推断序列，
缺席不得伪造新 DELETE。必须处理较早页已应用的旧 live，裁决完成后一起
提交内容状态与覆盖下限，再进入 READY。多来源联合证明与两种来源顺序、
真实写删/分区历史仍需验证，以上是待实现合同，不是当前已达成能力。

### 有界接收清单与最终内容裁决（实现待最终候选验收）

`snapshot_receive_manifest_v1` 绑定接收页收据：成功页在所属来源 peer 内
记录 `{PageNo, Rows}`；每行包含类型、键、版本、期限、明确来源或 unknown，
及完整原始条目 ETF 的 SHA-256 摘要。不长期保存 payload 副本；摘要仅为
本地接收证据；发送端完整内容证书及规范化承诺见以下当前合同。来源身份/代次和
Round 沿用既有 peer/cut 门禁。重复、非法或应用失败页不建立成功收据。

全部来源共用回合预算：项数不超过 `Nmax + Tmax`（默认 70000），序列化
字节账目不超过 `Nmax_bytes + Tmax_bytes`（默认 24 MiB）。每页计费为
`external_size({PageNo, Rows}) + 7`，保守包含清单列表封装；这是逻辑证据
预算，不是 Erlang 实际堆内存硬限。增量记账，不反复序列化整个历史清单。
摘要计算前输入已经通过单页项数/字节门禁；清单超限在任何缓存写入前
返回 `snapshot_manifest_items_overflow` 或 `snapshot_manifest_bytes_overflow`。
清单随回合销毁，不设置新进程、定时器或模块。

以下当前合同替代上文“待实现”的历史边界，预算保持不变；C04/AC-19 的最终状态仍由最终候选真实门禁决定：

- 十二字段版本应答绑定 Round、来源 pid/Boot/epoch 和全 epoch 保留元数据 census。
  只有真实零内容 census 可分类为空非权威；未知、超限或错误 census 不等于空。
- 十字段 source-cut 附带候选与独立完整保留元数据 `{count,SHA256}`。
  同一发送 actor 在尾部直接枚举，接收完整页集逐来源复算；counter 不能替代证书。
- 恢复回合冻结接收者保护窗口内的逐键义务。最终联合清单必须覆盖且不回退；
  跨来源序列目标须由该来源清单中的同版或更新行证明，任意空快照不能产生删除。
- 成功增量另登记有界逐事件收据，只有连续序列能扩展认证前缀；同版须核对
  实际类型、期限、来源与 live 值体，不能将忽略的冲突 payload 计为已应用。
- 完成证明后重检当前来源身份、回合期限与已见前缀；全通过才原子安装认证来源
  floor/covered_seq 与 READY。失效、未知、缺口、超限或超时保留恢复或降级状态。
- 一个有界 `cache_peer_loss` 槽保留首次任意 peer 失联时间。部分断连或孤岛超过 W
  均冷清副本并推进 epoch；非法成员配置安全拒绝，不能杀 actor 或伪造单节点 READY。
  新 epoch 无同 epoch READY 权威时只能降级；权威建立及可用性门禁仍未闭合。

上述能力通过 config_hash 绑定完整源码标记集，包括内容证书、空内容 census、
连续增量收据、认证 foreign floor、最终提交屏障、相同版本内容核对、
敏感键 peer Boot 健康与 `observed_peer_loss_window_cold_v2`。
旧八/九字段 source-cut 只作兼容诊断，不能签完整 READY。

### 旧 SET 的策略与历史受管边界（已实现）

`legacy_set_policy_boundary_v1`：旧 `{set, Key, Data, TTL, Depend}` 接收仅允许
未分类 legacy 键；local、invalidate、blocked、replicate 键不经无版本写入
覆盖。拒绝计入 reject_total，不写值或版本。`legacy_publish_policy_boundary_v1`
将同一判断用于公开 broadcast 的旧 SET 发布：拒绝返回 `{error, policy_denied}`，
不调用 syn；未分类键和旧 flush 的兼容发布保留。发送与接收共用一个判断，
不新增分类器或重复传输包装。该错误返回并不改变历史传输异常 `{ok,0}` 的
口径，不能把后者当投递成功。
`managed_revision_row_boundary_v1`：受管判定同时检查复制索引和版本行，
DELETE-only 墓碑即使无索引、Registry 已撤销，仍通过既有公开读写栅栏。
这项保证持续到版本元数据被合法回收；不是无限历史证明或超窗恢复闭合。

本地真实 syn/depcache/sync 回归已验证凭据正常 set 不发布、未分类键确实
发布、旧 SET 不覆盖凭据，以及实际 DELETE-only 撤销策略后的防复活边界。
当前 C10 驱动 M1 已纠正 replicate/legacy 键混用：未分类正例确实传播且无
版本行；受管反例先写真实值，再从旧节点广播 SET，同发送进程随后 call
同一接收 actor 的 ping_ok，确认目标消息已处理后检查值、版本不变且拒绝
计数增长。真实旧/新节点 M1/M2 定向诊断通过；版本读取对齐含 epoch 的
五字段行。混合重启 M3、完整矩阵两轮与 C12 绑定仍未通过，详见
`c10-legacy-build.md`。不能把这些定向结果等同 C10 闭合，生产资格未签发。

后续 `membership_capability_gate_v1` 增量：同步 actor 在现有 syn 成员元数据
声明 `cache_protocol=2`；公开 SET/DELETE 在分配时钟及修改数据前，要求本地
成员视图包含自身、数量在 Peermax 内且所有成员明确声明 V2。直接 V2
SET/DELETE 广播复用门禁，否则返回 `{error,incompatible_peers}`；旧式失效
不受影响。真实组件验证拒绝不改变值、时钟或 META，成员退出后可重新写入。
这是当前 syn 视图的能力检查，不替代 Boot/epoch/hash、READY、投递确认或
全网成员一致性；检查与发布之间仍有成员变化竞态。没有新增进程或模块。
此前 M1/M2 的结果属于该门禁前诊断；旧驱动仍需对齐混合期复制拒绝语义，
不能沿用其复制成功断言。M3 及 C04 联合内容证明仍未闭合。

`membership_capability_gate_v2` 再绑定既有 `cluster_nodes` 的预期全集：配置中
任一节点不在当前成员视图，或列表格式、节点类型及数量不合法，发布均拒绝。
显式空配置仍表示未声明其他节点，不凭此推断全网完整性；非空配置也不能
消除 syn 视图滞后或检查到发布间的成员竞态。所有检查在同一 try 表达式体
内，避免 of 分支异常逃逸。真实 actor 回归覆盖缺席、非法配置/非正规列表、
非 map 成员元数据、显式单节点以及拒绝后时钟/数据不变与进程存活。
