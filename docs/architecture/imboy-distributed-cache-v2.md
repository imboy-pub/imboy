# IMBoy 分布式缓存 V2（fenced cache sync）架构文档

> w0 计划 §15.3「长期保留文档」要求的架构篇。描述当前实现；收口仍在验证中。
> 权威协议规格：`docs/architecture/cache-v2.2.1/closeout-v1/protocol-budget-frozen.md`；
> 收尾结论与台账：同目录 `closeout-report-v1.md` / `ledger-v2.json`。
> 历史候选：`f1aa1ce016eaa6e4db18e52e6ee6fba51cdb745303c057aaeff650c9b990020c`
> （= sha256(git archive 2873dbfd)）。后续产品修改须重新绑定，旧证据不代表当前验收。

---

## 1. 定位与设计约束

- **不引入新中间件**：无 Redis、无 Mnesia；保留 depcache 做本地缓存，复用 syn 3.4.2
  （pg scope `imboy_cache`，group `dsync_handler`）做成员管理与消息广播。
- **渐进启用**：V2 行为仅对 `cache_fenced_namespaces` 显式列出的 namespace 生效；
  默认空表 = 全部走 legacy depcache 行为，QR 客户端 wire contract 不受影响。
- **失败封闭（fail-closed）**：replicate 复制授权要求「注册表登记 `{replicate, false}`」
  **且**「生产白名单命中」两个条件同时成立；未登记键一律拒绝（not_authorized）。
- **分代栅栏**：所有跨节点 SET/DELETE/失效以 revision（seq+boot id incarnation）定序，
  旧化身重放与跳号均有确定语义。

## 2. 模块图

| 模块 | 职责 |
|---|---|
| `src/lib/imboy_cache.erl` | depcache 门面（源自 z_depcache）；`put/4`、`put_delete/1`、`invalidate/1` 对上提供 local/replicate/invalidate 三种行为；同步本身委托独立 gen_server |
| `src/lib/imboy_cache_policy.erl` | namespace、fenced/replicate 授权、预算与 config_hash 输入；TTL、快照条目和来源格式的纯校验规则 |
| `src/lib/imboy_cache_sync.erl` | 同步核心 gen_server：公共 API、gen_server 串行化、generation 栅栏与物理键、发布路径（预算前置检查/Lamport 分配/syn 发布）、时钟与 revision 纯函数；V2 事件应用与恢复引擎分别委托 apply/recover（例外 4 初次拆分，后续收口修复） |
| `src/lib/imboy_cache_sync_apply.erl` | V2 事件应用、rev/tombstone 写入、复制元数据预算与发送会话 journal 捕获/连续窗口提取 |
| `src/lib/imboy_cache_sync_recover.erl` | 恢复引擎：发现/降级状态机、r1/r2 版本上界应答处理、C04 快照回合闭合与中止、9.4 定期校对、W4-06/07 有界清扫 |
| `src/lib/imboy_cache_sync_proto.erl` | 协议帧校验、化身登记及序列证据：envelope、摘要、来源 cut、连续应用下限与完成快照重放下限；ETS 更新仍由同步 gen_server 调用串行执行 |
| `src/lib/imboy_cache_sync_snapshot.erl` | 快照回合、分页发送、页摘要、接收缓冲与完成帧校验；发送游标绑定 Boot、epoch 和 journal 覆盖窗口，完整恢复切点仍未验收 |

共享 ETS：`imboy_cache_meta`（世代、peer counter、恢复状态）；同步族共享状态记录与
恢复预算 define 在 `imboy_cache_sync_state.hrl`。

恢复完成在开放 READY 前推进统一 fenced 栅栏，阻止恢复期间启动的旧 loader 回填。
该保护限于已启用 fenced 的命名空间；完整失效水位与跨来源恢复切点仍待验收。
done 必须有本回合来源自身序列确认，绑定 Boot、epoch、两项摘要及当前登记化身；
确认缺失、化身未知或序列缺口不能闭合。此确认不代表第三方来源的完整增量覆盖。
开放 READY 前还会复核全部已确认来源的当前身份；确认后重启会中止旧回合。
来源已保留的过期版本下限通过显式 expired 条目传递，沿用原版本、原期限与 N/bytes
预算，帮助冷接收端拒绝旧长 TTL 值；不代表缺失来源或超窗历史已完整覆盖。
live 快照在传输中失去可复制 TTL 时也保留原版本下限，超保护窗口整页拒绝。
V2 SET 增量在原期限加 W 的窗口内也保存同一下限，保留原期限、正常推进序列；
仍完整校验权限、值、摘要和来源身份，容量不足进入冷恢复，超窗事件拒绝。
这些逐键保护尚未证明完整跨来源恢复覆盖。
live 值被淘汰时，快照仍以 unavailable 下限传递其版本及原期限，拒绝较旧值回填。
同版可用副本可以保留或按原期限补回；缺值不合成 DELETE，计入原 N/bytes 预算。
失效尚未进入快照 journal；本地失效、V2 invalidate 和 legacy flush 均中止活动发送
回合，撤销预读页，避免发送失效前的旧值。接收端沿既有 deadline 有界重试；
持续失效可使恢复进入 DEGRADED，该屏障不代表完整增量追赶已实现。
宽限化身集合另受 `grace_max` 限制（本地候选默认 256 项，绑定预算摘要）。
注册新化身前先清理已到期行；满额时不替换当前 Boot 或删除其序列，
事件、legacy 收据与握手报告进入冷恢复。宽限记录清理在 ETS 内完成，
避免复制 journal 全值。当前登记来源另受 `peer_max` 限制（本地候选默认 64，
绑定预算摘要），满额拒绝首次登记；已登记来源的换 Boot 沿宽限预算处理。
注册失败的报告不写 epoch/counter；未知来源配置不匹配不增诊断行。
本轮未实现自动退役，降低预算不会删除既有登记；完整发现与恢复资源验收仍待完成。
发现回合构造与定期校对发送前还检查远端成员数；超过 `peer_max` 时不抽取部分来源，
清理低代受管副本、推进 epoch 并进入 DEGRADED，取消恢复定时器；
按既有规则保留的当前或未来代底层数据仍受公共读屏障约束。
这一检查限制本模块保留的回合及请求扇出；syn 返回完整成员列表本身仍由依赖库管理，
不构成整个节点 RSS 的硬上限，也不证明跨来源恢复切点已完整覆盖。

## 3. 关键机制

### 3.1 版本握手（r1 → r2）

- **r2（寻址修订）**：`version_request_frame(FromPid)` 发 3 元组
  `{cache_version_request, FromPid, FromNode}`，对端**直连回** version_report
  （携带 sender pid）；请求方据此 pid 直发 `{cache_snapshot_request, self(), Round}`，
  消除 syn 视图滞后导致的恢复饥饿。
- **r1 兼容**：2 元组旧帧仍被接受，经 syn 视图查找回信；peer counter 在两条路径都会
  持久化（`{peer_counter, FromNode}` → meta ETS），供冷启动水位恢复。
- **incarnation**：节点重启产生新 boot id；对端首见旧化身事件按 **changed** 处理
  （补偿信号），不再静默忽略。

### 3.2 缺口补偿与 fenced 世代

SET、DELETE、INVALIDATE 共用连续来源序列，接收水位按 `{NodeId, BootId}` 隔离；
新化身替换时删除旧水位，宽限旧化身重放不污染新水位。

seq 跳号（如 1→3）或化身变化时，推进统一 `fence_floor`，以 O(1) 操作使所有
受管旧物理键不可读，不枚举 Key，也不清理无关本地状态。每个 Key 的有效代次为
`max(局部 generation, floor)`；两者来自同一单调时钟。首次加载中、未发生过单键
失效的 Key 同样受保护。

generation 索引最多 `n_max` 项，序列化字节最多 `gen_max_bytes`（默认
`n_max × 128`）。写入超限时先推进新 floor，再回收索引，旧代次不会复用；
正常更新 O(1)，回收至多扫描有界索引。字节预算不等同于实际 ETS/BEAM 内存硬上限。
降低配置上限在下一次 generation 更新时生效。generation 写入由 sync 串行执行。

`memo` 各重载先解析实际 Key，再使用同一入口；MFA 派生 Key 不含 context 参数，
显式 undefined Key 同样先派生，派生仍为 undefined 时明确拒绝。仅显式配置的 fenced
namespace 进入栅栏。物理键为 `{cache_v2, Key, {{NodeId, BootId}, Generation}}`，
依赖标签同样绑定启动身份。loader 返回前复查代次与身份，sync 单独重启时，
存活的 depcache 旧条目与旧 loader 回填不会命中新生命周期。旧物理值按 TTL 回收。

末尾失效丢失由既有周期握手补偿：周期版本应答携带来源序列（10 字段），随后发送
`{cache_invalidation_watermark, NodeId, BootId, OriginSeq, ConfigHash, RegistryHash}`。
来源水位高于已接收序列时推进统一栅栏，并隐藏、清理受管 live 值，保留版本和墓碑，
在同 epoch 发起有期限的 authority 快照恢复。恢复中的新缺口中止本轮，不延长 Dmax；
重复水位不重开恢复，无 authority 到期 DEGRADED，禁止空缓存 READY。
legacy 保留旧 flush，随后附加 `{legacy_invalidation_seq, Origin, Boot, Seq, ConfigHash,
RegistryHash, Key}` 收据。收据独立执行该 Key 失效后记账，不能假定两次 publish 的
接收集合相同。正常连续收据不会触发完整恢复；宽限旧化身不推进当前序列。
Key 类型/大小、固定字段 4 KiB 及全帧 `max_value_bytes + 4096 + 32` 字节先校验。

主动周期校对仅在 READY 状态运行（当前间隔 60 秒），网络恢复后无需等待下一次
业务写入。能力限于升级节点及已启用 fenced 的派生缓存；旧节点会忽略附加水位。
sync 只保留一个周期校对定时器引用；手动校对或恢复 READY 会替换旧定时器，
已进入邮箱的旧超时按引用忽略，避免派生重复周期链。新增状态字段须统一编译并
重启 sync，不支持直接热加载到旧 record 状态。
三节点真实分区丢失末尾失效后，新增场景不发送校对消息、不新增业务写入，
同时要求周期校对计数增长、失效代次推进及两个缓存键回源；该场景连同完整
34 项驱动通过。此前手动校对场景出现过一次失败，后续完整复跑通过，但根因
未确定，不能据此宣称所有重连时序已覆盖或完整收口。
DEGRADED 不承诺同一补偿时限；水位只触发恢复，不替代完整快照/增量证明，安全授权仍须权威校验。

本轮代次表示与预算摘要已变化。升级须重启 sync 生成新 Boot，并重新冻结候选、
配置摘要及受影响验证；不得将新算法直接热加载到旧 `Local+Floor` 元数据上。

### 3.3 快照（C04）

- live 条目保留 `{Key, Value, ExpiresAtMs, Rev}`；墓碑采用显式五元组
  `{deleted, Key, undefined, ExpiresAtMs, Rev}`。枚举仍在保护窗内的版本行；墓碑无需读取值。
  页摘要绑定 kind、Key、revision，配置摘要绑定 `snapshot_tombstones_v1` 区分旧格式。
  接收端保留原删除版本与绝对期限，只应用较新版本，相同版本不续期；旧 live 页不能
  覆盖已恢复墓碑。整页预检索引、版本字节和墓碑数量/字节，不借用可能被跳过的缩减。
  该传播能力与自动缺口触发仍不等于完整增量追赶或超过墓碑保留窗的删除证明。
- 预算：`p_max=256` 项/页、`p_max_bytes=4 MiB`/页；接收在途页受
  `b_max=2048` 项、`b_max_bytes=32 MiB` 限制（`imboy_cache_policy` 可覆写）。
  已应用页释放缓冲预算；快照总量仍受实际索引与墓碑 N/T 容量约束。
  来源串行拉取：页应用后请求下一页，当前来源 done 后再请求下一来源。
  未实际请求的来源不能推进页或 done；页号保留连续计数，不累计页号集合。
  发送端每个接收进程最多一个游标，全部游标最多 8 个且序列化字节总量不超过
  `b_max_bytes`，过期按 Dmax 清理；游标仅保留后续 Key 和一页预读数据。
  来源 epoch、配置或 Registry 摘要变化会中止旧游标，不能跨冷清理继续预读页。
  该预算限制协议保留数据，不等同于 BEAM 实际内存或分布式邮箱硬上限。
  字节按完整条目列表的序列化大小计量。接收端先检查期限、页形状与预算，再校验
  全页授权、字段和索引容量；非法页在任何条目写入前中止。
  发送端也校验条目授权；单条超过页预算显式中止，以实际下一有效页决定 `HasMore`。
- 先冻结本轮发现的对端，等待全部状态应答；至少一个 hash 一致的 READY 对端可作为
  权威来源。非 READY 应答只完成发现分类，不提供快照；全部非权威时不能签 READY。
  版本报告先核对配置与 Registry 摘要，冷恢复另核对 epoch，再登记化身和版本上界；
  不兼容报告仅排除来源，已授权来源变得不兼容则中止回合，不污染 peer 元数据和时钟。
  授权时绑定来源 epoch；其后报告改变 epoch，即使 Boot/hash 不变或来源已 done，
  也中止旧回合。当前授权 Boot 的合法 V2 事件带来更高 epoch 时同样中止，
  无需等待报告；宽限旧 Boot 不改变当前来源。不同初始 epoch 的混合已在受控回调 fixture
  中复现旧代值可读；现已绑定每条版本的数据 epoch，低于全局代次下限的写入拒绝，
  读入口只开放本地完成恢复的代次。全部冻结来源完成报告分类后，才选择最高合法
  READY 代次的同代来源并开始拉取；目标不得低于已观察数据下限，完整切点仍待验收。合同见
  `cache-v2.2.1/closeout-v1/protocol-budget-frozen.md`；不能清除高代已确认增量后继续沿用序列确认。
  非恢复状态的报告还须与本地 epoch 一致，否则拒绝。
  报告 PID 必须属于当前本地 syn 成员视图；恢复中还须属于冻结回合的节点。
  节点同名不替代 PID 校验；视图滞后仍有未闭合风险。
  事件和序列收据不能自行登记未知 Boot，拒绝后有界恢复身份；合法成员握手
  可为身份恢复选择最高代次，预算冷恢复仍限制同代。已确认合成来源的组件
  定向回归通过；高代次先事件后握手的受控 actor 准入及真实三节点恢复路径已验证。
  健康三节点用例先用实际 reconcile/report 确认六项相互 Boot；该前置不能替代
  生产启动窗口可用性验证，也不代表跨来源完整覆盖或全项门禁已验收。
  合法远端复制因容量不能应用时进入冷隔离，目标代次严格高于被拒事件；旧代来源
  不能恢复旧值。非 READY 状态再次隔离不延长原期限，无合格新代来源则降级。
  最后一页与 done 之间，来源另发当前事件序列水位；缺少尾部 delta 时中止回合，
  随后的旧 done 不得签 READY。来源分配序列时同步镜像 ETS，同 Boot 内冷清理不重置。
  此屏障不替代跨来源覆盖和完整切点证明。
  完整回合开放 READY 前保留各来源自身 cut 的序列下限；同 Boot 的旧 SET/DELETE
  在化身登记前拒绝，不依赖逐键版本永久留存。旧 Boot 下限随有效宽限期清扫，
  第三方覆盖、未知新 Boot 及宽限期外重放仍未闭合。需要 authority 或已有未完成
  回合时，成员视图变空也继续等待来源，不回退为新建单节点 READY，总期限不延长。
  全部权威来源的页序、终帧总页数与水位必须闭合，且在截止期内。任一页失序、摘要不符、
  缓冲溢出或已选来源的 Boot/能力变化 → 本轮 abort（计数 `snapshot_abort_total`）。
  V2 事件或旧协议序列收据先观察到来源新 Boot 时，也立即撤销旧来源回合；
  同一帧同时出现化身变化和序列缺口只计一次回合失败，不延长恢复期限。
  abort 清受管 live 值并保留版本/墓碑；下一轮合法同版本快照可补回缺失 live 值。
  终帧完全相同的重复幂等，矛盾重复拒绝。
  终帧水位须覆盖该来源全部已接受页中 live/墓碑的最高版本 counter；较低水位拒绝。
  每个合法新页立即推进本地 Lamport 上界，后续回合中止也不回退已观察版本；
  未知来源、旧回合和已完成来源的迟到页不改动时钟。
- 恢复只持有一个可取消的定时器；旧定时器消息不推进状态。中止回合会重新安排发现，
  总 deadline 到期清理部分副本并进入 DEGRADED。`cluster_nodes` 非空而 syn 视图为空时
  不能误判为单节点；默认空 `cluster_nodes` 允许单节点空缓存启动。
- `take_page` 保证每页真实攒批（页满或发完才切页），杜绝单条页导致的轮次饥饿。
  组页累计条目数和编码字节，不反复扫描已有页；非空 tuple 条目列表的 ETF 开销为
  7 字节，每条独立外部编码去掉其 version 字节后累加。整页实际大小仍由
  `external_size` 计算并用于接收端预算校验，现有页/回合预算不变。

### 3.4 授权与配置指纹

事件先核对配置/Registry 摘要，再核对 epoch，最后登记化身；拒绝事件不得换 Boot 或
删除当前来源序列。旧 epoch invalidate 的保守失效例外仍须通过摘要校验。

摘要计算在调用进程内复用完全相同的配置输入，减少重复读取、编码和散列；
每次调用仍读取当前应用配置，变更后立即重新计算，不缓存授权判定。
预算摘要由同一次捕获的 replication 配置计算，字段顺序、默认值及 wire hash 不变。
budget 只在需要时从捕获配置纯计算；每个调用者最多保留 config/registry 两个摘要项，单项输入的外部编码不超过 4 KiB，且最多遍历 256 个结构单元；字符列表不能利用
ETF 的紧凑编码绕过此限制，子二进制也不得引用超过 4 KiB 的底层二进制。更大的合法配置
正常计算但不保留。此限制不是实际进程堆或节点 RSS 上限；长生命周期调用者较多时
须同时测量内存。代码函数版本令牌变化使旧摘要失配；只保留令牌而不保留旧代码的 local fun。
无需添加依赖或配置迁移。

Fingerprint reuse is caller-local and keyed by exact current configuration plus a function code-version token.
It preserves wire hashes and live authorization checks. Two slots (config/registry only) retain at most 4 KiB of
serialized input each, with a 256-cell structural admission guard and a backing-binary limit.
Larger valid inputs bypass reuse. This is not a heap/RSS guarantee.

`get/1`、`get/2`、`get_subkey/2`、`get_wait/1` 统一检查复制读取：当前 Registry 的 replicate Key
或仍有 rindex 的 Key，必须仍被授权、节点 READY、具有未过期 live revision 才可返回值。
缺元表/版本、墓碑、撤销或恢复中均返回 miss；缺版本不因节点 READY 自动放行。
复制读取绕过调用者的 depcache 进程内副本及缓存时钟，读取当前缓存实例；
返回或异常时均恢复调用者原模式与时钟设置。
读取前后比较完整版本行和 sync 启动身份，并重新核对授权、READY 与有效期；
等待期间删除、版本变化或重启会返回 miss。这是有限并发检测，不是全局事务。
复制 Key 的 `set`、`memo` 明确拒绝，`put(..., local)` 返回 `versioned_write_required`；
复制写入须用 `put(..., replicate)`，删除须用 `put_delete`。原生 get_wait 的 miss
不提供 V2 ownership token，不能据此配套裸 set 回填。直接 depcache/ETS 操作不在 facade 保护范围内。
历史未知 legacy Key 保留原读取合同；没有永久分类记录，不能据此证明其原始值属于复制副本。

- 注册表：基线登记 + `cache_registry_overrides`（`#{Key => {replicate, false}}`）。
- `replicate_allowed(Key)` = 注册表 `{replicate, false}` ∧ `whitelist()` 命中 namespace。
- 发送端与接收端都限制 Key 和 Value 的大小，并禁止嵌套 PID/ref/fun；接收端校验
  namespace、revision 元素、非负来源序列、16 字节 Boot、SET Value 与过期字段。
  缺字段和非法类型在应用缓存、登记来源化身前拒绝；不正规列表尾项同样检查。
  SET 期限不得超过当前时间加 `max_ttl_sec + 5` 秒，DELETE 不得超过 W+5 秒。
  DELETE 接收使用来源的绝对期限，不按接收时间重算，相同版本不续期；扣除既有时钟偏差预算后
  无有效剩余 TTL 的事件在一致性处理前拒绝，避免过期帧修改版本/化身。
- 版本应答接受 9/10/11 字段；10/11 字段还校验非负来源序列，11 字段校验 12 字节回合标识。应答校验非负整数 counter/epoch、16 字节 hash/Boot、boolean READY
  与发送 PID 所属节点。恢复中仅接收当前发现回合的节点，其他状态仅接收 syn
  中已发现的远端节点；非法结构和未知来源在合并版本/登记化身之前拒绝。
- config_hash = SHA-256 截取 16 字节（`budget_digest()` + whitelist + fenced_namespaces）；集群间
  config_hash/registry_hash 不一致会拒帧并计数（`ImBoyCacheSyncRejected` 观测点）。
  当前摘要还绑定完整协议能力标记集，包括 `ready_identity_announcement_v1`；
  新增 `snapshot_origin_envelope_v1` 和 `snapshot_origin_forwarding_v1` 绑定来源
  重放包装及接收端日志转交；默认白名单、fenced 集为空时为
  `de6d18cefa71d7e07ef7b91058fb8f68`；`snapshot_unavailable_journal_v1` 额外绑定
  unavailable 下限的日志捕获，避免将缺值伪造成删除或有效值。
- 恢复完成并安装快照下限后，READY 节点复用版本报告通知远端 syn 成员当前身份。
  发送人数受 peer_max 限制，超限交给既有校对入口，不新增进程或定时器。
  三节点重启后无数据写入、无显式对端校对的验证通过；周期校对可能参与，
  并发启动、成员视图滞后和完整跨来源覆盖仍未完成验收。

逐键版本元数据保留五元组 `{Rev, OriginSlot, Exp, Kind, Epoch}`。经校验的来源以
`OriginSlot={Boot,Seq}` 保存，节点取自 `Rev`；未知或无效来源保留旧 counter 槽位，
不将 counter 推断为来源序列。`revision_origin_metadata_v1` 绑定此布局，事件与包装
快照的预算预检复用实际写入构造器。该内部字段变更须统一编译并重启，不宣称热升级兼容。
基础快照对 live、deleted、expired、unavailable 均保留可靠来源包装；历史 Boot
仅作为逐键注记，由当前 requested authority 的 Boot/epoch 授权页数据，不注册旧身份。
无来源上下文的条目校验及直接事件仍使用严格身份门禁；注记不推进 covered_seq 或
foreign floor。`snapshot_base_origin_v1` 绑定此接收边界；跨来源完整覆盖证明仍待完成。
当前尾部切点增加原捕获的覆盖候选字段，整帧限 4 KiB，向量受 peer_max 限制并
按来源唯一、规范排序；error 原样传递，不能补造空覆盖。接收绑定当前回合及来源
Boot/代次，并保留完整帧作幂等与冲突判断。`snapshot_coverage_candidate_cut_v1`
绑定这项 wire 变更；候选暂存不登记第三方身份，也不安装 foreign floor 或赋予 READY 证明。
事件的 revision 节点必须等于 origin_node；否则在任何版本、来源序列或覆盖写入前
以 revision_origin_mismatch 拒绝。`event_revision_origin_binding_v1` 绑定这项门禁，
防止连续前缀被另一节点的版本污染；它本身不构成完整覆盖证明。
删除条目在原 Exp + 墓碑保护窗内继续作为版本下限传递，发送、应用和字节预算
均保留原 Exp，不续期。直接 DELETE 事件仍须未过期；过窗快照条目拒绝。接收端
逐条应用前及整页完成后复核保护窗，失效则中止回合，不确认页号或计数。已完整
校验并进入应用的页保留版本上限，部分应用后中止仍合并 Lamport 时钟并清理 live
值；未通过摘要/条目/预算门禁的页不贡献版本上限。这些受控时间边界回归不等同
于长分区、三机容量或跨来源完整证书验收。

### 3.5 事务提交后的业务失效

群成员 join/leave 只失效 `{group, Gid}`，不从旧缓存列表推导新列表。
启用 `cache_fenced_namespaces` 中的 `group` 后，成员读取复用 memo 的代次检查与
原生等待合并；跨失效完成的旧 loader/等待者重试当前代。成功非空列表缓存，
空结果及错误通过 `uncached/1` 的零 TTL 返回，连续失效直接回源时也解包该结果。
默认 legacy 分支保留原 get/set 兼容行为，不能据此宣称它已有并发回填栅栏。
入群事务通过 `elib_pg:after_commit(Conn, {group_members, Gid}, Fun)` 登记动作，
同事务相同 Id 去重，最多 2048 个 Id，超限抛 `abort_tx` 回滚。
Conn 入口须由 `elib_pg:with_tx` 建立上下文；直接驱动事务无上下文时明确拒绝。
事务封装有动作时核验真实 COMMIT 的 CommandComplete；ROLLBACK、提交异常及
SQL 错误后实际 ROLLBACK 均丢弃动作。连接归还池后执行有界动作，不能使用旧 Conn。
动作失败返回 `{error, {after_commit_failed, CommittedResult, Failures}}`，数据库已提交，
不能将它当作回滚或自动重放事务。嵌套事务各自归还连接并恢复外层待执行动作。
该机制不提供崩溃后的持久投递；完整群业务、并发回填与故障闭环仍须独立验证。

## 4. 可观测性

`/metrics`（`src/imboy_router.erl`，仅内网，nginx 对外 403）暴露：

| 指标 | 语义 |
|---|---|
| `imboy_cache_sync_status` | 2=ok / 1=recovering / 0=degraded / -1=进程不存在 |
| `imboy_cache_sync_epoch` | 当前 epoch |
| `imboy_cache_sync_queue_depth` | 队列深度（-1=进程不存在） |
| `imboy_cache_sync_peers` | syn 成员数（`syn:members(imboy_cache, dsync_handler)`） |
| `imboy_cache_sync_counter` | 本节点同步计数 |
| `imboy_cache_sync_peer_counter{peer}` | 每对端已见计数（水位） |
| `imboy_cache_sync_protocol_info{protocol,boot,config_hash,registry_hash}` | 本节点协议身份，值为 1；没有身份记录时不输出 |
| `imboy_cache_sync_observed_seq{peer,boot}` | 当前化身最大已见来源序列 |
| `imboy_cache_sync_applied_prefix{peer,boot}` | 当前化身连续应用下限；缺少证据为 -1 |
| `imboy_cache_sync_snapshot_floor{peer,boot}` | 当前化身已完成快照的来源自身序列下限；缺少证据为 -1 |
| `imboy_cache_sync_peer_epoch{peer}` | 节点最近已接受的代次报告，未绑定 Boot；缺少报告为 -1 |
| `imboy_cache_sync_hash_divergent{peer}` | 已登记节点的摘要不一致标记，未绑定 Boot；0 或 1 |

诊断只枚举当前已登记化身并读取固定字段，不导出缓存 Key/Value、rev 或 journal。
指标读取不是原子快照；Boot 标签隔离不同化身，重启和摘要变更会增加历史时间序列。
`protocol="2"` 表示代码协议能力，不表示当前配置已启用 V2 或完成能力协商。
最大已见、连续应用及快照下限
各有不同证明范围，不能互相代替；不同来源的 Lamport counter 差值不代表投递 lag。
这些信息尚不构成完整跨来源恢复证明，也不代表 Prometheus 实际抓取已验收。

Prometheus 告警（`deploy/prometheus/rules/imboy-alerts.yml`）：
`ImBoyCacheSyncRejected`、`ImBoyCacheSyncUnknownEvents`、`ImBoyCacheSyncStuckRecovering`
（status==1 持续 5m，critical）、`ImBoyCacheSyncDegraded`（status<=0，critical）、
`ImBoyCacheSyncSnapshotAborting`（10m 内 abort>2，warning）。
Grafana 面板：`deploy/grafana/dashboards/imboy-cache-sync.json`。

## 5. 配置键参考

| 键 | 默认 | 说明 |
|---|---|---|
| `dsync_enabled` | false | 同步 gen_server 总开关 |
| `cache_fenced_namespaces` | `[]` | 启用 V2 fenced 行为的 namespace；空=全 legacy |
| `cache_registry_overrides` | `#{}` | 键级注册表覆写（`{replicate, false}` 才可复制） |
| `cache_replication` | `#{whitelist => []}` | 生产复制白名单；**默认空=不复制** |
| `b_max` / `b_max_bytes` / `p_max` / `p_max_bytes` | 2048 / 32 MiB / 256 / 4 MiB | 快照预算（经 `rep_cfg` 覆写） |
| `j_max` / `j_max_bytes` | 2048 / 32 MiB | 发送会话版本变更捕获及分页重放预算；失效覆盖与跨来源完整切点确认尚未实现 |

## 6. 测试与验证工具

| 工具 | 用途 |
|---|---|
| `test/lib/imboy_cache_sync_tests.erl` | 29 个 EUnit（含 C04 闭环/缺页/溢出 abort、C05 缺口补偿、授权栅栏） |
| `scripts/cache_v21_dcheck.erl` | 三节点故障注入 dcheck（C9a–f；`main(["<out.json>"])`，`DCHECK_ONLY=<子串>` 单场景） |
| `scripts/cache_v21_mixed_dcheck.erl` | 混合故障轮（M7–M10：摘要不符/epoch 漂移/旧化身重放/r2 帧） |
| `scripts/cache_v2_bench.erl` | V2 全链路压测（`CACHE_V2_OUTPUT`/`CACHE_V2_OPERATIONS`/`CANDIDATE_SHA256` 环境变量约定） |
| `scripts/check_cache_closeout_v1.py` | 证据机械复算校验器（只读；`--self-test` 自证 6 负例全拒） |

## 7. 文档地图

（路径前缀 `docs/architecture/`）

- 协议与预算（权威、冻结）：`cache-v2.2.1/closeout-v1/protocol-budget-frozen.md`
- 运维手册（告警处置/诊断/回滚入口）：`docs/guides/operations/cache-cluster.md`
- 收尾报告与三结论：`cache-v2.2.1/closeout-v1/closeout-report-v1.md`
- 验收台账（96 项）：`cache-v2.2.1/closeout-v1/ledger-v2.json`
- 回滚步骤：`cache-v2.2.1/closeout-v1/rollback-v1.md`（注意：G-QR 单特性回滚须**先**执行 down 迁移再回退代码）
- 原计划冻结快照：`cache-v2.2.1/w0/plan-v2.2.1.md`（SHA256 见任务档案）。其 §15.3
  所列 `docs/plans/2026-10-10-distributed-cache-v2.2.md` 即本快照的原工作路径
  （`docs/plans/` 为仓内刻意不跟踪目录，见 .gitignore:163），内容即该快照本身。

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

### 有界接收清单（已实现，尚未用于最终内容裁决）

`snapshot_receive_manifest_v1` 绑定接收页收据：成功页在所属来源 peer 内
记录 `{PageNo, Rows}`；每行包含类型、键、版本、期限、明确来源或 unknown，
及完整原始条目 ETF 的 SHA-256 摘要。不长期保存 payload 副本；摘要仅为
本地接收证据，尚无发送端内容证书或跨版本规范化承诺。来源身份/代次和
Round 沿用既有 peer/cut 门禁。重复、非法或应用失败页不建立成功收据。

全部来源共用回合预算：项数不超过 `Nmax + Tmax`（默认 70000），序列化
字节账目不超过 `Nmax_bytes + Tmax_bytes`（默认 24 MiB）。每页计费为
`external_size({PageNo, Rows}) + 7`，保守包含清单列表封装；这是逻辑证据
预算，不是 Erlang 实际堆内存硬限。增量记账，不反复序列化整个历史清单。
摘要计算前输入已经通过单页项数/字节门禁；清单超限在任何缓存写入前
返回 `snapshot_manifest_items_overflow` 或 `snapshot_manifest_bytes_overflow`。
清单随回合销毁，不设置新进程、定时器或模块。

清单仍只记录实际收到的内容；完整覆盖合同、多个来源间的最终效果/缺席
裁决和 foreign floor 原子提交未实现，C04/AC-19 不因此闭合。

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
当前候选 C10 的旧代码树/混合矩阵还未复验；M1 既有驱动误将声明为 replicate
的 mixed_test 当作无版本 legacy 键，须分别保留未分类正例与受管拒绝反例，
不能继续把不安全的 raw 写入作为兼容成功。生产资格未签发。
