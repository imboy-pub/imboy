# 分布式缓存 V2.2.1 后续收口计划 V1

> task_id：CACHE_V221_BACKEND_CLOSEOUT_V1
> 状态：PLAN_READY / NOT_EXECUTED
> 日期：2026-10-11（Asia/Shanghai）
> 范围：仅 imboy 后端、后端测试、隔离数据库迁移验证和相关文档。
> 并行上限：10 个活动 agent，含协调者、审查者及其后代。

## 1. 目标、绑定与成功边界

本计划补齐已有缓存实现的缺口，不重建缓存系统，不增加 Redis/Mnesia，不启用方案 B。
用户本轮请求是编写后续计划并供 ZCODE 执行；本文件交付不代表以下实施已执行。

原计划：工作区 `.Codex/erlang_cache_plan.md`；仓内同字节快照：
`docs/architecture/cache-v2.2.1/w0/plan-v2.2.1.md`。
两者当前 SHA-256：
`a5523ee731bc822018831586e3d747ba109c2c66e734daf5633b494d9b18c713`。
第 19 节继续覆盖旧冲突条款。本计划不追改 W0 历史快照和原始执行日志。

编制时源码基线：`bd66525073801af6b9f666c553c714f465e93f92`。
编制时后端外来 WIP：未跟踪 `docs/business/imboy-open-source-productization-strategy-v1.md`
和 `docs/design/2026-10-08-micro-community/`；执行时须重新采样，禁止接管。
历史运行根：工作区
`.Codex/runs/CACHE_V2_2_1_W1_W5_LOCAL_IMPLEMENTATION/zcode-20261010T160733Z/`。

ZCODE 开始前必须机械核对原计划两个文件摘要及本文件由用户提供的摘要。
任一不符：`BLOCKED_PLAN_HASH_MISMATCH`，禁止以新摘要自动重绑。
若 HEAD 前进：记录与编制基线的差异、缓存影响面和未提交状态，建立新的执行基线；
与缓存所有权冲突则先隔离，禁止覆盖外来变更。

交付分为三个独立结论：

| 结论 | 成立条件 |
|---|---|
| BACKEND_LOCAL_CLOSEOUT_PASS | 本计划 C00～C14 全部通过；C13 中缺失的生产输入被明确列入外部阻塞；本地门禁全部通过 |
| LOCAL_CANDIDATE_PASS | 原计划 W0～W5、AC-01～30 和第 19 节所有必需门禁通过，含原始 SLO 合同；不得由前一结论推导 |
| PRODUCTION_ACCEPTANCE | 本计划不签发；需要目标部署环境、生产资格与明确外向授权 |

本地工程可以闭合而原计划仍为 BLOCKED。必须并列报告，不得换名称掩盖未完成。
没有生产规格、真实业务峰值和冻结 SLO 时，AC-26/27 保留 BLOCKED_EXTERNAL；
原计划总体保持 BLOCKED，禁止承诺无人值守必然达到 100% 原计划验收。

## 2. 范围与授权纪律

允许后续实施的技术范围：`src/`、`test/`、`scripts/`、`config/`、项目既有迁移目录、
`docs/`，以及按已有部署说明交付的缓存监控配置。每项写入须在 C00 登记精确路径。
复用 depcache、syn、现有指标、既有 API 和 PG 事务工具。

禁止修改 imboyapp、imboyadmin 或其他独立仓；不把客户端构建、浏览器、真机验收加入后端门禁。
保持 QR REST/SSE 的路径、字段、认证和客户端可观察行为；若无法兼容，记录
`BLOCKED_CLIENT_CONTRACT_CHANGE` 并隔离该任务，不能自行扩大到客户端。
禁止修改 vendored `erlang.mk`，禁止引入无关 TSID/组织域/产品功能改动。

ZCODE 执行授权覆盖后端代码、迁移文件及专属临时测试库上的迁移/回滚测试；
不覆盖共享库或生产库迁移、真实数据导入、生产启用 V2、重启用户主机、停止其他会话进程。
push、部署、发布、联系方式和会影响第三方的动作必须取得用户明确确认。
默认 legacy、生产复制白名单为空、confirmed=false；本地测试显式覆盖配置。
本地提交按独立功能拆分，命令级 author/committer 为 `leeyi <leeyisoft@qq.com>`；
仅 stage 本任务拥有的精确路径，不修改全局 Git 设置，不执行 reset/clean/stash 或 blanket-stage。

## 3. 需要纠正的历史结论

历史台账 86 项为 77 PASS、6 BLOCKED、2 NOT_RUN、1 PARTIAL，不能当作新候选验收。
C00 逐项核验并保留历史值、审查值与原因，不直接覆写原台账。

- 有界保留时间不能证明数量/bytes 有界；19.6、相关 W4/AC 在新台账降为待验。
- 现有快照整批枚举/发送、空快照完成和仅 counter 校对不能证明完整恢复合同。
- invalidate 路径未完成失效缺口补偿；重启回填不替代存活节点分区重连验证。
- boot_id 类型检查/自环过滤不替代 incarnation、epoch 和配置一致性校验。
- 最终全量原日志为 Failed 4 / Passed 11078 / make exit 非零，隔离复跑 8/8；
  应记录全量 FAIL + 定向复跑 PASS，不能把“零未归因失败”写成全量 PASS。
- 同构件摘要一致不证明全部性能归因，旧性能数据不能替代新 V2 路径压测。
- 台账旧 candidate 字段、旧汇总和混合驱动 1970 时间戳必须在新证据中纠正。
- 同步模块 894 行超过 800 行规范；按职责进行最小拆分，不能借此重写框架。

## 4. 并行组织和写入所有权

最多 10 个活动 agent，不强求每时每刻占满。以下为角色槽位，可以按波次复用：

| Agent | 所有权/职责 | 约束 |
|---|---|---|
| A0 | 协调、接口冻结、集成、候选冻结、资源租约、最终签发 | 唯一集成提交负责人、共享运行调度者 |
| A1 | `imboy_cache_sync.erl` 及其必要新内部模块 | 同步核心唯一写者，C02～C05 串行实施 |
| A2 | `imboy_cache_policy.erl`、Registry、相关配置及 policy 测试 | 接口冻结后并行；不写 sync/cache |
| A3 | QR handler/logic/DS/Repo、迁移、QR 测试 | 复用四层模式；不写缓存核心/策略 |
| A4 | `imboy_cache.erl`、回填/API 单测 | 不写 sync 或 QR；按冻结接口接入 |
| A5 | 三节点故障驱动、故障集成测试 | 不写混合驱动或产品核心 |
| A6 | 混合升级/回滚驱动与测试 | 不写三节点故障驱动或产品核心 |
| A7 | 性能 runner/bench 与性能报告 | 运行共享性能窗口须 A0 租约 |
| A8 | 新收口 verifier、证据 schema、总台账、运维交付文档 | 不改历史 verifier/历史证据；配置改动与 A2 划清路径 |
| A9 | 独立代码、安全与证据审查 | 只读；问题回给所属 owner，不能另写同一文件 |

C00 将上述目录范围展开为实际文件列表，测试文件也登记唯一 owner。
若现有文件重叠，先改变任务分配或排队；禁止依赖最后合并解决并行覆写。
A1 的内部模块拆分先冻结 exports/消息结构/错误返回；A2/A4/A5/A6 随后消费接口。
共享新测试 fixture 如有必要，由 A5 唯一维护，其他 agent 只消费。
worker 使用隔离工作树、独立构建产物；共享 main/ebin 不并行编译。
协调者安排本地 checkout/worktree，禁止自动提交外来 WIP；worker 不自行集成 main。
数据库、端口、cookie、epmd 节点名和证据目录每个任务唯一，登记租约及归属。
全量 EUnit、重故障压测和正式性能窗口互斥；最终阶段不安排 10 路高负载并发。

## 5. 波次与依赖

```text
波次 0：C00 基线/合同/审查台账 → C01 接口与预算冻结
波次 1：A1 C02→C03→C04→C05；A2 C06；A3 C08；A4 C07；
        A5/A6 编写故障/混合 oracle；A7 准备性能 profile；A8 准备 verifier；A9 审查
波次 2：核心与调用者集成 → C09 三节点故障 → C10 混合/回滚 → C11 监控
波次 3：代码审查无阻断项 → 冻结 candidate → C12 最终本地验证 → C13 外部输入判定
波次 4：C14 台账复算/文档签发/归档
```

QR 可独立持续推进；外部 SLO 缺失不阻塞本地正确性任务。
某任务阻塞后继续可独立任务；不越过它所支配的验收门禁。

## 6. 收口任务卡

### C00 — 基线、历史证据和接受项清点（A0/A8/A9）

输入：绑定计划、当前源码、历史报告/台账/原始日志。
核对 Git 根、HEAD、dirty 状态、依赖源码/beam、配置、脚本摘要和原计划 hash。
从原计划机械提取 W0～W5 Tasks、AC-01～30、第 19 节硬门禁及 SINGLE/THREE_NODE 条件。
保留 W4-10 与历史 W4-010、AC-22 与历史 AC-022 的显式 alias；禁止漏算、重复算或静默改 ID。
生成 new-ledger，旧 PASS 默认待重新绑定；分类为 REVERIFY、GAP_IMPLEMENTATION、
GAP_EVIDENCE、BLOCKED_EXTERNAL、合法例外。例外只在原计划明确允许时成立。
oracle：ID 集合逐项相等、无重复、每项可追溯原文；新汇总由实际条目生成。
输出：baseline.json、ownership.json、resources.json、required-ids.json、审查台账。

### C01 — 最小协议和资源预算冻结（A0/A1/A2/A9）

冻结协议版本、配置/Registry hash、sync epoch、incarnation 注册与旧化身拒绝规则、
快照 round/token/watermark/page digest、错误返回、冷恢复语义和状态转换。
明确复制值的权威来源：谁可作为 ready authority、如何排除失效孤岛、全节点丢失后的行为；
无合法来源时保持 DEGRADED，不能从空 peer done 推导 READY。
冻结 Nmax 项/bytes、Tmax 项/bytes、Jmax 项/bytes、Bmax 项/bytes、Pmax 每页项/bytes、
Rmax 同时恢复数、Dmax 总 deadline、最大 Value/TTL、队列高水位、W 保留窗口及其单位。
复用现有容量机制；没有 journal 的方案必须提供等价有界追赶和无漏事件证明，不能免验。
冻结“达到上限”的拒绝/降级/epoch 轮换/冷恢复行为；不得删除 tombstone 后接受旧事件。
oracle：配置/实现/测试使用同一预算摘要；故障测试前冻结，失败后不能调高预算获取 PASS。
这些值是本地工程预算；生产输入未知另列，不把本地参数冒充生产 SLO。

### C02 — 接收端校验、epoch/incarnation 和混合能力（A1；A2 配套）

按 C01 最小协议实现结构/类型/数值范围、TTL/过期、payload 大小、白名单、
namespace/key 一致性、epoch、有效 incarnation、config/registry hash 的双端校验。
异常信封不崩进程、不部分应用、不扩散敏感数据；记录明确 reject reason。
恢复报告/快照/done 绑定当前 round 和合法 peer；旧 round、未知 peer 的 done 不推进 READY。
混合期只允许能力交集：legacy 失效保留，replicate/confirmed 不得绕过协商启用。
oracle：缺字段、错误 revision、未来/过期 TTL、oversize、旧 BootId、旧 epoch、hash 不一致、
伪造/重复/晚到 done 均按冻结合同处理，sync 存活，值与状态不被错误修改。

### C03 — 真正有界的元数据、队列与快照（A1）

落实 C01 数量和 bytes 上限，包括 gen/dep-gen/origin 元数据，不只约束 live/rev/rindex。
快照分批分页，不先构造全部 payload；发送和接收都有预算、deadline 和过载路径。
临界点/并发超限保持原子拒绝和有界内存；失败不能留下半写入成功状态。
避免 depcache 逐出/TTL 在校验与读取之间导致快照 badmatch 崩溃。
将 sync 按需要拆成少量职责模块，所有模块 <800 行、函数遵守项目规范，保留错误处理。
oracle：持续唯一键、删除风暴 >Tmax、Value 临界大小、快照中逐出/过期、慢接收方；
记录每个表项/bytes、mailbox、heap、CPU 峰值和停止输入后的恢复趋势。
mailbox 采样高水位不能声称严格硬上限；必须证明输入约束与超限处置有效。

### C04 — 恢复屏障、分页快照与增量追赶（A1）

READY 依赖有效 authority、版本水位和完整快照回合；不可只由第一个空 done 决定。
混合快照、新 SET、DELETE、失效和 peer 退出时，无旧值复活、遗漏或重复错误。
无权威、源崩溃、buffer/journal 超限、deadline 到期有界中止为 DEGRADED。
超窗成员丢弃受管副本、进入新 epoch 冷恢复，不能无限增量或读未知旧值。
测试“有多个 peer 尚未应答”“全部 peer recovering”“孤岛空 syn 视图”，
区分显式单节点配置与暂时失联的多节点配置。
oracle：恢复中不参与可用读/confirmed；完成后逐键值、删除集合与 authority 一致；
全节点重启且无权威数据时明确 DEGRADED/回源，不制造成功。

### C05 — 存活节点重连及 invalidate 缺口补偿（A1）

为失效流建立能检测缺口的序列/epoch 或等价机制；counter 上界交换不能代替数据补偿。
节点断连后漏失效：重连检测并有界清理受影响受管 namespace/依赖标签，
推进回填 generation 后回权威；持续分区不能用敏感缓存放行。
明确旧 generation loader 和 get_wait/waiter 在补偿期的行为；合法重新请求可继续。
oracle：节点不重启，在分区期间修改 PG 权威、漏 SET/DELETE/flush，恢复连接后
在约定 deadline 内返回权威新值，不依赖等待旧 TTL 自然到期才算通过。

### C06 — Registry 权威写入钩子与安全策略闭环（A2）

重扫直接调用、wrapper、函数引用、动态 Key、匿名 memo 及其他存储例外。
逐 namespace 冻结 policy、敏感性、authority、TTL/bytes、mutation_hook 和例外原因。
钩子追到真实 COMMIT 后；ROLLBACK 不广播；权限/吊销在分区与同步失败时 fail-closed。
业务 hook 文件与其他 owner 冲突时由 A0 指派单一写者，A2 只提交所需改动说明。
生产白名单为空；W5-02 如无安全业务 Key，按原计划允许的例外正式说明，
三节点完整 SET 实测仍必需，禁止为了通过数量要求复制 QR/token/权限状态。
oracle：计划、扫描结果、Registry、配置、实现无静默缺漏；真实 PG 事务及跨节点读验证。

### C07 — API、memo/get_wait 与回填兼容（A4）

在 C01/C02 接口后最小接入，保持旧 API 返回、单位、依赖失效和匿名 memo 合同。
涵盖连续失效、依赖标签、等待者、loader 异常/崩溃、writer 锁超时及多 writer；
禁止绕开栅栏、改变 TTL 或缩短测试窗口掩盖竞态。
未启用 namespace 保持原合同；需要启用的本地 namespace 在 fixture 显式配置。
oracle：受控时序读取不泄漏旧代值；旧 loader 可落旧物理键但新请求不能命中，
等待者不会永久挂起；两轮独立定向回归通过。

### C08 — G-QR 后端原子消费（A3/A9）

以独立方案为输入重新审查，复用既有 PG 迁移/Repo 模式，建立权威状态机。
REST status 与 SSE 通过同一原子 claim/consume 路径；syn 只发送事件通知，
不得把 login_token 广播给全部订阅者。明确 claim 成功但传输失败的语义：
保证最多一个合法消费者，重试不能向另一个消费者泄漏 token；无法无损重试时要求重新扫码。
保持既有客户端 wire contract；若兼容无法成立则 BLOCKED_CLIENT_CONTRACT_CHANGE。
并发 confirm/cancel/expire、身份错配、重复请求、会话过期、SSE 断开须有明确结果。
迁移 apply/rollback 只在专属临时库执行；回滚不得把 token 恢复成可再次领取。
oracle：真实 PG + 至少两个后端节点，status/status、SSE/status、SSE/SSE 同步竞争，
仅一个合法消费者收到凭据，其余得到既有兼容非成功响应；不得以单进程 mock 代替。
测试日志仅记录合成标识/计数，不记录可用凭据。

### C09 — 三节点完整故障与资源矩阵（A5；依赖 C02～C07）

复用现有 dcheck，真实三个 BEAM 节点、唯一 cookie/端口，读节点本地状态作 oracle。
补齐并发 SET/DELETE、乱序/重复、TTL 不延长、重启化身、逐出、快照 delta、
partition >W、删除风暴 >Tmax、清理后旧 SET、authority 崩溃、全 peer recovering、
旧 round done、hash 不一致、失效缺口、资源上限/慢接收者/恢复 deadline。
所有 required 场景至少两轮独立通过；任何修复后重跑受影响场景与相邻回归。
loopback 仅签 LOCAL_THREE_NODE_PASS，不代表三机网络或目标硬件资格。
oracle：使用屏障/事件条件和有界等待，禁止 sleep 后偶然读到预期就宣称收敛。

### C10 — 混合版本、滚动升级与回滚（A6；依赖 C02～C07）

冻结真实旧版本 SHA/源码/beam 与新候选；禁用“新代码模拟旧版本”替代真实旧节点。
覆盖 legacy SET/失效、能力交集、不可协商、不同 hash、旧化身、升级中断、
回滚清受管副本/epoch、旧 snapshot/信封回放拒绝；历史驱动时间戳改成真实 UTC。
回滚后读回权威，无 V2 数据跨版本复活；退出 DEGRADED 手段与用户可见行为写入运维文档。
oracle：两轮完整矩阵零失败，状态与日志能区分 READY/RECOVERING/DEGRADED。

### C11 — 监控、诊断和文档一致性（A8；产品指标由 A1 接入）

复用 elib_metric 和现有 Grafana/Prometheus；补齐协议/config/Registry hash、epoch、
incarnation、状态、last accepted/applied、peer lag、队列/资源/拒绝原因和 legacy 计数。
静态规则校验不替代消费验证：隔离本地 metrics 请求、Prometheus scrape 与必要告警注入，
核对实际指标、规则和面板 query；没有本地运行能力时保留 GAP_EVIDENCE。
运行时标签有界，禁止按 Key/token 生成无限标签或泄露敏感值。
oracle：拒绝/过载/缺口/恢复超时各产生可区分信号，监控值对应节点真实状态；
配置/Registry/运维/测试负载单位与摘要一致。

### C12 — 最终候选测试及本地性能（A0/A5/A6/A7）

冻结所有代码、配置、依赖与脚本为 CANDIDATE_SHA，停止产品代码写入；
对候选执行定向、三节点、混合/回滚、QR、全量 EUnit 和性能，原始 exit 必须保存。
既有构建入口优先：`make compile`、`make eunit-local t=<实际套件>`、
`make eunit-local`；C00 核实配置、库和入口后才运行，不能连接共享/生产目标。
最终全量要求 exit=0、Failed=0、非预期取消/跳过=0，日志计数一致；
失败即 FAIL，隔离复跑不能替代整轮通过。无关失败不擅自修复，保留外部归属及阻塞证据。
缓存性能测试沿用 W0 可比 profile，同时覆盖新 API/协议、S/M/L Key 和 Value 分布、
混合业务负载、资源包络；Stress 仅在环境可承载且原计划允许时运行。
冻结测试前的本地相对阈值与统计判定（原计划有值则沿用），保留与生产 SLO 的区别；
基线/候选交错至少三组，记录 CPU/内存/调度器/负载/依赖/编译/profile。
先验证环境适用性，不预设“重启必能修复”；需用户主机重启时记录 WAITING_USER_ACTION。
吞吐、P99、错误率、同步收敛时间、mailbox/heap/metadata 峰值全部报告，
同字节 W0 微基准只作环境控制，不能替代新 V2 代码性能验证。
oracle：本地冻结阈值及资源预算全部达标；不达标保留 FAIL/PARTIAL，不调阈值。
候选代码/依赖/配置/driver 变更后重冻候选并重跑受影响门禁；全量须绑定最后代码候选。

### C13 — 原计划 SLO 与外部输入判定（A0/A7）

收集目标硬件/拓扑、业务峰值/增长、Key/Value 分布、绝对 SLO 与 headroom 的权威输入。
输入齐备则冻结并在对应环境执行 AC-26/27；没有对应环境不能用本机结果外推。
缺失则 C13 的“输入审查及缺口交付”可 PASS，但映射的原 AC 仍 BLOCKED_EXTERNAL，
写明需要谁提供什么、影响哪些 AC、如何恢复；不得让映射表吞掉原始阻塞。

### C14 — 证据复算、独立审查与签发（A0/A8/A9）

新建本轮 report/ledger，原报告保留历史，新增指向本轮的简短状态索引。
每项新 PASS 需 candidate、输入摘要、命令、exit、oracle、次数/计数、证据路径/sha256。
verifier 机械验证 ID 集、alias、摘要、文件存在、候选一致、完整日志与结果统计，
任何 required 非 PASS 阻止相应整体 PASS；空证据、过期 SHA、1970 时间戳等拒绝。
verifier 有小型 self-test：丢 ID/重复 ID/篡改摘要/非零 exit/混用候选/伪 PASS 必须失败。
历史证据只有受测输入完全不变且范围匹配才能继承，记录 equivalence；
历史全量不得继承为新候选全量，旧部分场景不得覆盖新增故障合同。
A9 独立审查协议、资源、QR、数据安全、候选绑定和所有 BLOCKED/例外；
阻断发现由 owner 修复并重验，不能靠文档改成 PASS。
最终并列签发：后端本地收口、原计划验收、生产资格三个状态及精确未闭合集合。

## 7. 证据、重试和持续推进

原始日志在仓外 `~/.codex/cache-closeout-runs/<session>/`，禁止放生产 PII/凭据。
仓内新交付位于 `docs/architecture/cache-v2.2.1/closeout-v1/`；
机器摘要、脱敏结果、台账和运维说明可入仓，原始日志不强行入仓。
每轮记录 started_at/finished_at、candidate/working_tree_digest、命令/exit、
profile/config/dependency hashes、环境、oracle 结果、sha256 manifest。
实现完成但未验状态为 IMPLEMENTED_NOT_VERIFIED；等待资源为 WAITING_RESOURCE；
缺外部输入为 BLOCKED_EXTERNAL；需要特定用户动作则 WAITING_USER_ACTION；
正确性/安全/资源门禁失败为 FAIL，超过修复预算为 NO_GO。

每个独立 root cause 最多两轮自动修复重试，每轮记变更和重跑结果；
不能重命名同一失败规避次数。环境失败先诊断目标归属，禁止动其他会话资源。
阻塞一个任务时持续执行无依赖的其他任务，直到可执行队列清空；
仅在全部目标达成或确无合法可执行任务时结束，后者交付 blocker/handoff 而不是宣称完成。

交付必须包含：原计划接受项映射、新台账、故障/性能矩阵、最终候选和差异、
摘要校验输出、已知限制、回滚步骤、下一步外部输入要求。

## 8. ZCODE 执行指令模板

将本文件路径、最终 SHA-256 和本文件 Git 提交由交付者填入，再开始执行：

```text
任务：CACHE_V221_BACKEND_CLOSEOUT_V1
只执行 imboy 后端收口，不修改 imboyapp、imboyadmin 或其他仓库。
计划：docs/architecture/cache-v2.2.1/backend-closeout-plan-v1.md
PLAN_SHA256：由用户提供的本文件最终摘要，必须机械核对，禁止自动重绑。
原计划 SHA256：a5523ee731bc822018831586e3d747ba109c2c66e734daf5633b494d9b18c713
原计划快照：docs/architecture/cache-v2.2.1/w0/plan-v2.2.1.md
执行全部 C00～C14，最多 10 个活动 agent（含协调者/审查者/后代）。
先做 C00/C01；按单文件唯一 owner 和 DAG 推进。sync 核心单写者，重测试串行租约。
允许后端实现、测试、迁移文件与专属临时测试库迁移验证；保持客户端 wire contract。
每个 root cause 最多两轮自动修复；阻塞任务旁路继续独立任务，禁止伪 PASS。
按功能做精确路径本地提交，Git 身份 leeyi <leeyisoft@qq.com>，不改变全局配置。
不得 push/部署/生产或共享库迁移/重启主机/干扰外来 WIP/使用真实凭据和数据。
最终复算原计划全部任务/AC/第19节门禁。原始 exit、真实 oracle、最终候选摘要必需。
全量非零退出不能用隔离复跑签 PASS；loopback 不能代表三机/生产容量。
缺生产 SLO 时允许交付 BACKEND_LOCAL_CLOSEOUT_PASS，但原计划总体必须仍为 BLOCKED。
持续推进至全部可执行任务验证完成；外部依赖无法满足时交付精确 BLOCKED/handoff。
最终返回三个独立结论、本地提交、验证命令/exit/计数、证据摘要及未闭合接受项。
```
