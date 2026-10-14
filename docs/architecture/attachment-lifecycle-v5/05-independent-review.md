# V5.5 交付前评审边界

2026-10-14。状态：DELIVERABLE_WITH_ADMISSION_GATES。20任务/七Gate NOT_STARTED；DESIGN_ONLY，implementation_authorized=false。目标9.9不作既定评分；作者不能签自己的独立验收。当前可交付ZCODE进行准入审查，用户发送06实施指令且准入通过后才实施。

## FACT → GAP → DECISION → CONTRACT → TASK → TEST → EVIDENCE

V5.4重新取证复评分8.90/NEEDS_REVISION（非第三方签核），六维依次9.0/8.8/9.0/8.8/9.0/8.8，等权均分8.90。本版修订如下：

| 问题 | 反例与最小修订 | Task/Gate及验收 | 状态 |
|---|---|---|---|
| R54-01 P1 | EB原行物理清理后同client_upload_id重试可能重建；在既有queue复制UUID+原声明，父锁后UUID锁保证跨表唯一，恢复查询墓碑优先 | W1 SQL/直接DML；W2恢复API；W4实际Purge两入口/G1/G2/G4；新asset/key/capability=0 | 已落实合同，运行待验 |
| R54-02 P2 | EB adapter只传mime/size，底层暂停后新签300秒；W2明确拥有adapter及冻结Context全调用链 | W2/G2四入口绝对expiry，W4仅统一driver | 已落实任务与断言，运行待验 |
| R54-03 P2 | enterprise泛化叶可只走seat而漏widget；三内容case精确绑定四入口、明确非适用项 | G2/G4/W6真实route/handler/request_id；mutation29/30拒绝入口替代/缺失 | 已落实集合，运行待验 |
| V55-PARALLEL | 原20任务全链串行；W5两任务已无共享文件，去掉无意义前后依赖 | W5并发、合并候选定向重验、提交串行；runner并行崩溃/重叠/部分向量自测 | 设计明确，工具能力W0核验 |
| V55-BASELINE | 三仓推进，旧WIP/未知delete结果描述过期，新增迁移172 | 当前基线重绑定；复用epoch/abort/auth hook；W0重建171 Up，W4检查阅读级联 | 源码/SQL静态证实，运行未验 |

## 质量判定与准入

没有已知未分配的P1/P2设计修订项；这不等于9.9或执行PASS。复评只对当前manifest有效。独立Reviewer按execution-contract.design_quality_rubric六维等权评分；不得用2272叶、绿灯数量或目标分数替代论证。若发现实际合同冲突，CONTRACT_CONFLICT并保留反例，不以扩预算/减测试/自改计划消除阻断。

自动验收范围为LOCAL_CANDIDATE_PASS：真实PG/Garage/Disk、四上传入口、两删除队列、真机/浏览器、冻结三仓候选全量及完整提交向量。产品UAT、发布和生产仍单独授权。历史Schema日志只含170 Up，当前171 Up/strict ledger/新迁移/运行时均待Gate。当前App ios WIP原样保护；端口、凭据、设备、容量与独立Reviewer能力必须在W0预检，缺失有限退出，不能假定无人值守等于永不阻断。

本次只改本计划目录，静态校验任务/DAG/叶/表内与跨表身份/manifest及迁移命名。修订未增加生命周期表或第三queue；仅两列恢复快照、既有adapter小范围前移、入口维度及并行协调。剩余有界代价是后端转发/完整hash读取与永久墓碑容量，必须在既定最大文件/负载Gate验证。

当前69 cases、2272唯一叶、W6-01 1271真实集成叶（1234带driver，37 catalog/plan），34验证器变异；66 Disk前置叶、48 Hold+24 Retention每入口组合仍保留。静态通过不代独立签核和运行验收。

---

# 历史V5.4修订记录（以下状态和“当前”仅指2026-10-13）

以下仅归档，不是当前准入指令；不覆盖上面的V5.5结论或06执行入口。

日期：2026-10-13。当前 **REVISED_PENDING_INDEPENDENT_REVIEW**。本文件记录历史审查与作者修订，不能把作者静态自检当独立关闭。20任务/七Gate仍NOT_STARTED，DESIGN_ONLY / implementation_authorized=false。

## FACT → GAP → DECISION → CONTRACT → TASK → TEST → EVIDENCE

最近V5.3质量复评为 **NEEDS_REVISION，8.50/10**，是作者对计划质量的重新分析，不是另一位独立Reviewer的签核，也不是实施验收。该评分仅绑定V5.3 manifest `50731b0d6a08189d28185bbfaf29db15e319e2b7ab16d773c7730840f8414e24`。V5.2历史独立评审为NEEDS_REVISION/8.03，绑定`7fb3a86bb959395d6eec9f56f38a8c59ab91b3e31209c69a470e3a685a1d464b`。V5.4尚无新评分，不以修订数量或测试叶数推算9.9。

| 六维 | V5.3评分 | 扣分依据及未验证边界 |
|---|---:|---|
| 架构正确性 | 8.6 | M3函数前向依赖；真实阶段运行未验 |
| 最小改动程度 | 8.8 | 复用两队列合理；必须核查新防覆写边界的成本 |
| 代码匹配度 | 8.7 | 会话原声明/主体持久字段缺失；调用方仅静态核对 |
| 可执行性 | 8.0 | 活holder无限锁等待；runner及设备资源未验 |
| 安全性 | 8.2 | 旧有效PUT可覆写已确认内容；真实drain未验 |
| 测试覆盖设计 | 8.7 | 关键防覆写/原声明恢复反例缺失；required均未运行 |
| 等权算术平均 | **8.50** | (8.6+8.8+8.7+8.0+8.2+8.7)/6 |

## 必须修订的设计缺陷与本版处理

| ISSUE_ID / 严重性 | FACT/GAP / 具体反例 | DECISION/CONTRACT最小修订 | TASK/Gate / TEST验收断言 | EVIDENCE |
|---|---|---|---|---|
| V53-01 P1 | W4-01/02在expand库调用意图函数，函数却到M3才创建；M3又等writer完成 | M2安装所有协议/守卫函数定义；M3仅挂守卫/收旧权限 | W1分库/G1、W4-04/G4；不加载M3也可调用，定义摘要一致 | 合同修订，SQL未实施 |
| V53-02 P1 | presign成功后重启，只有ID无原MIME/filename/widget session；不同声明无法可靠判等 | 两既有session各加不可变upload_registration；原主体独立于可匿名化审计actor | W1 registration-integrity、W2/W5/G2/G5；同ID异声明拒绝、跨session恢复0、合法匿名化不授权恢复 | 当前repo/迁移直接证实缺口，恢复未验 |
| V53-03 P1 | PUT A→stat A→confirmed/bound→旧URL PUT B；key未变但内容变了 | 同session durable content pin，客户端受控裸PUT；真实hash确认；Garage绝对内部expiry/真实drain，Disk同I PUT/DELETE锁 | W2 content-pin/confirmed-content、W4 driver-content/G2/G4、W6；B不落存储，合法对象hash保持；迟到PUT不在DELETE后重建 | 当前签名器host/UNSIGNED-PAYLOAD证实能力边界，新协议未验 |
| V53-04 P2 | runner持锁后SIGSTOP；lease过期但第二runner永远阻塞取锁 | LOCK_NB≤2秒有限拒绝；独占非权威observation，不写state/events、不强抢 | W0 runner-selftest/G0；exit75、权威日志不变、无子动作；人工可信恢复后调和 | 设计已明确；不是承诺完全无人干预恢复 |
| V53-05 执行阻断 | moment_ds在相同HEAD下继续外部修改，旧hash证据过期 | 重绑定当前源码快照、保留外部拥有权，明确ACL/after_commit影响及运行重验 | W0准入、W3-01/G3、W6；错hash拒绝，不继承历史PASS | 当前hash/diff静态核对；旧hash完整源码未保留，不能重构全差异 |
| V54-01 P2 | confirmed session已有不可变FK；若GC把session改intent，confirmed iff linked CHECK拒绝，清FK又违不可变 | 普通confirmed session终态保持，GC只转metadata；未确认session才进intent/deleted | W1 session-link-integrity/G1、W6；完整GC后原FK不变，恢复返回真实删除状态不恢复权限 | 本轮一致性复核发现并修订，运行未验 |

上述条目为 **ADDRESSED_PENDING_REVIEW_AND_EXECUTION_GATES**，不是独立关闭。新上传网关是修复旧可用URL覆写反例的边界改变，增加后端转发和Confirm读取成本；W2/W4必须用既有最大文件、并发、内存/临时空间、CORS/进度实测，不通过就阻断，不能以mock或取消校验降级。

## 历史V5.2问题及V5.3处理（未重写为本版独立通过）

| ISSUE_ID / 严重性 | FACT/GAP与具体反例 | DECISION/CONTRACT最小修订 | TASK/Gate与TEST断言 | EVIDENCE状态 |
|---|---|---|---|---|
| V52-01 P1 | W0-02要求未来strict runner，而W0-03依赖02；base查询不存在gc_verified | raw schema与static unknown留02；strict另库及追验放03 | W0-03 strict-ledger-bootstrap/G0；dirty=false且版本hash精确相等 | 合同已修订，运行未验 |
| V52-02 P1 | W3要求未适配的reject-size等入口阻断Hold，适配却在G3后 | W3只资格协议，W4整生产矩阵，W6双driver | W4-01 hold-entrypoints/G4；48 Hold+24 Retention全入口拒绝 | 合同已修订，入口实证未验 |
| V52-03 P1 | app直接INSERT queue ready，可绕过应用Hold判据触发Worker | 受限intent函数、列/表权限、真实角色负例；既有两队列不变 | W1 intent-authority、W4 writer/激活、W6；live引用/Hold/缺审计不得形成可执行job | SQL设计已补，函数/权限未实施 |
| V52-04 P1 | 内部presign外层幂等事务保存signed URL，之后COMMIT失败 | handler事务只保存稳定身份，提交后签名；普通handler显式503 | W2-01四入口/AC01；未知COMMIT无能力且同ID恢复 | 当前源码支持缺口，修复未验 |
| V52-05 P1 | presign已返回后崩溃，URL未持久化；原接口重新生成key；seat辅助函数直到confirm后才回调 | 同session status/renew、网络前client_upload_id、辅助函数逐阶段持久checkpoint | W2恢复API/W5/W6；过期能力同key、账号切换迟到回调零写入 | 接口/SQL/客户端任务已绑定，运行未验 |
| V52-06 P1 | VERIFYING超时无blocked转移；父死子活、lease过期导致双执行；event/state崩溃顺序不明 | OS锁、完整事件先fsync、原子snapshot、有限进程组终止、跨重启deadline | W0 runner新增故障叶；部分commit只读调和不重复提交 | 状态合同已补，runner尚不存在 |
| V52-07 P2 | seatSend.test.ts不存在；e2e不在配置testDir；换号仅文字要求 | 真实三组seat测试/辅助函数/API adapter及tests/e2e路径；恢复必验叶 | W5-02/W6-02；每个widget/seat场景和真实设备浏览器来源 | 路径静态核对；测试未运行 |
| V52-08 P2 | 绝对上传人不可变使迁移118用户删除SET NULL失败 | 审计actor合法匿名化例外；业务归属不变且不授权GC | W1 audit-actor-anonymization/W3/W6；合法用户删除与非法置NULL分开 | 迁移事实已证实，新guard未验 |
| V52-09 P2 | 顶层WIP过期；修改mode会改变授权hash；后续提交又不匹配初始HEAD | 当前快照与历史分开；外部可信receipt；任务输出hash和候选向量递进 | W0准入/runner及所有Gate；错hash、scope、部分向量拒绝 | 静态绑定已更新；没有实施授权 |
| V52-10 P2 | 顶层模型无关但W0-03仍固定GLM/Flash | 删除固定模型实施边界，主执行/Reviewer职责不变 | W0-03/static；合同各处一致 | 文本修订，不等于能力/验收证明 |


## 必须通过执行Gate验证

M1/M2/M3实SQL/角色权限/匿名化可见性、strict ledger、全部writer、百万行DDL锁窗口、升级/Down/恢复、两真实driver、并发故障窗口、120秒lease+reaper、全部客户端恢复及真机浏览器，仍为UNVERIFIED。测试未执行本身不等于设计错误，未知也不等于安全已证明。缺设备/服务/资源按required Gate阻断；不得降级required。

本版67 cases、2008唯一叶，W6-01真实集成case 1103叶（其中1066带真实driver）；28验证器变异、66既有Disk前置协议叶；四级Hold×12入口的48组合保持，另含24个Retention入口组合。数字仅用于集合一致性。旧56/1279/693属于更早版本，不能拿作当前输入集合。

本轮仅静态校验JSON/20任务与7Gate/依赖/精确叶/W6展开/路径/源码hash/manifest，并执行只读迁移命名门；历史原Up/catalog/probe日志未重新运行。没有实施代码、运行集成验收、连接业务库、提交或写memory。外部cache WIP仍活跃，baseline是带采样时间的输入，实施必须重新核对，不能声称所有foreign保持不变。

## 可选精简与准入决定

复用原计划gateway、现有elib_oss/EB代理、现有session JSON字段及两删除队列；没有新SDK/第三队列/第二生命周期系统。不增加无需求的对象版本框架或通用Agent平台。纯候选全量/台账场景仍只验一次，安全的真实driver/故障叶不为减少数量而删除；重复脚手架可复用，不能重复记一次执行为多个独立oracle。

接近9.9所需最小后续工作：独立检查本版函数安装顺序、持久声明/内容pin与所有writer边界、旧客户端裸PUT兼容、有限锁失败语义；核查全部任务预算和真实资源可获得性。明确网关成本与Garage drain不能证明时的保守结果，复核没有新循环或丢失验收场景。无新独立评审和运行可行性证据，不能客观承诺9.9。

**当前不启动W0，不推进任何Gate。** 本轮只授权修改计划。后续独立复核并取得绑定本版manifest/当前baseline/任务范围的可信实施receipt和所需独立commit授权，才从W0引导；G0完整通过后进W1，不越级。moment_ds foreign冲突仍须在W3-01写入前解决；用户UAT/发布/生产另授权与验收。
