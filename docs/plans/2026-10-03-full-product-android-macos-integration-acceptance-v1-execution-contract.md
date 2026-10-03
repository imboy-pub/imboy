# 全产品集成验收 V1.1 — 执行合同与小任务卡

日期：2026-10-03。仅计划，所有任务 PLANNED。配套主计划与 acceptance-set.json 共同使用。下述新脚本、运行表、目标目录均为**待实现产物**，不是已有可执行能力。现存目标仅为复用候选，必须先审查真实性、配置和断言。

## 1. 正在执行的 E2EE 计划：双向交接而非重复实现

上游：`imboy/docs/design/2026-10-03-e2ee-production-excellence/plan.md`，其文件 SHA 绑定在 acceptance-set.json。用户确认正在执行；本轮只发现 plan.md 与 zcode-prompt.md，未取得运行账本/阶段/候选证据，**执行阶段 UNKNOWN，不能推定任何 C 卡已 PASS**。上游目录为已有未跟踪工作，本计划不得修改、暂存或覆盖。

### 1.1 两条运行通道

- PREPARATION：冻结当前已提交快照，独立 worktree 做覆盖映射、用例设计、runner负例、L0/L1和可用设备的早期冒烟。结果标 `evidence_phase=PRELIMINARY`；通过只证明该旧快照，不进入最终通过分子。
- ACCEPTANCE：上游给出稳定、已提交、可构建的 candidate pair 后，本计划集成人纳入自己的候选，重跑受影响域；最终冻结三仓并重跑全部 required L2。证据标 `evidence_phase=FINAL`。不跟随上游未提交工作目录，不将“正在修”判断为PASS。
- C15–C17 高阶扩展不默认成为基础集成启动门；若其已接入选定候选/启用profile，则必须纳入相应行为与安全门，不能仅因属于W4就排除。C14之前可共享测试设计，但本计划不等待上游全部35号AC才开始工作。
- 客户OA外部联调不在范围；内部协议本地行为仍测。iOS不在本计划平台集合，绝不修改上游AC-22/27/33平台要求。上游因iOS缺失BLOCKED时，Android/macOS可单独通过本地profile门，E2EE PRODUCTION_QUALIFIED仍保持上游实际结论。

### 1.2 交接包与依赖方向

上游Coordinator导出只读 `handoff.json`，至少含：plan_sha256、run_id、backend_sha、app_sha、admin_sha或不适用理由、已审commit列表、原始evidence manifest及hash、逐AC实际状态及平台子格、feature profile、运行时策略、协议/原生库/依赖版本、schema/migration列表、build配置指纹、已知缺陷/决策限制、可共享资源及其当前owner。接收方验证提交存在、祖先关系、哈希、构建可复现和安全不变量；单一 verdict.json 不足以交接。

**避免循环依赖**：上游C11/C13负责自身安全资格测试，不等待本计划F17；本计划上游交接检查不要求上游消费本计划结果。双方可以复用同一次真实journey原始证据，但各自独立检查其集合与候选；不能互相引用对方PASS作为证据。

| 受影响的本计划域 | 上游实现/行为交接 | 必须补验的产品影响 |
|---|---|---|
| F02/F08 账号、设备、安全、存储 | C01/C03/C05/C07；AC03/04/07/08/11/12/15/16 | 撤销设备后断连拒绝、切号无串号、备份错误密码/恢复、退出清密钥、通知不泄露 |
| F03 单聊/附件/通知 | C02/C04/C05/C07/C10；AC05/06/09/10/11/12/15/16/21 | 双向密文链路、离线重试/重启、未信任设备阻断、篡改附件不显示、复制/转发符合策略 |
| F04/F05 群与群应用 | C04/C08；AC09/10/17/18 | 成员变化旋转、踢出者不能读新消息、历史授权、群文件归属与加密范围 |
| F06/F07 社交内容 | C04/C05及实际附件调用链 | 朋友圈/频道按已批准策略正常可见，不能误走仅真人E2EE解密入口，也不能隐式降级 |
| F09/F10/F11/F12/F15 企业、坐席、API | C01/C06/C08/C12；AC04/13/14/17/18/24/25/26 | required/托管/AI隔离、撤权、跨租户资料；坐席合法路径与非法明文导出 |
| F13/F14/F16 RTC、其他、全页 | C05/C06与实际依赖图 | 登录/本地库/权限变化回归；RTC不冒充文本E2EE覆盖，资金不误重复请求 |
| F17 全局 | C12/C14、AC00–29实际资格账本 | 最终候选、迁移、兼容门、明确未完成平台/安全资格 |

上述是最小影响集合；F00源码调用关系只能扩充，不可无依据缩小。AC编号来自上游，统一转换为 `AC-03` 等规范形式存机器表。

**最终产品profile门**要求该profile用到的E2EE实现AC及Android/macOS旅程子格有真实证据；如果上游仅本地实现通过，最终设备门仍BLOCKED。上游C14/AC29全局资格未完成时，可交付明确限定的Android/macOS产品集成结论，但不能称E2EE投产资格或全产品发布完成。原始上游AC仍保持BLOCKED/PARTIAL，禁止创建“适配版AC PASS”覆盖它。

### 1.3 配置矩阵：不能临时关闭加密换绿

F00登记 `product-profiles.json`：每个消息域/内容域的实际支持策略（personal C2C/C2G、企业required、已批准托管/AI、普通频道/朋友圈、客服），来源为产品决策和运行时配置，不能自行发明支持模式。至少覆盖候选默认profile及已支持的关闭/拒绝边界；关闭profile仅证明关闭行为，不替代开启profile。每个case标profile，后端最终生效配置与App生成配置共同入指纹；测试中切全局策略必须重启隔离实例并新建fixture generation，不能同时污染别的卡。

上游变更plan、SHA、schema、原生库或配置：保留旧证据，追加失效记录，重生成候选。默认最终证据全部重新绑定并实跑，不将旧日志换SHA。若只文档变动，也需记录差异并通过复用校验，不靠口头“没影响”。

## 2. 跨计划资源与唯一写者

1. 源码：E2EE所有权内的identity/olm/megolm/outbox/retry/attachment/CryptoStore/notification/feature/migration文件，本计划只读。缺陷提交最小复现、预期与测试patch建议给对应owner，由上游实现；不得另开worker抢修。非重叠业务修复才由本计划获独占路径后实现。本计划不替上游分配迁移编号。
2. 全局资源：沿用上游已建立的资源登记作为共同入口；若无法取得其状态，本计划只能进行不占用未知共享资源的文档/静态准备。不得在各自RUN_ROOT建两份互不相知的设备锁。两个Coordinator先记录同一 `lease_root`、资源规范名、协议版本及确认时间；这只是本地交接，不自动发任何第三方消息。
3. 资源名覆盖物理Android序列号、macOS用户/App容器/钥匙串/端口、Flutter SDK cache、vodozemac构建输出、各worktree build、DB/schema、Garage前缀、relay端口、Admin浏览器profile。不同worktree仍可能共用App容器和钥匙串；未证明隔离就独占同一macOS资源。禁止靠删缓存、卸载现有App或重置钥匙串抢资源。
4. 租约复用当前原子mkdir及owner.json，保留 `run,step,agent,process,resource` 必需字段，增加plan/run/card/heartbeat/namespace。多资源申请由唯一资源协调者按规范排序事务授予；任一失败立即释放本次已获资源，不能部分占用等另一端；worker不得自行多锁抢占。
5. 15分钟无heartbeat只触发调查；核对PID、进程开始时间、设备运行与owner确认后回收。损坏锁fail closed；上游未知或仍活跃时不抢占。heartbeat≤5分钟。
6. 先串行完成共享Flutter SDK预热/依赖准备，冻结SDK hash；独立build目录做一个双任务试跑，验证SDK锁与原生构建不互扰，再放行并发。仍争锁则串行构建，绝不删 `bin/cache/lockfile`/`.upgrade_lock`、禁锁或修改SDK。原生库缓存键含平台/架构/库版本/编译参数。
7. 总并发由共同资源表准入，不把本计划4名直接叠加上游10名。没拿到CPU/内存/设备预算时，本计划最多做一个不侵入上游的准备任务。设备有限时固定批次轮转，先P0消息/撤权，再域功能；已授予旅程不抢占。
8. L3每个冻结候选只由一个门执行者运行，后端两次连续通过视为一个L3门。上游C14证据只有源码SHA、runner/test树、配置、依赖、fixture和所需断言全相同，且独立复核通过，才可共享；否则安排错峰运行。不能因为“同机器跑过”复用。

## 3. 小任务模板、覆盖闭合与派发

配套 `*-dispatch-cards.json` 是派发索引。每个小卡继承主卡Acceptance，不新增虚假完成分母。每卡包含任务边界、候选复用目标和行为oracle；下列合同对每张小卡均强制适用。

**输入**：当前候选/上游版本、属于本卡的seed/page/API清单、所需profile/角色、真实调用入口、资源能力表。设计阶段不依赖设备。**所有权**：只写本卡新目录 `imboyapp/integration_test/full_acceptance/<小卡ID>/` 和 `RUN_ROOT/cards/<小卡ID>/`；Backend测试新目录由本卡精确manifest登记；Admin业务交互仅F12创建，F15只拥有协议/RBAC跨域测试，不重复改F12 spec。现有目标只读参考，要修改须取得其唯一owner租约。catalog、公共fixture、runner由F01唯一owner负责。

**步骤与阶段门**：
1. 领取本卡输入并读取实际源码/路由，生成 `case-map.tsv`；将新增入口和旧种子建立多对多映射，不继承历史PASS。
2. 每个case填写前置、角色、操作、UI预期、后端可观察事实、失败/恢复路径、平台/profile、证据关联ID。缺功能记实现缺口，不造测试PASS。
3. 复用目标先检查：真实UI触发、有效配置注入、唯一账号UID、无生产地址、非mock终验、断言可失败；未合格的目标重构或新建，记录理由。先写会失败的行为负例，再做最小修复。
4. L0：本域本地测试，不等F01设备；L1：只要求后端与夹具能力；L2：按平台能力申请租约，跨端另要求双端屏障。参考目标不是直接执行命令；F01为实际target注册argv/env名/timeout/oracle。
5. 修复提交返回owner，经独立review与受影响回归；每个独立功能/缺陷一个commit，不混用户WIP。
6. 输出用例/原始日志/exit/test-count/业务ID/服务端读取/双端UI证据及hash，提交给Coordinator汇总；worker只写自身结果，不能改总集合。

**粒度**：每张小卡默认≤25个唯一用户行为case或半日准备工作，任一超限继续拆 `<ID>-B01...`，一批一个owner；1个行为可映射多个旧重复行，但必须说明等价，不能删掉不同角色/边界。设备运行目标控制在15分钟旅程内，长旅程单列预算。F08的374行不能派给一个worker“全做完再交”。

**覆盖全集**：F00先锁定1846种子、1872生成项、188页面候选和API/WS/Admin路由的输入指纹；各域并行映射。旧种子 `F04+F05` 是待裁决分组，必须分到唯一小卡主owner，可关联其他卡。22歧义行读原文修复映射，原seed保持审计不改写。新增功能分配稳定 `NEW:<repo>:<domain>:<ordinal>`；删除/合并功能有源码定位和去向。

`required-cases.tsv` 主键 `(case_id,platform,profile,role_variant)`，另外保存parent_acceptance_ids、seed_ids、source_refs、owner、target、applicability、approval_ref、oracle。每个required主键必须且只能有一个最终结论；所有输入行必须至少映射一个case或有审核后的删除/合并/范围处置。目标去重按实际runner target+配置+平台+角色夹具，不将不同profile误合并。

F00全局关闭条件：输入去向覆盖100%、新增路由覆盖100%、重复主键=0、无owner=0、歧义=0、placeholder=0。分域闭合先解锁该域；全局闭合才允许F17。`F00-A02`中的NOT_APPLICABLE只对真实平台不适用项使用，缺设备/未实现/上游未完均不可N/A。原合同required项的缩减必须另版批准并保留旧集合，不能执行中编辑分母。

## 4. 基础小卡与能力依赖（不再一刀切等待F01全绿）

| 小卡 | 所有者与交付 | 解锁条件 / 验收 |
|---|---|---|
| F00-S01 输入与上游绑定 | Coordinator；输入/hash/当前三仓HEAD/上游UNKNOWN状态 | 可立即只读；输入快照和保护路径完整 |
| F00-S02 分域映射 | 各域小卡owner提交patch，Coordinator唯一汇总 | 每域独立关闭；全局按第3节核对 |
| F01-S01 runner资格 | 基础设施owner；现有runner扩展而非新框架 | 正负例均可本地执行；特别修正/验证 auto_test.py 在线run硬编码android问题 |
| F01-S02 后端与夹具 | 基础设施owner；隔离配置/seed/reset/namespace | 专属DB/对象/端口获租约；重播不新增重复关系，错误owner拒绝清理 |
| F01-S03 Android能力 | device runner；device/config/UID/build证据 | 真实设备与本卡授权/租约；缺失只阻塞Android和双端 |
| F01-S04 macOS能力 | device runner；App容器/窗口/config/build证据 | 获独占租约；不能用widget虚拟窗口代替真实窗口 |
| F01-S05 双端屏障 | runner owner；跨端就绪/消息关联/超时取消 | 两端ready后才发消息；任一端失败另一端退出并保留证据；顺序独立脚本不能通过 |
| F01-S06 verifier与校准 | runner owner开发、独立review | 第6节假绿负例全通过；第7节样本与估时可复算 |
| F15-S01 路由与WS清单 | Backend域；operation矩阵 | 本地可枚举；不能只看OpenAPI存在 |
| F15-S02 真后端授权 | Backend/Admin域；协议、RBAC、幂等、租户负例 | 仅后端/夹具能力；F12业务spec只读 |
| F16-S01 双平台导航 | UX/device runner；页面/状态矩阵 | 各平台独立可执行；缺另一平台不阻塞 |
| F16-S02 人工体验 | UX整理，用户评审 | 可审截图/录屏+问题表；未评审为BLOCKED/reason=USER_VISUAL_REVIEW |
| F17-S01 集成冻结 | 集成人；reviewed commits/candidate/required集合 | 全域预验及上游交接就绪；冻结后不修改候选 |
| F17-S02 全局门与设备重验 | 唯一gate runner；L3/最终L2原始证据 | 冻结候选+资源租约；与上游错峰或严格校验共享证据 |
| F17-S03 独立裁决 | 非实现作者的只读verifier | 任何状态均执行；只重算，不改结果，不合并源码 |

F01-A01由S03+S04共同成立；A02对应S02；A03对应S01+S05+S06。该父ID汇总关系不等于所有业务必须等它们全部PASS。能力标记为 `OFFLINE_READY/BACKEND_READY/ANDROID_READY/MACOS_READY/PAIR_READY/UPSTREAM_HANDOFF_READY`，不是测试状态或PASS证据。

## 5. 结果状态、重试和恢复

| 层 | 权威值/规则 |
|---|---|
| scheduler_state | WAITING、READY、RUNNING、DONE、INTERRUPTED；DONE指执行结束，不等于测试通过 |
| test_status | PLANNED、PASS、PASS_WITH_SKIPS、BLOCKED、FAIL、FLAKY；UNKNOWN仅未执行占位 |
| scope_verdict | PASS、PARTIAL、BLOCKED，与现有auto_test schema兼容 |
| applicability | REQUIRED、NOT_APPLICABLE、EXCLUDED_WITH_APPROVAL；后两者有来源/批准/版本，不写入test_status |
| reason | BLOCKED_ENV、BLOCKED_EXTERNAL、BLOCKED_UPSTREAM、GAP_IMPLEMENTATION、USER_VISUAL_REVIEW、SHA_DRIFT、SECURITY、INFRA_FAIL等具体代码；与现有runner映射表冻结 |
| 退出码 | 0=全部required PASS；1=测试/安全/证据失败；2=BLOCKED/FLAKY/PLANNED/required skip；64=参数/schema错误。执行前用当前result.py验证映射，不能仅写文档 |

PASS_WITH_SKIPS在runner可表达，但required集合中任何skip都不能总门PASS。FAIL修复前不靠重跑洗绿；FLAKY必须保留同指纹所有attempt。测试单格全FAIL时scope仍输出PARTIAL，另列fail_count；总门exit1，不能误解PARTIAL为可发布。

| Failure | Recovery | Retry预算 | Next state |
|---|---|---|---|
| 缺设备/上游/决策 | 输出具体缺少条件，只继续独立卡 | 不自动轮询重试 | scheduler WAITING / test BLOCKED |
| 已分类infra错误 | 核对进程/lease/config，修资源 | 原尝试+最多2次；同指纹保留全部 | PASS或FLAKY/BLOCKED，按现有分类器 |
| 行为/安全失败 | 保留失败，回原owner修复、review、新commit | 同根因最多2修复轮，第三次根因升级 | FAIL，修复后新候选PLANNED |
| 超时/crash/失联 | read-only reconcile业务ID、WIP、进程与租约 | 不确定发送/导入/资金操作禁止盲重放 | INTERRUPTED/BLOCKED；核实后READY |
| 上游/候选漂移 | 追加失效日志，重新集成/构建/绑定 | 一次干净集成仍冲突则停对应域 | BLOCKED/reason=SHA_DRIFT |
| 证据缺字段/hash错 | 回到真实运行补证据，不能手写PASS | 修源后一次复验 | FAIL或BLOCKED |

每小卡以 `plan hash → state → actual HEAD/process/lease → fixture generation → checkpoint` 恢复。回滚仅自己的合成fixture与已确认可逆本地变更；不清上游进程/数据库，不重放不确定外部写。超过预算留下明确next_action而非无限循环。

## 6. 独立验收器合同与反造假用例

F01-S06在**现有auto_test报告/结果库**中扩展验证，入口和精确argv登记 `commands.json`；本文不假装新命令已存在。F17-S03只读执行注册入口，不能自动修ledger、改required集合或生成“缺失PASS”。F01完成前仅可做准备，不能开展正式终验。

必备输入：冻结的plan+合同+dispatch+acceptance集合hash、三仓candidate与祖先清单、required-cases、case-map、commands、attempts、上游handoff、fixture/config/build/device指纹、artifacts manifest、defects、用户UX评审。路径限制在该run脱敏证据根；拒绝绝对越界/../逃逸及重复ID，不跟随任意外部symlink读取私密文件。

每个PASS必须有exit0、executed>0、required skipped=0、精确case事件、真实业务oracle和原始产物hash；执行次数或截图不是oracle。跨端还需同一run/journey/message_id、发送UI、持久化事实、接收UI/重进后读取。群撤权负例必须观察被撤销者实际请求/解密失败，不仅检查列表消失。事务/接单并发必须两个真正同时参与者和唯一持久化胜者，不用顺序调用冒充并发。

F01-S06须构造下列独立坏样本，每个应非零且定位缺失项：
1. 缺一个父Acceptance ID、额外未知ID、重复case/platform/profile/role键。
2. 删一个旧seed映射、隐藏新增页面、把歧义行当已核验、残留placeholder。
3. 修改候选SHA/config/build/fixture、过期上游plan、缺祖先提交。
4. exit0但零测试、required skip、只有mock/截图、无接收端消息事实。
5. evidence被篡改/路径逃逸、只有worker自签verdict、复用不同平台日志。
6. 设备缺失却写N/A、上游iOS BLOCKED却宣称全部安全资格通过。
7. 故意注入行为失败后只保存最后绿attempt，或未运行目标伪造PASS。
8. 正确完整的最小合成样本可以通过验证器结构门；明确其是验证器fixture，绝不能计入产品PASS。

最终报告分列：LOCAL_REGRESSION、ANDROID_DEVICE、MACOS_DEVICE、CROSS_DEVICE、ADMIN_REAL_BACKEND、UX_USER_REVIEW、E2EE_UPSTREAM_QUALIFICATION、EXTERNAL_INTEGRATIONS。无用户UX评审时总交付仍PARTIAL；可另报技术测试PASS，不能替用户签字。任何未满足required格子、P0/P1未关、来源缺失或安全门失败禁止总通过。上游高阶profile、iOS、外部支付/OA、生产部署不在本地双平台结论中偷换为完成。

## 7. 校准、进度与交付

F01-S06选择至少6类样本：单聊跨端、群成员撤权、朋友圈可见性、频道CRUD、组织架构、我/设置；另测一次构建和Admin真实后端。设备尚缺则只校准离线准备，设备估时UNKNOWN。不为估时启动未获资源的测试。

记录每样本的唯一行为数、用例补齐工时、L0/L1时间、构建/安装、Android/macOS占用、双端同步、失败修复和复验。按复杂度分层计算P50/P80；样本不足5项的层输出区间不足、不要伪造统计精度。

预计工期下界 = max(依赖关键路径, 各owner剩余工时/可用并发, Android专用时长+双端时长, macOS专用时长+双端时长, 独占后端时长) + 最终冻结回归；另列上游等待、租约等待、未估缺陷风险。P80由实测层分布与队列预算估算，不能简单把全部时间除以worker数。每完成一域或上游候选变动重算。

进度同时报四个分母：输入已映射/总输入；用例已具备/冻结required格子；预验通过/required格子；FINAL证据通过/required格子。1846旧行、61宏观ID和dispatch卡数分别列，禁止互相替代完成率。

交付必须有：精确版本文件集合、冻结required集合、按功能拆分提交、原始证据与hash、全attempt及恢复历史、逐平台/profile结果、未闭合清单与next_action、独立verdict。规划质量检查通过不表示产品已经测试通过，也不表示已达到E2EE投产资格。

## 8. 派发前 READY 清单

Coordinator只在以下项齐全时将某阶段标READY：实名owner与替补、精确路径lease、该阶段依赖能力、candidate/plan指纹、≤25行为的case-map、逐条可失败oracle、已审target、可直接执行的cwd/argv/env变量名/timeout、所需合成fixture与安全清理方式、证据目录、reviewer。缺失则WAITING并注明字段，不将本索引当可盲跑脚本。

设计READY不要求运行环境；运行READY不要求无关平台；最终READY要求完整全域覆盖与冻结交接。正式运行前将commands.json里的占位符全部消除，秘密仅由仓外配置注入。修改计划需同时更新所有hash绑定；上游计划漂移也触发交接重新核验。本次未收到上游运行账本，不自动联系其执行者或改变其调度。
