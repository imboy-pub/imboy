# 广州交付 V3.1：已知范围优化与跨执行器接力 / Known-scope revision

日期：2026-10-23。**PLAN_REVISED / EXECUTION_USER_PAUSED / NOT_ACCEPTED**。
本版是原 [V3](plan-v3.md) 的增量优化，不表示链接会话已经合并。原V3、baseline、252条来源索引和历史证据保留；本版优先修正调度、粒度、工具前置与接续方式，不减少原合同义务。本轮后续仅做计划设计，业务实施由用户明确恢复后进行。

## 1. 来源与当前事实 / Sources and current state

- 三仓与唯一运行根沿原V3：`/Users/leeyi/.codex/imboy-delivery-v3/`。已成功创建并复核 `worktrees/imboy`、`worktrees/imboyapp`、`worktrees/imboyadmin` 三个detached工作树，每仓只保留这一份执行树；不是三个新分支，不重复初始化。
- 当前候选：Backend `9f96b45ff5f864320585d1c74dac514add241893`；App `f1086336e1c316c6189101e26b2f3f071709c3ee`；Admin `2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44`。main未发生产品代码修改；外来未提交文档已按文件hash保护，未复制进候选。
- M3/arm64、36GiB内存；本轮实测可用磁盘由106GiB到105GiB。它是观察值，不是未来余量承诺。
- 中断前命令已完成，state/events与原始日志一致，没有本run RUNNING命令：状态存储10项测试、迁移文件223组配对、Moya退役8项独占PG测试均 raw exit 0。只对其实际断言成立，不签V02或V03整卡。
- 原V3六条失联材料已经从 Git `236b8e3544d6f15b903e30402c73f9f118e40070^` 恢复，SHA256与原materials-index一致；见运行根 `artifacts/source-recovery.json`。**只解除这六个文件缺失，不等于递归来源与全部子叶已闭合，也不复活旧执行授权。**
- 已提供 [时区审查原文](sources/timezone-review-provided.md)，SHA256 `c8fe9f58fc1bc216552326bf6ed1fd56cfbfdd2cbd0289cb28469b7d3e2139ba`。原审查Backend/App基线较当前本地main旧；其中代码结论需要按当前候选复核，建议不自动升级成产品决策。此文件是实际收到的正文副本，不冒充另一会话正文。
- 链接会话 `ADD-CONVERSATION-001`：https://chatgpt.com/g/g-p-6957e61ca83c8191b003207fce198b02-imboy/c/6ada8cbe-485c-83ea-aac1-df6d187eca0a 。正文、附件尚未提供，**BLOCKED_SOURCE / NOT_MERGED**。停止寻找尚不存在的additional-conversation.md及尝试读取此链接；不创建空文或URL占位文件冒充来源。

## 2. 实质修订记录 / What changed and why

| 原问题 | 当前源码/现场证据 | 调整 | 影响任务和验收 |
|---|---|---|---|
| 六个历史源被整体列缺失 | Git恢复文件hash逐一匹配原索引 | 允许核这些已恢复来源的原叶；继续追其递归引用，不维持虚假缺文件阻断 | V01及相关EO叶；不自动复用历史PASS |
| 大卡把“实现”和运行验收绑在一起 | V09同时含本机录音、PG、供应商、共享、删除；现有ASR调用当前被禁止 | V09八组独立子卡；其他大卡只按不同资源/独立出口拆分；供应商仅阻断ASR/GLM及依赖它们的完整旅程 | V04/V07/V08/V09/V10；父卡保留完整闭包 |
| 依赖可能被解释成整卡串行 | App CRUD不需要ASR结果；协议静态检查不需要PG；UI准备不等于设备通过 | 用命名输出作为依赖，准备与PASS分开；禁止以整卡未完成阻断独立叶 | V01–16；原Required全部保留 |
| 工具链与共享目标风险发现过晚 | App执行树无.dart_tool，flutter_chat_ui子模块未初始化；Dart 3.13.5 arm64；配套FRB库另有保留，但尚未与本次解析依赖核对 | 前置V02.ENV；只初始化锁定依赖一次，不复制SDK、不借主树WIP；不先跑大构建再发现版本不符 | V03.SQLite、V07.App、V09本机/真机 |
| 证据复用过粗或重跑已完成命令 | 三个新receipt已具candidate、argv、exit、log hash；原App日志有313 skip，原Dialyzer是基线绿205告警 | 先接纳有效的限定断言；计划措辞变更不重跑；源码/依赖/环境/断言变化仅失效相关闭包 | 所有卡，尤其V12；不把局部绿升级终验 |
| 频道分页的现有边界不接受正常毫秒cursor | `src/api/channel_handler.erl:messages/2`钳制至`16#7fffffff`；repo再走自动时间转换 | 加V07.TIME-PAGE，先测真实毫秒经handler→repo及同时间戳25条分页；单改上限不能签无丢重 | V07/V10；若组合cursor改API须先冻结兼容合同 |
| 客服时间fixture与真实wire不一致 | Backend `src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl`归一化ts；Seat/Widget的`to*Message`用optStr；只读调用两生产函数，秒整数均变null | 加V08.TIME-WIRE：先将秒整数纳入跨边界fixture，最小适配并验Seat/Widget调用方 | V08.history/Widget；不是单纯样式修复 |
| 时间基础方向可复用但跨层有断点 | App formatter直接格式化UTC；feed以inDays分今天/昨天；HTTP剥离顶层sv_ts；Admin审计起止构造不一致 | 按下节TZ映射分批核验；复用现有formatter/clock/DTO，不换整个时间库或设计系统 | V05/V07/V08/V12 |
| 换模型可能重建环境或双写 | 运行状态在普通JSON/JSONL，但当前owner关联Codex进程；state工具尚无安全释放/异工具接管完整演练 | 补V02.HANDOFF，明确释放、接管epoch、在途命令对账与相同提示词；切模型不重置预算或证据 | 所有卡；跨执行器可接力≠已经实测接力 |
| AC12 的20页、网络超时、后台恢复与三侧对账只有父项泛化映射 | 原合同 phase1.md:108；当前全量中3个分页组件成功，但完整运行另有失败，且组件使用fake HTTP/本地SQLite | 在 requirements-v3.1.json 细化4个原有场景，复用限定观察，真实后端/设备/网络闭环均保留未验；不新增合同 | V01/V05/V12；原AC12及其他Required不降级，T20独占期间不启用PG/网络故障任务 |

## 3. 逐卡分类与可复用出口 / Classification

分类不等于完成率。`VERIFIED_SCOPE`仅指已核的有限断言；`IMPLEMENTATION_LOCATED`只指有当前源码，不能推导整卡已实现或通过。

| 原卡 | 当前分类 | 当前可用结果 / 尚需出口 |
|---|---|---|
| V01 | PARTIAL_SOURCE_RECOVERED | 三仓稳定、17直接源hash相符、六源恢复；递归exact-set/修订冲突/子叶映射仍需核验 |
| V02 | VERIFIED_SCOPE + ENV_PENDING | 状态原子替换、子进程退出恢复、竞争写者等10项通过；资源抢占与跨工具交接尚未验 |
| V03 | VERIFIED_SCOPE + ACCEPTANCE_PENDING | 配对223组、独占PG Moya8项通过；225真实ledger/非空历史/SQLite/恢复仍待验 |
| V04 | IMPLEMENTATION_LOCATED / EVIDENCE_INSUFFICIENT | 已有Cache与附件V5脚本/实现；按实际profile与引用闭包补证，不重启暂停专项 |
| V05 | IMPLEMENTATION_LOCATED / AUDIT_PENDING | ACK/游标/群事件/profile路径存在；新时间审查的接收时间与顺序问题先分开核验 |
| V06 | IMPLEMENTATION_LOCATED / EVIDENCE_INSUFFICIENT | 目录sync、OA登录/SSO在当前候选；不能凭导出函数签身份正负例 |
| V07 | SOURCE_DEFECT_CONFIRMED / RUNTIME_REPRO_PENDING | cursor上限、相对日历分组和sv_ts断链源码已核；待真实边界用例，原生/推送另验 |
| V08 | FUNCTION_REPRO_CONFIRMED | Seat/Widget生产投影均丢秒整数时间；尚未修改产品源码；真实PG→UI仍须补证 |
| V09 | IMPLEMENTATION_LOCATED / INDEPENDENT_SUBCARDS | 离线历史177例仅作支持；ASR/GLM付费调用禁止；不阻断CRUD/录音/安全存储准备 |
| V10 | IMPLEMENTATION_LOCATED / EXACT_SET_PENDING | MC与CI原合同在；V1全部子场景与CI增量仍需逐叶核证 |
| V11 | PREPARE_ONLY | 可整理正式产物/恢复oracle，业务与目标依赖未齐，不签集成恢复 |
| V12 | PREPARE_ONLY / STRICT_GATES_OPEN | 原两轮EUnit、排除套件、raw Dialyzer、三仓/Widget/CI不减；205告警和313skip不豁免 |
| V13 | BLOCKED_CAPACITY_AND_WINDOW | 可准备测量合同；容量确认/独占足时窗口未绑定，不运行缩短替代测试 |
| V14 | BLOCKED_FINAL_REVIEW_INPUTS | 用户后续已授权子agent；稳定候选与完整门禁证据仍未齐。实现者及同轮定向审查不替代独立终审 |
| V15 | PREPARE_ONLY / HUMAN_EXTERNAL_GATES | 可整理交付草包；UAT/发布/生产分账，不代理用户签收 |
| V16 | PARTIAL_LEDGER | 持续保留所有余额；不能由广州状态推导面聊或五专项完成 |

## 4. 子卡与逐叶依赖 / Subcards and output dependencies

以下仅拆出具备不同资源或独立oracle的单元，原父卡与来源ID不变。每个子卡在唯一state.cards父卡.children下存状态/证据/阻断；不另建重复进度看板。一个子卡可有component检查与集成检查，component通过不替集成PASS。

| 子卡 | 可准备的前置 | 可签发对应叶PASS的前置与出口 |
|---|---|---|
| V04.AUTH | V01相关权限原文、当前调用链 | 独占PG与相关schema；COMMIT/ROLLBACK/撤权/QR并发和缓存旧值正负对拍 |
| V04.UPLOAD | 现有上传/下载API合同 | 独占对象+PG、purpose与授权；真实PUT/confirm/Range/hash、失效票据拒绝 |
| V04.GC | 引用/hold/retention合同与静态入口清单 | 对应schema/独占存储/实际引用闭包；双队列、lease/故障/不可误删；不等整个AUTH专项终结 |
| V07.DATA | 稳定API/DTO；V02.ENV用于App执行 | 序列/TSID/null/错误态与实际后端对应；不需要推送凭据或ASR |
| V07.UI | DATA可用且对应UI规格固定 | 依平台分别真机/原生；键盘/大字号/上下安全区/切空间/迟到响应 |
| V07.PUSH | V05消息和DND合同 | 自有设备/明确通道授权、消息/权限运行叶；其他UI无需等待它 |
| V08.CONTRACT | 现有Seat/Widget协议、隔离测试工具 | 原始wire→生产DTO→展示的正负断言，单测范围独立签发；完整旅程另验 |
| V08.HISTORY | 队列/发送API固定可准备 | DB与权限、同payload发送/确认、长历史/可见ACK/SSE/认领转接各相关叶；不等全部Widget开通 |
| V08.ONBOARD | 原snippet/Widget/hosted合同 | 真实独占服务/对象、安装域/origin；物理扫码必须真机，扫码缺失只阻断该叶 |
| V08.METRICS | 原统计、容量与业务日合同 | 实际数据/SQL对拍；固定offset与地区自然日不混为一项，冲突待确认 |
| V09.01 CRUD | 冻结个人/企业/幂等协议 | 对应PG/schema/权限；重放唯一记录、越权零写；不等ASR |
| V09.02 CAPTURE | V02.ENV、capture协议 | 真机录音/前后台/结束进程/恢复同capture；AAC实际解码、时长和段序 |
| V09.03 ATTACHMENT | 上传API/purpose可用 | V04.UPLOAD相关叶；补传同对象/引用/hold，拒绝旧票据 |
| V09.04 ASR | 供应商契约/离线分支可核 | 真实供应商授权/Key/累计预算、assembly与附件；当前付费禁止，真实叶BLOCKED_AUTHORIZATION |
| V09.05 SUMMARY | 摘要/引用协议可用，可用明确合成文本准备安全校验 | 真实GLM授权及预算、确定输入片段版本；完整ASR→摘要旅程再依赖04，纯引用安全不等04 |
| V09.06 SHARE | CRUD/授权接口固定 | 对应PG/自有双账号；分享、编辑冲突、受控回听、撤销与旧结果拒绝；不以ASR质量作准备依赖 |
| V09.07 GOVERNANCE | 删除/移交/导出/注销原合同 | 对应CRUD/附件/权限/留存叶；无误删、不串scope、可恢复和终态可读 |
| V09.08 BYOK | 账户/供应商键隔离合同 | 本设备安全存储/换账号/不迁移Key、保存零调用；不需要真实付费请求 |
| V10.MC | MC01–26及全部子叶 | 每个绑定/订阅/拒投/检索/讨论旅程的相关schema/权限/UI输出；不等待无关CI外部secret |
| V10.CI | workflow与配对候选清单 | 按原CI01–08真实同候选证据；缺远端授权只阻断对应CI叶，不能删门 |

依赖引用必须是`子卡.output + source/候选/环境指纹`，不写模糊“V04全部完成”。V11按所验业务取闭包；V12在稳定集成候选后；V13依V12及容量/完整窗口；V14终审依exact-set与所签业务/质量/24h；V15依V14及用户/外部确认。V16持续记录不反向阻塞开发。准备边与验收边分别做拓扑检查；任何全局能力变更都把实际受影响叶纳回闭包。

### V01显式继承集合补齐（2026-10-23）

原252条保持原字节。`requirements-v3.1.json.inherited_sets`补入附件合同明确的2272个叶、面聊82项必需检查、缓存15卡/56原任务/30 AC/第19节9项，以及EWS原23项与非OA16项。52个EWS协调ID单列为非验收叶，不能替代原要求；缓存旧别名显式保留：W4-010→W4-10、AC-022→AC-22。子项的source_pointer/line和源hash可反查原文，不按数量计算进度；所有新映射默认NOT_VERIFIED，既有窄证据不自动关闭叶。

这仍是部分展开：叙述来源中的角色/平台/故障子场景、非OA嵌套CS/ORG/FILE/UX等ID、附件oracle引用仍需逐项闭合。旧运行根中缺失的面聊冻结runner记录为待重绑，不删除检查或伪造旧hash。旧执行许可不继承；用户后续明确授权最多10个agent，当前宿主最多4个活跃执行器（含主控），实际并发取授权与宿主容量较小值。主控独占状态写入、数据库写入与重型槽；禁止提交/付费/外部通知和独立终审边界维持。`additional-conversation`继续BLOCKED_SOURCE。

## 5. 时区审查纳入范围 / Supplied timezone review

来源标记`TZ-REVIEW-001`，正文已完整保存；机器可读增量见[requirements-v3.1.json](requirements-v3.1.json)。不宣称已合并未提供的链接会话。审查中原“已确认”字样不替本候选复核；性能收益、业务日、DND跟随规则不是既定事实。

| 新ID/原文 | 并入卡 | 最小工作与验收 |
|---|---|---|
| TZ01/F1 | V07.UI、V08.CONTRACT | 复核旧消息/公告/朋友圈UTC直显、客服队列UTC与详情本地不一致；统一复用formatter，等价Z/+08时刻显示一致 |
| TZ02/F2 | V07.PUSH | DND服务器local_time路径已定位；先明确账号固定时区/设备跟随的产品合同，多设备不能最后写覆盖；未裁决只阻断该策略叶 |
| TZ03/F3 | V07.UI | 自然日按年月日/下一日零点，持续时长仍用duration；23:50→次日00:10显示昨天，纽约23/25小时日归组正确 |
| TZ04/F4 | V07.DATA、V08.METRICS | AuditLogPage同一时区半开区间，包含23:59:59.999、排除次日零点；先核API/SQL使用<还是<=，不能单改前端常量 |
| TZ05/F5 | V07.DATA、V10.MC | handler接收正常毫秒不截断，repo明确单位；25条且同时间戳跨页无重漏；若需组合cursor先冻结新旧客户端兼容 |
| TZ06/F6 | V08.CONTRACT/HISTORY/ONBOARD | 真实客服秒整数与旧ISO兼容；Seat/Widget均显示日期/时间；非法/缺失不伪造1970或当前时间 |
| TZ07/F7 | V07.DATA | HTTP顶层sv_ts实际到统一校时入口；错误/迟到响应、RTT样本筛选及不污染业务payload；校时与时区分开 |
| TZ08/F8接受时间 | V05 | 先核实时/暂存/正式表/归档server_ts来源，客户端快慢10min时事实一致；历史缺失不可凭空回填，编辑不能重置首次接受时刻 |
| TZ09/F8顺序 | V05 | conv_seq贯穿同步/本地分页/UI；乱序投递/补历史/多设备最终顺序一致；与显示修复独立，涉及SQLite先走V03 |
| TZ10/统计业务日 | V08.METRICS | 固定offset合同保持，地区时区/历史offset/周边界另核；未批准地区自然日语义不偷偷改报表口径 |
| TZ11/codec | V03、V05 | 秒/毫秒/类型边界、1999/2000前后signed往返、非法值明确失败；先定向证明再改，不能无依据换全局codec |
| TZ12/时间文档合同 | V01、V15 | occurrence/date/local-rule/duration分别定义，修规范与实际REST/WS/客服单位矛盾；不一刀切破坏现有接口 |
| TZ13/最小跨时区矩阵 | V07、V08、V11/12 | UTC/上海/纽约/加德满都、DST两端、运行中切时区、纯日期不平移、错钟及各端同instant；UTC环境不能直接return冒充通过 |

DND策略、业务统计时区、不可变服务端接受时刻的历史兼容/schema需要明确设计与原合同核对；不将审查建议直接部署。能独立复现和修复的显示/DTO/分页/校时先行，不能因这些决策未闭阻断全部时间工作。

## 6. 下一批执行入口 / Admitted batch specification

B/A/D分别为运行根内的三执行树。运行状态以运行根control/state.json为准；用户已恢复本轮本机实施，USER_PAUSED时不得执行。owned_paths按本行封闭；未列路径先做影响分析再扩，不改主树或保留区。

| 子项 | 精确owned_paths / cwd与argv | 资源、oracle与恢复 |
|---|---|---|
| V01.RECOVERED_REFS | 只读`artifacts/recovered-sources/*`、V3原requirement及原文；只写state中来源/映射 | 六文件已恢复不再重复提取；逐级核引用hash/原Required，未闭子叶保留；无DB/设备 |
| V02.ENV | 只写A的依赖生成配置与锁定子模块checkout，B的本run构建输出；不改pubspec/lock/vendor源 | 先核Flutter/Dart、FRB库与锁版本、主树SDK只读复用；A执行`git submodule update --init --recursive -- plugin/flutter_chat_ui`后核pin `7707c7bfbebc1fb273b851daebf778acfdb92a25`；pub解析仅锁定版本，未证明离线可用不反复重试；失败保留具体缺件 |
| V09.CONTRACT | B：只读`src/api/talk_record_routes.erl`、相关handler/OpenAPI、`scripts/test/test_talk_record_machine_contract.py`；只写run log/receipt。argv=`python3 scripts/test/test_talk_record_machine_contract.py` | 无服务/DB/供应商；当前60条注册路径与文档路径精确对齐，各路由方法集及local refs按脚本实际断言核；非零保留首因，不改断言凑数。静态绿只记component |
| V08.TIME-WIRE-REPRO | D：`src/modules/customer_service/seat/workbench/contract.test.ts`、`src/widget/customer_service/app/contract.test.ts`；修复前只增真实wire反例。argv=`bun test --isolate src/modules/customer_service/seat/workbench/contract.test.ts src/widget/customer_service/app/contract.test.ts src/modules/customer_service/seat/workbench/messageTime.test.ts` | 明确秒整数/ISO/null/非法输入；先红再最小修复生产适配及直接展示调用方。任何新增共享helper先列精确路径再写；本地模块不等同PG→浏览器通过 |
| V07.TIME-DART-PREP | A：只读/计划`lib/component/helper/datetime.dart`、`lib/component/http/http_transformer.dart`、`http_response.dart`及既有对应test；具体修改路径由复现冻结 | V02.ENV就绪后先现有定向测试与跨时区反例；日志实际显示本地时区与DST不可skip；不运行App全量或原生构建替代定向定位 |
| V03已有小门 | 无新增修改 | `v03-migration-files`、`v03-moya-private-pg`已有exit0与raw log，在候选/脚本/PG版本不变时复用；不是再跑一遍的READY任务。225/SQLite/ledger/恢复另待资源绑定 |

每条实际执行前补登记runner/输入hash、完整安全环境、设备或物理data目录、expected exit与实际执行数、timeout和scope；资源或参数未落实就是BIND_REQUIRED，不以模板为可运行证据。当前run_check是单命令记录工具，不是后台调度平台；启动失败/控制器中断时仍须查真实进程/目标，不保证自动重跑。

## 7. Codex与ZCODE/GLM接力 / Executor portability

这是用户明确的执行兼容需求`EXEC-HANDOFF-001`，不是业务App切换供应商。目标是在ChatGPT额度不足或主动换工具时，ZCODE使用其实际支持的GLM-5.3或GLM-5.3-Flash接续**相同三个worktree、相同候选、运行根、预算和证据**。不假设这些模型在ZCODE当前版本必可选；接手时核对本机工具与模型实际ID，记入执行器元数据。模型选择不产生业务付费API调用权限。

### 交接流程

1. 旧控制器停止接新任务。逐条列在途command/子进程PID+启动身份、日志、PG事务/供应商task状态；可逆短命令允许结束并收回exit。长命令若确实继续运行，转移观察权，不重复启动。未知外部结果保持RECONCILE_REQUIRED。
2. 持久化当前候选/WIP hash、输入/依赖、当前子卡与下一条确切argv、累计失败/预算、证据和NEXT_UNLOCK；append `HANDOFF_PREPARED`。保留state原版本链，不能重建一个全todo初态。
3. 明确释放旧controller写入权，记录handoff_id、from_executor、to_executor、释放epoch与在途命令清单。旧控制器仍活跃且未释放时，新方只读、禁止接管。不能仅因锁文件旧或额度耗尽而kill Codex/ZCODE。
4. 新执行器先读文件并核本机真实PID、工作树、状态与hash，随后持锁原子增加epoch并记`HANDOFF_ACCEPTED`。旧epoch的结果/写入必须拒绝；已完成有效任务复用。更换模型本身不失效业务证据；实际编译器/依赖/代码/断言变化才按闭包失效。
5. USER_PAUSED不会被检测到新模型自动解除；用户在新会话明确说“接管并执行”才恢复本run。只读恢复/计划审查不能解除暂停，更不能恢复其他专项。

**当前能力边界：** `control/tools/state_store.py`已实现release组件及对应定向守卫测试：在途RUNNING/RECONCILE_REQUIRED阻止释放，claim拒绝仍存活且未释放的旧owner；这些组件不等于完整防双写或实际跨工具接管已验收。`claim(resume_paused=True)`也只接管身份，不自动把USER_PAUSED改为ACTIVE；只有用户明确授权恢复本run后，接手者才以新claim返回的fence，通过独立`Store.transact(..., fence=新fence)`记录授权依据并恢复本run状态，保留其他专项暂停。不能手改JSON绕过owner检查。Codex→ZCODE→Codex真实往返仍待V02.HANDOFF验收。

### 交接验收与模型分工

V02.HANDOFF需分别演练：正常释放→接管；旧进程存活但明确释放；额度中断且未释放只读拒绝双写；命令仍活跃只观察；子进程已退出而receipt缺失先对账；旧epoch回写拒绝；暂停不自动解除；Codex→ZCODE→Codex往返后candidate/WIP/累计预算/hash和PASS叶保持。至少一次真实工具往返，单元测试或文字协议不代替该验收。

GLM-5.3-Flash可用于文件清点、已有用例运行、低风险局部修复和证据整理；权限/身份/迁移/并发/时间合同等复杂判断优先使用用户在ZCODE可用的GLM-5.3或更适配模型。此为分工建议，不能当质量已证明。无论何模型，测试、原Required、独立reviewer、UAT、真实平台与24h门完全一致。实施模型不能自称独立终审；工具不具能力时明确BLOCKED，不伪造替代证据。

普通JSON/JSONL、原始日志、Git、Python/Bun/Flutter/Erlang脚本是接续接口。不要依赖ChatGPT聊天摘要、functions cell/session ID、Codex MCP专属工具或模型隐藏记忆。把可持续命令的OS身份和日志写盘；PTY连接不可迁移不代表其子进程已退出。接力只需要同一份[接续提示词](resume-prompt.md)，不需要将整段长聊天重新粘贴。

## 8. 资源、证据与保留门禁 / Resource and gates

按用户后续授权可并行，当前宿主主控加最多3个子agent；每项指定精确owned_paths，禁止相互覆盖，子agent不写control/state或启动共享重操作。M3/36GiB上重型构建、全量、PG故障演练、性能/24h互斥；单套SDK、每仓一个工作树、一个Android构建槽。先记录实测空闲/实际高水位与余量；不足暂停相关重操作，不全局清缓存、不删除唯一证据。已运行的Moya测试用本run scratch下独占cluster和Unix socket并自动停止，不连接现有5432/4323。

计划升级和换模型都不重置累计失败/费用；历史未知保留UNKNOWN。当前禁止付费，旧5元目标不构成新调用授权。receipt只有命令exit0时仍需业务oracle、执行数、skip、候选/环境hash匹配；`COMMAND_EXIT_ZERO_ORACLE_REVIEW_REQUIRED`不能直接叫业务PASS。

V12原后端两轮全EUnit与排除套件、raw Dialyzer要求，App/Admin/Widget/适用CI、真实设备/推送/供应商、连续24h、独立终审、UAT和生产边界均不变。计划措辞/模型切换不使全部历史证据自动失效；受影响源码、环境、依赖、schema、profile或runner/断言改变时，精确标INVALID并重验。广州、面聊、五专项和新增时区叶分别签发，不混算卡数。

本版不自评9.9、不承诺固定人天或短会话完成全部验收。原V3与此增量、requirements-v3.1.json和唯一state是接续依据；未提供链接会话的正文始终保留来源阻断。

N4来源增量：已从原Git blob恢复两份底层合同，并将13个CS/ORG/FILE/UX/INTG子ID及原文行级oracle纳入`NONOA_N4_NESTED_REQUIRED`。全部待验，未继承历史PASS；阶段适用性与其余引用来源仍分别核对，原252记录及原Required保留。


### 2026-10-24：main 同步与候选重验补充

用户授权先将 Backend/App 的本地 main 合入当前任务分支，再提交本会话自有改动并合回 main；不含 push、部署或生产迁移。具体最终commit以运行根 control/state.json 当前 candidate 与 scratch/main-sync-20261024-r1/commits.json 为准，禁止继续使用旧固定HEAD的启动manifest。原baseline及原始receipt保留。

- Backend：main 的227为 user_collect_e2ee_v1；本任务未提交的 attachment_intent_physical_identity 改为228，上下SQL字节保持不变。旧227身份下的迁移证据不能签发当前228 PASS；V03及依赖该schema的附件/面聊/频道HTTP叶需新候选重绑定与必要重验。禁止在已存在旧测试库中直接重放变号迁移；先对账账本与独占性，重建合成资源需单独准入。
- App：生产SQLite保留main的v34，typed manifest/生成物按v34核验。原31→32历史矩阵继续验证历史边，不能机械把32替换34，也不能据此宣称最终34升级链通过。继承main的原生依赖/生成物变化应重绑V02/V09原生构建profile；保留区不另作手工修改。
- 两仓commit/tree或WIP变化后，原始历史成功与失败不抹除；按源码、schema、依赖、profile和实际断言影响建立失效范围。旧V08和Android ready草案必须重生成绑定并独立审查，不能仅更换hash绕过检查。最终原合同全门、真实平台/供应商/24h/独立终审/UAT均保持，不因合并或提交获得PASS。
- V08新增一次ready-only诊断授权保留累计计数；如尚未实际启动则仍未消费。额度剩约2%按用户要求安全暂停，ZCODE使用同一运行根/worktree/budget接续，不能双控制器写入。
