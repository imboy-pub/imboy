# 一期交付、微社群、企业组织统一收口计划 V1 / Unified Delivery Closeout

日期：2026-10-11。状态：PLAN_READY / NOT_EXECUTED。当前授权仅编制计划、冻结旧入口及文档验证；本文件不是业务实施或外向操作授权。

## 1. 目标与范围 / Goal and scope

以原验收合同为底线，复用已有实现与可信子证据，先补证、再修已复现缺陷，最后冻结三仓候选统一验收。目标是可按卡交付、逐格测试、快速发现阻断，不承诺省略真端门或压缩稳定性时间。

范围包含 imboy 后端、imboyapp Android 真机/macOS 原生、imboyadmin 真实浏览器及对应文档。缓存计划的 backend-only 限定仅适用于缓存，不适用于这里的三端原合同。不新增 iOS 强制验收；继承合同如包含 iOS，登记来源及范围决策，不能静默删除。

独立执行边界：

- CX 客服体验与投产资格正在其他会话执行。本计划保留一期 AC17/18 等关联义务，以依赖交接方式接收证据；不得重复修改其实现或代签 UAT。
- 缓存 backend-closeout-plan-v1 已由用户安排 ZCODE。其核心文件、构建环境、数据库与全量门须取得资源租约后才可使用；不重开缓存实施。
- OA 按需免登 JSBridge getAuthCode：用户反馈应已完成，U00核实源码/提交/证据，默认不重开实施；这不自动关闭企业 OA 的一次码、撤权、平台合同及客户联调。
- E2EE 专项、L4/relay专项、一期 AC31–34 性能/24h/指标留后续队列；普通 IM 回归及现有密文保留/授权负例仍保留。
- 客户联调、人工视觉签收、生产操作单列外部门禁。本地替身不等价真实客户验收。禁止自行通知第三方、push、部署或生产迁移。

## 2. 审计基线与旧入口冻结 / Audit and freeze

用户提供的最新独立审计是本轮差距输入，尚未在本轮重新运行测试验证。phase1 与微社群均维持 PARTIAL；企业组织历史 verdict 为 PARTIAL_LOCAL_CANDIDATE。历史 PASS 不自动变为新候选 PASS。

phase1 原父项为 AC00–34（35），另有 P1-LOC-02/P1-MOM-01 两子项；本地目标 AC00–30（31），子项单列不增加父项完成数。微社群精确父项 MC-AC-01–26（26）。EO-00–09只是任务索引，必须展开原 EWS、required cell、平台与角色集合，不用“48卡”替代真实验收集合。

重点纠偏：phase1 AC05/16/25等修订需授权溯源；AC10/12/14/23/29/30有父项结论越界，AC28同时存在全量非零、dialyze未差分和225 skips。微社群 MC01完整性不成立，MC21双轮日志及dirty候选绑定不足，MC22首先补真实VIEW发送编排，再验证产品路由，不预设必须补原生桥接；报告把UPDATE写成REPLACE须纠正。

三个旧计划统一标记 FROZEN / SUPERSEDED_FOR_EXECUTION；原正文与旧哈希保留在本目录 source-contracts，禁止继续向旧计划/旧run补登PASS。企业 verdict.json是历史证据，保持字节不变，冻结其执行入口及证据目录说明，不修改历史 verdict。

旧范围修订没有可核实授权时：保留原项为PARTIAL/UNVERIFIED，输出决策卡；正式修订另记 revised_scope_status，original_contract_status仍反映原合同差异。用户本轮排除的后期专项记 DEFERRED_BY_USER，不伪装原计划完成。

## 3. 启动门与不可变候选 / Entry gates

U00记录三仓HEAD/tree、完整tracked patch与未跟踪白名单内容hash、依赖锁、runner、配置脱敏hash、fixture schema、构建工具/产物hash。当前主树有缓存WIP与其他未跟踪文档，不能吸收或回滚。采用独立checkout/worktree，创建前检查现有worktree；分支和本地提交依执行授权及仓库规则。

历史protected WIP快照保持不变；主树并行演进不等于历史完整性通过。U00按路径、提交与内容归属分类重订本run基线，证明候选未混入外来WIP。新基线不能追溯消除旧run漂移；不得补造 attested 日志。

建立 control/requirements.tsv：source_plan/source_id/subcase/platform/role/oracle/required_local/dependency/owner/status/evidence/candidate。每个原父项必须展开全部子场景；源ID并集无缺失/重复覆盖，每项明确本地、依赖、后期或外部。共享测试可映射多个源ID，必须分别证明其子断言。

U00输出 control/leases.json 与无省略号 commands.sh。每worker独立build/ebin、数据库、端口、BEAM cookie和fixture命名空间。共享设备、PG资源压力、浏览器会话、全量EUnit及最终回退演练串行；没有租约不启动。

## 4. 任务卡与快速验收 / Task cards

每卡输出精确子项台账、原始日志/exit/hash、候选指纹、修复影响面和剩余阻断。先复现再最小修复，已有行为通过就不重写。时间盒指首次定位与交接，不是质量门截止时间。

| 卡/首次时间盒 | 依赖与唯一责任面 | 测试与验收oracle |
|---|---|---|
| U00 / 60–90min | 协调者，三仓只读基线/原合同/运行资源 | 输出完整源集合、可重建候选、WIP归属、租约；旧PASS降级或历史专项记录有原因；CX/缓存交接边界清楚 |
| U01 / 60min | U00；证据台账owner，仅control/reports | 校正3/23与2/24差异、run2九败/run3三败、每文件单独hash、MC22修复方向；缺证据标UNVERIFIED；不覆盖历史文件 |
| U02 / 90min | U00；后端测试环境owner，构建/runner/fixture | hird依赖可用并绑定版本；全量失败逐项复现；dialyze基线差分可复算；225 skips逐条归属/原合同映射；不得删测试、吞exit或改阈值 |
| U03 / 2h | U00；Core IM后端+App消息域，具体文件租约 | AC07–14全部子场景：ACK/阅读分离，四种成员事件×回滚/重放/离线，ACK前后崩溃，20页续拉/切网，编辑/附件授权，跨账号残留；真实WS+DB+两端本地集合一致 |
| U04 / 2h | U00；组织后端域与专用tests | AC15/16、EO02/03/05/06：生命周期、双角色/双组织/双WS、唯一owner、离岗交接失败恢复、旧token/WS/票据撤权；零越权副作用；完整组织到发消息旅程 |
| U05 / 2h | U00；微社群关系/迁移/权限后端及tests | MC02–08、14–20：fresh/upgrade保留，FK/唯一/大ID，绑定权限/负例/并发/回滚，全部入群入口、订单状态、退款/到期/DB故障；真实PG/API/WS，长连接无需重登失权 |
| U06 / 90min | U00；频道内容后端与专用tests | MC09–13：文本图片附件回归，精华×置顶四组合，tags部分编辑与payload保留，正文检索，分页及IDOR/撤权媒体；查询命中、对象字节与DB授权联合断言 |
| U07 / 2h | U03–06对应接口稳定；App组织/社群域 | MC21/22、EO04/08和AC12/14/15/16端侧：Android正反旅程，真实VIEW发送/接收、弱网/聊天撤权；组织树分页/失败/触控；macOS原合同治理；每轮独立不可覆盖日志、build/candidate绑定 |
| U08 / 90min | U04–06对应接口稳定；Admin组织/资产/社群页 | MC23及EO资产/治理缺格：真实浏览器双角色、scope切换、绑定冲突/精华管理，UI/API/DB逐一对拍；不拦截伪造业务响应 |
| U09 / 90min | U00；一期profile/推送及跨会话交接owner | AC01–06/17–24与EO07未覆盖项逐格核验；AC05禁用入口原要求与豁免分账；AC23点击会话/杀进程/切账号逐格真实设备；CX只接收匹配候选证据，OA getAuthCode只做继承核验 |
| U10 / 2h | U03–09受影响实现稳定；恢复/交付文档owner | AC25–27/29与MC24：tracked交付白名单、文档链接/命令、新环境安装升级；非空消息/订单/成员/附件fixtures，PG+Garage恢复到隔离目标，恢复服务实际读取与ID/字节对拍；业务数据库升级失败回退，不以marker替代 |
| U11 / 视完整门耗时 | U01–10本地子项完成且依赖已交接；集成owner | 重冻三仓最终候选；同候选全EUnit连续两轮RC0、Admin test/lint/typecheck/deadcode/build、App analyze/test及原必需集成门；required skip/cancel=0或授权修订分账；全量只集中跑 |
| U12 / 60min | U11；独立只读verifier | 缺/多/重复ID、错SHA/hash、日志复用、非零exit、假父PASS、错误设备、dirty字节漏绑/WIP混入负例应拒；真实正常包通过；父项全子项通过，统计由ledger生成 |

每卡至少一个明确负例。安全/权限/恢复卡必须验证DB无副作用或恢复后的实际业务读取，单纯HTTP200/截图/测试数量均不足。U03/U07共享App文件时协调者切分写集或串行；U04/U05共享群权限文件同理。

## 5. 调度与持续推进 / Scheduling

允许后续ZCODE执行时最多10个并发agent，包括协调者与verifier，不能10 worker再加管理agent。建议角色：A0协调集成；A1证据；A2环境全量；A3 IM；A4组织后端；A5社群权限；A6内容；A7 App；A8 Admin；A9恢复交付。结束阶段释放A1后复用为独立verifier，避免自己验收自己的修复。

波次0：U00串行建立合同、候选与租约；U01/U02可随后并行。
波次1：U03–06并行补后端与专项证据，U09同步核验交接；A7/A8可先准备既存真端runner、定位已有失败。
波次2：稳定接口上的U07/U08执行真端旅程，U10准备恢复包；禁止同一设备/DB压力窗口抢占。
波次3：U10恢复、U11最终门、U12终审串行。修复任何源/runner/fixture/config后重新冻结并重跑影响项；全量成功必须来自最终指纹。

阻塞单卡继续其他独立卡。每根因最多两轮最小修复，仍失败则交接BLOCKED+复现命令+NEXT_UNLOCK，禁止无限重跑。不存在环境/设备/签名时保留BLOCKED_ENV，不自动缩平台。优先快验收卡，完整全量轮不在每worker重复执行。

## 6. 命令与证据合同 / Commands and evidence

复用现有入口：imboy/scripts/test/phase1_gates/、imboy/scripts/verify_micro_community_acceptance.py；App integration_test/micro_community_owned_android_test.dart、micro_community_owned_android_negative_test.dart、enterprise/enterprise_scope_native_test.dart；Admin tests/e2e/enterprise-app-governance-real.spec.ts。U00须先读取runner及原计划命令，生成本run隔离参数；不能原样运行旧脚本误连共享库。

基础门从相应候选根执行：Backend make compile / make eunit / make dialyze；Admin bun run test / bun run lint / bun run typecheck / bun run build，deadcode按当前package脚本；App flutter analyze / flutter test，真端runner按实际设备ID。本段是命令模板，不是执行证据，环境/具体spec及参数必须在commands.sh完整展开。

RUN_ROOT放工作区外。control包含baseline、source-contract hashes、requirements、leases、candidate-manifest、commands、command-receipts、inheritance、recovery；artifacts按card/subcase/round独立目录；final包含ledger、verdict和report。秘密和PII不入仓、不写日志；仅合成数据。

每receipt记录argv/cwd、开始结束时间、exit、source/build/runner/config/fixturehash、平台设备、子断言、原始日志sha256。两个日志分别hash，不用一个hash表示多文件；各轮禁止覆盖与复用同一执行。历史引用携带原run绝对定位与digest，自包含副本或可解析映射；真实性缺口保持UNVERIFIED。

历史继承只适用于证明覆盖完整、字节绑定完整且影响分析支持的专项；共享授权/DBschema/profile/cache/序列变更会影响关联项，必须重验。历史全量不替代最终全量；旧build不替代新candidate。

## 7. 终态与逐卡交付 / Verdict

每卡可签 TASK_PASS（仅卡的精确子集合），不等价整个功能完成。

- UNIFIED_LOCAL_CANDIDATE_PASS：原本地required集合全部通过，依赖证据完整，最终候选质量门、恢复和独立终审通过，无未解决P0/P1；不包含明确后期专项与外部签收。
- REVISED_SCOPE_LOCAL_PASS：仅在正式授权修订后对修订集合全通过可签；必须同时列原合同未完成项，不能换名宣称原计划完成。
- PARTIAL：存在已执行可信成果，但至少一required子项缺证据/失败/依赖未闭合。
- BLOCKED：当前所需环境/授权/合同输入缺失，列解锁条件；独立卡继续推进。

verdict同时输出三个source_original_contract_status、current_scope_status、后期/外部/依赖队列及全部counts；不以两计划名义比例推导统一严格完成率。phase1 AC31–34保留DEFERRED_BY_USER；CORE_IM_9_9_LOCAL不得签发。企业外部门禁、UAT、客户签收及PRODUCTION分别NOT_EXECUTED/BLOCKED_EXTERNAL，不能被本地PASS覆盖。

## 8. ZCODE执行交接 / Handoff

执行者启动须机械核对本计划.sha256，失败即PLAN_HASH_MISMATCH并停止依赖操作；重新采样三仓现场状态，不沿用编制时HEAD。先完成U00，输出源合同全集及未核实授权决策卡，再按租约最多10agent持续推进。

不要重复开CX、缓存、OA getAuthCode、E2EE专项或L4专项任务；依赖未完成时如实等待交接，不能为了本计划PASS把它们标不适用。不得回滚别的会话WIP。每卡交接提供状态、改动路径、测试命令/exit/断言/hash、候选、NEXT_UNLOCK；最终独立终审不得根据摘要代签。

## 9. 冻结源摘要 / Source digests

- `docs/design/2026-10-08-micro-community/plan-v1.md`（冻结前）：`5388e75002469219a569cd00d00388dced3efe8be686a35c219b1ca03feb1adc`。
- `docs/design/2026-10-07-phase1-delivery-quality-plan.md`（冻结前）：`3074702ea0b00ab32eb58252a35cc0f62217376b938c932ba9a05ccced1d5e00`。
- `docs/design/2026-10-06-enterprise-organization-gap/plan-v1.md`（冻结前）：`484408ed8b0fec126267cb155875a75d2d24f7aef377df6b23cae8b48824961f`。
- 企业历史 verdict SHA256：`f280a117836e088c5f0a887bc190939cff981506c42251871b58e89399ff9aa2`；保留原字节。
