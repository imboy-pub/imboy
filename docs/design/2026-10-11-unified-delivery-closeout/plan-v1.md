# 一期交付、微社群、企业组织统一收口计划 V1 / Unified Delivery Closeout

日期：2026-10-11。状态：PLAN_READY / NOT_EXECUTED。执行模式：UNATTENDED_LOCAL_CLOSEOUT。当前会话仅编制执行包；用户把配套 zcode-prompt-v1.md 交给 ZCODE 启动后，该提示词授权其按本计划持续完成本地实施、测试、隔离环境修复及本地提交；不授权外向或生产操作。

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

旧范围修订没有可核实授权时默认恢复原合同，自动补测或修复，不等待用户选择豁免；只有不可兼容产品合同冲突才登记BLOCKED_CONTRACT并继续独立任务。正式修订另记 revised_scope_status，original_contract_status仍反映原合同差异。用户本轮排除的后期专项记 DEFERRED_BY_USER，不伪装原计划完成。

## 3. 启动门与不可变候选 / Entry gates

U00记录三仓HEAD/tree、完整tracked patch与未跟踪白名单内容hash、依赖锁、runner、配置脱敏hash、fixture schema、构建工具/产物hash。当前主树有缓存WIP与其他未跟踪文档，不能吸收或回滚。采用独立checkout/worktree，创建前检查现有worktree；分支和本地提交依执行授权及仓库规则。

历史protected WIP快照保持不变；主树并行演进不等于历史完整性通过。U00按路径、提交与内容归属分类重订本run基线，证明候选未混入外来WIP。新基线不能追溯消除旧run漂移；不得补造 attested 日志。

建立 control/requirements.tsv：cell_id/source_plan/source_id/source_locator/subcase/platform/role/oracle/required_local/dependency/owner/status/evidence/candidate。叶子唯一键为(source_plan, source_id, subcase, platform, role)，cell_id是该元组规范JSON的SHA256；父ID可以合法重复，禁止仅按父ID查重。source_locator绑定冻结源hash及节/行；多角色或多平台场景按原合同拆格，不能以ALL掩盖缺平台。每个原父项展开全部子场景，每格明确本地、依赖、后期或外部。共享测试可映射多格，必须分别证明子断言。U12从冻结源独立复核预期叶子全集，再核requirements与ledger，禁止三表从同一遗漏集合生成后自证完整。

U00输出 control/leases.json 与无省略号 commands.sh。每worker独立build/ebin、数据库、端口、BEAM cookie和fixture命名空间。共享设备、PG资源压力、浏览器会话、全量EUnit及最终回退演练串行；没有租约不启动。

## 4. 任务卡与快速验收 / Task cards

每卡输出精确子项台账、原始日志/exit/hash、候选指纹、修复影响面和剩余阻断。先复现再最小修复，已有行为通过就不重写。时间盒指首次定位与交接，不是质量门截止时间。

| 卡/首次时间盒 | 依赖与唯一责任面 | 测试与验收oracle |
|---|---|---|
| U00 / 60–90min | 协调者，三仓只读基线/原合同/运行资源 | 输出叶子集合/源定位/责任卡映射、最终门清单、可重建候选、WIP归属、共同租约入口和命令超时/心跳清单；旧PASS继承有依据；CX/缓存交接边界清楚 |
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
| U11 / 视完整门耗时 | 可执行修改已收束、三仓候选固定；集成owner；不要求受阻U07/U09全闭合 | 按final-gates.tsv运行前置齐备的最终子门，显式含最终候选dialyze；EUnit两轮，其余按冻结原合同轮次；受阻门逐项BLOCKED不挡独立门；全量集中跑 |
| U12 / 60min | U11可执行门已运行、受阻门有诊断；新独立只读verifier | 先独立复核源合同叶子全集，再验ledger/hash及正反例。FULL审查全required闭合才签统一PASS；PARTIAL审查核成果/阻断/续跑点，只签PARTIAL/BLOCKED；真实包准确性通过不等价功能PASS |

每卡至少一个明确负例。安全/权限/恢复卡必须验证DB无副作用或恢复后的实际业务读取，单纯HTTP200/截图/测试数量均不足。U03/U07共享App文件时协调者切分写集或串行；U04/U05共享群权限文件同理。

## 5. 调度与持续推进 / Scheduling

允许后续ZCODE执行时最多10个并发agent，包括协调者与verifier，不能10 worker再加管理agent。建议角色：A0协调集成；A1证据；A2环境全量；A3 IM；A4组织后端；A5社群权限；A6内容；A7 App；A8 Admin；A9恢复交付。结束阶段释放A1的并发名额，启动未参与本run实现、requirements/ledger编制及结论映射的新只读verifier；原A1只解释材料，不签发终审。

波次0：U00串行建立合同、候选与租约；U01/U02可随后并行。
波次1：U03–06并行补后端与专项证据，U09同步核验交接；A7/A8可先准备既存真端runner、定位已有失败。
波次2：稳定接口上的U07/U08执行真端旅程，U10准备恢复包；禁止同一设备/DB压力窗口抢占。
波次3：U10可执行恢复、U11可执行最终门、U12完整或部分结果终审串行；真实阻断不封死该收尾路径。依赖后续到达则集成、重冻并重验影响项，再签新的终审。全量成功必须来自最终指纹。

阻塞单卡继续其他独立卡。每根因连续两轮无新证据的相同修复必须转独立诊断；有新根因或新证据可继续修复并登记，不能两轮失败即草率放弃。不存在环境/设备/签名时先按第10节自恢复，再保留BLOCKED_ENV，不自动缩平台。优先快验收卡，完整全量轮不在每worker重复执行。

## 6. 命令与证据合同 / Commands and evidence

复用现有入口：imboy/scripts/test/phase1_gates/、imboy/scripts/verify_micro_community_acceptance.py；App integration_test/micro_community_owned_android_test.dart、micro_community_owned_android_negative_test.dart、enterprise/enterprise_scope_native_test.dart；Admin tests/e2e/enterprise-app-governance-real.spec.ts。U00须先读取runner及原计划命令，生成本run隔离参数；不能原样运行旧脚本误连共享库。

基础门从相应候选根执行：Backend make compile / make eunit / make dialyze；Admin bun run test / bun run lint / bun run typecheck / bun run build，deadcode按当前package脚本；App flutter analyze / flutter test，真端runner按实际设备ID。本段是命令模板，不是执行证据，环境/具体spec及参数必须在commands.sh完整展开。

U00冻结control/final-gates.tsv作为轮次、绑定和判定的唯一执行清单：gate_id/required_cells/source_locator/command_ref/candidate_binding/rounds/expected_exit/warning_policy/skip_policy/prerequisites/timeout_ref。Backend compile、dialyze，Admin test/lint/typecheck/deadcode/build，App analyze/test及原required真实集成/恢复门均必须有行，不能漏dialyze或引用U02旧候选结果。EUnit固定最终候选连续两轮；其余至少一次，原合同明确多轮则采用其轮次，并记录来源；无依据不统一强加两轮。遇到互斥原标准先登记BLOCKED_CONTRACT，不凭代理放宽。

dialyze默认最终候选RC0，原合同更严格警告要求同时保留；警告基线差分用于归因，不能把非零exit转换PASS。每门的exit、警告、skip/cancel规则启动前冻结，无法确定的规则阻断该门，不阻断独立门。真端历史轮次只有通过最终build/candidate影响分析才能继承；最终EUnit与dialyze必须实际在最终候选运行。候选变化按影响闭包失效重验，不因清单冻结拒绝必要修复。

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

执行者启动须机械核对本计划.sha256及配套提示词硬编码hash，失败即PLAN_HASH_MISMATCH并停止依赖操作；重新采样三仓现场状态，不沿用编制时HEAD。先完成U00，输出源合同全集及默认决策登记，再按租约最多10agent持续推进；不把U00报告作为等待人工批准的检查点。

不要重复开CX、缓存、OA getAuthCode、E2EE专项或L4专项任务；依赖未完成时如实等待交接，不能为了本计划PASS把它们标不适用。不得回滚别的会话WIP。每卡交接提供状态、改动路径、测试命令/exit/断言/hash、候选、NEXT_UNLOCK；最终独立终审不得根据摘要代签。

## 9. 冻结源摘要 / Source digests

- `docs/design/2026-10-08-micro-community/plan-v1.md`（冻结前）：`5388e75002469219a569cd00d00388dced3efe8be686a35c219b1ca03feb1adc`。
- `docs/design/2026-10-07-phase1-delivery-quality-plan.md`（冻结前）：`3074702ea0b00ab32eb58252a35cc0f62217376b938c932ba9a05ccced1d5e00`。
- `docs/design/2026-10-06-enterprise-organization-gap/plan-v1.md`（冻结前）：`484408ed8b0fec126267cb155875a75d2d24f7aef377df6b23cae8b48824961f`。
- 企业历史 verdict SHA256：`f280a117836e088c5f0a887bc190939cff981506c42251871b58e89399ff9aa2`；保留原字节。

## 10. 无人值守执行协议 / Unattended execution protocol

### 10.1 预授权默认决策

配套提示词启动后，以下本地动作无需再次询问：读取三仓及历史run；建立或复用隔离worktree/执行分支；安装锁定版本开发依赖到本run；构建本地hird；创建合成数据隔离PG/Garage和本run服务；修复本范围源码、runner、测试与文档；在已连接的用户测试设备运行原合同旅程；按功能提交到各自执行分支。不修改Git全局配置，本地提交命令级身份使用 leeyi <leeyisoft@qq.com>。

自动选择规则：原合同优先于无来源豁免；当前源码优先于旧缺陷定性；项目已有实现优先于新抽象；修复最小影响面；未知范围不扩大。macOS不支持移动OA时执行有来源的平台合同，不新增移动OA能力；个人企业入口沿“我→切换企业”，不复活旧通讯录入口。相互不兼容且没有权威裁决的要求保留双方来源，标BLOCKED_CONTRACT，不凭代理投票裁决。

不要发起常规“是否继续/是否跑测试/是否提交/选哪个方案”问题。自动完成授权内动作。外向操作、联系方式、生产、签名身份、凭证购买、第三方/UAT仍未授权：登记外部门禁即可，不阻塞其他本地卡，不把未授权动作强行做完。

### 10.2 启动预检与修复路径

U00逐项检查Git/WIP、工具链、hird、PG/Garage、真实浏览器、Android设备及macOS签名/运行条件，记录检查命令、exit、日志。预检失败先定位现有配置/工具/同仓说明再重试，不立即把“找不到命令”等价无法实施。

| 问题 | 自动恢复 | 不能越过的边界 |
|---|---|---|
| hird缺失 | 查本地源码/已安装二进制，按项目锁定工具链构建到本run并设置run级PATH；验证行为与版本 | 不生成假hird、不跳过调用，不替换生产验证逻辑 |
| PG连接墙、Garage/端口冲突 | 独立本run容器/数据库与端口，控制并发和连接池；记录创建资源及清理清单 | 不重启/改配置/删除共享或缓存/CX会话服务 |
| macOS签名或Android离线 | 复用现有合法配置与已连接测试设备；检查本run构建与设备状态；限三次有诊断依据的重连/启动 | 不改Apple账号/签名身份、购买描述文件、自动解锁设备或重置设备 |
| 历史日志缺失/覆盖/dirty漏绑 | 重新执行相应子场景，使用不可覆盖轮次目录与完整源码/构建绑定 | 不补造历史日志，不把新基线当旧完整性通过 |
| skips/dialyze失败 | 建立逐项归属与基线差分；修复范围内问题；无可核实豁免则执行原required用例 | 基线解释不等价原质量门PASS；不扩大ignore或关闭检查 |
| 文件写集冲突 | 暂停冲突卡，由A0划分具体路径/时段；先推进其他卡 | 不覆盖外来WIP；不随意复制他人dirty补丁 |

### 10.3 与CX/缓存会话的自动交接

先只读发现已有run、候选及交接文件，记录producer/run/candidate/hash/status。没有交接协议时，在本RUN_ROOT建立 dependency-request.json，详细列依赖ID、所需证据及版本，不自行向第三方发送消息。

本run的leases.json只是登记，不能证明跨会话授权。U00发现并记录双方共同认可的已有协调入口、规范resource_id、grantor/原子取得凭证、holder(run/agent/host/PID及启动标识)、授予/过期/释放条件及producer可发现的请求路径。没有共同入口或确认的授予凭证时，共享资源保持BLOCKED_RESOURCE；优先用可证明隔离的替代资源，不新建协调服务、不自行宣告租约。持有者是否死亡须核PID启动身份，不能只因TTL过期夺取仍在运行资源；无法确认则不抢占。

有共同祖先与干净提交、范围归属明确、证据匹配时，可在本run集成分支自动集成该本地已提交成果并重验影响项；不合并到共享main、不吸收dirty文件。依赖生产者仍在改动时，消费者固定一个已提交版本，后续版本变化由影响分析触发重验，不跟随每次主树漂移。

没有可用交接则每个调度波次检查一次；独立任务结束后进行三次间隔至少10分钟的非阻塞检查，期间完成台账/恢复包/审查，不阻塞睡眠。仍无交接的required项记BLOCKED_DEPENDENCY并输出可续跑checkpoint；不无限等待，也不签UNIFIED_LOCAL_CANDIDATE_PASS。

### 10.4 控制循环、崩溃恢复与结束条件

A0循环执行：复算可运行卡→分配不冲突写集/资源→实施与专项验收→读取交接并集成→更新账本→释放资源→调度下一卡。每完成子场景原子保存 control/checkpoint.json 和追加 events.jsonl，包含计划hash、源集合hash、候选、owner、task状态、资源、已完成receipt及下一动作。

U00登记control/runtime-policy.tsv：command/runner、已有超时参数及实际生效值、历史耗时依据、本run进程启动标识/进程组、worker心跳、日志与进度观测、诊断/回收步骤。优先复用runner超时机制。worker每60秒心跳；连续5分钟心跳缺失且无命令进度，或命令达到登记timeout，触发诊断，不直接判测试失败/跳过。健康心跳下的长全量命令按其timeout判断；首次卡时间盒不是timeout。诊断先保存日志/进程树/资源状态，只有确认属于本run且需终止才温和停止，超时中断记录TIMEOUT，不能算RC0。

agent失败先核查残留进程/写入，确认安全后释放该任务租约；未确认死亡不重复启动资源。最多两次恢复同一失联worker，之后换agent做独立诊断。恢复会话读取checkpoint并重算现场指纹，证据指纹不匹配则重验影响项，不能复用不明结果。只清理清单内确认属于本run的临时资源；保留失败证据与工作树。

修改后先专项，通过后本地分功能提交，集成到本run三仓分支。最终构建及各门按final-gates.tsv的唯一轮次在已提交固定候选运行，只有EUnit默认两轮；允许运行生成物的精确白名单hash，其余dirty必须解释并绑定。源或测试发生变化使受影响旧gate失效，按影响闭包重跑；新候选EUnit两轮不继承旧候选。

全程每5分钟更新progress.md，每张卡交接含下一动作。上下文压缩/工具超时不是任务结束，保留checkpoint并继续。只有以下条件可结束本次无人值守执行：

1. 全部本地required及交接项通过U12，输出UNIFIED_LOCAL_CANDIDATE_PASS与三仓提交/候选/证据。
2. 授权内可执行工作及U11可执行最终门已全部完成，剩余仅经诊断、自恢复和U12 PARTIAL终审确认的真实环境/合同/外部/依赖阻断：输出AUTONOMOUS_RUN_EXHAUSTED和总体PARTIAL/BLOCKED、逐项NEXT_UNLOCK。该状态表示本次自动执行穷尽，不表示功能收口。
3. 计划hash错误、确定数据隔离失效或越权风险：停止受影响操作，保全现场；其他不受影响任务仅在隔离可证明时继续。

“两轮失败”“剩一个任务”“需要用户选常规实现方案”都不是结束条件。不得只写报告、提交测试准备文件或标一批豁免后宣称干完。不能保证外部条件齐备时一次性全PASS，但必须一次启动完成全部可执行工作，并留下无需重复起步的续跑包。

### 10.5 最终交付物

必须交付三仓本地提交清单/执行分支与集成状态；准确的父项及子项ledger；每个原计划original_contract_status；不可变最终candidate与产物清单；实际commands/exit/oracle/hash；U12正反终审；blockers及NEXT_UNLOCK；resume-prompt.md。没有主仓合并/push/发布则如实记录NOT_PERFORMED，不把执行分支通过说成共享main通过。

resume-prompt.md固定本计划hash与RUN_ROOT，要求从checkpoint继续，仅重跑失效项。会话运行中自动发现设备/交接解锁并推进；执行已结束时，只能由已存在且获授权的持久调度器或重新启动的执行者加载checkpoint续跑。U00记录是否存在该调度入口；没有则如实写RESUME_REQUIRES_RELAUNCH，交付可直接发送的续跑提示词，不暗示已结束会话会自行唤醒，不新增后台守护服务。
