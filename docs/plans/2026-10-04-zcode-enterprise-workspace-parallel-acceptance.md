# 企业与 Workspace scope 全功能并行验收 / Enterprise Workspace Acceptance

**Goal / 目标：** 快速、高质量验收当前企业组织管理、企业群/频道、企业消息及归档、企业Admin和全部其他Workspace相关功能；复用已有成果，修真实阻断，形成完整当前候选证据。

**Architecture / 架构：** 当前源码/路由/历史合同联合盘点，以完整业务旅程批量覆盖功能；Backend与Admin可并行准备及验证，App/设备与全产品线单一owner协作；隔离环境执行，冻结候选终审。

**Tech Stack / 技术：** Erlang/OTP、PG18、Flutter Android真机/macOS、React/Bun、真实浏览器、既有验收runner。

## 1. 输入与完整范围 / Inputs and Scope

工作区 `/Users/leeyi/project/imboy.pub` 非Git根；读取根及三仓AGENTS/引用规范。重新记录每仓main、worktree、WIP、输入SHA和租约，不使用历史候选冒充当前候选。

主要权威合同：

- `imboy/docs/plans/2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md`，SHA256 `a866aa137eea856a9a6d9324d3cb46d3fb1f1fb8db73891808c85d1f17d23302`；读取其当前main/final-closure后续计划，逐条建立继承/替代关系，不混用冲突版本。
- `imboy/docs/plans/2026-10-02-enterprise-non-oa-final-acceptance-plan-v1.md`，SHA256 `e083be9d911bfec5c76f2b0cfdc87a1c09c9eded7d1172e0f455abf55ddd31f1`；它只覆盖非OA增量，不能限定本轮“全部企业功能”。
- `imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md`、`ux-final-decision.md`、组织目录签名/资料归属/Workspace归档等后续合同；`2026-10-02-enterprise-non-oa-closure/evidence/20261003-corrective-review/FINAL.md` 当前明确PARTIAL，不继承16/16完成声明。
- `imboy/docs/concepts/enterprise-business.md`、企业组织core/agent合同、`api/internal/v1/`、当前route/OpenAPI/Postman，以及已有Workspace QA记录。
- 全产品验收合同和客服并行验收合同：分工/证据交接输入，不替代本轮要求。

启动重算摘要，漂移须解释；历史统计31 endpoints/14 scopes/4 Directory API等须按实际source和权威合同重算，不写死当完成事实。新发现可达企业功能必须补入清单，不能只选下面表中的已知模块。

现场已有组织、enterprise_business、enterprise_apps、enterprise_access及Workspace/group/channel/message/retention源和用例。企业后台不只检查sidebar八个叶子：还需扫描全App router、页面组件、dialog/drawer、API、权限声明、深链及后台任务，登记菜单外操作。

本轮包含OA/Application本地实现、授权边界、配置/凭据/审计/失败路径；客户OA、外部SSO、外向Webhook/通知等真实联调需明确授权，登记BLOCKED_EXTERNAL，不删除。未来尚未定义功能不凭空发明；已承诺但缺实现的必需能力记GAP_IMPLEMENTATION。

## 2. 跨验收线所有权 / Parallel Ownership

企业线独立Backend/Admin worktree、run分支、PG容器/数据库、节点/cookie、服务端口、浏览器context、build输出和fixture namespace。RUN_ROOT位于企业Backend worktree的 `docs/design/2026-10-04-enterprise-workspace-acceptance/evidence/<run_id>/`。不写全产品或客服run控制文件，不重置/清理/stash他人WIP。

App相关源码、spec及Android/macOS设备首先由全产品线持有。企业协调者交付企业case manifest与缺陷卡，双方先登记唯一owner：优先让全产品执行企业设备旅程并交回同候选证据；也可经正式handoff转交企业线专属App路径/设备时间窗。没有handoff时企业线只读App，继续Backend/Admin，不抢占或重复跑。App源码修复和路由/i18n/token共享改动必须唯一owner。

企业线不修改客服坐席域；企业内嵌客服仅验租户/scope/入口关联，Seat67项由客服线负责并交证据。共享组织/resolver/auth/router/Makefile/vite/package/sidebar/全局路由等文件逐项登记唯一owner和integration队列；独立worktree不意味着允许不同owner同时修改同一合同。

所有资源写操作以已有精确批准范围为准；本合同不自动批准新数据库启动、迁移、合成联系方式、外部请求或清理。需批准时先完成具体拓扑/配置/动作审阅表，一次性提交，继续无依赖任务。禁止生产/共享DB、真实客户数据、外向通知、真实支付、push/部署。新迁移遵守原子编号预留，不使用历史最高编号猜新编号。

设备/Backend/全局门均需独占租约，三个run全局测试排队；对完全相同候选/config/runtime/fixture和oracle的共享门可复用原始证据，不跨SHA借用，也不把客服PASS转成企业App PASS。

## 3. 任务卡与验收矩阵 / Cards and Acceptance

每卡有4个新增协调ID：`EWS-NN-A01`盘点闭合；`A02`真实后端正负路径与持久化；`A03`所有适用Admin/App动作及角色/平台；`A04`跨scope、生命周期、故障/恢复和证据闭合。EWS-00–EWS-12共52个协调ID，只提供索引，不能代替继承计划的原ID或逐功能格。每项具体oracle见下表，所有缺项均保留。

| 卡 | 范围与必须验证的事实 | 建议owner |
|---|---|---|
| EWS-00 | 输入、源码/路由/API/后台任务全盘点，原合同/required IDs继承表，exact-set、资源与分工闭合；A02为gate假绿拒绝，A03为各执行面可用性，A04为漂移/恢复/污染拒绝 | 协调者 |
| EWS-01 | 组织创建/列表/详情/修改/归档/恢复；Owner/Admin/Member角色、邀请接受/拒绝/过期/撤销、成员停用/退出/移除、Owner转移与待确认；默认Workspace关系和原子失败 | Backend治理 |
| EWS-02 | 部门CRUD/树/调岗/主部门/部门管理员、Human Directory；组织图完整分页/父子/成员数/拖动缩放/适应；撤权/换号/换企业清缓存，晚到响应不能恢复旧数据 | Backend治理+App owner |
| EWS-03 | Workspace创建/更新/模板/默认关系/切换/列表/成员/邀请/角色/归档恢复；Org角色与Workspace角色独立，重复/并发/失败回滚、archived禁止写，数据归属和默认关系约束 | Backend治理 |
| EWS-04 | 企业群创建/列表/详情/成员管理/加入退出/踢人/邀请/权限/历史/附件；群成员是有效Workspace成员子集，离岗撤权、跨企业伪gid/scope、企业托管与真人E2EE正确隔离 | 消息owner |
| EWS-05 | 企业频道创建/订阅/发布/编辑/删除或现有生命周期/分页/搜索/权限/附件/通知；企业过滤须服务端强制，切scope及撤权、深链绕过、边界并发 | 消息owner |
| EWS-06 | 企业直聊/群消息、会话、未读/ACK、多设备/重连/重启、编辑/撤回、附件/历史搜索和消息归档；canonical内容与delivery分离，ACK不删除归档，workspace快照/发送者/来源application审计准确 | 消息owner |
| EWS-07 | 保留策略版本快照、retain_until、合法hold创建/解除/优先级、bounded purge、附件保留关联、审计不可篡改；注入时钟/独立fixture验证边界、批量/SKIP LOCKED/竞争；存在有效hold不得purge，仅无有效hold且retain_until到期、满足授权及其他策略才可删除；个人msg_store不算企业归档 | Backend合规 |
| EWS-08 | 离岗交接case/item、重试幂等/失败恢复/审计、联系人/会话/群/频道/项目/资料资产交接、先交接后移除、旧权限立即失效；不能仅隐藏按钮 | Backend合规 |
| EWS-09 | 企业资产/资料/content/file、搜索/下载/presign确认/失效/归属转换、项目CRUD及现有status；签名URL/附件授权跨scope拒绝，项目done不擅解释成archive/delete；其他Workspace内容全部盘点 | 资产/内容owner |
| EWS-10 | Application/Credential/Grant、scope目录、禁用/轮换/撤销、零Grant拒绝、deny优先级、幂等/cursor/audit、Internal route/OpenAPI/Postman响应一致；四认证域隔离，映射Human代发保留origin_application_id且为企业托管非E2EE | Backend应用+Admin |
| EWS-11 | 所有企业Admin菜单及菜单外页面/弹窗/操作/导出/日志：organization/workspace/project/group/channel/message/archive/enterprise_business/applications/offboarding/access；picker/query/preset只是UI提示，服务端仍scope校验；权限直达/撤权/分页/详情异步隔离 | Admin owner |
| EWS-12 | 企业App消息/通讯录/工作台/我、企业切换、全部现有企业原生入口、OA本地边界、客服及应用入口关联；Android/macOS真机与Admin治理结果消费、共享基建完整回归、独立冻结终审 | 全产品App owner+协调/V |

为每个发现功能建立唯一主owner和case/platform/role/profile集合。至少构造Org A两个Workspace、Org B一个Workspace；Owner/Admin/Member、无membership、退出/停用、archived、Application/Grant撤销等合成身份。请求org/ws/gid/channel/asset/message标识逐个单独篡改并证明无越权读写；有A访问权不能因此获得A下所有Workspace访问权。

平台与适用性按真实产品支持和原合同登记：没有UI的API/worker不要虚构UI PASS，纯浏览器功能不能替代App；required无入口记GAP，NOT_APPLICABLE必须有来源和批准，不能用关闭功能换绿。

## 4. 波次与提速 / Execution Waves

W0：重采三仓和资源；先运行现有证据完整性检查，审source binding、oracle和适用性；建立全量scope inventory、继承表、coverage/ledger。已通过旧日志可定位缺口，不能换HEAD手签新PASS。首检查点明确能立即执行的集合和冲突/待批准资源，不停留在另写计划。

W1：Backend治理/消息合规、Admin、Internal合同可独立并行。每人只写租约内路径，最多3名实施者+1名只读终审；资源不足顺序执行。先跑已有域用例，只有真实缺口才补测试或修产品。自动采集用户动作与后端事实，不按功能行数新建重复文件。

W2：环境批准/资格就绪后批量真实旅程：①组织/邀请/部门/默认Workspace；②Workspace角色/企业群频道及伪scope；③消息/归档/ACK/重连；④Admin撤权/切换/离岗交接；⑤资产/保留hold/purge；⑥Application Grant/Internal代发/审计。每动作定位独立ID，复用同一fixture，不六次重建环境。App旅程通过正式handoff排设备时间窗。

W3：FAIL保留原日志→根因→最小修复→独立review→受影响域回归→独立本地commit；只修阻断，不重新实现已完成域、不顺带改全产品/客服。代码/config/fixture/runner改变使相关证据失效。

W4：预验闭合冻结三仓SHA、plan/config/fixture/runtime/依赖/runner/build和artifact；一次完整全局门及全部required真实旅程，只读终审核 exact set/hash/source/oracle。发现失败回责任卡重新冻结，不放宽门。分别交企业本地、Admin、Android/macOS、Internal、外部集成和生产状态。

## 5. 复用命令与用例 / Existing Test Entrypoints

Backend：现有 `test/lib/workspace_guard_tests.erl`、resolver/default-workspace/access、`test/api/workspace_boundary_tests.erl`、`test/ds/group_member_workspace_subset_tests.erl`、真实Workspace创建/成员群/消息PG/归档并发、`test/features/enterprise_business/infrastructure/eb_retention_pg_tests.erl` 及offboarding用例。使用当前编译产物和本run隔离配置运行，不沿用默认共享PG。

Internal文档入口：Backend `python3 api/flatten_internal.py --check`；再比 runtime/OpenAPI/Postman method+path+schema+header和实际授权/幂等oracle，不能只比较endpoint数量。

Admin复用 `tests/e2e/admin-organization-governance.spec.ts`、`enterprise-app-governance-real.spec.ts`、`enterprise-ent-int01/`、`enterprise-business.spec.ts`、Workspace/archive历史spec；先检查每个是否mock或固定端口/凭据。Mock与菜单静态结果仅L0，不能记真实UI/API通过。

Admin单测通过 `bun run test`（脚本含isolate）运行明确域集合。真实浏览器沿现有Playwright配置最小参数化成本run隔离配置，先 `bunx playwright test --config=<本run企业配置> --list`核required集合，再租约内执行；不默认全`playwright.config.ts`指向安全环境，不下载浏览器掩盖缺能力。

App复用组织图/权限/enterprise_scope、workspace_channel_navigation和workspace integration用例，交全产品App owner审有效config/fixture/source/platform；widget/offline测试不替代原生窗口和真实数据。历史macOS未闭合的跨账号/迟到响应/故障恢复/管理员直达/真实窗口及触控板证据仍须原oracle验证。

最终全门按继承原合同完整命令集合执行：Backend compile/migration/contract/arch/security与全EUnit；Admin lint/typecheck/isolate全单测/build/真实企业浏览器；App分析/受影响域及完整required原生回归。后端全EUnit沿全产品合同两次连续RC0、TSID VM-global隔离，不能将失败归因存量后豁免。共享门排队且证据绑定完全一致才可复用。

## 6. 证据、恢复与最终交付 / Delivery

每格记录original ID+EWS索引、case/platform/role/scope、plan/三仓SHA、command/exit、executed/skip、实际UI/API/DB/worker oracle、配置/fixture/build/device指纹、脱敏artifact路径/hash、reviewer/attempt/失败/恢复。视图和HTTP200不是权限证明；归档查询必须实际验证保存/删除边界。数据库核查和破坏性fixture仅限明确批准隔离范围。

Failure→Recovery→Retry→Next State持久化；classified infra同指纹最多2重试，未知一次只读reconcile后仍未知则BLOCKED。超时查活handle并轮询，不盲重启，不自动重放不确定消息/迁移/导出/外部写。原始失败和恢复日志保留。

保存 `control/input-manifest.json`、`scope-inventory.json`、`inherited-requirements.json`、`leases.json`、`acceptance.tsv`、`state.json`、`recovery.jsonl`、不可变manifest、`final/verdict.json` 和 `final/report.md`。本run scope集合启动冻结，新增发现纳入版本；52协调ID不限制实际功能规模。

整体PASS须全部原required IDs和发现的required功能/平台闭合，无未解决P0/P1、无placeholder/unmapped/required skip、独立只读审查通过。待用户视觉评审及外部联调分别保留，不声称生产/法律合规认证。缺项逐ID给NEXT_UNLOCK，不降低产品profile偷偷换整体PASS。

提交全产品F11/F15/F16及客服租户关联证据索引，明确其未覆盖平台/角色和候选条件。集成须main独占租约、重采漂移并复验；只清理本run已确认集成的worktree/分支及获批准资源。无push或生产部署。执行者在验证后按独立功能本地commit，命令级身份 `leeyi <leeyisoft@qq.com>`，不混入外来改动。
