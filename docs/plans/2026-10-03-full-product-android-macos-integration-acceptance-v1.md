# IMBoy 全产品 Android / macOS 集成验收计划 V1

> **For Claude / GLM / Codex:** 按任务卡分步执行；参考 executing-plans 工作流。本文是规划合同，不表示测试已经执行。

**Goal:** 在隔离后端上，以 Android 真机和 macOS App 完成当前 IMBoy 个人、社群和企业功能的三端集成验收，形成逐功能、逐平台可复核的结果。

**Architecture:** 复用现有页面台账、功能目录、规格和 runner。先对齐当前源码，再按业务域并行准备及运行离线测试；设备执行受独占租约约束，跨端旅程同时占用 Android 与 macOS。后台使用真实后端，不用 mock 替代最终集成证据。

**Tech Stack:** Erlang/OTP、PostgreSQL、Garage、Flutter integration_test、ADB、macOS App、React/Bun、真实后端 Playwright、现有 auto_test.py。

日期：2026-10-03，Asia/Shanghai。状态：`PLAN_READY_SCOPE_ASSUMPTIONS_PENDING`。本轮仅制作计划，没有启动设备、创建测试账号或运行产品写入。

## 1. 需求解释及验收边界

用户要求的是“全产品功能测试”，不是只测首页，也不是只跑已有自动化文件。每个当前功能至少对应具体用户动作、可观察结果和平台结论；跨端通信必须实际完成发送端 UI → 后端 → 接收端 UI → 重进/重启后持久化读取。

“会话消息通过管理”暂解释为：会话列表管理、消息操作、通知管理、管理后台消息治理。若用户补充其他含义，在 F00 加入功能映射，不能删除既定范围。

Android 真机与 macOS App 是必需平台；不使用 Android/iOS 模拟器。后台浏览器是第三个验证界面，不能替代两个 App 平台。无 iOS 验收承诺。

默认承接已有约定：客户 OA 协议/SSO 联调、当前关闭的 E2EE 正向加密旅程不进入本轮必过集合。**该选择待用户回复确认**，F00 才冻结；用户如纳入任一项，须扩充对应独立任务和资源，不能在旧集合里写通过。工作台始终可见、禁用功能入口行为、历史密钥清理造成的登录退出影响仍属于基础验收。

资金域：本地合成余额、红包、转账、订单和付费频道的状态及权限必须测试；真实扣款/提现/第三方支付回调正式联调另列 `BLOCKED_EXTERNAL`。联系人绑定、注册验证码、邮件/短信使用本地合成身份与隔离接收器；外向动作不因本计划自动获得授权。

若某项当前未实现，记录 `GAP_IMPLEMENTATION`，不得删除或改为 PASS；若当前产品配置故意关闭，必须验证入口缺席和后端拒绝，并记录关闭配置及来源。测试当前可用功能和交付用户所需“全量功能”分别汇总，不能用“全部关闭”换取全产品通过。

## 2. 旧计划值得如何参考

| 资料 | 当前可复用价值 | 使用限制 |
|---|---|---|
| imboyapp/test/auto_test/README.md 及模块页面表 | 164 份页面文档，1846 条历史功能行；保留功能描述、失败场景、页面入口 | 旧“已通过”不继承；部分仅取消分支、数据链或静态检查，不能当完整操作 PASS |
| test/auto_test/functional_inventory.json | 1872 项生成清单、173 页记录，可辅助查漏 | 有 GENERATED_ORACLE_PLACEHOLDER；必须逐项换成真实行为判据 |
| test/auto_test/catalog.json | 150 个 target 条目、188 个页面候选，可做源码/依赖映射 | enabled、mock、API-only、来源映射必须重审；数量不是覆盖率 |
| test/auto_test/domain_coverage.json | 44 个能力定义，含企业、群、工作区与加密边界 | 域未完整覆盖朋友圈、全部个人子页等；不能独立充当产品总清单 |
| test/demo_flow/、integration_test/demo_flow/ | 单聊、群、频道、好友、账号的跨页旅程与驱动 | 生产/历史账号、旧 schema、静态直种会话、软探针需改成本轮隔离夹具及真实 UI |
| docs/plans/2026-09-18-auto-test-framework-hardening-plan.md | 目录核对、唯一 target、状态、证据、恢复、最终报告机制 | 是框架计划，不能替代功能验收；按当前代码检查实际完成度 |
| Backend 非 OA 计划与修正记录 | 组织图、四栏布局、撤权、资料归属、真实权限 oracle | 历史完成声明已撤销；本轮继承缺项而不是继承 PASS |
| Admin tests/e2e/enterprise-ent-int01/、customer-service-real/、customer-service-p2/ | 真实后台/API、企业群频道过滤、客服多上下文取证 | 本轮重新配置隔离端口/账号；mock spec 只能用于 L0/L1 |

本计划配套 `*-coverage-seed.tsv` 已逐行导入全部 1846 条旧功能描述，全部初始化为 `MAPPING_REQUIRED`。其中 22 行含额外管道分隔符，标为 `AMBIGUOUS_PIPE_REVIEW_REQUIRED`，F00 必须人工核对原行，不猜测字段。未抄入旧账户/密码或历史通过状态。

配套 `*-input-manifest.json` 保存 164 份输入文档指纹、三仓调查 HEAD 和实际 188 个 `*_page.dart` 源码候选。它是调查快照，不是最终候选，也不证明每个候选都是独立可访问页面。F00 要将新增模块页面、弹层、子页、API 能力和后台路由一并对齐。

## 3. 完整覆盖集合

以下是任务分组；**是否完成以逐功能覆盖表为准，不以本表打勾为准**。

| 卡 | 必须覆盖的动作与结果 | 平台/角色 |
|---|---|---|
| F02 账号/好友 | 首次启动、许可与权限、注册/登录/密码恢复/退出重登/同账号多设备；添加申请、接受拒绝撤回、备注标签、好友删除拉黑、资料访问 | Android+macOS；好友/非好友/被拉黑 |
| F03 单聊/会话/通知 | 文本/长文/表情、图片视频语音文件位置及当前支持类型；复制引用转发收藏删除撤回搜索；时间与发送失败重试、ACK/历史、未读清零、置顶/免打扰、消息草稿、分页、重启恢复、前后台通知及深链 | 两平台发送方向都测；在线/离线/重连 |
| F04 群通信/治理 | 普通/公开/私密群创建、加入申请/邀请/二维码/面对面；全部角色权限、成员增删与分页、群头像/名称/备注/我的昵称、公告、禁言、管理员/群主移交、举报、退群解散、群会话完整消息操作 | 群主、管理员、成员、非成员/已移除者；3 名以上合成成员 |
| F05 群应用全功能 | 群文件上传下载音频预览与权限；相册/照片浏览删除；任务分配状态及详情；投票创建/参与/结果/截止；日程创建编辑参与提醒；分类/标签/所有九宫格入口及当前实际应用 | 两平台 CRUD+跨成员可见性；过期/取消/空数据 |
| F06 朋友圈 | 文字/图片/视频发布编辑或删除、列表与详情分页、点赞取消/评论回复删除、@选择、好友选择与可见范围、通知跳转、屏蔽与撤权、视频播放、发布失败恢复 | 作者/好友/非好友/被排除者；两平台 |
| F07 频道 | 创建修改头像资料、发现筛选搜索、公开/私密/付费、订阅退订、邀请审批、管理员/订阅者、图文与当前内容类型发布、文章详情、评论治理、订单及订阅状态、删除/撤权与新读取 | 创建者/管理员/订阅者/未订阅者；双端内容传播 |
| F08 我及所有子页 | 个人资料头像昵称性别地区、二维码；收藏列表详情；黑名单；设置/语言/主题/最大字号/缓存与存储；账号安全/设备列表与踢出/改密/绑定/反馈及详情/关于/升级；注销完整本地合成账号流程 | Android+macOS；改动后重进、重启及服务端回读 |
| F09 组织/架构 | 创建、邀请申请审批、加入拒绝撤回退出、成员/部门 CRUD、角色转移、停用恢复；成员/组织架构切换，图形缩放拖动fit/节点路径/宽深树、分页搜索、403/404/重试、跨账号晚到响应、解散 | 两企业+个人；owner/admin/member/outsider/revoked |
| F10 工作区/项目 | 默认工作区、创建加入邀请退出、切换/成员管理/品牌设置/工作台空态；项目成员、频道、任务、里程碑、洞察及所有当前子页 | 工作区owner/member/非成员；Android+macOS |
| F11 企业群/频道/资料 | 企业群建立与归属、成员准入、撤权/退企业后访问；企业频道发布订阅、跨工作区不可见；文档资料上传/移动/下载/删除、所有者和归属、历史消息附件授权 | personal↔orgA↔orgB↔workspace；App+Admin+API |
| F12 客服/后台治理 | 合格坐席入口、上线下线、队列接单并发、转接结束、历史记录与附件；Widget访客→坐席；组织后台成员部门/群频道/资料治理、RBAC、列表筛选分页、审计与授权失效 | Android+macOS坐席；后台真实浏览器/合成访客 |
| F13 实时音视频/直播 | P2P及群RTC当前启用流程：呼叫接听拒绝取消/挂断/忙线/权限拒绝、相机麦克风切换、入离房/主持与成员；直播入口及当前功能 | Android↔macOS；双向声音/画面；禁用入口也验证 |
| F14 其他功能 | 全局/消息/网页搜索、扫一扫各分流/二维码登录；钱包/红包/转账/本地订单、当前bot/AI入口、定位发现附近、升级/反馈/外链/所有剩余源页面 | 平台不支持必须有来源与预期降级；资金与外部服务单列 |
| F15 后端/API边界 | 当前用户/Admin/Internal所有API和WS能力清单；认证、权限、租户边界、TSID、游标、幂等、错误码、上传/下载授权、撤权与审计；客户端消费结果 | 真后端/隔离PG；OA按范围分类，不能按路由缺失跳过 |
| F16 全页导航/体验 | personal「消息/通讯录/频道/我」，enterprise「消息/通讯录/工作台/我」；所有页与弹层返回/深链/空态/错误态/加载、暗色、最大字号、窄屏长文、多语言、桌面布局及触控 | Android实际屏幕+macOS实际窗口；功能与人工视觉分别记录 |

“群所有功能”采用 F04+F05+F11 联合集合，旧 26 个群页面一项不漏；“频道所有功能”采用 F07+F11+F14订单联合集合；“我及子页”采用 F08+F14+F02恢复身份集合。不可由一个入口截图替代几十个子功能。

## 4. 基础环境与夹具合同

1. 用三仓已提交的候选快照。现有 Backend 未跟踪 `docs/design/2026-10-03-e2ee-production-excellence/` 属其他工作，保护不暂存/清理。执行前重新采样三仓；版本漂移是待重新绑定的输入，不是自动错误归因。
2. 隔离 PostgreSQL 数据库、Garage bucket 前缀、HTTP/WS端口、Admin本地入口及测试应用配置；没有生产用户、旧私人账号、真实联系人。每个业务卡使用 `run_id/card/attempt` 数据命名空间；计数只查该命名空间。
3. Android 必须通过 ADB 序列号及设备属性证明是已连接真机；macOS 记录硬件/系统/App进程和窗口证据。可优先 `adb reverse` 将本机隔离 HTTP/WS映射给Android，并记录应用最终有效地址；仅做适用于当前TLS配置的映射，不跳过证书验证。
4. 初始至少：合成人类A/B/C、好友关系两方向；企业A/B与空企业C；各自default及其他workspace；owner/admin/member/outsider/revoked；普通群/企业群；普通频道/企业频道；坐席/主管/访客。身份通过稳定 UID 与角色 API确认，不以名称或邮箱证明。
5. 群至少3成员；成员/部门/文件/消息/频道数据超过当前limit两页以上；组织图至少103根部门、3层深；空数据、长名称、特殊字符、超大TSID、重复请求、授权过期分别有夹具。
6. UI流必须由真实 UI发起业务操作；API/SQL可以准备关系、观察结果和清理夹具，但不得在“发送消息”之前直接插入消息来假装用户发送成功。
7. A/B双端账号在跨端测试中固定，其他角色通过同一设备有序换号或隔离后台/API上下文参加。两台端点够验证基本双端链路，但多人RTC并发、同平台两台硬件、多厂商推送不能声称已覆盖；追加端点缺失的格子必须 `BLOCKED_ENV`。
8. 需要短信/支付/AI/地图/推送服务时，先列适配层本地测试与真实提供商验证的分界。Android前台本地通知成功不能代替杀进程后的系统推送；APNs不在本轮平台内。

## 5. 可并行策略与所有权

测试准备、后端域测试、Admin浏览器验证可以并行；**同一个 Android 设备、macOS App配置/账号、Flutter工程构建目录不能同时跑多个写入/构建任务**。

资源租约至少包含 `resource,owner,run_id,card,device_id,namespace,backend_port,started_at,expires_at,last_checkpoint`。跨端消息/RTC任务一次原子申请两个端点，未得到全部资源不部分占用。过期租约须先核实进程已结束，不能按时间直接抢设备。

| 波次 | 并行工作 | 必须串行的部分 |
|---|---|---|
| W0 | F00范围对齐与资源调查 | F00冻结required集合/输入 |
| W1 | F01环境/runner、各领域测试设计与旧target审查 | 公共fixture、catalog/schema及路由映射由F01独占 |
| W2 | F02–F15各自L0/L1、测试开发与不同namespace后台/API验证 | 同库全局feature/授权策略变更须租约；不足隔离能力则排队 |
| W3 | 一个Android单端卡+一个macOS单端卡+一个Admin域卡 | Flutter共同checkout存在startup lock，改用独立只读候选worktree和build目录；不能任意改系统Flutter全局缓存 |
| W4 | F03/F04/F06/F07/F11/F12/F13跨端旅程逐卡排队 | 每个旅程占用两个端点；其他worker继续独立L0/L1或后台 |
| W5 | F16视觉/全页收敛、F15契约残缺补验 | 用户视觉评审结果不能由worker自签 |
| W6 | F17唯一全量终审与最终候选回归 | 不再修改候选；发现bug回到责任卡并重冻结 |

建议3–4名执行者：协调/环境1人；消息群域1人；社交频道个人域1人；企业后台/API域1人。F13可由消息执行者承担。不要给每个worker派全量EUnit、全App和全后台；全量只由F17跑一次最终门。

每卡独占 `imboyapp/integration_test/full_acceptance/fNN_*/`、对应专属测试规格/台账patch和 `RUN_ROOT/FNN/`。这些是**待创建路径**。业务源码默认只读；缺陷先形成路径级修复卡并向协调者取得该文件独占租约，不是重复询问用户是否允许本地修复。共享router、tokens、i18n、API client、fixture、runner/catalog只由协调者或其指定唯一owner改动。

## 6. 任务卡与逐项验收

每个操作小步骤尽量2–5分钟；一个完整业务旅程可更长。通用步骤：①核对当前source/旧功能行；②写业务用例与明确oracle；③复用/补齐测试；④跑L0/L1；⑤取得设备租约跑L2；⑥对失败作最小修复、独立review、受影响回归；⑦填写逐平台证据；⑧按一个功能/小缺陷本地commit。禁止按“第几轮测试”混合不同业务变更。

### F00 — 当前范围与旧表核对（Owner：协调者）

前置：无；只写本计划配套coverage记录及调查输出，不改产品。

- 核对1846旧功能行、22歧义行、1872生成项、实际页面候选及后台所有路由/API/WS；以函数/入口/角色与当前实现为依据补漏和合并重复。
- 自动生成占位oracle的项逐一补成用户动作→界面状态→后端事实→恢复步骤；关闭/删除/平台不支持项须有来源定位、配置指纹及理由。
- 将F04+F05和F03+F13等共同分组拆成**唯一主owner**，允许secondary卡关联，不能重复计数。
- 确认OA/E2EE边界；将本地资金功能与外部资金动作分开；生成平台required集合。本文不把“未回复”当成扩大范围授权。
- Acceptance `F00-A01`：所有输入功能有去向，新增可达功能被登记；`F00-A02`：每个required功能有Android/macOS的具体case或经批准的NOT_APPLICABLE；`F00-A03`：旧PASS全部清零，placeholder=0、unmapped=0、歧义行=0。

### F01 — 隔离环境、夹具、runner与证据（Owner：协调/基础设施）

前置：F00；独占App scripts/auto_test_lib必要改动、catalog/规格schema/公共fixture；后端测试启动/夹具脚本必要改动，不改部署生产入口。

- 复用 `scripts/auto_test.py` 的effective config、machine events、requirements、state/report；先对现有代码作资格检查。
- **当前 `auto_test_device_shard.py` 是无凭证、无dart-define注入的离线shard**，不能直接承载带账号的全产品在线旅程。统一通过effective_target_config扩充现有runner，不写第二套“全量框架”。不把offline fixture指纹复用给在线测试。
- 每个target配置注入APP/API/WS/合成身份/feature profile、设备/namespace/期望UID；私密参数通过仓外权限受限配置读取，日志只写配置指纹。
- 验证无设备/错平台/假UID/缺配置/失败命令/skip/污染namespace时均不能绿灯；每个target只执行一次，aggregator all_tests与leaf不能双跑。
- Acceptance `F01-A01`：两平台真实设备及有效端点；`F01-A02`：夹具seed/reset幂等且不触及外部数据；`F01-A03`：runner拒绝假绿/配置漂移，脱敏证据与租约恢复有效。

### F02–F14 — 业务任务卡（Owner：各领域执行者）

前置：F00+A01/A02/A03、F01+A01/A02/A03；每卡执行第3节完整功能集合及覆盖表分配的所有行。

- 复用现有精确入口：F02 `integration_test/demo_flow/account_flow_test.dart`、`dual_account_message_flow_test.dart`及passport目录（不存在或含生产写入时在manifest标明并迁移）；F03 chat/conversation及demo_flow单聊；F04/F05 demo_flow/group_*；F06 moment及朋友圈demo；F07 channel_creator_flow与channel现有目录；F08 mine/settings/personal_info；F09 enterprise图与导航权限、organization；F10 workspace；F11 group_organization_local_api_flow与enterprise；F12 customer_service及Admin real suites；F13 chat/p2p/rtc；F14其余目录。**文件是否存在、是否真正UI、是否绑定当前后端由F00核验，名称不是可执行保证。**
- 每卡 `FNN-A01`：当前领域L0/L1+真实后端行为、具体oracle及所有功能映射通过；`FNN-A02`：required Android格子全部PASS；`FNN-A03`：required macOS格子全部PASS。多个功能可以共用一段旅程，但证据必须能定位各动作。
- F03、F04、F06、F07、F11、F12、F13加 `FNN-A04`：双端/跨角色真实旅程；F03/F04必须双向发送，至少一条离线补发/重连及重启后历史回读；F11必须第三身份/企业不可见；F12必须真实访客到坐席及并发队列唯一接单；F13必须真实双向媒体，不能只看200或房间计数。
- F13的多人/中继模式无资源时对应格子BLOCKED，不以API或视频缩略图代替。TURN模式需实际relay传输证据，且与同会话客户端/后端日志关联。
- 每卡交付case-map、target-list、fixture字段说明、逐平台结果、证据manifest、发现/修复/复验的缺陷列表、后续阻塞原因；单卡输出不能改变总集合或全局结论。

### F15 — API/WS与后台跨域权限全集（Owner：Backend/Admin域）

前置：F00/F01；App源码只读；独占域专属Backend行为tests和Admin real tests，公共fixture由F01协调。

- 枚举API v1、Admin、Internal v1和WS动作，记录消费者/权限/正向/负向/幂等/分页/恢复与启用profile；不能只验证OpenAPI文件存在或bundle是最新。
- App功能对应API必须与设备旅程绑定；其他API使用真实请求+PG事实/审计作为oracle。平台超管/组织管理员/普通成员/跨组织/过期撤销会话分别验证。
- 后台真实UI验证数据治理，mock测试只算L0/L1。消息内容不可见的安全策略按实际配置核对，不能为“查消息”绕开授权。
- `F15-A01`：精确操作集合与scope分类完整；`F15-A02`：required API/WS正负路径PASS；`F15-A03`：Admin真实UI/权限/撤权及至少一条App消费治理结果PASS。

### F16 — 所有页面与体验矩阵（Owner：设备协调/UX）

前置：F00/F01；各页功能可先做，跨业务视觉终审待F02–F14终态。

- 覆盖页/子页/弹层/深链与返回；对每页基础状态检查，风险页追加暗色/最大字号/长名称/错误/空列表/窄屏等。UI截图记录实际窗口和设备字号；测试setSurfaceSize不等于macOS真实窗口操作。
- 验证企业通讯录「成员｜组织架构」位置、pan两个轴、缩放比例、fit根节点可见、103节点完整授权及连线、展开与成员路径、断网恢复与撤权时旧图清除。
- `F16-A01`：全部required页面Android导航及状态；`F16-A02`：macOS导航及状态；`F16-A03`：人工UX意见逐项记录。无人评审时记录`PENDING_USER_VISUAL_REVIEW`，机器测试不能给用户代签。

### F17 — 唯一最终候选全量门（Owner：独立终审）

前置：所有required卡/平台格子PASS，或存在阻塞时仍执行always-run最终报告；只读，不替worker改ledger。

- 合并本地已审查修复，冻结三仓最终SHA、依赖/feature/runtime/runner/fixture/build；业务修复后只对受影响域重跑L0/L1，最终候选重跑全部required L2，不跨SHA借用真机结果。
- 协调者跑一次全局单元/静态/build门。Backend VM全局TSID套件与专项PG套件按Makefile现有隔离规则执行并逐项记录，不误把被排除套件看成已通过。全量EUnit要求冻结候选上两次连续exit0；专属VM单跑每个被排除required套件。多worker不得重复全量。
- 终审重新计算 required case/platform/Acceptance-ID exact set；检查每格SHA、命令、exit、executed>0、skipped=0、实际oracle、artifact hash、build/device/backend关联及清理残留，不能信任worker自签APPROVE。
- `F17-A01`：最终全局门；`F17-A02`：完整设备与跨端required集合证据；`F17-A03`：独立结论、限制及恢复记录完整。

## 7. 命令及执行产物

调查与静态门（在各自仓执行，不能在umbrella根执行Git）：

```bash
git rev-parse --show-toplevel
git rev-parse HEAD
git status --porcelain=v1
# imboyapp
python3 scripts/auto_test.py validate --strict
python3 scripts/auto_test.py catalog --check
flutter devices --machine
adb devices -l
# imboy
python3 api/flatten_internal.py --check
# imboyadmin
bun run typecheck
bun run test
```

失败要归因并形成基础卡，不能忽略退出码进入真机执行。`catalog --check`若发现过时清单，F01修复后复验；不是自动降低覆盖。

设备单target命令模板：**F01核实该target读取哪些key后填入仓外config；此处placeholder不直接执行。**

```bash
flutter test --no-pub --machine integration_test/full_acceptance/f03_messaging/c2c_test.dart \
  -d "$ANDROID_DEVICE" --dart-define-from-file="$PRIVATE_TARGET_CONFIG"
flutter test --no-pub --machine integration_test/full_acceptance/f09_organization/organization_test.dart \
  -d macos --dart-define-from-file="$PRIVATE_TARGET_CONFIG"
```

两个跨端参与者须runner屏障协调，不能把两个顺序独立进程当消息互通；参与者、测试ID、message_id和fixture前缀联合关联。后台只用经过重配的真实backend config，不能盲跑带固定历史端口或生产URL的旧命令。当前已有可用real config例子是 `playwright.customer-service-real.config.ts`；最终按F12/F15输入manifest指定。

RUN_ROOT=`imboy/docs/design/2026-10-03-full-product-device-acceptance/evidence/<run_id>/`（本计划不创建运行结果）。

最低产物：baseline.json、scope.json、required-cases.tsv、case-map.tsv、leases.json、fixture-manifest.json、candidate.json、acceptance.tsv、attempts.jsonl、recovery.tsv、artifacts.json、defects.tsv、FINAL.md及HTML总报告。复用auto_test当前schema，通过明确映射附加字段，不创建彼此冲突的第二套状态。

功能行最低字段：`case_id,seed_ids,owner,source/route,action,expected_UI,backend_oracle,role,scope,platform,fixture,priority,target,required,exclusion_reason,status,evidence_ref`。

执行结果最低字段：`run_id,case_id,platform,app_sha,backend_sha,admin_sha,build_hash,plan_hash,config_hash,fixture_generation,device_id,command_ref,exit_code,executed,skipped,oracle,evidence_path,evidence_sha256,attempt,status,reason,next_action`。不适用的repo字段须注明原因，不能填任意零SHA。

原始含认证信息日志保留在仓外私有目录；仓内仅合成数据脱敏摘要与证据指纹，不保存密码/token/验证码/真实联系人。清理只针对本轮namespace，不删除用户数据或其他worker进程。

## 8. 状态、恢复与停止规则

- 单格：`PLANNED / RUNNING / PASS / FAIL / FLAKY / BLOCKED_ENV / BLOCKED_EXTERNAL / GAP_IMPLEMENTATION / NOT_APPLICABLE`。NOT_APPLICABLE需平台/范围理由、来源和批准记录，不算PASS也不能替required缺项。
- FAIL不自动重试换绿；先保留原失败、定位root cause、修复、review，再新attempt复验。只有已分类基础设施故障可在同SHA/config/fixture上最多重试2次；仍失败转FLAKY/BLOCKED，不能丢旧日志。
- 断开设备/中断线程：保存current case与lease、last successful checkpoint；恢复先reconcile进程、设备、config/候选和fixture状态。发送/建群/付款等结果不确定时按业务ID核对，禁止直接重复外部写入。
- 默认单UI动作等待30秒，网络状态60秒，单旅程15分钟，复杂RTC/恢复30分钟；超时记录失败并安全释放自己的资源。不得用固定sleep代替状态oracle。
- 发现跨租户泄漏、身份错误、生产地址、非本轮数据写入或真实资金动作：立即停止该卡及共享资源相关卡，记录证据；不清理现场，协调者确认边界后恢复。
- 无环境仅阻塞依赖它的卡，离线卡继续；缺required条件不能skip。每个无关卡进度保留。
- 本地修复验证后按独立功能/缺陷commit，Git身份leeyi；不push、不部署、不正式通知第三方。本计划不授权生产迁移或联系方式设置。

## 9. 完成质量及工期估算

最终分别输出 `LOCAL_REGRESSION`、`ANDROID_DEVICE`、`MACOS_DEVICE`、`CROSS_DEVICE_JOURNEYS`、`ADMIN_REAL_BACKEND`、`UX_USER_REVIEW`、`EXTERNAL_INTEGRATIONS`，不合并成虚假的上线完成。

`FULL_PRODUCT_LOCAL_DEVICE_ACCEPTANCE_PASS`仅在required功能/平台集合及F00–F17 required IDs全部通过、无未解决P0/P1、无unmapped/placeholder/required skip、候选不漂移时成立。存在排除项必须写明产品profile和排除边界；仅某profile通过叫`PROFILE_LOCAL_DEVICE_ACCEPTANCE_PASS`。视觉待用户确认时保留独立待评审状态，不谎称用户满意。

工期为资源估算，非承诺：F00/F01约1–2工作日；领域用例补齐与L0/L1约2–4工作日并行；两端真实旅程/全页走查约2–4工作日（单Android+单macOS是主要串行瓶颈）；终审约0.5–1日。3–4名执行者首轮约5–10工作日，较大产品缺陷另计；F00完成后按实测target耗时、未映射数量和required格子重新估算。不能用“1846项旧PASS”估算一天内全量完成。

提效顺序：先核心单聊/群消息+组织边界P0，尽早暴露阻断；再并行子功能；后台/API与设备排队同时推进；同target去重、统一fixture、只跑域回归，最后一次全局门。P0早期通过表示可继续测试，不表示全部功能已验收。

## 10. 执行交接契约

执行者先完成F00，向协调者提交明确required集合与资源表，再开始F01。不得从旧README挑绿项、静默跳过缺环境的项目或自行增加第三方访问授权。

每卡完成提交：独占路径变更、候选SHA、Acceptance逐ID状态、command/exit/oracle/artifact绑定、失败与恢复历史、未解决问题。卡结束时回收自己的租约；协作者不覆盖彼此WIP。独立终审只有读取权，结果不满足就明确PARTIAL/BLOCKED，并指回缺失case。

目前交付的是计划、旧功能覆盖种子和输入manifest，全部为PLANNED；不包含真机PASS、执行时间承诺或生产可用结论。
