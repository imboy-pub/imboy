> FROZEN / SUPERSEDED_FOR_EXECUTION（2026-10-11）：后续执行统一使用 [统一收口计划](./2026-10-11-unified-delivery-closeout/plan-v1.md)。本页原状态与 PASS 属于历史声明，不作为现行验收；冻结前原文与摘要见新计划 source-contracts。旧 .sha256 仅对应冻结前正文，不再用于启动旧计划。

# 一期交付收敛与 Core IM 质量计划 / Phase 1 delivery and Core IM quality

日期：2026-10-07；版本：v1；状态：**PHASE1_CLOSURE_PARTIAL（2026-10-11 收口）**——台账 37 行：31 PASS 系 + 3 PASS_WITH_NOTES（AC23/P1-LOC-02/P1-MOM-01）+ 2 如实 PARTIAL（AC15=G1/G2 spec 未排程、AC28=hird CLI 环境缺件）+ 4 NOT_EXECUTED（AC31-34 拍板不跑）；目标 PHASE1_LOCAL_CANDIDATE_PASS 未达成，verdict 见 `~/.zcode-runs/phase1-closure-20261010T060003Z/final/`。
本文件是本次代码抽查、两项静态门实跑后形成的实施合同，不是运行验收或生产承诺。本轮只新增本计划，未修业务、未清理文件、未改配置、未启动数据库/设备/外部联调。

## 1. 结论与目标 / Decision

一期聚焦 Core IM、人工客服坐席、企业组织/工作区、OA 对接协议、IM 嵌入集成、消息推送及必要管理运维。二期 RTC、live_room、Bot、AI 助手、Agent Hub、MCP、E2EE 保留实现与测试，但一期不可发现、不可调用、不可被旧缓存或配置重新打开。三期协同文档、External Channel Adapter、Open Platform 仅作候选，不承诺实现，也不建设占位框架。

建议采用一个受版本控制的 `phase1_delivery` 产品 profile；它是本计划拟新增的标识，目前不存在。开发完整版与交付版共用源码，避免三端各自维护一份客户分叉。功能关闭不等于删除源码，仓库整洁也不等于文件越少越好。

**工作量初判为中等，不是几天小修**：一期必要收口约 18–31 人日；在此之上取得有证据的 Core IM ≥9.9，再预留 5–9 人日及至少 24 小时稳定性观察。两者合计 23–40 人日，不包含客户响应、签名/推送资质等待和新发现的重大迁移工作。单人串行通常约 5–8 工作周；不是报价或交期承诺。前 2 人日完成 Q00–Q01 后重新估算；若必须重做 C2G delivery schema、迁移历史 E2EE 数据或改动受保护原生文件，单独重估，不暗中扩范围。

9.9 是本文件明确限定场景的内部工程评分，不是 99.9% SLA，也不是多节点、所有平台和所有二期功能的成熟度。当前不提供重新估算的精确得分；旧计划的 8.5/10 没有可复算量表，不能当作起点线性加分。

## 2. 当前取证 / Current evidence

### 2.1 版本与保护范围

| 仓库 | 本次读取 HEAD | 工作区状态 |
|---|---|---|
| imboy | `327fddc798c8dc91a08edb7c72e513fe668c42a5` | 有既存未跟踪文档，保留 |
| imboyapp | `7c9c8a45cdd4f7b970b125acb1d1f644da990151` | 本次采样干净 |
| imboyadmin | `965e64e94935f5c2c43887670dd758b8a3c680e3` | 本次采样干净 |

保护的既存未跟踪路径：`imboy/docs/business/imboy-open-source-productization-strategy-v1.md`、`imboy/docs/design/2026-10-04-seat-console-parallel-acceptance/`、`imboy/docs/design/2026-10-06-enterprise-organization-gap/`。既存 ignored 文件同样不属于本计划清理权限。

来源计划：`docs/plans/2026-10-06-imboy-productization-hardening-plan-v1.md`，完整分段阅读；它被 `.gitignore:121` 忽略。新计划放在未被忽略的 `docs/design/`，不覆盖旧计划，也不强行把其证据目录加入 Git。

### 2.2 已核实差距与证据层级

| 编号 | 分类 | 现场证据 | 结论/任务 |
|---|---|---|---|
| F01 | GAP_IMPLEMENTATION | 实跑 `python3 scripts/check_product_feature_cross_repo.py`，exit=1，报告 `imboyapp/android/app/src/debug/AndroidManifest.xml` stale | 三端生成物门当前不通过；只确认首个报告点，非宣称仅一处漂移。Q01 |
| F02 | GAP_IMPLEMENTATION | `config/product-feature-manifest.json` 为 full-selected，选中 e2ee、bot_webhook、project 等；`imboy_profile_preset:default_features/0` 默认全 true；enterprise 预设只是 e2ee_mode=disabled | 现有 enterprise 预设不等于一期交付 profile。Q02 |
| F03 | GAP_IMPLEMENTATION | App `app_feature_registry.dart:isEnabled/_readFlag` 缺键默认 true，本地仅 live_room 硬关；`feature_keys.dart` 明说 e2ee 键只管 UI；Admin `features.ts:isAdminFeatureEnabled` 编译特性缺远端 false 时放行 | 缓存/远端配置不能作为交付关闭边界。Q02–Q03 |
| F04 | GAP_ACCEPTANCE | `feature_gate_middleware:execute/2` 只拦 required_feature；`imboy_feature:route_feature/3` 仅覆盖部分域；router 存在 AI/Bot/MCP/live_room 入口 | 需清点路由、WS、worker、启动入口；不能凭菜单隐藏声称全关闭，尚未逐接口实测。Q02 |
| F05 | CONTRACT_CONFLICT | `msg_ack_logic:client_ack/4` 的 C2G 走 per-uid ACK；`msg_c2g_timeline_repo:client_ack/2` 设置 client_ack；App `_handleMessageReadSync` 固定 c2c | 旧 T2.1 在送达 ACK 后发已读同步不成立，先冻结送达与阅读双合同。Q04 |
| F06 | GAP_IMPLEMENTATION | `msg_operation_ds:ack_c2g_timeline/2` 将 repo `{error,_}` 转为 ok | C2G 数据库写失败被掩盖；需故障回归检验重试和指标。Q04 |
| F07 | GAP_ACCEPTANCE | `group_event_handler:handle_event/2` 仅接 member_added/member_removed，其余丢弃；group_logic/group_ds 还存在直接业务路径 | 事件分支空载已确认，但不能据此断言用户完全收不到群通知。先追踪既有直发，避免双发。Q05 |
| F08 | CONTRACT_CONFLICT | `enterprise_webhook_logic.erl` 明确复用 bot_delivery、bot_webhook_delivery_worker、guard、sender | 一期必须保留 OA 投递基础设施；Bot 产品关闭不得粗暴裁掉全部 bot_*。Q02/Q08 |
| F09 | GAP_ACCEPTANCE | 现有 EO final/verdict.json 记录 PARTIAL_LOCAL_CANDIDATE、48 卡中 35 PASS/13 open，绑定 Backend 35b33de6/App 4a1df320 | 这是旧候选自述，当前 SHA 不同；原始日志/哈希未在本轮独立复算，不自动继承 PASS。Q00/Q07 |
| F10 | GAP_ACCEPTANCE | 实跑 Admin `bun run deadcode` exit=1：15 unused files、158 unused exports、1 unresolved import、5 duplicate exports 等 | 包含 seatMain、Widget main、Playwright 配置等可能误报；先修入口建模再删代码。Q10 |
| F11 | GAP_ACCEPTANCE | tracked 文件/Markdown：Backend 5801/615、App 5248/363、Admin 925/113；文件名 .bak/.orig/.rej/crash.dump 候选均为 0 | 不能沿用“Git 仓有 8 个快照垃圾”的说法；本地 ignored 配置另算。Q10/Q11 |
| F12 | GAP_ACCEPTANCE | src/lib 下超过 800 行候选 Backend 44/App 48/Admin 9，含 protobuf 生成物、测试；典型手写：router 2618、chat_page 2623、organization_admin_logic 2083 行 | 这是候选统计而非最终违规数；先排除生成/vendor/test，按职责和改动风险治理，不为达标机械拆分。Q10 |

统计为本次只读脚本对 `git ls-files` 的扫描；不是删除清单。Knip 结果含其它类型报告，上表只列已明确读取数量的类别。

已查看的旧企业报告路径：`docs/design/2026-10-06-enterprise-organization-gap/evidence/eo-20261006T001314Z/final/verdict.json`。其 open 包括 Admin 资产页、双账号缓存/双角色/成员治理、组织目录树、macOS 环境、OA 平台裁剪与入口契约、外部客户联调、人工视觉、生产未执行。13 项不能直接当 13 个代码 bug；Q00 必须逐项分类。

## 3. 产品分期与边界 / Scope

| 分期 | 交付能力 | 明确边界 |
|---|---|---|
| 一期必做 | 登录/会话、联系人、C2C/C2G、历史/离线/重连、多端、附件/消息治理；组织/部门/成员/工作区/RBAC；人工客服队列/认领/转接/关闭/历史/附件/SSE/Widget/坐席嵌入；OA SSO/internal API/Webhook；系统推送；必要 Admin/审计/备份恢复 | 现有组织内部 General 群与公告频道依赖必须保留；公共频道发现、付费频道不因共享底座自动进入一期 |
| 二期保留关闭 | RTC、live_room、Bot 产品、AI 助手、Agent Hub、MCP、E2EE | UI/搜索/推荐/深链/API/WS/后台触发均纳入关闭验收；共享 OA worker 按来源能力放行 |
| 三期可取消 | 协同文档、External Channel Adapter、Open Platform | 不展示入口、不写“即将上线”承诺，不创建通用抽象/占位 CRUD；internal API 不等于 Open Platform |
| 一期默认不承诺（本计划建议） | 钱包/支付/提现/红包、付费内容、项目管理等超出明确交付链路的功能 | 默认从一期模板隐藏/关停；先核共享依赖，必要时保留内部能力。产品范围新增时单独补验收，不借“企业相关”无限扩张 |
| 条件增量 | 商品/订单引用卡片、外部商城自动拉单 | 未被本次用户明确列为必需；旧计划认定 P0 商业缺口证据不足。先交付文本/附件/外部业务引用；若确认卡片为必需，增加 3–5 人日与独立合同，未验收前不能声称支持 |

平台建议：Backend + Admin 实际浏览器 + Flutter Android 真机/macOS 原生为本地强制矩阵，继承组织计划的要求；iOS 原生资质/受保护目录无法通过时不得承诺 iOS 交付。交付包最终清单要写清实际平台，缩小平台范围不能在执行中静默删除旧 required 项。

一期 E2EE OFF 的目标是普通 IM 明文协议（仍用 TLS），企业托管加密、keyring、访问控制保持；**不把关闭 E2EE 理解为关闭 TLS 或企业静态加密**。优先用于新部署/测试账号。不得自动解密、降级或迁移历史密文；有历史加密会话时显式拒绝不兼容写入并提供清晰说明。`storage_mode=disabled` 存在 E2EE 硬闸，但其与归档/搜索/保留关系需 Q02 真库验证，禁止为关 E2EE 牺牲消息历史。

## 4. 实施任务卡 / Work packages

统一约定：下表每卡是一个责任单元，不代表当前授权多代理执行。实施时逐卡分配唯一 owner。每卡完成 = 对应 AC 全 PASS + 原始证据 + 受影响回归 + 代码独立审查 + 独立功能本地提交。任何 DB/设备测试须先过 Q00。本次请求只授权产出计划，不开始这些实施卡。

| 卡/优先级 | Owner、前置与路径 | 工作与交付物 | 验收 ID | 人日 |
|---|---|---|---|---|
| Q00/P0 基线及继承 | 集成负责人；无；三仓/旧 EO 与 seat 计划 | 复采 HEAD/WIP/资源；把旧 required IDs 全量映射到本计划，逐项标 GAP_IMPLEMENTATION/GAP_ACCEPTANCE/CONTRACT_CONFLICT/BLOCKED_EXTERNAL；生成 requirements.json、commands.sh、fixture/端口表和候选清单 | AC00 | 0.5–1 |
| Q01/P0 生成物一致 | 集成负责人；Q00；manifest/generator 与三端生成文件 | 先对照生成器解释 AndroidManifest 漂移；由唯一生成器重建必要文件，禁止手工修改生成物；核全部 11 个产物；保护原生保留区与 foreign WIP | AC01 | 0.5–1 |
| Q02/P0 一期服务端 profile | Backend owner；Q01；imboy_profile_preset、imboy_feature、imboy_policy、policy_catalog、router、启动/worker、manifest/generator | 建 versioned phase1_delivery 白名单与域映射；缺省/未知关闭；API/WS/worker 统一授权；拒绝运行时开启被编译/交付策略禁止项。保留 OA outbox 依赖，验证 E2EE OFF 仍可归档、检索、推送 | AC02–AC04 | 2–3 |
| Q03/P0 两前端交付面 | App/Admin owner；Q02；feature registry、生成路由、App router、Admin features/FeatureRoute/sidebar、通知与深链消费 | 导航/搜索/推荐/消息加号/设置/深链/推送打开路径统一守卫；清旧缓存；缺配置、请求失败、过期全开缓存不露二期入口；普通用户与管理员都测 | AC05–AC06 | 1–2 |
| Q04/P0 C2G 送达/已读 | IM owner；Q00；msg_ack_logic/msg_operation_ds/msg_c2g_timeline_repo、消息拉取/归档、App message_s2c/chat 状态 | 先证伪双端离线丢消息、未打开聊天即已读；区分 per-device delivery 与 per-user read cursor；传播 DB 错误，游标单调、去重、乱序安全；必要 schema 变更先更新设计与迁移号，不沿用旧“不准改 schema”限制 | AC07–AC09 | 3–5 |
| Q05/P1 群生命周期 | IM owner；Q04；group_logic/group_ds/group_event_handler 与 App S2C | 转让/解散/退出/踢人真实调用链盘点；事务成功后单一路径通知，失败不通知；离线可补状态；解散前确定合法接收者；如已有直发有效，仅删除/标注无用双轨，不机械追加 | AC10 | 1–2 |
| Q06/P0 消息附件与登录 | IM/安全 owner；Q04/Q05；认证、staging/worker/archive、attach/asset authorization、App offline/attachment | 补 SERVER_ACK 持久化边界、重复/乱序/断线/分页、撤回编辑、越权附件与 token 吊销回归；只修验证出的缺陷，不另建传输层 | AC11–AC14 | 2–3 |
| Q07/P0 企业与坐席闭环 | 企业负责人；Q03/Q06；organization/workspace、enterprise_business/customer_service、Admin seat/Widget、App enterprise | 继承旧 EO/seat required 集合；收口缓存双账号、双组织、owner/admin/member、停用/离岗/撤权、资产；跑真实 Widget→坐席→DB→回显，不用 mock 替代 | AC15–AC18 | 2–4 |
| Q08/P0 OA/IM 集成协议 | 集成负责人；Q02/Q07；api/internal、enterprise_oa_sso、enterprise_internal、enterprise_webhook、交付说明 | 本地独立调用方使用正式协议跑鉴权/scope/幂等/审计/SSO/Webhook 重试死信重放；保留 origin_application_id；OA delegated Human 为企业管理消息；交付最小调用示例、错误码、版本与重试合同 | AC19–AC21 | 1–2 |
| Q09/P0 消息推送 | 推送 owner；Q03/Q06；push_notification_ds、token/DND、App push路由 | 按实际 Android/macOS/iOS 平台和 provider 做资格矩阵；本地替身测失败分类、过期 token、重试、免打扰；经明确授权后向自有测试设备验证后台/杀进程/点开/撤权，绝不向真实用户试推 | AC22–AC23 | 1–2 |
| Q10/P1 代码卫生 | 各仓 owner；Q02–Q09 行为稳定；Admin knip配置、变更涉及源/测试/依赖 | 先建多入口模型（seat/widget/Playwright/生成物）；每个候选以编译、动态注册、构建、运行引用确认；删真正废弃导出/重复实现/依赖；巨文件只拆已触达且有测试的职责；二期保留域独立标注 | AC24–AC25 | 1–2 |
| Q11/P1 文档与包装 | 文档/交付 owner；Q02/Q08/Q10；各仓 README/docs、主仓 build/deploy 文档 | 建单一文档索引、current/archive 标记；修版本/失实成熟度/过期命令/重复合同；一期交付包按白名单打包（不是复制工作区）；附依赖许可证、运行配置模板、升级回退、验收/故障排查；保留历史证据可追溯 | AC26–AC27 | 1–2 |
| Q12/P0 本地终验 | 验收 owner；Q01–Q11；候选构建/隔离环境 | 冻结 SHA+profile+依赖+fixture+runner；完整回归、真浏览器/原生矩阵、安装升级和 PG/Garage 恢复；复算 exact-set/证据哈希；分别输出本地/外部/生产状态 | AC28–AC30 | 1–2 |
| Q13/P1 9.9 冲刺 | 性能/验收 owner；Q12；压测/故障/监控与相关缺陷 | 先锁容量及延迟合同，执行 24h soak + 中断恢复/存储故障/并发压力；修复、重冻和复验；不夹带 RTC/E2EE 等二期能力 | AC31–AC34 | 5–9，另含24h观察 |

Q00–Q12 区间求和为 18–31 人日。顺序：Q00→Q01→Q02→Q03；Q04→Q05→Q06；二者汇合 Q07→Q08/Q09→Q10→Q11→Q12→Q13。独立只读准备可以重叠，但同一机器的 PG 写入、ebin、生成器、浏览器登录态、设备、全量门必须串行租用。

共享文件唯一 owner：`imboy_router.erl`/manifest/generator 由 Q02 owner 统一合入；`message_s2c.dart` Q04 owner；Admin `App.tsx`/features Q03 owner。其它任务交付最小变更契约，不能各自重写共享文件。

## 5. 验收合同 / Acceptance oracles

以下 35 个 AC（AC00–AC34）是父级清单；Q00 必须展开成带平台/角色/scope 的子格，并合并旧 required ID 的继承映射。父级 PASS 必须全部子格 PASS。NOT_RUN/SKIP/BLOCKED 不视作 PASS。

| ID | 可复现条件与必须看到的结果 |
|---|---|
| AC00 | 三仓 HEAD、状态、外来 WIP 哈希、旧 required ID 映射完整；测试库/账号/设备/端口有唯一归属。只读扫描后状态不变；继承项无静默遗漏。 |
| AC01 | generator `--check` 与跨仓一致性门退出 0；三端构建内 profile/manifest hash 匹配；从干净候选重生成无 diff。 |
| AC02 | 枚举二期七域及三期三域全部入口；一期 profile 下分别试匿名、普通用户、管理员及有效旧凭证，HTTP/WS 拒绝且 DB 无业务写入、无任务/外呼。不是只检查某个URL返回404。 |
| AC03 | 冷启动、空配置、远端全开、旧全开缓存、热重载均不能启动禁用域；AI 消息触发/RTC 信令/MCP工具/Bot任务均有负例；本地调用计数为0。OA 合法 webhook 仍可投递且无 Bot 产品发信权限。 |
| AC04 | 新环境 E2EE OFF 的文本/群聊/图片/文件、历史分页、搜索、重启后回读通过；企业 body_cipher/keyring 与 ACL 仍有效；历史密文不静默改明文；关闭前后存储与保留合同无丢失。 |
| AC05 | App Android/macOS 与 Admin 两角色巡检；禁用入口在导航、聊天操作、设置、发现、搜索均为0；直接路由/通知 payload/旧收藏链接同样拒绝，无空白页或401循环。 |
| AC06 | profile 三端不一致时有明确不兼容态，不静默全开/全关；断网或 app/features 失败时已允许的一期基本能力可按合同工作，禁用项始终关。 |
| AC07 | 同用户 A/B 两设备：A仅收到群消息且ACK，不打开聊天；B离线后上线完整取回消息，未读不得因A送达清零。真WS+数据库送达状态+两端本地消息和未读交叉核验。 |
| AC08 | A真正阅读到序号N，B最终同步至N；随后N+1到达仍未读；重复/逆序已读事件不回退游标、不误清新消息；消息/会话未读与服务端一致。 |
| AC09 | ACK落库注入失败：返回/重试按合同可观察，成功指标不得增长；恢复后可补ACK且只计一次；错误不得被ok吞掉；送达/阅读事件无相互循环。 |
| AC10 | 转让/解散/退出/踢人分别覆盖成功、事务回滚、重复请求、另一端离线；最终角色/成员/会话状态正确，通知幂等且未授权对象收不到；旧直发与事件总线不双发。 |
| AC11 | C2C/C2G在SERVER_ACK前后分别终止本run节点并恢复；已承诺接收的消息无丢失，重发client_msg_id无重复业务行/气泡；按conv_seq排序而非TSID猜顺序。 |
| AC12 | 离线积压超过单页及20页边界后能续拉完毕；网络反复切换、超时重试、后台恢复无遗漏/重复；DB计数、消息ID集合及客户端显示一致。 |
| AC13 | 撤回/编辑及历史读取保持一致；附件上传失败/重试/超限/票据过期有明确反馈；跨用户/群/组织、撤权后的原URL及新签发均被拒绝；验证实际对象字节与DB授权，非只看HTTP200。 |
| AC14 | 注销、密码变化、session epoch撤销在HTTP/WS/重连生效；双账号切换不得残留上一账号消息、组织、附件或推送目标。 |
| AC15 | 创建组织→owner→部门/邀请/成员→默认Workspace/General/公告→发消息；Admin、App与DB一致；跨组织/角色越权负例无副作用。 |
| AC16 | 离岗/停用/移除后新旧token、长连接、SSE、资产票据及本地缓存均按授权合同失效；owner转让保持唯一owner，不因重试出现无主或双主。 |
| AC17 | 真浏览器访客Widget发文本/附件→双坐席竞争认领仅一个成功→坐席回复→访客收到→转接/关闭/回访；DB消息/归属、SSE事件与两边UI一致。 |
| AC18 | 坐席嵌入错误origin、二维码重放、过期票据、权限撤销、SSE断线/超窗重同步均符合合同；产物无管理凭证/API泄漏，安装的Widget与后端资产hash配对。 |
| AC19 | 独立本地调用方完成internal API正反例：scope交集、跨组织拒绝、幂等同参回放/异参冲突、撤销凭据、限流；审计可追源且保留origin_application_id。 |
| AC20 | OA SSO一次码正常/过期/重放/redirect不符；本地OA页面实际跳转登录与退出/撤权可验证；桌面/移动平台不支持的入口按明确合同处理，不伪造SSO PASS。 |
| AC21 | 本地接收端故意失败后Webhook重试、签名校验、死信、重放通过；恶意目标/重定向被拒；Bot产品关闭不影响合法企业事件。真正客户联调另记 EXT01。 |
| AC22 | provider替身覆盖成功、超时、429/5xx、永久token无效、免打扰、重复事件；不向生产token发请求；平台/provider选择正确且日志无敏感内容。 |
| AC23 | 经用户批准的自有真机：前台/后台/杀进程各验证实际通知到达、去重、点击正确会话、账号切换/撤权不泄露；FCM/APNs/JPush按交付平台声明矩阵，不用一个provider替代其它必需项。无资质则BLOCKED_EXTERNAL。 |
| AC24 | Knip入口模型包含Admin/seat/Widget/Playwright；校准后无未解释的unused file/export/unresolved import；排除项逐条有owner、理由和复核期限，禁止整目录静默ignore。 |
| AC25 | 删除清单每项有动态/静态引用、构建与回归证据；废弃依赖确实从lock/产物消失；无新增手写>800行文件或函数>50行违规，既存巨文件登记且本次触达项不扩大；生成/vendor不手改。 |
| AC26 | 每个一期主题只有一个权威操作入口；active文档本地链接有效，示例命令在隔离环境可执行；历史文档显式归档并有替代链接。README不把二三期或历史PASS写成现行承诺。 |
| AC27 | 由tracked白名单生成交付包；不含真实配置、token、PII、crash dump、临时evidence、缓存和非交付媒体；新机器/干净目录依照README找到配置、启动、验收、恢复四个入口；供应链/许可证清单保留。 |
| AC28 | 冻结候选全量编译/静态检查/单测和三端相关集成门全部通过；required集合与ledger逐格相等，skip/cancel=0或有正式范围裁决；受影响旧用例不被新AC替代。 |
| AC29 | 空测试环境安装+升级演练；PG/Garage联合备份恢复后消息ID集合与附件哈希一致；升级失败回退可恢复服务；迁移不可逆时采用备份恢复路线，不假称down脚本即回滚。 |
| AC30 | 每个交付产物记录三仓SHA、profile/manifest、构建工具/依赖、配置脱敏hash、平台、artifact SHA256；二期实现存在不等于一期可启用；终验后变更必须重新评估并重冻。 |
| AC31 | Q13开始前冻结负载/硬件/数据量/采集工具。建议基线：单节点8vCPU/16GiB（若不可得则重定标）、1000 WS连接、稳定50条客户端消息/s、100人群10条/s持续30分钟；群广播投递另计，不混用入站/出站吞吐。 |
| AC32 | 上述测试环境下建议门槛：SERVER_ACK p95≤300ms、在线文本接收UI p95≤1s/p99≤2s；测端到端时钟误差并校准；已确认消息丢失=0、重复业务消息=0、跨scope泄露=0；报告样本量与分位值。阈值须测试前冻结，不因跑不过事后放宽。 |
| AC33 | 24小时soak中断网、节点重启、短时PG/Garage不可用各至少3次；已承诺持久化数据无丢失，恢复依赖后5分钟内回归稳态；同负载预热后至末尾RSS净增≤10%，无持续单调泄漏；连接数回落到基线±5%。 |
| AC34 | 监控可发现投递失败/队列积压/存储故障并恢复；RPO=0仅适用已确认消息的进程重启场景，不延伸为磁盘灾损；备份恢复建议RPO≤24h、RTO≤60min且必须实测。按第7节计算评分，并附剩余债务与适用范围。 |

EXT01：真实客户 OA/SSO/Webhook 联调；EXT02：客户视觉/角色旅程签收；EXT03：实际交付环境安装和恢复验证。三者与外发/客户操作均须另行目标授权。本地替身可完成 AC19–21，但不能替代 EXT01。生产固定 NOT_EXECUTED，直到有单独授权和证据。

## 6. 现有测试入口与执行约束 / Runners

下列为已核实存在的命令/文件入口，不是本轮已跑通过的证据。Q00 必须读对应 runner，绑定隔离数据库、非生产配置、fixture、端口和设备后生成**无省略号**的 commands.sh；未绑定环境不能直接启动会写库的测试。

```bash
# imboy 仓；静态门
python3 scripts/check_product_feature_cross_repo.py
python3 scripts/generate_product_features.py --check
python3 api/flatten_internal.py --check
make compile

# imboy 仓；仅在 Q00 环境门之后，逐个 targeted
make eunit-local t=msg_ack_logic_tests
make eunit-local t=group_event_handler_tests
make eunit-local t=group_logic_tests
make eunit-local t=feature_route_http_tests
make eunit-local t=imboy_policy_tests
make eunit-local t=push_notification_ds_tests

# imboyadmin 仓；通过 script 保证 --isolate
bun run test src/services/api/features.test.ts
bun run test src/modules/customer_service
bun run test src/modules/organization
bun run deadcode
bun run lint
bun run build
bun run build:widget
bun run verify:widget
bun run verify:widget-pairing

# imboyapp 仓
flutter analyze
flutter test test/unit_test/
flutter devices
```

现有真实浏览器入口：`playwright.customer-service-seat-embed.config.ts`、`tests/e2e/customer-service-seat-embed/journey.spec.ts`、`dual-seat-claim.spec.ts`、`tests/e2e/enterprise-ent-int01/`。先列用例再跑 Q00 生成的隔离配置。按根规则优先尝试 autoglm-browser-agent；不可用时记录原因，采用现有 Playwright runner；浏览器不委派子代理。

现有原生入口：`integration_test/enterprise/enterprise_scope_native_test.dart`、`organization_chart_nonoa_macos_test.dart`、`workspace_channel_navigation_macos_test.dart`、`enterprise_cs_e2e_test.dart`。Q00 从实际设备发现 targetPlatform，Android必须真机、macOS必须原生；不以Chrome或模拟器替代。尚无的 C2G 双设备、禁用域和故障场景测试由对应任务新增；禁止把计划中的拟建 runner 写成已存在。

`run_product_feature_matrix.sh` 目前只支持 base-only/full-selected/overseas_baseline/agent_hub，且会改写共享生成物、构建APK并使用lock。Q02须增加phase1_delivery支持并审阅隔离/恢复逻辑后才运行，禁止现在直接传新profile并假称有效。`verify_product_feature_artifacts.py` 当前物理资产探针主要是 moment，不足以证明全部二期域均被裁剪；本轮要求补域入口/副作用探针，完全物理裁剪非共享模块作为渐进优化。

中途只跑受影响 targeted；最终全量次数不得凭“旧计划只允许一次”覆盖更严格的继承合同：EO旧门要求同候选连续两次后端全量，Q00若继承为required则保留。默认Q12集中执行两次，App/Admin各全量一次；失败先修局部，重冻后重跑必要最终门。任何 baseline 未解释失败不能在9.9或本地PASS中豁免。

## 7. 评分与最终状态 / Score and verdict

十项各100分，共1000分，总分/100为十分制：

| 维度 | 必需证据 |
|---|---|
| 持久化/幂等/顺序 | AC11–12、AC32–33 |
| 群聊/多端/送达与阅读 | AC07–10 |
| 身份/权限/隔离 | AC13–16 |
| 离线/恢复/通知连续性 | AC12、AC14、AC22–23 |
| 附件完整性与访问控制 | AC13、AC18、AC29 |
| 一期企业/客服集成 | AC15–21，外部状态独立披露 |
| 发布组合/关闭边界 | AC01–06、AC30 |
| 可维护性与文档 | AC24–27 |
| 性能与稳定性 | AC31–33 |
| 可恢复性与可观测性 | AC28–30、AC34 |

每维评分规则：100=该维全部必需证据通过且无剩余问题；95=全部必需证据通过，只剩有复现与owner/期限的非关键体验/整洁度问题；80=功能正例通过但缺异常/真端证据；50=仅代码/单测或历史证据；0=关键失败或无有效证据。必须给出逐维理由，不按“写完几行代码”加分。

达到≥9.9的充要门：总分≥990；AC00–34及其所有required子格全PASS；无P0/P1，无已知丢消息/越权/禁用域绕过；最多两个95分维度，且不足只能属于不影响任何AC的非关键体验/整洁度项。BLOCKED/部分通过不许95分；关二期功能不能给二期本身加分。

状态分离：
- `PLAN_READY`：本文件已就绪，不代表任何实施卡已完成。
- `PHASE1_LOCAL_CANDIDATE_PASS`：AC00–30全部通过且继承required本地项全通过。
- `CORE_IM_9_9_LOCAL`：前项 + AC31–34 + 上述评分规则；只对冻结的profile/平台/负载有效。
- `PHASE1_CUSTOMER_ACCEPTED`：本地通过 + EXT01–03 + 实际承诺平台/provider全部合格 + 客户签收；不因Core IM分高自动成立。
- 任一必需项缺环境/资质：对应域 `BLOCKED_EXTERNAL`，整体最多PARTIAL；生产独立 `NOT_EXECUTED`，不得写PRODUCTION_READY。

## 8. 清理准则与旧计划处理 / Hygiene and supersession

可清理：被完整入口模型证明无引用的手写死代码；重复且有权威替代的文档；被tracked包装流程误收的临时物；过期的空壳入口；无调用且无交付依赖的包。
不得按名字批量删除：bot_*（OA共享）、e2ee_*（历史兼容）、迁移历史、合法测试、生成源/生成器、第三方许可证、旧验收证据、用户WIP。静态grep零引用不是动态Erlang注册/Flutter插件不使用的证明。

文档治理目标是一期README→安装/配置→接口→运维/验收这一条清晰主线；二期只在独立说明出现，三期只在路线图候选段落。一次性调试记录从当前导航退出，归档保留出处；未经逐项清单审核不批量删除历史文件。人工写的新文档必须有owner、适用profile/版本、替代关系；不要再新建多份重复“总计划”。

超过800行的文件先排除生成/vendor/test后建债务表；一期变更触达的热点优先提取真正独立职责，未触达巨文件不为形式漂亮大改。移走文件到另一个垃圾目录不算清理。验收关注“未解释候选为0、active文档错误为0、交付包临时物为0”，不定强制删多少行/删多少文档的指标。

旧产品化计划任务关系：
- W0基线保留；crash dump只做脱敏本地诊断，不上传、不自动复制进仓。
- W1文档事实修正并入Q11，数字和版本重新采样。
- T2.1以Q04替代；先区分送达/已读，不能ACK即读。
- T2.2以Q05替代；核直接发送路径后决定补线或去重复。
- T2.3 FTS密文排除留在二期安全回归；一期必须保证历史密文不泄露，不能仅凭partial index认定查询安全。存在partial index不等于普通SQL不返回密文，必须检验实际查询谓词。
- W3商品卡片为条件增量，不能硬编码迁移165或暴露明文订单元数据；若启用重新评估数据权限、最小化字段与真实客服合同。
- W4提现通知移出一期主线。
- W5配置卫生并入Q11交付包白名单；仅产报告不等于泄露风险消除。真实密钥处置/轮换仍待目标授权，禁止在计划或日志打印值。
- 旧T5.2不能把任意403/404解释为未部署：需显式capabilities与feature error code，权限不足、资源不存在分别处理。
- W6以Q12/Q13与继承门为准；mock降级只算测试准备，不能顶替真链路验收。

## 9. 证据、恢复与执行边界 / Evidence and recovery

Q00建立run目录（默认仓外、权限收紧；repo只放脱敏摘要）。最小文件：baseline.json、requirements.json、inheritance.tsv、commands.sh、leases.json、acceptance.tsv、candidate-manifest.json、artifacts.sha256、verdict.json。

每个验收子格记录：ID/来源要求/任务/平台/角色/组织/设备、三仓SHA、plan_sha256、profile_hash、fixture与runner hash、完整命令、退出码、执行/跳过/取消数、UI/API/DB或文件字节oracle、日志及证据hash、attempt、状态、原因、NEXT_UNLOCK。证据摘要不是原始结果替代品。最终复算要求ID exact-set相等，重复行/空oracle/旧SHA混用一律不通过。

恢复流程：保留失败证据→分类→最小修复→审查→targeted→本地独立提交→重冻。未知结果写操作先查询幂等状态，不盲重放；同环境错误最多重试2次，同缺陷修复最多3轮后登记阻断，继续无依赖任务。租用资源不能同时运行，不能杀foreign进程或改用未知数据库。只允许撤销本卡自有提交；禁止reset/clean/stash/整仓checkout覆盖WIP。数据库回退只针对本run隔离库和已验证备份。

代码实施授权后：按独立功能本地提交，命令级身份 `leeyi <leeyisoft@qq.com>`，不改全局Git配置。禁止push、发布、部署、生产迁移、真实客户数据和第三方通知；不改 erlang.mk、ios/*、macos/*、plugin/r_upgrade。需要这些行为时先准备可审查产物再请求目标授权。

二期顺带维护仅限同一修复自然改善的共享错误处理/文档/测试，建议不超过一期工作量10%，任何二期新能力不得阻塞一期门。若Q00/Q04表明主体语义需大改，先报告实际差距和新增估算，不以9.9目标掩盖工期。

## 10. 启动条件 / Next execution

下一次明确授权实施后，从Q00开始，先交付当前候选、旧验收继承表、完整测试命令与环境授权表，再实施Q01。前2人日检查点回答三件事：生成物漂移是否已修复；C2G双端是否复现真实丢失/已读错误；一期关闭面与OA共享依赖是否能在现有结构小改完成。此时更新18–31人日估算，保留初始估算及变化原因，不把时间预算耗尽当作完成。

## 2026-10-08 用户范围修订

朋友圈（moment）与附近的人（location）纳入一期必交付范围，原默认关闭条款作废；二期七域及其余排除域不变。此修订不代表功能已验收。

新增 required 子项并归入 AC05、AC13–AC16、AC28–AC30：
- P1-MOM-01：Android 真机/macOS 原生发布文本与图片、浏览分页、点赞评论删除及另一端同步，UI/API/DB 一致。
- P1-MOM-02：私密/好友/指定可见范围、拉黑与删除后，列表、详情及附件实际字节权限无越权。
- P1-LOC-01：Android 真机/macOS 原生允许/拒绝/撤回定位权限、无定位及超时可恢复，附近列表与距离符合合成坐标 oracle。
- P1-LOC-02：关闭附近可见性、拉黑、账号切换和撤权后不泄露原账号位置或不可见用户；服务端列表与端侧缓存交叉验证。
- P1-SOC-03：三仓一期 manifest/profile 一致，朋友圈与附近入口允许，其余二期入口继续拒绝；同一冻结候选完成构建与受影响回归。

测试只使用本 run 自有账号和合成坐标，不向第三方通知，不采集真实用户位置；运行报告与材料全部仓外。
