# IMBoy 频道、群组与会话体验审计及改造方案 V2.1

初审：2026-10-20；V2 修订：2026-10-22；计划收口：2026-10-27。

**计划状态：PLAN_READY。实施状态：IN_PROGRESS（2026-10-27 获授权从 main 建 worktree）。设备与完整功能验收：NOT_RUN。**

用户授权本轮收口任务计划。本文是本专项唯一当前计划入口，保留 F01–F22、C1–C5、W00–W13、J01–J29；[V1](./plan-v1.md) 保持历史取证快照，不再形成另一份 V1.1 执行计划。本轮补齐当前缺口、角色/状态/动作细则、D1–D7 设计默认值、工作包依赖与退出条件；视觉真源继续为 imboyapp/DESIGN.md。

PLAN_READY 只表示计划可用于实施拆解与评审，不表示功能已修复、测试已通过、产品已上线。计划授权不转为业务实施、commit、push、合并、部署、生产迁移或影响第三方的操作授权。

English: V2.1 is the sole current plan. It preserves the historical V1 and all existing finding, contract, work-package and acceptance IDs, adds current-source verification and explicit planning defaults, and closes planning gaps only. Implementation and runtime acceptance remain open.

## 1. 范围、证据与结论

### 1.1 V1 历史取证范围（2026-10-20）

| 仓库 | HEAD | 本轮取证边界 |
|---|---|---|
| imboy | `7fcc7dc2ac4d88d758f08598a33c84743ccebf99` | channel/group/workspace/conversation API、Logic、Repo；架构与概念文档 |
| imboyapp | `0ca535d2c83ff7df8b194a5f6baf170bb10ee261` | 个人/企业壳、工作区列表、频道创建/详情/列表、群创建/详情、聊天/会话、设计 Token、相关测试目录 |

V1 取证来自当时工作树，不是干净发布候选。后端已有 attachment、micro-community、workspace、organization-resource-access 等其他任务 WIP；取证期间 `workspace_logic.erl`、频道访问相关 DS/Repo/同步链与测试也有外部写入。本轮不修改、暂存、提交这些文件；实施前须重新绑定候选与权限契约。收尾复查已看到其他任务为频道列表增加访问资格谓词；这与 F13 所述“个人/工作区展示范围”是两个不同问题，不能把范围缺口误写成完全没有授权过滤。App 已有 `i18n_review_2026-10-20/`，不属于本任务。

方法：静态跟踪“入口 → 路由 → 状态 → API → 服务端约束”，并检查已有测试覆盖方向；没有启动服务、执行写接口、发送消息、使用真实账号或进行真机操作。本文的代码分支结论可用于安排修复，不能替代实际设备体验验收。

证据等级：**A = 代码直接可确认；B = 代码存在明确风险，用户可见表现需复现；D = 计划设计变化，非线上现状**。优先级 P1 表示影响主要任务的正确性/可达性，P2 表示效率与一致性，P3 表示规模优化。本轮没有足够运行证据宣称线上 P0 故障。

### 1.2 总体判断

现有基础能力较多，最需完善的是**场景归属明确、列表状态独立、动作反馈可信、离开与返回可恢复**。应优先打通已有能力，不新增一套聊天/频道体系。

- **群组**是多人交流与协作主体，群聊是其中的交流行为；**频道**是发布与订阅主体，评论针对具体内容。
- **个人/企业是体验上下文**；资源的真实归属是 `personal` 或具体 `workspace`，创建后不可变。
- 企业壳里的工作区群聊、企业托管会话、客服会话有不同数据模型，不能只凭入口都在“企业”里就合并数据源。
- 组织是业务对象，不是租户。不能从该裁决推导“工作区/群成员校验可以取消”，也不能自动推导组织管理员拥有全部资源内容权限。
- 现有群详情已把公告/文件/相册、投票/日程/作业、其他设置分组，已有角色策略、草稿隔离与部分无障碍支持，继续复用。

权威解释：`docs/architecture/2026-10-12-organization-positioning-amendment.md` §1–3；`src/logic/group_logic.erl:788`、`src/logic/channel_logic.erl:374` 的不可变归属守卫。`docs/glossary.md`、`docs/concepts/collaboration-hierarchy.md` 与 App `CLAUDE.md` 的部分租户措辞仍过时，实施时应同步纠正这些权威文档，不能借本次 UX 改造扩大授权。

### 1.3 V2 历史复核（2026-10-22）

| 仓库 | 本次 HEAD | 边界 |
|---|---|---|
| imboy | bed97a9a631e6da58c08a6cd30da010efc2b586f | 有其他任务 WIP；只读抽查，不代表干净候选 |
| imboyapp | f2340548c700d2e271420e0f58cc517b82951428 | 按当前文件复核关键调用链；未运行 App |

本节仅记录 2026-10-22 历史，当前结论以 §1.4 和第 3 节为准。V2 当时没有重新审计全部 F 项。F02/F03/F04/F05/F06/F15 相关链及工作区快照过期字段做了定向静态复核，其余继续标为 V1 证据，实施相应工作包时更新。HEAD 不能描述未提交内容；以下 SHA-256 仅定位本次关键文件快照，不是完整候选清单。实施前记录两仓 HEAD、相关 staged/unstaged/untracked 文件及内容摘要；中途被外部修改即重新核对，不能覆盖其他任务。

| 本次复核文件（相对聚合工作区） | SHA-256 |
|---|---|
| imboyapp/lib/store/api/channel_api.dart | f80e1a9ba453d45f4fd9d55b9a5e9eba7133a690097da0f2d7d16fad5b3db2fc |
| imboyapp/lib/page/channel/channel_provider.dart | de64d19209ffb66d1a6d87e06da6d7431e141ee80ce45fcd1f772f9beb5dabba |
| imboyapp/lib/service/channel_service.dart | 65fb0bfa80dda507d16e4b943d1dd8bf726abbd2a06973c691e3f63a982a4712 |
| imboyapp/lib/modules/organization/presentation/organization_manage_page.dart | 3e8ff9b9b6c969b5efdbc67d1b728d8538ad1aa143057193c2b7bfbf02aa2a42 |
| imboy/src/api/channel_handler.erl | 403b9b7d196242b10bf43b3394ec6df837d945cb41ab41517ca3e71e5855b0e6 |
| imboy/src/repo/group_repo.erl | 7e1416ce90a1a6730a76b2f62e8a3bf9db730a8cfb3214d265656a612cf375f1 |

V2 修订要点：F02 包含组织治理页频道创建；创建恢复同时覆盖群与频道；F15 真分页必须联动本地订阅和未读；消息 expire_at 与授权有效期分开；旧客户端兼容、持久化升级/回退、F→工作包→旅程→测试建立明确边界。下文新增参数、响应和失效规则均为目标契约，不声称当前已经支持。

### 1.4 V2.1 当前复核与证据基线

| 仓库 | 本次复核 HEAD | 证据范围 |
|---|---|---|
| imboy | `313016123e735dd3c305f173422a9dac79462469` | 群/频道创建与归属守卫、订阅/管理列表、消息分页、工作区资格、会话和既有专项计划 |
| imboyapp | `b6afea5842461acacc7b7b6861961875a920198c` | 个人/企业入口、频道列表/详情/API/内容流、群列表/建群/详情、聊天/会话、当前 DESIGN.md 和相关测试源码 |

初审基线分别为 Backend `7fcc7dc2`、App `0ca535d2c`，不能继续用初审行号描述当前实现。本次静态逐项复核代码分支及直接调用方，未启动服务、执行写接口、使用真实账号或跑设备验收。测试文件存在只能证明有测试源码，不能证明本候选已通过。

本次 App 工作树干净；Backend 初始有其他任务修改 docs 索引、ADR 索引、附件协议及历史计划，复核期间已由其他任务提交为 31301612（仅文档），另有既有未跟踪资料。相关业务源与本轮取证指纹保持一致。本轮恢复 V1 原始字节后，只将收口内容留在本文件，不吸收、回滚或提交其他改动。该文件目前未跟踪；未获 commit 授权，保持未提交不属于执行失败。

关键输入 SHA256（用于识别后续漂移，不等于运行证据）：

| 输入 | SHA256 |
|---|---|
| `imboyapp/DESIGN.md` | `7f3b25ab277e4e45ad7ca3d3112e2298bc746a7a4e1ba53e0eec4c38fcbba12a` |
| `imboyapp/lib/page/channel/channel_provider.dart` | `1ff039e39fe1de9ff892f489513bfcbda446798bbd58b22b1798351bddf97748` |
| `imboyapp/lib/store/api/channel_api.dart` | `e67b604582815fd27739b677db03edd6516cfe218e68e64c15a4305ad75f7e3a` |
| `imboy/src/api/channel_handler.erl` | `21bd462553489e3f8b6cd38d9d691db467d3e67ff5e92d2eba0ea39c8c8ebb4c` |

本轮逐项静态复核 F01–F22：20 项 OPEN、2 项 PARTIAL（F04/F05），无运行证据关闭项。范围仍含个人/工作区群与频道、单聊/群聊及会话管理，不缩减为最容易修改的页面。角色与归属约束继续以现行架构/服务端为准。

与交付 V3.2 的接口：docs/design/2026-10-23-guangzhou-delivery-v3/plan-v3.2.md 的 B03/A02/A04/Q01–Q04 有重叠。W00 核实活动写者及候选绑定收据；相同候选/输入/场景/oracle 可复用，否则只补差异，不重复修复，不自动继承另一任务的提交授权或平台豁免。

### 1.5 企业 UIUX V1.1 的既有实施与复用边界

`_task_packages/IMBoy_Enterprise_UIUX_V1_1/TASKS.md` 的 plan_id 为 `imboy-enterprise-uiux-v1`，与本文历史 V1/V2 不是同一计划。其范围只含 Flutter 展示、局部展示组件、文案和测试，明确不改 API/DTO/Provider/Service/Domain、权限和分页；P1 是企业频道详情，P2 是工作台/OA 入口，P3 是客服坐席列表。不能用该包完成替代本文群/频道归属、创建恢复、授权失效、分页、已读和会话持久化工作。

该包已实际实施。App 当前 HEAD 包含 `047d94b4e`（P1/P3 场景及图标字体修正）和 `3a80c6db1`（合并后收口）；源码可见唯一频道标题、筛选默认折叠等改动。STATUS.md 声称的原 merge `32dd2e919` 不是当前 HEAD 祖先，故以当前可达实施提交判断已集成，不直接照抄历史合并拓扑。执行记录入口：`imboyapp/docs/plans/enterprise-uiux-v1/execution/STATUS.md`。

- 视觉/局部交互复用：唯一标题、统计占位、动作禁用/忙态、讨论横幅方向、筛选折叠、错误空态、工作台行对齐；W12 优先复验差异，不重新整套换肤。F05 的错误呈现已有改善，但失败传播/尾部分页恢复仍未闭合；F12 的箭头改良不等于绑定群/私聊目的地正确。
- 历史自动证据：FINAL_GATES.md 记录候选 `d397d38c8` 的目标 143/143、全量 10830/10830、零失败零跳过；后续 `95dc3cddf` 修正仅补目标/图像/analyze，全量沿用父候选。它们不是当前 HEAD 的全量通过收据。
- 本轮只读清点该包 p1/p2/p3 图像：120/72/120 张，唯一图 64/32/48，合计 312 张、144 张唯一图，与最终修正说明一致。相同图不自动等于无效覆盖；ios/android 平台轴未产生图像差异，不能冒充原生验收。
- 原生验收仍不完整：T30.a Android P1 记录 6 PASS + 1 PARTIAL，且部分角色/慢网断言未覆盖；其余 8 张原生卡及 T28.m 真实跨会话仍待验。卡片 DONE 是记账，不等于全部断言通过。
- 台账有漂移：ACCEPTANCE_RESULTS.json 停在 revision 3、candidate=null/NOT_VERIFIED；STATUS.md 前段仍有旧候选和“golden 未生成”等历史语句；运行真源已清理，不能直接运行旧恢复命令宣称续跑成功。后续若恢复该包，先对齐最终候选、人工义务和证据引用，不改静态 TASKS.md 初始 TODO 来伪装执行状态。

因此本专项 V2 的 W00–W13 仍为未执行；现成视觉实现是可复用输入，不计为本专项 J01–J29 已验收。W00 先做上述证据对齐，W12 只补剩余视觉/可访问性差异；P2 OA、P3 客服专属工作继续留在原包，不扩入本文业务范围。

## 2. 已有能力与四种场景

| 场景 | 当前已有能力 | 主要缺口 |
|---|---|---|
| 个人群组 | 好友选人建群、群列表/发现、成员角色、公告、文件/相册、投票/日程/作业、昵称/备注、免打扰、退出/解散 | 建群页缺搜索与加载失败恢复；群头像跳转错误；搜索只覆盖已加载项；清空与移除语义交织 |
| 工作区群组 | 服务端 scope/成员约束、已加入群列表、授权消息快照、组织治理页建群/改名 | 日常建群入口与治理入口脱节；群列表缺搜索/主动刷新；会话行缺管理动作；创建后改名失败的恢复路径不足 |
| 个人频道 | 创建、已订阅/我管理、发现、发布、评论、订阅/邀请、付费能力受功能位约束、分享/二维码/文章导航 | 两 Tab 共用列表状态；详情共用状态；错误可能显示为空；阅读与已读语义不精确；归属展示与列表范围不完整 |
| 工作区频道 | 服务端内部化、工作区频道列表、嵌入式内容页、治理侧归档/恢复、分享授权入口 | 通用创建页/API 不传工作区；列表仅目录信息；通用讨论引导与实际绑定讨论入口重叠；没有明确“我的订阅/本工作区目录”区分 |

已有聊天能力包括引用、编辑、转发、撤回、失败重试、@、禁言状态、历史失败提示与部分定位能力；“代码存在”不等于每种消息类型/平台均通过验收。频道微社区、付费、发现、E2EE 等均要按实际交付配置判断可用性。

English: Keep groups for discussion/collaboration and channels for publishing/subscription. Reuse the existing implementations; repair scope, state ownership and recovery paths before adding features.

## 3. 22 项缺口的当前处置

证据：A=代码直接确认；B=具体用户表现待复现；D=设计变化。状态只用 OPEN（未闭合）、PARTIAL（已有部分改善）；本次无一项以运行证据关闭。优先级沿用初审：9 项 P1、13 项 P2，P1 不代表已证实线上 P0。

表内路径从聚合工作区开始；后续简写 A/ 指 imboyapp，B/ 指 imboy。行号绑定 §1.4，漂移后按函数重新定位。

| ID / 优先级 / 状态 / 证据 | 当前结论与剩余工作 | 当前证据 | 任务 |
|---|---|---|---|
| F01 / P1 / OPEN / A+D | 企业壳全局发起聊天仍走好友建个人群；明确个人域语义，另在工作区群列表提供 scoped 创建，保留既定五项菜单 | `imboyapp/lib/page/workspace_shell/workspace_plus_menu.dart:62`；`imboyapp/lib/page/group/launch_chat/launch_chat_provider.dart:180` | W04、W05 |
| F02 / P1 / OPEN / A | 通用创建页、Notifier、API 及组织治理 _createChannel 均不传 scope/workspace_id；治理页目前虽设私有免费仍会默认 personal，须一起修复 | `imboyapp/lib/page/channel/channel_create_page.dart:78`；`imboyapp/lib/page/channel/channel_provider.dart:715`；`imboyapp/lib/store/api/channel_api.dart:32`；`imboyapp/lib/modules/organization/presentation/organization_manage_page.dart:395` | W04、W05 |
| F03 / P1 / OPEN / A+B | 两 Tab 仍共用一个 ChannelListState，订阅事件强制刷新同一状态；Tab 之间没有独立代次/集合 | `imboyapp/lib/page/channel/channel_list_page.dart:137`；`imboyapp/lib/page/channel/channel_provider.dart:165`、`:261` | W01 |
| F04 / P1 / PARTIAL / A+B | 已新增频道与消息请求代次、旧加载锁保护；仍为无参数详情 Provider，旧 messages 可保留，叠页/返回隔离未闭合。不得重写为“完全没有防乱序” | `imboyapp/lib/page/channel/channel_provider.dart:327`、`:371`、`:450`；`imboyapp/lib/page/channel/channel_detail_state.dart:30` | W01、W02 |
| F05 / P1 / PARTIAL / A+B | 主详情链已用 getMessagesPage，失败抛错且空列表时有错误页；旧 getMessages 仍吞失败，已有列表时错误无专用恢复。须检查全部调用方及尾部分页失败 | `imboyapp/lib/store/api/channel_api.dart:394`、`:417`；`imboyapp/lib/page/channel/channel_provider.dart:450`；`imboyapp/lib/page/channel/widgets/channel_message_feed.dart:418` | W01、W02 |
| F06 / P1 / OPEN / A+B | 详情 API 仍以 null 合并多类失败，缓存回退不能区分临时离线与撤权；补类型化错误与授权失效呈现，不宣称已证明泄漏 | `imboyapp/lib/store/api/channel_api.dart:64`；`imboyapp/lib/page/channel/channel_provider.dart:414` | W01、W02 |
| F07 / P1 / OPEN / A | 会话头像仍统一进入联系人资料，C2G 把群 ID 当用户 ID；按对象分派 | `imboyapp/lib/page/conversation/conversation_page.dart:515` | W03 |
| F08 / P1 / OPEN / A | 单聊免打扰无会话行即返回、写失败只记日志，调用方仍提示成功；补持久化身份、失败回滚和读取忙态 | `imboyapp/lib/page/chat/chat_setting/chat_setting_page.dart:97`、`:412` | W03 |
| F09 / P2 / OPEN / A | 侧滑移除未消费远端 false；失败保持原项并解释，动作防重 | `imboyapp/lib/page/conversation/conversation_page.dart:415`；`imboyapp/lib/page/conversation/conversation_provider.dart:700` | W03 |
| F10 / P2 / OPEN / A | 工作区群目录仅已加入群，无搜索、主动刷新/创建；补日常入口，不伪称企业全部群 | `imboyapp/lib/page/workspace/workspace_groups_page.dart:44`、`:92` | W04、W05、W07 |
| F11 / P2 / OPEN / A | 工作区频道目录只有通用图标、名称简介；消息页频道筛选仍切到目录，缺更新摘要/未读/订阅态 | `imboyapp/lib/page/workspace/workspace_channels_page.dart:158`；`imboyapp/lib/page/workspace_shell/enterprise_shell_page.dart:464` | W07 |
| F12 / P2 / OPEN / A+D | 常驻讨论引导跳群列表，与真实绑定讨论入口并存；“私聊”实际进个人会话列表；收敛为准确目的地 | `imboyapp/lib/page/workspace/workspace_channel_detail_page.dart:79`、`:111`；`imboyapp/lib/page/channel/channel_detail_page.dart:299` | W06、W10 |
| F13 / P1 / OPEN / A+B | 订阅/管理列表已有访问谓词，但未按所选空间分区；订阅安全列缺 scope/workspace_id。是展示范围缺口，不能误报为无授权 | `imboy/src/repo/channel_repo.erl:225`、`:275`；`imboy/src/logic/channel_logic_subscription.erl:22` | W07 |
| F14 / P2 / OPEN / A+D | 首次返回消息即标首条已读、取消监听；不验证前台可见、不跟踪后续阅读 | `imboyapp/lib/page/channel/channel_detail_page.dart:77` | W08、W09 |
| F15 / P2 / OPEN / A | 订阅频道清单仍回全量和 cursor/limit 回显。消息分页虽已改善，不能代替订阅清单分页 | `imboy/src/api/channel_handler.erl:389`；`imboy/src/repo/channel_repo.erl:225` | W07 |
| F16 / P2 / OPEN / A+B | 引用定位只查已加载列表，目标不存在即静默返回；补可行动反馈与合法上下文拉取 | `imboyapp/lib/page/chat/chat/chat_page.dart:936` | W11 |
| F17 / P2 / OPEN / A+D | 群清空记录还隐藏会话；群主离开仅显解散，没有就地转让后退出路径；分开后果与恢复 | `imboyapp/lib/page/group/group_detail/widgets/group_danger_zone.dart:94`、`:133` | W06、W10 |
| F18 / P2 / OPEN / A | 好友建群有索引和已选头像，但无搜索、页面错误状态/重试；失败不应等同空好友 | `imboyapp/lib/page/group/launch_chat/launch_chat_provider.dart:165`；`imboyapp/lib/page/group/launch_chat/launch_chat_page.dart:249` | W04、W05 |
| F19 / P2 / OPEN / A+B | 工作区消息 30 秒/事件刷新会显整页 loading；行无会话管理动作、模型无相应偏好字段 | `imboyapp/lib/page/workspace/workspace_conversations_page.dart:84`、`:175`、`:339`；`imboyapp/lib/store/model/workspace_conversation_model.dart:8` | W09 |
| F20 / P2 / OPEN / A | 治理建群仍为先建空群再改名；改名失败群已存在，缺继续同 gid 的恢复流程 | `imboyapp/lib/modules/organization/presentation/organization_manage_page.dart:333`；`imboy/src/api/group_handler.erl:233` | W04、W05 |
| F21 / P2 / OPEN / A+D | 手动未读仍写 unreadNum=1；标已读本地失败被忽略；人工待处理与消息水位混用 | `imboyapp/lib/page/conversation/conversation_page.dart:366` | W08、W09 |
| F22 / P2 / OPEN / A | 群/频道列表只搜索已加载项，频道搜索时关加载更多；频道旧服务端当前全量返回，不据此声称第 51 个必漏；新真分页启用须一起补完整范围搜索 | `imboyapp/lib/page/group/group_list/group_list_page.dart:116`；`imboyapp/lib/page/channel/channel_list_page.dart:207` | W07 |

已有 TZ05 测试源码（`imboyapp/test/unit_test/page/channel/channel_detail_actions_test.dart:145`、`:240`）覆盖迟到响应/分页代次，可复用，不代表账号隔离、叠页和真机旅程已通过。

## 4. 页面与动作规格

### 4.1 信息架构和线框

不新增一级导航。群/频道列表负责找对象，消息页负责处理更新；详情和聊天复用既有实现。个人顶端频道摘要可折叠，默认最多占一条聚合行，点击进入完整频道更新页；不能丢掉原有已订阅访问入口。

```text
个人消息                         企业消息 · 企业名 / 工作区名
搜索会话                   +     搜索当前列表                +
频道更新 · N 个有更新 >           消息 / 日程 / 待办
置顶会话                         全部 · 未读 · 群聊 · 频道更新
最近会话                         仅当前工作区获授权的摘要

群组目录                         频道目录
归属：个人 / 企业·工作区           归属：个人 / 企业·工作区
搜索                    创建      搜索                    创建
我的群 / 我管理                   我订阅 / 我管理 / 本工作区目录
群名 · 人数 · 我的角色             头像 · 名称 · 简介 · 订阅态
```

“本工作区目录”仅企业有；个人提供既有发现入口。未获准读取内容的频道可按目录策略展示，但不返回内容摘要。创建按钮只给可创建者；未加载资格时禁用并显示加载说明，禁止先亮按钮再拒绝。

```text
创建群组                         创建频道
归属：企业名 / 工作区名（固定）     归属：由入口固定（不可提交时漂移）
名称                             名称
选成员 [搜索]                    简介 / 头像（可稍后）
已选头像与数量                    内部 / 公开 / 邀请（按资格约束）
姓名 + 部门/重名区分               更多设置：ID / 标签 / 已启用付费
                         创建                              创建
```

名称 trim 后非空、长度按现有服务端上限；姓名/ID 不能互作输入替代。选人搜索清除不丢已选；不可选成员显示资格原因。工作区只选 active 工作区成员，组织通讯录不能替代工作区资格，不顺带邀请/授权。个人保留既有最少选人约束；工作区名称必填，创建者计入人数仅一次。

```text
< 频道名 · 工作区归属        分享  ⋯
简介（可展开）    订阅 / 已订阅 / 权益状态
置顶内容摘要（更多可展开）
内容：作者 · 时间 · 正文/标题 · 评论
有新内容 [查看]（用户正在读旧内容时）
讨论群「实际群名」>（仅合法绑定存在）
发布者：[发布内容…]    读者：没有聊天式输入框
```

只保留一个频道身份标题与一个真实讨论入口。无绑定不造 General，也不常驻“去群里讨论”横幅；群目录放低频菜单。个人消息入口若保留须叫“个人消息”，不能叫对某人的私聊。

### 4.2 角色 × 能力矩阵

这是 UI 呈现规则，动作仍由服务端逐次裁决；“可”始终受资源状态、功能位、来源策略和目标角色约束。未知角色默认无写能力，不从组织角色推断群/频道权限。

| 对象/角色 | 读/进入 | 写/管理 | 离开/治理 |
|---|---|---|---|
| 工作区 owner/member | 读获授权目录；群聊天还需群成员；频道内容还需内容资格 | 可创建工作区群/频道；不能自动管理其他人创建的资源 | 工作区治理独立资格；归档后按既有合同只读/不可写 |
| 工作区 guest | 获授权目录与已加入资源 | 不创建；已有群/频道写能力再看资源级角色 | 无工作区所有者治理动作 |
| 非 active 工作区成员 | 不进入内部内容；链接显示无访问资格 | 无 | 返回原上下文；不以缓存恢复资格 |
| 群成员(1)/群嘉宾(2) | 获准历史、群资料 | 未禁言且策略允许才发送/协作；不能踢人/禁言/发管理公告 | 可按现行政策退出；不解散、不转让 |
| 群管理员(3)/副群主(5) | 同成员 | 管理公告/成员/禁言按目标角色矩阵；不自动管理同级或更高角色 | 不解散、不转让所有者 |
| 群主(4) | 同成员 | 群资料与合法成员治理 | 独立“转让后退出”“解散群组”，目标须有合法资格 |
| 频道访客/未订阅 | 仅策略允许的介绍或预览 | 订阅/接受邀请/购买按状态；不发布 | 无退订/治理动作 |
| 频道订阅者/已购用户 | 订阅与已购分开计算内容资格，私有/内部限制继续生效 | 评论/讨论按现有权限；无发布/管理 | 退订不退款；讨论资格变化重新授权 |
| 频道编辑(1) | 获授权内容 | 可发布，自己的内容编辑/删除按服务端合同；不显频道管理员设置 | 退出治理身份与退订分开，不假定编辑=管理员 |
| 频道管理员(2)/创建者(3) | 获授权内容 | 发布及获准频道治理；分配角色/删除/归档再核具体动作 | 创建者转移/退出依赖现有保护，不承诺普通退订能移交所有权 |
| 来源托管资源 | 可读范围按来源及服务端投影 | 来源只读字段给说明/管理去向，不在本地绕过 | 不把来源托管群单向归档误做可恢复 |

角色依据：`imboyapp/lib/modules/group_collab/domain/value/group_role.dart`、`imboy/include/group_role.hrl`、`imboyapp/lib/store/model/channel_model.dart:67`、`imboy/src/logic/workspace_logic.erl:765`。服务端与客户端矩阵不一致时，先定位根因并修直接调用方，不新增通用权限框架。

### 4.3 创建、加入、邀请、订阅的状态转移

| 当前状态/动作 | 主动作与忙态 | 成功/返回 | 失败/恢复 |
|---|---|---|---|
| 创建表单未完成/资格未载入 | 创建禁用，字段内说明原因；返回可用 | 不产生资源 | 保留输入，资格失败有重试 |
| 正在创建 | 单次提交，按钮忙态且防重；已选成员冻结 | 以响应实际 ID/归属进入资源；刷新对应列表 | 普通拒绝保留输入并定位字段/成员；切账号使旧响应失效 |
| 创建结果未知 | 显示“正在确认创建结果”，不允许重新生成另一请求 | 查询/同幂等键重试得到原 ID 后进入 | 无法确认时保留恢复信息，不假装未创建；不得自动删已建资源 |
| 群已建、改名失败 | “群已创建，名称未保存”，可进入/重试改名 | 始终恢复同 gid | 不弹通用创建失败诱导重复建群；后台刷新后仍可找回 |
| 未加入群/个人公开群 | 介绍与当前策略允许的“加入”；企业目录不冒充群发现 | 服务端确认成员关系后进聊天 | 私有/已满/被拒/成员资格变化给对应说明；审核态仅后端已支持时展示“等待审核”，不新造审批流程 |
| 已加入群/已订阅频道 | “进入群聊/查看内容”，避免重复确认 | 返回原目录位置 | 网络失败不改成员/订阅事实；撤权重新授权 |
| 有效邀请 | 接受/拒绝只执行现有支持动作，忙态防重 | 免费邀请成功对账；付费邀请仅进入购买上下文，不当作已购 | 过期/撤销/身份不符显示不可用；已接受做幂等回读；不发送新的邀请绕过原状态 |
| 邀请链接失败/不认识资源 | 返回与重试；不显示敏感内容 | 权威解析归属后进单资源 | 不自动切全局空间，不把 403 和断网混为不存在 |
| 订阅成功、回读失败 | “已订阅，正在刷新” | 对账恢复订阅态与未读 | 重试对账而不是重新购买；部分成功不能显示完全失败 |
| 加入绑定讨论群 | 未加入才显“加入讨论群”，解释影响并确认；已加入显“进入讨论” | 再读绑定与资格后加入/进入同群 | 绑定变更、撤权、付费不足各自反馈；不自动加入、不盲跳默认群 |

工作区频道固定内部，不展示会被服务端强制改私有的“公开”选项。付费、邀请、发现、微社区按当前功能位启用；关闭功能时不保留假成功入口。真实邀请/加入可能影响第三方，实测须使用经用户确认的专用身份与对象范围。

## 5. 单聊、群聊与会话管理交互细则

### 5.1 内容与聊天操作

| 动作/状态 | 交互规格 | 不能省略的检查 |
|---|---|---|
| 频道发布/编辑 | 发布与评论明确分开；提交前可取消，提交中防重；失败保留正文/附件/草稿；成功由真结果确认后清草稿 | 编辑/管理员/创建者能力及归属不漂移；受理超时复用消息身份 |
| 评论 | 顶部保留原帖摘要；发送失败留内容可重试；返回定位原帖 | 原帖已删/无权访问解释状态；不以评论权限替代发帖权限 |
| 内容治理 | 置顶/精华/删除只给真实能力；动作结果回写同一条内容，取消无副作用 | 功能位、目标 ID/版本和服务端结果；不可用项不假显成功 |
| 聊天发送/上传 | 进度、已受理、送达、已读分别展示；失败气泡可重试/更多，输入草稿保留 | 重试沿用同消息幂等身份；禁言/撤权不盲重试；沿用 conv_seq，不从 ID 猜顺序 |
| 引用/历史定位 | 输入区引用可取消；先查已加载/本地，再合法拉上下文；定位成功高亮且可回原位置 | 找不到明确“未在本机/历史不可用/权限不足”；无 around-message 能力时先给反馈，再补契约，不无限拉全部历史 |
| 编辑/撤回/删除 | 明确编辑态/已编辑/已撤回；本人本机删除与撤回分开；失败保留原消息 | 沿用服务端时限与状态约束；菜单可见与执行能力不同，不机械合并策略；“为所有人删除”不能承诺无限期 |
| @、禁言 | 摘要标识 @我；禁言显示到期/原因与可操作下一步 | 到期重新确认能力；管理员动作校验双方角色；通知与已读不共用状态 |
| 多选/转发/收藏 | 显示数量、目的地/账号空间与确认，逐项失败可识别 | 沿用授权附件组件与现有加密策略；不复制无权内容、不泄漏密文占位文本 |
| 草稿/换空间 | 按账号、类型、对象隔离；返回恢复内容/光标；异步落盘保持原所有者 | 不迁移无归属旧草稿、不让迟到写入污染新账号；语音/附件草稿仍用既有生命周期 |

### 5.2 会话、阅读、离开与恢复

会话行信息次序为名称/类型、时间、摘要、未读/通知态。摘要可表达草稿、@我、发送失败、普通内容；人工待处理用独立小点/标记，不能改写真实 unread_count。置顶、免打扰、已读、待处理独立；桌面右键/键盘、移动侧滑和读屏动作调用同一能力。

| 动作 | 成功含义 | 失败/返回与多端语义 |
|---|---|---|
| 打开/返回 | 点行进聊天，C2G 头像进群资料，C2C 头像进联系人；返回恢复列表查询/位置 | 对象失效返回安全目录，不残留错误对象；叠页仍绑定原资源 |
| 同空间刷新 | 有效授权快照内保留列表/滚动，局部刷新；首次加载用骨架 | 刷新失败保留仍有效数据并重试；切账号/空间立即去旧上下文，授权到期不得无限缓存 |
| 置顶 | 服务端成功后更新排序，失败不改变原事实 | 既有多端合同保持；新增工作区偏好要有权威版本与事件，不只改本机展示 |
| 免打扰 | 保存成功后开关和列表一致；未建会话也能保存偏好 | 写失败回滚，读取中/写入中禁重入。现有仅本机策略须标“仅此设备”，不暗示已同步；无服务端同步合同不宣称多端一致 |
| 频道查看/聊天已读 | 前台、未遮挡、真实可见目标推进单调水位；显式标已读执行独立请求 | 失败给重试，不能把失败洗成成功；新帖/消息到达不抢滚动，清未读后人工待处理独立保留 |
| 从列表移除 | 隐藏本人列表项，保留成员/订阅和消息；显式再打开可恢复入口 | 沿用当前服务端软删除语义，本期不擅自改“新消息自动复现”政策；失败保持原项，提供反馈 |
| 清空本机记录 | 清本设备允许清的记录，保持会话入口与关系 | 说明云端可否重新拉取，按保留期与密钥实际能力，不泛称永久删除；缓存与重试队列一致 |
| 普通成员退出 | 成员关系由服务端确认后再退出路由；以后访问按重新加入策略 | 失败保留页面；重加入世代/历史范围重新校验，不能恢复旧授权 |
| 群主退出/解散 | 主按钮“转让后退出”，选择合格接任人；另有红色“解散群组”确认 | 转让成功但退出失败必须提示已转让并仅重试退出；解散为独立高危动作，不通过普通返回触发 |
| 退订/归档 | 退订不退款；讨论资格按政策重验；归档展示真实状态 | 不自动退出无关群；仅有恢复合同/权限才显恢复，来源托管群不假造恢复 |

所有危险确认含对象名称、作用范围、后果和明确动词；取消无副作用；不使用固定 pop 两次假设入口栈。群资料保留现有信息/成员/内容/协作/个人偏好/危险操作分组；成员预览的加/减操作不作为成员计数。

English: The design defaults separate personal/workspace scope, publishing/discussion, actual unread watermarks and manual reminders. Matrices specify who can act, what success means and how failures, partial results and navigation recover without widening authority.

## 6. 状态、权限和接口补齐

### 6.1 所有主页面的最小状态集合

| 状态 | 统一处理 |
|---|---|
| 首次加载 | 页面骨架/加载说明，保留返回动作 |
| 成功空 | 说明为什么为空；有资格时提供创建/加入/发现的一个主动作 |
| 搜索无结果 | 保留关键词，提供清除条件，不推荐跨范围资源来凑结果 |
| 刷新失败 | 同上下文且仍符合 C3 的数据可保留；保护资源无有效授权依据时显示待授权/重试，不能用消息 expire_at 证明资格 |
| 分页失败 | 原列表与游标保留，尾部重试，不替换整页、不重复插入 |
| 权限不足/已撤权 | 按 C3 在处理明确拒绝/撤权的同一状态更新中移除受保护展示和写入口；离线未知与明确拒绝分开 |
| 已归档/已解散/已删除 | 明确状态与合法后续动作；只有具备恢复能力和权限才展示恢复 |
| 提交结果未知/部分成功 | 说明哪些动作已完成，保留资源 ID/幂等键；后续恢复同一动作 |

### 6.2 权限展示原则

按 §4.2 角色矩阵呈现。客户端判据不替代服务端逐次授权；不从组织角色推断群/频道管理权，不把目录资格当作内容资格，不先建通用 RBAC。消息 expire_at 只是内容过期字段，绝不能作为 §5.2 所述有效授权快照的依据；工作区展示采用 C3/D6 的独立本地再校验预算。

### 6.3 契约工作包（目标增量，设计值见第 10 节，实施前校验兼容）

| 契约 | 增量与必须保留的边界 | 对应工作包 |
|---|---|---|
| C1 创建上下文与幂等 | 复用现有群/频道创建链；个人、工作区列表、治理入口显式归属；群初始名与 request_id，频道 request_id；结果未知可恢复 | W04 |
| C2 列表范围、真分页与本地同步 | personal/workspace 参数、旧客户端默认行为、稳定游标、每页授权和订阅落库、未读范围、全量消费者 | W07 |
| C3 撤权与缓存展示 | 区分内容过期/授权有效期/离线未知；账号/工作区/成员世代隔离；确定失效和回归判据 | W02、W08、W09 |
| C4 会话偏好与查看水位 | 置顶/隐藏/待处理各自独立，明确本机/多端范围；水位单调、来源和世代明确 | W08、W09、W10 |
| C5 错误与历史定位 | 保留成功空、拒绝、不可用、失败、结果未知；逐个调整现有 API 消费者；历史不足再扩现有接口 | W02、W03、W11 |

### 6.4 C1：创建与结果恢复

**现状与入口。** ChannelApi.createChannel 及 channel_handler:create 当前没有创建 request_id 契约；发布内容/下单已有 request_id 不能证明频道创建幂等。组织治理页也直接调用 createChannel。群治理当前“创建后改名”必须保留同一 gid 恢复，不能等待原子创建改造才修部分成功提示。

**计划契约（按第 10 节 D5；实施授权独立）。**

1. 复用现有创建路由和 Handler→Logic→DS→Repo。新 App 在首次提交前生成并保存 request_id；一次创建意图的自动重试、手动重试和进程恢复沿用该键及原载荷。切换归属或改变创建载荷须先对账旧意图，不能把未知请求改成新资源。
2. 服务端幂等身份至少为 (当前认证 uid, 创建操作类型, request_id)，载荷摘要包含 scope、workspace_id、初始名、成员集合及实际创建选项。同键同载荷只返回同一资源；同键不同载荷明确冲突且不创建；相同键不能在另一个工作区悄悄再建。
3. 资源与受理结果须形成可靠的持久提交边界；同键并发只产生一个资源。先查可复用的持久幂等实现；若不足，使用现有 PostgreSQL 的最小记录/唯一约束，不用内存锁或新通用任务平台冒充重启安全。初始群名纳入同一受理事务；后续通知不能因重放重复触发。
4. 超时后优先沿用同一 POST 重放受理结果，不为此新建平行创建 API。返回沿用现有成功 envelope 和 gid/频道 id 字段，只做已约定的增量；原始成功结果不可因刷新失败变成“未创建”。处理字段/错误名采用 D5；数值码复用现有错误注册表，不能由 App 猜测。
5. 重放仍校验当前身份、资源归属及访问资格；已撤权时拒绝读出原资源内容，不复活关系、不重复创建。持久结果保留窗口不得短于客户端允许的恢复窗口；窗口外的未知请求停止自动重建并给出可行动提示。两个窗口及最小受理事实保留规则已在第 10 节 D5 明确。
6. 旧客户端不传 request_id 时保留既有创建行为，不能宣称获得新幂等保证；新工作区创建入口只有后端能力就绪后才启用。回退 App 不删除受理记录；回退 Backend 前核对仍在恢复中的新客户端请求。

以下仅为新 App 频道创建的目标请求示例（workspace_id=123 为合成值，字段不是已上线声明）；个人入口显式 scope=personal 并省略 workspace_id。保留 visibility/access_type/join_policy 三字段，交付档位和服务端内部化规则继续生效。

~~~json
{"name":"验收频道","scope":"workspace","workspace_id":123,"visibility":1,"access_type":0,"join_policy":1,"request_id":"ux-v2-create-001"}
~~~

### 6.5 C2：范围、兼容分页与订阅对账

| 调用方式 | 推荐目标行为 | 兼容约束 |
|---|---|---|
| 旧客户端，无 scope、无真分页选择 | 继续返回原授权集合和既有响应形状 | 不静默改为 personal，不默认截成第一页；资格校验仍生效 |
| 新客户端，scope=personal | 只返回 personal；不接受与之矛盾的 workspace_id | 入口采用第 10 节 D2；不会通过客户端过滤一页来实现 |
| 新客户端，scope=workspace、workspace_id | 只返回目标工作区内有资格的结果 | 目录资格不代表已订阅、可读或可发布 |
| 新客户端，显式选择真分页 | 推荐增量参数 pagination=cursor；返回 next_cursor/has_more | 命名采用第 10 节 D5；未声明支持前不发送，不能仅凭 HTTP 200 判断支持 |
| 已有只返回 List 的消费者 | 保持旧契约，或明确改为逐页完整扫描/使用逐项权威状态 | 不在 getSubscribedChannels 内静默把“全量”改成“第一页” |

真分页须同时满足：

- 先授权过滤再分页；稳定排序采用不可变排序值加唯一 id，游标绑定账号、scope/workspace、listKind、搜索条件、排序和版本，必须校验合法性与绑定。游标错误明确失败，不回落成另一范围的第一页。
- 新协议以服务端 has_more/next_cursor 为准；has_more=false 时 next_cursor=null，true 时游标必须前进。不能继续用“页长不足 limit 就结束”覆盖权威结果；重复游标/矛盾元数据进入可恢复错误，保留原列表，禁止无限请求。
- 首次扫描固定排序上界，后续页重验当前资格；新增资源/订阅通过刷新进入新一轮，撤权对象立即排除。稳定未变集合不漏不重；不把“扫描期间新增尚未出现”当成永久丢失，也不承诺游标是冻结的权限快照。
- 每一页先按当前账号和目标上下文幂等 upsert 频道/订阅，再进行对应未读对账；翻页不能只有内存 append。本地存储失败须显示降级/重试状态，不能一边吞掉同步失败一边宣称全部完成。Provider 的生命周期/代次变化后不得写入另一个账号的库。
- 复用 ChannelService.syncSubscribedChannels 的逐项保存能力并保留现有订阅字段；部分页不代表完整订阅集合，禁止据其删除未出现的订阅。对账失败继续围绕同一页恢复，不重新制造订阅时间和用户偏好。
- syncUnreadSummary 当前遍历已有本地订阅；新增分页必须先补齐当页订阅，再正确更新未读。全局汇总与 scoped 汇总分开：缺席对象只有在“该范围完整、权威、允许清零”的契约成立时才能置零，不因 personal 页面刷新而清空其他工作区未读。
- 审查 getSubscribedChannelsPage/getSubscribedChannels 的全部调用方，包括频道发现页的订阅态、ChannelService 同步、列表和未读入口；区分 List 返回值和完整集合，不能仅改列表页。

目标分页 payload 示例（包在现有成功 envelope 内，合成 id）；订阅/管理响应须补齐真实 scope/workspace_id。新响应 personal 的 workspace_id=null；旧响应缺字段读取兼容，不能把未知归属伪造为 personal。

~~~json
{"list":[{"id":456,"scope":"workspace","workspace_id":123,"is_subscribed":true}],"next_cursor":"opaque-next-page","has_more":true}
~~~

后端兼容增量、App 显式范围、每页落库和未读回归属于同一启用边界 W07。可以先落后端兼容代码，但调用方未准备好前不启用真分页；不能把 F13、F15 和同步修复分散到互不约束的阶段。

### 6.6 C3：撤权、缓存与消息到期

**字段事实。** group_repo 的 latest_message.expire_at 直接来自 msg_c2g.expire_at，App WorkspaceConversationModel.expiresAt 据此隐藏过期消息摘要；它不是授权有效期。当前 30 秒轮询是对账机制，也不是“撤权后最多 30 秒仍有权”的契约。不得将消息 TTL、缓存年龄与资格有效期混用。

| 触发 | 推荐目标表现与可验证判据 |
|---|---|
| 收到权威撤权事件或明确资格拒绝 | 在处理该结果的同一次状态更新中清除受保护展示、关写入口并失效对应缓存；随后到达的旧请求/事件不能恢复内容；测试推进该事件回合后断言 |
| 网络失败/服务不可用，资格未知 | 与明确撤权分开；按第 10 节 D6 处理；工作区无明确授权有效期时不从失败回读/冷缓存继续恢复受保护内容，显示待授权重试；不影响另有既定离线策略的个人 E2EE 历史 |
| 若现有权威契约确有授权截止时间 | 引用实际字段和来源，到期立即失效；失败重试不延长截止时间，不能借消息 expire_at 替代。当前抽查未确认独立授权截止字段，不声称已有租约 |
| 消息 expire_at 到期 | 删除/隐藏相应消息摘要并重取合法摘要；不据此改变群成员资格，也不恢复其他历史 |
| 切账号/工作区、退出再重新加入 | 隔离 Provider/数据库写入目标；重加入使用当前成员世代/历史边界，不复用旧资格、水位或偏好；后端已有 member_generation_id/start_seq 的链要逐层核实投影 |
| App 重启、页面返回、WebSocket 重连 | 重新获取当前资格后再恢复保护页面；旧链接/旧缓存不等于权限。事件缺失时按 D6 的对账时限处理，不承诺不可观测的即时撤权 |

D6 已明确最大 30 秒本地展示预算、前后台/断网行为及无事件失效条件，绑定测试时钟；该预算不等于服务端授权租约，不为本 UX 项目造通用租约系统。

### 6.7 C4/C5：持久化、调用方与回退

- 偏好身份至少区分账号、会话来源/类型、资源及适用成员世代；工作区偏好不借个人会话行代存。新增字段默认值推荐置顶=false、隐藏=false、待处理=false，真实未读仍由水位计算；旧 unreadNum=1 无法可靠反推人工提醒，禁止凭此批量回填待处理。
- D7 已逐动作确定本机/多端范围、排序、版本冲突与退出/重加入规则；只实现本计划既定动作，不新增可配置偏好平台。
- 需要 SQLite 增量时先修改 manifest_all.dart 真源，生成派生物并过现有迁移门；需要 PostgreSQL 变更时遵循现有迁移机制，先兼容增量、明确约束/回填。测试覆盖旧库升级、重复启动、失败重试及账号/世代隔离；无数据库变更的工作包不执行真实迁移。
- 回退使用旧客户端仍可读取的增量结构，不删除旧列或偏好数据。回退服务端/关闭新增入口前处理未完成创建请求和活跃新游标；不以“回滚代码”承诺恢复旧权限或自动降级数据库。正式迁移前必须给出版本兼容矩阵和可验证回退步骤。
- 修改 ChannelApi 的 null/空列表错误语义时，搜索全部 getChannel/getMessages/createChannel 等直接及 Service 间接消费者；区分网络、业务拒绝与解析失败，逐一更新 fallback 和测试 fixtures，不能把吞错搬到 Service。沿用现有 envelope；内部堆栈不进入 UI。
- Provider 参数化同时调整生成物、监听/事件订阅、详情/列表/嵌入页及测试 overrides；路由改动核对 routes/router/barrel 的实际调用链。新增文案改 assets/i18n 真源并生成；按触及范围检查既有十语言和功能位，不手改生成文件。只在实际签名/路由改变时修改相应导出与配置。

English: Proposed contracts are additive and must preserve legacy callers. Creation retries require durable idempotency; each paginated page must hydrate local subscriptions before unread reconciliation. Message expiry is separate from authorization. D5–D7 now have concrete planning defaults in section 10; implementation still requires candidate-bound compatibility checks.

## 7. 设计系统审计与验收口径

按 ecc:design-system 的审计模式复用当前 Token，不生成另一份 DESIGN.md、design-tokens.json 或 CSS 真源。以下分数仅保留初审的结构预评作为观察起点，**不是本次视觉重测分、对比度证书或产品总分**；W12 要用实际窗口、读屏和测量结果完成审计，不沿用预分洗绿。

| 维度 / 结构预评分 | 当前落点 | 修整与验收 |
|---|---|---|
| 颜色 7/10 | `imboyapp/DESIGN.md:421` 已改 primaryDark；菜单次文字仍用固定灰 | 主按钮复用现役组件；正文字/图标逐组合实测对比度，不换品牌 |
| 字体 7/10 | `imboyapp/lib/page/channel/widgets/channel_overflow_menu.dart:213` 强指定 PingFang SC | 使用现有系统字体/FontSizeType，长名/大字不溢出 |
| 间距 6/10 | `imboyapp/lib/page/chat/chat_setting/chat_setting_page.dart:227` 散落数值 | 触及处映射 AppSpacing，页面横距按 DESIGN.md，不全仓格式化 |
| 组件 6/10 | `imboyapp/lib/page/workspace/workspace_channels_page.dart:161` 与 `imboyapp/lib/page/workspace/workspace_groups_page.dart:122` 行/卡片不同 | 同类目录用既有行/搜索/状态组件，不合并授权数据源 |
| 响应式 6/10 | `imboyapp/lib/page/workspace_shell/workspace_shell_breakpoint.dart:18` 重复断点 | 复用 AppBreakpoints；320/360/600/899/900/1200、横屏/键盘检查 |
| 暗色 6/10 | `imboyapp/lib/page/channel/widgets/channel_discussion_entry.dart:188` 静态系统蓝 | 动态语义色；浅/暗/当前支持 OLED 逐组合验收 |
| 动效 7/10 | `imboyapp/lib/page/chat/chat/chat_page.dart:974` 引用滚动偏长 | 用现有 duration/curve，减少动态效果开关下不强制长滚动 |
| 无障碍 6/10 | `imboyapp/lib/page/group/launch_chat/launch_chat_page.dart:60` 固定行高；会话已有语义动作 | iOS ≥44pt/Android ≥48dp、读屏名称/选中/错误、焦点顺序、大字可操作 |
| 密度 6/10 | `imboyapp/lib/page/workspace/workspace_channel_detail_page.dart:89` 与绑定讨论入口叠加 | 一个身份头、一个实际讨论入口，设置折叠但保留能力 |
| 状态 5/10 | F05 已部分改善，F08/F09 仍失败反馈不足 | 首载/空/搜索空/刷新失败/分页失败/撤权/归档/部分成功逐项区别 |

不把既定企业抽屉渐变或原生导航毛玻璃机械归为 AI 风格污染；不新增装饰性卡片、渐变或大标题。没有当前渲染证据，不做“全面换肤必要”结论。

## 8. 分期实施与回归边界

### 8.1 阶段依赖

| 阶段 | 工作包与范围 | 依赖与出口 |
|---|---|---|
| S0 契约与基线 | W00：核对已定 D1–D7、相关文件指纹、接口与验收配置 | 设计按第 10 节，不重复等待产品猜测；运行前置不足只影响对应包，计划授权不等于实施授权 |
| S1 已有缺陷修复 | W01–W03：状态隔离、错误传播、路由/偏好失败反馈、引用不可定位提示 | 无产品语义变化的修复可先做；W02 的工作区缓存行为按 D6；每包定向验证 |
| S2 创建与目录闭环 | W04–W06：群/频道创建、治理入口、选人/目录、讨论入口 | W04 按 D4/D5；新入口依赖 W01/W02 和后端受理；W06 不改既有讨论资格规则 |
| S3 范围、阅读与偏好 | W07–W10：范围+真分页+同步、查看语义/摘要、工作区偏好、清空/移除 | W07 按 D2/D5；W08 按 D1/D3/D6；W09 按 D6/D7；W10 按 D4/D7 |
| S4 历史与视觉精修 | W11–W12：完整历史定位、触达/暗色/响应式/键盘 | 与 S1 的基本不可定位反馈分开；不把真分页/未读同步留到此阶段 |
| S5 集成验收 | W13：最终两仓候选、全部适用旅程和强制门 | 前置包出口齐备；缺设备/身份/档位证据保留 NOT_EXECUTED/BLOCKED，不发总 PASS |

### 8.2 可独立评审的工作包

路径缩写：App 为 imboyapp/，BE 为 imboy/；所列是主要落点，不免除全调用方搜索。各包开始前补齐精确读写路径、现有测试入口和直接调用方；同一文件被多个包修改时串行集成，不把包边界误当成文件互不重叠。

W04 内先独立修复已有建群部分成功反馈，再分别完成群原子受理、频道幂等受理和 App 入口接入；W07 内可分别评审后端兼容增量与 App 全调用方迁移，但必须共用同一启用出口。子项沿用父包的相关旅程和边界，不能将整包压成一次无人审阅的大任务。

| 包 / 主类型 | F 与契约 | 主要调用方/模块及 fixtures | 出口与旅程 |
|---|---|---|---|
| W00 / plan | 全部、D1–D7 | 本文、现有权限/概念/API 文档；记录候选与受影响功能位 | 当前设计与能力/兼容表一致；固定实际运行资源/平台，前置缺口只阻断受影响包 |
| W01 / fix | F03/F04 | App channel_provider、列表/详情/嵌入页、生成物和 provider overrides | J06/J07/J08；反序完成与旧事件不得写回其他账号/目标 |
| W02 / fix | F05/F06、C3/C5 | App channel_api、channel_service 及详情/编辑/发布/订阅/事件消费者；错误 fixtures | J09/J10/J11/J29；网络与拒绝区分，旧请求不恢复撤权内容，订阅成功后回读失败只重试对账 |
| W03 / fix | F07/F08/F09、F16 基本反馈 | App conversation_page/provider/item、chat_setting、chat_page 及路由调用方 | J15/J16/J17/J18；持久化失败不假成功，定位失败可解释 |
| W04 / fix+impl | F01/F02/F20、C1 | BE 现有 group/channel Handler→Repo、app_manifest_handler/ETag 及测试；App GroupApi/ChannelApi、manifest 能力读取/来源缓存、channel_create/provider、organization_manage、入口与响应 fixtures | J01/J02/J03/J04/J05/J23/J24/J25；原子受理、归属、部分成功与重放一致 |
| W05 / impl | F10/F18 | App workspace_groups、launch_chat、实际工作区成员 API/选择器 | J01/J03/J14；搜索/失败重试/已选保持，无顺带加入授权 |
| W06 / change | F12 | App workspace_channel_detail、channel_detail、ChannelDiscussionEntry | J12；唯一合法绑定入口、不自动加入或造 General |
| W07 / change+db | F13/F15/F22、C2 | BE subscribed/managed 查询与 manifest 能力声明；App 列表/发现、ChannelService、channel_repo_sqlite、未读同步/能力解析和分页 fixtures | J08/J14/J20/J26/J27；全部范围与本地同步在启用前一起通过；不预设一定需迁移 |
| W08 / change | F11/F14、C3/C4 | BE 既有授权摘要/水位；App 频道更新行、详情可见性、后台/路由监听 | J10/J13/J21/J29；查看水位单调、来源明确，不把加载当阅读 |
| W09 / impl | F19/F21、C3/C4 | BE 会话偏好/成员世代投影；App workspace_conversations/model、个人待处理动作与迁移真源 | J10/J16/J17/J28/J29；偏好独立、跨端/世代按 D7，失败无假成功 |
| W10 / change | F17、C4 | App group_danger_zone、会话清空/隐藏/退出调用链与现有 BE 软删除/转让契约 | J19/J28；四种后果分离，回退不破坏成员/记录 |
| W11 / impl | F16 完整定位、C5 | 既有历史 API、conv_seq 与成员世代边界、App 引用定位及历史 fixtures | J18；目标可授权时定位；过期/无权/密钥缺失分别解释 |
| W12 / change | 第 7 节设计修整 | 触及 UI、Token、文案/语义标签；复用现有组件，不全仓换肤 | J22；窄屏/大字/明暗/键盘/读屏各有平台证据 |
| W13 / test+review | 全部 | 最终两仓候选、交付档位/功能位、适用单元/Widget/契约/设备门 | J01–J29 逐项结果、命令/退出码/执行数/行为断言/证据引用，无历史结果替代 |

执行基线（2026-10-27）：Backend worktree `/Users/leeyi/project/imboy.pub/.worktrees/channel-group-ux-v2.1/imboy` 从 main `313016123e735dd3c305f173422a9dac79462469` 创建；App 同级 `imboyapp` 从 main `b6afea5842461acacc7b7b6861961875a920198c` 创建，两仓分支均为 `codex/channel-group-ux-v2.1`。源工作树 WIP 未带入；本计划及 V1 按原字节复制至 Backend worktree。实施授权不含 commit/push/合并/部署。

W00：IN_PROGRESS，已隔离两仓、核对本仓规则与本机 Flutter/Erlang 入口，初始化固定 App 子模块 `7707c7bfbebc1fb273b851daebf778acfdb92a25` 并解析锁定依赖。使用仓库 bootstrap_local_config.dart 创建公开 loopback 占位环境，生成环境及 Provider 派生文件；不复制真实凭据。后端数据库/运行资源及最终平台验收前置仍待核定。

W03：IN_PROGRESS，正在验证群/个人头像目的地、移除失败与防重、本机免打扰的无消息持久化及失败回滚；不能定位的基础反馈尚未完成。首次定向测试被缺失环境生成物阻断（编译失败，断言未执行），补齐生成物后复跑；不得算作功能通过。

其余工作包为 TODO；第 3 节 PARTIAL 只指已有代码改善，不是工作包 PASS。责任与精确依赖如下；同文件有交叉时一个写者。

| 包 | 责任 | 必须先完成 |
|---|---|---|
| W00 | 主集成者 | 无 |
| W01 | App | W00 |
| W02 | App / Backend 错误契约 | W00 |
| W03 | App | W00 |
| W04 | Backend + App API/入口 | W00 |
| W05 | App | W03、W04 |
| W06 | App | W01、W02 |
| W07 | Backend + App 列表/同步 | W01、W04 |
| W08 | Backend + App 阅读/摘要 | W02、W07 |
| W09 | Backend + App 偏好 | W03、W08 |
| W10 | App / Backend 生命周期合同 | W03、W04、W09 |
| W11 | App / Backend 历史上下文 | W02、W08 |
| W12 | App 视觉/可访问性 | W05、W06、W07、W08、W09、W10、W11 |
| W13 | 主集成者 | W01–W12 全部退出 |

现有测试优先扩展，新增文件只在已有落点无法承载时提出；下表均为当前存在的文件/目录，不表示已覆盖或已运行。对应包完成时记录实际测试函数/用例，不能只引用文件存在。

| 工作包 | 现有测试落点（相对各仓） |
|---|---|
| W01 | App test/unit_test/page/channel/channel_provider_state_test.dart、channel_list_state_sync_test.dart、channel_detail_event_test.dart、channel_load_more_guard_test.dart |
| W02 | App test/unit_test/page/channel/、test/unit_test/service/channel_service_operations_test.dart；BE test/logic/channel_workspace_read_tests.erl |
| W03/W10 | App test/unit_test/page/conversation/conversation_authority_actions_test.dart、widget/conversation_item_layout_test.dart、test/unit_test/page/chat/chat_setting/chat_setting_page_test.dart、test/unit_test/page/group/ |
| W04/W05/W06 | App test/unit_test/integration/channel_integration_test.dart、test/unit_test/page/workspace/、test/unit_test/page/channel/；BE test/logic/channel_scope_tests.erl、group_scope_tests.erl、test/api/app_manifest_handler_tests.erl |
| W07/W08 | App test/unit_test/service/channel_service_unread_summary_sync_test.dart、channel_s2c_sync_test.dart、test/unit_test/page/channel/；BE test/logic/channel_scope_tests.erl、channel_workspace_read_tests.erl |
| W09 | App test/unit_test/store/api/workspace_conversations_api_test.dart、test/unit_test/page/workspace/workspace_conversations_page_test.dart；BE 既有群/工作区资格与成员世代套件 |
| W11/W12/W13 | App 现有 chat/channel 布局与交互测试、integration_test/；BE 受影响历史/授权/契约套件；具体平台入口由 W00 选定 |

### 8.3 验证、集成与启用

开发期间仅验证受影响模块与直接调用方；单个工作包的绿灯不等于完整产品验收。最终候选由主集成者统一执行适用全量与强制门；验证后修复或候选变化使证据失效时必要重验。不并发运行会共享 BEAM/ETS/meck/数据库或构建目录的检查，除非已有独立隔离证据。

- App 最终执行静态分析、完整适用单元/Widget 测试；若触及 SQLite，使用既有 scripts/run_sqlite_migration_gate.sh --local，并检查生成物一致性；触及功能位/交付档位则补当前强制门。W00 从仓库当前脚本/CI 绑定确切命令与前置条件，不把本表当固定不变的命令全集。
- Backend 若实施代码变更，最终执行 IMBOYENV=local make eunit 及适用类型/协议/授权/专项门，补齐该入口排除的必要套件；开发阶段按实际模块定向运行。规划/文档修订阶段不启动应用或执行真实迁移。
- 真实功能验收使用真机，不能用模拟器替代。桌面/Web 布局可用组件约束测试补充，平台清单由 W00 按实际交付确定；缺少某平台证据要保留缺口，不用 Android 代替全部平台。
- 新后端兼容能力先就绪，再启用显式范围/真分页/创建恢复；未知能力或回退组合按 C1/C2 降级。W04/W07/W09 涉及持久化时必须有升级/回退证据；不能因为单个 UI 已完成就开启不完整链。
- 每个 J 记录两仓 SHA、相关 WIP 摘要或干净候选、平台/版本、交付配置、角色/世代、命令/执行方式、退出码/执行数、行为断言和证据引用。人工设备步骤的退出码/自动执行数可标不适用，但必须给出观察和操作记录；失败/跳过/未执行分别保留。不为每轮检查新增过程报告，复用项目既有证据存放机制。
- 发消息、邀请/移除成员等会影响第三方的真实操作，执行前确认专用测试身份和范围。本文未授权 commit、push、合并、部署、生产迁移或第三方操作。

当前已核实的后续入口：App 定向可用 `flutter test test/unit_test/page/channel/channel_detail_actions_test.dart --no-pub`；最终 `dart analyze lib`、`flutter test --no-pub`、`dart run scripts/check_boundaries.dart`。仅在依赖已就绪时使用 --no-pub。Backend 定向 `IMBOYENV=local make eunit t=channel_scope_tests`；最终 `IMBOYENV=local make eunit`、`make dialyze-check` 并按当前 CI 补排除集合/适用契约架构安全门，不抬告警基线。W00 核执行工具链与环境后绑定具体 argv；本轮没有运行这些业务检查。

English: Work packages are review and verification boundaries, not autonomous execution authorization. Enablement depends on complete backend/client contracts; final gates and real-device journeys must reference the same candidate.

## 9. 最小验收旅程（J01–J29 当前全部 NOT_RUN）

| 编号 | 条件与动作 | 必须看到的结果 |
|---|---|---|
| J01 | 企业工作区群列表 → 建群 → 选本工作区成员 → 提交 | 请求归属正确，进入同一群聊，个人域无误建资源 |
| J02 | 企业壳全局“发起聊天” | 在开始选择前明确其个人域语义；不暗示工作区建群 |
| J03 | 工作区 guest/无资格用户经按钮与深链尝试创建 | 界面解释限制，服务端拒绝；无半成资源 |
| J04 | 建群成功但改名失败/创建响应超时 → 重试 | 保留原 gid；改名仅重试改名，未知创建沿用原 request_id；并发/重启恢复不重复建群 |
| J05 | 个人创建页、工作区列表创建页、组织治理页分别创建频道 | 三个入口的请求/响应归属、表单策略、目标列表与回跳一致；治理页不能误建 personal |
| J06 | 订阅/管理 Tab 快切，反序完成请求，期间收到订阅事件 | 当前 Tab 只显示自身集合，滚动与查询条件不串 |
| J07 | A 频道 → B 频道 → 返回 A，A 请求晚返回，随后发布 | 标题、内容、评论、发布目标始终同一频道 |
| J08 | 登录切账号、切工作区、打开外部频道卡片 | 不残留旧上下文，不凭卡片自动切全局空间，不绕过权威授权 |
| J09 | 频道详情/消息请求分别返回 503、403、404 | 网络失败、拒绝、不可用与成功空可区分；不把失败当零内容 |
| J10 | 页面打开时撤权；旧响应随后返回；返回/重启/离线重入/重新加入 | 按 C3/D6 逐触发验收；明确拒绝的同一状态更新移除保护内容与写入口，旧响应不得恢复；新世代不承继旧资格 |
| J11 | 订阅已成功但回读失败 → 重试 | 说明已发生的订阅，完成对账；不误导再次购买/重复加入 |
| J12 | 有绑定/无绑定/绑定变更/已加入讨论群 | 有唯一准确讨论入口；不盲跳 General，不自动加入，不重复确认已加入 |
| J13 | 频道后台预加载/遮挡/前台阅读/阅读中来新帖 | 未读按所定查看语义推进，旧内容阅读位置不被抢走 |
| J14 | 工作区群/频道目录为空、搜索无结果、刷新失败 | 各自文案和下一步准确，刷新失败不显示成功空 |
| J15 | 个人 C2C 与 C2G 分别点头像、点整行 | 资料对象和聊天对象正确，返回原列表与位置 |
| J16 | 新会话还无消息时切免打扰；模拟本地写入失败 | 成功可回读；失败回滚且不提示成功；按定义核实另一设备行为 |
| J17 | 置顶、免打扰、标已读/待处理、移除，模拟服务端失败 | 各状态独立，失败不假成功，未读水位不倒退 |
| J18 | 发送/上传失败、重试、撤回、编辑、引用未加载旧消息 | 反馈准确、重试不重复、时限有效；不能定位有说明/合法恢复 |
| J19 | 群主离开、普通成员退出、清空本机、从列表移除 | 四种后果清楚，取消无副作用，返回栈和剩余权限正确 |
| J20 | 群/频道超过一页，搜索尾页对象；空页/末页/重复游标/并发新增移除 | 同一扫描稳定未变集合不漏不重；集合变化通过刷新核对，每页实时授权；范围真实，元数据错误不无限请求，见 C2 |
| J21 | 已归档工作区/频道与来源托管群 | 正确只读/不可用，只有真实支持恢复的对象出现恢复动作 |
| J22 | 320/360 窄屏、横屏、键盘、大字、明暗、读屏/桌面键盘 | 主动作可达、无溢出/遮挡、焦点/标签清楚；独立记录真实平台证据 |
| J23 | 频道已创建但响应丢失；同载荷同 request_id 并发重试、重启恢复 | 仅一个频道，恢复相同 id/归属；不重复执行创建后副作用；刷新失败仍显示创建已受理 |
| J24 | 同 request_id 改频道名/成员/工作区；恢复窗口到期后再尝试 | 不创建第二资源；冲突/过期按 D5 明确反馈，保留未知结果，禁止静默换键重建 |
| J25 | 创建成功后撤权，再重放原请求 | 不返回无权内容、不复活关系、不重复创建；当前身份授权仍有效 |
| J26 | 新空本地库，合成 53 个订阅、limit=50、尾页有未读；落库/汇总分别失败再恢复 | 第二页订阅行与未读正确；已有偏好保留；不因 partial page 删除/清零其他范围；失败可见且可恢复 |
| J27 | 旧客户端无 scope/分页参数；新客户端跨 personal/workspace；错范围/重复游标 | 旧授权集合和响应兼容；新范围准确；has_more/next_cursor 一致；未知能力不误启真分页 |
| J28 | 旧 SQLite/服务端数据升级、新偏好默认值、切账号/重加入、App/Backend 回退组合 | 无需从 unreadNum 猜人工提醒；升级幂等、数据保留、世代隔离；回退不丢偏好、不误恢复权限/未知创建 |
| J29 | 消息 expire_at 到期、明确撤权、断网回读、授权截止/事件缺失分别推进测试时钟 | 内容过期不冒充撤权；无授权截止字段不伪造租约；依 D6 的精确触发/时限断言缓存与写能力，不以轮询频率证明安全 |

不采用“一个总 PASS”覆盖全部场景。每个旅程绑定候选 SHA、平台、交付配置、角色与结果；失败、未运行、跳过保留。历史测试通过或组件测试不能证明本轮产品已完备。

## 10. 计划采用的设计与兼容默认值

D1–D7 均为本轮授权收口后的**计划设计输入**，不是已上线事实或业务实施授权。无需下一位执行者再次猜产品方向；若发现与用户当前明确裁决冲突，保留已有行为并指出具体冲突。普通实现细节由责任者按现有架构解决，不为每个可逆修改重复申请批准。

| 决策 | 计划采用的具体规则 | 实施出口 |
|---|---|---|
| D1 企业消息范围 | 当前工作区群消息 + 独立频道更新入口；目录和更新区分；企业托管/客服继续独立；不新增统一收件箱 | W08，不混三个会话真源 |
| D2 个人频道范围 | 新客户端个人页仅 personal、工作区页仅指定 workspace；无 scope 的旧请求保留原授权集合；未知 scope 不当 personal，外链不自动切空间 | W07 的兼容/本地同步/未读共同通过后启用 |
| D3 查看语义 | 频道叫“已查看新内容”：前台未遮挡且可见目标推进水位；预加载/后台/仅进入不清零；显式标已读单独执行；新内容不抢滚动 | W08；人工待处理与真实未读分离 |
| D4 创建与离开 | 发起群聊一直建群，选一人不偷改单聊；成功直达、完善资料非阻塞；清空本机保留入口，移除仅隐藏，群主可转让后退出或单独解散 | W04/W10；保留全局五项 + 菜单并明示个人域；新消息自动复现沿用当前服务端合同，不另改政策 |
| D5 幂等与协议兼容 | 采用下方 D5 细则：24h 客户端恢复、7d 完整响应、最小受理事实保留；显式 pagination=cursor；manifest 版本识别、终止游标及错误名固定 | W04/W07，全部旧调用方保持可用；新增字段均为目标合同 |
| D6 撤权/缓存 | 采用下方 D6 细则：消息 TTL 不等于授权；明确拒绝同状态更新失效；工作区冷启动/返回/前台重入先授权；前台本地展示预算最多 30s、失败不延长 | W02/W08/W09；不是服务端授权租约，也不承诺不可观测的即时撤权 |
| D7 偏好与演进 | 置顶/隐藏/人工待处理按账号/来源/资源及工作区成员世代服务端版本化；免打扰保留本机合同并标“仅此设备”；新增 false，不从 unreadNum 反推人工提醒 | W09/W10，升级/回退与重新加入验证通过 |

### D5：请求、窗口、能力和分页

- 创建响应沿用既有 envelope/id/归属，新增 request_id 和 creation_state=accepted/replayed；同键变载荷返回业务错误 request_id_conflict，不产生资源。超出恢复窗返回 create_recovery_expired；数值码在现有错误注册表分配，不能先硬编码碰撞值。
- 客户端结果未知的自动/手动恢复窗口为首次意图创建后 24 小时；窗口外停止自动 POST，提示人工核对既有资源。服务端完整结果保存至少 7 天；之后可删完整响应，但保留最小受理事实（账号/操作/键/摘要/资源 ID/时间及已删除标识），直至账户生命周期依既有删除合同清理，避免旧键再次创建。重放仍须当前授权，资源删除不复活。7 天不是允许旧键重新创建的时点。
- 复用已有 GET /api/v1/app/manifest（app_manifest_handler）作增量能力声明：目标字段 channel_contracts={create_request_id:1,list_scope:1,subscription_pagination:1}。对应能力仅在后端合同和兼容测试完成后声明；同步更新 manifest 序列化/ETag、App 解析与旧 fixtures，版本变化参与缓存失效；App 按当前服务器来源绑定声明，缺字段/未知版本走旧模式，不能凭 HTTP 200 或本机缓存开启新合同。后端回退导致不支持时保留未知创建，不自动换键重发。
- 新列表显式 pagination=cursor，终止为 has_more=false/next_cursor=null。响应矛盾或不前进报可重试协议错误，不猜下一页。personal 请求省略 workspace_id，响应统一为 null（读取旧响应缺字段仍兼容）；workspace 必须有正整数 JSON ID，App 按 EntityId 保持精度。
- 游标版本 1：绑定账号、scope/workspace、listKind、q、排序和首次扫描上界；订阅页稳定顺序用首次扫描内不可变订阅行 ID 倒序，管理/目录用不可变资源 ID 倒序，个人置顶排序由已同步的偏好在列表层单独呈现。并发新增在刷新后进入，撤权每页排除；不假称权限冻结快照。沿用现有安全游标实现，无可复用者采用最小版本化、防篡改游标，错误不得回落另一范围。

### D6：触发和时间边界

- 当前未确认独立服务端授权截止字段；不新增通用租约体系，不把消息 expire_at 或 30 秒轮询频率宣称为授权时效。
- 工作区受保护内容冷启动、页面返回、重新前台、切账号/工作区、WS 重连时必须重新授权后再恢复；网络已知断开或重新授权失败时转“待授权/重试”，不从失败回读/冷缓存恢复保护内容。个人 E2EE 已有离线历史策略保持。
- 前台可见期间，最近一次**当前上下文权威读取成功**只给本地最多 30 秒展示预算，采用单调时钟；预算到期立即撤去受保护内容并重验，不等挂起请求超时，不因失败/重试/消息到达延长。此为客户端最大再校验间隔，绝非后端授予的租约；后端每次读写仍重新授权。
- 明确拒绝/撤权事件在处理它的同一次状态更新中失效内容、缓存与写能力；旧响应/旧世代不得恢复。若未来已有权威截止字段，以更早截止为准，但须另行绑定实际字段与来源。J29 用可控时钟分别验证 30 秒到期、断网、后台和内容 TTL。

### D7：范围、合并和升级/回退

- 置顶、列表隐藏、人工待处理采用服务端按账号/来源类型/资源/适用成员世代的偏好；修改携带当前版本，服务端版本单调，冲突先回读，不用客户端时间或无限重试覆盖他端。固定排序为置顶优先、最新权威消息时间倒序、资源 ID 稳定打平。
- 免打扰本期沿用仅本机规则，界面明确“仅此设备”，按账号/类型/资源存储；不能显示“已同步全部设备”。通知抑制、会话角标和设置回读必须同源一致。日后跨端免打扰属于独立合同，不在本计划暗中实现。
- 新偏好默认 false；个人关系不因清空/移除而消失；工作区退出/撤权后的旧世代偏好不用于新世代，重加入按默认开始，历史记录不冒作当前资格。已有数据不可凭 unreadNum=1 回填人工提醒。
- SQLite 按 manifest 真源增量，PostgreSQL 按当前迁移编号增量；旧 App+新 Backend 保持旧字段/集合；新 App+旧 Backend 缺能力时保留旧只读可用路径，不启新工作区创建恢复/真分页/偏好写；回退关闭新增入口但保留受理记录/偏好列，不自动 down 数据库。新 App+新 Backend 需 J23–J29 和强制迁移门齐备。

## 11. 计划收口与后续交接

计划完成条件：全部 22 项按 §1.4 当前源重核；D1–D7 无空白设计选择；角色/动作/失败/返回与部分成功规格齐备；W00–W13 的依赖、责任、调用方、退出 oracle 可核；F→W→J01–J29 不遗漏、DAG 无环；命令/测试/视觉真源与既有交付复用边界已校验。本轮仅对这些文档条件作 PLAN_READY 判断，不要求先写业务代码。

实施从 W00 重新采样执行候选与资源开始；首批 W01/W02/W03，W04 的后端受理在无资源冲突时接力；共享文件串行写。持久化改动先兼容，未知能力不启新写入；最终 W13 核全部适用平台/角色/分支，不用静态检查代替验收，不跳过失败或取消。已有修复仅减去重复工作，不能直接关闭整包。

本次没有修改业务代码、创建子 agent/线程/worktree、提交、推送或执行第三方操作。后续关闭任何 F/W/J 都必须回写匹配候选的证据；只有全部 Required 通过才能关闭产品实施。当前只关闭“收口任务计划”这一授权目标。

English handoff: D1–D7 are concrete planning defaults, not deployed behavior. Keep all 29 acceptance journeys and the original W00–W13 IDs, reuse candidate-bound V3.2 work, preserve legacy compatibility and close implementation only with real matching evidence.
