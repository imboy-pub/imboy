# IMBoy 频道、群组与会话体验审计及改造方案 V2.2

修订：2026-10-27。**计划：PLAN_READY（限本文件确定的契约与子卡）；业务实施：PARTIAL_UNSUBMITTED；完整运行/设备验收：NOT_RUN。**

本轮仅修订计划、当前入口与静态证据，不修改业务代码、不执行写接口/迁移、不 commit/push。上一轮已在隔离 worktree 开始 W03；现存代码与测试 WIP 原样保留，不因本轮文档修订变成 PASS。V2.2 是唯一当前规范；[V1](./plan-v1.md)、[V2/V2.1 历史稿](./plan-v2.md) 保留取证，不作为另一套现行计划。保留 F01–F22、C1–C5、D1–D7、W00–W13、J01–J29；子编号只细化原义务。视觉真源仍是 App DESIGN.md。

产品边界保持：群负责交流协作、频道负责发布订阅；个人与工作区归属不可变；企业托管/客服各自真源不合并；组织不是租户；不新增通用权限、租约、偏好或任务平台。PLAN_READY 不是实施/发布授权或功能验收通过。

English: V2.2 is the only current planning entry. Preserve all original IDs and product boundaries. This revision changes documentation only; existing unsubmitted implementation and historical test receipts remain separate from runtime acceptance.

## 1. 范围、证据与结论

### 1.1 历史取证

V1（2026-10-20）与 V2（2026-10-22）的候选、外来 WIP、逐文件摘要及初审方法完整保留在 [V2/V2.1 历史稿](./plan-v2.md) §1.1/1.3。本版不重用旧行号/旧绿灯解释当前候选；当前三端以 §1.6 为准。

证据等级：A=源码直接确认，B=源码风险且表现需复现，D=目标设计，均不自动代表运行通过。P1 影响正确性/可达性，P2 影响效率/一致性；无证据不宣称线上 P0。

### 1.2 总体判断

现有基础能力较多，最需完善的是**场景归属明确、列表状态独立、动作反馈可信、离开与返回可恢复**。应优先打通已有能力，不新增一套聊天/频道体系。

- **群组**是多人交流与协作主体，群聊是其中的交流行为；**频道**是发布与订阅主体，评论针对具体内容。
- **个人/企业是体验上下文**；资源的真实归属是 `personal` 或具体 `workspace`，创建后不可变。
- 企业壳里的工作区群聊、企业托管会话、客服会话有不同数据模型，不能只凭入口都在“企业”里就合并数据源。
- 组织是业务对象，不是租户。不能从该裁决推导“工作区/群成员校验可以取消”，也不能自动推导组织管理员拥有全部资源内容权限。
- 现有群详情已把公告/文件/相册、投票/日程/作业、其他设置分组，已有角色策略、草稿隔离与部分无障碍支持，继续复用。

权威解释：`docs/architecture/2026-10-12-organization-positioning-amendment.md` §1–3；`src/logic/group_logic.erl:788`、`src/logic/channel_logic.erl:374` 的不可变归属守卫。`docs/glossary.md`、`docs/concepts/collaboration-hierarchy.md` 与 App `CLAUDE.md` 的部分租户措辞仍过时，实施时应同步纠正这些权威文档，不能借本次 UX 改造扩大授权。

### 1.3 V2 历史说明

V2 补充了组织治理创建、群与频道创建恢复、分页同步、消息 TTL 与授权分离、旧客户端兼容。全部属于目标合同，历史候选见旧稿，不声称已支持。

### 1.4 V2.1 历史说明

V2.1 曾按 Backend 31301612 / App b6afea584 复核为 20 OPEN、2 PARTIAL；其完整记录和关键文件摘要保留原稿。V2.2 对 22 项重新核对默认分支并分列新 W03 WIP（第 3 节），不把历史缺口数量直接当当前状态。与交付 V3.2 重叠仍先查候选/授权/输入/行为断言再复用，不继承提交授权或平台豁免。

### 1.5 企业 UIUX V1.1 的既有实施与复用边界

`_task_packages/IMBoy_Enterprise_UIUX_V1_1/TASKS.md` 的 plan_id 为 `imboy-enterprise-uiux-v1`，与本文历史 V1/V2 不是同一计划。其范围只含 Flutter 展示、局部展示组件、文案和测试，明确不改 API/DTO/Provider/Service/Domain、权限和分页；P1 是企业频道详情，P2 是工作台/OA 入口，P3 是客服坐席列表。不能用该包完成替代本文群/频道归属、创建恢复、授权失效、分页、已读和会话持久化工作。

该包已实际实施。App 当前 HEAD 包含 `047d94b4e`（P1/P3 场景及图标字体修正）和 `3a80c6db1`（合并后收口）；源码可见唯一频道标题、筛选默认折叠等改动。STATUS.md 声称的原 merge `32dd2e919` 不是当前 HEAD 祖先，故以当前可达实施提交判断已集成，不直接照抄历史合并拓扑。执行记录入口：`imboyapp/docs/plans/enterprise-uiux-v1/execution/STATUS.md`。

- 视觉/局部交互复用：唯一标题、统计占位、动作禁用/忙态、讨论横幅方向、筛选折叠、错误空态、工作台行对齐；W12 优先复验差异，不重新整套换肤。F05 的错误呈现已有改善，但失败传播/尾部分页恢复仍未闭合；F12 的箭头改良不等于绑定群/私聊目的地正确。
- 历史自动证据：FINAL_GATES.md 记录候选 `d397d38c8` 的目标 143/143、全量 10830/10830、零失败零跳过；后续 `95dc3cddf` 修正仅补目标/图像/analyze，全量沿用父候选。它们不是当前 HEAD 的全量通过收据。
- 本轮只读清点该包 p1/p2/p3 图像：120/72/120 张，唯一图 64/32/48，合计 312 张、144 张唯一图，与最终修正说明一致。相同图不自动等于无效覆盖；ios/android 平台轴未产生图像差异，不能冒充原生验收。
- 原生验收仍不完整：T30.a Android P1 记录 6 PASS + 1 PARTIAL，且部分角色/慢网断言未覆盖；其余 8 张原生卡及 T28.m 真实跨会话仍待验。卡片 DONE 是记账，不等于全部断言通过。
- 台账有漂移：ACCEPTANCE_RESULTS.json 停在 revision 3、candidate=null/NOT_VERIFIED；STATUS.md 前段仍有旧候选和“golden 未生成”等历史语句；运行真源已清理，不能直接运行旧恢复命令宣称续跑成功。后续若恢复该包，先对齐最终候选、人工义务和证据引用，不改静态 TASKS.md 初始 TODO 来伪装执行状态。

截至该历史复核时，本专项 V2 的 W00–W13 尚未执行；随后 W00/W03 已启动，见 §1.6。现成视觉实现是可复用输入，不计为本专项 J01–J29 已验收。W00 先做上述证据对齐，W12 只补剩余视觉/可访问性差异；P2 OA、P3 客服专属工作继续留在原包，不扩入本文业务范围。

### 1.6 V2.2 三端基线、PR 与工作区隔离（R01/R16）

2026-10-27 14:36:59 +08:00（06:36:59Z），对各仓 `origin` 执行 `git ls-remote --symref origin HEAD`，三次 exit=0；自动发现默认分支均为 main，没有用工作目录名字猜仓库。主仓与 worktree 属同一 Git common-dir。SHA 为采样时点，后续变化须记录差异，不能悄悄替换本表。

| 端 | 真实主仓 / origin | 远端默认分支最新 SHA | 本地 main / 本任务 HEAD |
|---|---|---|---|
| Backend | `/Users/leeyi/project/imboy.pub/imboy` / `git@gitee.com:imboy-pub/imboy.git` | main `313016123e735dd3c305f173422a9dac79462469` | main `7fbf3a1ac0c7ffa9ca502d3e55365fbf6a17e222`；任务 HEAD `31301612` |
| App（含 Web 消费链） | `/Users/leeyi/project/imboy.pub/imboyapp` / `https://gitee.com/imboy-pub/imboy-flutter.git` | main `b6afea5842461acacc7b7b6861961875a920198c` | main 与任务 HEAD 同远端；任务工作树有 9 个 W03 修改文件 |
| Admin | `/Users/leeyi/project/imboy.pub/imboyadmin` / `git@gitee.com:imboy-pub/imboy-admin-frontend.git` | main `2c3b54283ed970513f08fc3d4075e4617c04ef85` | main/HEAD 同远端，工作树干净；本任务无 Admin worktree |

Backend 本地未推送增量只有 `7fbf3a1a`，涉及审核消息不透明 ID、迁移 230、对应测试及 TSID 文档，共 6 文件；与 F 所引群/频道/会话源码无交叉，但 R10 必须保留“不透明消息 ID 不转整数”的约束。本轮未把它合入任务分支或执行迁移。主仓还有既有未跟踪资料，排除在业务候选外，不清理。

同轮只读查询 Gitee API `/repos/imboy-pub/{repo}/pulls?state=open&per_page=100&page=1` 与 `gh pr list --state open --limit 100`：三仓各两处均返回空列表；因此本次未发现这两托管站的未合并 PR。Gitcode 镜像 PR 未查询，不把镜像未知状态推定为已合并或无差异；执行前以 origin + 选定候选重核。PR 列表不是本地分支已推送的证据。

本节静态采样时目录为 `.worktrees/channel-group-ux-v2.1/{imboy,imboyapp}`；此后用户明确授权删除旧分支/worktree、重建 `codex/channel-group-ux-v2.2`，已按 §11.1 记录迁移，不把这次显式更换候选当作静默重基线。9 个 App WIP 的 SHA256、22 项发现所引源码的默认分支 SHA256、采样结果见 [静态证据](./evidence-v2.2.json)。恢复时先比对这些输入；外部变化先标受影响断言 STALE，再决定是否需显式重基线，不自动 reset/clean/stash。

上一轮定向五文件测试历史 exit=0、40/40；末次占位字段精简后受影响三文件增量 exit=0、19/19，在本轮开始前启动、本轮仅收取结束状态。它们只支持 W03 的局部实现，不代表本轮执行了业务测试，不是远端 main 或 J01–J29 的总 PASS。曾出现缺生成物编译失败和共享 Future 完成回调循环等待；均已修正后才有上述结果。完整全量与设备门未执行。

#### 三端消费者审计结论

| 面 | 实际源码与接口 | 影响结论及验收义务 |
|---|---|---|
| 群/频道治理 | Admin `src/modules/{groups,channels}/api/public.ts`、对应 Group/Channel List/DetailPage；调用 ADM `/group/list/detail/update/dissolve`、`/channel/list/detail/:id`，不是 App 的创建/订阅清单接口（Admin 基址为 /api/adm） | 经核查不强制改 Admin 业务代码。W04 只增 App 创建合同、W07 保留 ADM page/size/keyword envelope 时可无代码差异；共享 Logic/Repo 改动仍必须跑 Admin 契约回归 |
| 工作区归属/角色 | Admin list params 有 preset/organization_id/workspace_id；Channel 有 scope/workspace_id，讨论面板仅 personal；useAdminPermission 负责管理角色，Backend 再次裁决 | 不向 Admin 注入 App 工作区成员角色，不给组织管理员额外内容权。W04/W07/W10 修改投影时复验；若现有字段/错误形状变化，须最小同步对应 DTO/调用方，不能宣称无影响后跳过 |
| 归档 | `src/services/api/workspaces.ts` 已有 archiveWorkspace/restoreWorkspace；频道编辑 status 不等于资源归档，App 治理面的归档合同不得映射成 Admin status 开关 | 保留现行能力边界。回归已归档资源的只读/拒写与真实恢复动作；不为凑三端代码量添加频道归档 UI |
| ID/manifest | `src/services/api/client.ts:60` 在 JSON.parse 前使用 safeParseBigIntJson；`src/services/api/features.ts:50` 校验编译 manifest hash/schema，读取 `/admin/config/features` | R10 补原始 wire→路由→请求字节验证；新增合同字段只改变 ETag/合同版本，不任意改编译功能 hash 导致旧 Admin 拒绝加载 |

## 2. 已有能力与四种场景

| 场景 | 当前已有能力 | 主要缺口 |
|---|---|---|
| 个人群组 | 好友选人建群、群列表/发现、成员角色、公告、文件/相册、投票/日程/作业、昵称/备注、免打扰、退出/解散 | 建群页缺搜索与加载失败恢复；群头像跳转错误；搜索只覆盖已加载项；清空与移除语义交织 |
| 工作区群组 | 服务端 scope/成员约束、已加入群列表、授权消息快照、组织治理页建群/改名 | 日常建群入口与治理入口脱节；群列表缺搜索/主动刷新；会话行缺管理动作；创建后改名失败的恢复路径不足 |
| 个人频道 | 创建、已订阅/我管理、发现、发布、评论、订阅/邀请、付费能力受功能位约束、分享/二维码/文章导航 | 两 Tab 共用列表状态；详情共用状态；错误可能显示为空；阅读与已读语义不精确；归属展示与列表范围不完整 |
| 工作区频道 | 服务端内部化、工作区频道列表、嵌入式内容页、治理侧归档/恢复、分享授权入口 | 通用创建页/API 不传工作区；列表仅目录信息；通用讨论引导与实际绑定讨论入口重叠；没有明确“我的订阅/本工作区目录”区分 |

已有聊天能力包括引用、编辑、转发、撤回、失败重试、@、禁言状态、历史失败提示与部分定位能力；“代码存在”不等于每种消息类型/平台均通过验收。频道微社区、付费、发现、E2EE 等均要按实际交付配置判断可用性。

English: Keep groups for discussion/collaboration and channels for publishing/subscription. Reuse the existing implementations; repair scope, state ownership and recovery paths before adding features.

## 3. 22 项缺口的当前处置（逐项复核，默认分支与 WIP 分列）

证据：A=代码直接确认；B=具体用户表现待复现；D=设计变化。状态只用 OPEN（未闭合）、PARTIAL（已有部分改善）；本次无一项以运行证据关闭。优先级沿用初审：9 项 P1、13 项 P2，P1 不代表已证实线上 P0。

表内路径从聚合工作区开始；A=App、B=Backend。源引用逐项用 §1.6 远端 SHA 读取并记录摘要；下表状态描述默认分支，不能把 WIP 算成已发布修复。F04/F05 仍仅部分改善，其余默认分支仍有相应缺口；这一计数来自本次逐项核验，未直接继承旧统计。

| ID / 优先级 / 状态 / 证据 | 当前结论与剩余工作 | 当前证据 | 任务 |
|---|---|---|---|
| F01 / P1 / OPEN / A+D | 企业壳全局发起聊天仍走好友建个人群；明确个人域语义，另在工作区群列表提供 scoped 创建，保留既定五项菜单 | `imboyapp/lib/page/workspace_shell/workspace_plus_menu.dart:62`；`imboyapp/lib/page/group/launch_chat/launch_chat_provider.dart:180` | W04、W05 |
| F02 / P1 / OPEN / A | 通用创建页、Notifier、API 及组织治理 _createChannel 均不传 scope/workspace_id；治理页目前虽设私有免费仍会默认 personal，须一起修复 | `imboyapp/lib/page/channel/channel_create_page.dart:78`；`imboyapp/lib/page/channel/channel_provider.dart:715`；`imboyapp/lib/store/api/channel_api.dart:32`；`imboyapp/lib/modules/organization/presentation/organization_manage_page.dart:395` | W04、W05 |
| F03 / P1 / OPEN / A+B | 两 Tab 仍共用一个 ChannelListState，订阅事件强制刷新同一状态；Tab 之间没有独立代次/集合 | `imboyapp/lib/page/channel/channel_list_page.dart:137`；`imboyapp/lib/page/channel/channel_provider.dart:165`、`:261` | W01 |
| F04 / P1 / PARTIAL / A+B | 已新增频道与消息请求代次、旧加载锁保护；仍为无参数详情 Provider，旧 messages 可保留，叠页/返回隔离未闭合。不得重写为“完全没有防乱序” | `imboyapp/lib/page/channel/channel_provider.dart:327`、`:371`、`:450`；`imboyapp/lib/page/channel/channel_detail_state.dart:30` | W01、W02 |
| F05 / P1 / PARTIAL / A+B | 主详情链已用 getMessagesPage，失败抛错且空列表时有错误页；旧 getMessages 仍吞失败，已有列表时错误无专用恢复。须检查全部调用方及尾部分页失败 | `imboyapp/lib/store/api/channel_api.dart:394`、`:417`；`imboyapp/lib/page/channel/channel_provider.dart:450`；`imboyapp/lib/page/channel/widgets/channel_message_feed.dart:418` | W01、W02 |
| F06 / P1 / OPEN / A+B | 详情 API 仍以 null 合并多类失败，缓存回退不能区分临时离线与撤权；补类型化错误与授权失效呈现，不宣称已证明泄漏 | `imboyapp/lib/store/api/channel_api.dart:64`；`imboyapp/lib/page/channel/channel_provider.dart:414` | W01、W02 |
| F07 / P1 / OPEN；WIP 已改待验 / A | 会话头像仍统一进入联系人资料，C2G 把群 ID 当用户 ID；按对象分派 | `imboyapp/lib/page/conversation/conversation_page.dart:515` | W03 |
| F08 / P1 / OPEN；WIP 部分改善 / A | 单聊免打扰无会话行即返回、写失败只记日志，调用方仍提示成功；补持久化身份、失败回滚和读取忙态 | `imboyapp/lib/page/chat/chat_setting/chat_setting_page.dart:97`、`:412` | W03、W09.a |
| F09 / P2 / OPEN；WIP 已改待验 / A | 侧滑移除未消费远端 false；失败保持原项并解释，动作防重 | `imboyapp/lib/page/conversation/conversation_page.dart:415`；`imboyapp/lib/page/conversation/conversation_provider.dart:700` | W03 |
| F10 / P2 / OPEN / A | 工作区群目录仅已加入群，无搜索、主动刷新/创建；补日常入口，不伪称企业全部群 | `imboyapp/lib/page/workspace/workspace_groups_page.dart:44`、`:92` | W04、W05、W07 |
| F11 / P2 / OPEN / A | 工作区频道目录只有通用图标、名称简介；消息页频道筛选仍切到目录，缺更新摘要/未读/订阅态 | `imboyapp/lib/page/workspace/workspace_channels_page.dart:158`；`imboyapp/lib/page/workspace_shell/enterprise_shell_page.dart:464` | W07、W08 |
| F12 / P2 / OPEN / A+D | 常驻讨论引导跳群列表，与真实绑定讨论入口并存；“私聊”实际进个人会话列表；收敛为准确目的地 | `imboyapp/lib/page/workspace/workspace_channel_detail_page.dart:79`、`:111`；`imboyapp/lib/page/channel/channel_detail_page.dart:299` | W06、W10 |
| F13 / P1 / OPEN / A+B | 订阅/管理列表已有访问谓词，但未按所选空间分区；订阅安全列缺 scope/workspace_id。是展示范围缺口，不能误报为无授权 | `imboy/src/repo/channel_repo.erl:225`、`:275`；`imboy/src/logic/channel_logic_subscription.erl:22` | W07 |
| F14 / P2 / OPEN / A+D | 首次返回消息即标首条已读、取消监听；不验证前台可见、不跟踪后续阅读 | `imboyapp/lib/page/channel/channel_detail_page.dart:77` | W08、W09 |
| F15 / P2 / OPEN / A | 订阅频道清单仍回全量和 cursor/limit 回显。消息分页虽已改善，不能代替订阅清单分页 | `imboy/src/api/channel_handler.erl:389`；`imboy/src/repo/channel_repo.erl:225` | W07 |
| F16 / P2 / OPEN；WIP 基础提示 / A+B | 引用定位只查已加载列表，目标不存在即静默返回；补可行动反馈与合法上下文拉取 | `imboyapp/lib/page/chat/chat/chat_page.dart:936` | W11 |
| F17 / P2 / OPEN / A+D | 群清空记录还隐藏会话；群主离开仅显解散，没有就地转让后退出路径；分开后果与恢复 | `imboyapp/lib/page/group/group_detail/widgets/group_danger_zone.dart:94`、`:133` | W06、W10 |
| F18 / P2 / OPEN / A | 好友建群有索引和已选头像，但无搜索、页面错误状态/重试；失败不应等同空好友 | `imboyapp/lib/page/group/launch_chat/launch_chat_provider.dart:165`；`imboyapp/lib/page/group/launch_chat/launch_chat_page.dart:249` | W04、W05 |
| F19 / P2 / OPEN / A+B | 工作区消息 30 秒/事件刷新会显整页 loading；行无会话管理动作、模型无相应偏好字段 | `imboyapp/lib/page/workspace/workspace_conversations_page.dart:84`、`:175`、`:339`；`imboyapp/lib/store/model/workspace_conversation_model.dart:8` | W09 |
| F20 / P2 / OPEN / A | 治理建群仍为先建空群再改名；改名失败群已存在，缺继续同 gid 的恢复流程 | `imboyapp/lib/modules/organization/presentation/organization_manage_page.dart:333`；`imboy/src/api/group_handler.erl:233` | W04、W05 |
| F21 / P2 / OPEN / A+D | 手动未读仍写 unreadNum=1；标已读本地失败被忽略；人工待处理与消息水位混用 | `imboyapp/lib/page/conversation/conversation_page.dart:366` | W08、W09 |
| F22 / P2 / OPEN / A | 群/频道列表只搜索已加载项，频道搜索时关加载更多；频道旧服务端当前全量返回，不据此声称第 51 个必漏；新真分页启用须一起补完整范围搜索 | `imboyapp/lib/page/group/group_list/group_list_page.dart:116`；`imboyapp/lib/page/channel/channel_list_page.dart:207` | W07 |

F07 WIP 已按 C2C/C2G 分派头像；F08 已补无消息偏好保存与失败状态，但不能由单聊推及群 KV/频道订阅；本次额外核实群 KV 键不含账号，W09.a 必须处理这条直接消费者链；F09 已消费失败并共享在途请求；F16 仅有未加载提示，完整历史定位仍属 W11。这四项只安排差异复验，不能重复机械重写。

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
| 频道查看/聊天已读 | 频道按§6.8更新边界推进；聊天按原单调已读合同；显式标已读执行独立请求 | 失败给重试，不能把失败洗成成功；新帖/消息到达不抢滚动，清未读后人工待处理独立保留 |
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

### 6.4 C1：持久受理、恢复窗口与副作用（R02–R04）

现状：`group_handler:add` 读取 member_uids/scope/workspace_id，不读 request_id/初始群名；group_logic:add/workspace_add_tx 已用 elib_pg:with_tx 包住资源、成员和成员世代。channel_ds:create_channel_tx 已把频道、创建者订阅/管理员放同一连接。现有 enterprise_internal_idempotency_repo 的身份是 org/application 且过期会重置，**不能照搬其 TTL 重置语义**给用户群/频道创建；只复用 SQL/摘要工具与事务模式。

目标仍复用现有 POST 创建路由、成功 envelope 与 group/member_list、channel id 字段。新客户端提交前保存 `(服务器来源,账号,操作,request_id,规范载荷,client_intent_created_at)`；同键自动/人工重试不变，变更归属或载荷先处理旧意图；HttpRetryInterceptor/http_request_policy 的传输及鉴权重试也必须保留同键和 recover_only，禁止中间件另起无键请求。群同成员集允许不同意图创建多个群，不能恢复已退役 user_id_sum 去重。 群初始名称目标字段固定为 title，频道为 name；返回增量 request_id、creation_state=accepted/replayed。稳定错误符号 request_id_conflict/create_not_accepted/create_recovery_expired 分开；数值码在既有 include/error_code.hrl 注册并经现有 App 错误码生成/三端 fixtures 同步，不能由客户端临时猜码。

| 新合同 | manifest 顶层增量字段（目标，当前均不存在） | Backend 声明条件 | App 启用/缺失或未知版本 |
|---|---|---|---|
| 群原子创建/幂等/只查受理 | `group_contracts.create_request_id=1` | W04.b 事务、载荷摘要、崩溃恢复和副作用断言通过，所有承载节点一致支持 | W04.d 才发初始名/request_id/recover_only；缺失或版本≠1 不启新入口，已有未知意图只保留/核对，绝不剥离键走旧 POST |
| 频道创建幂等/只查受理 | `channel_contracts.create_request_id=1` | W04.c 同上 | 对应频道入口单独检查，不借频道能力开启群创建 |
| 列表范围/分页/完整搜索 | `channel_contracts.list_scope=1,list_search=1,subscription_pagination=1,managed_pagination=1,workspace_directory_pagination=1`；`group_contracts.list_scope=1,list_search=1,list_pagination=1` | W07.a/b，旧消费者回归及稳定查询通过 | 字段版本1的范围固定为§6.5对应接口，不能借订阅分页能力启管理/目录分页；按接口分别识别，不知道能力保留旧读路径，不声称搜索覆盖全集；App 每页落库和对账就绪后才联合启用 |
| 频道查看更新边界 | `channel_contracts.view_watermark=1` | W08 的 D3 新水位与混合旧 mark_read 适配全部就绪 | 未知能力不发送新边界，不把原 clear_unread 当新版精确水位 |
| 既定会话置顶/隐藏/待处理的版本写入 | `conversation_contracts.preference_version=1` | W09.b/c/d：现有/旧客户端写路径也参与版本推进、当前成员世代投影和回退兼容成立 | 显式目标值+expected_version；缺失能力保留既有明确可用动作，不伪造新版跨端/世代保证，不写新版字段 |

复用 app_manifest_handler:build_manifest/compute_etag 和 App AppManifest.fromMap/toMap/cache；合同增量参与 ETag，不借 generated_at 或旧缓存启用。缓存绑定服务器来源与账号；304 必须已有同来源可识别合同，否则无条件回取。功能位与合同能力都满足才启用；合同字段不是 compiled_features，保留既有 manifest_hash/schema 的含义。Admin 只消费当前需要的字段，不被迫开启 App 新协议。混合节点策略见 §10.4。

**事务与摘要。** 身份唯一键 `(认证 uid, create_group/create_channel, request_id)`。服务端先规范化并做当前资格校验，再以同一 PostgreSQL 连接/事务完成“唯一受理记录占位/锁→资源→群初始名及去重后的成员/世代（或频道创建者订阅/管理员）→受理资源 ID、摘要、server_accepted_at、成功结果→必要持久通知入队”。任何一步失败全部回滚，重放不再执行资源和通知入队。不能先提交 receipt 再创建资源；也不能丢弃 join_group 返回错误。规范形式：名称 Unicode 原值仅 trim 既定空白、不额外大小写/NFKC；成员按已验证 ID 的十进制规范值去重、移除创建者、稳定数字排序；补齐服务端实际默认选项；personal 无 workspace_id，workspace 必须合法正 ID；固定字段序列和 UTF-8 序列化后 SHA256。摘要排除 request_id、recover_only 和两端时间；同键不同规范载荷冲突，不能把“顺序不同”当新意图。

通知链静态事实：group_member_ds:join_group 用 after_commit 做缓存失效；group_member_logic 的独立 join 路径提交后 publish 域事件。after_commit 不是持久 outbox。W04.b/c 先复用现有 `src/ds/{message_ds,msg_s2c_ds}.erl`、`src/repo/msg_s2c_repo.erl` 的持久投递链；缺同事务入队接口时只补该链的 Conn 入口，不建通用任务/Outbox 平台。每个逻辑效果身份 `(uid,操作,request_id,effect_kind,target_uid)` 固定，持久消息 ID 固定；重发同投递可重复到达，但接收方不产生第二份邀请/成员动作。不得通过先对外发送再写去重记录承诺 exactly-once。W04 退出必须同时统计资源、成员、通知逻辑身份和业务效果数量。

**恢复状态表：两个时钟分工。** `client_intent_created_at` 只决定客户端是否可发“可能创建”的恢复请求；进程内用单调 elapsed，重启用持久时间，时钟倒退/无法可信计算时转只查，不能延长 24h。`server_accepted_at` 是数据库首次成功事务时间，只决定正常授权且未触发既有删除策略时完整结果至少保留 7×24h（不由客户端时间控制）。24h 是客户端派发截止，不是服务端受理有效期；排队请求第一次迟到服务器仍可首次受理一次，客户端随后用同键只查核对。

| 客户端年龄/服务端事实 | 允许请求 | 服务端结果及界面 |
|---|---|---|
| `<24h`，从未到服务器/未提交 | 原键原载荷重放，可首次受理 | 一个 accepted 资源；超时仍 unknown，不能换键 |
| `<24h`，已提交但响应丢失，受理年龄 `<7d` | 原键重放 | 授权仍有效且未触发内容删除策略时返回同 ID、replayed；删除例外见下文；不重复通知 |
| `>=24h`（包括 24h 精确边界），是否受理未知 | 仅原 POST 加 `recover_only=true`，绝不创建；或已有目录/ID只读核对 | 未受理返回稳定 `create_not_accepted`，仅表示查询时未见已提交受理，不能证明在途原请求不会迟到；已受理且 `<7d`、有权且未触发内容删除策略时返回同结果；只查操作本身不新增受理占位/资源/通知 |
| 受理年龄 `>=7d`，完整结果已清理 | 同键只查/重放都不得创建 | 最小 receipt 存在则 `create_recovery_expired`；有当前资格时只给可用于核对的 ID/状态，不再承诺原完整 body。过期结果未清理时也按过期元信息合同返回，清理时机不改变语义 |
| 已删除资源/撤权/改为无资格 | 同键不复活任何对象 | 当前授权决定可返回元数据范围；无权统一不可用、不泄露名称/成员；receipt 仍保留已受理事实，不把错误改为首次创建 |
| 查询未见受理且原请求可能仍在途 | 保留原意图和未知状态，不提供换键重建 | `create_not_accepted` 不是终态证明；旧请求迟到仍可能首次受理，只读核对不能消除竞态 |
| 已有可靠证据证明原请求不可能再受理，且用户决定重新发起 | 用户显式新建意图，生成新键 | 保留原意图与终止证据；无法证明则继续核对，不用失败 toast 暗中重发。本版未提供可证明终止的服务端封存机制，因此未知意图界面不提供换键恢复；客户端超时、断连、单次未见 receipt 或用户确认均不是终止证明 |

最小 receipt 保留 uid/操作/键/摘要/资源 ID/server_accepted_at/删除标识；账户删除按现有删除合同清理，旧账号凭据同时失效。7d 不是删键重建时点；完整响应字段含敏感内容时，删除或撤权按既有策略先于七天保留义务生效：不返回无权或已删除内容，返回不可用或允许的最小元信息；保留不含内容的去重事实，不能重建。

| 边界/故障测试 | 固定断言 | 子卡/旅程 |
|---|---|---|
| 23:59:59.999→24:00:00；设备时钟倒退/重启 | 精确边界改 recover_only；不可信时钟只查，不发可能创建 POST | W04.d / J24.a |
| server accepted 6d23:59:59.999→7d；客户端与服务器时钟偏差 | 服务端年龄独立决定完整结果，原键永不重建 | W04.b/c / J24.b |
| 提交前崩溃/成员失败 | 无资源/receipt/逻辑通知；同键可首次受理 | W04.b/c / J23.a |
| 提交后、响应前崩溃 | 一个资源、同一成员集/初始名、同 receipt；重放同 ID | W04.b/c / J23.b |
| 通知已发、确认前崩溃 | 固定消息/效果身份恢复；网络投递可重试，业务邀请仅一份 | W04.b/c / J23.c |
| 原键规范载荷冲突、资源删除、撤权、无能力 | 冲突/不可用/过期/只查结果分开；不剥键或换键 | W04.d / J24.c、J25.a |

### 6.5 C2：四类清单、完整搜索与删除对账（R07/R08）

| 列表 | 现有接口/授权范围 | 当前搜索/排序/分页 | 目标与本地责任 |
|---|---|---|---|
| 个人群 | GroupApi.page→`/api/v1/group/page`；自己的 owner/join/manager 资格 | attr/page/size；App 本地过滤已加载列表；group_repo:page 默认 created_at DESC | 原入口增加服务端 q（原 attr+scope 交集），稳定 created_at,id tie-break；保持旧 page/size。尾页可由服务端 q 查到，分页保存群基础投影，不把搜索缺席当退出 |
| 已订阅频道 | `/api/v1/channels/subscribed`；订阅 active 且当前可访问 | 当前全量，回显 cursor/limit；ORDER BY is_pinned,subscribed_at；App 本地搜索 | 新显式 scope/pagination/q，稳定 s.id DESC＋扫描上界；每页先落频道/订阅，再同步同范围未读；旧 List 消费者保留全量形状 |
| 我管理频道 | `/api/v1/channels/managed`；当前管理角色＋可访问 | 当前 List 全量；ORDER BY role,created_at；App 本地搜索 | 新显式范围/q/游标用 c.id DESC；角色只展示，不做可变游标键；旧调用仍全量。管理集合不是订阅集合，不创建订阅/清未读 |
| 工作区群/频道目录 | `/api/v1/workspaces/:id/groups`（资源目录）、相同 `/groups?member_only=1`（已加入子集，preview=1 时返回消息快照）；`/:id/channels`；workspace/member/content 各自资格 | 群目录限量按 created_at，成员消息按 id；频道目录按既有 created_at；App 当前入口群只显示已加入 | 新目录 q 在服务端授权交集查询，资源 id 稳定打平/游标扫描；成员群与可见目录分开标注。只缓存非敏感目录投影，不冒充订阅或内容授权 |

工作区接口准确路由以 `imboy_router` 委托组成及 WorkspaceApi 字面值共同核对；Admin `/api/adm/group/list`、`/api/adm/channel/list` 的 page/size/keyword/preset 不变。搜索不通过公域发现 API 替代私有目录，否则尾页私有对象不可达且可能拓宽范围。

`channel_subscription.id` 在迁移 00000003 已是 BIGINT PK；upsert_active 用 ON CONFLICT 只改 status/subscribed_at，**重新激活复用 id**。所以它能稳定排序，但不能当订阅世代或激活事件序号；扫描中重新激活属于集合变化，刷新新扫描才纳入，不为配合文案增加另一个排序 ID。频道订阅不假定已有 generation 字段。

新协议：pagination=cursor；终止 has_more=false/next_cursor=null，非终止游标必须前进；绑定账号、scope/workspace、listKind、规范 q、排序版本、上界，复用现有 enterprise_cursor_v2 的防篡改实现模式而非其授权域。关键词/范围变化取消或废弃旧代次和游标，滚动状态按列表上下文保存。稳定未变集合全扫描无漏重；并发变化要刷新，页间仍逐对象授权。无新增能力时旧清单不静默截为第一页；完整搜索未支持则如实标已加载范围，不宣称“全量搜索”。

直接消费者必须一起核对：ChannelApi.getSubscribedChannelsPage/getSubscribedChannels/getManagedChannels、ChannelListPage/Previews、ChannelService.syncSubscribedChannels/getSubscribedChannels、发现页订阅标记、未读汇总、WorkspaceApi 和群目录调用方。页落库失败保留页与重试状态；账号/代次变更不写错库。部分页只 upsert，永不删除缺席关系。

| 证据来源 | 权威范围 | 可以删除/清零什么 |
|---|---|---|
| 单页/搜索结果/失败或取消扫描 | 只证明返回的每个对象及其显式状态 | upsert 返回项；缺席不删除、不退出、不清零 |
| 完整成功的未变集合扫描 | 该账号/明确范围/listKind 的清单 | 可更新目录快照；无服务器集合修订/结束一致性证明时，仍不凭缺席删除本地订阅。当前协议无此证明，不把完成翻页当快照 |
| 显式退订成功/权威订阅失效通知 | 对应账号+频道+该订阅状态 | 移除对应订阅及其摘要/未读贡献；通知与并发新订阅竞争时回读逐项权威状态，不误删新激活关系 |
| 内容 403/明确撤权，或已删资源的权威状态 | 对应内容资格/对象 | 立即撤去保护内容/摘要和当前显示的角标贡献；订阅保留为待核对，不能把所有内容拒绝推断为永久退订；404 若隐匿存在性也不能泄漏原信息 |
| 全局/explicit scoped unread summary | 响应声明的账号、范围，且对象有显式 unread_count | 只更新显式返回值（含 0）；缺席是 unknown，不能写 0。旧 summary 无 completeness，禁止借它全清其他范围 |
| 本地尾页对象未收到事件 | 其资格尚未知 | 范围全扫描后对本地缺席项使用现有已授权详情/订阅态逐项对账，批量处理有界；无权内容不恢复。没有权威结论时保留待核对状态并从保护展示排除，不物理删关系 |

删除与未读状态不是同一字段：显示无当前资格时 badge 不贡献，但数据库未知计数不伪造为 0；重新取得资格后用权威计数恢复。若实现要按全扫描缺席清理，必须先交付同范围集合版本/fence 的最小查询合同与并发证明，否则此优化不适用，不能降低上述安全规则。J26 分别验证第二页失败后恢复、扫描中退订/重新订阅、搜索缺席不清理、尾页撤权和 scoped/global 汇总隔离。

### 6.6 C3：30 秒本地展示预算与三层缓存（R06）

消息 expire_at 来自 msg_c2g，是消息 TTL；不等于资格有效期。当前仓库未确认服务端授权截止字段，不造通用租约。预算只适用于工作区受保护内容/消息摘要，个人 E2EE 既定离线历史另按原合同。

授权响应匹配键 K=`服务器来源+账号+workspace+资源+群成员 generation_id（适用时）+请求代次`。只有 K 完整匹配、实际校验内容资格的详情/内容/工作区消息响应能刷新预算；目录、统计、通知、缓存命中和单独 manifest 成功均不能刷新。对应响应无法提供/绑定当前世代时不启受保护跨生命周期缓存恢复；不能用 workspace_member 的 joined_at 冒充不可变世代。

时间目标：成功响应到达前台时设单调 t0，deadline=t0+30s，t0+20s 发起后台再校验，保留同上下文内容且不整页 loading。有效成功到达可设新 t0；失败/超时不延长 deadline。已知断网、明确拒绝/撤权、后台/离开保护页立即撤去保护展示；返回/前台/WS 重连先校验再恢复。deadline 到达仍无有效结果必须先失效展示，再显示待授权重试；不会为了避免闪屏等待旧请求。旧截止回调须核对当前 K/当前 deadline，不能清掉已经续验的内容。

| 层次 | 撤权/到期处理 | 重新进入规则 |
|---|---|---|
| 渲染与动作状态 | 同一次状态更新移除保护文本/缩略图/写入口，列表不再贡献未读摘要 | 当前 K 内容授权成功后重建，迟到旧响应不回填 |
| 内存 | 清对应资源/账号/世代 Provider 与衍生摘要，取消订阅或推进代次 | 普通网络错误可以保留非敏感错误上下文，但不保留可渲染的失效保护内容 |
| 持久缓存 | 当前工作区消息模型为 ephemeral snapshot，不新增离线持久正文。既有频道 SQLite 保护记录设不可渲染/清除范围化内容，失败时安全标记优先，不能声称擦掉全部附件文件/系统截图 | 冷启动不能凭旧落盘内容恢复资格；个人 E2EE 数据不随工作区预算批量删除，附件仍走 AssetsService 授权 |

到期不是删除成员资格，也不自动退订；只有服务端动作才能改变成员关系。可控时钟断言见 J29.a–g，覆盖19.999/20/29.999/30秒、刷新成功/失败、迟到响应、不同代次、断网、后台、消息 TTL。正常成功刷新不得周期性清屏。

### 6.7 C4/C5：对象偏好真源、版本与完整错误消费（R09）

| 对象 × 动作 | 当前权威存储/同步范围 | 世代与本期决定 |
|---|---|---|
| 个人 C2C 免打扰 | App chat_setting→SQLite conversation.is_muted；当前无消息行会丢写，W03 WIP 已补隐藏占位行 | 账号+类型+peer，本机；不假称跨端。验证通知/角标/回读同源及首消息占位转实 |
| C2G 群免打扰（含工作区群；非禁言） | GroupDetail→`lib/service/group_notice_config.dart` 的 setNoticeDisabled 本机 KV 真源，conversation.is_muted 是投影；message 管线合并 KV | 当前键 `group_notice_disabled:$gid` 不含账号，StorageService 是共享设备 KV；目标改为来源/账号/群范围键并同步通知/投影消费者。无可信所有者的旧键保留但不自动导入当前账号，默认 false，由用户明确重新设置；群重入以既有成员世代规则复核，不用单聊 SQLite 替代 KV。群禁言 mute_until 是管理员权限，不是 DND；通用 ChatSettingPage 若 type=C2G 也必须复用同一 KV 写入及投影，不得仅写 SQLite 后声称成功 |
| 频道免打扰 | ChannelService.setMuted→本地 channel_subscription SQLite，服务端表虽有 is_muted/notifications_enabled，当前 App 未通过它写同步 | 本期保留现有本机路径，不把“服务端有列”当已接线；不新增跨端同步合同 |
| 个人 C2C/C2G 置顶、列表隐藏 | 既有 conversation_pin/conversation_delete + listMine 投影，服务端多端；新消息复现按原合同 | 复用原表/Handler，C2C 无成员世代；C2G 应按当前 group_member_generation 约束旧值，不另建平行偏好存储 |
| 工作区群会话置顶/隐藏/待处理 | 当前 ephemeral WorkspaceConversationModel 无完整偏好动作 | 同一 group ID 的归属不可变，scope 由资源推导；扩展既有 conversation 动作与投影，身份包含当前群成员 generation_id；不要把个人全局会话缓存当工作区授权快照 |
| 人工待处理（个人/工作区群） | 当前个人用 unreadNum=1 模拟，不是真实未读 | 沿用 user_setting 的既有用户设置存储承载必要的按会话 manual_pending/version 元数据；pin/delete 仍各由原表作唯一真源。不建立可配置偏好平台，不从 unreadNum 回填历史人工意图 |
| 企业托管/客服会话 | 各自已有服务端模型与 CAS | 不改这些模型/权限/动作，不把本计划 conversation 合同套到坐席 CAS |

当前 workspace_member 复用行、joined_at 可更新，没有独立不可变“工作区世代 ID”；本计划保护对象是工作区群会话，**复用 group_member_generation.id/start_seq**。workspace_ds:remove_member_tx 已级联 close_history_generation；重入群创建新 generation。工作区资格仍逐次校验；频道订阅 ID 不作为群成员世代。若消费者缺该投影，W09.b 只补这个真实 descriptor，不新造通用世代平台。

目标版本合同：在同一业务事务内读取当前身份/成员世代、锁对应用户设置版本、应用明确目标值并推进版本；新版 expected_version 不符返回 conflict+可安全回读的当前版本，旧 pin/unpin/delete/restore 无 expected_version 也走同一动作入口并推进版本。无变化的同目标重试返回当前状态，不重复副作用。现有 pin/delete 行需保持与当前世代绑定；本期选择在现有 pin/delete 行增加适用 generation_id（C2C=0，C2G=真实当前成员世代），同时更新唯一键/旧写适配；版本/manual_pending 使用 user_setting 中限定的会话元数据。旧行仅可绑定核实为 active 的当前世代，否则保留为不应用的历史。W09.d 的固定退出要求是“旧值不能用于新世代”，不得放行仅靠客户端筛选的实现。保留旧数据/回退时不 down 数据库；旧节点未适配世代读写前禁止接收新版偏好流量（§10.4）。

客户端保留 `desired_value` 和来源操作，冲突回读后若已达目标则成功，否则展示最新值与仍可执行的原目标，不自动再取反或无限覆盖另一端。超时重试发原目标，不能执行 toggle。偏好新值默认 false，真实 unread/read_seq 不依赖 manual_pending。sqlite 变更只改 manifest_all.dart 并生成，PG 增量按现有迁移链；本轮不分配抢占迁移编号、不执行迁移。

C5 全消费者要求：ChannelApi.getChannel/getMessages/createChannel 等签名改变时，逐一调整 ChannelService、详情/编辑/发布/订阅/发现/治理/分享/事件路径、provider overrides、生成物和 fixtures。区分成功空、503/超时、明确拒绝、404隐匿、解析失败、结果未知；不能把吞错搬到 Service。引用完整定位沿用历史 API/conv_seq 与成员边界，缺钥/过期/无权分别解释。路由变更核对真实 router/barrel，所有新增文案来自 assets/i18n 并生成十语言，不手改生成物。

### 6.8 C4/D3：频道“查看更新边界”的精确定义（R05）

现状：channel_logic_message:mark_as_read 的参数名是 `_MessageIdBin`，实际清整频道未读；last_read_at/unread_count 并不证明每帖已看。频道内容页排序是 created_at DESC,id DESC，置顶另列；不能把传一个 message_id 等同于按它推进水位。

本期采用**单一更新边界**，不是逐帖阅读记录。设每频道已提交发布更新的单调序列 S（编辑/置顶不分配新的发布序号），订阅当前激活周期已查看边界 V；查看更新 B 时 V'=max(V,B)，未查看更新数为当前可读、未删除/过期且 `update_seq > V'` 的发布项（作者自己的新发布不计其未查看，沿既有通知口径）。界面称“已查看至此更新/有 N 条新更新”，不称“每条均已阅读”。正常倒序首页最上方当前新更新真正可见后，允许把较早未展示更新一并纳入“查看至此”；这是产品边界语义，明确告知，不伪装成逐帖回执。

必要性判断：现有 last_message_id、created_at 和 TSID 均未证明提交顺序，且发布未读在事务外累加；不能用它们假造单调序列。本期 W08 只为频道增加最小 update_seq/频道发布计数与订阅 view_seq，复用 channel/message/subscription 现有事务锁（不改消息 ID，不扩逐帖阅读表）。发帖与序列分配、计数/水位变更必须同事务；旧帖子按当前稳定展示顺序一次性回填，旧聚合 unread_count 不可推导逐帖看过事实，迁移时已有订阅 view_seq=0（宁可提示存量更新，也不从聚合数反推已看过），既有 unread_count 保留至新版权威重算完成；不把存量未读洗零，也不补发迁移历史通知。该增量需迁移兼容门及旧 mark_read 适配后才声明 view_watermark=1。

只在“未筛选的正常更新流/显式标已查看”推进：当前账号/频道/资格/激活状态匹配，前台 route 为当前可交互顶层；至少一条对应边界项可见面积≥50%，持续500ms，用单调时钟；键盘仅在遮挡目标导致不足50%时中断，模态/分享/导航覆盖及后台立即重置停留。置顶旧帖、筛选结果、单帖跳转、背景预加载、带缺口且不在首页边界的页都不推进；页面持有的 B 固定于该次成功内容响应，不从后来未展示的新推送取最大值。显式“标已查看”单独确认当前服务端边界，可与自动查看区分。跨端旧请求使用数据库 GREATEST，晚到旧边界不倒退；退订/重新订阅重置激活阅读状态，复用订阅行 id 不代表复用旧查看资格，目标在状态变更事务递增最小 activation_version 并重置 view_seq；它是频道订阅激活版本，当前尚无此字段，旧 descriptor 的迟到写必须拒绝；不得用它给群成员世代命名。

English: Contracts remain additive. Capability checks are per operation; 24-hour client retry eligibility and 7-day server response retention use different clocks. Viewing means an update boundary, not individual read receipts. Cache budgets are local recheck limits, never leases.

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
| S5 集成验收 | W13：最终三端候选（Admin 可无代码差异）、全部适用旅程和强制门 | 前置包出口齐备；缺设备/身份/档位证据保留 NOT_EXECUTED/BLOCKED，不发总 PASS |

### 8.2 可独立评审的工作包

路径缩写：App 为 imboyapp/，BE 为 imboy/；所列是主要落点，不免除全调用方搜索。各包开始前补齐精确读写路径、现有测试入口和直接调用方；同一文件被多个包修改时串行集成，不把包边界误当成文件互不重叠。

W04 内先独立修复已有建群部分成功反馈，再分别完成群原子受理、频道幂等受理和 App 入口接入；W07 内可分别评审后端兼容增量与 App 全调用方迁移，但必须共用同一启用出口。子项沿用父包的相关旅程和边界，不能将整包压成一次无人审阅的大任务。

| 包 / 主类型 | F 与契约 | 主要调用方/模块及 fixtures | 出口与旅程 |
|---|---|---|---|
| W00 / plan | 全部、D1–D7 | 本文、现有权限/概念/API 文档；记录候选与受影响功能位 | 当前设计与能力/兼容表一致；固定实际运行资源/平台，前置缺口只阻断受影响包 |
| W01 / fix | F03/F04 | App channel_provider、列表/详情/嵌入页、生成物和 provider overrides | J06/J07/J08；反序完成与旧事件不得写回其他账号/目标 |
| W02 / fix | F05/F06、C3/C5 | App channel_api、channel_service 及详情/编辑/发布/订阅/事件消费者；错误 fixtures | J09/J10/J11/J29；网络与拒绝区分，旧请求不恢复撤权内容，订阅成功后回读失败只重试对账 |
| W03 / fix | F07/F08/F09、F16 基本反馈 | App conversation_page/provider/item、chat_setting、chat_page 及路由调用方 | J15、J16 的 C2C 向量、J17/J18 基础反馈；跨对象 DND 向量由 W09.a 接力，父 J 最终由 W13 统一判定；持久化失败不假成功 |
| W04 / fix+impl | F01/F02/F20、C1 | BE 现有 group/channel Handler→Repo、app_manifest_handler/ETag 及测试；App GroupApi/ChannelApi、manifest 能力读取/来源缓存、channel_create/provider、organization_manage、入口与响应 fixtures | J01/J02/J03/J04/J05/J23/J24/J25；原子受理、归属、部分成功与重放一致 |
| W05 / impl | F10/F18 | App workspace_groups、launch_chat、实际工作区成员 API/选择器 | J01/J03/J14；搜索/失败重试/已选保持，无顺带加入授权 |
| W06 / change | F12 | App workspace_channel_detail、channel_detail、ChannelDiscussionEntry | J12；唯一合法绑定入口、不自动加入或造 General |
| W07 / change+db | F10/F11（目录）/F13/F15/F22、C2 | BE subscribed/managed 查询与 manifest 能力声明；App 列表/发现、ChannelService、channel_repo_sqlite、未读同步/能力解析和分页 fixtures | J08/J14/J20/J26/J27；全部范围与本地同步在启用前一起通过；不预设一定需迁移 |
| W08 / change | F11（消息更新入口）/F14、C3/C4 | BE 既有授权摘要/水位；App 频道更新行、详情可见性、后台/路由监听 | J10/J13/J21/J29；查看水位单调、来源明确，不把加载当阅读 |
| W09 / impl | F08（群DND账号范围）/F19/F21、C3/C4 | BE 会话偏好/成员世代投影；App workspace_conversations/model、个人待处理动作与迁移真源 | J10/J16/J17/J28/J29；偏好独立、跨端/世代按 D7，失败无假成功 |
| W10 / change | F17、C4 | App group_danger_zone、会话清空/隐藏/退出调用链与现有 BE 软删除/转让契约 | J19/J28；四种后果分离，回退不破坏成员/记录 |
| W11 / impl | F16 完整定位、C5 | 既有历史 API、conv_seq 与成员世代边界、App 引用定位及历史 fixtures | J18；目标可授权时定位；过期/无权/密钥缺失分别解释 |
| W12 / change | 第 7 节设计修整 | 触及 UI、Token、文案/语义标签；复用现有组件，不全仓换肤 | J22；窄屏/大字/明暗/键盘/读屏各有平台证据 |
| W13 / test+review | 全部 | 最终三端候选（Admin 可无代码差异）、交付档位/功能位、适用单元/Widget/契约/设备门 | J01–J29 逐项结果、命令/退出码/执行数/行为断言/证据引用，无历史结果替代 |

W00 准入记录区分静态基线与运行验收：记录三端实际 HEAD/tree、已暂存和未暂存差异、未跟踪文件及子模块指纹；记录受影响功能位的编译清单、服务器来源及账号快照，有未知项标明未知。逐项列明各适用运行资源、平台和角色已绑定的身份或 BLOCKED 及受影响工作包、验证和启用条件；资源未就绪只阻断依赖该资源的开发、运行验证和启用，不把发现设备或记录 BLOCKED 当作验收通过。静态准入记录齐全且设计、能力和兼容表一致后，按本节出口评估 W00；不改变下方开发 DAG。准入快照不是最终候选冻结：候选、配置或资源变化时重新绑定受影响指纹并重验失效证据，最终验收仍使用同一最终候选和实际资源。

当前实施状态：W00 静态准入 PASS（见 §11.1 的资源阻断与重绑定）；W01/W02/W03/W04（含 W04.d.g/W04.d.c App 入口）代码侧收口并提交，全部新合同 manifest 未声明（新入口默认关闭）；W05–W12 代码侧已实施并提交（App：`563a1dd34` W05、`f206d1e94` W06、`ad8298f6b`/`2b4bb3bb0`/`2f093146d` W07.c/d、`732323e4f` W08.c、`6bce288b9`/`f95d7ce29`/`776d94601`/`8603006ea`/`b31476483` W09.a–c、`f7a02b38f`/`e255516ee` W10、`e8f14e90a` W11、`ec8ed92ad`/`8b2c67626` W12；Backend 对应 handler/ds/repo/logic 增量见 W07 子卡与 2026-10-28 接力节），各包定向收据见 §11.1；包级出口尚未逐一判定，W13/J 逐项验收仍 NOT_RUN（2026-10-29 起：单机真机冒烟门与合成服务端源/账号快照已绑定跑通，见 §11.1；双设备旅程、推送与生产源绑定仍 BLOCKED）。父包旧依赖表由下面的开发 DAG、文件写入串行约束和启用表统一替代；§8.1 是路线图，不再隐含另一份硬依赖。局部绿灯不关闭相关 J 项，更不关闭 W13。

### 开发 DAG、共享文件和联合启用（R11）

下表是唯一开发前置真源。父 W04/W07/W09 是子卡聚合节点，完成须全部子卡退出；不把聚合节点误当子卡启动前置。开发允许兼容代码先落、入口保持关闭。开发无依赖不代表可先启用；后面的启用条件同时约束客户端和服务端。

| 节点 | 开发前置 | 责任/范围 |
|---|---|---|
| W00 | 无 | 三端候选/资源准入 |
| W01 | W00 | 频道状态隔离 |
| W02 | W00 | 错误/保护展示 |
| W03 | W00 | 会话动作 |
| W04 | W04.a、W04.b、W04.c、W04.d | 创建父包（子卡聚合） |
| W04.a | W00 | 见下方子卡 |
| W04.b | W00 | 见下方子卡 |
| W04.c | W00 | 见下方子卡 |
| W04.d | W04.d.g、W04.d.c | 见下方子卡 |
| W04.d.g | W04.b、W01、W02 | 见下方子卡 |
| W04.d.c | W04.c、W01、W02 | 见下方子卡 |
| W05 | W03、W04.d.g | 选人/工作区群入口 |
| W06 | W01、W02 | 讨论目标 |
| W07 | W07.a、W07.b、W07.c、W07.d | 清单父包（子卡聚合） |
| W07.a | W00 | 见下方子卡 |
| W07.b | W07.a | 见下方子卡 |
| W07.c | W07.b、W01、W02 | 见下方子卡 |
| W07.d | W07.c | 见下方子卡 |
| W08 | W02、W07 | 更新入口/水位 |
| W09 | W09.a、W09.b、W09.c、W09.d | 偏好父包（子卡聚合） |
| W09.a | W03 | 见下方子卡 |
| W09.b | W00 | 见下方子卡 |
| W09.c | W09.a、W09.b | 见下方子卡 |
| W09.d | W09.c | 见下方子卡 |
| W10 | W03、W06 | 清空/离开 |
| W11 | W02 | 历史上下文 |
| W12 | W00 | 视觉/可访问性 |
| W13 | W01、W02、W03、W04、W05、W06、W07、W08、W09、W10、W11、W12 | 集中集成验收 |

| 联合启用 | 必须同时退出的义务 |
|---|---|
| 新工作区群创建 | W04.b/W04.d.g + W05 + W01/W02 相关断言；群能力=1，全节点部署一致；不等频道 W04.c 才可开发/单独验收群 |
| 新频道创建 | W04.c/W04.d.c + 频道入口资格/错误/回跳；频道能力=1，全节点一致 |
| 新范围/搜索/真分页 | W07.a–d + W01/W02 + App 全消费者、未读/删除对账、Admin 原 ADM 消费者不回归；不必等待整个 W04 创建合同 |
| 查看更新与保护摘要 | W08 + W07 + W02；D3 序列/旧清未读适配、D6 20/30秒调度、成员/激活状态同时通过 |
| 新版偏好 | W09.a–d + W03；工作区 UI 与保护缓存同时依赖 W08，个人待处理不必等待工作区页面全部完工 |
| 清空/隐藏/离开正式启用 | W10 + W03/W06 + W09.d 相关世代/迁移兼容断言；开发不必等待整个 W09 |
| 完整历史与视觉交付 | W11 历史边界 + W02/W08 保护规则；W12 的页面级修整可先开发，最终各触及调用方必须集成后再测 |
| 交付候选 | W01–W12 全部退出、W13 全部 Required 子断言及三端适用强制门通过；不是任意一包总 PASS |

共享文件串行约束：App channel_provider/channel_api/ChannelService（W01/02/04/07/08）、conversation 与 SQLite manifest（W03/09/10）、workspace 页面（W05/06/08/09/12）、manifest/ETag/错误码/i18n 生成（W04/07/08/09），Backend group/channel DS/Repo（W04/07/08/09/10）均单写者串行集成。Admin 仅在确有消费差异时触及对应 public.ts/页面，不并行覆盖。F11 的 W07 只负责目录范围/完整查询，W08 负责更新摘要/消息入口；两者联合验收 J13/J14/J20，不能在任一包单独关闭 F11。

### 可验收子卡（R12）

读写清单用 A=App、B=Backend、M=Admin；同名多个文件以现有目录限定。每卡共同只读 DESIGN.md、glossary、现行权限/部署规则及三端消费者；共同禁止保留区、真实凭据、生产数据、无关功能、Redis、新通用权限/租约/偏好/任务平台。所有涉及持久化的回退只关闭新入口/协议且保留数据，绝不自动 down DB；不改的端只审计/回归。

| 子卡 | 读写文件/边界（未列出的消费者先搜索再纳入直接影响） | 定向验证/固定退出 | 回退与禁止范围 |
|---|---|---|---|
| W04.a | A organization_manage_page.dart `_createGroup`/GroupApi，读取群编辑响应 | 既建 gid 保留；改名失败只重试改名；J04.a，扩展现有组织治理/GroupApi 测试 | 可退回旧展示但保留已建 gid；不改创建事务/群成员策略 |
| W04.b | B group_handler/group_logic/group_ds/group_repo、group_member_ds；拟新增 B src/repo/resource_create_receipt_repo.erl（只支持本计划两类创建）、现有 migrations；app_manifest_handler；读 msg_s2c 投递链 | J01/J03/J04.b/J23.a–c/J24；群/初始名/成员/receipt/通知持久身份同事务，崩溃后重放不重效；group_scope_tests＋拟新增同目录 group_creation_recovery_tests | 先关闭 group_contracts 新入口；原 receipt 保留；不恢复 user_id_sum，不改同成员集多群政策 |
| W04.c | B channel_handler/channel_logic/channel_ds/channel_repo、同一拟新增 resource_create_receipt_repo.erl 的频道创建受理、manifest/migrations；读 channel_subscription/admin 和 msg_s2c | J05/J23/J24/J25；频道与创建者身份/receipt/通知一致；channel_scope_tests＋拟新增同目录 channel_creation_recovery_tests | 单独关闭频道创建能力；不牵连已合格群创建 |
| W04.d.g | A GroupApi、launch_chat、organization_manage、workspace_groups、AppManifest parser/cache/fixtures、必要创建意图本机持久化 | J01–J04/J08/J24.a/J27；24h 后只查，无能力不剥键；精度断言 J27.d/e | 关工作区新入口、保留旧意图；不更改个人选一人仍建群语义 |
| W04.d.c | A ChannelApi/CreateChannelNotifier/channel_create/organization_manage、manifest/意图持久化、直接消费者与 fixtures | J05/J11/J23/J24/J25/J27.d/e；三入口归属、同键恢复一致 | 关新创建入口，保留订阅/旧只读；不删未知意图 |
| W04.d | 只聚合上述两个 App 接入子卡与直接调用方 | 群/频道各自退出，整体父卡才 DONE | 无额外代码或第二套协议 |
| W07.a | B channel_handler/channel_logic_subscription/channel_repo/subscription_repo，group_handler/group_repo，workspace_handler/group_repo；app_manifest_handler；读 M public.ts | 旧 envelope/ADM page,size,keyword 不变；J20/J27.a/b；现有 scope/subscription/manifest 套件增量 | 能力保持关闭；禁止把旧全量接口默认截为50 |
| W07.b | 同上四类清单的 q/排序/游标查询和 enterprise_cursor_v2 复用 | J20.a–d/J27.c；尾页搜索、关键词换代、游标绑定/前进/激活竞争 | 拒绝新游标并同范围重扫，不能变 scope 或猜 offset |
| W07.c | A ChannelApi/GroupApi/WorkspaceApi、列表/发现/Previews、ChannelService；受影响模型/HTTP raw ID 解码/route 与 fixtures；M 只需回归时不改 | J14/J20/J27；全部 List 消费者无第一页伪全量，ID 原始 wire 来回不变 | 按接口关新协议，未知意图不降级写；不全仓改 ID |
| W07.d | A channel_repo_sqlite/ChannelService/未读同步/事件处理与账号数据库绑定；B 显式订阅状态/summary 投影 | J26.a–e；每页先落订阅，部分页/搜索不删除，撤权移除展示、计数 unknown≠0 | 停新扫描/写游标，保留已落页与既有偏好；不按缺席批删 |
| W09.a | A conversation_page/provider/model、group_notice_config.dart/GroupDetail/message.dart 的 DND 键与投影、必要 SQLite manifest；B 现有 conversation_handler/logic、user_setting 的 manual_pending 元数据 | J16.a/b、J17.c/J28.a；DND账号键隔离，人工待处理独立于 unread；明确目标 true/false，旧 unreadNum 不推断 | 关新待处理动作，保留数据；不改变真实水位 |
| W09.b | B conversation_pin/delete/logic/handler、user_setting 版本/世代元数据、workspace/群世代只读投影；A workspace_conversations/model/API | J17.a/b/d/J28.c；当前 generation、真源存储、工作区资格一致 | 暂停新偏好入口，不把 workspace snapshot 落入个人缓存 |
| W09.c | 上述现有写动作与 S2C/查询消费者、conflict fixtures；不建通用事件平台 | J17.d/e；旧写也推进版本，冲突回读保留 desired_value；超时重试非toggle | 冻结新 CAS 写并保留当前版本；不能回退到不识别版本的旧节点 |
| W09.d | A manifest_all.dart 及生成物、B 现有迁移目录中的最小偏好/世代增量、三端混合版本 fixtures | J28.a–f；新旧数据默认值、账号/重入、旧写/回退组合全证据；明确 pin/delete 与世代元数据的原子关系 | 仅兼容版本回退，保留列/旧世代元数据；不迁移真实生产数据、不自动清除旧偏好 |


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
- Backend 若实施代码变更，最终执行下表的 eunit-local（内部调用 make eunit）及适用类型/协议/授权/专项门，补齐默认入口排除的必要套件；开发阶段按实际模块定向运行。规划/文档修订阶段不启动应用或执行真实迁移。
- 真实功能验收使用真机，不能用模拟器替代。桌面/Web 布局可用组件约束测试补充，平台清单由 W00 按实际交付确定；缺少某平台证据要保留缺口，不用 Android 代替全部平台。
- 新后端兼容能力先就绪，再启用显式范围/真分页/创建恢复；未知能力或回退组合按 C1/C2 兼容退路（未知创建绝不降级成无键写）。W04/W07/W09 涉及持久化时必须有升级/回退证据；不能因为单个 UI 已完成就开启不完整链。
- 每个 J 记录三端 SHA、相关 WIP 摘要或干净候选、平台/版本、交付配置、角色/世代、命令/执行方式、退出码/执行数、行为断言和证据引用。人工设备步骤的退出码/自动执行数可标不适用，但必须给出观察和操作记录；失败/跳过/未执行分别保留。不为每轮检查新增过程报告，复用项目既有证据存放机制。
- 发消息、邀请/移除成员等会影响第三方的真实操作，执行前确认专用测试身份和范围。本文未授权 commit、push、合并、部署、生产迁移或第三方操作。

当前已核实的后续入口：App 定向可用 `flutter test test/unit_test/page/channel/channel_detail_actions_test.dart --no-pub`；最终 `dart analyze lib`、`flutter test --no-pub`、`dart run scripts/check_boundaries.dart`。仅在依赖已就绪时使用 --no-pub。Backend 定向 `IMBOYENV=local make eunit t=channel_scope_tests`；最终 `IMBOYENV=local make eunit`、`make dialyze-check` 并按当前 CI 补排除集合/适用契约架构安全门，不抬告警基线。W00 核执行工具链与环境后绑定具体 argv；本轮没有运行这些业务检查。

English: Work packages are review and verification boundaries, not autonomous execution authorization. Enablement depends on complete backend/client contracts; final gates and real-device journeys must reference the same candidate.


### 8.4 真实验证入口、资源与证据失效（R15）

以下只读核对了路径/目标/CI，**本轮未执行这些业务命令**。A/B/M 工作目录必须是 §1.6 相应候选；Admin 未修改时仍可用固定 SHA 的只读源确认与兼容定向检查，任何需生成输出的最终门在独立验收目录运行，避免污染共享主仓。日志在仓外 `/tmp/imboy-ux-v22-validation/<repo>/<candidate>/`；执行时才创建，命令前记录工具版本、候选/dirty 摘要/功能位/配置输入摘要，捕获真实退出码，pipe 到 tee 必须 pipefail。秘密只记录来源与脱敏摘要。

| 阶段/端 | 工作目录与真实命令 | 工具/资源、固定适用条件 |
|---|---|---|
| 开发 B 定向 | Backend；`IMBOYENV=local make eunit-local t=channel_scope_tests`（替换为实际受影响套件）；`IMBOYENV=local make compile` | OTP 28；Makefile 以 t 进入受控 EUNIT_TEST_SPEC；现有 config/sys.local.config 可用时使用默认，否则显式 EUNIT_CONFIG=config/sys 并按 CI 物化隔离配置；TEST_WITH_DB 仅用于需真库的套件。禁止无意连接共享数据库 |
| 开发 A 定向 | App；`flutter test --no-pub test/unit_test/page/channel/channel_provider_state_test.dart` 等子卡对应文件；`dart analyze <本卡实际受影响文件>` | CI Flutter 3.47.5 与锁文件；bootstrap_local_config.dart/生成环境、固定子模块、SQLite ffi、必要 vodozemac 原生路径为前置；无设备单元与需设备 integration 分开 |
| 开发 M 定向 | Admin；`bun test --isolate src/modules/channels/api/channels.test.ts src/modules/groups/api/groups.test.ts src/services/api/workspaces.test.ts src/lib/safeParseBigIntJson.test.ts`，视触及范围再补相关页面 | CI Bun 1.3.14，保留原 Admin 权限/preset/分页/ID raw wire；裸 bun test 不合格 |
| 最终 B 全量/类型/契约 | `IMBOYENV=local make eunit-local`；`IMBOYENV=local make dialyze-check`；`make rest-contract-check`；`bash scripts/check_server_zero_crypto.sh`；触及边界再 `bash scripts/check_module_boundaries.sh` | backend-ci.yml 全量口径与 eunit-local→make eunit 一致。隔离 PostgreSQL 18（必需扩展按 CI 镜像）、独立数据库/HTTP/relay 端口与构建目录。IMBOY_AUTO_MIGRATE=false 用于不需自动迁移的定向；需要迁移 gate 必须在独立合成库执行，不借该变量跳过验证 |
| 最终 B 排除集 | Makefile EUNIT_TEST_SPEC:188 与 backend-ci.yml/native-eunit-acceptance.yml 逐套件适用对照；触及创建/订阅/水位/偏好必须补真实 PG/并发/崩溃测试 | 不把一个 make eunit-local 当全部验收。当前附件、Agent/HIRD、真 PG 专项等排除集合按候选枚举；本专项无触及可有依据地 N/A，有触及则采用对应既有脚本/独立入口，缺资源保留 BLOCKED |
| 最终 A 静态与全量 | `bash scripts/quality/check_analyzer_ratchet.sh`；`dart analyze lib`；`flutter test --no-pub --reporter json`，并按 ci.yml 的隔离 job 覆盖 smoke/integration/native 适用部分 | ratchet 和裸 analyze 各记录结果；CI 部分 analyzer continue-on-error 不能替本计划零新增/适用裸分析要求。全量需原生 vodozemac 与确定性 UI 字体/Goldens 前置；失败/skip 不能抹平，历史10830绿灯不复用到当前候选 |
| 最终 A 迁移/功能位 | 触 SQLite 时 `dart run tool/generate_sqlite_migrations.dart --check`、`bash scripts/run_sqlite_migration_gate.sh --local`；触功能位/路由时按 .github/workflows/quality.yml 的 `bash scripts/check_auto_test_framework.sh`、`python3 scripts/auto_test.py validate` | 未触及 SQLite 的卡无需各自跑迁移；最终候选触及时集中跑。跨仓工具需要并排 Backend 源，明确真实路径；不改生成物绕过门 |
| 最终 M | `bun run lint`、`bun run typecheck`、`bun run deadcode`、`bun run test`、`bun run build`；CI 覆盖 `bun test src --isolate --coverage` + .github/workflows/ci.yml 检查函数≥74%、行≥87% | quality.yml 的 ratchet 与 CI 的覆盖阈值分开记录，不用局部 API 测试替全量/构建。共享契约无影响仍需候选绑定兼容证据，不强行制造源码 diff |
| 原生/浏览器 J | 使用 integration_test/README 与对应已维护 Dart 场景，Android/iOS 真机、macOS 桌面；Admin Playwright 用项目 test:e2e 入口；Flutter Web 的 raw-wire 用真实浏览器合成 fixture | W00 固定设备 OS/构建/角色/隔离服务与合成数据，不复制生产账号、不群发。Web 组件 test platform override 不是浏览器大整数解码；各平台分别收据，缺者 BLOCKED |

共享 BEAM/ETS/meck/PG/build/native 目录的检查串行，除非证据证明隔离；子卡只定向，W13 集中跑最终适用全量。Schema/协议/IDL/运行时/共享依赖变化使涉及端的全量/迁移/协议证据失效；Provider/查询/授权/状态变化至少使其直接调用方与相关 J 失效；纯索引/解释文字不使未改业务候选重新冒充“已测”。修复全量失败或新增代码后必要重验，没有“只跑一次”的豁免。历史基线失败单列，但必需门未解决则不能 PASS。

## 9. 固定验收子断言（R13）

保留 J01–J29 旅程身份，以下子项是独立判定单元，父项仅当全部 Required/适用子项通过才可 PASS。多参数向量（“分别/各”）执行时必须按 `子项/参数/平台` 分开收据，不能一例代表全向量。前置统一使用合成隔离资源、§1.6固定候选/交付配置、当前账号和真实成员资格；不使用真实用户数据或发消息给第三方。故障测试均在隔离 fixture/受控服务注入。

验证列为**拟扩展的现有测试位置**，编号后需落具体同名 J 子项 case；不是已经覆盖的声明。自动 A/B/M + 必需真实平台 N 共同组成证据，N 指 integration_test/README.md 所列真机/桌面方法，不能把它当现成业务断言文件。J18/J19/J22 的细化设备脚本尚未实现，属业务任务未完成，计划 oracle 已确定。R=无条件 Required；C-channel/attachment=实际交付功能开启时正例 Required，关闭须有功能门证据与 J27.f 负例 PASS 才可 N/A。平台已承诺支持却缺设备是 BLOCKED，非 N/A。FAIL/NOT_RUN/BLOCKED/NOT_APPLICABLE/STALE 分开；本轮所有完整子断言均 NOT_RUN，历史 W03 定向收据不自动填 PASS。

测试落点：

- `AS`：App `test/unit_test/page/channel/channel_provider_state_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AE`：App `test/unit_test/page/channel/channel_detail_event_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AA`：App `test/unit_test/page/channel/channel_detail_actions_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AC`：App `test/unit_test/page/conversation/conversation_authority_actions_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AP`：App `test/unit_test/page/conversation/conversation_page_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AM`：App `test/unit_test/page/chat/chat_setting/chat_setting_burn_picker_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AQ`：App `test/unit_test/page/channel/channel_list_state_sync_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AU`：App `test/unit_test/service/channel_service_unread_summary_sync_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AW`：App `test/unit_test/page/workspace/workspace_channel_detail_page_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AV`：App `test/unit_test/page/workspace/workspace_conversations_page_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AG`：App `test/unit_test/api/group_api_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AF`：App `test/unit_test/app_core/feature_flags/app_manifest_cache_recovery_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `AH`：App `test/unit_test/component/http/http_parse_test.dart`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `BS`：Backend `test/logic/channel_scope_tests.erl`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `BG`：Backend `test/logic/group_scope_tests.erl`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `BR`：Backend `test/logic/channel_workspace_read_tests.erl`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `BM`：Backend `test/api/app_manifest_handler_tests.erl`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `BP`：Backend `test/integration/conversation_pin_delete_integration_tests.erl`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `MI`：Admin `src/lib/safeParseBigIntJson.test.ts`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `MW`：Admin `src/services/api/workspaces.test.ts`（在此扩展 `Jxx.y` 命名 case；NC 为执行方法文档）。
- `NC`：App `integration_test/README.md`（执行方法；具体场景在现有 integration_test/ 对应域中实现 `Jxx.y` 命名 case）。

- `MG`：Admin `src/modules/groups/api/groups.test.ts`（拟扩展相应 J 子项）。
- `MC`：Admin `src/modules/channels/api/channels.test.ts`（拟扩展相应 J 子项）。
- `MF`：Admin `src/services/api/features.test.ts`（拟扩展相应 J 子项）。
- `MV`：Admin `src/pages/workspaces/WorkspaceDetailPage.test.tsx`（拟扩展相应 J 子项）。
- `AN`：App `test/unit_test/service/group_notice_config_test.dart`（拟扩展相应 J 子项）。

| 子断言 | 前置数据/角色 | 操作/故障注入 | 可观察结果 | 自动/人工与测试位置 | 适用 | 结果/证据 |
|---|---|---|---|---|---|---|
| J01.a | workspace owner/member；选择1人/多人 | 工作区创建 | 归属/成员集合正确，成功进同群，不偷改单聊 | AG,BG | R | NOT_RUN / — |
| J02.a | 企业壳成员 | 打开全局发起聊天 | 选择前明确个人域，保留五项菜单 | AG | R | NOT_RUN / — |
| J03.a | guest/无资格；按钮与深链分别 | 创建工作区资源 | 客户端解释且服务端拒绝，无资源/受理/成员副作用 | AG,BG,BS | R | NOT_RUN / — |
| J04.a | 旧治理入口已有gid | 改名失败后重试 | 只改原gid，显示已创建，不再次建群 | AG | R | NOT_RUN / — |
| J04.b | 有群能力/新意图 | 初始名创建与重放 | 同事务名与成员，始终同gid | AG,BG | R | NOT_RUN / — |
| J05.a | personal/workspace/治理三入口分别 | 创建频道 | 请求/响应归属、策略与回跳一致；每入口独立收据 | AA,BS | C-channel | NOT_RUN / — |
| J06.a | 两个Tab有不同集合/滚动/查询 | 反序响应＋订阅事件 | 各Tab集合/条件隔离，旧代次无写回 | AQ,AS | C-channel | NOT_RUN / — |
| J07.a | 详情A/B叠页 | A晚返回、返回A后发布 | 标题/内容/评论/发布目标同资源，未用旧状态 | AS,AA | C-channel | NOT_RUN / — |
| J08.a | 两个账号、两个workspace、合成外链分别 | 切换或打开外链 | 不残留内容、不自动切空间、不写错账号库 | AS,AV,AF | R | NOT_RUN / — |
| J09.a | 有资格资源，空列表合法 | 详情/消息各503、403、404、成功空分别 | 四态可区分且安全文案/重试准确，拒绝不走缓存成功 | AA,BR | C-channel | NOT_RUN / — |
| J10.a | 前台保护页 | 权威撤权后旧响应完成 | 同状态更新撤内容和写入口，旧代次不能恢复 | AE,BR | R | NOT_RUN / — |
| J10.b | 同资源重新加入新generation | 重入后旧数据/偏好到达 | 仅当前世代可呈现，旧资格不继承 | AV,BR | R | NOT_RUN / — |
| J10.c | 冷启动/返回/重连分别 | 资格未知或请求失败 | 授权前无受保护渲染 | AA,AV | R | NOT_RUN / — |
| J11.a | 订阅已提交 | 回读失败、点击重试 | 显示已订阅但待对账；不再购买/重复入订阅 | AA | C-channel | NOT_RUN / — |
| J12.a | 有/无/变更绑定分别；已入群与未入群分别 | 点击讨论入口 | 唯一准确目的地，不造General、不自动加入；无资格明确说明 | AW,AA | C-channel | NOT_RUN / — |
| J13.a | 正常未筛选倒序首页 | 最新边界项≥50%持续500ms | 推进至该响应B；较早项被纳入查看边界，文案不称逐帖已读 | AA,BR | C-channel | NOT_RUN / — |
| J13.b | 置顶旧帖/筛选/单帖跳转/分页缺口分别 | 显示或滚动 | 不推进正常流边界 | AA,BR | C-channel | NOT_RUN / — |
| J13.c | 后台/弹层遮挡/不足50%分别 | 推进停留时钟 | 500ms计时重置，不推进 | AA,AE | C-channel | NOT_RUN / — |
| J13.d | 两端B1<B2 | B2后晚到B1 | 服务端GREATEST，水位不倒退；未展示的新推送不并入旧B | BR | C-channel | NOT_RUN / — |
| J13.e | 退订后重订同订阅id | 重入且迟到旧mark请求 | 新激活descriptor隔离旧边界，不复用旧资格 | AA,BS | C-channel | NOT_RUN / — |
| J14.a | 群/频道工作区空集合 | 加载成功空 | 准确空态/合法下一步，不越权补资源 | AW,AG | R | NOT_RUN / — |
| J14.b | 目录有数据 | 搜索无结果/刷新失败分别 | 关键词保留；失败保留合法上下文，不显示成功空 | AW,AQ | R | NOT_RUN / — |
| J15.a | C2C、C2G分别 | 点头像并返回 | 分别进人/群资料，回原列表与位置 | AP | R | NOT_RUN / — |
| J15.b | C2C、C2G；移动/桌面壳分别 | 点整行并返回 | 正确chat type/对象与宿主目的地 | AP | R | NOT_RUN / — |
| J16.a | 无消息C2C/已有行/群KV/频道订阅分别 | 设mute=true，重开读取 | 按对象真源可回读，首消息不丢偏好；不制造可见空会话 | AC,AM,AN | R | NOT_RUN / — |
| J16.b | 原状态false | 注入持久化失败/切账号/连续点击分别 | 状态不假成功，目标不取反，账号隔离且可重试 | AM,AC | R | NOT_RUN / — |
| J16.c | 两设备同账号 | 修改本机DND后读另一端 | 本机合同不冒称同步；已存在服务端动作继续符合其真源 | N + NC | R | NOT_RUN / — |
| J17.a | 个人/工作区当前成员分别 | 明确pin=true/false | 原服务端真源回读/排序正确，失败不假成功 | AC,BP | R | NOT_RUN / — |
| J17.b | 可见会话 | 明确hidden=true/false，失败/超时/防重分别 | 本地与远端结果一致或明确待核对；不隐藏错账号对象 | AC,BP | R | NOT_RUN / — |
| J17.c | 有真实未读、manual_pending独立 | 标已读及设待处理分别 | 水位单调，人工标记不写unread=1，彼此不清除 | AC,AV | R | NOT_RUN / — |
| J17.d | 两个端读同version | A写后B旧version写；回读再重试原目标 | 冲突可见，保留desired_value，不无限覆盖/重复取反 | AC,BP | R | NOT_RUN / — |
| J17.e | 旧客户端无expected_version | pin/unpin/delete/restore分别 | 同业务入口推进版本，新端能检测旧写冲突 | BP | R | NOT_RUN / — |
| J18.a | C2C/C2G发送场景 | 发送失败后重试 | 失败可见、重试同逻辑消息不重复 | N + NC | R | NOT_RUN / — |
| J18.b | 合成附件上传 | 断网/恢复/重试 | 保留任务、附件归属与重试状态，不重复业务消息 | N + NC | C-attachment | NOT_RUN / — |
| J18.c | 本人消息 | 允许/过期撤回分别 | 时限真实，撤回失败保留合法状态 | N + NC | R | PASS（Android 真机 2026-10-29，允许+过期两分支，见 §11.1；暴露并修复发送方撤回不回显缺陷 App `ab13f4a06`）/ — |
| J18.d | 本人可编辑消息 | 编辑成功/拒绝分别 | 编辑态/已编辑/失败恢复准确 | N + NC | R | NOT_RUN / — |
| J18.e | 引用目标已加载 | 点击引用 | 滚动/高亮目标或给出可行动失败 | N + NC | R | PASS（Android 真机 2026-10-29，滚动定位分支，见 §11.1）/ — |
| J18.f | 未加载、过期、无权、缺钥目标分别 | 完整历史定位 | 有权按历史边界拉上下文定位；其余各有说明，不绕过E2EE/成员边界 | N + NC | R | NOT_RUN / — |
| J19.a | 普通群成员 | 取消/确认退出分别 | 取消无副作用；退出清当前资格且返回正确 | NC,BG | R | NOT_RUN / — |
| J19.b | 群主且有可转让成员 | 转让后退出 | 转让/退出分步结果准确，不默认解散 | NC,BG | R | NOT_RUN / — |
| J19.c | 群主 | 单独解散确认/取消 | 后果说明清楚；取消无副作用，成功不留旧资格 | NC,BG | R | NOT_RUN / — |
| J19.d | 本机有群历史 | 仅清空本机记录 | 入口仍在，不误隐藏/退群 | N + NC | R | NOT_RUN / — |
| J19.e | 本机可见会话 | 仅从列表移除 | 只隐藏不清记录/退群，复现遵既有消息合同 | N + NC | R | NOT_RUN / — |
| J20.a | 群/订阅/管理/工作区目录各>1页 | 搜索尾页私有合法对象 | 服务端全范围交集可找到，不靠当前页/公域发现 | AG,AQ,BS | R | NOT_RUN / — |
| J20.b | 关键词q1请求未完成 | 改q2并晚到q1 | 废弃旧cursor/代次，不污染q2 | AS,AQ | R | NOT_RUN / — |
| J20.c | 固定未变集合 | 逐页到末页，注入重复cursor/矛盾元数据分别 | 稳定排序无漏重；错误停止加载且可重试 | AQ,BS | R | NOT_RUN / — |
| J20.d | 扫描并发新增/重激活/移除分别 | 继续扫描与刷新 | 旧上界不误纳入/误删，新扫描收敛，每页实时授权 | AQ,BS | R | NOT_RUN / — |
| J21.a | workspace/频道已归档、来源托管群分别 | 打开/尝试写/查看恢复动作 | 按真实对象只读/拒写；仅真实支持且有权限才可恢复 | AW,BS,MV,MC | R | NOT_RUN / — |
| J22.a | 320/360窄屏各主题 | 布局主操作 | 无overflow，主按钮可达，标注实际平台 | N + NC | R | NOT_RUN / — |
| J22.b | 横屏/低高窗口分别 | 滚动操作 | 内容/危险动作可达，无裁剪 | N + NC | R | NOT_RUN / — |
| J22.c | 键盘弹出 | 编辑/搜索/发送 | 输入/提交/取消不被遮挡 | N + NC | R | NOT_RUN / — |
| J22.d | 大字与长名，中英文分别 | 放大布局 | 文字自然增高/语义完整，不关闭TextScaler | N + NC | R | NOT_RUN / — |
| J22.e | 明暗主题各状态 | 测颜色/命中区 | 正文≥4.5:1、动作≥3:1，命中区≥既有44pt规范（原V1.1要求48时取48） | N + NC | R | NOT_RUN / — |
| J22.f | 读屏/桌面键盘各交付平台 | 聚焦/执行菜单与危险动作 | 焦点顺序、标签、忙态/禁用语义完整，各平台独立证据 | N + NC | R | NOT_RUN / — |
| J23.a | 群/频道受理事务分别 | 提交前崩溃或成员/receipt入队失败 | 资源/成员/receipt/逻辑通知全回滚，同键可首次受理 | BG,BS | R | NOT_RUN / — |
| J23.b | 已提交未响应 | 崩溃、并发/重启重放 | 同资源/规范载荷/初始名，只有一个逻辑受理结果 | BG,BS | R | NOT_RUN / — |
| J23.c | 通知持久入队已发送未确认 | 崩溃恢复重投 | 固定effect/message身份，一份邀请/成员副作用 | BG,BS | R | NOT_RUN / — |
| J24.a | 未知意图，client年龄23:59:59.999/24h/时钟倒退分别 | 恢复/重启 | 24h内原键；边界或时间不可信后仅recover_only，不剥键 | AG,AA,AF | R | NOT_RUN / — |
| J24.b | server受理6d23:59:59.999/7d，及7天内删除/撤权分别 | 原键恢复 | 正常有权时7天内保留结果；删除/撤权不泄露内容；过7d只元信息/expired；receipt仍阻止重建，两个时钟独立 | BG,BS | R | NOT_RUN / — |
| J24.c | 同键改名/成员顺序/重复成员/workspace；只查未见受理后旧POST迟到 | 重放请求或尝试新键重建 | 规范等价重放、实际载荷变化冲突；旧请求可能在途时禁止换键，超时/断连/单次只查/用户确认不构成终止证据；迟到的同原键请求最多受理一次 | BG,BS,AG | R | NOT_RUN / — |
| J25.a | 已创建后撤权/删除资源分别 | 同键重放或只查 | 不返回无权内容、不复活、不重建，最小受理事实保留 | BG,BS | R | NOT_RUN / — |
| J26.a | 新空本地库53订阅，尾页有未读 | 50+3逐页落库/汇总 | 每页先订阅再计数，尾页可见且偏好不重置 | AU,AQ | C-channel | NOT_RUN / — |
| J26.b | 第二页/落库/汇总分别失败 | 恢复同页/对账 | 保留已完成部分且错误可见，不删除缺席项 | AU,AQ | C-channel | NOT_RUN / — |
| J26.c | 扫描中退订/重订 | 继续翻页并收到权威事件 | 事件对应状态回读，旧扫描不删新激活关系 | AU,AE | C-channel | NOT_RUN / — |
| J26.d | 有本地53项 | 搜索返回1项 | 其余52关系/未读不因缺席清理 | AU,AQ | C-channel | NOT_RUN / — |
| J26.e | 本地尾页对象被撤权；global/scoped summary分别 | 无事件重验或缺席/显式0分别 | 失效对象无保护摘要/badge；unknown不写0、不清其他范围 | AU,BR | C-channel | NOT_RUN / — |
| J27.a | 旧客户端/旧Admin，无新参数 | 读取各原清单 | 原envelope/完整旧集合/ADM page,size,keyword可用 | BS,BG,MW,MG,MC | R | NOT_RUN / — |
| J27.b | 新客户端缺字段/未知合同/304无同源缓存分别 | 尝试新创建/分页/偏好 | 不误启，不剥request_id，来源缓存隔离 | AF,BM,MF | R | NOT_RUN / — |
| J27.c | personal/workspace与错范围cursor分别 | 新范围/分页/搜索 | 范围准确，终止null、前进有效；错cursor不回落 | AQ,BS | R | NOT_RUN / — |
| J27.d | 2^53−1/+1/BIGINT上界合成原始文本 | Backend→App native/Web→模型/路由→请求→Backend | 所有ID十进制逐字符不变，禁止先构造JS Number fixture | N + AH,MI,MW | R | NOT_RUN / — |
| J27.e | 长数字字符串、时间/金额、小数、Xid分别 | 同解析/回传链 | 非ID保持原义，非法ID明确拒绝；不全仓强转 | AH,MI | R | NOT_RUN / — |
| J27.f | 功能位关闭或合同不可识别 | 访问按钮/深链/旧写恢复 | 门禁拒绝/旧合法读明确，N/A功能正例不能覆盖负例义务 | AF,BM | R | NOT_RUN / — |
| J28.a | 旧SQLite/PG合成库 | 升级/重复启动/失败恢复 | 最小增量可重入，manual默认false，旧unread不反推 | AC,BP | R | NOT_RUN / — |
| J28.b | 一账号两端旧/新客户端 | 同目标重试/旧写/版本冲突 | 旧写参与版本，新端期望冲突与回读成立 | AC,BP | R | NOT_RUN / — |
| J28.c | 退出再加入workspace群 | 旧generation偏好/水位/内容回填 | 只当前generation生效；旧数据保留却不恢复旧资格 | AV,BR | R | NOT_RUN / — |
| J28.d | 旧App/Admin+新Backend、新App+旧Backend分别 | 读/写/恢复 | 按§10.4允许组合兼容，禁用项可解释且无无键新建 | AF,BM,MW | R | NOT_RUN / — |
| J28.e | 新旧Backend同来源节点 | 采样manifest后请求落旧节点 | 部署被禁止/不启新合同；不能以某次200通过 | BM | R | NOT_RUN / — |
| J28.f | 已产生receipt/游标/偏好新数据 | 兼容Backend/App回退 | 排空与admission关闭，原意图/列/数据保留，不down、不猜cursor | AF,BM,BP | R | NOT_RUN / — |
| J29.a | 当前K有效，单调t0 | 19.999/20s推进 | 20s开始静默校验，内容不整页清屏 | AA,AV | R | NOT_RUN / — |
| J29.b | 校验pending/失败 | 29.999/30s推进 | 30s硬截止撤展示与写入口，失败不延期 | AA,AV | R | NOT_RUN / — |
| J29.c | 续验有效成功或旧代次迟到分别 | 完成响应与旧截止回调 | 只有当前K内容资格更新deadline，旧响应/旧timer无效 | AA,AE | R | NOT_RUN / — |
| J29.d | 目录/统计/缓存/不匹配账号或世代各响应 | 响应成功 | 均不能续内容预算 | AA,AV | R | NOT_RUN / — |
| J29.e | 已知断网/后台/返回/重连分别 | 状态切换并推进时钟 | 立即撤保护展示，恢复前重授权；无失败缓存回填 | AE,AV | R | NOT_RUN / — |
| J29.f | 明确403/撤权事件 | 处理事件当回合 | 渲染/内存同状态失效，持久内容不可冷恢复，个人E2EE不批删 | AE,BR | R | NOT_RUN / — |
| J29.g | 消息expire_at到期，资格仍有效 | 推进内容TTL | 仅该摘要隐藏/重取；不退成员、不用TTL延资格 | AV,BR | R | NOT_RUN / — |

## 10. 计划采用的设计与兼容默认值

D1–D7 均为本轮授权收口后的**计划设计输入**，不是已上线事实或业务实施授权。无需下一位执行者再次猜产品方向；若发现与用户当前明确裁决冲突，保留已有行为并指出具体冲突。普通实现细节由责任者按现有架构解决，不为每个可逆修改重复申请批准。

| 决策 | 计划采用的具体规则 | 实施出口 |
|---|---|---|
| D1 企业消息范围 | 当前工作区群消息 + 独立频道更新入口；目录和更新区分；企业托管/客服继续独立；不新增统一收件箱 | W08，不混三个会话真源 |
| D2 个人频道范围 | 新客户端个人页仅 personal、工作区页仅指定 workspace；无 scope 的旧请求保留原授权集合；未知 scope 不当 personal，外链不自动切空间 | W07 的兼容/本地同步/未读共同通过后启用 |
| D3 查看语义 | 频道叫“已查看新内容”：前台未遮挡且满足§6.8停留阈值时推进更新边界；预加载/后台/仅进入不清零；显式标已读单独执行；新内容不抢滚动 | W08；人工待处理与真实未读分离 |
| D4 创建与离开 | 发起群聊一直建群，选一人不偷改单聊；成功直达、完善资料非阻塞；清空本机保留入口，移除仅隐藏，群主可转让后退出或单独解散 | W04/W10；保留全局五项 + 菜单并明示个人域；新消息自动复现沿用当前服务端合同，不另改政策 |
| D5 幂等与协议兼容 | 采用下方 D5 细则：24h 客户端恢复、7d 完整响应、最小受理事实保留；显式 pagination=cursor；manifest 版本识别、终止游标及错误名固定 | W04/W07，全部旧调用方保持可用；新增字段均为目标合同 |
| D6 撤权/缓存 | 采用下方 D6 细则：消息 TTL 不等于授权；明确拒绝同状态更新失效；工作区冷启动/返回/前台重入先授权；前台本地展示预算最多 30s、失败不延长 | W02/W08/W09；不是服务端授权租约，也不承诺不可观测的即时撤权 |
| D7 偏好与演进 | 置顶/隐藏/人工待处理按账号/来源/资源及适用的群成员世代服务端版本化；免打扰逐对象沿用 §6.7 真源，本机项标“仅此设备”；新增 false，不从 unreadNum 反推人工提醒 | W09/W10，升级/回退与重新加入验证通过 |

### D5：协议、能力与恢复

以 §6.4/6.5 为完整规范：新增各对象合同字段；24h 仅控制客户端可创建恢复，>=24h 使用 recover_only；7d 只控制服务端完整响应；最小 receipt 不因 TTL 重置；无能力绝不剥离 request_id 重发。分页/search 逐接口识别和有界对账；不修改旧 ADM/旧 List 形状。整数 ID 保真由 §10.3 的 raw-wire 断言证明，类型别名本身不是证明。

### D6：保护展示

以 §6.6 为完整规范：有效内容授权 K 匹配；20s 提前再校验、30s 硬截止；离线/后台/明确拒绝使展示失效；三层缓存分别处理，个人 E2EE 离线合同不受侵入。

### D7：对象偏好与兼容

以 §6.7 为完整规范：逐对象确认当前本机/服务端真源，现有 pin/delete 继续权威，新版目标值/CAS/旧写同事务版本推进；人工待处理与真实未读分离；群/工作区群复用真实群成员世代。不新增并行偏好真源。

### 10.3 三端大整数 ID 原始链验证（R10）

RFC 8259 §6 说明常见双精度解析器的整数互操作边界（[原文](https://www.rfc-editor.org/info/rfc8259/)）；“EntityId=String”若发生在普通 JSON 数字解码后，无法修复已损失的位。Backend elib_response 用 jsone 编码整数；App HttpClient 使用 Dio 默认 JSON 解码，entityIdOf 对 num 调 toInt，不能证明 Dart→JS 保真；ChannelModel/GroupModel 等还持有 int。Admin client 在解析文本前调用 safeParseBigIntJson，已有边界源码测试，但端到端回传仍须补证。本轮仅静态识别风险，不把它写成已实测丢失或全链路 PASS。

| 输入（合成十进制原始文本） | 范围与 oracle | 固定验证链 |
|---|---|---|
| 9007199254740991 / 9007199254740993 | 2^53−1 与 2^53+1；值必须逐字符相同 | Backend jsone 原始 body→App VM 和 Web 真实解析入口→EntityId/频道/群模型→route param→请求序列化→Backend 实际解码值与原始十进制比较 |
| 9223372036854775807；审计候选中合法 TSID 大值 | PostgreSQL signed BIGINT 上界/合法 ID 上界，非真实用户资源；不在 JS 中先构造 Number fixture | 同链；Admin client transform→DTO→渠道/群路由→put/post 原始 body→服务器；正好2^53另做相邻边界，不拿它替代+1 |
| 字符串中的长数字、时间戳、价格、负数/小数、不透明消息 ID | ID 修复不得改字符串内容，不误把时间/金额当 ID；Xid 消息 ID 保持文本 | 扩展既有 safeParseBigIntJson、App HTTP/模型与 Backend envelope 测试；无法支持的资源 ID 非法输入明确拒绝，不能截断 |

最小处理顺序：先跑上述链；若 Flutter Web 真实解码不保真，W07.c/W04.d 优先在现有 HTTP raw response 解码入口对目标 ID 保留精确十进制，再调整受影响模型/路由，保留 Backend JSON integer 和既有可接受 string 输入兼容。没有证据不全仓改成字符串 ID、不改消息 ID 类型。Web 支持范围若无法提供真实浏览器解码回传证据，对 Web 该合同记 BLOCKED，不用 native int 代替。自动无网络 echo fixture 可验证解码/编码；真实服务端 loopback 接收用合成数据，未经后续实施授权本轮不执行。

### 10.4 混合版本与安全切换（R14）

| 组合 | 允许条件与行为 |
|---|---|
| 旧 App/旧 Admin + 新 Backend | 原 envelope/列表及 ADM page/size 不变；旧偏好目标写也推进版本；旧 mark_read 由同一新水位适配器处理显式清到当次边界，不绕开新版计数 |
| 新 App + 旧 Backend | 缺合同字段不启新版创建恢复/真分页/版本偏好；保留原只读/既有明确能力。已提交未知意图保持原键，仅在确认支持 recover_only 后核对，不降级成无键写 |
| 新 Admin + 新 Backend | 当前 ADM 合同兼容即可，不强迫 Admin 使用 App 合同；共享投影变化需对应 DTO/页面和权限回归 |
| 新 Backend/旧 Backend 节点同服务来源负载均衡 | **不支持新合同启用期间混跑。** 当前 manifest 来自单节点，不能证明下一请求节点支持；旧 Handler 会忽略未知 request_id，HTTP 200 不能补救。统一先部署兼容版至所有节点、验证每节点合同/存储，再切换新客户端流量；未完成前无任何节点可宣称新能力 |
| 新版写入后回退 Backend | 只可回退到已具备 receipt 不重置/旧写推进版本/世代过滤的兼容版本，不能回退到忽略字段的原始旧 Handler。先停止新写 admission、处理在途未知意图/通知、停止发新游标、清合同缓存并排空连接；保留表/列/receipt，不 down DB |
| App 回退/新游标遇旧节点 | 保留未完成意图，不自动另建；游标失效明确错误后在同范围重新开始，不能把 opaque cursor 当旧 offset 或改变 scope；偏好数据保留，旧端只见其已兼容字段 |

切换条件需由候选与各节点构建 SHA/manifest/路由分布证据证明；仅测试单节点不能放行混部。此为部署组合限制，不增加新配置平台，也不是本轮部署操作授权。

## 11. R01–R16 处置与计划完成条件

| 建议 | 处置 | 落点 | 核实依据/调整理由 |
|---|---|---|---|
| R01 | 接受 | §1.6/第3节/§8.4 | 三仓 origin/main 实测；ADM 路由/DTO/归档/权限消费者独立；F07/08/09/16 默认分支与 WIP 分列，Admin 无无条件必改代码。 |
| R02 | 调整后接受 | §6.4/§10.4 | 现有 manifest 无 contracts map；在既有顶层增对象字段，区分群/频道/偏好/水位，合同 ETag 与编译 hash 不混用，明确禁止剥键。 |
| R03 | 调整后接受 | §6.4 恢复及边界表 | 24h 客户端派发、7d 服务端响应不是同一时钟；recover_only 复用 POST 只查，receipt 不因过期重置。 |
| R04 | 接受 | §6.4/W04.b/c/J23 | 已有资源事务和 after_commit，但后者仅缓存失效且不持久；通知复用 msg_s2c 持久链，新增 Conn 入口若必要只限该链。 |
| R05 | 调整后接受 | §6.8/J13 | 当前 mark_as_read 忽略 message_id；既有时间/TSID 不证明提交序，限定最小频道 update_seq/view_seq，不做逐帖阅读系统。 |
| R06 | 接受 | §6.6/J29 | 原“当前上下文读取”过宽，改内容授权 K、20s提前验/30s硬截止及三层缓存，不造服务器租约。 |
| R07 | 调整后接受 | §6.5/W07/J20 | 订阅 id 实际存在且重激活复用；工作区成员列表是 /groups?member_only=1，不虚构 member-groups 路由；完整搜索分四类。 |
| R08 | 接受 | §6.5 权威范围表/J26 | 当前 summary 无集合完整性证明；缺席≠0，完成翻页仍不得无条件清理，显式对象状态才可销账。 |
| R09 | 调整后接受 | §6.7/W09/J16/17/28 | 单聊SQLite、群KV＋投影、频道本机订阅、服务端pin/delete分别核对；不把群禁言当免打扰，不造workspace世代；已确认群KV无账号前缀，按现有KV机制补最小范围隔离。 |
| R10 | 接受，修复条件化 | §10.3/J27.d/e | Admin有解码前保护，App默认Dio+num转字符串无法证明Web精度；固定raw-wire测试，未经失败证据不全仓改ID。 |
| R11 | 接受 | §8.2开发DAG/联合启用/F11 | 开发前置、文件串行与启用分别列；W07不等整个W04，W10开发不等整个W09。 |
| R12 | 接受 | §8.2子卡 | W04/W07/W09落14个子节点（含App群/频道分别接入），明确路径/禁区/测试/退出/回退，未新增任务平台。 |
| R13 | 接受 | §9断言表 | 所有J都有固定子断言，J18/J22/J28/J29分拆；Required/条件适用、拟扩展测试和NOT_RUN逐行记录。 |
| R14 | 调整后接受 | §10.4/J28 | 当前单节点manifest不代表集群；明确新合同不支持新旧Backend混跑，兼容全节点部署后才启用；Admin纳入回归。 |
| R15 | 接受 | §8.4 | 按三仓真实Makefile/CI/脚本核入口，保存ratchet与裸分析、覆盖阈值和默认排除集的差别，本轮不执行业务门。 |
| R16 | 接受 | §1.6/§11.1 | 沿用原worktree/HEAD，静态证据不当运行引擎；WIP指纹保护、历史局部收据与当前完整验收分离，唯一V2.2入口。 |


PLAN_READY 表示：三端基线与 WIP 分开；16 项建议逐条有裁决；22 F 有复核记录；C/D 合同无相互冲突的窗口/水位/真源；子卡、直接消费者、自动覆盖和依赖校验完整；缺运行资源不伪装 PASS。它不表示所有实现细节、所有平台或生产环境已验证。下面列出的实施阻塞只阻断相应运行/启用，不用文档完善销账。

- 整体业务仍 PARTIAL_UNSUBMITTED，W00/W03 未最终验收；其余 W01/W02/W04–W13 没有完整退出证据。
- 未取得本候选独占 Backend PG/附件/通知隔离资源及授权合成账号；没有启动服务/写接口/迁移。后续真实副作用验证只允许合成隔离环境，不得打扰第三方。
- iOS/Android 真机、macOS 与 Flutter Web 实际解码/读屏验收资源待绑定；缺设备是 BLOCKED，不能 N/A 或用 widget 替代。规划必需断言已固定，不等设备到了才发明标准。
- origin 之后若变化，或选择合入 Backend 本地 7fbf3a1a，应显式更新执行候选和受影响证据；本轮没有重基线现有 worktree。
- 无任何新能力已经声明上线；W04/W07/W08/W09 的数据库兼容、崩溃/时钟/竞争断言均待执行。混部禁用规则先满足，才能启用新合同。

### 11.1 轻量续跑（R16）

本文件是版本管理内的计划；evidence-v2.2.json 是静态审计/映射证据，**不是运行状态机**。不复用已被清理的 enterprise-uiux runtime，不新建通用任务引擎。后续实施直接在本节维护最小当前子卡/候选/证据/阻塞/下一步；临时完整日志放仓外，验收必要摘要按规范保留。

当前候选（2026-10-27 用户明确授权重建）：Backend `7fbf3a1ac0c7ffa9ca502d3e55365fbf6a17e222` / App `7dee230dc78e85f883d8a1e67b6dfec3a6fe83fa`，均从各仓当时本地 main 创建；Admin 继续只读。两仓分支均为 `codex/channel-group-ux-v2.2`，目录分别为 `/Users/leeyi/project/imboy.pub/.worktrees/channel-group-ux-v2.2/imboy` 与同级 `imboyapp`。旧 v2.1 分支、worktree 已移除；9 个 App 代码/测试 WIP、5 个计划文件迁移时逐字节校验一致，备份见 evidence 中 execution_relocation.backup。

W00 准入复核（2026-10-27，仅静态）：两任务 worktree 的 HEAD 仍为上述值，App 的 `plugin/flutter_chat_ui` 已检出 `62245bd40067d73ddb5bf98d340537cbbe2ae3e7`；Admin 当前 main HEAD 为 `2c3b54283ed970513f08fc3d4075e4617c04ef85`，工作树干净。App 存在本专项未提交修改及 v37 迁移增量，HEAD/tree 不是完整候选，实施与验收前须重新绑定实际文件指纹。本机 Flutter 缓存版本 3.47.8、Dart 3.13.7；App CI 固定 Flutter 3.47.5，本机测试不能伪称 CI 环境。Bun 本机为 1.3.14；各工具可执行文件存在不等于运行资源可用。静态编译清单含 `channel`，发现/邀请/订单受该父功能约束；当前服务器来源、账号和实际功能快照未绑定，目标 contracts 字段尚不可当作已启用。Backend 隔离 PG/附件/通知与合成身份、iOS/Android 真机、macOS/Web 与 Admin 浏览器验收资源尚未按候选固定，分别阻断对应运行或启用断言；不能推断设备不存在。该次复核时 W00 为 `IN_PROGRESS`，当时尚未声称唯一开发 DAG 的 W00 退出条件已达成；不得将局部测试或旧静态证据升级为 W13 PASS。

W00 补充：默认 `erl` 为 OTP 29 / ERTS 17.0.2，Backend CI 使用 OTP 28；本机 `/opt/homebrew/opt/erlang@28/bin/erl` 已确认是 OTP 28 / ERTS 16.4.0.3，后端定向检查可显式绑定该工具，但仍需隔离构建及数据库资源。`psql` 18.4 和 Docker CLI 29.9.2 仅证明客户端存在，不证明服务或隔离库可用。当前 App 编译清单含 `channel`、`enterprise_business`，但不含 `channel_discover`、`channel_invitation`、`channel_order`；现有 `AppFeatureRegistry.isEnabled` 先验证编译清单，因此经该入口检查的三个子键在当前构建中为 false，不能因父 `channel` 开启就声称子入口开启。Backend manifest 静态构造尚无 `group_contracts/channel_contracts/conversation_contracts`；运行节点 manifest、实际来源/账号快照及各节点一致性均未取证。此前 v37 SQLite 门禁 13 项通过于 17:52 结束，此后其分析范围内的 `channel_service.dart` 等发生变化，完整历史门禁相对当前候选已过期；最终候选须重新执行。W04.a 的 fixture 定向方案可继续只读准备，但该子卡开发前置明确为 W00；在 W00 按本计划退出前不启动写入，也不把真实服务/设备未绑定误报为不存在。

W00 只读资源复核（2026-10-27 18:24 +08）：`docker info --format '{{.ServerVersion}}'` 返回 29.9.2；`flutter devices --machine` 列出 Android 真机、iOS 真机、macOS 和 Chrome。仅证明本机当时可发现 Docker 服务及设备，不证明专用隔离服务、合成账号、构建/签名或设备验收可用，也未绑定到当前候选。两任务 HEAD 与 Admin HEAD 同上；App 本地未暂存 diff 内容已变化，之前采样及受影响测试证据不自动适用。该次复核时 W00 为 `IN_PROGRESS`，W04.a 等硬前置当时尚未退出；不更改运行验收状态。

W00 专用合成 PostgreSQL 准备：容器 `imboy-v22-pg-6b2199c0` 使用 `imboy/pg18:3.6.1-2`，仅绑定 `127.0.0.1:57559`；准备时核对 PostgreSQL 18.1，已创建 15 个扩展，未执行项目迁移。该实例仅供本任务合成数据，保留供后续使用；尚未与最终候选、通知/附件资源及测试配置绑定，单凭该实例不能判定运行验收通过；静态准入出口见下文。

W00 App 未冻结采样（2026-10-27 18:15 +08）：HEAD `7dee230dc78e85f883d8a1e67b6dfec3a6fe83fa` 的 tree 为 `c7d76258a88b90c383c531510892c8e7378c4ade`；`git diff --binary` SHA256 为 `d713d5581b632ea8520673e47b95336f278700197e99457656beb0fde76feca6`，`git diff --cached --binary` 为空；未跟踪文件 5 个，按 Git 输出的 UTF-8 路径字节排序，逐项拼接 8 字节大端路径长度、路径字节、文件 SHA256 原始 32 字节后的聚合 SHA256 为 `21235e73b4c93352494f9ede346bb347235cc5213d5e4be0a3d9b62439209db0`。这只是共享工作树的时点采样，后续写入会使其变化，不能代替最终候选冻结，也不证明测试有效。

当前子卡 W01/W03 均为 PARTIAL_UNSUBMITTED。App 固定子模块和本地依赖已初始化；W01 增加订阅/管理状态及搜索隔离、账号与请求代次保护、事务对账和失败提示。未读汇总缺席不能区分零与撤权：保留本地值并提示待核对，权威收敛及完整分页仍由 W07 完成。W03 保留既有会话 WIP。新候选定向验证：频道状态与 SQLite 对账 41 项、频道页面交互 6 项、W03 会话/聊天设置 40 项均退出 0；W01 受影响文件 dart analyze 无问题，build_runner 退出 0。原始日志分别为 `/tmp/imboy-v22-w01-unit.log`、`/tmp/imboy-v22-w01-widget.log`、`/tmp/imboy-v22-w03-recheck.log`、`/tmp/imboy-v22-build-runner.log`；仅证明各自覆盖范围，不关闭父包/J 项。旧候选收据仍不可迁用；全量、设备和运行验收未执行。下一步继续 W01 详情/嵌入调用方及 W03 剩余边界，再按依赖推进后续工作包。

历史迁移核对脚本 `/tmp/check_imboy_v22_relocated.py` 以 App 差异必须恰为迁移前 9 文件为前提；实施 W01 后重跑在 `APP_DIFF_PATH_DRIFT` 退出 1，不能再代表当前候选静态检查通过。后续应按当前增量及原 WIP 分别复核，而非删除新增文件迎合历史断言。

W01 详情增量：切换目标时立即清空旧详情和消息，发布完成后按频道目标代次拒绝写入新目标；频道详情、事件和分页定向测试 25 项退出 0（`/tmp/imboy-v22-w01-detail.log`），相关文件 dart analyze 无问题。当前详情 provider 仍被叠页和嵌入入口共享，账号切换及叠页返回的完整隔离未解决，J07 不关闭。

认证退出意图现会立即清空详情并使在途详情/消息/发布回写失效，认证转换期间不启动详情或消息加载；列表、详情、事件及分页定向 35 项退出 0（`/tmp/imboy-v22-detail-auth.log`），相关文件 dart analyze 无问题。静态复审确认叠页 B 返回 A 后仍可能取到 B 的共享状态，必须完成页面归属隔离；发布服务在 notifier 返回前可能执行 SQLite/outbox 写入，客户端代次保护不能证明跨账号写库安全，该直接消费者仍须在 W01/W02 修复和验证。认证完成后页面恢复加载也须验收，不能凭退出清空关闭 J07/J08。

发布服务现对网络响应期间认证失效增加守卫；列表认证及服务操作定向 118 项退出 0（`/tmp/imboy-v22-publish-auth.log`），相关文件 dart analyze 无问题。复审 HIGH：响应已经返回并进入 `_cacheOrEnqueue` 后，`getChannel`、消息缓存和 outbox 的异步访问仍可跨账号；需要绑定原账号数据库及事务，并测试缓存读取挂起后切号、成功和失败两个分支。该局部守卫不能关闭发布持久化隔离、J08 或 W02。

后续增量对缓存读取、远端频道读取及消息缓存各等待点增加会话检查；缓存读取挂起时退出、释放后不调用消息仓库或消息 outbox 的回归包含在 118 项定向测试中（`/tmp/imboy-v22-publish-cache-auth.log`，退出 0），静态分析无问题。复审 HIGH 仍存在：已进入仓储写入后，仓储内部异步获取当前数据库，外层检查无法保证写入账号；写入中切号及失败分支仍待固定句柄/事务验证。发布 outbox 重放路径也未完成相同保护。

发布与消息 outbox 仓储已支持在外部 SQLite 事务内入队、查询、删除、重试，保持旧调用兼容；真实 SQLite 回滚覆盖与关联测试共 123 项退出 0（`/tmp/imboy-v22-outbox-txn2.log`），相关文件静态分析无问题。服务层尚未接入绑定会话的事务，不能将仓储能力测试当成跨账号安全验收；下一步需绑定原句柄并覆盖写入中切号和重放。

生产直接发布路径已接入原账号数据库句柄：成功缓存及清理、缓存失败转消息 outbox、网络失败转发布 outbox 均以绑定事务执行并检查会话代次；真实 SQLite 三条分支及相关测试共 126 项退出 0（`/tmp/imboy-v22-publish-bound.log`），相关静态分析无问题。代码静态复审通过，但事务执行中切号的真实回滚测试尚缺，旧 forTest 默认分支的测试不能证明生产绑定分支；发布/消息 outbox 重放及其他频道缓存路径仍未绑定，J08/W02 仍未关闭。

发布 outbox 重放已使用原账号句柄及原 request_id；失败以事务更新重试次数。真实 SQLite 的直接发布事务中**模拟**会话失效时消息与请求删除一起回滚，重放成功/失败及相关测试共 129 项退出 0（`/tmp/imboy-v22-publish-replay.log`），相关静态分析无问题。静态复审通过该路径；真实认证事件与消息 outbox 重放、其他缓存调用方仍需验证及隔离，不关闭 J08/W02。

消息 outbox 重放及空缓存网络回退已绑定会话与数据库句柄，覆盖成功、失败重试、模拟事务失效和网络等待期间失效；相关定向 134 项退出 0（`/tmp/imboy-v22-message-replay.log`），静态分析及复审通过。缓存与 outbox 双失败时保留成功获取的网络结果并记录日志；调用方尚无持久化失败提示，仍属 W02。真实认证切换、其他频道缓存调用链及最终验收仍未完成。

隔离临时路径下新增真实 `quitLogin` 测试：A 会话网络请求挂起，退出完成后手动设置 B UID 并注入另一份内存 SQLite，释放 A 响应不返回旧消息且 B 的消息及 outbox 表为空；与绑定事务测试共 26 项退出 0（`/tmp/imboy-v22-real-auth.log`）。测试没有执行真实 B `loginAfter`、原生 SQLCipher 建库或设备验收，不证明完整账号切换；事务执行中真实退出与其他调用链仍需验证。

详情本地优先读取、权威刷新和快照缓存现绑定原账号数据库与会话检查；SQLite 用例覆盖缓存命中、网络等待期间模拟失效及写事务内模拟失效，相关定向 157 项退出 0（`/tmp/imboy-v22-channel-bound.log`），静态分析及该增量复审通过。详情错误类型、数据库不可用与撤权的区分、其他调用链及真实设备切号仍未完成，不关闭 W01/W02。

发布栏和图文页传入页面目标，目标不符时拒绝发布；custom_id 必须匹配当前已解析频道后才规范化为数值 ID，避免别名回归及无条件信任共享状态。相关定向测试 32 项退出 0（`/tmp/imboy-v22-channel-target.log`），静态分析、生成器及增量复审通过。该守卫不能替代叠页独立 provider；附件 presign、图文入口的真实交互和叠页返回仍未验收。

媒体及图文上传入口增加目标检查；页面绑定创建时账号和会话，上传后和最终发布前复核，图文页监听详情以避免异步期间 provider 被回收。目标不符保留草稿；同频道切账号后的旧图文草稿拒绝发布，相关定向 85 项退出 0（`/tmp/imboy-v22-upload-target.log`），相关静态分析无问题。挂起上传跨账号的真实回归、上传结果清理及叠页独立状态仍未完成；不能据此关闭 W01。

工作区或来源未知的旧频道缓存暂不在权威刷新前渲染；同频道刷新清除旧受保护内容，初始本地读取异常进入失败态并失效旧发布目标。详情相关定向 32 项退出 0（`/tmp/imboy-v22-protected-cache.log`），静态分析与生成器通过。当时兼容限制：v36 SQLite 不保存 scope，重启后的个人频道旧缓存也为未知；下段记录后续 v37 修复及仍保留的限制。30 秒预算、撤权分类及完整 W02 仍未实现。

现已新增 SQLite v37 可空 `channel.scope/workspace_id` 迁移及模型映射：v36 旧行保持 NULL，服务端已知来源可往返保存，缺省来源更新不覆盖既有来源；个人来源经真实 SQLite 回读后可离线显示频道元信息。迁移失败注入证明两列与版本回滚；`bash scripts/run_sqlite_migration_gate.sh --local` 第三轮退出 0（13 PASS，日志 `/tmp/imboy-sqlite-gate.bdzWnA`），频道定向 164 项退出 0（`/tmp/imboy-v22-v37-channel.log`）；前两轮各有 1 项失败，分别修正未知版本 schema 检查和无效故障注入。v37 无降级边，不能声称旧 App 可直接打开 v37 数据库；旧行在权威刷新前仍未知，离线消息历史、原生设备和运行验收未完成。无真实生产迁移。

详情新增保留失败状态的 checked API；已有个人频道缓存的刷新接入该接口：仅网络异常或 502/503/504 保留缓存，其余错误清内存内容并失效请求代次；后端不存在的实际 envelope code=1 不按文案猜测。API 错误码、无效响应和详情分支等定向 150 项退出 0（`/tmp/imboy-v22-checked-detail-r2.log`），静态分析及生成器通过。工作区/未知来源错误分类、缓存持久清理、30 秒预算和非成功反馈仍待 W02；本项不关闭 J09/J10/J29。

详情的 custom_id 回退已接入 checked API；删除事件会使加载、消息及发布代次失效，别名尚未解析时暂记删除 ID 并在解析及缓存后检查。服务缓存事务接收请求代次守卫，真实 SQLite 回归覆盖写入期间代次失效回滚；包含旧别名响应和缓存挂起期间删除的定向测试共 74 项退出 0（`/tmp/imboy-v22-custom-detail.log`），静态分析、生成器及增量复审通过。未完成跨会话删除事件溯源、持久缓存撤权处置和真实设备验收，J07/J10 不关闭。

详情 Widget 参数变化时重新加载，外部链接页绑定请求代次；A→B→返回 A 时按当前路由恢复目标，包含 B 仍加载中的场景。相关定向 45 项退出 0（`/tmp/imboy-v22-detail-reuse-focused.log`），静态分析、生成器及增量复审通过。此前含 layout 的一次运行 51 项通过、10 项 golden 跳过（缺基线），不能算视觉验收；当前仍使用共享详情 Provider，真正的叠页独立状态、晚响应的页面级组合、统计/已读及 custom_id 深链仍需完成，J07 不关闭。

恢复时沿用上述工作目录/分支/HEAD，先 hash 比对并查实际进程句柄；仍在途只等原句柄，终止才决定重跑。新任务首次执行可采样最新默认分支候选，续跑不得悄悄切 main。候选/输入变动只使相关证据 STALE，但依赖它的联合启用与最终门一并失效；报告路径变动本身不冒充新业务候选已测。

English handoff: Planning is ready only at the documented static scope. Business implementation is partial and unsubmitted; runtime and device acceptance remain open. Resume the existing worktrees, preserve WIP, validate changed inputs and enable no new contract until its joint gates pass.

订阅契约在途增量：Backend 为当前单次请求中可证明的前置拒绝提供 990/991/992，敏感错误经脱敏处理；OTP 28 mock 定向 51 项通过（`/tmp/imboy-v22-subscription-contract-sanitized.log`）。App 对已确认成功但本地对账未完成的情况提供进程内对账和订阅/退订双向互斥，服务定向 143 项通过（`/tmp/imboy-v22-confirmed-subscription-r3.log`）。两端增量已审查；这不证明跨进程重启、响应丢失、另一端反向修改或标准 PG/HTTP 行为。W02/J11 保持未关闭；验收和最终候选需要重新绑定证据。

三端候选重绑定（2026-10-29，供 W13 验收固定目标）：Backend `940a8a735f5485611f829eff2f1a669346c2d7e6`（tree `b1679be10e2d97086ec61ff289b6f51a416b3849`；= 全量门 v6 代码候选 `81cee79e` + 本计划 docs 增量，docs 变更不使代码门失效）、App `73aa608fc60e74c0dd3f9494522f1a0dd269952d`（tree `e1512ddc13cf91cb7d147efcf9d6506de5784cce`；= `80cc19f7c` + J18.e 设备测试文件，无 lib 变更）、Admin main `d7a06dd5a22a266941d4250963748561a0ba9e1a`（tree `bd05a5adff99bdc50b5e07fc483b2222b1f82f99`）。三仓工作树在绑定时刻干净（无暂存/未暂存/未跟踪增量），分支分别为 `codex/channel-group-ux-v2.2`（Backend/App）与 `main`（Admin）。候选此后再变化则本绑定 STALE，W13 验收须使用当时实际最终候选重跑适用门。

Web 平台 SQLite 门补跑（2026-10-29）：`offline_group_wire_web_test` 此前因缺 `SQLITE_WASM_URL` 编译期常量而条件跳过，本次以 `flutter test --platform chrome --dart-define=SQLITE_WASM_URL=http://127.0.0.1:18999/sqlite3.wasm` 真实运行，wasm 由本机 CORS 静态服务器从项目 `web/sqlite3.wasm` 提供，1 项 exit 0（日志 `/tmp/imboy-v22-offline-web-v2.log`）。首次运行因静态服务器无 CORS 头被浏览器拦截（exit 1、无异常栈），补 CORS 后通过。该测试在 VM 内 mock HttpClient，证明的是 Web 平台 + 真实 SQLite WASM 驱动的离线群角色链路与 64 位 ID 字符串往返，不是真实网络或真实设备。`olm_pfs_production_path_test` 仍 BLOCKED（sqflite_sqlcipher 原生平台通道，需真机/桌面集成壳）。

Tier 2 真机冒烟门跑通（2026-10-29，`integration_test/smoke/smoke_test.dart`，README 定义的合并门控）：**SMOKE_EXIT=0，All tests passed**（日志 `/tmp/imboy-v22-smoke-device-v3.log`）。绑定链条：服务端源 = Backend 候选 `a863ae24` 的本地 release（`_rel/imboy` 1.0.0-alpha.83，daemon 起，127.0.0.1:9800，`/api/v1/init` 200，迁移账本 236 无漂移）+ 合成库 `imboy_v22_synthetic@127.0.0.1:57559`；账号快照 = `scripts/seed_demo.sh` 经节点 RPC 种 3 个合成用户 `demo_alice(120619870873454592)/demo_bob(120619871628429312)/demo_carol(120619872402278400)`（互为好友 + 演示消息，纯合成数据，未触共享库/真实用户）；设备 = Android 真机 MRD AL00（XWE6R19916004085，Android 9）USB + `adb reverse tcp:9800`；App = 候选 `80cc19f7c` debug 构建，`--dart-define=APP_ENV=local_office API_BASE_URL=http://127.0.0.1:9800`。链路：init 200 → initConfig GCM 解密成功 → 明文登录回退成功（RPC 建号存 hmac_sha512，md5 被拒后自动重试，客户端既有行为）→ UI 登录 → 主 Shell 挂载 → 截图。前两次运行失败如实记录：v1 无 `APP_ENV` 走 pro 环境密钥；v2 `local_office` 仍解密失败——根因是服务端 `ensure_solidified_keys/0` 留空时按**节点名哈希**派生密钥，而 sys.local.config 注释声称留空默认值「与 Flutter 客户端硬编码一致」，实现与注释漂移（窗口外既有问题，未改代码）；修复 = 合成服务端 `sys.local.config` 与 release 内 sys.config 显式设 `solidified_key/iv` 对齐 App `.env.local_office`（32×'0'/16×'0'，该文件 gitignored）。边界：单设备单账号冒烟，证明安装/启动/后端绑定/登录/主界面链路；不证明双设备消息收发、E2EE 互操作或推送；设备上原有旧版 imboy 因签名不匹配被 flutter 自动卸载重装（本地数据被替换）。

J18.e 设备脚本落地并跑通（2026-10-29，plan §9 首个补齐的细化设备脚本）：新增 `imboyapp/integration_test/chat/j18e_quote_locate_test.dart`（App `73aa608fc`），真机（MRD AL00）走合成源：登录 demo_alice → 打开与 demo_bob 的 C2C → 在已加载历史窗口内动态选取最旧填充消息 → 长按菜单「引用」→ 真实发送链（WS）发出带引用消息 → 点击引用条 → **目标气泡从视口外滚动进入视口**（`J18E_EXIT=0`，日志 `/tmp/imboy-v22-j18e-v5.log`；「触发消息高亮」×1、失败提示 ×0——走的是 oracle 的滚动/高亮成功分支）。fixture：`imboy_ctl msg send` 预置 20 条填充消息（bob→alice，合成库）。过程如实记录：v1 WS override 路径错（`/ws`≠后端路由 `/api/v1/ws`）；v2 固定引用 #05 超出首启离线拉取窗口（实测最近 10 条，pageSize 16 的本地页与离线窗口不同）失败；v3 起动态选取已加载目标后通过。两点观察：① 首启离线拉取窗口（10）小于本地聊天页 pageSize（16），历史其余部分靠归档服务异步逐页补齐，测试因此必须动态选目标；② `_locationTargetId` 定位高亮最终落在 `messageScrollManagerProvider.highlightedMessageId`，当前无任何 widget 消费该状态做视觉高亮——「高亮」一半目前是纯状态，视觉呈现缺失（oracle 以「滚动到位」判定通过；视觉高亮缺口记录在案，不在本窗口修）。J18 其余子项、J19/J22 设备脚本仍 NOT_RUN。

J18.c 设备脚本落地并跑通（2026-10-29，plan §9 第二个补齐的细化设备脚本；首个由设备脚本暴露并修复产品缺陷的项）：新增 `imboyapp/integration_test/chat/j18c_revoke_window_test.dart`（App `6d81458a2`），真机（MRD AL00）走合成源：登录 demo_alice → 打开与 demo_bob 的 C2C → 经真实输入链发 OLD、FRESH 两条 → FRESH 等正向 sent 后长按「撤回」→ **原文消失、「撤回了一条消息」占位出现**（允许分支）→ 从 OLD 气泡 widget 读真实 createdAt，按 `kDefaultRevokeWindowMs` 真实等待出窗（99.3s，不改时钟/常量）→ 长按 OLD（入口门控只看作者 `canRevoke = isSentByMe`，菜单仍有「撤回」）→ 点击 → **「超过 2 分钟，无法撤回」即时提示、OLD 仍为文本气泡**（过期分支；全程只发出 1 次撤回请求，OLD 在客户端短路未发网络）。时限两层一致：客户端 `message_revoke_policy.dart` 120s 短路 + 后端 `msg_c2c_logic.erl` `REVOKE_TIMEOUT_MS=120000`。真机四轮原始退出码 v1=1 / v2=1 / v3=1 / **v4=0**（日志 `/tmp/j18c_run1..4.log`）。过程如实记录：v1 第二条消息发不出——`enterText` 内部 `showKeyboard` 仅在 `focusedEditable` 变化时 `requestKeyboard()`，发送后 ChatInput `unfocus()` 关了输入连接，同一字段二次 `enterText` 静默写不进（测试侧改为先 tap 再 enterText 并校验控制器文本；仓内既有用例每例只发一次故从未踩到）。**v2 暴露产品缺陷**：客户端发出 `message_revoke`、服务端回 `message_revoke_ack`（重试队列按 ws_action_ack 清除）后，30s 内 FRESH 原文仍在——`websocket.dart` 对所有 `*_ACK` action 帧在分发前 `return`（`b6fa0238f5` 2026-01-28 引入、`5b22f9b3d` 2026-09-03 大写归一，均为分支基线祖先，窗口外既有），而发送方本机"原消息转已撤回 + UI 刷新"在 `MessageActions._processRevokeAck` 完成、后端只给发起方回这一帧（`c2c_revoke_ack`）、客户端无乐观本地转换——**本人撤回在服务端成功后发送方气泡永不更新，用户只看到「撤回成功」toast**；`message_edit_ack` 同构受影响（J18.d 发送方路径）；`message.dart:368` 注释本就写明"控制 action（已读/撤回/输入状态/ACK）保持原有明文元数据路径"，是传输层过滤越权截断。修复（App `ab13f4a06`）：抽纯函数 `lib/service/ws_ack_forward_policy.dart`（例外集仅 `MESSAGE_REVOKE_ACK`/`MESSAGE_EDIT_ACK`），传输层过滤改调它；`CLIENT_ACK_CONFIRM/ERROR` 与其余 `*_ACK`（含 `message_read_ack`）行为不变，重试队列二次清除幂等；`websocket_server_ack_test` 原用 `message_revoke_ack` 断言「不转发」钉死了错误契约，改用 `message_read_ack` 保留原意并补 `type=C2C` 真实帧形的正向转发用例；新增 12 例纯函数单测（两文件 20/20 通过）。v3 修复生效（ack 进 `_handleRevokeAction(isAck=true)` → status=31 → 原文消失），仅末尾"占位数不增"断言因懒加载列表滚动后视口外占位不再构建而脆断；改为"OLD 仍为 TextMessage 气泡"判定后 v4 通过。App 候选重验（含 lib 变更）：`dart analyze lib` 0 issues；`check_analyzer_ratchet.sh` PASS（全仓 0 issue）；`test/unit_test/service/` 整组 +2749 −2——`olm_pfs_production_path_test` 为接力节已记录的 sqflite_sqlcipher 平台通道环境门；`migration_v35_order_facts_test` 钉 latestVersion=36 而 manifest 已为 37（v37 由 `418b08fd4` 引入、分支基线祖先、基线外漂移；姊妹 `migration_v36_message_id_test` 已钉 37），App `9415756db` 仅同步 pin，该文件重跑 8/8 通过。`check_file_size_ratchet.sh` 在分支上整体已红（30 个超限文件，多为 W 卡产物；`websocket.dart` 改前即 1400 > 基线 1368，本变更净 +1 → 1401），该门未被本计划列为门禁，记录为上游拆分决策项，不在本窗口处理。未重跑全仓 `flutter test` 与 integration_test 其余用例（与接力节口径一致）。J18 其余子项（a/b/d/f）、J19/J22 设备脚本仍 NOT_RUN。

三端候选重绑定（2026-10-29 晚，J18.c 后；替代上一条绑定，供 W13 验收固定目标）：Backend `46da9f0e1df9beca57b91d190cedd4943e664b45`（tree `81ca2300ddbb45585231de91ef4cf8c9e2e55ebd`；= 全量门 v6 代码候选 `81cee79e` + 本计划 docs 增量，docs 变更不使代码门失效）、App `9415756dba94619648e0c70f3afc44b5950e2417`（tree `177fce0199bf555a599bd9a4f642aafd652b1fc6`；= `73aa608fc` + `ab13f4a06`（**lib 变更**：`websocket.dart` 过滤改调新纯函数 `ws_ack_forward_policy.dart`）+ `6d81458a2`/`9415756db` 测试，上述 App 重验即针对此候选）、Admin main `d7a06dd5a22a266941d4250963748561a0ba9e1a`（tree `bd05a5adff99bdc50b5e07fc483b2222b1f82f99`，未变）。三仓工作树在绑定时刻干净，分支分别为 `codex/channel-group-ux-v2.2`（Backend/App）与 `main`（Admin）。候选此后再变化则本绑定 STALE。

### 11.2 本轮校验结果

静态校验覆盖 22项F、5项C、7项D、14项W与29项J原编号、28 个开发节点及无环依赖、14 个子节点、全部固定子断言和测试落点、R01–R16 一一处置；完整计数与验证时点写入 evidence-v2.2.json.validation。此结果仅为文档结构/引用和输入保护检查，业务门全部仍按真实状态，不把 NOT_RUN 改 PASS。

频道头部与服务隔离增量：头部按账号、会话、频道和操作代次屏蔽陈旧结果；订阅对账与静音写入绑定原账号数据库事务，会话失效回滚且不发送静音事件。头部定向 17 项、服务相关定向 133 项及补充事件/配置断言 27 项退出 0；定向分析和独立复审通过。日志分别为 `/tmp/imboy-v22-service-isolation.log`、`/tmp/imboy-v22-service-isolation-assert.log`。测试通过直接模拟会话变化，并非真实设备认证切换；其他服务消费者、全量验证及 W01/W02/J08 未关闭。创建恢复增加迟到请求竞态与删除例外断言，属于计划修订，未证明业务实现。

W00 启动采样与资源矩阵（2026-10-27 18:49:50 +0800，非最终冻结）：三端 HEAD/tree、暂存/未暂存 diff SHA256、未跟踪路径逐项 SHA256 记录于 evidence 的 `w00_start_snapshot`；后端计划自身由 `plan_sha256` 对照，证据文件不能自签散列。App 子模块 `plugin/flutter_chat_ui` 为 `62245bd40067d73ddb5bf98d340537cbbe2ae3e7`。采样时并行工作可能继续改变源码；后续候选变化须重采样并按 §8.4 使受影响收据失效。此记录只固定时点，不证明最终候选或验收。

| 资源或输入 | 当前状态及影响 |
|---|---|
| 合成 PostgreSQL | 专用本机容器 `imboy-v22-pg-6b2199c0` / 127.0.0.1:57559 已运行；服务配置、合成账号和候选尚未绑定，项目迁移未执行。需 PG 的 W04.b/c、W07.a/b、W09.b/d 和相关 W13 运行验证 BLOCKED |
| 合成身份、通知、附件及隔离服务 | 尚未与专用实例及候选绑定；涉及通知/附件或真实写入的相关子卡运行验证及启用 BLOCKED，不连接其他任务容器 |
| Android、iOS、macOS、Web 与 Admin 浏览器 | 设备/平台可发现不等于验收身份、构建、角色和隔离服务绑定；各自涉及的 W04.d、W07.c、W12、W13 运行旅程分别 BLOCKED |
| 功能位和服务端能力 | 编译清单含 channel、enterprise_business，不含 channel_discover/channel_invitation/channel_order；服务器来源、账号功能快照及各节点 manifest 未绑定。W04/W07/W08/W09 新合同启用 BLOCKED |
| 工具链及 CI | 本机 Flutter 3.47.8，CI 3.47.5；OTP 28 可单独绑定，默认 OTP 29；Admin Bun 1.3.14。工具存在不等于检查通过，CI 对齐及相应门仍待实际运行 |

W00 18:49 静态采样当时为 IN_PROGRESS：设计与兼容表曾做静态一致性复核，创建迟到/删除例外已补充；服务器来源与账号功能快照未绑定，测试角色及服务配置未固定，且并行代码变更后须核对最新采样。以上 BLOCKED 是对应资源的状态，不改变开发 DAG，也不判定任何 J 或运行验收通过。

2026-10-27 晚间接手续跑（ZCode 会话，基线 8ff3c032/7dee230d 及双 diff SHA256 复核一致后执行）：

- 环境修复：Backend 任务树 14 个失败依赖残根（gpb/cowboy/hex_core 等，仅含 noop Makefile 或空 .git）与 2 个陈旧 `.erlang.mk/dep_built` 标记清除后 `make deps`（28 依赖）+ `make compile`（1060 beam）exit 0（OTP 28）；构建再生的 pb 产物噪音已还原 HEAD，未混入提交。`.imboy.plt` 已建，5 个已改模块过钩子 undef 门。合成 PG 容器凭据与 env 失配已重置并写入 gitignored `config/sys.local.config`（仅 127.0.0.1:57559）；应用启动完成全量迁移（284 表）。
- W02/W04.b 提交（Backend `719346af`、`09632761`）：990/991/992 前置拒绝与查询失败区分（6 套件 181 项 OTP 28 通过）；建群初始成员失败中止事务新增真 PG 回归 `group_creation_recovery_pg_tests`（资格成员落库/无资格整体回滚/混合成员集无残留，Test passed）。
- App 提交（`418b08fd4` 频道 W01/W02+v37+十语言 i18n、`86076aa2f` 会话 W03、`0a01409a4`+`8d9246fa2` 组织治理 W04.a 含重启恢复）：提交前 372 项定向 + 受影响文件 analyze + SQLite 迁移门 13 PASS 均对当前候选执行；`86076aa2f` 内一处 gitleaks generic-api-key 误报按仓内先例内联豁免。
- W04.a 收口：待改名意图按「来源+账号+工作区」持久化（org_group_rename_pending_store），页面重建/重启恢复并绑定当前账号世代，重试绝不重建群；组织页测试 12 项通过。
- W01 叠页归属隔离（`19f4644e6`）：ChannelDetailNotifier 按目标快照（上限 8、仅 personal、认证切换/频道删除失效），返回原目标同步恢复快照；共享 watch 渲染点（详情页 mismatch 分支、消息流、工作区嵌入宿主、阅读页标题）统一走 `ownChannelDetailState` 归属过滤；新增 3 项 Notifier 快照用例 + 7 项纯函数用例，相关 8 文件 163 项回归通过。
- W03 贯通结论：占位行写入、首消息 save（isShow 恢复且 is_muted 保留）、通知抑制函数与 message.dart 接线（含 C2G KV 合并源）、开关忙态/失败回滚均有各自定向证据；J16.c 两设备同账号向量因设备资源 BLOCKED，未执行。J16/J17 相关父项保持未关闭。
- W04.b 后端受理合同（`076d5694`）：迁移 00000231 建 `resource_create_receipt`（PK uid/operation/request_id，无 TTL、永不重置）；receipt repo（advisory 串行化 + ON CONFLICT 占位 + 同事务回填；群/频道规范载荷 SHA256，排除 request_id/recover_only/两端时间）；`group_logic:add_with_receipt/8`（首受理=群+初始 title+成员/世代+回填+持久通知入队同一事务；重放零副作用；recover_only 只查；7d 服务端年龄；删除例外优先；request_id_conflict）；通知链补 `msg_store_repo:stage_s2c_tx` + `msg_s2c_ds:send_tx`（事务内 staging、after_commit 投递、MsgId 由效果身份确定派生，staging UNIQUE 兜底）；解散同事务置位 resource_deleted；error_code 5462-5465 稳定符号；handler request_id 分支（缺席时旧路径逐字不变）。真 PG 套件 6/6（J23.a-c/J24.b/c 向量）+ 回归 group_scope 3/3、group_logic 23/23、msg_s2c_ds 6/6。`group_contracts` manifest 未声明（新入口保持关闭，启用待 W04.d 与全节点一致）。
- W04.c 后端频道受理合同（`fe190d16`）：`channel_logic_message:create_channel_with_receipt/7` 复用同一 receipt repo（canonical_channel_digest：名称 trim、服务端默认三策略位、tags 去重排序）；占位→既有 `channel_ds:create_channel_tx`（频道+创建者订阅/管理员+配额）→回填同一事务；频道创建无成员邀请故无通知入队义务；`channel_ds:delete` 同事务置位删除标识；handler 分支同 W04.b 口径（成功 envelope 增 request_id/creation_state）。真 PG 套件 4/4 + 回归 channel_scope 4/4、channel_ds 17/17、channel_handler 106/106、channel_ds_idempotency 2/2。`channel_contracts` manifest 未声明（同上）。测试期间修正两处实现缺陷（fetch_created_channel 状态硬编码、重放快路径未取 DTO）与一处测试编码笔误（无 /utf8 的中文字面量按低字节截断）。
- 状态：W01/W03/W04.a/W04.b/W04.c 代码侧收口并提交；W04.d.g/W04.d.c（App 入口接入：request_id 发送、manifest 能力门、意图持久化、24h/recover_only 客户端语义、J24.a/J27）与 W05–W13 未实施；全部新合同未启用；W13/J 验收 NOT_RUN。上表资源矩阵不变。
- W04.d.g App 群路受理入口（App `4b9ed068f`）：manifest 增量 group_contracts/channel_contracts（缺失折叠空 map=不启用；门判定 `create_request_id is int && == 1`，true/"1"/2/1.0 一律拒绝）；CreationIntentStore（SharedPreferences 按「来源+账号+操作+scopeId」隔离，损坏安全缺省，仅成功/服务端可证明终止才清除）；GroupApi.createGroupWithReceipt（request_id/recover_only/recover 标志仅只查时携带；传输失败=结果未知与信封拒绝区分）；GroupCreationReceiptService（先存后发、24h 派发含时钟倒退只查、5462 冲突保留意图、5464/5465 清意图）；organization_manage._createGroup 与 LaunchChatService.groupAdd 接入（capabilityMissing 且无未知意图回落旧键less路径逐字不变；已有未知意图绝不剥键）。离线合成验证：合同套件 22 项（J24.a 时钟向量含 23:59:59.999/精确边界/倒退、J27.d/e 门精度、载荷键不剥、生命周期）+ 受影响定向套件 feature_flags/launch_chat/i18n 门 112、smoke+organization 210、enterprise_shell 28 全过；touched 文件 analyze 0。i18n 增 organization.manage.creationRecoveryUnavailable ×10 语。测试中发现并修复：安全存储通道未 mock 时 token_decryption_failed 标记会把请求拦在 _setDefaultConfig 造成跨用例交替失败（setUp mock 通道返回空值+清标记）。
- W04.d.c App 频道受理入口 + 群路共享化（App `25dbfa559`）：抽取 `creation_receipt_flow.dart`（runCreationReceiptFlow：群/频道共用的意图生命周期状态机，两路「同键恢复一致」由单一实现保证；枚举/结果改名 CreationFlowFailure/CreationFlowOutcome，群路 service 改薄封装、签名不变）；`ChannelApi.createChannelWithReceipt`（fields 原样透传+request_id/recover_only 附加，键less createChannel 逐字保留为回退；5464/5465 提取 channel_id 核对键）；`ChannelCreationReceiptService`（scopeId：工作区按 workspaceId、个人域 'personal'；载荷 name/visibility/access_type/join_policy/description/avatar/custom_id/tags/scope/workspace_id）；CreateChannelNotifier.createChannel 与 organization_manage._createChannel（工作区频道固定 visibility=1/access=0/join=1）接入，capabilityMissing 且无未知意图回落旧路径、已有未知意图绝不剥键；i18n 增 channel.creationRecoveryUnavailable ×10 语。离线合成验证：合同套件 27 项（含频道载荷键不剥/5464 核对键/create_channel 意图隔离/群频道能力门互不波及/工作区频道路冲突重放原键原载荷）、频道页目录 394 过 20 跳过、launch_chat+organization+i18n 门 106 全过；touched 文件 analyze 0。
- W05 选人/工作区群入口（App `563a1dd34`）：F10 工作区群目录补日常入口——客户端搜索（关键词跨过滤/刷新保留）、下拉主动刷新（invalidate+await，失败仍走错误态不显示成功空）、创建入口（搜索行常驻按钮 + 空态 CTA → 新建 WorkspaceGroupCreatePage）；目录语义不变（仅本账号已加入的本工作区群，J14.a 准确空态不越权补资源）。选人页成员来自真实工作区成员 API（GET /workspaces/:id/members，第一页 50），本地搜索/错误重试/选择计数；创建走受理合同（能力关闭且无未知意图回落键less workspace 建群，成员集语义不变；选人不产生任何授权变更，guest 由服务端拒绝，J03.a）。F18 选人页：联系人搜索（昵称+拼音客户端过滤，已选保持）；加载失败新增可重试错误态（loadError），失败不再等同空好友。离线合成验证：目录/选人套件 5 项 + LaunchChatState LC-6..9（合计 14 项）通过；workspace 页目录 + smoke 330 项通过；触及文件 analyze 0。测试基建沿用 W04.d.g 的安全存储通道 mock（防 token_decryption_failed 拦截）。
- W06 讨论目的地收敛（App `f206d1e94`）：F12 两处反模式移除——workspace 频道详情页删「去 General 群聊聊」常驻横幅（跳群列表、与真实绑定入口并存）与进个人会话列表的「全部消息/私聊」按钮；讨论唯一合法目的地=内容区内真实绑定讨论入口（ChannelDiscussionEntry，embedded ChannelDetailPage 提供）。J12.a 无资格明确说明：有绑定但资格不足（未订阅/付费未购）新增说明行（channel.discussionIneligible ×10 语），加入仍需显式确认且绑定身份核对（不自动加入、不造 General）。i18n 删 workspace.discussInGroupGuide/dmEntry ×10 语。固化旧横幅契约的三个套件改写到收敛后契约（workspace 详情页结构、uiux/p1 discuss banner、uiux copy contract 第 1 项改为键移除断言；p1 夹具翻转为横幅缺席）。离线合成验证：改写套件 13 项 + workspace/channel 页目录 539 过 20 跳 + uiux+频道集成 132 过 2 跳；触及文件 analyze 0。
- W07.a 服务端 q 切片（后端 `3f797164`/`d0013e88`/`2e9ac73d`）：C2 四类清单的可选服务端 q——个人群 page 三 attr（owner/join/manager）加 q（授权交集内 title ILIKE，escape_like/trim/64 截断）+ 稳定排序 created_at DESC,id DESC tie-break（旧 join/manager 仅 id 排序对同刻建群翻页漏/重）；已订阅/我管理频道经 channel_logic 门面 /2 委托 repo /3（旧全量形状与置顶/角色排序零变化，管理命中不产生订阅副作用）；工作区群目录（member_only 0/1 两路径，q 在资格 SQL 内 $5）与工作区频道分页目录（paged=1，$5；paged=0 全量保留旧形状供旧消费者）。全部新路径只在既有授权交集内缩小结果，绝不放宽；清单能力保持未声明（J27.a/b 旧客户端零变化）。真 PG 套件 `group_page_q_pg_tests`（缺省=空 q 一致/同刻 tie-break/不放宽授权/manager 交集/通配中和/trim 等价）与 `channel_list_q_pg_tests`（双列表同款 + 门面传输形状 + 管理无订阅副作用）均过；回归 group_scope 3/3、group_logic 23/23、channel_scope 4/4、channel_creation_recovery 4/4。实施中修复：群/频道门面与 repo 导出漏 /4 或 /3（undef）、含 CJK 二进制分段须各自 /utf8（复现 W04.c 教训）、Erlang 不同元数子句必须句点分隔、erlang.mk 增量与 beam 缓存在长会话中的陈旧问题（以全量重编+清 beam-cache 解决）。
- W07.b 切片 1——个人群签名游标协议（后端 `f8c9f10a` 前 SHA 以 git log 为准）：新模块 `list_cursor`（复用 enterprise_cursor_v2 的 canonical_json+HMAC-SHA256 常数时间验签+24h 过期+无 padding base64url 防篡改构造；授权域独立——payload 绑定 uid/list_kind/scope/workspace_id/规范 q/排序版本/upper，绑定不符统一 invalid 不回显差异字段；签名密钥 config `{imboy, list_cursor_signing_key}` ≥32B，缺失 fail-closed 503——**部署期须为各节点配置该密钥**）；group_ds 三 attr id-keyset 查询（id 唯一不可变 → 全扫描无漏/重；展示序仍 created_at,id）+ group_logic 游标页（limit+1 lookahead + group_transfer 投影）+ group_handler `pagination=cursor` 分支（envelope {list,has_more,next_cursor}，末页 null；绑定/篡改/过期一律 400 invalid_request 绝不回落首页 J27.c；无 pagination 参数 → 旧 envelope 逐字不变 J27.a）。真 PG 套件 `group_cursor_pg_tests` 过（J20.a 三交集全扫描含 q 尾页、J20.b 关键词换代 invalid 不回落、J20.c 篡改/跨账号/错 kind 拒绝、J20.d 并发高 id 不误入旧扫描且重扫收敛、终止页契约）。切片 2（频道 subscribed/managed 新排序 s.id/c.id DESC 游标）与切片 3（workspace 目录/成员群游标绑定化）未实施。
- W07.b 切片 2——频道两族游标（后端 `6ba8c328`）：subscribed 走 s.id DESC keyset（subscription.id BIGINT PK 重激活复用 → 稳定；投影含 cursor_id=s.id 作下页上界，安全列内联）；managed 走 c.id DESC keyset（cursor_id=c.id）；两族均接受 W07.a q 片段（$4），空 q SQL 逐字节同旧路径（legacy envelope 拆为 subscribed_legacy/managed_legacy 原样保留）；channel_cursor_page 共用 handler 分支（绑定不符/篡改/过期一律 400，密钥缺失 503）。真 PG 套件 `channel_cursor_pg_tests`（J20.a 授权交集扫描完整性、J20.b 关键词换代拒绝、J20.c 篡改/跨账号/错 kind、J20.d 并发更高 s.id 订阅不误入旧扫描且重扫收敛、终止页契约）过；回归 channel_list_q_pg/channel_scope/channel_creation_recovery 全绿。
- W07.b 切片 3——工作区目录游标绑定化（后端 `d3d96ecc`）：/workspaces/:id/groups 显式 pagination=cursor 分支接入 list_cursor（member_only=1 → ws_group_member，复用既有 g.id ASC keyset 并把数值 next_cursor 重签为 token；member_only=0 → ws_group_directory，新增 workspace_groups/4 g.id-before keyset 变体（稳定 created_at,id DESC）+ has_more/next_cursor envelope）；绑定不符统一 400 不回落（J27.c）；无 pagination=cursor → 数值 cursor 旧语义零变化（J27.a）。list_cursor 机制向量由 group/channel 两 PG 套件承载；回归 group_scope 3/3、group_cursor_pg/channel_cursor_pg 通过。
- W07.c 切片 1——清单 cursor 协议客户端层（App `ad8298f6b`）：新 `list_cursor_client.dart`（八族 list_kind 常量与服务端域一致；`listCursorContractEnabled` 能力门恰为整数 1（J27.b 精度）；`cursorQuery` opt-in 参数构造 pagination=cursor+token/limit/q；CursorPage envelope 类型）；AppManifest 增 `listContracts` 字段（缺失折叠空 map=未启用）+ `listCursorGateEnabled` 委托门——未声明合同保持全部 legacy 清单路径逐字节零变化（J27.a/b）。离线单测：门精度（true/'1'/2/0/1.0 全拒）、首页/翻页 query 形态、空 token 首页语义、kind 常量对齐；feature_flags 57 + smoke 242 回归通过。消费方（频道订阅页/群列表/工作区目录页）接新协议属于 W07.c 切片 2（与 W07.d 本地同步一起启用）。
- W07.c 切片 2——消费方接线（App `b40c1f7e7`/`f677934e4`）：频道订阅页 loadSubscribedChannels/loadMoreSubscribedChannels 经 ChannelApi.getSubscribedChannelsCursorPage（显式 has_more envelope，keyset limit+1 服务端判定，终止页 next_cursor null）接签名游标协议，state.cursor 线程化签名 token；WorkspaceApi 两目录消费循环（member groups → workspaceGroupsProvider/conversations；channels paged 目录）manifest 门开启时发 pagination=cursor 并线程 opaque token，终止页空 next_cursor 契约收口，门关闭 → 数值 cursor 旧契约（含 next==lastId 前进性与 next==0 终止强检）逐字节零变化（J27.a/b/c）；无效 token 一律 load error 不回落首页。回归：workspace 目录 145、channel 目录 394 过 20 跳、provider_state 23、detail_actions 42 全过。
- W07.d 订阅落库同步（App `2b4bb3bb0`）：频道订阅页 loadMoreSubscribedChannels 每页游标页返回后落库——syncChannelSnapshot（先订阅再计数，页内事务原子，upsert-only：缺席不删除 = J26.d 撤权移除展示由后续全量对账承接）+ syncChannelUnreadSnapshot（只写汇总显式返回项；unknown 缺席不写 0、不清其他范围 = J26.e；未读>0 且缺席 → 待核对不达标），任一失败置 state.syncError（失败可见）且已完成部分保留（页内回滚不牵连 = J26.b）；J26.a 每页先订阅再计数、尾页可见且偏好不重置由逐页顺序保证。离线向量 ×2：J26.a（CH11 预置未读 5 触发待核对→尾页显式 4 解决、CH10 显式 3 保持）、J26.b（页 2 isCurrent 中途失效整页回滚、CH10 保留）；channel_snapshot_sync 37/37 全过，channel 目录回归 394 过 20 跳，触及文件 analyze 0。W07.d 依赖 manifest 声明才实际生效（游标协议路径），legacy 路径行为零变化。
- W07.c 收尾——个人群消费方接线（App `2f093146d`）：GroupApi.pageCursor（pagination=cursor + attr 白名单 owner/join/manager；显式 has_more envelope；绑定不符/篡改/过期 throwIfFailed 上抛，不回落首页 J27.c）；GroupListService 三处消费点门选路径（AppManifest.listCursorGateEnabled）——_pullAndSyncAllViews / _pullAndSyncByAttr（self-heal）/ page() 非 all 兜底，门开启走 _pullAndSyncViaCursor（token 线程化、终止页空 next_cursor 契约收口、逐页落库+成员阴影同步、服务端拒绝即终止并保留已同步部分——与 legacy `payload==null→终止` 同类，绝无第一页伪全量；游标 limit 按服务端上限 200 分页，UI size=1000 由循环累计覆盖）；门关闭 legacy page/size 路径逐字节零变化（J27.a/b）。离线向量 ×4：门开 token 线程化至终止页（want=1000→limit 200）、J27.c 恰一次拒绝即终止且部分保留、J27.a 门关 legacy query 逐字、self-heal 逐 attr 循环计数（4 请求收口）；group 目录 328 全过、creation_receipt 27 过，触及文件 analyze 0。
- W08 查看更新边界（§6.8 C4/D3）：后端 `9b2dba08` + App `732323e4f`。后端迁移 232（channel.publish_seq / channel_message.update_seq / channel_subscription.view_seq + activation_version；旧帖 id 序回填 1..N，订阅 view_seq 保留 0 不从聚合 unread 反推已看过）；发布事务内同事务完成序号分配（channel 行锁串行化）+ 订阅者未读累加（消除事务外累加窗口）+ 作者自看推进（GREATEST）；advance_view GREATEST + 超上界夹 publish_seq；重订阅激活事务内 activation_version+1 且 view_seq 重置（J13.e）；mark_viewed 端点（/api/v1/channel/:id/viewed）显式标已查看边界并返回未查看数（排除撤销/删除）；旧 mark_read 保持仅清 unread_count 不构成精确水位（§10）。真 PG 套件 channel_view_watermark_pg_tests（V1–V9：单调序列/作者自看/GREATEST/重订阅/clear_unread 隔离/count_unseen/边界夹取/幂等不换序号/多频道独立）过；回归 idempotency 2、scope 4、creation_recovery 4、workspace_read 4、list_q_pg/cursor_pg、message 20、archive 14、fanout 4 全绿。App：viewWatermarkEnabled 门（恰为整数 1）、ChannelViewTracker+visibleFraction（≥50%/500ms 停留、遮蔽重置 J13.c、边界固定内容响应、客户端不倒退护栏）、ChannelApi.markViewed（失败不洗成已查看）、详情页门选分支（门开=边界推进监听不关闭；门关=legacy 单次 markAsRead 逐字零变化，F14 现状保留）、feed 更新行「有 N 条新更新」×10 语。离线向量 13 项过；channel 目录+feature_flags+copy contract 468 过 20 跳。**残留**：像素级 50% 面积实测、滚动再入 J13.b 全向量、路由覆盖监听细化留待 W13 真机；view_watermark 能力位保持未声明（启用待部署期全节点迁移一致）。
- W09.a 切片 1——DND 账号键隔离（App `6bce288b9`）：group_notice_config 键升级为来源/账号/群范围 `group_notice_disabled:$accountUid:$gid`（J16.a）；共享设备 KV 旧键保留不删但绝不自动导入当前账号，默认 false 由用户显式重设；空账号/gid<=0 读写全拒；三消费方接账号键（群详情读写/消息管线安全壳）；ChatSettingPage type=C2G 补齐 KV 真源写入（此前仅写 SQLite 即声称成功，§6.7 点名缺陷：KV 先写→投影→投影失败回滚 KV）。离线向量 13 项过（账号隔离/空账号不读/旧键不导入/写拒绝/隔离矩阵）。
- W09.a 切片 2——人工待处理独立于 unread（后端 `991538c7` + App `f95d7ce29`）：user_setting_ds.set_manual_pending（显式目标 true/false，行锁事务读改写 SELECT FOR UPDATE → 应用目标 → 推进该会话版本；无变化同目标重试返回当前状态不重复副作用；非布尔拒绝）；conversation_handler POST manual_pending + GET manual_pending/list 投影；App ConversationApi.setManualPending + ConversationNotifier.setManualPending + state.manualPending（服务端成功才落本地投影；不写 unreadNum）；conversation_page 标未读分支移除 unread=1 hack（不再写会话行），标已读分支不动 pending（彼此不清除 §5.2）。真 PG 套件 manual_pending_user_setting_pg_tests（V1–V5）过；App 离线向量 3 项过（零水位影响/标已读不清 pending/失败不假成功目标不取反）；conversation 目录 76 全过。**残留**：会话列表 pending 可视标记（dot UI）、manual_pending_list 启动同步、J16.a/b 真机双设备向量留待后续切片/W13。
- W09.b——pin/delete 世代绑定（后端 `6f79d1e7` + App `776d94601`）：迁移 233（conversation_pin/conversation_delete 增 generation_id：C2C=0 恒适用；C2G=写入时 active 成员世代；旧行一次性回绑核实为 active 的世代，否则保留为不应用的历史）；group_member_ds.active_generation_id/2；pin/mark_deleted 写入绑定世代（已有行 UPSERT 刷新——重入群重新置顶/隐藏即换绑）；list/is_pinned/is_deleted 读取投影世代过滤（旧值不能用于新世代，W09.d 固定退出，不靠客户端筛选）。真 PG 套件 conversation_pref_generation_pg_tests（V1–V5：C2C 恒适用/绑定 active/退群不适用/重入换绑/回填幂等）过；回归 pin_logic 10、conversation_logic 6 全过。App：WorkspaceConversationModel 增 memberGenerationId 解析服务端 gen.id 投影（世代身份 descriptor；旧服务端折叠 0）；workspace 目录+conversations 155 全过。**残留**：workspace 偏好动作端到端（J17.a/b 真机）、W09.c CAS 版本合同、W09.d SQLite manifest 增量与混合版本 fixtures。
- W09.c——偏好版本 CAS 合同（后端 `c1a605b0` + App `8603006ea`）：set_manual_pending/4 可选 expected_version——行锁事务内 CAS，不符返回 {conflict, CurrentEntry}（handler conflict+current 可安全回读，desired_value 保留由客户端决定重试，不自动取反）；bump_conversation_version/2——pin/unpin/delete/restore 旧写成功后经同一业务入口推进该会话版本（J17.e 新端凭 stale expected_version 可检测旧写冲突；条目不存在以 value=false、version=1 建立）；conversation_uk3/3 服务端构造与 App ConversationUk3Generator 约定一致。App：ConversationApi 携 expectedVersion + 冲突 envelope 映射 ManualPendingWrite.conflict；notifier 权威版本线程化，冲突回读已达目标即成功/未达目标冲突可见。真 PG 套件扩至 V1–V9（CAS 匹配推进/stale 冲突回读/同版本重放无副作用/旧写 bump 可检测/uk3 构造）过；App 离线向量 4 项（含 J17.d 冲突回读双分支）过；回归 pin_logic 10、conversation_logic 6、conversation 目录 77 全过。**残留**：两设备 CAS 竞争向量留待 W13 真机；W09.d 剩余=混合版本证据映射（迁移 232/233 回填+世代+CAS 向量已覆盖旧写/回退组合；App 本地 SQLite 无新列需求——DND 为 KV、manual_pending 在服务端、pin/delete 世代在服务端），J28 设备矩阵留待 W13。
- W10——危险区四后果分离（App `f7a02b38f`）：清空记录不再隐藏会话（J19.d：clearMessages 只清本机记录，入口保留；隐藏=独立的「从列表移除」J19.e 既有合同）；群主双路径（J19.b/c）——主按钮「转让后退出」（确认→TransferOwnerPage 合格接任人单选（排除自己）→transfer→leave 分步结果准确；转让成功但退出失败提示已转让并只重试退出，不停留解散假设）+ 独立红色「解散群聊」确认（取消无副作用）；普通成员单一「退出群聊」（J19.a）。i18n ×4 键 ×10 语。定向向量 3 项（群主双动作渲染/成员单动作/清空保入口不隐藏——fixture 会话行 is_show=1 不变）过；group 目录 331 全过（含矩阵5 契约随 W09.a 账号键更新）、conversation 81 过。**残留**：真机转让/退出分步与取消无副作用实测（J19.a–e 平台证据）留待 W13。
- W11——引用定位授权上下文拉取（App `e8f14e90a`）：F16 引用点击升级——已加载目标滚动定位；未加载目标在成员世代授权边界内 loadMessagesAround（既有服务，服务端裁决过期/无权/越界——越界拉取为空）拉取上下文后重试定位；仍不可达给可行动说明（不再静默返回）。_scrollToMessageId 返回定位结果，深链路径语义不变。chat 目录 583 全过。**残留**：过期/无权/缺钥分文案与真机定位实测（J18.e/f 平台证据）留待 W13。
- W12——设计审计可代码侧修整（App `ec8ed92ad`）：§7 审计表五处机械落点——channel_overflow_menu 去 PingFang SC 强指定（回归系统字体）；workspace_shell_breakpoint 复用 AppBreakpoints.wide 真源（900 重复声明即漂移点）；channel_discussion_entry 图标静态蓝→明暗动态语义色；launch_chat 行高改最小高度约束（大字随 TextScaler 增高，≥44pt）；chat_page 引用滚动回归组件默认时长。触及区回归 638 过 34 跳。**残留**：对比度实测/暗暗 OLED/窄屏横屏键盘/读屏逐组合（J22.a–f 平台证据）与菜单次文字固定灰核对、workspace 行/卡片组件统一、密度叠加收敛的视觉复测留待 W13。
- 状态（更新）：W00–W12 全部代码侧收口并提交；W00 静态准入后各包残留项均列于对应日志条目；全部新合同 manifest 未声明（启用待部署期全节点一致 + list_cursor_signing_key）；W13/J 验收 NOT_RUN（真机/双设备/服务端源绑定/账号快照资源 BLOCKED，见 W00 资源矩阵）（新入口默认关闭，启用待部署期全节点一致；部署期须为各节点配置 list_cursor_signing_key）；W13/J 验收 NOT_RUN（真机/双设备/服务端源绑定/账号快照资源 BLOCKED，见 W00 资源矩阵）。上表资源矩阵不变。

W00 静态准入出口（2026-10-27T18:54:58+08:00）：按 §8.2，三端 HEAD/tree、暂存/未暂存差异、未跟踪文件和子模块的当前时点双次采样一致，完整结果见 evidence `w00_start_snapshot`。D1–D7 与能力/兼容表经静态复核，创建恢复的迟到请求和删除例外已按 §6.4/J24 修订；功能编译清单已记录，服务器来源、账号快照和节点 manifest 未知，按上表阻断相关运行/启用。各适用资源、平台和角色已列为已准备或 BLOCKED，并指明影响范围；专用 PG 可连接不等于应用、迁移或数据准备完成。W00 静态准入 PASS，仅允许不依赖被阻断资源的后续开发；所有受影响验收、合同启用与 W13 保持 NOT_RUN/BLOCKED。后续代码、配置和资源改变需重绑定并重验受影响证据，此时点采样非最终候选。

2026-10-28 契约差异修复续跑（ZCode 会话，基线 bf23d721 复核后执行）：

- W09.c 旧写原子性（后端 `8792af8a`）：pin/unpin/delete/restore 与 bump_conversation_version 合并进同一 elib_pg:with_tx——子步骤失败 throw({abort_tx,_}) 整体回滚（elib_pg 契约：闭包返回 {error,_} 仍 COMMIT，中止必须 abort_tx；失败注入实证 pin 行残留即此坑）；无变化幂等重试不推进版本不推送；stale expected_version → {conflict, CurrentEntry}；C2G 事务内绑定 active 世代（group_member_ds:active_generation_id_tx/3，事务内读用 elib_pg:query 带 Conn——one/3 无 Conn 变体）；缓存失效/推送移至提交后。真 PG 套件 `conversation_legacy_write_version_tx_pg_tests`（版本链/幂等不推进/失败注入回滚/CAS-stale conflict/C2G 世代绑定）exit=0。
- W08 旧 mark_read 水位适配（后端 `8792af8a` + `9d0e5ee9`）：advance_view_to_current_tx 单语句 GREATEST(view_seq, publish_seq) 显式清到当次服务端发布边界（单调、不从聚合 unread 推断、迁移前状态不洗零）；未订阅 not_subscribed、归档 skipped、适配失败仅记日志不阻断旧端点。按 §10.4「新能力未达全节点部署前保持关闭」补 config 开关 `imboy.channel_legacy_mark_read_watermark` 默认 false——存量 mark_as_read 契约测试与 `channel_view_watermark_pg_tests` V3（旧合同只清 unread 不推水位）恢复绿；适配步骤 `do_legacy_mark_read_advance_view/2` 独立导出，新套件 `channel_legacy_mark_read_watermark_pg_tests` V1–V5 直调验证开启态行为（不经进程级全局开关，避免全量并发竞态），exit=0。**残留（审计 P1-4）**：flag=ON 端到端（含配置链路）无测试承载；channel_view_watermark V3 与新套件在开关翻转时合同互斥，部署期启用任务须同步改承载。
- W09.a 用户闭环（App `b31476483`）：ConversationNotifier.syncManualPendingFromServer 启动/恢复同步（uid/sessionGeneration 防护、null 结果保留现状、成功整体替换 manualPending+版本缓存）；会话列表人工待处理可视标记（橙色旗标）+ 滑动动作三态（标已读/取消人工待处理/标未读）+ 冲突展示服务端值不自动取反 + 失败 showError 不假成功；i18n ×10 语；离线向量 9 用例过。
- W02/W08 D6 勘察+复核结论：**后端最小改动清单为空**——工作区群消息读路径已有 fail-closed 世代+工作区绑定逐次校验（group_history_access_repo + organization_resource_access_repo workspace 谓词，拒绝映射 403）；K 六分量可构造（工作区群列表响应含 member_generation_id，W09.b 投影；history 单端点不回显 generation 按 §6.6 降级=不启跨生命周期缓存恢复，不阻断 J29）；撤权事件通道已有（remove_member_tx → imboy_domain_event → msg_s2c group_member_leave 含被移除者本人，save 离线可补）；20s 再校验=重调既有内容接口，无租约无新端点无后端 flag。若 20s 再校验走 msg/history(c2g) 需目标环境 msg_archive_enabled=true。App 侧实施（workspace_access_provider/guard + J29.a-g 可控时钟测试）由并行 agent 产出中，交接时未完成未提交。
- 全量 eunit（v3 轮，8792af8a 未提交态）：Failed 52 / Passed 13473 / Skipped 0，exit=2。分类：12 处为本候选引入（channel_logic_tests mark_as_read×2=适配链 undef、conversation_logic_tests×6=mock 未对齐 _tx 契约、channel_view_watermark V3×1=W08 契约变更、pin_delete_integration×3=flaky 波动），前 9 处已修复并单跑绿（`9d0e5ee9`）；conversation_pin_delete_integration_tests 残留 2 例 c2c/c2g_message_not_ready（消息管线落库等待超时，跨轮失败集合不稳定 3→2，失败点在 delete 调用前发送段，与本候选无调用路径交集，flaky 如实记录）。其余约 40 失败为存量环境门（talk_record_owned_eunit_fixture×25 等 TALK_OWNED_PG_HOST 族、adm_group_handler、enterprise_internal_wiring_http、workspace_unread_http_checks、cache_authority_error、msg_store_ack_durability×5、isolated_pg_fixture 包裹 organization_membership_journey/textguard×3、enterprise_directory_sync_pg 等）。v4 全量（9d0e5ee9 后）交接时仍在跑，结果待接力者复核记录。
- W09.c/W08 独立审计（对照 §6.7/§6.8/§10.4 逐条）：无 P0。P1×4——①is_pinned_tx/is_deleted_tx 查询失败折叠 false（应 abort_tx，与「不吞错」冲突）；②active_generation_id_tx 查询错误折叠 0（建议 fail-closed）；③user_setting JSON 解码失败折叠 {ok,#{}} 致整份覆盖（数据风险，随 bump 扩展到四条高频旧写路径）；④上文 flag 开关-测试耦合。P2：user_setting_ds 硬匹配 ok= 绕开显式 abort 风格、unpin/restore 跨世代行物理删除且误报 unpinned/restored、W08 clear_unread 与适配两事务非原子的可接受张力、死代码（bump_conversation_version/2 等）、覆盖缺口（delete C2G tx 世代绑定、失败注入仅 bump 一步、stale 世代行 unpin/restore、conflict envelope 白名单、W08 skipped/失败日志/flag-ON 端到端）。已核对通过：同事务原子写、幂等不重复副作用、缓存/推送提交后、conflict 可回读无泄露、旧写同入口推进版本、世代写绑+读过滤、FOR UPDATE 行锁、flag OFF 逐语句与改动前一致。
- J01–J29 验收盘点（静态盘点，自动化证据口径）：父项 29——自动化 VERIFIED（部分）6（J04/J13/J14/J20/J23/J24）、BLOCKED 4（J16.c 双设备、J18 真机、J22 多平台、J28.d/e/f 部署组合/回退授权）、NOT_RUN 19；plan 级 W13 对全部 29 项 NOT_RUN（资源 BLOCKED）。系统性缺口：§9 全部指定测试落点（除 AN）零 `Jxx.y` 命名 case，向量散落新增文件；J27.d 2^53 raw-wire 链无 case（creation_receipt 套件「J27.d/e 门精度」仅为能力门整数判定）；Admin 六个指定文件零 J case。
- 状态：W09.c/W08 契约差异修复收口（后端 `8792af8a`+`9d0e5ee9`、App `b31476483`）；D6 后端侧无改动、App 侧进行中；审计 P1①②③与 P2 覆盖缺口为接力工作项；全部新合同 manifest 仍未声明；W13/J 验收 NOT_RUN（资源矩阵不变）。

#### 2026-10-28 Codex 在途收口与用户暂停

- 用户要求本轮在途修改提交、合入 main 并同步任务分支后暂停，不再推进新包。Backend 定向候选：`a9257823`（P1①②③失败中止、缺行并发互斥、stale 世代保护）、`0cf0f4e9`（W08 开关真实入口测试）、`ce4f4813`（W11 有界授权上下文）。真实隔离 PostgreSQL 仅使用容器 `imboy-v22-pg-6b2199c0` 的 `127.0.0.1:57559/imboy_v22_synthetic`；外发隔离、自动迁移关闭。
- 本轮定向：事务读失败 11/11、旧写原子性及失败注入 37/37、偏好并发 1 个 EUnit 容器、W08 flag 8/8及两个 PG 容器、W11 handler/logic 58/58与 PG 16/16、conversation_logic 6/6，均 exit=0；OpenAPI 24/24 exit=0。未完成的历史全量 v4 exit=137，不能采用为通过证据。
- main 已有 `00000231_user_dnd_rule_time_zone`，本任务迁移合并时顺延：创建 receipt=232、查看水位=233、偏好世代=234；上文旧编号仅是历史候选记录。编号/成对门通过（既有199断档告警保留），合成库内按新文件 down/up、与231 DND共存及结构断言 exit=0，整个事务最终 ROLLBACK；这不证明生产升级、真实迁移账本重放或回退安全，旧合成库账本不能冒充新编号迁移通过。
- App v36 两分支文件完全相同（不重编号），本任务仍为 v37。App W12 定向 53/53 exit=0；D6与W11最终候选仍须以本轮末次测试及提交日志为准。证据目录 `/tmp/imboy-v22-codex-handoff-7LZCre/`，历史绿灯不迁用到合并候选。
- W13/J 正式验收维持 NOT_RUN/BLOCKED；真机、双设备、混部/回退、最终全量与发布资格未完成，全部新合同 manifest 未声明。此次本地提交/合并不授权启用新合同、push、部署或生产迁移。
- 暂停收口结果：Backend 合并提交 `80b24aa5`，App 在途 `8b2c67626`（W12）、`ecd236d5a`（D6保护展示与W11上下文/真实seq分页），App 合并提交 `ab5b43eb2`。两仓 main 均已回合至任务分支，正常 pre-commit/commit-msg 钩子通过；App 合并后定向 96/96、SQLite迁移门 13 PASS/0 FAIL/0 BLOCKED，exit=0（`app-merged-focused.log`、`app-merged-migration-v2.log`）。W12另有53项定向通过，均不代表J父项或真机通过。
- Backend 合并后组合定向 exit=2：115通过、0失败、0跳过，但 W11 PG setup 被取消（要求迁移head=234，现有隔离合成库账本仍为旧233）。未修改账本或套件断言制造通过；恢复时须重新准备按最终编号升级的隔离合成库再验，本轮不关闭该门。App整仓分析曾exit=1（36条问题，包含本轮随后修复的6条类型告警及既有/测试info）；最终提交钩子分析通过且保留conversation_provider一条info，不能记为整仓零问题。Admin未改，既有deadcode强制门失败不在本轮扩修。
- 接手主仓另有已完成同树提交遗留的 cherry-pick 状态：确认与main树和patch-id相同后仅quit元数据，未reset或改写内容。Backend main原有16份未跟踪文件逐项保护，其中阻挡合并的两份plan-v1/v2原件移至证据目录的 `main-before-merge/imboy/preserved-plan-files/`；其余原位保留。全部agent已冻结关闭；本条证据补录提交并同步后按用户要求暂停，未授权操作不执行。

#### 2026-10-28 W11 隔离候选补充验证

- Backend 候选 HEAD `7ab5b76d37b15ec15112892b09503f8c99ae801a`：仓外隔离副本使用专用合成库升级至迁移 236（version `{ok,236,false}`），W11 `msg_history_context_pg_tests` 16 项、`msg_history_context_tests` 58 项、`group_history_boundary_tests` 14 项分别退出 0；日志 `/tmp/imboy-v22-w11-isolated-test.log`、`/tmp/imboy-v22-w11-api-test.log`、`/tmp/imboy-v22-w11-boundary-test.log`。副本构建重新生成的两个 protobuf 文件经 erlfmt 后与候选逐字节一致，其余跟踪文件一致；测试并非设备验收。REST 合同覆盖、零密码学和模块边界脚本分别退出 0，日志 `/tmp/imboy-v22-rest-contract.log`、`/tmp/imboy-v22-zero-crypto.log`、`/tmp/imboy-v22-module-boundaries.log`；只覆盖各脚本声明范围。
- App HEAD `6ff8620bb78425df584419c9d86d9877d5d14531`，历史上下文定向 22 项退出 0；SQLite 本地迁移门 13 PASS、0 FAIL、0 BLOCKED。相关日志 `/tmp/imboy-v22-w11-app-test.log` 与 `/tmp/imboy-v22-current-sqlite-gate.log`。上述检查在 analyzer 修复之前执行，不自动适用于后续变化。Analyzer 修复后的测试 64 项退出 0，ratchet 全仓 0 issue；当前工作树另有未跟踪 Web 大整数回归探针，Chrome 编译因七处大整数常量失败且未运行断言。App 全量曾中断，退出 130，不是通过。
- Admin HEAD `69feaf0408177fa7d856ee7c89c35274857de8c4` 加一处未提交移除内部类型导出；隔离候选 deadcode/lint/typecheck/build 退出 0，覆盖率测试 2848 通过、0 失败，函数 78.00%、行 88.79%；主仓同一 diff 的 deadcode 退出 0。运行输入及所有三端候选仍需最终重绑定。
- 本节为局部验证记录，不关闭 J18、J27、W11 或 W13；真实设备、全量、浏览器原始报文及发布验收仍 NOT_RUN/BLOCKED。

#### 2026-10-28 Web 大整数与引用定位续查

- Chrome 原始响应探针复现 `9007199254740993` 经默认 Dio 解码和频道模型变为 `9007199254740992`，退出 1；仅证明该探针链路，非完整业务请求。新建未接线的定向解码切片及测试，在 VM、Chrome 各 5 项通过，覆盖 HTML 502 与空响应，独立复审通过；频道模型、其他 ID 字段、WebSocket 和真实请求链仍未贯通。
- Chrome 使用项目 `web/sqlite3.wasm` 与真实 SQLite Web 驱动：将字符串 `9007199254740993` 写入 INTEGER 列，按原字符串查询并以 CAST TEXT 原值读回，1 项退出 0；测试源码与日志分别为 `/tmp/imboy-v22-channel-web-sqlite-id-test.dart` 和 `/tmp/imboy-v22-channel-web-sqlite-id-r3.log`。这是原始 SQL 实验，不证明频道仓库和旧数据正确，也不决定是否需要迁移。
- J18 静态复核：过期、403、缺钥提示已有实现；滚动定位可能在目标未实际挂载时报告成功，且既有 key 不一定指向点击的引用目标，修复和验证待完成。以上均不关闭 J18、J27 或 W13；候选与最终验收仍需重新绑定。

#### 2026-10-28 接力续跑：中断遗留修复登记、合成库重建与分组提交

- 接手状态：前序会话因额度中断，Backend 候选 HEAD `7ab5b76d` 工作树遗留三组源码修复与一个回归测试未提交、未在本计划登记；App HEAD `6ff8620bb` 工作树遗留 249 个脏文件（ID 字符串化接线、会话隔离、消息定位），analyze 1 error、21 个用例失败。以下逐项登记与收口。
- Backend 修复登记（均未对应本计划 F/C/D/W/J 编号，属中断期间修正的既有缺陷）：
  - `f97686d8` fix(friend)：`confirm_friend/4` 收紧为确认人只能确认发给自己的申请（To 规范化后须等于 CurrentUid，以已绑定变量相等匹配实现并加注释），From/To 经 `confirm_id/1` 限定正整数或 ≤19 位纯数字串且 ≤int8；越权/非法在 pending gating 之前拒绝且零 DS 调用。既有调用方（friend_handler 4 参形态、CT friend_management_flow/websocket_connection_flow、REST confirm_request）契约核对无破坏。friend_logic_tests 28/28、friend_handler_tests 9/9、新增 friend_confirm_authorization_pg_tests exit 0。
  - `3dc0aaf2` fix(channel)：评论 `create/5` 的 parent_id 经 `parse_parent_id/1` 白名单（≥0 且 ≤int8，拒绝负数/小数/非数字/空串/null）→ 400「无效的父评论ID」。channel_comment_logic_tests 5/5、新增 channel_comment_parent_pg_tests exit 0（含 2^53+1 原值读回）。
  - `4a1971d0` fix(fts)：`group_search_page/4` 空关键词带分类分支 SQL 用 `$3` 但原恒传两参，带分类发现页触发 PG "no parameter $3"，补传 CategoryId。fts_group_repo_tests 8/8（断言升级为精确参数列表）、新增 fts_group_discover_pg_tests exit 0。
  - `efd51fae` test(message)：peer_id 整数与 2^53+1 十进制串经 `validate_message` 均接受的 R10 回归锁定，message_ds_tests 19/19。
  - 三个新 PG 套件接手时 setup 为 `{ok, Conn} =` 硬匹配（缺库 badmatch 且不在全量 Excl），已对齐 TEST_WITH_DB/cs_* 惯例：`eunit_runner:eunit_setup_with_db/0` 直接作 setup，`{error,_}` 返回 `[]` skip；fts 手工 `meck:new(elib_pg)` 改 `?WITH_MECKS`。三者无全局 runtime 改写，不进 Excl；缺库运行不得计为通过。
- 合成库：`imboy_v22_synthetic@127.0.0.1:57559` 账本停在 233 而 `conversation_pin/conversation_delete.generation_id` 列与索引已存在——`80b24aa5` 合并时迁移重编号为 232–234 造成账本与库结构漂移，app 启动自动升级在 234 报 duplicate_column 并留下 dirty。未 force 账本（旧编号 232/233 对应内容不可靠），以同容器内 `imboy_v22_w11_42676176e1`（前序会话 W11 专用库，账本 236 干净）为 TEMPLATE 重建同名库（`DROP ... WITH (FORCE)` + `CREATE DATABASE ... TEMPLATE`），重建后账本 `236|f`。残留 w11 合成行（group 8 行 UID 7xxx 系列 category_id NULL、user 2 行 agent_ark/agent_bailian 迁移种子）保留，与本轮套件 ID 不相交。`config/sys.local.config` 已指向该库，`make eunit-local` 默认即连合成库，未触碰共享库 imboy_pg18:4323。
- Backend 定向验证（上述候选，逐模块 eunit-local/inner，日志 `/tmp/imboy-v22-takeover-*.log`）：8 模块全 exit 0；3 个 PG 套件日志均含 `Application imboy started`、无迁移错误、无 skip。friend_logic 加注释后重跑 28/28。
- App 接力（基于 `6ff8620bb` 工作树）：analyze 唯一 error 为 e2ee_bootstrap_test 引用已删除的 `unawaitedFn`（删测被移除行为的用例）；14 个失败为断言仍期望 int 而实现已返回 string（channel/group/workspace/store/http 五处，含自写 decoder 测试期望写错），逐个核对实现后同步；e2ee 6 个失败为 fixture 未初始化 StorageService/currentUid 触发新账号归属守卫（守卫为有意 fail-closed，lib 零改动，补 setUpAll 与对称性用例的 currentUid 切换）与 secret_inventory_registry 一条死注册条目。修复后 analyze 0 issues；ID wire + 消息定位组 +1831 全过（20 skip）；e2ee + mine + contact 组 +1090 -1，唯一失败 olm_pfs_production_path_test 为 sqflite_sqlcipher 平台通道缺失环境门；offline_group_wire_web_test 需 `-D SQLITE_WASM_URL` 条件跳过。dart format 16 文件仅格式。分组提交：`85b050e70`（ID 十进制字符串化 + 会话隔离/E2EE，242 文件；两主题在 denylist/contact/new_friend/message_s2c 等 14 个核心文件交叉、不可独立回滚，故合并）、`afc19287a`（消息定位真实可见性，J18 修复）、`80cc19f7c`（Semantics flagsCollection 适配，与三主题无关）。未跑全量与 integration_test。
- 修正前序记录：上节「Web 大整数与引用定位续查」所述「新建未接线的定向解码切片」「频道模型、其他 ID 字段、WebSocket 和真实请求链仍未贯通」「滚动定位……修复和验证待完成」均已被 `85b050e70`/`afc19287a` 超越——解码器已挂接 16 个 api 与 message_s2c，模型 ID 全链路字符串化，定位可见性修复落地；该节保留为当时快照。
- 全量门 v5（候选 `efd51fae`）：全量 eunit-local exit 2，Failed 59 / Passed 13641 / Skipped 0（日志 `/tmp/imboy-v22-eunit-full-v5.log`）；dialyze 31 条 NEW 告警（codex 合并带入，未过棘轮门，日志 `/tmp/imboy-v22-dialyze-v5.log`）。分类：59 个失败中 0 个由本窗口引入——51 个为 v3 基线内既有环境门失败（talk_record fixture 包装 ×26、PG/压力套件等），另 8 个为窗口外行为漂移（msg_store_ack ×5：worker 调 `msg_c2c_ds:write_msg/11` 而测试 mock 停在 /9；organization_invitation_notify ×1：`push_notification_logic` 已改调 `is_dnd_now/1` 而 mock 停在 `is_dnd_at`；cs_closure ×1：`zone_day_window/2` 端口回调欠登记冻结契约表；enterprise_internal_wiring_http ×1：即下条漏导出 bug）。dialyze 31 条中 2 条为 W07.a（`2e9ac73d`）漏导出真 bug，修复后 28 条交子代理清零。
- dialyze 修复（两轮，未改 `dialyzer.baseline`，无 `-dialyzer nowarn`）：`25f9c930` 补齐 `workspace_channel_page/5`、`list_workspace_groups/{3,before}/4` 导出（W07.a 遗留，工作台路径运行时 undef）+ workspace_handler 两处 `{ok,_}` 元组未解包真 bug（cursor 传元组、`respond_ws_group_signed` 对元组做 map 更新）+ group_repo SQL 缺 `/binary` + spec 拓宽与死子句清理（按上游 spec 准确才删，`join_initial_member` 保留 P0 回滚语义并以 `{ok,_} =` 精确匹配，group_logic_tests mock 对齐真实 `{abort_tx,_}` 失败通道）。修后 `make dialyze-check` exit 0 GREEN（176 条全部基线内、新增 0，日志 `/tmp/imboy-v22-dialyze-v7.log` 与 `dialyze-last.log`）。
- 窗口外漂移对齐 `81cee79e`：msg_store_ack mock 升 /11、organization_invitation_notify mock 改 `is_dnd_now/1`、cs_ports 冻结合约表补录 `{zone_day_window, 2}`（dcc10eea 欠登记，cs_closure 按 behaviour_info 排序集核对）。
- 修复涉及模块及直接调用方定向验证 26 模块次全 EXIT=0（前节 8 次 + 本次 18 次：group_repo 16/16、channel_handler 106/106、group_logic 23/23、channel_invitation_repo 15/15、channel_logic_subscription 14/14、channel_logic_message 20/20、group_handler 5/5、msg_store_ack 5/5、organization_invitation_notify 3/3、cs_closure 8/8、discovery/contract/workspace 游标套件与 3 个 cursor PG 套件 pass；日志 `/tmp/imboy-v22-takeover2-*.log`、`/tmp/imboy-v22-inner4-summary.txt`）。
- 全量门 v6 终验（候选 `81cee79e`）：全量 eunit-local exit 2，Failed 51 / Passed 13649 / Skipped 0，耗时 2076s（日志 `/tmp/imboy-v22-eunit-full-v6.log`）——较 v5 恰好减少 8 个失败，逐一对应上述修复；余 51 个均为 v3 基线内既有环境门失败，本窗口新增失败 0（msg_history_context ×2 仍为 VM 污染敏感，单独运行 58/58 全过）；dialyze-check exit 0 GREEN，176 条全部基线内、新增 0（新鲜分析，日志 `/tmp/imboy-v22-dialyze-v6-final.log`）。后端最终候选 `81cee79e`，门禁双绿。
- 本节为登记与定向验证，不关闭任何 J 项、W11 或 W13；真实设备、浏览器原始报文、integration_test 与发布验收仍 NOT_RUN/BLOCKED；三端候选仍需最终重绑定。
