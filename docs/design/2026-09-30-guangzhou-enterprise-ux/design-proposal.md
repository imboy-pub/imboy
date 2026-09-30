# 广州企业空间 UX 设计提案 V1

> 2026-10-01：此文件和配套 V1 原型为历史概念稿；导航、范围与实现建议以 [V2 定稿建议](./ux-decision-v2.md) 为准。V2 已补齐全部计划正文阅读和当前企业链路核对；旧稿中相冲突内容不再作为实施依据。

日期：2026-09-30。状态：DESIGN_PROPOSED；尚未实施、尚未进行真机或客户验收。

## 结论

建议采用企业微信式的轻量日常导航，借鉴飞书的阅读/管理分离、钉钉的企业通讯录与 OA 工作台。员工只需要理解“公司、同事、群聊、公告、办公”，不需要学习 Organization、Workspace、Application、Grant。底层实体与权限继续保留。

推荐广州企业模式底部四项：消息、通讯录、工作台、我。工作台仅在支持的平台且有有效配置时出现；未配置时为三项。企业频道从一级 Tab 收到消息页“公告”分类中；广州项目功能保持关闭。通用个人模式继续使用其既有导航。

这里的“广州企业模式”是待实施的展示规则，不能通过简单改名字假装完成数据范围切换。

## 调查范围与证据边界

对 docs/plans 下全部 26 个 Markdown 文件建立内容索引，提取 UI/UX、广州需求、角色、旅程与边界；重点对照广州 APP、Enterprise V2.1、Enterprise UX V1.1、工作台 OA 计划及当前入口源码。不是对 12,266 行逐项完成实施审计；部署、迁移、通话等计划只用于确认依赖与限制。末尾记录每份文档校验和，避免历史版本混为一谈。

源码基线：
- imboy：c0e8660d55e8699e24cf720d7833313c6afaeee8
- imboyapp：54576b776f4691c88f4c42726c1b9aa20d9ede26
- imboyadmin：5598b7ef45b06670f185bf17100fb1155d8e0376

三个仓库均存在用户 WIP。本报告依据当时工作区源码，不能声称全部事实属于以上提交的纯净快照。未启动服务、未用真实账户、未验证生产与真机。

## 当前复杂度的来源

| 事实（CONFIRMED） | 对用户的影响（INFERRED） | 提案（PROPOSED） |
|---|---|---|
| organization_detail_page.dart 展示成员、部门、群、频道、工作区工具、企业工具以及管理入口 | 页面按内部实体分组，用户难判断该点哪个 | 去掉日常企业“目录首页”，进入企业就到消息 |
| 详情页群与频道入口都 push 到同一个 manage 页面 | 看群与管群混淆，入口语义与目标不一致 | 群入口进入已加入群；公告入口进入已订阅企业频道；治理只从管理入口进入 |
| organization_tools_page.dart 聚合已加入工作区与项目 | 广州关闭项目后，“工具”入口缺少日常用途 | 广州不展示该项目聚合入口；OA 独立工作台 |
| 底部导航由 Conversation、Contact、可选 Channel、可选 OA、Mine 构成 | 个人频道与企业公告竞争一级入口 | 企业模式收起独立频道 Tab；个人模式保留 |
| /enterprise 页面加载首个组织、业务身份、业务联系人与企业托管会话 | 容易和普通员工企业群聊混为一谈 | 保留业务域，按授权从客服/业务入口进入，不能直接与 C2G 合并 |
| Admin 企业组包含九个叶子，企业群/频道使用 preset=enterprise | 用户反复选择企业与工作区；菜单预设不等于租户授权 | 企业上下文置于页头，业务导航收敛；继续由服务端校验 |
| OA Tab 当前仍是编译期配置；路由未发现 Human workbench entries 端点 | 工作台计划的“标准包动态发现”不能算已实现 | 依赖 9/29 计划补齐配置发现与生命周期 |

关键源码（均相对于各独立仓库）：
- imboyapp/lib/modules/organization/presentation/organization_detail_page.dart:296
- imboyapp/lib/modules/organization/presentation/organization_tools_page.dart
- imboyapp/lib/modules/organization/presentation/organization_manage_page.dart
- imboyapp/lib/modules/organization/infrastructure/current_organization_store.dart
- imboyapp/lib/page/bottom_navigation/bottom_navigation_page.dart:111
- imboyapp/lib/modules/enterprise/presentation/enterprise_home_page.dart:76
- imboyadmin/src/components/layout/sidebarSchema.ts:191
- imboy/src/adm/adm_admin_handler.erl:910
- imboy/src/imboy_router.erl:91

## 成熟产品参考

取用交互原则，不复制整套产品功能；本轮未登录竞品，也未逐屏复刻最新客户端。

- 企业微信：参考轻量企业沟通方向。本轮官方页面检索没有取得足够可靠的当前布局说明，不把四 Tab 作为已核实的最新版本事实。
- 飞书：[成员与部门管理](https://www.feishu.cn/hc/zh-CN/articles/360033588234-管理员创建并管理部门)支持管理后台维护部门；[管理员角色](https://www.feishu.cn/hc/zh-CN/articles/927897012636-管理员类型和角色介绍)区分治理权限。借鉴阅读与维护分离。
- 飞书：[公告应用](https://www.feishu.cn/hc/zh-CN/articles/360041499553-使用公告应用)展示工作台公告入口。只借鉴公告易发现；不据此为 IMBoy增加签署/催读/已读统计。
- 钉钉：[企业通讯录](https://page.dingtalk.com/wow/dingtalk/act/addressbookpc)强调找同事与跨部门沟通；[H5 微应用](https://open.dingtalk.com/doc-mobile)支持应用进入工作台。借鉴找人路径与 OA 容器。

## 用户与信息架构

### 员工

消息：当前公司名称 + 搜索 + 全部/群聊/公告分类。默认显示本人有权查看的资源。“全部”仅指当前企业内可见消息，不包含全部公司数据。已有个人对话不凭通讯录关系强行变成企业资源。

通讯录：当前公司名称 + 搜索同事 + 部门树 + 同事列表。点人进详情，再使用已实现并获授权的聊天动作；无需先进入企业详情或工作区壳。普通员工不显示组织维护按钮。

工作台：一期点击直接进入本企业配置 OA H5。页面可提供原生标题、返回/关闭与重试，OA 内部布局由客户系统负责。无原生审批数、考勤、财务报表、应用宫格。SSO、origin、清会话、平台守卫按既有合同执行。

我：个人资料、通知、安全、加入/切换企业。管理员额外看到“管理企业”。企业切换也可从各页头公司名进入同一选择器。

### 老板 / 企业管理员

默认也看员工消息页；管理不是老板登录后的强制首页。“我 → 管理企业”进入四类任务：成员与部门、群与公告、工作区、企业设置。邀请同事是成员页的主要动作。归档、移交与解散放到详情“更多”，显示具体对象与影响，并二次确认。

工作区 Owner 只看到其负责工作区的资源和治理动作，不显示企业级成员角色、企业归档或全公司数据。

### 平台 Admin

保留现有平台后台。提案中的“企业控制台”是平台管理员选定企业后的聚焦视图，不是让企业老板拿 Admin Cookie 登录。老板治理复用 Human JWT 与已有 membership 权限；若以后建设老板 Web 控制台，需单独核验 Human 网页认证与接口覆盖。

企业聚焦侧栏：概览（纯快捷入口、不新增统计）、成员与部门、群与公告、应用与集成、设置与审计。工作区放在设置子页；离岗交接放成员详情；客服仅在有授权时作为独立业务入口。平台运营、财务、系统能力仍可从平台模式进入。

## 企业 scope 的具体行为

- 企业是顶层上下文；Workspace 是企业内部资源范围，绝不能改称部门。部门与 Workspace 成员关系不等价。
- 只有一个已加入工作区时隐藏工作区选择；多个时在消息页显示次级选择器。默认进入既有默认/最近且仍获授权的工作区；不能凭本地指针扩大可见范围。
- “全员群”“公告频道”属于所选 Workspace。多工作区下显示“总部 · 全员群”，避免误以为企业全部成员都在该群。
- 切换复用现有 membership 真源、两阶段提交与缓存失效。加载失败保留原企业，提示“切换失败，仍在原企业”，不能页头先切而消息仍旧。
- 搜索、分页、列表与消息详情均绑定 account + organization + 必要的 workspace；服务端重新验证。菜单隐藏与 preset 仅展示规则。
- 离岗、停用、归档、邀请过期分别展示，不能统一当空列表。无企业时显示加入邀请/输入邀请码，不引导搜索企业。
- OA 代发保留“来自企业应用”事实与来源详情；不得出现 E2EE 标识。Human 群聊继续既有 E2EE。

## 视觉与交互规则

原型沿用 IMBoy 名称，无客户 Logo 或实际联系方式；所有名字、人数、消息均为虚构演示内容。

企业蓝作为操作强调，浅灰列表背景，白色主体；列表优先于大卡片宫格。统一 44px 以上点击目标、清晰焦点与状态文本；实现时复用 AppColors/AppSpacing/FontSizeType，支持暗色与字号放大。

页面只保留一个主要动作。列表行显示名称、摘要、时间；ID、scope、role code 放详情诊断区。员工界面使用“企业”“同事”“公告”，管理员资源页保留“工作区”帮助理解范围。

## 关键旅程与验收

| ID | 旅程 | 验收 |
|---|---|---|
| UX-01 | 接受邀请进入公司 | 默认工作区与全员群可发现；不经过多级企业目录 |
| UX-02 | 找同事 | 通讯录 → 人员详情最多两次点击；查询只在授权企业 |
| UX-03 | 看公告 | 消息 → 公告分类；多工作区名称不歧义；无个人付费频道混入 |
| UX-04 | 老板邀请成员 | 我 → 管理企业 → 成员；不输入 TSID，不扩大权限 |
| UX-05 | 切换公司失败 | 原企业与消息保持一致；错误可重试；无前企业缓存泄漏 |
| UX-06 | OA | Android/iOS 有配置时可达；失败/过期可恢复；macOS/Web 不显示未支持入口 |
| UX-07 | 管理资源 | 员工看不到写动作；Owner 范围正确；服务端拒绝跨 scope |
| UX-08 | 广州功能收敛 | 项目正常入口与直达路由都关闭；个人模式行为保留 |

静态效果图只用于布局评审；不是以上旅程的验证证据。必须在实施后使用真实 Android/iOS 设备检验，不能用模拟器代替。

## 落地步骤

### Step 1 — 收敛企业导航与角色入口

意图：确认企业模式与个人模式的入口配置，移除企业目录首页的日常必经路径，定义角色矩阵。复用现有选择器、路由和页面，不重建 Organization 数据模型。

Acceptance：四项/三项导航状态完整；普通员工与管理员动作矩阵明确；广州项目入口与直达规则一致。

Out of scope: 新增数据表、客户白标、生产部署。

### Step 2 — 消息、公告与通讯录布局

意图：复用现有群、频道、通讯录页面与接口，提供当前企业内的可见资源布局。实施前先验证群/频道读取是否能完整满足组织与工作区过滤；不支持时登记精确缺口，禁止先展示错误范围。

Acceptance：群和公告不再跳到管理页；账号/企业/工作区范围隔离；切换失败保留原上下文。

Out of scope: 新增数据表、OA 审批业务、公告签署与已读统计。

### Step 3 — 管理入口与企业聚焦后台

意图：APP 管理按成员、群与公告、工作区、设置分区；平台 Admin 复用现有路由、筛选与 Drawer，统一服务端侧栏和客户端 fallback。不得向老板开放平台管理员登录与权限。

Acceptance：成员/资源列表保留分页；Workspace Owner 权限正确；preset 与直接 URL 均不能跨企业越权。

Out of scope: 新增老板 Web 登录体系、平台权限下放、生产迁移。

### Step 4 — 工作台与真实旅程门

意图：按 9/29 工作台计划补齐配置发现、切换保活、登出清理及平台门控，再验证广州核心体验；本提案沿用 OA 直接 H5，不建设应用宫格。外部 OA 缺失时本地容器与客户联调分别记录。

Acceptance：配置错误不展示必败入口；登出/切组织无 OA 会话残留；Android/iOS 核心旅程有真实设备证据，外部 OA 未验收保持 BLOCKED_EXTERNAL_OA。

Out of scope: OA 侧业务开发、应用宫格、待办聚合、push、发布、部署。

## Plan-Orchestrate Result

Plan：本文件；Lang：unknown（Flutter、TypeScript、Erlang 跨仓，不指定单一语言）；ECC mode：plugin；Steps：4；Scope：all。

| # | Title | Tags | Chain |
|---|---|---|---|
| 1 | 收敛导航与角色 | design | ecc:planner,ecc:architect |
| 2 | 消息公告通讯录 | impl,security | ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer |
| 3 | 管理与企业后台 | impl,security | ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer |
| 4 | 工作台与旅程门 | test | ecc:tdd-guide,ecc:e2e-runner |

链理由：第 1 步评审产品与架构；第 2/3 步由通用审查覆盖跨语言，安全审查收尾，实际代码需各语言专项检查；第 4 步以真实旅程验收收尾。下面只生成命令，没有执行 orchestrate。

### Step 1 编排命令

```text
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-1] 只读评审广州企业空间的日常导航与角色入口。以当前三仓源码重新验证本报告事实，列出企业/个人展示规则、成员/管理员/工作区Owner矩阵、现有route复用和关闭项目规则。不要把企业业务托管会话混入普通员工群聊，不凭历史计划认定代码已实现。输出页面流、精确改动路径和依赖，保留共享WIP；Acceptance: 三或四项底部导航状态完整；角色动作矩阵可机械核对；广州项目入口与直达规则一致；Out of scope: 新增数据表、客户白标、生产部署。"
```

### Step 2 编排命令

```text
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-2] 实施消息、公告与通讯录的最小布局变化，先核验群/频道读接口能按当前企业和工作区限定本人可见数据，再复用现有页面与provider。保留个人模式与E2EE，企业频道免费内部可见，OA来源事实保留。缺读接口时停在精确缺口，不用客户端过滤冒充授权。按account/org/workspace隔离并复用两阶段切换，保护他人WIP；Acceptance: 群公告进入阅读面；跨scope负例被服务端拒绝；切换失败原页头与列表一致；Out of scope: 新增数据表、OA 审批业务、公告签署与已读统计。"
```

### Step 3 编排命令

```text
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-3] 实施APP成员、群与公告、工作区、设置四类治理入口，平台Admin建立选定企业的聚焦视图并复用现有路由与Drawer。同步服务端侧栏配置与前端fallback，列表保持默认size10及筛选重置page1。老板继续HumanJWT治理，客服使用独立SeatJWT；WorkspaceOwner只治理所辖资源。保护共享WIP并按功能验证；Acceptance: 既有分页操作可达；各角色写权限矩阵通过；preset和直接URL均不越权；Out of scope: 新增老板 Web 登录体系、平台权限下放、生产迁移。"
```

### Step 4 编排命令

```text
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-4] 先对照2026-09-29工作台OA计划确认已实施项，补齐Human配置发现、OA保活、登出清理、组织切换和平台守卫，再验收邀请入企、找同事、读公告、老板邀请、scope切换失败等核心旅程。工作台一期直接H5，保留一次性SSO和exact-origin限制，禁止模拟器充当设备证据。记录三仓候选SHA与dirty基线，缺设备/真实OA分别阻塞对应门；Acceptance: 配置失败无必败入口；会话隔离检查通过；Android/iOS真机证据或明确BLOCKED；Out of scope: OA 侧业务开发、应用宫格、待办聚合、push、发布、部署。"
```

### Batch execution

以下为顺序执行命令，每一步先通过其依赖门。

```text
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-1] 只读评审广州企业空间的日常导航与角色入口。以当前三仓源码重新验证本报告事实，列出企业/个人展示规则、成员/管理员/工作区Owner矩阵、现有route复用和关闭项目规则。不要把企业业务托管会话混入普通员工群聊，不凭历史计划认定代码已实现。输出页面流、精确改动路径和依赖，保留共享WIP；Acceptance: 三或四项底部导航状态完整；角色动作矩阵可机械核对；广州项目入口与直达规则一致；Out of scope: 新增数据表、客户白标、生产部署。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-2] 实施消息、公告与通讯录的最小布局变化，先核验群/频道读接口能按当前企业和工作区限定本人可见数据，再复用现有页面与provider。保留个人模式与E2EE，企业频道免费内部可见，OA来源事实保留。缺读接口时停在精确缺口，不用客户端过滤冒充授权。按account/org/workspace隔离并复用两阶段切换，保护他人WIP；Acceptance: 群公告进入阅读面；跨scope负例被服务端拒绝；切换失败原页头与列表一致；Out of scope: 新增数据表、OA 审批业务、公告签署与已读统计。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-3] 实施APP成员、群与公告、工作区、设置四类治理入口，平台Admin建立选定企业的聚焦视图并复用现有路由与Drawer。同步服务端侧栏配置与前端fallback，列表保持默认size10及筛选重置page1。老板继续HumanJWT治理，客服使用独立SeatJWT；WorkspaceOwner只治理所辖资源。保护共享WIP并按功能验证；Acceptance: 既有分页操作可达；各角色写权限矩阵通过；preset和直接URL均不越权；Out of scope: 新增老板 Web 登录体系、平台权限下放、生产迁移。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/design-proposal.md#step-4] 先对照2026-09-29工作台OA计划确认已实施项，补齐Human配置发现、OA保活、登出清理、组织切换和平台守卫，再验收邀请入企、找同事、读公告、老板邀请、scope切换失败等核心旅程。工作台一期直接H5，保留一次性SSO和exact-origin限制，禁止模拟器充当设备证据。记录三仓候选SHA与dirty基线，缺设备/真实OA分别阻塞对应门；Acceptance: 配置失败无必败入口；会话隔离检查通过；Android/iOS真机证据或明确BLOCKED；Out of scope: OA 侧业务开发、应用宫格、待办聚合、push、发布、部署。"
```

## 文档索引

| 文档 | 行数 | SHA-256 |
|---|---:|---|
| 2026-09-20-customer-service-hosted-widget-deploy-plan-v1.md | 596 | `9990c8590d3c37277b1d1d94b2295385feb1d6d3d65fa6514f2b47c44da6bbe1` |
| 2026-09-20-customer-service-hosted-widget-deploy-prompt-v1.md | 162 | `313c9480bc0066a2f73a939f679d9614d5e74002ca680151239b107bc48a6f16` |
| 2026-09-20-customer-service-widget-seat-web-zcode-plan-v1.md | 938 | `f29ae7b23666c6e084dcdf77507829d652fcc3f725daeb7b902503c13e8d71f7` |
| 2026-09-20-customer-service-widget-seat-web-zcode-prompt-v1.md | 183 | `56081b2350abbf357c754556ebc8e6adbc7801c8814edf30e7e5fbff3a79d385` |
| 2026-09-20-enterprise-pilot-v1-implementation-plan.md | 360 | `d98e01bbb684f19e27bc33dd57e3802632c753e07868d36abc9ef144939344c2` |
| 2026-09-21-enterprise-admin-prod-customer-service-v1-implementation-plan.md | 393 | `0f4cf1948192182fc6adc4ddb90861635ad18108dbbdc1ae28ef98600696eb37` |
| 2026-09-21-enterprise-admin-prod-customer-service-v1-zcode-prompt.md | 169 | `350366f806c0754e76ed90b4dad039bb889c6ce2dc8ab153283d7f8476dcad1a` |
| 2026-09-21-enterprise-internal-platform-full-v1-implementation-plan.md | 151 | `6d3cd8fd780abb6e6f9444c8604f9c61f1b0a4425e3c52a6164a1e6e8c3f6781` |
| 2026-09-21-enterprise-pilot-v1-zcode-prompt.md | 216 | `5983fee11d4f98d2259fde057001d12a5cd887270e8e5b729aebd2afca2a1e30` |
| 2026-09-21-guangzhou-enterprise-app-v1-implementation-plan.md | 333 | `ce56ce6c674faf3d6700c02cea1ab8fc3b7157aef7640d81cee3d0967fac28c0` |
| 2026-09-21-livekit-single-service-migration-deployment-plan-v1.md | 471 | `28aa35a4c8439266f2ee6f84764b373af586aecb243c5945d7cf20e55a2d337e` |
| 2026-09-21-livekit-single-service-zcode-orchestrate-v1.md | 287 | `542437597910192965099608ee3f56b7cc569e80e320946760c44300e30a2355` |
| 2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md | 1095 | `a866aa137eea856a9a6d9324d3cb46d3fb1f1fb8db73891808c85d1f17d23302` |
| 2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.md | 951 | `5e65971a6a83834d6981992a03d652775407a2e756aab036831cacf5abe18264` |
| 2026-09-23-enterprise-organization-admin-internal-v1-unified-plan.md | 704 | `625f3931c2e65d5082da4b6db0dcb50533d179bde762506b4b2f15e39936c74b` |
| 2026-09-24-customer-service-agent-workspace-enterprise-ux-efficient-zcode-plan-v1.1.md | 1201 | `cd952bd45a9dcb4a4939d1aecdc632ee041a0584fdb351b46601c1bb5e1f4967` |
| 2026-09-24-enterprise-organization-admin-internal-v21-current-main-closure-plan-v1.md | 363 | `f482c4888c415e53b5ec4eb8be4ad0807f99628e01235fe029d17187bcf03a24` |
| 2026-09-26-enterprise-organization-admin-internal-v21-final-closure-plan-v2.1.md | 474 | `9ac84b1a2464e4dc3ae9b35c57e8964fc15acbf1768bdb5fa99753f1fc59ef98` |
| 2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.1.md | 493 | `e24526d8eb25467e30564fd00220238ee54da8f85e68b1008b0665a1a3c2b725` |
| 2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2-unattended-zcode-prompt-v1.md | 276 | `504eacdbb9290914e798146d5069db4103210500870caaaa39ee208783d74ef9` |
| 2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2.md | 612 | `e26e0b29fe6accf3e6a6bf4b7b30d5c53ce5ff07ded3144d6cdaab776c47d918` |
| 2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.md | 341 | `60b88658c1d9377f4445a4a458128b27d1735939c91fc67b50e87387b377bb4d` |
| 2026-09-28-cross-plan-production-readiness-closure-v1.3.md | 716 | `bf325eeb5709dc884260d72e3c7a6b39aa74cfe313c22534c110d8e0713ff4de` |
| 2026-09-28-cs-message-plaintext-storage-evaluation-v1.md | 111 | `ac938a53f1805338c9041bfa006d8c8cbfb1eae34a85ce0105bda098a24c670c` |
| 2026-09-28-customer-service-seat-console-embed-mall-admin-plan-v1.md | 198 | `efecd3e8b5d878ff5d3fe21677ddd4dd2304e3b4664c306c6d8d0521e396b744` |
| 2026-09-29-workbench-tab-oa-h5-v1-implementation-plan.md | 472 | `77a747a655a3c7c2792e09e529934108c4a15cd8fab833879677e3c334563fe0` |

## V1.1 补充 — Personal 与 Workspace 切换及裁剪

对应效果稿：enterprise-scope-detail.svg。静态六屏，用于评审目标交互。

### 切换规则

采用一个页头选择器：个人空间 / 我的企业；选企业后展开其已加入工作区。点击公司名切企业，点击次级工作区名直接展开同一个选择器中的工作区部分。只有一个工作区直接进入，多个时恢复该账号在该企业最近一次仍有权限的工作区，否则选获授权默认工作区；默认工作区不可访问时进入明确空态，不擅自加入。

Personal 顶层不是组织空指针的别名：明确的 UI 模式决定个人页面，组织指针保留最近企业偏好。不要通过清掉 CurrentOrganizationStore 来假装个人模式，再让 WorkspacePicker 的 null 分支显示全部企业工作区。

切换层级：账号 → 展示模式（个人/企业）→ 企业 membership → 工作区 membership。Workspace 仍是资源范围，部门是组织架构。非企业 Workspace 若真实存在且仍需使用，归单独“其他工作区”分组，先核验模型与产品需求，不强塞进任何公司。

切换先校验目标 membership 和资源、完成加载后再提交页头及页面。失败维持原 scope；离岗/失权清除可见缓存并返回选择器；登出清除该账号会话。每个 scope 记住自己的列表滚动位置与选中分类，草稿沿用现有会话标识隔离。存在尚未完成的附件发送时阻止误切或明确提示其仍在原会话继续，不悄悄改发送目标。

### 页面范围

| 内容 | 个人模式 | 企业工作区模式 |
|---|---|---|
| 私人消息 | 个人会话列表 | 跨域提醒，点击回个人；不伪装成工作区消息 |
| 群 | 个人群 | 所选 Workspace 中本人已加入群 |
| 频道 | 个人订阅/发现、既有付费行为 | 内部频道与公告，无公开/付费入口 |
| 通讯录 | 既有个人联系人 | 企业授权目录；工作区成员另在资源详情查看 |
| 工作台 | 不显示企业 OA | 有配置且平台支持才显示，按 OA 已有 org/app 合同 |
| 未读 | 个人未读 | 当前工作区未读；其他域只提示，需先核验计数来源 |
| 管理 | 个人设置 | 按企业和工作区角色裁剪 |

企业级 OA 不因切 Workspace 强制重启；若配置授予范围实际含 Workspace，则必须重验后使用。企业切换与登出需要清理旧 OA 会话。OA 代发消息走已存在托管业务域，不仅凭 sender 或公司成员关系加入普通 E2EE 列表。

同事详情的“发起聊天”是目标交互：实现前核验现有 directory 到聊天路由与授权合同。若现有聊天是个人 C2C，应明确切回个人模式打开；若企业托管 direct 有独立授权，再从其业务入口打开。不能新造一个“同事 DM 属工作区”的安全假设。

### 可裁剪项

| 入口/页面 | 建议 | 删除前条件 |
|---|---|---|
| 企业目录首页 | 从日常流程移除，选择器承接切换，管理页承接治理 | 所有原入口与旧 URL 有去向 |
| 企业工具与工作区工具双入口 | 广州不显示项目工具聚合 | 未删除通用项目能力 |
| 群/频道跳同一管理页 | 改阅读入口，治理集中管理企业 | 正常列表与详情权限完整 |
| 工作区五项导航 | 广州收敛为消息/通讯录/可选工作台/我 | 工作区群、公告、成员仍可达 |
| 工作区 Overview 卡片汇总 | 广州可去掉一级入口，文件回各群/频道详情 | 最近文件的原始资源入口可达 |
| 创建工作区常驻员工 footer | 按角色显示，移入管理企业 | 服务端创建权限仍独立验证 |
| 首屏业务身份/联系人/托管会话混合列表 | 普通员工不强推；业务身份有授权才提供 | 不能删除 enterprise 托管域及客服能力 |
| 当前 scope 与底部同时存在频道入口 | 企业模式合并到消息分类 | 个人频道原流程保留 |

本轮只更新设计文件，没有删除业务文件。优先丢弃重复展示与导航，实体、权限、加密与历史数据不动；只有引用、路由、功能开关与角色旅程都确认不再需要的页面才删代码。

### 补充验收

个人 → 公司A总部 → A销售 → 公司B → 个人，逐步核对标题、消息、通讯录、未读、草稿、OA及权限。至少覆盖切换超时、成员被停用、工作区归档、账号A/B、一个/多个/无工作区、未配置OA。桌面沿用相同页头层级，OA平台限制不因布局稿被解除。

## V1.2 可点击原型

clickable-prototype.html 提供 32 个页面状态，通过原生锚点和展开面板切换。响应式布局覆盖宽屏与窄屏。所有数据、角色、未读与重试结果均为预设；不是异步状态机或业务验收。已检查 ID 唯一性、全部导航目标存在、无脚本及外部请求。搜索、筛选、输入发送、草稿与滚动恢复未模拟。状态页提供切换失败、失权、归档、OA失效；危险操作只展示影响说明与取消，不执行。

## V1.3 企业组织架构

organization-chart.svg 展示手机部门树与宽屏部门关系图；可点击原型加入 org-chart、org-chart-map、sales-dept、sales-child-dept、empty-dept、org-chart-manage、department-edit、dept-archive 共八个页面，现共40个状态。员工从通讯录进入，管理员从成员与部门进入维护。

当前源码依据：HumanDirectoryDepartment 字段为 id/name/parent_id/member_count，member_count 为本级 active 成员数；HumanDirectoryMember 没有职位字段，一人可有多个 department_ids。organization_departments_page.dart 已有展开树、创建子部门、移动与归档路径。Admin OrganizationDepartmentsPage.tsx 明确其接口不提供部门成员数及 department_admin 局部角色。

因此图使用部门父子关系，不创造个人汇报线、职位、部门负责人的新字段。手机默认部门树，宽屏支持关系图；人员通过部门详情查看，不把所有人塞入全图。展开时保留当前位置、面包屑返回父级，成员列表分页；直属人数不叠加子级人数，避免多部门兼职重复计数。所有树数据必须在授权 scope 内分页加载；图不能靠首屏数据假装完整，尚未加载的分支显示加载/继续读取状态。

部门治理复用现有接口及并发版本校验；新增上级不得形成循环。成员选择复用当前企业有效成员，不要求手填ID。归档影响以当前后端校验为准，失败/冲突刷新重试，原型不承诺自动迁移或级联删除。部门成员与Workspace成员关系独立，增删部门关系不暗中授予工作区权限。
