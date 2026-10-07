# 广州企业 IM：UI / UX 定稿建议 V2

日期：2026-10-01。状态：`DESIGN_DECISION_READY`，业务实施已启动，整体尚未验收完成。此文取代 V1/V1.1/V1.3 中相冲突的导航、范围及入口建议。最新目标已包含客服可投产、企业加入退出与资料归属、OA / 全部 Internal API；本设计作为实施输入。E2EE 本期关闭入口，实施时核验新消息的协议选择和存量密文兼容，不以隐藏界面代替链路验证。

## 1. 产品决定

员工打开企业后直接处理消息，不经过企业详情、工具目录或工作区概览。老板使用同一套界面，管理入口收进「我」。

| 模式 | 一级导航 | 页面顶部 | 默认落点 |
|---|---|---|---|
| 个人 | 消息、通讯录、我 | 个人 ▾ | 原有账号会话；频道合并进消息分类，目标版按资源归属区分个人与企业群 |
| 企业 | 消息、通讯录、工作台、我 | 企业名称 ▾；必要时显示工作区 | 最近仍有权限的工作区消息；首次使用有效默认工作区 |
| 企业，未配置 OA / 平台不支持 | 消息、通讯录、我 | 同上 | 消息；不展示空工作台，公告仍在消息页内 |

个人频道合并进个人消息页「频道」分类，保留订阅与付费能力。企业消息页用「群聊、公告」两个分类，默认群聊；企业内部其他频道也进入公告分类，用「频道」标识区分，不能冒充正式公告。企业不展示朋友圈、附近的人、助手广场、付费频道发现和项目入口。

采用现有 IMBoy 蓝、列表和清晰的文字按钮，不增加大仪表盘、办公宫格、业务数字卡片或另一套设计系统。OA H5 直接承载客户已有办公系统。

## 2. 个人、企业、工作区怎么切换

全 App 只有一个「切换空间」入口：点左上名称。手机使用底部面板；宽屏使用侧栏顶部菜单。

```text
切换空间
  ○ 个人
  ✓ 示例企业 A
      ✓ 总部
      ○ 销售协作
  ○ 示例企业 B
  ──────────
  扫码 / 输入邀请码加入企业
```

一个企业只有一个有效且已加入工作区时，点击企业直接进入，不再让用户选工作区；名称仍可在资源详情核对。多个时才展示子项，日常页顶栏显示「示例企业 A · 总部」。部门始终属于组织架构，不能把工作区改称部门。

个人模式是独立展示偏好，不能通过清空 `CurrentOrganizationStore` 实现：现有工作区选择器的空指针分支会展示跨企业已加入工作区，不是私人空间。保留最近企业偏好，个人 / 企业来回切换时不丢位置。

切换先验证 membership 和目标资源，准备好后同时更新企业名、工作区、列表和导航；失败保持原上下文并提示重试。默认工作区查询失败时，不能保留另一企业的选中工作区；只能选择目标企业内仍获授权的工作区，或呈现「尚无可用工作区」。

企业通讯录和 OA 跟随企业；群与频道跟随工作区；账号资料、私聊和安全设置跟随账号。切工作区不重启企业级 OA；切企业、切账号需重新核对 OA 身份，尤其两家公司共用同一 OA origin 时，不能靠 Cookie 同源假定身份正确。

## 3. 消息页

```text
示例企业 A · 总部 ▾             搜索  ＋
个人私聊                                  ›
群聊                         公告
─────────────────────────────────────
全员群                 周四安排       09:20
销售协作群             新客户资料     08:45
─────────────────────────────────────
消息          通讯录          工作台       我
```

「个人私聊」是明确的跨空间快捷入口，点击进入账号的个人会话；不能把现有 C2C 根据同事关系变成企业或工作区消息。其他工作区提醒从切换面板呈现，计数必须有可靠来源；一期缺少计数时仅显示名称，不填演示数。

群聊复用既有聊天、群详情、附件、语音与视频动作。公告复用频道内容能力，保留真实发布来源。企业应用消息显示「来自企业应用」；其托管消息与 Human 私聊 / 群聊保持原有身份和审计边界。E2EE 本期不在日常 UI 出现，仍需保留存量消息的正确读取路径。

企业群列表与私聊列表需要不同的数据范围。当前 `ConversationModel` 没有可直接信任的 `organization_id/workspace_id` 字段，Workspace 壳还直接复用全局 `ConversationPage`；所以目标布局需要把「工作区已加入群」与其会话关联起来，不能只过滤标题或换导航文案。个人页排除企业群也需要权威资源归属，不能猜测。

列表每行只显示名称、摘要、时间与真实未读；顶栏一个主要动作「＋」。无权创建的员工，菜单仅提供其有权执行的动作；不展示点进去必定失败的创建工作区 / 管理按钮。搜索按当前页真实范围执行，不把尚未具备的全企业消息搜索画成可用功能。

## 4. 通讯录与组织架构

手机默认就是部门树与同事列表，不再要求「企业 → 企业详情 → 成员 → 部门」。

```text
示例企业 A ▾                 搜索同事 / 部门
我的部门
组织架构
  ▾ 销售部                         直属 2 人
      ▸ 广州销售组
      ▸ 客户协作组
  ▸ 行政部                         直属 1 人
未分配部门的同事
```

点部门后展示面包屑、直接子部门和直属成员。成员资料保留「发消息（个人会话）」主动作，复用当前联系人 / C2C 权限判断；不新增企业私聊模型，不绕过现有好友或聊天限制。当前目录已经能进入成员资料，发消息还绕到通用 IM 资料页，这是应缩短的路径。

宽屏采用「左部门树 + 右成员列表 / 资料」；「查看组织图」作为次级入口，画部门父子关系，点击节点进入同一部门详情。组织图不默认一次加载所有人员，不创造职位、汇报线、部门负责人字段。

目录现有数据只有部门 id/name/parent_id/member_count 和成员 user_id/display_name/avatar/department_ids。人数为直属有效成员数，不能加总子部门数当企业人数；支持多部门归属。搜索支持部门名、昵称、账号，当前不支持职位、手机、邮箱。输入一个字时提示继续输入，按已有 2–64 字符合同执行。

普通员工只读。组织管理员在此看到「管理」次级按钮，进入同一套部门维护 / 成员治理；工作区 Owner 不自动获得企业目录治理权。停用、离岗、企业归档、空部门和读取失败分别显示原因。

平台 Admin 的部门接口缺少直属人数和部门成员挂载能力，不能把 App Human 接口的数据直接塞给 Admin Cookie 界面。宽屏图可复用部门结构；人数与成员维护必须按该认证面真实接口覆盖呈现。

## 5. 工作台、我、管理企业

工作台一期只有 OA H5，直接进入当前企业授权应用；用原生标题和错误恢复承载，不新建审批、考勤、报销业务。Android / iOS 有有效配置才显示，macOS / Web 本期隐藏。未配置不是系统异常，也不放一个空宫格。

9/29 工作台计划的「返回所有企业条目后取第一项」与按企业切换存在冲突，响应示例还没有企业标识。实施前必须明确当前企业的条目合同及服务端校验；刷新列表不能保证打开当前企业 OA。保留已有一次性 code、exact HTTPS origin、SSO 与错误状态能力，补齐换配置、保活和退出会话。

「我」显示个人资料、通知、安全、切换空间；有权限才出现「管理企业」或「管理工作区」。客服人员另见授权的客服工作台入口，不能把坐席身份作为普通企业成员角色。

管理企业只保留四类任务：

| 任务 | 常见操作 | 收起的内容 |
|---|---|---|
| 成员与部门 | 邀请同事、查看 / 调整成员、维护部门 | 角色代码、TSID、离岗历史进入详情 |
| 群与公告 | 找群、创建 / 改名、维护成员 / 订阅者 | 解散 / 归档进入对象的更多菜单 |
| 工作区 | 查看、切换、创建与治理授权资源 | 一个工作区不在员工日常导航单独占位 |
| 企业设置 | 企业资料、允许的设置与生命周期动作 | 移交、归档等影响大的动作放详情末尾 |

老板登录后的首页仍是消息。企业管理权限、工作区治理权限、平台权限和 Seat JWT 四种身份保持各自边界；部门管理员也不能被扩成公司管理员。

## 6. imboyadmin 与客服

`imboyadmin` 是平台运营后台；老板不因拥有企业 Owner 角色就能用平台管理员 Cookie 登录。现有九条企业治理路由继续保留，在选定企业的聚焦视图归为五类任务：成员与部门、群与频道、工作区、应用与集成、设置与审计。没有强制「企业概览」或新统计首页。

页头保留选定企业，列表只需必要的工作区 / 状态筛选；名称可点进关联对象，默认不让运营人员抄 ID。原九条路由、权限、查询条件和 backend sidebar 仍需一致；菜单分组不是租户授权。离岗交接从成员详情进入，项目在广州聚焦视图隐藏，平台全局治理仍按权限可达。

客服日常工作用独立 Seat 工作台，宽屏三栏：会话 / 排队列表、当前对话、客户信息。当前源码已有 `src/seat/seatMain.tsx` 与后端 `/seat/:public_seat_console_id`；应复用这套入口，不再按旧计划把客服操作混进平台后台日常导航。平台后台保留坐席、安装与控制台治理。

客服的排队、接单、转接、结束、忙碌 / 离开、附件和客户上下文沿用现有组件；不在这次员工 IM 改版中重建客服系统。应用、客服身份、普通同事、系统通知不能因为布局简化被合并成一种发送身份。

## 7. 企业资料归属

不加第五个 Tab：消息页次级入口「工作区资料」，群详情保留「群文件」。资料页标题始终显示当前企业和工作区，支持搜索、打开、上传及已有能力允许的治理动作。OA 文档从 OA H5 进入；本期不新造协同编辑器或审批系统。

一期以现有附件 / 企业资产模型为基础，让新企业资料有明确的 organization、workspace 及可选群 / 频道来源；组织与工作区是所有权及访问范围，上传人是审计事实。上传者离岗不把企业资料变成个人文件，也不自动删除；读取、下载和删除每次由服务端按范围授权，链接经现有授权服务生成。切工作区只展示该工作区资料；查看同事或部门不自动获得资料权限。

现有模型是否覆盖普通 Human 上传、企业托管上传、群与频道附件，需要逐条确认。缺少归属的历史资料不能根据文件名或上传者当前所在企业猜测回填；单独列为兼容 / 迁移事项，生产回填需在数据影响方案可审阅后执行。

## 8. 保留、合并、隐藏、删除

| 现有内容 | 决定 | 原因 / 限制 |
|---|---|---|
| 私聊、群聊、附件、通话、频道 | 保留并复用 | 核心沟通能力，不重写 |
| 企业选择器 / 工作区选择器 | 合并成一个可理解的切换面板 | 底层 membership 和两阶段切换复用 |
| 企业详情的日常目录首页 | 退出员工主路径，必要详情仍保留 | 不必先认识内部实体才能聊天 |
| 「群」「频道」都进入 manage 的重复入口 | 日常阅读直达消息分类；治理集中 | 区分看内容和管资源 |
| 「工具」「企业工具」两个入口 | 广州日常界面隐藏 | 项目聚合不充当 OA 工作台 |
| Workspace 概览、群、频道等第二套底部导航 | 广州统一四 / 三项；通用产品保留受支持路径 | 避免切换后重新学习导航 |
| 项目 | 广州入口与直达路由关闭 | 保留模型、接口、历史与其他产品配置 |
| 朋友圈、附近的人、助手广场、公开 / 付费发现 | 广州企业模式隐藏 | 个人能力按原权限保留 |
| 创建工作区常驻按钮 | 根据真实治理资格显示 | 不让每位员工都看到创建入口 |
| Org / WS / Application / Grant / TSID | 日常 UI 收起 | 技术事实与权限不删除 |
| 离岗、移交、归档、解散 | 详情中保留 | 不能为简洁而丢掉治理与后果说明 |
| 废弃业务代码 | 本轮不判定删除 | 先完成新路由引用、角色与历史兼容验证，再删真正无调用内容 |

## 9. 三端实施顺序

先改上下文和数据范围，再改入口，最后裁剪旧展示；不先换皮再补权限。

| 顺序 | imboy | imboyapp | imboyadmin | 完成条件 |
|---|---|---|---|---|
| 1：上下文正确 | 复用现有组织 / 工作区授权、目录；补齐会话资源归属与当前企业 OA 条目必要合同 | 统一个人 / 企业选择、目标工作区落位、失败恢复 | 选定企业与关联列表过滤一致 | 切到 B 后没有 A 的工作区、目录、OA 或未读；失权可恢复 |
| 2：员工路径 | 无新增 OA 办公业务 | 三 / 四项导航；群聊与公告；通讯录树与成员主动作；OA 容器 | 保留平台权限，归类现有企业页面 | 加入企业后直达消息；两次点击找人；管理不挤入员工主路径 |
| 3：治理与收尾 | 保留幂等、版本校验、加密、归档 / 解散差异 | 角色裁剪、危险动作与旧深链兼容 | 企业聚焦导航；独立 Seat 路径复用 | 各角色、直达链接、大小屏与真实设备通过验证 |

本轮只交付设计文件。后续实施范围应是导航、上下文、必要投影和页面复用；不新造 Organization、消息系统、统一认证或另一套后台。

## 10. 验收要点

1. 个人 → 企业 A 总部 → A 销售协作 → 企业 B → 个人，标题、目录、资源、权限一致；失败不半切换。
2. 一个 / 多个 / 无工作区，默认工作区不可读、归档、成员暂停与账号更换均有正确落点。
3. 企业群与个人 C2C 不混称；分域未读没有来源就不展示；草稿和附件发送目标不随切换误改。
4. 普通员工不能从隐藏按钮或直达链接取得写权限；工作区 Owner 不变成企业管理员。
5. 组织树分页、成员分页、空部门、多部门归属、直属人数与搜索限制正确；关系图显示加载不完整状态。
6. 同事资料发消息沿用现有 C2C 授权，清楚显示个人会话；企业应用与客服消息保持真实来源。
7. OA 无配置 / 配置改变 / 超时 / 401 / 退出 / 跨企业同 origin 正确；Android、iOS 为广州产品要求，桌面不放未支持入口。
8. 老板常见邀请、成员与群治理无需抄写 ID；高影响动作显示具体对象、影响、确认与错误恢复。
9. 宽屏和手机使用相同信息架构；暗色、大字号、焦点与点击目标沿用 App token / Admin 组件规范。
10. 真实接口与设备旅程验证后才能声明实施完成；HTML 预览只验证布局和导航，不能替代业务验证。

## 11. 阅读与源码依据

计划目录目前是 **26 份 Markdown，共 12,266 行，另有 9 个 SHA256 侧文件**。本轮覆盖全部正文：25 份全文读取；统一计划 V2 的 826 行与已读 V2.1 逐行相同，复用该阅读覆盖，其余 125 行差异全文读取。9 个侧文件全部与对应正文匹配。

源码核对覆盖企业需求相关的路由、组织 / 工作区与目录授权、App 导航 / 切换 / 成员 / OA 链路、Admin 菜单 / 部门 / Seat 入口；不是声称三仓所有代码均已逐行阅读或所有计划已实现。当前 Internal 注册表已是 **32 端点、26 path**，超出 V2.1 计划的 31 端点；实施清单以当前源码为准。未启动业务服务、未跑全套测试、未做真实用户与真机验收。

主要证据：

| 位置 | 核实结果 | 本设计影响 |
|---|---|---|
| `imboy/src/imboy_router.erl:695,803,807,811,815` | mine 与四个 Human Directory 路由存在 | 企业选择与找人复用 |
| `imboy/src/lib/organization/application/organization_directory_app.erl:155` | 有效组织 / 成员授权先于目录读取 | 目录不能靠本地指针取得权限 |
| `imboy/src/repo/workspace_repo.erl:144` | mine 是账号全部有效成员工作区 | 每次按目标企业收敛，空指针不是个人域 |
| `imboyapp/lib/modules/organization/presentation/organization_picker_page.dart:150,260` | 预载 / 提交存在；默认工作区 best-effort | 保留机制，补完整目标企业落位 |
| `imboyapp/lib/page/workspace/workspace_picker_page.dart:57,93` | 按当前企业过滤；创建按钮常驻 | 合并面板、角色裁剪 |
| `imboyapp/lib/page/workspace_shell/workspace_shell_page.dart:118` | 会话直接复用全局页面 | 不能冒充工作区会话隔离 |
| `imboyapp/lib/store/model/conversation_model.dart:13` | 无显式组织 / 工作区归属字段 | 目标版分域需要权威资源关联 |
| `imboyapp/lib/modules/organization/presentation/organization_detail_page.dart:296` | 多实体入口；群 / 频道同进管理 | 移出日常必经路径 |
| `imboyapp/lib/modules/organization/presentation/organization_member_detail_page.dart:118` | 消息动作仍在通用 IM 资料页 | 缩短同事发消息路径 |
| `imboyapp/lib/modules/enterprise/presentation/enterprise_home_page.dart:76` | 首个组织 + 业务身份 / 托管会话 | 不用它替代员工消息页 |
| `imboyapp/lib/modules/enterprise_oa/enterprise_oa_runtime_config.dart:90` | 当前 OA 来自构建配置 | 标准包动态发现仍需实施 |
| `imboyadmin/src/components/layout/sidebarSchema.ts:161` 与后端 sidebar | 企业九个治理叶子 | 分类呈现，保留路由与授权一致 |
| `imboyadmin/src/modules/organization/pages/OrganizationDepartmentsPage.tsx:68` | 平台目录缺人数 / 成员挂载 | 不借用 Human 认证面伪造功能 |
| `imboyadmin/src/seat/seatMain.tsx:18`；`imboy/src/imboy_router.erl:2286` | 独立 Seat 入口与帧路由已存在 | 复用当前实现，超越旧计划现状 |

### 计划版本如何取舍

| 文档组 | 采用的约束 | 不应误用的历史内容 |
|---|---|---|
| Pilot / 广州 / Enterprise Admin / Internal Full | 邀请制、Org 1:N WS、默认工作区、角色分离、H5 办公、项目配置隐藏 | 早期 Internal API 数量、旧菜单数量、客户白标交付不等于当前完成 |
| Organization Unified V1 / V2 / V2.1 | 以 V2.1 修正身份与授权边界、31 Internal / 14 scopes、Human Directory | V1 45 API、应用自建 / 自授工作区、Seat 绑定工作区等不沿用 |
| Widget / Hosted / 企业 UX 9/24 | 员工与治理分离；复用附件、分页、队列、来源与四认证域 | 旧 Hosted prompt 的预期 SHA 和自动部署条件已与修订主计划冲突，不直接执行 |
| Seat Console Embed 9/28 | 客服操作进入独立 Seat 面，平台负责治理 | 旧「坐席只挂 Admin SPA」基础设施选择被替代；源码已有新入口 |
| Workbench 9/29 | OA H5、受支持平台、有效配置、SSO 生命周期 | all-org 条目取第一项不能满足当前企业上下文，需修订合同 |
| Plaintext Evaluation / LiveKit | 保留客服存储与 Human 加密边界；通话 UX 复用 | 不能根据布局删除加密、推断生产媒体链已通过 |
| Closure 9/24、9/26、9/27 V1/V1.1/V1.2、9/28 V1.3 及 prompts | 作为依赖、恢复与验证要求；版本与运行证据分开 | 历史 main/candidate PASS、侧文件匹配均不证明当前产品可交付 |

以下文件台账由当前目录内容生成，SHA 只绑定文档内容，不表示已实施或验收。

| 文档 | 行数 | 阅读覆盖 | SHA256 | 侧文件 |
|---|---:|---|---|---|
| [2026-09-20-customer-service-hosted-widget-deploy-plan-v1.md](../../plans/2026-09-20-customer-service-hosted-widget-deploy-plan-v1.md) | 596 | 全文 | `9990c8590d3c37277b1d1d94b2295385feb1d6d3d65fa6514f2b47c44da6bbe1` | 无侧文件 |
| [2026-09-20-customer-service-hosted-widget-deploy-prompt-v1.md](../../plans/2026-09-20-customer-service-hosted-widget-deploy-prompt-v1.md) | 162 | 全文 | `313c9480bc0066a2f73a939f679d9614d5e74002ca680151239b107bc48a6f16` | 无侧文件 |
| [2026-09-20-customer-service-widget-seat-web-zcode-plan-v1.md](../../plans/2026-09-20-customer-service-widget-seat-web-zcode-plan-v1.md) | 938 | 全文 | `f29ae7b23666c6e084dcdf77507829d652fcc3f725daeb7b902503c13e8d71f7` | 无侧文件 |
| [2026-09-20-customer-service-widget-seat-web-zcode-prompt-v1.md](../../plans/2026-09-20-customer-service-widget-seat-web-zcode-prompt-v1.md) | 183 | 全文 | `56081b2350abbf357c754556ebc8e6adbc7801c8814edf30e7e5fbff3a79d385` | 无侧文件 |
| [2026-09-20-enterprise-pilot-v1-implementation-plan.md](../../plans/2026-09-20-enterprise-pilot-v1-implementation-plan.md) | 360 | 全文 | `d98e01bbb684f19e27bc33dd57e3802632c753e07868d36abc9ef144939344c2` | 无侧文件 |
| [2026-09-21-enterprise-admin-prod-customer-service-v1-implementation-plan.md](../../plans/2026-09-21-enterprise-admin-prod-customer-service-v1-implementation-plan.md) | 393 | 全文 | `0f4cf1948192182fc6adc4ddb90861635ad18108dbbdc1ae28ef98600696eb37` | 无侧文件 |
| [2026-09-21-enterprise-admin-prod-customer-service-v1-zcode-prompt.md](../../plans/2026-09-21-enterprise-admin-prod-customer-service-v1-zcode-prompt.md) | 169 | 全文 | `350366f806c0754e76ed90b4dad039bb889c6ce2dc8ab153283d7f8476dcad1a` | 无侧文件 |
| [2026-09-21-enterprise-internal-platform-full-v1-implementation-plan.md](../../plans/2026-09-21-enterprise-internal-platform-full-v1-implementation-plan.md) | 151 | 全文 | `6d3cd8fd780abb6e6f9444c8604f9c61f1b0a4425e3c52a6164a1e6e8c3f6781` | 无侧文件 |
| [2026-09-21-enterprise-pilot-v1-zcode-prompt.md](../../plans/2026-09-21-enterprise-pilot-v1-zcode-prompt.md) | 216 | 全文 | `5983fee11d4f98d2259fde057001d12a5cd887270e8e5b729aebd2afca2a1e30` | 无侧文件 |
| [2026-09-21-guangzhou-enterprise-app-v1-implementation-plan.md](../../plans/2026-09-21-guangzhou-enterprise-app-v1-implementation-plan.md) | 333 | 全文 | `ce56ce6c674faf3d6700c02cea1ab8fc3b7157aef7640d81cee3d0967fac28c0` | 无侧文件 |
| [2026-09-21-livekit-single-service-migration-deployment-plan-v1.md](../../plans/2026-09-21-livekit-single-service-migration-deployment-plan-v1.md) | 471 | 全文 | `28aa35a4c8439266f2ee6f84764b373af586aecb243c5945d7cf20e55a2d337e` | 无侧文件 |
| [2026-09-21-livekit-single-service-zcode-orchestrate-v1.md](../../plans/2026-09-21-livekit-single-service-zcode-orchestrate-v1.md) | 287 | 全文 | `542437597910192965099608ee3f56b7cc569e80e320946760c44300e30a2355` | 无侧文件 |
| [2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md](../../plans/2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.1.md) | 1095 | 全文 | `a866aa137eea856a9a6d9324d3cb46d3fb1f1fb8db73891808c85d1f17d23302` | MATCH |
| [2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.md](../../plans/2026-09-23-enterprise-organization-admin-internal-v1-unified-plan-v2.md) | 951 | 826 等同行复用 + 125 差异全文 | `5e65971a6a83834d6981992a03d652775407a2e756aab036831cacf5abe18264` | 无侧文件 |
| [2026-09-23-enterprise-organization-admin-internal-v1-unified-plan.md](../../plans/2026-09-23-enterprise-organization-admin-internal-v1-unified-plan.md) | 704 | 全文 | `625f3931c2e65d5082da4b6db0dcb50533d179bde762506b4b2f15e39936c74b` | 无侧文件 |
| [2026-09-24-customer-service-agent-workspace-enterprise-ux-efficient-zcode-plan-v1.1.md](../../plans/2026-09-24-customer-service-agent-workspace-enterprise-ux-efficient-zcode-plan-v1.1.md) | 1201 | 全文 | `cd952bd45a9dcb4a4939d1aecdc632ee041a0584fdb351b46601c1bb5e1f4967` | MATCH |
| [2026-09-24-enterprise-organization-admin-internal-v21-current-main-closure-plan-v1.md](../../plans/2026-09-24-enterprise-organization-admin-internal-v21-current-main-closure-plan-v1.md) | 363 | 全文 | `f482c4888c415e53b5ec4eb8be4ad0807f99628e01235fe029d17187bcf03a24` | MATCH |
| [2026-09-26-enterprise-organization-admin-internal-v21-final-closure-plan-v2.1.md](../../plans/2026-09-26-enterprise-organization-admin-internal-v21-final-closure-plan-v2.1.md) | 474 | 全文 | `9ac84b1a2464e4dc3ae9b35c57e8964fc15acbf1768bdb5fa99753f1fc59ef98` | MATCH |
| [2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.1.md](../../plans/2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.1.md) | 493 | 全文 | `e24526d8eb25467e30564fd00220238ee54da8f85e68b1008b0665a1a3c2b725` | MATCH |
| [2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2-unattended-zcode-prompt-v1.md](../../plans/2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2-unattended-zcode-prompt-v1.md) | 276 | 全文 | `504eacdbb9290914e798146d5069db4103210500870caaaa39ee208783d74ef9` | MATCH |
| [2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2.md](../../plans/2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2.md) | 612 | 全文 | `e26e0b29fe6accf3e6a6bf4b7b30d5c53ce5ff07ded3144d6cdaab776c47d918` | MATCH |
| [2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.md](../../plans/2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.md) | 341 | 全文 | `60b88658c1d9377f4445a4a458128b27d1735939c91fc67b50e87387b377bb4d` | MATCH |
| [2026-09-28-cross-plan-production-readiness-closure-v1.3.md](../../plans/2026-09-28-cross-plan-production-readiness-closure-v1.3.md) | 716 | 全文 | `bf325eeb5709dc884260d72e3c7a6b39aa74cfe313c22534c110d8e0713ff4de` | MATCH |
| [2026-09-28-cs-message-plaintext-storage-evaluation-v1.md](../../plans/2026-09-28-cs-message-plaintext-storage-evaluation-v1.md) | 111 | 全文 | `ac938a53f1805338c9041bfa006d8c8cbfb1eae34a85ce0105bda098a24c670c` | 无侧文件 |
| [2026-09-28-customer-service-seat-console-embed-mall-admin-plan-v1.md](../../plans/2026-09-28-customer-service-seat-console-embed-mall-admin-plan-v1.md) | 198 | 全文 | `efecd3e8b5d878ff5d3fe21677ddd4dd2304e3b4664c306c6d8d0521e396b744` | 无侧文件 |
| [2026-09-29-workbench-tab-oa-h5-v1-implementation-plan.md](../../plans/2026-09-29-workbench-tab-oa-h5-v1-implementation-plan.md) | 472 | 全文 | `77a747a655a3c7c2792e09e529934108c4a15cd8fab833879677e3c334563fe0` | 无侧文件 |

### 当前源码基线

此轮阅读的是含现有 WIP 的工作区；不把工作区事实冒充纯净提交结果。

- imboy：`07e685dbc9708afc702abc979fd394ef3407dd94`；工作区变更 8 项（包含本轮文档或既有工作）。
- imboyapp：`54576b776f4691c88f4c42726c1b9aa20d9ede26`；工作区变更 8 项（包含本轮文档或既有工作）。
- imboyadmin：`5598b7ef45b06670f185bf17100fb1155d8e0376`；工作区变更 1 项（包含本轮文档或既有工作）。


## 12. 本轮实际检查

| 检查 | 结果 | 结论范围 |
|---|---|---|
| 计划正文与 9 个侧文件 | 26 份 / 12,266 行覆盖，9 个 MATCH | 阅读与文档内容绑定 |
| Internal manifest | 12/12 PASS；32 端点 / 26 path | 路由、授权登记、机器契约一致；不代表全部业务通过 |
| Postman 请求计数 | 32，与当前注册表一致 | 数量核对，非真实 API 调用 |
| 客服状态机现有 EUnit | 15/15 PASS，exit=0 | 纯 domain、零 I/O；未验证数据库并发、流与存储 |
| V2 HTML | 20 个预设场景，ID 唯一，所有锚点存在，无脚本 / 外部资产 | 设计导航完整性 |
| 本地浏览器 | 1384px 部门页及组织图；390px 消息 / 个人页宽度无溢出，导航四 / 三项 | 实际渲染与抽样导航，不模拟异步状态或认证 |
| 执行链 | 10 步，每链最多 4 名代理，任务 200–600 字，计划 SHA 绑定一致 | 命令已生成，未自动启动 `/orchestrate` |

API 对接文档的 SSO 方向和计数已修正为独立本地提交 `36b48615`，仓内提交检查通过。业务源文件未改；三端实施、真实 OA / 存储与设备验收仍未完成。生产部署、迁移、push 均未执行。

浏览器优先技能 `autoglm-browser-agent` 在当前环境未注册，使用 Codex 内置浏览器验证本地预览。预览不读取真实用户或连接生产系统。
