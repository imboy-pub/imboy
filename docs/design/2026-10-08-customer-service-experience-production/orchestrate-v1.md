# Plan-Orchestrate Result / 逐卡执行提示词

**Plan**: `docs/design/2026-10-08-customer-service-experience-production/plan-v1.md`

**Plan SHA-256**: `14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc`

**Lang**: `unknown`（三仓源码数量：Erlang1874、TS/JS718、Dart2055、Python130、Java1、Kotlin2；排除deps/vendor/generated等，没有语言超过60%，reviewer按skill使用code-reviewer。）

**ECC mode**: `plugin`（已发现用户目录下 .claude/plugins/marketplaces/ecc。）

**Steps**: 11；**Scope**: all。

只生成，不执行。不把custom顺序链误认为并行授权；每条需前置卡证据通过和主会话handoff，缺用户设计确认、UAT签收或独占资源时停止对应卡。用户启动一条命令后只执行该卡，不越过下一张卡。plan hash变化必须重新生成本文件。最终review结论必须绑定候选，不用本轮计划结构验证冒充业务PASS。

## Steps overview / 总览

| # | Title | Tags | Chain |
|---|---|---|---|
| 1 | CX-00 基线、继承与复现 | plan | `ecc:planner` |
| 2 | CX-01 首接待体验与页面合同 | design | `ecc:planner,ecc:architect` |
| 3 | CX-02 队列、待响应与搜索 | impl,db | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer` |
| 4 | CX-03 中文输入、草稿与发送 | impl,security | `ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer` |
| 5 | CX-04 长会话、滚动与已读 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 6 | CX-05 接单、转接、结束与在线 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 7 | CX-06 真实接入、扫码与访客 | impl,security | `ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer` |
| 8 | CX-07 附件、客户上下文与运营 | impl,security | `ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer` |
| 9 | CX-08 故障、安全、稳定性与包装 | test,security | `ecc:tdd-guide,ecc:e2e-runner,ecc:security-reviewer,ecc:code-reviewer` |
| 10 | CX-09 独立终验与用户亲验 | test,review | `ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer` |
| 11 | CX-10 发布资格包，不执行发布 | docs,review | `ecc:doc-updater,ecc:code-reviewer` |

---

## Step 1 — CX-00 基线、继承与复现

**Intent**: 只读核对当前客服实现、三仓漂移、主Q07交接和完整旧Required，先得到真实失败旅程与独占环境。

**Tags**: plan

**Chain rationale**: 规划者整理证据、合同和依赖；本卡不实施业务代码，不启动占用中的环境。

```bash
/ecc:orchestrate custom "ecc:planner" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-1] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；先只读复采imboy/imboyadmin/imboyapp和主Q07交接，核队列未读、IME、共享草稿、50条历史及ACK风险；恢复旧Seat V2原合同并合并其他旧Required。缺handoff仅继续只读准备，禁止接管别的会话。 Acceptance: 三仓WIP/资源owner与继承exact-set完整；合成旅程分类并生成绑定环境的commands.sh；用户预先锁容量平台及重新估算，CX00-A01至A05全有证据；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 2 — CX-01 首接待体验与页面合同

**Intent**: 定义新手开通到第一次接待的唯一主线、信息层级、状态文案与响应式交互合同，并由用户先走查。

**Tags**: design

**Chain rationale**: planner负责主任务与边界，architect复用现有组件和状态机；不在设计链内隐藏实施。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-2] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；基于现有开通向导/SeatWorkspacePage与共享UI设计开通、扫码、接单、回复、转接、结束、历史的最小主线；明确待接待/我的/历史、权限空态与失败下一步，产出可逐态演示原型并约用户走查。 Acceptance: 主线和状态权威定义齐全；1440/1280/1024/768嵌入及390访客视口与200%缩放操作可达；用户确认设计，CX01-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 3 — CX-02 队列、待响应与搜索

**Intent**: 让坐席在列表发现未读和待回复、找到非首屏会话，并保持权限与分页事实一致。

**Tags**: impl,db

**Chain rationale**: 测试先复现跨页和计数问题，database-reviewer核聚合/索引/scope，code-reviewer按多语言兜底收口。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-3] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-01及主Q03/Q06 handoff通过后租用客服共享文件；复用queuePanel/contract/workbenchApi/hooks与cs_pg_session补权威未读、待回复、本人投影、scope内摘要检索及稳定cursor分页，不逐行N+1，不复制消息真源。 Acceptance: 未读与待回复分别对账且别人会话不可写；非已加载会话可检索并无分页丢重；首屏搜索p95≤1.5s且越权零泄漏，CX02-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 4 — CX-03 中文输入、草稿与发送

**Intent**: 消除中文确认误发、切会话草稿/附件串台和不安全失败重试，优先保护操作者与访客。

**Tags**: impl,security

**Chain rationale**: TDD重现输入与异步回包风险，通用审查覆盖现有TS实现，安全审查最后验证scope/payload/id绑定。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-4] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-02释放共享hook后，以sessionView/useSeatSend最小修复IME确认、A/B会话与组织草稿隔离、在途快照固定及旧回包不清新草稿；同参retry复用id/已confirm附件，编辑失败内容用新id；不做云草稿或正文localStorage。 Acceptance: IME/双击/切换零误发串台；失败和未知结果安全恢复且无重复上传消息；pending≤150ms且发送状态来自真源，CX03-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 5 — CX-04 长会话、滚动与已读

**Intent**: 补足超过50条消息的历史与阅读体验，修正当前会话可见性驱动的合法游标。

**Tags**: impl,db,security

**Chain rationale**: 先测分页锚点和跨会话ACK，数据库审查核游标单调与消息归属，安全审查收口不可误ACK边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-5] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-03通过后复用useSeatMessages/FocusAck、sessionView及现有企业分页；支持200条历史加载与滚动锚点、新消息定位；read ref随scope/session重置，仅foreground/focus实际可见消息ACK，拒跨会话ID，捕获并安全重试失败。 Acceptance: 四页历史无丢重且不跳阅读锚；后台/未可见消息不ACK、当前新消息可见能推进；跨会话/断线/失败游标和DB一致，CX04-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 6 — CX-05 接单、转接、结束与在线

**Intent**: 在并发和故障下保持唯一接待归属，并使转接、结束具备明确后果与取消能力。

**Tags**: impl,db,security

**Chain rationale**: 真实事务测试覆盖CAS/容量，数据库审查守原子性，安全审查确认旧owner撤写和scope约束。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-6] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-04通过后复用detailPanel/presence及queued-active-closed/CAS；双坐席同刻认领，away/满容量按服务器规则；用已有ConfirmDialog保护转接结束和草稿/在途状态，取消零副作用，409只解释刷新，按现有授权实现离线接管。 Acceptance: 并发恰一胜且容量/在线事实可信；转接旧owner只读新owner可回复、结束取消不写库；再联系/历史/审计一致，CX05-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 7 — CX-06 真实接入、扫码与访客

**Intent**: 把生成的接入代码、物理扫码和访客各种实际状态连成真实可用旅程。

**Tags**: impl,security

**Chain rationale**: 先写接入旅程测试，通用review覆盖Widget和必要移动适配，安全审查阻止放宽iframe/凭证边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-7] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-01设计后可准备非重叠Widget测试，真实集成等CX-05；复用开通向导/二维码/Widget控制器，把Admin生成的Widget与Seat snippet真正注入宿主页；用Android物理扫码且独立验App坐席，说明无人在线/重开/过期，不新增联系方式。 Acceptance: 开通至访客被接待及真实双snippet加载成功；物理扫码/继承移动坐席非API替代；所有访客态与origin/sandbox/三凭证隔离有效，CX06-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 8 — CX-07 附件、客户上下文与运营

**Intent**: 完善已有附件和客户事实的可信反馈，把评价与统计从占位文案升级为真实投影。

**Tags**: impl,security

**Chain rationale**: 用测试验证字节与数据事实，不新建CRM；安全审查最后核授权撤销和日志最小化。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-8] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-05/06通过后复用既有上传/content授权、客户卡片和CsStatsPanel；展示附件阶段/失败重试并清理blob；真实评价/首响/接待量/队列按权威定义及时区对账，无值显示未知，诊断不输出token或客户正文。 Acceptance: 真实文件hash一致且超限过期失败可恢复；跨scope/撤权/切会话无资产泄漏；上下文评价统计与SQL一致，CX07-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 9 — CX-08 故障、安全、稳定性与包装

**Intent**: 对冻结候选实施隔离故障、全量回归、容量24h稳定性与安装恢复，不能由聚焦测试替代。

**Tags**: test,security

**Chain rationale**: 测试指导者补现有套件缺口，E2E运行者组织真链路，安全与代码审查核失败归因及修复候选；不发布。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:security-reviewer,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-9] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-02至07完成后冻结三仓/profile/runner/fixture/构建hash；独占真实PG/Garage/nginx跑断网60s、SSE乱序超窗、重启/存储故障与越权负例；targeted再全门再锁定容量24h，clean安装升级恢复，同候选不得混旧证据。 Acceptance: 恢复≤10s且安全负例无副作用；各方向≥200样本p95≤2s/p99≤5s并24h无丢重泄漏崩溃；全门及产物恢复同hash，CX08-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 10 — CX-09 独立终验与用户亲验

**Intent**: 独立复算工程证据并让用户无指导亲自完成五项任务，禁止代理代签或自封投产。

**Tags**: test,review

**Chain rationale**: 本链只验收不修业务；测试指导者审覆盖，E2E做新fixture回放，独立代码审查者复算并报告真实状态。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-10] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-08同候选合格后独立新fixture回放访客到双坐席转接结束附件历史重连，复算Required exact-set/退出码/执行数/零skip/hash；约用户无指导完成接单、中文附件、双会话草稿、转接历史、断网恢复五任务，代理不得代签。 Acceptance: 独立UI/API/SQL/字节oracle和完整继承集合一致；用户五任务100%且P0/P1体验问题0明确签收；分层状态不把PARTIAL写投产，CX09-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

---

## Step 11 — CX-10 发布资格包，不执行发布

**Intent**: 准备交付与发布审批包，分别列出本地/UAT/外部资格和生产未执行状态。

**Tags**: docs,review

**Chain rationale**: 文档owner组织真实运维合同，独立review核证据与权限；本链既不部署也不配置联系人。

```bash
/ecc:orchestrate custom "ecc:doc-updater,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-11] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-09用户签收后整理scope/平台/profile/容量、安装升级回退、监控故障手册、实际隔离备份RTO/RPO；准备限定目标组件/时窗/secret/TLS/CSP与上线复验审批包；缺外部资格记阻断，不自动配置通知联系人或执行发布。 Acceptance: 白名单交付包和运行恢复合同可复验；承诺平台/外部Required有资格证据；签收/资格一致而生产NOT_EXECUTED，CX10-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```

## Batch execution / 按顺序复制

这是命令汇总，不是本轮执行授权；前一张失败、缺handoff、等待用户确认即停，不一口气绕过人工门。任何浏览器自动化按工作区首选autoglm-browser-agent规则在当前会话操作，不交给spawned child；不可用才用现有Playwright fallback，验收仍须真实链路。

```bash
/ecc:orchestrate custom "ecc:planner" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-1] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；先只读复采imboy/imboyadmin/imboyapp和主Q07交接，核队列未读、IME、共享草稿、50条历史及ACK风险；恢复旧Seat V2原合同并合并其他旧Required。缺handoff仅继续只读准备，禁止接管别的会话。 Acceptance: 三仓WIP/资源owner与继承exact-set完整；合成旅程分类并生成绑定环境的commands.sh；用户预先锁容量平台及重新估算，CX00-A01至A05全有证据；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-2] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；基于现有开通向导/SeatWorkspacePage与共享UI设计开通、扫码、接单、回复、转接、结束、历史的最小主线；明确待接待/我的/历史、权限空态与失败下一步，产出可逐态演示原型并约用户走查。 Acceptance: 主线和状态权威定义齐全；1440/1280/1024/768嵌入及390访客视口与200%缩放操作可达；用户确认设计，CX01-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-3] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-01及主Q03/Q06 handoff通过后租用客服共享文件；复用queuePanel/contract/workbenchApi/hooks与cs_pg_session补权威未读、待回复、本人投影、scope内摘要检索及稳定cursor分页，不逐行N+1，不复制消息真源。 Acceptance: 未读与待回复分别对账且别人会话不可写；非已加载会话可检索并无分页丢重；首屏搜索p95≤1.5s且越权零泄漏，CX02-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-4] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-02释放共享hook后，以sessionView/useSeatSend最小修复IME确认、A/B会话与组织草稿隔离、在途快照固定及旧回包不清新草稿；同参retry复用id/已confirm附件，编辑失败内容用新id；不做云草稿或正文localStorage。 Acceptance: IME/双击/切换零误发串台；失败和未知结果安全恢复且无重复上传消息；pending≤150ms且发送状态来自真源，CX03-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-5] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-03通过后复用useSeatMessages/FocusAck、sessionView及现有企业分页；支持200条历史加载与滚动锚点、新消息定位；read ref随scope/session重置，仅foreground/focus实际可见消息ACK，拒跨会话ID，捕获并安全重试失败。 Acceptance: 四页历史无丢重且不跳阅读锚；后台/未可见消息不ACK、当前新消息可见能推进；跨会话/断线/失败游标和DB一致，CX04-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-6] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-04通过后复用detailPanel/presence及queued-active-closed/CAS；双坐席同刻认领，away/满容量按服务器规则；用已有ConfirmDialog保护转接结束和草稿/在途状态，取消零副作用，409只解释刷新，按现有授权实现离线接管。 Acceptance: 并发恰一胜且容量/在线事实可信；转接旧owner只读新owner可回复、结束取消不写库；再联系/历史/审计一致，CX05-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-7] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-01设计后可准备非重叠Widget测试，真实集成等CX-05；复用开通向导/二维码/Widget控制器，把Admin生成的Widget与Seat snippet真正注入宿主页；用Android物理扫码且独立验App坐席，说明无人在线/重开/过期，不新增联系方式。 Acceptance: 开通至访客被接待及真实双snippet加载成功；物理扫码/继承移动坐席非API替代；所有访客态与origin/sandbox/三凭证隔离有效，CX06-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:security-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-8] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-05/06通过后复用既有上传/content授权、客户卡片和CsStatsPanel；展示附件阶段/失败重试并清理blob；真实评价/首响/接待量/队列按权威定义及时区对账，无值显示未知，诊断不输出token或客户正文。 Acceptance: 真实文件hash一致且超限过期失败可恢复；跨scope/撤权/切会话无资产泄漏；上下文评价统计与SQL一致，CX07-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:security-reviewer,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-9] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-02至07完成后冻结三仓/profile/runner/fixture/构建hash；独占真实PG/Garage/nginx跑断网60s、SSE乱序超窗、重启/存储故障与越权负例；targeted再全门再锁定容量24h，clean安装升级恢复，同候选不得混旧证据。 Acceptance: 恢复≤10s且安全负例无副作用；各方向≥200样本p95≤2s/p99≤5s并24h无丢重泄漏崩溃；全门及产物恢复同hash，CX08-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-10] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-08同候选合格后独立新fixture回放访客到双坐席转接结束附件历史重连，复算Required exact-set/退出码/执行数/零skip/hash；约用户无指导完成接单、中文附件、双会话草稿、转接历史、断网恢复五任务，代理不得代签。 Acceptance: 独立UI/API/SQL/字节oracle和完整继承集合一致；用户五任务100%且P0/P1体验问题0明确签收；分层状态不把PARTIAL写投产，CX09-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
/ecc:orchestrate custom "ecc:doc-updater,ecc:code-reviewer" "[Plan: docs/design/2026-10-08-customer-service-experience-production/plan-v1.md#step-11] plan_sha256=14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc；CX-09用户签收后整理scope/平台/profile/容量、安装升级回退、监控故障手册、实际隔离备份RTO/RPO；准备限定目标组件/时窗/secret/TLS/CSP与上线复验审批包；缺外部资格记阻断，不自动配置通知联系人或执行发布。 Acceptance: 白名单交付包和运行恢复合同可复验；承诺平台/外部Required有资格证据；签收/资格一致而生产NOT_EXECUTED，CX10-A01至A05闭合；Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。"
```
