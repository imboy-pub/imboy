# 客服坐席体验整改与投产资格任务计划 / Seat Experience and Production Qualification Plan

日期：2026-10-08（Asia/Shanghai）；版本：V1；状态：`PLAN_READY / NOT_EXECUTED`。

本轮仅授权：源码只读分析、计划文件及其结构验证。未实施业务代码，未运行业务验收、数据库迁移、设备测试、部署或第三方通知。本文件不是投产报告；任务链仅供用户随后启动，不在本轮调用 `/ecc:orchestrate`。

## 1. 判断与交付目标 / Decision and Objective

**目前不能说“体验合格”，更不能说“可投产”。** 用户自验未通过是尚未闭合的产品验收事实；已有实现和自动化不能替代它。本轮没有复现用户现场，所以不能把所有抱怨直接定为代码缺失，也不能把源码风险宣称为已复现生产事故。

客服坐席不是“能发送消息的三个面板”。首版必须让坐席知道：谁在等、谁归我处理、哪条还没回、消息有没有真正发出、转给谁、何时可结束、断线怎么恢复；访客必须知道：已接入还是排队、回复来自谁、发送失败怎么办、无人在线怎么办、结束后如何再联系。

本次采用**最小完整闭环**：复用企业消息真源、现有坐席/Widget、React Query、SSE 失效刷新、CAS、共享 UI 组件和真实 E2E；先修错误语义和操作链，再修信息密度与样式。不重写聊天引擎，不另外建设客服消息库。

完成标准分开报告，不用“9.9分”代替实际验收：

| 状态 | 精确定义 |
|---|---|
| `IMPLEMENTATION_READY` | 目标代码和针对性测试完成，尚不能代表真实链路合格 |
| `CS_LOCAL_TECH_PASS` | 同一冻结候选：新增 Required、继承 Required 本地项、真实 PG/Garage/浏览器/约定真机全部通过，完整回归通过 |
| `CS_UAT_ACCEPTED` | 前项 + 用户亲自完成 CX09-A03/A04 并明确签收该候选；代理不得代签 |
| `CS_RELEASE_ELIGIBLE` | 前项 + 容量/稳定性/安装恢复/运行与发布合同闭合，必需外部资格均合格；仅具备申请发布资格 |
| `CS_PRODUCTION_ACCEPTED` | 另获目标限定发布授权后，实际部署及上线复验通过；本计划阶段始终为 `NOT_EXECUTED` |

状态不能跳跃：用户未签收、必需真机缺席、Required SKIP、全量门失败，均不可写 `CS_RELEASE_ELIGIBLE`。某个可选部署平台未获资格，就明确不承诺；不能静默删除历史合同中本来 Required 的平台。

## 2. 现状与证据 / Current Evidence

### 2.1 编写时快照 / Snapshot

| 仓库 | 首次读取 HEAD | 15:57 前复读 HEAD |
|---|---|---|
| Backend `imboy` | `e6728a0b3115c92c56f37afe1df605425f57c1a5` | `6996fc9ab54591bec63b01829d3fd450f33b61ce` |
| Admin `imboyadmin` | `62c40f27ba498049dd911e9f7d4e9d1929620c66` | 同首次 |
| App `imboyapp` | `dc4784e2b719f6b9241f4e14350070aa84f193ff` | `9eef7a929ac423df74eb60ddbbffe332dcbf7bf2` |

这是正在并行变化的工作区，**不是冻结候选**。首次读取的 Backend 测试 WIP、App 附件/频道及 integration_test/preview WIP、既存 docs/design 与商业策略文件均属 foreign；本轮不接管、不归档、不提交。CX-00 重新记录当时 HEAD、dirty diff 摘要、分支/worktree、构建归属和租约；不可直接使用这些快照 SHA 作为执行候选。

关联主计划：`../2026-10-07-phase1-delivery-quality-plan.md`，读取 SHA-256：`2943a3fc394e65058dee07650d75228536a3008a76164cae5a498ee774fee4ee`。本轮不改它；它后续变化时必须重新对齐，不覆盖另一会话成果。

### 2.2 源码确认项与待证伪风险 / Findings and Hypotheses

| 编号/分类 | 当前可核实依据（工作区相对路径） | 用户影响与本计划处置 |
|---|---|---|
| F01 `GAP_IMPLEMENTATION` | `imboyadmin/src/modules/customer_service/seat/workbench/queuePanel.tsx` 的 `seat-session-unread-*` 仍是 aria-hidden 空槽；`contract.ts` 的列表摘要没有 unreadCount | 详情已有未读不等于能在队列发现漏回。CX-02 补权威队列未读/待响应；禁止逐行 N+1 查询 |
| F02 `GAP_IMPLEMENTATION` | 同文件明确“搜索已加载会话”，只在 view.sessions 过滤 | 当前文案诚实，但超一页后无法找完整历史。CX-02 加 scope 内服务端检索与分页，不假装全量 |
| F03 `GAP_IMPLEMENTATION`，风险待实跑 | `sessionView.tsx` Enter 无 isComposing/keyCode 229 守卫；访客 `src/widget/customer_service/app/ui.ts` 已有该守卫 | 中文输入法确认可能触发误发送。CX-03 最小修复坐席输入边界，不复制整套 Widget |
| F04 `GAP_IMPLEMENTATION`，风险待实跑 | `useSeatSend` 的 draft/clientMsgIdRef 在 hook 生命周期共享；页面切 selectedSessionId/组织/工作区时不重挂 hook，未见按会话隔离 | 潜在草稿/附件/失败重试串会话、旧异步完成清掉新草稿。CX-03 先补 A/B 切换与异步回包失败用例，再修真源 |
| F05 `GAP_IMPLEMENTATION` | `useSeatMessages` 仅请求 limit=50；消息视图未给历史分页入口、滚动锚点、新消息定位控制 | 长会话和读历史体验未闭合。CX-04 复用现有消息分页协议，不另建历史服务 |
| F06 `GAP_IMPLEMENTATION`，风险待实跑 | `useSeatFocusAck` ref 不随 scope/session 重置；打开即 ack 不检查 viewport，lastSeen 更新不直接驱动 ack，void ack 无本地失败反馈 | 可能误标已读、跨会话游标、当前打开会话新增消息不及时消除未读。CX-04 按可见消息和会话归属证伪并修正 |
| F07 `GAP_IMPLEMENTATION` | `detailPanel.tsx` 关闭直接 onClose(detail.version)，转接直接提交；暂无确认和草稿/在途发送保护 | 结束会话与转错人缺少安全护栏。CX-05 复用 ConfirmDialog；不加不可逆批量关闭 |
| F08 `GAP_IMPLEMENTATION` | 同面板结束后固定“等待访客评价或已关闭”，而未消费真实评价事实 | 无法区分未评价/已评价/不可评价。CX-07 接真实只读投影，无事实就显示未知，不伪造 |
| F09 `GAP_ACCEPTANCE` | 当前已有 presenceBar、客户上下文、附件授权下载、开通向导、统计、SSE 与多个真实 E2E 入口 | 不能全部重做或宣布没有。CX-01/06/07 做新手旅程与真实运行证据补全 |
| F10 `GAP_ACCEPTANCE` | 当前 dual-seat-claim.spec.ts 已有 Promise.all 同 expected_version 真并发用例 | 旧审计的“只串行 CAS”缺口已出现代码补充；本轮未跑，不能继续叫未实现，也不能自动继承 PASS |
| F11 `CONTRACT_CONFLICT` | 旧 Seat V2 文档当前正文已退出工作树，只有 .sha256；从 Git `984d40e6` 可读原合同。旧 V2 A05 错误码、A10 TSID、A08 Admin 入口等需按新主合同重核 | CX-00 保留原 ID/原文，逐条 reconciled，不为了绿灯删 ID/改口径；产品负责人批准语义改版才可 supersede |
| F12 `BLOCKED_EXTERNAL` | 本轮没有用户现场路径、失败操作录像、物理扫码/移动坐席与外部托管运行证据 | CX-00 先用合成场景复现；需要人工证据时问一条具体问题。真实客户账号/现场 PII 不入仓 |

以上为源码分析，不是完整漏洞鉴定或真实业务回放。旧运行证据仅作索引；需要重新绑定当前候选、fixture、runner 和完整日志。

## 3. 与主计划及其他会话的边界 / Coordination

本计划是客服体验专项，细化主计划 Q07/AC17–18，并给 Q11/Q12/Q13 提供客服域证据；**不取代主计划、不把客服两条验收扩大为整个一期通过**。组织 AC15–16、推送、OA、Core IM 仍归主计划 owner。

先只读准备 CX-00/01。实施前收到主计划协调者书面 handoff：候选 SHA/profile、Q03 功能入口/认证合同、Q06 企业消息与附件边界、Q07 坐席现有任务清单、保护路径、已占资源、资源释放时间和共享文件 owner。

- 缺 handoff：`BLOCKED_NO_HANDOFF`，可继续设计/生成测试契约，不动重叠代码或共享环境。
- 主计划已经修复某 Fxx：验证并引用其提交，不在本专项复制第二份修复；只补缺失体验/验收。
- `imboy_router.erl`、生成器/feature manifest、Admin `App.tsx`/鉴权、App `message_s2c.dart` 和附件底座由主计划 owner 控制。本专项提交最小接口差异，由该 owner 合入。
- `workbenchHooks.tsx`、`contract.ts`、`workbenchApi.ts`、`SeatWorkspacePage.tsx` 多卡共用，属于客服集成 owner；按 CX02→03→04→05→07 顺序租用。Backend `cs_pg_session.erl` 同样单 owner；不能靠多 worktree 解决共享写冲突。
- 浏览器/PG/Garage/编译 ebin/物理设备/全量回归使用独占 lease；不借用另一会话登录态，不杀 foreign 进程，不把旧 scratch 库当可用环境。
- 禁止改 `erlang.mk`、`ios/*`、`macos/*`、`plugin/r_upgrade`；物理资格不足则报告阻断，不用模拟器替代。

## 4. 体验合同 / Experience Contract

### 4.1 最小信息架构 / Minimal Workbench

```text
组织 / 工作区 / 本人坐席       在线状态·连接恢复·当前负载     帮助 / 退出
┌───────────────────┬────────────────────────┬────────────────────┐
│ 待接待 / 我的 / 历史 │ 访客名·来源·归属·会话状态 │ 客户最小上下文       │
│ 搜索与筛选范围提示  │ 按时间连续的消息历史      │ 有权限的资产/事实    │
│ 未读·最近消息·等待  │ 加载更早 / 新消息定位     │ 转接·结束（确认）    │
│ 行级接单/冲突解释   │ 正文·附件·发送/失败重试   │ 实际评价 / 历史归档  │
└───────────────────┴────────────────────────┴────────────────────┘
```

这是展示分组，不新增 queued/active/closed 之外的会话状态机。“我的”严格按本人 identity 投影；其他坐席会话仅按现有权限查看，不能写。已读不等于已回复；“待回复”以最后有效访客消息与坐席回复的权威事实判定，不通过 read cursor 推断。新分类是否能由现有索引高效提供，CX-00/CX-02 先验证。

桌面主验 1440×900、1280×720；1024×768 使用已有详情抽屉；嵌入验 768×600；访客移动验 390×844。200% 缩放下首要操作仍可达，允许合理分栏降级，不允许按钮或发送区被裁掉。主业务字体以项目 token 的 14–16px 档为目标，辅助信息按现有 token；不加新主题系统。

### 4.2 关键语义 / Critical Semantics

- 身份：Admin 运营凭证、Seat 凭证、Visitor 凭证不互通；首页开通→真实已生成 snippet→访客→物理扫码坐席闭环。
- 草稿：按账号/组织/工作区/会话隔离；本标签页切会话后保留各自正文和待选附件；正在发送或结果未知的操作固定 session 与 payload。修改失败消息视为新 payload，不能冒用旧幂等键。纯同参重试复用原 client_msg_id/已 confirm 附件。
- 草稿首版仅内存：刷新前提示会丢失，注销/撤权清理；不把凭证、客户正文或 File 持久化到 localStorage。跨窗口云草稿、自动保存服务不在范围。
- 发送：中文组合输入确认不发；Enter 发送、Shift+Enter 换行；按钮/快捷键一致。显示“发送中/服务器已接收/失败可重试/已读”且各状态有权威来源。没有访客收取 ACK 就不伪造“已送达”。单次请求失败不抹去正文和附件。
- 阅读：当前会话、页面可见且有焦点、消息实际进入可见区才推进本会话 read cursor；读取/收到不是回复。旧历史滚动位置不被 SSE 刷新打断；新消息计数和回到底部入口真实可用。
- 队列：服务端提供排序/总数/分页/搜索/权威未读或待回复投影；明确搜索范围与是否脱敏。等待时长按服务器锚点更新并标数据新鲜度，不以客户端墙钟伪造服务端事实。
- 转接/结束：目标姓名、所属 scope、在线/容量可见；确认前草稿/发送状态可检查；取消无副作用，409 只刷新归属并解释，不盲重试写。按现有原子转接模型工作，不新增待接转接状态机；原 owner 写权限真实消失。
- 访客：排队、无人在线留言、接待、失联、结束、重开或新建都由明确现有协议决定；访客发送新消息不能悄悄进入已结束会话或永久消失。无值时显示未知，不显示杜撰客服/评分/等待排名。
- 可访问性：按 WCAG 2.2 键盘、焦点和状态播报原则验；关键操作目标建议≥32px，触屏关键按钮≥44px，本计划为产品标准；WCAG 2.5.8 的最低24×24px及例外另核。role=status/alert 不等于必须跳焦点；错误必须同时有可见信息。

### 4.3 锁定容量与指标 / Locked Qualification Profile

下列为**建议验收目标而非实测成绩/SLA**。CX-00 锁硬件、网络、数据量、产品 profile、浏览器版本与统计脚本；如不匹配预期客户负载，实施前由用户批准调整，不能失败后降门。

建议单节点资格档：2 组织×2 工作区，20 在线坐席，100 active/200 queued，单会话200条历史，历史1000会话；混合10 msg/s，测试最长正文、10MiB或产品实际上限中较小的附件。负例另含上限+1字节。若该档无法在本机可信运行，记录环境阻断，不能以小样本代替。

| 指标 | 建议阈值与计量 |
|---|---|
| UI 首次反馈 | 接单/发送/转接点击后≤150ms显示 pending；浏览器 performance 时间，独立于后端完成 |
| 端到端回显 | foreground、RTT≤50ms/无丢包：访客提交→坐席可见与反向，各≥200样本，p95≤2s、p99≤5s；HTTP200不算终点 |
| 队列操作 | 同档：首屏/搜索/切会话 p95≤1.5s；全量历史翻页无重复遗漏；记录请求数/SQL计划 |
| 重连 | 60s网络中断恢复后≤10s队列/归属/消息与DB收敛；超窗明确 full resync，无重复业务消息 |
| 基础稳定性 | 24h soak：跨租户泄漏0、重复业务消息0、持久化成功但两端不可恢复消息0、崩溃0、未处理异步错误0 |
| 内存/连接 | 30min热身后每小时采样，最后6h内存中位数相对前6h≤20%增长；连接/查询不持续爬升，结束回收≤5min；系统噪声注明，不靠一次截图 |
| 用户主任务 | 用户不接受操作指导，完成5项主任务成功率100%；无阻断、误发、串台、不可恢复草稿丢失；用户逐项确认可用 |

故障窗口计入恢复测试，不把错误请求混入正常延迟样本掩盖 p99。正常工况与故障工况分别出报告、完整分母/失败数；平均值不能代替尾延迟。

## 5. 步骤任务卡 / Owned Steps

估算是人日/工程投入，非自动代理耗时或交期承诺。共 18–30 人日，另需≥24h观察及用户/真机/部署环境等待；与主计划已完成项扣重后由 CX-00 重估。每卡唯一 owner，执行人不自任最终验收人；一个人执行时必须另有独立复核和用户签收。

| Step | 卡 / 优先级 / Owner | 依赖 | 主要写集（须 handoff） | 人日 |
|---|---|---|---|---|
| 1 | CX-00/P0 集成协调者 | 无；只读可先做 | 本run合同/租约/fixture/command映射，非业务源码 | 1–2 |
| 2 | CX-01/P0 产品体验owner | CX-00设计基线 | workflow-contract、状态/文案/布局说明；若共享UI已有契约复用 | 1–2 |
| 3 | CX-02/P0 队列owner | CX-01、主Q03/Q06 handoff | queuePanel、列表contract/api/hooks、cs_pg_session与列表handler及相邻测试 | 2–4 |
| 4 | CX-03/P0 编辑器owner | CX-02共享文件释放 | sessionView composer、useSeatSend、attachmentUpload与相邻测试 | 2–3 |
| 5 | CX-04/P0 消息阅读owner | CX-03 | sessionView历史、useSeatMessages/FocusAck、企业消息分页/ack最小适配 | 2–3 |
| 6 | CX-05/P0 接待生命周期owner | CX-04 | detailPanel/presence、claim/transfer/close、容量/会话事务与测试 | 2–3 |
| 7 | CX-06/P0 接入与访客owner | CX-01；运行集成等CX-05 | 已有开通向导、seatQrLoginPanel、Widget ui/controller/chatMachine、embed fixtures | 2–3 |
| 8 | CX-07/P1 附件及运营owner | CX-05/06 | 现有附件/context/rating/stat投影、CsStatsPanel与运营页测试 | 1–2 |
| 9 | CX-08/P0 工程与安全owner | CX-02至07 | 现有E2E/harness/报告、实际缺陷最小修复、交付runbook；共享网关主owner | 3–4 |
| 10 | CX-09/P0 独立验收owner + 用户 | CX-08冻结候选 | 仓外证据/复算/UAT签收，不直接修代码 | 1–2 |
| 11 | CX-10/P0 发布资格owner | CX-09 | 交付清单/容量/运行手册/发布审批包，仅资格准备 | 1–2 |

<a id="step-1"></a>

### Step 1 — CX-00 基线、继承与复现 / Baseline

读取 F01–12 对应源码和全部调用方，确认是否被另一会话修好；核对现有真实测试如何启用，禁止测试0执行假绿。产出 baseline.json、classification.tsv、requirements.json、inheritance.tsv、leases.json、commands.sh。旧原文先从固定 Git blob 恢复到**仓外**，不恢复被产品仓清理的历史文件。

用户现场复现先使用自有合成账号与场景；无法复现时仅请求失败入口/角色/期望与实际结果，不索要真实客户数据或生产口令。输出 3–5 个最先影响接待的失败旅程和最小修复顺序。

验收组：CX00-A01/A02（版本、并行边界与继承完整）；CX00-A03/A04（真实复現分类与环境/runner）；CX00-A05（锁目标/容量/工时且Required不可静默降级）。

<a id="step-2"></a>

### Step 2 — CX-01 首接待体验与页面合同 / Workflow Design

用现有组件定义组织开通→启用坐席→安装Widget/Seat→扫码→接单→答复→转接/结束→找历史的完整主线。流程失败提示写清原因、下一步、责任角色；空态不把权限/网络/配置异常说成“暂无数据”。首屏优先待接待/我的未回复；身份、连接与状态常可见。

产出可点击或可逐态演示的轻量原型/现有组件页面契约，不引入新原型框架。约用户一次10–15min走查，不以通过截图替代交互合同；未签设计只做非重叠测试准备。

验收组：CX01-A01/A02（开通主线与信息架构）；CX01-A03/A04（响应式/六态/无障碍）；CX01-A05（用户确认，现场痛点映射）。

<a id="step-3"></a>

### Step 3 — CX-02 队列、待响应与搜索 / Inbox

补服务端按授权scope的列表未读/待回复聚合、本人/排队/历史筛选、搜索与稳定排序分页；复用现有键集分页与query cache，不新增全局事件总线。显示真实接待人/最近消息/时间/等待新鲜度；旧Admin/DataTable分页规范用于运营列表，工作台现有cursor列表不强改成页码。

搜索只支持本scope的脱敏访客显示名、来源、会话ID和已有合法消息摘要，不新增全消息FTS或开放PII字段。如果业务要求正文检索，另做安全/性能合同。筛选变化重置cursor，计数是服务端事实；SSЕ只失效权威查询。

验收组：CX02-A01/A02（真实未读/待回复及本人投影）；CX02-A03/A04（跨页搜索/排序/冲突）；CX02-A05（性能与跨scope负例）。

<a id="step-4"></a>

### Step 4 — CX-03 中文输入、草稿与发送 / Safe Composer

先写中文IME确认、A/B会话/组织切换、失败后编辑、双点击、未知发送结果、上传confirm成功而发送失败的测试；再最小修复共享hook，不新建第二发送服务。发送快照固定scope/payload/id，切换后完成不清新草稿；同参retry与修改后新消息严格区分。

在途操作切会话可保持原操作后台完成，但新会话不得继承busy/附件/错误/id；切组织/注销可取消未发起操作，已发起写不能伪称已取消。空白与超长输入、同一文件重选、撤权、401/403/422/429反馈完整；视觉pending不当作已送达。

验收组：CX03-A01/A02（IME/隔离/在途完成）；CX03-A03/A04（幂等/错误/附件重试）；CX03-A05（显示与服务器事实一致）。

<a id="step-5"></a>

### Step 5 — CX-04 长会话、滚动与已读 / History and Read

200条消息至少跨4个50条页，稳定顺序/去重；保持历史锚点，底部阅读时新消息可跟随，读旧消息时不跳。新消息入口、时间/日期格式与附件尺寸稳定，刷新不抹掉已有消息为全屏loading。

按可见消息而非整页最大ID推进本会话游标；切会话重置ref，离线/后台不ACK，失败有记录且安全重试，新增消息可见后可继续ACK。Backend拒绝非本会话消息ID，游标单调且不越scope；已读与待回复/首响分别计算。

验收组：CX04-A01/A02（长历史/滚动）；CX04-A03/A04（可见性ACK/新消息/跨会话负例）；CX04-A05（失败恢复与分页正确）。

<a id="step-6"></a>

### Step 6 — CX-05 接单、转接、结束与在线 / Session Lifecycle

复用queued→active→closed及CAS/seat行锁；测试双方同步竞争、满容量/away/离线时能否接待的现行权威规则。presence仅是状态展示与容量决策输入，不等于强行释放active归属；故障离线会话的合法接管沿现有主管授权/转接合同处理，不偷偷自动抢走。

接单成功给明确归属并进入会话；转接确认展示目标及后果，旧坐席立即只读，新坐席获得历史上下文；结束确认显示未发送草稿/在途消息，取消不写库。结束后访客再联系的行为与Widget一致，动作事件可审计。返回409后说明已由谁/为何变化，不盲重放。

验收组：CX05-A01/A02（并发/容量/在线）；CX05-A03/A04（转接/结束防误操作）；CX05-A05（结束再联系/历史/审计）。

<a id="step-7"></a>

### Step 7 — CX-06 真实接入、扫码与访客 / Onboarding and Visitor

Admin生成snippet真实贴入本run宿主页加载，不能用手写fixture替换；站点origin/CSP/sandbox/asset pairing实际生效。二维码SVG必须用Android物理设备实扫确认，API代confirm仅算Backend合同测试。App移动坐席已有真实入口必须按继承范围验，不用手机扫码成功替代App接待验收。

访客排队/无在线坐席/网络异常/结束/再次打开均有明确事实与重试行为；不要求电话/邮箱作为首版必填，也不新增真实联系人或通知配置。保留现有安全sandbox，不为改善下载/扫码随意开放top-navigation、popup或forms。

验收组：CX06-A01/A02（真实开通及双生成snippet/物理扫码）；CX06-A03/A04（访客全态/再次接入）；CX06-A05（origin/sandbox/凭证隔离/移动坐席资格）。

<a id="step-8"></a>

### Step 8 — CX-07 附件、客户上下文与运营 / Context and Operations

沿现有presign→PUT→confirm→消息与受权content下载；给真实阶段/错误/重试/取消提示。测试图片、普通文件、超限、票据过期、断线、删除与撤权；比对上传下载字节hash，不只验DOM链接。跨会话blob取消/回收必须真实生效。

客户卡片仅现有最小授权事实，不造CRM；订单/PII新增字段另需数据契约。真实评价、首响/接待量/关闭量/当前队列与SQL真源对账，明确统计时区、定义、空值与采样时间；监控能关联匿名化session/request而不输出token/原文。

验收组：CX07-A01/A02（附件/授权撤销）；CX07-A03/A04（上下文/真实评价与统计）；CX07-A05（可诊断且不泄漏）。

<a id="step-9"></a>

### Step 9 — CX-08 故障、安全、稳定性与包装 / Engineering Qualification

冻结profile、三仓SHA、dependency lock、runner/fixture、构建/manifest/静态产物；复用现有测试套件新增缺口用例，不新造第二大型验收框架。真实fault injection含60s断网、SSE重复/乱序/超窗、服务重启、PG不可用、Garage上传失败、token过期/吊销、429；故障仅在隔离自有run。

先targeted→全门→容量/24h观察；失败修复重新冻结，过期证据不得拼成全绿。全量门外域失败仅允许分类，不以“不属于客服”抹去release阻断。交付包必须使用同一候选编译产物，clean sandbox安装/升级/备份恢复能回到本run业务事实。

验收组：CX08-A01/A02（故障/安全）；CX08-A03/A04（容量24h/全门）；CX08-A05（产物安装恢复一致）。

<a id="step-10"></a>

### Step 10 — CX-09 独立终验与用户亲验 / Independent Acceptance

由非实施owner按后述 exact-set/日志/字节hash复算。独立创建本run访客、坐席A/B、主管、另组织scope，使用正式build；不接受mock、API代扫码、old-report或test.skip为Required PASS。

用户亲验至少五项，不给操作指导：1)找到等待最久并接待；2)中文多行+附件发送；3)切换两访客后分别继续草稿并定位未回复；4)转给B再结束且找历史；5)断网后恢复处理且判断发送结果。每项记实际成功、耗时、误操作、困惑及用户意见。P0/P1体验问题0；低严重度可登记，但用户不同意即不能签收。

验收组：CX09-A01/A02（独立复算/全真实旅程）；CX09-A03/A04（用户亲验/整改再验）；CX09-A05（状态分层、不自封投产）。

<a id="step-11"></a>

### Step 11 — CX-10 发布资格包，不执行发布 / Release Eligibility

交付scope/版本、服务时间/容量、认证与权限、安装/升级/回退/监控、故障runbook、备份恢复、责任人和支持边界明确。联系人/邮箱/外部通知接收人保持未配置，等待用户核实决定，不凭Git邮箱填入。

生产准入书单列：目标host/site、允许修改组件、时窗、备份/回退RTO/RPO、运行secret管理、TLS/Origin/CSP、监控与授权。生产验证必须再获明确授权和资源，不在本计划无声执行。有限范围试运行应覆盖真实接待及回退演练，观察至少24h；获授权后再锁具体生产窗口与实际负载，不能用本地soak替代。

验收组：CX10-A01/A02（交付包/运行手册）；CX10-A03/A04（发布合同/外部资格）；CX10-A05（用户签收与状态报告一致、生产仍未执行）。

## 6. Required 验收集合 / Required Acceptance

`acceptance-required.tsv` 是55个新增Required父项真源；每项必须展开平台/角色/scope/故障子格，父项通过意味着子格全部通过。`inherited-seat-required.tsv` 保留旧Seat V2的67个原始ID与原文，来源Git blob SHA-256与旧sidecar一致；**不是67个当前PASS**。

CX-00还须读取既存 `docs/plans/2026-09-20-customer-service-widget-seat-web-zcode-plan-v1.md`、hosted Widget计划、enterprise-admin计划及主Q00 inheritance，以原文显式Required和经批准的替代关系生成完整并集。旧V2只是已机械提取的一部分，不宣称完整旧计划已覆盖。App/iOS/macOS Required不能因为此专项重心在Web而漏掉：缺设备/资质记阻断；改变合同须用户审批并留原ID映射。

验收状态限定：`PASS / FAIL / NOT_RUN / BLOCKED_EXTERNAL / CONTRACT_CONFLICT / SUPERSEDED_APPROVED`；最后一种不是通过，必须有批准人与新目标ID。UNKNOWN/SKIP/PASS_WITH_NOTES都不能冒充Required PASS。冲突由owner提出可审查方案，用户或被明确授权的合同负责人决定；禁止以通用404掩盖认证/参数/能力错误。

结构化每行至少：id、source_plan/hash、task、platform、role、scope、candidate_shas、profile_hash、fixture_hash、runner_hash、command、exit、executed/failed/skipped、oracle、raw_evidence_path/hash、attempt、status、reason、next_unlock。口头“全绿”、只有截图、HTTP200、grep匹配、已编译不算行为oracle。

本run证据默认仓外权限0700目录，仓内只留脱敏计划/摘要。证据目录必须新建并使用唯一run_id，无生产PII；不得顺手复制旧run-config/凭证/crash dump。关闭/撤权后排队会话及资产断言需SQL/API/两端UI联合验证，不输出正文/秘密。

## 7. 现有命令与环境绑定 / Verified Runners

以下是已核对存在的入口，**本轮没有运行这些业务命令**。CX-00必须读取各runner/Makefile后生成展开且无占位的commands.sh；参数化端口/隔离库不能只改一个环境变量而忽略harness中的默认值。命令未绑定不准执行有状态测试。

```bash
# Backend；执行前绑定本run PG/Garage/config，主协调者释放 ebin/数据库
cd /Users/leeyi/project/imboy.pub/imboy
make compile
make eunit-local t=cs_queue_summary_pg_tests
make eunit-local t=cs_read_cursor_pg_tests
make eunit-local t=cs_seat_transaction_pg_tests
make eunit-local t=cs_transfer_capacity_pg_tests
make eunit-local t=cs_presence_dispatch_pg_tests
make eunit-local t=cs_seat_workbench_tests
make eunit-local t=cs_seat_console_handler_tests
make eunit-local t=cs_message_event_tx_tests
make eunit-local
make arch-check
make security-gate

# Admin；必须用现有script保证 --isolate，报告/trace写仓外
cd /Users/leeyi/project/imboy.pub/imboyadmin
bun run test src/modules/customer_service
bun run test src/widget/customer_service
bun run test
bun run lint
bun run typecheck
bun run deadcode
bun run build
bun run build:widget
bun run verify:widget
bun run verify:widget-pairing
# 以下在已租用真实nginx/Backend/PG/Garage与合成fixture上：
SC153_E2E_EXECUTE=1 bunx playwright test --config=playwright.customer-service-seat-embed.config.ts
bun run test:e2e:customer-service:real
bun run test:e2e:customer-service:p2
```

现有真实harness：`imboyadmin/tests/e2e/customer-service-seat-embed/fixtures/harness-seat-embed.sh`，默认Backend9801/nginx18443、真实dist-widget/PG；默认配置不意味着已获资源租约。它的二维码辅助API不是物理扫码证明；它的credential假账号也是合成fixture而非可对外使用的联系方式。

App已存在 `integration_test/customer_service/cs_workbench_e2e_test.dart` 和 `cs_degradation_acceptance_test.dart`；CX-00阅读所需运行/种子前置，发现真实Android设备ID/targetPlatform后把精确 `flutter test integration_test/customer_service/... -d <真实ID>` 两条完全展开到commands.sh。macOS/iOS按继承profile建立独立格；本轮不随意启动设备或构建受保护原生目录。

PG/Garage恢复、customer_service_e2e.sh、迁移门会写状态，必须核当前runner参数与库名守卫后再登记；不能照抄旧脚本默认密码/数据库连接直接跑。新性能与UAT用例优先加进已有E2E目录，只有确有缺口才增加最小runner，并自带一次确定性小测试。

## 8. 验收与恢复协议 / Verification and Recovery

1. targeted失败保留原始证据→归因（实现/验收/合同/环境）→最小修复→独立代码审查→受影响回归→本地功能提交→重新冻结。修复后不得用旧hash的PASS继承新候选，除非独立影响分析明确覆盖且最终候选仍完整复跑必需门。
2. 验收owner复算：Required精确集合一致、无重复、父子格完整；命令/退出码/非零执行数/Required skipped=0；oracle非空且属于本候选；文件SHA一致，证据不泄漏。
3. 故障恢复独立检查业务：归属唯一、消息唯一真源、幂等键绑定scope/payload、草稿隔离、游标合法、资产授权、各UI收敛、presence负载；流程退出0不是自动通过。
4. 同一环境错误最多重试2次；同缺陷修复最多3轮再报告阻断与NEXT_UNLOCK，不无限循环。未获资源/现场/用户签收不是能靠“继续”自动解决的授权问题。
5. 未知写结果先读幂等状态/消息真源，不盲重放；只清本run带标识的fixture，不能批量关闭未知会话或清别人的库。
6. 本地提交仅在实施授权、验证和review完成后按独立功能进行，命令级 `leeyi <leeyisoft@qq.com>`；不改全局Git配置，不混foreign WIP；本轮计划文件不提交、不push。

最小交付文件：baseline.json、requirements.json、inheritance.tsv、leases.json、commands.sh、acceptance.tsv、candidate-manifest.json、artifacts.sha256、uat-signoff.json、final/verdict.json、final/report.md。UAT签收记录候选hash/版本、五项结果、用户原意见与明确确认；代理生成空签收模板可，代填accept=true不可。

## 9. 不做事项与外部参考 / Exclusions and References

Out of scope: 重写聊天引擎、第二客服消息库、AI/Bot自动回复、RTC、全渠道接入、CRM/工单平台、规则引擎、云草稿、浏览器系统通知或外部消息推送、生产操作、真实客户数据、第三方通知、联系方式配置、未获handoff的主计划共享文件、受保护原生目录。

本计划不要求为了视觉升级增加新依赖；直接复用 Button/Input/ConfirmDialog/EntityDrawer/EmptyState/ErrorState 与现有token。快捷回复宏、批量接待、快捷键大全、满意度运营自动化不作为本轮强制新增。若用户现场证明某项是首接待必需，再另做范围/验收变更。

参考仅用于交互与无障碍原则，不等于照抄供应商功能/状态机，也不证明IMBoy已有或没有对应能力：

- Intercom 官方 Get started with Intercom Inbox：明确分配、回复、关闭与处理工作流；仅借鉴动作信息架构，不引入其AI/全渠道/自动化范围。
- W3C WCAG 2.2 Understanding 4.1.3 Status Messages：成功/等待/错误能被辅助技术识别，同时不给用户无谓焦点跳转。
- W3C WCAG 2.2 Understanding 2.5.8 Target Size (Minimum)：24×24 CSS px或适用间距/例外；本计划关键操作采用更大产品目标，而非误写WCAG全控件44px强制。

本轮检索日期2026-10-08。未做真实浏览器审美评分、未运行业务测试、未确认客户负载或生产运行；这些差距都进入CX-00/CX-08/CX-09/CX-10，不能由这份计划抵消。
