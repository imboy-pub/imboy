# IMBoy Customer Service / Agent Workspace + Enterprise UX 高效 ZCODE 实施计划 V1.1

> 日期：2026-09-24
> 性质：current-main 代码考古后的产品架构、实施 DAG、任务清单、无人值守状态机与验收合同
> 执行方式：闲时、可恢复、最多 8 个活跃 Agent（含唯一协调器 A0）
> 完整性：以相邻 `.sha256` 为准；本文件不内嵌自引用哈希
> 基线来源：Customer Service V2 Phase 0、Enterprise Upgrade 全套设计、Enterprise Organization V21 closure 与当前三仓源码

## 0. 执行结论

这不是把两份旧设计逐项照抄实施的计划。current main 已经完成一部分企业能力，另有一些旧设计假设已失效；客服侧还发现了比“富消息不足”更早的 P0 缺陷：附件在历史重载、Seat 授权下载和 Web Seat 发送链上没有真正闭合。

实施顺序冻结为：

1. **M1 附件真闭环**：先修复已有能力的断链，再做图片预览；不新建富消息框架、缩略图服务或消息表。
2. **M2 Agent Workspace 生产力**：队列摘要/等待时长、未读、客户上下文、presence；每项独立契约、独立验收。
3. **M3 Enterprise UX 真实缺口**：只实施 current main 仍缺的 Admin/Flutter 体验，不重建已存在的邀请、频道发现、九叶导航或 `/enterprise` 业务页。
4. **M4 运营治理**：手工席位 entitlement 与按需统计；不接计费，不建预聚合平台。
5. **最终候选**：每仓只在冻结 SHA 上跑一次全量门；卡片内禁止重复跑全局 EUnit、全量 Bun、全量 Flutter。

计划默认允许 M1、M2、M3 分里程碑独立收口。M1 通过即可形成有用户价值的本地候选，不必等待 M4。

当前状态只能写为：

```text
PLAN_READY
IMPLEMENTATION_NOT_STARTED
EXTERNAL_NOT_EXECUTED
PRODUCTION_NOT_EXECUTED
RELEASE_NO_GO
```

### 0.1 Fast Execution Index

ZCODE 启动、续跑或 A0 重启时先读本节；只有处理具体卡片时才读取对应任务节，不得每轮重新通读或重新研究产品范围。

| 目的 | 唯一入口 | 立即动作 |
|---|---|---|
| 首次启动 | 5.4、5.6、5.10、7 | 校验 sidecar，原子取得 plan lock 并扫描全部同 plan hash RUN；仅历史也为 0 时创建 `RUN_ROOT/INIT` |
| A0 重启/换会话 | 5.10、14.3 | 从 `control/run.json` adopt RUN，双采样 supervisor/lease，再 reconcile，不新建 RUN |
| 找可执行卡 | 6、`control/task-queue.json` | 按 DAG 重算 `ELIGIBLE`；只派发 barrier 已满足且 path/resource lease 可获得的卡 |
| 派发 Worker | 5.2、5.7、5.9 | 原子取得 task/path/resource lease，写 attempt 与 deadline，再启动 Worker |
| Worker 卡住/崩溃 | 5.8、5.9、5.10 | 保存证据，回收本 run lease，按 retry budget reassign；预算耗尽则单卡 `BLOCKED` |
| 环境损坏 | 5.9、5.10 | 只重建本 run worktree/cache/scratch DB/动态端口；不得触碰 foreign 资源 |
| 卡级验证 | 4、各卡 L0 | 只跑 focused command；超时终止本 run 已验证 process group，禁止无限等待 |
| 泳道/真实门 | 12.1、12.2 | 满足依赖后各跑一次 L1/L2；mock/static 不得代替真实门 |
| 最终冻结 | 12.3、13 | 冻结 candidate SHA 后每仓最多一次 L3，更新 Acceptance Ledger 与 milestone |
| 停止判定 | 5.9、14.1 | 普通故障不得停 Run；仅 Hard STOP 白名单或不可恢复终态可停止 |

启动顺序固定为：`adopt-or-create RUN -> baseline -> barriers -> derive eligible -> lease -> dispatch -> watchdog/reconcile -> integrate -> verify -> advance milestone`。当前可执行卡只以 `control/task-queue.json` 中 `state=ELIGIBLE` 且依赖、lease、资源三项同时满足为准；聊天记录、旧报告和 Worker 自报不构成可执行或 PASS 事实。

快速默认参数：Supervisor tick `10s`，Worker heartbeat `15s`，heartbeat dead `60s`，无实质进展 stall `10m`，Worker startup `2m`；精确 command timeout 和 retry budget 见 5.8、5.9。任何 budget 均持久化且只减不增；A0 重启不能重置次数。

### 0.2 本计划的生产级终态

本计划不是只做企业 UI 补差。最终目标由三个同时成立的产品闭环组成：

1. **App 企业组织架构管理闭环**：Flutter 中企业 owner/admin/member 按权限完成组织创建/切换/详情与设置，成员角色/Owner 转移/suspend/restore/offboard，邀请，部门树 create/rename/move/archive/成员与管理员归属，以及企业群、企业频道、Workspace/工作工具入口；所有已有治理动作均有成功、失败、权限、CAS/幂等、刷新恢复和 Android/iOS 真机证据。
2. **Customer Service / Agent Workspace 闭环**：从 Widget 访客进入、排队/派单、坐席上线与容量、双向文本和附件、历史恢复、未读与转接、客户上下文、结束/评价，到平台/企业治理、席位额度和运营统计形成真实 Backend/DB/Web/Flutter 业务闭环。
3. **Internal API 闭环**：继承 V21 已实现的 `31 operations / 14 fixed scopes / 4 Human Directory APIs`，对 runtime/OpenAPI/Postman 一致性、认证、Grant/Workspace 边界、IDOR、cursor、幂等、限流、审计、错误信封和 schema parity 做当前 SHA 重验；只修现有合同回归，不擅自增加 endpoint/scope。

`M1/M2/M3/M4` 仍可分阶段独立交付，测试和故障也按阶段隔离；但只有三个产品闭环、M1-M4 全部、Internal API 门和最终 SHA 门全部通过，才可写 `PRODUCTION_GRADE_LOCAL_CANDIDATE_PASS`。它仅表示隔离本地候选达到投产质量，不代表已合并、已部署或已上线；最终始终保持 `PRODUCTION_NOT_EXECUTED` 与 `RELEASE_NO_GO`。

## 1. Authoring Baseline

以下是计划编写时的只读快照，执行时必须在 W0 重新采集，不得沿用：

| Repo | HEAD | 工作树事实 |
|---|---|---|
| `imboy` | `f7e904d26958e77eaa0eb46dce53bb74ae37c971` | `main`；`docs/customer-service-v2/`、`docs/enterprise-upgrade/` 未跟踪 |
| `imboyadmin` | `342a674d0beb7dc591e94b02fa5e0d97c50192b7` | `main`；干净 |
| `imboyapp` | `a1d6eac535c6271a7bfde462c7f02f68d59708dc` | `main`；已有用户修改 `macos/Podfile.lock`，必须保护 |

当前 migration head 为 `145`，`priv/migrations/` 共 288 个 up/down 文件。Enterprise UX 泳道默认 `DB_CHANGE=0`；Customer Service M2/M4 只有经过对应决策卡后才可预留新迁移编号。

设计输入当前未跟踪，但它们是本计划允许且要求参考的只读设计输入。隔离 worktree 不会自动包含它们，因此 A0 在 W0 必须从共享路径完整读取 `docs/customer-service-v2/` 与 `docs/enterprise-upgrade/`，把文件清单、逐文件 SHA256 和只读快照写入 `RUN_ROOT/input-design-snapshot/`；Worker 从该快照或共享绝对路径只读引用。不得因目录未跟踪而跳过设计约束，也不得擅自修改、删除、移动、提交或把这些目录吸收到任务 commit。本计划同时固化执行范围、决策、任务、验收和停止条件；输入文档与本计划冲突时，以 current-main 代码事实和本计划的重基线裁决为准，并记录差异。

### 1.1 已验证的 current-main 事实

- Customer Service 是独立 feature slice，复用 Organization、Workspace、business identity、enterprise conversation/message/asset。
- Seat 是 business identity 的运营属性，不是新的用户主体；现有治理字段为 `enabled/max_concurrent/version`。
- 会话状态只有 `queued/active/closed`，claim/transfer/close 使用 CAS。
- Widget、Web Seat Workspace、Flutter Seat Workspace 均已存在；Web Seat 使用独立 Seat JWT/QR 认证域。
- 客服消息真源是 enterprise message，服务端托管加密，非 E2EE；不得接入个人 IM 的 Olm/Megolm 数据域。
- Web 合同已有附件 `mime/file_name/size/status`、附件数组和 `last_message.preview` 类型，但真实后端历史读不返回附件，队列后端也不返回 preview。
- Widget 可上传单附件，但刷新/重连后的历史投影会丢失附件。
- Web Seat 当前不能上传附件；其生成的 CS content URL 没有对应后端路由，普通 `<a href>` 也不能携带内存 Seat Bearer。
- Flutter 可上传单附件并解析理想化 `asset_ids`，但真实历史响应未证明含附件投影。
- Enterprise V21 Organization/Internal/Admin 基线已进入 main；不得重做 endpoint、scope、Human Directory、Grant 或九叶治理面。
- Admin 已有 `DataTable/FilterBar/BatchActionBar/EntityDrawer`，不得另造平行组件体系。
- Flutter 已有 Organization 入口、邀请管理和频道发现；`/enterprise` 承载 Enterprise Business，不得改写为组织五 Tab。
- 最新历史证据显示 Backend focused 206/206、Admin 2057/2057 与真实后端九叶 10/10、Flutter Organization 338/338；但 Backend 全量仍记录 3 个跨套件污染失败，Flutter 全仓 analyze 仍有 27 条既有 warning/info。这些只能作为基线分类输入，不是当前 HEAD 的 PASS。

### 1.2 旧 Enterprise Upgrade 任务重基线

| 旧任务 | current-main 判定 | 本计划处理 |
|---|---|---|
| T-P0-1 Admin 域组件 | `PARTIAL` | 先做 capability matrix，只补 shared 缺口 |
| T-P0-2 Flutter 大一统模型 | `PARTIAL / 目标过宽` | 取消大一统模型，只做真实页面需要的错误适配 |
| T-P0-3 AsyncState/Permission | `PARTIAL` | 收敛现有 `OrgStateView.permissionDenied`，不重复实现 |
| T-P0-4 侧栏八项分组 | `INVALID_ASSUMPTION` | 九叶已存在；仅做 Agent Workspace 纳入与权限回归 |
| T-P1-1 成员升级 | `PARTIAL` | 补复合筛选、列持久化、关系 Drawer；不重写现有治理 |
| T-P1-2 邀请管理 | `DONE_CAPABILITY` | 只做原型/回归差距，不新建 invitation_section |
| T-P1-3 部门升级 | `PARTIAL` | 补搜索、徽章、排序、左右布局 |
| T-P1-4 群 12 页收敛 | `MISSING_ABSTRACTION` | 按页面簇机械收敛，URL/行为不变 |
| T-P1-5 频道治理 | `PARTIAL` | 补 UI 缺口和通知覆盖；不做持久治理时间线 |
| T-P1-6 Flutter 企业聚合 | `INVALID_ASSUMPTION` | 在 Organization-owned 路由增量改，不覆盖 `/enterprise` |
| T-P1-7 群详情拆分 | `MISSING` | 先冻结 799 行行为矩阵，再机械拆分 |
| T-P1-8 频道发现 | `DONE_CORE / UNKNOWN_UI_GAP` | 先核验字段和旅程，无缺口即取消卡 |
| T-P2-1 Admin i18n | `MISSING` | 页面稳定后单 owner 收口 |
| T-P2-2 频道 8 页收敛 | `MISSING_ABSTRACTION` | 依赖群布局抽象稳定后分批迁移 |
| T-P2-3 Workspace 测试债 | `PARTIAL / 名称漂移` | 先 inventory，再只补缺测页面 |
| T-P2-4 Organization Profile | `PARTIAL` | 由 shared capability matrix 决定最小差距 |
| T-P2-5 原型核验 | `MISSING / 原型已旧` | 先更新差距账本，再以 current IA 为准 |

## 2. 产品边界与冻结决策

### 2.1 本计划必须交付

- Flutter App 内现有企业能力形成 owner/admin/member 权限分层的生产级管理闭环；不能只验入口、页面打开或视觉差距，必须验组织、成员、邀请、部门、群、频道、Workspace/工具的真实写后读、失败回滚与真机旅程。
- V21 Internal API 的 `31/14/4` 合同在当前候选 SHA 上完整重验；runtime/OpenAPI/Postman 集合差为零，认证、授权、边界、幂等、cursor、限流、审计、错误和 schema 安全矩阵通过。
- 附件在 Widget -> Backend -> Web Seat/Flutter Seat 的发送、历史重载、授权查看链路可重复闭合。
- 图片 MIME 使用已有安全 content endpoint 做缩略展示/预览；其他文件保持文件项。第一版不做服务端缩略图和宽高元数据。
- Web Seat 可复用既有 presign -> PUT -> confirm -> `asset_ids` 协议发送单文件；失败重试保留正文、文件与 `client_msg_id`。
- 队列返回授权后的 `last_message.preview` 和服务端 `waiting_seconds`，不把密文、对象键或敏感客户信息暴露给未授权视图。
- 建立 per-assignment durable read cursor；新坐席接手时以 transfer 时的最后消息为起点，历史可读但不会把转接前全部历史算成新未读。
- presence 明确区分治理开关 `enabled` 与运行态 `online/away/busy/offline`。默认 heartbeat 30 秒，90 秒无 heartbeat 为 offline；手动 away 优先于自动 online；容量满时派生 busy。
- 客户上下文仅包含掩码名、来源、首次/最近出现时间、同组织历史客服会话、明确授权的备注。不得返回电话、邮箱、原始外部身份或跨组织资料。
- Enterprise Admin/Flutter 只补 current-main 缺口，并与 Customer Service/Agent Workspace 入口保持一致。
- 席位 entitlement 第一版为人工配置的 `seat_limit`；现存组织默认 unlimited，降低 limit 不自动停用既有 Seat，只阻止新增/恢复超额 Seat并返回确定错误。
- 运营统计第一版直接从现有 session/event/message 事实按需聚合；无查询证据不得建设日报表、队列、缓存或异步数仓。

### 2.2 明确不做

- 不做订单/商城域、订单引用消息、订单上下文。当前没有权威订单真源，状态为 `BLOCKED_PRODUCT_DECISION`。
- 不做 AI 客服、机器人、知识库回答、自动工单、语音/视频客服、全渠道接入。
- 不做计费、支付、套餐购买、自动扣费；`seat_limit` 只是手工 entitlement。
- 不新建独立 Customer Service Erlang 节点、数据库、附件存储或消息表。
- 不把 Customer Service 接入个人 IM E2EE，不修改 Human-Human C2C/C2G E2EE 行为。
- 不新增 Admin 一级导航；不把平台 Admin Cookie 与 Seat JWT、Human JWT 混用。
- 本期不新增浏览器版企业租户管理门户及其登录会话。企业 owner/admin 自助治理只落在现有 Flutter Organization 工作台并复用 Human JWT；平台 Admin 继续使用平台 Admin Cookie，Web Seat 继续使用 Seat JWT，三者不得混用。未来若做 Web 租户门户，必须另立计划设计面向 Human 身份的 Web 会话、组织切换和权限校验，不属于本计划。
- 不修改 `imboy/erlang.mk`、`imboyapp/ios/*`、`imboyapp/macos/*`、`imboyapp/plugin/r_upgrade`。
- 不 push、不发 PR、不部署、不生产迁移、不使用生产凭证/真实客户数据、不通知第三方、不发布。

发现任务必须突破上述边界时，结束该卡为 `BLOCKED_SCOPE_EXPANSION`，记录 `WHY / SOURCE_EVIDENCE / MINIMUM_PROPOSAL`，其他独立卡继续。

## 3. 目标体验

### 3.1 Web Agent Workspace

保持现有 `/customer-service/workspace` 与 Seat JWT 域，升级为安静、密集、重复操作友好的三栏工作台：

```text
┌───────────────┬─────────────────────────────────┬──────────────────┐
│ 队列/进行中/已结束 │ 当前会话                         │ 客户上下文          │
│ 未读、等待时长      │ 文本/图片/文件、发送、重试、ACK      │ 掩码资料、历史、备注   │
│ 搜索/筛选/优先级    │ 转接/结束保持现有 CAS               │ Seat 状态/容量提示    │
└───────────────┴─────────────────────────────────┴──────────────────┘
```

- 1280px 以上三栏；768-1279px 右栏 Drawer；小于 768px 队列/会话/上下文分层导航。
- 消息区、队列、错误和权限状态不得互相覆盖；所有异步动作有 loading、成功、失败、重试状态。
- 图像预览必须经带 Seat Bearer 的 fetch 获取 Blob，Object URL 卸载/更新时 revoke；JWT、object key、upload URL 不得进入 DOM、日志或地址栏。
- 键盘和可访问性：队列可聚焦，消息列表 `role=log`，状态变化使用节制的 live region，按钮有可读名称，点击目标不小于 44px。

### 3.2 Widget

- 保持 iframe、visit token、Origin/HMAC/JTI 安全模型。
- 单附件能力先真正闭环；同一消息已有附件数组时可渲染多项，但第一版 composer 不要求一次选择多文件。
- 图片显示缩略内容并可预览，非图片显示文件名/大小/下载状态。
- 无可用 Seat 时展示离线说明并允许留下普通消息；不引入机器人或新的留言域。

### 3.3 Flutter Seat Workspace

- 保持 Customer Service feature slice 与普通个人聊天存储/E2EE 隔离。
- 只复用成熟图片/文件展示的最小底层能力，不把客服消息强塞进个人 IM `MessageModel`。
- 所有附件 URL/内容必须经 `AssetsService.viewUrl` 或 Customer Service 已授权客户端获取，禁止裸 `Image.network`。
- Android/iOS 功能验收只认可真机；卡片开发期使用 unit/widget test，最终集中跑完整旅程。

### 3.4 Enterprise UX

- Admin 复用现有 shared primitives；页面不是营销卡片集合，保持高密度治理工具风格。
- Organization 页面和 Customer Service 页面共享实体列表、Profile、治理动作的交互语言，但不创建大一统业务模型。
- Flutter 企业入口继续属于 Organization 路由；`/enterprise` 保留 Enterprise Business 语义。
- Empty/Loading/Error/Permission 四态、权限动作隐藏、危险操作确认、分页筛选重置、TSID lossless、i18n 和暗色模式都是验收项。

## 4. 高效验证模型

### 4.1 四级测试阶梯

| Level | 触发时机 | 允许命令 | 禁止行为 |
|---|---|---|---|
| L0 Card | 每次小改 | format/lint changed paths；新增失败测试；最近邻精确测试 | 全仓 EUnit/Bun/Flutter |
| L1 Lane | 同一 repo 一组卡集成后 | affected-domain bundle；必要的 typecheck/build | 重复跑其他泳道 |
| L2 Journey | Backend+一个消费者契约集成后 | 单个真实 Backend Playwright/HTTP/真机旅程 | mock/static 代替真链 |
| L3 Freeze | 最终候选 SHA，每仓最多一次 | Backend full EUnit；Admin full test/typecheck/build；Flutter full analyze/test | Worker 自行重复全量 |

只有以下路径变化才升级回归范围：

| 变化 | 升级门 |
|---|---|
| `priv/migrations/`、Repo SQL、共享测试 harness | Backend scratch-DB domain bundle + L3 Backend |
| `src/imboy_router.erl`、auth、capability、Facade/port | contract/arch/security + HTTP negative matrix |
| Admin `App.tsx`、sidebar、shared primitive | Admin typecheck/build + 对应路由/菜单 Playwright |
| Flutter router、shared UI、enterprise store/message asset | Flutter affected-domain + 最终 analyze/test |
| 纯页面样式/文案 | changed-file lint + 精确 component/widget test；不触发 Backend |

### 4.2 结果规则

- `test`：`exit_code=0 && test_count>0 && skipped=0` 才可 PASS。
- `mechanical`：`exit_code=0 && oracle_count>0` 才可 PASS。
- HTTP 200、页面能打开、截图、mock-only、旧报告、零测试、取消或 timeout 都不是 PASS。
- L3 失败必须分类为 `NEW_FAILURE / FIXED_BASELINE_FAILURE / UNCHANGED_BASELINE_FAILURE / BLOCKED_ENVIRONMENT`。
- `UNCHANGED_BASELINE_FAILURE` 不能写成 PASS，但不会阻止不依赖它的卡继续；最终状态为 `LOCAL_CANDIDATE_PARTIAL_BASELINE_DEBT`。
- 修复后只重跑受影响 closure。任何代码变化都会失效绑定旧 SHA 的相关 PASS，但不失效无依赖且文件闭包未变的其他仓证据。
- 真机旅程集中执行一次，禁止每个 Flutter 子卡重复安装、登录和截图。

### 4.3 Focused 命令模板

Backend 卡按实际受影响模块选取，不得把列表机械全跑：

```bash
test -n "${TASK_WORKTREE:-}"
cd "$TASK_WORKTREE"
test "$(git rev-parse --show-toplevel)" = "$TASK_WORKTREE"
test -f "$RUN_ROOT/config/backend-scratch.config"
test -n "${RUN_DB:-}"
test "$RUN_DB" != "imboy_v1"
make compile
make eunit-local EUNIT_CONFIG="$RUN_ROOT/config/backend-scratch" t=eb_message_app_tests
make eunit-local EUNIT_CONFIG="$RUN_ROOT/config/backend-scratch" t=eb06_readwrite_entry_tests
make eunit-local EUNIT_CONFIG="$RUN_ROOT/config/backend-scratch" t=cs_handler_tests
make eunit-local EUNIT_CONFIG="$RUN_ROOT/config/backend-scratch" t=cs_widget_handler_tests
make eunit-local EUNIT_CONFIG="$RUN_ROOT/config/backend-scratch" t=cs_seat_workbench_tests
make eunit-local EUNIT_CONFIG="$RUN_ROOT/config/backend-scratch" t=cs_route_contract_tests
make eunit-local EUNIT_CONFIG="$RUN_ROOT/config/backend-scratch" t=channel_logic_notify_tests
make migrations-check
```

`RUN_DB` 必须非空且是 A0 为本 run 创建的唯一数据库名，`backend-scratch.config` 的 `pg_conf` 必须指向该 DB/端口。A0/TEST-00 必须用 Erlang `file:consult/1` 或项目现有 config loader 解析配置并把解析值与 `resources.json` 比对，不得靠字符串 grep；发现默认 `imboy_v1`、foreign DB 或配置与 ledger 不一致时硬阻塞。多模块必须使用 shell loop 逐个运行并逐个记数，不得猜逗号 target。`make arch-check / contract-check / security-gate` 仅在路由、鉴权、Facade/port、契约文件变化时执行；`contract-check` 必须显式传 `ADMIN_DIR="$ADMIN_CANDIDATE" FLUTTER_DIR="$APP_CANDIDATE"`。

Admin/Widget 卡按文件运行：

```bash
test -n "${TASK_WORKTREE:-}"
cd "$TASK_WORKTREE"
test "$(git rev-parse --show-toplevel)" = "$TASK_WORKTREE"
bun test --isolate src/modules/customer_service/seat/seatApiClient.test.ts
bun test --isolate src/modules/customer_service/seat/workbench/contract.test.ts src/modules/customer_service/seat/workbench/workbenchApi.test.ts
bun test --isolate src/modules/customer_service/seat/workbench/SeatWorkspacePage.test.tsx
bun test --isolate src/widget/customer_service/app/widgetApi.test.ts src/widget/customer_service/app/controller.test.ts src/widget/customer_service/app/ui.a11y.test.ts
bunx eslint <changed-files...>
```

Flutter 卡按纵切运行：

```bash
test -n "${TASK_WORKTREE:-}"
cd "$TASK_WORKTREE"
test "$(git rev-parse --show-toplevel)" = "$TASK_WORKTREE"
dart analyze lib/modules/customer_service
dart analyze test/customer_service
flutter test test/customer_service/cs_api_contract_test.dart
flutter test test/customer_service/cs_session_flow_test.dart
flutter test test/customer_service/cs_session_page_test.dart
flutter test test/customer_service/cs_workspace_org_test.dart
```

真实链只在 L2 执行：

```bash
test -n "${ADMIN_CANDIDATE:-}"
cd "$ADMIN_CANDIDATE"
test "$(git rev-parse --show-toplevel)" = "$ADMIN_CANDIDATE"
bunx playwright test --config=playwright.customer-service-p2.config.ts tests/e2e/customer-service-p2/a07-attachment-history-roundtrip.spec.ts
bunx playwright test --config=playwright.customer-service-p2.config.ts tests/e2e/customer-service-p2/a08-agent-productivity-real.spec.ts
bunx playwright test --config=playwright.customer-service-p2.config.ts tests/e2e/customer-service-p2/a01-four-context-chain.spec.ts
bunx playwright test --config=playwright.customer-service-p2.config.ts tests/e2e/customer-service-p2/a02-auth-domain-matrix.spec.ts
```

`a07`、`a08` 是本计划必须新增的真实链 spec；现有 `a01/a02` 只是回归护栏，不能替代图片 DOM、历史刷新、Seat Bearer 内容读取、Web Seat 反向上传、未读/transfer/presence/context Oracle。Flutter 对应新增 `integration_test/customer_service/cs_attachment_roundtrip_test.dart` 与 `cs_agent_productivity_test.dart`，仅在合法真机租约下集中执行。

## 5. 协调、工作树与所有权

### 5.1 并发上限

- `A0` 是唯一持久化 Supervisor、协调器和集成者。
- 最大活跃 Agent 数为 8，包含 A0，因此同时最多 7 个非协调器。
- 完成的席位可复用，不限制累计 Agent 数。
- 最省时的并发单位是 **Backend / Admin-Widget / Flutter 三条 repo lane**，不是为每张卡创建同仓 writer。
- 同一 repo lane 连续持有其 worktree 和依赖缓存；多个 Agent 不得并写同一 candidate worktree。

### 5.2 角色

| Agent | 所有权 | 禁止越界 |
|---|---|---|
| A0 Supervisor | baseline、状态机、watchdog、leases、卡队列、reconcile、candidate integration、ledger、最终状态 | 不替 Worker 偷改业务；不写共享 main；不得因普通故障停整个 Run |
| A1 Backend | CS/Enterprise Backend 纵切、SQL、迁移、focused EUnit | 不改 Admin/Flutter；迁移先获 reservation |
| A2 Agent Web | `imboyadmin/src/modules/customer_service/seat/` 与 CS E2E helper | 不改平台 Organization 页面或 shared 热点 |
| A3 Widget | `imboyadmin/src/widget/customer_service/`、widget build/verify | 不改 Seat/Auth/Admin shared 热点 |
| A4 Enterprise Admin | Organization/Group/Channel 页面簇 | 不改 CS Seat/Widget；shared primitive 需单独 lease |
| A5 Flutter CS | `lib/modules/customer_service/` 与对应测试 | 不改个人 E2EE/聊天数据模型、保留区 |
| A6 Flutter Enterprise | Organization/Group/Channel 增量 UX | 不覆盖 `/enterprise` 业务语义、保留区 |
| F1 | 只读独立 review、hash/Oracle/安全/UX 复核 | 不修代码；发现问题退回 owner |

A0 可在席位空闲后复用 A2-A6 执行后继卡，但每次必须更新 lease。Git author/committer 使用命令级 `leeyi <leeyisoft@qq.com>`；这只授权本地任务提交，不授权 main 合并、push、部署或发布。

### 5.3 共享热点

以下路径默认由 A0 或明确的单一 owner 串行处理：

- Backend：`src/imboy_router.erl`、`priv/migrations/`、enterprise message/asset 共享读路径、auth、Facade/port。
- Admin：`src/App.tsx`、`src/components/layout/sidebarSchema.ts`、`Sidebar.tsx`、`sidebarFilters.ts`、`src/components/shared/index.ts`、`DataTable.tsx`。
- Flutter：`lib/config/router/app_router.dart`、`lib/component/ui/async_state_view.dart`、Organization routes、共享 asset renderer、所有生成 i18n 文件。

任何 Agent 遇到已租用热点，写 `BLOCKED_PATH_LEASE` 并转做独立卡，不等待占座。

### 5.4 Run layout

```text
RUN_ID=cs-agent-entux-v1-<UTC>-<random8>
RUNS_ROOT=/Users/leeyi/project/imboy.pub/.Codex/runs
RUN_ROOT=/Users/leeyi/project/imboy.pub/.Codex/runs/$RUN_ID
WORKTREE_ROOT=/Users/leeyi/project/imboy.pub/.Codex/worktrees/$RUN_ID
IMBOY_CANDIDATE=$WORKTREE_ROOT/imboy/integration
ADMIN_CANDIDATE=$WORKTREE_ROOT/imboyadmin/integration
APP_CANDIDATE=$WORKTREE_ROOT/imboyapp/integration

RUNS_ROOT/.locks/<PLAN_SHA256>.lock/owner.json
control/run.json
control/supervisor.json
control/supervisor.lock/owner.json
control/baseline.json
control/leases.json
control/workers.json
control/resources.json
control/task-queue.json
control/test-impact.json
control/internal-api-manifest.yaml
config/backend-scratch.config
control/integration-ledger.json
control/acceptance-ledger.json
control/pre-test-candidate-manifest.json
control/state-transitions.jsonl
control/recovery-ledger.jsonl
control/commands.jsonl
control/heartbeats/<WORKER_ID>.json
control/progress/<WORKER_ID>.json
evidence/<TASK_ID>/RESULT.json
evidence/<TASK_ID>/*
FINAL/candidate-manifest.json
FINAL/acceptance-matrix.md
FINAL/changed-files.txt
FINAL/commands.jsonl
FINAL/final-report.md
FINAL/RESULT.json
```

每条命令记录 `task_id/repo/cwd/candidate_sha/command/env_allowlist/start/end/exit/test_count/oracle_count/skipped/stdout_hash/stderr_hash`。

### 5.5 唯一控制面与持久化合同

本节不是第二套执行体系。现有 DAG 决定依赖，`control/task-queue.json` 是 Task 状态真源，`control/leases.json` 是写权限真源，`control/acceptance-ledger.json` 是验收真源；Run、Supervisor、Worker、Milestone 状态只是同一事实的可恢复控制面投影。冲突时按 `Git/DB/OS 实况 -> leases/resources -> task queue -> integration ledger -> acceptance ledger -> supervisor projection -> 对话` 的顺序裁决，并把修正写入 transition audit。

所有 control JSON 必须包含 `schema_version/run_id/revision/updated_at/updated_by/supervisor_epoch`，以同目录临时文件写入、`fsync` 后原子 rename。`revision` 只用于发现漂移，不冒充跨进程 CAS；所有共享 control 写入必须先持有 5.10 定义的原子目录锁和 epoch fencing，写前/rename 前各核对一次 epoch，失配立即放弃。JSONL 逐条 append + flush，禁止原地重写历史。`run.json` 至少持久化 `run_state/candidate_profile/selected_milestones/require_internal_api/base_sha/candidate_sha/last_progress_at/recovery_budget/probe_budget/final_status`；其余文件各自保存 supervisor epoch、Worker、Task、lease、资源和验收细节。

每次状态转换必须向 `control/state-transitions.jsonl` 写一条完整记录：

```text
transition_id, timestamp, run_id, layer, entity_id,
from, to, reason_code, reason_detail, task_id, worker_id,
attempt, retry_budget_before, retry_budget_after,
base_sha, candidate_sha, command_id, command, result,
exit_code, evidence_refs, evidence_hash, actor, supervisor_epoch
```

`evidence_hash` 固定为：将 `evidence_refs` 归一为按路径字节序排序的 `[{path,sha256}]` canonical JSON 后做 SHA-256；空证据使用空数组 `[]` 的 SHA-256。`transition_id = sha256(run_id|layer|entity_id|from|to|attempt|reason_code|evidence_hash)`；同 ID 重放必须 no-op。每次 recovery 另写 `recovery-ledger.jsonl`，包含 `recovery_id/fault_class/detection/reclaim/rebuild/reassign/outcome/evidence`。缺字段、无 evidence 或 JSON revision 回退均不得静默接受，进入 `RECONCILING`。

### 5.6 Unattended Operation State Machine

#### 5.6.1 Run 状态机

| 状态 | 进入条件 | 允许动作 | 正常退出条件 | 超时条件 | 自动恢复动作 | 必须持久化 | 下一状态 |
|---|---|---|---|---|---|---|---|
| `INIT` | sidecar 有效；无可 adopt 的本计划 RUN | 建目录、写 schema、生成 RUN_ID/epoch | control 文件原子创建且互相引用一致 | 5m 无进展 | 清理仅本次未发布的 temp 文件后重试 1 次 | plan hash、RUN_ID、selected milestones | `BASELINE_CHECKING` / `RECOVERING` / `HARD_STOP` |
| `BASELINE_CHECKING` | INIT 完成或恢复要求重采样 | 执行 CSX-00、保护 WIP、探测 Git/DB/port/device | baseline 双采样一致，scratch 资源归属明确 | 20m；单探针见 5.8 | 重试 transient probe；冲突卡局部阻塞 | 三仓 HEAD/status、foreign WIP、resource owners、sample times | `READY` / `DEGRADED_CONTINUE` / `RECOVERING` / `HARD_STOP` |
| `READY` | baseline 有效，至少可计算 DAG | 重算 barrier、queue、eligible 集合 | 队列 revision 已写且有 eligible 或明确依赖等待 | 5m | 重建 queue projection，不改任务事实 | barrier、eligible、blocked dependency、queue revision | `DISPATCHING` / `WAITING_DEPENDENCY` / `BLOCKED` |
| `DISPATCHING` | 至少一张 ELIGIBLE 卡且有席位 | A0 在有效 supervisor lock/epoch 下原子登记 task/path/resource lease，启动 Worker | Worker 到 STARTING 或本轮无更多可派卡 | 每个 Worker 2m 未启动 | 释放未生效 lease，计一次 dispatch failure 并 reassign | dispatch_id、worker、task、lease、attempt、deadline | `EXECUTING` / `RECOVERING` / `WAITING_DEPENDENCY` |
| `EXECUTING` | 至少一个 Worker RUNNING/PROGRESSING/TESTING/COMMITTING | watchdog、收 heartbeat/progress、继续派发独立卡 | tick 到期需 reconcile，或所有活动卡终态 | 10m 无任何实质 progress；heartbeat 60s | 将异常 Worker 分类并进入 reclaim；健康 Worker 不受影响 | active workers、last heartbeat/progress、command deadlines | `RECONCILING` / `RECOVERING` / `INTEGRATING` |
| `RECONCILING` | 周期 tick、事件触发或状态不一致 | 执行 5.10 全量对账；修正 projection | 所有可自动修复不一致已闭合 | 5m 或同一 invariant 连续 3 tick 不闭合 | 按故障矩阵局部恢复；不确定且可能损失数据才停 | invariant results、drift、reconcile epoch、repairs | `DISPATCHING` / `EXECUTING` / `RECOVERING` / `DEGRADED_CONTINUE` / `HARD_STOP` |
| `INTEGRATING` | 当前里程碑所需卡 PASS/允许取消，lane commit ready | A0 幂等集成已 review commit，更新 candidate SHA | commit 均已包含且 worktree clean | 每 repo 20m | 检测 ancestor/patch-id；冲突退 owner，最多 2 次 | source commit、patch-id、candidate before/after、conflict evidence | `VERIFYING` / `RECOVERING` / `BLOCKED` |
| `VERIFYING` | candidate SHA 冻结或 L1/L2 门 ready | 只执行相应 L1/L2/L3，写 Acceptance Ledger | 所选门有真实 oracle 和终态 | 按 5.8 command timeout | 环境/瞬态有限重试；代码/测试失败回任务，不盲重跑同 SHA | candidate SHA、command IDs、counts、oracle、evidence hash | `MILESTONE_ADVANCING` / `RECOVERING` / `DEGRADED_CONTINUE` / `BLOCKED` |
| `MILESTONE_ADVANCING` | 当前 milestone acceptance 已结算 | 原子更新 milestone 与下游 DAG | milestone 终态、下游 queue 重算完成 | 5m | 从 Acceptance Ledger 重建 milestone projection | milestone acceptance set、status、candidate SHA | `READY` / `INTEGRATING` / `COMPLETED` / `DEGRADED_CONTINUE` |
| `COMPLETED` | 所选 milestone 均终态，FINAL hash-valid | 只读核验、释放本 run lease/resource、写最终报告 | FINAL/RESULT 完整，资源释放结果已记账 | 15m 收尾 | 重入只补缺失报告/释放动作，不重跑已绑定门 | final hashes、release results、`RELEASE_NO_GO` | 终态；证据损坏则 `RECOVERING` |
| `RECOVERING` | 可恢复 fault、Worker 丢失、环境异常、A0 adopt | 保存故障证据、reclaim/rebuild/reassign | invariant 恢复或卡预算耗尽 | 单次 20m；总预算见 5.9 | 预算内重试；耗尽仅 BLOCKED 当前卡并继续独立 DAG | fault class、budget、actions、before/after evidence | `RECONCILING` / `DEGRADED_CONTINUE` / `BLOCKED` / `HARD_STOP` |
| `DEGRADED_CONTINUE` | 一张或多张非关键/可隔离卡 BLOCKED，仍有独立卡 | 标记受影响依赖，继续调度其他 eligible 卡 | 有新 eligible、阻塞解除或已无独立工作 | 30m 无队列变化时强制结算 | 按有限 probe schedule 重探；耗尽后结算受影响 milestone | blocked set、unaffected set、degraded reason | `DISPATCHING` / `WAITING_DEPENDENCY` / `MILESTONE_ADVANCING` / `BLOCKED` |
| `WAITING_DEPENDENCY` | 无可派卡，但存在运行中前置或可恢复外部资源等待 | 保持 watchdog，有限重探，不占 Worker 席位 | 依赖 PASS/取消、资源恢复或 probe 耗尽 | 按 5.9 六次 probe schedule | 重算 DAG；耗尽后把依赖卡 BLOCKED 并结算 | waiting tasks、dependency/resource IDs、probe budget/fingerprint | `READY` / `DEGRADED_CONTINUE` / `BLOCKED` |
| `BLOCKED` | 当前所选范围无 eligible/running，至少一项有明确解锁条件 | 生成 blocker 和最小解锁条件；执行有限重探 | 依赖/资源恢复，或 probe 耗尽后完成结算 | 按 5.9 六次 probe，耗尽不再定时唤醒 | 条件恢复回 `READY`；耗尽后转里程碑/Run 终态报告 | blocker class、affected tasks、unlock oracle、probe budget/fingerprint | `READY` / `DEGRADED_CONTINUE` / `MILESTONE_ADVANCING` / `FAILED_TERMINAL` |
| `HARD_STOP` | 命中 14.1 白名单且继续可能破坏安全/WIP/数据 | 立即停止新派发；仅终止已验证为本 run 的进程；保存证据 | 不自动退出，需要新的明确授权/修复事实 | 无 | 只读复核；A0 重启必须保持 HARD_STOP，不能清零 | trigger、scope、live resources、evidence、required authority | 终态，或经新授权后 `BASELINE_CHECKING` |
| `FAILED_TERMINAL` | 所选范围已无安全可行路径，且不属于需保护现场的 HARD_STOP | 生成失败报告、释放安全可释放资源 | FINAL failure record hash-valid | 15m 收尾 | 重入只补报告，不重新派发 | terminal reasons、task/milestone states、release results | 终态 |

Run 正常链必须保持用户指定顺序；`RECONCILING` 是 EXECUTING 期间的周期状态，完成后可回 `EXECUTING/DISPATCHING`，不是跳过集成或验证的捷径。Run 进入 `BLOCKED` 只表示本次所选范围当前无可推进工作；单卡失败通常进入 `DEGRADED_CONTINUE`。

#### 5.6.2 A0 Supervisor 状态机

| 状态 | 进入条件 | 允许动作 | 退出条件 | 超时条件 | 自动恢复 | 持久化字段 | 下一状态 |
|---|---|---|---|---|---|---|---|
| `CREATED` | RUN 新建 | 创建 supervisor record | record 原子落盘 | 1m 未开始 acquire | 重写未发布 temp record | supervisor_id、epoch | `ACQUIRING` |
| `ACQUIRING` | 首次启动或 restart | 按 5.10 原子 `mkdir` 取得 supervisor lease；双采样旧 heartbeat/PID/epoch | lease 获得或确认活跃 owner | 2m | stale 才原子 rename/reclaim；不得抢活 A0 | owner、PID/PGID、process_start、lease expiry | `ADOPTING` / `ACTIVE` / `CONFLICT` |
| `ADOPTING` | 找到未终态 RUN | 按 5.10 重建内存、adopt Worker/lease/queue | durable state 与实况完成对账 | 5m | 重复 adopt 幂等；不创建新 RUN | adopted revision、orphan list、candidate SHA | `RECONCILING` / `FAILED` |
| `ACTIVE` | lease 有效且 state 一致 | 每 10s tick、续租、选择下一控制动作 | 选定 dispatch/monitor/integrate/verify | heartbeat 写入失败 30s | 新实例可在 stale 双采样后 adopt | heartbeat、last_progress、loop counter | `DISPATCHING` / `MONITORING` / `INTEGRATING` / `VERIFYING` |
| `DISPATCHING` | Run=DISPATCHING | 原子 lease + 启动 Worker | Worker STARTING 或无更多 eligible | 单 dispatch 2m | 撤销未生效 dispatch；reassign | dispatch IDs、attempts | `MONITORING` / `RECOVERING` |
| `MONITORING` | 有活动 Worker | watchdog、采集事件，不写业务 | 到 tick 或收到异常事件 | supervisor 30s 无 heartbeat | 转 reconcile，不等待人工 | worker snapshot hash | `RECONCILING` / `RECOVERING` |
| `RECONCILING` | tick/restart/invariant violation | 执行 5.10 检查集 | invariant 闭合或 fault 已分类 | 5m | 有限修复，不一致持续则分类 | reconcile epoch、checks、repairs | `ACTIVE` / `RECOVERING` / `FAILED` |
| `RECOVERING` | 可恢复 fault | reclaim/rebuild/reassign | invariant 恢复或 budget 耗尽 | 20m/次 | budget 耗尽局部 BLOCKED | recovery IDs、budgets | `RECONCILING` / `ACTIVE` / `FAILED` |
| `INTEGRATING` | Run=INTEGRATING | 幂等 cherry-pick/merge 到 candidate | commit 已包含或冲突已归属卡片 | 20m/repo | patch-id/ancestor 检测；冲突退卡 | integration operation IDs | `VERIFYING` / `ACTIVE` / `FAILED` |
| `VERIFYING` | Run=VERIFYING | 执行有 deadline 的门并记账 | gate 得到可审计终态 | 5.8 对应 timeout | fault matrix 处理 | command IDs、candidate SHA | `ACTIVE` / `RECOVERING` / `FAILED` |
| `QUIESCING` | Run 终态或 HARD_STOP | 禁止新派发，回收安全资源，flush ledger | 安全资源释放和 ledger flush 结束 | 15m | 重启继续 quiesce，不重跑任务 | unreleased resources、flush hashes | `STOPPED` / `FAILED` |
| `STOPPED` | 收尾完整 | 只读 | 无 | 无 | 无 | terminal run state、finished_at | 终态 |
| `CONFLICT` | 发现另一个活跃 A0 | 不抢 lease；当前实例退出 | 当前实例退出 | 立即 | 若之后明确 stale，由新实例重新 ACQUIRING | competing owner evidence | 当前实例终态 |
| `FAILED` | A0 无法安全判定/持久化控制状态 | 停派发并把 Run 转相应终态 | Run 已转 HARD_STOP/FAILED_TERMINAL | 立即 | 数据风险用 HARD_STOP；否则 FAILED_TERMINAL | error、last durable revision | `QUIESCING` |

#### 5.6.3 Worker 状态机

| 状态 | 进入条件 | 允许动作 | 退出条件 | 超时条件 | 自动恢复 | 持久化字段 | 下一状态 |
|---|---|---|---|---|---|---|---|
| `CREATED` | A0 生成 worker_id | 写能力/owner/lane | worker record 完整 | 30s 未派发 | 未派发可丢弃重建 | worker_id、capabilities | `ASSIGNED` |
| `ASSIGNED` | A0 在当前 epoch 下已原子登记 task/path/resource lease | 接收只读合同 | Worker ack assignment | 30s 未 ack | 释放 lease，换 Worker | task、lease、attempt | `STARTING` / `FAILED` |
| `STARTING` | Worker ack | 校验 worktree/base/paths/env | preflight 通过或明确失败 | 2m | 启动失败记 WORKER_CRASH/ENVIRONMENT | PID/PGID、process_start、worktree | `RUNNING` / `CRASHED` / `FAILED` |
| `RUNNING` | preflight 通过 | 读取卡片、执行最小改动 | 产生 progress、进入 test 或出现异常 | heartbeat 60s；progress 10m | watchdog 分类 | command、heartbeat、last_progress | `PROGRESSING` / `TESTING` / `HEARTBEAT_TIMEOUT` / `STALLED` / `CRASHED` |
| `PROGRESSING` | 有真实源码/测试/证据增量 | 继续实现并每 15s heartbeat | 实现 ready 或出现异常 | 10m 无实质 progress | 保存 diff/日志后 reclaim | changed-path hash、progress kind | `PROGRESSING` / `TESTING` / 异常状态 |
| `TESTING` | 实现 ready | 只跑卡片 focused tests | command 产生 PASS/FAIL/timeout | 按 5.8；无输出不延长 deadline | timeout 后终止本 run PGID并分类 | command ID、deadline、counts | `COMMITTING` / `FAILED` / `STALLED` |
| `COMMITTING` | focused gate PASS | 检查 exclusive paths、创建独立本地 commit/RESULT | commit/RESULT durable 或明确失败 | 5m | 先查 commit/patch-id，避免重复提交 | tree SHA、commit SHA、RESULT hash | `DONE` / `FAILED` |
| `DONE` | commit + RESULT durable | 释放 Worker lease，等待 A0 验收 | lease 已释放 | 1m 未释放 | A0 幂等 release | final heartbeat、commit/result | 终态 |
| `HEARTBEAT_TIMEOUT` | 60s 无 heartbeat | 禁止继续信任该 Worker | 双采样确认存活或失联 | 第二采样 15s | 进程活且有可信 progress 可回 RUNNING，否则 reclaim | samples、PID evidence | `RUNNING` / `RECLAIMING` |
| `STALLED` | 10m 无实质 progress或 command timeout | 保存 stdout/stderr/diff/process tree | evidence 封存完成 | 1m | 终止仅本 run PGID，计 WORKER_STALL | stall reason、last progress、artifacts | `RECLAIMING` |
| `CRASHED` | 进程退出/会话消失且未 DONE | 保存 exit/core/log/diff | crash evidence 封存完成 | 1m | 计 WORKER_CRASH，reclaim | exit、orphan process/lease evidence | `RECLAIMING` |
| `RECLAIMING` | timeout/stall/crash | 冻结 worktree，回收 lease/resource，判 retry | ownership/lease 已结算 | 5m | 不能确认 ownership 则不杀不删，升级保护 | reclaimed IDs、snapshot hash、budget | `REASSIGNED` / `FAILED` |
| `REASSIGNED` | retry budget > 0 且任务仍安全 | 生成新 worker_id，沿用 task attempt+1 和证据 | 新 assignment 写入 | 2m | dispatch 失败继续预算规则 | previous/new worker、attempt | `ASSIGNED` |
| `FAILED` | 不可恢复或 budget 耗尽 | 写 Worker 终态，不直接判 Task PASS/FAIL | failure record 和 lease release durable | 1m | A0 将 Task 转 BLOCKED 或 RETRYING | failure class、evidence、lease release | 终态 |

Heartbeat 只是存活信号；`last_progress` 仅在“命令完成并有 evidence、新增 before-failing test、changed-path hash 改变、创建 commit/RESULT、Task/Acceptance 有合法转换”时更新。重复读取、重复说明、只有日志心跳、等待锁、无新输出的长命令都不算 progress。

#### 5.6.4 Task 状态机

| 状态 | 进入条件 | 允许动作 | 退出条件 | 超时条件 | 自动恢复 | 持久化字段 | 下一状态 |
|---|---|---|---|---|---|---|---|
| `PENDING` | 卡存在但依赖/barrier 未满足 | 只读重算依赖 | 依赖满足、取消或确定不选择 | 每 tick 重算，无 wall timeout | 依赖满足自动推进 | depends、acceptance IDs | `ELIGIBLE` / `CANCELLED` / `SKIPPED` |
| `ELIGIBLE` | 所有依赖为 PASS/允许取消，scope/path/resource 可用 | 等待 A0 派发 | A0 在当前 epoch 下完成原子 lease 登记或资源阻塞 | 30m 未派发触发公平性检查 | 优先最老 eligible，不占 lease | eligible_at、priority、required resources | `ASSIGNED` / `BLOCKED` |
| `ASSIGNED` | task/path/resource lease 原子成功 | 启动唯一 Worker | Worker RUNNING 或 assignment 失败 | 2m | 释放 stale lease并 reassign | worker、lease、attempt | `RUNNING` / `FAILED` |
| `RUNNING` | Worker RUNNING/PROGRESSING/TESTING/COMMITTING | 记录 progress 与 focused gate | Worker 交付 RESULT 或失败 | heartbeat 60s / progress 10m / command 见 5.8 | crash/stall 分类后 retry | worker state、last_progress、changed paths | `VERIFYING` / `FAILED` |
| `VERIFYING` | Worker RESULT/commit 已交付 | F1/A0 核验路径、commit、test/evidence | 核验得到 PASS/FAIL | 10m 卡级核验 | evidence 缺失退回 retry，不猜 PASS | candidate/source SHA、acceptance evidence | `PASSED` / `FAILED` |
| `PASSED` | 所有卡 Acceptance 与 focused gate 满足 | 供 DAG 解锁和集成 | 正常终态；证据失效例外 | 无 | 若依赖闭包/candidate 变化使证据失效，审计后回 ELIGIBLE | commit、RESULT/evidence hash、valid_for_sha | 终态或 `ELIGIBLE` |
| `FAILED` | Worker/test/code/verification 失败已分类 | 计算 budget，不直接无限重跑 | retry 决策 durable | 1m | 可恢复则 RETRYING；否则 BLOCKED | fault class、attempt、budget、evidence | `RETRYING` / `BLOCKED` |
| `RETRYING` | fault 可恢复且 budget > 0 | backoff、必要环境重建、重新派发 | backoff 到期且恢复前置满足 | backoff 上限 3m | 保留前次 evidence/commit，attempt+1 | next_retry_at、budget、recovery ID | `ELIGIBLE` / `BLOCKED` |
| `BLOCKED` | budget 耗尽、scope/lease/dependency/resource 明确阻塞 | 释放席位，记录解锁 oracle，执行有限 probe | unlock oracle 成立或 probe 耗尽 | 按 5.9 六次 probe；耗尽后 dormant | 条件成立自动回 ELIGIBLE；耗尽后不再定时唤醒 | blocker、unlock oracle、affected milestone、probe budget/fingerprint | `ELIGIBLE` / `SKIPPED` / `CANCELLED` |
| `CANCELLED` | current-main 已完成或 A0 在范围内判定不需实施 | 只读保存证据 | 终态 | 无 | 不自动重开；基线变化需新 transition | cancel reason、source evidence | 终态 |
| `SKIPPED` | 未选择该里程碑，或永久失败前置使可选后继不可达 | 不执行、不伪装 PASS | 终态 | 无 | 若用户选择范围/依赖事实改变，可回 PENDING | skip reason、dependency | 终态或 `PENDING` |

`FAILED` 是一次失败事件，不是最终状态；相同 `candidate_sha + command_id + evidence_hash` 只能扣一次预算。`BLOCKED` 卡恢复为 `ELIGIBLE` 时必须先验证 blocker 的 unlock oracle，而不是因时间经过自动清零。

#### 5.6.5 Milestone 状态机

| 状态 | 进入条件 | 允许动作 | 退出条件 | 超时条件 | 自动恢复 | 持久化字段 | 下一状态 |
|---|---|---|---|---|---|---|---|
| `PLANNED` | M1-M4 或 INTERNAL_API closure 在计划中 | 选择/不选择本候选 | selection 已确定 | 无 | 从 selected scope 重建 | acceptance set、selected | `READY` / `NOT_IN_CANDIDATE` |
| `READY` | barrier PASS 且至少一张任务可推进 | 释放任务到 DAG | 至少一张 Task 进入活动态或确认阻塞 | 5m | 重算 task set | task IDs、barrier SHA | `EXECUTING` / `BLOCKED` |
| `EXECUTING` | 至少一张 milestone 卡活动或待执行 | 依 DAG 执行 | 必需任务终态或无可推进任务 | 无全局超时；由卡级 watchdog 控制 | 局部故障继续独立卡 | task state summary、last_progress | `VERIFYING` / `DEGRADED` / `BLOCKED` |
| `VERIFYING` | 必需任务终态且集成 SHA 冻结 | 执行 milestone L1/L2/Acceptance | 所有 gate 得到终态 | 5.8 对应门超时 | 有限恢复；代码失败退任务 | candidate SHA、gate IDs、ledger refs | `PASSED` / `DEGRADED` / `BLOCKED` |
| `PASSED` | 该 milestone 全部必需 Acceptance PASS | 解锁后继/候选 | 正常终态；证据失效例外 | 无 | 证据失效则审计后回 VERIFYING | acceptance hashes、candidate SHA | 终态或 `VERIFYING` |
| `DEGRADED` | 可选卡/基线债阻塞但允许继续其他 milestone | 报告缺口并继续 DAG | blocker 恢复或 probe 耗尽结算 | 按 5.9 有限 probe | blocker 恢复则 EXECUTING/VERIFYING；耗尽保持诚实缺口 | blocked optional IDs、impact、probe budget | `EXECUTING` / `VERIFYING` / `BLOCKED` |
| `BLOCKED` | 任一必需 Acceptance 不可达或预算耗尽 | 保存最小解锁条件，不阻塞独立 milestone | unlock oracle 成立或 probe 耗尽 | 按 5.9 六次 probe；耗尽后 terminal snapshot | 条件恢复自动 READY/EXECUTING；耗尽后结算 FAILED/NOT PASS | failed/blocking IDs、unlock oracle、probe budget/fingerprint | `READY` / `EXECUTING` / `FAILED` |
| `NOT_IN_CANDIDATE` | 本 run 未选择该 milestone | 禁止派发其卡 | 终态 | 无 | 仅显式扩展 selected scope 可重开 | selection reason | 终态或 `PLANNED` |
| `FAILED` | 必需任务永久失败且本 run 无可恢复路径 | 结算但保持 `RELEASE_NO_GO` | 终态 | 无 | 新 RUN 或明确修复事实才重开 | terminal task/evidence set | 终态 |

### 5.7 自动转换与 DAG 继续规则

1. Worker heartbeat 超时：`RUNNING -> HEARTBEAT_TIMEOUT`，15 秒后第二次采样；若 PID/PGID、start time 和 worktree 均属于本 run 且出现有效 progress，可恢复 `RUNNING`，否则 `RECLAIMING -> REASSIGNED`。
2. Worker crash/stall：A0 先封存 diff、日志、command 和 process evidence，再回收 task/path/resource lease；retry budget 足够则 Task `FAILED -> RETRYING -> ELIGIBLE`，新 Worker 使用新 ID 和递增 attempt。旧 Worker 后续回连只能上报 evidence，禁止继续写。
3. 环境故障：只重建本 run 的动态端口、scratch DB、worktree 或缓存；候选 commit/patch-id 已存在则复用，不重复提交。环境重建完成后原 Task 回 `ELIGIBLE`。
4. 测试失败：进程退出/端口瞬断等才归 `TRANSIENT/ENVIRONMENT` 并有限重试；稳定断言失败归 `TEST_FAILURE/CODE_FAILURE`，同一 SHA 不盲重跑，退回 owner 修复。连续失败达到预算后仅该卡 `BLOCKED`。
5. Task `PASSED/CANCELLED` 解锁下游；可恢复 `BLOCKED` 的依赖恢复且 unlock oracle 成立时自动回 `ELIGIBLE`。永久阻塞只让其必需后继 `WAITING_DEPENDENCY/BLOCKED/SKIPPED`，不影响其他分支。
6. 队列每个 tick 都从 DAG + Task 真源重算；不存在“Worker 消失导致卡永久 RUNNING”或“A0 重启后从 W0 重来”。
7. 所有 eligible 卡均结束且当前 milestone 需要集成时进入 `INTEGRATING`；没有需要集成但仍等待可恢复依赖时进入 `WAITING_DEPENDENCY`；无独立工作且解锁条件不可满足时才进入 Run `BLOCKED/FAILED_TERMINAL`。

### 5.8 Watchdog 与 command deadline

| 对象 | 默认 deadline | 到期动作 |
|---|---:|---|
| Supervisor tick/heartbeat | 10s / 15s | 30s 未续租后由新 A0 双采样；活实例不可抢占 |
| Worker heartbeat | 每 15s，60s dead | 双采样后 reclaim/reassign |
| Worker startup | 2m | 释放未生效 lease，记 crash/environment |
| Worker 实质 progress | 10m | 标记 STALLED，封存 evidence，安全终止本 run PGID |
| L0 changed-file lint/unit/widget test | 5m/命令 | timeout；最多按 fault matrix 处理 |
| Backend compile / 单 EUnit module | 15m / 10m | 保存日志并终止本 run PGID |
| Widget build/verify、Admin typecheck/build | 10m / 15m | 同上 |
| Flutter module analyze / focused test | 10m / 10m | 同上；不升级全仓测试 |
| L2 Playwright/HTTP journey | 15m/spec | 保存 trace/log/DB snapshot 后分类 |
| L3 Backend / Admin / Flutter | 45m / 30m / 45m | 不无限等待；timeout 为 BLOCKED_ENVIRONMENT 或真实失败 |
| 单个真机 journey | 30m | 保存 device/app/fixture evidence；不换模拟器代证 |
| COMMITTING / 卡级 VERIFYING | 5m / 10m | 查 commit/RESULT 幂等状态，再 recovery |

命令可以在 manifest 中声明更短 deadline；延长必须由 `TEST-00` 以历史时长证据预先写入，Worker 不得临时取消 timeout。终止进程前必须同时核对 PID、PGID、process start time、cwd 位于本 run worktree、command_id 匹配；任一不符则不得 kill，转 `PROTECTED_WIP/LEASE_CONFLICT`。只杀本 run 的 process group，不得 `pkill`、按端口泛杀或清理 foreign container。

### 5.9 故障分类、预算与停止策略

统一上限：单 Task 最多 `3` 个实现 attempt（首次 + 2 次修复/重派），单 command 最多 `2` 次 transient retry，单资源最多 `2` 次 rebuild，单 Worker 最多 `2` 次 replacement；取最先耗尽者。command backoff 为 `15s -> 60s -> 180s`，带 0-20% jitter。每个 blocker 另有持久化 `probe_count/max_probes=6/next_probe_at/max_wait_until/blocker_fingerprint/generation`，probe 时刻为 `1m/2m/5m/10m/20m/30m`，总等待不超过 68 分钟；同一 Run 最多 2 个 fingerprint generation，只有依赖/resource fingerprint 实际改变才产生新 generation，A0/会话重启和单纯时间经过都不能重置预算。

probe 耗尽后该 Task 保持 dormant `BLOCKED`，不再注册 timer、不占 Worker/进程；A0 结算受影响 Acceptance/Milestone，继续其余 DAG。全部独立工作结束后，有可交付里程碑则生成带 blocker 的 FINAL 并进入 `COMPLETED`，完全无可交付路径则 `FAILED_TERMINAL`；Supervisor 随后 QUIESCING/STOPPED，不得永久轮询。未来外部事实真正变化时由新 RUN 或明确 resume 重新验证 unlock oracle，旧 probe 历史必须保留。

| 故障类 | 识别 Oracle | 自动预算/动作 | 卡与 DAG 结果 | 允许全局 HARD_STOP |
|---|---|---|---|---|
| `TRANSIENT` | 一次性 I/O、锁、网络/进程抖动且 invariant 未坏 | 同 command 最多 2 次；15s/60s | 耗尽后卡 BLOCKED，其他分支继续 | 否 |
| `WORKER_CRASH` | Worker 进程/会话异常退出 | 最多 replacement 2 次；保存 diff/log 后 reassign | 耗尽后卡 BLOCKED | 否 |
| `WORKER_STALL` | heartbeat 活但 10m 无实质 progress，或 command deadline | 最多 replacement 2 次；安全终止本 run PGID | 耗尽后卡 BLOCKED | 否 |
| `ENVIRONMENT` | 本 run port/DB/cache/worktree/toolchain 可重建故障 | 每资源最多 rebuild 2 次 | 卡 RETRYING；耗尽 BLOCKED_ENVIRONMENT，独立卡继续 | 仅 migration corruption/ownership 无法安全判断 |
| `TEST_FAILURE` | 测试运行正常但断言/Oracle 失败 | 同 SHA 0 次盲重跑；允许最多 2 次修复 attempt | 修复后 focused rerun；耗尽卡 BLOCKED | 否 |
| `CODE_FAILURE` | compile/type/lint/行为失败 | 最多 2 次修复 attempt | 耗尽卡 BLOCKED，下游等待，独立卡继续 | 否 |
| `LEASE_CONFLICT` | path/resource/task lease owner 不一致或仍活跃 | 0 次抢占；等待/转独立卡；stale 双采样后才 reclaim | 卡 WAITING/BLOCKED，可恢复后 ELIGIBLE | 否；无法确认 foreign ownership 时可保护性停机 |
| `SCOPE_EXPANSION` | 必须突破 2.2 或卡片 exclusive paths/产品边界 | 0 次 | 当前卡 `BLOCKED_SCOPE_EXPANSION`，独立卡继续 | 否，除非已发生破坏性外向动作 |
| `BASELINE_DRIFT` | base/main/WIP 与 baseline ledger 不一致 | 只允许 1 次只读重采样和 candidate 可重建证明 | 可隔离则继续；不可恢复 base SHA 漂移则停 | 是，仅不可恢复且继续会吸收/覆盖 WIP |
| `SECURITY_VIOLATION` | 鉴权弱化、跨租户泄漏、secret/凭证暴露 | 0 次；停写、保存最小证据 | Run HARD_STOP；不得自行“放宽测试” | 是 |
| `PROTECTED_WIP` | 可能覆盖、删除、stash、混入用户/foreign WIP | 0 次；不触碰现场 | 冲突局部可隔离则卡 BLOCKED；否则 HARD_STOP | 是 |
| `UNKNOWN` | 无法可靠归类 | 只允许 1 次只读取证/reconcile，不执行破坏性恢复 | 无数据风险则卡 BLOCKED 并继续；可能数据损失则停 | 仅可能造成数据损失/越权时 |

“自动恢复优先于停止”是强制规则：普通 test/code/worker/environment 故障、无设备、可恢复依赖、单卡预算耗尽均不得停止整个 Run。只有安全违规、受保护 WIP、未授权破坏性/外向动作、不可恢复 base SHA 漂移、migration corruption/ownership 不可安全判断、或 UNKNOWN 且可能造成数据损失时可进入 `HARD_STOP`。

### 5.10 Supervisor Reconciliation Loop 与 A0 重启 adopt

A0 在尚不知道 `RUN_ROOT` 时，先计算已校验计划文件的 SHA256，并以原子 `mkdir` 获取 `RUNS_ROOT/.locks/<PLAN_SHA256>.lock`。目录创建成功才可执行 run discovery；`owner.json` 写 `run_id/supervisor_id/epoch/PID/PGID/process_start/heartbeat`。目录已存在时双采样 30 秒：owner 仍活跃则本实例 `CONFLICT` 退出；确认 stale 后，只有一个实例能原子 rename 为 `.stale.<old_epoch>` 并重新 `mkdir`，失败者重新读取，不得 `rm -rf` 锁。

持有 plan lock 后扫描 `RUNS_ROOT/*/control/run.json`，按 plan SHA 找全部 RUN。存在非终态 RUN 时：一个则 adopt；多个时，只有“恰有一个 live supervisor，且其他 RUN 为空 INIT 或其全部 commit/ledger 已被该 live RUN 证明包含”时才 adopt live RUN；否则只有恰有一个 candidate/ledger 通过 ancestor、patch-id 和 evidence hash 证明包含其余 RUN 的全部进展时才可 adopt 它。多个空 INIT 可确定性选择最早 RUN_ID并把重复事实记入 recovery ledger。若多个 RUN 各有不能证明被包含的独立 commit/迁移/副作用，分类为 `UNKNOWN` 且有丢失进展风险，进入 HARD_STOP，不得按 mtime 或“看起来最新”选择。

若没有非终态 RUN但存在 hash-valid 的 `COMPLETED/FAILED_TERMINAL/HARD_STOP` RUN：只有一个时返回其 FINAL；多个构成可验证 `previous_run_id/previous_final_hash` 单链时返回唯一叶子 FINAL；多个互不包含时只读返回冲突清单。三种情况都不得创建第二个 RUN；`HARD_STOP` 只能按其记录的授权恢复，不能靠新 RUN 绕过。只有启动输入显式携带 `NEW_RUN_AUTHORIZATION`，并持久化 `reason/previous_run_id/previous_final_hash/changed_fact_fingerprint` 后，才允许新建后继 RUN；该授权不能从时间经过、调度器重试或对话旧值推断。仅当同 plan hash 的历史和非终态 RUN 均为零时，才可无额外授权创建首个 RUN。

确定 `RUN_ROOT` 后用同样的原子目录协议取得 `control/supervisor.lock`，再将 epoch 单调加一。所有 A0 control 写、Worker dispatch token、lease 和 command 都携带该 epoch；执行副作用前和落盘前重新读取 lock owner，epoch 不匹配即 fencing 拒绝。plan lock 在 Run 活跃期间续 heartbeat；Supervisor 正常收尾先把 `terminal_run_id/final_state/final_hash/finished_at` 写入 owner，再做可审计 rename 为 `.released.<epoch>`，不删除锁证据；released lock 信息必须与 terminal RUN/FINAL 双向核验。

A0 每 10 秒执行一次完整 tick；为降低成本，昂贵 Git/DB/migration 探针每 60 秒或事件触发执行，但其结果仍属于同一 reconcile epoch。每轮必须检查：

1. supervisor lease/epoch/heartbeat 与实际 PID/PGID/start time；发现旧 A0 时双采样间隔 30 秒，只有两次均 stale 且进程不存在才 reclaim。
2. Worker heartbeat、`last_progress`、process、command deadline、worktree dirty/tree/HEAD 与 Worker 声明是否一致。
3. task/path/resource lease 是否唯一、未过期、owner 存活；旧 Worker 回连不得恢复写权限。
4. Task queue 是否可由 DAG、barrier、Task 终态和 selected milestones 确定性重建；漏掉的 eligible 自动补回。
5. 三仓 worktree/branch/base/candidate SHA、commit/patch-id、changed paths 是否与 integration ledger 一致；共享 main 是否仍只读。
6. migration reservation 是否唯一，up/down 文件号、scratch DB migration table 与 candidate SHA 是否一致。
7. 动态端口、scratch DB/container、配置文件和进程 ownership；禁止接管 foreign 端口/DB/process。
8. Acceptance Ledger 的 PASS 是否绑定当前 candidate SHA、command、`test_count>0/oracle_count>0/skipped=0` 和可读 evidence hash。
9. Milestone 状态是否能由 Acceptance Ledger 重算；任何手工写出的 PASS 必须被纠正并记录 audit。

adopt 顺序为：完成全局 discovery/plan lock -> 校验 control revision/hash -> 获取 supervisor directory lock/新 epoch -> 重采样实况 -> 将无存活进程的活动 Worker 标为 CRASHED -> reclaim stale lease -> 检查已存在 commit/patch-id/RESULT -> 重建 queue/eligible -> 恢复 command deadline 或将不可确认命令分类 UNKNOWN -> 进入 `RECONCILING` -> 继续 DAG。若旧 A0 仍活跃，新实例写 `CONFLICT` 后退出，不要求用户干预，也不得双协调。

### 5.11 幂等性与副作用防重

- 每次 dispatch、command、recovery、integration、transition 都有确定 `operation_id`；同 ID 已成功则重放 no-op，失败重放沿用原 attempt/budget。
- 提交前查 `Task-ID` trailer、tree SHA 和 patch-id；commit 已存在则复用，禁止重复 commit。A0 集成前查 candidate ancestor/patch-id，已包含则只补 ledger。
- migration 只允许本 run scratch DB；执行前核对 reservation、migration table 和 candidate SHA。已应用同 hash 则 no-op；同编号异 hash 立即 HARD_STOP。任何不确定 corruption 不自动 drop 数据库。
- command evidence 只有在 `candidate_sha + command + cwd + env_allowlist + fixture_hash` 全同且 artifact hash-valid 时可复用；否则重跑对应最小门。
- API/fixture 创建必须携带 run-scoped idempotency key；synthetic identity 先查后建。计划禁止对外消息、部署和第三方通知，因此恢复流程也不得发送它们。
- reconcile/recovery 不能 reset/clean/stash、删除不明 worktree、覆盖 dirty file 或重写已有 commit；崩溃 worktree先只读封存，确认归属后再恢复。

### 5.12 卡片结果与 Acceptance 接线

Worker 的 `RESULT.json` 只能把 Task 推到 `VERIFYING`；A0/F1 校验 commit、exclusive paths、focused tests 和 evidence 后才可 `PASSED`。Task PASS 解锁 DAG，里程碑 PASS 只由第 13 节 Acceptance 集合和 L2/L3 门计算。任何状态机恢复都不得直接伪造 Acceptance PASS；失效的 candidate SHA 必须使相关验收回到待验证。

卡片连续失败耗尽预算后，A0 写 `BLOCKED_<FAULT_CLASS>`、unlock oracle 和影响范围，释放席位并继续所有其他 eligible 卡。若其依赖后来由另一个合法卡/环境恢复满足，reconcile 可自动 `BLOCKED -> ELIGIBLE`；`SECURITY_VIOLATION/PROTECTED_WIP/SCOPE_EXPANSION` 不得无新事实自动重开。

## 6. 依赖 DAG 与波次

`control/run.json.candidate_profile` 只有两种：默认 `FULL_PRODUCT`（本计划最终目标，选择 M1-M4 且 `require_internal_api=true`）和阶段性 `MILESTONE_CHECKPOINT`（只冻结 `selected_milestones`，Internal 由 `require_internal_api` 显式选择）。A0 可在 FULL_PRODUCT 运行过程中生成 M1/M2/M3 的 interim checkpoint，但不得因此结束剩余 DAG；单里程碑本地候选从不被未选择的 Internal 卡阻塞。

```text
W0 CSX-00 baseline/handoff ─┬─> W1 TEST-00 impact manifest
                            ├─> W1 CSX-01 contract correction
                            ├─> W1 ENT-00 current-gap matrix
                            └─> W1 INT-00 current Internal contract matrix

CS-BARRIER  = CSX-00 + TEST-00 + CSX-01
ENT-BARRIER = CSX-00 + TEST-00 + ENT-00
INT-BARRIER = CSX-00 + TEST-00 + INT-00

CS-BARRIER  ─> CS-BE-01 / CS-DEC-01 / CS-DEC-02 / CS-DEC-03
ENT-BARRIER ─> ENT-FND-01 / ENT-FND-02 / ENT-BE-01
INT-BARRIER ─> INT-BE-01 ─┬─> INT-BE-02 ─┬─> INT-BE-05 ─> INT-BE-06 ─> INT-INT-01
                          ├─> INT-BE-03 ─┘
                          └─> INT-BE-04 ──────────────────┘

CS-BE-01 ─┬─> CS-WEB-01 ─> CS-WEB-02 ─┐
          ├─> CS-WGT-01 ──────────────┴─> CS-INT-01
          └─> CS-APP-01
CS-INT-01 + CS-APP-01 focused closure ─> M1

CS-BE-01 ─> CS-BE-02 ─> CS-WEB-03
CS-DEC-01 ─> CS-BE-03 ─> CS-WEB-04
CS-DEC-02 ─> CS-BE-04 ─┬─> CS-WEB-05
                       └─> CS-APP-02
CS-DEC-02 ─> CS-BE-05 ─┬─> CS-WEB-05
                       └─> CS-WGT-02
CS-QUEUE-* + CS-CONTEXT-* + CS-RUNTIME-* ─> CS-INT-02 (M2)

ENT-FND-01 ─┬─> ENT-ADM-01
            ├─> ENT-ADM-02 ─> ENT-ADM-03
            └─> ENT-ADM-04
ENT-FND-02 ─┬─> ENT-APP-01
            ├─> ENT-APP-02
            ├─> ENT-APP-03
            └─> ENT-APP-04 ─> ENT-APP-05 / ENT-APP-06
ENT-BE-01 + ENT-ADM-01..04 ─> ENT-UX-01 ─┐
ENT-APP-01..06 ─> ENT-UX-02 ─┴─> ENT-INT-01 (M3)

CS-DEC-03 ─> CS-BE-06 ─┬─> CS-ADM-01
                       └─> CS-APP-03
CS-BARRIER ─> CS-BE-07 ─> CS-ADM-02             (M4)
M1 + M2 + M4 ─> CS-INT-03 production closure

selected milestones + (INT-INT-01 only when require_internal_api=true) -> W6 integration -> F1 -> profile freeze
M1 + M2 + M3 + M4 + CS-INT-03 + INT-INT-01 + DEVICE-01/02 -> PRODUCTION_GRADE_LOCAL_CANDIDATE_PASS
```

W2/W3 Customer Service 与 W4 Enterprise/Internal 可跨仓并行；同仓共享热点按 lease 串行。M4 不阻塞 M1/M2/M3 的里程碑报告。

`control/task-queue.json` 只有在对应 barrier 的所有 RESULT 均为 PASS 时，才可把实现卡从 `PENDING` 改为 `ELIGIBLE`。没有 `TEST-00` 的 path-to-test 映射、没有 run-scoped scratch config 或仍指向默认 `imboy_v1` 时，任何 Backend 实现卡都不得启动。

## 7. W0-W1：基线、裁决与地基

### [ ] CSX-00 稳定基线与 handoff

- **Owner**：A0，只读。
- **动作**：校验 plan hash；先完成 5.10 的 plan lock/run discovery/supervisor epoch；写入默认 `FULL_PRODUCT` profile；完整读取未跟踪设计目录；将其文件清单、逐文件 SHA256 和只读快照存入 `RUN_ROOT/input-design-snapshot/`；两次采样三仓 HEAD/status/worktrees/branches、active runs、进程、端口、DB、migration、设备与 WIP，间隔至少 10 秒；创建唯一 scratch DB `imboy_csagent_<runid>` 和 `RUN_ROOT/config/backend-scratch.config`，并通过结构化解析机械断言其中 `pg_conf` 的数据库名/端口与 ledger 一致、RUN_DB 非空且不等于默认 `imboy_v1`。
- **保护**：未跟踪设计目录允许只读参考但不得改动/提交；记录其状态/hash，并保护 `imboyapp/macos/Podfile.lock`；不得 reset/clean/stash/覆盖/吸收。
- **启动门**：发现 active coordinator、shared DB/port/device 或共享路径 lease 时，只阻塞冲突卡；独立只读和隔离 worktree 卡可继续。
- **Acceptance `CSX-00`**：plan/supervisor 原子锁、epoch fencing、candidate profile、三仓 BASE_SHA、foreign WIP、资源 ownership、run-scoped worktree/branch、scratch PG/动态端口和 EUnit scratch config 均有证据；对 scratch DB 完成 migration head/连接/ownership 探针。

### [ ] CSX-01 Customer Service 契约勘误

- **Owner**：A1，只读证据写入 RUN_ROOT。
- **冻结契约**：历史消息必须投影 `assets:[{id,mime,size_bytes,file_name,status}]`；纯文本为 `assets=[]`；禁止 `object_key/upload_url/token`。
- **核对**：runtime route、Handler、Application、SQL、Web/Widget/Flutter parser、现有测试假载荷的集合差异。
- **Acceptance `CSX-01`**：输出逐字段 producer/consumer matrix；附件断链均有当前源码行号；订单引用保持 `BLOCKED_PRODUCT_DECISION`。

### [ ] ENT-00 Enterprise current-gap matrix

- **Owner**：A0；A4/A6 只读提供各自 repo 证据。
- **动作**：把旧 15 卡重新标为 `DONE/PARTIAL/MISSING/INVALID_ASSUMPTION`；列出现有 shared primitives、路由、页面、测试和原型偏差；同时建立 Flutter owner/admin/member 的组织、成员、邀请、部门、群、频道、Workspace/工具 `capability -> API -> UI -> permission -> local test -> device journey` 矩阵。
- **Acceptance `ENT-00`**：任何后继卡必须指向一个 `PARTIAL/MISSING` gap；`DONE` 卡自动取消，不允许“为了统一”重写；App 每项生产能力都有成功、拒绝、刷新恢复与真机 Oracle，不得以“页面已存在”代替业务闭环。

### [ ] INT-00 Internal API current-contract matrix

- **Owner**：A1 + F1，只读；证据写入 RUN_ROOT。
- **动作**：从 runtime routes、`api/openapi-internal.yaml`、bundle、Postman 和 scope registry 分别提取 method/path/operation/scope；在本 run 生成 `control/internal-api-manifest.yaml`，头部绑定 `RUN_ID/plan_sha/backend_candidate_sha/generated_at`，路由逐项来自 current source，不得复制历史 RUN manifest；冻结 V21 current contract 为 `31 operations / 14 fixed scopes / 4 Human Directory APIs`，并建立 auth/Grant/Workspace boundary/IDOR/cursor/idempotency/rate/audit/error/schema 的 test mapping。
- **边界**：这不是新增 API 设计卡；集合不足或不一致先判 current regression。确需新增 endpoint/scope/schema 才能满足计划外业务时，结束为 `BLOCKED_SCOPE_EXPANSION`。
- **Acceptance `INT-API-00`**：四套集合来源、逐项 producer/consumer/test、已知差异和修复 owner 完整；OpenAPI bundle `$ref=0`；没有 wildcard scope；每个 mutation 均映射幂等、审计和权限负例。

### [ ] TEST-00 Test impact manifest

- **Owner**：F1 只读，A0 写 control 文件。
- **产物**：`changed_path_glob -> L0 tests -> L1 bundle -> escalation trigger -> journey`。
- **Acceptance `TEST-00`**：所有实现卡都有最少一个能因回归而失败的 test/oracle；没有卡以全仓测试作为唯一 L0 门。

### [ ] ENT-FND-01 Admin shared capability gap

- **Owner**：A4，独立 Admin worktree。
- **动作**：复用 `DataTable/FilterBar/BatchActionBar/EntityDrawer`；只补经 ENT-00 证明缺失的 Profile 分区、治理动作/危险确认或列持久化能力。
- **禁止**：创建与 shared primitives 平行的 `modules/enterprise/components` 大套件；dev-only Storybook 路由不是交付要求。
- **Acceptance `ENT-FND-01`**：至少一个真实 Organization 页面消费新增能力；八态/持久化/键盘测试通过；现有消费者无 API 破坏。

### [ ] ENT-FND-02 Flutter permission/error 兼容收敛

- **Owner**：A6，独立 Flutter worktree。
- **动作**：复用现有 `OrgStateView.permissionDenied` 和错误分类；只抽真实后继页面共同需要的最小 adapter/render helper。
- **禁止**：新建 member/group/channel/department/invitation 大一统模型；删除旧模型；修改生成 i18n 以外的保留区。
- **Acceptance `ENT-FND-02`**：Empty/Loading/Error/Permission 四态可单测；新页面不自行解析错误字符串；现有 Organization 行为不变。

## 8. W2：M1 附件真闭环

### [ ] CS-BE-01 历史消息资产投影

- **Owner/paths**：A1；enterprise message read adapter/application/tests 与必要的 Customer Service projection。
- **实现**：一次分页查询同时返回绑定资产白名单；避免逐消息 N+1；保持消息排序、cursor、托管加密解密和租户边界。
- **安全**：仅 active/允许占位状态；不返回存储 URL、object key、上传凭证或跨 Org/Workspace 资产。
- **L0**：`eb_message_app_tests`、`eb06_readwrite_entry_tests`、新增 PG projection test；若触及 Facade/route，再跑 arch/contract/security。
- **Acceptance `CS-ATT-01`**：纯文本 `assets=[]`；附件消息刷新后字段完整；跨租户 403/404 且结果集无泄漏；查询数不随消息数线性增加。
- **Commit**：独立 Backend commit。

### [ ] CS-WEB-01 Seat 授权下载与预览

- **Depends**：CS-BE-01 合同冻结，可先用 contract fixture 开发。
- **Owner/paths**：A2；Seat API/client/contract/session view。
- **实现**：删除虚构 CS content path；使用 Seat Bearer fetch 真实 enterprise asset content；Blob URL 生命周期可回收；图片内联预览，文件安全下载。
- **L0**：SeatApiClient、contract、workbench API、SessionView 精确测试。
- **Acceptance `CS-ATT-02`**：无 Cookie；header 有 Seat Bearer；DOM/href/log 无 token/object key/upload URL；401/403/404、取消和 unmount 均清理资源。

### [ ] CS-WEB-02 Web Seat 单文件发送

- **Depends**：CS-WEB-01；复用现有 Backend 资产协议。
- **Owner/paths**：A2；composer/API/hooks/精确测试。
- **实现**：presign -> PUT -> confirm -> send `asset_ids`；正文可空但正文和附件不能同时空；失败保留内容并可用同一 `client_msg_id` 重试。
- **Acceptance `CS-ATT-03`**：上传/确认/发送顺序被测试钉死；失败不重复创建逻辑消息；文件类型/大小前端提示不替代服务端校验；键盘/屏幕阅读器可用。

### [ ] CS-WGT-01 Widget 历史附件与图片体验

- **Depends**：CS-BE-01。
- **Owner/paths**：A3；Widget contract/controller/api/ui/tests。
- **实现**：历史消息保留附件；内容经 visit-token 授权 fetch；图片缩略预览，其他文件项；保留当前单附件 composer。
- **L0**：widget API/controller/a11y 精确测试，随后 `bun run build:widget && bun run verify:widget` 作为 L1。
- **Acceptance `CS-ATT-04`**：发附件后刷新、SSE 重连、翻历史仍存在；token/secret/object key 不进 DOM、日志、manifest；失败态可重试。

### [ ] CS-APP-01 Flutter 历史附件恢复与最小渲染复用

- **Depends**：CS-BE-01。
- **Owner/paths**：A5；Customer Service API/flow/session page/tests。
- **实现**：解析真实 `assets`；图片与普通文件分型；复用授权资源获取和现有最小 renderer，不导入个人消息 store/E2EE。
- **L0**：`cs_api_contract_test`、`cs_session_flow_test`、`cs_session_page_test` 与静态 import boundary test。
- **Acceptance `CS-ATT-05`**：刷新后附件仍在；图片可预览、文件可打开；URL 授权合规；暗色/横竖屏/错误态不溢出；个人聊天行为零变化。

### [ ] CS-INT-01 Widget/Backend/Web Seat 真实附件闭环

- **Depends**：CS-BE-01、CS-WEB-02、CS-WGT-01；不等待 Flutter 真机。
- **Owner**：F1/A0，只读验收；使用 scratch PG、synthetic Org/Seat/Visitor、loopback 服务。
- **Journey**：Widget 发图片 -> Web Seat 看见 -> 刷新仍在 -> Seat Bearer 授权查看；Web Seat 反向发送 -> Widget 看见并刷新仍在；纯文本回归；跨租户和撤权负例。Flutter 附件消费不由 Playwright 代证，统一进入 DEVICE-01/02。
- **Acceptance `CS-ATT-06`**：真实 Cowboy/DB，不允许 `page.route`、静态 JSON 或 mock API；至少一个 DB Oracle、一个 HTTP Oracle、一个 Widget DOM Oracle、一个 Web Seat DOM Oracle；secret scan=0。
- **里程碑**：CS-ATT-01..06 全 PASS 即本地 `M1_ATTACHMENT_CLOSURE_PASS`；它证明 Backend/Widget/Web Seat 真链和 Flutter focused closure，Flutter 真机附件链仍必须由 DEVICE-01/02 才能进入生产级候选。

## 9. W3：M2 Agent Workspace 生产力

### [ ] CS-BE-02 队列摘要与等待时长

- **Owner**：A1。
- **实现**：在授权 Seat list projection 返回脱敏 `last_message.preview` 和权威 `waiting_seconds`；preview 由已解密正文安全截断，附件-only 显示类型占位。
- **禁止**：在公开 Widget bootstrap、SSE 或未授权平台列表暴露正文；不得增加轮询型 N+1。
- **Acceptance `CS-QUEUE-01`**：queued/active/closed 排序稳定；Unicode 截断正确；附件/空消息占位确定；授权负例和查询计划有证据。

### [ ] CS-WEB-03 队列 triage

- **Depends**：CS-BE-02。
- **Owner**：A2。
- **实现**：显示摘要、等待时长、未读占位；按现有三状态筛选/搜索，所有计数以服务端为准；响应式布局不改变列表宽度。
- **Acceptance `CS-QUEUE-02`**：筛选/切换/加载更多不丢 selection；等待时长不会由客户端时钟漂移制造负值；Empty/Loading/Error/Permission 完整。

### [ ] CS-DEC-01 客户上下文权限冻结

- **Owner**：A1 + F1，只读设计证据。
- **默认决策**：只读掩码 profile、来源、first/last seen、同 Org 历史客服会话、允许读取的 note；电话/邮箱/外部 ID 永不投影；note 写入不在本卡。
- **Acceptance `CS-CONTEXT-00`**：字段白名单、principal、API path、审计、跨 Org 负例、数据保留和 UI 空态逐项冻结。

### [ ] CS-BE-03 客户上下文读模型

- **Depends**：CS-DEC-01。
- **Owner**：A1。
- **实现**：由 session/org/workspace/contact 事实派生上下文；使用 Seat `conversation.read` 与 session ownership 授权，不给 `customer_service` 粗暴扩展 sales contact scope。
- **Acceptance `CS-CONTEXT-01`**：一个只读 endpoint/既有 detail 投影的最小方案；白名单外字段不存在；转接后新 Seat 可读，撤权立即拒绝；读操作无写副作用。

### [ ] CS-WEB-04 客户上下文面板

- **Depends**：CS-BE-03。
- **Owner**：A2。
- **实现**：右栏显示掩码资料、历史会话和备注；窄屏变 Drawer；不得展示未授权 PII。
- **Acceptance `CS-CONTEXT-02`**：切换 session 不显示上一客户陈旧数据；并发请求 stale response 被拒；loading/error/permission/empty 有独立状态。

### [ ] CS-DEC-02 未读/presence 数据语义

- **Owner**：A1 + F1。
- **冻结**：read cursor 绑定 session + assignment/identity；转接时从 transfer 边界开始计新未读。presence heartbeat=30s、TTL=90s；manual away 优先；capacity 满派生 busy；enabled=false 永远不可用。
- **Acceptance `CS-RUNTIME-00`**：状态机、时钟注入、节点并发、重放/idempotency、转接与撤权语义形成测试表；迁移编号由 A0 reservation 后确定。

### [ ] CS-BE-04 Durable read cursor / unread

- **Depends**：CS-DEC-02。
- **Owner**：A1；需要独立 migration up/down。
- **实现**：ACK 单调前进、不可回退；未读由 cursor 与 message fact 计算；SSE 只提示刷新，不能当作已读。
- **Acceptance `CS-RUNTIME-01`**：重复/乱序 ACK 幂等；跨 Seat/Org 拒绝；transfer 前历史默认 0 unread；新消息精确增加；迁移回滚和 cycle test 通过。

### [ ] CS-BE-05 Seat presence 与派单

- **Depends**：CS-DEC-02。
- **Owner**：A1；需要独立 migration up/down，不能与 CS-BE-04 同时写 migration head。
- **实现**：持久/共享可见 heartbeat lease；`cs_dispatch` 只选择 enabled + online + capacity available；无在线 Seat 时保持 queued。
- **Acceptance `CS-RUNTIME-02`**：可注入时钟覆盖 TTL 边界；away/offline 不派单；busy 仍可查看已分配会话但不接新会话；多节点并发无双派。

### [ ] CS-WEB-05 Web 状态、未读与可用性

- **Depends**：CS-BE-04/05。
- **Owner**：A2。
- **实现**：状态 segmented control、未读 badge、聚焦/可见性驱动 ACK；网络断开不得误报 online。
- **Acceptance `CS-RUNTIME-03`**：刷新恢复；多标签页不倒退 cursor；403/seat disabled 降级只读；状态文本、颜色、图标均可访问。

### [ ] CS-APP-02 Flutter 状态与未读对齐

- **Depends**：CS-BE-04/05。
- **Owner**：A5。
- **实现**：与 Web 共用 wire 语义；App lifecycle 控制 heartbeat；进入会话后在权威展示完成时 ACK。
- **Acceptance `CS-RUNTIME-04`**：后台不保持假 online；resume 后重同步；转接/撤权/离线重连无陈旧未读；focused unit/widget tests 通过。

### [ ] CS-WGT-02 无在线 Seat 提示

- **Depends**：CS-BE-05。
- **Owner**：A3。
- **实现**：Widget 仍允许发普通消息并显示异步回复说明；不创建机器人/工单/留言新模型。
- **Acceptance `CS-RUNTIME-05`**：无在线 Seat 仍只创建一个 queued session；文案不承诺响应时限；Seat 上线后原会话可 claim。

## 10. W4：M3 Enterprise UX 真实缺口

### [ ] INT-BE-01 Internal API current-SHA 重资格

- **Depends**：INT-BARRIER；可与纯前端 Enterprise 卡并行，涉及 router/auth/migration 时独占 Backend 热点。
- **Owner/paths**：A1；API artifacts、runtime route/scope registry 与只读验证；无缺陷时不改代码。
- **执行**：`python3 api/flatten_internal.py --check`；`python3 scripts/check_enterprise_release_manifest.py --repo . --manifest "$RUN_ROOT/control/internal-api-manifest.yaml"`；`make contract-check ADMIN_DIR="$ADMIN_CANDIDATE" FLUTTER_DIR="$APP_CANDIDATE"`；`make migrations-check`；focused `enterprise_internal_wiring_http_tests`。禁止依赖校验脚本的历史默认 manifest 或共享相邻仓。
- **Acceptance `INT-API-01`**：manifest 与命令证据精确绑定 current Backend/Admin/Flutter candidate SHA；runtime/OpenAPI/Postman 均为 31 operations、25 paths、集合差 0；14 fixed scopes、wildcard=0；4 Human Directory GET；migration head 与本候选一致。计数漂移先 BLOCKED，不自动修改合同。

### [ ] INT-BE-02 31-operation 真实 HTTP conformance

- **Depends**：INT-BE-01。
- **Owner/paths**：A1；扩充现有 `enterprise_internal_wiring_http_tests` 及最小 test harness，业务实现仅在 before-failing test 证明回归时修改。
- **L0**：`enterprise_internal_wiring_http_tests`、`enterprise_internal_pg_tests`、`enterprise_internal_read_pg_tests`，使用 disposable PG + 真 Cowboy + synthetic Application credential/Grant。
- **Acceptance `INT-API-02A`**：`covered_operation_ids` 精确等于 31 个 runtime operation ID；每个 method/path 至少一个预期成功或业务可达响应，禁止只断言非 404；mutation 除合同明确单次使用项外均验证 Idempotency-Key；无 credential=401、缺 scope=403、跨 Org/未覆盖 Workspace 同体拒绝，envelope/operationId/schema 对齐。

### [ ] INT-BE-03 写操作审计政策与原子性

- **Depends**：INT-BE-01。
- **Owner/paths**：A1；现有 Internal mutation application/repo/audit tests，不新增通用审计平台。
- **L0**：受影响的 `enterprise_msg_asset_webhook_pg_tests`、`enterprise_webhook_governance_pg_tests`、`enterprise_admin_governance_pg_tests`、`enterprise_app_lifecycle_migration_pg_tests`。
- **Acceptance `INT-API-02B`**：冻结每个 mutation 的 `REQUIRED_AUDIT/REGISTERED_DEVIATION`；REQUIRED 成功后 audit count +1 且含 org/application/actor/action/resource/correlation；事务失败时业务行与 audit 均不落；幂等重放不重复审计或严格符合冻结政策。缺真实审计时必须在既有事务内修复，不能只补假测试。

### [ ] INT-BE-04 Human Directory 真实 HTTP/JWT/PG 闭环

- **Depends**：INT-BE-01。
- **Owner/paths**：A1；现有四个 Human Directory GET 的 router/application/repo/tests。
- **L0**：`organization_directory_app_tests`、`organization_directory_handler_tests`、`organization_directory_pg_tests`，并补最小真实 HTTP harness。
- **Acceptance `INT-API-02C`**：四接口均经真实 router+Human JWT+PG；active member 的 root/child department、members/me/search/cursor/limit 正确；suspended/removed/outsider fail-closed，跨 Org/missing 同体不泄漏；`member_count/department_ids` 与治理结果一致，查询数不随成员数线性增加，只读请求 DB 无写入。

### [ ] INT-BE-05 Application credential / Grant 生命周期

- **Depends**：INT-BE-02、INT-BE-03。
- **Owner/paths**：A1；现有 application/credential/grant 管理和 Internal auth 路径。
- **L0**：`enterprise_application_grant_pg_tests`、`enterprise_app_lifecycle_migration_pg_tests`、`enterprise_admin_governance_pg_tests`、`enterprise_internal_grant_pg_tests`、`enterprise_internal_pg_tests`。
- **Acceptance `INT-API-02D`**：创建 Application -> 签发 credential -> 授予精确 scope/workspace -> Internal 调用成功 -> rotate 后旧 secret 立即 401、新 secret 成功 -> revoke Grant/credential 后立即拒绝；zero Grant、expired/disabled/archived 全部 fail-closed；全链审计完整，secret 只展示一次且 DB 只存 digest。

### [ ] INT-BE-06 Internal API affected-domain 候选门

- **Depends**：INT-BE-02..05。
- **Owner**：F1 只读验证，A0 提供 frozen Backend candidate。
- **执行**：A0 先从最终候选重建并冻结本 run `internal-api-manifest.yaml`，再显式 `--manifest` 运行 release-manifest check；顺序逐个运行 Internal wiring/PG/read/cursor/SSO/Grant/rate/audit/application lifecycle/Human Directory 精确 suites，再按触发条件运行 compile/contract/arch/security/migrations；contract-check 显式传本 run Admin/Flutter candidate；禁止逗号 target 和重复全量 EUnit。
- **Acceptance `INT-API-03`**：每 suite `exit=0/test_count>0/failed=0/skipped=0`；31/14/4 未漂移；disposable DB 独立且未触碰保护库；全部日志/hash 绑定同一 Backend candidate SHA。

### [ ] ENT-ADM-01 成员与部门体验补差

- **Depends**：ENT-FND-01。
- **Owner**：A4。
- **范围**：成员复合筛选/列持久化/关系 Drawer；部门搜索高亮、成员数徽章、前端排序、左树右详情。现有 invite/suspend/restore/remove/owner transfer/CAS 不重写。
- **Acceptance `ENT-ADM-01`**：分页/筛选变化重置 page=1；批操作失败显示成功/失败数并可重试失败项；部门非法移动前端置灰且后端兜底；精确 component tests + 单个真实路由旅程。

### [ ] ENT-ADM-02 群治理页面簇收敛

- **Depends**：ENT-FND-01。
- **Owner**：A4，一次只租一个页面簇。
- **实现**：先选 2-3 个重复最高的群页面验证 schema/layout，再分批迁移其余页面；URL、权限、请求和危险动作语义不变。
- **Acceptance `ENT-ADM-02`**：每批逐 URL 冒烟；旧/新行为矩阵 diff=0；抽象减少真实重复而非增加 wrapper；每批独立 commit/revert。

### [ ] ENT-ADM-03 频道治理页面簇与通知覆盖

- **Depends**：ENT-ADM-02 稳定布局。
- **Owner**：A4；只改 Admin 页面簇。
- **实现**：迁移频道页面簇；消费 ENT-BE-01 已确认的现有通知契约，不在本卡修改 Backend。
- **禁止**：把 best-effort 通知宣传成持久审计时间线；若需要持久化则 `BLOCKED_SCOPE_EXPANSION`。
- **Acceptance `ENT-ADM-03`**：逐 URL 行为等价；通知动作矩阵在 UI 文案中不夸大；请求/响应契约 diff=0；精确页面 test。

### [ ] ENT-BE-01 频道治理通知覆盖

- **Depends**：ENT-BARRIER；可与 ENT-ADM-01/02 并行，但独占 Backend channel notify 路径。
- **Owner**：A1。
- **实现**：核查 rename/archive/restore/admin-role 等动作的现有 best-effort 系统通知覆盖，仅补缺失成功路径；不改请求/响应契约。
- **L0**：`channel_logic_notify_tests`、`channel_logic_archive_tests`、涉及角色路径时 `channel_logic_tests`。
- **Acceptance `ENT-BE-01`**：动作 x 通知 coverage matrix 完整；缺失路径有 before-failing EUnit；请求/响应契约 diff=0；独立 Backend commit/RESULT。

### [ ] ENT-ADM-04 IA、Workspace 测试与 Organization Profile 最小补差

- **Depends**：ENT-FND-01。
- **Owner**：A4，shared route/sidebar 由 A0 串行集成。
- **实现**：保留 current 九叶与 Customer Service；仅补缺失 WorkspaceDetail/ProjectList 测试；Organization Profile 只消费 ENT-FND-01 已有能力。
- **Acceptance `ENT-ADM-04`**：所有旧 URL 可达或有明确 redirect；权限不足菜单不渲染且直达 fail-closed；不新增一级导航。

### [ ] ENT-APP-01 Organization 聚合增量

- **Depends**：ENT-FND-02。
- **Owner**：A6。
- **实现**：在 Organization-owned route 增量组织企业头、成员/部门/群/频道/工具入口和管理员区；保留 ContactPage entry 与 `/enterprise` Enterprise Business。
- **Acceptance `ENT-APP-01`**：current organization 切换原子性不回归；member 看不到管理员动作；五域使用现有 API；四态完整；文案进 slang。

### [ ] ENT-APP-02 799 行群详情等价拆分

- **Depends**：ENT-FND-02。
- **Owner**：A6。
- **动作**：先写解散/退出/设置/改角色/归档恢复行为矩阵测试，再机械拆为 profile/member/settings/governance。
- **禁止**：顺手修改权限判断、路由、API 或群角色语义。
- **Acceptance `ENT-APP-02`**：权限条件源码/测试 diff 等价；文件拆分后无单文件超过 800 行；真机集中验收而非每子卡重复。

### [ ] ENT-APP-03 邀请与频道发现差距核验

- **Depends**：ENT-FND-02、ENT-00 capability matrix。
- **Owner**：A6，先只读。
- **决策**：现有 create/accept/reject/revoke/expired 和 channel discover/join 已满足目标则 `CANCEL_ALREADY_DONE`；只实现有源码/旅程证据的缺口。
- **Acceptance `ENT-APP-03`**：不能因旧计划写“Flutter 零 UI”而重复造页面；任何代码提交必须附 before-failing test。

### [ ] ENT-APP-04 组织、成员与持久化生产闭包

- **Depends**：ENT-FND-02、ENT-00 capability matrix；与 ENT-APP-01..03 同属 A6 Flutter lane，按文件 lease 串行。
- **Owner/paths**：A6；Organization API/model/store/cache、组织/成员/邀请页面与精确测试。
- **执行**：闭合组织 create/update/archive/restore、原子切换、成员分页/详情/changeRole/remove/transferOwner，以及 Backend 已有的 suspend/restore/offboard；邀请保持 ENT-APP-03 的现有契约。logout/account switch 清理、冷启动恢复和两阶段切换回滚必须有测试。
- **L0**：`organization_api_contract_test.dart`、`organization_models_parse_test.dart`、current organization store/cache、member detail 与新增 members 精确 widget tests。
- **Acceptance `ENT-APP-04`**：owner 不可被非法治理；suspend 后目标成员立即失去该 Org mine/directory/workspace，restore 后恢复，offboard 为终态；网络/CAS/重复提交不显示假成功；账号 A/B、Org A/B 缓存隔离；TSID 全程 lossless。

### [ ] ENT-APP-05 部门治理生产闭包

- **Depends**：ENT-APP-04 的权限/持久化语义稳定。
- **Owner/paths**：A6；Organization department API/page/controller/tests。
- **执行**：在现有 create root/child、rename、move、archive、addMember 基础上接通既有 API 的 removeMember、set/unsetAdmin；不新增部门实体或权限 scope。
- **L0**：`organization_api_contract_test.dart`、`human_directory_api_contract_test.dart`、`human_directory_controller_test.dart` 与新增 department widget test。
- **Acceptance `ENT-APP-05`**：完整 journey 为 create root/child -> rename -> move -> add/remove member -> set/unset admin -> archive；移动到自身/子孙失败；旧 version CAS 冲突刷新最新树；Human Directory 的 member_count/department_ids 与治理事实一致。

### [ ] ENT-APP-06 Workspace、群、频道与工作工具入口闭包

- **Depends**：ENT-APP-04；群详情等价拆分依赖 ENT-APP-02。
- **Owner/paths**：A6；OrganizationManage 与既有 Workspace/Group/Channel/Project/Task 路由 adapter 和精确测试。只复用现有页面/API，不复制业务模块。
- **执行**：按 Org owner/admin/member 与 Workspace owner 四身份，闭合现有 Workspace create/update/archive/restore/default 及成员治理深链、群 create/dissolve/成员角色/设置深链、频道 create/rename/archive/restore/admin/subscriber 深链；增加只展示当前 Org/Workspace 已授权现有 Project/Task 等工具的聚合入口。
- **Acceptance `ENT-APP-06`**：personal/其他 Org 资源不可见且直达由服务端拒绝；治理写后重新读取服务端事实；切换 Org 后资源/工具列表原子重建；任何需要新工具实体/API 的项 `BLOCKED_SCOPE_EXPANSION`。

### [ ] ENT-UX-01 Admin i18n、原型与无障碍收口

- **Depends**：ENT-ADM-01..04 均有终态 RESULT；`CANCEL_ALREADY_DONE` 可作为已核验前置，`BLOCKED` 不可。
- **Owner**：A4；Admin i18n/原型生成与共享文件保持单 owner。
- **实现**：Admin 企业域硬编码文案入键；更新 prototype delta ledger，使原型承认九叶、Customer Service 和 Enterprise Business 现状。
- **Acceptance `ENT-UX-01`**：无新硬编码关键文案；键盘/焦点/对比度/44px 点击目标/暗色模式检查；浏览器桌面与移动截图无重叠或溢出。

### [ ] ENT-UX-02 Flutter i18n 与无障碍收口

- **Depends**：ENT-APP-01..06 均有终态 RESULT；`CANCEL_ALREADY_DONE` 可作为已核验前置，`BLOCKED` 不可。
- **Owner**：A6；slang 源与生成文件单 owner。
- **实现**：新增/变更文案进入 slang；核对 token、暗色模式、语义标签、动态字号和 44pt 触达区。
- **Acceptance `ENT-UX-02`**：语言生成 diff 可复现；无手改生成文件；Android/iOS 目标尺寸 widget test 无溢出，真机视觉结果进入 DEVICE 卡。

## 11. W5：M4 运营治理

### [ ] CS-DEC-03 席位 entitlement 决策冻结

- **Depends**：CS-BARRIER。
- **Owner**：A1 + F1。
- **默认**：每 Org 一条 versioned entitlement，`seat_limit=null` 表示 unlimited；现有 Org 初始化为 unlimited；降低 limit 不停用存量 Seat；新增/恢复超额返回确定冲突。
- **Acceptance `CS-GOV-00`**：与 `max_concurrent` 区别清楚；并发 provision 使用事务/CAS；不引用 billing/payment；migration up/down 与兼容策略完整。

### [ ] CS-BE-06 席位 entitlement

- **Depends**：CS-DEC-03；A0 migration reservation。
- **Owner**：A1。
- **实现**：最小表/字段与 tenant/platform 治理 endpoint；所有创建、恢复、幂等 provisioning 路径统一检查。
- **Acceptance `CS-GOV-01`**：N 个并发请求不会超过 limit；幂等重放不重复计数；存量超额可读可减不可增；跨 Org/权限负例通过。

### [ ] CS-ADM-01 平台 Organization 客服摘要

- **Depends**：CS-BE-06。
- **Owner**：A4。
- **实现**：平台 Organization 页显示 seat used/limit/enabled/active sessions，并链接既有 Customer Service 治理面。
- **Acceptance `CS-GOV-02A`**：只使用 Admin Cookie `/api/adm` 域；TSID lossless；无权限直达 fail-closed；额度变化和并发冲突有明确反馈。

### [ ] CS-APP-03 Flutter 企业自助客服治理

- **Depends**：CS-BE-06。
- **Owner**：A6。
- **实现**：在已登录 Flutter Organization owner/admin 区消费既有 tenant API 允许的 Seat/Widget 治理能力。
- **禁止**：新增浏览器版企业租户门户或其登录会话；member 不渲染治理入口；Human JWT 不得调用 `/api/adm`，也不得混用平台 Admin Cookie 或 Seat JWT。
- **Acceptance `CS-GOV-02B`**：Human JWT 与租户权限负例通过；额度变化/超额冲突可解释；切换 Organization 后无陈旧治理数据；TSID lossless。

### [ ] CS-BE-07 按需客服统计

- **Depends**：CS-BARRIER。
- **Owner**：A1。
- **指标**：今日新会话、首次响应时间、关闭量、评分、当前 queued/active；时间范围和时区显式。
- **实现**：先以现表查询和受控 limit 完成；只有 `EXPLAIN ANALYZE` 证明需要才加索引，不建日报表/队列/缓存。
- **Acceptance `CS-GOV-03A`**：指标公式 fixture 可手算复核；空数据/跨日/未关闭/未评分语义确定；跨 Org 隔离；查询预算有证据。

### [ ] CS-ADM-02 客服运营统计视图

- **Depends**：CS-BE-07。
- **Owner**：A4。
- **实现**：在现有 Customer Service 平台治理面显示时间范围、关键指标与清晰空态；不创建通用分析仪表盘框架。
- **Acceptance `CS-GOV-03B`**：UI 数值与 fixture API 逐项相等；时区/范围可见；loading/error/empty/permission 完整；不以客户端重算覆盖服务端事实。

## 12. W6：集成、真实旅程与最终冻结

### 12.1 Lane checkpoint

- Backend：Customer Service/Enterprise message affected bundle；Internal API 仅顺序跑 INT-BE-06 定义的 affected-domain suites；迁移 cycle；若触及 route/auth/port，再跑 contract/arch/security。
- Agent Web/Widget：affected unit；typecheck/build；widget build/verify；指定真实后端 Playwright。
- Enterprise Admin：affected unit；typecheck/build；仅改动路由的 Playwright。
- Flutter：`test/customer_service`、`test/organization` 与受影响 Group/Workspace/Channel/Tools 精确 tests；模块 analyze。

### 12.2 真实旅程

| Journey | Oracle |
|---|---|
| J-CS-01 Widget 发图 -> Web Seat | DB asset binding + HTTP history + DOM image + refresh persistence |
| J-CS-02 Web Seat 发附件 -> Widget | presign/confirm/message idempotency + Widget reload |
| J-CS-03 Web/Flutter 未读与转接 | cursor DB + queue count + transfer boundary |
| J-CS-04 presence/离线 | heartbeat/TTL DB + dispatch target + Widget queued state |
| J-CS-05 客户上下文 | 白名单字段 + cross-org denial + stale response rejection |
| J-CS-06 客服生产闭环 | Widget bootstrap -> 排队/派单 -> 双向消息/附件 -> 未读/转接 -> 结束/评价 -> entitlement/统计 |
| J-ENT-01 Admin 成员/部门 | real Backend filter/CAS/permission |
| J-ENT-02 Admin 群/频道 | URL 等价 + action side effects + permission |
| J-ENT-03 Flutter Organization | 真机组织创建/切换/设置、成员角色/Owner 转移、邀请、部门生命周期、五域入口与权限 |
| J-ENT-04 Flutter Group | 真机解散/退出/设置/角色/归档恢复行为等价 |
| J-INT-01 Internal API | 31 operation real HTTP+PG、14 scope、4 Human Directory、auth/Grant/boundary/IDOR/idempotency/audit |

无设备时 J-ENT-03/04 为 `BLOCKED_NO_DEVICE`，不能用模拟器或截图代替；不阻塞 Web/Backend 本地候选，但 `DEVICE_PASS=false`。

### [ ] CS-INT-02 M2 Agent Productivity 真实集成门

- **Depends**：CS-QUEUE-01..02、CS-CONTEXT-00..02、CS-RUNTIME-00..05 全部 PASS。
- **Owner**：F1 只读验收，A0 管理 scratch runtime。
- **执行**：新增 `a08-agent-productivity-real.spec.ts`，覆盖 J-CS-03..05 的 Backend/DB/Web 部分；Flutter 真机部分进入 DEVICE-01/02。
- **Acceptance `CS-INT-02`**：真实 Cowboy + scratch DB；未读/transfer cursor、presence/dispatch/离线、context 白名单/跨 Org/stale response 均有 DB+HTTP+UI Oracle；无 mock route。

### [ ] ENT-INT-01 M3 Enterprise UX 真实浏览器门

- **Depends**：ENT-UX-01/02 PASS；若某实现卡为 `CANCEL_ALREADY_DONE`，必须有 current-main 复核证据。
- **Owner**：F1 只读验收，A0 管理 runtime。
- **执行**：新增或扩展一个 current-main Enterprise UX Playwright spec，覆盖 J-ENT-01/02；J-ENT-03/04 进入 DEVICE-01/02。
- **Acceptance `ENT-INT-01`**：真实 Backend 的成员/部门 filter/CAS/permission、群/频道 URL/动作副作用/权限全部通过；无 `page.route`、静态 JSON 或 mock API。

### [ ] CS-INT-03 Customer Service 生产业务闭环门

- **Depends**：`CS-ATT-01..06`、`CS-QUEUE-01..02`、`CS-CONTEXT-00..02`、`CS-RUNTIME-00..05`、`CS-GOV-00..03B` 全部 PASS；Flutter 部分仍由 DEVICE-01/02 代证。
- **Owner**：F1 只读验收，A0 管理 scratch runtime；复用前述 synthetic Org/Workspace/Seat/Visitor，不创建第二套 fixture。
- **执行**：一次 J-CS-06：合法 Widget bootstrap -> 无在线 Seat 排队 -> Seat heartbeat 上线/容量派单 -> claim -> 双向文本与附件/历史刷新 -> read cursor/unread -> transfer boundary -> 客户上下文白名单 -> close -> visitor rating -> 平台/企业治理读取 -> seat entitlement 冲突 -> 统计与手算 fixture 一致；同步跑撤权、跨租户、重复请求和 secret 负例。
- **Acceptance `CS-PROD-01`**：每一阶段至少有 DB 状态、HTTP 合同和对应 Web UI Oracle；所有 mutation 的重放结果确定且无重复逻辑消息/席位/评价；统计与事件事实一致；Browser 无 mock route，Flutter 不由浏览器冒充；失败点可从已有 idempotency key 续跑。

### [ ] INT-INT-01 Internal API 真实生产合同门

- **Depends**：INT-BE-06 PASS，Backend candidate SHA 冻结。
- **Owner**：F1 只读验收，A0 管理 scratch PG/loopback Cowboy 和 synthetic application/grant/organization/workspace。
- **执行**：从 runtime/OpenAPI/Postman 共同集合生成确定性 31-operation matrix，在真实 Cowboy + scratch PG 执行合法主链和最小负例；复用现有 HTTP/PG test harness，不引入新的通用 runner/dependency。
- **Acceptance `INT-API-04`**：31 runtime IDs/OpenAPI operation IDs/Postman requests 一一对应，14 fixed scopes 无 wildcard，4 Human Directory 读接口完整；合法请求的 DB/audit 副作用正确；invalid/expired credential、disabled app/org、origin mismatch、missing scope、revoked/expired Grant、workspace/organization IDOR、cursor tamper、idempotency digest conflict、rate limit 均 fail-closed 且错误信封一致；response/schema set difference=0。

### [ ] DEVICE-01 Android 集中真机门

- **Depends**：所选 `CS-APP-01/02/03`、`ENT-APP-01..06` 的 candidate SHA 已冻结并取得合法 Android 设备租约。
- **Owner**：F1 只读验收；A0 不得把模拟器或 macOS 结果改写为真机。
- **执行**：运行新增 Customer Service 附件/生产力测试和专属 `integration_test/organization_management_e2e_test.dart`，覆盖 J-ENT-03/04；使用 owner/member 两个 synthetic 账号与真实 Backend/PG，记录 device id、OS、app SHA/build hash、fixture。
- **Acceptance `DEVICE-01`**：Android 完成组织创建/切换、邀请双账号、成员 suspend/restore/offboard、部门治理、Workspace/群/频道治理、工具进入、杀进程冷启动和断网恢复；Customer Service 所选旅程同时通过。无设备则 `BLOCKED_NO_DEVICE`，本地候选可独立报告但 `DEVICE_PASS=false`。

### [ ] DEVICE-02 iOS 集中真机门

- **Depends**：所选 `CS-APP-01/02/03`、`ENT-APP-01..06` 的 candidate SHA 已冻结并取得合法 iOS 设备租约。
- **Owner**：F1 只读验收；A0 不得改动 `ios/*` 或把模拟器/macOS 结果改写为真机。
- **执行**：运行与 DEVICE-01 相同的 Customer Service 和 Organization management journeys；单独记录 device id、OS、app SHA/build hash、fixture。
- **Acceptance `DEVICE-02`**：iOS 完成与 Android 相同的组织/客服生产闭环；无设备则 `BLOCKED_NO_DEVICE`，不得沿用 Android 结果。

### 12.3 Final freeze

1. A0 只集成已 review 的独立本地 commits 到 run-scoped candidate branches。
2. 冻结三仓 SHA 到 `pre-test-candidate-manifest`，之后禁止隐式生成/格式化改变 SHA。
3. L3 每仓最多跑一次。失败后只修复根因并重跑受影响仓的 L3，不重跑未变化仓。
4. F1 校验 git diff、依赖闭包、test/oracle counts、skip、日志 hash、secret scan、真实后端标记、设备事实。
5. 生成 FINAL；共享 main 保持不写，记录 `MERGE_TO_MAIN=NOT_PERFORMED`。

## 13. Acceptance Ledger

| ID | 阻塞里程碑 | 验收 |
|---|---:|---|
| CSX-00 | 全部 | baseline/handoff/worktree/resource/WIP 保护成立 |
| CSX-01 | M1 | 附件 producer/consumer 契约冻结且断链证据完整 |
| TEST-00 | 全部 | test impact manifest 覆盖全部实现卡 |
| CS-ATT-01..06 | M1 | Backend/Widget/Web Seat 真实附件闭环，Flutter focused closure、安全负例和刷新持久性通过；Flutter 真链由 DEVICE-01/02 单独代证 |
| CS-QUEUE-01..02 | M2 | 队列摘要/等待/triage 通过 |
| CS-CONTEXT-00..02 | M2 | 客户上下文白名单、后端与 UI 通过 |
| CS-RUNTIME-00..05 | M2 | read cursor、unread、presence、Web/App/Widget 通过 |
| CS-INT-02 | M2 | J-CS-03..05 的真实 Backend/DB/Web 集成门通过 |
| CS-PROD-01 | Product closure | J-CS-06 客服全生命周期、治理、entitlement 与统计真实闭环通过 |
| ENT-00 | M3 | current gap matrix 完整，无重复实施 DONE 能力 |
| ENT-FND-01..02 | M3 | 最小共享能力与兼容层通过 |
| ENT-BE-01 | M3 | 频道治理 best-effort 通知覆盖通过 |
| ENT-ADM-01..04 | M3 | Admin current gaps 与真实路由通过 |
| ENT-APP-01..06 | M3 | Flutter 企业聚合、组织/成员/部门/群/频道/Workspace/工具生产管理闭环通过 |
| ENT-UX-01..02 | M3 | Admin/Flutter i18n/a11y/prototype delta 通过 |
| ENT-INT-01 | M3 | J-ENT-01..02 真实 Backend 浏览器门通过 |
| INT-API-00 | Internal | current 31/14/4 合同与 test mapping 冻结 |
| INT-API-01 | Internal | current candidate 的 runtime/OpenAPI/Postman/scope/directory/migration 计数一致 |
| INT-API-02A..02D | Internal | 31-op HTTP、审计原子性、Human Directory、credential/Grant 生命周期通过 |
| INT-API-03 | Internal | affected-domain suites 与静态门绑定同一 Backend candidate SHA |
| INT-API-04 | Internal | J-INT-01 真实 Cowboy+PG 生产合同门通过 |
| CS-GOV-00..01 | M4 | entitlement 决策、事务边界与兼容迁移通过 |
| CS-GOV-02A | M4 | 平台 Organization 客服摘要通过 |
| CS-GOV-02B | M4 | Flutter 企业自助客服治理通过 |
| CS-GOV-03A | M4 | 后端按需统计正确性、隔离与查询预算通过 |
| CS-GOV-03B | M4 | Admin 运营统计视图与服务端事实一致 |
| FINAL-01 | Candidate | 三仓 frozen SHA 与 L3 结果、证据 hash 一致 |
| DEVICE-01 | Device | Android 真机改动旅程通过 |
| DEVICE-02 | Device | iOS 真机改动旅程通过 |

里程碑状态：

```text
M1_ATTACHMENT_CLOSURE_PASS
M2_AGENT_PRODUCTIVITY_PASS
M3_ENTERPRISE_UX_PASS
M4_OPERATIONS_GOVERNANCE_PASS
INTERNAL_API_CLOSURE_PASS
LOCAL_CANDIDATE_PASS | LOCAL_CANDIDATE_PARTIAL_BASELINE_DEBT
DEVICE_PARTIAL | DEVICE_PASS | BLOCKED_NO_DEVICE
PRODUCTION_GRADE_LOCAL_CANDIDATE_PASS | PRODUCTION_GRADE_LOCAL_CANDIDATE_BLOCKED
EXTERNAL_NOT_EXECUTED
PRODUCTION_NOT_EXECUTED
MERGE_TO_MAIN_NOT_PERFORMED
RELEASE_NO_GO
```

`M2_AGENT_PRODUCTIVITY_PASS` 必须包含 `CS-INT-02=PASS`；`M3_ENTERPRISE_UX_PASS` 必须包含 `ENT-INT-01=PASS` 与 `ENT-APP-01..06`；`INTERNAL_API_CLOSURE_PASS` 必须包含 `INT-API-00..04`。`LOCAL_CANDIDATE_PASS` 要求所选里程碑全部 acceptance PASS、L3 无新增失败、候选工作树 clean、证据 hash 有效。未选择的后续里程碑写 `NOT_IN_THIS_CANDIDATE`，不得伪装 PASS。DEVICE-01/02 单独决定 `DEVICE_PASS`，不会被本地浏览器或 widget test 替代。

`PRODUCTION_GRADE_LOCAL_CANDIDATE_PASS` 是更严格的产品闭环状态，必须同时满足 M1-M4、`CS-PROD-01`、`M3_ENTERPRISE_UX_PASS`、`INTERNAL_API_CLOSURE_PASS`、`FINAL-01` 和 Android+iOS `DEVICE_PASS`，且 Backend/Flutter/API contract/migration head 证据绑定同一候选集合。任一项未执行或 BLOCKED，只能写 `PRODUCTION_GRADE_LOCAL_CANDIDATE_BLOCKED`；它仍不授权 merge、push、deploy、生产迁移或 release。

## 14. 停止、回滚与恢复

### 14.1 Hard STOP

只有以下情况允许全局 `HARD_STOP`：

- 继续执行必然覆盖/吸收用户 WIP，或无法安全区分 foreign worktree/branch/run/DB/port/device ownership。
- 已发生或继续会造成鉴权弱化、跨租户泄漏、E2EE 边界破坏、附件 secret/真实凭证暴露或安全断言被绕过。
- 需要或已经发生未授权破坏性操作、push/deploy/生产迁移、真实客户数据使用、第三方通知或发布。
- base SHA/候选历史发生不可恢复漂移，无法在不吸收 foreign WIP 的前提下重建。
- migration 编号同号异 hash、scratch/foreign DB ownership 不明、migration corruption 无法安全判定。
- `UNKNOWN` 状态继续自动恢复可能导致数据丢失、越权或不可逆副作用。

计划外订单、AI、计费、浏览器版企业租户门户/登录会话、持久治理时间线和其他 `SCOPE_EXPANSION` 只将对应卡置为 `BLOCKED_SCOPE_EXPANSION`；普通 Worker/test/code/environment/设备/依赖故障只做卡级恢复或 BLOCKED，均继续其他 eligible DAG。

### 14.2 卡级回滚

- 每卡一个或少量单一功能 commit；A0 可从 candidate revert，不改共享 main。
- DB 卡必须先证明 down migration 和 upgrade/down/upgrade cycle；回滚不得删除非本 run 数据。
- UI 卡保留旧路由和 API 兼容；不得靠永久 feature flag 维持两套实现。

### 14.3 Crash resume

恢复必须执行 5.10 的 A0 adopt，而不是新建 RUN。持久化读取顺序：

```text
FINAL/candidate-manifest.json（若 hash-valid）
control/pre-test-candidate-manifest.json
control/integration-ledger.json
control/acceptance-ledger.json
control/task-queue.json
control/leases.json + control/resources.json
control/workers.json + heartbeat/progress
control/supervisor.json + control/run.json
worker RESULT.json
raw evidence
conversation summary（最后）
```

列表前部是可验证结果，不代表可以跳过后部控制记录。A0 取得 supervisor lease 后重采样 Git、worktree、进程、端口、DB、migration、device、WIP 和 lease；用 operation/transition ID、revision、candidate SHA、commit/patch-id 和 evidence hash 对账。可重建 projection 自动修复；stale Worker 自动 CRASHED/reclaim/reassign；依赖和 eligible queue 自动重算。只有 14.1 风险才 HARD_STOP，不得因为普通 state drift 询问用户或选择“看起来最新”的记录。

## 15. 最终报告要求

最终报告必须包含：

1. `candidate_profile/selected_milestones/require_internal_api`，执行了哪些里程碑，哪些明确未进入候选。
2. 三仓 BASE_SHA/candidate SHA/branch/worktree/dirty state/集成 commits/current main tip。
3. 每个 Acceptance ID 的 `PASS/FAIL/BLOCKED/CANCEL_ALREADY_DONE/NOT_IN_THIS_CANDIDATE`。
4. L0/L1/L2/L3 命令、exit、test/oracle/skip、日志与 hash。
5. 附件四端、队列、上下文、未读、presence、客服 J-CS-06 全生命周期、Enterprise UX/App 管理闭环的真实 Oracle。
6. Internal API 的 31/14/4 集合差、31-operation HTTP、审计、Human Directory、credential/Grant 生命周期与证据 SHA。
7. 全量失败分类、每类 retry/reassign/rebuild 次数、budget 耗尽卡和基线 replay 证据；不得把既有红写成绿。
8. state transition/recovery ledger 摘要，证明 A0 adopt、Worker reclaim、queue 恢复和幂等去重可追溯。
9. Android/iOS 分开报告，说明设备、OS、app SHA/build hash、fixture；无设备就 BLOCKED。
10. protected WIP 与 shared main 未被修改的证明。
11. remaining blockers、最小解锁条件和下一张 eligible card。
12. `NO PUSH / NO DEPLOY / NO PRODUCTION / NO THIRD-PARTY EFFECT / RELEASE_NO_GO`。

## 16. 一次性 ZCODE Coordinator Prompt

将下面整段一次提交给 ZCODE。它是执行合同，不是要求再写一份方案。

```text
你是本任务唯一 A0 Supervisor 和 run-scoped candidate integrator。立即执行：
/Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-09-24-customer-service-agent-workspace-enterprise-ux-efficient-zcode-plan-v1.1.md

先校验相邻 .sha256，读取 Fast Execution Index、5.4-5.12、6、13、14 和当前要执行的任务卡；不要每轮重读全部计划或重新做产品研究。读取工作区根级及 imboy/imboyadmin/imboyapp 当前有效的 AGENTS.md/CLAUDE.md/DESIGN.md。docs/customer-service-v2/ 与 docs/enterprise-upgrade/ 是未跟踪但合法的只读设计输入：首次 W0 完整读取、记录清单和逐文件 SHA256、复制到 RUN_ROOT/input-design-snapshot/，之后 Worker 使用快照；不得修改、删除、移动或提交原目录。

启动时必须 adopt-or-create：以原子 mkdir 获取 `RUNS_ROOT/.locks/<PLAN_SHA>.lock`，然后扫描固定 `RUNS_ROOT/*/control/run.json`；严格按 5.10 处理全部同 plan hash RUN。存在可安全 adopt 的非终态 RUN 就从 control 持久化状态取得 `supervisor.lock`、递增 epoch并执行 re-attach/reconcile，禁止新建 RUN 或从 W0 重来；只有 terminal RUN 时只读返回 FINAL，除非启动输入显式含 NEW_RUN_AUTHORIZATION 及变化 fingerprint。旧 A0 heartbeat/PID/epoch 双采样仍活跃时，本实例写 CONFLICT 后退出，禁止双协调。历史 RUN 为零才创建首个 RUN，写 INIT，并创建 run-scoped 三仓 worktree/branch、唯一 scratch PostgreSQL 数据库 imboy_csagent_<runid>、动态 loopback ports 和 backend-scratch.config。共享 main 永远只读；保护全部 staged/unstaged/untracked/ignored WIP，特别是设计目录与 imboyapp/macos/Podfile.lock。不得 reset、clean、stash、覆盖、blanket stage、删除或接管 foreign branch/worktree/process/lease/DB/device。

你必须持续运行持久化 Supervisor Loop：每 10 秒 reconcile run/supervisor/worker/task/milestone 五层状态、heartbeat、last_progress、PID/PGID、command deadline、task/path/resource lease、queue/DAG、worktree/branch/candidate SHA、commit/patch-id、migration reservation、port/DB/device ownership和 Acceptance Ledger；昂贵 Git/DB 探针至少每 60 秒或事件触发。维护 run.json、supervisor.json、workers.json、resources.json、leases.json、task-queue.json、integration-ledger.json、acceptance-ledger.json、state-transitions.jsonl、recovery-ledger.jsonl、heartbeats、progress 和 commands。每次 transition 都记录 timestamp/from/to/reason/task/worker/attempt/candidate SHA/command/result/evidence；使用 revision drift check + 5.10 原子目录锁 + epoch fencing + operation_id 去重，重放不得重复 commit、migration、fixture 或副作用。

并发上限 8 个活跃 Agent，包含 A0，所以非协调器最多 7 个。本次默认 `candidate_profile=FULL_PRODUCT`、选择 M1-M4、`require_internal_api=true`；M1/M2/M3 通过时生成 interim checkpoint 后继续完整 DAG。先完成 CSX-00/TEST-00/CSX-01/ENT-00/INT-00：CS-BARRIER、ENT-BARRIER、INT-BARRIER 分别满足后才释放对应实现卡。按 DAG 机会式派发，以 Backend/Admin-Widget/Flutter repo lane 为主要并发单位；同一 candidate worktree 只有一个 writer，共享 router/migration/sidebar/shared UI/router/i18n 单 owner 串行 lease。优先 M1，同时推进无冲突 Enterprise/Internal 卡，随后 M2/M3/M4；里程碑可独立收口，但生产级候选必须满足计划 0.2 的三个产品闭环。

Worker heartbeat 每 15 秒，60 秒 dead，2 分钟未启动或 10 分钟无实质 progress 自动 watchdog。focused command 使用 5.8 deadline，超时只能终止 PID/PGID/start time/cwd/command_id 全部证明属于本 run 的 process group。Worker crash/stall 自动保存 diff/log/evidence、reclaim stale lease、扣持久化 retry budget并 reassign；环境故障只重建本 run worktree/cache/scratch DB/动态端口。严格按 5.9 分类 TRANSIENT、WORKER_CRASH、WORKER_STALL、ENVIRONMENT、TEST_FAILURE、CODE_FAILURE、LEASE_CONFLICT、SCOPE_EXPANSION、BASELINE_DRIFT、SECURITY_VIOLATION、PROTECTED_WIP、UNKNOWN；禁止无限 retry。预算耗尽只将该卡 BLOCKED并继续所有其他 ELIGIBLE 卡；blocker 最多六次有限 probe，耗尽后不再轮询，结算 FINAL 并停止 Supervisor。依赖恢复且 unlock oracle 成立时自动回 ELIGIBLE。普通故障不得询问用户或提前停止仍可执行的 Run。

只有 protected WIP 无法隔离、security violation、未授权破坏性/外向操作、不可恢复 base SHA 漂移、migration corruption/ownership 无法安全判断、UNKNOWN 且可能数据损失时才 HARD_STOP。订单/AI/计费、浏览器版企业租户门户/登录会话、持久治理时间线或其他范围扩张只 BLOCKED_SCOPE_EXPANSION 当前卡并继续 DAG。

严格执行 L0-L3：每卡只跑 changed-path lint、before-failing test 和最近邻 focused tests；lane 集成后跑一次 affected-domain；Backend+消费者合并后才跑指定真实旅程；最终冻结 SHA 后每仓最多一次 L3。禁止 Worker 跑全局 EUnit、全量 bun、全量 Flutter。每卡 RESULT 必含 TASK_ID、owner、exclusive paths、BASE_SHA、attempt、changed paths、tests、test/oracle/skip counts、evidence hashes、commit SHA。exit 0、HTTP 200、页面打开、截图、mock/static E2E、旧证据、零测试均不能单独 PASS。

真实门必须使用 Cowboy + scratch PG + synthetic identities：M1 经过 CS-INT-01，M2 经过 CS-INT-02，客服完整闭环经过 CS-INT-03，M3 经过 ENT-INT-01，Internal API 经过 INT-BE-01..06 与 INT-INT-01。Flutter 企业组织生产管理和客服旅程由 DEVICE-01 Android、DEVICE-02 iOS 分开证明，不得以模拟器、macOS、浏览器或另一平台代证。Internal 必须证明 current-SHA 的 31 operations/14 fixed scopes/4 Human Directory、真实 HTTP+PG、审计原子性和 credential/Grant 轮换撤销闭环。

使用命令级 Git 身份 leeyi <leeyisoft@qq.com> 创建授权范围内的独立本地任务提交；不得 merge 到共享 main，不得 push/PR/deploy/生产迁移/发布，不得使用生产凭证/真实客户数据，不得通知第三方。完成后由 F1 独立只读复核并生成 FINAL。最终分别报告 M1/M2/M3/M4、INTERNAL_API、LOCAL_CANDIDATE、DEVICE、PRODUCTION_GRADE_LOCAL_CANDIDATE、EXTERNAL、PRODUCTION、MERGE_TO_MAIN、RELEASE；即使生产级本地候选通过，也必须保持 EXTERNAL_NOT_EXECUTED、PRODUCTION_NOT_EXECUTED、MERGE_TO_MAIN_NOT_PERFORMED、RELEASE_NO_GO。
```
