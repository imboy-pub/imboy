# 客服坐席 V2 并行验收执行计划 / Seat Console Parallel Acceptance

**Goal / 目标：** 快速验收已实现的客服坐席系统，修复真正阻断项，完成原 V2 的全部67项本地候选验收；与全产品 Android/macOS 验收并行但不竞争共享资源。

**Architecture / 架构：** 复用现有 Backend/Admin 实现、Seat build、网关及 Playwright 用例；独立 worktree 和环境、批量业务旅程、一次冻结全局门。原计划要求和安全边界不因本补充合同降低。

**Tech Stack / 技术栈：** Erlang/OTP、PG18、React/Bun/Vite、nginx、真实 Chromium/Playwright。

## 1. 权威输入 / Inputs

原计划：`/Users/leeyi/project/imboy.pub/imboy/docs/architecture/2026-09-28-customer-service-seat-console-embed-mall-admin-implementation-plan-v2.md`。

SHA256：`580a1b16a17e61f42bb4982a82a4c4b6ce963ca71943f943d356d410608793fe`。启动重新核对文件及相邻 `.sha256`；不一致先对账，不盲沿用旧摘要。

精确 required 集合：SC-00 A01–A12；SC-BE A01–A10；SC-FE A01–A09；SC-BLD A01–A09；SC-OPS A01–A07；SC-E2E A01–A10；SC-INT A01–A05；SC-GATE A01–A05，共67。同目录 `2026-10-04-zcode-seat-console-acceptance-set.json` 提供机器集合，仍须从原计划重算比对。

读取根、Backend/Admin AGENTS.md 与引用规范；阅读 Admin `tests/e2e/customer-service-seat-embed/README.md`、Playwright config、harness、helpers、全部 specs。原67项不代表客服系统所有未来功能；本轮只称原V2通过，不称整个全产品计划完成。

现场发现：原计划摘要未变；现有 build/verify/pairing、snippet 浏览器部署用例、双坐席 CAS 用例均存在。历史 `.Codex/runs/seat-console-embed-20260928T101922Z-11258` 的账本/终审文件本次未找到，不能继承旧PASS；可定位其他实际留存证据，但不得重新手写历史运行结果。

已知优先核查：SC-BE-A05原要求 invalid/missing/revoked统一404，当前源码说明 malformed public ID先400；必须现场测试并按原合同修复，或提交范围明确的合同变更等待用户批准，不自行改合同。snippet须实际把生成值部署到真实宿主；双坐席当前主要证明顺序旧版本CAS拒绝，仍需独立真实同时竞争。

## 2. 与全产品线并行 / Parallel Isolation

客服线只操作 `imboy`、`imboyadmin`，不写 `imboyapp`。全产品线的 F12/F15可消费本轮证据，但不能直接将客服67项PASS转换为全产品设备/治理PASS。

客服协调者创建两仓独立本地 worktree，从启动时已核对 main 提交派生，使用唯一run分支；记录base/candidate/main及输入指纹。用户本请求允许本次并行工作树准备，不授权push或生产。不要把dirty main自动复制、stash、reset或clean。

独占 RUN_ROOT：`/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-10-04-seat-console-parallel-acceptance/evidence/<run_id>/`；实际应写在客服 Backend worktree的同一仓内相对路径。仅本run读写；不写全产品 `zcode-acceptance-20261004` 或其 continuation-state。

独占新PG容器/数据库、Backend Erlang node/cookie/runtime配置、nginx prefix/pid/TLS和临时目录、Admin/Seat构建目录、Chromium context、trace/report。申请端口时列候选后现场检查及登记租约，不把空闲观察当批准。

不能直接运行当前harness默认配置：它含 `imboy_pg18`、`sc153_e2e`、9801/18443/18080、固定组织/工作区，部分清理为全workspace UPDATE，部分oracle取最新行。必须证明隔离后的所有请求和SQL只触及本run，才允许EXECUTE。同一客服数据库内只运行一个有状态suite，`workers=1`；测试内部可对两个独立身份同步发claim，套件间不能并发污染。

如果端口/origin在helpers/template中硬编码，先最小参数化到一份effective-config，并用负例验证配置同源和污染拒绝；不是另造runner。公共build输出放独立Admin worktree并冻结，不与全产品主目录dist互相覆盖。

新环境启动、夹具/迁移/凭证及回收以明确批准范围为准；若已有批准精确覆盖本run可复用记录，否则先提交具体审阅清单，不默认为同意。禁生产/共享DB、真实客户数据、外部短信邮件推送、对象存储写入和生产部署。附件使用本run批准的隔离存储。秘密存仓外受限配置，日志不存token/密码/PII。

## 3. 所有权 / Ownership

允许最多3名实施者与1名只读终审，若资源不足则由单执行者顺序承担。每人必须知道存在其他协作者，不回退或吸收他人改动。

| 卡 | 独占职责和路径 | 依赖/禁止 |
|---|---|---|
| S0 协调/环境 | 本run control/ledger、两仓worktree和资源租约、独立runtime、Admin Seat harness/helpers/config | 不写全产品控制文件，不抢共享资源 |
| S1 Backend | `src/features/customer_service/**`相关Seat精确模块、`test/features/customer_service/**`及`test/api/seat_console_routes_tests.erl` | 不改非客服业务；共用router/Makefile只能提handoff由集成人应用 |
| S2 Admin/静态 | `src/modules/customer_service/api/seatConsoles*`、`pages/CsWidgetInstallationsPage*`、Seat entry/相关单测和build verifier | vite/package/全局App路由/nginx模板列共享变更清单，不能与全产品owner同时改 |
| S3 浏览器 | Seat embed specs/host fixtures；S0拥有helpers/config，S3通过handoff请求变更 | EXECUTE需SC-INT通过和候选产物冻结；与其他写者路径无重叠 |
| V 只读终审 | 核67-ID、候选/产物/真实oracle、恢复记录 | 不合并、不改源码/账本、不自签执行结论 |

人数不足时S2/S3同人；共享文件每时刻唯一owner。迁移已有不重新占号；核原reservation及真实现有migration，新增迁移必须执行原计划原子预留，不沿用历史“153空闲”。SC-00历史编号/旧WIP义务逐项对账，不能删除或凭当前无WIP直接编历史PASS。

## 4. 波次、命令与检查点 / Waves

### W0 — 启动与资格

按原SC-00两次间隔至少10秒同构采样：两个Git根、HEAD/index/status/worktree、进程/端口/DB/container、迁移与foreign reservation；未知资源为foreign。生成67行完整账本、resources/capabilities/leases/state及input manifest。状态恢复复用原合同，不重置retry预算。

输出实际可用命令/配置；探测缺能力不得联网下载补绿。优先跑已有离线域测试发现真正阻断，只有本run租约完备才写build输出/启动服务。

### W1 — 并行复用域门，不重做实现

S1检验CRUD、scope/权限、origin校验、公开frame、撤销语义、SQL及安全负例；S2检验管理UI、生成iframe代码、入口/auth隔离、Seat/Widget产物、配对和缓存。S0可同步准备隔离拓扑，S3只准备/审查旅程。

Admin有效入口：`bun run test -- src/modules/customer_service/api/seatConsoles.test.ts src/modules/customer_service/pages/CsWidgetInstallationsPage.test.tsx`；测试脚本本身含`--isolate`，保留实跑count/exit。静态构建依原合同执行 `bun run build:widget`、`bun run verify:widget`、`bun run verify:widget-pairing`。

Backend依原计划执行 `bash scripts/test/cs_deploy_unit_test.sh`、`bash scripts/test/customer_service_deploy_test.sh`、`bash scripts/check_widget_asset_pairing.sh`；运行前审阅实际脚本和有效配置，发现共享写/外向动作则停对应命令，不凭名称断定安全。EUnit按真实新编译模块/隔离配置执行，mock单测与真实PG结果分别登记。

产物门通过才 `bash deploy/widget/dryrun.sh --dist <客服Admin-worktree>/dist-widget`；没有Docker等价能力则BLOCKED，不把静态检查代替dry-run。

### W2 — 集成并冻结真实核心旅程

先SC-INT五项逐项证明：迁移/路由/产物/nginx一致、真PG CRUD/frame、真实网关四组API、integration SHA域门。冻结两仓candidate及构建产物，登记 `SC-INT=PASS` 后才置 `SC153_E2E_EXECUTE=1`。

优先一条批量业务旅程：Admin创建console→复制生成snippet→真实商城宿主部署→iframe QR→隔离身份确认→访客排队→坐席接单→双向消息/SSE→附件上传/预览/下载→重载核事实。各动作映射独立ID/oracle，不重复搭环境；不能用DB直接改origin替代Admin PUT旅程，保留原允许DB oracle的边界。

随后执行evil-origin浏览器CSP、sandbox能力拒绝、origin轮换、revoke新加载、Cookie/JWT权限负例、跨租户、请求/日志泄漏和已有Widget回归。补充两个真实身份同时持相同版本发claim的同步屏障，要求恰一成功、一冲突、DB唯一归属、失败端队列收敛；顺序stale replay仍保留但不能冒充并发。

真实浏览器命令：在租约/effective配置完成且SC-INT通过后，`SC153_E2E_EXECUTE=1 bunx playwright test --config=playwright.customer-service-seat-embed.config.ts`。禁止核心`page.route` mock，禁止缺EXECUTE时的skip算通过。浏览器交互遵循工作区autoglm优先规则；原计划Playwright套件用于可重跑自动验收，不以手工截图代替。

### W3 — 最小修复与一次最终门

只修真实FAIL/安全阻断；保留原失败→最小修复→独立review→受影响域回归→独立本地commit→新候选重验。局部日志不跨SHA签原PASS；不顺便重构存量或扩大产品范围。

所有预验通过后运行原SC-GATE全部Backend/Admin命令，包含全EUnit、lint/typecheck、Admin全单测/build、Seat产物门及真实浏览器。全局门独占测试执行资源，不与全产品全门同时运行；协调者登记队列。若共用可证明同一候选/config的完全相同门可复用原始证据，不能复用不同候选或不同DB结果。原门退出0、不缺Required断言，不豁免存量失败换绿。

## 5. 证据、失败恢复、交付 / Evidence and Delivery

每ID固定candidate pair、plan SHA、config/fixture/build/artifact指纹、command/exit、非零oracle/count、skip分类、日志/hash、reviewer、attempt、owner、failure_class/root_cause/recovery/next_unlock。67-ID exact set禁止用自评条目数量代替。

冻结保存Seat/Widget实际JS/CSS、manifest/checksum及后端常量配对；不能让manifest指向被删除dist或新head产物。每attempt保留HTTP/SQL/浏览器trace及脱敏检查，不把HTTP200或截图视为行为完成。

FAIL不盲重跑；仅已分类transient同指纹按原预算重试。超时先核活handle，不因观察超时重启；不确定迁移/消息/外部写不得自动重放。只按run/owner归属回收已授权资源；foreign进程/容器/worktree/WIP不动。安全故障/共享冲突停相应卡并留NEXT_UNLOCK，可独立卡继续。

固定交付 `control/acceptance-ledger.json`、`control/state.json`、state/recovery history、candidate/artifact manifest、`final/verdict.json`、`final/report.md`。明确LOCAL_CANDIDATE_PASS或PARTIAL/BLOCKED以及 `PRODUCTION_NOT_AUTHORIZED`；原67项全部通过才称V2本地验收完成。

给全产品线单独交付F12/F15证据索引、候选pair和限制，保留其App/设备旅程义务。合并前独占integration租约并重采main；主线漂移则比较后集成/retest，合并后只删除本run确认已集成的worktree/分支。无push、部署、第三方通知。

首个检查点交付输入/67-ID/resource对账和能立即执行集合，随后实际运行既有验收。不得只交新计划，不以测试数增加宣告完成，也不先猜工期再安排任务。
