# 两份计划 ZCODE 后续验收执行合同

**Goal:** 在已集成的三仓 main 上完成两份原计划的逐项验收和必要修复，不缩减范围，不继承历史 PASS。

**Architecture:** 复用现有 runner、功能目录、规格、账本和隔离环境方案。按输入核对、环境资格、核心旅程、分域全量、冻结终审推进；前置依赖只阻塞相应任务。

**Tech Stack:** Erlang/OTP、PostgreSQL 18、Flutter Android 真机/macOS、React 管理后台、现有 Python 验收工具。

## 1. 权威输入与当前边界

工作区 `/Users/leeyi/project/imboy.pub` 不是 Git 仓库；分别读取三仓 AGENTS.md 及其引用规范。

必须完整阅读：

- `/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-10-03-e2ee-production-excellence/plan.md`：36 IDs，AC-00–AC-35。
- `/Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-10-03-full-product-android-macos-integration-acceptance-v1.md`：61 IDs，含 F02–F14 模板展开及七个 A04。
- 同目录 `2026-10-03-full-product-android-macos-integration-acceptance-v1-execution-contract.md` 及 acceptance-set、dispatch-cards、input-manifest 配套文件；若实际文件名不同，先定位，不猜测内容。
- `/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-10-03-full-product-device-acceptance/next-wave-resource-review.md`。
- `/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-10-03-full-product-device-acceptance/evidence/main-closure-20261003/continuation-state.json` 和 `remaining-acceptance.tsv`。

两份计划 SHA256 分别为 `e9c8b33ddbe2b019818f82f1564f88baf99f1ca5f75d27934cba547ea5d86ce6`、`aeb4dc3da1d0c51ebecaf6a1edfad07558e3dbcaca6615e9c1b2e011b84832aa`。启动时重新计算；漂移先比较需求并更新输入记录，不继续使用旧绑定。

相关 worktree/分支已清理，仍须现场核验。不要重复合并、重建历史执行环境或覆盖无关 WIP。后端未跟踪文件 `docs/business/imboy-open-source-productization-strategy-v1.md` 属于外来改动。

本轮只读目录快照：1982 功能、188 页面，1763 功能无目标，231 为占位 oracle。219 个已关联功能只对应18个目标；文件和 catalog 指纹一致只证明关联完整性，不证明行为覆盖。

已发现错误关联：`fn:settings:e2ee_backup_import_page:001`–`:012` 指向 `integration_test/mine/mine_subpages_smoke_test.dart`，该测试仅打开页面、检查文本，不覆盖导入动作。此发现未修改 main。修复必须同时处理生成源，不能只手改生成 JSON，不能将导航冒烟当导入通过。

## 2. 授权与资源

本合同授权范围沿用会话和项目规则：本地只读调查、测试准备、必要代码修复、验证后独立本地提交。没有 push、部署、发布、生产迁移、真实资金或第三方通知授权。

隔离 PG、设备/账号/实例、MITM 等资源以已有明确批准记录为准。资源发现不等于授权：本地候选镜像、Android 连接状态和空闲端口均须重新检查。没有批准时先完成可独立的映射、离线测试和配置审阅，再一次性提交具体待确认表。

PG 候选镜像 ID、建议端口和阶段范围见资源审阅文件；不要拉新镜像、接共享数据库或盲跑默认配置。PG-only 批准不自动涵盖后端启动、迁移、夹具及清理。测试必须使用合成身份、独立 namespace，凭证与 PII 不入仓。Android 必须真机，macOS 必须实际 App 实例。

D02/D03/D04/D05/D07 及 D06 条件性扩展范围仍须核对决策记录。MLS/PQ/见证/SLO 未批准不能选择默认值；已有批准范围不因新增决策被整体撤销。保留区仍受项目限制。

## 3. 执行顺序和责任

### W0：输入、进程和资源对账

协调者独占输入快照、账本和资源表。逐仓记录 root、branch、HEAD、status、worktree、相关分支；核验活动进程/租约，不因旧 state 显示 RUNNING 就重启任务。建立新 run_id，将证据放在后端计划目录下独立 evidence 子目录。

输出 `input-snapshot.json`、`resource-register.json` 和 `acceptance.tsv`；97-ID 精确集合无缺失、重复或额外项。保持历史失败、旧证据及其候选身份，当前 PASS 从零重验。

### W1：F00 覆盖核验与 F01 环境资格

功能映射 owner 独占 App `test/auto_test/` 相关目录与映射生成源；环境 owner 独占现有 runner、配置与夹具。若 ZCODE 使用多执行者，先登记独占路径，所有人不得回退他人改动；设备执行串行持有租约。

先核查已有目标是否真的执行每个功能，再补缺少的目标。每格写用户动作、UI 结果、后端事实、权限负例、恢复步骤和平台要求；禁止按页面名批量绑定同一冒烟测试。去重须保留原始功能去向和可核验来源，不删除 required 项。

App 内执行 `python3 scripts/auto_test_lib/inventory.py --check` 和 `python3 scripts/auto_test_lib/inventory.py --acceptance-check`，保留真实输出和退出码。前者本次现场 RC=0，188页面/1982功能；后者预期在缺映射时失败，失败不能被忽略或改 gate 换绿。更改生成源后再生、检查稳定性，并运行现有相关工具测试。

环境 owner 审核实际 effective config 和出站能力；获范围批准后才启动隔离资源，验证 PG18 及所需扩展、幂等 seed/reset、设备有效端点、runner 假绿拒绝矩阵。不得另造第二套全量框架。

### W2：尽早执行六条校准旅程

本域映射、真实环境和设备资格就绪即可运行，不等待全部 F00 闭合：跨端 C2C、群撤权、动态可见性、频道 CRUD、组织图、我的/设置。按原合同记录设计、补测试、构建、单端/跨端占用和修复复验耗时；另执行一次真实后台旅程。

跨端用 runner 屏障关联参与者、账号、message_id 和 namespace；验证 UI→真实后端→对端 UI→重启持久化。撤权检查真实解密/访问能力，不只看页面提示。此阶段仅校准和暴露阻断，不能代替全产品或36项 E2EE终验。

### W3：原计划全部分域与安全门

依原 dispatch cards 覆盖 F02–F16，保留 Android/macOS 每个 required 格；逐项执行 AC-00–AC-35 的原 oracle。真 PG 补足迁移及四个此前排除的 PG E2EE 套件；资金域使用合成本地数据，外部支付单独 BLOCKED_EXTERNAL。

MLS/PQ、透明度、见证、真机负例、MITM、元数据、fuzz、负载/SLO须按原要求给实际证据；未实现记 GAP_IMPLEMENTATION，缺批准记对应 BLOCKED，不用普通 Olm、mock或离线 vectors 替代。

每次失败先保存 command/exit/log，分类后仅做阻断验收的最小修复；独立 review、受影响域回归、独立本地提交，再使相关旧证据失效。不要每修一个问题就运行全产品和全 EUnit。

### W4：冻结与唯一全量终审

全部预验闭合后冻结三仓 SHA、计划/依赖/runtime/config/fixture/runner/build 指纹。最终候选运行原计划完整全局门；后端全 EUnit 要求两个连续 RC=0，VM-global TSID套件按现有隔离机制执行。所有 required 设备集合在最终候选重跑，禁止跨 SHA 借证据。

集成人、门执行者、只读终审职责分离；终审不改 ledger 或候选。若修复代码、测试、fixture/config，回责任卡、新候选、重验受影响域及必要最终门。始终生成报告，不能因资源缺失跳过最终状态输出。

## 4. 证据、恢复与停止条件

每项记录原 ID/case/platform、plan 与三仓 SHA、配置/fixture/build 指纹、命令、真实退出码、executed/skipped 数、业务 oracle、artifact 路径和 SHA256、设备/后端关联、reviewer、时间及 attempt。日志脱敏；截图为补充。每 target 一次，禁止 aggregator 与 leaf重复执行。

Failure → Recovery → Retry → Next State 必须持久化：FAIL 保留原 attempt，修复后新 attempt；已分类基础设施故障同指纹最多重试2次；超时先查具体活进程并轮询同 handle，不能观察超时就重启；不确定消息/迁移/外部写禁止盲重放。仅清理可证明 run/owner 归属且在批准范围内的资源，禁止全局 prune。

立即停对应卡：生产/共享资源误指向、真实身份或秘密泄露、无授权外向操作、保留区修改、租约冲突、安全失败或证据篡改。继续可独立任务；不靠缩范围、关闭功能、N/A、skip或放宽断言获得通过。

## 5. 交付与完成标准

提交完整97-ID账本、功能/平台 exact-set、证据 manifest、失败恢复记录、缺陷与独立review、精确候选、清理记录及 always-run 最终报告。分别输出 LOCAL_REGRESSION、ANDROID_DEVICE、MACOS_DEVICE、CROSS_DEVICE_JOURNEYS、ADMIN_REAL_BACKEND、UX_USER_REVIEW、EXTERNAL_INTEGRATIONS，以及原 E2EE LOCAL_CANDIDATE / PRODUCTION_QUALIFIED / TOP_TIER_PROFILE / PRODUCTION_DEPLOYED。

只有原 required 集合全部满足才能称整体完成。资源/决策缺失或协议未完成应逐ID列明，不将局部绿灯换算整体完成率。不报告无实测依据的周数；完成六旅程校准后才按原合同给P50/P80，授权等待单列。

首个检查点必须交付：现场输入快照、97-ID精确对账、已有绑定有效性抽查及错误清单、完整资源/决策待确认表、可以立即运行的任务集合。随后进入实际执行，不停留在重新设计计划。
